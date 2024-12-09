#include "parselets/prefix_parselets.h"

#include <ranges>


#include "parser.h"
#include "precedences.h"
namespace Yoyo
{
    std::unique_ptr<Expression> PrefixOperationParselet::parse(Parser& parser, Token tk)
    {
        if(tk.type == TokenType::Ampersand && parser.discard(TokenType::Mut)) tk.type = TokenType::RefMut;
        //prefix operations are right associative
        auto self = std::make_unique<PrefixOperation>(tk, nullptr);

        auto old_parent = parser.parent;
        parser.parent = self.get();
        auto expr = parser.parseExpression(Precedences::Prefix - 1);
        auto end = expr->end;
        parser.parent = old_parent;

        self->operand = std::move(expr);

        return Expression::attachSLAndParent(std::move(self),
            tk.loc, end, parser.parent
            );
    }
    std::unique_ptr<Expression> IntLiteralParselet::parse(Parser& parser, Token tk)
    {
        return Expression::attachSLAndParent(
            std::make_unique<IntegerLiteral>(tk.text), tk.loc,
            SourceLocation{tk.loc.line, tk.loc.column + tk.text.size()}, parser.parent);
    }
    std::unique_ptr<Expression> RealLiteralParselet::parse(Parser& parser, Token tk)
    {
        return Expression::attachSLAndParent(
            std::make_unique<RealLiteral>(tk), tk.loc,
            SourceLocation{tk.loc.line, tk.loc.column + tk.text.size()},
            parser.parent);
    }
    std::unique_ptr<Expression> StringLiteralParselet::parse(Parser& parser, Token tk)
    {
        std::vector<std::variant<std::string, std::unique_ptr<Expression>>> values;
        size_t pos = 0;
        size_t current_pos = 0;
        while(pos != tk.text.npos)
        {
            pos = tk.text.find_first_of('$', pos);
            if(pos == tk.text.npos) break;
            if(tk.text[pos + 1] != '{') { pos++; continue; }
            if(current_pos != pos)
            {
                values.emplace_back(std::string{tk.text.begin() + current_pos, tk.text.begin() + pos});
            }
            pos += 2; //skip the ${
            std::string str(tk.text.begin() + pos, tk.text.end());
            Parser subparser(std::move(str));
            auto expr = subparser.parseExpression(0);
            if(subparser.failed()) parser.error("Failed to parse sub expression in string literal", std::nullopt);
            auto rbrace = subparser.Peek(); // has to be a '}'
            if(!rbrace) return nullptr;
            if(rbrace->type != TokenType::RCurly) parser.error("Expected '}", rbrace);
            auto offset = rbrace->text.data() - subparser.getSource().data();
            values.emplace_back(std::move(expr));
            pos += offset + 1;
            current_pos = pos;
        }
        if(current_pos != tk.text.size() - 1)
        {
            values.emplace_back(std::string{tk.text.begin() + current_pos, tk.text.end()});
        }

        auto expr = std::make_unique<StringLiteral>(std::move(values));
        for(auto& v : expr->literal)
            if(std::holds_alternative<std::unique_ptr<Expression>>(v))
                std::get<1>(v)->parent = expr.get();

        return Expression::attachSLAndParent(std::move(expr), tk.loc,
            SourceLocation{tk.loc.line, tk.loc.column + tk.text.size()}, parser.parent);
    }
    std::unique_ptr<Expression> NameParselet::parse(Parser& parser, Token tk)
    {
        auto curly = parser.Peek();
        if(curly && curly->type == TokenType::LCurly)
        {
            auto body = parser.parseObjectLiteral();
            return Expression::attachSLAndParent(
                std::make_unique<ObjectLiteral>(Type{.name = std::string{tk.text}}, std::move(body)), tk.loc,
                parser.discardLocation, parser.parent);
        }
        //if we see ::< it's a generic
        if(!parser.discard(TokenType::TemplateOpen))
            return Expression::attachSLAndParent(std::make_unique<NameExpression>(std::string(tk.text)), tk.loc,
                SourceLocation{tk.loc.line, tk.loc.column + tk.text.size()}, parser.parent);
        std::vector types{parser.parseType(0).value_or(Type{})};
        while(!parser.discard(TokenType::Greater))
        {
            if(!parser.discard(TokenType::Comma)) parser.error("Expected ','", parser.Peek());
            types.push_back(parser.parseType(0).value_or(Type{}));
        }
        return Expression::attachSLAndParent(std::make_unique<GenericNameExpression>(std::string(tk.text), std::move(types)),
            tk.loc, parser.discardLocation, parser.parent);
    }
    std::unique_ptr<Expression> GroupParselet::parse(Parser& parser, Token tk)
    {
        //empty tuple
        if(parser.discard(TokenType::RParen))
            return Expression::attachSLAndParent(std::make_unique<TupleLiteral>(std::vector<std::unique_ptr<Expression>>{}),
                tk.loc, parser.discardLocation, parser.parent);

        auto old_parent = parser.parent;
        parser.parent = old_parent;

        auto expr = parser.parseExpression(0);
        if(!expr) parser.synchronizeTo({{TokenType::RParen, TokenType::Comma}});
        bool is_tuple = false;
        std::vector<std::unique_ptr<Expression>> expressions;
        while(parser.discard(TokenType::Comma))
        {
            expressions.push_back(std::move(expr));
            is_tuple = true;
            expr = parser.parseExpression(0);
            if(!expr) parser.synchronizeTo({{TokenType::RParen, TokenType::Comma}});
        }
        if(is_tuple) expressions.push_back(std::move(expr));
        if(!parser.discard(TokenType::RParen))
        {
            parser.error("Expected ')'", parser.Peek());
        }

        parser.parent = old_parent;
        return is_tuple ? Expression::attachSLAndParent(std::make_unique<TupleLiteral>(std::move(expressions)),
            tk.loc, parser.discardLocation, parser.parent)
            : Expression::attachSLAndParent(std::make_unique<GroupingExpression>(std::move(expr)),
                tk.loc, parser.discardLocation, parser.parent);
    }
    std::unique_ptr<Expression> ArrayLiteralParselet::parse(Parser& parser, Token tk)
    {
        if(parser.discard(TokenType::RSquare))
            return Expression::attachSLAndParent(std::make_unique<ArrayLiteral>(std::vector<std::unique_ptr<Expression>>{}),
                tk.loc, parser.discardLocation, parser.parent);

        auto old_parent = parser.parent;
        parser.parent = old_parent;

        auto expr = parser.parseExpression(0);
        if(!expr) parser.synchronizeTo({{TokenType::RSquare, TokenType::Comma}});
        std::vector<std::unique_ptr<Expression>> expressions;
        expressions.push_back(std::move(expr));
        while(parser.discard(TokenType::Comma))
        {
            expr = parser.parseExpression(0);
            if(!expr) parser.synchronizeTo({{TokenType::RSquare, TokenType::Comma}});
            expressions.push_back(std::move(expr));
        }
        if(!parser.discard(TokenType::RSquare))
        {
            parser.error("Expected ']'", parser.Peek());
        }

        parser.parent = old_parent;

        return Expression::attachSLAndParent(std::make_unique<ArrayLiteral>(std::move(expressions)),
            tk.loc, parser.discardLocation, parser.parent);
    }

    std::unique_ptr<Expression> BoolLiteralParselet::parse(Parser& parser, Token tk)
    {
        return Expression::attachSLAndParent(std::make_unique<BooleanLiteral>(tk), tk.loc,
            SourceLocation{tk.loc.line, tk.loc.column + tk.text.size()}, parser.parent);
    }
    std::unique_ptr<Expression> LambdaParselet::parse(Parser& parser, Token tok)
    {
        std::vector<std::string> captures;
        if(tok.type == TokenType::Pipe)
        {
            do
            {
                auto tk = parser.Peek();
                if(!tk) return nullptr;
                if(tk->type == TokenType::Identifier)
                {
                    parser.Get();
                    std::string name(tk->text);
                    captures.emplace_back(name);
                }
            } while(parser.discard(TokenType::Comma));
            if(!parser.discard(TokenType::Pipe)) parser.error("Expected '|'", parser.Peek());
        }
        auto tk = parser.Peek();
        if(!tk) return nullptr;
        FunctionSignature sig = *parser.parseFunctionSignature();



        auto stat = parser.parseStatement();
        auto expr = std::make_unique<LambdaExpression>(std::move(captures), std::move(sig), std::move(stat));

        expr->body->parent = expr.get();
        //lambda hash depends on the pointer from `make_unique`
        expr->hash = std::to_string(reinterpret_cast<std::uintptr_t>(expr.get()));
        return Expression::attachSLAndParent(std::move(expr), tok.loc, stat->end, parser.parent);
    }

    std::unique_ptr<Expression> NullLiteralParselet::parse(Parser& parser, Token tk)
    {
        return Expression::attachSLAndParent(std::make_unique<NullLiteral>(), tk.loc,
            SourceLocation{tk.loc.line, tk.loc.column + tk.text.size()} , parser.parent);
    }
    std::unique_ptr<Expression> CharLiteralParselet::parse(Parser& parser, Token tk)
    {
        auto expr = std::make_unique<CharLiteral>();
        expr->value = 0;
        for(size_t i = 0; i < tk.text.size(); i++)
            reinterpret_cast<uint8_t*>(&expr->value)[i] = tk.text[i];
        SourceLocation end{.line = tk.loc.line, .column = tk.loc.column + tk.text.size() + 1};
        return Expression::attachSLAndParent(std::move(expr), tk.loc, end, parser.parent);
    }

}
