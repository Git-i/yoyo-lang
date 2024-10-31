#include "parselets/prefix_parselets.h"

#include <ranges>


#include "parser.h"
#include "precedences.h"
namespace Yoyo
{
    std::unique_ptr<Expression> PrefixOperationParselet::parse(Parser& parser, Token tk)
    {
        //prefix operations are right associative
        auto expr = parser.parseExpression(Precedences::Prefix - 1);
        return std::make_unique<PrefixOperation>(tk, std::move(expr));
    }
    std::unique_ptr<Expression> IntLiteralParselet::parse(Parser& parser, Token tk)
    {
        return std::make_unique<IntegerLiteral>(tk);
    }
    std::unique_ptr<Expression> RealLiteralParselet::parse(Parser& parser, Token tk)
    {
        return std::make_unique<RealLiteral>(tk);
    }
    std::unique_ptr<Expression> StringLiteralParselet::parse(Parser& parser, Token tk)
    {
        return std::make_unique<StringLiteral>(tk);
    }
    std::unique_ptr<Expression> NameParselet::parse(Parser& parser, Token tk)
    {
        return std::make_unique<NameExpression>(tk);
    }
    std::unique_ptr<Expression> GroupParselet::parse(Parser& parser, Token tk)
    {
        //empty tuple
        if(parser.discard(TokenType::RParen))
            return std::make_unique<TupleLiteral>(std::vector<std::unique_ptr<Expression>>{});
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
        return is_tuple ? static_cast<std::unique_ptr<Expression>>(std::make_unique<TupleLiteral>(std::move(expressions)))
            : static_cast<std::unique_ptr<Expression>>(std::make_unique<GroupingExpression>(std::move(expr)));
    }
    std::unique_ptr<Expression> ArrayLiteralParselet::parse(Parser& parser, Token tk)
    {
        if(parser.discard(TokenType::RSquare))
            return std::make_unique<ArrayLiteral>(std::vector<std::unique_ptr<Expression>>{});
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
        return std::make_unique<ArrayLiteral>(std::move(expressions));
    }

    std::unique_ptr<Expression> BoolLiteralParselet::parse(Parser& parser, Token tk)
    {
        return std::make_unique<BooleanLiteral>(tk);
    }
    auto print(int);
    std::unique_ptr<Expression> LambdaParselet::parse(Parser& parser, Token tok)
    {
        std::vector<std::pair<std::string, ParamType>> captures;
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
                    auto t = ParamType::In;
                    if(parser.discard(TokenType::Colon))
                    {
                        tk = parser.Peek();
                        if(!tk) return nullptr;
                        if(tk->type == TokenType::In) parser.Get();
                        else if(tk->type == TokenType::InOut)
                        {
                            parser.Get();
                            t = ParamType::InOut;
                        }
                    }
                    captures.emplace_back(name, t);
                }
            } while(parser.discard(TokenType::Comma));
            if(!parser.discard(TokenType::Pipe)) parser.error("Expected '|'", parser.Peek());
        }
        auto tk = parser.Peek();
        if(!tk) return nullptr;
        FunctionSignature sig;
        if(tk->type == TokenType::LParen) sig = *parser.parseFunctionSignature();
        else if(tk->type == TokenType::Arrow)
        {
            parser.Get();
            auto ty = parser.parseType(0);
            sig = FunctionSignature{.returnType = std::move(ty).value_or(Type{}), .return_is_ref = false, .parameters = {}};
        }
        auto stat = parser.parseStatement();
        auto expr = std::make_unique<LambdaExpression>(std::move(captures), std::move(sig), std::move(stat));
        //lambda hash depends on the pointer from `make_unique`
        expr->hash = std::to_string(reinterpret_cast<std::uintptr_t>(expr.get()));
        return expr;
    }



}