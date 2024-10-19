#include "parser.h"

#include <precedences.h>
#include "statement.h"
namespace Yoyo
{

    Parser::Parser(std::string src) : source(std::move(src)), scn(source)
    {
        auto prefix_op_parselet = std::make_shared<PrefixOperationParselet>();
        auto name_parselet = std::make_shared<NameParselet>();
        prefixParselets[TokenType::Minus] = prefix_op_parselet;
        prefixParselets[TokenType::Bang] = prefix_op_parselet;
        prefixParselets[TokenType::IntegerLiteral] = std::make_shared<IntLiteralParselet>();
        prefixParselets[TokenType::RealLiteral] = std::make_shared<RealLiteralParselet>();
        prefixParselets[TokenType::StringLiteral] = std::make_shared<StringLiteralParselet>();
        prefixParselets[TokenType::Identifier] = name_parselet;
        prefixParselets[TokenType::SPIdentifier] = name_parselet;
        prefixParselets[TokenType::LParen] = std::make_shared<GroupParselet>();

        auto sum_parselet = std::make_shared<BinaryOperationParselet>(Precedences::Sum);
        auto product_parselet = std::make_shared<BinaryOperationParselet>(Precedences::Product);
        auto equality_parselet = std::make_shared<BinaryOperationParselet>(Precedences::Equality);
        auto relational_parselet = std::make_shared<BinaryOperationParselet>(Precedences::Relational);
        auto access_parselet = std::make_shared<BinaryOperationParselet>(Precedences::MemberAccess);
        infixParselets[TokenType::Plus] = sum_parselet;
        infixParselets[TokenType::Minus] = sum_parselet;
        infixParselets[TokenType::Star] = product_parselet;
        infixParselets[TokenType::Slash] = product_parselet;
        infixParselets[TokenType::Percent] = product_parselet;
        infixParselets[TokenType::DoubleEqual] = equality_parselet;
        infixParselets[TokenType::BangEqual] = equality_parselet;
        infixParselets[TokenType::Greater] = relational_parselet;
        infixParselets[TokenType::GreaterEqual] = relational_parselet;
        infixParselets[TokenType::LessEqual] = relational_parselet;
        infixParselets[TokenType::Less] = relational_parselet;
        infixParselets[TokenType::Dot] = access_parselet;

        infixParselets[TokenType::Pipe] = std::make_shared<BinaryOperationParselet>(Precedences::BitOr);
        infixParselets[TokenType::Ampersand] = std::make_shared<BinaryOperationParselet>(Precedences::BitAnd);
        infixParselets[TokenType::Caret] = std::make_shared<BinaryOperationParselet>(Precedences::BitXor);

        auto assign_parselet = std::make_shared<BinaryOperationParselet>(Precedences::Assignment, true);
        infixParselets[TokenType::Equal] = assign_parselet;
        infixParselets[TokenType::PlusEqual] = assign_parselet;
        infixParselets[TokenType::MinusEqual] = assign_parselet;
        infixParselets[TokenType::StarEqual] = assign_parselet;
        infixParselets[TokenType::SlashEqual] = assign_parselet;
        infixParselets[TokenType::PercentEqual] = assign_parselet;
        infixParselets[TokenType::AmpersandEqual] = assign_parselet;
        infixParselets[TokenType::PipeEqual] = assign_parselet;
        infixParselets[TokenType::CaretEqual] = assign_parselet;

        infixParselets[TokenType::DoubleAmpersand] = std::make_shared<LogicalOperationParselet>(Precedences::LogicAnd);
        infixParselets[TokenType::DoublePipe] = std::make_shared<LogicalOperationParselet>(Precedences::LogicOr);

        infixParselets[TokenType::Bang] = std::make_shared<PostfixOperationParselet>(Precedences::InvalidPropagate);
        infixParselets[TokenType::LParen] = std::make_shared<CallOperationParselet>(Precedences::Call);
    }

    PrefixParselet* Parser::GetPrefixParselet(TokenType t)
    {
        if(const auto f = prefixParselets.find(t); f != prefixParselets.end())
            return f->second.get();
        return nullptr;
    }
    InfixParselet* Parser::GetInfixParselet(TokenType t)
    {
        if(const auto f = infixParselets.find(t); f != infixParselets.end())
            return f->second.get();
        return nullptr;
    }
    uint32_t Parser::GetNextPrecedence()
    {
        auto tk = Peek();
        if(!tk) return 0;
        auto parselet = GetInfixParselet(tk->type);
        if(!parselet) return 0;
        return parselet->precedence();
    }

    bool Parser::discard(TokenType t)
    {
        auto tk = Peek();
        if(tk && tk->type == t)
        {
            Get();
            return true;
        }
        return false;
    }

    void Parser::pushToken(Token t)
    {
        peekBuffer.push_back(t);
    }

    std::optional<Token> Parser::Peek()
    {
        if(!peekBuffer.empty())
        {
            return peekBuffer.back();
        }
        auto tk = scn.NextToken();
        if(!tk) return std::nullopt;
        peekBuffer.push_back(tk.value());
        return peekBuffer.back();
    }

    std::optional<Token> Parser::Get()
    {
        if(!peekBuffer.empty())
        {
            auto bk = peekBuffer.back();
            peekBuffer.pop_back();
            return bk;
        }
        return scn.NextToken();
    }


    std::unique_ptr<Expression> Parser::parseExpression(uint32_t precedence)
    {
        auto tk = Get();
        if(!tk) return nullptr; //todo: handle invalid token
        auto prefixParselet = GetPrefixParselet(tk->type);
        if(!prefixParselet) return nullptr;
        auto left = prefixParselet->parse(*this, tk.value());

        while(precedence < GetNextPrecedence())
        {
            //GetNext ensures we have a valid token
            tk = Get();
            auto infixParselet = GetInfixParselet(tk->type);
            left = infixParselet->parse(*this, std::move(left), *tk);
        }
        return left;
    }

    std::optional<Type> parseArrayType(Token t, Parser& parser)
    {
        auto next = parser.parseType(0);
        if(!next) return std::nullopt;
        if(!parser.discard(TokenType::RSquare)) return std::nullopt;
        return Type("__arr", {*next});
    }
    std::optional<Type> parseTypeGroup(Token t, Parser& parser)
    {

        auto next = parser.parseType(0);
        if(!next) return std::nullopt;
        if(!parser.discard(TokenType::RCurly)) return std::nullopt;
        return *next;
    }
    static constexpr uint32_t PipePrecedence = 1;
    static constexpr uint32_t AmpersandPrecedence = 2;
    static constexpr uint32_t TemplatePrecedence = 3;
    uint32_t Parser::GetNextTypePrecedence()
    {
        auto tk = Peek();
        if(!tk) return 0;
        switch(tk->type)
        {
        case TokenType::Ampersand: return AmpersandPrecedence; break;
        case TokenType::Pipe: return PipePrecedence; break;
        case TokenType::TemplateOpen: return TemplatePrecedence; break;
        default: return 0;
        }
    }
    std::optional<Type> parseAmpTypeExpr(Parser& p, Type left)
    {
        auto t = p.parseType(AmpersandPrecedence);
        if(!t) return std::nullopt;
        if(left.name == "__tup")
        {
            left.subtypes.push_back(*t);
            return left;
        }
        return Type("__tup", {left, *t});
    }
    std::optional<Type> parsePipeTypeExpr(Parser& p, Type left)
    {
        auto t = p.parseType(PipePrecedence);
        if(!t) return std::nullopt;
        if(left.name == "__var")
        {
            left.subtypes.push_back(*t);
            return left;
        }
        return Type("__var", {left, *t});
    }
    std::optional<Type> parseTemplateTypeExpr(Parser& p, Type left)
    {
        auto t = p.parseType(0);
        if(!t) return std::nullopt;
        left.subtypes.push_back(std::move(t).value());
        while(p.discard(TokenType::Comma))
        {
            t = p.parseType(0);
            if(!t) return std::nullopt;
            left.subtypes.push_back(std::move(t).value());
        }
        if(!p.discard(TokenType::Greater))
        {
            if(p.discard(TokenType::DoubleGreater))
            {
                p.pushToken(Token{TokenType::Greater});
                return left;
            }
            return std::nullopt;
        }
        return left;
    }
    std::optional<Type> Parser::parseType(uint32_t precedence)
    {
        auto tk = Get();
        if(!tk) return std::nullopt;
        std::optional<Type> t;
        switch(tk->type)
        {
        case TokenType::Identifier: t = Type(std::string(tk->text), {}); break;
        case TokenType::LSquare: t = parseArrayType(*tk, *this); break;
        case TokenType::LCurly: t = parseTypeGroup(*tk, *this); break;
        default: t = std::nullopt;
        }
        while(precedence < GetNextTypePrecedence())
        {
            if(!t) return std::nullopt;
            tk = Get();
            switch(tk->type)
            {
            case TokenType::Ampersand: t = parseAmpTypeExpr(*this, std::move(t).value()); break;
            case TokenType::Pipe: t = parsePipeTypeExpr(*this, std::move(t).value()); break;
            case TokenType::TemplateOpen: t = parseTemplateTypeExpr(*this, std::move(t).value()); break;
            default: /*unreachable*/;
            }
        }
        return t;
    }
}
