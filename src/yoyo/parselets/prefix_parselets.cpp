#include "parselets/prefix_parselets.h"


#include "parser.h"
#include "precedences.h"
namespace Yoyo
{
    std::unique_ptr<Expression> PrefixOperationParselet::parse(Parser& parser, Token tk)
    {
        //prefix operations are right associative
        return std::make_unique<PrefixOperation>(tk, parser.parseExpression(Precedences::Prefix - 1));
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
        auto expr = parser.parseExpression(0);
        if(!parser.discard(TokenType::RParen)) return nullptr;
        return std::make_unique<GroupingExpression>(std::move(expr));
    }
    std::unique_ptr<Expression> BoolLiteralParselet::parse(Parser& parser, Token tk)
    {
        return std::make_unique<BooleanLiteral>(tk);
    }



}