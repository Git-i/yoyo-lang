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

}