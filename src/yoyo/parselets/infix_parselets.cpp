#include "parselets/infix_parselets.h"

#include <parser.h>

namespace Yoyo
{
    std::unique_ptr<Expression> BinaryOperationParselet::parse(Parser& parser, std::unique_ptr<Expression> left, Token tk)
    {
        const auto prec_diff = is_right ? 1 : 0;
        return std::make_unique<BinaryOperation>(tk, std::move(left), parser.parseExpression(prec - prec_diff));
    }
    std::unique_ptr<Expression> LogicalOperationParselet::parse(Parser& parser, std::unique_ptr<Expression> left, Token tk)
    {
        const auto prec_diff = is_right ? 1 : 0;
        return std::make_unique<LogicalOperation>(tk, std::move(left), parser.parseExpression(prec - prec_diff));
    }
    std::unique_ptr<Expression> PostfixOperationParselet::parse(Parser& parser, std::unique_ptr<Expression> left, Token tk)
    {
        return std::make_unique<PostfixOperation>(tk, std::move(left));
    }



}
