#include "parselets/infix_parselets.h"

#include <parser.h>
#include <llvm/ExecutionEngine/JITLink/JITLink.h>

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
    std::unique_ptr<Expression> CallOperationParselet::parse(Parser& parser, std::unique_ptr<Expression> left, Token tk)
    {
        std::vector<std::unique_ptr<Expression>> arguments;
        if(parser.discard(TokenType::RParen))
            return std::make_unique<CallOperation>(std::move(left), std::move(arguments));

        arguments.push_back(parser.parseExpression(0));
        while(!parser.discard(TokenType::RParen))
        {
            if(!parser.discard(TokenType::Comma)) parser.error("Expected ','", parser.Peek());
            auto arg = parser.parseExpression(0);
            arguments.push_back(std::move(arg));
        }
        return std::make_unique<CallOperation>(std::move(left), std::move(arguments));
    }

    std::unique_ptr<Expression> ScopeOperationParselet::parse(Parser& parser, std::unique_ptr<Expression> left, Token tk)
    {
        auto as_name = dynamic_cast<NameExpression*>(left.get());
        auto as_scope = dynamic_cast<ScopeOperation*>(left.get());
        if(!as_name && !as_scope)
        {
            parser.error("Left of '::' must be a name", tk);
            return nullptr;
        }
        auto expr = parser.parseExpression(prec);
        auto expr_as_name = dynamic_cast<NameExpression*>(expr.get());
        if(!expr_as_name)
        {
            parser.error("Right of '::' must be a name", tk);
            return nullptr;
        }

        if(as_name)
        {
            return std::make_unique<ScopeOperation>(Type{.name= std::string{as_name->token.text}}, "", std::string{expr_as_name->token.text});
        }
        if(as_scope)
        {
            as_scope->scope += "::" + as_scope->type.name;
            as_scope->type.name = as_scope->name;
            as_scope->name = std::string{expr_as_name->token.text};
            return left;
        }
        return nullptr;
    }
}
