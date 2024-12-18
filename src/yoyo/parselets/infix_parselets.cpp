#include "parselets/infix_parselets.h"

#include <parser.h>
#include <llvm/ExecutionEngine/JITLink/JITLink.h>

namespace Yoyo
{
    std::unique_ptr<Expression> BinaryOperationParselet::parse(Parser& parser, std::unique_ptr<Expression> left, Token tk)
    {
        const auto prec_diff = is_right ? 1 : 0;
        auto fn = [&parser](std::unique_ptr<BinaryOperation>&& e)
        {
            e->lhs->parent = e.get(); e->rhs->parent = e.get(); e->parent = parser.parent;
            e->beg = e->lhs->beg; e->end = e->rhs->end; return e;
        };
        return fn(std::make_unique<BinaryOperation>(tk, std::move(left), parser.parseExpression(prec - prec_diff)));
    }
    std::unique_ptr<Expression> LogicalOperationParselet::parse(Parser& parser, std::unique_ptr<Expression> left, Token tk)
    {
        const auto prec_diff = is_right ? 1 : 0;
        auto fn = [&parser](std::unique_ptr<LogicalOperation>&& e)
        {
            e->lhs->parent = e.get(); e->rhs->parent = e.get(); e->parent = parser.parent;
            e->beg = e->lhs->beg; e->end = e->rhs->end; return e;
        };
        return fn(std::make_unique<LogicalOperation>(tk, std::move(left), parser.parseExpression(prec - prec_diff)));
    }
    std::unique_ptr<Expression> PostfixOperationParselet::parse(Parser& parser, std::unique_ptr<Expression> left, Token tk)
    {
        auto fn = [&parser](std::unique_ptr<PostfixOperation>&& e)
        {
            e->operand->parent = e.get(); e->parent = parser.parent;
            e->beg = e->operand->beg; e->end = SourceLocation{e->beg.line, e->beg.column + e->op.text.size()};
            return e;
        };
        return fn(std::make_unique<PostfixOperation>(tk, std::move(left)));
    }
    std::unique_ptr<Expression> CallOperationParselet::parse(Parser& parser, std::unique_ptr<Expression> left, Token tk)
    {
        auto fn = [&parser](std::unique_ptr<CallOperation>&& e)
        {
            e->callee->parent = e.get(); e->parent = parser.parent;
            for(auto& arg : e->arguments) arg->parent = e.get();
            e->beg = e->callee->beg; e->end = parser.discardLocation; return e;
        };
        std::vector<std::unique_ptr<Expression>> arguments;
        if(parser.discard(TokenType::RParen))
            return fn(std::make_unique<CallOperation>(std::move(left), std::move(arguments)));

        arguments.push_back(parser.parseExpression(0));
        while(!parser.discard(TokenType::RParen))
        {
            if(!parser.discard(TokenType::Comma)) parser.error("Expected ','", parser.Peek());
            auto arg = parser.parseExpression(0);
            arguments.push_back(std::move(arg));
        }
        return fn(std::make_unique<CallOperation>(std::move(left), std::move(arguments)));
    }

    std::unique_ptr<Expression> ScopeOperationParselet::parse(Parser& parser, std::unique_ptr<Expression> left, Token tk)
    {
        auto as_name = dynamic_cast<NameExpression*>(left.get());
        if(!as_name) { parser.error("Expression to the left of '::' must be a name", tk); return nullptr; }
        Type original_type {as_name->text};
        if(auto as_gen_name = dynamic_cast<GenericNameExpression*>(as_name))
            for(auto& arg : as_gen_name->arguments)
                original_type.subtypes.emplace_back(std::move(arg));
        auto others = parser.parseType(0);
        if(others)
        {
            others->name = original_type.full_name() + "::" + others->name;
            original_type = std::move(others).value();
        }
        auto curly = parser.Peek();
        if(curly && curly->type == TokenType::LCurly)
        {
            auto body = parser.parseObjectLiteral();
            return Expression::attachSLAndParent(
                std::make_unique<ObjectLiteral>(std::move(original_type), std::move(body)), tk.loc,
                parser.discardLocation, parser.parent);
        }
        return Expression::attachSLAndParent(std::make_unique<ScopeOperation>(std::move(original_type)),
            left->beg, curly->loc, parser.parent);
    }
}
