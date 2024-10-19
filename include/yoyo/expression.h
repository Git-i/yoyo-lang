#pragma once
#include "token.h"
#include <memory>
#include <vector>

namespace Yoyo
{
    class Expression {
    public:
        virtual ~Expression() = default;
    };
    class IntegerLiteral : public Expression {
    public:
        Token token;
        explicit IntegerLiteral(const Token& tk) : token(tk) {}
    };
    class RealLiteral : public Expression {
    public:
        Token token;
        explicit RealLiteral(const Token& tk) : token(tk) {}
    };
    class StringLiteral : public Expression {
    public:
        Token token;
        explicit StringLiteral(const Token& tk) : token(tk) {}
    };
    class NameExpression : public Expression {
    public:
        Token token;
        explicit NameExpression(const Token& tk) : token(tk) {}
    };
    class PrefixOperation : public Expression {
    public:
        Token op;
        std::unique_ptr<Expression> operand;
        PrefixOperation(const Token& op, std::unique_ptr<Expression> operand) : op(op), operand(std::move(operand)) {}
    };
    class GroupingExpression : public Expression {
    public:
        std::unique_ptr<Expression> expr;
        explicit GroupingExpression(std::unique_ptr<Expression> expr) : expr(std::move(expr)) {}
    };
    class BinaryOperation : public Expression {
    public:
        Token op;
        std::unique_ptr<Expression> lhs;
        std::unique_ptr<Expression> rhs;
        BinaryOperation(const Token& op, std::unique_ptr<Expression> left, std::unique_ptr<Expression> right)
            : op(op), lhs(std::move(left)), rhs(std::move(right)) {}
    };
    class LogicalOperation : public Expression {
    public:
        Token op;
        std::unique_ptr<Expression> lhs;
        std::unique_ptr<Expression> rhs;
        LogicalOperation(const Token& op, std::unique_ptr<Expression> left, std::unique_ptr<Expression> right)
            : op(op), lhs(std::move(left)), rhs(std::move(right)) {}
    };
    class PostfixOperation : public Expression {
    public:
        Token op;
        std::unique_ptr<Expression> operand;
        PostfixOperation(const Token& op, std::unique_ptr<Expression> operand) : op(op), operand(std::move(operand)) {}
    };
    class CallOperation : public Expression {
    public:
        std::unique_ptr<Expression> callee;
        std::vector<std::unique_ptr<Expression>> arguments;
        CallOperation(std::unique_ptr<Expression> callee, std::vector<std::unique_ptr<Expression>> args) :
            callee(std::move(callee)), arguments(std::move(args)) {}
    };
}
