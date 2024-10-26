#pragma once
#include "token.h"
#include <memory>
#include <vector>
#include <variant>
namespace Yoyo
{
    class SubscriptOperation;
    class CallOperation;
    class PostfixOperation;
    class LogicalOperation;
    class PrefixOperation;
    class BinaryOperation;
    class GroupingExpression;
    class NameExpression;
    class StringLiteral;
    class RealLiteral;
    class ArrayLiteral;
    class TupleLiteral;
    class BooleanLiteral;
    class IntegerLiteral;
    using ExpressionVariant = std::variant<
        IntegerLiteral*,
        BooleanLiteral*,
        TupleLiteral*,
        ArrayLiteral*,
        RealLiteral*,
        StringLiteral*,
        NameExpression*,
        PrefixOperation*,
        BinaryOperation*,
        GroupingExpression*,
        LogicalOperation*,
        PostfixOperation*,
        CallOperation*,
        SubscriptOperation*>;
    class Expression {
    public:
        virtual ~Expression() = default;
        virtual ExpressionVariant toVariant() = 0;
    };
    class IntegerLiteral : public Expression {
    public:
        Token token;
        explicit IntegerLiteral(const Token& tk) : token(tk) {}
        ExpressionVariant toVariant() override;
    };
    class BooleanLiteral : public Expression {
    public:
        Token token;
        explicit BooleanLiteral(const Token& tk) : token(tk) {}
        ExpressionVariant toVariant() override;

    };
    class TupleLiteral : public Expression {
    public:
        std::vector<std::unique_ptr<Expression>> elements;
        explicit TupleLiteral(std::vector<std::unique_ptr<Expression>> elems) : elements(std::move(elems)) {}
        ExpressionVariant toVariant() override;
    };
    class ArrayLiteral : public Expression {
    public:
        std::vector<std::unique_ptr<Expression>> elements;
        explicit ArrayLiteral(std::vector<std::unique_ptr<Expression>> elems) : elements(std::move(elems)) {}
        ExpressionVariant toVariant() override;
    };
    class RealLiteral : public Expression {
    public:
        Token token;
        explicit RealLiteral(const Token& tk) : token(tk) {}
        ExpressionVariant toVariant() override;
    };
    class StringLiteral : public Expression {
    public:
        Token token;
        explicit StringLiteral(const Token& tk) : token(tk) {}
        ExpressionVariant toVariant() override;
    };
    class NameExpression : public Expression {
    public:
        Token token;
        explicit NameExpression(const Token& tk) : token(tk) {}
        ExpressionVariant toVariant() override;
    };
    class PrefixOperation : public Expression {
    public:
        Token op;
        std::unique_ptr<Expression> operand;
        PrefixOperation(const Token& op, std::unique_ptr<Expression> operand) : op(op), operand(std::move(operand)) {}
        ExpressionVariant toVariant() override;
    };
    class GroupingExpression : public Expression {
    public:
        std::unique_ptr<Expression> expr;
        explicit GroupingExpression(std::unique_ptr<Expression> expr) : expr(std::move(expr)) {}
        ExpressionVariant toVariant() override;
    };
    class BinaryOperation : public Expression {
    public:
        Token op;
        std::unique_ptr<Expression> lhs;
        std::unique_ptr<Expression> rhs;
        BinaryOperation(const Token& op, std::unique_ptr<Expression> left, std::unique_ptr<Expression> right)
            : op(op), lhs(std::move(left)), rhs(std::move(right)) {}
        ExpressionVariant toVariant() override;
    };
    class LogicalOperation : public Expression {
    public:
        Token op;
        std::unique_ptr<Expression> lhs;
        std::unique_ptr<Expression> rhs;
        LogicalOperation(const Token& op, std::unique_ptr<Expression> left, std::unique_ptr<Expression> right)
            : op(op), lhs(std::move(left)), rhs(std::move(right)) {}
        ExpressionVariant toVariant() override;
    };
    class PostfixOperation : public Expression {
    public:
        Token op;
        std::unique_ptr<Expression> operand;
        PostfixOperation(const Token& op, std::unique_ptr<Expression> operand) : op(op), operand(std::move(operand)) {}
        ExpressionVariant toVariant() override;
    };
    class CallOperation : public Expression {
    public:
        std::unique_ptr<Expression> callee;
        std::vector<std::unique_ptr<Expression>> arguments;
        CallOperation(std::unique_ptr<Expression> callee, std::vector<std::unique_ptr<Expression>> args) :
            callee(std::move(callee)), arguments(std::move(args)) {}
        ExpressionVariant toVariant() override;
    };
    class SubscriptOperation : public Expression {
    public:
        std::unique_ptr<Expression> object;
        std::unique_ptr<Expression> index;
        SubscriptOperation(std::unique_ptr<Expression> obj, std::unique_ptr<Expression> idx)
            : object(std::move(obj)), index(std::move(idx)) {}
        ExpressionVariant toVariant() override;
    };
}
