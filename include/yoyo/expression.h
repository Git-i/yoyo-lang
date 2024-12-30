#pragma once
#include <array>
#include <ast_node.h>
#include <func_sig.h>

#include "token.h"
#include <memory>
#include <unordered_map>
#include <utility>
#include <vector>
#include <variant>

namespace Yoyo
{
    class Statement;
}

namespace Yoyo
{
    class NullLiteral;
    class ObjectLiteral;
    class LogicalOperation;
    class StringLiteral;
    class RealLiteral;
    class ArrayLiteral;
    class TupleLiteral;
    class BooleanLiteral;
    class IntegerLiteral;
    class LambdaExpression;
    class ScopeOperation;
    class CharLiteral;

    class SubscriptOperation;
    class CallOperation;
    class PostfixOperation;
    class PrefixOperation;
    class BinaryOperation;
    class GroupingExpression;
    class NameExpression;
    class GenericNameExpression;
    class AsExpression;
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
        SubscriptOperation*,
        LambdaExpression*,
        ScopeOperation*,
        ObjectLiteral*,
        NullLiteral*,
        CharLiteral*,
        GenericNameExpression*,
        AsExpression*>;
    class Expression : public ASTNode {
    public:
        ~Expression() override = default;
        virtual ExpressionVariant toVariant() = 0;
        static auto attachSLAndParent(std::unique_ptr<Expression> self, SourceLocation bg, SourceLocation end,ASTNode* parent = nullptr)
            -> std::unique_ptr<Expression>
        {
            self->beg = bg;
            self->end = end;
            self->parent = parent;
            return self;
        }
    };
    class IntegerLiteral : public Expression {
    public:
        std::string text;
        explicit IntegerLiteral(std::string tk) : text(std::move(tk)) {}
        explicit IntegerLiteral(const std::string_view tk) : text(tk) {}
        ExpressionVariant toVariant() override;
    };
    class BooleanLiteral : public Expression {
    public:
        Token token;
        explicit BooleanLiteral(const Token& tk) : token(tk) {}
        ExpressionVariant toVariant() override;

    };
    class NullLiteral : public Expression { ExpressionVariant toVariant() override {return this;}};
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
        std::vector<std::variant<std::string, std::unique_ptr<Expression>>> literal;
        explicit StringLiteral(decltype(literal) value) : literal(std::move(value)) {}
        ExpressionVariant toVariant() override;
    };
    class NameExpression : public Expression {
    public:
        std::string text;
        explicit NameExpression(std::string tk) : text(std::move(tk)) {}
        ExpressionVariant toVariant() override;
    };
    class GenericNameExpression: public NameExpression
    {
    public:
        std::vector<Type> arguments;
        GenericNameExpression(std::string name, std::vector<Type> arguments)
            : NameExpression(std::move(name)), arguments(std::move(arguments)) {}
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
    class LambdaExpression : public Expression {
    public:
        std::vector<std::string> captures;
        FunctionSignature sig;
        std::unique_ptr<Statement> body;
        std::string hash;
        LambdaExpression(std::vector<std::string> captures, FunctionSignature sig, std::unique_ptr<Statement> body);
        ExpressionVariant toVariant() override;
    };
    class ObjectLiteral : public Expression
    {
    public:
        Type t;
        std::unordered_map<std::string, std::unique_ptr<Expression>> values;
        ObjectLiteral(Type t, std::unordered_map<std::string, std::unique_ptr<Expression>> values)
            : t(std::move(t)), values(std::move(values)) {}
        ExpressionVariant toVariant() override;
    };
    class ScopeOperation : public Expression
    {
    public:
        Type type;
        explicit ScopeOperation(Type tp)
            : type(std::move(tp)){}
        ExpressionVariant toVariant() override;

    };
    /// Used for primitive casts; ex: @code i32_value as i64 @endcode
    class AsExpression : public Expression
    {
    public:
        std::unique_ptr<Expression> expr;
        Type dest;
        AsExpression(std::unique_ptr<Expression> expr, Type dest) : expr(std::move(expr)), dest(std::move(dest)) {}
        ExpressionVariant toVariant() override;
    };
    class CharLiteral : public Expression
    {
    public:
        uint32_t value;
        ExpressionVariant toVariant() override;
    };
}
