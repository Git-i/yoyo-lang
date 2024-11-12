#pragma once
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
    class LambdaExpression;
    class ScopeOperation;
    class ObjectLiteral;
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
        NullLiteral*>;
    class Expression {
    public:
        virtual ~Expression() = default;
        virtual ExpressionVariant toVariant() = 0;
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
        std::vector<std::pair<std::string, ParamType>> captures;
        FunctionSignature sig;
        std::unique_ptr<Statement> body;
        std::string hash;
        LambdaExpression(std::vector<std::pair<std::string, ParamType>> captures, FunctionSignature sig, std::unique_ptr<Statement> body);
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
        /// Lexicographically(or whatever) every scope operation is basically a type
        /// name::name::name(possible template args) is the exact same format for a type with @c ::
        /// for a valid scope operation however, all the "subtypes" must be identifier, with only the
        /// second to last or last being actual types(types can't have subtypes)
        /// - second to last for @code <other-things>::Type::member_function @endcode
        /// - second to last also applies for enum and unions
        /// - last for initialization expressions @code <other-things>::Type{...} @endcode
        ///
        /// Considerations:
        /// - For init expression we can completely eliminate the need for this type by
        /// surrendering it to the expr
        Type type;
        std::string scope;
        std::string name;
        explicit ScopeOperation(Type second_to_last, std::string scope, std::string last)
            : type(std::move(second_to_last)),
              scope(std::move(scope)),
              name(std::move(last)){}
        ExpressionVariant toVariant() override;

    };
}
