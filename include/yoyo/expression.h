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
    struct OverloadDetailsBinary;
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
    class GCNewExpression;
    class MacroInvocation;
    class SpawnExpression;
    class TryExpression;
    class BlockExpression;
    class IfExpression;
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
        GCNewExpression*,
        AsExpression*,
        MacroInvocation*,
        SpawnExpression*,
        TryExpression*,
        BlockExpression*,
        IfExpression*>;
    enum class Ownership { Owning = 0, NonOwning, NonOwningMut };
    class YOYO_API Expression : public ASTNode {
    public:
        ~Expression() override = default;
        virtual ExpressionVariant toVariant() = 0;
        Type evaluated_type;
        static auto attachSLAndParent(std::unique_ptr<Expression> self, SourceLocation bg, SourceLocation end,ASTNode* parent = nullptr)
            -> std::unique_ptr<Expression>
        {
            self->beg = bg;
            self->end = end;
            self->parent = parent;
            return self;
        }
    };
    class YOYO_API IntegerLiteral : public Expression {
    public:
        std::string text;
        explicit IntegerLiteral(std::string tk) : text(std::move(tk)) {}
        explicit IntegerLiteral(const std::string_view tk) : text(tk) {}
        ExpressionVariant toVariant() override;
    };
    class YOYO_API BooleanLiteral : public Expression {
    public:
        Token token;
        explicit BooleanLiteral(const Token& tk) : token(tk) {}
        ExpressionVariant toVariant() override;

    };
    class YOYO_API NullLiteral : public Expression { ExpressionVariant toVariant() override {return this;}};
    class YOYO_API TupleLiteral : public Expression {
    public:
        std::vector<std::unique_ptr<Expression>> elements;
        TupleLiteral(TupleLiteral&&) noexcept = default;
        explicit TupleLiteral(std::vector<std::unique_ptr<Expression>> elems) : elements(std::move(elems)) {}
        ExpressionVariant toVariant() override;
    };
    class YOYO_API ArrayLiteral : public Expression {
    public:
        std::variant<
            std::vector<std::unique_ptr<Expression>>,
            std::pair<std::unique_ptr<Expression>, std::unique_ptr<Expression>>
        >
            elements;
        ArrayLiteral(ArrayLiteral&&) noexcept = default;
        explicit ArrayLiteral(std::vector<std::unique_ptr<Expression>> elems) : elements(std::move(elems)) {}
        explicit ArrayLiteral(std::pair<std::unique_ptr<Expression>, std::unique_ptr<Expression>> elems) 
            : elements(std::move(elems)) {}
        ExpressionVariant toVariant() override;
    };
    class YOYO_API RealLiteral : public Expression {
    public:
        Token token;
        explicit RealLiteral(const Token& tk) : token(tk) {}
        ExpressionVariant toVariant() override;
    };
    class YOYO_API StringLiteral : public Expression {
    public:
        StringLiteral(StringLiteral&&) noexcept = default;
        std::vector<std::variant<std::string, std::unique_ptr<Expression>>> literal;
        explicit StringLiteral(decltype(literal) value) : literal(std::move(value)) {}
        ExpressionVariant toVariant() override;
    };
    class YOYO_API NameExpression : public Expression {
    public:
        std::string text;
        explicit NameExpression(std::string tk) : text(std::move(tk)) {}
        ExpressionVariant toVariant() override;
    };
    class YOYO_API GenericNameExpression: public NameExpression
    {
    public:
        std::vector<Type> arguments;
        GenericNameExpression(std::string name, std::vector<Type> arguments)
            : NameExpression(std::move(name)), arguments(std::move(arguments)) {}
        ExpressionVariant toVariant() override;
    };
    class YOYO_API PrefixOperation : public Expression {
    public:
        Token op;
        std::unique_ptr<Expression> operand;
        PrefixOperation(const Token& op, std::unique_ptr<Expression> operand) : op(op), operand(std::move(operand)) {}
        ExpressionVariant toVariant() override;
    };
    class YOYO_API GroupingExpression : public Expression {
    public:
        std::unique_ptr<Expression> expr;
        explicit GroupingExpression(std::unique_ptr<Expression> expr) : expr(std::move(expr)) {}
        ExpressionVariant toVariant() override;
    };
    class YOYO_API BinaryOperation : public Expression {
    public:
        Token op;
        std::unique_ptr<Expression> lhs;
        std::unique_ptr<Expression> rhs;
        // populated by the type checker
        OverloadDetailsBinary* selected = nullptr;
        std::vector<Type> subtypes;
        ModuleBase* module;
        BinaryOperation(const Token& op, std::unique_ptr<Expression> left, std::unique_ptr<Expression> right)
            : op(op), lhs(std::move(left)), rhs(std::move(right)) {}
        ExpressionVariant toVariant() override;
    };
    class YOYO_API LogicalOperation : public Expression {
    public:
        Token op;
        std::unique_ptr<Expression> lhs;
        std::unique_ptr<Expression> rhs;
        LogicalOperation(const Token& op, std::unique_ptr<Expression> left, std::unique_ptr<Expression> right)
            : op(op), lhs(std::move(left)), rhs(std::move(right)) {}
        ExpressionVariant toVariant() override;
    };
    class YOYO_API PostfixOperation : public Expression {
    public:
        Token op;
        std::unique_ptr<Expression> operand;
        PostfixOperation(const Token& op, std::unique_ptr<Expression> operand) : op(op), operand(std::move(operand)) {}
        ExpressionVariant toVariant() override;
    };
    class YOYO_API CallOperation : public Expression {
    public:
        std::unique_ptr<Expression> callee;
        std::vector<std::unique_ptr<Expression>> arguments;
        CallOperation(std::unique_ptr<Expression> callee, std::vector<std::unique_ptr<Expression>> args) :
            callee(std::move(callee)), arguments(std::move(args)) {}
        ExpressionVariant toVariant() override;
    };
    class YOYO_API SubscriptOperation : public Expression {
    public:
        std::unique_ptr<Expression> object;
        std::unique_ptr<Expression> index;
        // populated by the type checker
        OverloadDetailsBinary* selected = nullptr;
        std::vector<Type> subtypes;
        ModuleBase* module;
        SubscriptOperation(std::unique_ptr<Expression> obj, std::unique_ptr<Expression> idx)
            : object(std::move(obj)), index(std::move(idx)) {}
        ExpressionVariant toVariant() override;
    };
    class YOYO_API LambdaExpression : public Expression {
    public:
        struct Capture {
            Ownership cp_type; std::string name;
        };
        std::vector<Capture> captures;
        FunctionSignature sig;
        std::unique_ptr<Statement> body;
        std::string hash;
        LambdaExpression(std::vector<Capture> captures, FunctionSignature sig, std::unique_ptr<Statement> body);
        ExpressionVariant toVariant() override;
    };
    class YOYO_API ObjectLiteral : public Expression
    {
    public:
        Type t;
        std::unordered_map<std::string, std::unique_ptr<Expression>> values;
        ObjectLiteral(ObjectLiteral&&) noexcept = default;
        ObjectLiteral(Type t, std::unordered_map<std::string, std::unique_ptr<Expression>> values)
            : t(std::move(t)), values(std::move(values)) {}
        ExpressionVariant toVariant() override;
    };
    class YOYO_API ScopeOperation : public Expression
    {
    public:
        Type type;
        explicit ScopeOperation(Type tp)
            : type(std::move(tp)){}
        ExpressionVariant toVariant() override;

    };
    /// Used for primitive casts; ex: @code i32_value as i64 @endcode
    class YOYO_API AsExpression : public Expression
    {
    public:
        std::unique_ptr<Expression> expr;
        Type dest;
        AsExpression(std::unique_ptr<Expression> expr, Type dest) : expr(std::move(expr)), dest(std::move(dest)) {}
        ExpressionVariant toVariant() override;
    };
    class YOYO_API CharLiteral : public Expression
    {
    public:
        uint32_t value;
        ExpressionVariant toVariant() override;
    };
    class YOYO_API GCNewExpression : public Expression
    {
    public:
        std::unique_ptr<Expression> target_expression;
        ExpressionVariant toVariant() override;
        GCNewExpression(std::unique_ptr<Expression> expr)
            : target_expression(std::move(expr)){}
    };
    class YOYO_API MacroInvocation: public Expression
    {
    public:
        std::unique_ptr<Expression> macro_name;

        std::variant<
            std::unique_ptr<Expression>,
            std::vector<std::pair<Token, SourceLocation>>
        > left;
        std::unique_ptr<Expression> right;
        
        std::unique_ptr<Expression> result;
        MacroInvocation(std::unique_ptr<Expression> name, std::unique_ptr<Expression> left)
            : macro_name(std::move(name)), left(std::move(left)) {}
        MacroInvocation(std::unique_ptr<Expression> name, std::unique_ptr<Expression> left, std::unique_ptr<Expression> right)
            : macro_name(std::move(name)), left(std::move(left)), right(std::move(right)) {}

        ExpressionVariant toVariant() override;
    };
    class YOYO_API SpawnExpression : public Expression
    {
    public:
        std::unique_ptr<Expression> call_expr;
        SpawnExpression(std::unique_ptr<Expression> call_expr)
            : call_expr(std::move(call_expr)) {}
        ExpressionVariant toVariant() override;
    };
    // <expr>.try
    class YOYO_API TryExpression : public Expression {
    public:
        std::unique_ptr<Expression> expression;
        TryExpression(std::unique_ptr<Expression> e) : expression(std::move(e)) {}
        ExpressionVariant toVariant() override;
    };
    // Migrated from statement
    // { <statement>...; <expr> }
    class YOYO_API BlockExpression : public Expression {
    public:
        std::vector<std::unique_ptr<Statement>> statements;
        std::unique_ptr<Expression> expr;
        BlockExpression(std::vector<std::unique_ptr<Statement>> stats, std::unique_ptr<Expression> ex)
            : statements(std::move(stats)), expr(std::move(ex)) {}
        ExpressionVariant toVariant() override;
    };
    class YOYO_API IfExpression : public Expression
    {
    public:
        std::unique_ptr<Expression> condition;
        std::unique_ptr<Expression> then_expr;
        std::unique_ptr<Expression> else_expr;
        // true if the "then" block does not need to return the same as else
        // because control goes elsewhere, we set it to true becaue unreachable nodes
        // are not visited by the CFG generator
        bool then_transfers_control = true;
        bool else_transfers_control = true;
        IfExpression(std::unique_ptr<Expression> cond, std::unique_ptr<Expression> then_, std::unique_ptr<Expression> else_)
            : condition(std::move(cond)), then_expr(std::move(then_)), else_expr(std::move(else_)) {
        }
        ExpressionVariant toVariant() override;
    };
}
