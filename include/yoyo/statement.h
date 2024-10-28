#pragma once
#include <func_sig.h>
#include <variant>

#include "type.h"
#include "expression.h"
#include "class_entry.h"
#include <optional>
namespace Yoyo
{
    class ExpressionStatement;
    class VariableDeclaration;
    class FunctionDeclaration;
    class IfStatement;
    class ReturnStatement;
    class WhileStatement;
    class BlockStatement;
    class ClassDeclaration;
    class ForStatement;
    typedef std::variant<
        ForStatement*,
        ClassDeclaration*,
        BlockStatement*,
        WhileStatement*,
        IfStatement*,
        ReturnStatement*,
        FunctionDeclaration*,
        VariableDeclaration*,
        ExpressionStatement*> StatementVaraint;
    class Statement
    {
    public:
        virtual ~Statement() = default;
        virtual StatementVaraint toVariant() = 0;
    };
    class ExpressionStatement : public Statement
    {
    public:
        std::unique_ptr<Expression> expression;
        explicit ExpressionStatement(std::unique_ptr<Expression> exp) : expression(std::move(exp)) {};
        StatementVaraint toVariant() override;
    };
    class VariableDeclaration : public Statement
    {
    public:
        std::optional<Type> type;
        Token identifier;
        std::unique_ptr<Expression> initializer;
        bool is_mut;
        VariableDeclaration(Token iden, std::optional<Type> t, std::unique_ptr<Expression> init, bool mut)
            : type(std::move(t)), identifier(iden), initializer(std::move(init)), is_mut(mut) {};
        StatementVaraint toVariant() override;
    };
    class FunctionDeclaration : public Statement
    {
    public:
        FunctionSignature signature;
        Token identifier;
        std::unique_ptr<Statement> body;
        FunctionDeclaration(Token ident, FunctionSignature sig, std::unique_ptr<Statement> body)
            : signature(std::move(sig)), identifier(ident), body(std::move(body)) {}
        StatementVaraint toVariant() override;
    };
    class ReturnStatement : public Statement
    {
    public:
        std::unique_ptr<Expression> expression;
        explicit ReturnStatement(std::unique_ptr<Expression> exp) : expression(std::move(exp)) {}
        StatementVaraint toVariant() override;
    };
    class IfStatement : public Statement
    {
    public:
        std::unique_ptr<Expression> condition;
        std::unique_ptr<Statement> then_stat;
        std::unique_ptr<Statement> else_stat;
        IfStatement(std::unique_ptr<Expression> cond, std::unique_ptr<Statement> then_, std::unique_ptr<Statement> else_)
            : condition(std::move(cond)), then_stat(std::move(then_)), else_stat(std::move(else_)) {}
        StatementVaraint toVariant() override;
    };
    class WhileStatement : public Statement
    {
    public:
        std::unique_ptr<Expression> condition;
        std::unique_ptr<Statement> body;
        WhileStatement(std::unique_ptr<Expression> cond, std::unique_ptr<Statement> body)
            : condition(std::move(cond)), body(std::move(body)) {}
        StatementVaraint toVariant() override;
    };
    class BlockStatement : public Statement
    {
    public:
        std::vector<std::unique_ptr<Statement>> statements;
        explicit BlockStatement(std::vector<std::unique_ptr<Statement>> stats) : statements(std::move(stats)) {}
        StatementVaraint toVariant() override;
    };

    class ClassDeclaration : public Statement
    {
    public:
        Token identifier;
        std::vector<ClassVariable> vars;
        std::vector<ClassMethod> methods;
        ClassDeclaration(Token ident, std::vector<ClassVariable> vars, std::vector<ClassMethod> methods)
            : identifier(ident), vars(std::move(vars)), methods(std::move(methods)) {}
        StatementVaraint toVariant() override;
    };
    class ForStatement : public Statement
    {
    public:
        std::unique_ptr<Expression> iterable;
        std::vector<Token> names;
        std::unique_ptr<Statement> body;
        ForStatement(std::vector<Token> names, std::unique_ptr<Expression> expr, std::unique_ptr<Statement> body)
            : iterable(std::move(expr)), names(std::move(names)), body(std::move(body)) {}
        StatementVaraint toVariant() override;
    };

}