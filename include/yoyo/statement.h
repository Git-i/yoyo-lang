#pragma once
#include <func_sig.h>

#include "type.h"
#include "expression.h"
namespace Yoyo
{
    class Statement
    {
    public:
        virtual ~Statement() = default;
    };
    class ExpressionStatement : public Statement
    {
    public:
        std::unique_ptr<Expression> expression;
        explicit ExpressionStatement(std::unique_ptr<Expression> exp) : expression(std::move(exp)) {};
    };
    class VariableDeclaration : public Statement
    {
    public:
        std::optional<Type> type;
        Token identifier;
        std::unique_ptr<Expression> initializer;
        VariableDeclaration(Token iden, std::optional<Type> t, std::unique_ptr<Expression> init)
            : type(std::move(t)), identifier(iden), initializer(std::move(init)) {}
    };
    class FunctionDeclaration : public Statement
    {
    public:
        FunctionSignature signature;
        Token identifier;
        std::unique_ptr<Statement> body;
        FunctionDeclaration(Token ident, FunctionSignature sig, std::unique_ptr<Statement> body)
            : signature(std::move(sig)), identifier(ident), body(std::move(body)) {}
    };
    class ReturnStatement : public Statement
    {
    public:
        std::unique_ptr<Expression> expression;
        explicit ReturnStatement(std::unique_ptr<Expression> exp) : expression(std::move(exp)) {}
    };
    class IfStatement : public Statement
    {
    public:
        std::unique_ptr<Expression> condition;
        std::unique_ptr<Statement> then_stat;
        std::unique_ptr<Statement> else_stat;
        IfStatement(std::unique_ptr<Expression> cond, std::unique_ptr<Statement> then_, std::unique_ptr<Statement> else_)
            : condition(std::move(cond)), then_stat(std::move(then_)), else_stat(std::move(else_)) {}
    };
}