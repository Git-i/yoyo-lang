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
    };
}