#pragma once
#include "type.h"
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
    };
}