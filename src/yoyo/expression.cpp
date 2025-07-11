#include "expression.h"
#include "statement.h"
namespace Yoyo
{
    ExpressionVariant IntegerLiteral::toVariant()
    {
        return this;
    }

    ExpressionVariant BooleanLiteral::toVariant()
    {
        return this;
    }

    ExpressionVariant TupleLiteral::toVariant()
    {
        return this;
    }

    ExpressionVariant ArrayLiteral::toVariant()
    {
        return this;
    }

    ExpressionVariant RealLiteral::toVariant()
    {
        return this;
    }

    ExpressionVariant StringLiteral::toVariant()
    {
        return this;
    }

    ExpressionVariant NameExpression::toVariant()
    {
        return this;
    }

    ExpressionVariant GenericNameExpression::toVariant()
    {
        return this;
    }

    ExpressionVariant PrefixOperation::toVariant()
    {
        return this;
    }

    ExpressionVariant GroupingExpression::toVariant()
    {
        return this;
    }

    ExpressionVariant BinaryOperation::toVariant()
    {
        return this;
    }

    ExpressionVariant LogicalOperation::toVariant()
    {
        return this;
    }

    ExpressionVariant PostfixOperation::toVariant()
    {
        return this;
    }

    ExpressionVariant CallOperation::toVariant()
    {
        return this;
    }

    ExpressionVariant SubscriptOperation::toVariant()
    {
        return this;
    }
    ExpressionVariant LambdaExpression::toVariant()
    {
        return this;
    }

    ExpressionVariant ObjectLiteral::toVariant()
    {
        return this;
    }

    ExpressionVariant ScopeOperation::toVariant()
    {
        return this;
    }

    ExpressionVariant AsExpression::toVariant()
    {
        return this;
    }

    ExpressionVariant CharLiteral::toVariant()
    {
        return this;
    }

    ExpressionVariant GCNewExpression::toVariant()
    {
        return this;
    }
    LambdaExpression::LambdaExpression(std::vector<Capture> captures, FunctionSignature sig, std::unique_ptr<Statement> body)
    : captures(std::move(captures)), sig(std::move(sig)), body(std::move(body)) {}


    ExpressionVariant MacroInvocation::toVariant()
    {
        return this;
    }

    ExpressionVariant SpawnExpression::toVariant()
    {
        return this;
    }

}
