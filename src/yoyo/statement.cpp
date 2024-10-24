#include "statement.h"
namespace Yoyo
{
    StatementVaraint ExpressionStatement::toVariant()
    {
        return this;
    }
    StatementVaraint VariableDeclaration::toVariant()
    {
        return this;
    }
    StatementVaraint FunctionDeclaration::toVariant()
    {
        return this;
    }
    StatementVaraint ReturnStatement::toVariant()
    {
        return this;
    }
    StatementVaraint IfStatement::toVariant()
    {
        return this;
    }
    StatementVaraint WhileStatement::toVariant()
    {
        return this;
    }
    StatementVaraint BlockStatement::toVariant()
    {
        return this;
    }
    StatementVaraint ClassDeclaration::toVariant()
    {
        return this;
    }
    StatementVaraint ForStatement::toVariant()
    {
        return this;
    }






}