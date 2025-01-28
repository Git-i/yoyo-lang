#include "statement.h"
namespace Yoyo
{
    StatementVariant ExpressionStatement::toVariant()
    {
        return this;
    }
    StatementVariant ModuleImport::toVariant()
    {
        return this;
    }

    StatementVariant AliasDeclaration::toVariant()
    {
        return this;
    }

    StatementVariant GenericAliasDeclaration::toVariant()
    {
        return this;
    }

    StatementVariant VariableDeclaration::toVariant()
    {
        return this;
    }
    StatementVariant FunctionDeclaration::toVariant()
    {
        return this;
    }

    StatementVariant GenericFunctionDeclaration::toVariant()
    {
        return this;
    }

    StatementVariant OperatorOverload::toVariant()
    {
        return this;
    }

    StatementVariant ReturnStatement::toVariant()
    {
        return this;
    }
    StatementVariant IfStatement::toVariant()
    {
        return this;
    }
    StatementVariant WhileStatement::toVariant()
    {
        return this;
    }
    StatementVariant BlockStatement::toVariant()
    {
        return this;
    }
    StatementVariant ClassDeclaration::toVariant()
    {
        return this;
    }
    StatementVariant GenericClassDeclaration::toVariant()
    {
        return this;
    }
    StatementVariant ForStatement::toVariant()
    {
        return this;
    }
    StatementVariant ContinueStatement::toVariant()
    {
        return this;
    }
    StatementVariant BreakStatement::toVariant()
    {
        return this;
    }
    StatementVariant EnumDeclaration::toVariant()
    {
        return this;
    }

    StatementVariant ConditionalExtraction::toVariant()
    {
        return this;
    }
    StatementVariant WithStatement::toVariant()
    {
        return this;
    }
    StatementVariant InterfaceDeclaration::toVariant()
    {
        return this;
    }
    StatementVariant GenericInterfaceDeclaration::toVariant()
    {
        return this;
    }
    StatementVariant CImportDeclaration::toVariant()
    {
        return this;
    }
}
