#pragma once
#include "ast_node.h"
#include "expression.h"
#include "statement.h"
#include <memory>
namespace Yoyo
{
    //No reflection so we have to this cursed nonsense
    struct ExpressionTreeCloner
    {
        std::unique_ptr<Expression> operator()(IntegerLiteral*);
        std::unique_ptr<Expression> operator()(BooleanLiteral*);
        std::unique_ptr<Expression> operator()(TupleLiteral*);
        std::unique_ptr<Expression> operator()(ArrayLiteral*);
        std::unique_ptr<Expression> operator()(RealLiteral*);
        std::unique_ptr<Expression> operator()(StringLiteral*);
        std::unique_ptr<Expression> operator()(NameExpression*);
        std::unique_ptr<Expression> operator()(GenericNameExpression*);
        std::unique_ptr<Expression> operator()(PrefixOperation*);
        std::unique_ptr<Expression> operator()(BinaryOperation*);
        std::unique_ptr<Expression> operator()(GroupingExpression*);
        std::unique_ptr<Expression> operator()(LogicalOperation*);
        std::unique_ptr<Expression> operator()(PostfixOperation*);
        std::unique_ptr<Expression> operator()(CallOperation*);
        std::unique_ptr<Expression> operator()(SubscriptOperation*);
        std::unique_ptr<Expression> operator()(LambdaExpression*);
        std::unique_ptr<Expression> operator()(ScopeOperation*);
        std::unique_ptr<Expression> operator()(ObjectLiteral*);
        std::unique_ptr<Expression> operator()(NullLiteral*);
        std::unique_ptr<Expression> operator()(AsExpression*);
        std::unique_ptr<Expression> operator()(CharLiteral*);
        std::unique_ptr<Expression> operator()(GCNewExpression*);
        std::unique_ptr<Expression> operator()(MacroInvocation*);
        std::unique_ptr<Expression> operator()(SpawnExpression*);
        std::unique_ptr<Expression> operator()(TryExpression*);
        std::unique_ptr<Expression> operator()(IfExpression*);
        std::unique_ptr<Expression> operator()(BlockExpression*);
        std::unique_ptr<Expression> operator()(ConditionalExtraction*);

        static std::unique_ptr<Expression> copy_expr(Expression*, ASTNode*);
        static std::unique_ptr<Expression> copy_expr(std::unique_ptr<Expression>&, ASTNode*);
    private:
        ExpressionTreeCloner() = default;
    };
    struct StatementTreeCloner
    {
        std::unique_ptr<Statement> operator()(FunctionDeclaration*);
        std::unique_ptr<Statement> operator()(ClassDeclaration*);
        std::unique_ptr<Statement> operator()(VariableDeclaration*);
        std::unique_ptr<Statement> operator()(WhileStatement*);
        std::unique_ptr<Statement> operator()(ForStatement*);
        std::unique_ptr<Statement> operator()(ReturnStatement*);
        std::unique_ptr<Statement> operator()(ExpressionStatement*);
        std::unique_ptr<Statement> operator()(EnumDeclaration*);
        std::unique_ptr<Statement> operator()(ModuleImport*);
        std::unique_ptr<Statement> operator()(WithStatement*);
        std::unique_ptr<Statement> operator()(OperatorOverload*);
        std::unique_ptr<Statement> operator()(GenericFunctionDeclaration*);
        std::unique_ptr<Statement> operator()(GenericAliasDeclaration*);
        std::unique_ptr<Statement> operator()(AliasDeclaration*);
        std::unique_ptr<Statement> operator()(InterfaceDeclaration*);
        std::unique_ptr<Statement> operator()(GenericInterfaceDeclaration*);
        std::unique_ptr<Statement> operator()(GenericClassDeclaration*);
        std::unique_ptr<Statement> operator()(BreakStatement*);
        std::unique_ptr<Statement> operator()(ContinueStatement*);
        std::unique_ptr<Statement> operator()(CImportDeclaration*);
        std::unique_ptr<Statement> operator()(ConstantDeclaration*);
        std::unique_ptr<Statement> operator()(UnionDeclaration* decl);
        std::unique_ptr<Statement> operator()(MacroDeclaration* decl);
        std::unique_ptr<Statement> operator()(UsingStatement* decl);

        static std::unique_ptr<Statement> copy_stat(Statement*, ASTNode* parent);
        static std::unique_ptr<Statement> copy_stat(std::unique_ptr<Statement>&, ASTNode* parent);
        template<typename T>
        static std::unique_ptr<Statement> copy_stat_specific(T* val, ASTNode* parent)
        {
            auto cs = StatementTreeCloner{}(val);
            cs->beg = val->beg; cs->end = val->end;
            return cs;
        }

        static std::unique_ptr<Expression> copy_expr(Expression*, ASTNode* parent);
        static std::unique_ptr<Expression> copy_expr(std::unique_ptr<Expression>&, ASTNode* parent);
    private:
        StatementTreeCloner() = default;
    };
}
