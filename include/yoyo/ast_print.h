#include "expression.h"
#include "statement.h"
namespace Yoyo {
	// debug print the AST
	struct ASTPrinter {
        std::ostream& stream;
        std::string prefix;
        void operator()(FunctionDeclaration*) {}
        void operator()(ClassDeclaration*) {}
        void operator()(VariableDeclaration*);
        void operator()(IfStatement*);
        void operator()(WhileStatement*);
        void operator()(ForStatement*);
        void operator()(BlockStatement*);
        void operator()(ReturnStatement*);
        void operator()(ExpressionStatement*);
        void operator()(EnumDeclaration*) {}
        void operator()(UsingStatement*) {}
        void operator()(ModuleImport*) {}
        void operator()(ConditionalExtraction*);
        void operator()(WithStatement*);
        void operator()(OperatorOverload*) {}
        void operator()(GenericFunctionDeclaration*) {}
        void operator()(AliasDeclaration*) {}
        void operator()(GenericAliasDeclaration*) {}
        void operator()(GenericClassDeclaration*) {}
        void operator()(InterfaceDeclaration*){}
        void operator()(BreakStatement*);
        void operator()(ContinueStatement*);
        void operator()(ConstantDeclaration*) {}
        void operator()(CImportDeclaration*){}
        void operator()(UnionDeclaration*){}
        void operator()(MacroDeclaration*){}

        void operator()(IntegerLiteral*);
        void operator()(BooleanLiteral*);
        void operator()(TupleLiteral*);
        void operator()(ArrayLiteral*) {}
        void operator()(RealLiteral*) {}
        void operator()(StringLiteral*) {}
        void operator()(NameExpression*);
        void operator()(GenericNameExpression*) {}
        void operator()(PrefixOperation*){}
        void operator()(BinaryOperation*) {}
        void operator()(GroupingExpression*) {}
        void operator()(LogicalOperation*) {}
        void operator()(PostfixOperation*) {}
        void operator()(CallOperation*) {}
        void operator()(SubscriptOperation*) {}
        void operator()(LambdaExpression*) {}
        void operator()(ScopeOperation*) {}
        void operator()(ObjectLiteral*) {}
        void operator()(NullLiteral*) {}
        void operator()(AsExpression*) {}
        void operator()(CharLiteral*) {}
        void operator()(GCNewExpression*){}
        void operator()(MacroInvocation*){}
        void operator()(SpawnExpression*){}
	};
}