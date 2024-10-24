#pragma once
#include <statement.h>

#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "module.h"
namespace Yoyo
{
    class IRGenerator
    {
    public:
        llvm::LLVMContext& context;
        Module* module;
        llvm::Module* code;
        llvm::IRBuilder<>* builder;

        std::string block_hash;
        void operator()(FunctionDeclaration*);
        void operator()(ClassDeclaration*);
        void operator()(VariableDeclaration*);
        void operator()(IfStatement*);
        void operator()(WhileStatement*);
        void operator()(ForStatement*);


        explicit IRGenerator(llvm::LLVMContext& ctx) : context(ctx) {}
        void HandleFunctionDeclaration(FunctionDeclaration* decl);
        Module GenerateIR(std::string_view name, std::vector<std::unique_ptr<Statement>> statements);
    };
    struct TopLevelVisitor
    {
        IRGenerator* irgen;
        void operator()(FunctionDeclaration*);
        void operator()(ClassDeclaration*);
        void operator()(VariableDeclaration*);
        void operator()(IfStatement*);
        void operator()(WhileStatement*);
        void operator()(ForStatement*);
    };
    class ExpressionTypeChecker
    {
        IRGenerator* irgen;
        constexpr std::unordered_map<std::string, std::string> operators = {

        };
    public:
        explicit ExpressionTypeChecker(IRGenerator* gen) : irgen(gen) {}
        std::optional<Type> operator()(IntegerLiteral*);
        std::optional<Type> operator()(BooleanLiteral*);
        std::optional<Type> operator()(TupleLiteral*);
        std::optional<Type> operator()(ArrayLiteral*);
        std::optional<Type> operator()(RealLiteral*);
        std::optional<Type> operator()(StringLiteral*);
        std::optional<Type> operator()(NameExpression*);
        std::optional<Type> operator()(PrefixOperation*);
        std::optional<Type> operator()(BinaryOperation*);
        std::optional<Type> operator()(GroupingExpression*);
        std::optional<Type> operator()(LogicalOperation*);
        std::optional<Type> operator()(PostfixOperation*);
        std::optional<Type> operator()(CallOperation*);
        std::optional<Type> operator()(SubscriptOperation*);
    };
}