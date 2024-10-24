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
        std::vector<std::unordered_map<std::string, llvm::AllocaInst*>> variables;
        std::string block_hash;
        void operator()(FunctionDeclaration*);
        void operator()(ClassDeclaration*);
        void operator()(VariableDeclaration*);
        void operator()(IfStatement*);
        void operator()(WhileStatement*);
        void operator()(ForStatement*);

        void error();
        explicit IRGenerator(llvm::LLVMContext& ctx) : context(ctx) {}
        void HandleFunctionDeclaration(FunctionDeclaration* decl);
        Module GenerateIR(std::string_view name, std::vector<std::unique_ptr<Statement>> statements);
    };
    struct TopLevelVisitor
    {
        Module* mod;
        //return true on success
        bool operator()(FunctionDeclaration*);
        bool operator()(ClassDeclaration*);
        bool operator()(VariableDeclaration*);
        bool operator()(IfStatement*);
        bool operator()(WhileStatement*);
        bool operator()(ForStatement*);
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
    class ExpressionEvaluator
    {
        IRGenerator* irgen;
    public:
        explicit ExpressionEvaluator(IRGenerator* gen) : irgen(gen) {}
        llvm::Value* operator()(IntegerLiteral*);
        llvm::Value* operator()(BooleanLiteral*);
        llvm::Value* operator()(TupleLiteral*);
        llvm::Value* operator()(ArrayLiteral*);
        llvm::Value* operator()(RealLiteral*);
        llvm::Value* operator()(StringLiteral*);
        llvm::Value* operator()(NameExpression*);
        llvm::Value* operator()(PrefixOperation*);
        llvm::Value* operator()(BinaryOperation*);
        llvm::Value* operator()(GroupingExpression*);
        llvm::Value* operator()(LogicalOperation*);
        llvm::Value* operator()(PostfixOperation*);
        llvm::Value* operator()(CallOperation*);
        llvm::Value* operator()(SubscriptOperation*);
    };
}