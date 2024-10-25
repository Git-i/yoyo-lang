#pragma once
#include <fn_type.h>
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
        std::vector<std::unordered_map<std::string, std::pair<llvm::AllocaInst*, VariableDeclaration*>>> variables;
        std::vector<std::unordered_map<std::string, std::pair<llvm::StructType*, ClassDeclaration*>>> types;
        std::string block_hash;

        llvm::Type* ToLLVMType(const Type& type, bool is_ref);
        llvm::FunctionType* ToLLVMSignature(const FunctionSignature& sig);
        llvm::AllocaInst* Alloca(std::string_view name, llvm::Type* type);
        bool isShadowing(const std::string&) const;
        void operator()(FunctionDeclaration*);
        void operator()(ClassDeclaration*);
        void operator()(VariableDeclaration*);
        void operator()(IfStatement*);
        void operator()(WhileStatement*);
        void operator()(ForStatement*);
        void operator()(BlockStatement*);
        void operator()(ReturnStatement*);
        void operator()(ExpressionStatement*);

        void error();
        void pushScope() {variables.emplace_back(); types.emplace_back();}
        void popScope() {variables.pop_back(); types.pop_back();}
        explicit IRGenerator(llvm::LLVMContext& ctx) : context(ctx) {}
        llvm::StructType* hanldeClassDeclaration(ClassDeclaration* decl, bool is_anon);
        Module GenerateIR(std::string_view name, std::vector<std::unique_ptr<Statement>> statements);
    };
    struct TopLevelVisitor
    {
        IRGenerator* irgen;
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
    public:
        explicit ExpressionTypeChecker(IRGenerator* gen) : irgen(gen) {}
        /// We return FunctionType because it's a subclass of Type and some
        /// expressions ( @c NameExpression and @c BinaryExpression ) can be a function
        std::optional<FunctionType> operator()(IntegerLiteral*);
        std::optional<FunctionType> operator()(BooleanLiteral*);
        std::optional<FunctionType> operator()(TupleLiteral*);
        std::optional<FunctionType> operator()(ArrayLiteral*);
        std::optional<FunctionType> operator()(RealLiteral*);
        std::optional<FunctionType> operator()(StringLiteral*);
        std::optional<FunctionType> operator()(NameExpression*);
        std::optional<FunctionType> operator()(PrefixOperation*);
        std::optional<FunctionType> operator()(BinaryOperation*);
        std::optional<FunctionType> operator()(GroupingExpression*);
        std::optional<FunctionType> operator()(LogicalOperation*);
        std::optional<FunctionType> operator()(PostfixOperation*);
        std::optional<FunctionType> operator()(CallOperation*);
        std::optional<FunctionType> operator()(SubscriptOperation*);
    };
    class ExpressionEvaluator
    {
        IRGenerator* irgen;
    public:
        enum ComparisonPredicate
        {
            EQ, GT, LT, EQ_GT, EQ_LT, NE
        };
        explicit ExpressionEvaluator(IRGenerator* gen) : irgen(gen) {}
        llvm::Value* doAssign(llvm::Value* lhs, llvm::Value* rhs, const Type& left_type, const Type& right_type);
        llvm::Value* doDot(Expression* lhs, Expression* rhs, const Type& left_type, const Type& right_type);
        llvm::Value* doAddition(llvm::Value*,llvm::Value*,const Type&,const Type&) const;
        llvm::Value* doMinus(llvm::Value*, llvm::Value*, const Type&, const Type&) const;
        llvm::Value* doMult(llvm::Value*, llvm::Value*, const Type&, const Type&) const;
        llvm::Value* doDiv(llvm::Value*, llvm::Value*, const Type&, const Type&) const;
        llvm::Value* doRem(llvm::Value* lhs, llvm::Value* rhs, const Type& left_type, const Type& right_type) const;
        llvm::Value* doCmp(ComparisonPredicate p, llvm::Value* lhs, llvm::Value* rhs, const Type& left_type,
                           const Type& right_type) const;
        struct LValueEvaluator
        {
            IRGenerator* irgen;
            llvm::Value* operator()(NameExpression*);
            llvm::Value* operator()(BinaryOperation*);
            llvm::Value* operator()(Expression*);
        };
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