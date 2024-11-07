#pragma once
#include <fn_type.h>
#include <statement.h>
#include "engine.h"
#include <utility>

#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "module.h"
namespace Yoyo
{
    class IRGenerator
    {
    public:
        llvm::Value* currentReturnAddress;
        Type this_t;
        Type return_t;
        llvm::BasicBlock* returnBlock;
        bool in_class = false;
        llvm::LLVMContext& context;
        Module* module;
        llvm::Module* code;
        std::unique_ptr<llvm::IRBuilder<>> builder;
        std::vector<std::unordered_map<std::string, std::pair<llvm::Value*, VariableDeclaration*>>> variables;
        std::vector<std::unordered_map<std::string, std::tuple<std::string, llvm::StructType*, ClassDeclaration*>>> types;
        std::unordered_map<std::string, std::pair<std::vector<std::pair<std::string, ParamType>>*, llvm::StructType*>> lambdas;
        std::unordered_map<std::string, FunctionSignature> lambdaSigs;
        std::string block_hash;

        std::tuple<std::string, llvm::StructType*, ClassDeclaration*>* findType(const std::string& name);
        llvm::Type* ToLLVMType(const Type& type, bool is_ref);
        void saturateSignature(FunctionSignature& sig, Module* md);
        llvm::FunctionType* ToLLVMSignature(const FunctionSignature& sig);
        llvm::AllocaInst* Alloca(std::string_view name, llvm::Type* type);
        llvm::Value* Malloc(std::string_view name, llvm::Value* size);
        bool isShadowing(const std::string&) const;
        void operator()(FunctionDeclaration*);
        void operator()(ClassDeclaration*);
        Type reduceLiteral(const Type& src, llvm::Value* val);
        void operator()(VariableDeclaration*);
        void operator()(IfStatement*);
        void operator()(WhileStatement*);
        void operator()(ForStatement*);
        void operator()(BlockStatement*);
        void operator()(ReturnStatement*);
        void operator()(ExpressionStatement*);
        void operator()(EnumDeclaration*){}; //TODO
        void operator()(ModuleImport*){};

        void error();
        void pushScope() {variables.emplace_back(); types.emplace_back();}
        void popScope() {variables.pop_back(); types.pop_back();}
        std::optional<Type> inferReturnType(Statement* stat);
        explicit IRGenerator(llvm::LLVMContext& ctx) : context(ctx) {}
        llvm::StructType* hanldeClassDeclaration(ClassDeclaration* decl, bool is_anon);
        void GenerateIR(std::string_view name, std::vector<std::unique_ptr<Statement>> statements, Module* md);
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
        std::optional<Type> target;
    public:
        std::optional<FunctionType> checkNameWithinClassOrModule(Module*, ClassDeclaration* type, std::string_view name);
        std::optional<FunctionType> checkNameWithinEnum(EnumDeclaration* type, std::string_view name);
        explicit ExpressionTypeChecker(IRGenerator* gen, std::optional<Type> target = std::nullopt) : irgen(gen),
            target(std::move(target)) {}
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
        std::optional<FunctionType> operator()(LambdaExpression*);
        std::optional<FunctionType> operator()(ScopeOperation*);
        std::optional<FunctionType> operator()(ObjectLiteral*);
    };
    class ExpressionEvaluator
    {
        IRGenerator* irgen;
        std::optional<Type> target;
    public:
        Type lastDeducedType;
        enum ComparisonPredicate
        {
            EQ, GT, LT, EQ_GT, EQ_LT, NE
        };
        explicit ExpressionEvaluator(IRGenerator* gen, std::optional<Type> target = std::nullopt) : irgen(gen),
            target(std::move(target)) {}
        llvm::Value* doAssign(llvm::Value* lhs, llvm::Value* rhs, const Type& left_type, const Type& right_type);
        llvm::Value* doDot(Expression* lhs, Expression* rhs, const Type& left_type, bool load_prim = true);
        llvm::Value* doAddition(llvm::Value*,llvm::Value*,const Type&,const Type&, const Type&) const;
        llvm::Value* doMinus(llvm::Value*, llvm::Value*, const Type&, const Type&, const Type&) const;
        llvm::Value* doMult(llvm::Value*, llvm::Value*, const Type&, const Type&, const Type&) const;
        llvm::Value* doDiv(llvm::Value*, llvm::Value*, const Type&, const Type&, const Type&) const;
        llvm::Value* doRem(llvm::Value*, llvm::Value*, const Type&, const Type&, const Type&) const;
        llvm::Value* doCmp(ComparisonPredicate p, llvm::Value* lhs, llvm::Value* rhs, const Type& left_type,
                           const Type& right_type, const Type&) const;
        llvm::Value* fillArgs(bool,const FunctionSignature&,std::vector<llvm::Value*>&, llvm::Value*,
            std::vector<std::unique_ptr<Expression>>& exprs);
        llvm::Value* doInvoke(CallOperation* op, const Type&);
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
        llvm::Value* operator()(LambdaExpression*);
        llvm::Value* operator()(ScopeOperation*);
        llvm::Value* operator()(ObjectLiteral*);
    };
}