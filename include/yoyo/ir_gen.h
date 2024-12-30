#pragma once
#include <fn_type.h>
#include <list>
#include <span>
#include <statement.h>
#include "engine.h"
#include <utility>

#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "module.h"
#include "cfg_node.h"
namespace Yoyo
{
    class IRGenerator;
    //TODO: also rename, why can't AI do this :-(
    /// The basic principle is:
    /// - If the result of the expressions references any variable, it should be reflected in the borrows
    /// or mutable_borrows maps
    class BorrowResult
    {
        IRGenerator* irgen;
    public:
        enum BorrowType { Mut, Const };
        explicit BorrowResult(IRGenerator* irgen):irgen(irgen){}


        using borrow_result_t = std::vector<std::pair<std::string, BorrowType>>;
        struct LValueBorrowResult
        {
            IRGenerator* irgen;
            borrow_result_t operator()(NameExpression*);
            borrow_result_t operator()(BinaryOperation*);
            borrow_result_t operator()(Expression*){}
            borrow_result_t operator()(CallOperation*);
            borrow_result_t operator()(PrefixOperation*);
            borrow_result_t operator()(AsExpression*);
            borrow_result_t operator()(GroupingExpression*){}
        };

        //literals don't borrow (hopefully)
        borrow_result_t operator()(IntegerLiteral*) const {return {};}
        borrow_result_t operator()(BooleanLiteral*) const {return {};}
        borrow_result_t operator()(TupleLiteral*) const {return {};}
        borrow_result_t operator()(ArrayLiteral*) const {return {};}
        borrow_result_t operator()(RealLiteral*) const {return {};}
        borrow_result_t operator()(StringLiteral*) const {return {};}
        borrow_result_t operator()(ObjectLiteral*) const {return {};}
        borrow_result_t operator()(NullLiteral*) const {return {};}
        borrow_result_t operator()(CharLiteral*) const {return {};}
        borrow_result_t operator()(LogicalOperation*) const {return {};}


        borrow_result_t operator()(NameExpression*);
        borrow_result_t operator()(PrefixOperation*);
        borrow_result_t operator()(BinaryOperation*);
        borrow_result_t operator()(GroupingExpression*);
        borrow_result_t operator()(CallOperation*);
        borrow_result_t doCall(CallOperation* expr);

        borrow_result_t operator()(PostfixOperation*){}
        borrow_result_t operator()(SubscriptOperation*){}
        borrow_result_t operator()(LambdaExpression*){}
        borrow_result_t operator()(ScopeOperation*){}
        borrow_result_t operator()(AsExpression*);
    };
    class IRGenerator
    {
        template<std::input_iterator It> void pushScopeWithConstLock(It begin, It end);
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
        std::unique_ptr<Statement>* current_Statement; //we keep the current the statement in the case we want to steal it
        //we keep the alloca of the variable, its type and its drop flag
        std::vector<std::unordered_map<std::string, std::tuple<llvm::Value*, Type, llvm::Value*>>> variables;
        std::unordered_map<std::string, std::pair<std::vector<std::string>*, llvm::StructType*>> lambdas;
        std::unordered_map<std::string, FunctionSignature> lambdaSigs;
        std::unordered_map<std::string, BorrowResult::borrow_result_t> lifetimeExtensions;
        std::vector<CFGNodeManager> function_cfgs;
        std::string block_hash;

        llvm::Type* ToLLVMType(const Type& type, bool is_ref);
        void saturateSignature(FunctionSignature& sig, Module* md);
        llvm::FunctionType* ToLLVMSignature(const FunctionSignature& sig);
        llvm::AllocaInst* Alloca(std::string_view name, llvm::Type* type);
        llvm::Value* Malloc(std::string_view name, llvm::Value* size);
        void Free(llvm::Value* value);
        bool isShadowing(const std::string&) const;
        Type reduceLiteral(const Type& src, llvm::Value* val);
        void operator()(FunctionDeclaration*);
        void operator()(ClassDeclaration*);
        void operator()(VariableDeclaration*);
        void operator()(IfStatement*);
        void operator()(WhileStatement*);
        void operator()(ForStatement*);
        void operator()(BlockStatement*);
        void operator()(ReturnStatement*);
        void operator()(ExpressionStatement*);
        void operator()(EnumDeclaration*){}
        void operator()(ModuleImport*){}
        void operator()(ConditionalExtraction*);
        void operator()(WithStatement*);
        void operator()(OperatorOverload*);
        void operator()(GenericFunctionDeclaration*);
        void operator()(AliasDeclaration*);
        void operator()(GenericAliasDeclaration*);

        void error();
        std::string reset_hash();
        void annotateClass(ClassDeclaration*);
        static FunctionDeclaration* GetParentFunction(ASTNode* node);
        static std::string mangleGenericArgs(std::span<const Type> list);
        void pushScope() {variables.emplace_back();}
        void popScope();
        std::optional<Type> inferReturnType(Statement* stat);
        explicit IRGenerator(llvm::LLVMContext& ctx) : context(ctx) {}
        llvm::StructType* hanldeClassDeclaration(std::span<const ClassVariable> vars, Ownership own, std::string_view name);
        void GenerateIR(std::string_view name, std::vector<std::unique_ptr<Statement>> statements, Module* md);
    };



    struct TopLevelVisitor
    {
        IRGenerator* irgen;
        //return true on success
        bool operator()(std::unique_ptr<FunctionDeclaration>) const;
        bool operator()(std::unique_ptr<ClassDeclaration>) const;
        bool operator()(std::unique_ptr<OperatorOverload>);
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
        bool hasToStr(const Type& t);
        /// We return FunctionType because it's a subclass of Type and some
        /// expressions ( @c NameExpression and @c BinaryExpression ) can be a function
        std::optional<FunctionType> operator()(IntegerLiteral*);
        std::optional<FunctionType> operator()(BooleanLiteral*);
        std::optional<FunctionType> operator()(TupleLiteral*);
        std::optional<FunctionType> operator()(ArrayLiteral*);
        std::optional<FunctionType> operator()(RealLiteral*);
        std::optional<FunctionType> operator()(StringLiteral*);
        std::optional<FunctionType> operator()(NameExpression*);
        std::optional<FunctionType> operator()(GenericNameExpression*);
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
        std::optional<FunctionType> operator()(NullLiteral*);
        std::optional<FunctionType> operator()(AsExpression*);
        std::optional<FunctionType> operator()(CharLiteral*);

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
        llvm::Value* implicitConvert(llvm::Value*, const Type&, const Type&, llvm::Value* = nullptr) const;
        llvm::Value* doAssign(llvm::Value* lhs, llvm::Value* rhs, const Type& left_type, const Type& right_type);
        llvm::Value* clone(llvm::Value* value, const Type& left_type, llvm::Value* into = nullptr) const;
        void destroy(llvm::Value* value, const Type& type) const;
        llvm::Value* doDot(Expression* lhs, Expression* rhs, const Type& left_type, bool load_prim = true);
        llvm::Value* doAddition(llvm::Value*,llvm::Value*,const Type&,const Type&) const;
        llvm::Value* doMinus(llvm::Value*, llvm::Value*, const Type&, const Type&) const;
        llvm::Value* doMult(llvm::Value*, llvm::Value*, const Type&, const Type&) const;
        llvm::Value* doDiv(llvm::Value*, llvm::Value*, const Type&, const Type&) const;
        llvm::Value* doRem(llvm::Value*, llvm::Value*, const Type&, const Type&) const;
        llvm::Value* doCmp(ComparisonPredicate p, llvm::Value* lhs, llvm::Value* rhs, const Type& left_type,
                           const Type& right_type, const Type&) const;
        llvm::Value* fillArgs(bool,const FunctionSignature&,std::vector<llvm::Value*>&, llvm::Value*,
            std::vector<std::unique_ptr<Expression>>& exprs);
        llvm::Value* doInvoke(CallOperation* op, const Type&);
        void generateGenericFunction(Module*, const std::string&,GenericFunctionDeclaration*, std::span<Type>);
        void generateGenericAlias(Module*, const std::string&,GenericAliasDeclaration*, std::span<Type>);
        //the malloc and the size
        std::pair<llvm::Value*, llvm::Value*> doToStr(llvm::Value*, const Type&);
        struct LValueEvaluator
        {
            IRGenerator* irgen;
            llvm::Value* operator()(NameExpression*);
            llvm::Value* operator()(BinaryOperation*);
            llvm::Value* operator()(Expression*);
            llvm::Value* operator()(PrefixOperation*);
            llvm::Value* operator()(GroupingExpression*);
        };
        llvm::Value* operator()(IntegerLiteral*);
        llvm::Value* operator()(BooleanLiteral*);
        llvm::Value* operator()(TupleLiteral*);
        llvm::Value* operator()(ArrayLiteral*);
        llvm::Value* operator()(RealLiteral*);
        llvm::Value* operator()(StringLiteral*);
        llvm::Value* operator()(NameExpression*);
        llvm::Value* operator()(GenericNameExpression*);
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
        llvm::Value* operator()(NullLiteral*);
        llvm::Value* operator()(AsExpression*);
        llvm::Value* operator()(CharLiteral*);
    };
    //TODO: rename
    class LifetimeExceedsFunctionChecker
    {
        IRGenerator* irgen;
    public:
        explicit LifetimeExceedsFunctionChecker(IRGenerator* irgen)
            : irgen(irgen)
        {
        }
        bool operator()(NameExpression*);
        bool operator()(Expression*);

    };

    void validate_expression_borrows(Expression*, IRGenerator*);
    void validate_borrows(std::span<const std::pair<Expression*, BorrowResult::borrow_result_t>> param, IRGenerator* irgen);

}