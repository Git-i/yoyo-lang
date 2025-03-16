#pragma once
#include <fn_type.h>
#include <list>
#include <span>
#include <statement.h>
#include "engine.h"
#include <utility>
#include "error.h"
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
            borrow_result_t operator()(Expression*){ return {}; }
            borrow_result_t operator()(CallOperation*);
            borrow_result_t operator()(PrefixOperation*);
            borrow_result_t operator()(AsExpression*);
            borrow_result_t operator()(GroupingExpression*){ return {}; }
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
        //TODO
        borrow_result_t operator()(PostfixOperation*){ return {}; }
        borrow_result_t operator()(SubscriptOperation*){ return {}; }
        borrow_result_t operator()(LambdaExpression*){ return {}; }
        borrow_result_t operator()(ScopeOperation*){ return {}; }
        borrow_result_t operator()(GCNewExpression*){ return {}; }
        borrow_result_t operator()(AsExpression*);
    };
    class IRGenerator
    {
        template<std::input_iterator It> void pushScopeWithConstLock(It begin, It end);
    public:
        SourceView* view;
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
        std::unordered_map<std::string, BorrowResult::borrow_result_t> lifetimeExtensions;
        std::vector<CFGNodeManager> function_cfgs;
        std::string block_hash;
        llvm::BasicBlock* break_to = nullptr;
        llvm::BasicBlock* continue_to = nullptr;

        llvm::Type* ToLLVMType(const Type& type, bool is_ref);
        void saturateSignature(FunctionSignature& sig, Module* md);
        llvm::FunctionType* ToLLVMSignature(const FunctionSignature& sig);
        llvm::AllocaInst* Alloca(std::string_view name, llvm::Type* type);
        llvm::Value* Malloc(std::string_view name, llvm::Value* size);
        llvm::Value* GCMalloc(llvm::Value* size);
        llvm::Value* GCMalloc(size_t size);
        void printString(const char* string);
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
        void operator()(EnumDeclaration*);
        void operator()(ModuleImport*){}
        void operator()(ConditionalExtraction*);
        void operator()(WithStatement*);
        void operator()(OperatorOverload*);
        void operator()(GenericFunctionDeclaration*);
        void operator()(AliasDeclaration*);
        void operator()(GenericAliasDeclaration*);
        void operator()(GenericClassDeclaration*);
        void operator()(InterfaceDeclaration*);
        void operator()(BreakStatement*);
        void operator()(ContinueStatement*);
        void operator()(ConstantDeclaration*);
        void operator()(CImportDeclaration*) {}
        void operator()(UnionDeclaration*) ;

        void error(const Error& err);
        std::string reset_hash();
        void annotateClass(ClassDeclaration*);
        void checkClass(ClassDeclaration*);
        static FunctionDeclaration* GetParentFunction(ASTNode* node);
        static std::string mangleGenericArgs(std::span<const Type> list);
        void pushScope() {variables.emplace_back();}
        void popScope();
        void callDestructors();
        std::optional<Type> inferReturnType(Statement* stat);
        explicit IRGenerator(llvm::LLVMContext& ctx) : context(ctx) {}
        bool has_error = false;
        llvm::StructType* hanldeClassDeclaration(std::span<const ClassVariable> vars, Ownership own, std::string_view name);
        bool GenerateIR(std::string_view name, std::vector<std::unique_ptr<Statement>> statements, Module* md, Engine* eng);
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
        struct Result : std::variant<FunctionType, Error>
        {
            operator bool() const {
                return std::holds_alternative<FunctionType>(*this);
            }
            FunctionType* operator->() {
                return &std::get<0>(*this);
            }
            FunctionType& value()& { if (!*this) debugbreak(); return std::get<0>(*this); }
            FunctionType&& value() && { if (!*this) debugbreak(); return std::get<0>(std::move(*this)); }
            const FunctionType& value() const& { if (!*this) debugbreak(); return std::get<0>(*this); }
            FunctionType& operator*() & { return value(); }
            FunctionType&& operator*() && { return std::move(*this).value(); }
            const FunctionType& operator*() const& { return value(); }
            std::optional<FunctionType> to_optional() {
                return (*this) ? std::optional{ value() } : std::nullopt;
            }
            FunctionType value_or_error() const {
                return (*this) ? value() : FunctionType{ Type{ "__error_type" } };
            }
            const Error& error() const {
                return std::get<1>(*this);
            }
        };
        Result operator()(IntegerLiteral*);
        Result operator()(BooleanLiteral*);
        Result operator()(TupleLiteral*);
        Result operator()(ArrayLiteral*);
        Result operator()(RealLiteral*);
        Result operator()(StringLiteral*);
        Result operator()(NameExpression*);
        Result operator()(GenericNameExpression*);
        Result operator()(PrefixOperation*);
        Result operator()(BinaryOperation*);
        Result operator()(GroupingExpression*);
        Result operator()(LogicalOperation*);
        Result operator()(PostfixOperation*);
        Result operator()(CallOperation*);
        Result operator()(SubscriptOperation*);
        Result operator()(LambdaExpression*);
        Result operator()(ScopeOperation*);
        Result operator()(ObjectLiteral*);
        Result operator()(NullLiteral*);
        Result operator()(AsExpression*);
        Result operator()(CharLiteral*);
        Result operator()(GCNewExpression*);
    };
    class ExpressionEvaluator
    {
        IRGenerator* irgen;
        std::optional<Type> target;
    public:
        Type lastDeducedType;
        enum ComparisonPredicate
        {
            EQ, GT, LT, EQ_GT, EQ_LT, NE, SPACE
        };
        explicit ExpressionEvaluator(IRGenerator* gen, std::optional<Type> target = std::nullopt) : irgen(gen),
            target(std::move(target)) {}
        llvm::Value* implicitConvert(Expression* xp, llvm::Value*, const Type&, const Type&, llvm::Value* = nullptr) const;
        llvm::Value* clone(Expression* xp, llvm::Value* value, const Type& left_type, llvm::Value* into = nullptr) const;
        void destroy(llvm::Value* value, const Type& type) const;
        llvm::Value* doDot(Expression* lhs, Expression* rhs, const Type& left_type, bool load_prim = true);
        llvm::Value* doAddition(Expression*,Expression*,const Type&,const Type&);
        llvm::Value* doShl(Expression*,Expression*,const Type&,const Type&);
        llvm::Value* doShr(Expression*,Expression*,const Type&,const Type&);
        llvm::Value* doMinus(Expression*, Expression*, const Type&, const Type&);
        llvm::Value* doMult(Expression*, Expression*, const Type&, const Type&);
        llvm::Value* doDiv(Expression*, Expression*, const Type&, const Type&);
        llvm::Value* doRem(Expression*, Expression*, const Type&, const Type&);
        llvm::Value* doCmp(ComparisonPredicate p, Expression*, Expression*, const Type& left_type,
                           const Type& right_type, const Type&);
        llvm::Value* fillArgs(bool,const FunctionSignature&,std::vector<llvm::Value*>&, llvm::Value*,
            std::vector<std::unique_ptr<Expression>>& exprs);
        llvm::Value* doInvoke(CallOperation* op, const Type&);
        llvm::Value* doUnionVar(CallOperation* op, Type&);
        void generateGenericFunction(Module*, const std::string&,GenericFunctionDeclaration*, std::span<Type>);
        void generateGenericAlias(Module*, const std::string&,GenericAliasDeclaration*, std::span<Type>);
        void generateGenericInterface(Module*, const std::string&, GenericInterfaceDeclaration*, std::span<Type>);
        void generateGenericClass(Module*, const std::string&, GenericClassDeclaration*, std::span<Type>);
        void generateGenericClass(Module*, const std::string&, GenericClassDeclaration*, std::span<const Type>);
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
        llvm::Value* operator()(GCNewExpression*);
    };
    class ConstantEvaluator
    {
        llvm::Constant* constConvert(llvm::Constant* in, const Type& source, const Type& dest);
    public:
        IRGenerator* irgen;
        std::vector<std::pair<std::string, std::string>> disallowed_consts;
        llvm::Constant* operator()(IntegerLiteral*);
        llvm::Constant* operator()(BooleanLiteral*);
        llvm::Constant* operator()(RealLiteral*);
        llvm::Constant* operator()(PrefixOperation*);
        llvm::Constant* operator()(NameExpression*);
        llvm::Constant* operator()(BinaryOperation*);
        llvm::Constant* operator()(GroupingExpression*);
        llvm::Constant* operator()(LogicalOperation*);
        llvm::Constant* operator()(ScopeOperation*);
        //llvm::Constant* operator()(AsExpression*);
        llvm::Constant* operator()(CharLiteral*);
        llvm::Constant* operator()(ObjectLiteral*);
        llvm::Constant* operator()(StringLiteral*);
        llvm::Constant* operator()(Expression*) { return nullptr; }
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