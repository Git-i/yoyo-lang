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
#include "token.h"
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
        borrow_result_t operator()(MacroInvocation*);
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
    public:
        template<std::input_iterator It> void pushScopeWithConstLock(It begin, It end);
        SourceView* view;
        
        Type this_t;
        Type return_t;
        bool in_class = false;
        Module* module;
        std::unique_ptr<Statement>* current_Statement; //we keep the current the statement in the case we want to steal it
        std::unordered_map<std::string, BorrowResult::borrow_result_t> lifetimeExtensions;
        std::vector<CFGNodeManager> function_cfgs;
        std::string block_hash;

        void saturateSignature(FunctionSignature& sig, Module* md);
        bool isShadowing(const std::string&) const;
        
        
        void error(const Error& err);
        std::string reset_hash();
        void annotateClass(ClassDeclaration*);
        void checkClass(ClassDeclaration*);
        static FunctionDeclaration* GetParentFunction(ASTNode* node);
        static std::string mangleGenericArgs(std::span<const Type> list);
        
        void callDestructors(size_t depth = 0);
        std::optional<Type> inferReturnType(Statement* stat);
        
        bool has_error = false;
        
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
        Result operator()(MacroInvocation*);
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
    struct MacroEvaluator
    {
        // objects in macros can only be
        // str, Expr(and subtypes), Stat(and subtypes), null, int, float, bool ,
        // token
        IRGenerator* irgen;
        struct OwnedToken {
            TokenType type;
            std::string text;
            static OwnedToken from_token(const Token& tk) {
                return OwnedToken{ tk.type, std::string(tk.text) };
            }
        };
        struct ObjectTy;
        // normally tokens use string view 
        // in the case where we transofom and owned token to a tokseq
        // we need to keep the string alive
        // the vector is just the way we can cheese the parser to parse
        // directly from tokens and not a string
        struct TokSeq {
            std::string composite_string;
            std::vector<std::pair<Token, SourceLocation>> tokens;
        };
        using VarintTy = std::variant<
            std::monostate,
            std::string,
            std::unique_ptr<ASTNode>,
            std::unique_ptr<ASTNode>*,
            bool,
            double,
            int64_t,
            OwnedToken,
            TokSeq,
            TokSeq*,
            std::vector<ObjectTy>,
            std::vector<ObjectTy>*,
            std::pair<std::string, std::function<ObjectTy(std::vector<ObjectTy>)>>
        >;
        struct ObjectTy : VarintTy {
            ObjectTy() = default;
            template<typename T>
            ObjectTy(T&& obj) noexcept : VarintTy(std::forward<T>(obj)) {}
            ObjectTy(ObjectTy&& other) noexcept = default;
            ObjectTy& operator=(ObjectTy&& other) noexcept = default;
            ObjectTy clone_or_ref_to();
        };
        struct MapTy : std::unordered_map<std::string, ObjectTy> {
            MapTy() = default;
            MapTy(const MapTy&) = delete;
            MapTy(MapTy&&) noexcept = default;
        };
        ObjectTy return_addr;
        bool has_returned;
        std::vector<MapTy> variables;
        void operator()(VariableDeclaration*);
        void operator()(IfStatement*);
        void operator()(WhileStatement*);
        void operator()(ForStatement*);
        void operator()(BlockStatement*);
        void operator()(ReturnStatement*);
        void operator()(ExpressionStatement*);
        void operator()(ConditionalExtraction*);
        void operator()(BreakStatement*);
        void operator()(ContinueStatement*);
        void operator()(Statement*);
        void eval(MacroInvocation*);
    };

    void validate_expression_borrows(Expression*, IRGenerator*);
    void validate_borrows(std::span<const std::pair<Expression*, BorrowResult::borrow_result_t>> param, IRGenerator* irgen);

}