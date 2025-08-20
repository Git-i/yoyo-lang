#pragma once
#include <fn_type.h>
#include <list>
#include <span>
#include <statement.h>
#include "engine.h"
#include <utility>
#include <functional>
#include <ranges>
#include "error.h"
#include "module.h"
#include "cfg_node.h"
#include "token.h"
#include "borrow_checker.h"
#include "type_checker.h"
namespace Yoyo
{
    struct BorrowChecker;
    class IRGenerator
    {
    public:
        template<std::input_iterator It> void pushScopeWithConstLock(It begin, It end);
        SourceView* view;
        
        Type this_t;
        Type return_t;
        bool in_class = false;
        ModuleBase* module;
        std::vector<std::vector<UsingStatement*>> used_types;
        std::unique_ptr<Statement>* current_Statement; //we keep the current the statement in the case we want to steal it
        //std::unordered_map<std::string, BorrowResult::borrow_result_t> lifetimeExtensions;
        //std::unordered_map<std::string, BorrowResult::borrow_result_t> lambda_borrows;
        std::vector<CFGNodeManager> function_cfgs;
        std::vector<BorrowChecker> function_borrow_checkers;
        std::string block_hash;
        bool has_error = false;

        void saturateSignature(FunctionSignature& sig, ModuleBase* md);
        bool isShadowing(const std::string&) const;
        
        
        void error(const Error& err);
        std::string reset_hash();
        void annotateClass(ClassDeclaration*);
        void checkClass(ClassDeclaration*);
        static FunctionDeclaration* GetParentFunction(ASTNode* node);
        template<std::ranges::forward_range T>
        static std::string mangleGenericArgs(const T& list) 
            requires std::same_as<std::remove_cvref_t<decltype(*list.begin())>, Type>
        {
            if (std::ranges::empty(list)) return "";
            std::string final = "::<" + list.begin()->full_name();
            for (auto& tp : std::ranges::subrange(list.begin() + 1, list.end()))
                final += "," + tp.full_name();
            final += ">";
            return final;
        }
        
        
        std::optional<Type> inferReturnType(Statement* stat);
        
        virtual std::optional<Type> getVariableType(const std::string& name, Expression*) = 0;
        virtual void doFunction(FunctionDeclaration*) = 0;
        virtual void doClass(ClassDeclaration*) = 0;
        virtual void doAlias(AliasDeclaration*) = 0;
        virtual void doConst(ConstantDeclaration*) = 0;
        void generateGenericFunction(ModuleBase* mod, const std::string& hash, GenericFunctionDeclaration* fn, std::span<Type> types);
        void generateGenericClass(ModuleBase* mod, const std::string& hash, GenericClassDeclaration* decl, std::span<Type> types);
        void generateGenericClass(ModuleBase* mod, const std::string& hash, GenericClassDeclaration* decl, std::span<const Type> types);
        void generateGenericAlias(ModuleBase* mod, const std::string& block, GenericAliasDeclaration* decl, std::span<Type> types);
        void generateGenericInterface(ModuleBase* md, const std::string& block, GenericInterfaceDeclaration* decl, std::span<Type> types);
        std::optional<Error> apply_using(Type&, ModuleBase*&, std::string&);
    };
    
    class ExpressionTypeChecker
    {
        IRGenerator* irgen;
        std::optional<Type> target;
    public:
        std::optional<FunctionType> checkNameWithinClassOrModule(ModuleBase*, ClassDeclaration* type, std::string_view name);
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
        Result operator()(SpawnExpression*);
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

}