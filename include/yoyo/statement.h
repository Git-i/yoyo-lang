#pragma once
#include "ast_node.h"
#include <func_sig.h>
#include <utility>
#include <variant>

#include "type.h"
#include "expression.h"
#include "class_entry.h"
#include <optional>
#include <cstdint>
#include "generic_clause.h"
namespace Yoyo
{
    class ExpressionStatement;
    class VariableDeclaration;
    class FunctionDeclaration;
    class IfStatement;
    class ReturnStatement;
    class WhileStatement;
    class BlockStatement;
    class ClassDeclaration;
    class ForStatement;
    class ModuleImport;
    class EnumDeclaration;
    class ConditionalExtraction;
    class WithStatement;
    class OperatorOverload;
    class GenericFunctionDeclaration;
    class AliasDeclaration;
    class GenericAliasDeclaration;
    class InterfaceDeclaration;
    class GenericInterfaceDeclaration;
    class GenericClassDeclaration;
    typedef std::variant<
        ForStatement*,
        ClassDeclaration*,
        BlockStatement*,
        WhileStatement*,
        IfStatement*,
        ReturnStatement*,
        FunctionDeclaration*,
        VariableDeclaration*,
        ExpressionStatement*,
        ModuleImport*,
        EnumDeclaration*,
        ConditionalExtraction*,
        OperatorOverload*,
        WithStatement*,
        GenericFunctionDeclaration*,
        GenericAliasDeclaration*,
        AliasDeclaration*,
        InterfaceDeclaration*,
        GenericInterfaceDeclaration*,
        GenericClassDeclaration*> StatementVariant;
    enum class Ownership {Owning = 0, NonOwning, NonOwningMut};
    struct Attribute {
        std::string name;
        std::vector<std::string> params;
        [[nodiscard]] bool has_params() const {
            return !params.empty();
        }
    };
    class Statement : public ASTNode
    {
    public:
        virtual ~Statement() = default;
        virtual StatementVariant toVariant() = 0;
        std::vector<Attribute> attributes;
        static auto attachSLAndParent(std::unique_ptr<Statement> self, SourceLocation bg, SourceLocation end,ASTNode* parent = nullptr)
            -> std::unique_ptr<Statement>
        {
            self->beg = bg;
            self->end = end;
            self->parent = parent;
            return self;
        }
    };
    class ExpressionStatement : public Statement
    {
    public:
        std::unique_ptr<Expression> expression;
        explicit ExpressionStatement(std::unique_ptr<Expression> exp) : expression(std::move(exp)) {};
        StatementVariant toVariant() override;
    };
    class ModuleImport : public Statement
    {
    public:
        std::string module_name;
        std::string module_path;
        ModuleImport(std::string name, std::string path) : module_name(std::move(name)),
            module_path(std::move(path)) {};
        StatementVariant toVariant() override;
    };
    class AliasDeclaration: public Statement
    {
    public:
        Type type;
        std::string name;
        AliasDeclaration(std::string name, Type t) : type(std::move(t)), name(std::move(name)) {};
        StatementVariant toVariant() override;
    };
    class GenericAliasDeclaration: public AliasDeclaration
    {
    public:
        GenericClause clause;
        explicit GenericAliasDeclaration(std::string name, Type t, GenericClause gc)
            : AliasDeclaration(std::move(name), std::move(t)),
            clause(std::move(gc)) {};
        StatementVariant toVariant() override;
    };
    class VariableDeclaration : public Statement
    {
    public:
        std::optional<Type> type;
        Token identifier;
        std::unique_ptr<Expression> initializer;
        bool is_mut;
        VariableDeclaration(Token iden, std::optional<Type> t, std::unique_ptr<Expression> init, bool mut)
            : type(std::move(t)), identifier(iden), initializer(std::move(init)), is_mut(mut) {};
        StatementVariant toVariant() override;
    };
    class FunctionDeclaration : public Statement
    {
    public:
        FunctionSignature signature;
        std::string name;
        std::unique_ptr<Statement> body;
        FunctionDeclaration(std::string name, FunctionSignature sig, std::unique_ptr<Statement> body)
            : signature(std::move(sig)), name(std::move(name)), body(std::move(body)) {}
        StatementVariant toVariant() override;
    };
    class GenericFunctionDeclaration: public FunctionDeclaration
    {
    public:
        GenericClause clause;
        GenericFunctionDeclaration(GenericClause cl, FunctionDeclaration body)
            : FunctionDeclaration(std::move(body)), clause(std::move(cl)) {}
        StatementVariant toVariant() override;
    };
    class OperatorOverload : public Statement
    {
    public:
        FunctionSignature signature;
        TokenType tok;
        std::unique_ptr<Statement> body;
        OperatorOverload(TokenType ident, FunctionSignature sig, std::unique_ptr<Statement> body)
            : signature(std::move(sig)), tok(ident), body(std::move(body)) {}
        StatementVariant toVariant() override;
    };
    class ReturnStatement : public Statement
    {
    public:
        std::unique_ptr<Expression> expression;
        explicit ReturnStatement(std::unique_ptr<Expression> exp) : expression(std::move(exp)) {}
        StatementVariant toVariant() override;
    };
    class IfStatement : public Statement
    {
    public:
        std::unique_ptr<Expression> condition;
        std::unique_ptr<Statement> then_stat;
        std::unique_ptr<Statement> else_stat;
        IfStatement(std::unique_ptr<Expression> cond, std::unique_ptr<Statement> then_, std::unique_ptr<Statement> else_)
            : condition(std::move(cond)), then_stat(std::move(then_)), else_stat(std::move(else_)) {}
        StatementVariant toVariant() override;
    };
    class WhileStatement : public Statement
    {
    public:
        std::unique_ptr<Expression> condition;
        std::unique_ptr<Statement> body;
        WhileStatement(std::unique_ptr<Expression> cond, std::unique_ptr<Statement> body)
            : condition(std::move(cond)), body(std::move(body)) {}
        StatementVariant toVariant() override;
    };
    class BlockStatement : public Statement
    {
    public:
        std::vector<std::unique_ptr<Statement>> statements;
        explicit BlockStatement(std::vector<std::unique_ptr<Statement>> stats) : statements(std::move(stats)) {}
        StatementVariant toVariant() override;
    };

    class ClassDeclaration : public Statement
    {
    public:
        std::string name;
        std::vector<ClassVariable> vars;
        std::vector<ClassMethod> methods;
        //consider an unordered map, although types should generally implement too many interfaces
        std::vector<Type> interfaces;
        std::vector<InterfaceImplementation> impls;
        std::string destructor_name;
        std::optional<bool> is_trivially_destructible; //we use optional bool because we want to lazy evaluate
        bool has_clone;
        Ownership ownership;
        ClassDeclaration(
            Token ident,
            std::vector<ClassVariable> vars,
            std::vector<ClassMethod> methods,
            Ownership sh,
            std::vector<Type> intfs,
            std::vector<InterfaceImplementation> impls)
            : name(ident.text), vars(std::move(vars)), methods(std::move(methods)), ownership(sh), 
            interfaces(std::move(intfs)), impls(std::move(impls)){}
        StatementVariant toVariant() override;
    };
    class GenericClassDeclaration : public ClassDeclaration
    {
    public:
        GenericClause clause;
        GenericClassDeclaration(
            Token ident,
            std::vector<ClassVariable> vars,
            std::vector<ClassMethod> methods,
            Ownership sh,
            std::vector<Type> intfs,
            std::vector<InterfaceImplementation> impls,
            GenericClause cl) :
            ClassDeclaration(ident, 
                std::move(vars), 
                std::move(methods), sh, std::move(intfs), std::move(impls)), clause(std::move(cl)) {}
        StatementVariant toVariant() override;
    };
    class ForStatement : public Statement
    {
    public:
        std::unique_ptr<Expression> iterable;
        std::vector<Token> names;
        std::unique_ptr<Statement> body;
        ForStatement(std::vector<Token> names, std::unique_ptr<Expression> expr, std::unique_ptr<Statement> body)
            : iterable(std::move(expr)), names(std::move(names)), body(std::move(body)) {}
        StatementVariant toVariant() override;
    };

    class EnumDeclaration : public Statement
    {
    public:
        std::string identifier;
        std::unordered_map<std::string, int32_t> values;
        EnumDeclaration(std::string iden, std::unordered_map<std::string, int32_t> vals)
            : identifier(std::move(iden)), values(std::move(vals)) {}
        StatementVariant toVariant() override;
    };
    // if |value| (optional) { }
    // if |value| (result) {} else |error| {}
    class ConditionalExtraction : public Statement
    {
    public:
        std::string captured_name;
        bool is_ref;
        std::unique_ptr<Expression> condition;
        std::unique_ptr<Statement> body;
        std::string else_capture;
        bool else_is_ref;
        std::unique_ptr<Statement> else_body;
        ConditionalExtraction(
            std::string name,
            bool is_ref,
            std::unique_ptr<Expression> cond,
            std::unique_ptr<Statement> body,
            std::unique_ptr<Statement> else_,
            std::string else_name = "",
            bool else_is_ref = false)
                : captured_name(std::move(name)), is_ref(is_ref), condition(std::move(cond)), body(std::move(body))
                , else_capture(std::move(else_name)), else_is_ref(else_is_ref), else_body(std::move(else_)) {}
        StatementVariant toVariant() override;

    };
    class WithStatement : public Statement
    {
    public:
        std::string name;
        std::unique_ptr<Expression> expression;
        std::unique_ptr<Statement> body;
        WithStatement(std::string name, std::unique_ptr<Expression> exp, std::unique_ptr<Statement> body)
            : name(std::move(name)), expression(std::move(exp)), body(std::move(body)) {}
        StatementVariant toVariant() override;
    };
    class InterfaceDeclaration : public Statement
    {
    public:
        std::string name;
        std::vector<std::unique_ptr<FunctionDeclaration>> methods;
        StatementVariant toVariant() override;
    };
    class GenericInterfaceDeclaration : public InterfaceDeclaration
    {
    public:
        GenericClause clause;
        StatementVariant toVariant() override;
    };
}