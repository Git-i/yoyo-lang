#pragma once
#include <func_sig.h>
#include <variant>

#include "type.h"
#include "expression.h"
#include "class_entry.h"
#include <optional>
#include <cstdint>
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
        ConditionalExtraction*> StatementVariant;
    class Statement
    {
    public:
        virtual ~Statement() = default;
        virtual StatementVariant toVariant() = 0;
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
        Token identifier;
        std::unique_ptr<Statement> body;
        FunctionDeclaration(Token ident, FunctionSignature sig, std::unique_ptr<Statement> body)
            : signature(std::move(sig)), identifier(ident), body(std::move(body)) {}
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
        Token identifier;
        std::vector<ClassVariable> vars;
        std::vector<ClassMethod> methods;
        ClassDeclaration(Token ident, std::vector<ClassVariable> vars, std::vector<ClassMethod> methods)
            : identifier(ident), vars(std::move(vars)), methods(std::move(methods)) {}
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
        Token identifier;
        std::unordered_map<std::string, int32_t> values;
        EnumDeclaration(Token iden, std::unordered_map<std::string, int32_t> vals)
            : identifier(iden), values(std::move(vals)) {}
        StatementVariant toVariant() override;
    };
    // if |value| (optional) { }
    // if |value| (result) {} else |error| {}
    class ConditionalExtraction : public Statement
    {
    public:
        std::string captured_name;
        std::unique_ptr<Expression> condition;
        std::unique_ptr<Statement> body;
        std::string else_capture;
        std::unique_ptr<Statement> else_body;
        ConditionalExtraction(
            std::string name,
            std::unique_ptr<Expression> cond,
            std::unique_ptr<Statement> body,
            std::unique_ptr<Statement> else_,
            std::string else_name = "")
                : captured_name(std::move(name)), condition(std::move(cond)), body(std::move(body))
                , else_capture(std::move(else_name)), else_body(std::move(else_)) {}
        StatementVariant toVariant() override;

    };
}