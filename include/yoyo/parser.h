#pragma once
#include <scanner.h>
#include <span>
#include <statement.h>
#include <type.h>
#include <unordered_map>

#include "parselets/prefix_parselets.h"
#include "parselets/infix_parselets.h"
#include <vector>
namespace Yoyo
{
    class Parser
    {
    public:
        explicit Parser(std::string source);
        std::unique_ptr<Expression> parseExpression(uint32_t precedence);
        std::unique_ptr<Statement> parseVariableDeclaration(Token identifier);
        std::unique_ptr<Statement> parseEnumDeclaration(Token identifier);
        std::unique_ptr<Statement> parseClassDeclaration(Token identifier, bool);
        std::unordered_map<std::string, std::unique_ptr<Expression>> parseObjectLiteral();
        std::unique_ptr<Statement> parseDeclaration();
        std::unique_ptr<Statement> parseReturnStatement(Token);
        std::unique_ptr<Statement> parseExpressionStatement();
        std::unique_ptr<Statement> parseConditionalExtraction(Token);
        std::unique_ptr<Statement> parseIfStatement(Token);
        std::unique_ptr<Statement> parseBlockStatement(Token);
        std::unique_ptr<Statement> parseForStatement(Token);
        std::unique_ptr<Statement> parseWhileStatement(Token);
        std::unique_ptr<Statement> parseStatement();
        std::unique_ptr<Statement> parseWithStatement(Token);
        uint32_t GetNextTypePrecedence();
        std::vector<std::unique_ptr<Statement>> parseProgram();
        std::optional<Type> parseType(uint32_t precedence);
        [[nodiscard]] bool discard(TokenType t);
        [[nodiscard]] bool failed() const {return has_error;};
        void pushToken(Token t, SourceLocation loc);
        void error(std::string message, std::optional<Token> tk);

        void synchronizeTo(std::span<const TokenType> t);///< skip all tokens till the next token of type @c t
        std::optional<Token> Peek();
        std::optional<Token> Get();
        std::optional<std::pair<Token, SourceLocation>> PeekWithEndLocation();
        std::optional<std::pair<Token, SourceLocation>> GetWithEndLocation();
        std::optional<FunctionSignature> parseFunctionSignature();
        std::optional<GenericClause> parseGenericClause();
        const Scanner& getScanner() const { return scn; };
        const std::string& getSource() const { return source; };
        ASTNode* parent = nullptr;
        SourceLocation discardLocation;
    private:
        bool has_error = false;
        std::unique_ptr<Statement> parseFunctionDeclaration(Token identifier);

        uint32_t GetNextPrecedence();
        std::unique_ptr<Statement> parseModuleImport(Token identifier);
        std::unique_ptr<Statement> parseTopLevelDeclaration();
        std::unique_ptr<Statement> parseOperatorOverload(const Token& tok);
        bool isTopLevelDeclaration();
        PrefixParselet* GetPrefixParselet(TokenType t);
        InfixParselet* GetInfixParselet(TokenType t);
        std::string source;
        Scanner scn;
        std::unordered_map<TokenType, std::shared_ptr<PrefixParselet>> prefixParselets;
        std::unordered_map<TokenType, std::shared_ptr<InfixParselet>> infixParselets;
        std::vector<std::pair<Token, SourceLocation>> peekBuffer;
    };
}