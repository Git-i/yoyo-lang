#pragma once
#include <scanner.h>
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
        std::unique_ptr<Statement> parseDeclaration();
        std::unique_ptr<Statement> parseReturnStatement();
        std::unique_ptr<Statement> parseExpressionStatement();
        std::unique_ptr<Statement> parseIfStatement();
        std::unique_ptr<Statement> parseStatement();
        uint32_t GetNextTypePrecedence();
        std::optional<Type> parseType(uint32_t precedence);
        [[nodiscard]] bool discard(TokenType t);
        void pushToken(Token t);
    private:
        std::unique_ptr<Statement> parseFunctionDeclaration(Token identifier);
        std::optional<FunctionSignature> parseFunctionSignature();

        std::optional<Token> Get();
        std::optional<Token> Peek();
        uint32_t GetNextPrecedence();
        PrefixParselet* GetPrefixParselet(TokenType t);
        InfixParselet* GetInfixParselet(TokenType t);
        std::string source;
        Scanner scn;
        std::unordered_map<TokenType, std::shared_ptr<PrefixParselet>> prefixParselets;
        std::unordered_map<TokenType, std::shared_ptr<InfixParselet>> infixParselets;
        std::vector<Token> peekBuffer;
    };
}