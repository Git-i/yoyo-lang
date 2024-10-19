#pragma once
#include <scanner.h>
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
        [[nodiscard]] bool discard(TokenType t);
    private:
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