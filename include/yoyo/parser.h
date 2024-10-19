#pragma once
#include <scanner.h>
#include <unordered_map>

#include "parselets/prefix_parselets.h"
#include "parselets/infix_parselets.h"
namespace Yoyo
{
    class Parser
    {
    public:
        explicit Parser(std::string source);
        std::unique_ptr<Expression> parseExpression(uint32_t precedence);
    private:
        [[nodiscard]] bool discard(TokenType t);
        Token Get();
        Token Peek();
        uint32_t GetNextPrecedence();
        PrefixParselet* GetPrefixParselet(TokenType t);
        InfixParselet* GetInfixParselet(TokenType t);
        std::string source;
        Scanner scn;
        std::unordered_map<TokenType, std::shared_ptr<PrefixParselet>> prefixParselets;
        std::unordered_map<TokenType, std::shared_ptr<InfixParselet>> infixParselets;
    }
}