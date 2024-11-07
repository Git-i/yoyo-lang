#pragma once
#include <optional>
#include <unordered_map>

#include "token.h"
namespace Yoyo {
    class Scanner {
    public:
        explicit Scanner(std::string_view);

        std::optional<Token> NextToken();
        size_t GetLine();
    private:
        [[nodiscard]] char Get();
        [[nodiscard]] char Peek() const;
        [[nodiscard]] char PeekNext() const;
        [[nodiscard]] bool NextIs(char);
        [[nodiscard]] bool IsEof() const;
        [[nodiscard]] Token ScanIdentifier();
        [[nodiscard]] Token ScanNumber();
        [[nodiscard]] Token ScanStringLiteral();
        void handleLineComment();
        void handleBlockComment();
        size_t position = 0;
        size_t line = 1;
        std::string_view source;
        std::unordered_map<std::string_view, TokenType> keywords;
    };
}