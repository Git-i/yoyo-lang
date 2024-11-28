#pragma once
#include <optional>
#include <source_location.h>
#include <unordered_map>

#include "token.h"
namespace Yoyo {
    class Scanner {
    public:
        explicit Scanner(std::string_view);

        std::optional<Token> NextToken();
        std::optional<Token> NextTokenInternal();
        size_t GetLine();
        [[nodiscard]] size_t GetOffset() const {return position;}
        [[nodiscard]] SourceLocation GetSourceLocation() const;

    private:
        [[nodiscard]] char Get();
        [[nodiscard]] char Peek() const;
        [[nodiscard]] char PeekNext() const;
        [[nodiscard]] bool NextIs(char);
        [[nodiscard]] bool IsEof() const;
        [[nodiscard]] Token ScanIdentifier();
        [[nodiscard]] Token ScanNumber();
        [[nodiscard]] std::optional<Token> ScanStringLiteral();
        [[nodiscard]] std::optional<Token> ScanCharLiteral();
        void nextLine();
        void handleLineComment();
        void handleBlockComment();
        size_t position = 0;
        size_t col = 1;
        size_t line = 1;
        std::string_view source;
        std::unordered_map<std::string_view, TokenType> keywords;
    };
}