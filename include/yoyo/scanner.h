#pragma once
#include <optional>

#include "token.h"
namespace Yoyo {
    class Scanner {
    public:
        explicit Scanner(std::string_view);

        std::optional<Token> NextToken();
        size_t GetLine();
    private:
        char Get();
        char Peek() const;
        bool NextIs(char);
        bool IsEof() const;
        size_t position = 0;
        size_t line = 1;
        std::string_view source;
    };
}