#include <istream>
#include <optional>

#include "token.h"
namespace Yoyo {
    class Scanner {
        explicit Scanner(std::u8string_view);
        explicit Scanner(std::string_view);
        std::optional<Token> NextToken();
        size_t GetLine();
    private:
        char8_t Get();
        char8_t Peek() const;
        bool NextIs(char);
        bool IsEof() const;
        size_t position = 0;
        size_t line = 1;
        std::u8string_view source;
    };
}