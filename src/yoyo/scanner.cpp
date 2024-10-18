#include "scanner.h"

#include <optional>

namespace Yoyo {
    Scanner::Scanner(std::u8string_view view) : source(view) {}
    Scanner::Scanner(std::string_view view)
    {
        source = std::u8string_view(reinterpret_cast<const char8_t*>(view.data()), view.size());
    }

    std::optional<Token> Scanner::NextToken()
    {
        while(!IsEof())
        {
            char8_t c = Get();
            switch (c)
            {
            case '+': return NextIs('=') ? Token{TokenType::PlusEqual} : Token{TokenType::Plus};
            case '-':
                {
                    if (NextIs('=')) return Token{TokenType::MinusEqual};
                    if (NextIs('>')) return Token{TokenType::Arrow};
                    return Token{TokenType::Minus};
                }
            case '*': return NextIs('=') ? Token{TokenType::StarEqual} : Token{TokenType::Star};
            case '/': return NextIs('=') ? Token{TokenType::SlashEqual} : Token{TokenType::Slash}; //todo: comments
            case '!': return NextIs('=') ? Token{TokenType::BangEqual} : Token{TokenType::Bang};
            case '&':
                {
                    if (NextIs('=')) return Token{TokenType::AmpersandEqual};
                    if (NextIs('&')) return Token{TokenType::DoubleAmpersand};
                    return Token{TokenType::Ampersand};
                }
            case '|':
                {
                    if (NextIs('=')) return Token{TokenType::PipeEqual};
                    if (NextIs('|')) return Token{TokenType::DoublePipe};
                    return Token{TokenType::Pipe};
                }
            case '(': return Token{TokenType::LParen};
            case ')': return Token{TokenType::RParen};
            case '{': return Token{TokenType::LCurly};
            case '}': return Token{TokenType::RCurly};
            case '[': return Token{TokenType::LSquare};
            case ']': return Token{TokenType::RSquare};
            case '.': return Token{TokenType::Dot};
            case ',': return Token{TokenType::Comma};
            case ':': return Token{TokenType::Colon};
            case '^': return Token{TokenType::Caret};
            case '_':
                {
                    if (const char8_t next = Peek(); std::isalnum(next))
                    {
                        std::u8string str = u8"_";
                        while (std::isalnum(Peek()))
                        {
                            str.push_back(Get());
                        }
                        return Token{TokenType::SPIdentifier};
                    }
                    return Token{TokenType::Underscore};
                }
            case '\t': [[fallthrough]];
            case ' ': [[fallthrough]];
            case '\r': break;
            case '\n': line++; break;
            default: return std::nullopt;
            }
        }
        return Token{TokenType::Eof};
    }

    char8_t Scanner::Get()
    {
        return source[position++];
    }

    char8_t Scanner::Peek() const
    {
        return source[position];
    }

    bool Scanner::NextIs(char c)
    {
        if (Peek() == c)
        {
            Get(); return true;
        }
        return false;
    }

    bool Scanner::IsEof() const
    {
        if (position >= source.size()) return true;
        return false;
    }
}
