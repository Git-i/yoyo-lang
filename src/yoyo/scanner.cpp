#include "scanner.h"

#include <cstdint>
#include <locale>
#include <optional>

namespace Yoyo {
    Scanner::Scanner(std::string_view view): source(view),
    keywords({
        {"if", TokenType::If},
        {"else", TokenType::Else},
        {"while", TokenType::While},
        {"in", TokenType::In},
        {"inout", TokenType::InOut},
        {"ref", TokenType::Ref},
        {"return", TokenType::Return},
        {"class", TokenType::Class},
        {"struct", TokenType::Struct},
        {"union", TokenType::Union},
        {"enum", TokenType::Enum},
        {"enum_flag", TokenType::EnumFlag},
        {"scheme", TokenType::Scheme},
        {"interface", TokenType::Interface},
        {"true", TokenType::True},
        {"false", TokenType::False},
        {"pub", TokenType::Pub},
        {"mod", TokenType::Mod},
        {"static", TokenType::Static},
        {"this", TokenType::This},
        {"mut", TokenType::Mut},
        {"called", TokenType::Called},
        {"module", TokenType::Module},

    })
    {

    }

    std::optional<Token> Scanner::NextToken()
    {
        while(!IsEof())
        {
            char c = Get();
            switch (c)
            {
            case ';': return Token{TokenType::SemiColon};
            case '+': return NextIs('=') ? Token{TokenType::PlusEqual} : Token{TokenType::Plus};
            case '-':
                {
                    if (NextIs('=')) return Token{TokenType::MinusEqual};
                    if (NextIs('>')) return Token{TokenType::Arrow};
                    return Token{TokenType::Minus};
                }
            case '*': return NextIs('=') ? Token{TokenType::StarEqual} : Token{TokenType::Star};
            case '/':
                {
                    if(NextIs('/'))
                    {
                        handleLineComment();
                        break;
                    }
                    if (NextIs('*'))
                    {
                        handleBlockComment();
                        break;
                    }
                    return NextIs('=') ? Token{TokenType::SlashEqual} : Token{TokenType::Slash};
                }
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
            case '>':
                {
                    if (NextIs('=')) return Token{TokenType::GreaterEqual};
                    if (NextIs('>')) return Token{TokenType::DoubleGreater};
                    return Token{TokenType::Greater};
                }
            case '<':
                {
                    if (NextIs('=')) return Token{TokenType::LessEqual};
                    if (NextIs('<')) return Token{TokenType::DoubleLess};
                    return Token{TokenType::Less};
                }
            case '(': return Token{TokenType::LParen};
            case ')': return Token{TokenType::RParen};
            case '{': return Token{TokenType::LCurly};
            case '}': return Token{TokenType::RCurly, {source.begin() + position -1, 1}};
            case '[': return Token{TokenType::LSquare};
            case ']': return Token{TokenType::RSquare};
            case '.': return Token{TokenType::Dot};
            case ',': return Token{TokenType::Comma};
            case ':':
                {
                    if (NextIs(':'))
                    {
                        if(NextIs('<')) return Token{TokenType::TemplateOpen};
                        return Token{TokenType::DoubleColon};
                    }
                    return Token{TokenType::Colon};
                }
            case '^': return Token{TokenType::Caret};
            case '_':
                {
                    if (const char8_t next = Peek(); std::isalnum(next))
                    {
                        std::string str = "_";
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
            case '=':
                {
                    if (NextIs('=')) return Token{TokenType::DoubleEqual};
                    return Token{TokenType::Equal};
                }
            case '"':
                {
                    return ScanStringLiteral();
                }
            
            default:
                {
                    if(std::isdigit(c)) return ScanNumber();
                    if(std::isalpha(c)) return ScanIdentifier();
                    return std::nullopt;
                }
            }
        }
        return Token{TokenType::Eof};
    }

    Token Scanner::ScanIdentifier()
    {
        size_t iden_begin = position - 1;
        while (!IsEof() && (std::isalnum(Peek()) || Peek() == '_')) std::ignore = Get();
        auto view = std::string_view(source.begin() + iden_begin, source.begin() + position);
        if (const auto kw = keywords.find(view); kw != keywords.end())
        {
            return {kw->second, view};
        }
        return {TokenType::Identifier, view};
    }

    Token Scanner::ScanNumber()
    {
        size_t number_begin = position - 1;
        while (!IsEof() && std::isdigit(Peek())) std::ignore = Get();
        if (IsEof() || Peek() != '.' || position + 1 >= source.size() || !std::isdigit(PeekNext()))
            return {TokenType::IntegerLiteral,
            std::string_view(source.begin() + number_begin, source.begin() + position)};
        std::ignore = Get();//discard the dot
        while (std::isdigit(Peek())) std::ignore = Get();
        return {TokenType::RealLiteral,
            std::string_view(source.begin() + number_begin, source.begin() + position)};
    }
    std::optional<Token> Scanner::ScanStringLiteral()
    {
        size_t iden_begin = position;
        uint32_t num_open_braces = 0;
        bool in_sub_string = false;
        while (!IsEof())
        {
            char c = Get();
            if(c == '"')
            {
                if(num_open_braces == 0) break;
                in_sub_string = !in_sub_string;
            }
            else if(c == '$')
            {
                if(IsEof()) return std::nullopt;
                if(Get() == '{') { num_open_braces++; in_sub_string = false; }
            }
            else if(c == '{')
            {
                if(num_open_braces && !in_sub_string) num_open_braces++;
            }
            else if(c == '}')
            {
                if(num_open_braces && !in_sub_string) num_open_braces--;
            }
        }
        auto view = std::string_view(source.begin() + iden_begin, source.begin() + position - 1);
        return Token{TokenType::StringLiteral, view};
    }

    void Scanner::handleLineComment()
    {
        while(!IsEof() && Peek() != '\n') std::ignore = Get();
        if(!IsEof()) std::ignore = Get(); //discard the new-line
    }

    void Scanner::handleBlockComment()
    {
        uint32_t nest_level = 1;
        while(!IsEof() && nest_level > 0)
        {
            char next = Get();
            if(next == '/' && !IsEof() && Peek() == '*')
            {
                std::ignore = Get(); nest_level++;
            }
            if(next == '*' && !IsEof() && Peek() == '/')
            {
                std::ignore = Get(); nest_level--;
            }
        }
    }


    char Scanner::Get()
    {
        return source[position++];
    }

    char Scanner::Peek() const
    {
        return source[position];
    }
    char Scanner::PeekNext() const
    {
        return source[position + 1];
    }

    bool Scanner::NextIs(char c)
    {
        if(IsEof()) return false;
        if (Peek() == c)
        {
            std::ignore = Get(); return true;
        }
        return false;
    }

    bool Scanner::IsEof() const
    {
        if (position >= source.size()) return true;
        return false;
    }
}
