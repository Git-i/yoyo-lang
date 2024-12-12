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
        {"operator", TokenType::Operator},
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
        {"null", TokenType::Null},
        {"with", TokenType::With},
        {"as", TokenType::As},
        {"fn", TokenType::Fn},
        {"alias", TokenType::Alias},
    })
    {

    }
    std::optional<Token> Scanner::NextToken()
    {
        while(!IsEof())
        {
            char c = Get();
            auto loc = GetSourceLocation();
            switch (c)
            {
            case ';': return Token{TokenType::SemiColon, loc};
            case '+': return NextIs('=') ? Token{TokenType::PlusEqual, loc} : Token{TokenType::Plus, loc};
            case '-':
                {
                    if (NextIs('=')) return Token{TokenType::MinusEqual, loc};
                    if (NextIs('>')) return Token{TokenType::Arrow, loc};
                    return Token{TokenType::Minus, loc};
                }
            case '*': return NextIs('=') ? Token{TokenType::StarEqual, loc} : Token{TokenType::Star, loc};
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
                    return NextIs('=') ? Token{TokenType::SlashEqual, loc} : Token{TokenType::Slash, loc};
                }
            case '!': return NextIs('=') ? Token{TokenType::BangEqual, loc} : Token{TokenType::Bang, loc};
            case '&':
                {
                    if (NextIs('=')) return Token{TokenType::AmpersandEqual, loc};
                    if (NextIs('&')) return Token{TokenType::DoubleAmpersand, loc};
                    return Token{TokenType::Ampersand, loc};
                }
            case '|':
                {
                    if (NextIs('=')) return Token{TokenType::PipeEqual, loc};
                    if (NextIs('|')) return Token{TokenType::DoublePipe, loc};
                    return Token{TokenType::Pipe, loc};
                }
            case '>':
                {
                    if (NextIs('=')) return Token{TokenType::GreaterEqual, loc};
                    if (NextIs('>')) return Token{TokenType::DoubleGreater, loc};
                    return Token{TokenType::Greater, loc};
                }
            case '<':
                {
                    if (NextIs('=')) return Token{TokenType::LessEqual, loc};
                    if (NextIs('<')) return Token{TokenType::DoubleLess, loc};
                    return Token{TokenType::Less, loc};
                }
            case '(': return Token{TokenType::LParen, loc};
            case ')': return Token{TokenType::RParen, loc};
            case '{': return Token{TokenType::LCurly, loc};
            case '}': return Token{TokenType::RCurly, loc, {source.begin() + position -1, 1}};
            case '[': return Token{TokenType::LSquare, loc};
            case ']': return Token{TokenType::RSquare, loc};
            case '.': return Token{TokenType::Dot, loc};
            case ',': return Token{TokenType::Comma, loc};
            case ':':
                {
                    if (NextIs(':'))
                    {
                        if(NextIs('<')) return Token{TokenType::TemplateOpen, loc};
                        return Token{TokenType::DoubleColon, loc};
                    }
                    return Token{TokenType::Colon, loc};
                }
            case '^': return Token{TokenType::Caret, loc};
            case '_':
                {
                    if (const char8_t next = Peek(); std::isalnum(next))
                    {
                        std::string str = "_";
                        while (std::isalnum(Peek()))
                        {
                            str.push_back(Get());
                        }
                        return Token{TokenType::SPIdentifier, loc};
                    }
                    return Token{TokenType::Underscore, loc};
                }
            case '?': return Token{TokenType::Question, loc};
            case '\t': [[fallthrough]];
            case ' ': [[fallthrough]];
            case '\r': [[fallthrough]];
            case '\n': break;
            case '=':
                {
                    if (NextIs('=')) return Token{TokenType::DoubleEqual, loc};
                    return Token{TokenType::Equal, loc};
                }
            case '"': return ScanStringLiteral();
            case '\'': return ScanCharLiteral();
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
        auto loc = GetSourceLocation();
        loc.column--;
        size_t iden_begin = position - 1;
        while (!IsEof() && (std::isalnum(Peek()) || Peek() == '_')) std::ignore = Get();
        auto view = std::string_view(source.begin() + iden_begin, source.begin() + position);
        if (const auto kw = keywords.find(view); kw != keywords.end())
        {
            return {kw->second, loc, view};
        }
        return {TokenType::Identifier, loc, view};
    }

    Token Scanner::ScanNumber()
    {
        auto loc = GetSourceLocation();
        loc.column--;
        size_t number_begin = position - 1;
        while (!IsEof() && std::isdigit(Peek())) std::ignore = Get();
        if (IsEof() || Peek() != '.' || position + 1 >= source.size() || !std::isdigit(PeekNext()))
            return {TokenType::IntegerLiteral, loc,
            std::string_view(source.begin() + number_begin, source.begin() + position)};
        std::ignore = Get();//discard the dot
        while (std::isdigit(Peek())) std::ignore = Get();
        return {TokenType::RealLiteral, loc,
            std::string_view(source.begin() + number_begin, source.begin() + position)};
    }
    std::optional<Token> Scanner::ScanStringLiteral()
    {
        auto loc = GetSourceLocation();
        loc.column--;
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
        return Token{TokenType::StringLiteral, loc, view};
    }

    std::optional<Token> Scanner::ScanCharLiteral()
    {
        auto loc = GetSourceLocation();
        loc.column--;
        size_t begin = position;
        size_t num_bytes = 0;
        auto next_byte = std::bit_cast<uint8_t>(Get());
        std::string_view view;

        if(next_byte <= 0x7F) num_bytes = 1;
        else if(next_byte <= 0xDF) num_bytes = 2;
        else if(next_byte <= 0xEF) num_bytes = 3;
        else if(next_byte <= 0xF7) num_bytes = 4;
        else return std::nullopt;

        for(size_t i = 1; i < num_bytes; i++) std::ignore = Get();
        view = std::string_view{source.begin() + begin, source.begin() + begin + num_bytes};
        if(Peek() == '\'')
        {
            std::ignore = Get(); return Token{TokenType::CharLiteral, loc, view};
        }
        return std::nullopt;
    }

    void Scanner::nextLine()
    {
        line++;
        col = 0;
    }

    void Scanner::handleLineComment()
    {
        while(!IsEof() && Peek() != '\n') std::ignore = Get();
        if(!IsEof())
        {
            std::ignore = Get(); //discard the new-line
        }
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


    SourceLocation Scanner::GetSourceLocation() const {return SourceLocation{line, col};}

    char Scanner::Get()
    {
        if(Peek() == '\n') nextLine();
        col++;
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
