#include "scanner.h"

#include <csignal>
#include <cstdint>
#include <locale>
#include <optional>
#include <bit>

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
        {"gcnew", TokenType::GCNew},
        {"impl", TokenType::Impl},
        {"for", TokenType::For},
        {"break", TokenType::Break},
        {"continue", TokenType::Continue},
        {"const", TokenType::Const},
    })
    {

    }
    std::optional<Token> Scanner::NextToken()
    {
        while(!IsEof())
        {
            auto loc = GetSourceLocation();
            char c = Get();
            switch (c)
            {
            case ';': return Token{TokenType::SemiColon, loc};
            case '+': return NextIs('=') ? Token{TokenType::PlusEqual, loc, {source.data() + position - 2, 2} } : 
                Token{TokenType::Plus, loc, {source.data() + position - 1, 1} };
            case '-':
                {
                    if (NextIs('=')) return Token{TokenType::MinusEqual, loc, {source.data() + position - 2, 2} };
                    if (NextIs('>')) return Token{TokenType::Arrow, loc, {source.data() + position - 2, 2} };
                    return Token{TokenType::Minus, loc, {source.data() + position - 1, 1} };
                }
            case '*': return NextIs('=') ? Token{TokenType::StarEqual, loc, {source.data() + position - 2, 2} } :
                Token{TokenType::Star, loc, {source.data() + position - 1, 1} };
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
                    return NextIs('=') ? Token{TokenType::SlashEqual, loc, {source.data() + position - 2, 2} } : 
                        Token{TokenType::Slash, loc, {source.data() + position - 1, 1} };
                }
            case '%': return NextIs('=') ? Token{TokenType::PercentEqual, loc, {source.data() + position - 2, 2} } :
                Token{TokenType::Percent, loc, {source.data() + position - 1, 1} };
            case '!': return NextIs('=') ? Token{TokenType::BangEqual, loc, {source.data() + position - 2, 2} } : 
                Token{TokenType::Bang, loc, {source.data() + position - 1, 1} };
            case '&':
                {
                    if (NextIs('=')) return Token{TokenType::AmpersandEqual, loc, {source.data() + position - 2, 2} };
                    if (NextIs('&')) return Token{TokenType::DoubleAmpersand, loc, {source.data() + position - 2, 2} };
                    return Token{TokenType::Ampersand, loc, {source.data() + position - 1, 1} };
                }
            case '|':
                {
                    if (NextIs('=')) return Token{TokenType::PipeEqual, loc, {source.data() + position - 2, 2} };
                    if (NextIs('|')) return Token{TokenType::DoublePipe, loc, {source.data() + position - 2, 2} };
                    return Token{TokenType::Pipe, loc, {source.data() + position - 1, 1} };
                }
            case '>':
                {
                    if (NextIs('=')) return Token{TokenType::GreaterEqual, loc, {source.data() + position - 2, 2} };
                    if (NextIs('>')) return Token{TokenType::DoubleGreater, loc, {source.data() + position - 2, 2} };
                    return Token{TokenType::Greater, loc, {source.data() + position - 1, 1} };
                }
            case '<':
                {
                    if (NextIs('='))
                    {
                        if (NextIs('>')) return Token{ TokenType::Spaceship, loc, {source.data() + position - 3, 3} };
                        return Token{ TokenType::LessEqual, loc, {source.data() + position - 2, 2} };
                    }
                    if (NextIs('<')) return Token{TokenType::DoubleLess, loc, {source.data() + position - 2, 2} };
                    return Token{TokenType::Less, loc, {source.data() + position - 1, 1} };
                }
            case '(': return Token{TokenType::LParen, loc, {source.data() + position - 1, 1} };
            case ')': return Token{TokenType::RParen, loc, {source.data() + position - 1, 1} };
            case '{': return Token{TokenType::LCurly, loc, {source.data() + position - 1, 1} };
            case '}': return Token{TokenType::RCurly, loc, {source.data() + position -1, 1}};
            case '[': return Token{TokenType::LSquare, loc, {source.data() + position - 1, 1} };
            case ']': return Token{TokenType::RSquare, loc, {source.data() + position - 1, 1} };
            case '.': 
            {
                if (NextIs('.'))
                {
                    if (NextIs('=')) return Token{ TokenType::DoubleDotEqual, loc, {source.data() + position - 3, 3} };
                    return Token{ TokenType::DoubleDot, loc, {source.data() + position - 2, 2} };
                }
                return Token{ TokenType::Dot, loc, {source.data() + position - 1, 1} };
            }
            case ',': return Token{TokenType::Comma, loc, {source.data() + position - 1, 1} };
            case ':':
                {
                    if (NextIs(':'))
                    {
                        if(NextIs('<')) return Token{TokenType::TemplateOpen, loc, {source.data() + position - 3, 3} };
                        return Token{TokenType::DoubleColon, loc, {source.data() + position - 2, 2} };
                    }
                    return Token{TokenType::Colon, loc, {source.data() + position - 1, 1} };
                }
            case '^': return Token{TokenType::Caret, loc, {source.data() + position - 1, 1} };
            case '#':
                {
                    if(NextIs('(')) return Token{TokenType::AttrOpen, loc, {source.data() + position - 2, 2} };
                    return std::nullopt;
                }
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
            case '?': return Token{TokenType::Question, loc, {source.data() + position - 1, 1} };
            case '\t': [[fallthrough]];
            case ' ': [[fallthrough]];
            case '\r': [[fallthrough]];
            case '\n': break;
            case '=':
                {
                    if (NextIs('=')) return Token{TokenType::DoubleEqual, loc, {source.data() + position - 2, 2} };
                    return Token{TokenType::Equal, loc, {source.data() + position - 1, 1} };
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
        if (Peek() && Peek() == '!') { std::ignore = Get(); return { TokenType::Directive, loc, view }; };
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
        bool in_sub_string = true;
        while (!IsEof())
        {
            char c = Get();
            if(c == '"')
            {
                if(num_open_braces == 0) break;
                in_sub_string = !in_sub_string;
            }
            else if (c == '\\')
            {
                //escape the next character
                if (in_sub_string) std::ignore = Get();
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
