#pragma once
#include <string_view>
namespace Yoyo {
    enum class TokenType
    {
        Invalid, Eof, NewLine,
        Identifier, SPIdentifier,
        If, Else, While, For,
        Plus, Minus, Slash, Star, Bang, Percent,
        Equal, BangEqual, PlusEqual, MinusEqual, SlashEqual, StarEqual, PercentEqual,
        AmpersandEqual, PipeEqual,CaretEqual,
        Less, Greater, DoubleEqual, LessEqual, GreaterEqual,
        DoubleColon, Colon, Arrow,
        LParen, RParen, RCurly, LCurly, LSquare, RSquare,
        Dot, Comma,
        Ampersand, Pipe, DoubleAmpersand, DoublePipe,
        Caret,
        IntegerLiteral, RealLiteral, StringLiteral, True, False,
        TemplateOpen, /// ::<
        DoubleGreater, DoubleLess,
        Class, Struct, Enum, EnumFlag, Union, Scheme, Interface,
        SemiColon, In, InOut, This, Ref, Return, Tilde, Pub, Static, Mod,
        Underscore, Mut,
    };
    struct Token
    {
        TokenType type;
        std::string_view text;
        [[nodiscard]] bool is_assignment() const
        {
            const auto as_int = static_cast<std::underlying_type_t<TokenType>>(type);
            return as_int >= static_cast<std::underlying_type_t<TokenType>>(TokenType::Equal) &&
                as_int <= static_cast<std::underlying_type_t<TokenType>>(TokenType::CaretEqual);
        }
    };
}