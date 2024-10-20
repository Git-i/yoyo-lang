#pragma once
#include <string_view>
namespace Yoyo {
    enum class TokenType
    {
        Invalid, Eof,
        Identifier, SPIdentifier,
        If, Else, While, For,
        Plus, Minus, Slash, Star, Bang, Percent,
        Equal, BangEqual, PlusEqual, MinusEqual, SlashEqual, StarEqual, PercentEqual,
        Less, Greater, DoubleEqual, LessEqual, GreaterEqual,
        DoubleColon, Colon, Arrow,
        LParen, RParen, RCurly, LCurly, LSquare, RSquare,
        Dot, Comma,
        Ampersand, Pipe, DoubleAmpersand, DoublePipe,
        AmpersandEqual, PipeEqual,
        Caret, CaretEqual,
        IntegerLiteral, RealLiteral, StringLiteral, True, False,
        TemplateOpen, /// ::<
        DoubleGreater, DoubleLess,
        Class, Struct, Enum, EnumFlag, Union, Scheme, Interface,
        SemiColon, In, InOut, This, Ref, Return,
        Underscore
    };
    struct Token
    {
        TokenType type;
        std::string_view text;
    };
}