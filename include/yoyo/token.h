#pragma once
namespace Yoyo {
    enum class TokenType
    {
        Invalid, Eof,
        Identifier, SPIdentifier,
        If, While, For,
        Plus, Minus, Slash, Star, Bang, Percent,
        Equal, BangEqual, PlusEqual, MinusEqual, SlashEqual, StarEqual, PercentEqual,
        Less, Greater, DoubleEqual, LessEqual, GreaterEqual,
        Colon, Arrow,
        LParen, RParen, RCurly, LCurly, LSquare, RSquare,
        Dot, Comma,
        Ampersand, Pipe, DoubleAmpersand, DoublePipe,
        AmpersandEqual, PipeEqual,
        Caret, CaretEqual,
        IntegerLiteral, RealLiteral, StringLiteral,
        Underscore
    };
    struct Token
    {
        TokenType type;
        std::string_view text;
    };
}