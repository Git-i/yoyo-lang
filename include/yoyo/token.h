namespace Yoyo {
    enum class TokenType
    {
        Invalid, Eof,
        Identifier, SPIdentifier,
        If, While, For,
        Plus, Minus, Slash, Star, Bang,
        Equal, BangEqual, PlusEqual, MinusEqual, SlashEqual, StarEqual,
        Less, Greater, DoubleEqual, LessEqual, GreaterEqual,
        Colon, Arrow,
        LParen, RParen, RCurly, LCurly, LSquare, RSquare,
        Dot, Comma,
        Ampersand, Pipe, DoubleAmpersand, DoublePipe,
        AmpersandEqual, PipeEqual,
        Caret,
        Underscore
    };
    struct Token
    {
        TokenType type;
    };
}