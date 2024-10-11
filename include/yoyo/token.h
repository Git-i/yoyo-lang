namespace Yoyo {
    enum class TokenType
    {
        Invalid, Eof,
        Identifier,
        If, While, For,
        Plus, Minus, Slash, Star,
        Equal, BangEqual, PlusEqual, MinusEqual, SlashEqual, StarEqual,
        Less, Greater, DoubleEqual, LessEqual, GreaterEqual,
        Colon, Arrow,
        LParen, RParen, RCurly, LCurly, LSquare, RSquare,
        Underscore
    };
    struct Token
    {
        TokenType type;
    };
}