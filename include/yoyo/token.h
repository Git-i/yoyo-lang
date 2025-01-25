#pragma once
#include <string_view>
#include "source_location.h"
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
        IntegerLiteral, RealLiteral, StringLiteral, CharLiteral, True, False,
        TemplateOpen, /// ::<
        DoubleGreater, DoubleLess,
        Class, Struct, Enum, EnumFlag, Union, Scheme, Interface, Module,
        SemiColon, In, InOut, This, Operator, Return, Called, Tilde, Pub, Static, Mod, With,
        Question, Null, As, Fn, Alias,
        AttrOpen, /// #(
        Spaceship, // <=>
        Underscore, Mut, RefMut, GCNew, Impl, Break, Continue
    };
    struct Token
    {
        TokenType type;
        SourceLocation loc;
        std::string_view text;
        [[nodiscard]] bool is_assignment() const
        {
            const auto as_int = static_cast<std::underlying_type_t<TokenType>>(type);
            return as_int >= static_cast<std::underlying_type_t<TokenType>>(TokenType::Equal) &&
                as_int <= static_cast<std::underlying_type_t<TokenType>>(TokenType::CaretEqual);
        }
        [[nodiscard]] bool can_be_overloaded() const
        {
            return type == TokenType::Plus
                || type == TokenType::Minus
                || type == TokenType::Slash
                || type == TokenType::Star
                || type == TokenType::Percent
                || type == TokenType::Ampersand
                || type == TokenType::Pipe
                || type == TokenType::Caret
                || type == TokenType::Spaceship;
        }
        [[nodiscard]] bool can_be_overloaded_binary_only() const
        {
            return type == TokenType::Plus
                || type == TokenType::Slash
                || type == TokenType::Star
                || type == TokenType::Percent
                || type == TokenType::Ampersand
                || type == TokenType::Pipe
                || type == TokenType::Caret
                || type == TokenType::Spaceship;
        }
    };
}