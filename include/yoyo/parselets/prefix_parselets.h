#pragma once
#include "../expression.h"
namespace Yoyo {
    class Parser;
    class PrefixParselet {
    public:
        virtual ~PrefixParselet() = default;
        virtual std::unique_ptr<Expression> parse(Parser& parser, Token tk) = 0;
    };
    class PrefixOperationParselet : public PrefixParselet {
    public:
        std::unique_ptr<Expression> parse(Parser& parser, Token tk) override;
    };
    class RealLiteralParselet : public PrefixParselet {
    public:
        std::unique_ptr<Expression> parse(Parser& parser, Token tk) override;
    };
    class IntLiteralParselet : public PrefixParselet {
    public:
        std::unique_ptr<Expression> parse(Parser& parser, Token tk) override;
    };
    class StringLiteralParselet : public PrefixParselet {
    public:
        std::unique_ptr<Expression> parse(Parser& parser, Token tk) override;
    };
    class BoolLiteralParselet : public PrefixParselet {
    public:
        std::unique_ptr<Expression> parse(Parser& parser, Token tk) override;
    };
    class NameParselet : public PrefixParselet
    {
    public:
        std::unique_ptr<Expression> parse(Parser& parser, Token tk) override;
    };
    class GroupParselet : public PrefixParselet
    {
    public:
        std::unique_ptr<Expression> parse(Parser& parser, Token tk) override;
    };
    class ArrayLiteralParselet : public PrefixParselet
    {
    public:
        std::unique_ptr<Expression> parse(Parser& parser, Token tk) override;
    };
    class LambdaParselet : public PrefixParselet
    {
    public:
        std::unique_ptr<Expression> parse(Parser& parser, Token tk) override;
    };
    class NullLiteralParselet : public PrefixParselet
    {
    public:
        std::unique_ptr<Expression> parse(Parser& parser, Token tk) override;
    };
    class CharLiteralParselet : public PrefixParselet
    {
    public:
        std::unique_ptr<Expression> parse(Parser& parser, Token tk) override;
    };
}