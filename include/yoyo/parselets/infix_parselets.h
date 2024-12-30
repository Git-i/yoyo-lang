#pragma once
#include "../expression.h"
namespace Yoyo {
    class Parser;
    class InfixParselet
    {
    public:
        virtual ~InfixParselet() = default;
        virtual std::unique_ptr<Expression> parse(Parser& parser, std::unique_ptr<Expression> left, Token tk) = 0;
        virtual uint32_t precedence() = 0;
    };
    class BinaryOperationParselet : public InfixParselet
    {
        uint32_t prec;
        bool is_right;
    public:
        std::unique_ptr<Expression> parse(Parser& parser, std::unique_ptr<Expression> left, Token tk) override;
        uint32_t precedence() override {return prec;}
        explicit BinaryOperationParselet(uint32_t precedence, bool is_r = false) : prec(precedence), is_right(is_r) {}

    };
    class LogicalOperationParselet : public InfixParselet
    {
        uint32_t prec;
        bool is_right;
    public:
        std::unique_ptr<Expression> parse(Parser& parser, std::unique_ptr<Expression> left, Token tk) override;
        uint32_t precedence() override {return prec;}
        explicit LogicalOperationParselet(uint32_t precedence, bool is_r = false) : prec(precedence), is_right(is_r) {}
    };
    class PostfixOperationParselet : public InfixParselet
    {
        uint32_t prec;
    public:
        uint32_t precedence() override {return prec;}
        std::unique_ptr<Expression> parse(Parser& parser, std::unique_ptr<Expression> left, Token tk) override;
        explicit PostfixOperationParselet(uint32_t precedence) : prec(precedence) {}
    };
    class CallOperationParselet : public InfixParselet
    {
        uint32_t prec;
    public:
        uint32_t precedence() override {return prec;}
        explicit CallOperationParselet(uint32_t precedence) : prec(precedence) {}
        std::unique_ptr<Expression> parse(Parser& parser, std::unique_ptr<Expression> left, Token tk) override;
    };
    class ScopeOperationParselet: public InfixParselet
    {
        uint32_t prec;
    public:
        uint32_t precedence() override {return prec;}
        explicit ScopeOperationParselet(uint32_t precedence) : prec(precedence) {}
        std::unique_ptr<Expression> parse(Parser& parser, std::unique_ptr<Expression> left, Token tk) override;
    };
    class AsExpressionParselet: public InfixParselet
    {
        uint32_t prec;
    public:
        uint32_t precedence() override {return prec;}
        explicit AsExpressionParselet(uint32_t precedence) : prec(precedence) {}
        std::unique_ptr<Expression> parse(Parser& parser, std::unique_ptr<Expression> left, Token tk) override;
    };

}