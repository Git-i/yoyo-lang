#include "parselets/prefix_parselets.h"


#include "parser.h"
#include "precedences.h"
namespace Yoyo
{
    std::unique_ptr<Expression> PrefixOperationParselet::parse(Parser& parser, Token tk)
    {
        //prefix operations are right associative
        auto expr = parser.parseExpression(Precedences::Prefix - 1);
        return std::make_unique<PrefixOperation>(tk, std::move(expr));
    }
    std::unique_ptr<Expression> IntLiteralParselet::parse(Parser& parser, Token tk)
    {
        return std::make_unique<IntegerLiteral>(tk);
    }
    std::unique_ptr<Expression> RealLiteralParselet::parse(Parser& parser, Token tk)
    {
        return std::make_unique<RealLiteral>(tk);
    }
    std::unique_ptr<Expression> StringLiteralParselet::parse(Parser& parser, Token tk)
    {
        return std::make_unique<StringLiteral>(tk);
    }
    std::unique_ptr<Expression> NameParselet::parse(Parser& parser, Token tk)
    {
        return std::make_unique<NameExpression>(tk);
    }
    std::unique_ptr<Expression> GroupParselet::parse(Parser& parser, Token tk)
    {
        //empty tuple
        if(parser.discard(TokenType::RParen))
            return std::make_unique<TupleLiteral>(std::vector<std::unique_ptr<Expression>>{});
        auto expr = parser.parseExpression(0);
        if(!expr) parser.synchronizeTo({{TokenType::RParen, TokenType::Comma}});
        bool is_tuple = false;
        std::vector<std::unique_ptr<Expression>> expressions;
        while(parser.discard(TokenType::Comma))
        {
            expressions.push_back(std::move(expr));
            is_tuple = true;
            expr = parser.parseExpression(0);
            if(!expr) parser.synchronizeTo({{TokenType::RParen, TokenType::Comma}});
        }
        if(is_tuple) expressions.push_back(std::move(expr));
        if(!parser.discard(TokenType::RParen))
        {
            parser.error("Expected ')'", parser.Peek());
        }
        return is_tuple ? static_cast<std::unique_ptr<Expression>>(std::make_unique<TupleLiteral>(std::move(expressions)))
            : static_cast<std::unique_ptr<Expression>>(std::make_unique<GroupingExpression>(std::move(expr)));
    }
    std::unique_ptr<Expression> ArrayLiteralParselet::parse(Parser& parser, Token tk)
    {
        if(parser.discard(TokenType::RSquare))
            return std::make_unique<ArrayLiteral>(std::vector<std::unique_ptr<Expression>>{});
        auto expr = parser.parseExpression(0);
        if(!expr) parser.synchronizeTo({{TokenType::RSquare, TokenType::Comma}});
        std::vector<std::unique_ptr<Expression>> expressions;
        expressions.push_back(std::move(expr));
        while(parser.discard(TokenType::Comma))
        {
            expr = parser.parseExpression(0);
            if(!expr) parser.synchronizeTo({{TokenType::RSquare, TokenType::Comma}});
            expressions.push_back(std::move(expr));
        }
        if(!parser.discard(TokenType::RSquare))
        {
            parser.error("Expected ']'", parser.Peek());
        }
        return std::make_unique<ArrayLiteral>(std::move(expressions));
    }

    std::unique_ptr<Expression> BoolLiteralParselet::parse(Parser& parser, Token tk)
    {
        return std::make_unique<BooleanLiteral>(tk);
    }



}