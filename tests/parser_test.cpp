#include <catch2/catch_test_macros.hpp>
#include "parser.h"

TEST_CASE("Test sum/factor precedence", "[parser]")
{
    Yoyo::Parser p("10 + 20 * 21");
    auto exp = p.parseExpression(0);
    auto b_exp = dynamic_cast<Yoyo::BinaryOperation*>(exp.get());
    REQUIRE(b_exp != nullptr);
    REQUIRE(b_exp->op.type == Yoyo::TokenType::Plus);
    auto b_exp2 = dynamic_cast<Yoyo::BinaryOperation*>(b_exp->rhs.get());
    REQUIRE(b_exp2 != nullptr);
    REQUIRE(b_exp2->op.type == Yoyo::TokenType::Star);
}

TEST_CASE("Test Member access and Call", "[parser]")
{
    Yoyo::Parser p("10 + 20 * 21.lol(10, 20)");
    auto exp = p.parseExpression(0);
    auto add_exp = dynamic_cast<Yoyo::BinaryOperation*>(exp.get());
    auto product_exp = dynamic_cast<Yoyo::BinaryOperation*>(add_exp->rhs.get());
    auto call_exp = dynamic_cast<Yoyo::CallOperation*>(product_exp->rhs.get());
    REQUIRE(call_exp != nullptr);
    REQUIRE(call_exp->arguments.size() == 2);
}

TEST_CASE("Test Grouping experession", "[parser]")
{
    Yoyo::Parser p("(10 + 20) * 21");
    auto exp = p.parseExpression(0);
    auto product_exp = dynamic_cast<Yoyo::BinaryOperation*>(exp.get());
    REQUIRE(product_exp->op.type == Yoyo::TokenType::Star);
    auto grp = dynamic_cast<Yoyo::GroupingExpression*>(product_exp->lhs.get());
    REQUIRE(grp != nullptr);
    REQUIRE(dynamic_cast<Yoyo::BinaryOperation*>(grp->expr.get()) != nullptr);
}