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

TEST_CASE("Test type parsing", "[types][parser]")
{
    Yoyo::Parser p("[foo2 & foo::<bar, baz::<int>>]");
    auto type = *p.parseType(0);
    REQUIRE(type.name == "__arr");
    REQUIRE(type.subtypes[0].name == "__tup");
    REQUIRE(type.subtypes[0].subtypes[0].name == "foo2");
    REQUIRE(type.subtypes[0].subtypes[1].name == "foo");
    REQUIRE(type.subtypes[0].subtypes[1].subtypes[1].name == "baz");
}

TEST_CASE("Variable parsing", "[parser]")
{
    Yoyo::Parser p1("lol: int;");
    Yoyo::Parser p2("lol: = 100;");
    Yoyo::Parser p3("lol: _ = 100;");
    auto s1 = p1.parseDeclaration();
    auto s2 = p2.parseDeclaration();
    auto s3 = p3.parseDeclaration();

    auto decl1 = dynamic_cast<Yoyo::VariableDeclaration*>(s1.get());
    REQUIRE(decl1->identifier.text == "lol");
    REQUIRE(decl1->type->name == "int");

    auto decl2 = dynamic_cast<Yoyo::VariableDeclaration*>(s2.get());
    REQUIRE(decl2->identifier.text == "lol");
    REQUIRE(decl2->type == std::nullopt);

    auto decl3 = dynamic_cast<Yoyo::VariableDeclaration*>(s3.get());
    REQUIRE(decl3->identifier.text == "lol");
    REQUIRE(decl3->type == std::nullopt);

}

TEST_CASE("Tuple Literal vs Grouping", "[parser]")
{
    Yoyo::Parser p1("(10)");
    Yoyo::Parser p2("(20, 10.50)");
    auto exp1 = p1.parseExpression(0);
    auto exp2 = p2.parseExpression(0);

    auto grp = dynamic_cast<Yoyo::GroupingExpression*>(exp1.get());
    REQUIRE(grp != nullptr);
    auto tup = dynamic_cast<Yoyo::TupleLiteral*>(exp2.get());
    REQUIRE(tup != nullptr);
    REQUIRE(tup->elements.size() == 2);
}