#include <catch2/catch.hpp>
#include "parser.h"

#include <precedences.h>
TEST_CASE("Expression Parsing", "[parser]")
{
    Yoyo::Parser p("10 + 20 * \"lol\"!.back()");
    auto exp = p.parseExpression(Yoyo::Precedences::ScopeResolve);

}