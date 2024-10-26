#include <ir_gen.h>
#include <parser.h>
#include <catch2/catch_test_macros.hpp>
TEST_CASE("Test IR")
{
    std::string source = R"(
lol: class = {
        a: f64,
    }
dome: () -> f64 = {
    return 0.5;
}
main: () -> f64 = {
    d: lol;
    a: f64 = 0.0;
    b := 10.4;
    if(a > b) return b;
    else if(a == b) return a;
    return a + d.a + dome();
}
)";
    Yoyo::Parser p1(std::move(source));
    auto decl = p1.parseProgram();
    REQUIRE(!p1.failed());
    llvm::LLVMContext context;
    Yoyo::IRGenerator gen(context);
    auto mod = gen.GenerateIR("MOO", std::move(decl));
    mod.code->print(llvm::errs(), nullptr);
}
