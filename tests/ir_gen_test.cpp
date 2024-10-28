#include <ir_gen.h>
#include <parser.h>
#include <catch2/catch_test_macros.hpp>
TEST_CASE("Test IR")
{
    std::string source = R"(
lol: class = {
    a: f64,
    damm: (this) -> f64 = {
        return this.a;
    }
}
dome: (b: inout f64) -> lol = {
    a: lol;
    b = 100.0;
    return a;
}
main: () -> f64 = {
    d: lol;
    a: f64 = 0.0;
    b : mut = 10.4;
    b = 20.5;
    if(a > b) return b;
    else if(a == b) return a;
    return a + d.a + dome(b).damm();
}
)";
    Yoyo::Parser p1(std::move(source));
    auto decl = p1.parseProgram();
    REQUIRE(!p1.failed());
    llvm::LLVMContext context;
    Yoyo::IRGenerator gen(context);
    auto mod = gen.GenerateIR("MOO", std::move(decl));
    mod.code->print(llvm::outs(), nullptr);
}
