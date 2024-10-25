#include <ir_gen.h>
#include <parser.h>
#include <catch2/catch_test_macros.hpp>
TEST_CASE("Test IR")
{
    std::string source = R"(
main: () -> f64 = {
    a: f64 = 0.0;
    b := 10.4;
    return a + b;
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
