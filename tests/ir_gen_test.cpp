#include <csignal>
#include <iostream>
#include <ir_gen.h>
#include <parser.h>
#include <catch2/catch_test_macros.hpp>
#include "llvm/Support/InitLLVM.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
TEST_CASE("Test IR")
{
    std::string src2 = R"(
lol: module = MOO

baz: class = {
    x: lol::foo
}

takes_foo: (param: lol::foo) -> i32 = {
    a: baz;
    return 5;
}
)";
    std::string source = R"(
call_callable: (fn: called () -> f64) -> f64 = return fn.invoke();
test_impl_conv: (a: i64) -> i64 & f64 = return (a, 10);

foo: class = {
    x: bar
}
bar: class = {
    y: i32
}
main: () -> f64 = {
    /* this is meant to be in the parser /* test */ */
    d: foo;
    a: f64 = 5.0;
    b : mut = 10.0;
    b = 20;
    lambda:= |b: inout| return b;
    lol:= (10, 20, 30);
    //b = lol.0;
    test_impl_conv(10);
    call_callable(lambda);
    if(a < 10) return b;
    else if(a == b) return a;
    return 0.0;
}
)";
    int argc = 1;
    const char* argv[] = {"foo"};
    const char** lol = argv;

    Yoyo::Engine engine;
    engine.addModule("MOO", source);
    engine.addModule("MOO2", src2);
    engine.compile();

    for(auto& mod: engine.modules)
    {
        std::cout << mod.first << " --------------------------------------------------"<< "\n";
        mod.second->code->print(llvm::outs(), nullptr);
        std::cout << "----------------------------------------------------------------" << "\n";
    }
    /*
    llvm::InitLLVM llvm(argc, lol);
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    

    Yoyo::Parser p1(std::move(source));
    auto decl = p1.parseProgram();
    REQUIRE(!p1.failed());
    auto context = std::make_unique<llvm::LLVMContext>();
    Yoyo::IRGenerator gen(*context);
    auto mod = gen.GenerateIR("MOO", std::move(decl));
    mod.code->print(llvm::outs(), nullptr);
    if(verifyModule(*mod.code, &llvm::errs())) raise(SIGTRAP);

    llvm::ExitOnError ExitOnErr;
    auto j = llvm::orc::LLJITBuilder().create();
    std::ignore = j.get()->addIRModule(llvm::orc::ThreadSafeModule(std::move(mod.code), std::move(context)));
    auto addr = j.get()->lookup("main").get();
    double(*fn)() = addr.toPtr<double()>();
    auto res = fn();
    REQUIRE(res == 10.0);
    std::cout << res;
    */
}

