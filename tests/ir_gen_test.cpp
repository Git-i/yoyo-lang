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
    std::string source = R"(
lol: class = {
    a: f64,
    damm: (this) -> f64 = {
        return this.a;
    }
}
dome: (b: inout f64) -> lol = {
    a: mut lol;
    b = 100.0;
    a.a = b;
    return a;
}
call_callable: (fn: callable () -> f64) -> f64 = {
    return 0.0;
}
main: () -> f64 = {
    d: mut lol;
    d.a = 10.0;
    a: f64 = 5.0;
    b : mut = 10.4;
    b = 20.5;
    lambda:= |b: inout| -> f64 { return b; };
    call_callable(b);
    if(a > b) return b;
    else if(a == b) return a;
    return a + d.a + dome(b).damm();
}
)";
    int argc = 1;
    const char* argv[] = {"foo"};
    const char** lol = argv;
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
    std::cout << fn();
}

