#include <csignal>
#include <iostream>
#include <ir_gen.h>
#include <parser.h>
#include <catch2/catch_test_macros.hpp>
#include "llvm/Support/InitLLVM.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"

Yoyo::AppModule* md;

int32_t func(void* arg)
{
    std::string_view sv =  Yoyo::Engine::viewString(arg);
    std::cout << sv << std::endl;
    return 0;
}
TEST_CASE("Test IR")
{
    std::string src2 = R"(
lol: module = MOO //The import system is not too strong rn
app: module = APP
baz: class = {
    x: lol::foo,
    y: i32 & f64
}

val: enum = {
    abcd, efgh
}
get_int: (tup: &mut {i32 & f32}, tup2: i32) -> &mut i32 = return tup.0;
get_int_const: (tup: &{i32 & f32}, tup2: &i32) -> &i32 = return tup.0;

takes_foo: (param: i32) -> f64 = {
    a : mut i32 & f32 = (10, 20);
    get_int_const(a, a.0);
    *get_int(a, *get_int_const(a, a.0)) = 400;
    app::func("${a.0}");
    return 100;
}
)";
    std::string source = R"(
test_impl_conv: (a: i64) -> i64 & f64 = return (a, 10);
foo: class = {
    x: bar
}
bar: class = {
    y: f64
}
returns_foo: () -> foo = {
    a: mut foo;
    a.x.y = 300.0;
    return a;
}
)";

    int argc = 1;
    const char* argv[] = {"foo"};
    const char** lol = argv;

    Yoyo::Engine engine;
    md = engine.addAppModule("APP");
    md->addFunction("(x: &str) -> i32", reinterpret_cast<void*>(&func), "func");
    engine.addModule("MOO", source);
    engine.addModule("MOO2", src2);
    engine.compile();

    uint32_t idx = 3;
    for(auto& mod: engine.modules)
    {
        idx += 1;
        idx %= 8;
        auto str = "\033[1;3" + std::to_string(idx) + "m";
        std::cout <<  str << std::flush;
        mod.second->code->print(llvm::outs(), nullptr);
        std::cout << "\033[0m" << std::flush;
        
        if(verifyModule(*mod.second->code, &llvm::errs())) raise(SIGTRAP);
    }
    llvm::InitLLVM llvm(argc, lol);
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();


    Yoyo::Parser p1(std::move(source));
    auto decl = p1.parseProgram();
    REQUIRE(!p1.failed());

    llvm::ExitOnError ExitOnErr;
    auto j = llvm::orc::LLJITBuilder().create();

    auto llvm_ctx = std::unique_ptr<llvm::LLVMContext>(static_cast<llvm::LLVMContext*>(engine.llvm_context));
    auto ctx = llvm::orc::ThreadSafeContext(std::move(llvm_ctx));
    for(auto& mod: engine.modules)
    {
        std::ignore = j.get()->addIRModule(llvm::orc::ThreadSafeModule(std::move(mod.second->code), ctx));
    }
    std::string unmangled_name = engine.modules["MOO2"]->module_hash + "takes_foo";
    auto addr = j.get()->lookup(unmangled_name).get();
    double(*fn)(int) = addr.toPtr<double(int)>();
    auto res = fn(30);
    std::cout << res;
    REQUIRE(res == 10.0);
}

