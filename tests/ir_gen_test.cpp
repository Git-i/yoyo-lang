#include <codecvt>
#include <csignal>
#include <iostream>
#include <ir_gen.h>
#include <parser.h>
#include <catch2/catch_test_macros.hpp>
#include "llvm/Support/InitLLVM.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "graphviz/gvc.h"

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

Vec2: struct = {
    x: f32, y: f32,
    new: fn -> Vec2 = return Vec2{ .x = 90, .y = 0 };
}
operator: +(lhs: Vec2, rhs: Vec2) -> Vec2 = return Vec2{ .x = lhs.x + rhs.x, .y = lhs.y + rhs.y };
operator: -(lhs: Vec2, rhs: Vec2) -> Vec2 = return Vec2{ .x = lhs.x - rhs.x, .y = lhs.y - rhs.y };
operator: *(lhs: Vec2, rhs: Vec2) -> Vec2 = return Vec2{ .x = lhs.x * rhs.x, .y = lhs.y * rhs.y };
operator: /(lhs: Vec2, rhs: Vec2) -> Vec2 = return Vec2{ .x = lhs.x / rhs.x, .y = lhs.y / rhs.y };
print: fn(v: &Vec2) = app::func(&"${v.x}, ${v.y}");

takes_foo: fn -> f64 = {
    a := Vec2{ .x = 10, .y = 20 };
    b := Vec2{ .x = 5, .y = 6 };
    (a + b).print();
    (a - b).print();
    (a * b).print();
    (a / b).print();
    return 0;
}
)";
    std::string source = R"(
test_impl_conv: fn(a: i64) -> (i64, f64) = return (a, 10);
foo: class = {
    x: bar
}
bar: class = {
    y: f64
}
returns_foo: fn -> foo = {
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
    auto j = ExitOnErr(llvm::orc::LLJITBuilder().create());

    auto llvm_ctx = std::unique_ptr<llvm::LLVMContext>(static_cast<llvm::LLVMContext*>(engine.llvm_context));
    auto ctx = llvm::orc::ThreadSafeContext(std::move(llvm_ctx));
    for(auto& mod: engine.modules)
    {
        auto err = j->addIRModule(llvm::orc::ThreadSafeModule(std::move(mod.second->code), ctx));
        if(err)
        {
            raise(SIGTRAP);
        }
    }
    std::string unmangled_name = engine.modules["MOO2"]->module_hash + "takes_foo";
    auto addr = j->lookup(unmangled_name).get();
    double(*fn)() = addr.toPtr<double()>();
    auto res = fn();
    std::cout << res;
    REQUIRE(res == 10.0);
}


void prepare_edge(Yoyo::CFGNode* node, Agraph_t* graph, std::unordered_map<Yoyo::CFGNode*, Agnode_t*>& nodes, std::set<Yoyo::CFGNode*>& prepared)
{
    if(prepared.contains(node)) return;
    prepared.insert(node);
    for(auto child: node->children)
    {
        agedge(graph, nodes[node], nodes[child], nullptr, TRUE);
        prepare_edge(child, graph, nodes, prepared);
    }
};
TEST_CASE("Test CFG")
{
    char name[] = "CFG";
    Yoyo::Parser p(R"(
        main: fn = {
            a := 0;
            b := 0;
            if(a > b) {
                a = 10;
            } else {
                b = 10;
                if(b == 10) return 8;
            }
            while(a > 2) { 10 + 10; }
            with(a as &mut expr) {
                if |val| (a) a = 10;
                return 90;
            }
        }
    )");
    auto graph = agopen(name, Agdirected, nullptr);
    Yoyo::CFGNodeManager manager;
    auto root = Yoyo::CFGNode::prepareFromFunction(manager,
        dynamic_cast<Yoyo::FunctionDeclaration*>(p.parseDeclaration().get()));
    std::unordered_map<Yoyo::CFGNode*, Agnode_t*> nodes;
    size_t idx = 0;
    for(auto& node: manager.nodes)
    {
        std::string name = "Node" + std::to_string(idx) + " " + node->debug_name + ": " + std::to_string(node->depth);
        nodes[node.get()] = agnode(graph, name.data(), true);
        idx += 1;
    }
    std::set<Yoyo::CFGNode*> prepared;
    prepare_edge(root, graph, nodes, prepared);
    auto f = fopen("test_result.graphviz", "w");
    agwrite(graph, f);
}