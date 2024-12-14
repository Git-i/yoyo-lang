#include <codecvt>
#include <csignal>
#include <fstream>
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
    std::ifstream ifs("source.yoyo");
    std::ostringstream oss;
    oss << ifs.rdbuf();
    std::string src2 = oss.str();
    int argc = 1;
    const char* argv[] = {"foo"};
    const char** lol = argv;

    Yoyo::Engine engine;
    md = engine.addAppModule("APP");
    md->addFunction("(x: &str) -> i32", reinterpret_cast<void*>(&func), "func");
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
        agedge(graph, nodes[node], nodes[child], nullptr, true);
        prepare_edge(child, graph, nodes, prepared);
    }
};
void print_uses(decltype(std::declval<Yoyo::CFGNodeManager>().uses)& uses)
{
    auto& cout = std::cout;
    auto print_sloc = [&cout](Yoyo::SourceLocation loc)
    {
        cout << '[' << loc.line << ':' << loc.column << ']';
    };
    for(auto& [var, details]: uses)
    {
        std::cout << "Uses of " << var << '\n';
        for(auto& det : details)
        {
            std::cout << "    First uses occur here: \n";
            for(auto& first : det.second.first)
            {
                std::cout << "    ";
                print_sloc(first->beg);
                std::cout << ' ';
                print_sloc(first->end);
                std::cout << " by node:" << det.first;
                std::cout << '\n';
            }
            std::cout << "    Last uses occur here: \n";
            for(auto& first : det.second.second)
            {
                std::cout << "    ";
                print_sloc(first->beg);
                std::cout << ' ';
                print_sloc(first->end);
                std::cout << " by node:" << det.first;
                std::cout << '\n';
            }
        }
    }
    std::cout << std::flush;
}
TEST_CASE("Test CFG")
{
    char name[] = "CFG";
    Yoyo::Parser p(1 + R"(
        main: fn = {
            a: i32 = 0;
            while (a > 10) {
                a += 2;
                if(true) {
                    return 0;
                } else {
                    a = 10;
                }
            }
        }
    )");
    auto graph = agopen(name, Agdirected, nullptr);
    Yoyo::CFGNodeManager manager;
    auto tree = p.parseDeclaration();
    auto tree_as_fn = reinterpret_cast<Yoyo::FunctionDeclaration*>(tree.get());
    auto root = Yoyo::CFGNode::prepareFromFunction(manager, tree_as_fn);
    manager.annotate();
    print_uses(manager.uses);
    std::unordered_map<Yoyo::CFGNode*, Agnode_t*> nodes;
    size_t idx = 0;
    for(auto& node: manager.nodes)
    {
        std::string name = "Node" + std::format("{:0x}", reinterpret_cast<std::uintptr_t>(node.get()))
            + " " + node->debug_name + ": " + std::to_string(node->depth);
        nodes[node.get()] = agnode(graph, name.data(), true);
        idx += 1;
    }
    std::set<Yoyo::CFGNode*> prepared;
    prepare_edge(root, graph, nodes, prepared);
    auto f = fopen("test_result.graphviz", "w");
    agwrite(graph, f);
}