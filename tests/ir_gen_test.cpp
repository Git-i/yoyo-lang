#include <codecvt>
#include <csignal>
#include <fstream>
#include <iostream>
#include <ir_gen.h>
#include <parser.h>
#include "error.h"
#include <catch2/catch_test_macros.hpp>
#include "llvm/Support/InitLLVM.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#ifdef USE_GRAPHVIZ
#include "graphviz/gvc.h"
#endif

Yoyo::AppModule* md;

int32_t func(void* arg)
{
    std::string_view sv =  Yoyo::Engine::viewString(arg);
    std::cout << sv << std::endl;
    return 0;
}
uint32_t read_int()
{
    uint32_t val = 10;
    std::cin >> val;
    return val;
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
    md = engine.addAppModule("test");
    md->addFunction("(x: &str) -> i32", func, "print");
    md->addFunction("() -> u32", read_int, "read_uint");
    engine.addModule("source.yoyo", src2);
    engine.compile();
    engine.addDynamicLibrary("c_file.dll");
    uint32_t idx = 3;
    for(auto& mod: engine.modules)
    {
        idx += 1;
        idx %= 8;
        auto str = "\033[1;3" + std::to_string(idx) + "m";
        std::cout <<  str << std::flush;
        mod.second->dumpIR();
        std::cout << "\033[0m" << std::flush;
        if (llvm::verifyModule(*mod.second->code.getModuleUnlocked(), &llvm::errs())) Yoyo::debugbreak();
    }
    engine.prepareForExecution();
    std::string unmangled_name = engine.modules["source.yoyo"]->module_hash + "main";
    auto addr = engine.jit->lookup(unmangled_name);
    if (!addr) Yoyo::debugbreak();
    void(*fn)() = addr.get().toPtr<void()>();
    fn();
}

TEST_CASE("Error formatting", "[errors]")
{
    std::string source = 1 + R"(
main: fn = {
    variable := 100;
    variable = 40;
    var2: i32 = "Hello";
    std::print(variable);
})";
    Yoyo::Parser p(source);
    auto prg = p.parseProgram();
    REQUIRE(!p.failed());
    REQUIRE(prg.size() == 1);
    auto decl = dynamic_cast<Yoyo::FunctionDeclaration*>(prg[0].get());
    auto body = dynamic_cast<Yoyo::BlockStatement*>(decl->body.get());
    Yoyo::Error e(body->statements[1].get(), "Attempting to assign to immutable value");
    auto expr_stat = dynamic_cast<Yoyo::ExpressionStatement*>(body->statements[1].get());
    auto expr = dynamic_cast<Yoyo::BinaryOperation*>(expr_stat->expression.get());
    e.markers.emplace_back(Yoyo::SourceSpan{expr->lhs->beg, expr->lhs->end}, "Expression is immutable");
    Yoyo::SourceView vw(source, "source.yoyo");
    std::cout << e.to_string(vw, true) << std::endl;

    Yoyo::Error e2(body->statements[2].get(), "Attempting to assign between incompatible types");
    auto var_decl = dynamic_cast<Yoyo::VariableDeclaration*>(body->statements[2].get());
    Yoyo::SourceLocation end{ var_decl->beg.line, var_decl->beg.column + var_decl->identifier.text.size() };
    e2.markers.emplace_back(Yoyo::SourceSpan{ var_decl->beg, end }, "Expression is of type 'i32'");
    e2.markers.emplace_back(Yoyo::SourceSpan{ var_decl->initializer->beg, var_decl->initializer->end }, "Expression is of type 'str'");
    std::cout << e2.to_string(vw, true) << std::endl;

}

#ifdef USE_GRAPHVIZ
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

TEST_CASE("Test CFG")
{
    char name[] = "CFG";
    Yoyo::Parser p(1 + R"(
    main: fn = {
        var1: mut = std::env::args()
            .skip(1)
            .next()
            .expect("gimme a number pls")
            .parse::<i32>()
            .expect("that wasn't a number dumbass");
        var2: mut = 0;
        while (var1 > var2) {
            temp: i32;
            if (var1 % 2 == 0)
                temp = var1 / 2;
            else
                temp = var1 * 3 + 1;

            if (temp > 30 && temp < 70)
                var1 += 1;
            else
                var1 = temp;

            var2 += 1;
            if(var2 > var1)
                break;
        }
        if (var1 % 2 == 0)
            var2 += 1 + var1 / 2;
        println("{}", var2);
    }
    )");
    auto graph = agopen(name, Agdirected, nullptr);
    Yoyo::CFGNodeManager manager;
    auto tree = p.parseDeclaration();
    auto tree_as_fn = reinterpret_cast<Yoyo::FunctionDeclaration*>(tree.get());
    auto root = Yoyo::CFGNode::prepareFromFunction(manager, tree_as_fn);
    manager.annotate();
    auto print_uses = [](const decltype(manager.first_uses)& uses)
    {
        for(const auto& use : uses)
        {
            std::cout << "    For " << use.first << ":\n";
            for(auto& expr: use.second)
            {
                std::cout << std::format("        [{},{}] [{},{}]\n", expr->beg.line, expr->beg.column, expr->end.line, expr->end.column);
            }
        }
        std::cout << std::flush;
    };
    //std::cout << "First uses:\n";
    //print_uses(manager.first_uses);
    //std::cout << "Last uses:\n";
    //print_uses(manager.last_uses);
    //std::cout << std::endl;
    std::unordered_map<Yoyo::CFGNode*, Agnode_t*> nodes;
    size_t idx = 0;
    for(auto& node: manager.nodes)
    {;
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
#endif