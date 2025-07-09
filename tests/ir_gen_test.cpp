#include <codecvt>
#include <csignal>
#include <fstream>
#include <iostream>
#include <ir_gen.h>
#include <parser.h>
#include "error.h"
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_all.hpp>
#ifdef USE_GRAPHVIZ
#include "graphviz/gvc.h"
#endif
#include <yvm/yvm_engine.h>
#include <yvm/app_module.h>
#include <ranges>

struct YoyoString {
    char* text;
    uint64_t size;
    uint64_t capacity;
};
int32_t func(void* arg)
{
    std::string_view sv =  Yoyo::Engine::viewString(arg);
    std::cout << sv << std::endl;
    return -76;
}
void test_assert(bool cond, void* string) {
    struct AssertMatcher : Catch::Matchers::MatcherBase<bool> {
    public:
        void* in_str;
        AssertMatcher(void* str) : in_str(str) {}
        bool match(const bool& arg) const override {
            return arg;
        }
        std::string describe() const override {
            return "-> " + std::string(Yoyo::Engine::viewString(in_str));
        }
    };
    REQUIRE_THAT(cond, AssertMatcher{string});
}
void print_int(uint64_t arg) {
    std::cout << arg << std::endl;
}
void get_string(void* out) {
    

    auto as_str = reinterpret_cast<YoyoString*>(out);
    as_str->text = static_cast<char*>(malloc(15));
    as_str->size = 15;
    as_str->capacity = 15;
    strcpy(as_str->text, "Hello from c++");
}
uint32_t read_int()
{
    uint32_t val = 10;
    std::cin >> val;
    return val;
}
int32_t random_int(int32_t low, int32_t high) {
    return rand() % (high - low + 1) + low;
}
int32_t cast_integer(uint64_t val) {
    return val;
}

Yoyo::Fiber createFiberFor(Yoyo::ModuleBase* mod, const std::string& function_name) {
    auto eng = dynamic_cast<Yoyo::YVMEngine*>(mod->engine);
    return eng->createFiber(eng->findFunction(mod, function_name).value());
}
void addTestModule(Yoyo::YVMEngine* eng) {
    auto md = eng->addAppModule("test");
    md->addFunction("(x: &str) -> i32", func, "print");
    md->addFunction("(x: bool, y: &str) -> void", test_assert, "assert");
}
constexpr bool emit_ir = false;
TEST_CASE("Test IR") 
{
    std::ifstream ifs("source.yoyo");
    std::ostringstream oss;
    oss << ifs.rdbuf();
    std::string src2 = oss.str();
    int argc = 1;
    const char* argv[] = {"foo"};
    const char** lol = argv;

    std::ifstream raylib_file("raylib-yoyo/module.yoyo");
    std::ostringstream oss2;
    oss2 << raylib_file.rdbuf();
    std::string raylib_src = oss2.str();

    Yoyo::YVMEngine engine;
    auto md = engine.addAppModule("test");
    md->addFunction("() -> i32", static_cast<int32_t(*)()>([]() -> int32_t { return -50; }), "get_int");
    md->addFunction("(x: u64) -> void", print_int, "print_int");
    md->addFunction("(x: &str) -> i32", func, "print");
    md->addFunction("() -> str", get_string, "get_string");
    //md->addFunction("() -> u32", read_int, "read_uint");
    //md->addFunction("(:i32, :i32) -> i32", random_int, "random_int");
    //md->addFunction("(:u64) -> i32", cast_integer, "unsafe_int_cast");
    auto src_md = engine.addModule("source.yoyo", src2);
    //engine.addModule("rl", raylib_src);
    engine.addDynamicLibrary("c_file.dll");
    engine.compile();
    uint32_t idx = 3;
    engine.prepareForExecution();
    if constexpr (emit_ir) {
        for (auto& mod : engine.modules)
        {
            idx += 1;
            idx %= 8;
            auto str = "\033[1;3" + std::to_string(idx) + "m";
            std::cout << str << std::flush;
            std::cout << reinterpret_cast<Yoyo::YVMModule*>(mod.second.get())->dumpIR() << std::endl;
            std::cout << "\033[0m" << std::flush;
            //if (llvm::verifyModule(*mod.second->code.getModuleUnlocked(), &llvm::errs())) Yoyo::debugbreak();
        }
    }
    
    std::string func_2_name = src_md->module_hash + "func_2";
    auto fn2 = engine.findFunction(src_md, func_2_name).value();
    auto fib2 = engine.createFiber(fn2);
    engine.execute();
}
TEST_CASE("Index Operator", "[operators]")
{
    std::string source(1 + R"(
I32_Index: struct = {}
operator: [](obj: &I32_Index, arg: i32) -> i32 = return arg;
main: fn(inp: i32) = {
    a := I32_Index{};
    test::assert(a[inp] == inp, &"a[inp] == inp");
}
)");
    Yoyo::YVMEngine engine;
    addTestModule(&engine);
    auto mod = engine.addModule("source", source);
    REQUIRE(engine.compile());
    engine.prepareForExecution();
    for (auto i : std::views::iota(0i32, 10i32)) {
        auto fib = createFiberFor(mod, "source::main");
        *(int32_t*)fib.parameters = i;
        engine.execute();
    }
}
TEST_CASE("Mutable Index Operator", "[operators]")
{
    std::string source(1 + R"(
Indexer: struct = { val: u32 }
operator: mut [](obj: &mut u32, arg: Indexer) = *obj = arg.val;
operator: [](obj: &u32, arg: Indexer) = return;
main: fn(val: u32) = {
    b: mut u32 = 100;
    b[Indexer{.val}];
    test::assert(b == val, &"b == val");
}
)");
    Yoyo::YVMEngine engine;
    addTestModule(&engine);
    auto mod = engine.addModule("source", source);
    REQUIRE(engine.compile());
    engine.prepareForExecution();
    for (uint32_t i : std::views::iota(0u, 10u)) {
        auto fib = createFiberFor(mod, "source::main");
        *(uint32_t*)fib.parameters = i;
        engine.execute();
    }
}
TEST_CASE("Test union initialization", "[operators]")
{
    std::string source(1 + R"(
Color: union = {
    ColorRGB: struct = { r: u8, g: u8, b: u8, a: u8 }
    
    RGB: ColorRGB,
    Hex: u32,

    to_str: fn(&this) -> str = {
        if |rgb| ((*this) as RGB) return "rgb: ${rgb.r}, ${rgb.g}, ${rgb.b}";
        else if |hex| ((*this) as Hex) return "hex: ${hex}";
        else return "Undefined";
    }
}
main: fn = {
    c1 := Color::RGB(Color::ColorRGB{.r = 10, .g = 20, .b = 22, .a = 40});
    c2 := COlor::Hex(12456);
    test::print(c1.to_str());
    test::print(c2.to_str());
}
)");
    Yoyo::YVMEngine engine;
    addTestModule(&engine);
    auto mod = engine.addModule("source", source);
    REQUIRE(engine.compile());
    engine.prepareForExecution();
    auto fib = createFiberFor(mod, "source::main");
    engine.execute();
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