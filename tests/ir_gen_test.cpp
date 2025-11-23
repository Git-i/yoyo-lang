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
void test_str_cmp(void* str1, void* str2) {
    auto sv1 = Yoyo::Engine::viewString(str1);
    auto sv2 = Yoyo::Engine::viewString(str2);
    CHECK(sv1 == sv2);
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
Yoyo::YVMAppModule* addTestModule(Yoyo::YVMEngine* eng) {
    auto md = eng->addAppModule("test");
    md->addFunction("(x: &str) -> i32", func, "print");
    md->addFunction("(x: bool, y: &str) -> void", test_assert, "assert");
    md->addFunction("(x: &str, y: &str) -> void", test_str_cmp, "str_cmp");
    return md;
}
constexpr bool emit_ir = true;
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
TEST_CASE("Index Operator", "[operators][array][static-array]")
{
    std::string source(1 + R"(
ColorType: enum = { Red, Green, Blue }
ColorValue: struct = {
    red_val: i32,
    green_val: i32,
    blue_val: i32
}
operator: [](obj: &ColorValue, arg: ColorType) -> &i32 = return &obj.red_val;
main: fn(inp: i32) = {
    a : mut = ColorValue{
        .red_val = inp,
        .green_val = 100,
        .blue_val = 50
    };
    test::assert(a[ColorType::Red] == inp, &"a[inp] == inp");

    //arrays have special index operators that must be tested separately
    arr: [i32; 10] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
    test::assert(arr[test::i32_to_u64(inp)] == inp, &"arr[inp] == inp");
}
)");
    Yoyo::YVMEngine engine;
    auto test_mod = addTestModule(&engine);
    test_mod->addFunction("(x: i32) -> u64", static_cast<uint64_t(*)(int32_t)>([](int32_t x) -> uint64_t { return x; }), "i32_to_u64");
    auto mod = engine.addModule("source", source);
    REQUIRE(engine.compile());
    engine.prepareForExecution();
    if constexpr (emit_ir) std::cout << reinterpret_cast<Yoyo::YVMModule*>(mod)->dumpIR() << std::flush;
    for (auto i : std::views::iota(0i32, 10i32)) {
        auto fib = createFiberFor(mod, "source::main");
        *(int32_t*)fib.parameters = i;
        engine.execute();
    }
}
TEST_CASE("Mutable Index Operator", "[operators]")
{
    // This test case is wrong, TODO
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
TEST_CASE("Test union initialization", "[unions]")
{
    std::string source(1 + R"(
Color: union = {
    ColorRGB: struct = { r: u8, g: u8, b: u8, a: u8 }
    
    RGB: ColorRGB,
    Hex: u32,

    to_str: fn(&this) -> str = {
        if |rgb| ((*this) as RGB) return "rgb: ${rgb.r}, ${rgb.g}, ${rgb.b}, ${rgb.a}";
        else if |hex| ((*this) as Hex) return "hex: ${hex}";
        else return "Undefined";
    }
}
main: fn = {
    c1 := Color::RGB(Color::ColorRGB{.r = 10, .g = 20, .b = 22, .a = 40});
    c2 := Color::Hex(12456);
    test::str_cmp(&c1.to_str(), &"rgb: 10, 20, 22, 40");
    test::str_cmp(&c2.to_str(), &"hex: 12456");
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
TEST_CASE("Test garbage collected refcells", "[gc][can_panic]")
{
    std::string source(1 + R"(
func: fn(x: &mut i32, y: &mut i32) = return;
main: fn = {
    b: i32 = 10;
    a: ^i32 = gcnew b;
    test::print(&"before panic");
    func(a, a);
    test::print(&"after panic");
}
)");
    Yoyo::YVMEngine engine;
    addTestModule(&engine);
    auto mod = engine.addModule("source", source);
    REQUIRE(engine.compile());
    engine.prepareForExecution();
    if constexpr (emit_ir) std::cout << reinterpret_cast<Yoyo::YVMModule*>(mod)->dumpIR() << std::flush;
    auto fib = createFiberFor(mod, "source::main");
    engine.execute();
}
TEST_CASE("Test borrow operator", "[operator][type-checker]")
{
    std::string source(1 + R"(
Struct: struct = {
    func: fn(&this) = return;
}
Struct2: struct = {
    func2: fn(&this) = return;
}
Wrapper: struct::<T> = {
    data: T
}
operator: &::<T>(input: &Wrapper::<T>) -> &T = {
    return &input.data;
}
main: fn = {
    a := Wrapper{ .data = Struct{} };
    a.func();
    b := Wrapper{ .data = Struct2{} };
    b.func2();
}
)");
    Yoyo::YVMEngine engine;
    addTestModule(&engine);
    auto mod = engine.addModule("source", source);
    REQUIRE(engine.compile());
    engine.prepareForExecution();
    if constexpr (emit_ir) std::cout << reinterpret_cast<Yoyo::YVMModule*>(mod)->dumpIR() << std::flush;
    auto fib = createFiberFor(mod, "source::main");
    engine.execute();
}
TEST_CASE("Test static array", "[array][static_array]")
{
    std::string source(1 + R"(
read_value: fn(val: &u32) = return;
something: fn -> bool = return true;
main: fn = {
    x: mut = 22;
    y: mut = 44;
    p: mut &u32 = &x;
    //y = y + 1;                 
    q: mut &u32 = &y;
    if(something()) {
        p = q;              
        //x = x + 1;             
    } else {
        //y = y + 1;             
    }               
    read_value(p);
    
    array := [p, q];
    array2: [&u32; 2] = [&0, &1];

    array_ref: &[ &u32; 2 ] = if(something()) { &array } else { &array2 };
}
)");
    Yoyo::YVMEngine engine;
    auto test_md = addTestModule(&engine);
    test_md->addFunction("(x: &[i32; 10]) -> void", static_cast<void(*)(void*)>([](void* arr) { 
        for (auto i : std::views::iota(0, 10)) REQUIRE(i + 1 == reinterpret_cast<uint32_t*>(arr)[i]);
    }), "check_array");
    auto mod = engine.addModule("source", source);
    REQUIRE(engine.compile());
    engine.prepareForExecution();
    if constexpr (emit_ir) std::cout << reinterpret_cast<Yoyo::YVMModule*>(mod)->dumpIR() << std::flush;
    auto fib = createFiberFor(mod, "source::main");
    engine.execute();
}
TEST_CASE("Test using statement", "[using][scope]")
{
    std::string source(1 + R"(
Module1: struct = {
    Type: struct = {
        to_str: fn(&this) -> str = return "Module 1 type";
    }
}
Module2: struct = {
    TypeWrapper: struct = {
        Type: struct = {
            to_str: fn(&this) -> str = return "Module 2 type";
        }
        Type2: struct = {
            to_str: fn(&this) -> str = return "Module 2 type 2";
        }
    }
}
Module3: struct = {
    Type: struct = { to_str: fn(&this) -> str = return "Module 3 type"; }
}
using test::print;
main: fn = {
    {
        using Module1::Type;
        print(&Type{}.to_str());
    }
    {
        using Module2::{
            TypeWrapper::{Type, Type2}
        };
        print(&Type{}.to_str());
        print(&Type2{}.to_str());
    }
    {
        using Module3::*;
        print(&Type{}.to_str());
    }
}
)");
    Yoyo::YVMEngine engine;
    addTestModule(&engine);
    auto mod = engine.addModule("source", source);
    REQUIRE(engine.compile());
    engine.prepareForExecution();
    if constexpr (emit_ir) std::cout << reinterpret_cast<Yoyo::YVMModule*>(mod)->dumpIR() << std::flush;
    auto fib = createFiberFor(mod, "source::main");
    engine.execute();
}
TEST_CASE("Simple borrow checker", "[borrow-checker]") {
    std::string source(1 + R"(
GenericType: class::<T> = {
    value: T,
    new: fn -> GenericType::<T> = return;
    use: fn(&this) -> T = return this.value;
}
Type: struct = {
    func: fn(param: &str) = return;
}
main: fn = {
    x := GenericType::new();
    y := x.use();
    y.Type::func();
    test::print(&x.value);
}
)");
    Yoyo::YVMEngine engine;
    addTestModule(&engine);
    auto mod = engine.addModule("source", source);
    REQUIRE(engine.compile());
    engine.prepareForExecution();
    if constexpr (emit_ir) std::cout << reinterpret_cast<Yoyo::YVMModule*>(mod)->dumpIR() << std::flush;
    auto fib = createFiberFor(mod, "source::main");
    engine.execute();
}
TEST_CASE("Test Results", "[type-checker], [result-type]") {
    std::string source(1 + R"(
// test::return_result: fn -> [i64; 10] \ str;
constant: const i32 = 10;
main: fn -> i32 \ str = {
    result: mut = test::return_result();
    obj := if(true) {
        result.try
    } else {
        [10; constant]
    }
}
)");
    Yoyo::YVMEngine engine;
    auto test_mod = addTestModule(&engine);
    test_mod->addFunction("-> [i64; 10] \\ str", static_cast<void(*)(void*)>([](void* in) {}), "return_result");
    auto mod = engine.addModule("source", source);
    REQUIRE(engine.compile());
    engine.prepareForExecution();
    if constexpr (emit_ir) std::cout << reinterpret_cast<Yoyo::YVMModule*>(mod)->dumpIR() << std::flush;
    auto fib = createFiberFor(mod, "source::main");
    engine.execute();
}
TEST_CASE("Test if expression", "[expressions], [if-expression]") {
    std::string source(1 + R"(
produce: fn::<T>(val: T) -> T = return val; 
print_i32: fn(val: i32) = "${val}".test::print();
main: fn -> bool = {
    result: mut = if(true) {
        val := 10;
        print_i32(val);
        val
    } else {
        produce(10)
    }
    cond := true; cond2 := false; // they can be any bools
    var9 := if (cond){
        if(cond2) {
            return false;
        } else {
            "true and false"
        }
    } else {
        "false and false"
    };
    var9.test::print();
    print_i32(result);
    return true;
}
)");
    Yoyo::YVMEngine engine;
    auto test_mod = addTestModule(&engine);
    auto mod = engine.addModule("source", source);
    REQUIRE(engine.compile());
    engine.prepareForExecution();
    if constexpr (emit_ir) std::cout << reinterpret_cast<Yoyo::YVMModule*>(mod)->dumpIR() << std::flush;
    auto fib = createFiberFor(mod, "source::main");
    engine.execute();
}
TEST_CASE("Test interfaces", "[type-checker], [interfaces]") {
    std::string source(1 + R"(
takes_intf: fn::<T: impl Interface>(arg: T) = return;
takes_generic_intf: fn::<O, T: impl InterfaceWrapper::<O>::Interface>(arg: T) -> O = return; 
main: fn = {
    obj1 := Generic1::new();
    obj2 := Generic2::new();
    obj3: i32 = takes_generic_intf(obj1);
    takes_intf(obj1);
    takes_intf(obj2);
    obj2.take_t(obj3);
}
Generic1: struct::<T> = {
    impl Interface {
    }
    impl InterfaceWrapper::<T>::Interface {
    }
    new: fn -> Generic1::<T> = return;
}
Generic2: struct::<T, E> = {
    new: fn -> Generic2::<T, E> = return;
    take_t: fn(&this, arg: T) = return;
}
Interface: interface = {
    impl::<T> for Generic2::<T, f32> {
    }
}
InterfaceWrapper: struct::<T> = {
    Interface: interface = {
    }
}
)");
    Yoyo::YVMEngine engine;
    auto test_mod = addTestModule(&engine);
    auto mod = engine.addModule("source", source);
    REQUIRE(engine.compile());
    engine.prepareForExecution();
    if constexpr (emit_ir) std::cout << reinterpret_cast<Yoyo::YVMModule*>(mod)->dumpIR() << std::flush;
    auto fib = createFiberFor(mod, "source::main");
    engine.execute();
}
TEST_CASE("Test operator overloading", "[type-checker][operator-overloading]")
{
    std::string source(1 + R"(
// dot product
operator: *::<T>(a: Vec2::<T>, b: Vec2::<T>) -> T = {
    return a.x * b.x + a.y * b.y;
}
main: fn = {
    veci1 := Vec2::<i32>::new(10, 20);
    veci2 := Vec2::new(20, 30);
    vecf1 := Vec2::<f32>::new(30.0, 10.0);
    vecf2 := Vec2::new(100.0, 20.0);
    
    veci1.to_str().test::print();
    "${veci2 * veci1}".test::print();
    "${vecf1 * vecf2}".test::print();
}
Vec2: struct::<T> = {
    x: T, y: T,
    new: fn(x: T, y: T) -> Vec2::<T> = return Vec2::<T>{ .x, .y };
    to_str: fn(&this) -> str = return "{ ${this.x}, ${this.y} }";
}
)");
    Yoyo::YVMEngine engine;
    addTestModule(&engine);
    auto mod = engine.addModule("source", source);
    REQUIRE(engine.compile());
    engine.prepareForExecution();
    if constexpr (emit_ir) std::cout << reinterpret_cast<Yoyo::YVMModule*>(mod)->dumpIR() << std::flush;
    auto fib = createFiberFor(mod, "source::main");
    engine.execute();
}
//TEST_CASE("Test lambdas", "[lambda][borrow-checker]")
//{
//    std::string source(1 + R"(
//reassign_int: fn(b: &mut i32) = *b = 20;
//main: fn = {
//    b: mut = 100;
//    // lambdas can be "stored" even if they're non owning
//    func := |&mut b| {
//        *b = 40;
//    }
//    reassign_int(&mut b); // variables held by lambdas can still be borrowed even if owned by lambda
//    // variables are borrowed when the lambda is used (or moved)
//    func();
//}
//)");
//    Yoyo::YVMEngine engine;
//    addTestModule(&engine);
//    auto mod = engine.addModule("source", source);
//    REQUIRE(engine.compile());
//    engine.prepareForExecution();
//    if constexpr (emit_ir) std::cout << reinterpret_cast<Yoyo::YVMModule*>(mod)->dumpIR() << std::flush;
//    auto fib = createFiberFor(mod, "source::main");
//    engine.execute();
//}
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

TEST_CASE("Test CFG", "[CFG]")
{
    char name[] = "CFG";
    std::string src(1 + R"(
    main: fn = {
        var1: mut = if(true) {
            return 10;
        } else {
            "Hello"
        }
        if({ 
            elem.stuff();
            if(true) {
                return true;
            } else { true }
        }) { return 100; }
        while(true) {
            var2 := if(true) { break; } else { continue; }
            if(b) //unreachable
                10
            else 20;
        }
    }
    )");
    Yoyo::Parser p(src);
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