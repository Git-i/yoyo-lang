#include <ir_gen.h>
#include <yvm/yvm_module.h>
#include <parser.h>
#include <ranges>
#include <statement.h>
#include <yvm/yvm_engine.h>
#include <yvm/yvm_irgen.h>
#include <gc/gc.h>
#include <yvm/app_module.h>
#include "yvm/fwd_decl.h"
namespace Yoyo
{
    YVMEngine::YVMEngine()
    {
        vm.do_native_call = [](void* function, Yvm::VM::Type* begin, size_t arg_size, void* proto) {
            return NativeType::doCall(static_cast<NativeProto*>(proto), arg_size, begin, function);
            };
        //instrinsic zero is gcnew
        vm.intrinsic_handler = [](Yvm::Stack& stack, uint8_t intrinsic) {
            switch (intrinsic)
            {
            case 0: stack.push(GC_MALLOC(stack.pop<32>())); break;
            }
            };
        YVMModule::makeBuiltinModule(this);
    }

    YVMEngine::~YVMEngine()
    {
        for (auto mod : external_dlls) NativeType::free_native_library(mod);
    }

    YVMAppModule* YVMEngine::addAppModule(const std::string& name)
    {
        if (modules.contains(name)) return nullptr;
        auto& md = modules[name];
        md = std::make_unique<YVMAppModule>();
        md->module_hash = name + "::";
        md->engine = this;
        auto app_md = reinterpret_cast<YVMAppModule*>(md.get());
        vm.add_module(&app_md->code);
        return app_md;
    }

    void YVMEngine::addModule(const std::string& module_name, std::string source)
    {
        auto p = Parser(source);
        if (modules.contains(module_name)) return;
        auto prog = p.parseProgram();
        if (p.failed()) return;
        auto& md = modules[module_name];
        md = std::make_unique<YVMModule>();
        md->engine = this;
        md->module_hash = module_name + "::";
        md->modules["core"] = modules.at("core").get();
        vm.add_module(&reinterpret_cast<YVMModule*>(md.get())->code);
        for (auto& stat : prog)
        {
            if (!std::visit(ForwardDeclaratorPass1{ reinterpret_cast<YVMModule*>(md.get()), stat, md->module_hash }, stat->toVariant()))
            {
                modules.erase(module_name);
                break;
            };
        }
        sources[module_name] = { std::move(source), std::move(prog) };

    }

    void YVMEngine::compile()
    {
        YVMIRGenerator irgen;
        auto keys_view = std::ranges::views::keys(modules);
        std::vector module_names(keys_view.begin(), keys_view.end());

        for (auto& mod : modules)
        {
            auto yvm_mod = reinterpret_cast<YVMModule*>(mod.second.get());
            if (!sources.contains(mod.first)) continue;
            auto src = sources.extract(mod.first);
            SourceView vw(src.mapped().first, mod.first);
            irgen.view = &vw;
            irgen.GenerateIR(mod.first, std::move(src.mapped().second), yvm_mod, this);
        }
    }
    void YVMEngine::addDynamicLibrary(std::string_view path)
    {
        auto mod = NativeType::load_native_library(std::string(path));
        if (mod) external_dlls.push_back(mod);
    }
    void* YVMEngine::findNativeFunction(const std::string& name)
    {
        for (auto mod : external_dlls) {
            auto fn = NativeType::get_library_fn(mod, name);
            if (fn) return fn;
        }
        return nullptr;
    }
    void YVMEngine::prepareForExecution()
    {
        vm.link();
    }
}
