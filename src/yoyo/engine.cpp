#include "engine.h"

#include <ir_gen.h>
#include <module.h>
#include <parser.h>
#include <ranges>
#include <statement.h>
#include <llvm/Support/TargetSelect.h>
namespace Yoyo
{
    llvm::LLVMContext* getLLVMContext(Module* md)
    {
        return md->engine->llvm_context.getContext();
    }
    struct ForwardDeclaratorPass1
    {
        Module* md;
        std::unique_ptr<Statement>& stmt;
        bool operator()(FunctionDeclaration* decl) const
        {
            md->functions[md->module_hash].emplace_back(decl->name, decl->signature);
            return true;
        }
        bool operator()(GenericFunctionDeclaration* decl) const
        {
            std::ignore = stmt.release();
            md->generic_fns[md->module_hash].emplace_back(decl);
            return true;
        }
        bool operator()(ClassDeclaration* decl) const
        {
            std::string name = std::string{decl->identifier.text};
            std::string mangled_name_prefix = md->module_hash + "__class__" + name + "__%";
            if(md->classes.contains(name)) return false;

            for(auto& fn : decl->methods)
            {
                auto fn_decl = reinterpret_cast<FunctionDeclaration*>(fn.function_decl.get());
                md->functions[mangled_name_prefix].emplace_back(fn_decl->name, fn_decl->signature);
            }
            md->classes[md->module_hash].emplace_back(
                mangled_name_prefix,
                nullptr,
                std::unique_ptr<ClassDeclaration>{decl}
            );

            return true;
        }
        bool operator()(ModuleImport* imp)
        {
            md->engine->addModule(imp->module_path, "");
            md->modules[imp->module_name] = md->engine->modules[imp->module_path].get();
            return true;
        }
        bool operator()(EnumDeclaration* decl)
        {
            std::ignore = stmt.release();
            md->enums[std::string{decl->identifier.text}] = std::unique_ptr<EnumDeclaration>(decl);
            return true;
        }
        bool operator()(OperatorOverload*) {return true;}
        bool operator()(AliasDeclaration* decl)
        {
            md->aliases[md->module_hash].emplace(decl->name, decl->type);
            return true;
        }
        bool operator()(GenericAliasDeclaration* decl)
        {
            std::ignore = stmt.release();
            md->generic_aliases[md->module_hash].emplace_back(decl);
            return true;
        }
        bool operator()(InterfaceDeclaration* decl)
        {
            std::ignore = stmt.release();
            md->interfaces[md->module_hash].emplace_back(decl);
            return true;
        }
        bool operator()(Statement*) const {return false;};
    };
    struct ForwardDeclaratorPass2
    {
        Module* md;
        //to prevent infinitely looping we stop when we see a name already here
        std::vector<Type> encountered_names;
        bool operator()(ClassDeclaration* decl);
        bool operator()(Statement*);
    };

    bool ForwardDeclaratorPass2::operator()(ClassDeclaration* decl)
    {
        std::string name = std::string{decl->identifier.text};
        auto t = md->findType(md->module_hash, name);
        if(t && std::get<1>(*t)) return true;
        std::vector<std::string> var_names(decl->vars.size());
        std::vector<std::string> fn_names(decl->methods.size());
        //check for duplicate names, TODO: move this to a function
        std::ranges::transform(decl->vars, var_names.begin(), [](ClassVariable& var)
        {
            return var.name;
        });
        std::ranges::transform(decl->methods, fn_names.begin(), [](ClassMethod& method)
        {
            return method.name;
        });
        for(const auto& name: var_names)
            if(std::ranges::find(fn_names, name) != fn_names.end()){return false;}

        for(const auto& name: fn_names)
            if(std::ranges::find(var_names, name) != var_names.end()){return false;}
        for(size_t i = 0; i < var_names.size(); ++i)
        {
            for(size_t j = 0; j < var_names.size(); ++j)
            {
                if(j == i) continue;
                if(var_names[i] == var_names[j]) {return false;}
            }
        }
        for(size_t i = 0; i < fn_names.size(); ++i)
        {
            for(size_t j = 0; j < fn_names.size(); ++j)
            {
                if(j == i) continue;
                if(fn_names[i] == fn_names[j]) {return false;}
            }
        }
        //----------------------------------------------------------
        encountered_names.push_back(Type{.name = name, .module = md});
        std::vector<llvm::Type*> args(decl->vars.size());
        std::transform(decl->vars.begin(), decl->vars.end(), args.begin(),
                       [this](ClassVariable& p)
                       {
                           p.type.saturate(md, nullptr);
                           return md->ToLLVMType(p.type, md->module_hash, encountered_names);
                       });
        if(std::ranges::any_of(args, [](llvm::Type* t) {return t == nullptr;})) return false;
        std::get<1>(*t) = llvm::StructType::create(*getLLVMContext(md), args, name);
        return true;
    }

    bool ForwardDeclaratorPass2::operator()(Statement*)
    { return true; }


    extern "C"
        const char* __asan_default_options() {
        // Clang reports ODR Violation errors in mbedtls/library/certs.c.
        // NEED TO REPORT THIS ISSUE
        return "detect_container_overflow=0";
    }
    Engine::Engine()
    {
        llvm::InitializeNativeTarget();
        llvm::InitializeNativeTargetAsmPrinter();
        auto value = llvm::orc::LLJITBuilder().create();
        llvm_context = llvm::orc::ThreadSafeContext(std::make_unique<llvm::LLVMContext>());
        if (!value) Yoyo::debugbreak();
        std::move(value).moveInto(jit);
        makeBuiltinModule(this);
    }

    Engine::~Engine()
    {

        //delete static_cast<llvm::LLVMContext*>(llvm_context);
    }

    AppModule* Engine::addAppModule(const std::string& name)
    {
        auto& ctx = *llvm_context.getContext();
        if(modules.contains(name)) return nullptr;
        auto& md = modules[name];
        md = std::make_unique<AppModule>();
        md->module_hash = "__" + name + "%";
        md->engine = this;
        md->code = llvm::orc::ThreadSafeModule(std::make_unique<llvm::Module>(name, ctx), llvm_context);
        return reinterpret_cast<AppModule*>(md.get());
    }

    void Engine::addModule(const std::string& module_name, std::string source)
    {
        auto p = Parser(source);
        if(modules.contains(module_name)) return;
        auto prog = p.parseProgram();
        if(p.failed()) return;
        auto& md = modules[module_name];
        md = std::make_unique<Module>();
        md->engine = this;
        md->module_hash = "__" + module_name + std::to_string(reinterpret_cast<std::uintptr_t>(md.get())) + "%";
        for(auto& stat : prog)
        {
            if (!std::visit(ForwardDeclaratorPass1{md.get(), stat}, stat->toVariant()))
            {
                modules.erase(module_name);
                break;
            };
        }
        sources[module_name] = { std::move(source), std::move(prog) };

    }

    void Engine::compile()
    {
        IRGenerator irgen(*llvm_context.getContext());
        auto keys_view = std::ranges::views::keys(modules);
        std::vector module_names(keys_view.begin(), keys_view.end());
        for (const auto& module_name : module_names)
        {
            if (sources.contains(module_name))
            {
                for (auto& stat : sources[module_name].second)
                {
                    if (!stat) continue; //by this point some statements have already been handled(enums)
                    if (!std::visit(ForwardDeclaratorPass2{ modules[module_name].get() }, stat->toVariant()))
                    {
                        modules.erase(module_name);
                        break;
                    };
                }
            }
        }
        for (auto& mod : modules)
        {
            if (!mod.second->code)
            {
                auto src = sources.extract(mod.first);
                SourceView vw(src.mapped().first, mod.first);
                irgen.view = &vw;
                if (!irgen.GenerateIR(mod.first, std::move(src.mapped().second), mod.second.get(), this))
                    mod.second->code.~ThreadSafeModule();
            }
        }
    }

    std::string_view Engine::viewString(void* str)
    {
        struct String{char* data; uint64_t len; uint64_t cap;};
        auto arg_as_str = static_cast<String*>(str);
        return std::string_view{arg_as_str->data, arg_as_str->len};
    }
    void Engine::prepareForExecution()
    {
        for (auto& [name, module] : modules)
            if(module->code) jit->addIRModule(std::move(module->code));
    }
}
