#include <ir_gen.h>
#include <yvm/yvm_module.h>
#include <parser.h>
#include <ranges>
#include <statement.h>
#include <yvm/yvm_engine.h>
#include <yvm/yvm_irgen.h>
namespace Yoyo
{
    //Given a declaration, forward declare it and all the other sustatements
    struct ForwardDeclaratorPass1
    {
        ModuleBase* md;
        std::unique_ptr<Statement>& stmt;
        std::string block;
        bool operator()(FunctionDeclaration* decl) const
        {
            md->functions[block].emplace_back(decl->name, decl->signature, decl->attributes);
            std::string new_blk = block + decl->name + "::";
            std::visit(ForwardDeclaratorPass1{ md, decl->body, new_blk }, decl->body->toVariant());
            return true;
        }
        bool operator()(ConstantDeclaration* decl) const
        {
            //constants should never be moved out of or destroyed
            decl->type.is_lvalue = true;
            decl->type.is_mutable = false;
            md->constants[block].emplace_back(decl->type, decl->name, decl);
            return true;
        }
        bool operator()(GenericFunctionDeclaration* decl) const
        {
            md->generic_fns[block].emplace_back(decl);
            return true;
        }
        bool operator()(ClassDeclaration* decl) const
        {
            std::string mangled_name_prefix = block + decl->name + "::";

            for (auto& stt : decl->stats)
            {
                std::visit(ForwardDeclaratorPass1{ md, stt, mangled_name_prefix }, stt->toVariant());
            }
            md->aliases[mangled_name_prefix]["This"] = Type{
                .name = decl->name,
                .module = md,
                .block_hash = block,
            };
            md->classes[block].emplace_back(
                mangled_name_prefix,
                nullptr,
                std::unique_ptr<ClassDeclaration>{decl}
            );

            return true;
        }
        bool operator()(GenericClassDeclaration* decl) const
        {
            md->generic_classes[block].emplace_back(decl);
            return true;
        }
        bool operator()(ModuleImport* imp)
        {
            reinterpret_cast<YVMEngine*>(md->engine)->addModule(imp->module_path, "");
            md->modules[imp->module_name] = md->engine->modules[imp->module_path].get();
            return true;
        }
        bool operator()(EnumDeclaration* decl)
        {
            std::string new_blk = block + decl->identifier + "::";

            for (auto& stt : decl->stats)
            {
                std::visit(ForwardDeclaratorPass1{ md, stt, new_blk }, stt->toVariant());
            }
            md->aliases[new_blk]["This"] = Type{
                .name = decl->identifier,
                .module = md,
                .block_hash = block,
            };
            md->enums[block].emplace_back(decl);
            return true;
        }
        bool operator()(OperatorOverload*) { return true; }
        bool operator()(AliasDeclaration* decl)
        {
            md->aliases[block].emplace(decl->name, decl->type);
            return true;
        }
        bool operator()(GenericAliasDeclaration* decl)
        {
            md->generic_aliases[block].emplace_back(decl);
            return true;
        }
        bool operator()(InterfaceDeclaration* decl)
        {
            std::ignore = stmt.release();
            md->interfaces[block].emplace_back(decl);
            return true;
        }
        bool operator()(GenericInterfaceDeclaration* decl)
        {
            md->generic_interfaces[block].emplace_back(decl);
            return true;
        }
        bool operator()(UnionDeclaration* decl)
        {
            std::string new_blk = block + decl->name + "::";

            for (auto& stt : decl->sub_stats)
            {
                std::visit(ForwardDeclaratorPass1{ md, stt, new_blk }, stt->toVariant());
            }
            md->aliases[new_blk]["This"] = Type{
                .name = decl->name,
                .module = md,
                .block_hash = block,
            };
            md->unions[block].emplace_back(
                std::unique_ptr<UnionDeclaration>{decl},
                nullptr
            );

            return true;
        }
        bool operator()(BlockStatement* stat) {
            for (auto& sub : stat->statements) {
                std::visit(ForwardDeclaratorPass1{ md, sub, block }, sub->toVariant());
            }
            return false;
        }
        bool operator()(MacroDeclaration* decl) {
            md->macros[block].emplace_back(decl);
            return true;
        }
        bool operator()(Statement*) const { return false; };
    };


    extern "C"
        const char* __asan_default_options() {
        // Clang reports ODR Violation errors in mbedtls/library/certs.c.
        // NEED TO REPORT THIS ISSUE
        return "detect_container_overflow=0";
    }
    
    Engine::~Engine()
    {

    }

    YVMAppModule* YVMEngine::addAppModule(const std::string& name)
    {
        return nullptr;
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
        for (auto& stat : prog)
        {
            if (!std::visit(ForwardDeclaratorPass1{ md.get(), stat, md->module_hash }, stat->toVariant()))
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
            //if (!yvm_mod->code)
            //{
            //    auto src = sources.extract(mod.first);
            //    SourceView vw(src.mapped().first, mod.first);
            //    irgen.view = &vw;
            //    if (!irgen.GenerateIR(mod.first, std::move(src.mapped().second), llvm_mod, this))
            //        llvm_mod->code.~ThreadSafeModule();
            //}
        }
    }
    void YVMEngine::addDynamicLibrary(std::string_view path)
    {
        
    }
    void* YVMEngine::createGlobalConstant(const Type& type, const std::vector<Constant>& args, IRGenerator* irgen_g)
    {
        return nullptr;
    }
    std::string_view Engine::viewString(void* str)
    {
        struct String { char* data; uint64_t len; uint64_t cap; };
        auto arg_as_str = static_cast<String*>(str);
        return std::string_view{ arg_as_str->data, arg_as_str->len };
    }
    void YVMEngine::prepareForExecution()
    {
    }
}
