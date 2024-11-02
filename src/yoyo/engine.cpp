#include "engine.h"

#include <ir_gen.h>
#include <module.h>
#include <parser.h>
#include <statement.h>

namespace Yoyo
{
    llvm::LLVMContext* getLLVMContext(Module* md)
    {
        return static_cast<llvm::LLVMContext*>(md->engine->llvm_context);
    }
    struct ForwardDeclaratorPass1
    {
        Module* md;
        bool operator()(FunctionDeclaration* decl)
        {
            std::string mangled_name = md->module_hash + std::string{decl->identifier.text};
            if(md->functions.contains(mangled_name)) return false;
            md->functions[mangled_name] = decl->signature;
            return true;
        }
        bool operator()(ClassDeclaration* decl)
        {
            std::string name = std::string{decl->identifier.text};
            std::string mangled_name_prefix = md->module_hash + "__class__" + name + "__";
            if(md->classes.contains(name)) return false;

            md->classes[name] = {mangled_name_prefix, nullptr, decl};
            return true;
        }
        bool operator()(Statement*) {return false;};
    };
    struct ForwardDeclaratorPass2
    {
        Module* md;
        //to prevent infinitely looping we stop when we see a name already here
        std::vector<std::string> encountered_names;
        bool operator()(ClassDeclaration* decl)
        {
            std::string name = std::string{decl->identifier.text};
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

            std::vector<llvm::Type*> args(decl->vars.size());
            std::transform(decl->vars.begin(), decl->vars.end(), args.begin(),
                [this](const ClassVariable& p)
                {
                    if(md->classes.contains(p.type.name))
                    {
                        auto& p_decl = md->classes[p.type.name];
                        if(std::get<1>(p_decl) == nullptr)
                        {
                            //the type is recursive
                            if(std::ranges::find(encountered_names, p.type.name) != encountered_names.end())
                                return static_cast<llvm::Type*>(nullptr);
                            encountered_names.push_back(p.type.name);
                            (*this)(std::get<2>(p_decl));
                        }
                    }
                    return md->ToLLVMType(p.type, false);
                });
            if(std::ranges::any_of(args, [](llvm::Type* t) {return t == nullptr;})) return false;
            std::get<1>(md->classes[name]) = llvm::StructType::create(*getLLVMContext(md), args, name);
            return true;
        }
        bool operator()(Statement*) { return true; };
    };

    Engine::Engine()
    {
        llvm_context = new llvm::LLVMContext();
    }

    Engine::~Engine()
    {
        delete static_cast<llvm::LLVMContext*>(llvm_context);
    }

    void Engine::addModule(const std::string& module_name, std::string source)
    {
        Parser p(std::move(source));
        if(modules.contains(module_name)) return;
        auto prog = p.parseProgram();
        if(p.failed()) return;
        auto& md = modules[module_name];
        md = std::make_unique<Module>();
        md->engine = this;
        for(auto& stat : prog)
        {
            if (!std::visit(ForwardDeclaratorPass1{md.get()}, stat->toVariant()))
            {
                modules.erase(module_name);
                break;
            };
        }
        for(auto& stat : prog)
        {
            if (!std::visit(ForwardDeclaratorPass2{md.get()}, stat->toVariant()))
            {
                modules.erase(module_name);
                break;
            };
        }

    }

    void Engine::compile()
    {
        IRGenerator irgen(*static_cast<llvm::LLVMContext*>(llvm_context));
        for(auto& mod : modules)
        {
            if(mod.second->code == nullptr)
            {
                auto src = sources.extract(mod.first);
                irgen.GenerateIR(mod.first, std::move(src.mapped()), mod.second.get());
            }
        }
    }
}
