#include "ir_gen.h"
namespace Yoyo
{
    bool TopLevelVisitor::operator()(FunctionDeclaration* decl)
    {
        std::string mangled_name = std::string{decl->identifier.text};
        if(irgen->module->functions.contains(mangled_name)) return false;
        irgen->module->functions[mangled_name] = decl->signature;
        (*irgen)(decl);
        return true;
    }
    bool TopLevelVisitor::operator()(ClassDeclaration* decl)
    {
        std::string name = std::string{decl->identifier.text};
        std::string mangled_name_prefix = "__class__" + name + "__";
        if(irgen->module->classes.contains(name)) return false;

        irgen->module->classes[name] = {mangled_name_prefix, irgen->hanldeClassDeclaration(decl, false), decl};
        irgen->this_t = Type{.name = name, .subtypes = {}};
        for(auto& fn: decl->methods)
        {
            auto fn_decl = reinterpret_cast<FunctionDeclaration*>(fn.function_decl.get());
            std::string mangled_name = mangled_name_prefix + std::string{fn_decl->identifier.text};
            if(irgen->module->functions.contains(mangled_name)) return false;
            irgen->module->functions[mangled_name] = fn_decl->signature;
            irgen->in_class = true;
            irgen->block_hash = mangled_name_prefix;
            (*irgen)(fn_decl);
            irgen->block_hash = "";
            irgen->in_class = false;
        }
        return true;
    }

}