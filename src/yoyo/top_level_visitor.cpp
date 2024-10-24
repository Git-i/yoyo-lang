#include "ir_gen.h"
namespace Yoyo
{
    bool TopLevelVisitor::operator()(FunctionDeclaration* decl)
    {
        std::string mangled_name = std::string{decl->identifier.text};
        if(mod->functions.contains(mangled_name)) return false;
        mod->functions[mangled_name] = decl->signature;

        return true;
    }
    bool TopLevelVisitor::operator()(ClassDeclaration* decl)
    {
        std::string mangled_name_prefix = "__class__" + std::string{decl->identifier.text} + "__";
        if(mod->classes.contains(std::string{decl->identifier.text})) return false;
        mod->classes[std::string{decl->identifier.text}] = decl;
        for(auto& fn: decl->methods)
        {
            auto fn_decl = reinterpret_cast<FunctionDeclaration*>(fn.function_decl.get());
            std::string mangled_name = mangled_name_prefix + std::string{fn_decl->identifier.text};
            if(mod->functions.contains(mangled_name)) return false;
            mod->functions[mangled_name] = fn_decl->signature;
        }
        return true;
    }

}