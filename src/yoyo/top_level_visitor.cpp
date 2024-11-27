#include "ir_gen.h"
namespace Yoyo
{
    bool TopLevelVisitor::operator()(std::unique_ptr<FunctionDeclaration> decl)
    {
        std::unique_ptr<Statement> stat = std::move(decl);
        irgen->current_Statement = &stat;
        (*irgen)(reinterpret_cast<FunctionDeclaration*>(stat.get()));
        return true;
    }
    bool TopLevelVisitor::operator()(std::unique_ptr<ClassDeclaration> decl)
    {
        std::string name = std::string{decl->identifier.text};
        std::string mangled_name_prefix = "__class__" + name + "__";

        std::ignore = std::get<2>(irgen->module->classes[name]).release();

        irgen->this_t = Type{.name = name, .subtypes = {}};
        auto decl_ptr = decl.get();
        std::get<2>(irgen->module->classes[name]) = std::move(decl);
        for(auto& fn: decl_ptr->methods)
        {
            auto fn_decl = reinterpret_cast<FunctionDeclaration*>(fn.function_decl.get());

            std::string& mangled_name = std::get<0>(irgen->module->classes[name]);

            if(irgen->module->functions.contains(mangled_name)) return false;
            irgen->module->functions[mangled_name] = fn_decl->signature;
            irgen->in_class = true;
            auto curr_hash = std::move(irgen->block_hash);
            irgen->block_hash = curr_hash + mangled_name_prefix;

            irgen->current_Statement = &fn.function_decl;
            (*irgen)(fn_decl);
            irgen->block_hash = std::move(curr_hash);
            irgen->in_class = false;
        }
        irgen->hanldeClassDeclaration(decl_ptr, true);
        return true;
    }

}