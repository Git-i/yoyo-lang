#include "ir_gen.h"
namespace Yoyo
{
    bool TopLevelVisitor::operator()(std::unique_ptr<FunctionDeclaration> decl) const
    {
        std::unique_ptr<Statement> stat = std::move(decl);
        irgen->current_Statement = &stat;
        (*irgen)(reinterpret_cast<FunctionDeclaration*>(stat.get()));
        return true;
    }
    bool TopLevelVisitor::operator()(std::unique_ptr<ClassDeclaration> decl) const
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

            std::string mangled_name = std::get<0>(irgen->module->classes[name]) + fn.name;

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
    bool handleBinaryOverload(OperatorOverload* decl, IRGenerator* irgen)
    {
        if(decl->signature.parameters.size() != 2) irgen->error();
        OverloadDetailsBinary bin {.left = decl->signature.parameters[0].type,
            .right = decl->signature.parameters[1].type,
            .result = decl->signature.returnType};
        std::string old_hash = std::move(irgen->block_hash);
        std::string name = bin.mangled_name(decl->tok);
        Token tk{.text = name};
        FunctionDeclaration fn_decl(tk, std::move(decl->signature), std::move(decl->body));
        (*irgen)(&fn_decl);
        irgen->block_hash = std::move(old_hash);
        irgen->module->overloads.binary_details_for(decl->tok)->emplace_back(std::move(bin));
        return true;
    }
    bool TopLevelVisitor::operator()(std::unique_ptr<OperatorOverload> decl)
    {
        irgen->saturateSignature(decl->signature, irgen->module);
        if(!std::ranges::any_of(decl->signature.parameters, [this](FunctionParameter& param)
        {
            return param.type.module == irgen->module;
        })) {irgen->error(); return false;}
        //check if overload already exists
        if(Token{decl->tok}.can_be_overloaded_binary_only())
        {
            return handleBinaryOverload(decl.get(), irgen);
        }
        //minus can be overloaaded as binary and unary
        if(decl->tok == TokenType::Minus)
        {
            if(decl->signature.parameters.size() == 2) return handleBinaryOverload(decl.get(), irgen);
            if(decl->signature.parameters.size() != 1) {irgen->error(); return false;}
        }
        raise(SIGTRAP);
    }
}
