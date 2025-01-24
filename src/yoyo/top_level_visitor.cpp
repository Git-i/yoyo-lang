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
    bool implementsInterfaceMethod(const FunctionSignature& cls, const FunctionSignature& interface)
    {
        if (cls.parameters.size() != interface.parameters.size()) return false;
        if (!cls.returnType.is_equal(interface.returnType)) return false;
        for (size_t i = 0; i < cls.parameters.size(); i++)
        {
            auto& cls_param = cls.parameters[i];
            auto& intf_param = interface.parameters[i];
            if (!cls_param.type.is_equal(intf_param.type))
                if (cls_param.name != "this" || intf_param.name != "this") return false;
        }
        return true;
    }
    bool TopLevelVisitor::operator()(std::unique_ptr<ClassDeclaration> decl) const
    {
        std::string name = decl->name;
        std::string mangled_name_prefix = name + "::";

        auto ty = irgen->module->findType(irgen->block_hash, name);

        irgen->this_t = Type{.name = name, .subtypes = {}};
        irgen->this_t.saturate(irgen->module, irgen);
        std::ignore = std::get<2>(*ty).release();
        auto decl_ptr = decl.get();
        std::get<2>(*ty) = std::move(decl);
        irgen->in_class = true;
        irgen->checkClass(decl_ptr);
        auto curr_hash = std::move(irgen->block_hash);
        irgen->block_hash = curr_hash + mangled_name_prefix;
        for(auto& fn: decl_ptr->methods)
        {
            auto fn_decl = reinterpret_cast<FunctionDeclaration*>(fn.function_decl.get());
            irgen->saturateSignature(fn_decl->signature, irgen->module);
            irgen->current_Statement = &fn.function_decl;
            (*irgen)(fn_decl);
        }
        for (auto& impl : decl_ptr->impls)
        {
            if (!impl.impl_for.module) continue;
            std::pair<std::string, InterfaceDeclaration*> pair;
            if (!impl.impl_for.subtypes.empty())
                pair = impl.impl_for.module->findInterface(impl.impl_for.block_hash, impl.impl_for.name + IRGenerator::mangleGenericArgs(impl.impl_for.subtypes));
            else pair = impl.impl_for.module->findInterface(impl.impl_for.block_hash, impl.impl_for.name);
            auto [hash, interface] = std::move(pair);
            if (!interface) continue;
            if (impl.methods.size() != interface->methods.size())
            {
                continue;
            }
            auto class_hash = std::move(irgen->block_hash);
            irgen->block_hash = class_hash + hash + interface->name + "::";
            for (auto& mth : impl.methods)
            {
                auto it = std::ranges::find_if(interface->methods, [&mth](auto& method) {
                    return method->name == mth->name;
                    });
                if (it == interface->methods.end())
                    irgen->error(Error(mth.get(), "Function does not exist as part of the interface"));
                else
                {
                    irgen->saturateSignature(mth->signature, irgen->module);
                    irgen->in_class = false;
                    irgen->saturateSignature((*it)->signature, impl.impl_for.module);
                    irgen->in_class = true;
                    if (!implementsInterfaceMethod(mth->signature, (*it)->signature))
                        irgen->error(Error(mth.get(), "Provided function is not a valid implementation of the interface"));
                }
                (*irgen)(mth.get());
            }
            irgen->block_hash = std::move(class_hash);
        }
        irgen->block_hash = std::move(curr_hash);
        irgen->in_class = false;
        return true;
    }
    bool handleBinaryOverload(OperatorOverload* decl, IRGenerator* irgen)
    {
        if(decl->signature.parameters.size() != 2) irgen->error(Error(decl, "More or less than 2 operands for binary operator"));
        OverloadDetailsBinary bin {.left = decl->signature.parameters[0].type,
            .right = decl->signature.parameters[1].type,
            .result = decl->signature.returnType};
        //spaceship operator need special return type
        if (decl->tok == TokenType::Spaceship) {
            std::string err = "Comparison operator overload must return either core::CmpEq, core::CmpOrd or core::CmpPartOrd";
            const auto& nm = decl->signature.returnType.name;
            auto md = decl->signature.returnType.module;
            if (md != md->engine->modules.at("core").get()) 
                irgen->error(Error(decl, err));
            else if (nm != "CmpEq" && nm != "CmpOrd" && nm != "CmpPartOrd")
                irgen->error(Error(decl, err));
        }
        std::string old_hash = irgen->reset_hash();
        std::string name = bin.mangled_name(decl->tok);
        FunctionDeclaration fn_decl(name, std::move(decl->signature), std::move(decl->body));
        (*irgen)(&fn_decl);
        irgen->block_hash = std::move(old_hash);
        //remove the module hash infront of the final name
        auto fn = irgen->code->getFunction(irgen->module->module_hash + name);
        fn->setName(name);
        irgen->module->overloads.add_binary_detail_for(decl->tok, std::move(bin));
        return true;
    }
    bool TopLevelVisitor::operator()(std::unique_ptr<OperatorOverload> decl)
    {
        irgen->saturateSignature(decl->signature, irgen->module);
        if(!std::ranges::any_of(decl->signature.parameters, [this](FunctionParameter& param)
        {
            return param.type.deref().module == irgen->module;
        })) {irgen->error(Error(decl.get(), "This module does not own any type being overloaded")); return false; }
        //check if overload already exists
        if(Token{decl->tok}.can_be_overloaded_binary_only())
        {
            return handleBinaryOverload(decl.get(), irgen);
        }
        //minus can be overloaaded as binary and unary
        if(decl->tok == TokenType::Minus)
        {
            if(decl->signature.parameters.size() == 2) return handleBinaryOverload(decl.get(), irgen);
            if(decl->signature.parameters.size() != 1) {irgen->error(Error(decl.get(), "Unary '-' with too many / too few operands")); return false; }
        }
        debugbreak();
    }
}
