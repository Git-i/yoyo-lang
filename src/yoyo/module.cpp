#include "module.h"

#include <type.h>
#include <fn_type.h>
namespace Yoyo
{
    FunctionSignature* Module::findFunction(const std::string& name)
    {
        if(auto fn = functions.find(name); fn != functions.end())
        {
            return &fn->second;
        }
        return nullptr;
    }

    llvm::Type* Module::ToLLVMType(const Type& type, bool is_ref)
    {
        auto& context = *static_cast<llvm::LLVMContext*>(engine->llvm_context);
        if(type.is_integral())
            return llvm::Type::getIntNTy(context, *type.integer_width());
        if(type.is_floating_point())
            return *type.float_width() == 32 ? llvm::Type::getFloatTy(context) : llvm::Type::getDoubleTy(context);
        if(type.is_boolean())
            return llvm::Type::getInt1Ty(context);
        if(type.name == "void")
            return llvm::Type::getVoidTy(context);
        if(type.is_opaque_pointer())
            return llvm::PointerType::get(context, 0);
        if(type.name == "__called_fn")
        {
            auto ptr_ty = llvm::PointerType::get(context, 0);
            //called functions are always struct{void* context, void* function}
            return llvm::StructType::get(context, {ptr_ty, ptr_ty});
        }
        if(type.is_tuple())
        {
            std::vector<llvm::Type*> args;
            for(auto& subtype : type.subtypes)
            {
                auto ty = ToLLVMType(subtype, false);
                if(!ty) return nullptr;
                args.push_back(ty);
            }
            return llvm::StructType::get(context, args);
        }
        //if(in_class && type.name == "This") return ToLLVMType(this_t, is_ref);

        if(auto t = classes.find(type.name); t != classes.end()) return std::get<1>(t->second);
        return nullptr;
    }
}
