#include "module.h"

#include <type.h>
#include <fn_type.h>
#include <statement.h>
#include "engine.h"
namespace Yoyo
{
    struct ForwardDeclaratorPass2
    {
        Module* md;
        //to prevent infinitely looping we stop when we see a name already here
        std::vector<Type> encountered_names;
        bool operator()(ClassDeclaration* decl);
        bool operator()(Statement*);
    };
    FunctionSignature* Module::findFunction(const std::string& name)
    {
        if(auto fn = functions.find(name); fn != functions.end())
        {
            return &fn->second;
        }
        return nullptr;
    }

    llvm::Type* Module::ToLLVMType(const Type& type, bool is_ref, const std::vector<Type>& disallowed_types)
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
        if(type.is_opaque_pointer() || type.is_reference())
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
                auto ty = ToLLVMType(subtype, false, disallowed_types);
                if(!ty) return nullptr;
                args.push_back(ty);
            }
            return llvm::StructType::get(context, args);
        }
        if(type.is_optional())
        {
            std::array<llvm::Type*, 2> args{};
            args[0] = ToLLVMType(type.subtypes[0], false, disallowed_types);
            args[1] = llvm::Type::getInt1Ty(context);
            return llvm::StructType::get(context, args);
        }
        if(type.is_str())
        {
            std::array<llvm::Type*, 3> args{};
            args[0] = llvm::PointerType::get(context, 0);
            for(auto& sub_t : std::ranges::subrange(args.begin() + 1, args.end())) sub_t = llvm::Type::getInt64Ty(context);
            return llvm::StructType::get(context, args);
        }
        if(type.is_variant())
        {
            std::array<llvm::Type*, 2> args{};
            size_t size = 0;
            auto& layout = code->getDataLayout();
            for(auto& subtype : type.subtypes)
            {
                auto sub_t = ToLLVMType(subtype, false, disallowed_types);
                auto as_struct = llvm::dyn_cast_or_null<llvm::StructType>(sub_t);
                size_t sz = 0;
                if(!as_struct) sz = sub_t->getPrimitiveSizeInBits() / 8;
                else sz = layout.getStructLayout(as_struct)->getSizeInBytes();
                if(sz > size) size = sz;
            }
            args[0] = llvm::ArrayType::get(llvm::Type::getInt8Ty(context), size);
            args[1] = llvm::Type::getInt32Ty(context); // 2^32 is a reasonable amount of variant subtypes
            return llvm::StructType::get(context, args);
        }
        if(type.is_enum())
        {
            return llvm::Type::getInt32Ty(context);
        }

        //if(in_class && type.name == "This") return ToLLVMType(this_t, is_ref);
        if(type.is_lambda()) return nullptr;
        if(auto t = type.module->classes.find(type.name); t != classes.end())
        {
            auto& ptr = std::get<1>(t->second);
            if(ptr) return ptr;
            //class is not yet defined but is recursive
            if(auto find_it = std::ranges::find(disallowed_types, type); find_it != disallowed_types.end())
                return nullptr;
            ForwardDeclaratorPass2{type.module, disallowed_types}(std::get<2>(t->second).get());
            return ptr;
        }
        return nullptr;
    }
}
