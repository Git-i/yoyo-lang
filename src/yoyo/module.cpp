#include "module.h"

#include <type.h>
#include <fn_type.h>
#include <statement.h>
#include <llvm/IR/IRBuilder.h>

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

    Module::ClassDetails* Module::findType(const std::string& block, const std::string& name)
    {
        for(auto&[hash, details_list] : classes)
        {
            if(!block.starts_with(hash)) continue;
            for(auto& detaials : details_list)
                if(std::get<2>(detaials)->identifier.text == name) return &detaials;
        }
        return nullptr;
    }

    llvm::Type* Module::ToLLVMType(const Type& type, const std::string& hash, const std::vector<Type>& disallowed_types)
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
        if(type.is_char())
            return llvm::Type::getInt32Ty(context);
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
                auto ty = ToLLVMType(subtype, hash, disallowed_types);
                if(!ty) return nullptr;
                args.push_back(ty);
            }
            return llvm::StructType::get(context, args);
        }
        if(type.is_optional())
        {
            std::array<llvm::Type*, 2> args{};
            args[0] = ToLLVMType(type.subtypes[0], hash, disallowed_types);
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
                auto sub_t = ToLLVMType(subtype, hash, disallowed_types);
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
        if(auto t = findType(hash, type.name))
        {
            auto& ptr = std::get<1>(*t);
            if(ptr) return ptr;
            //class is not yet defined but is recursive
            if(auto find_it = std::ranges::find(disallowed_types, type); find_it != disallowed_types.end())
                return nullptr;
            ForwardDeclaratorPass2{type.module, disallowed_types}(std::get<2>(*t).get());
            return ptr;
        }
        return nullptr;
    }

    void makeBuiltinModule(Engine* eng)
    {
        if(eng->modules.contains("__builtin")) return;
        eng->modules["__builtin"] = std::make_unique<Module>();
        auto module = eng->modules.at("__builtin").get();
        module->module_hash = "";
        module->engine = eng;
        auto& operators = module->overloads;
        std::array types = {
            Type{"f64", {}, nullptr, module},
            Type{"f32", {}, nullptr, module},
            Type{"i64", {}, nullptr, module},
            Type{"i32", {}, nullptr, module},
            Type{"i16", {}, nullptr, module},
            Type{"i8", {}, nullptr, module},
            Type{"u64", {}, nullptr, module},
            Type{"u32", {}, nullptr, module},
            Type{"u16", {}, nullptr, module},
            Type{"u8", {}, nullptr, module},
            };
        auto& ctx = *static_cast<llvm::LLVMContext*>(eng->llvm_context);
        module->code = std::make_unique<llvm::Module>("__builtin", ctx);
        llvm::IRBuilder<> builder(ctx);
        for(auto& t : types)
        {
            auto as_llvm = module->ToLLVMType(t, "", {});
            auto fn_ty = llvm::FunctionType::get(as_llvm, {as_llvm, as_llvm}, false);
            auto mangled_name_for = [&t](const std::string& op_name)
            {
                // __operator_<name>__<type_lhs>__<type_rhs>
                return "__operator__" + op_name + "__" + t.name + "__" + t.name;
            };
            auto plus_fn = llvm::Function::Create(fn_ty, llvm::GlobalValue::ExternalLinkage,
                mangled_name_for("plus"), module->code.get());
            auto minus_fn = llvm::Function::Create(fn_ty, llvm::GlobalValue::ExternalLinkage,
                mangled_name_for("minus"), module->code.get());
            auto mul_fn = llvm::Function::Create(fn_ty, llvm::GlobalValue::ExternalLinkage,
                mangled_name_for("mul"), module->code.get());
            auto div_fn = llvm::Function::Create(fn_ty, llvm::GlobalValue::ExternalLinkage,
                mangled_name_for("div"), module->code.get());

            plus_fn->addFnAttr(llvm::Attribute::AlwaysInline);
            minus_fn->addFnAttr(llvm::Attribute::AlwaysInline);
            mul_fn->addFnAttr(llvm::Attribute::AlwaysInline);
            div_fn->addFnAttr(llvm::Attribute::AlwaysInline);

            builder.SetInsertPoint(llvm::BasicBlock::Create(ctx, "entry", plus_fn));
            if(t.is_integral()) builder.CreateRet(builder.CreateAdd(plus_fn->getArg(0), plus_fn->getArg(1)));
            else builder.CreateRet(builder.CreateFAdd(plus_fn->getArg(0), plus_fn->getArg(1)));
            operators.plus.emplace_back(t,t, t);

            builder.SetInsertPoint(llvm::BasicBlock::Create(ctx, "entry", minus_fn));
            if(t.is_integral()) builder.CreateRet(builder.CreateSub(minus_fn->getArg(0), minus_fn->getArg(1)));
            else builder.CreateRet(builder.CreateFSub(minus_fn->getArg(0), minus_fn->getArg(1)));
            operators.minus.emplace_back(t,t,t);

            builder.SetInsertPoint(llvm::BasicBlock::Create(ctx, "entry", mul_fn));
            if(t.is_integral()) builder.CreateRet(builder.CreateMul(mul_fn->getArg(0), mul_fn->getArg(1)));
            else builder.CreateRet(builder.CreateFMul(mul_fn->getArg(0), mul_fn->getArg(1)));
            operators.mul.emplace_back(t,t,t);

            builder.SetInsertPoint(llvm::BasicBlock::Create(ctx, "entry", div_fn));
            if(t.is_signed_integral()) builder.CreateRet(builder.CreateSDiv(div_fn->getArg(0), div_fn->getArg(1)));
            else if(t.is_unsigned_integral()) builder.CreateRet(builder.CreateUDiv(div_fn->getArg(0), div_fn->getArg(1)));
            else builder.CreateRet(builder.CreateFDiv(div_fn->getArg(0), div_fn->getArg(1)));
            operators.div.emplace_back(t,t,t);
        }
        for(auto& t : std::ranges::subrange(types.begin(), types.begin() + 6))
        {
            auto as_llvm = module->ToLLVMType(t, "", {});
            auto fn_ty = llvm::FunctionType::get(as_llvm, {as_llvm}, false);
            auto mangled_name_for = [&t](const std::string& op_name)
            {
                return "__operator__" + op_name + "__" + t.name;
            };
            auto fn = llvm::Function::Create(fn_ty, llvm::GlobalValue::ExternalLinkage, mangled_name_for("un_neg"),
                module->code.get());
            builder.SetInsertPoint(llvm::BasicBlock::Create(ctx, "entry", fn));
            if(t.is_integral()) builder.CreateRet(builder.CreateNeg(fn->getArg(0)));
            else builder.CreateRet(builder.CreateFNeg(fn->getArg(0)));
            operators.un_neg.emplace_back(t,t);
        }
        for(auto& t : std::ranges::subrange(types.begin() + 2, types.end()))
        {
            auto as_llvm = module->ToLLVMType(t, "", {});
            auto fn_ty = llvm::FunctionType::get(as_llvm, {as_llvm, as_llvm}, false);
            auto mangled_name_for = [&t](const std::string& op_name)
            {
                return "__operator__" + op_name + "__" + t.name + "__" + t.name;
            };
            auto fn = llvm::Function::Create(fn_ty, llvm::GlobalValue::ExternalLinkage, mangled_name_for("mod"),
                module->code.get());
            builder.SetInsertPoint(llvm::BasicBlock::Create(ctx, "entry", fn));
            if(t.is_signed_integral()) builder.CreateRet(builder.CreateSRem(fn->getArg(0), fn->getArg(1)));
            else builder.CreateRet(builder.CreateURem(fn->getArg(0), fn->getArg(1)));
            operators.mod.emplace_back(t,t,t);
        }


    }
}
