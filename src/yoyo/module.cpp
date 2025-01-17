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
    std::pair<std::string, Module::FunctionDetails*> Module::findFunction(const std::string& block, const std::string& name)
    {
        for(auto&[hash, details_list] : functions)
        {
            if(!block.starts_with(hash)) continue;
            for(auto& details : details_list)
                if(details.name == name) return {hash, &details};
        }
        return {"", nullptr};
    }

    std::pair<std::string, GenericFunctionDeclaration*> Module::findGenericFn(const std::string& block,
        const std::string& name)
    {
        for(auto&[hash, details_list] : generic_fns)
        {
            if(!block.starts_with(hash)) continue;
            for(auto& details : details_list)
                if(details->name == name) return {hash, details.get()};
        }
        return {"", nullptr};
    }

    Type* Module::findAlias(const std::string& block, const std::string& name)
    {
        for(auto&[hash, details_list] : aliases)
        {
            if(!block.starts_with(hash)) continue;
            if(details_list.contains(name)) return &details_list.at(name);
        }
        return nullptr;
    }
    std::pair<std::string, InterfaceDeclaration*> Module::findInterface(const std::string& block, const std::string& name)
    {
        for (auto& [hash, interface_list] : interfaces)
        {
            if (!block.starts_with(hash)) continue;
            for (auto& intf : interface_list)
                if (intf->name == name) return { hash, intf.get() };
        }
        return { "", nullptr };
    }
    std::pair<std::string, GenericInterfaceDeclaration*> Module::findGenericInterface(const std::string& block, const std::string& name)
    {
        for (auto& [hash, interface_list] : generic_interfaces)
        {
            if (!block.starts_with(hash)) continue;
            for (auto& intf : interface_list)
                if (intf->name == name) return { hash, intf.get() };
        }
        return { "", nullptr };
    }

    std::pair<std::string, GenericAliasDeclaration*> Module::findGenericAlias(const std::string& block, const std::string& name)
    {
        for(auto&[hash, details_list] : generic_aliases)
        {
            if(!block.starts_with(hash)) continue;
            auto it = std::ranges::find_if(details_list, [&name](auto& details) { return details->name == name;});
            if(it == details_list.end()) return {"", nullptr};
            return {hash, it->get()};
        }
        return {"",nullptr};
    }

    Module::ClassDetails* Module::findType(const std::string& block, const std::string& name)
    {
        for(auto&[hash, details_list] : classes)
        {
            if(!block.starts_with(hash)) continue;
            for(auto& details : details_list)
                if(std::get<2>(details)->identifier.text == name) return &details;
        }
        return nullptr;
    }

    std::optional<std::string> Module::hashOf(const std::string& block, const std::string& name)
    {
        for(auto&[hash, details_list] : classes)
        {
            if(!block.starts_with(hash)) continue;
            auto it = std::ranges::find_if(details_list, [&name](auto& det)
            {
                return std::get<2>(det)->identifier.text == name;
            });
            if(it != details_list.end()) return hash;
        }
        if (auto [hash, intf] = findInterface(block, name); intf)
        {
            return hash;
        }
        return std::nullopt;
    }
    llvm::Type* Module::ToLLVMType(const Type& type, const std::string& hash, const std::vector<Type>& disallowed_types)
    {
        auto& context = *engine->llvm_context.getContext();
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
                auto ty = subtype.module->ToLLVMType(subtype, hash, disallowed_types);
                if(!ty) return nullptr;
                args.push_back(ty);
            }
            return llvm::StructType::get(context, args);
        }
        if(type.is_optional())
        {
            std::array<llvm::Type*, 2> args{};
            args[0] = type.subtypes[0].module->ToLLVMType(type.subtypes[0], hash, disallowed_types);
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
            auto& layout = code.getModuleUnlocked()->getDataLayout();
            for(auto& subtype : type.subtypes)
            {
                auto sub_t = subtype.module->ToLLVMType(subtype, hash, disallowed_types);
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
        if(type.is_ref_conversion_result())
        {
            return llvm::StructType::get(context, {llvm::PointerType::get(context, 0), llvm::Type::getInt1Ty(context)});
        }
        if(type.is_value_conversion_result())
        {
            return llvm::StructType::get(context, {
                ToLLVMType(type.subtypes[0], hash, disallowed_types), llvm::Type::getInt1Ty(context)});
        }
        if(type.is_static_array())
        {
            return llvm::ArrayType::get(ToLLVMType(type.subtypes[0], hash, disallowed_types), type.static_array_size());
        }
        if (type.is_slice())
        {
            return llvm::StructType::get(context, {
                llvm::PointerType::get(context, 0), //data
                llvm::Type::getInt64Ty(context) //size
            });
        }
        if (type.is_view())
        {
            if (type.subtypes[0].is_str()) debugbreak();
            auto& sub = type.subtypes[0];
            auto [hsh, intf] = sub.module->findInterface(sub.block_hash, sub.name);
            if (intf)
            {
                //we add 1 to store the actual class
                std::vector<llvm::Type*> subtypes(intf->methods.size() + 1, llvm::PointerType::get(context, 0));
                return llvm::StructType::get(context, subtypes);
            }
            return nullptr;
        }
        if(type.is_lambda()) return nullptr;
        if(auto t = findType(type.block_hash, type.name))
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
        module->module_hash = "__builtin";
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
        auto& ctx = *eng->llvm_context.getContext();
        module->code = llvm::orc::ThreadSafeModule(std::make_unique<llvm::Module>("__builtin", ctx), eng->llvm_context);
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
                mangled_name_for("plus"), module->code.getModuleUnlocked());
            auto minus_fn = llvm::Function::Create(fn_ty, llvm::GlobalValue::ExternalLinkage,
                mangled_name_for("minus"), module->code.getModuleUnlocked());
            auto mul_fn = llvm::Function::Create(fn_ty, llvm::GlobalValue::ExternalLinkage,
                mangled_name_for("mul"), module->code.getModuleUnlocked());
            auto div_fn = llvm::Function::Create(fn_ty, llvm::GlobalValue::ExternalLinkage,
                mangled_name_for("div"), module->code.getModuleUnlocked());

            plus_fn->addFnAttr(llvm::Attribute::AlwaysInline);
            minus_fn->addFnAttr(llvm::Attribute::AlwaysInline);
            mul_fn->addFnAttr(llvm::Attribute::AlwaysInline);
            div_fn->addFnAttr(llvm::Attribute::AlwaysInline);

            builder.SetInsertPoint(llvm::BasicBlock::Create(ctx, "entry", plus_fn));
            if(t.is_integral()) builder.CreateRet(builder.CreateAdd(plus_fn->getArg(0), plus_fn->getArg(1)));
            else builder.CreateRet(builder.CreateFAdd(plus_fn->getArg(0), plus_fn->getArg(1)));
            operators.add_binary_detail_for(TokenType::Plus, t,t, t);

            builder.SetInsertPoint(llvm::BasicBlock::Create(ctx, "entry", minus_fn));
            if(t.is_integral()) builder.CreateRet(builder.CreateSub(minus_fn->getArg(0), minus_fn->getArg(1)));
            else builder.CreateRet(builder.CreateFSub(minus_fn->getArg(0), minus_fn->getArg(1)));
            operators.add_binary_detail_for(TokenType::Minus, t,t,t);

            builder.SetInsertPoint(llvm::BasicBlock::Create(ctx, "entry", mul_fn));
            if(t.is_integral()) builder.CreateRet(builder.CreateMul(mul_fn->getArg(0), mul_fn->getArg(1)));
            else builder.CreateRet(builder.CreateFMul(mul_fn->getArg(0), mul_fn->getArg(1)));
            operators.add_binary_detail_for(TokenType::Star, t,t,t);

            builder.SetInsertPoint(llvm::BasicBlock::Create(ctx, "entry", div_fn));
            if(t.is_signed_integral()) builder.CreateRet(builder.CreateSDiv(div_fn->getArg(0), div_fn->getArg(1)));
            else if(t.is_unsigned_integral()) builder.CreateRet(builder.CreateUDiv(div_fn->getArg(0), div_fn->getArg(1)));
            else builder.CreateRet(builder.CreateFDiv(div_fn->getArg(0), div_fn->getArg(1)));
            operators.add_binary_detail_for(TokenType::Slash, t,t,t);
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
                module->code.getModuleUnlocked());
            builder.SetInsertPoint(llvm::BasicBlock::Create(ctx, "entry", fn));
            if(t.is_integral()) builder.CreateRet(builder.CreateNeg(fn->getArg(0)));
            else builder.CreateRet(builder.CreateFNeg(fn->getArg(0)));
            operators.un_overloads.emplace_back(t,t);
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
                module->code.getModuleUnlocked());
            builder.SetInsertPoint(llvm::BasicBlock::Create(ctx, "entry", fn));
            if(t.is_signed_integral()) builder.CreateRet(builder.CreateSRem(fn->getArg(0), fn->getArg(1)));
            else builder.CreateRet(builder.CreateURem(fn->getArg(0), fn->getArg(1)));
            operators.add_binary_detail_for(TokenType::Percent, t,t,t);
        }
        auto iterator = std::make_unique<GenericInterfaceDeclaration>();
        FunctionSignature sig;
        sig.returnType = Type{ "__opt", { Type{"OutputTy"} } };
        sig.parameters.emplace_back(FunctionParameter{ .type = Type {"__ref_mut", {Type{"This"}}}, .name = "this" });
        iterator->clause = GenericClause{ {"OutputTy"} };
        iterator->name = "Iterator";
        iterator->methods.emplace_back(std::make_unique<FunctionDeclaration>("next", std::move(sig), nullptr));
        module->generic_interfaces[module->module_hash].emplace_back(std::move(iterator));
    }
    void Module::dumpIR()
    {
        code.getModuleUnlocked()->print(llvm::outs(), nullptr);
    }
}
