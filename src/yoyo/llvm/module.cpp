#include "llvm/llvm_module.h"
#include "engine.h"
#include <llvm/IR/IRBuilder.h>
#include "ir_gen.h"
#include <ranges>
#include <numbers>
#include <llvm/llvm_engine.h>
namespace Yoyo
{
    llvm::Type* LLModule::ToLLVMType(const Type& type, const std::string& hash, IRGenerator* irgen, const std::vector<Type>& disallowed_types)
    {
        auto& context = *reinterpret_cast<LLVMEngine*>(engine)->llvm_context.getContext();
        if (type.is_integral())
            return llvm::Type::getIntNTy(context, *type.integer_width());
        if (type.is_floating_point())
            return *type.float_width() == 32 ? llvm::Type::getFloatTy(context) : llvm::Type::getDoubleTy(context);
        if (type.is_boolean())
            return llvm::Type::getInt1Ty(context);
        if (type.name == "void")
            return llvm::Type::getVoidTy(context);
        if (type.is_opaque_pointer() || type.is_reference())
            return llvm::PointerType::get(context, 0);
        if (type.is_char())
            return llvm::Type::getInt32Ty(context);
        if (type.name == "__called_fn")
        {
            auto ptr_ty = llvm::PointerType::get(context, 0);
            //called functions are always struct{void* context, void* function}
            return llvm::StructType::get(context, { ptr_ty, ptr_ty });
        }
        if (type.is_tuple())
        {
            std::vector<llvm::Type*> args;
            for (auto& subtype : type.subtypes)
            {
                auto ty = reinterpret_cast<LLModule*>(subtype.module)->ToLLVMType(subtype, hash, irgen, disallowed_types);
                if (!ty) return nullptr;
                args.push_back(ty);
            }
            return llvm::StructType::get(context, args);
        }
        if (type.is_lambda())
        {
            return reinterpret_cast<LLModule*>(type.module)->lambdas.at(type.name).first;
        }
        if (type.is_optional())
        {
            std::array<llvm::Type*, 2> args{};
            args[0] = reinterpret_cast<LLModule*>(type.subtypes[0].module)->ToLLVMType(type.subtypes[0], hash, irgen, disallowed_types);
            args[1] = llvm::Type::getInt1Ty(context);
            return llvm::StructType::get(context, args);
        }
        if (type.is_str())
        {
            std::array<llvm::Type*, 3> args{};
            args[0] = llvm::PointerType::get(context, 0);
            for (auto& sub_t : std::ranges::subrange(args.begin() + 1, args.end())) sub_t = llvm::Type::getInt64Ty(context);
            return llvm::StructType::get(context, args);
        }
        if (type.is_variant())
        {
            std::array<llvm::Type*, 2> args{};
            size_t size = 0;
            auto& layout = code.getModuleUnlocked()->getDataLayout();
            for (auto& subtype : type.subtypes)
            {
                auto sub_t = reinterpret_cast<LLModule*>(subtype.module)->ToLLVMType(subtype, hash, irgen, disallowed_types);
                auto as_struct = llvm::dyn_cast_or_null<llvm::StructType>(sub_t);
                size_t sz = 0;
                if (!as_struct) sz = sub_t->getPrimitiveSizeInBits() / 8;
                else sz = layout.getStructLayout(as_struct)->getSizeInBytes();
                if (sz > size) size = sz;
            }
            args[0] = llvm::ArrayType::get(llvm::Type::getInt8Ty(context), size);
            args[1] = llvm::Type::getInt32Ty(context); // 2^32 is a reasonable amount of variant subtypes
            return llvm::StructType::get(context, args);
        }
        if (type.get_decl_if_enum())
        {
            return llvm::Type::getInt32Ty(context);
        }
        if (auto [blk, unn] = findUnionWithType(type.block_hash, type.name); unn.second)
        {
            return unn.second;
        }
        if (type.is_ref_conversion_result())
        {
            return llvm::StructType::get(context, { llvm::PointerType::get(context, 0), llvm::Type::getInt1Ty(context) });
        }
        if (type.is_value_conversion_result())
        {
            return llvm::StructType::get(context, {
                ToLLVMType(type.subtypes[0], hash, irgen, disallowed_types), llvm::Type::getInt1Ty(context) });
        }
        if (type.is_static_array())
        {
            return llvm::ArrayType::get(ToLLVMType(type.subtypes[0], hash, irgen, disallowed_types), type.static_array_size());
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
        if (type.is_lambda()) return nullptr;
        if (auto t = findClassWithType(type.block_hash, type.name); std::get<2>(t.second))
        {
            auto ptr = std::get<1>(t.second);
            if (ptr) return ptr;
            //class is not yet defined but is recursive
            if (auto find_it = std::ranges::find(disallowed_types, type); find_it != disallowed_types.end())
            {
                auto decl = std::get<2>(t.second);
                irgen->error(Error(decl, "Type is recursive"));
            }

            //class is not defined yet and not recursive
            auto not_allowed = disallowed_types;
            not_allowed.push_back(type);

            auto decl = std::get<2>(t.second);
            std::vector<llvm::Type*> args;
            for (auto& subvar : decl->vars)
            {
                auto& subtype = subvar.type;

                std::get<0>(t.second).swap(irgen->block_hash);
                subtype.saturate(this, irgen);
                std::get<0>(t.second).swap(irgen->block_hash);

                auto ty = reinterpret_cast<LLModule*>(subtype.module)->ToLLVMType(subtype, hash, irgen, not_allowed);
                if (!ty) return nullptr;
                args.push_back(ty);
            }
            return llvm::StructType::get(context, args);

        }
        return nullptr;
    }
    void addTrigFunctions(Module* md, std::span<Type> types, llvm::IRBuilder<>& bld);
    void addFloatConsts(Module* md, std::span<Type> types);
    void addRangeIteratorMethods(Module* md, std::span<Type> types, llvm::IRBuilder<>& bld);
    void LLModule::makeBuiltinModule(Engine* engi)
    {
        auto eng = reinterpret_cast<LLVMEngine*>(engi);
        if (eng->modules.contains("core")) return;
        eng->modules["core"] = std::make_unique<LLModule>();
        auto module = reinterpret_cast<LLModule*>(eng->modules.at("core").get());
        module->module_hash = "core";
        module->engine = eng;
        constexpr int32_t eq = 1;
        constexpr int32_t ne = 0;
        constexpr int32_t less = 2;
        constexpr int32_t greater = 3;
        constexpr int32_t unord = 4;
        auto cmp_eq = std::make_unique<EnumDeclaration>("CmpEq", decltype(EnumDeclaration::values){
            {"Eq", eq}, { "Ne", ne }
        }, decltype(EnumDeclaration::stats){});
        auto cmp_ord = std::make_unique<EnumDeclaration>("CmpOrd", decltype(EnumDeclaration::values){
            {"Eq", eq}, { "Less", less }, { "Greater", greater }
        }, decltype(EnumDeclaration::stats){});
        auto cmp_pord = std::make_unique<EnumDeclaration>("CmpPartOrd", decltype(EnumDeclaration::values){
            {"Eq", eq}, { "Less", less }, { "Greater", greater }, { "Unord", unord }
        }, decltype(EnumDeclaration::stats){});
        module->enums[module->module_hash].emplace_back(std::move(cmp_eq));
        module->enums[module->module_hash].emplace_back(std::move(cmp_ord));
        module->enums[module->module_hash].emplace_back(std::move(cmp_pord));

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
        for (auto& t : types)
        {
            auto as_llvm = module->ToLLVMType(t, "", nullptr, {});
            auto fn_ty = llvm::FunctionType::get(as_llvm, { as_llvm, as_llvm }, false);
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
            if (t.is_integral()) builder.CreateRet(builder.CreateAdd(plus_fn->getArg(0), plus_fn->getArg(1)));
            else builder.CreateRet(builder.CreateFAdd(plus_fn->getArg(0), plus_fn->getArg(1)));
            operators.add_binary_detail_for(TokenType::Plus, t, t, t);

            builder.SetInsertPoint(llvm::BasicBlock::Create(ctx, "entry", minus_fn));
            if (t.is_integral()) builder.CreateRet(builder.CreateSub(minus_fn->getArg(0), minus_fn->getArg(1)));
            else builder.CreateRet(builder.CreateFSub(minus_fn->getArg(0), minus_fn->getArg(1)));
            operators.add_binary_detail_for(TokenType::Minus, t, t, t);

            builder.SetInsertPoint(llvm::BasicBlock::Create(ctx, "entry", mul_fn));
            if (t.is_integral()) builder.CreateRet(builder.CreateMul(mul_fn->getArg(0), mul_fn->getArg(1)));
            else builder.CreateRet(builder.CreateFMul(mul_fn->getArg(0), mul_fn->getArg(1)));
            operators.add_binary_detail_for(TokenType::Star, t, t, t);

            builder.SetInsertPoint(llvm::BasicBlock::Create(ctx, "entry", div_fn));
            if (t.is_signed_integral()) builder.CreateRet(builder.CreateSDiv(div_fn->getArg(0), div_fn->getArg(1)));
            else if (t.is_unsigned_integral()) builder.CreateRet(builder.CreateUDiv(div_fn->getArg(0), div_fn->getArg(1)));
            else builder.CreateRet(builder.CreateFDiv(div_fn->getArg(0), div_fn->getArg(1)));
            operators.add_binary_detail_for(TokenType::Slash, t, t, t);
        }
        //comparison operators
        for (auto& t : types | std::views::filter([](auto& t) { return t.is_integral(); }))
        {
            //integer comparison is totally ordered
            Type result{ .name = "CmpOrd", .module = module, .block_hash = module->module_hash };
            std::string mangled_name = "__operator__cmp__" + t.name + "__" + t.name;
            auto as_llvm = module->ToLLVMType(t, "", nullptr, {});
            auto i32 = llvm::Type::getInt32Ty(ctx);
            auto fn_ty = llvm::FunctionType::get(i32, { as_llvm, as_llvm }, false);
            auto cmp_fn = llvm::Function::Create(fn_ty, llvm::GlobalValue::ExternalLinkage, mangled_name, module->code.getModuleUnlocked());
            builder.SetInsertPoint(llvm::BasicBlock::Create(ctx, "entry", cmp_fn));
            auto arg0 = cmp_fn->getArg(0), arg1 = cmp_fn->getArg(1);
            auto is_eq = builder.CreateICmpEQ(arg0, arg1);
            auto equal_bb = llvm::BasicBlock::Create(ctx, "equal_bb", cmp_fn);
            auto equal_cont = llvm::BasicBlock::Create(ctx, "equal_cont", cmp_fn);
            builder.CreateCondBr(is_eq, equal_bb, equal_cont);
            builder.SetInsertPoint(equal_bb);
            builder.CreateRet(llvm::ConstantInt::get(i32, eq));
            builder.SetInsertPoint(equal_cont);
            llvm::Value* is_gt = t.is_signed_integral() ? builder.CreateICmpSGT(arg0, arg1) : builder.CreateICmpUGT(arg0, arg1);
            auto gt_bb = llvm::BasicBlock::Create(ctx, "gt_bb", cmp_fn);
            auto gt_cont = llvm::BasicBlock::Create(ctx, "gt_cont", cmp_fn);
            builder.CreateCondBr(is_gt, gt_bb, gt_cont);
            builder.SetInsertPoint(gt_bb);
            builder.CreateRet(llvm::ConstantInt::get(i32, greater));
            builder.SetInsertPoint(gt_cont);
            builder.CreateRet(llvm::ConstantInt::get(i32, less));
            operators.add_binary_detail_for(TokenType::Spaceship, t, t, std::move(result));
        }
        for (auto& t : types | std::views::filter([](auto& t) { return t.is_floating_point(); }))
        {
            //float comparison is partially ordered because of NaN
            Type result{ .name = "CmpPartOrd", .module = module, .block_hash = module->module_hash };
            std::string mangled_name = "__operator__cmp__" + t.name + "__" + t.name;
            auto as_llvm = module->ToLLVMType(t, "", nullptr, {});
            auto i32 = llvm::Type::getInt32Ty(ctx);
            auto fn_ty = llvm::FunctionType::get(i32, { as_llvm, as_llvm }, false);
            auto cmp_fn = llvm::Function::Create(fn_ty, llvm::GlobalValue::ExternalLinkage, mangled_name, module->code.getModuleUnlocked());
            builder.SetInsertPoint(llvm::BasicBlock::Create(ctx, "entry", cmp_fn));
            auto arg0 = cmp_fn->getArg(0), arg1 = cmp_fn->getArg(1);
            auto is_eq = builder.CreateFCmpOEQ(arg0, arg1);
            auto equal_bb = llvm::BasicBlock::Create(ctx, "equal_bb", cmp_fn);
            auto equal_cont = llvm::BasicBlock::Create(ctx, "equal_cont", cmp_fn);
            builder.CreateCondBr(is_eq, equal_bb, equal_cont);
            builder.SetInsertPoint(equal_bb);
            builder.CreateRet(llvm::ConstantInt::get(i32, eq));
            builder.SetInsertPoint(equal_cont);
            llvm::Value* is_gt = builder.CreateFCmpOGT(arg0, arg1);
            auto gt_bb = llvm::BasicBlock::Create(ctx, "gt_bb", cmp_fn);
            auto gt_cont = llvm::BasicBlock::Create(ctx, "gt_cont", cmp_fn);
            builder.CreateCondBr(is_gt, gt_bb, gt_cont);
            builder.SetInsertPoint(gt_bb);
            builder.CreateRet(llvm::ConstantInt::get(i32, greater));
            builder.SetInsertPoint(gt_cont);
            llvm::Value* is_uno = builder.CreateFCmpUNO(arg0, arg1);
            auto uno_bb = llvm::BasicBlock::Create(ctx, "uno_bb", cmp_fn);
            auto uno_cont = llvm::BasicBlock::Create(ctx, "uno_cont", cmp_fn);
            builder.CreateCondBr(is_uno, uno_bb, uno_cont);
            builder.SetInsertPoint(uno_bb);
            builder.CreateRet(llvm::ConstantInt::get(i32, unord));
            builder.SetInsertPoint(uno_cont);
            builder.CreateRet(llvm::ConstantInt::get(i32, less));
            operators.add_binary_detail_for(TokenType::Spaceship, t, t, std::move(result));
        }
        for (auto& t : std::ranges::subrange(types.begin(), types.begin() + 6))
        {
            auto as_llvm = module->ToLLVMType(t, "", nullptr, {});
            auto fn_ty = llvm::FunctionType::get(as_llvm, { as_llvm }, false);
            auto mangled_name_for = [&t](const std::string& op_name)
                {
                    return "__operator__" + op_name + "__" + t.name;
                };
            auto fn = llvm::Function::Create(fn_ty, llvm::GlobalValue::ExternalLinkage, mangled_name_for("un_neg"),
                module->code.getModuleUnlocked());
            builder.SetInsertPoint(llvm::BasicBlock::Create(ctx, "entry", fn));
            if (t.is_integral()) builder.CreateRet(builder.CreateNeg(fn->getArg(0)));
            else builder.CreateRet(builder.CreateFNeg(fn->getArg(0)));
            operators.un_overloads.emplace_back(t, t);
        }
        for (auto& t : std::ranges::subrange(types.begin() + 2, types.end()))
        {
            auto as_llvm = module->ToLLVMType(t, "", nullptr, {});
            auto fn_ty = llvm::FunctionType::get(as_llvm, { as_llvm, as_llvm }, false);
            auto mangled_name_for = [&t](const std::string& op_name)
                {
                    return "__operator__" + op_name + "__" + t.name + "__" + t.name;
                };
            auto mod_fn = llvm::Function::Create(fn_ty, llvm::GlobalValue::ExternalLinkage, mangled_name_for("mod"),
                module->code.getModuleUnlocked());
            builder.SetInsertPoint(llvm::BasicBlock::Create(ctx, "entry", mod_fn));
            if (t.is_signed_integral()) builder.CreateRet(builder.CreateSRem(mod_fn->getArg(0), mod_fn->getArg(1)));
            else builder.CreateRet(builder.CreateURem(mod_fn->getArg(0), mod_fn->getArg(1)));
            operators.add_binary_detail_for(TokenType::Percent, t, t, t);

            auto sh_fn_ty = llvm::FunctionType::get(as_llvm, { as_llvm, as_llvm }, false);
            auto rsh_fn = llvm::Function::Create(sh_fn_ty, llvm::GlobalValue::ExternalLinkage, mangled_name_for("shr"),
                module->code.getModuleUnlocked());
            builder.SetInsertPoint(llvm::BasicBlock::Create(ctx, "entry", rsh_fn));
            builder.CreateRet(builder.CreateLShr(rsh_fn->getArg(0), rsh_fn->getArg(1)));
            operators.add_binary_detail_for(TokenType::DoubleGreater, t, t, t);

            auto lsh_fn = llvm::Function::Create(sh_fn_ty, llvm::GlobalValue::ExternalLinkage, mangled_name_for("shl"),
                module->code.getModuleUnlocked());
            builder.SetInsertPoint(llvm::BasicBlock::Create(ctx, "entry", lsh_fn));
            builder.CreateRet(builder.CreateShl(lsh_fn->getArg(0), lsh_fn->getArg(1)));
            operators.add_binary_detail_for(TokenType::DoubleLess, t, t, t);

        }
        auto iterator = std::make_unique<GenericInterfaceDeclaration>();
        FunctionSignature sig;
        sig.returnType = Type{ "__opt", { Type{"OutputTy"} } };
        sig.parameters.emplace_back(FunctionParameter{ .type = Type {"__ref_mut", {Type{"This"}}}, .name = "this" });
        iterator->clause = GenericClause{ {"OutputTy"} };
        iterator->name = "Iterator";
        iterator->methods.emplace_back(std::make_unique<FunctionDeclaration>("next", std::move(sig), nullptr));
        module->generic_interfaces[module->module_hash].emplace_back(std::move(iterator));
        addTrigFunctions(module, std::span{ types.begin(), types.begin() + 2 }, builder);
        addFloatConsts(module, std::span{ types.begin(), types.begin() + 2 });
        addRangeIteratorMethods(module, std::span{ types.begin() + 2, types.end() }, builder);


        module->functions["str::"].emplace_back(ModuleBase::FunctionDetails{
                .name = "c_str",
                .sig = FunctionSignature{
                    .returnType = Type{.name = "ptr", .module = module },
                    .parameters = { FunctionParameter{.type = Type{.name = "__ref", .subtypes = {Type{.name = "str", .module = module}}, .module = module}, .name = "this"}}
                },
                .attributes = std::vector{ Attribute{"public"}}
            });
        auto ptr_ty = llvm::PointerType::get(ctx, 0);
        auto size_ty = llvm::Type::getInt64Ty(ctx);
        auto c_str_sig = llvm::FunctionType::get(ptr_ty, { ptr_ty }, false);
        auto string_ty = llvm::StructType::get(ctx, { ptr_ty, size_ty, size_ty });
        auto c_str_fn = llvm::Function::Create(c_str_sig, llvm::GlobalValue::ExternalLinkage, "str::c_str", module->code.getModuleUnlocked());
        builder.SetInsertPoint(llvm::BasicBlock::Create(ctx, "entry", c_str_fn));
        builder.CreateRet(
            builder.CreateLoad(ptr_ty, builder.CreateStructGEP(string_ty, c_str_fn->getArg(0), 0)));

    }
    void addFloatConsts(LLModule* md, std::span<Type> types)
    {
        auto& ctx = *md->code.getContext().getContext();
        for (auto& type : types)
        {
            auto as_llvm = md->ToLLVMType(type, "", nullptr, {});
            md->constants[type.name + "::"].emplace_back(type, "QNAN", llvm::ConstantFP::getQNaN(as_llvm));
            md->constants[type.name + "::"].emplace_back(type, "SNAN", llvm::ConstantFP::getSNaN(as_llvm));
            md->constants[type.name + "::"].emplace_back(type, "PI", llvm::ConstantFP::get(as_llvm, std::numbers::pi));
            md->constants[type.name + "::"].emplace_back(type, "INV_PI", llvm::ConstantFP::get(as_llvm, std::numbers::inv_pi));
            md->constants[type.name + "::"].emplace_back(type, "E", llvm::ConstantFP::get(as_llvm, std::numbers::e));
            md->constants[type.name + "::"].emplace_back(type, "LN2", llvm::ConstantFP::get(as_llvm, std::numbers::ln2));
            md->constants[type.name + "::"].emplace_back(type, "LN10", llvm::ConstantFP::get(as_llvm, std::numbers::ln10));
            md->constants[type.name + "::"].emplace_back(type, "SQRT2", llvm::ConstantFP::get(as_llvm, std::numbers::sqrt2));
            md->constants[type.name + "::"].emplace_back(type, "SQRT3", llvm::ConstantFP::get(as_llvm, std::numbers::sqrt3));

        }
    }
    //integer types only
    void addRangeIteratorMethods(LLModule* md, std::span<Type> types, llvm::IRBuilder<>& bld)
    {
        auto& ctx = bld.getContext();
        for (auto& type : types) {
            auto return_ty = Type{ .name = "__opt", .subtypes = {type} };
            auto return_as_llvm = md->ToLLVMType(return_ty, "", nullptr, {});
            auto subtype_llvm = md->ToLLVMType(type, "", nullptr, {});
            auto range_as_llvm = llvm::StructType::get(ctx, { subtype_llvm, subtype_llvm }, false);
            auto range_ty = Type{ .name = "range_" + type.name, .module = md, .block_hash = "core::" };

            std::vector<std::unique_ptr<FunctionDeclaration>> methods;
            methods.emplace_back(
                std::make_unique<FunctionDeclaration>(
                    "next",
                    FunctionSignature{
                        .returnType = Type{.name = "__opt", .subtypes = {type}, .module = md},
                        .parameters = {
                            FunctionParameter{.type = Type{.name = "__ref_mut", .subtypes = {range_ty}}, .name = "this"}
                        }
                    },
                    nullptr
                )
            );
            std::vector<InterfaceImplementation> impl;
            impl.emplace_back(
                Type{ .name = "Iterator", .subtypes = {type}, .module = md, .block_hash = "core::" },
                SourceSpan{},
                std::move(methods)
            );
            auto decl_ptr =
                std::make_unique<ClassDeclaration>(
                    Token{ .text = range_ty.name },
                    std::vector<ClassVariable>{},
                    std::vector<std::unique_ptr<Statement>>{}, Ownership::Owning, std::move(impl));

            md->classes["core::"].emplace_back("core::range_" + type.name + "::", range_as_llvm, std::move(decl_ptr));

            auto ptr_ty = llvm::PointerType::get(bld.getContext(), 0);
            // for the .next() it translates to fn(:&mut range_ty) -> ty?
            auto iter_sig = llvm::FunctionType::get(llvm::Type::getVoidTy(bld.getContext()), { ptr_ty, ptr_ty }, false);

            auto iter_fn = llvm::Function::Create(
                iter_sig,
                llvm::GlobalValue::ExternalLinkage,
                "core::range_" + type.name + "::core::Iterator::<" + type.name + ">::next",
                md->code.getModuleUnlocked());

            iter_fn->addAttributeAtIndex(1, llvm::Attribute::get(ctx, llvm::Attribute::StructRet, return_as_llvm));

            bld.SetInsertPoint(llvm::BasicBlock::Create(ctx, "entry", iter_fn));
            auto start_ptr = bld.CreateStructGEP(range_as_llvm, iter_fn->getArg(1), 0);
            auto end_val = bld.CreateLoad(subtype_llvm,
                bld.CreateStructGEP(range_as_llvm, iter_fn->getArg(1), 1));
            auto old_start_val = bld.CreateLoad(subtype_llvm, start_ptr);
            auto start_val = bld.CreateAdd(old_start_val, llvm::ConstantInt::get(subtype_llvm, 1));
            bld.CreateStore(start_val, start_ptr);
            llvm::Value* cmp_res;
            if (type.is_signed_integral())
                cmp_res = bld.CreateICmpSLT(old_start_val, end_val);
            else cmp_res = bld.CreateICmpULT(old_start_val, end_val);
            bld.CreateStore(cmp_res, bld.CreateStructGEP(return_as_llvm, iter_fn->getArg(0), 1));
            bld.CreateStore(old_start_val, bld.CreateStructGEP(return_as_llvm, iter_fn->getArg(0), 0));
            bld.CreateRetVoid();
        }
    }
    void addTrigFunctions(LLModule* md, std::span<Type> types, llvm::IRBuilder<>& bld)
    {
        auto& ctx = *md->code.getContext().getContext();
        for (auto& type : types)
        {
            FunctionSignature sig;
            sig.returnType = type;
            sig.parameters.emplace_back(type, "this");
            ModuleBase::FunctionDetails sdets{ .name = "sin", .sig = sig, .attributes = std::vector{Attribute{"public"}} };
            ModuleBase::FunctionDetails cdets{ .name = "cos", .sig = sig, .attributes = std::vector{Attribute{"public"}} };
            ModuleBase::FunctionDetails tdets{ .name = "tan", .sig = sig, .attributes = std::vector{Attribute{"public"}} };
            md->functions[type.name + "::"].emplace_back(std::move(sdets));
            md->functions[type.name + "::"].emplace_back(std::move(cdets));
            //md->functions[type.name + "::"].emplace_back(std::move(tdets));

            auto as_llvm = md->ToLLVMType(type, "", nullptr, {});
            auto f_ty = llvm::FunctionType::get(as_llvm, { as_llvm }, false);
            auto sin_func = llvm::Function::Create(f_ty, llvm::GlobalValue::ExternalLinkage, type.name + "::" + "sin", md->code.getModuleUnlocked());
            bld.SetInsertPoint(llvm::BasicBlock::Create(ctx, "entry", sin_func));
            bld.CreateRet(bld.CreateIntrinsic(as_llvm, llvm::Intrinsic::sin, { sin_func->getArg(0) }));

            auto cos_func = llvm::Function::Create(f_ty, llvm::GlobalValue::ExternalLinkage, type.name + "::" + "cos", md->code.getModuleUnlocked());
            bld.SetInsertPoint(llvm::BasicBlock::Create(ctx, "entry", cos_func));
            bld.CreateRet(bld.CreateIntrinsic(as_llvm, llvm::Intrinsic::cos, { cos_func->getArg(0) }));

            //auto tan_func = llvm::Function::Create(f_ty, llvm::GlobalValue::ExternalLinkage, type.name + "::" + "tan", md->code.getModuleUnlocked());
            //bld.SetInsertPoint(llvm::BasicBlock::Create(ctx, "entry", tan_func));
            //bld.CreateRet(bld.CreateIntrinsic(as_llvm, llvm::Intrinsic::tan, { tan_func->getArg(0) }));
        }
    }
    void LLModule::dumpIR()
    {
        code.getModuleUnlocked()->print(llvm::outs(), nullptr);
    }
    std::pair<std::string, std::pair<UnionDeclaration*, llvm::StructType*>> LLModule::findUnionWithType(const std::string& block, const std::string& name)
    {
        for (auto& [hash, unn_list] : unions)
        {
            if (!block.starts_with(hash)) continue;
            for (auto& unn : unn_list)
                if (unn->name == name) return { hash, {unn.get(), union_types[unn.get()]} };
        }
        return { "", {nullptr, nullptr} };
    }
}
