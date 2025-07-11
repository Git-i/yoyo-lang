#include "yvm/yvm_module.h"
#include "engine.h"
#include "ir_gen.h"
#include <ranges>
#include <numbers>
#include <yvm/yvm_engine.h>
#include <yvm/yvm_irgen.h>
#include "yoyo_vm/disassembler.h"
namespace Yoyo
{
    
    std::pair<std::string, std::tuple<std::string, StructNativeTy*, ClassDeclaration*>> YVMModule::findClassWithType(const std::string& block, const std::string& name)
    {
        if (auto cls = findClass(block, name); cls.second) {
            return { cls.first, {cls.second->first, classes_types[cls.second->second.get()], cls.second->second.get() } };
        }
        return { "", {"", nullptr, nullptr}};
    }
    std::pair<std::string, std::pair<UnionDeclaration*, StructNativeTy*>> YVMModule::findUnionWithType(const std::string& block, const std::string& name)
    {
        if (auto unn = findUnion(block, name); unn.second) {
            return { unn.first, {unn.second, union_types[unn.second]}};
        }
        return { "", { nullptr, nullptr} };
    }
    NativeTy* YVMModule::toNativeType(const Type& type, const std::string& hash, IRGenerator* irgen, const std::vector<Type>& disallowed_types)
    {
        auto& eng = *reinterpret_cast<YVMEngine*>(engine);
        if (type.is_integral())
        {
            switch (*type.integer_width())
            {
            case 8: return type.is_unsigned_integral() ? NativeType::getU8() : NativeType::getI8();
            case 16: return type.is_unsigned_integral() ? NativeType::getU16() : NativeType::getI16();
            case 32: return type.is_unsigned_integral() ? NativeType::getU32() : NativeType::getI32();
            case 64: return type.is_unsigned_integral() ? NativeType::getU64() : NativeType::getI64();
            default: return nullptr;
            }
        }
        if (type.is_floating_point())
            return *type.float_width() == 32 ? NativeType::getF32() : NativeType::getF64();
        if (type.is_boolean())
            return NativeType::getU8();
        if (type.name == "void")
            return NativeType::getVoid();
        if (type.is_opaque_pointer() || type.is_reference())
            return NativeType::getPtrTy();
        if (type.is_char())
            return NativeType::getU32();
        if (type.name == "__called_fn")
        {
            auto ptr_ty = NativeType::getPtrTy();
            //called functions are always struct{void* context, void* function}
            return eng.struct_manager.get_struct_type({ { ptr_ty, ptr_ty } });
        }
        if (type.is_tuple())
        {
            std::vector<NativeTy*> args;
            for (auto& subtype : type.subtypes)
            {
                auto ty = reinterpret_cast<YVMModule*>(subtype.module)->toNativeType(subtype, hash, irgen, disallowed_types);
                if (!ty) return nullptr;
                args.push_back(ty);
            }
            return eng.struct_manager.get_struct_type(args);
        }
        if (type.is_lambda())
        {
            return reinterpret_cast<YVMModule*>(type.module)->lambdas.at(type.name).first;
        }
        if (type.is_optional())
        {
            std::array<NativeTy*, 2> args{};
            args[0] = reinterpret_cast<YVMModule*>(type.subtypes[0].module)->toNativeType(type.subtypes[0], hash, irgen, disallowed_types);
            args[1] = NativeType::getU8();
            return eng.struct_manager.get_struct_type(args);
        }
        if (type.is_str())
        {
            std::array<NativeTy*, 3> args{};
            args[0] = NativeType::getPtrTy();
            args[1] = NativeType::getU64();
            args[2] = NativeType::getU64();
            return eng.struct_manager.get_struct_type(args);
        }
        if (type.block_hash == "core::" && type.name == "Fiber") {
            auto ptr_ty = NativeType::getPtrTy();
            return eng.struct_manager.get_struct_type({ { ptr_ty, ptr_ty, ptr_ty, ptr_ty } });
        }
        if (type.is_variant())
        {
            //TODO: add array native type and union native type
            
            //std::array<llvm::Type*, 2> args{};
            //size_t size = 0;
            //auto& layout = code.getModuleUnlocked()->getDataLayout();
            //for (auto& subtype : type.subtypes)
            //{
            //    auto sub_t = reinterpret_cast<LLModule*>(subtype.module)->ToLLVMType(subtype, hash, irgen, disallowed_types);
            //    auto as_struct = llvm::dyn_cast_or_null<llvm::StructType>(sub_t);
            //    size_t sz = 0;
            //    if (!as_struct) sz = sub_t->getPrimitiveSizeInBits() / 8;
            //    else sz = layout.getStructLayout(as_struct)->getSizeInBytes();
            //    if (sz > size) size = sz;
            //}
            //args[0] = llvm::ArrayType::get(llvm::Type::getInt8Ty(context), size);
            //args[1] = llvm::Type::getInt32Ty(context); // 2^32 is a reasonable amount of variant subtypes
            //return llvm::StructType::get(context, args);
        }
        if (type.get_decl_if_enum())
        {
            return NativeType::getU32();
        }
        if (auto [blk, unn] = findUnionWithType(type.block_hash, type.name); unn.second)
        {
            return unn.second;
        }
        if (type.is_ref_conversion_result())
        {
            return eng.struct_manager.get_struct_type({ { NativeType::getPtrTy(), NativeType::getU8() } });
        }
        if (type.is_value_conversion_result())
        {
            return eng.struct_manager.get_struct_type({ {
                toNativeType(type.subtypes[0], hash, irgen, disallowed_types), NativeType::getU8() } });
        }
        if (type.is_static_array())
        {
            //TODO: Array native types
            //return llvm::ArrayType::get(ToLLVMType(type.subtypes[0], hash, irgen, disallowed_types), type.static_array_size());
        }
        if (type.is_slice())
        {
            return eng.struct_manager.get_struct_type({ {
                NativeType::getPtrTy(), //data
                NativeType::getU64() //size
                } });
        }
        if (type.is_view())
        {
            if (type.subtypes[0].is_str()) debugbreak();
            auto& sub = type.subtypes[0];
            auto [hsh, intf] = sub.module->findInterface(sub.block_hash, sub.name);
            if (intf)
            {
                //we add 1 to store the actual class
                std::vector<NativeTy*> subtypes(intf->methods.size() + 1, NativeType::getPtrTy());
                return eng.struct_manager.get_struct_type(subtypes);
            }
            return nullptr;
        }
        if (type.is_lambda()) return nullptr;
        if (auto t = findUnionWithType(type.block_hash, type.full_name_no_block()); t.second.first) {
            auto ptr = t.second.second;
            if (ptr) return ptr;
            //union is recursive
            if (auto find_it = std::ranges::find(disallowed_types, type); find_it != disallowed_types.end())
            {
                auto decl = t.second.first;
                irgen->error(Error(decl, "Type is recursive"));
            }
            auto not_allowed = disallowed_types;
            not_allowed.push_back(type);

            auto decl = t.second.first;

            std::array<NativeTy*, 2> final_args;

            std::vector<NativeTy*> args;
            for (auto& [name, field_ty] : decl->fields) {
                std::string block = t.first + type.full_name_no_block() + "::";
                if (irgen) block.swap(irgen->block_hash);
                field_ty.saturate(this, irgen);
                if (irgen) block.swap(irgen->block_hash);

                auto ty = reinterpret_cast<YVMModule*>(field_ty.module)->toNativeType(field_ty, hash, irgen, not_allowed);
                if (!ty) return nullptr;
                args.push_back(ty);
            }
            final_args[0] = eng.union_manager.get_union_type(args);
            final_args[1] = NativeType::getU32();
            return eng.struct_manager.get_struct_type(final_args);
        }
        if (auto t = findClassWithType(type.block_hash, type.full_name_no_block()); std::get<2>(t.second))
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
            std::vector<NativeTy*> args;
            for (auto& subvar : decl->vars)
            {
                auto& subtype = subvar.type;

                if(irgen) std::get<0>(t.second).swap(irgen->block_hash);
                subtype.saturate(this, irgen);
                if(irgen) std::get<0>(t.second).swap(irgen->block_hash);

                auto ty = reinterpret_cast<YVMModule*>(subtype.module)->toNativeType(subtype, hash, irgen, not_allowed);
                if (!ty) return nullptr;
                args.push_back(ty);
            }
            return eng.struct_manager.get_struct_type(args);

        }
        return nullptr;
    }
    
    void YVMModule::makeBuiltinModule(YVMEngine* eng) {
        auto module = std::make_unique<YVMModule>();
        auto mod = module.get();
        eng->modules["core"] = std::move(module);
        mod->module_hash = "core::";
        eng->vm.add_module(&mod->code);
        mod->engine = eng;
        //-----------Comparison enum---------------------
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
        mod->enums[mod->module_hash].emplace_back(std::move(cmp_eq));
        mod->enums[mod->module_hash].emplace_back(std::move(cmp_ord));
        mod->enums[mod->module_hash].emplace_back(std::move(cmp_pord));
        //---------------------------------------------------
        auto& operators = mod->overloads;
        std::array types = {
            Type{"f64", {}, nullptr, mod},
            Type{"f32", {}, nullptr, mod},
            Type{"i64", {}, nullptr, mod},
            Type{"i32", {}, nullptr, mod},
            Type{"i16", {}, nullptr, mod},
            Type{"i8", {}, nullptr, mod},
            Type{"u64", {}, nullptr, mod},
            Type{"u32", {}, nullptr, mod},
            Type{"u16", {}, nullptr, mod},
            Type{"u8", {}, nullptr, mod},
        };
        const auto add_for = std::unordered_map<std::string, Yvm::OpCode>{
            { "f64", Yvm::OpCode::FAdd64 },
            { "f32", Yvm::OpCode::FAdd32 },
            { "i64", Yvm::OpCode::Add64 },
            { "i32", Yvm::OpCode::Add32 },
            { "i16", Yvm::OpCode::Add16 },
            { "i8",  Yvm::OpCode::Add8 },
            { "u64", Yvm::OpCode::Add64 },
            { "u32", Yvm::OpCode::Add32 },
            { "u16", Yvm::OpCode::Add16 },
            { "u8",  Yvm::OpCode::Add8 },
        };
        const auto sub_for = std::unordered_map<std::string, Yvm::OpCode>{
            { "f64", Yvm::OpCode::FSub64 },
            { "f32", Yvm::OpCode::FSub32 },
            { "i64", Yvm::OpCode::Sub64 },
            { "i32", Yvm::OpCode::Sub32 },
            { "i16", Yvm::OpCode::Sub16 },
            { "i8",  Yvm::OpCode::Sub8 },
            { "u64", Yvm::OpCode::Sub64 },
            { "u32", Yvm::OpCode::Sub32 },
            { "u16", Yvm::OpCode::Sub16 },
            { "u8",  Yvm::OpCode::Sub8 },
        };
        const auto mul_for = std::unordered_map<std::string, Yvm::OpCode>{
            { "f64", Yvm::OpCode::FMul64 },
            { "f32", Yvm::OpCode::FMul32 },
            { "i64", Yvm::OpCode::Mul64 },
            { "i32", Yvm::OpCode::Mul32 },
            { "i16", Yvm::OpCode::Mul16 },
            { "i8",  Yvm::OpCode::Mul8 },
            { "u64", Yvm::OpCode::Mul64 },
            { "u32", Yvm::OpCode::Mul32 },
            { "u16", Yvm::OpCode::Mul16 },
            { "u8",  Yvm::OpCode::Mul8 },
        };
        const auto div_for = std::unordered_map<std::string, Yvm::OpCode>{
            { "f64", Yvm::OpCode::FDiv64 },
            { "f32", Yvm::OpCode::FDiv32 },
            { "i64", Yvm::OpCode::IDiv64 },
            { "i32", Yvm::OpCode::IDiv32 },
            { "i16", Yvm::OpCode::IDiv16 },
            { "i8",  Yvm::OpCode::IDiv8 },
            { "u64", Yvm::OpCode::UDiv64 },
            { "u32", Yvm::OpCode::UDiv32 },
            { "u16", Yvm::OpCode::UDiv16 },
            { "u8",  Yvm::OpCode::UDiv8 },
        };
        Yvm::Emitter em(false);
        for (auto& t : types) {
            auto mangled_name_for = [&t](const std::string& op_name)
                {
                    // __operator_<name>__<type_lhs>__<type_rhs>
                    return "__operator__" + op_name + "__" + t.name + "__" + t.name;
                };
            operators.add_binary_detail_for(TokenType::Plus, t, t, t, "");
            operators.add_binary_detail_for(TokenType::Minus, t, t, t, "");
            operators.add_binary_detail_for(TokenType::Star, t, t, t, "");
            operators.add_binary_detail_for(TokenType::Slash, t, t, t, "");

            em.write_1b_inst(add_for.at(t.name));
            em.write_1b_inst(Yvm::OpCode::Ret);
            em.close_function(&mod->code, mangled_name_for("plus"));

            em.write_1b_inst(sub_for.at(t.name));
            em.write_1b_inst(Yvm::OpCode::Ret);
            em.close_function(&mod->code, mangled_name_for("minus"));

            em.write_1b_inst(mul_for.at(t.name));
            em.write_1b_inst(Yvm::OpCode::Ret);
            em.close_function(&mod->code, mangled_name_for("mul"));

            em.write_1b_inst(div_for.at(t.name));
            em.write_1b_inst(Yvm::OpCode::Ret);
            em.close_function(&mod->code, mangled_name_for("div"));
        }
        // all the integral types
        for (auto& t : std::ranges::subrange(types.begin() + 2, types.end())) {
            Type result{ .name = "CmpOrd", .module = mod, .block_hash = mod->module_hash };
            std::string mangled_name = "__operator__cmp__" + t.name + "__" + t.name;

            em.write_2b_inst(Yvm::OpCode::StackAddr, 0);
            em.write_2b_inst(Yvm::OpCode::StackAddr, 1);
            em.write_2b_inst(Yvm::OpCode::CmpEq, *t.integer_width());
            auto ne_bb = em.unq_label_name("not equal");
            em.create_jump(Yvm::OpCode::JumpIfFalse, ne_bb);
            em.write_const(eq);
            em.write_1b_inst(Yvm::OpCode::Ret);

            em.create_label(ne_bb);
            em.write_2b_inst(t.is_signed_integral() ? Yvm::OpCode::ICmpGt : Yvm::OpCode::UCmpGt, *t.integer_width());
            auto ngt_bb = em.unq_label_name("not greater");
            em.create_jump(Yvm::OpCode::JumpIfFalse, ngt_bb);
            em.write_const(greater);
            em.write_1b_inst(Yvm::OpCode::Ret);

            em.create_label(ngt_bb);
            em.write_const(less);
            em.write_1b_inst(Yvm::OpCode::Ret);
            em.close_function(&mod->code, mangled_name);
            operators.add_binary_detail_for(TokenType::Spaceship, t, t, std::move(result), "");
        }
        registerStringDestructor(mod);
        auto iterator = std::make_unique<GenericInterfaceDeclaration>();
        FunctionSignature sig;
        sig.returnType = Type{ "__opt", { Type{"OutputTy"} } };
        sig.parameters.emplace_back(FunctionParameter{ .type = Type {"__ref_mut", {Type{"This"}}}, .name = "this" });
        iterator->clause = GenericClause{ {"OutputTy"} };
        iterator->name = "Iterator";
        iterator->methods.emplace_back(std::make_unique<FunctionDeclaration>("next", std::move(sig), nullptr));
        mod->generic_interfaces[mod->module_hash].emplace_back(std::move(iterator));
        //------------------core::sleep---------------------------------------
        mod->functions[mod->module_hash].emplace_back(FunctionDetails{
            .name = "sleep",
            .sig = FunctionSignature {
                .returnType = Type{ .name = "void", .module = mod },
                .return_is_ref = false,
                .parameters = { FunctionParameter{.type = Type{.name = "u64", .module = mod }, .name = "duration" } },
            },
            .attributes = { Attribute{"public"} }
        });
        em.write_2b_inst(Yvm::OpCode::ExternalIntrinsic, 11);
        em.close_function(&mod->code, "core::sleep");
        //-----------------------------------------------------------------------
    }
    std::string YVMModule::dumpIR()
    {
        std::string final;
        for (auto& [name, body] : code.code) {
            final += name + ":\n\n";
            final += Yvm::Disassembler::disassemble(body, &reinterpret_cast<YVMEngine*>(engine)->vm);
        }
        return final;
    }
    void YVMModule::registerStringDestructor(YVMModule* mod)
    {
        Yvm::Emitter em(false);
        auto str_ty = reinterpret_cast<StructNativeTy*>(mod->toNativeType(Type{ "str" }, "", nullptr, {}));
        em.write_ptr_off(NativeType::getElementOffset(str_ty, 0));
        em.write_2b_inst(Yvm::OpCode::Load, Yvm::Type::ptr);
        em.write_1b_inst(Yvm::OpCode::Free);
        em.close_function(&mod->code, "__destructor_for_str");
    }
}
