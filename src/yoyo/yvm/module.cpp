#include "yvm/yvm_module.h"
#include "engine.h"
#include "ir_gen.h"
#include <ranges>
#include <numbers>
#include <yvm/yvm_engine.h>
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
            return nullptr;
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
            std::vector<NativeTy*> args;
            for (auto& subvar : decl->vars)
            {
                auto& subtype = subvar.type;

                std::get<0>(t.second).swap(irgen->block_hash);
                subtype.saturate(this, irgen);
                std::get<0>(t.second).swap(irgen->block_hash);

                auto ty = reinterpret_cast<YVMModule*>(subtype.module)->toNativeType(subtype, hash, irgen, not_allowed);
                if (!ty) return nullptr;
                args.push_back(ty);
            }
            return eng.struct_manager.get_struct_type(args);

        }
        return nullptr;
    }
    void YVMModule::makeBuiltinModule(YVMEngine* eng) {

    }
}
