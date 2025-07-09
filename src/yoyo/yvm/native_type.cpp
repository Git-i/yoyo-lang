#include "yvm/native_type.h"
#include <ranges>

namespace Yoyo
{
    namespace NativeType
    {
        NativeTy* getI8()
        {
            return &ffi_type_sint8;
        }

        NativeTy* getI16()
        {
            return &ffi_type_sint16;
        }

        NativeTy* getI32()
        {
            return &ffi_type_sint32;
        }

        NativeTy* getI64()
        {
            return &ffi_type_sint64;
        }

        NativeTy* getU8()
        {
            return &ffi_type_uint8;
        }

        NativeTy* getU16()
        {
            return &ffi_type_uint16;
        }

        NativeTy* getU32()
        {
            return &ffi_type_uint32;
        }

        NativeTy* getU64()
        {
            return &ffi_type_uint64;
        }

        NativeTy* getF32()
        {
            return &ffi_type_float;
        }
        NativeTy* getF64()
        {
            return &ffi_type_double;
        }
        NativeTy* getPtrTy()
        {
            return &ffi_type_pointer;
        }
        NativeTy* getVoid()
        {
            return &ffi_type_void;
        }
        Yvm::VM::Type doCall(NativeProto* proto, size_t nargs, Yvm::VM::Type* args, void* function)
        {
            using namespace Yvm;
            std::vector<void*> fn_args(proto->nargs);
            for (auto i : std::views::iota(0u, proto->nargs)) {
                //
                if (proto->arg_types[i] == &ffi_type_sint8) fn_args[i] = &args[i].i8;
                else if (proto->arg_types[i] == &ffi_type_uint8) fn_args[i] = &args[i].u8;
                else if (proto->arg_types[i] == &ffi_type_uint16) fn_args[i] = &args[i].u16;
                else if (proto->arg_types[i] == &ffi_type_sint16) fn_args[i] = &args[i].i16;
                else if (proto->arg_types[i] == &ffi_type_uint32) fn_args[i] = &args[i].u32;
                else if (proto->arg_types[i] == &ffi_type_sint32) fn_args[i] = &args[i].i32;
                else if (proto->arg_types[i] == &ffi_type_sint64) fn_args[i] = &args[i].i64;
                else if (proto->arg_types[i] == &ffi_type_uint64) fn_args[i] = &args[i].u64;
                else if (proto->arg_types[i] == &ffi_type_float) fn_args[i] = &args[i].f32;
                else if (proto->arg_types[i] == &ffi_type_double) fn_args[i] = &args[i].f64;
                else if (proto->arg_types[i] == &ffi_type_pointer) fn_args[i] = &args[i].ptr;
                else fn_args[i] = args[i].ptr;
            }
            ffi_arg ret_val;
            VM::Type ret_obj{ .u64 = 0 };
            void* ret_ptr = &ret_val;
            //----------libffi is wierd with integer return types------------
            if (proto->rtype == &ffi_type_sint8) ret_ptr = &ret_val;
            else if (proto->rtype == &ffi_type_uint8) ret_ptr = &ret_val;
            else if (proto->rtype == &ffi_type_uint16) ret_ptr = &ret_val;
            else if (proto->rtype == &ffi_type_sint16) ret_ptr = &ret_val;
            else if (proto->rtype == &ffi_type_uint32) ret_ptr = &ret_val;
            else if (proto->rtype == &ffi_type_sint32) ret_ptr = &ret_val;
            else if (proto->rtype == &ffi_type_sint64) ret_ptr = &ret_val;
            else if (proto->rtype == &ffi_type_uint64) ret_ptr = &ret_val;
            //--------------------------------------------------------------------
            else if (proto->rtype == &ffi_type_float) ret_ptr = &ret_obj.f32;
            else if (proto->rtype == &ffi_type_double) ret_ptr = &ret_obj.f64;
            else if (proto->rtype == &ffi_type_pointer) ret_ptr = &ret_obj.ptr;
            // for structural types yoyo supplies us a pointer to memory of sufficent size as the last arg
            else {
                ret_obj.ptr = args[proto->nargs].ptr;
                ret_ptr = ret_obj.ptr;
            }
            ffi_call(proto, reinterpret_cast<void(*)()>(function), ret_ptr, fn_args.data());
            if (proto->rtype == &ffi_type_sint8) ret_obj.i8 = reinterpret_cast<int8_t&>(ret_val);
            else if (proto->rtype == &ffi_type_uint8) ret_obj.u8 = reinterpret_cast<uint8_t&>(ret_val);
            else if (proto->rtype == &ffi_type_uint16) ret_obj.u16 = reinterpret_cast<uint16_t&>(ret_val);
            else if (proto->rtype == &ffi_type_sint16) ret_obj.i16 = reinterpret_cast<int16_t&>(ret_val);
            else if (proto->rtype == &ffi_type_uint32) ret_obj.u32 = reinterpret_cast<uint32_t&>(ret_val);
            else if (proto->rtype == &ffi_type_sint32) ret_obj.i32 = reinterpret_cast<int32_t&>(ret_val);
            else if (proto->rtype == &ffi_type_sint64) ret_obj.i64 = reinterpret_cast<int64_t&>(ret_val);
            else if (proto->rtype == &ffi_type_uint64) ret_obj.u64 = reinterpret_cast<uint64_t&>(ret_val);
            return ret_obj;
            
        }
        NativeProto* get_proto_for(std::vector<NativeTy*> args, NativeTy* ret_ty)
        {
            auto ret = new NativeProto;
            ret->args = std::move(args);
            ffi_prep_cif(ret, FFI_DEFAULT_ABI, ret->args.size(), ret_ty, ret->args.data());
            return ret;
        }
        void destroy_proto(NativeProto* arg)
        {
            delete arg;
        }
        StructNativeTy* makeForStruct(std::span<NativeTy* const> types)
        {
            auto ret_val = new StructNativeTy;
            ret_val->size = 0;
            ret_val->alignment = 0;
            ret_val->type = FFI_TYPE_STRUCT;
            ret_val->elements = new ffi_type*[types.size() + 1];
            ret_val->elements[types.size()] = nullptr;
            for (auto i : std::views::iota(size_t{ 0 }, types.size())) {
                ret_val->elements[i] = types[i];
            }
            ret_val->offsets.resize(types.size());
            ffi_get_struct_offsets(FFI_DEFAULT_ABI, ret_val, ret_val->offsets.data());
            return ret_val;
        }
        UnionNativeTy* makeForUnion(std::span<NativeTy* const> types)
        {
            auto ret_val = new UnionNativeTy;   
            auto biggest_size = std::ranges::max(types, {}, [](NativeTy* const arg) { return NativeType::get_size(arg); });
            auto biggest_align = std::ranges::max(types, {}, [](NativeTy* const arg) { return NativeType::get_align(arg); });
            ret_val->size = NativeType::get_size(biggest_size);
            ret_val->alignment = NativeType::get_align(biggest_align);
            ret_val->elements = new ffi_type*[2];
            ret_val->elements[1] = nullptr;
            return ret_val;
        }
        uint32_t get_size(NativeTy* const type)
        {
            return type->size;
        }
        uint32_t get_align(NativeTy* const type)
        {
            return type->alignment;
        }
        void freeForStruct(StructNativeTy* type)
        {
            delete[] type->elements;
            delete type;
        }
        void freeForUnion(UnionNativeTy* type) {
            delete[] type->elements;
            delete type;
        }
        size_t getElementOffset(const StructNativeTy* obj, size_t idx)
        {
            return obj->offsets[idx];
        }
        NativeTy* getStructElementType(StructNativeTy* type, size_t idx)
        {
            return type->elements[idx];
        }
        size_t getNumElements(const StructNativeTy* type)
        {
            return type->offsets.size();
        }
    }
    
}

