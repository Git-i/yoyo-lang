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
        uint32_t get_size(NativeTy* type)
        {
            return type->size;
        }
        void freeForStruct(StructNativeTy* type)
        {
            delete[] type->elements;
            delete type;
        }
        size_t getElementOffset(const StructNativeTy* obj, size_t idx)
        {
            return obj->offsets[idx];
        }
    }
    
}

