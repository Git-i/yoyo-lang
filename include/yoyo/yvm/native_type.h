#pragma once
#include "ffi.h"
#include <span>
#include <vector>
namespace Yoyo
{
	using NativeTy = ffi_type;
	struct StructNativeTy : public NativeTy {
		std::vector<size_t> offsets;
	};
	namespace NativeType
	{
		NativeTy* getI8();
		NativeTy* getI16();
		NativeTy* getI32();
		NativeTy* getI64();

		NativeTy* getU8();
		NativeTy* getU16();
		NativeTy* getU32();
		NativeTy* getU64();

		NativeTy* getF32();
		NativeTy* getF64();

		NativeTy* getPtrTy();

		StructNativeTy* makeForStruct(std::span<NativeTy* const>);
		size_t getElementOffset(const StructNativeTy* type, size_t idx);
		uint32_t get_size(NativeTy*);
		void freeForStruct(StructNativeTy*);
	}
}