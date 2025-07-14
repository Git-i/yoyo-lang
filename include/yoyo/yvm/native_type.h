#pragma once
#include "ffi.h"
#include <span>
#include <vector>
#include "yoyo_vm/vm.h"
namespace Yoyo
{
	using NativeTy = ffi_type;
	struct NativeModule;
	struct NativeProto : public ffi_cif {
		std::vector<NativeTy*> args;
	};
	struct StructNativeTy : public NativeTy {
		std::vector<size_t> offsets;
	};
	struct ArrayNativeTy : public NativeTy { size_t original_size; };
	struct UnionNativeTy : public NativeTy {};
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
		NativeTy* getVoid();

		Yvm::VM::Type doCall(NativeProto* proto, size_t nargs, Yvm::VM::Type* args, void* function);

		NativeProto* get_proto_for(std::vector<NativeTy*>, NativeTy*);
		void destroy_proto(NativeProto*);

		StructNativeTy* makeForStruct(std::span<NativeTy* const>);
		UnionNativeTy* makeForUnion(std::span<NativeTy* const>);
		ArrayNativeTy* makeForArray(NativeTy* type, size_t size);
		size_t getElementOffset(const StructNativeTy* type, size_t idx);
		size_t getElementOffset(const ArrayNativeTy* type, size_t idx);
		NativeTy* getStructElementType(StructNativeTy* type, size_t idx);
		size_t getNumElements(const StructNativeTy* type);
		uint32_t get_size(NativeTy* const);
		uint32_t get_align(NativeTy* const);
		void freeForStruct(StructNativeTy*);
		void freeForUnion(UnionNativeTy*);

		NativeModule* load_native_library(const std::string& lib_name);
		void* get_library_fn(NativeModule* module, const std::string& fn_name);
		void free_native_library(NativeModule*);
	}
}