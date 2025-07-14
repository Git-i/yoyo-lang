#pragma once
#include <vector>
#include <memory>
#include "native_type.h"
#include <unordered_map>
#include <set>
namespace Yoyo {
	struct UniqueFISelector {
	private:
		NativeProto* initialize_proto(NativeTy* ret);
	public:
		std::vector<std::unique_ptr<UniqueFISelector>> children;
		UniqueFISelector* parent = nullptr;
		NativeTy* node_type = nullptr;
		std::unordered_map<NativeTy*, NativeProto*> protos; //the key is for the return type
		NativeProto* get_proto(std::span<NativeTy*>, NativeTy* ret);
	};

	struct StructTypeSelector {
	private:
		NativeTy* initialize_struct_type();
	public:
		std::vector<std::unique_ptr<StructTypeSelector>> children;
		StructTypeSelector* parent = nullptr;
		NativeTy* node_type = nullptr;
		NativeTy* struct_type = nullptr;
		NativeTy* get_struct_type(std::span<NativeTy* const>);
	};

	struct UnionTypeSelector {
		std::vector<std::pair<std::set<NativeTy*>, NativeTy*>> used_unions;
	public:
		NativeTy* get_union_type(std::span<NativeTy* const>);
	};

	struct ArrayTypeSelector {
		// there has to be a better data structure for this
		// basically we map each type to map of size to array type
		std::unordered_map<
			NativeTy*,
			std::unordered_map<size_t, NativeTy*>> used_arrays;
	public:
		NativeTy* get_array_type(NativeTy* tp, size_t size);
	};
}