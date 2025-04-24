#pragma once
#include <vector>
#include <memory>
#include "native_type.h"
namespace Yoyo {
	struct UniqueFISelector {

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
}