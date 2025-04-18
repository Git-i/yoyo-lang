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
		StructTypeSelector* parent;
		NativeTy* node_type;
		NativeTy* struct_type;
		NativeTy* get_struct_type(std::span<NativeTy* const>);
	};
}