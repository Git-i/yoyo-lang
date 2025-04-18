#include "yvm/unique_type_selector.h"

namespace Yoyo
{
	NativeTy* StructTypeSelector::initialize_struct_type()
	{
		if (struct_type) return struct_type;
		size_t child_depth = 0;
		auto parent_iter = parent;
		while (parent_iter) child_depth++, parent_iter = parent_iter->parent;

		std::vector<NativeTy*> types(child_depth);
		auto curr_par = parent;
		for (; child_depth >= 1; child_depth--) {
			types[child_depth - 1] = curr_par->node_type;
			curr_par = curr_par->parent;
		}
		struct_type = NativeType::makeForStruct(types);
	}
	NativeTy* StructTypeSelector::get_struct_type(std::span<NativeTy* const> data)
	{
		for (auto& child : children) {
			if (child->node_type == data[0])
				if (data.size() == 1) return child->initialize_struct_type();
				return child->get_struct_type(std::span{ data.begin() + 1, data.end() });
		}
		size_t child_depth = 0;
		auto parent_iter = parent;
		while (parent_iter) child_depth++, parent_iter = parent_iter->parent;
		NativeTy* const* arr = data.data() - child_depth;
		auto should_child = this;
		for (auto ptr : data) {
			auto& new_child = should_child->children.emplace_back();
			new_child = std::make_unique<StructTypeSelector>();
			new_child->node_type = ptr;
			new_child->struct_type = nullptr;
			new_child->parent = should_child;
			should_child = new_child.get();
		}
		return should_child->initialize_struct_type();
	}
}

