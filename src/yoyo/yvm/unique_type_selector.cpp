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
		types[child_depth - 1] = node_type;
		--child_depth;
		for (; child_depth >= 1; child_depth--) {
			types[child_depth - 1] = curr_par->node_type;
			curr_par = curr_par->parent;
		}
		struct_type = NativeType::makeForStruct(types);
		return struct_type;
	}
	NativeTy* StructTypeSelector::get_struct_type(std::span<NativeTy* const> data)
	{
		if (data.size() == 0) return NativeType::getU8();
		for (auto& child : children) {
			if (child->node_type == data[0])
			{
				if (data.size() == 1) return child->initialize_struct_type();
				return child->get_struct_type(std::span{ data.begin() + 1, data.end() });
			}	
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
	NativeProto* UniqueFISelector::initialize_proto(NativeTy* ret)
	{
		if (protos.contains(ret)) return protos.at(ret);
		size_t child_depth = 0;
		auto parent_iter = parent;
		while (parent_iter) child_depth++, parent_iter = parent_iter->parent;
		if (child_depth == 0) {
			protos[ret] = NativeType::get_proto_for({}, ret);
			return protos.at(ret);
		}
		std::vector<NativeTy*> types(child_depth);
		auto curr_par = parent;
		types[child_depth - 1] = node_type;
		--child_depth;
		for (; child_depth >= 1; child_depth--) {
			types[child_depth - 1] = curr_par->node_type;
			curr_par = curr_par->parent;
		}
		protos[ret] = NativeType::get_proto_for(types, ret);
		return protos.at(ret);
	}
	NativeProto* UniqueFISelector::get_proto(std::span<NativeTy*> data, NativeTy* ret)
	{
		if (data.empty()) return initialize_proto(ret);
		for (auto& child : children) {
			if (child->node_type == data[0])
			{
				if (data.size() == 1) return child->initialize_proto(ret);
				return child->get_proto(std::span{ data.begin() + 1, data.end() }, ret);
			}
		}
		size_t child_depth = 0;
		auto parent_iter = parent;
		while (parent_iter) child_depth++, parent_iter = parent_iter->parent;
		NativeTy* const* arr = data.data() - child_depth;
		auto should_child = this;
		for (auto ptr : data) {
			auto& new_child = should_child->children.emplace_back();
			new_child = std::make_unique<UniqueFISelector>();
			new_child->node_type = ptr;
			new_child->parent = should_child;
			should_child = new_child.get();
		}
		return should_child->initialize_proto(ret);
	}
	NativeTy* UnionTypeSelector::get_union_type(std::span<NativeTy* const> data)
	{
		auto data_as_set = std::set(data.begin(), data.end());
		for (auto& [set, type] : used_unions) {
			if (set == data_as_set) {
				return type;
			}
		}
		auto new_type = NativeType::makeForUnion(data);
		used_unions.emplace_back(std::move(data_as_set), new_type);
		return new_type;
	}
	NativeTy* ArrayTypeSelector::get_array_type(NativeTy* tp, size_t size)
	{
		auto& this_map = used_arrays[tp];
		if (!this_map.contains(size)) this_map[size] = NativeType::makeForArray(tp, size);
		return this_map.at(size);
	}
}

