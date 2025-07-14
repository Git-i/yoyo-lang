#include "module.h"

#include <type.h>
#include <fn_type.h>
#include <statement.h>
#include <ir_gen.h>
#include <ranges>
#include "engine.h"
namespace Yoyo
{
    std::pair<std::string, ModuleBase::FunctionDetails*> ModuleBase::findFunction(const std::string& block, const std::string& name)
    {
        for(auto&[hash, details_list] : functions)
        {
            if(!block.starts_with(hash)) continue;
            for(auto& details : details_list)
                if(details.name == name) return {hash, &details};
        }
        return {"", nullptr};
    }
    std::pair<std::string, std::tuple<Type, std::string, std::variant<Constant, ConstantDeclaration*>>*> ModuleBase::findConst(const std::string& block, const std::string& name)
    {
        for (auto& [hash, details_list] : constants)
        {
            if (!block.starts_with(hash)) continue;
            for (auto& details : details_list)
                if (std::get<1>(details) == name) return { hash, &details };
        }
        return { "", nullptr };
    }
    std::pair<std::string, GenericFunctionDeclaration*> ModuleBase::findGenericFn(const std::string& block,
        const std::string& name)
    {
        for(auto&[hash, details_list] : generic_fns)
        {
            if(!block.starts_with(hash)) continue;
            for(auto& details : details_list)
                if(details->name == name) return {hash, details.get()};
        }
        return {"", nullptr};
    }

    Type* ModuleBase::findAlias(const std::string& block, const std::string& name)
    {
        // Aliases require special behaviour because they can shadow each other (only "This" is capable of this)
        std::vector<std::pair<std::string, Type*>> found_aliases;
        for(auto&[hash, details_list] : aliases)
        {
            if(!block.starts_with(hash)) continue;
            if(details_list.contains(name)) found_aliases.emplace_back(hash, &details_list.at(name));
        }
        if (found_aliases.empty()) return nullptr;
        // we pick the one with the longest name (i.e the deepest block)
        return std::ranges::max(found_aliases, {}, [](auto& elem) { return elem.first.length(); }).second;
    }
    std::pair<std::string, InterfaceDeclaration*> ModuleBase::findInterface(const std::string& block, const std::string& name)
    {
        for (auto& [hash, interface_list] : interfaces)
        {
            if (!block.starts_with(hash)) continue;
            for (auto& intf : interface_list)
                if (intf->name == name) return { hash, intf.get() };
        }
        return { "", nullptr };
    }
    std::pair<std::string, EnumDeclaration*> ModuleBase::findEnum(const std::string& block, const std::string& name)
    {
        for (auto& [hash, enum_list] : enums)
        {
            if (!block.starts_with(hash)) continue;
            for (auto& enm: enum_list)
                if (enm->identifier == name) return { hash, enm.get()};
        }
        return { "", nullptr };
    }
    std::pair<std::string, UnionDeclaration*> ModuleBase::findUnion(const std::string& block, const std::string& name)
    {
        for (auto& [hash, unn_list] : unions)
        {
            if (!block.starts_with(hash)) continue;
            for (auto& unn : unn_list)
                if (unn->name == name) return { hash, unn.get() };
        }
        return { "", nullptr };
    }
    MacroDeclaration* ModuleBase::findMacro(const std::string& block, const std::string& name)
    {
        for (auto& [hash, mac_list] : macros)
        {
            if (!block.starts_with(hash)) continue;
            for (auto& mac : mac_list)
                if (mac->name == name) return mac.get();
        }
        return nullptr;
    }
    
    std::pair<std::string, GenericInterfaceDeclaration*> ModuleBase::findGenericInterface(const std::string& block, const std::string& name)
    {
        for (auto& [hash, interface_list] : generic_interfaces)
        {
            if (!block.starts_with(hash)) continue;
            for (auto& intf : interface_list)
                if (intf->name == name) return { hash, intf.get() };
        }
        return { "", nullptr };
    }

    std::pair<std::string, GenericAliasDeclaration*> ModuleBase::findGenericAlias(const std::string& block, const std::string& name)
    {
        for(auto&[hash, details_list] : generic_aliases)
        {
            if(!block.starts_with(hash)) continue;
            auto it = std::ranges::find_if(details_list, [&name](auto& details) { return details->name == name;});
            if(it == details_list.end()) return {"", nullptr};
            return {hash, it->get()};
        }
        return {"",nullptr};
    }

    std::pair<std::string, std::pair<std::string, std::unique_ptr<ClassDeclaration>>*> ModuleBase::findClass(const std::string& block, const std::string& name)
    {
        for(auto&[hash, details_list] : classes)
        {
            if(!block.starts_with(hash)) continue;
            for(auto& details : details_list)
                if (details.second->name == name) return { hash, &details };
        }
        return { "",nullptr };
    }
    std::pair<std::string, GenericClassDeclaration*> ModuleBase::findGenericClass(const std::string& block, const std::string& name)
    {
        for (auto& [hash, details_list] : generic_classes)
        {
            if (!block.starts_with(hash)) continue;
            for (auto& details : details_list)
                if (details->name == name) return { hash, details.get() };
        }
        return { "", nullptr };
    }

    std::optional<std::string> ModuleBase::hashOf(const std::string& block, const std::string& name)
    {
        for(auto&[hash, details_list] : classes)
        {
            if(!block.starts_with(hash)) continue;
            auto it = std::ranges::find_if(details_list, [&name](auto& det)
            {
                return det.second->name == name;
            });
            if(it != details_list.end()) return hash;
        }
        if (auto [hash, intf] = findInterface(block, name); intf) return hash;
        if (auto [hash, intf] = findGenericInterface(block, name); intf) return hash;
        if (auto [hash, _] = findFunction(block, name); _) return hash;
        if (auto [hash, _] = findGenericFn(block, name); _) return hash;
        if (auto [hash, _] = findGenericClass(block, name); _) return hash;
        if (auto [hash, _] = findEnum(block, name); _) return hash;
        if (auto [hash, _] = findUnion(block, name); _) return hash;
        //if (modules.contains(name)) return modules.at(name)->module_hash;
        return std::nullopt;
    }
    
}
