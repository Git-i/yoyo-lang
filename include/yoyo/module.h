#pragma once

#include <statement.h>
#include <type.h>
#include <unordered_map>
#include "overload_details.h"
#include "constant.h"

namespace Yoyo
{
    class Engine;
    class ModuleBase
    {
    public:
        struct FunctionDetails
        {
            std::string name;
            FunctionSignature sig;
            std::vector<Attribute> attributes;
            bool is_private() const {
                return attributes.end()
                    == std::ranges::find_if(attributes, [](const auto& attr) { return attr.name == "public"; });
            }
        };
        ModuleOverloadDetails overloads;
        Engine* engine;
        std::unordered_map<std::string, ModuleBase*> modules;
        std::string module_hash;
        std::unordered_map<std::string, std::vector<FunctionDetails>> functions;
        std::unordered_map<std::string, std::vector<std::pair<std::string, std::unique_ptr<ClassDeclaration>>>> classes;
        std::unordered_map<std::string, std::vector<std::unique_ptr<UnionDeclaration>>> unions;
        std::unordered_map<std::string, std::vector<std::unique_ptr<GenericFunctionDeclaration>>> generic_fns;
        std::unordered_map<std::string, std::vector<std::unique_ptr<InterfaceDeclaration>>> interfaces;
        std::unordered_map<std::string, std::vector<std::unique_ptr<GenericAliasDeclaration>>> generic_aliases;
        std::unordered_map<std::string, std::vector<std::unique_ptr<GenericInterfaceDeclaration>>> generic_interfaces;
        std::unordered_map<std::string, std::unordered_map<std::string, Type>> aliases;
        std::unordered_map<std::string, std::vector<std::unique_ptr<GenericClassDeclaration>>> generic_classes;
        std::unordered_map<std::string, std::vector<std::unique_ptr<EnumDeclaration>>> enums;
        std::unordered_map<std::string, std::vector<std::tuple<Type, std::string, std::variant<Constant, ConstantDeclaration*>>>> constants;
        std::unordered_map<std::string, std::vector<std::unique_ptr<UnionDeclaration>>> generic_unions;
        std::unordered_map<std::string, std::vector<std::unique_ptr<MacroDeclaration>>> macros;
        std::pair<std::string, FunctionDetails*> findFunction(const std::string& block, const std::string& name);
        std::pair<std::string, GenericFunctionDeclaration*> findGenericFn(const std::string& block, const std::string& name);
        Type* findAlias(const std::string& block, const std::string& name);
        std::pair<std::string, GenericAliasDeclaration*> findGenericAlias(const std::string& block, const std::string& name);
        std::pair<std::string, GenericClassDeclaration*> findGenericClass(const std::string& block, const std::string& name);
        std::pair<std::string, InterfaceDeclaration*> findInterface(const std::string& block, const std::string& name);
        std::pair<std::string, GenericInterfaceDeclaration*> findGenericInterface(const std::string& block, const std::string& name);
        std::pair<std::string, EnumDeclaration*> findEnum(const std::string& block, const std::string& name);
        MacroDeclaration* findMacro(const std::string& block, const std::string& name);
        std::pair<std::string, UnionDeclaration*> findUnion(const std::string& block, const std::string& name);
        std::pair<std::string, std::pair<std::string, std::unique_ptr<ClassDeclaration>>*> findClass(const std::string& block, const std::string& name);
        std::pair<std::string, std::tuple<Type, std::string, std::variant<Constant, ConstantDeclaration*>>*> findConst(const std::string& block, const std::string& name);
        std::optional<std::string> hashOf(const std::string& base_block, const std::string& name);
    };
    /*
     * void makeStd();
     * void makeStdExtended();
     */
}
