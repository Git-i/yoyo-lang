#pragma once

#include <statement.h>
#include <type.h>
#include <unordered_map>
#include <llvm/IR/Module.h>
#include "overload_details.h"


namespace Yoyo
{
    class Engine;
    struct Module
    {
        using ClassDetails = std::tuple<std::string,llvm::StructType*, std::unique_ptr<ClassDeclaration>>;
        struct FunctionDetails
        {
            std::string name;
            FunctionSignature sig;
        };
        std::unique_ptr<llvm::Module> code;
        std::unordered_map<std::string, std::vector<FunctionDetails>> functions;
        std::unordered_map<std::string, std::vector<ClassDetails>> classes;
        std::unordered_map<std::string, std::vector<std::unique_ptr<GenericFunctionDeclaration>>> generic_fns;
        std::unordered_map<std::string, std::unique_ptr<EnumDeclaration>> enums;
        std::unordered_map<std::string, std::unordered_map<std::string, Type>> aliases;
        std::unordered_map<std::string, Module*> modules;
        ModuleOverloadDetails overloads;
        Engine* engine;
        std::string module_hash;
        std::pair<std::string, FunctionDetails*> findFunction(const std::string& block, const std::string& name);
        std::pair<std::string, GenericFunctionDeclaration*> findGenericFn(const std::string& block, const std::string& name);
        Type* findAlias(const std::string& block, const std::string& name);
        ClassDetails* findType(const std::string& block, const std::string& name);
        std::optional<std::string> hashOf(const std::string& base_block, const std::string& name);
        llvm::Type* ToLLVMType(const Type& type, const std::string& hash, const std::vector<Type>& disallowed_types);
    };
    void makeBuiltinModule(Engine* eng);
    /*
     * void makeStd();
     * void makeStdExtended();
     */
}
