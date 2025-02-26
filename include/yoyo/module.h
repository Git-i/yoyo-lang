#pragma once

#include <statement.h>
#include <type.h>
#include <unordered_map>
#include <llvm/IR/Module.h>
#include "overload_details.h"
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>

namespace Yoyo
{
    class Engine;
    struct YOYO_API Module
    {
        using ClassDetails = std::tuple<std::string,llvm::StructType*, std::unique_ptr<ClassDeclaration>>;
        struct FunctionDetails
        {
            std::string name;
            FunctionSignature sig;
        };
        llvm::orc::ThreadSafeModule code;
        std::unordered_map<std::string, std::vector<FunctionDetails>> functions;
        std::unordered_map<std::string, std::vector<ClassDetails>> classes;
        std::unordered_map<std::string, std::vector<std::unique_ptr<GenericFunctionDeclaration>>> generic_fns;
        std::unordered_map<std::string, std::unordered_map<std::string, Type>> aliases;
        std::unordered_map<std::string, std::vector<std::unique_ptr<InterfaceDeclaration>>> interfaces;
        std::unordered_map<std::string, std::vector<std::unique_ptr<GenericAliasDeclaration>>> generic_aliases;
        std::unordered_map<std::string, std::vector<std::unique_ptr<GenericInterfaceDeclaration>>> generic_interfaces;
        std::unordered_map<std::string, std::vector<std::unique_ptr<GenericClassDeclaration>>> generic_classes;
        std::unordered_map<std::string, std::vector<std::unique_ptr<EnumDeclaration>>> enums;
        std::unordered_map<std::string, std::pair<llvm::StructType*, std::unique_ptr<LambdaExpression>>> lambdas;
        std::unordered_map<std::string, std::vector<std::tuple<Type, std::string, llvm::Constant*>>> constants;
        std::unordered_map<std::string, std::vector<std::pair<std::unique_ptr<UnionDeclaration>, llvm::StructType*>>> unions;
        std::unordered_map<std::string, std::vector<std::unique_ptr<UnionDeclaration>>> generic_unions;
        std::unordered_map<std::string, Module*> modules;
        ModuleOverloadDetails overloads;
        Engine* engine;
        std::string module_hash;
        std::pair<std::string, FunctionDetails*> findFunction(const std::string& block, const std::string& name);
        std::pair<std::string, GenericFunctionDeclaration*> findGenericFn(const std::string& block, const std::string& name);
        Type* findAlias(const std::string& block, const std::string& name);
        std::pair<std::string, GenericAliasDeclaration*> findGenericAlias(const std::string& block, const std::string& name);
        ClassDetails* findType(const std::string& block, const std::string& name);
        std::pair<std::string, GenericClassDeclaration*> findGenericClass(const std::string& block, const std::string& name);
        std::pair<std::string, InterfaceDeclaration*> findInterface(const std::string& block, const std::string& name);
        std::pair<std::string, GenericInterfaceDeclaration*> findGenericInterface(const std::string& block, const std::string& name);
        std::pair<std::string, EnumDeclaration*> findEnum(const std::string& block, const std::string& name);
        std::pair<std::string, UnionDeclaration*> findUnion(const std::string& block, const std::string& name);
        std::pair<std::string, std::pair<std::unique_ptr<UnionDeclaration>, llvm::StructType*>*>findUnionWithType(const std::string& block, const std::string& name);
        std::pair<std::string, std::tuple<Type, std::string, llvm::Constant*>*> findConst(const std::string& block, const std::string& name);
        std::optional<std::string> hashOf(const std::string& base_block, const std::string& name);
        llvm::Type* ToLLVMType(const Type& type, const std::string& hash, IRGenerator*, const std::vector<Type>& disallowed_types);
        void dumpIR();
    };
    void makeBuiltinModule(Engine* eng);
    /*
     * void makeStd();
     * void makeStdExtended();
     */
}
