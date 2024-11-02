#pragma once
#include <type.h>
#include <unordered_map>
#include <llvm/IR/Module.h>



namespace Yoyo
{
    struct Module
    {
        std::unique_ptr<llvm::Module> code;
        std::unordered_map<std::string, FunctionSignature> functions;
        std::unordered_map<std::string, std::tuple<std::string,llvm::StructType*, ClassDeclaration*>> classes;
        std::unordered_map<std::string, Type> aliases;
        std::unordered_map<std::string, Module*> modules;

        Engine* engine;
        std::string module_hash;
        FunctionSignature* findFunction(const std::string& name);

        llvm::Type* ToLLVMType(const Type& type, bool is_ref);
    };
}
