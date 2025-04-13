#pragma once

#include "module.h"
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>

namespace Yoyo
{

    struct YOYO_API LLModule : public ModuleBase
    {
        llvm::orc::ThreadSafeModule code;

        std::unordered_map<ClassDeclaration*, llvm::StructType*> classes_types;
        std::unordered_map<UnionDeclaration*, llvm::StructType*> union_types;
        std::unordered_map<std::string, std::pair<llvm::StructType*, std::unique_ptr<LambdaExpression>>> lambdas;

        std::pair<std::string, std::tuple<std::string, llvm::StructType*, ClassDeclaration*>> findClassWithType(const std::string& block, const std::string& name);
        std::pair<std::string, std::pair<UnionDeclaration*, llvm::StructType*>>findUnionWithType(const std::string& block, const std::string& name);

        llvm::Type* ToLLVMType(const Type& type, const std::string& hash, IRGenerator*, const std::vector<Type>& disallowed_types);
        static void makeBuiltinModule(Engine* eng);
        void dumpIR();
    };
}