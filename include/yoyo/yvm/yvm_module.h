#pragma once
#include "yvm_engine.h"
#include "module.h"
#include "native_type.h"
namespace Yoyo
{

    struct YOYO_API YVMModule : public ModuleBase
    {

        YVMModule& operator=(const YVMModule& other) = delete;
        YVMModule(const YVMModule&) = delete;
        YVMModule() = default;
        Yvm::Module code;
        std::unordered_map<ClassDeclaration*, StructNativeTy*> classes_types;
        std::unordered_map<UnionDeclaration*, StructNativeTy*> union_types;
        std::unordered_map<std::string, std::pair<StructNativeTy*, std::unique_ptr<LambdaExpression>>> lambdas;

        std::pair<std::string, std::tuple<std::string, StructNativeTy*, ClassDeclaration*>> findClassWithType(const std::string& block, const std::string& name);
        std::pair<std::string, std::pair<UnionDeclaration*, StructNativeTy*>>findUnionWithType(const std::string& block, const std::string& name);

        NativeTy* toNativeType(const Type& type, const std::string& hash, IRGenerator*, const std::vector<Type>& disallowed_types);
        static void makeBuiltinModule(YVMEngine* eng);
        void dumpIR();
    };
}