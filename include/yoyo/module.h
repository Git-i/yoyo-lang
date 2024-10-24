#pragma once
namespace Yoyo
{
    struct Module
    {
        std::unique_ptr<llvm::Module> code;
        std::unordered_map<std::string, FunctionSignature> functions;
        std::unordered_map<std::string, ClassDeclaration*> classes;
        FunctionSignature* findFunction(const std::string& name)
        {
            if(auto fn = functions.find(name); fn != functions.end())
            {
                return &fn->second;
            }
            return nullptr;
        }
    };
}