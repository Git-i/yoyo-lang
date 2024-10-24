#pragma once
namespace Yoyo
{
    struct Module
    {
        std::unique_ptr<llvm::Module> code;
        std::unordered_map<std::string, FunctionDeclaration> functions;
        FunctionDeclaration* findFunction(const std::string& name);
        {
            if(auto fn = functions.find(name); fn != functions.end())
            {
                return &fn->second;
            }
            return nullptr;
        }
    };
}