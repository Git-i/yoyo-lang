#pragma once

#include <memory>
#include <string>
#include <vector>
#include <unordered_map>
#include "app_module.h"
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>


namespace Yoyo {
    class ModuleBase;
    class Statement;
    class Type;
    class YOYO_API Engine
    {
    protected:
    public:
        Engine();
        ~Engine();
        Engine(Engine&&) noexcept = default;
        AppModule* addAppModule(const std::string& name);
        void addModule(const std::string& module_name, std::string source);
        void compile();
        void prepareForExecution();
        void addStaticLibrary(std::string_view path);
        void addDynamicLibrary(std::string_view path);
        virtual void* createGlobalConstant(const Type& type, const std::vector<Constant>& args, IRGenerator*) = 0;
        std::unordered_map<std::string, std::unique_ptr<ModuleBase>> modules;
        std::unordered_map<std::string, std::pair<std::string, std::vector<std::unique_ptr<Statement>>>> sources;
        static std::string_view viewString(void* str);



        llvm::orc::ThreadSafeContext llvm_context;
        std::unique_ptr<llvm::orc::LLJIT> jit;
        
    };
}
