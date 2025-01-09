#pragma once

#include <memory>
#include <string>
#include <vector>
#include <unordered_map>
#include "app_module.h"
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>


namespace Yoyo {
    struct Module;
    class Statement;

    class YOYO_API Engine
    {
    public:
        Engine();
        ~Engine();
        Engine(Engine&&) noexcept = default;
        AppModule* addAppModule(const std::string& name);
        void addModule(const std::string& module_name, std::string source);
        void compile();
        void prepareForExecution();
        std::unordered_map<std::string, std::unique_ptr<Module>> modules;
        std::unordered_map<std::string, std::pair<std::string, std::vector<std::unique_ptr<Statement>>>> sources;
        static std::string_view viewString(void* str);
        llvm::orc::ThreadSafeContext llvm_context;
        std::unique_ptr<llvm::orc::LLJIT> jit;
        
    };
}
