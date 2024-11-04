#pragma once

#include <memory>
#include <string>
#include <vector>
#include <unordered_map>
#include "app_module.h"


namespace Yoyo {
    struct Module;
    class Statement;

    class Engine
    {
    public:
        Engine();
        ~Engine();
        /// Adds an app module
        AppModule* addAppModule(const std::string& name);
        void addModule(const std::string& module_name, std::string source);
        void compile();
        std::unordered_map<std::string, std::unique_ptr<Module>> modules;
        std::unordered_map<std::string, std::vector<std::unique_ptr<Statement>>> sources;
        void* llvm_context;
    };
}
