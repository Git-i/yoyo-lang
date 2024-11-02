#pragma once
#include <memory>
#include <string>
#include <vector>
#include <unordered_map>


namespace Yoyo {
    struct Module;
    class Statement;

    class Engine
    {
    public:
        Engine();
        ~Engine();
        void addModule(const std::string& module_name, std::string source);
        void compile();
        std::unordered_map<std::string, std::unique_ptr<Module>> modules;
        std::unordered_map<std::string, std::vector<std::unique_ptr<Statement>>> sources;
        void* llvm_context;
    };
}
