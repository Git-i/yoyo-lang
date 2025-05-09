#pragma once

#include <memory>
#include <string>
#include <vector>
#include <unordered_map>
#include "common.h"

#include "runtime.h"
namespace Yoyo {
    class ModuleBase;
    class Statement;
    class Type;
    struct Constant;
    class IRGenerator;
    class YOYO_API Engine
    {
    protected:
        Runtime rt;
    public:
        Engine();
        virtual ~Engine() = default;
        Engine(Engine&&) noexcept = default;
        virtual void* createGlobalConstant(const Type& type, const std::vector<Constant>& args, IRGenerator*) = 0;
        std::unordered_map<std::string, std::unique_ptr<ModuleBase>> modules;
        std::unordered_map<std::string, std::pair<std::string, std::vector<std::unique_ptr<Statement>>>> sources;
        static std::string_view viewString(void* str);
        void execute();
        template<class Rep, class Period>
        void sleep(const std::chrono::duration<Rep, Period>& sleep_duration) { 
            sleep(std::chrono::duration_cast<std::chrono::milliseconds>(sleep_duration).count());
        }
        void sleep(uint64_t milliseconds);
    };
}
