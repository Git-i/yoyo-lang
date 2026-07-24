#pragma once

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "common.h"
#include "runtime.h"
namespace Yoyo {
class ModuleBase;
class Statement;
class Type;
struct Constant;
class IRGenerator;
namespace BorrowChecker { struct FunctionSummary; }
class YOYO_API Engine {
protected:
    friend class IRGenerator;
    friend class ModuleBase;
    Runtime rt;
    std::unique_ptr<std::unordered_map<std::string, BorrowChecker::FunctionSummary>> function_borrow_checker_infos;
public:
    Engine();
    virtual ~Engine() = default;
    Engine(Engine&&) noexcept = default;
    virtual void* createGlobalConstant(const Type& type,
                                       const std::vector<Constant>& args,
                                       IRGenerator*) = 0;
    std::unordered_map<std::string, std::unique_ptr<ModuleBase>> modules;
    std::unordered_map<
        std::string,
        std::pair<std::string, std::vector<std::unique_ptr<Statement>>>>
        sources;
    static std::string_view viewString(void* str);
    void execute();
    template <class Rep, class Period>
    void sleep(const std::chrono::duration<Rep, Period>& sleep_duration) {
        sleep(std::chrono::duration_cast<std::chrono::milliseconds>(
                  sleep_duration)
                  .count());
    }
    const std::vector<std::unique_ptr<Statement>>* get_module_parse_output(
        const std::string& mod);
    void sleep(uint64_t milliseconds);
};
}  // namespace Yoyo
