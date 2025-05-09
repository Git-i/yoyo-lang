#define MINICORO_IMPL
#include "runtime.h"
#include "minicoro.h"
#include <ranges>
namespace Yoyo {
    void Runtime::run() {
        while (!waiting.empty() || !routs.empty()) {
            if (routs.empty()) {
                printf("routs");
                for (size_t i = 0; i < waiting.size(); i++) {
                    auto& [coro, reason] = waiting[i];
                    if (std::holds_alternative<TimePoint>(reason) &&
                        std::get<TimePoint>(reason) > std::chrono::steady_clock::now())
                    {
                        routs.push_back(coro);
                        waiting.erase(waiting.begin() + i);
                    }
                }
            }
            for (size_t i = 0; i < routs.size(); i++) {
                auto rou = routs[i];
                mco_resume(rou);
                // if it was already placed in the suspend buffer
                // don't suspend again
                if (mco_status(rou) == MCO_SUSPENDED)
                {
                    auto key_range = waiting | std::views::keys;
                    if (auto find_it = std::ranges::find(key_range, rou); find_it == key_range.end())
                    {
                        waiting.emplace_back(rou, std::monostate{});
                    }
                    routs.erase(routs.begin() + i); i--;
                }
                for (size_t i = 0; i < waiting.size(); i++) {
                    auto& [coro, reason] = waiting[i];
                    if (std::holds_alternative<TimePoint>(reason) &&
                        std::get<TimePoint>(reason) > std::chrono::steady_clock::now())
                    {
                        routs.push_back(coro);
                        waiting.erase(waiting.begin() + i); i--;
                    }
                }
                if (mco_status(rou) == MCO_DEAD)
                {
                    auto find_it = std::ranges::find_if(waiting, [rou](const auto& elem) {
                        return std::holds_alternative<mco_coro*>(elem.second)
                            && std::get<mco_coro*>(elem.second) == rou;
                        });
                    if (find_it != waiting.end()) {
                        routs.push_back(find_it->first);
                        waiting.erase(find_it);
                    }
                    routs.erase(routs.begin() + i);
                    if (is_externally_owned[rou] == false) {
                        mco_destroy(rou);
                        is_externally_owned.erase(rou);
                    }
                    else {
                        dead_coros.push_back(rou);
                    }
                }
                for (size_t i = 0; i < dead_coros.size(); i++) {
                    if (is_externally_owned[dead_coros[i]] == false) {
                        mco_destroy(rou);
                        dead_coros.erase(dead_coros.begin() + i);
                    }
                }
            }
        }
    }
    void Runtime::add_job(mco_coro* co) {
        routs.emplace_back(co);
        is_externally_owned[co] = true;
    }
    void Runtime::release_job(mco_coro* co)
    {
        is_externally_owned[co] = false;
    }
}
