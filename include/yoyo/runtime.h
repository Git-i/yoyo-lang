#pragma once
#include "common.h"
#include <vector>
#include <variant>
#include <chrono>
#include <unordered_map>
struct mco_coro;

namespace Yoyo {
    
	// super primitive runtime will probably replace later
    struct YOYO_API Runtime {
        std::vector<mco_coro*> routs;
        std::vector<mco_coro*> dead_coros;
        // the bool is a flag whether the fiber is externally owned
        std::unordered_map<mco_coro*, bool> is_externally_owned;
        using TimePoint = std::chrono::time_point<std::chrono::steady_clock>;
        std::vector<std::pair<mco_coro*,
            std::variant<mco_coro*, TimePoint, std::monostate>>> waiting;
        void run();
        void add_job(mco_coro*);
        void release_job(mco_coro*);
    };
    struct YOYO_API Fiber {
        void* parameters;
        void* return_val;
        mco_coro* coro;
        Runtime* rt;
        Fiber(void* param, void* ret_val, mco_coro* coro, Runtime* rt)
            : parameters(param), return_val(ret_val), coro(coro), rt(rt) {}
        Fiber(const Fiber&) = delete;
        template<typename T>
        T get() {
            auto val = std::move(*reinterpret_cast<T*>(return_val));
            forget();
            return val;
        }
        ~Fiber() {
            if(coro) rt->release_job(coro);
        }
        void forget() { rt->release_job(coro); coro = nullptr; }
    };
}