#include "engine.h"
#include "yvm/yvm_module.h"
#include <ir_gen.h>
#include <module.h>
#include <parser.h>
#include <ranges>
#include <statement.h>
#include "minicoro.h"
namespace Yoyo
{

    extern "C"
        const char* __asan_default_options() {
        // Clang reports ODR Violation errors in mbedtls/library/certs.c.
        // NEED TO REPORT THIS ISSUE
        return "detect_container_overflow=0";
    }
    Engine::Engine()
    {
    }


    std::string_view Engine::viewString(void* str)
    {
        struct String{char* data; uint64_t len; uint64_t cap;};
        auto arg_as_str = static_cast<String*>(str);
        return std::string_view{arg_as_str->data, arg_as_str->len};
    }

    void Engine::execute()
    {
        rt.run();
    }

    void Engine::sleep(uint64_t milliseconds)
    {
        auto cor = mco_running();
        rt.waiting.emplace_back(
            cor,
            Runtime::TimePoint::clock::now() + std::chrono::milliseconds(milliseconds)
        );
        mco_yield(cor);
    }

}
