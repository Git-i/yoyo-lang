#include "engine.h"
#include "yvm/yvm_module.h"
#include <ir_gen.h>
#include <module.h>
#include <parser.h>
#include <ranges>
#include <statement.h>
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

    Engine::~Engine()
    {

        //delete static_cast<llvm::LLVMContext*>(llvm_context);
    }

    std::string_view Engine::viewString(void* str)
    {
        struct String{char* data; uint64_t len; uint64_t cap;};
        auto arg_as_str = static_cast<String*>(str);
        return std::string_view{arg_as_str->data, arg_as_str->len};
    }

}
