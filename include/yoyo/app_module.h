#pragma once
#include "module.h"
#include <func_sig.h>
namespace Yoyo
{
    class AppModule : public Module
    {
    public:
        void addFunction(std::string sig, void* func, std::string name);
        std::string_view viewString(void* str);
    };
}