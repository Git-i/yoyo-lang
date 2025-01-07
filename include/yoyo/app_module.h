#pragma once
#include "module.h"
#include <func_sig.h>
namespace Yoyo
{
    class YOYO_API AppModule : public Module
    {
    public:
        void addFunction(std::string sig, void* func, std::string name);
    };
}