#pragma once
#include "type.h"
namespace Yoyo
{
    struct FunctionParameter
    {
        Type type;
        std::string name;
    };
    struct FunctionSignature
    {
        Type returnType;
        bool return_is_ref;
        std::vector<FunctionParameter> parameters;
    };
}