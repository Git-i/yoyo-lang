#pragma once
#include "type.h"
namespace Yoyo
{
    enum class ParamType
    {
        In, InOut
    };
    struct FunctionParameter
    {
        Type type;
        ParamType convention;
        std::string name;
    };
    struct FunctionSignature
    {
        Type returnType;
        bool return_is_ref;
        std::vector<FunctionParameter> parameters;
    };
}