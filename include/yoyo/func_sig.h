#pragma once
#include "type.h"
namespace Yoyo
{
    struct FunctionParameter
    {
        Type type;
        std::string name;
    };
    struct YOYO_API FunctionSignature
    {
        Type returnType;
        bool return_is_ref;
        std::vector<FunctionParameter> parameters;
        std::string pretty_name(const std::string& block_hash) const;
    };
}