#pragma once
#include "constraint.h"
#include <unordered_map>
namespace Yoyo
{
    struct GenericClause
    {
        //TODO: include constraints
        std::vector<std::string> types;
        std::unordered_map<std::string, std::vector<Constraint>> constraints;
    };
}
