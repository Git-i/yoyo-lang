#pragma once
#include <unordered_map>

#include "constraint.h"
namespace Yoyo {
struct GenericClause {
    std::vector<std::string> types;
    std::unordered_map<std::string, std::vector<Constraint>> constraints;
};
}  // namespace Yoyo
