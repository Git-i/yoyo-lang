#pragma once
#include <variant>

#include "type.h"
namespace Yoyo {
struct ImplConstraint {
    Type other;
};
struct SatisfyConstraint {
    Type scheme;
};
using Constraint = std::variant<ImplConstraint, SatisfyConstraint>;
}  // namespace Yoyo
