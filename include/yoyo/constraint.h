#pragma once
#include "type.h"
#include <variant>
namespace Yoyo {
    struct ImplConstraint {
        Type other;
    };
    struct SatisfyConstraint {
        Type scheme;
    };
    using Constraint = std::variant<ImplConstraint, SatisfyConstraint>;
}
