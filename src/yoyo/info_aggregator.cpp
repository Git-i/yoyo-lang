#include "info_aggregator.h"

#include <iterator>

#include "borrow_checker.h"
#include "type_checker.h"
namespace Yoyo {
namespace Info {
RecordedTypeCheckerState RecordedTypeCheckerState::applied(
    TypeCheckerStateDiff diff) {
    RecordedTypeCheckerState new_state;
    new_state.active_constraints.reserve(active_constraints.size());
    new_state.generated_constraints.reserve(generated_constraints.size());

    auto clone_constraint =
        [](const ConstraintInformation& in) -> ConstraintInformation {
        return ConstraintInformation{
            .generated_by = in.generated_by,
            .constraint =
                std::make_unique<TypeCheckerConstraint>(*in.constraint)};
    };
    std::ranges::transform(active_constraints,
                           std::back_inserter(new_state.active_constraints),
                           clone_constraint);
    std::ranges::transform(generated_constraints,
                           std::back_inserter(new_state.generated_constraints),
                           clone_constraint);

    new_state.subsitutions = subsitutions;
    new_state.apply(std::move(diff));
    return new_state;
}

void RecordedTypeCheckerState::apply(TypeCheckerStateDiff diff) {
    switch (diff.op) {
        using enum TypeCheckerStateDiff::Operator;
        using enum TypeCheckerStateDiff::Operand;
    case Clear: {
        switch (diff.apply_to) {
        case ActiveConstraints:
            active_constraints.clear();
            break;
        case GeneratedConstraints:
            generated_constraints.clear();
            break;
        case Substitutions:
            subsitutions.clear();
            break;
        case Unifications:
            unifications.clear();
            break;
        }
        break;
    }
    case Add: {
        switch (diff.apply_to) {
        case ActiveConstraints:
            active_constraints.push_back(std::move(std::get<1>(diff.arg)));
            break;
        case GeneratedConstraints:
            generated_constraints.push_back(std::move(std::get<1>(diff.arg)));
            break;
        case Substitutions: {
            debugbreak();
            break;
        }
        case Unifications:
            unifications.push_back(std::get<3>(diff.arg));
            break;
        }
        break;
    }
    case Replace: {
        switch (diff.apply_to) {
        case ActiveConstraints:
            [[fallthrough]];
        case GeneratedConstraints:
            [[fallthrough]];
        case Unifications:
            debugbreak();
            break;
        case Substitutions: {
            auto& arg = std::get<2>(diff.arg);
            subsitutions[arg.type_var] = arg;
            break;
        }
        }
        break;
    }
    case Remove: {
        if (diff.apply_to == ActiveConstraints) {
            auto arg = std::get<0>(diff.arg);
            active_constraints.erase(active_constraints.begin() + arg);
        } else
            debugbreak();

        break;
    }
    case Flush: {
        std::ranges::move(std::move(generated_constraints),
                          std::back_inserter(active_constraints));
        generated_constraints.clear();
        break;
    }
    }
}
}  // namespace Info
}  // namespace Yoyo
