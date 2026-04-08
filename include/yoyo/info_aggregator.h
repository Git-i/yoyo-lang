#pragma once
#include "ast_node.h"
#include "type.h"
#include <cstdint>
#include <format>
#include <memory>
#include <unordered_map>
#include <variant>
#include <vector>
namespace Yoyo {
struct TypeCheckerConstraint;
namespace Info{

struct ConstraintInformation {
    std::variant<ASTNode*, uintptr_t> generated_by;
    std::unique_ptr<TypeCheckerConstraint> constraint;
};
struct SubstitutionInformation {
    std::string type_var;
    std::vector<Type> result_type;
    uint64_t generated_by;
};
struct UnificationInformation {
    std::string parent_var;
    std::string other_var;
    uint64_t generated_by;
};
struct TypeCheckerStateDiff {
    enum Operator {
        Clear,
        Add,
        Replace,
        Remove
    };
    enum Operand {
        ActiveConstraints,
        GeneratedConstraints,
        Substitutions,
        Unifications
    };
    Operator op;
    Operand apply_to;
    // this specifies what to apply the Operations
    // size_t is used to specify what constraint to target
    // std::string  specifies what substitution to target
    std::variant<size_t, ConstraintInformation, SubstitutionInformation, UnificationInformation> arg;
    std::string to_string() {
        auto operator_string = [](Operator op) {
            switch (op) {
            case Clear: return "Clear";
            case Add: return "Add";
            case Replace: return "Replace";
            case Remove: return "Remove";
            }
        };
        auto operand_string = [](Operand op) {
            switch (op) {
            case ActiveConstraints: return "Active Constraints";
            case GeneratedConstraints: return "Generated Constraints";
            case Substitutions: return "Substitutions";
            case Unifications: return "Unifications";
            }
        };
        return std::format("Apply {} to {}", operator_string(op), operand_string(apply_to));
    }
};
struct RecordedTypeCheckerState {
    std::vector<ConstraintInformation> active_constraints;
    std::vector<ConstraintInformation> generated_constraints;
    std::unordered_map<std::string, SubstitutionInformation> subsitutions;
    // TODO: store unifications here
    void apply(const TypeCheckerStateDiff& diff);
    std::string to_string() {
        std::string final_string;
        final_string += "[[Active]]\n";
        final_string += "[[Generated]]\n";
        final_string += "[[Substitutions]]";
        return final_string;
    }
};
struct FunctionInformation {
    RecordedTypeCheckerState initial_state;
    std::vector<TypeCheckerStateDiff> steps;
    // this holds a list of where the diffs for each iteration start and stop
    std::vector<size_t> iterations;
    std::string to_string() {
        std::string final_string = "Initial state:\n";
        final_string += initial_state.to_string() + "\n";

        final_string += "Operations:\n";
        for (auto& diff : steps) {
            final_string += diff.to_string() + "\n\n";
        }
        if(!steps.empty()) {
            final_string.pop_back();
            final_string.pop_back();
        }
        return final_string;
    }
};
// This struct is responsible for collecting detailed about the type checker and
// borrow checker phases of compilation, so it can be supplied to other programs for inspection
struct InformationAggregator {
    // map from function full name to detailed info
    std::unordered_map<std::string, FunctionInformation> function_info;
};
}
}
