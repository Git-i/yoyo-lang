#include "borrow_checker.h"

#include <algorithm>
#include <cstddef>
#include <deque>
#include <iostream>
#include <iterator>
#include <memory>
#include <optional>
#include <ranges>
#include <string>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#include "class_entry.h"
#include "error.h"
#include "expression.h"
#include "ir_gen.h"
#include "overload_details.h"
#include "token.h"
namespace Yoyo {
bool has_type_variable(const Yoyo::Type& tp);
}
#define RE_REPR(x)                                                        \
    x->evaluated_type = stt->best_repr(x->evaluated_type);                \
    if (has_type_variable(x->evaluated_type)) {                           \
        irgen->error(                                                     \
            Yoyo::Error(x, "Could not resolve type of this expression")); \
    }
namespace Yoyo {
namespace BorrowChecker {
//======================================================
InstructionVariant CondBrInstruction::to_variant() { return this; }
InstructionVariant BrInstruction::to_variant() { return this; }
InstructionVariant RetInstruction::to_variant() { return this; }
InstructionVariant PhiInstruction::to_variant() { return this; }
InstructionVariant AssignInstruction::to_variant() { return this; }
InstructionVariant CallFunctionInstruction::to_variant() { return this; }
InstructionVariant RelocateValueInstruction::to_variant() { return this; }
InstructionVariant NewArrayInstruction::to_variant() { return this; }
InstructionVariant NewPrimitiveInstruction::to_variant() { return this; }
InstructionVariant BorrowValueInstruction::to_variant() { return this; }
InstructionVariant DomainSubsetConstraint::to_variant() { return this; }
InstructionVariant DomainDependenceEdgeConstraint::to_variant() { return this; }
InstructionVariant DomainExtensionConstraint::to_variant() { return this; }
InstructionVariant DomainPhiInstruction::to_variant() { return this; }
InstructionVariant DerefOperation::to_variant() { return this; }
InstructionVariant DerefLoadOperation::to_variant() { return this; }
InstructionVariant MayStoreOperation::to_variant() { return this; }
InstructionVariant MayLoadOperation::to_variant() { return this; }
InstructionVariant NewAggregateInstruction::to_variant() { return this; }
InstructionVariant DropInstruction::to_variant() { return this; }
//=======================================================
void BorrowCheckerEmitter::operator()(EnumDeclaration*) {}
void BorrowCheckerEmitter::operator()(UsingStatement*) {}
void BorrowCheckerEmitter::operator()(ModuleImport*) {}
void BorrowCheckerEmitter::operator()(FunctionDeclaration*) {}
void BorrowCheckerEmitter::operator()(ClassDeclaration*) {}
void BorrowCheckerEmitter::operator()(OperatorOverload*) {}
void BorrowCheckerEmitter::operator()(GenericFunctionDeclaration*) {}
void BorrowCheckerEmitter::operator()(AliasDeclaration*) {}
void BorrowCheckerEmitter::operator()(GenericAliasDeclaration*) {}
void BorrowCheckerEmitter::operator()(GenericClassDeclaration*) {}
void BorrowCheckerEmitter::operator()(InterfaceDeclaration*) {}
void BorrowCheckerEmitter::operator()(BreakStatement*) {}
void BorrowCheckerEmitter::operator()(ContinueStatement*) {}
void BorrowCheckerEmitter::operator()(ConstantDeclaration*) {}
void BorrowCheckerEmitter::operator()(CImportDeclaration*) {}
void BorrowCheckerEmitter::operator()(UnionDeclaration*) {}
void BorrowCheckerEmitter::operator()(MacroDeclaration*) {}
void BorrowCheckerEmitter::operator()(WhileStatement* stat) {
    auto while_cond = function->new_block("while_cond");
    auto while_body = function->new_block("while_body");
    auto while_exit = function->new_block("while_exit");

    current_block->add_instruction(new BrInstruction(while_cond));
    current_block = while_cond;

    current_block->add_instruction(
        new CondBrInstruction({while_body, while_exit},
                              std::visit(*this, stat->condition->toVariant())));

    current_block = while_body;
    std::visit(*this, stat->body->toVariant());
    if (!current_block->is_terminated()) {
        current_block->add_instruction(new BrInstruction(while_cond));
    }

    current_block = while_exit;
}
void BorrowCheckerEmitter::operator()(VariableDeclaration* decl) {
    decl->type = stt->best_repr(*decl->type);
    auto variable_name = name_based_on(decl->identifier.text);
    auto value = std::visit(*this, decl->initializer->toVariant());
    if (decl->initializer)
        current_block->add_instruction(new RelocateValueInstruction(
            std::move(value), std::string(variable_name)));
    // maybe we need an unitiailized value instruction??
    variables.back().emplace_back(std::string(decl->identifier.text),
                                  std::move(variable_name));
}
void BorrowCheckerEmitter::operator()(ForStatement*) {
    // TODO
}
void BorrowCheckerEmitter::operator()(ReturnStatement* stat) {
    if (stat->expression) {
        auto val = std::visit(*this, stat->expression->toVariant());
        current_block->add_instruction(new RetInstruction(std::move(val)));
    } else {
        current_block->add_instruction(new RetInstruction);
    }
    // Drop all the valid variables (TODO)
}
void BorrowCheckerEmitter::operator()(ExpressionStatement* stat) {
    auto val = std::visit(*this, stat->expression->toVariant());
    if (!stat->expression->evaluated_type.is_void())
        drop_object(std::move(val));
}
Value BorrowCheckerEmitter::operator()(ConditionalExtraction* stat) {
    RE_REPR(stat);
    // to get here we're guaranteed to be valid from the type checker
    // the condition can either be an optional, result (not implemented)
    // or a __conv_result for the last case the `Value` produced is
    // already correct, we just need to borrow (or move) it. and for the
    // first 2 cases we need a .value field
    auto cond = std::visit(*this, stat->condition->toVariant());
    auto then_block = function->new_block("cond_extract_then");
    auto else_block =
        stat->else_body ? function->new_block("cond_extract_else") : nullptr;
    auto cont_block = function->new_block("cond_extract_cont");
    current_block->add_instruction(new CondBrInstruction(
        {then_block, else_block ? else_block : cont_block}, Value(cond)));

    current_block = then_block;
    auto then_capture_name = name_based_on(stat->captured_name);
    variables.emplace_back();
    variables.back().emplace_back(stat->captured_name, then_capture_name);
    if (stat->then_capture_tp == ConditionalExtraction::Own) {
        // TODO: figure out hove move works in this IR
        current_block->add_instruction(new RelocateValueInstruction(
            std::move(cond), std::move(then_capture_name)));
    } else
        current_block->add_instruction(new BorrowValueInstruction(
            std::move(cond), std::move(then_capture_name)));

    auto then_value = std::visit(*this, stat->body->toVariant());
    if (!current_block->is_terminated()) {
        destroy_and_remove_block();
        current_block->add_instruction(new BrInstruction(cont_block));
    } else {
        auto last = std::move(current_block->instructions.back());
        current_block->instructions.pop_back();
        destroy_and_remove_block();
        current_block->instructions.push_back(std::move(last));
    }
    auto else_value = Value::empty();
    if (else_block) {
        current_block = else_block;
        else_value = std::visit(*this, stat->else_body->toVariant());
        if (!else_block->is_terminated()) {
            else_block->add_instruction(new BrInstruction(cont_block));
        }
    }

    current_block = cont_block;
    if (stat->evaluated_type.is_void())
        return Value::empty();
    else {
        if (then_value.is_empty()) return else_value;
        if (else_value.is_empty()) return then_value;

        auto name = temporary_name();
        current_block->add_instruction(
            new PhiInstruction({then_value, else_value}, std::string(name)));
        return Value::from(std::move(name));
    }
}
void BorrowCheckerEmitter::operator()(WithStatement* stat) {
    // This statement might just be dying
}
Value BorrowCheckerEmitter::operator()(IfExpression* expr) {
    RE_REPR(expr);
    auto cond = std::visit(*this, expr->condition->toVariant());

    auto then_block = function->new_block("if_then");

    auto else_block =
        expr->else_expr ? function->new_block("if_else") : nullptr;
    auto cont_block = function->new_block("if_cont");

    current_block->add_instruction(new CondBrInstruction(
        {then_block, else_block ? else_block : cont_block}, std::move(cond)));

    current_block = then_block;
    auto then_value = std::visit(*this, expr->then_expr->toVariant());
    if (!current_block->is_terminated()) {
        current_block->add_instruction(new BrInstruction(cont_block));
    }

    auto else_value = Value::empty();
    if (else_block) {
        current_block = else_block;
        else_value = std::visit(*this, expr->else_expr->toVariant());
        if (!else_block->is_terminated()) {
            else_block->add_instruction(new BrInstruction(cont_block));
        }
    }

    current_block = cont_block;
    if (expr->evaluated_type.is_void())
        return Value::empty();
    else {
        if (then_value.is_empty()) return else_value;
        if (else_value.is_empty()) return then_value;

        auto name = temporary_name();
        current_block->add_instruction(
            new PhiInstruction({then_value, else_value}, std::string(name)));
        return Value::from(std::move(name));
    }
}
Value BorrowCheckerEmitter::operator()(BlockExpression* expr) {
    RE_REPR(expr);
    variables.emplace_back();
    for (auto& stt : expr->statements) {
        std::visit(*this, stt->toVariant());
        if (current_block->is_terminated()) {
            auto store = std::move(current_block->instructions.back());
            current_block->instructions.pop_back();
            destroy_and_remove_block();
            current_block->instructions.push_back(std::move(store));
            return Value::empty();
        }
    }
    Value ret = Value::empty();
    if (expr->expr) ret = std::visit(*this, expr->expr->toVariant());
    destroy_and_remove_block();
    return ret;
}
Value BorrowCheckerEmitter::operator()(IntegerLiteral* lit) {
    RE_REPR(lit);
    auto name = temporary_name();
    current_block->add_instruction(
        new NewPrimitiveInstruction(std::string(name)));
    return Value::from(std::move(name));
}
Value BorrowCheckerEmitter::operator()(BooleanLiteral* lit) {
    RE_REPR(lit);
    auto name = temporary_name();
    current_block->add_instruction(
        new NewPrimitiveInstruction(std::string(name)));
    return Value::from(std::move(name));
}
Value BorrowCheckerEmitter::operator()(TupleLiteral* exp) {
    RE_REPR(exp);
    std::vector<Value> args;
    args.reserve(exp->elements.size());
    std::ranges::transform(
        exp->elements, std::back_inserter(args),
        [this](auto& expr) { return std::visit(*this, expr->toVariant()); });

    auto name = temporary_name();
    current_block->add_instruction(
        new CallFunctionInstruction("__builtin_make_tuple", std::string(name),
                                    std::move(args), exp->evaluated_type),
        exp);
    return Value::from(std::move(name));
}
Value BorrowCheckerEmitter::operator()(ArrayLiteral* lit) {
    RE_REPR(lit);
    auto array_obj = temporary_name();
    // [<expr>, <expr>, ... ]
    if (auto elems = std::get_if<std::vector<std::unique_ptr<Expression>>>(
            &lit->elements);
        elems) {
        std::vector<Value> values;
        values.reserve(elems->size());
        std::ranges::transform(*elems, std::back_inserter(values),
                               [this](auto& expr) {
                                   return std::visit(*this, expr->toVariant());
                               });
        current_block->add_instruction(
            new NewArrayInstruction(std::move(values), std::string(array_obj)));
    }
    // [<expr>; <expr>] repeat first <expr>, sencond <expr> times
    // (second expr is a constant)
    else {
        auto& elem_size =
            std::get<std::pair<std::unique_ptr<Expression>,
                               std::unique_ptr<Expression>>>(lit->elements);
        std::ignore = elem_size;
        // TODO
    }
    return Value::from(std::move(array_obj));
}
Value BorrowCheckerEmitter::operator()(RealLiteral* lit) {
    RE_REPR(lit);
    auto name = temporary_name();
    current_block->add_instruction(
        new NewPrimitiveInstruction(std::string(name)));
    return Value::from(std::move(name));
}
Value BorrowCheckerEmitter::operator()(StringLiteral* lit) {
    RE_REPR(lit);
    for (auto& entry : lit->literal) {
        if (std::holds_alternative<std::unique_ptr<Expression>>(entry)) {
            auto& capture = std::get<std::unique_ptr<Expression>>(entry);
            RE_REPR(capture.get());
            Value to_string_arg;
            if (!capture->evaluated_type.should_sret()) {
                to_string_arg = std::visit(*this, capture->toVariant());
            } else {
                auto arg = temporary_name();
                current_block->add_instruction(new BorrowValueInstruction(
                    std::visit(*this, capture->toVariant()), std::string(arg)));
                to_string_arg = Value::from(std::move(arg));
            }

            current_block->add_instruction(
                new CallFunctionInstruction(
                    "__builtin_to_string_for_" +
                        capture->evaluated_type.full_name(),
                    // drop the string or not, it doesn't really matter
                    // string is fully owning
                    temporary_name(), {std::move(to_string_arg)},
                    lit->evaluated_type),
                capture.get());
        }
    }
    /*
     * HACK:
     * since strings are not supported we just call "new primitive" to
     * emulate building a string from parts this should still be
     * correct, becuase string assembly does not involve references
     * (those are handled by the function calls above)
     */
    auto val = temporary_name();
    current_block->add_instruction(
        new NewPrimitiveInstruction(std::string(val)), lit);
    return Value::from(std::move(val));
}
Value BorrowCheckerEmitter::operator()(NameExpression* name) {
    RE_REPR(name);
    if (name->evaluated_type.name == "__fn")
        return Value::function(std::string(name->evaluated_type.block_hash));
    for (auto& block : variables | std::views::reverse) {
        for (auto& [var_name, id] : block) {
            if (var_name == name->text) return Value::from(std::string(id));
        }
    }
    return Value::constant();
}
Value BorrowCheckerEmitter::operator()(GenericNameExpression* name) {
    RE_REPR(name);
    return Value::constant();
}
Value BorrowCheckerEmitter::operator()(PrefixOperation* pfx) {
    RE_REPR(pfx);
    switch (pfx->op.type) {
        // BIG TODO regarding moving from behind references and all that
    case TokenType::Star: {
        auto this_eval = std::visit(*this, pfx->operand->toVariant());
        auto result = temporary_name();
        current_block->add_instruction(
            new DerefLoadOperation(std::move(this_eval), std::string(result)),
            pfx);
        return Value::from(std::move(result));
    }
    case TokenType::Ampersand:
        [[fallthrough]];
    case TokenType::RefMut: {
        RE_REPR(pfx->operand.get());
        auto this_eval = LValueEmitter{*this}.do_expr(pfx->operand.get());
        auto result = temporary_name();
        current_block->add_instruction(new BorrowValueInstruction(
            std::move(this_eval), std::string(result)));
        return Value::from(std::move(result));
    }
    default:
        debugbreak();
    }
    return Value::empty();
}
Value BorrowCheckerEmitter::operator()(BinaryOperation* op) {
    RE_REPR(op);
    for (auto& sub : op->subtypes) {
        sub = stt->best_repr(sub);
        if (has_type_variable(sub)) {
            irgen->error(
                Error(op, "Could not resolve all generics for this operation"));
        }
    }
    auto token_tp = op->op.type;
    switch (token_tp) {
        using enum TokenType;
    case GreaterEqual:
        [[fallthrough]];
    case LessEqual:
        [[fallthrough]];
    case BangEqual:
        [[fallthrough]];
    case DoubleEqual:
        [[fallthrough]];
    case Spaceship:
        [[fallthrough]];
    case Greater:
        token_tp = TokenType::Spaceship;
        break;
    default:
        break;
    }
    // extend to all assignments
    if (token_tp == TokenType::Equal) {
        // we need to evaluate the rhs first
        auto rhs = std::visit(*this, op->rhs->toVariant());
        RE_REPR(op->lhs.get());
        auto lhs = LValueEmitter{*this}.do_expr(op->lhs.get());
        current_block->add_instruction(
            new AssignInstruction(std::move(lhs), std::move(rhs)), op);
        return Value::empty();
    } else if (token_tp != TokenType::Dot && token_tp != TokenType::Equal) {
        auto result = temporary_name();
        auto lhs = std::visit(*this, op->lhs->toVariant());
        auto rhs = std::visit(*this, op->rhs->toVariant());
        current_block->add_instruction(
            new CallFunctionInstruction(
                op->selected->mangled_name(token_tp), std::string(result),
                {std::move(lhs), std::move(rhs)}, op->evaluated_type),
            op);
        return Value::from(std::move(result));
    } else if (token_tp == TokenType::Dot &&
               op->evaluated_type.name != "__bound_fn") {
        // memeber access
        auto as_name = dynamic_cast<NameExpression*>(op->rhs.get());
        if (!as_name) debugbreak();
        auto left_value = std::visit(*this, op->lhs->toVariant());
        // If access is behind a reference, we need to desugar into
        // dereference + access
        if (op->lhs->evaluated_type.is_reference()) {
            std::string new_value = temporary_name();
            current_block->add_instruction(
                new DerefLoadOperation(std::move(left_value), new_value), op);
            left_value = Value::from(std::move(new_value));
        }
        return left_value.member(std::string(as_name->text));
    } else {
        // TODO
        // could be function binding or assignment
        return Value::empty();
    }
}
Value BorrowCheckerEmitter::operator()(GroupingExpression* grp) {
    RE_REPR(grp);
    return std::visit(*this, grp->expr->toVariant());
}
Value BorrowCheckerEmitter::operator()(LogicalOperation* lg) {
    RE_REPR(lg);
    // We could do a branch here to represent short circuiting
    // but I don't feel its necessary

    // these two must return bools so there's no need to drop them
    std::visit(*this, lg->lhs->toVariant());
    std::visit(*this, lg->rhs->toVariant());

    std::string result = temporary_name();
    current_block->add_instruction(
        new NewPrimitiveInstruction(std::string(result)));
    return Value::from(std::move(result));
}
Value BorrowCheckerEmitter::operator()(PostfixOperation*) {
    return Value::empty();
}
Value BorrowCheckerEmitter::operator()(CallOperation* op) {
    RE_REPR(op);
    auto callee_val = std::visit(*this, op->callee->toVariant());
    auto& callee_type = op->callee->evaluated_type;
    if (callee_type.name == "__bound_fn") {
        // TODO
        debugbreak();
        return Value::empty();
    }
    if (callee_type.name.starts_with("__union_var")) {
        auto result = temporary_name();
        auto inner_val = std::visit(*this, op->arguments[0]->toVariant());
        auto field_name = std::string(
            callee_type.name.begin() + 1 + callee_type.name.find_first_of('$'),
            callee_type.name.end());
        current_block->add_instruction(new NewAggregateInstruction{
            {{std::move(field_name), std::move(inner_val)}},
            Type(op->evaluated_type),
            std::string(result)});
        return Value::from(std::move(result));
    }
    auto result = temporary_name();
    std::vector<Value> args;
    args.reserve(op->arguments.size());
    std::ranges::transform(
        op->arguments, std::back_inserter(args),
        [this](auto& arg) { return std::visit(*this, arg->toVariant()); });
    current_block->add_instruction(
        new CallFunctionInstruction(callee_val.function_name().value(),
                                    std::string(result), std::move(args),
                                    op->evaluated_type),
        op);
    return Value::from(std::move(result));
}
Value BorrowCheckerEmitter::operator()(SubscriptOperation* op) {
    RE_REPR(op);
    auto object = std::visit(*this, op->object->toVariant());
    auto index = std::visit(*this, op->index->toVariant());
    auto& idx_type = op->index->evaluated_type;
    if (op->object->evaluated_type.deref().is_static_array() &&
        idx_type.is_unsigned_integral() && idx_type.integer_width() == 64) {
        // array element access (member access is not fully resolved wrt
        // to refernces)
        return std::move(object).member("[*]");
    }
    // TODO: handle this operator for evey other type
    debugbreak();
    return Value::empty();
}
Value BorrowCheckerEmitter::operator()(LambdaExpression*) {
    return Value::empty();
}
Value BorrowCheckerEmitter::operator()(TryExpression*) {
    return Value::empty();
}
Value BorrowCheckerEmitter::operator()(ScopeOperation* scp) {
    RE_REPR(scp);
    if (scp->evaluated_type.name == "__fn") {
        return Value::function(std::string(scp->evaluated_type.block_hash));
    }
    // either function, enum or constant (TODO)
    return Value::constant();
}
Value BorrowCheckerEmitter::operator()(ObjectLiteral* lit) {
    RE_REPR(lit);
    auto name = temporary_name();
    std::unordered_map<std::string, Value> value_map;
    for (auto& [name, expr] : lit->values) {
        value_map[name] = std::visit(*this, expr->toVariant());
    }
    current_block->add_instruction(new NewAggregateInstruction(
        std::move(value_map), Type(lit->evaluated_type), std::string(name)));
    return Value::from(std::move(name));
}
Value BorrowCheckerEmitter::operator()(NullLiteral* lit) {
    RE_REPR(lit);
    return Value::empty();
}
Value BorrowCheckerEmitter::operator()(AsExpression* ss) {
    RE_REPR(ss);
    // if we return __conv_result we can just propagate the value and
    // allow the conditional extraction to take care of it-
    if (ss->evaluated_type.name.starts_with("__conv_result")) {
        return std::visit(*this, ss->expr->toVariant())
            .member(std::string(ss->dest.name));
    }
    return Value::empty();
}
Value BorrowCheckerEmitter::operator()(CharLiteral* lit) {
    RE_REPR(lit);
    auto name = temporary_name();
    current_block->add_instruction(
        new NewPrimitiveInstruction(std::string(name)));
    return Value::from(std::move(name));
}
Value BorrowCheckerEmitter::operator()(GCNewExpression* gcn) {
    RE_REPR(gcn);
    // TODO
    return Value::empty();
}
Value BorrowCheckerEmitter::operator()(MacroInvocation* ivc) {
    RE_REPR(ivc);
    return std::visit(*this, ivc->result->toVariant());
}
Value BorrowCheckerEmitter::operator()(SpawnExpression* exr) {
    RE_REPR(exr);
    // The fiber situation is crazyy
    // fibers can't borrow from other fibers
    return Value::from("__literal");
}

template <typename T>
static std::string instruction_to_string(T* inst) {
    if constexpr (std::is_same_v<T, CondBrInstruction>) {
        auto first_name = inst->options[0]->debug_name;
        for (auto block : std::ranges::subrange(
                 std::next(inst->options.begin()), inst->options.end()))
            first_name += ", " + block->debug_name;
        return std::format("cond br {} [{}]", inst->br_on.to_string(),
                           first_name);
    }
    if constexpr (std::is_same_v<T, BrInstruction>) {
        return std::format("br {}", inst->next->debug_name);
    }
    if constexpr (std::is_same_v<T, RetInstruction>) {
        return std::format("ret {}",
                           inst->ret_val ? inst->ret_val->to_string() : "void");
    }
    if constexpr (std::is_same_v<T, PhiInstruction>) {
        std::string input = inst->args[0].to_string();
        for (auto& arg : std::ranges::subrange(std::next(inst->args.begin()),
                                               inst->args.end()))
            input += ", " + arg.to_string();
        return std::format("%{} = phi({})", inst->into, input);
    }
    if constexpr (std::is_same_v<T, AssignInstruction>) {
        return std::format("{} = {}", inst->lhs.to_string(),
                           inst->rhs.to_string());
    }
    if constexpr (std::is_same_v<T, CallFunctionInstruction>) {
        std::string input;
        if (!inst->val.empty()) {
            input = inst->val[0].to_string();
            for (auto& arg : std::ranges::subrange(std::next(inst->val.begin()),
                                                   inst->val.end()))
                input += ", " + arg.to_string();
        }
        return std::format("%{} = call {}({})", inst->into, inst->function_name,
                           input);
    }
    if constexpr (std::is_same_v<T, RelocateValueInstruction>) {
        return std::format("%{} = {}", inst->into, inst->val.to_string());
    }
    if constexpr (std::is_same_v<T, NewArrayInstruction>) {
        std::string input = inst->values[0].to_string();
        for (auto& arg : std::ranges::subrange(std::next(inst->values.begin()),
                                               inst->values.end()))
            input += ", " + arg.to_string();
        return std::format("%{} = [{}]", inst->into, input);
    }
    if constexpr (std::is_same_v<T, NewPrimitiveInstruction>) {
        return std::format("%{} = primitive", inst->into);
    }
    if constexpr (std::is_same_v<T, BorrowValueInstruction>) {
        return std::format("%{} = borrow {}", inst->into,
                           inst->val.to_string());
    }
    if constexpr (std::is_same_v<T, DomainSubsetConstraint>) {
        return std::format("{} ⊇ {}", inst->super.to_string(),
                           inst->sub.to_string());
    }
    if constexpr (std::is_same_v<T, DomainExtensionConstraint>) {
        return std::format("{} ⊇ {} ∪ {}", inst->super.to_string(),
                           inst->old_super.to_string().empty()
                               ? inst->super.to_string()
                               : inst->old_super.to_string(),
                           inst->sub.to_string());
    }
    if constexpr (std::is_same_v<T, DomainDependenceEdgeConstraint>) {
        return std::format("{} => {}", inst->d1.to_string(),
                           inst->d2.to_string());
    }
    if constexpr (std::is_same_v<T, DomainPhiInstruction>) {
        std::string input;
        if (!inst->args.empty()) {
            input = inst->args[0].to_string();
            for (auto& arg : std::ranges::subrange(
                     std::next(inst->args.begin()), inst->args.end()))
                input += ", " + arg.to_string();
        }
        return std::format("{} = domain φ({})", inst->into, input);
    }
    if constexpr (std::is_same_v<T, DerefOperation>) {
        return std::format("%{} = deref {} from domain {}", inst->into,
                           inst->reference.to_string(),
                           inst->ref_domain.to_string());
    }
    if constexpr (std::is_same_v<T, MayStoreOperation>) {
        return std::format("{} = χ({}) [may store]",
                           inst->new_domain.to_string(),
                           inst->old_domain.to_string());
    }
    if constexpr (std::is_same_v<T, MayLoadOperation>) {
        return std::format("µ({}) [may load]", inst->domain.to_string());
    }
    if constexpr (std::is_same_v<T, DerefLoadOperation>) {
        return std::format("%{} = deref load {} from domain {}", inst->into,
                           inst->reference.to_string(),
                           inst->ref_domain.to_string());
    }
    if constexpr (std::is_same_v<T, NewAggregateInstruction>) {
        std::string body = "{";
        NewAggregateInstruction* inst_cast = inst;
        if (!inst_cast->values.empty()) {
            body += inst_cast->values.begin()->first + ": " +
                    inst_cast->values.begin()->second.to_string();
        }
        if (inst_cast->values.size() > 1) {
            for (auto& [name, val] :
                 std::ranges::subrange(std::next(inst_cast->values.begin()),
                                       inst_cast->values.end())) {
                body += ", " + name + ": " + val.to_string();
            }
        }
        body += "}";
        return std::format("%{} = aggrg ({}) {}", inst->into,
                           inst->type_name.full_name(), body);
    }
    if constexpr (std::is_same_v<T, DropInstruction>) {
        return "drop " + inst->val.to_string();
    }
    return "not implemented";
};
std::string BasicBlock::to_string(bool include_dfa, DomainCheckerState* state) {
    std::string instructions_string;
    auto append_set = [&instructions_string](std::set<std::string>& in) {
        if (!in.empty()) {
            instructions_string += *in.begin();
            for (auto& elem : std::ranges::subrange{
                     std::ranges::next(in.begin(), 1), in.end()})
                instructions_string += ", " + elem;
        }
        instructions_string += "}\n";
    };
    for (auto& inst : instructions) {
        if (include_dfa) {
            instructions_string += "in: {";
            append_set(state->dfa_in[inst.get()]);
        }
        instructions_string +=
            std::visit([](auto* inst) { return instruction_to_string(inst); },
                       inst->to_variant()) +
            '\n';
        if (include_dfa) {
            instructions_string += "out: {";
            append_set(state->dfa_out[inst.get()]);
        }
    }
    instructions_string.pop_back();  // remove the trailing \n

    std::string preds_string;
    if (!preds.empty()) {
        preds_string = "preds(" + preds[0]->debug_name;
        for (auto pred :
             std::ranges::subrange(std::next(preds.begin()), preds.end()))
            preds_string += ", " + pred->debug_name;
        preds_string += ")";
    }
    return std::format("{} {}:\n{}", debug_name, preds_string,
                       instructions_string);
}
Domain DomainCheckerState::new_domain_var() {
    return Domain{.name = "'?" + std::to_string(last_id++)};
}
void DomainCheckerState::register_value_base_type(const std::string& value,
                                                  BorrowCheckerType&& type) {
    type_mapping.emplace(value, std::move(type));
}
BorrowCheckerType DomainCheckerState::type_to_borrow_checker_type(
    const Type& type) {
    if (type.is_integral() || type.is_floating_point() ||
        type.get_decl_if_enum()) {
        return BorrowCheckerType::new_primitive();
    }
    if (type.get_decl_if_class(irgen) || type.get_decl_if_union()) {
        return BorrowCheckerType::new_aggregate_from(Type(type));
    }
    // HACK: strings are not properly supported (so nothing fancy can
    // happen) and we just treat them like primitives
    if (type.is_str()) return BorrowCheckerType::new_primitive();
    debugbreak();
    return BorrowCheckerType{};
}
const BorrowCheckerType& DomainCheckerState::field_lookup(
    const BorrowCheckerType& type, const std::string& base_name,
    std::span<const std::string> fields) {
    if (fields.empty()) {
        return type;
    }
    // lvalue field lookup just project the paths unto the lvalue (eg
    // lvaue<('0), {x: i32, y: i32}>.x => lvalue<('0, .x), /* I think
    // the type here remains*/>)
    if (auto lvalue = std::get_if<BorrowCheckerType::LValue>(&type.details);
        lvalue) {
        auto& final_type =
            field_lookup(*lvalue->subtype.get(), base_name, fields);
        if (lvalue->subtype->details.index() == BorrowCheckerType::Named &&
            final_type.details.index() != BorrowCheckerType::LValue) {
            auto name_so_far = base_name;
            for (auto i : std::views::iota(0u, fields.size())) {
                name_so_far += "." + fields[i];
                auto& type_entry = named_value_type_cache.at(name_so_far);
                auto actual_type = BorrowCheckerType{};
                actual_type.domains = type.domains;
                actual_type.details.emplace<BorrowCheckerType::LValue>(
                    std::make_unique<BorrowCheckerType>(
                        type_entry.cloned(this)));
                std::get<BorrowCheckerType::LValue>(actual_type.details)
                    .subpath = std::vector<std::string>{fields.begin(),
                                                        fields.begin() + i + 1};
                std::swap(type_entry, actual_type);
            }
        }
        return final_type;
    }
    if (auto named = std::get_if<BorrowCheckerType::Named>(&type.details);
        named) {
        std::string full_name = base_name;
        for (auto& field : fields) full_name += "." + field;
        if (named_value_type_cache.contains(full_name))
            return named_value_type_cache.at(full_name);
        // populate the cache with the type of each field reaching up to
        // the desired pointe e.g val.f1.f2.f3 creates entries for
        // val.f1 val.f1.f2 and val.f1.f2
        std::string name_so_far = base_name;
        Type type_so_far = named->actual_type;
        for (auto i : std::views::iota(0u, fields.size())) {
            // auto& prev_type = (i == 0) ? type :
            // named_value_type_cache.at(name_so_far); type so far is
            // guaranteed to be a struct/union/tuple/array by the type
            // system
            name_so_far += "." + fields[i];
            if (auto decl = type_so_far.get_decl_if_class(this->irgen)) {
                auto it = std::ranges::find_if(
                    decl->vars, [&fields, i](ClassVariable& var) {
                        return var.name == fields[i];
                    });
                // this probably will never happen if the type checker
                // has already run
                if (it == decl->vars.end()) debugbreak();
                type_so_far = it->type;
                named_value_type_cache.emplace(
                    name_so_far, type_to_borrow_checker_type(it->type));
                // apply proper domain substitutions here
            } else if (auto decl = type_so_far.get_decl_if_union()) {
                if (!decl->fields.contains(fields[i])) debugbreak();
                type_so_far = decl->fields.at(fields[i]);
                named_value_type_cache.emplace(
                    name_so_far, type_to_borrow_checker_type(type_so_far));
            } else
                debugbreak();
        }
        return named_value_type_cache.at(full_name);
    } else
        debugbreak();
    return type;  // this should never be reached
}
const BorrowCheckerType& DomainCheckerState::get_value_type(
    const Value& value) {
    if (!type_mapping.contains(value.base_name)) debugbreak();
    if (!value.subpaths.empty()) {
        auto& base_type = type_mapping.at(value.base_name);
        return field_lookup(base_type, value.base_name, value.subpaths);
    }
    return type_mapping.at(value.base_name);
}
std::unique_ptr<BorrowCheckerFunction> DomainCheckerState::check_function(
    FunctionDeclaration* decl, IRGenerator* irgen, const FunctionSignature& sig,
    TypeCheckerState* stt) {
    auto function = std::make_unique<BorrowCheckerFunction>();
    func = &*function;
    this->irgen = irgen;
    entry_block = function->new_block("entry");
    std::visit(BorrowCheckerEmitter{irgen, stt, &*function, entry_block},
               decl->body->toVariant());
    std::cout << "Phase 1\n" << function->to_string() << std::endl;
    for (auto& block : function->blocks) {
        for (auto it = block->instructions.begin();
             it != block->instructions.end(); ++it) {
            it = std::visit(
                DomainVariableInserter{this, block->instructions, it},
                (*it)->to_variant());
        }
    }
    std::cout << "Phase 2\n"
              << type_mapping.to_string() << "\n\n"
              << function->to_string() << std::endl;
    using namespace std::ranges;
    using namespace std::views;

    bool has_change = true;
    while (has_change) {
        has_change = false;
        for (auto& block : function->blocks) {
            for (auto& inst : block->instructions)
                has_change = std::visit(InclusionPointerAnalyser{ptgraph, this},
                                        inst->to_variant()) ||
                             has_change;
        }
    }
    std::cout << ptgraph.to_graphviz() << std::endl;

    calc_block_preds();
    transform_to_ssa();
    std::cout << "Phase 3\n"
              << type_mapping.to_string() << "\n\n"
              << function->to_string() << std::endl;
    clear_dependencies();

    // build DUG
    build_dug();
    std::cout << def_use_graph.to_graphviz() << std::endl;

    do_primary_analysis();
    std::cout << final_ptg.to_graphviz();

    do_domain_validity_analysis(
        entry_block);  // perform an analysis to know what domains are
                       // valid for every instruction
    std::cout << "Phase 7\n" << function->to_string(true, this) << std::endl;

    for (auto& block : function->blocks) {
        for (auto& inst : block->instructions)
            std::visit(BorrowCheckVisitor{this, irgen}, inst->to_variant());
    }
    return function;
}
void DomainCheckerState::do_primary_analysis() {
    std::set<Instruction*> worklist;
    // initialize worklist to contain all instructions that induce new
    // domains (deref load and subset constraint)
    for (auto& [def, uses] : def_use_graph.edges) {
        if (dynamic_cast<DomainSubsetConstraint*>(def) ||
            dynamic_cast<DerefLoadOperation*>(def))
            worklist.insert(def);
        for (auto& [use, domain] : uses) {
            if (dynamic_cast<DomainSubsetConstraint*>(use) ||
                dynamic_cast<DerefLoadOperation*>(def))
                worklist.insert(use);
        }
    }
    while (!worklist.empty()) {
        using enum TopLevelPointsToGraph::AdditionStatus;

        auto inst_to_string = [](Instruction* inst) {
            auto ret_val = std::visit(
                [](auto* inst) { return instruction_to_string(inst); },
                inst->to_variant());
            if (ret_val.starts_with("%")) ret_val.insert(ret_val.begin(), '\\');
            return ret_val;
        };

        std::cout << "\nWorklist:\n";
        for (auto inst : worklist)
            std::cout << inst_to_string(inst) << std::endl;

        auto node = *worklist.begin();
        worklist.erase(worklist.begin());

        if (auto inst = dynamic_cast<DomainSubsetConstraint*>(node)) {
            // p > q
            bool has_change = false;
            if (inst->sub.is_var()) {
                for (auto& pointee :
                     final_ptg.get_pointees_of(inst->sub.to_string())) {
                    has_change =
                        (final_ptg.add_new_relation(inst->super.to_string(),
                                                    pointee) == Changed) ||
                        has_change;
                }
            }
            // p > {q}
            else if (!inst->sub.is_null()) {
                auto as_val = Value::from(inst->sub.to_string());
                auto& val_type = get_value_type(as_val);
                // for lvalues we reborrow, so we need to look at what
                // the lvalue is from and extract the references from
                // there
                if (auto lval = std::get_if<BorrowCheckerType::LValue>(
                        &val_type.details)) {
                    bool has_change = false;
                    for (auto& pointee :
                         final_ptg.get_pointees_of(inst->lvalue_domain)) {
                        auto edge = pointee;
                        // apply the path;
                        for (auto& path : lval->subpath) edge += "." + path;
                        has_change =
                            final_ptg.add_new_relation(inst->super.to_string(),
                                                       edge) == Changed ||
                            has_change;
                    }
                } else
                    has_change = final_ptg.add_new_relation(
                                     inst->super.to_string(),
                                     inst->sub.to_string()) == Changed;
            }

            if (has_change) {
                for (auto [use, domain] : def_use_graph.edges[node])
                    worklist.insert(use);
            }
        } else if (auto inst = dynamic_cast<DomainExtensionConstraint*>(node)) {
            // p_n > p_o + q
            bool has_change = false;
            if (inst->sub.is_var()) {
                for (auto& pointee :
                     final_ptg.get_pointees_of(inst->sub.to_string())) {
                    has_change =
                        (final_ptg.add_new_relation(inst->super.to_string(),
                                                    pointee) == Changed) ||
                        has_change;
                }
                for (auto& pointee :
                     final_ptg.get_pointees_of(inst->old_super.to_string())) {
                    has_change =
                        (final_ptg.add_new_relation(inst->super.to_string(),
                                                    pointee) == Changed) ||
                        has_change;
                }
            }
            // p > {q}
            else if (!inst->sub.is_null())
                debugbreak();
            else {
                // sub is null
                debugbreak();
            }
            if (has_change) {
                for (auto [use, domain] : def_use_graph.edges[node])
                    worklist.insert(use);
            }
        } else if (auto inst = dynamic_cast<DomainPhiInstruction*>(node)) {
            bool has_change = false;

            for (auto& domain : inst->args) {
                for (auto& pointee :
                     final_ptg.get_pointees_of(domain.to_string())) {
                    has_change = (final_ptg.add_new_relation(
                                      inst->into, pointee) == Changed) ||
                                 has_change;
                }
            }

            if (has_change) {
                for (auto [use, domain] : def_use_graph.edges[node])
                    worklist.insert(use);
            }
        } else if (auto inst = dynamic_cast<AssignInstruction*>(node)) {
            // assign instructions only get added here if they have
            // associated "may store" i.e its a write into an lvalue

            // a domain points to a single concrete memory location we
            // strong update else we weak update
            auto& pts_lhs =
                final_ptg.get_pointees_of(inst->lhs_domain.to_string());
            bool is_single_update = pts_lhs.size() == 1;

            bool has_change = false;
            for (auto& elem : pts_lhs) {
                // every element of pts_lhs is updated in the may store
                // operation
                auto& type = get_value_type(Value::from(std::string(elem)));
                if (type.domains.empty())
                    break;  // update does not change any domains
                // lookup the type's old and new domains in the may
                // store operations; old_domain is useful for weak
                // update
                Domain old_domain, new_domain = type.domains[0].first;
                for (auto ms_op : inst->may_stores) {
                    if (ms_op->new_domain.name.starts_with(new_domain.name)) {
                        old_domain = ms_op->old_domain;
                        new_domain = ms_op->new_domain;
                        break;
                    }
                }

                if (is_single_update /* && domain is concrete (not shared)*/) {
                    // strong update
                    for (auto& pts_rhs : final_ptg.get_pointees_of(
                             inst->rhs_domain.to_string())) {
                        has_change = final_ptg.add_new_relation(
                                         new_domain.name, pts_rhs) == Changed ||
                                     has_change;
                    }
                } else {
                    // weak update (add new ones and preserve old ones)
                    for (auto& pts_rhs : final_ptg.get_pointees_of(
                             inst->rhs_domain.to_string())) {
                        has_change = final_ptg.add_new_relation(
                                         new_domain.name, pts_rhs) == Changed ||
                                     has_change;
                    }
                    for (auto& pts_rhs :
                         final_ptg.get_pointees_of(old_domain.to_string())) {
                        has_change = final_ptg.add_new_relation(
                                         new_domain.name, pts_rhs) == Changed ||
                                     has_change;
                    }
                }
            }

            if (has_change) {
                for (auto [use, domain] : def_use_graph.edges[node])
                    worklist.insert(use);
            }
        } else if (auto inst = dynamic_cast<DerefLoadOperation*>(node)) {
            // val = *pointer, pts(v) = pts(pts(pointer))
            auto& target_t =
                get_value_type(Value::from(std::string(inst->into)));
            if (target_t.domains.empty()) continue;

            // we can safely assume this is version 1 of the domain,
            // because this instruction creates a new domain
            auto target = target_t.domains[0].first;
            target.name += "__1";

            bool has_change = false;
            for (auto& pts_reference :
                 final_ptg.get_pointees_of(inst->ref_domain.to_string())) {
                // every reference here has an associated may load
                // operation, we use that to get the versioned name
                auto domain =
                    get_value_type(Value::from(std::string(pts_reference)))
                        .domains[0]
                        .first;
                for (auto ml_op : inst->may_loads) {
                    if (ml_op->domain.name.starts_with(domain.name)) {
                        domain = ml_op->domain;
                        break;
                    }
                }
                for (auto& pts_pts :
                     final_ptg.get_pointees_of(domain.to_string())) {
                    has_change = final_ptg.add_new_relation(
                                     target.name, pts_pts) == Changed ||
                                 has_change;
                }
            }

            // deref load operation can't define anything so they use a
            // definer ( the subset null constraint inserted after them)
            if (has_change) {
                for (auto [use, domain] : def_use_graph.edges[inst])
                    worklist.insert(use);
            }
        } else if (auto inst = dynamic_cast<DerefOperation*>(node)) {
            // I don't think there's any special behaviour required here
            std::ignore = inst;
        } else
            debugbreak();
    }
}
struct TransferFunctionVisitor {
    DomainCheckerState* state;
    BasicBlock* block;
    size_t instruction_idx;
    // Generate KILL sets (all the domains invalidated when the provided
    // values are modified)
    std::vector<std::pair<std::string, KillReason>> kill_for_modifying_values(
        std::span<const Value> values) {
        std::vector<std::pair<std::string, KillReason>> result;
        auto unstable_suffix =
            // returns true is the input is a path that begins with the
            // input path and contains an unstable link if the first
            // part of the pair is true, the second part is the value
            // that made it true
            [this, values](std::string value) -> std::pair<bool, std::string> {
            auto input = Value::from(std::move(value));
            bool is_unstable = false;
            std::string affected_value;
            for (auto& elem : values) {
                // elem must be a prefix of input
                if (!input.to_string().starts_with(elem.to_string())) continue;
                // look past the prefix and see if any unstable links
                // exist
                auto extra_fields = std::span<std::string>{
                    input.subpaths.begin() + elem.subpaths.size(),
                    input.subpaths.end()};
                auto& elem_ty = state->get_value_type(elem);
                auto* current_type = &elem_ty;
                // access all extra_fields from elem_ty and see if
                // there's and unstable link
                auto current_value = elem;
                for (auto& field : extra_fields) {
                    // if fields remain, it must be an Aggregate type:
                    // array, box (not implemented yet), or named type.
                    if (auto as_arr = std::get_if<BorrowCheckerType::Array>(
                            &current_type->details)) {
                        // TODO: handle arrays
                        std::ignore = as_arr;
                        debugbreak();
                    } else if (auto as_named =
                                   std::get_if<BorrowCheckerType::Named>(
                                       &current_type->details)) {
                        // access of a struct field is stable, and that
                        // of a union is not
                        if (as_named->actual_type.get_decl_if_union()) {
                            is_unstable = true;
                            affected_value = elem.to_string();
                        }
                    } else
                        debugbreak();
                    if (is_unstable) break;
                    current_value.subpaths.push_back(field);
                    current_type = &state->get_value_type(current_value);
                }
            }
            return std::make_pair(is_unstable, std::move(affected_value));
        };
        for (auto& [domain, point_to] : state->final_ptg.domain_to_node) {
            // check if the domain points to anything that is being
            // modified
            for (auto& val : point_to) {
                if (auto [is_unstable, affected] = unstable_suffix(val);
                    is_unstable) {
                    result.emplace_back(
                        domain,
                        KillReason{.source = KillReason::Assign,
                                   .affected_value = std::move(affected),
                                   .bad_pointee = val});
                    break;
                }
            }
        }
        return result;
    }
    std::set<std::string> kill_for_dropping_value(const Value& in) {
        std::set<std::string> result;
        for (auto& [domain, point_to] : state->final_ptg.domain_to_node) {
            for (auto& val : point_to) {
                // hopefully this string match would suffice
                if (Value::from(std::string(val))
                        .to_string()
                        .starts_with(in.to_string())) {
                    result.insert(domain);
                    break;
                }
            }
        }
        return result;
    }
    std::set<std::string> operator()(NewPrimitiveInstruction* inst) {
        // GEN and KILL are empty sets
        return state->dfa_in[inst];
    }
    std::set<std::string> operator()(NewAggregateInstruction* inst) {
        return state->dfa_in[inst];
    }
    std::set<std::string> operator()(DomainSubsetConstraint* inst) {
        auto in = state->dfa_in[inst];
        // GEN = the new domain introduced
        // TODO: should probably remove the old version of the domain
        in.insert(inst->super.to_string());
        return in;
    }
    std::set<std::string> operator()(DomainExtensionConstraint* inst) {
        auto in = state->dfa_in[inst];
        in.insert(inst->super.to_string());
        return in;
    }
    std::set<std::string> operator()(DomainPhiInstruction* inst) {
        // this intrusction introduces a new doamin only if all the
        // input domains are valid
        auto in = state->dfa_in[inst];

        in.insert(inst->into);
        return in;
    }
    std::set<std::string> operator()(RelocateValueInstruction* inst) {
        return state->dfa_in[inst];
    }
    std::set<std::string> operator()(BorrowValueInstruction* inst) {
        return state->dfa_in[inst];
    }
    std::set<std::string> operator()(CondBrInstruction* inst) {
        return state->dfa_in[inst];
    }
    std::set<std::string> operator()(BrInstruction* inst) {
        return state->dfa_in[inst];
    }
    std::set<std::string> operator()(RetInstruction* inst) {
        return state->dfa_in[inst];
    }
    std::set<std::string> operator()(DerefOperation* inst) {
        // The valid sets don't change during this operation
        return state->dfa_in[inst];
    }
    std::set<std::string> operator()(DerefLoadOperation* inst) {
        return state->dfa_in[inst];
    }
    std::set<std::string> operator()(PhiInstruction* inst) {
        // This always introduces a new value (no reassignment)
        // TODO: we still don't hanlde move of the input values
        return state->dfa_in[inst];
    }
    std::set<std::string> operator()(AssignInstruction* inst) {
        // assign modifies a value, so it invalidates all domains that
        // points to values originating from that value this will
        // contain all the modified values by this assign (it will be
        // more than one in the case of lvalue assign)
        std::vector<Value> lhs_values;
        std::optional<std::string> origin_string = std::nullopt;
        if (auto& type = state->get_value_type(inst->lhs);
            type.details.index() == BorrowCheckerType::LValue) {
            origin_string =
                std::get<BorrowCheckerType::LValue>(type.details).origin;
            auto pointees =
                state->final_ptg.get_pointees_of(inst->lhs_domain.to_string()) |
                std::views::transform([](std::string input) {
                    return Value::from(std::move(input));
                });
            lhs_values.insert(lhs_values.end(), pointees.begin(),
                              pointees.end());
        } else {
            lhs_values.push_back(inst->lhs);
        }
        auto kill = kill_for_modifying_values(lhs_values);
        std::vector<std::string> output;
        std::ranges::set_difference(
            state->dfa_in[inst],
            kill | std::views::transform([](auto& elem) { return elem.first; }),
            std::back_inserter(output));
        if (output.size() != state->dfa_in[inst].size()) {
            // add kill reasons for the killed domains
            auto& in = state->dfa_in[inst];
            for (auto& [domain, reason] : kill) {
                if (in.contains(domain) &&
                    !state->domain_kill_reason.contains(domain)) {
                    reason.killing_instruction = inst->origin;
                    reason.lvalue_source = origin_string;
                    state->domain_kill_reason[domain] = reason;
                }
            }
        }
        return std::set<std::string>{output.begin(), output.end()};
    }
    std::set<std::string> operator()(DropInstruction* inst) {
        // similarly to assign all domains point to children of the
        // dropped value are invalidated but, the domains pointing to
        // the dropped value are also invalidated, and stability does
        // not matter
        auto kill = kill_for_dropping_value(inst->val);
        std::vector<std::string> output;
        std::ranges::set_difference(state->dfa_in[inst], kill,
                                    std::back_inserter(output));
        return std::set<std::string>{output.begin(), output.end()};
    }
    std::set<std::string> operator()(CallFunctionInstruction* inst) {
        // functions cannot do anything with references now
        return state->dfa_in[inst];
    }
    std::set<std::string> operator()(Instruction* inst) {
        debugbreak();
        return {};
    }
};
void DomainCheckerState::do_domain_validity_analysis(BasicBlock* entry) {
    // this is not the optimal order for a forward DFA, but it'll work
    auto blocks_without_entry =
        func->blocks |
        std::views::transform([](auto& elem) { return elem.get(); }) |
        std::views::filter([entry](auto elem) { return elem != entry; });
    std::deque<BasicBlock*> worklist(blocks_without_entry.begin(),
                                     blocks_without_entry.end());
    // we handle the entry block specially
    enum TransferStatus : uint8_t { Changed = 0, Unchanged };
    auto transfer_fn = [this](Instruction* inst, BasicBlock* block,
                              size_t idx) -> TransferStatus {
        auto visitor = TransferFunctionVisitor{this, block, idx};
        auto out_k = std::visit(visitor, inst->to_variant());
        if (out_k != dfa_out[inst]) {
            dfa_out[inst] = std::move(out_k);
            return Changed;
        } else
            return Unchanged;
    };
    dfa_in.insert(
        std::pair{entry->instructions.front().get(), std::set<std::string>{}});
    transfer_fn(entry->instructions.front().get(), entry, 0);
    for (auto idx : std::views::iota(1u, entry->instructions.size())) {
        auto inst = entry->instructions[idx].get();
        dfa_in.insert(
            std::pair{inst, dfa_out[entry->instructions[idx - 1].get()]});
        transfer_fn(inst, entry, idx);
    }
    while (!worklist.empty()) {
        auto current_block = worklist.front();
        worklist.pop_front();
        // handle first instruction
        auto first_inst = current_block->instructions.front().get();
        auto in_first_inst = std::set<std::string>{};
        // IN_k = ∩_{p in preds(k)} OUT_p
        for (const auto& domain :
             dfa_out[current_block->preds[0]->instructions.back().get()]) {
            bool part_of_intersection = true;
            for (auto pred :
                 current_block->preds |
                     std::views::transform([this](BasicBlock* pred) {
                         return std::cref(
                             dfa_out[pred->instructions.back().get()]);
                     })) {
                if (!pred.get().contains(domain)) {
                    part_of_intersection = false;
                    break;
                }
            }
            if (part_of_intersection) in_first_inst.insert(domain);
        }
        TransferStatus last_inst_status = Changed;
        dfa_in[first_inst] = std::move(in_first_inst);
        // last_inst_status is the transfer status of the last
        // instruction, it changes if the block has more than
        // instruction
        last_inst_status = transfer_fn(first_inst, current_block, 0);
        auto last_inst = current_block->instructions.back().get();
        if (current_block->instructions.size() > 1) {
            for (auto idx :
                 std::views::iota(1u, current_block->instructions.size() - 1)) {
                auto inst = current_block->instructions[idx].get();
                dfa_in[inst] =
                    dfa_out[current_block->instructions[idx - 1].get()];
                transfer_fn(inst, current_block, idx);
            }
            dfa_in[last_inst] =
                dfa_out[current_block
                            ->instructions[current_block->instructions.size() -
                                           2]
                            .get()];
            last_inst_status =
                transfer_fn(last_inst, current_block,
                            current_block->instructions.size() - 1);
        }
        // we handle the last instruction specially, because we'll need
        // to update the worklist if it changes
        if (last_inst_status == Changed) {
            // insert all the successors into the worklist (that aren't
            // already there)
            for (auto succ : last_inst->children()) {
                auto it = std::ranges::find(worklist, succ);
                if (it != worklist.end()) worklist.push_back(succ);
            }
        }
    }
}
void DomainCheckerState::build_dominators() {
    // Incomplete on purpose
    dominators.register_for(entry_block, entry_block);
    bool should_loop = true;
    // the order should not matter
    while (should_loop) {
        should_loop = false;
    }
}
void DomainCheckerState::calc_block_preds() {
    for (auto& block : func->blocks) {
        for (auto child : block->instructions.back()->children())
            child->preds.push_back(block.get());
    }
}
std::string ValueProducer::name_for(const std::string& val) {
    if (!var_map.contains(val)) var_map[val] = 0;
    return std::format("{}__{}", val, ++var_map[val]);
}
void ssa_transform_do_block(BasicBlock* block, UseStorageTy& storage,
                            ValueProducer& producer, DomainCheckerState*);
std::optional<std::string> lookup_var_recursive(const std::string& arg,
                                                BasicBlock* start_at,
                                                UseStorageTy& storage,
                                                ValueProducer& producer,
                                                DomainCheckerState* state) {
    if (start_at->preds.size() == 1) {
        auto pred = start_at->preds[0];
        if (!storage.contains(pred))
            ssa_transform_do_block(pred, storage, producer, state);

        if (storage[pred].contains(arg))
            return storage[pred][arg];
        else
            return lookup_var_recursive(arg, pred, storage, producer, state);
    } else {
        auto new_name = producer.name_for(arg);
        auto phi = std::unique_ptr<DomainPhiInstruction>(
            new DomainPhiInstruction({}, std::string(new_name)));
        // if the assignment is downward exposed we add it to our latest
        // instances
        if (!storage[start_at].contains(arg)) storage[start_at][arg] = new_name;

        for (auto& pred : start_at->preds) {
            if (!storage.contains(pred))
                ssa_transform_do_block(pred, storage, producer, state);

            if (storage[pred].contains(arg))
                phi->args.push_back(Domain(storage[pred][arg]));
            else {
                if (auto val = lookup_var_recursive(arg, pred, storage,
                                                    producer, state))
                    phi->args.push_back(Domain(*val));
                else
                    return std::nullopt;
            }
        }
        if (phi->args.empty())
            storage[start_at][arg] = "null";
        else {
            bool is_trivial_phi = true;
            for (auto& elem : phi->args) {
                if (elem.name != phi->args[0].name) {
                    is_trivial_phi = false;
                    break;
                }
            }
            if (is_trivial_phi)
                storage[start_at][arg] = phi->args[0].name;
            else
                start_at->instructions.emplace(start_at->instructions.begin(),
                                               std::move(phi));
        }
        return storage[start_at][arg];
    }
}
void ssa_transform_do_block(BasicBlock* block, UseStorageTy& storage,
                            ValueProducer& producer,
                            DomainCheckerState* state) {
    if (storage.contains(block)) return;
    auto& storage_entry = storage[block];
    // perform local value numbering for everything in this block
    // and collect un-numbered uses into this vector
    std::vector<std::reference_wrapper<std::string>> unfound_uses;
    std::vector<std::pair<AssignInstruction*, size_t>> deferred_assignments;
    for (auto it = block->instructions.begin(); it != block->instructions.end();
         ++it) {
        std::visit(
            [&]<typename T>(T* tp) {
                if constexpr (std::is_same_v<T, DomainExtensionConstraint>) {
                    if (!tp->super.is_var()) debugbreak();

                    if (!storage_entry.contains(tp->super.name)) {
                        tp->old_super.name = tp->super.name;
                        unfound_uses.push_back(std::ref(tp->old_super.name));
                    } else
                        tp->old_super.name = storage_entry[tp->super.name];

                    if (tp->sub.is_var()) {
                        if (storage_entry.contains(tp->sub.name))
                            tp->sub.name = storage_entry[tp->sub.name];
                        else
                            unfound_uses.push_back(std::ref(tp->sub.name));
                    }
                    auto new_name = producer.name_for(tp->super.name);
                    storage_entry[tp->super.name] = new_name;
                    tp->super.name = new_name;

                    // state->pgraph.add_edge(new_name,
                    // Domain(tp->sub));
                    // state->pgraph.add_edge(new_name,
                    // Domain(tp->old_super));
                }
                if constexpr (std::is_same_v<T, DomainSubsetConstraint>) {
                    if (!tp->super.is_var()) debugbreak();
                    auto new_name = producer.name_for(tp->super.name);
                    storage_entry[tp->super.name] = new_name;

                    // if (tp->sub.is_null())
                    // state->pgraph.initialize(new_name); else
                    // state->pgraph.add_edge(new_name,
                    // Domain(tp->sub));

                    tp->super.name = new_name;

                    if (tp->sub.is_var()) {
                        if (storage_entry.contains(tp->sub.name))
                            tp->sub.name = storage_entry[tp->sub.name];
                        else
                            unfound_uses.push_back(std::ref(tp->sub.name));
                    }
                    if (!tp->lvalue_domain.empty()) {
                        if (storage_entry.contains(tp->lvalue_domain))
                            tp->lvalue_domain =
                                storage_entry[tp->lvalue_domain];
                        else
                            unfound_uses.push_back(std::ref(tp->lvalue_domain));
                    }
                }
                if constexpr (std::is_same_v<T,
                                             DomainDependenceEdgeConstraint>) {
                    if (tp->d1.is_var()) {
                        if (storage_entry.contains(tp->d1.name))
                            tp->d1.name = storage_entry[tp->d1.name];
                        else
                            unfound_uses.push_back(std::ref(tp->d1.name));
                    }
                }
                if constexpr (std::is_same_v<T, AssignInstruction>) {
                    AssignInstruction* inst = tp;
                    auto& type = state->get_value_type(inst->lhs);
                    auto as_lval =
                        std::get_if<BorrowCheckerType::LValue>(&type.details);
                    if (!as_lval) return;
                    // write the rhs_domain and lhs_domain as they're
                    // uses for the DUG lhs is used to read its pointees
                    // for update
                    inst->lhs_domain = type.domains[0].first;
                    if (storage_entry.contains(inst->lhs_domain.name))
                        inst->lhs_domain.name =
                            storage_entry[inst->lhs_domain.name];
                    else
                        unfound_uses.push_back(std::ref(inst->lhs_domain.name));
                    // if its a pointer to pointer we need to add may
                    // store instruction(s)
                    if (!as_lval->subtype->domains.empty()) {
                        inst->rhs_domain =
                            state->get_value_type(inst->rhs).domains[0].first;
                        if (storage_entry.contains(inst->rhs_domain.name))
                            inst->rhs_domain.name =
                                storage_entry[inst->rhs_domain.name];
                        else
                            unfound_uses.push_back(
                                std::ref(inst->rhs_domain.name));

                        for (auto pointee : state->ptgraph.get_pointees_of(
                                 type.domains[0].first.to_string())) {
                            auto& type = state->get_value_type(
                                Value::from(std::move(pointee)));
                            auto new_name =
                                producer.name_for(type.domains[0].first.name);
                            auto op = new MayStoreOperation(
                                tp, Domain(type.domains[0].first.name),
                                Domain(new_name));
                            inst->may_stores.push_back(op);

                            if (storage_entry.contains(
                                    type.domains[0].first.name))
                                op->old_domain.name = storage_entry.at(
                                    type.domains[0].first.name);
                            else
                                unfound_uses.push_back(
                                    std::ref(op->old_domain.name));

                            storage_entry[type.domains[0].first.name] =
                                new_name;

                            it = block->instructions.emplace(++it, op);
                        }
                    }
                }
                if constexpr (std::is_same_v<T, DerefLoadOperation>) {
                    // if its a reference to reference add may load
                    // instructions

                    if (storage_entry.contains(tp->ref_domain.name))
                        tp->ref_domain.name =
                            storage_entry[tp->ref_domain.name];
                    else
                        unfound_uses.push_back(std::ref(tp->ref_domain.name));

                    auto& pointer_type = state->get_value_type(tp->reference);
                    auto& pointer_details = std::get<BorrowCheckerType::RefPtr>(
                        pointer_type.details);
                    if (!pointer_details.subtype->domains.empty()) {
                        for (auto pointee : state->ptgraph.get_pointees_of(
                                 pointer_type.domains[0].first.to_string())) {
                            auto& type = state->get_value_type(
                                Value::from(std::move(pointee)));
                            auto op = new MayLoadOperation(
                                tp, Domain(type.domains[0].first));
                            tp->may_loads.push_back(op);
                            if (storage_entry.contains(
                                    type.domains[0].first.name))
                                op->domain.name = storage_entry.at(
                                    type.domains[0].first.name);
                            else
                                unfound_uses.push_back(
                                    std::ref(op->domain.name));

                            it = block->instructions.emplace(++it, op);
                        }
                    }
                }
                if constexpr (std::is_same_v<T, DerefOperation>) {
                    if (storage_entry.contains(tp->ref_domain.name))
                        tp->ref_domain.name =
                            storage_entry[tp->ref_domain.name];
                    else
                        unfound_uses.push_back(std::ref(tp->ref_domain.name));
                }
            },
            (*it)->to_variant());
    }
    // perform global value numbering
    if (block->preds.size() == 1) {
        std::erase_if(
            unfound_uses, [&, pred = block->preds[0]](
                              std::reference_wrapper<std::string> arg) {
                if (!storage.contains(pred))
                    ssa_transform_do_block(pred, storage, producer, state);

                if (storage[pred].contains(arg))
                    arg.get() = storage[pred][arg];
                else {
                    if (auto val = lookup_var_recursive(arg, pred, storage,
                                                        producer, state))
                        arg.get() = *val;
                    else
                        return false;
                }
                return true;
            });

    } else {
        std::erase_if(
            unfound_uses, [&](std::reference_wrapper<std::string> arg) {
                auto new_name = producer.name_for(arg);
                auto phi = std::unique_ptr<DomainPhiInstruction>(
                    new DomainPhiInstruction({}, std::string(new_name)));
                // if the assignment is downward exposed we add it to
                // our latest instances
                if (!storage_entry.contains(arg)) storage_entry[arg] = new_name;

                for (auto& pred : block->preds) {
                    if (!storage.contains(pred))
                        ssa_transform_do_block(pred, storage, producer, state);

                    if (storage[pred].contains(arg))
                        phi->args.push_back(Domain(storage[pred][arg]));
                    else {
                        if (auto val = lookup_var_recursive(arg, pred, storage,
                                                            producer, state))
                            phi->args.push_back(Domain(*val));
                        else
                            return false;
                    }
                }
                if (phi->args.empty()) {
                    storage_entry[arg] = "null";
                } else {
                    bool is_trivial_phi = true;
                    for (auto& elem : phi->args) {
                        if (elem.name != phi->args[0].name) {
                            is_trivial_phi = false;
                            break;
                        }
                    }
                    if (is_trivial_phi)
                        storage_entry[arg] = phi->args[0].name;
                    else
                        block->instructions.emplace(block->instructions.begin(),
                                                    std::move(phi));
                }

                arg.get() = storage_entry[arg];
                return true;
            });
    }
    if (!unfound_uses.empty()) debugbreak();
}
void DomainCheckerState::transform_to_ssa() {
    ValueProducer producer;
    UseStorageTy latest_variables;
    // this is not post order, its just kinda close to it
    for (auto& block : func->blocks) {
        ssa_transform_do_block(block.get(), latest_variables, producer, this);
    }
}
void DomainCheckerState::clear_dependencies() {
    std::vector<std::pair<std::string, std::string>> dependence_edges;
    auto add_dependencies_in_block = [&dependence_edges](BasicBlock* blk) {
        for (auto& inst : blk->instructions) {
            std::visit(
                [&dependence_edges]<typename T>(T* inst) {
                    if constexpr (std::is_same_v<
                                      T, DomainDependenceEdgeConstraint>)
                        dependence_edges.emplace_back(inst->d1.name,
                                                      inst->d2.name);
                },
                inst->to_variant());
        }
    };
    for (auto& block : func->blocks) {
        add_dependencies_in_block(block.get());
    }
    // write this to graphviz for visualization
    std::string gvz_content;
    for (const auto& [from, to] : dependence_edges) {
        gvz_content += std::format("    \"{}\" -> \"{}\";\n", from, to);
    }
    auto final_graph = std::format("digraph G {{\n{}}}", gvz_content);
    std::cout << final_graph << std::endl;
}
void DomainCheckerState::build_dug() {
    std::unordered_set<Instruction*> skip_instruction;
    struct DUGEdgeInserter {
        DomainCheckerState* state;
        std::unordered_set<Instruction*>& skip;
        // points to the instruction that defines a variable
        std::unordered_map<std::string, Instruction*> defining_instruction;
        // uses of a variable where the definition isn't found yet
        std::unordered_map<std::string, std::vector<Instruction*>> delayed_uses;
        void add_definition(const std::string& var, Instruction* inst) {
            if (defining_instruction.contains(var)) return;
            defining_instruction[var] = inst;
            if (delayed_uses.contains(var)) {
                auto node = delayed_uses.extract(var);
                for (auto& elem : node.mapped()) {
                    state->def_use_graph.edges[inst].emplace(elem, var);
                }
            }
        }
        void add_use(const std::string& var, Instruction* inst) {
            if (defining_instruction.contains(var)) {
                state->def_use_graph.edges[defining_instruction[var]].emplace(
                    inst, var);
            } else {
                // I honestly don't know that it is possible to use a
                // variable before definition, maybe in loops or
                // depending on the order we walk the cfg
                delayed_uses[var].emplace_back(inst);
            }
        }
        void operator()(DomainSubsetConstraint* con) {
            add_definition(con->super.to_string(), con);
            if (con->sub.is_var()) add_use(con->sub.to_string(), con);
        }
        void operator()(DomainExtensionConstraint* con) {
            add_definition(con->super.to_string(), con);
            if (con->sub.is_var()) add_use(con->sub.to_string(), con);
            if (con->old_super.is_var())
                add_use(con->old_super.to_string(), con);
        }
        void operator()(DomainPhiInstruction* op) {
            add_definition(op->into, op);
            for (auto& operand : op->args) {
                if (operand.is_var()) add_use(operand.to_string(), op);
            }
        }
        void operator()(MayStoreOperation* op) {
            add_definition(op->new_domain.to_string(), op->origin);
            add_use(op->old_domain.to_string(), op->origin);
        }
        void operator()(AssignInstruction* op) {
            if (op->may_stores.empty()) return;

            // we use the domain of the pointer we dereferenced to get
            // this lvalue
            add_use(op->lhs_domain.to_string(), op);
            // we use the domain we're copying into here
            add_use(op->rhs_domain.to_string(), op);
        }
        void operator()(MayLoadOperation* op) {
            add_use(op->domain.to_string(), op->origin);
            auto parent = dynamic_cast<DerefLoadOperation*>(op->origin);
            auto& type =
                state->get_value_type(Value::from(std::string(parent->into)));
            if (!type.domains.empty())
                add_definition(type.domains[0].first.to_string(), parent);
        }
        void operator()(DerefLoadOperation* op) {
            add_use(op->ref_domain.to_string(), op);
            for (auto inst : op->definer) {
                auto as_sub_constraint =
                    dynamic_cast<DomainSubsetConstraint*>(inst);
                skip.insert(as_sub_constraint);
                add_definition(as_sub_constraint->super.to_string(), op);
            }
        }
        void operator()(DerefOperation* op) {
            // this should also count as a use, we're going to need to
            // know the domain at this instruction during check time I'm
            // not sure how to handle this, however as there may be an
            // associated store or reborrow
            add_use(op->ref_domain.to_string(), op);
        }
        void operator()(Instruction*) {}
    };
    DUGEdgeInserter edge_inserter{this, skip_instruction};
    for (auto& block : func->blocks) {
        for (auto& inst : block->instructions) {
            if (skip_instruction.contains(inst.get())) continue;
            std::visit(edge_inserter, inst->to_variant());
        }
    }
}
static auto clone_type(const BorrowCheckerType& type) -> BorrowCheckerType {
    using enum BorrowCheckerType::TypeType;
    switch (type.details.index()) {
    case Primitive:
        return BorrowCheckerType::new_primitive();
    case Aggregate: {
        auto ntype = BorrowCheckerType();
        ntype.domains = type.domains;
        BorrowCheckerType::FieldMap fmap;
        for (auto& [fname, ftype] : std::get<Aggregate>(type.details).fields)
            fmap.emplace(fname, clone_type(ftype));
        ntype.details.emplace<Aggregate>(std::move(fmap));
        return ntype;
    }
    case Union: {
        auto ntype = BorrowCheckerType();
        ntype.domains = type.domains;
        BorrowCheckerType::FieldMap fmap;
        for (auto& [fname, ftype] : std::get<Union>(type.details).fields)
            fmap.emplace(fname, clone_type(ftype));
        ntype.details.emplace<Union>(std::move(fmap));
        return ntype;
    }
    case UniquePtr: {
        auto ntype = BorrowCheckerType();
        ntype.domains = type.domains;
        ntype.details.emplace<UniquePtr>(std::make_unique<BorrowCheckerType>(
            clone_type(*std::get<UniquePtr>(type.details).subtype)));
        return ntype;
    }
    case Array: {
        auto ntype = BorrowCheckerType();
        ntype.domains = type.domains;
        ntype.details.emplace<Array>(std::make_unique<BorrowCheckerType>(
            clone_type(*std::get<Array>(type.details).subtype)));
        return ntype;
    }
    case RefPtr: {
        auto ntype = BorrowCheckerType();
        ntype.domains = type.domains;
        ntype.details.emplace<RefPtr>(std::make_unique<BorrowCheckerType>(
            clone_type(*std::get<RefPtr>(type.details).subtype)));
        return ntype;
    }
    case Named: {
        auto ntype = BorrowCheckerType();
        ntype.domains = type.domains;
        ntype.details.emplace<Named>(
            Type(std::get<Named>(type.details).actual_type));
        return ntype;
    }
    case LValue: {
        auto ntype = BorrowCheckerType();
        ntype.domains = type.domains;
        ntype.details.emplace<LValue>(std::make_unique<BorrowCheckerType>(
            clone_type(*std::get<LValue>(type.details).subtype)));
        std::get<LValue>(ntype.details).subpath =
            std::get<LValue>(type.details).subpath;
        return ntype;
    }
    default:
        debugbreak();
    }
    return BorrowCheckerType{};
};
BorrowCheckerType BorrowCheckerType::borrowed(DomainCheckerState* stt) const {
    BorrowCheckerType new_type;
    new_type.domains.emplace_back(stt->new_domain_var(), false);
    // all child domains become bidirectional
    // If `this` is an lvalue, we reborrow i.e &a, where a is of type T
    // and T is lvalue<R> returns &R
    auto inner = details.index() == LValue
                     ? std::get<LValue>(details).subtype->cloned(stt)
                     : cloned(stt);
    for (auto& [domain, _] : inner.domains)
        new_type.domains.emplace_back(domain, true);
    new_type.details.emplace<RefPtr>(
        std::make_unique<BorrowCheckerType>(std::move(inner)));
    return new_type;
}
BorrowCheckerType BorrowCheckerType::cloned(DomainCheckerState* stt) const {
    std::unordered_map<std::string, Domain> old_to_new_map;

    auto new_type = moved(stt);
    for (auto& [domain, is_bidr] : domains) {
        old_to_new_map[domain.to_string()] = stt->new_domain_var();
    }

    auto substitute_domains = [&old_to_new_map](BorrowCheckerType& type,
                                                const auto& self) -> void {
        for (auto& [domain, is_bidr] : type.domains) {
            domain = old_to_new_map[domain.to_string()];
        }
        switch (type.details.index()) {
        case Primitive:
            break;
        case Aggregate: {
            auto& fmap = std::get<Aggregate>(type.details);
            for (auto& [_, ftype] : fmap.fields) self(ftype, self);
            break;
        }
        case Union: {
            auto& fmap = std::get<Union>(type.details);
            for (auto& [_, ftype] : fmap.fields) self(ftype, self);
            break;
        }
        case UniquePtr: {
            auto& dets = std::get<UniquePtr>(type.details);
            self(*dets.subtype, self);
            break;
        }
        case Array: {
            auto& dets = std::get<Array>(type.details);
            self(*dets.subtype, self);
            break;
        }
        case RefPtr: {
            auto& dets = std::get<RefPtr>(type.details);
            self(*dets.subtype, self);
            break;
        }
        case Named: {
            // this probably doesn't require any special behaviour
            break;
        }
        case LValue: {
            auto& dets = std::get<LValue>(type.details);
            self(*dets.subtype, self);
            break;
        }
        default:
            debugbreak();
        }
    };

    substitute_domains(new_type, substitute_domains);
    return new_type;
}
BorrowCheckerType BorrowCheckerType::moved(DomainCheckerState*) const {
    BorrowCheckerType new_type;
    new_type.domains = domains;
    if (auto aggrg = std::get_if<Aggregate>(&details); aggrg) {
        decltype(aggrg->fields) new_fields;
        for (auto& [fname, ftype] : aggrg->fields)
            new_fields.emplace(fname, clone_type(ftype));
        new_type.details.emplace<Aggregate>(std::move(new_fields));
    } else if (auto unn = std::get_if<Union>(&details); unn) {
        decltype(unn->fields) new_fields;
        for (auto& [fname, ftype] : unn->fields)
            new_fields.emplace(fname, clone_type(ftype));
        new_type.details.emplace<Union>(std::move(new_fields));
    } else if (auto arr = std::get_if<Array>(&details); arr) {
        new_type.details.emplace<Array>(
            std::make_unique<BorrowCheckerType>(clone_type(*arr->subtype)));
    } else if (auto unq = std::get_if<UniquePtr>(&details); unq) {
        new_type.details.emplace<UniquePtr>(
            std::make_unique<BorrowCheckerType>(clone_type(*unq->subtype)));
    } else if (auto ref = std::get_if<RefPtr>(&details); ref) {
        new_type.details.emplace<RefPtr>(
            std::make_unique<BorrowCheckerType>(clone_type(*ref->subtype)));
    } else if (auto prim = std::get_if<Primitive>(&details); prim) {
        new_type.details.emplace<Primitive>();
    } else if (auto named = std::get_if<Named>(&details); named) {
        new_type.details.emplace<Named>(Type(named->actual_type));
    } else if (auto lvalue = std::get_if<LValue>(&details); lvalue) {
        new_type.details.emplace<LValue>(
            std::make_unique<BorrowCheckerType>(clone_type(*lvalue->subtype)));
        std::get<LValue>(new_type.details).subpath = lvalue->subpath;
    } else {
        debugbreak();
    }
    return new_type;
}
BorrowCheckerType BorrowCheckerType::deref() const {
    // deref can be called on RefPtr<T> or LValue<RefPtr<T>> to yield
    // LValue<T>
    auto ref_ptr_type = this;
    if (auto as_lval = std::get_if<LValue>(&details); as_lval) {
        ref_ptr_type = as_lval->subtype.get();
    }
    auto& ref_ptr = std::get<RefPtr>(ref_ptr_type->details);
    auto new_type = BorrowCheckerType{};
    new_type.domains = ref_ptr_type->domains;
    new_type.details.emplace<LValue>(
        std::make_unique<BorrowCheckerType>(clone_type(*ref_ptr.subtype)));
    return new_type;
}
BorrowCheckerType BorrowCheckerType::new_primitive() {
    BorrowCheckerType new_type;
    new_type.details.emplace<Primitive>();
    return new_type;
}
BorrowCheckerType BorrowCheckerType::new_aggregate_from(Type&& tp) {
    BorrowCheckerType new_type;
    new_type.details.emplace<Named>(Type(tp));
    // auto& as_named = std::get<Named>(new_type.details);
    return new_type;
}
BorrowCheckerType BorrowCheckerType::new_array_of(BorrowCheckerType&& tp) {
    BorrowCheckerType new_type;
    // domains retain thier bidirectionality
    for (auto& [domain, bidr] : tp.domains)
        new_type.domains.emplace_back(domain, bidr);
    new_type.details.emplace<Array>(
        std::make_unique<BorrowCheckerType>(clone_type(tp)));
    return new_type;
}
std::string BorrowCheckerType::to_string() const {
    std::string result;
    switch (details.index()) {
    case Primitive:
        return "primitive";
    case Aggregate: {
        auto& fmap = std::get<Aggregate>(details);
        result += "struct { ";
        for (auto& [fname, ftype] : fmap.fields)
            result += std::format("{}: {}, ", fname, ftype.to_string());
        result += "}";
        break;
    }
    case Union: {
        auto& fmap = std::get<Union>(details);
        result += "union { ";
        for (auto& [fname, ftype] : fmap.fields)
            result += std::format("{}: {}, ", fname, ftype.to_string());
        result += "}";
        break;
    }
    case UniquePtr: {
        auto& dets = std::get<UniquePtr>(details);
        result = std::format("UniquePtr<{}>", dets.subtype->to_string());
        break;
    }
    case Array: {
        auto& dets = std::get<Array>(details);
        result = std::format("[{}]", dets.subtype->to_string());
        break;
    }
    case RefPtr: {
        auto& dets = std::get<RefPtr>(details);
        result = std::format("&{} {}", domains[0].first.to_string(),
                             dets.subtype->to_string());
        break;
    }
    case LValue: {
        auto& dets = std::get<LValue>(details);
        std::string path_str = "";
        if (!dets.subpath.empty()) {
            path_str += ", ";
            for (auto& path : dets.subpath) {
                path_str += "." + path;
            }
        }
        result = std::format("lvalue<({}{}), {}>", domains[0].first.to_string(),
                             path_str, dets.subtype->to_string());
        break;
    }
    case Named:
        result = std::get<Named>(details).actual_type.full_name();
        break;
    default:
        debugbreak();
    }
    return result;
}
std::string ValueTypeMapping::to_string() {
    std::string result = "Value - Type mapping:\n";
    for (auto& [value, type] : *this) {
        result += "%" + value + " :: " + type.to_string() + "\n";
    }
    return result;
}
bool InclusionPointerAnalyser::operator()(AssignInstruction* inst) {
    auto& type = state->get_value_type(inst->lhs);
    auto as_lval = std::get_if<BorrowCheckerType::LValue>(&type.details);
    if (!as_lval) return false;
    // pointer to primitive isn't special
    if (as_lval->subtype->domains.empty()) return false;
    auto& right_dom = state->get_value_type(inst->rhs).domains[0].first;
    bool has_change = false;
    for (auto pointee :
         ptg.get_pointees_of(type.domains[0].first.to_string())) {
        // TODO rhs as lvalue
        DomainSubsetConstraint con(
            Domain(state->get_value_type(Value::from(std::move(pointee)))
                       .domains[0]
                       .first),
            Domain(right_dom));
        has_change = (*this)(&con) || has_change;
    }
    return has_change;
}
bool InclusionPointerAnalyser::operator()(DerefLoadOperation* op) {
    bool has_change = false;
    auto& out_t = state->get_value_type(Value::from(std::string(op->into)));
    if (out_t.domains.empty()) return false;
    auto out_domain = out_t.domains[0].first;
    // if its a pointer-to-pointer load, the result introduces a new
    // domain
    for (auto pointee : ptg.get_pointees_of(op->ref_domain.to_string())) {
        // all the pointees of the pointees of the references are
        // pointees of the output domain
        DomainSubsetConstraint con(
            Domain(out_domain),
            Domain(state->get_value_type(Value::from(std::move(pointee)))
                       .domains[0]
                       .first));
        has_change = (*this)(&con) || has_change;
    }
    return has_change;
}
bool InclusionPointerAnalyser::operator()(DomainSubsetConstraint* con) {
    // p > q
    if (con->sub.is_var()) {
        bool has_change = false;
        for (auto& pointee : ptg.get_pointees_of(con->sub.to_string())) {
            has_change =
                ptg.add_edge(con->super.to_string(), pointee) || has_change;
        }
        return has_change;
    }
    // p > {q}
    else if (!con->sub.is_null()) {
        auto as_val = Value::from(con->sub.to_string());
        auto& val_type = state->get_value_type(as_val);
        // for lvalues we reborrow, so we need to look at what the
        // lvalue is from and extract the references from there
        if (auto lval =
                std::get_if<BorrowCheckerType::LValue>(&val_type.details)) {
            bool has_change = false;
            for (auto& pointee :
                 ptg.get_pointees_of(val_type.domains[0].first.to_string())) {
                auto edge = pointee;
                // apply the path;
                for (auto& path : lval->subpath) edge += "." + path;
                has_change =
                    ptg.add_edge(con->super.to_string(), edge) || has_change;
            }
            con->lvalue_domain = val_type.domains[0].first.to_string();
        } else
            return ptg.add_edge(con->super.to_string(), con->sub.to_string());
    }
    return false;
}
bool InclusionPointerAnalyser::operator()(DomainExtensionConstraint* con) {
    // p > p + q
    if (con->sub.is_var()) {
        bool has_change = false;
        for (auto& pointee : ptg.get_pointees_of(con->sub.to_string())) {
            has_change =
                ptg.add_edge(con->super.to_string(), pointee) || has_change;
        }
        return has_change;
    }
    // p > p + {q}
    else if (!con->sub.is_null()) {
        auto as_val = Value::from(con->sub.to_string());
        auto& val_type = state->get_value_type(as_val);
        // for lvalues we reborrow, so we need to look at what the
        // lvalue is from and extract the references from there
        if (auto lval =
                std::get_if<BorrowCheckerType::LValue>(&val_type.details)) {
            bool has_change = false;
            for (auto& pointee :
                 ptg.get_pointees_of(val_type.domains[0].first.to_string())) {
                auto edge = pointee;
                // apply the path;
                for (auto& path : lval->subpath) edge += "." + path;
                has_change =
                    ptg.add_edge(con->super.to_string(), edge) || has_change;
            }
        } else
            return ptg.add_edge(con->super.to_string(), con->sub.to_string());
    }
    return false;
}
Value LValueEmitter::do_expr(Expression* expr) {
    if (auto px = dynamic_cast<PrefixOperation*>(expr)) {
        if (px->op.type == TokenType::Star) {
            auto this_eval = std::visit(em, px->operand->toVariant());
            auto result = em.temporary_name();
            em.current_block->add_instruction(
                new DerefOperation(std::move(this_eval), std::string(result)),
                expr);
            return Value::from(std::move(result));
        }
    }
    if (auto bexp = dynamic_cast<BinaryOperation*>(expr)) {
        if (auto rhs_name = dynamic_cast<NameExpression*>(bexp->rhs.get());
            rhs_name && bexp->op.type == TokenType::Dot) {
            auto left_value = do_expr(bexp->lhs.get());
            if (bexp->lhs->evaluated_type.is_reference()) {
                auto result = em.temporary_name();
                em.current_block->add_instruction(
                    new DerefOperation(std::move(left_value), result), expr);
                left_value = Value::from(std::move(result));
            }
            return left_value.member(std::string(rhs_name->text));
        }
    }
    // auto dereference occurs in field access behind reference
    return std::visit(em, expr->toVariant());
}
std::string DefUseGraph::to_graphviz() {
    auto inst_to_string = [](Instruction* inst) {
        auto ret_val =
            std::visit([](auto* inst) { return instruction_to_string(inst); },
                       inst->to_variant());
        if (ret_val.starts_with("%")) ret_val.insert(ret_val.begin(), '\\');
        return ret_val;
    };
    std::ostringstream out;
    out << "digraph DefUseGraph " << " {\n";
    for (const auto& [src, targets] : edges) {
        if (targets.empty()) {
            out << "    \"" << inst_to_string(src) << "\";\n";
        } else {
            for (const auto& dst : targets) {
                out << "    \"" << inst_to_string(src) << "\" -> \""
                    << inst_to_string(dst.first) << "\" [label=\"" << dst.second
                    << "\"]" << ";\n";
            }
        }
    }
    out << "}\n";
    auto out_str = out.str();
    return out_str;
}
std::string TopLevelPointsToGraph::to_graphviz() {
    std::ostringstream out;
    out << "digraph FlowGraph" << " {\n";
    for (const auto& [src, targets] : domain_to_node) {
        if (targets.empty()) {
            out << "    \"" << src << "\";\n";
        } else {
            for (const auto& dst : targets) {
                out << "    \"" << src << "\" -> \"" << dst << "\";\n";
            }
        }
    }
    out << "}\n";
    auto out_str = out.str();
    return out_str;
}
}  // namespace BorrowChecker
}  // namespace Yoyo
namespace Yoyo {
namespace BorrowChecker {
using BlockIteratorTy = DomainVariableInserter::BlockIteratorTy;
void DomainVariableInserter::add_assign_constraints_between_types(
    const BorrowCheckerType& left, const BorrowCheckerType& right) {
    if (left.domains.empty()) return;
    // change this for structural types, but single types can only have
    // one domain
    current_position = instructions.emplace(
        ++current_position,
        new DomainSubsetConstraint(Domain(left.domains[0].first),
                                   Domain(right.domains[0].first)));
}
void DomainVariableInserter::add_extend_constraints_between_types(
    const BorrowCheckerType& left, const BorrowCheckerType& right) {
    if (left.domains.empty()) return;
    current_position = instructions.emplace(
        ++current_position,
        new DomainExtensionConstraint(Domain(left.domains[0].first),
                                      Domain(right.domains[0].first)));
}

void DomainVariableInserter::add_extend_constraints_between_multiple_types(
    const BorrowCheckerType& left, std::ranges::input_range auto const& right) {
    // this is the case for array literals with a bidirectional domain
    std::vector<std::unique_ptr<DomainDependenceEdgeConstraint>>
        edge_constraints;

    for (auto i : std::views::iota(0u, left.domains.size())) {
        const auto& ldom = left.domains[i];

        for (const BorrowCheckerType& type : right) {
            const auto& rdom = type.domains[i];
            // if (ldom.second == true) {
            //     current_position =
            //     instructions.emplace(++current_position, new
            //     DomainExtensionConstraint(Domain(ldom.first),
            //     Domain(rdom.first)));
            //     edge_constraints.emplace_back(new
            //     DomainDependenceEdgeConstraint(Domain(ldom.first),
            //     Domain(rdom.first)));
            // }
            // else
            current_position = instructions.emplace(
                ++current_position,
                new DomainExtensionConstraint(Domain(ldom.first),
                                              Domain(rdom.first)));
        }
    }
    if (!edge_constraints.empty()) {
        current_position = instructions.insert(
            ++current_position,
            std::make_move_iterator(edge_constraints.begin()),
            std::make_move_iterator(edge_constraints.end()));
        current_position += edge_constraints.size() - 1;
    }
}

void DomainVariableInserter::initialize_domains_to_null(
    const BorrowCheckerType& type) {
    for (auto& dom : type.domains) {
        current_position = instructions.emplace(
            ++current_position,
            new DomainSubsetConstraint(Domain(dom.first), Domain("null")));
    }
}
BlockIteratorTy DomainVariableInserter::operator()(CondBrInstruction*) {
    // there's no domain thing going on here
    return current_position;
}
BlockIteratorTy DomainVariableInserter::operator()(BrInstruction*) {
    // nothing here either
    return current_position;
}
BlockIteratorTy DomainVariableInserter::operator()(DropInstruction*) {
    return current_position;  // Lvalue's cannot be dropped, so...
}
BlockIteratorTy DomainVariableInserter::operator()(RetInstruction*) {
    return current_position;
}
BlockIteratorTy DomainVariableInserter::operator()(PhiInstruction* inst) {
    auto result = state->get_value_type(inst->args[0]).cloned(state);
    initialize_domains_to_null(result);
    add_extend_constraints_between_multiple_types(
        result, inst->args | std::views::transform([this](const auto& val) {
                    return std::cref(state->get_value_type(val));
                }));
    state->register_value_base_type(inst->into, std::move(result));
    return current_position;
}
BlockIteratorTy DomainVariableInserter::operator()(AssignInstruction* inst) {
    auto& left_type = state->get_value_type(inst->lhs);
    // lvalue assignments are handled in phase 3
    if (left_type.details.index() == BorrowCheckerType::LValue)
        return current_position;
    auto& right_type = state->get_value_type(inst->rhs);
    add_assign_constraints_between_types(left_type, right_type);
    return current_position;
}
BlockIteratorTy DomainVariableInserter::operator()(
    CallFunctionInstruction* func) {
    // we implement this only for functions that don't have anything to
    // do with references i.e no input or output references

    // check that no inputs have anything to do with references
    for (auto i : std::views::iota(0u, func->val.size())) {
        auto& in = func->val[i];
        if (!state->get_value_type(in).domains.empty()) {
            state->irgen->error(Error(
                func->origin, "References are not supported in function calls",
                std::format("The {}th parameter contains a reference", i + 1)));
        }
    }
    // TODO: check that the return value does not contain a reference
    state->register_value_base_type(
        func->into, state->type_to_borrow_checker_type(func->expected_return));
    return current_position;
}
BlockIteratorTy DomainVariableInserter::operator()(
    RelocateValueInstruction* inst) {
    // no difference between assign tbh
    auto& original = state->get_value_type(inst->val);
    if (inst->val.base_name.starts_with("__tmp")) {
        // we just steal everything in this case, else we do a proper
        // copy
        auto new_tp = original.moved(state);
        state->register_value_base_type(inst->into, std::move(new_tp));
    } else {
        auto new_tp = original.cloned(state);
        add_assign_constraints_between_types(new_tp, original);
        state->register_value_base_type(inst->into, std::move(new_tp));
    }
    return current_position;
}
BlockIteratorTy DomainVariableInserter::operator()(
    NewAggregateInstruction* inst) {
    auto new_tp = BorrowCheckerType::new_aggregate_from(Type(inst->type_name));
    state->register_value_base_type(inst->into, std::move(new_tp));
    return current_position;
}
BlockIteratorTy DomainVariableInserter::operator()(NewArrayInstruction* inst) {
    // TODO handle zero length arrays
    auto& first_elem = state->get_value_type(inst->values[0]);
    auto subtype = first_elem.cloned(state);
    initialize_domains_to_null(subtype);
    add_extend_constraints_between_multiple_types(
        subtype, inst->values | std::views::transform([this](const auto& val) {
                     return std::cref(state->get_value_type(val));
                 }));
    state->register_value_base_type(
        inst->into, BorrowCheckerType::new_array_of(std::move(subtype)));
    return current_position;
}
BlockIteratorTy DomainVariableInserter::operator()(
    NewPrimitiveInstruction* inst) {
    state->register_value_base_type(inst->into,
                                    BorrowCheckerType::new_primitive());
    return current_position;
}
BlockIteratorTy DomainVariableInserter::operator()(
    BorrowValueInstruction* inst) {
    // Introduces a new domain
    auto& og_type = state->get_value_type(inst->val);
    auto new_type = og_type.borrowed(state);
    current_position = instructions.emplace(
        ++current_position,
        new DomainSubsetConstraint(Domain(new_type.domains[0].first),
                                   inst->val.as_domain()));
    state->register_value_base_type(inst->into, std::move(new_type));
    return current_position;
}
BlockIteratorTy DomainVariableInserter::operator()(DerefOperation* op) {
    auto& reference_type = state->get_value_type(op->reference);
    op->ref_domain = reference_type.domains[0].first;
    auto type = reference_type.deref();
    std::get<BorrowCheckerType::LValue>(type.details).origin =
        op->reference.to_string();
    state->register_value_base_type(op->into, std::move(type));
    return current_position;
}
BlockIteratorTy DomainVariableInserter::operator()(DerefLoadOperation* op) {
    // load does not return an lvalue
    auto& reference_type = state->get_value_type(op->reference);
    op->ref_domain = reference_type.domains[0].first;

    auto new_type = std::get<BorrowCheckerType::RefPtr>(reference_type.details)
                        .subtype->cloned(state);
    // we insert a subset to null instruction to indicate a definition
    // of a new domain the actual points-to information will be resolved
    // correctly later
    for (auto& [domain, _] : new_type.domains) {
        auto inst = new DomainSubsetConstraint(Domain(domain), Domain("null"));
        current_position = instructions.emplace(++current_position, inst);
        op->definer.emplace_back(inst);
    }
    state->register_value_base_type(op->into, std::move(new_type));
    return current_position;
}
void add_kill_reason_to_error(Error& err, const KillReason& reason) {
    auto span = SourceSpan{reason.killing_instruction->beg,
                           reason.killing_instruction->end};
    auto detail = std::string{};
    if (reason.source == KillReason::Assign) {
        detail = std::format(
            "Value may point to {}, which was invalidated when {} was "
            "modified",
            reason.bad_pointee, reason.affected_value);
        if (reason.lvalue_source)
            detail += std::format(" ({} may point to {})",
                                  reason.lvalue_source.value(),
                                  reason.affected_value);
    }
    err.markers.emplace_back(span, std::move(detail));
}
void BorrowCheckVisitor::operator()(DerefOperation* inst) {
    if (!state->dfa_in[inst].contains(inst->ref_domain.to_string())) {
        if (!state->domain_kill_reason.contains(inst->ref_domain.to_string()))
            debugbreak();
        const auto& reason =
            state->domain_kill_reason.at(inst->ref_domain.to_string());
        Error err(inst->origin,
                  "Attempt to dereference value that might point to "
                  "invalid memory");
        add_kill_reason_to_error(err, reason);
        irgen->error(err);
    }
}
void BorrowCheckVisitor::operator()(DerefLoadOperation* inst) {
    if (!state->dfa_in[inst].contains(inst->ref_domain.to_string())) {
        irgen->error(Error(inst->origin,
                           "Attempt to dereference value that might "
                           "point to invalid memory"));
    }
}
}  // namespace BorrowChecker
}  // namespace Yoyo
