#include "ir_gen.h"
#include <type_traits>
#include <variant>
#include <ranges>
#include <algorithm>
#include <iostream>
#include "borrow_checker.h"
namespace Yoyo { bool has_type_variable(const Yoyo::Type& tp);}
#define RE_REPR(x) x->evaluated_type = stt->best_repr(x->evaluated_type);\
if(has_type_variable(x->evaluated_type)) {\
    irgen->error(Yoyo::Error(x, "Could not resolve type of this expression"));\
}
namespace Yoyo{ 
    namespace BorrowChecker
    {
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
        InstructionVariant MayStoreOperation::to_variant() { return this; }
        InstructionVariant MayLoadOperation::to_variant() { return this; }
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
        void BorrowCheckerEmitter::operator()(WhileStatement * stat) {
            auto while_cond = function->new_block("while_cond");
            auto while_body = function->new_block("while_body");
            auto while_exit = function->new_block("while_exit");

            current_block->add_instruction(new BrInstruction(while_cond));
            current_block = while_cond;

            current_block->add_instruction(new CondBrInstruction(
                { while_body, while_exit },
                std::visit(*this, stat->condition->toVariant())
            ));

            current_block = while_body;
            std::visit(*this, stat->body->toVariant());
            if (!while_body->is_terminated()) {
                current_block->add_instruction(new BrInstruction(while_cond));
            }

            current_block = while_exit;
        }
        void BorrowCheckerEmitter::operator()(VariableDeclaration * decl) {
            decl->type = stt->best_repr(*decl->type);
            auto variable_name = name_based_on(decl->identifier.text);
            auto value = std::visit(*this, decl->initializer->toVariant());
            if (decl->initializer) current_block->add_instruction(new RelocateValueInstruction(
                std::move(value), std::string(variable_name)));
            // maybe we need an unitiailized value instruction??
            variables.back().emplace_back(std::string(decl->identifier.text), std::move(variable_name));
        }
        void BorrowCheckerEmitter::operator()(ForStatement*) {
            // TODO
        }
        void BorrowCheckerEmitter::operator()(ReturnStatement * stat) {
            current_block->add_instruction(stat->expression ?
                new RetInstruction(std::visit(*this, stat->expression->toVariant())) :
                new RetInstruction);
            // Drop all the valid variables (TODO)
        }
        void BorrowCheckerEmitter::operator()(ExpressionStatement * stat) {
            drop_object(std::visit(*this, stat->expression->toVariant()));
        }
        void BorrowCheckerEmitter::operator()(ConditionalExtraction * stat) {
            // TODO
        }
        void BorrowCheckerEmitter::operator()(WithStatement * stat) {
            // This statement might just be dying
        }
        Value BorrowCheckerEmitter::operator()(IfExpression * expr) {
            RE_REPR(expr);
            auto cond = std::visit(*this, expr->condition->toVariant());

            auto then_block = function->new_block("if_then");

            auto else_block = expr->else_expr ? function->new_block("if_else") : nullptr;
            auto cont_block = function->new_block("if_cont");

            current_block->add_instruction(new CondBrInstruction(
                { then_block, else_block ? else_block : cont_block },
                std::move(cond)));

            current_block = then_block;
            auto then_value = std::visit(*this, expr->then_expr->toVariant());
            if (!then_block->is_terminated()) {
                then_block->add_instruction(new BrInstruction(cont_block));
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
            if (expr->evaluated_type.is_void()) return Value::empty();
            else {
                if (then_value.is_empty()) return else_value;
                if (else_value.is_empty()) return then_value;

                auto name = temporary_name();
                current_block->add_instruction(new PhiInstruction({ then_value, else_value }, std::string(name)));
                return Value::from(std::move(name));
            }
        }
        Value BorrowCheckerEmitter::operator()(BlockExpression * expr) {
            RE_REPR(expr);
            variables.emplace_back();
            for (auto& stt : expr->statements) {
                std::visit(*this, stt->toVariant());
                if (current_block->is_terminated()) {
                    variables.pop_back();
                    return Value::empty();
                }
            }
            Value ret = Value::empty();
            if (expr->expr) ret = std::visit(*this, expr->expr->toVariant());
            for (auto& variable : variables.back() | std::views::reverse) {
                // drop here
            }
            variables.pop_back();
            return ret;
        }
        Value BorrowCheckerEmitter::operator()(IntegerLiteral * lit) {
            RE_REPR(lit);
            auto name = temporary_name();
            current_block->add_instruction(new NewPrimitiveInstruction(std::string(name)));
            return Value::from(std::move(name));
        }
        Value BorrowCheckerEmitter::operator()(BooleanLiteral * lit) {
            RE_REPR(lit);
            auto name = temporary_name();
            current_block->add_instruction(new NewPrimitiveInstruction(std::string(name)));
            return Value::from(std::move(name));
        }
        Value BorrowCheckerEmitter::operator()(TupleLiteral * exp) {
            RE_REPR(exp);
            std::vector<Value> args;
            args.reserve(exp->elements.size());
            std::ranges::transform(exp->elements, std::back_inserter(args), [this](auto& expr)
                {
                    return std::visit(*this, expr->toVariant());
                });

            auto name = temporary_name();
            current_block->add_instruction(new CallFunctionInstruction("__builtin_make_tuple", std::string(name), std::move(args)));
            return Value::from(std::move(name));
        }
        Value BorrowCheckerEmitter::operator()(ArrayLiteral * lit) {
            RE_REPR(lit);
            auto array_obj = temporary_name();
            // [<expr>, <expr>, ... ]
            if (auto elems = std::get_if<std::vector<std::unique_ptr<Expression>>>(&lit->elements); elems) {
                std::vector<Value> values;
                values.reserve(elems->size());
                std::ranges::transform(*elems, std::back_inserter(values), [this](auto& expr)
                    {
                        return std::visit(*this, expr->toVariant());
                    });
                current_block->add_instruction(new NewArrayInstruction(std::move(values), std::string(array_obj)));
            }
            // [<expr>; <expr>] repeat first <expr>, sencond <expr> times (second expr is a constant)
            else {
                auto& elem_size = std::get<std::pair<std::unique_ptr<Expression>, std::unique_ptr<Expression>>>(lit->elements);
                // TODO
            }
            return Value::from(std::move(array_obj));
        }
        Value BorrowCheckerEmitter::operator()(RealLiteral * lit) {
            RE_REPR(lit);
            auto name = temporary_name();
            current_block->add_instruction(new NewPrimitiveInstruction(std::string(name)));
            return Value::from(std::move(name));
        }
        Value BorrowCheckerEmitter::operator()(StringLiteral * lit) {
            RE_REPR(lit);
            for (auto& entry : lit->literal) {
                if (std::holds_alternative<std::unique_ptr<Expression>>(entry)) {
                    auto& capture = std::get<std::unique_ptr<Expression>>(entry);

                    auto arg = temporary_name();
                    current_block->add_instruction(new BorrowValueInstruction(
                        std::visit(*this, capture->toVariant()),
                        std::string(arg)
                    ));

                    current_block->add_instruction(new CallFunctionInstruction(
                        "__builtin_to_string_for_" + capture->evaluated_type.full_name(),
                        // drop the string or not, it doesn't really matter string is fully owning
                        temporary_name(),
                        { Value::from(std::move(arg)) }
                    ));

                }
            }
            return Value::from(temporary_name());
        }
        Value BorrowCheckerEmitter::operator()(NameExpression * name) {
            RE_REPR(name);
            if (name->evaluated_type.name == "__fn") return Value::function(std::string(name->evaluated_type.block_hash));
            for (auto& block : variables | std::views::reverse) {
                for (auto& [var_name, id] : block) {
                    if (var_name == name->text) return Value::from(std::string(id));
                }
            }
            return Value::constant();
        }
        Value BorrowCheckerEmitter::operator()(GenericNameExpression * name) {
            RE_REPR(name);
            return Value::constant();
        }
        Value BorrowCheckerEmitter::operator()(PrefixOperation * pfx) {
            RE_REPR(pfx);
            auto this_eval = std::visit(*this, pfx->operand->toVariant());
            switch (pfx->op.type) {
                // BIG TODO regarding moving from behind references and all that
            case TokenType::Star: {
                auto result = temporary_name();
                current_block->add_instruction(new DerefOperation(std::move(this_eval), std::string(result)));
                return Value::from(std::move(result));
            }
            case TokenType::Ampersand: [[fallthrough]];
            case TokenType::RefMut: {
                auto result = temporary_name();
                current_block->add_instruction(new BorrowValueInstruction(std::move(this_eval), std::string(result)));
                return Value::from(std::move(result));
            }
            }
        }
        Value BorrowCheckerEmitter::operator()(BinaryOperation * op) {
            RE_REPR(op);
            for (auto& sub : op->subtypes) {
                sub = stt->best_repr(sub);
                if (has_type_variable(sub)) {
                    irgen->error(Error(op, "Could not resolve all generics for this operation"));
                }
            }
            auto token_tp = op->op.type;
            switch (token_tp) {
                using enum TokenType;
            case GreaterEqual: [[fallthrough]];
            case LessEqual: [[fallthrough]];
            case BangEqual: [[fallthrough]];
            case DoubleEqual: [[fallthrough]];
            case Spaceship: [[fallthrough]];
            case Greater: token_tp = TokenType::Spaceship; break;
            default: break;
            }
            // extend to all assignments
            if (token_tp == TokenType::Equal) {
                // we need to evaluate the rhs first
                auto rhs = std::visit(*this, op->rhs->toVariant());
                auto lhs = std::visit(*this, op->lhs->toVariant());
                current_block->add_instruction(new AssignInstruction(
                    std::move(lhs),
                    std::move(rhs)));
                return Value::empty();
            }
            else if (token_tp != TokenType::Dot && token_tp != TokenType::Equal) {
                auto result = temporary_name();
                current_block->add_instruction(new CallFunctionInstruction(
                    op->selected->mangled_name(token_tp),
                    std::string(result),
                    { std::visit(*this, op->lhs->toVariant()), std::visit(*this, op->rhs->toVariant()) }
                ));
                return Value::from(std::move(result));
            }
            else {
                // TODO
                // could be member access, function binding or assignment
                return Value::empty();
            }
        }
        Value BorrowCheckerEmitter::operator()(GroupingExpression * grp) {
            RE_REPR(grp);
            return std::visit(*this, grp->expr->toVariant());
        }
        Value BorrowCheckerEmitter::operator()(LogicalOperation * lg) {
            RE_REPR(lg);
            // We could do a branch here to represent short circuiting
            // but I don't feel its necessary

            // these two must return bools so there's no need to drop them
            std::visit(*this, lg->lhs->toVariant());
            std::visit(*this, lg->rhs->toVariant());

            std::string result = temporary_name();
            current_block->add_instruction(new NewPrimitiveInstruction(std::string(result)));
            return Value::from(std::move(result));
        }
        Value BorrowCheckerEmitter::operator()(PostfixOperation*) { return Value::empty(); }
        Value BorrowCheckerEmitter::operator()(CallOperation * op) {
            RE_REPR(op);
            auto callee_val = std::visit(*this, op->callee->toVariant());
            auto& callee_type = op->callee->evaluated_type;
            if (callee_type.name == "__bound_fn") {
                // TODO
                return Value::empty();
            }
            auto result = temporary_name();
            std::vector<Value> args;
            args.reserve(op->arguments.size());
            std::ranges::transform(op->arguments, std::back_inserter(args), [this](auto& arg)
                {
                    return std::visit(*this, arg->toVariant());
                });
            current_block->add_instruction(new CallFunctionInstruction(
                callee_val.function_name().value(),
                std::string(result),
                std::move(args)
            ));
            return Value::from(std::move(result));
        }
        Value BorrowCheckerEmitter::operator()(SubscriptOperation * op) {
            RE_REPR(op);
            auto object = std::visit(*this, op->object->toVariant());
            auto index = std::visit(*this, op->index->toVariant());
            auto& idx_type = op->index->evaluated_type;
            if (
                op->object->evaluated_type.deref().is_static_array() &&
                idx_type.is_unsigned_integral() &&
                idx_type.integer_width() == 64
                ) {
                // array element access (member access is not fully resolved wrt to refernces)
                return std::move(object).member("[*]");
            }


        }
        Value BorrowCheckerEmitter::operator()(LambdaExpression*) { return Value::empty(); }
        Value BorrowCheckerEmitter::operator()(TryExpression*) { return Value::empty(); }
        Value BorrowCheckerEmitter::operator()(ScopeOperation * scp) {
            RE_REPR(scp);
            if (scp->evaluated_type.name == "__fn") {
                return Value::function(std::string(scp->evaluated_type.block_hash));
            }
            // either function, enum or constant (TODO)
            return Value::constant();
        }
        Value BorrowCheckerEmitter::operator()(ObjectLiteral * lit) {
            RE_REPR(lit);
            // TODO aggregate types
            return Value::empty();
        }
        Value BorrowCheckerEmitter::operator()(NullLiteral * lit) {
            RE_REPR(lit);
            return Value::empty();
        }
        Value BorrowCheckerEmitter::operator()(AsExpression * ss) {
            RE_REPR(ss);
            // TODO
            return Value::empty();
        }
        Value BorrowCheckerEmitter::operator()(CharLiteral * lit) {
            RE_REPR(lit);
            auto name = temporary_name();
            current_block->add_instruction(new NewPrimitiveInstruction(std::string(name)));
            return Value::from(std::move(name));
        }
        Value BorrowCheckerEmitter::operator()(GCNewExpression * gcn) {
            RE_REPR(gcn);
            // TODO
            return Value::empty();
        }
        Value BorrowCheckerEmitter::operator()(MacroInvocation * ivc) {
            RE_REPR(ivc);
            return std::visit(*this, ivc->result->toVariant());
        }
        Value BorrowCheckerEmitter::operator()(SpawnExpression * exr) {
            RE_REPR(exr);
            // The fiber situation is crazyy
            // fibers can't borrow from other fibers
            return Value::from("__literal");
        }


        std::string BasicBlock::to_string()
        {
            auto instruction_to_string = []<typename T>(T * inst) -> std::string
            {
                if constexpr (std::is_same_v<T, CondBrInstruction>) {
                    auto first_name = inst->options[0]->debug_name;
                    for (auto block : std::ranges::subrange(std::next(inst->options.begin()), inst->options.end()))
                        first_name += ", " + block->debug_name;
                    return std::format("cond br {} [{}]", inst->br_on.to_string(), first_name);
                }
                if constexpr (std::is_same_v<T, BrInstruction>) {
                    return std::format("br {}", inst->next->debug_name);
                }
                if constexpr (std::is_same_v<T, RetInstruction>) {
                    return std::format("ret {}", inst->ret_val ? inst->ret_val->to_string() : "void");
                }
                if constexpr (std::is_same_v<T, PhiInstruction>) {
                    std::string input = inst->args[0].to_string();
                    for (auto& arg : std::ranges::subrange(std::next(inst->args.begin()), inst->args.end()))
                        input += ", " + arg.to_string();
                    return std::format("%{} = phi({})", inst->into, input);
                }
                if constexpr (std::is_same_v<T, AssignInstruction>) {
                    return std::format("{} = {}", inst->lhs.to_string(), inst->rhs.to_string());
                }
                if constexpr (std::is_same_v<T, CallFunctionInstruction>) {
                    std::string input;
                    if (!inst->val.empty()) {
                        input = inst->val[0].to_string();
                        for (auto& arg : std::ranges::subrange(std::next(inst->val.begin()), inst->val.end()))
                            input += ", " + arg.to_string();
                    }
                    return std::format("%{} = call {}({})", inst->into, inst->function_name, input);
                }
                if constexpr (std::is_same_v<T, RelocateValueInstruction>) {
                    return std::format("%{} = {}", inst->into, inst->val.to_string());
                }
                if constexpr (std::is_same_v<T, NewArrayInstruction>) {
                    std::string input = inst->values[0].to_string();
                    for (auto& arg : std::ranges::subrange(std::next(inst->values.begin()), inst->values.end()))
                        input += ", " + arg.to_string();
                    return std::format("%{} = [{}]", inst->into, input);
                }
                if constexpr (std::is_same_v<T, NewPrimitiveInstruction>) {
                    return std::format("%{} = primitive", inst->into);
                }
                if constexpr (std::is_same_v<T, BorrowValueInstruction>) {
                    return std::format("%{} = borrow {}", inst->into, inst->val.to_string());
                }
                if constexpr (std::is_same_v<T, DomainSubsetConstraint>) {
                    return std::format("{} ⊇ {}", inst->super.to_string(), inst->sub.to_string());
                }
                if constexpr (std::is_same_v<T, DomainExtensionConstraint>) {
                    return std::format("{} ⊇ {} ∪ {}", inst->super.to_string(), 
                        inst->old_super.to_string().empty() ?
                            inst->super.to_string() :
                            inst->old_super.to_string(), inst->sub.to_string());
                }
                if constexpr (std::is_same_v<T, DomainDependenceEdgeConstraint>) {
                    return std::format("{} => {}", inst->d1.to_string(), inst->d2.to_string());
                }
                if constexpr (std::is_same_v<T, DomainPhiInstruction>) {
                    std::string input;
                    if (!inst->args.empty()) {
                        input = inst->args[0].to_string();
                        for (auto& arg : std::ranges::subrange(std::next(inst->args.begin()), inst->args.end()))
                            input += ", " + arg.to_string();
                    }
                    return std::format("{} = domain φ({})", inst->into, input);
                }
                if constexpr (std::is_same_v<T, DerefOperation>) {
                    return std::format("%{} = deref {}", inst->into, inst->reference.to_string());
                }
                if constexpr (std::is_same_v<T, MayStoreOperation>) {
                    return std::format("{} = χ({})", inst->new_domain.to_string(), inst->old_domain.to_string());
                }
                return "not implemented";
            };
            std::string instructions_string;
            for (auto& inst : instructions) {
                instructions_string += std::visit(instruction_to_string, inst->to_variant()) + '\n';
            }
            instructions_string.pop_back(); // remove the trailing \n

            std::string preds_string;
            if (!preds.empty()) {
                preds_string = "preds(" + preds[0]->debug_name;
                for (auto pred : std::ranges::subrange(std::next(preds.begin()), preds.end()))
                    preds_string += ", " + pred->debug_name;
                preds_string += ")";
            }
            return std::format("{} {}:\n{}", debug_name, preds_string, instructions_string);
        }
        Domain DomainCheckerState::new_domain_var()
        {
            return Domain{ .name = "'?" + std::to_string(last_id++) };
        }
        void DomainCheckerState::register_value_base_type(const std::string& value, BorrowCheckerType&& type)
        {
            type_mapping.emplace(value, std::move(type));
        }
        const BorrowCheckerType& DomainCheckerState::get_value_type(const Value& value)
        {
            if (!value.subpaths.empty()) debugbreak();
            if (!type_mapping.contains(value.base_name)) debugbreak();
            return type_mapping.at(value.base_name);
        }
        std::unique_ptr<BorrowCheckerFunction> DomainCheckerState::check_function(
            FunctionDeclaration* decl, IRGenerator* irgen, const FunctionSignature& sig, TypeCheckerState* stt)
        {
            auto function = std::make_unique<BorrowCheckerFunction>();
            func = &*function;
            entry_block = function->new_block("entry");
            std::visit(BorrowCheckerEmitter{ irgen, stt, &*function, entry_block }, decl->body->toVariant());
            std::cout << "Phase 1\n" << function->to_string() << std::endl;
            for (auto& block : function->blocks) {
                for (auto it = block->instructions.begin(); it != block->instructions.end(); ++it) {
                    it = std::visit(DomainVariableInserter{ this, block->instructions, it }, (*it)->to_variant());
                }
            }
            std::cout << "Phase 2\n" << type_mapping.to_string() << "\n\n" <<  function->to_string() << std::endl;
            using namespace std::ranges;
            using namespace std::views;
            
            bool has_change = true;
            while (has_change) {
                has_change = false;
                for (auto& block: function->blocks) {
                    for (auto& inst : block->instructions)
                        has_change = std::visit(InclusionPointerAnalyser{ ptgraph, this }, inst->to_variant()) || has_change;
                }
            }
            std::cout << ptgraph.to_graphviz() << std::endl;
            
            calc_block_preds();
            transform_to_ssa();
            std::cout << "Phase 3\n" << type_mapping.to_string() << "\n\n" << function->to_string() << std::endl;
            clear_dependencies();
            return function;
        }
        void DomainCheckerState::build_dominators()
        {
            // Incomplete on purpose
            dominators.register_for(entry_block, entry_block);
            bool should_loop = true;
            // the order should not matter
            auto reverse_post_order = [](BasicBlock* block) 
                {

                    for (auto child : block->instructions.back()->children()) {
                        
                    }
                };
            while (should_loop) {
                should_loop = false;

            }
        }
        void DomainCheckerState::calc_block_preds()
        {
            for (auto& block : func->blocks) {
                for (auto child : block->instructions.back()->children())
                    child->preds.push_back(block.get());
            }
        }
        std::string ValueProducer::name_for(const std::string& val) {
            if (!var_map.contains(val)) 
                var_map[val] = 0;
            return std::format("{}__{}", val, ++var_map[val]);
        }
        void ssa_transform_do_block(BasicBlock* block, UseStorageTy& storage, ValueProducer& producer, DomainCheckerState*);
        std::optional<std::string> lookup_var_recursive(const std::string& arg, BasicBlock* start_at, UseStorageTy& storage, ValueProducer& producer, DomainCheckerState* state) {
            if (start_at->preds.size() == 1) {
                auto pred = start_at->preds[0];
                if (!storage.contains(pred)) ssa_transform_do_block(pred, storage, producer, state);

                if (storage[pred].contains(arg)) return storage[pred][arg];
                else return lookup_var_recursive(arg, pred, storage, producer, state);
            }
            else {
                auto new_name = producer.name_for(arg);
                auto phi = std::unique_ptr<DomainPhiInstruction>(new DomainPhiInstruction({}, std::string(new_name)));
                // if the assignment is downward exposed we add it to our latest instances
                if (!storage[start_at].contains(arg)) storage[start_at][arg] = new_name;
                

                for (auto& pred : start_at->preds) {
                    if (!storage.contains(pred)) ssa_transform_do_block(pred, storage, producer, state);

                    if (storage[pred].contains(arg)) phi->args.push_back(Domain(storage[pred][arg]));
                    else {
                        if (auto val = lookup_var_recursive(arg, pred, storage, producer, state)) phi->args.push_back(Domain(*val));
                        else return std::nullopt;
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
                        start_at->instructions.emplace(start_at->instructions.begin(), std::move(phi));
                }
                return storage[start_at][arg];
            }
        }
        void ssa_transform_do_block(BasicBlock* block, UseStorageTy& storage, ValueProducer& producer, DomainCheckerState* state) {
            if (storage.contains(block)) return;
            auto& storage_entry = storage[block];
            // perform local value numbering for everything in this block
            // and collect un-numbered uses into this vector
            std::vector<std::reference_wrapper<std::string>> unfound_uses;
            std::vector<std::pair<AssignInstruction*, size_t>> deferred_assignments;
            for (auto it = block->instructions.begin(); it != block->instructions.end(); ++it) {
                std::visit(
                    [&]<typename T>(T* tp)
                    {
                        if constexpr (std::is_same_v<T, DomainExtensionConstraint>) {
                            if (!tp->super.is_var()) debugbreak();

                            if (!storage_entry.contains(tp->super.name))
                            {
                                tp->old_super.name = tp->super.name;
                                unfound_uses.push_back(std::ref(tp->old_super.name));
                            }
                            else tp->old_super.name = storage_entry[tp->super.name];

                            if (tp->sub.is_var())
                                if (storage_entry.contains(tp->sub.name)) tp->sub.name = storage_entry[tp->sub.name];
                                else unfound_uses.push_back(std::ref(tp->sub.name));

                            auto new_name = producer.name_for(tp->super.name);
                            storage_entry[tp->super.name] = new_name;
                            tp->super.name = new_name;

                            //state->pgraph.add_edge(new_name, Domain(tp->sub));
                            //state->pgraph.add_edge(new_name, Domain(tp->old_super));
                        }
                        if constexpr (std::is_same_v<T, DomainSubsetConstraint>) {
                            if (!tp->super.is_var()) debugbreak();
                            auto new_name = producer.name_for(tp->super.name);
                            storage_entry[tp->super.name] = new_name;

                            //if (tp->sub.is_null()) state->pgraph.initialize(new_name);
                            //else state->pgraph.add_edge(new_name, Domain(tp->sub));

                            tp->super.name = new_name;

                            if (tp->sub.is_var())
                                if (storage_entry.contains(tp->sub.name)) tp->sub.name = storage_entry[tp->sub.name];
                                else unfound_uses.push_back(std::ref(tp->sub.name));
                        }
                        if constexpr (std::is_same_v<T, DomainDependenceEdgeConstraint>) {
                            if (tp->d1.is_var())
                                if (storage_entry.contains(tp->d1.name)) tp->d1.name = storage_entry[tp->d1.name];
                                else unfound_uses.push_back(std::ref(tp->d1.name));
                        }
                        if constexpr (std::is_same_v<T, AssignInstruction>) {
                            AssignInstruction* inst = tp;
                            auto& type = state->get_value_type(inst->lhs);
                            auto as_lval = std::get_if<BorrowCheckerType::LValue>(&type.details);
                            if (!as_lval) return;

                            // if its a pointer to pointer we need to add may store instruction(s)

                            if (!as_lval->subtype->domains.empty()) {
                                for (auto pointee : state->ptgraph.get_pointees_of(type.domains[0].first.to_string())) {
                                    auto& type = state->get_value_type(Value::from(std::move(pointee)));
                                    auto new_name = producer.name_for(type.domains[0].first.name);
                                    auto op = new MayStoreOperation(tp, Domain(), Domain(new_name));
                                    
                                    if (storage_entry.contains(type.domains[0].first.name))
                                        op->old_domain.name = storage_entry.at(type.domains[0].first.name);
                                    else
                                        unfound_uses.push_back(std::ref(op->old_domain.name));

                                    storage_entry[type.domains[0].first.name] = new_name;
                                    
                                    it = block->instructions.emplace(++it, op);
                                }
                            }
                            
                        }
                    }, 
                    (*it)->to_variant());
            }
            // perform global value numbering
            if (block->preds.size() == 1) {
                std::erase_if(unfound_uses, [&, pred = block->preds[0]](std::reference_wrapper<std::string> arg)
                    {
                        if (!storage.contains(pred)) ssa_transform_do_block(pred, storage, producer, state);

                        if (storage[pred].contains(arg)) arg.get() = storage[pred][arg];
                        else {
                            if (auto val = lookup_var_recursive(arg, pred, storage, producer, state)) arg.get() = *val;
                            else return false;
                        }
                        return true;
                    });
                
            }
            else {
                std::erase_if(unfound_uses, [&](std::reference_wrapper<std::string> arg)
                    {
                        auto new_name = producer.name_for(arg);
                        auto phi = std::unique_ptr<DomainPhiInstruction>(new DomainPhiInstruction({}, std::string(new_name)));
                        // if the assignment is downward exposed we add it to our latest instances
                        if (!storage_entry.contains(arg)) storage_entry[arg] = new_name;

                        for (auto& pred : block->preds) {
                            if (!storage.contains(pred)) ssa_transform_do_block(pred, storage, producer, state);

                            if (storage[pred].contains(arg)) phi->args.push_back(Domain(storage[pred][arg]));
                            else {
                                if (auto val = lookup_var_recursive(arg, pred, storage, producer, state)) phi->args.push_back(Domain(*val));
                                else return false;
                            }
                        }
                        if (phi->args.empty()) {
                            storage_entry[arg] = "null";
                        }
                        else {
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
                                block->instructions.emplace(block->instructions.begin(), std::move(phi));
                        }
                        
                        arg.get() = storage_entry[arg];
                        return true;
                    });
            }
            if (!unfound_uses.empty()) debugbreak();
        }
        void DomainCheckerState::transform_to_ssa()
        {
            ValueProducer producer;
            UseStorageTy latest_variables;
            // this is not post order, its just kinda close to it
            for (auto& block : func->blocks) {
                ssa_transform_do_block(block.get(), latest_variables, producer, this);
            }
        }
        void DomainCheckerState::clear_dependencies() {
            std::vector<std::pair<std::string, std::string>> dependence_edges;
            auto add_dependencies_in_block = [&dependence_edges](BasicBlock* blk)
            {
                for(auto& inst : blk->instructions) {
                    std::visit([&dependence_edges]<typename T>(T* inst)
                        {
                            if constexpr (std::is_same_v<T, DomainDependenceEdgeConstraint>)
                                dependence_edges.emplace_back(inst->d1.name, inst->d2.name);
                        }, inst->to_variant());
                }
            };
            for(auto& block : func->blocks) {
                add_dependencies_in_block(block.get());
            }
            // write this to graphviz for visualization
            std::string gvz_content;
            for(const auto&[from, to] : dependence_edges) {
                gvz_content += std::format("    \"{}\" -> \"{}\";\n", from, to);
            }
            auto final_graph = std::format("digraph G {{\n{}}}", gvz_content);
            std::cout << final_graph << std::endl;
        }
        static auto clone_type(const BorrowCheckerType& type) -> BorrowCheckerType
        {
            using enum BorrowCheckerType::TypeType;
            switch (type.details.index()) {
            case Primitive: return BorrowCheckerType::new_primitive();
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
                ntype.details.emplace<UniquePtr>(std::make_unique<BorrowCheckerType>(clone_type(
                    *std::get<UniquePtr>(type.details).subtype
                )));
                return ntype;
            }
            case Array: {
                auto ntype = BorrowCheckerType();
                ntype.domains = type.domains;
                ntype.details.emplace<Array>(std::make_unique<BorrowCheckerType>(clone_type(
                    *std::get<Array>(type.details).subtype
                )));
                return ntype;
            }
            case RefPtr: {
                auto ntype = BorrowCheckerType();
                ntype.domains = type.domains;
                ntype.details.emplace<RefPtr>(std::make_unique<BorrowCheckerType>(clone_type(
                    *std::get<RefPtr>(type.details).subtype
                )));
                return ntype;
            }
            default: debugbreak();
            }
        };
        BorrowCheckerType BorrowCheckerType::borrowed(DomainCheckerState* stt) const
        {
            BorrowCheckerType new_type;
            new_type.domains.emplace_back(stt->new_domain_var(), false);
            // all child domains become bidirectional
            auto inner = cloned(stt);
            for (auto& [domain, _] : inner.domains) new_type.domains.emplace_back(domain, true);
            new_type.details.emplace<RefPtr>(std::make_unique<BorrowCheckerType>(
                std::move(inner)
            ));
            return new_type;
        }
        BorrowCheckerType BorrowCheckerType::cloned(DomainCheckerState* stt) const
        {
            std::unordered_map<std::string, Domain> old_to_new_map;
            
            auto new_type = moved(stt);
            for (auto& [domain, is_bidr] : domains) {
                
                    old_to_new_map[domain.to_string()] = stt->new_domain_var();
                
            }
            
            auto substitute_domains = [&old_to_new_map](BorrowCheckerType& type, const auto& self) -> void
                {
                    for (auto& [domain, is_bidr] : type.domains) {
                        domain = old_to_new_map[domain.to_string()];
                    }
                    switch (type.details.index()) {
                    case Primitive: break;
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
                    default: debugbreak();
                    }
                };
            
            

            substitute_domains(new_type, substitute_domains);
            return new_type;
        }
        BorrowCheckerType BorrowCheckerType::moved(DomainCheckerState*) const
        {
            BorrowCheckerType new_type;
            new_type.domains = domains;
            if (auto aggrg = std::get_if<Aggregate>(&details); aggrg) {
                decltype(aggrg->fields) new_fields;
                for (auto& [fname, ftype] : aggrg->fields) new_fields.emplace(fname, clone_type(ftype));
                new_type.details.emplace<Aggregate>(std::move(new_fields));
            }
            else if (auto unn = std::get_if<Union>(&details); unn) {
                decltype(unn->fields) new_fields;
                for (auto& [fname, ftype] : unn->fields) new_fields.emplace(fname, clone_type(ftype));
                new_type.details.emplace<Union>(std::move(new_fields));
            }
            else if (auto arr = std::get_if<Array>(&details); arr) {
                new_type.details.emplace<Array>(std::make_unique<BorrowCheckerType>(clone_type(*arr->subtype)));
            }
            else if (auto unq = std::get_if<UniquePtr>(&details); unq) {
                new_type.details.emplace<UniquePtr>(std::make_unique<BorrowCheckerType>(clone_type(*unq->subtype)));
            }
            else if (auto ref = std::get_if<RefPtr>(&details); ref) {
                new_type.details.emplace<RefPtr>(std::make_unique<BorrowCheckerType>(clone_type(*ref->subtype)));
            }
            else if (auto prim = std::get_if<Primitive>(&details); prim) {
                new_type.details.emplace<Primitive>();
            }
            else { debugbreak(); }
            return new_type;
        }
        BorrowCheckerType BorrowCheckerType::deref() const
        {
            // deref can be called on RefPtr<T> or LValue<RefPtr<T>> to yield LValue<T>
            auto ref_ptr_type = this;
            if (auto as_lval = std::get_if<LValue>(&details); as_lval) {
                ref_ptr_type = as_lval->subtype.get();
            }
            auto& ref_ptr = std::get<RefPtr>(ref_ptr_type->details);
            auto new_type = BorrowCheckerType{};
            new_type.domains = ref_ptr_type->domains;
            new_type.details.emplace<LValue>(std::make_unique<BorrowCheckerType>(clone_type(*ref_ptr.subtype)));
            return new_type;
        }
        BorrowCheckerType BorrowCheckerType::new_primitive()
        {
            BorrowCheckerType new_type;
            new_type.details.emplace<Primitive>();
            return new_type;
        }
        BorrowCheckerType BorrowCheckerType::new_array_of(BorrowCheckerType&& tp)
        {
            BorrowCheckerType new_type;
            // domains retain thier bidirectionality
            for (auto& [domain, bidr] : tp.domains) new_type.domains.emplace_back(domain, bidr);
            new_type.details.emplace<Array>(std::make_unique<BorrowCheckerType>(
                clone_type(tp)
            ));
            return new_type;
        }
        std::string BorrowCheckerType::to_string() const
        {
            std::string result;
            switch (details.index()) {
            case Primitive: return "primitive";
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
                result = std::format("&{} {}", domains[0].first.to_string(), dets.subtype->to_string());
                break;
            }
            case LValue: {
                auto& dets = std::get<LValue>(details);
                result = std::format("lvalue<({}), {}>", domains[0].first.to_string(), dets.subtype->to_string());
                break;
            }
            default: debugbreak();
            }
            return result;
        }
        std::string ValueTypeMapping::to_string()
        {
            std::string result = "Value - Type mapping:\n";
            for (auto& [value, type] : *this) {
                result += "%" + value + " :: " + type.to_string() + "\n";
            }
            return result;
        }
        bool InclusionPointerAnalyser::operator()(DomainSubsetConstraint* con) {
            // p > q
            if (con->sub.is_var()) {
                bool has_change = false;
                for (auto& pointee : ptg.get_pointees_of(con->sub.to_string())) {
                    has_change = ptg.add_edge(con->super.to_string(), pointee) || has_change;
                }
                return has_change;
            }
            // p > {q}
            else if(!con->sub.is_null()) {
                return ptg.add_edge(con->super.to_string(), con->sub.to_string());
            }
            return false;
        }
        bool InclusionPointerAnalyser::operator()(DomainExtensionConstraint* con)
        {
            // p > p + q
            if (con->sub.is_var()) {
                bool has_change = false;
                for (auto& pointee : ptg.get_pointees_of(con->sub.to_string())) {
                    has_change = ptg.add_edge(con->super.to_string(), pointee) || has_change;
                }
                return has_change;
            }
            // p > p + {q}
            else if (!con->sub.is_null()) {
                return ptg.add_edge(con->super.to_string(), con->sub.to_string());
            }
            return false;
        }
    }
}
namespace Yoyo {
    namespace BorrowChecker {
        using BlockIteratorTy = DomainVariableInserter::BlockIteratorTy;
        void DomainVariableInserter::add_assign_constraints_between_types(const BorrowCheckerType& left, const BorrowCheckerType& right)
        {
            if (left.domains.empty()) return;
            // change this for structural types, but single types can only have one domain
            current_position = instructions.emplace(++current_position, new DomainSubsetConstraint(Domain(left.domains[0].first), Domain(right.domains[0].first)));
            
        }
        void DomainVariableInserter::add_extend_constraints_between_types(const BorrowCheckerType& left, const BorrowCheckerType& right)
        {
            if (left.domains.empty()) return;
            current_position = instructions.emplace(++current_position, new DomainExtensionConstraint(Domain(left.domains[0].first), Domain(right.domains[0].first)));
        }

        void DomainVariableInserter::add_extend_constraints_between_multiple_types(const BorrowCheckerType& left, std::ranges::input_range auto const& right)
        {
            // this is the case for array literals with a bidirectional domain
            std::vector<std::unique_ptr<DomainDependenceEdgeConstraint>> edge_constraints;
            
            for (auto i : std::views::iota(0u, left.domains.size())) {
                const auto& ldom = left.domains[i];

                for(const BorrowCheckerType& type : right)
                {
                    const auto& rdom = type.domains[i];
                    //if (ldom.second == true) {
                    //    current_position = instructions.emplace(++current_position, new DomainExtensionConstraint(Domain(ldom.first), Domain(rdom.first)));
                    //    edge_constraints.emplace_back(new DomainDependenceEdgeConstraint(Domain(ldom.first), Domain(rdom.first)));
                    //}
                    //else
                        current_position = instructions.emplace(++current_position, new DomainExtensionConstraint(Domain(ldom.first), Domain(rdom.first)));
                }
            }
            if(!edge_constraints.empty())
            {
                current_position = instructions.insert(++current_position,
                    std::make_move_iterator(edge_constraints.begin()),
                    std::make_move_iterator(edge_constraints.end())
                );
                current_position += edge_constraints.size() - 1;
            }
        }
        
        void DomainVariableInserter::initialize_domains_to_null(const BorrowCheckerType& type) {
            for (auto& dom : type.domains) {
                
                current_position = instructions.emplace(++current_position, new DomainSubsetConstraint(Domain(dom.first), Domain("null")));
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
        BlockIteratorTy DomainVariableInserter::operator()(RetInstruction*) { return current_position; }
        BlockIteratorTy DomainVariableInserter::operator()(PhiInstruction* inst) {
            auto result = state->get_value_type(inst->args[0]).cloned(state);
            initialize_domains_to_null(result);
            add_extend_constraints_between_multiple_types(result, inst->args | std::views::transform([this](const auto& val)
                {
                    return std::cref(state->get_value_type(val));
                }));
            state->register_value_base_type(inst->into, std::move(result));
            return current_position;
        }
        BlockIteratorTy DomainVariableInserter::operator()(AssignInstruction* inst) {
            auto& left_type = state->get_value_type(inst->lhs);
            // lvalue assignments are handled in phase 3
            if (left_type.details.index() == BorrowCheckerType::LValue) return current_position;
            auto& right_type = state->get_value_type(inst->rhs);
            add_assign_constraints_between_types(left_type, right_type);
            return current_position;
        }
        BlockIteratorTy DomainVariableInserter::operator()(CallFunctionInstruction*) {
            // literally magic
            return current_position;
        }
        BlockIteratorTy DomainVariableInserter::operator()(RelocateValueInstruction* inst) {
            // no difference between assign tbh
            auto& original = state->get_value_type(inst->val);
            auto new_tp = original.moved(state);
            state->register_value_base_type(inst->into, std::move(new_tp));
            return current_position;
        }
        BlockIteratorTy DomainVariableInserter::operator()(NewArrayInstruction* inst) {
            // TODO handle zero length arrays
            auto& first_elem = state->get_value_type(inst->values[0]);
            auto subtype = first_elem.cloned(state);
            initialize_domains_to_null(subtype);
            add_extend_constraints_between_multiple_types(subtype, inst->values | std::views::transform([this](const auto& val)
                {
                    return std::cref(state->get_value_type(val));
                }));
            state->register_value_base_type(inst->into, BorrowCheckerType::new_array_of(std::move(subtype)));
            return current_position;
        }
        BlockIteratorTy DomainVariableInserter::operator()(NewPrimitiveInstruction* inst) {
            state->register_value_base_type(inst->into, BorrowCheckerType::new_primitive());
            return current_position;
        }
        BlockIteratorTy DomainVariableInserter::operator()(BorrowValueInstruction* inst) {
            // Introduces a new domain
            auto& og_type = state->get_value_type(inst->val);
            auto new_type = og_type.borrowed(state);
            current_position = instructions.emplace(++current_position, new DomainSubsetConstraint(Domain(new_type.domains[0].first), inst->val.as_domain()));
            state->register_value_base_type(inst->into, std::move(new_type));
            return current_position;
        }
        BlockIteratorTy DomainVariableInserter::operator()(DerefOperation* op)
        {
            state->register_value_base_type(op->into, state->get_value_type(op->reference).deref());
            return current_position;
        }
    }
}
