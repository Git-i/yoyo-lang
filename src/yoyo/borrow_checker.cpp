#include "ir_gen.h"
#include <variant>
#include <ranges>
#include <algorithm>
#include <numeric>
#include <iostream>
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
        InstructionVariant DomainEqualityConstraint::to_variant() { return this; }
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
            if (decl->initializer) current_block->add_instruction(new RelocateValueInstruction(
                std::visit(*this, decl->initializer->toVariant()), std::string(variable_name)));
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
            auto cont_block = function->new_block("if_cont");

            auto else_block = expr->else_expr ? function->new_block("if_else") : nullptr;

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
            case TokenType::Star: return this_eval; // borrows are checked at creation time
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
                current_block->add_instruction(new AssignInstruction(
                    std::visit(*this, op->lhs->toVariant()),
                    std::visit(*this, op->rhs->toVariant())));
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
                    return std::format("{} ⊆ {}", inst->sub.to_string(), inst->super.to_string());
                }
                if constexpr (std::is_same_v<T, DomainEqualityConstraint>) {
                    return std::format("{} <=> {}", inst->d1.to_string(), inst->d2.to_string());
                }
                return "not implemented";
            };
            std::string instructions_string;
            for (auto& inst : instructions) {
                instructions_string += std::visit(instruction_to_string, inst->to_variant()) + '\n';
            }
            instructions_string.pop_back(); // remove the trailing \n

            return std::format("{}:\n{}", debug_name, instructions_string);
        }
        std::unique_ptr<BorrowCheckerFunction> DomainCheckerState::check_function(
            FunctionDeclaration* decl, IRGenerator* irgen, const FunctionSignature& sig, TypeCheckerState* stt)
        {
            auto function = std::make_unique<BorrowCheckerFunction>();
            auto entry_block = function->new_block("entry");
            std::visit(BorrowCheckerEmitter{ irgen, stt, &*function, entry_block }, decl->body->toVariant());
            std::cout << "Phase 1\n" << function->to_string() << std::endl;
            for (auto& block : function->blocks) {
                for (auto it = block->instructions.begin(); it != block->instructions.end(); ++it) {
                    it = std::visit(DomainVariableInserter{this, block->instructions, it}, (*it)->to_variant());
                }
            }
            std::cout << "Phase 2\n" << function->to_string() << std::endl;
            return function;
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
                ntype.details.emplace<UniquePtr>(std::make_unique<BorrowCheckerType>(clone_type(
                    *std::get<UniquePtr>(type.details).subtype
                )));
                return ntype;
            }
            case Array: {
                auto ntype = BorrowCheckerType();
                ntype.details.emplace<Array>(std::make_unique<BorrowCheckerType>(clone_type(
                    *std::get<Array>(type.details).subtype
                )));
                return ntype;
            }
            case RefPtr: {
                auto ntype = BorrowCheckerType();
                ntype.details.emplace<RefPtr>(std::make_unique<BorrowCheckerType>(clone_type(
                    *std::get<RefPtr>(type.details).subtype
                )));
                return ntype;
            }
            default: debugbreak();
            }
        };
        BorrowCheckerType BorrowCheckerType::cloned(DomainCheckerState* stt) const
        {
            std::unordered_map<std::string, Domain> old_to_new_map;
            BorrowCheckerType new_type;
            for (auto& [domain, is_bidr] : domains) {
                if (!is_bidr) {
                    old_to_new_map[domain.to_string()] = stt->new_domain_var();
                }
            }
            
            auto substitute_domains = [&old_to_new_map](BorrowCheckerType& type, const auto& self) -> void
                {
                    for (auto& [domain, is_bidr] : type.domains) {
                        if (!is_bidr) domain = old_to_new_map[domain.to_string()];
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
                new_type.details.emplace<Array>(std::make_unique<BorrowCheckerType>(clone_type(*unq->subtype)));
            }
            else if (auto ref = std::get_if<UniquePtr>(&details); unq) {
                new_type.details.emplace<Array>(std::make_unique<BorrowCheckerType>(clone_type(*ref->subtype)));
            }
            else { debugbreak(); }

            substitute_domains(new_type, substitute_domains);
            return new_type;
        }
}
}
namespace Yoyo {
    namespace BorrowChecker {
        using BlockIteratorTy = DomainVariableInserter::BlockIteratorTy;
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
            for (auto& elem : inst->args) {
                add_constraints_between_types(result, state->get_value_type(elem));
            }
            state->register_value_base_type(inst->into, std::move(result));
            return current_position;
        }
        BlockIteratorTy DomainVariableInserter::operator()(AssignInstruction* inst) {
            auto& left_type = state->get_value_type(inst->lhs);
            auto& right_type = state->get_value_type(inst->rhs);
            add_constraints_between_types(left_type, right_type);
            return current_position;
        }
        BlockIteratorTy DomainVariableInserter::operator()(CallFunctionInstruction*) {
            // literally magic
            return current_position;
        }
        BlockIteratorTy DomainVariableInserter::operator()(RelocateValueInstruction* inst) {
            // no difference between assign tbh
            state->register_value_base_type(inst->into, state->get_value_type(inst->val).cloned(state));
            return current_position;
        }
        BlockIteratorTy DomainVariableInserter::operator()(NewArrayInstruction* inst) {
            // TODO handle zero length arrays
            auto& first_elem = state->get_value_type(inst->values[0]);
            auto subtype = first_elem.cloned(state);
            for (auto& elem : inst->values) {
                add_constraints_between_types(subtype, state->get_value_type(elem));
            }
            state->register_value_base_type(inst->into, BorrowCheckerType::new_array_of(std::move(subtype)));
            return current_position;
        }
        BlockIteratorTy DomainVariableInserter::operator()(NewPrimitiveInstruction* inst) {
            state->register_value_base_type(inst->into, BorrowCheckerType::new_primitive());
            return current_position;
        }
        BlockIteratorTy DomainVariableInserter::operator()(BorrowValueInstruction* inst) {
            // Introduces a new domain
            auto domain = state->new_domain_var();
            instructions.emplace(++current_position, new DomainSubsetConstraint(Domain(domain), inst->val.as_domain()));
            state->register_value_base_type(inst->into, state->get_value_type(inst->val).borrowed(std::move(domain)));
            return current_position;
        }
    }
}