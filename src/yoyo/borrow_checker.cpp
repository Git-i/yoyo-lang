#include "ir_gen.h"
#include <variant>
#include <ranges>
namespace Yoyo { bool has_type_variable(const Yoyo::Type& tp); }
#define RE_REPR(x) x->evaluated_type = stt->best_repr(x->evaluated_type);\
if(has_type_variable(x->evaluated_type)) {\
    irgen->error(Yoyo::Error(x, "Could not resolve type of this expression"));\
}
namespace Yoyo
{
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
        auto& checker = irgen->function_borrow_checkers.back();
        auto while_cond = checker.make_block();
        auto while_body = checker.make_block();
        auto while_exit = checker.make_block();

        checker.create_br(while_cond);
        checker.set_block(while_cond);
        checker.drop_object(std::visit(*this, stat->condition->toVariant()), stat->condition.get());
        checker.create_cond_br(while_body, while_exit);

        checker.set_block(while_body);
        std::visit(*this, stat->body->toVariant());
        checker.create_br(while_cond);

        checker.set_block(while_cond);
    }
    void BorrowCheckerEmitter::operator()(VariableDeclaration* decl) {
        decl->type = stt->best_repr(*decl->type);
        auto& checker = irgen->function_borrow_checkers.back();
        if (decl->initializer) std::visit(*this, decl->initializer->toVariant());
        // variables have to be fully owning so we don't need to worry about moving
        variables.back().emplace_back(std::string(decl->identifier.text), checker.make_object());
    }
    void BorrowCheckerEmitter::operator()(ForStatement*) {
        // TODO
    }
    void BorrowCheckerEmitter::operator()(ReturnStatement* stat) {
        if (stat->expression) {
            auto& checker = irgen->function_borrow_checkers.back();
            checker.drop_object(std::visit(*this, stat->expression->toVariant()), stat);
        }
    }
    void BorrowCheckerEmitter::operator()(ExpressionStatement* stat) {
        auto& checker = irgen->function_borrow_checkers.back();
        checker.drop_object(std::visit(*this, stat->expression->toVariant()), stat->expression.get());
    }
    void BorrowCheckerEmitter::operator()(ConditionalExtraction* stat) {
        auto& checker = irgen->function_borrow_checkers.back();
        auto cond = std::visit(*this, stat->condition->toVariant());

        auto if_stat = checker.make_block();
        auto if_cont = checker.make_block();
        auto else_stat = stat->else_body ? checker.make_block() : nullptr;

        checker.create_cond_br(if_stat, else_stat ? else_stat : if_cont);
        checker.set_block(if_stat);
        variables.emplace_back();
        variables.back().emplace_back(stat->captured_name, stat->is_ref ? cond : checker.make_object());
        std::visit(*this, stat->body->toVariant());
        variables.pop_back();
        checker.drop_object(cond, stat->condition.get());
        checker.create_br(if_cont);

        if (else_stat) {
            checker.set_block(else_stat);
            checker.drop_object(cond, stat->condition.get());
            std::visit(*this, stat->else_body->toVariant());
            checker.create_br(if_cont);
        }

        checker.set_block(if_cont);
        checker.drop_object(cond, stat->condition.get());
    }
    void BorrowCheckerEmitter::operator()(WithStatement* stat) {
        auto& checker = irgen->function_borrow_checkers.back();
        auto expr = std::visit(*this, stat->expression->toVariant());
        auto new_expr = checker.move_object(expr, stat->expression.get());
        variables.emplace_back();
        variables.back().emplace_back(stat->name, new_expr);
        std::visit(*this, stat->body->toVariant());
        checker.drop_object(expr, stat);
        variables.pop_back();
    }
    Value BorrowCheckerEmitter::operator()(IfExpression* expr) {
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
    Value BorrowCheckerEmitter::operator()(BlockExpression* expr) {
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
    Value BorrowCheckerEmitter::operator()(IntegerLiteral* lit) {
        RE_REPR(lit);
        auto name = temporary_name();
        current_block->add_instruction(new NewPrimitiveInstruction(std::string(name)));
        return Value::from(std::move(name));
    }
    Value BorrowCheckerEmitter::operator()(BooleanLiteral* lit) {
        RE_REPR(lit);
        auto name = temporary_name();
        current_block->add_instruction(new NewPrimitiveInstruction(std::string(name)));
        return Value::from(std::move(name));
    }
    Value BorrowCheckerEmitter::operator()(TupleLiteral* exp) {
        RE_REPR(exp);
        std::vector<Value> args;
        std::ranges::transform(exp->elements, std::back_inserter(args), [this](auto& expr)
            {
                return std::visit(*this, expr->toVariant());
            });

        auto name = temporary_name();
        current_block->add_instruction(new CallFunctionInstruction("__builtin_make_tuple", std::string(name), std::move(args)));
        return Value::from(std::move(name));
    }
    std::string BorrowCheckerEmitter::operator()(ArrayLiteral* lit) {
        RE_REPR(lit);
        auto& checker = irgen->function_borrow_checkers.back();
        auto array = checker.make_object();
        // [<expr>, <expr>, ... ]
        if (std::holds_alternative<std::vector<std::unique_ptr<Expression>>>(lit->elements)) {
            auto& array_elems = std::get<std::vector<std::unique_ptr<Expression>>>(lit->elements);
            for (auto& elem : array_elems) {
                auto elem_eval = std::visit(*this, elem->toVariant());
                clone_into(checker, elem_eval, array, elem->evaluated_type, lit, irgen); // TODO: update when type system is better
            }
        }
        // [<expr>; <expr>] repeat first <expr>, sencond <expr> times (second expr is a constant)
        else {
            auto& elem_size = std::get<std::pair<std::unique_ptr<Expression>, std::unique_ptr<Expression>>>(lit->elements);
            auto elem_eval = std::visit(*this, elem_size.first->toVariant());
            clone_into(checker, elem_eval, array, elem_size.first->evaluated_type, lit, irgen);
        }
        return array;
    }
    Value BorrowCheckerEmitter::operator()(RealLiteral* lit) {
        RE_REPR(lit);
        auto name = temporary_name();
        current_block->add_instruction(new NewPrimitiveInstruction(std::string(name)));
        return Value::from(std::move(name));
    }
    std::string BorrowCheckerEmitter::operator()(StringLiteral* lit) {
        RE_REPR(lit);
        auto& checker = irgen->function_borrow_checkers.back();
        for (auto& entry : lit->literal) {
            if (std::holds_alternative<std::unique_ptr<Expression>>(entry)) {
                auto& capture = std::get<std::unique_ptr<Expression>>(entry);
                // evaluate the expression
                // borrow it to convert to string
                // drop the borrow
                checker.drop_object(
                    checker.borrow_values({ {
                        { std::visit(*this, capture->toVariant()), BorrowType::Const }
                    } }, lit), lit
                );
            }
        }
        return "__literal";
    }
    Value BorrowCheckerEmitter::operator()(NameExpression* name) {
        RE_REPR(name);
        for (auto& block : variables | std::views::reverse) {
            for (auto& [var_name, id] : block) {
                if (var_name == name->text) return Value::from(std::string(id));
            }
        }
        return "__literal";
    }
    std::string BorrowCheckerEmitter::operator()(GenericNameExpression* name) {
        RE_REPR(name);
        // this is probably a function
        // need to actually check tho so TODO
        return "__literal";
    }
    std::string BorrowCheckerEmitter::operator()(PrefixOperation* pfx) {
        RE_REPR(pfx);
        auto& checker = irgen->function_borrow_checkers.back();
        auto this_eval = std::visit(*this, pfx->operand->toVariant());
        switch (pfx->op.type) {
        case TokenType::Star: return this_eval; // borrows are checked at creation time
        case TokenType::Ampersand: return checker.borrow_values({ {{this_eval, BorrowType::Const}} }, pfx);
        case TokenType::RefMut: return checker.borrow_values({ {{this_eval, BorrowType::Mut}} }, pfx);
        }
    }
    std::string BorrowCheckerEmitter::operator()(BinaryOperation* op) {
        RE_REPR(op);
        for (auto& sub : op->subtypes) {
            sub = stt->best_repr(sub);
            if (has_type_variable(sub)) {
                irgen->error(Error(op, "Could not resolve all generics for this operation"));
            }
        }
        using enum TokenType;
        auto do_overloadable_explicit_token = [op, this](TokenType tk) {
            auto& checker = irgen->function_borrow_checkers.back();
            auto res = do_call_like({ {
                op->lhs.get(),
                op->rhs.get()
            } }, irgen, op, *this);
            if (stt->is_non_owning(op->evaluated_type, irgen)) return res;
            else {
                checker.drop_object(res, op);
                return (std::string)checker.make_object();
            }
            };
        auto do_overloadable = [op, this, &do_overloadable_explicit_token]() {
            return do_overloadable_explicit_token(op->op.type);
            };
        switch (op->op.type) {
        case Minus: [[fallthrough]];
        case Star: [[fallthrough]];
        case Slash: [[fallthrough]];
        case Percent: [[fallthrough]];
        case DoubleGreater: [[fallthrough]];
        case DoubleLess: [[fallthrough]];
        case Plus: return do_overloadable();
        case Less: [[fallthrough]];
        case GreaterEqual: [[fallthrough]];
        case LessEqual: [[fallthrough]];
        case BangEqual: [[fallthrough]];
        case DoubleEqual: [[fallthrough]];
        case Spaceship: [[fallthrough]];
        case Greater: return do_overloadable_explicit_token(TokenType::Spaceship);
        case Dot: {
            if (op->evaluated_type.name == "__bound_fn") {
                std::visit(*this, op->lhs->toVariant());
            }
            // field borrow TODO
            else {
                std::visit(*this, op->lhs->toVariant());
            }
        }
        case DoubleDotEqual: RE_REPR(op->lhs.get()); return ""; // TODO irgen->error(Error(op, "Not implemented yet")); return {};
        case DoubleDot: irgen->error(Error(op, "Not implemented yet")); return {};
        }
    }
    Value BorrowCheckerEmitter::operator()(GroupingExpression* grp) {
        RE_REPR(grp);
        return std::visit(*this, grp->expr->toVariant());
    }
    std::string BorrowCheckerEmitter::operator()(LogicalOperation* lg) {
        RE_REPR(lg);

        return "__literal";
    }
    std::string BorrowCheckerEmitter::operator()(PostfixOperation*) { return ""; }
    std::string BorrowCheckerEmitter::operator()(CallOperation* op) {
        RE_REPR(op);
        auto& checker = irgen->function_borrow_checkers.back();
        auto& callee = op->evaluated_type;
        if (callee.name.starts_with("__union_var"))
        {
            irgen->error(Error(op, "Not implemented"));
            return "";
        }

        //if (callee.is_bound) {
        //    irgen->error(Error(op, "Not implemented"));
        //    return "";
        //}
        std::vector<Expression*> input_types;
        input_types.push_back(op->callee.get());
        for (auto i : std::views::iota(0u, op->arguments.size())) {
            input_types.emplace_back(op->arguments[i].get());
        }

        auto res = do_call_like(input_types, irgen, op, *this);
        if (stt->is_non_owning(op->evaluated_type, irgen)) return res;
        else {
            checker.drop_object(res, op);
            return (std::string)checker.make_object();
        }
    }
    std::string BorrowCheckerEmitter::operator()(SubscriptOperation* op) {
        RE_REPR(op);
        irgen->error(Error(op, "Not implemented yet")); return "";
    }
    std::string BorrowCheckerEmitter::operator()(LambdaExpression*) { return ""; }
    std::string BorrowCheckerEmitter::operator()(TryExpression*) { return ""; }
    std::string BorrowCheckerEmitter::operator()(ScopeOperation* scp) {
        RE_REPR(scp);
        // either function, enum or constant
        return "__literal";
    }
    std::string BorrowCheckerEmitter::operator()(ObjectLiteral* lit) {
        RE_REPR(lit);
        auto& checker = irgen->function_borrow_checkers.back();
        // implicit conversion can happen here but they don't involve borrows
        auto obj = checker.make_object();
        for (auto& [name, elem] : lit->values) {
            auto elem_eval = std::visit(*this, elem->toVariant());
            auto& type = elem->evaluated_type;
            clone_into(checker, elem_eval, obj, type, lit, irgen);
        }
        return obj;
    }
    std::string BorrowCheckerEmitter::operator()(NullLiteral* lit) {
        RE_REPR(lit);
        return "__literal";
    }
    std::string BorrowCheckerEmitter::operator()(AsExpression* ss) {
        RE_REPR(ss);
        auto& checker = irgen->function_borrow_checkers.back();
        auto src = std::visit(*this, ss->expr->toVariant());
        auto res = checker.make_object();
        convert_into(checker, src, res, ss->expr->evaluated_type, ss->evaluated_type, ss, irgen);
        return res;
    }
    Value BorrowCheckerEmitter::operator()(CharLiteral* lit) {
        RE_REPR(lit);
        auto name = temporary_name();
        current_block->add_instruction(new NewPrimitiveInstruction(std::string(name)));
        return Value::from(std::move(name));
    }
    std::string BorrowCheckerEmitter::operator()(GCNewExpression* gcn) {
        RE_REPR(gcn);
        return "__no_borrow";
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
    
}