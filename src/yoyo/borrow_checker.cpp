#include "ir_gen.h"
#include <variant>
#include <ranges>
namespace Yoyo
{
    std::pair<std::string, OverloadDetailsBinary*> resolveBin(const Type& lhs, const Type& rhs, TokenType t, IRGenerator* irgen);
    void clone_into(BorrowChecker& checker, const std::string& src, const std::string& dst, const Type& type, Expression* ex, IRGenerator* irgen) {
        if (type.is_lvalue)
        {
            // actual clone
            // the clone method looks like:
            // clone: (&this) -> This (for non integral types)
            // and clone: (this) -> This (for integral types)
            if (type.should_sret())
            {
                // clone: (&this) -> This
                auto borrow = checker.borrow_values({ {{src, BorrowType::Const}} }, ex); // &this
                // call the function
                // if the type is non owning the result still borrows
                // else we make a new clean object
                if (type.is_non_owning(irgen)) checker.move_into(borrow, dst, ex);
                else checker.drop_object(borrow, ex);
            }
            // if we use clone: (this) -> This there's no borrow involved unless the type is non owning
            else
            {
                if(type.is_non_owning(irgen))
                    checker.move_into(
                        checker.borrow_values({ {{src, BorrowType::Const}} }, ex),
                        dst, ex);
            }
        }
        else
        {
            // just move
            checker.move_into(src, dst, ex);
        }
    }
    void convert_into(BorrowChecker& checker, const std::string& src, const std::string& dst, const Type& provided, const Type& target, Expression* expr, IRGenerator* irgen) {
        if (provided.is_equal(target)) {
            return clone_into(checker, src, dst, provided, expr, irgen);
        }
        if (target.is_integral() || target.is_floating_point()) return;
        // &mut T -> &T (this_eval: &mut T) -> &T
        if (target.is_reference() && !provided.is_gc_reference()) {
            checker.move_into(provided.is_lvalue ?
                checker.borrow_values({ {{src, BorrowType::Const}} }, expr) :
                checker.borrow_as_temporary(src, expr),
                dst, expr);
        }
        // implicit conversion work similarly to clone
        // they borrow the value temporarily to produce a different type (difference from clone)
        // convert: (&this) -> Out or convert: (this) -> Out
        if (provided.should_sret()) {
            auto borrow = provided.is_lvalue ?
                checker.borrow_values({ {{src, BorrowType::Const}} }, expr) :
                checker.borrow_as_temporary(src, expr);
            if (target.is_non_owning(irgen)) checker.move_into(borrow, dst, expr);
            else checker.drop_object(borrow, expr);
        }
        else {
            // convert: (this) -> Out but typeof(this) cannot be non-owning so do nothing
        }
    }
    // first parameter maps target to provided type
    std::string do_call_like(
        std::span<const std::tuple<const Type&, const Type&, Expression*>> input_types, 
        IRGenerator* irgen,
        Expression* exp,
        BorrowCheckerEmitter& em
    ) {
        // this is where the bulk of the logic of the borrow checker lies
        // it handles logic for borrows in any kind of expression with multiple inputs and one ouptut (basically anything)
        // it also handles copy/move/implicit convert automatically
        auto& checker = irgen->function_borrow_checkers.back();
        std::string out = checker.make_object();
        for (auto& [target, provided, expr] : input_types) {
            auto this_eval = std::visit(em, expr->toVariant());
            convert_into(checker, this_eval, out, provided, target, expr, irgen);
        }
        return out;
    }
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
    void BorrowCheckerEmitter::operator()(VariableDeclaration* decl) {
        auto& checker = irgen->function_borrow_checkers.back();
        if (decl->initializer) std::visit(*this, decl->initializer->toVariant());
        // variables have to be fully owning so we don't need to worry about moving
        variables.back().emplace_back(std::string(decl->identifier.text), checker.make_object());
    }
    void BorrowCheckerEmitter::operator()(IfStatement* stat) {
        auto& checker = irgen->function_borrow_checkers.back();
        auto if_stat = checker.make_block();
        auto if_cont = checker.make_block();
        auto else_stat = stat->else_stat ? checker.make_block() : nullptr;
        checker.drop_object(std::visit(*this, stat->condition->toVariant()), stat->condition.get());

        checker.create_cond_br(if_stat, else_stat ? else_stat : if_cont);
        checker.set_block(if_stat);
        std::visit(*this, stat->then_stat->toVariant());
        checker.create_br(if_cont);

        if (else_stat) {
            checker.set_block(else_stat);
            std::visit(*this, stat->else_stat->toVariant());
            checker.create_br(if_cont);
        }

        checker.set_block(if_cont);
    }
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
    void BorrowCheckerEmitter::operator()(ForStatement*) {
        // TODO
    }
    void BorrowCheckerEmitter::operator()(BlockStatement* stat) {
        auto& checker = irgen->function_borrow_checkers.back();
        variables.emplace_back();
        for (auto& stt : stat->statements) {
            std::visit(*this, stt->toVariant());
        }

        for (auto& variable : variables.back() | std::views::reverse) {
            checker.drop_object(variable.second, stat);
        }
        variables.pop_back();
    }
    void BorrowCheckerEmitter::operator()(ReturnStatement*) {
    // TODO
    }
    void BorrowCheckerEmitter::operator()(ExpressionStatement* stat) {
        auto& checker = irgen->function_borrow_checkers.back();
        checker.drop_object(std::visit(*this, stat->expression->toVariant()), stat->expression.get());
    }
    void BorrowCheckerEmitter::operator()(ConditionalExtraction* stat){
        auto& checker = irgen->function_borrow_checkers.back();
        auto cond = std::visit(*this, stat->condition->toVariant());
        
        auto if_stat = checker.make_block();
        auto if_cont = checker.make_block();
        auto else_stat = stat->else_body ? checker.make_block() : nullptr;

        checker.create_cond_br(if_stat, else_stat ? else_stat : if_cont);
        checker.set_block(if_stat);
        std::visit(*this, stat->body->toVariant());
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

    std::string BorrowCheckerEmitter::operator()(IntegerLiteral*) {
        // __literal is a special object to denote it can't be borrowed
        // mutably
        return "__literal";
    }
    std::string BorrowCheckerEmitter::operator()(BooleanLiteral*) {
        return "__literal";
    }
    std::string BorrowCheckerEmitter::operator()(TupleLiteral* exp) {
        auto& checker = irgen->function_borrow_checkers.back();
        // implicit conversion can happen here but they don't involve borrows
        auto tuple = checker.make_object();
        for (auto& elem : exp->elements) {
            auto elem_eval = std::visit(*this, elem->toVariant());
            clone_into(checker, elem_eval, tuple, exp->evaluated_type, exp, irgen); // TODO: update when type system is better
        }
        return tuple;
    }
    std::string BorrowCheckerEmitter::operator()(ArrayLiteral* lit) {
        auto& checker = irgen->function_borrow_checkers.back();
        auto array = checker.make_object();
        // [<expr>, <expr>, ... ]
        if (std::holds_alternative<std::vector<std::unique_ptr<Expression>>>(lit->elements)) {
            auto& array_elems = std::get<std::vector<std::unique_ptr<Expression>>>(lit->elements);
            for (auto& elem : array_elems) {
                auto elem_eval = std::visit(*this, elem->toVariant());
                auto type = std::visit(ExpressionTypeChecker{ irgen }, elem->toVariant());
                clone_into(checker, elem_eval, array, *type, lit, irgen); // TODO: update when type system is better
            }
        }
        // [<expr>; <expr>] repeat first <expr>, sencond <expr> times (second expr is a constant)
        else {
            auto& elem_size = std::get<std::pair<std::unique_ptr<Expression>, std::unique_ptr<Expression>>>(lit->elements);
            auto type = std::visit(ExpressionTypeChecker{ irgen }, elem_size.first->toVariant());
            type->is_lvalue = true; // type must be an lvalue because its copied mutliple times
            auto elem_eval = std::visit(*this, elem_size.first->toVariant());
            clone_into(checker, elem_eval, array, *type, lit, irgen);
        }
        return array;
    }
    std::string BorrowCheckerEmitter::operator()(RealLiteral*) {
        return "__literal";
    }
    std::string BorrowCheckerEmitter::operator()(StringLiteral* lit) {
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
    std::string BorrowCheckerEmitter::operator()(NameExpression* name) {
        for (auto& block : variables | std::views::reverse) {
            for (auto& [var_name, id] : block) {
                if (var_name == name->text) return id;
            }
        }
        irgen->error(Error(name, "Borrow checker internal error"));
    }
    std::string BorrowCheckerEmitter::operator()(GenericNameExpression*) {
        // this is probably a function
        // need to actually check tho so TODO
        return "__literal";
    }
    std::string BorrowCheckerEmitter::operator()(PrefixOperation* pfx) {
        auto& checker = irgen->function_borrow_checkers.back();
        auto this_eval = std::visit(*this, pfx->operand->toVariant());
        switch (pfx->op.type) {
        case TokenType::Star: return this_eval; // borrows are checked at creation time
        case TokenType::Ampersand: return checker.borrow_values({ {{this_eval, BorrowType::Const}} }, pfx);
        case TokenType::RefMut: return checker.borrow_values({ {{this_eval, BorrowType::Mut}} }, pfx);
        }
    }
    std::string BorrowCheckerEmitter::operator()(BinaryOperation* op) {
        using enum TokenType;
        auto do_overloadable_explicit_token = [op, this](TokenType tk) {
            auto& checker = irgen->function_borrow_checkers.back();
            auto tp_check = ExpressionTypeChecker{ irgen };
            auto left = std::visit(tp_check, op->lhs->toVariant()).value();
            auto right = std::visit(tp_check, op->rhs->toVariant()).value();
            auto [block, target] = resolveBin(left, right, tk, irgen);
            auto res = do_call_like({ {
                {target->left, left, op->lhs.get()},
                {target->right, right, op->rhs.get()}
            } }, irgen, op, *this);
            if (target->result.is_non_owning(irgen)) return res;
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
        case Dot: // return doDot(op->lhs.get(), op->rhs.get(), *left_t);
        case DoubleDotEqual: irgen->error(Error(op, "Not implemented yet")); return {};
        case DoubleDot: irgen->error(Error(op, "Not implemented yet")); return {};
        }
    }
    std::string BorrowCheckerEmitter::operator()(GroupingExpression* grp) {
        return std::visit(*this, grp->expr->toVariant());
    }
    std::string BorrowCheckerEmitter::operator()(LogicalOperation*) {
        return "__literal";
    }
    std::string BorrowCheckerEmitter::operator()(PostfixOperation*) { return ""; }
    std::string BorrowCheckerEmitter::operator()(CallOperation* op) {
        auto& checker = irgen->function_borrow_checkers.back();
        ExpressionTypeChecker type_checker{ irgen };
        auto result_type = type_checker(op).value();
        auto callee = std::visit(type_checker, op->callee->toVariant()).value();
        if (callee.name.starts_with("__union_var"))
        {
            irgen->error(Error(op, "Not implemented"));
            return "";
        }

        if (callee.is_bound) {
            irgen->error(Error(op, "Not implemented"));
            return "";
        }
        std::vector<std::tuple<const Type&, const Type&, Expression*>> input_types;
        std::vector<Type> evaluated_type;
        // this is extremely unsafe it will be changed with the type system rewrite (TODO)
        evaluated_type.reserve(op->arguments.size());
        for (auto i : std::views::iota(0u, op->arguments.size())) {
            evaluated_type.push_back(std::visit(type_checker, op->arguments[i]->toVariant()).value());
            input_types.emplace_back(callee.sig.parameters[i].type, evaluated_type.back(), op->arguments[i].get());
        }

        auto res = do_call_like(input_types, irgen, op, *this);
        if (result_type.is_non_owning(irgen)) return res;
        else {
            checker.drop_object(res, op);
            return (std::string)checker.make_object();
        }
    }
    std::string BorrowCheckerEmitter::operator()(SubscriptOperation* op) {
        irgen->error(Error(op, "Not implemented yet")); return "";
    }
    std::string BorrowCheckerEmitter::operator()(LambdaExpression*) { return ""; }
    std::string BorrowCheckerEmitter::operator()(ScopeOperation*) {
        // either function, enum or constant
        return "__literal";
    }
    std::string BorrowCheckerEmitter::operator()(ObjectLiteral* lit) {
        auto& checker = irgen->function_borrow_checkers.back();
        // implicit conversion can happen here but they don't involve borrows
        auto obj = checker.make_object();
        for (auto& [name, elem] : lit->values) {
            auto elem_eval = std::visit(*this, elem->toVariant());
            auto type = std::visit(ExpressionTypeChecker{ irgen }, elem->toVariant());
            clone_into(checker, elem_eval, obj, *type, lit, irgen);
        }
        return obj;
    }
    std::string BorrowCheckerEmitter::operator()(NullLiteral* lit) {
        return "__literal";
    }
    std::string BorrowCheckerEmitter::operator()(AsExpression*) {
        return "__not_implemented";
    }
    std::string BorrowCheckerEmitter::operator()(CharLiteral*) {
        return "__literal";
    }
    std::string BorrowCheckerEmitter::operator()(GCNewExpression*) {
        return "__no_borrow";
    }
    std::string BorrowCheckerEmitter::operator()(MacroInvocation* ivc) {
        return std::visit(*this, ivc->result->toVariant());
    }
    std::string BorrowCheckerEmitter::operator()(SpawnExpression*) {
        // fibers can't borrow from other fibers
        return "__literal";
    }
    
}