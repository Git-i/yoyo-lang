#include <ranges>

#include "ir_gen.h"
namespace Yoyo
{

    bool LifetimeExceedsFunctionChecker::operator()(Expression*)
    {
        return false;
    }
    bool LifetimeExceedsFunctionChecker::operator()(BinaryOperation* op)
    {
        if(op->op.type != TokenType::Dot) return false;
        return std::visit(*this, op->lhs->toVariant());
    }
    bool LifetimeExceedsFunctionChecker::operator()(CallOperation* op)
    {
        //if it does not return a reference its false
        if(auto as_bin = dynamic_cast<BinaryOperation*>(op->callee.get()))
        {
            if(!std::visit(*this, as_bin->lhs->toVariant())) return false;
        }
        for(auto& arg : op->arguments)
        {
            if(!std::visit(*this, arg->toVariant())) return false;
        }
        return true;
    }
    bool LifetimeExceedsFunctionChecker::operator()(GroupingExpression* grp)
    {
        return std::visit(*this, grp->expr->toVariant());
    }
    bool LifetimeExceedsFunctionChecker::operator()(SubscriptOperation* sub)
    {
        return std::visit(*this, sub->object->toVariant());
    }
    bool LifetimeExceedsFunctionChecker::operator()(NameExpression* nm)
    {
        //the lifetime can only exceed if it's a function parameter, or a global
        //TODO: add globals
        auto fn = IRGenerator::GetParentFunction(nm);
        if(auto it = std::ranges::find_if(fn->signature.parameters, [&](auto& p)
        {
            return p.name == nm->text;
        }); it != fn->signature.parameters.end())
        {
            return true;
        }
        return false;
    }
    void validate_borrows(std::span<const std::pair<Expression*, BorrowResult::borrow_result_t>> param, IRGenerator* irgen)
    {
        std::unordered_map<std::string, Expression*> mut_borrow;
        std::unordered_map<std::string, std::vector<Expression*>> const_borrows;

        for(auto& expr: param)
        {
            for(auto& borrow : expr.second)
            {
                if(borrow.second == BorrowResult::Const)
                {
                    //if the value has been mutably borrowed before its error
                    if(mut_borrow.contains(borrow.first)) { irgen->error(); return; }
                    const_borrows[borrow.first].push_back(expr.first);
                }
                else if(borrow.second == BorrowResult::Mut)
                {
                    //if it's been mutably or immutably borrowed its error
                    if(mut_borrow.contains(borrow.first)) { irgen->error(); return; }
                    if(const_borrows.contains(borrow.first)) { irgen->error(); return; }
                    mut_borrow[borrow.first] = expr.first;
                }
            }
        }
    }
    BorrowResult::borrow_result_t BorrowResult::operator()(NameExpression* expr)
    {
        return {{expr->text, Const }};
    }
    BorrowResult::borrow_result_t BorrowResult::operator()(PrefixOperation* expr)
    {
        //dereference propagates borrow
        if(expr->op.type == TokenType::Star)
        {
            auto v = std::visit(*this, expr->operand->toVariant());
            for(auto& val : v) val.second = Const;
            return v;
        }
        std::visit(*this, expr->operand->toVariant());
        return {};
    }
    BorrowResult::borrow_result_t BorrowResult::operator()(BinaryOperation* expr)
    {
        //binary expression results can't borrow except '.'
        if(expr->op.type == TokenType::Dot)
            return std::visit(*this, expr->lhs->toVariant());
        auto left_t = std::visit(ExpressionTypeChecker{irgen}, expr->lhs->toVariant());

        //operator overloading exists
        if(left_t->is_primitive())
        {
            std::array<std::pair<Expression*, borrow_result_t>, 2> borrows;
            borrows[0].first = expr->lhs.get();
            borrows[0].second = expr->op.is_assignment() ?
                std::visit(LValueBorrowResult{irgen}, expr->lhs->toVariant()) :
                std::visit(*this, expr->lhs->toVariant());

            borrows[1].first = expr->rhs.get();
            borrows[1].second = std::visit(*this, expr->rhs->toVariant());
            validate_borrows(borrows, irgen);
            return {};
        }
        //TODO operator overloading
        return {};
    }

    BorrowResult::borrow_result_t BorrowResult::operator()(GroupingExpression* expr)
    {
        return std::visit(*this, expr->expr->toVariant());
    }

    BorrowResult::borrow_result_t BorrowResult::operator()(CallOperation* expr)
    {
        auto callee_ty = std::visit(ExpressionTypeChecker{irgen}, expr->callee->toVariant());
        std::vector<std::pair<Expression*, borrow_result_t>> borrows;
        bool is_bound = callee_ty->is_bound();
        //bound functions borrow the first arg too!
        if(is_bound)
        {
            auto as_bexp = reinterpret_cast<BinaryOperation*>(expr->callee.get());
            auto& type = callee_ty->sig.parameters[0].type;
            auto is_mut_borrow = type.is_non_owning_mut(irgen);
            borrows.emplace_back(as_bexp->lhs.get(),
                is_mut_borrow ? std::visit(LValueBorrowResult{irgen}, as_bexp->lhs->toVariant())
                    : std::visit(*this, as_bexp->lhs->toVariant()));
        }
        for(size_t i = 0; i < expr->arguments.size(); i++)
        {
            auto& arg = expr->arguments[i];
            auto& type = callee_ty->sig.parameters[i + is_bound].type;
            auto is_mut_borrow = type.is_non_owning_mut(irgen);
            borrows.emplace_back(arg.get(),
                is_mut_borrow ? std::visit(LValueBorrowResult{irgen}, arg->toVariant())
                    : std::visit(*this, arg->toVariant()));
        }
        validate_borrows(borrows, irgen);
        //mutable non owning return borrows only mutable args
        borrow_result_t out;
        if(callee_ty->sig.returnType.is_non_owning_mut(irgen))
        {
            for(auto& borrow : borrows)
            {
                auto v = std::views::filter(borrow.second, [](auto& b) { return b.second == Mut; });
                out.insert(out.begin(),
                    std::make_move_iterator(v.begin()),
                    std::make_move_iterator(v.end()));
            }
        }
        //immutable non owning borrows all args, but does so immutably
        else if(callee_ty->sig.returnType.is_non_owning(irgen))
        {
            for(auto& borrow : borrows)
            {
                std::ranges::for_each(borrow.second, [](auto& b){ b.second = Const; });
                out.insert(out.begin(),
                    std::make_move_iterator(borrow.second.begin()),
                    std::make_move_iterator(borrow.second.end()));
            }
        }
        return out;
    }

    void validate_expression_borrows(Expression* expr, IRGenerator* irgen)
    {
        validate_borrows({{{expr, std::visit(BorrowResult{irgen}, expr->toVariant())}}}, irgen);
    }
}
