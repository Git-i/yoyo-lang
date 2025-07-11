#include <ranges>

#include "ir_gen.h"
namespace Yoyo
{

    bool LifetimeExceedsFunctionChecker::operator()(Expression* expr)
    {
        return true; //TODO fix this
        auto borrows = std::visit(BorrowResult{irgen}, expr->toVariant());
        for(auto&[name, _] : borrows)
        {
            NameExpression nexpr(std::move(name));
            nexpr.parent = expr->parent;
            if(!(*this)(&nexpr)) return false;
        }
        return true;
    }

    bool LifetimeExceedsFunctionChecker::operator()(NameExpression* nm)
    {
        return true; //TODO fix this
        //the lifetime can only exceed if it's a function parameter(or borrowed from a param), or a global
        //TODO: add globals
        if(irgen->lifetimeExtensions.contains(nm->text))
        {
            bool is_valid = true;
            for(auto& expr : irgen->lifetimeExtensions[nm->text])
            {
                NameExpression nm2(expr.first);
                nm2.parent = nm->parent;
                if(!(*this)(&nm2)) is_valid = false;
            }
            return is_valid;
        }
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

        auto is_borrowed = [](auto& a, const std::string& value) {
            auto view = a | std::views::keys;
            auto it = std::ranges::find_if(view, [&value](const std::string& str) {
                    return str == value || 
                    (str.starts_with(value) && str[value.size()] == ':') ||
                    (value.starts_with(str) && value[str.size()] == ':');
                });
            if (it != view.end()) return *it;
            return std::string("");
            };

        for(auto& expr: param)
        {
            for(auto& borrow : expr.second)
            {
                if(borrow.second == BorrowResult::Const)
                {
                    //if the value has been mutably borrowed before its error
                    if (auto brw = is_borrowed(mut_borrow, borrow.first); !brw.empty()) {
                        auto err_borrow = mut_borrow[brw];
                        Error err(expr.first, "Attempt to borrow mutably borrowed value");
                        err.markers.emplace_back(SourceSpan{ err_borrow->beg, err_borrow->end }, "Mutable borrow occurs here");
                        irgen->error(err); 
                        return; 
                    }
                    const_borrows[borrow.first].push_back(expr.first);
                }
                else if(borrow.second == BorrowResult::Mut)
                {
                    //if it's been mutably or immutably borrowed its error
                    if (auto brw = is_borrowed(mut_borrow, borrow.first); !brw.empty()) {
                        auto err_borrow = mut_borrow[brw];
                        Error err(expr.first, "Attempt to borrow mutably borrowed value");
                        err.markers.emplace_back(SourceSpan{ err_borrow->beg, err_borrow->end }, "Mutable borrow occurs here");
                        irgen->error(err);
                        return;
                    }
                    if(auto brw = is_borrowed(const_borrows, borrow.first); !brw.empty()) { 
                        Error err(expr.first, "Attempt to mutably borrow already borrowed value");
                        for (auto expr : const_borrows[brw]) {
                            err.markers.emplace_back(SourceSpan{ expr->beg, expr->end }, "Borrow occurs here");
                        }
                        irgen->error(err); return; 
                    }
                    mut_borrow[borrow.first] = expr.first;
                }
            }
        }
    }
    BorrowResult::borrow_result_t BorrowResult::LValueBorrowResult::operator()(NameExpression* expr)
    {
        bool should_const = false;
        if (auto var = irgen->getVariableType(expr->text, expr))
        {
            should_const = var->is_gc_reference();
            if (!(var->is_mutable || var->is_mutable_reference() || var->is_gc_reference())) {
                irgen->error(Error(expr, "'" + expr->text + "' cannot be mutably borrowed"));
            }
        }
        if(irgen->lifetimeExtensions.contains(expr->text))
        {
            auto v = irgen->lifetimeExtensions[expr->text];
            v.emplace_back(expr->text, Mut);
            return  v;
        }
        return {{expr->text, should_const ? Const : Mut }};
    }

    BorrowResult::borrow_result_t BorrowResult::LValueBorrowResult::operator()(BinaryOperation* expr)
    {
        BorrowResult{irgen}(expr);
        if (expr->op.type == TokenType::Dot)
        {
            auto res = std::visit(*this, expr->lhs->toVariant());
            //field borrow
            if (res.size() == 1)
            {
                auto r_nm = dynamic_cast<NameExpression*>(expr->rhs.get());
                if (!r_nm) return res;
                //propagating a field borrow
                if (dynamic_cast<NameExpression*>(expr->lhs.get()) || res[0].first.find("::") != std::string::npos) {
                    res[0].first += "::" + r_nm->text;
                }
            }
            return res;
        }
        return {};
    }

    BorrowResult::borrow_result_t BorrowResult::LValueBorrowResult::operator()(CallOperation* expr)
    {
        return BorrowResult{irgen}.doCall(expr);
    }

    BorrowResult::borrow_result_t BorrowResult::LValueBorrowResult::operator()(PrefixOperation* expr)
    {
        BorrowResult{irgen}(expr);
        if(expr->op.type == TokenType::Star)
            return std::visit(*this, expr->operand->toVariant());
        if(expr->op.type == TokenType::RefMut)
            return std::visit(*this, expr->operand->toVariant());
        irgen->error(Error(expr, "Expression cannot be mutably borrowed"));//expression cannot be mutably borrowed
        return {};
    }

    BorrowResult::borrow_result_t BorrowResult::LValueBorrowResult::operator()(AsExpression* expr)
    {
        auto dst_ty = std::visit(ExpressionTypeChecker{irgen}, expr->toVariant());
        std::array<std::pair<Expression*, borrow_result_t>, 1> borrows = {
            {
                {expr,  std::visit(*this, expr->expr->toVariant())}
            }
        };
        validate_borrows(borrows, irgen);
        if(dst_ty->is_non_owning(irgen)) return borrows[0].second;
        return {};
    }

    BorrowResult::borrow_result_t BorrowResult::operator()(NameExpression* expr)
    {
        if(irgen->lifetimeExtensions.contains(expr->text))
        {
            auto v = irgen->lifetimeExtensions[expr->text];
            for(auto& val : v) val.second = Const;
            v.emplace_back(expr->text, Const);
            return v;
        }
        return {{expr->text, Const }};
    }
    BorrowResult::borrow_result_t BorrowResult::operator()(PrefixOperation* expr)
    {
        //dereference propagates borrow
        if(expr->op.type == TokenType::Star || expr->op.type == TokenType::RefMut || expr->op.type == TokenType::Ampersand)
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
            auto right_borrow = std::visit(*this, expr->rhs->toVariant());
            //assignment does not borrow the right(it clones)
            if(expr->op.is_assignment()) validate_borrows({{{expr->rhs.get(),right_borrow}}}, irgen);
            else borrows[1].second = std::move(right_borrow);
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
        auto v = doCall(expr);
        for(auto& val : v) val.second = Const;
        return v;
    }
    BorrowResult::borrow_result_t BorrowResult::operator()(MacroInvocation* invc)
    {
        ExpressionTypeChecker{ irgen }(invc);
        return std::visit(*this, invc->result->toVariant());
    }
    BorrowResult::borrow_result_t BorrowResult::doCall(CallOperation* expr)
    {
        auto callee_ty = std::visit(ExpressionTypeChecker{irgen}, expr->callee->toVariant());
        std::vector<std::pair<Expression*, borrow_result_t>> borrows;
        if (callee_ty->name.starts_with("__union_var"))
        {
            //TODO
            return {};
        }
        bool is_bound = callee_ty->is_bound;
        //bound functions borrow the first arg too!
        if(is_bound)
        {
            auto as_bexp = reinterpret_cast<BinaryOperation*>(expr->callee.get());
            auto& type = callee_ty->sig.parameters[0].type;
            auto is_mut_borrow = type.is_non_owning_mut(irgen);

            if(type.is_non_owning(irgen))
                borrows.emplace_back(as_bexp->lhs.get(),
                    is_mut_borrow ? std::visit(LValueBorrowResult{irgen}, as_bexp->lhs->toVariant())
                    : std::visit(*this, as_bexp->lhs->toVariant()));
        }
        for(size_t i = 0; i < expr->arguments.size(); i++)
        {
            auto& arg = expr->arguments[i];
            auto& type = callee_ty->sig.parameters[i + is_bound].type;
            auto is_mut_borrow = type.is_non_owning_mut(irgen);

            if(type.is_non_owning(irgen))
                borrows.emplace_back(arg.get(),
                    is_mut_borrow ? std::visit(LValueBorrowResult{irgen}, arg->toVariant())
                    : std::visit(*this, arg->toVariant()));
        }
        validate_borrows(borrows, irgen);
        borrow_result_t out;
        if(callee_ty->sig.returnType.is_non_owning_mut(irgen))
        {
            for(auto& borrow : borrows)
            {
                out.insert(out.begin(),
                    std::make_move_iterator(borrow.second.begin()),
                    std::make_move_iterator(borrow.second.end()));
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

    BorrowResult::borrow_result_t BorrowResult::operator()(AsExpression* expr)
    {
        auto dst_ty = std::visit(ExpressionTypeChecker{irgen}, expr->toVariant());
        std::array<std::pair<Expression*, borrow_result_t>, 1> borrows = {
            {
                {expr,  std::visit(*this, expr->expr->toVariant())}
            }
        };
        validate_borrows(borrows, irgen);
        if(dst_ty->is_non_owning(irgen)) return borrows[0].second;
        return {};
    }

    BorrowResult::borrow_result_t BorrowResult::operator()(SpawnExpression* exr)
    {
        // arguments passed into spawn must be fully owning (i.e fibers don't borrow)
        auto res = doCall(reinterpret_cast<CallOperation*>(exr->call_expr.get()));
        if (!res.empty()) irgen->error(Error(exr, "Spawn expression must be fully owning"));
        return borrow_result_t();
    }

    void validate_expression_borrows(Expression* expr, IRGenerator* irgen)
    {
        validate_borrows({{{expr, std::visit(BorrowResult{irgen}, expr->toVariant())}}}, irgen);
    }
}
