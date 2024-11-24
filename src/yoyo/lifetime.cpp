#pragma once
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







}