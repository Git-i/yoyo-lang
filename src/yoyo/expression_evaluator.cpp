#include "ir_gen.h"
#include "fn_type.h"
namespace Yoyo
{
    /// IMPORTANT:
    /// I don't know where to place this, but all structural types are pointers
    llvm::Value* ExpressionEvaluator::doAssign(llvm::Value* lhs, llvm::Value* rhs, const Type& left_type, const Type& right_type)
    {
        if(!left_type.is_lvalue) {irgen->error(); return nullptr;}
        if(left_type.is_primitive())
        {
            if(!left_type.is_equal(right_type)) {irgen->error(); return nullptr;}
            return irgen->builder->CreateStore(rhs, lhs);
        }
        //Copy for non primitives is memberwise, but can be explicitly
        //overloaded with the __copy method for classes
        //__copy is always (this: inout, other: in Other_ty) -> void
        //copy is mangled as __class__<source>____copy_with__class__<dest>
        auto l_data = irgen->findType(left_type.name);
        if(!l_data) {irgen->error(); return nullptr;}
        std::string cp_name = std::get<0>(*l_data) + "__copy_with";
        auto r_data = irgen->findType(right_type.name);
        cp_name += std::get<0>(*r_data);
        auto cp_fn = irgen->code->getFunction(cp_name);
        if(cp_fn)
        {
            //if right is a structure it's a pointer, otherwise it is a value
            irgen->builder->CreateCall(cp_fn, {lhs, rhs});
        }
        else
        {
            //member wise copy is generated by default for same type
            if(!left_type.is_equal(right_type)) {irgen->error(); return nullptr;}
            llvm::Type* llvm_t = irgen->ToLLVMType(left_type, false);
            auto as_class = left_type.get_decl_if_class(irgen);
            size_t idx = 0;
            auto zero = llvm::ConstantInt::get(llvm::Type::getInt64Ty(irgen->context), 0);
            for(auto& var : as_class->vars)
            {
                auto mem_idx = llvm::ConstantInt::get(llvm::Type::getInt64Ty(irgen->context), idx);
                llvm::Value* mem_wise_rhs = irgen->builder->CreateGEP(llvm_t, rhs, {zero, mem_idx});
                auto mem_wise_type = irgen->ToLLVMType(var.type, false);
                if(var.type.is_primitive()) mem_wise_rhs = irgen->builder->CreateLoad(mem_wise_type, mem_wise_rhs);
                doAssign(lhs, rhs, var.type, var.type);
                idx++;
            }
        }
        return nullptr;
    }
    llvm::Value* ExpressionEvaluator::doDot(Expression* lhs, Expression* rhs, const Type& left_type)
    {
        if(auto cls = left_type.get_decl_if_class(irgen))
        {
            if(auto* name_expr = dynamic_cast<NameExpression*>(rhs))
            {
                std::string name(name_expr->token.text);
                if(auto var = std::ranges::find_if(cls->vars, [&name](ClassVariable& v)
                {
                    return name == v.name;
                }); var != cls->vars.end())
                {
                    auto llvm_t = irgen->ToLLVMType(left_type, false);
                    auto out_t = irgen->ToLLVMType(var->type, false);
                    llvm::Value* ptr;
                    auto idx = llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), std::distance(var, cls->vars.begin()));
                    auto zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 0);

                    auto lalloc = std::visit(*this, lhs->toVariant());
                    ptr = irgen->builder->CreateGEP(llvm_t, lalloc, {zero, idx});

                    if(var->type.is_primitive()) return irgen->builder->CreateLoad(out_t, ptr, var->name);
                    return ptr;
                }
            }
        }
    }
    llvm::Value* ExpressionEvaluator::doAddition(
        llvm::Value* lhs,
        llvm::Value* rhs,
        const Type& left_type,
        const Type& right_type) const
    {
        if(left_type.is_integral())
            return irgen->builder->CreateAdd(lhs, rhs, "addtmp");
        if(left_type.is_floating_point())
            return irgen->builder->CreateFAdd(lhs, rhs, "addtmp");
        return nullptr;
    }
    llvm::Value* ExpressionEvaluator::doMinus(
        llvm::Value* lhs,
        llvm::Value* rhs,
        const Type& left_type,
        const Type& right_type) const
    {
        if(left_type.is_integral())
            return irgen->builder->CreateAdd(lhs, rhs, "addtmp");
        if(left_type.is_floating_point())
            return irgen->builder->CreateFAdd(lhs, rhs, "addtmp");
        return nullptr;
    }
    llvm::Value* ExpressionEvaluator::doMult(
        llvm::Value* lhs,
        llvm::Value* rhs,
        const Type& left_type,
        const Type& right_type) const
    {
        if(left_type.is_integral())
            return irgen->builder->CreateMul(lhs, rhs, "multmp");
        if(left_type.is_floating_point())
            return irgen->builder->CreateFMul(lhs, rhs, "multmp");
        return nullptr;
    }
    llvm::Value* ExpressionEvaluator::doDiv(
        llvm::Value* lhs,
        llvm::Value* rhs,
        const Type& left_type,
        const Type& right_type) const
    {
        if(left_type.is_signed_integral())
            return irgen->builder->CreateSDiv(lhs, rhs, "divtmp");
        if(left_type.is_unsigned_integral())
            return irgen->builder->CreateUDiv(lhs, rhs, "divtmp");
        if(left_type.is_floating_point())
            return irgen->builder->CreateFDiv(lhs, rhs, "divtmp");
        return nullptr;
    }
    llvm::Value* ExpressionEvaluator::doRem(
        llvm::Value* lhs,
        llvm::Value* rhs,
        const Type& left_type,
        const Type& right_type) const
    {
        if(left_type.is_signed_integral())
            return irgen->builder->CreateSRem(lhs, rhs, "divtmp");
        if(left_type.is_unsigned_integral())
            return irgen->builder->CreateURem(lhs, rhs, "divtmp");
        return nullptr;
    }
    llvm::Value* ExpressionEvaluator::doCmp(
        ComparisonPredicate p,
        llvm::Value* lhs,
        llvm::Value* rhs,
        const Type& left_type,
        const Type& right_type) const
    {
        llvm::CmpInst::Predicate pred{};
        if(left_type.is_signed_integral())
        {
            switch(p)
            {
            case EQ: pred = llvm::CmpInst::ICMP_EQ; break;
            case GT: pred = llvm::CmpInst::ICMP_SGT; break;
            case LT: pred = llvm::CmpInst::ICMP_SLT; break;
            case EQ_GT: pred = llvm::CmpInst::ICMP_SGE; break;
            case EQ_LT: pred = llvm::CmpInst::ICMP_SLE; break;
            case NE: pred = llvm::CmpInst::ICMP_NE; break;
            }
            return irgen->builder->CreateICmp(pred, lhs, rhs, "cmptmp");
        }
        if(left_type.is_unsigned_integral())
        {
            switch(p)
            {
            case EQ: pred = llvm::CmpInst::ICMP_EQ; break;
            case GT: pred = llvm::CmpInst::ICMP_UGT; break;
            case LT: pred = llvm::CmpInst::ICMP_ULT; break;
            case EQ_GT: pred = llvm::CmpInst::ICMP_UGE; break;
            case EQ_LT: pred = llvm::CmpInst::ICMP_ULE; break;
            case NE: pred = llvm::CmpInst::ICMP_NE; break;
            }
            return irgen->builder->CreateICmp(pred, lhs, rhs, "cmptmp");
        }
        if(left_type.is_floating_point())
        {
            switch(p)
            {
            case EQ: pred = llvm::CmpInst::FCMP_OEQ; break;
            case GT: pred = llvm::CmpInst::FCMP_OGT; break;
            case LT: pred = llvm::CmpInst::FCMP_OLT; break;
            case EQ_GT: pred = llvm::CmpInst::FCMP_OGE; break;
            case EQ_LT: pred = llvm::CmpInst::FCMP_OLE; break;
            case NE: pred = llvm::CmpInst::FCMP_ONE; break;
            }
            return irgen->builder->CreateFCmp(pred, lhs, rhs, "cmptmp");
        }
        return nullptr;
    }

    llvm::Value* ExpressionEvaluator::LValueEvaluator::operator()(NameExpression*nm)
    {
        std::string name(nm->token.text);
        for(size_t i = irgen->variables.size(); i > 0; --i)
        {
            size_t idx = i - 1;
            if(auto var = irgen->variables[idx].find(name); var != irgen->variables[idx].end())
            {
                return var->second.first;
            }
        }
    }

    llvm::Value* ExpressionEvaluator::LValueEvaluator::operator()(BinaryOperation*)
    {
        //TODO
    }

    llvm::Value* ExpressionEvaluator::LValueEvaluator::operator()(Expression*)
    {
        //TODO
    }

    llvm::Value* ExpressionEvaluator::operator()(IntegerLiteral* lit) {
        auto t = std::visit(ExpressionTypeChecker{irgen}, std::variant<IntegerLiteral*>{lit});
        if(t->is_signed_integral())
        {
            const auto ll = std::stoll(std::string{lit->token.text});
            return llvm::ConstantInt::getSigned(irgen->ToLLVMType(*t, false), ll);
        }
        const auto ul = std::stoull(std::string{lit->token.text});
        return llvm::ConstantInt::get(irgen->ToLLVMType(*t, false), ul);
    }
    llvm::Value* ExpressionEvaluator::operator()(BooleanLiteral* lit)
    {
        if(lit->token.text == "true") return llvm::ConstantInt::getTrue(irgen->context);
        return llvm::ConstantInt::getFalse(irgen->context);
    }
    llvm::Value* ExpressionEvaluator::operator()(TupleLiteral*) {}
    llvm::Value* ExpressionEvaluator::operator()(ArrayLiteral*) {}
    llvm::Value* ExpressionEvaluator::operator()(RealLiteral* lit)
    {
        return llvm::ConstantFP::get(llvm::Type::getDoubleTy(irgen->context), lit->token.text);
    }
    llvm::Value* ExpressionEvaluator::operator()(StringLiteral*) {}
    llvm::Value* ExpressionEvaluator::operator()(NameExpression* nm)
    {
        std::string name(nm->token.text);
        for(size_t i = irgen->variables.size(); i > 0; --i)
        {
            size_t idx = i - 1;
            if(auto var = irgen->variables[idx].find(name); var != irgen->variables[idx].end())
            {
                auto alloc = llvm::dyn_cast_or_null<llvm::AllocaInst>(var->second.first);
                if(alloc)
                {
                    if(var->second.second->type->is_primitive()) return irgen->builder->CreateLoad(alloc->getAllocatedType(), var->second.first, name);
                }
                return var->second.first;
            }
        }
        if(auto fn = irgen->code->getFunction(name))
        {
            return fn;
        }
        return nullptr;
    }
    llvm::Value* ExpressionEvaluator::operator()(PrefixOperation*) {}
    llvm::Value* ExpressionEvaluator::operator()(BinaryOperation* op)
    {
        auto type_checker = ExpressionTypeChecker{irgen};
        if(!std::visit(type_checker, op->toVariant())) return nullptr;

        auto l_as_var = op->lhs->toVariant();
        auto r_as_var = op->rhs->toVariant();

        llvm::Value* lhs;
        llvm::Value* rhs;
        if(op->op.type != TokenType::Dot)
        {
            lhs = std::visit(*this, l_as_var);
            rhs = std::visit(*this, r_as_var);
        }

        auto left_t = std::visit(type_checker, l_as_var);
        auto right_t = std::visit(type_checker, r_as_var);

        switch(op->op.type)
        {
            using enum TokenType;
        case Plus: return doAddition(lhs, rhs, *left_t, *right_t);
        case Minus: return doMinus(lhs, rhs, *left_t, *right_t);
        case Star: return doMult(lhs, rhs, *left_t, *right_t);
        case Slash: return doDiv(lhs, rhs, *left_t, *right_t);
        case Percent: return doRem(lhs, rhs, *left_t, *right_t);
        case Greater: return doCmp(GT, lhs, rhs, *left_t, *right_t);
        case Less: return doCmp(LT, lhs, rhs, *left_t, *right_t);
        case GreaterEqual: return doCmp(EQ_GT, lhs, rhs, *left_t, *right_t);
        case LessEqual: return doCmp(EQ_LT, lhs, rhs, *left_t, *right_t);
        case BangEqual: return doCmp(NE, lhs, rhs, *left_t, *right_t);
        case DoubleEqual: return doCmp(EQ, lhs, rhs, *left_t, *right_t);
        case Dot: return doDot(op->lhs.get(), op->rhs.get(), *left_t);
        }
        return nullptr;
    }
    llvm::Value* ExpressionEvaluator::operator()(GroupingExpression* op)
    {
        return std::visit(*this, op->expr->toVariant());
    }
    llvm::Value* ExpressionEvaluator::operator()(LogicalOperation*) {}
    llvm::Value* ExpressionEvaluator::operator()(PostfixOperation*) {}
    llvm::Value* ExpressionEvaluator::operator()(CallOperation* op)
    {
        auto t = std::visit(ExpressionTypeChecker{irgen}, op->callee->toVariant());
        if(!t || !t->is_function()) return nullptr;
        auto fn = reinterpret_cast<FunctionType&>(*t);
        if(fn.is_bound)
        {
            //callee is a binary dot expr
            auto expr = reinterpret_cast<BinaryOperation*>(op->callee.get());
            auto left_t = std::visit(ExpressionTypeChecker{irgen}, expr->lhs->toVariant());
            if(auto cls = left_t->get_decl_if_class(irgen))
            {
                //handle member functions
                if(auto rhs = dynamic_cast<NameExpression*>(expr->rhs.get()))
                {
                    std::string name(rhs->token.text);
                    if(auto var = std::ranges::find_if(cls->methods, [&name](ClassMethod& m)
                        {
                            return name == m.name;
                        }); var != cls->methods.end())
                    {
                        auto decl = reinterpret_cast<FunctionDeclaration*>(var->function_decl.get());
                        if(auto found = irgen->findType(left_t->name))
                        {
                            llvm::Value* return_value = nullptr;
                            auto function_name  = std::get<0>(*found) + name;
                            auto callee = irgen->code->getFunction(function_name);
                            bool uses_sret = callee->hasStructRetAttr();
                            std::vector<llvm::Value*> args(op->arguments.size() + 1 + uses_sret); // +1 because its bound
                            if(uses_sret)
                            {
                                auto ret_t = irgen->ToLLVMType(decl->signature.returnType, false);
                                return_value = irgen->Alloca("return_temp", ret_t);
                                args[0] = return_value;
                            }
                            args[uses_sret] = std::visit(*this, expr->lhs->toVariant());
                            std::ranges::transform(op->arguments, args.begin() + 1 + uses_sret, [this](std::unique_ptr<Expression>& v)
                            {
                                return std::visit(*this, v->toVariant());
                            });
                            auto call_val = irgen->builder->CreateCall(callee, args);
                            if(!uses_sret) return_value = call_val;
                            return return_value;
                        }
                        return nullptr;
                    }
                }
            }
            llvm::Value* return_value;
            auto* callee = llvm::dyn_cast_or_null<llvm::Function>(std::visit(*this, expr->rhs->toVariant()));
            if(!callee) return nullptr;
            bool uses_sret = callee->hasStructRetAttr();
            std::vector<llvm::Value*> args(op->arguments.size() + 1 + uses_sret);
            if(uses_sret)
            {
                auto ret_t = irgen->ToLLVMType(fn.sig.returnType, false);
                return_value = irgen->Alloca("return_temp", ret_t);
                args[0] = return_value;
            }
            args[uses_sret] = std::visit(*this, expr->lhs->toVariant());
            std::ranges::transform(op->arguments, args.begin() + 1 + uses_sret, [this](std::unique_ptr<Expression>& v)
            {
                return std::visit(*this, v->toVariant());
            });
            auto call_val = irgen->builder->CreateCall(callee->getFunctionType(), callee, args);
            if(!uses_sret) return_value = call_val;
            return return_value;
        }
        llvm::Value* return_value;
        auto callee_val = std::visit(*this, op->callee->toVariant());
        auto* callee = llvm::dyn_cast_or_null<llvm::Function>(callee_val);
        if(!callee) return nullptr;
        bool uses_sret = callee->hasStructRetAttr();
        std::vector<llvm::Value*> args(op->arguments.size() + uses_sret);
        if(uses_sret)
        {
            auto ret_t = irgen->ToLLVMType(fn.sig.returnType, false);
            return_value = irgen->Alloca("return_temp", ret_t);
            args[0] = return_value;
        }
        std::ranges::transform(op->arguments, args.begin() + uses_sret, [this](std::unique_ptr<Expression>& v)
        {
            return std::visit(*this, v->toVariant());
        });
        auto call_val = irgen->builder->CreateCall(callee->getFunctionType(), callee, args);
        if(!uses_sret) return_value = call_val;
        return return_value;
    }
    llvm::Value* ExpressionEvaluator::operator()(SubscriptOperation*) {}
}