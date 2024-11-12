#include <cmath>
#include <csignal>
#include <llvm/Support/Error.h>

#include "ir_gen.h"
#include "fn_type.h"
namespace Yoyo
{
    auto findType(const std::string& name, IRGenerator* irgen, Module* mod) ->
        std::tuple<std::string, llvm::StructType*, std::unique_ptr<ClassDeclaration>>*
    {
        if(mod == irgen->module) return irgen->findType(name);
        if(mod->classes.contains(name))
        {
            return &mod->classes.at(name);
        }
        return nullptr;
    }
    int64_t getIntMinOf(const Type& type)
    {
        if(type.is_signed_integral())
        {
            switch (*type.integer_width())
            {
            case 8: return std::numeric_limits<int8_t>::min();
            case 16: return std::numeric_limits<int16_t>::min();
            case 32: return std::numeric_limits<int32_t>::min();
            case 64: return std::numeric_limits<int64_t>::min();
                default: return 0;/*unreachable*/
            }
        }
        if(type.is_unsigned_integral())
        {
            switch (*type.integer_width())
            {
            case 8: return std::numeric_limits<uint8_t>::min();
            case 16: return std::numeric_limits<uint16_t>::min();
            case 32: return std::numeric_limits<uint32_t>::min();
            case 64: return std::numeric_limits<uint64_t>::min();
            default: return 0;/*unreachable*/
            }
        }
        if(type.is_floating_point())
        {
            if(*type.float_width() == 32) return -std::pow(2, 24);
            if(*type.float_width() == 64) return -std::pow(2, 53);
        }
        return 0;
    }
    uint64_t getIntMaxOf(const Type& type)
    {
        if(type.is_signed_integral())
        {
            switch (*type.integer_width())
            {
            case 8: return std::numeric_limits<int8_t>::max();
            case 16: return std::numeric_limits<int16_t>::max();
            case 32: return std::numeric_limits<int32_t>::max();
            case 64: return std::numeric_limits<int64_t>::max();
            default: return 0;/*unreachable*/
            }
        }
        if(type.is_unsigned_integral())
        {
            switch (*type.integer_width())
            {
            case 8: return std::numeric_limits<uint8_t>::max();
            case 16: return std::numeric_limits<uint16_t>::max();
            case 32: return std::numeric_limits<uint32_t>::max();
            case 64: return std::numeric_limits<uint64_t>::max();
            default: return 0;/*unreachable*/
            }
        }
        if(type.is_floating_point())
        {
            if(*type.float_width() == 32) return std::pow(2, 24);
            if(*type.float_width() == 64) return std::pow(2, 53);
        }
        return 0;
    }
    double getFloatMinOf(const Type& type)
    {
        if(type.is_floating_point())
        {
            if(*type.float_width() == 32) return std::numeric_limits<float>::min();
            if(*type.float_width() == 64) return std::numeric_limits<double>::min();
        }
        return 0;
    }

    double getFloatMaxOf(const Type& type)
    {
        if(type.is_floating_point())
        {
            if(*type.float_width() == 32) return std::numeric_limits<float>::max();
            if(*type.float_width() == 64) return std::numeric_limits<double>::max();
        }
        return 0;
    }
    llvm::Value* implicitConvert(llvm::Value* val, const Type& src, const Type& dst, IRGenerator* irgen)
    {
        if(!dst.is_assignable_from(src)) {irgen->error(); return nullptr;}
        llvm::Type* dest = irgen->ToLLVMType(dst, false);
        if(src.name == "ilit")
        {
            auto as_int = llvm::dyn_cast<llvm::ConstantInt>(val);
            int64_t min_bound = getIntMinOf(dst);
            uint64_t max_bound = getIntMaxOf(dst);
            if(dst.is_integral())
            {
                if(as_int->isNegative())
                {
                    if(as_int->getSExtValue() >= min_bound && as_int->getSExtValue() <= max_bound)
                        return irgen->builder->CreateSExtOrTrunc(val, dest);
                    irgen->error();
                    return nullptr;
                }
                if(as_int->getZExtValue() <= max_bound)
                    return irgen->builder->CreateZExtOrTrunc(val, dest);
                irgen->error();
                return nullptr;
            }
            if(dst.is_floating_point())
            {
                if(as_int->isNegative())
                {
                    if(as_int->getSExtValue() >= min_bound && as_int->getSExtValue() <= max_bound)
                        return irgen->builder->CreateSIToFP(val, dest);
                    irgen->error();
                    return nullptr;
                }
                if(as_int->getZExtValue() <= max_bound)
                    return irgen->builder->CreateUIToFP(val, dest);
                irgen->error();
                return nullptr;
            }
        }
        if(src.name == "flit")
        {
            auto as_float = llvm::dyn_cast<llvm::ConstantFP>(val);
            double min_bound = getFloatMinOf(dst);
            double max_bound = getFloatMaxOf(dst);
            double value = as_float->getValue().convertToDouble();
            if(dst.is_floating_point())
            {
                if(*dst.float_width() == 64) return val;
                if(value >= min_bound && value <= max_bound)
                    return irgen->builder->CreateFPTrunc(val, dest);
            }
        }
        if(dst.is_unsigned_integral())
        {
            if(!dst.is_equal(src)) val = irgen->builder->CreateZExt(val, dest, "assign_zext");
        }
        else if(dst.is_signed_integral())
        {
            if(!dst.is_equal(src))
                if(src.is_signed_integral()) val = irgen->builder->CreateSExt(val, dest, "assign_sext");
                else if(src.is_unsigned_integral()) val = irgen->builder->CreateZExt(val, dest, "assign_zext");
        }
        else if(dst.is_floating_point())
        {
            if(!dst.is_equal(src))
                if(src.is_floating_point()) val = irgen->builder->CreateFPExt(val, dest, "assign_fpext");
                else if(src.is_unsigned_integral()) val = irgen->builder->CreateUIToFP(val, dest, "assign_uitofp");
                else if(src.is_signed_integral()) val = irgen->builder->CreateSIToFP(val, dest, "assign_uitofp");
        }
        return val;
    }
    /// IMPORTANT:\n
    /// I don't know where to place this, but all structural types are pointers
    llvm::Value* ExpressionEvaluator::doAssign(llvm::Value* lhs, llvm::Value* rhs, const Type& left_type, const Type& right_type)
    {
        if(!left_type.is_assignable_from(right_type)) {irgen->error(); return nullptr;}
        if(!left_type.is_mutable) {irgen->error(); return nullptr;}
        if(left_type.is_primitive())
        {
            return irgen->builder->CreateStore(implicitConvert(rhs, right_type, left_type, irgen), lhs);
        }
        if(left_type.is_tuple())
        {
            auto as_llvm = irgen->ToLLVMType(left_type, false);
            if(!left_type.is_equal(right_type)) {irgen->error(); return nullptr;}
            auto zero_const = llvm::ConstantInt::get(irgen->builder->getInt32Ty(), 0);
            size_t idx = 0;
            for(auto& sub : left_type.subtypes)
            {
                auto idx_const = llvm::ConstantInt::get(irgen->builder->getInt32Ty(), idx);
                auto ptr = irgen->builder->CreateGEP(as_llvm, lhs, {zero_const, idx_const});
                Type tp = sub;
                tp.is_mutable = true;
                doAssign(ptr, rhs, tp, tp);
                idx++;
            }
            return nullptr;
        }
        if(left_type.is_enum())
        {
            return irgen->builder->CreateStore(rhs, lhs);
        }
        if(left_type.is_optional())
        {
            auto opt_ty = irgen->ToLLVMType(left_type, false);
            //opt is {data, bool}
            //TODO destroy data if exists
            if(right_type.name == "__null")
            {
                auto has_value = irgen->builder->CreateStructGEP(opt_ty, lhs, 1);
                return irgen->builder->CreateStore(llvm::ConstantInt::getFalse(irgen->context), has_value);
            }
            //subtype implicit conversion
            if(!right_type.is_equal(left_type))
            {
                Type tp = left_type.subtypes[0];
                tp.is_mutable = true;
                doAssign(irgen->builder->CreateStructGEP(opt_ty, lhs, 0), rhs, tp, tp);
                return irgen->builder->CreateStore(llvm::ConstantInt::getTrue(irgen->context), irgen->builder->CreateStructGEP(opt_ty, lhs, 1));
            }
            auto right_has_value = irgen->builder->CreateLoad(llvm::Type::getInt1Ty(irgen->context),
                irgen->builder->CreateStructGEP(opt_ty, rhs, 1));
            auto fn = irgen->builder->GetInsertBlock()->getParent();
            auto r_is_valid = llvm::BasicBlock::Create(irgen->context, "opt_valid_assign", fn, irgen->returnBlock);
            auto r_is_invalid = llvm::BasicBlock::Create(irgen->context, "opt_invalid_assign", fn, irgen->returnBlock);
            auto opt_assign_cont = llvm::BasicBlock::Create(irgen->context, "opt_assign_cont", fn, irgen->returnBlock);
            irgen->builder->CreateCondBr(right_has_value, r_is_valid, r_is_invalid);

            irgen->builder->SetInsertPoint(r_is_valid);
            auto value_rhs = irgen->builder->CreateStructGEP(opt_ty, rhs, 0);
            if(!right_type.subtypes[0].should_sret())
            {
                auto subtype = llvm::dyn_cast<llvm::StructType>(opt_ty)->getElementType(0);
                value_rhs = irgen->builder->CreateLoad(subtype, value_rhs);
            }
            Type tp = left_type.subtypes[0];
            tp.is_mutable = true;
            doAssign(irgen->builder->CreateStructGEP(opt_ty, lhs, 0), value_rhs, tp, tp);
            irgen->builder->CreateBr(opt_assign_cont);

            irgen->builder->SetInsertPoint(r_is_invalid);
            auto has_value = irgen->builder->CreateStructGEP(opt_ty, lhs, 1);
            irgen->builder->CreateStore(llvm::ConstantInt::getFalse(irgen->context), has_value);
            irgen->builder->CreateBr(opt_assign_cont);

            irgen->builder->SetInsertPoint(opt_assign_cont);
            return nullptr;
        }
        //Copy for non primitives is memberwise, but can be explicitly
        //overloaded with the __copy method for classes
        //__copy is always (this: inout, other: in This) -> void
        //copy is mangled as __class__<source>____copy
        auto l_data = findType(left_type.name, irgen, left_type.module);
        if(!l_data) {irgen->error(); return nullptr;}
        std::string cp_name = std::get<0>(*l_data) + "__copy";
        auto cp_fn = irgen->code->getFunction(cp_name);
        if(cp_fn)
        {
            irgen->builder->CreateCall(cp_fn, {lhs, rhs});
        }
        else
        {
            //member wise copy is generated by default for same type
            if(!left_type.is_equal(right_type)) {irgen->error(); return nullptr;}
            llvm::Type* llvm_t = irgen->ToLLVMType(left_type, false);
            auto as_class = left_type.get_decl_if_class(irgen);
            size_t idx = 0;
            auto zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 0);
            for(auto& var : as_class->vars)
            {
                auto mem_idx = llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), idx);
                llvm::Value* mem_wise_rhs = irgen->builder->CreateGEP(llvm_t, rhs, {zero, mem_idx});
                llvm::Value* mem_wise_lhs = irgen->builder->CreateGEP(llvm_t, lhs, {zero, mem_idx});
                auto mem_wise_type = irgen->ToLLVMType(var.type, false);
                if(!var.type.should_sret()) mem_wise_rhs = irgen->builder->CreateLoad(mem_wise_type, mem_wise_rhs);
                auto as_lvalue = var.type;
                as_lvalue.is_mutable = true;
                doAssign(mem_wise_lhs, mem_wise_rhs, as_lvalue, var.type);
                idx++;
            }
        }
        return nullptr;
    }
    llvm::Value* ExpressionEvaluator::doDot(Expression* lhs, Expression* rhs, const Type& left_type, bool load_primitive)
    {
        if(left_type.is_tuple())
        {
            if(auto idx = dynamic_cast<IntegerLiteral*>(rhs))
            {
                auto idx_int = std::stoul(std::string{idx->text});
                auto out_type = left_type.subtypes[idx_int];

                auto llvm_t = irgen->ToLLVMType(left_type, false);
                auto llvm_idx = llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), idx_int);
                auto zero_const = llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 0);
                auto left_ptr = std::visit(*this, lhs->toVariant());
                auto ptr = irgen->builder->CreateGEP(llvm_t, left_ptr, {zero_const, llvm_idx});
                if(load_primitive && out_type.is_primitive())
                    return irgen->builder->CreateLoad(irgen->ToLLVMType(out_type, false), ptr);
                return ptr;
            }
        }
        if(auto cls = left_type.get_decl_if_class(irgen))
        {
            if(auto* name_expr = dynamic_cast<NameExpression*>(rhs))
            {
                std::string name(name_expr->text);
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

                    if(load_primitive && var->type.is_primitive()) return irgen->builder->CreateLoad(out_t, ptr, var->name);
                    return ptr;
                }
            }
        }
        return nullptr;
    }


    /*
     * Implicit conversion is not done in expression except literals
     * ilit op ilit = ilit
     * ilit op flit = flit
     * ilit op int = int
     * ilit op float = float
     *
     * flit op float = float
     * flit op flit = flit
     */
    void convertLiterals(llvm::Value** lhs,
        llvm::Value** rhs,
        const Type& left_type,
        const Type& right_type, IRGenerator* irgen)
    {
        if(left_type.name == "ilit")
        {
            auto as_int = llvm::dyn_cast<llvm::ConstantInt>(*lhs);
            if(right_type.name == "ilit") return;
            if(right_type.name == "filt")
            {
                if(as_int->isNegative()) *lhs = irgen->builder->CreateSIToFP(*lhs, llvm::Type::getDoubleTy(irgen->context));
                else *lhs = irgen->builder->CreateUIToFP(*lhs, llvm::Type::getDoubleTy(irgen->context));
                return;
            }
            *lhs = implicitConvert(*lhs, left_type, right_type, irgen);
            return;
        }
        if(left_type.name == "flit")
        {
            if(right_type.name == "flit") return;
            *lhs = implicitConvert(*lhs, left_type, right_type, irgen);
            return;
        }
        if(right_type.name == "ilit" || right_type.name == "flit")
            convertLiterals(rhs, lhs, right_type, left_type, irgen);
    }
    llvm::Value* ExpressionEvaluator::doAddition(
        llvm::Value* lhs,
        llvm::Value* rhs,
        const Type& left_type,
        const Type& right_type,
        const Type& result_type) const
    {
        //integral result can come from overloaded operators
        if(result_type.is_integral() && left_type.is_integral())
        {
            convertLiterals(&lhs, &rhs, left_type, right_type, irgen);
            return irgen->builder->CreateAdd(lhs, rhs, "addtmp");
        }
        //left can be an integer literal
        if(result_type.is_floating_point() && (left_type.is_integral() || left_type.is_floating_point()))
        {
            convertLiterals(&lhs, &rhs, left_type, right_type, irgen);
            return irgen->builder->CreateFAdd(lhs, rhs, "addtmp");
        }
        return nullptr;
    }
    llvm::Value* ExpressionEvaluator::doMinus(
        llvm::Value* lhs,
        llvm::Value* rhs,
        const Type& left_type,
        const Type& right_type,
        const Type& result_type) const
    {
        if(result_type.is_integral() && left_type.is_integral())
        {
            convertLiterals(&lhs, &rhs, left_type, right_type, irgen);
            return irgen->builder->CreateAdd(lhs, rhs, "addtmp");
        }
        if(result_type.is_floating_point() && (left_type.is_integral() || left_type.is_floating_point()))
        {
            convertLiterals(&lhs, &rhs, left_type, right_type, irgen);
            return irgen->builder->CreateFAdd(lhs, rhs, "addtmp");
        }
        return nullptr;
    }
    llvm::Value* ExpressionEvaluator::doMult(
        llvm::Value* lhs,
        llvm::Value* rhs,
        const Type& left_type,
        const Type& right_type,
        const Type& result_type) const
    {
        if(result_type.is_integral() && left_type.is_integral())
        {
            convertLiterals(&lhs, &rhs, left_type, right_type, irgen);
            return irgen->builder->CreateMul(lhs, rhs, "multmp");
        }
        if(result_type.is_floating_point() && (left_type.is_integral() || left_type.is_floating_point()))
        {
            convertLiterals(&lhs, &rhs, left_type, right_type, irgen);
            return irgen->builder->CreateFMul(lhs, rhs, "multmp");
        }
        return nullptr;
    }
    llvm::Value* ExpressionEvaluator::doDiv(
        llvm::Value* lhs,
        llvm::Value* rhs,
        const Type& left_type,
        const Type& right_type,
        const Type& result_type) const
    {
        if(result_type.is_signed_integral() && left_type.is_integral())
        {
            convertLiterals(&lhs, &rhs, left_type, right_type, irgen);
            return irgen->builder->CreateSDiv(lhs, rhs, "divtmp");
        }
        if(result_type.is_unsigned_integral() && left_type.is_integral())
        {
            convertLiterals(&lhs, &rhs, left_type, right_type, irgen);
            return irgen->builder->CreateUDiv(lhs, rhs, "divtmp");
        }
        if(result_type.is_integral()) //integer literal
        {
            convertLiterals(&lhs, &rhs, left_type, right_type, irgen);
            auto lhs_int = llvm::dyn_cast<llvm::ConstantInt>(lhs);
            auto rhs_int = llvm::dyn_cast<llvm::ConstantInt>(rhs);
            if(lhs_int->isNegative() || rhs_int->isNegative()) return irgen->builder->CreateSDiv(lhs, rhs, "divtmp");
            return irgen->builder->CreateUDiv(lhs, rhs, "divtmp");
        }
        if(result_type.is_floating_point() && (left_type.is_integral() || left_type.is_floating_point()))
        {
            convertLiterals(&lhs, &rhs, left_type, right_type, irgen);
            return irgen->builder->CreateFDiv(lhs, rhs, "divtmp");
        }
        return nullptr;
    }
    llvm::Value* ExpressionEvaluator::doRem(
        llvm::Value* lhs,
        llvm::Value* rhs,
        const Type& left_type,
        const Type& right_type,
        const Type& result_type) const
    {
        if(left_type.is_signed_integral()  && left_type.is_integral())
        {
            convertLiterals(&lhs, &rhs, left_type, right_type, irgen);
            return irgen->builder->CreateSRem(lhs, rhs, "divtmp");
        }
        if(left_type.is_unsigned_integral() && left_type.is_integral())
        {
            convertLiterals(&lhs, &rhs, left_type, right_type, irgen);
            return irgen->builder->CreateURem(lhs, rhs, "divtmp");
        }
        if(result_type.is_integral())
        {
            convertLiterals(&lhs, &rhs, left_type, right_type, irgen);
            auto lhs_int = llvm::dyn_cast<llvm::ConstantInt>(lhs);
            auto rhs_int = llvm::dyn_cast<llvm::ConstantInt>(rhs);
            if(lhs_int->isNegative() || rhs_int->isNegative()) return irgen->builder->CreateSRem(lhs, rhs, "divtmp");
            return irgen->builder->CreateURem(lhs, rhs, "divtmp");
        }
        return nullptr;
    }
    llvm::Value* ExpressionEvaluator::doCmp(
        ComparisonPredicate p,
        llvm::Value* lhs,
        llvm::Value* rhs,
        const Type& left_type,
        const Type& right_type,
        const Type& result_type) const
    {
        llvm::CmpInst::Predicate pred{};
        auto get_int_cmp_pred = [](const bool is_signed, ComparisonPredicate p)
        {
            if(is_signed)
                switch(p)
                {
                case EQ: return llvm::CmpInst::ICMP_EQ;
                case GT: return llvm::CmpInst::ICMP_SGT;
                case LT: return llvm::CmpInst::ICMP_SLT;
                case EQ_GT: return llvm::CmpInst::ICMP_SGE;
                case EQ_LT: return llvm::CmpInst::ICMP_SLE;
                case NE: return llvm::CmpInst::ICMP_NE;
                }
            else
                switch(p)
                {
                case EQ: return llvm::CmpInst::ICMP_EQ;
                case GT: return llvm::CmpInst::ICMP_UGT;
                case LT: return llvm::CmpInst::ICMP_ULT;
                case EQ_GT: return llvm::CmpInst::ICMP_UGE;
                case EQ_LT: return llvm::CmpInst::ICMP_ULE;
                case NE: return llvm::CmpInst::ICMP_NE;
                }
            return llvm::CmpInst::BAD_ICMP_PREDICATE;
        };
        if(left_type.is_signed_integral() && right_type.is_signed_integral())
        {
            convertLiterals(&lhs, &rhs, left_type, right_type, irgen);
            pred = get_int_cmp_pred(true, p);
            return irgen->builder->CreateICmp(pred, lhs, rhs, "cmptmp");
        }
        if(right_type.is_unsigned_integral() && left_type.is_integral())
        {
            convertLiterals(&lhs, &rhs, left_type, right_type, irgen);
            pred = get_int_cmp_pred(false, p);
            return irgen->builder->CreateICmp(pred, lhs, rhs, "cmptmp");
        }
        if(left_type.is_integral() && right_type.is_integral())
        {
            convertLiterals(&lhs, &rhs, left_type, right_type, irgen);
            auto lhs_int = llvm::dyn_cast<llvm::ConstantInt>(lhs);
            auto rhs_int = llvm::dyn_cast<llvm::ConstantInt>(rhs);
            pred = get_int_cmp_pred(lhs_int->isNegative() || rhs_int->isNegative(), p);
            return irgen->builder->CreateICmp(pred, lhs, rhs, "divtmp");
        }
        if(left_type.is_floating_point() && (left_type.is_integral() || left_type.is_floating_point()))
        {
            convertLiterals(&lhs, &rhs, left_type, right_type, irgen);
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
        std::string name(nm->text);
        for(size_t i = irgen->variables.size(); i > 0; --i)
        {
            size_t idx = i - 1;
            if(auto var = irgen->variables[idx].find(name); var != irgen->variables[idx].end())
            {
                if(!var->second.second->is_mut) return nullptr;
                return var->second.first;
            }
        }
    }

    llvm::Value* ExpressionEvaluator::LValueEvaluator::operator()(BinaryOperation* bop)
    {
        //Dot operations can also be lvalue'd
        if(bop->op.type != TokenType::Dot) return nullptr;
        auto left_t = std::visit(ExpressionTypeChecker{irgen}, bop->lhs->toVariant());
        if(!left_t) return nullptr;
        if(!left_t->is_mutable) return nullptr;
        return ExpressionEvaluator{irgen}.doDot(bop->lhs.get(), bop->rhs.get(), *left_t, false);
    }

    llvm::Value* ExpressionEvaluator::LValueEvaluator::operator()(Expression*)
    {
        //TODO
    }

    llvm::Value* ExpressionEvaluator::operator()(IntegerLiteral* lit) {
        const auto ul = std::stoull(std::string{lit->text});
        return llvm::ConstantInt::get(llvm::Type::getInt64Ty(irgen->context), ul);
    }
    llvm::Value* ExpressionEvaluator::operator()(BooleanLiteral* lit)
    {
        if(lit->token.text == "true") return llvm::ConstantInt::getTrue(irgen->context);
        return llvm::ConstantInt::getFalse(irgen->context);
    }
    llvm::Value* ExpressionEvaluator::operator()(TupleLiteral* tup)
    {
        auto tuple_t = ExpressionTypeChecker{irgen, target}(tup);
        if(!tuple_t) { irgen->error(); return nullptr; }
        auto llvm_t = irgen->ToLLVMType(*tuple_t, false);
        auto tuple_tmp = irgen->Alloca("tuple_lit", llvm_t);
        auto zero_const = llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 0);
        size_t idx = 0;
        for(auto& expr: tup->elements)
        {
            auto idx_const = llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), idx);
            auto idx_ptr = irgen->builder->CreateGEP(llvm_t, tuple_tmp, {zero_const, idx_const}, "tuple_elem");
            auto type = std::visit(ExpressionTypeChecker{irgen}, expr->toVariant());
            tuple_t->subtypes[idx].is_mutable = true;
            doAssign(idx_ptr, std::visit(*this, expr->toVariant()), tuple_t->subtypes[idx], *type);
            idx++;
        }
        return tuple_tmp;
    }
    llvm::Value* ExpressionEvaluator::operator()(ArrayLiteral*) {}
    llvm::Value* ExpressionEvaluator::operator()(RealLiteral* lit)
    {
        return llvm::ConstantFP::get(llvm::Type::getDoubleTy(irgen->context), lit->token.text);
    }
    //str is of type {ptr, int64, int64} for buffer, size, capacity
    llvm::Value* ExpressionEvaluator::operator()(StringLiteral* lit)
    {
        ExpressionTypeChecker type_checker{irgen};
        if(!type_checker(lit)) {irgen->error(); return nullptr;}
        std::vector<std::pair<llvm::Value*, llvm::Value*>> substrings;
        for(auto& substr: lit->literal)
        {
            if(std::holds_alternative<std::string>(substr))
            {
                auto& as_str = std::get<std::string>(substr);
                auto gvar = irgen->builder->CreateGlobalString(as_str);

                auto str_size = llvm::ConstantInt::get(llvm::Type::getInt64Ty(irgen->context) ,as_str.size());
                substrings.emplace_back(irgen->Malloc("tmp_string_buffer", str_size), str_size);

                irgen->builder->CreateMemCpy(substrings.back().first, std::nullopt, gvar, std::nullopt, str_size);
            }
            else //if(std::holds_alternative<std::unique_ptr<Expression>>)
            {
                auto& as_expr = std::get<1>(substr);
                auto type = std::visit(type_checker, as_expr->toVariant());
                auto value = std::visit(*this, as_expr->toVariant());
                substrings.push_back(doToStr(value, *type));
            }
        }
        llvm::Value* final_len = substrings.front().second;
        //combine all the strings into one buffer
        for(auto& pair : std::ranges::subrange(substrings.begin() + 1, substrings.end()))
            final_len = irgen->builder->CreateAdd(final_len, pair.second);
        llvm::Value* final_buffer = irgen->Malloc("string_buffer", final_len);
        llvm::Value* offset = llvm::ConstantInt::get(llvm::Type::getInt64Ty(irgen->context), 0);
        for(auto& pair : substrings)
        {
            auto current_pointer = irgen->builder->CreateGEP(llvm::Type::getInt8Ty(irgen->context), final_buffer, {offset});
            irgen->builder->CreateMemCpy(current_pointer, std::nullopt, pair.first, std::nullopt, pair.second);
            irgen->Free(pair.first);
            offset = irgen->builder->CreateAdd(offset, pair.second);
        }
        auto llvm_t = irgen->ToLLVMType(Type{"str"}, false);
        auto string = irgen->Alloca("str_obj", llvm_t);

        auto buffer_ptr = irgen->builder->CreateStructGEP(llvm_t, string, 0);
        auto size_ptr = irgen->builder->CreateStructGEP(llvm_t, string, 1);
        auto cap_ptr = irgen->builder->CreateStructGEP(llvm_t, string, 2);

        irgen->builder->CreateStore(final_buffer, buffer_ptr);
        irgen->builder->CreateStore(final_len, size_ptr);
        irgen->builder->CreateStore(final_len, cap_ptr);

        return string;

    }
    llvm::Value* ExpressionEvaluator::operator()(NameExpression* nm)
    {
        std::string name(nm->text);
        for(size_t i = irgen->variables.size(); i > 0; --i)
        {
            size_t idx = i - 1;
            if(auto var = irgen->variables[idx].find(name); var != irgen->variables[idx].end())
            {
                if(var->second.second->type->is_primitive() || var->second.second->type->is_enum())
                {
                    return irgen->builder->CreateLoad(irgen->ToLLVMType(*var->second.second->type, false), var->second.first, name);
                }
                return var->second.first;
            }
        }
        if(auto fn = irgen->code->getFunction(irgen->module->module_hash + name))
        {
            return fn;
        }
        return nullptr;
    }
    llvm::Value* ExpressionEvaluator::operator()(PrefixOperation*) {}
    llvm::Value* ExpressionEvaluator::operator()(BinaryOperation* op)
    {
        auto type_checker = ExpressionTypeChecker{irgen};
        auto res =  type_checker(op);
        if(!res) {irgen->error(); return nullptr;}

        auto l_as_var = op->lhs->toVariant();
        auto r_as_var = op->rhs->toVariant();

        llvm::Value* lhs;
        llvm::Value* rhs;
        if(op->op.type != TokenType::Dot && !op->op.is_assignment())
        {
            lhs = std::visit(*this, l_as_var);
            rhs = std::visit(*this, r_as_var);
        }

        auto left_t = std::visit(type_checker, l_as_var);
        auto right_t = std::visit(type_checker, r_as_var);

        switch(op->op.type)
        {
            using enum TokenType;
        case Plus: return doAddition(lhs, rhs, *left_t, *right_t, *res);
        case Minus: return doMinus(lhs, rhs, *left_t, *right_t, *res);
        case Star: return doMult(lhs, rhs, *left_t, *right_t, *res);
        case Slash: return doDiv(lhs, rhs, *left_t, *right_t, *res);
        case Percent: return doRem(lhs, rhs, *left_t, *right_t, *res);
        case Greater: return doCmp(GT, lhs, rhs, *left_t, *right_t, *res);
        case Less: return doCmp(LT, lhs, rhs, *left_t, *right_t, *res);
        case GreaterEqual: return doCmp(EQ_GT, lhs, rhs, *left_t, *right_t, *res);
        case LessEqual: return doCmp(EQ_LT, lhs, rhs, *left_t, *right_t, *res);
        case BangEqual: return doCmp(NE, lhs, rhs, *left_t, *right_t, *res);
        case DoubleEqual: return doCmp(EQ, lhs, rhs, *left_t, *right_t, *res);
        case Dot: return doDot(op->lhs.get(), op->rhs.get(), *left_t);

        case Equal:
                return doAssign(std::visit(LValueEvaluator{irgen}, l_as_var), std::visit(*this, r_as_var), *left_t, *right_t);
        default:; //TODO
        }
        return nullptr;
    }
    llvm::Value* ExpressionEvaluator::operator()(GroupingExpression* op)
    {
        return std::visit(*this, op->expr->toVariant());
    }
    llvm::Value* ExpressionEvaluator::operator()(LogicalOperation*) {}
    llvm::Value* ExpressionEvaluator::operator()(PostfixOperation*) {}
    llvm::Value* ExpressionEvaluator::fillArgs(bool uses_sret,
        const FunctionSignature& sig,
        std::vector<llvm::Value*>& args, llvm::Value* first, std::vector<std::unique_ptr<Expression>>& exprs)
    {
        llvm::Value* return_value = nullptr;
        if(uses_sret)
        {
            auto ret_t = irgen->ToLLVMType(sig.returnType, false);
            return_value = irgen->Alloca("return_temp", ret_t);
            args[0] = return_value;
        }
        if(first) args[uses_sret] = first;
        bool is_bound = first != nullptr;
        for(size_t i = 0; i < exprs.size(); i++)
        {
            if(sig.parameters[i + is_bound].convention == ParamType::InOut)
                args[i + is_bound + uses_sret] = std::visit(LValueEvaluator{irgen}, exprs[i]->toVariant());
            else
            {
                auto arg = std::visit(*this, exprs[i]->toVariant());
                auto tp = std::visit(ExpressionTypeChecker{irgen}, exprs[i]->toVariant());
                if(sig.parameters[i + is_bound].type.name == "__called_fn")
                {
                    auto llvm_t = irgen->ToLLVMType(sig.parameters[i + is_bound].type, false);
                    auto buffer = irgen->Alloca("called_fn_buffer", llvm_t);
                    if(tp->is_function())
                    {
                        irgen->builder->CreateStore(llvm::ConstantPointerNull::get(llvm::PointerType::get(irgen->context, 0)),
                            irgen->builder->CreateGEP(llvm_t, buffer, {
                                llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 0),
                                llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 0),
                            }));
                        irgen->builder->CreateStore(arg,
                            irgen->builder->CreateGEP(llvm_t, buffer, {
                                llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 0),
                                llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 1),
                            }));
                    }
                    if(tp->is_lambda())
                    {
                        irgen->builder->CreateStore(arg,
                            irgen->builder->CreateGEP(llvm_t, buffer, {
                                llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 0),
                                llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 0),
                            }));
                        irgen->builder->CreateStore(irgen->code->getFunction(irgen->block_hash + tp->name),
                            irgen->builder->CreateGEP(llvm_t, buffer, {
                                llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 0),
                                llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 1),
                            }));
                    }
                    args[i + is_bound + uses_sret] = buffer;
                }
                else args[i + is_bound + uses_sret] = implicitConvert(arg, *tp, sig.parameters[i + is_bound].type, irgen);
            }
        }
        return return_value;
    }

    llvm::Value* ExpressionEvaluator::doInvoke(CallOperation* op, const Type& left_t)
    {
        auto expr = reinterpret_cast<BinaryOperation*>(op->callee.get());
        auto fn_store = std::visit(*this, expr->lhs->toVariant());
        auto const_zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 0);
        auto as_llvm_t = irgen->ToLLVMType(left_t, false);
        auto ctx_ptr = irgen->builder->CreateGEP(as_llvm_t, fn_store, {const_zero,const_zero}, "fn_context_ptr_ptr");
        auto fn_ptr = irgen->builder->CreateGEP(as_llvm_t, fn_store, {const_zero,
            llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 1)}, "fn_ptr_ptr");

        auto ptr_ty = llvm::PointerType::get(irgen->context, 0);
        auto ctx = irgen->builder->CreateLoad(ptr_ty, ctx_ptr, "fn_ctx");
        auto fn = irgen->builder->CreateLoad(ptr_ty, fn_ptr, "fn_ptr");

        bool uses_sret = left_t.signature->returnType.should_sret();
        std::vector<llvm::Value*> args(op->arguments.size() + uses_sret);
        auto args_w_lambda = args;
        args_w_lambda.push_back(ctx);
        auto return_val = fillArgs(uses_sret, *left_t.signature, args, nullptr, op->arguments);
        auto ctx_as_int = irgen->builder->CreatePtrToInt(ctx, llvm::Type::getInt64Ty(irgen->context), "fn_ctx_as_int");
        auto zero64 = llvm::ConstantInt::get(llvm::Type::getInt64Ty(irgen->context), 0);
        auto is_lambda = irgen->builder->CreateICmpEQ(ctx_as_int, zero64, "is_lambda");
        auto current_fn = irgen->builder->GetInsertBlock()->getParent();
        auto then_bb = llvm::BasicBlock::Create(irgen->context, "lambda_call", current_fn, irgen->returnBlock);
        auto else_bb = llvm::BasicBlock::Create(irgen->context, "fn_call", current_fn, irgen->returnBlock);
        auto cont_bb = llvm::BasicBlock::Create(irgen->context, "after_call", current_fn, irgen->returnBlock);
        irgen->builder->CreateCondBr(is_lambda, then_bb, else_bb);

        auto fn_t = irgen->ToLLVMSignature(*left_t.signature);
        FunctionSignature sig = *left_t.signature;
        sig.parameters.push_back(FunctionParameter{.type = Type{"__ptr"}, .convention = ParamType::InOut});
        auto lambda_t = irgen->ToLLVMSignature(sig);

        auto return_as_llvm = irgen->ToLLVMType(left_t.signature->returnType, false);

        irgen->builder->SetInsertPoint(then_bb);
        auto call_resl = irgen->builder->CreateCall(lambda_t, fn, args_w_lambda);

        irgen->builder->CreateBr(cont_bb);

        irgen->builder->SetInsertPoint(else_bb);
        auto call_resf = irgen->builder->CreateCall(fn_t, fn, args);
        irgen->builder->CreateBr(cont_bb);

        irgen->builder->SetInsertPoint(cont_bb);
        if(!uses_sret && !left_t.signature->returnType.is_void())
        {
            auto PN = irgen->builder->CreatePHI(return_as_llvm, 2, "invoke_result");
            PN->addIncoming(call_resl, then_bb);
            PN->addIncoming(call_resf, else_bb);
            return_val = PN;
        }


        return return_val;
    }

    llvm::Value* ExpressionEvaluator::operator()(CallOperation* op)
    {
        ExpressionTypeChecker type_checker{irgen};
        auto t = std::visit(type_checker, op->callee->toVariant());
        if(!t || !(t->is_function() || t->is_lambda())) {irgen->error(); return nullptr;}
        if(!type_checker(op))
            {irgen->error(); return nullptr;}
        bool is_lambda = t->is_lambda();
        auto fn = reinterpret_cast<FunctionType&>(*t);
        auto expr = dynamic_cast<BinaryOperation*>(op->callee.get());
        std::optional<FunctionType> left_ty;
        if(expr) left_ty = std::visit(type_checker, expr->lhs->toVariant());
        if(left_ty && left_ty->name == "__called_fn")
        {
            return doInvoke(op, *left_ty);
        }
        if(fn.is_bound)
        {
            //expr is guaranteed to be valid if the function is bound
            //callee is a binary dot expr
            auto left_t = std::visit(ExpressionTypeChecker{irgen}, expr->lhs->toVariant());
            if(auto cls = left_t->get_decl_if_class(irgen))
            {
                //handle member functions
                if(auto rhs = dynamic_cast<NameExpression*>(expr->rhs.get()))
                {
                    std::string name(rhs->text);
                    if(auto var = std::ranges::find_if(cls->methods, [&name](ClassMethod& m)
                        {
                            return name == m.name;
                        }); var != cls->methods.end())
                    {
                        auto decl = reinterpret_cast<FunctionDeclaration*>(var->function_decl.get());
                        auto found = irgen->findType(left_t->name);
                        auto function_name  = std::get<0>(*found) + name;
                        auto callee = irgen->code->getFunction(function_name);
                        bool uses_sret = callee->hasStructRetAttr();
                        std::vector<llvm::Value*> args(op->arguments.size() + 1 + uses_sret); // +1 because its bound
                        llvm::Value* return_value = fillArgs(uses_sret, decl->signature, args, std::visit(*this, expr->lhs->toVariant()), op->arguments);
                        auto call_val = irgen->builder->CreateCall(callee, args);
                        if(!uses_sret) return_value = call_val;
                        return return_value;
                    }
                }
            }
            auto right_t = std::visit(ExpressionTypeChecker{irgen}, expr->rhs->toVariant());
            auto* callee = is_lambda ?
                irgen->code->getFunction(irgen->block_hash + right_t->name) :
                llvm::dyn_cast_or_null<llvm::Function>(std::visit(*this, expr->rhs->toVariant()));
            if(!callee) return nullptr;
            bool uses_sret = callee->hasStructRetAttr();
            std::vector<llvm::Value*> args(op->arguments.size() + 1 + uses_sret);
            llvm::Value* return_value = fillArgs(uses_sret, fn.sig, args, std::visit(*this, expr->lhs->toVariant()), op->arguments);
            if(is_lambda) args.push_back(std::visit(*this, expr->rhs->toVariant()));
            auto call_val = irgen->builder->CreateCall(callee->getFunctionType(), callee, args);
            if(!uses_sret) return_value = call_val;
            return return_value;
        }

        auto callee_val = std::visit(*this, op->callee->toVariant());
        auto* callee = is_lambda ?
            irgen->code->getFunction(irgen->block_hash + t->name) :
            llvm::dyn_cast_or_null<llvm::Function>(callee_val);
        if(!callee) return nullptr;
        bool uses_sret = callee->hasStructRetAttr();
        std::vector<llvm::Value*> args(op->arguments.size() + uses_sret);
        llvm::Value* return_value = fillArgs(uses_sret, fn.sig, args, nullptr, op->arguments);
        if(is_lambda) args.push_back(callee_val);
        auto call_val = irgen->builder->CreateCall(callee->getFunctionType(), callee, args);
        if(!uses_sret) return_value = call_val;
        return return_value;
    }
    llvm::Value* ExpressionEvaluator::operator()(SubscriptOperation*) {}

    llvm::Value* ExpressionEvaluator::operator()(LambdaExpression* expr)
    {
        //TODO: check for capture/parameter duplicates
        if(expr->sig.returnType.name == "__inferred")
        {
            auto t = irgen->inferReturnType(expr->body.get());;
            expr->sig.returnType = *t;
        }
        std::vector<llvm::Type*> context_types;
        for(auto& capture : expr->captures)
        {
            Token tk{.type = TokenType::Identifier, .text = capture.first};
            NameExpression name(std::string(tk.text));
            auto type = ExpressionTypeChecker{irgen}(&name);
            if(type->is_function()) { irgen->error(); return nullptr; }
            if(capture.second == ParamType::InOut && !type->is_mutable) { irgen->error(); return nullptr; }
            llvm::Type* tp = irgen->ToLLVMType(*type, false);
            context_types.push_back(capture.second == ParamType::In ? tp : tp->getPointerTo());
        }
        auto context = llvm::StructType::get(irgen->context, context_types);
        auto ctx_object = irgen->Alloca("lambda_context", context);
        //copy types into the context
        size_t idx = 0;
        auto zero_const = llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 0);
        for(auto& capture : expr->captures)
        {
            auto idx_const = llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), idx);
            Token tk{.type = TokenType::Identifier, .text = capture.first};
            NameExpression name(std::string(tk.text));
            auto type = ExpressionTypeChecker{irgen}(&name);
            type->is_mutable = true;
            llvm::Value* val = capture.second == ParamType::InOut ?
                LValueEvaluator{irgen}(&name) :
                (*this)(&name);
            llvm::Value* lhs = irgen->builder->CreateGEP(context, ctx_object, {zero_const, idx_const});
            if(capture.second == ParamType::InOut) irgen->builder->CreateStore(val, lhs);
            else doAssign(lhs, val, *type, *type);
            idx++;
        }
        std::string name = "__lambda" + expr->hash;
        Token tk{.type = TokenType::Identifier, .text = name};
        irgen->lambdas[name] = {&expr->captures, context};
        irgen->lambdaSigs[name] = expr->sig;
        FunctionSignature sig = expr->sig;
        //insert a pointer to the context at the end of the signature
        sig.parameters.push_back(FunctionParameter{.type = Type{name}, .convention = ParamType::InOut});
        FunctionDeclaration decl(tk,std::move(sig), std::move(expr->body));
        (*irgen)(&decl);
        return ctx_object;
    }

    llvm::Value* ExpressionEvaluator::operator()(ScopeOperation* op)
    {
        auto ty = ExpressionTypeChecker{irgen}(op);
        if(!ty) {irgen->error();return nullptr;}
        //either a function or global var or enum
        if(ty->is_function())
        {
            if(ty->module == irgen->module)
            {
                if(ty->module->classes.contains(op->type.name))
                {
                    auto& class_entry = ty->module->classes.at(op->type.name);
                    std::string mangled_name = std::get<0>(class_entry) + op->name;
                    return irgen->code->getFunction(mangled_name);
                }
                std::string mangled_name = ty->module->module_hash + op->name;
                return irgen->code->getFunction(mangled_name);
            }
            else
            {
                std::string mangled_name;
                if(ty->module->classes.contains(op->type.name))
                {
                    auto& class_entry = ty->module->classes.at(op->type.name);
                    mangled_name = std::get<0>(class_entry) + op->name;
                }
                else mangled_name = ty->module->module_hash + op->name;
                auto fn = irgen->code->getFunction(mangled_name);
                if(fn) return fn;
                fn = llvm::Function::Create(irgen->ToLLVMSignature(ty->sig), llvm::GlobalValue::ExternalLinkage,
                    mangled_name, irgen->code);
                if(!ty->sig.returnType.is_builtin())
                {
                    auto return_as_llvm_type = irgen->ToLLVMType(ty->sig.returnType, false);
                    fn->addAttributeAtIndex(1, llvm::Attribute::get(irgen->context, llvm::Attribute::StructRet, return_as_llvm_type));
                }
                return fn;
            }
        }
        if(ty->is_enum())
        {
            auto decl = ty->module->enums[op->type.name].get();
            return llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), decl->values.at(op->name));
        }
        return nullptr;
    }

    llvm::Value* ExpressionEvaluator::operator()(ObjectLiteral* lit)
    {
        auto t = ExpressionTypeChecker{irgen}(lit);
        if(!t) {irgen->error();return nullptr;}
        auto as_llvm_type = irgen->ToLLVMType(*t, false);
        auto decl = t->get_decl_if_class(irgen);
        auto value = irgen->Alloca("obj_lit",as_llvm_type);

        auto zero_const = llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 0);
        for(size_t i = 0; i < decl->vars.size(); i++)
        {
            auto idx_const = llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), i);
            auto& var = decl->vars[i];
            auto val_ty = std::visit(ExpressionTypeChecker{irgen, var.type}, lit->values[var.name]->toVariant());
            auto val = std::visit(ExpressionEvaluator{irgen, var.type}, lit->values[var.name]->toVariant());

            auto mem_ptr = irgen->builder->CreateGEP(as_llvm_type, value, {zero_const, idx_const});
            auto as_mut = var.type;
            as_mut.is_mutable = true; as_mut.is_lvalue = true;
            doAssign(mem_ptr, val, as_mut, *val_ty);
        }
        return value;
    }
}
