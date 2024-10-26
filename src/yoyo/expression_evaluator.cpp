#include "ir_gen.h"
namespace Yoyo
{

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
        ComparsionPredicate p,
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
                return irgen->builder->CreateLoad(var->second.first->getAllocatedType(), var->second.first, name);
            }
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
        auto lhs = std::visit(*this, l_as_var);
        auto rhs = std::visit(*this, r_as_var);

        auto left_t = std::visit(type_checker, l_as_var);
        auto right_t = std::visit(type_checker, r_as_var);

        if(!left_t || !right_t) return nullptr;
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
        case GreaterEqual: doCmp(EQ_GT, lhs, rhs, *left_t, *right_t);
        case LessEqual: doCmp(EQ_LT, lhs, rhs, *left_t, *right_t);
        case BangEqual: doCmp(NE, lhs, rhs, *left_t, *right_t);
        case DoubleEqual: doCmp(EQ, lhs, rhs, *left_t, *right_t);
        }
        return nullptr;
    }
    llvm::Value* ExpressionEvaluator::operator()(GroupingExpression* op)
    {
        return std::visit(*this, op->expr->toVariant());
    }
    llvm::Value* ExpressionEvaluator::operator()(LogicalOperation*) {}
    llvm::Value* ExpressionEvaluator::operator()(PostfixOperation*) {}
    llvm::Value* ExpressionEvaluator::operator()(CallOperation*) {}
    llvm::Value* ExpressionEvaluator::operator()(SubscriptOperation*) {}
}