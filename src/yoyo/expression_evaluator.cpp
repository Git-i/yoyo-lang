#include "ir_gen.h"
namespace Yoyo
{
    llvm::Value* addIN(llvm::Value* iN, llvm::Value* other, llvm::IRBuilder<>*builder, const Type& otherty, const Type& thisty)
    {
        if(otherty.is_integral())
        {
            const auto width = *otherty.integer_width();
            const auto N = *thisty.integer_width();
            if(width == N) return builder->CreateAdd(iN, other, "iaddtmp");
            if(width > N) return builder->CreateAdd(other,
                thisty.is_signed_integral() ?
                builder->CreateSExt(iN, llvm::Type::getIntNTy(builder->getContext(), width), "extension_temp") :
                builder->CreateZExt(iN, llvm::Type::getIntNTy(builder->getContext(), width), "extension_temp"), "iaddtmp" );
            return builder->CreateAdd(iN,
                otherty.is_signed_integral() ?
                builder->CreateSExt(other, llvm::Type::getIntNTy(builder->getContext(), N), "iexttmp") :
                builder->CreateZExt(other, llvm::Type::getIntNTy(builder->getContext(), N), "iexttmp"), "iaddtmp" );
        }
        return nullptr;
    }
    llvm::Value* addFN(llvm::Value* fN, llvm::Value* other, llvm::IRBuilder<>*builder, const Type& otherty, const Type& thisty)
    {
        if(otherty.is_floating_point())
        {
            const auto other_w = *otherty.float_width();
            const auto this_w = *thisty.float_width();
            if(other_w == this_w) return builder->CreateFAdd(fN, other, "faddtmp");
            //since there's only doubles and float, other_w being greater means it's a double
            auto dbl = llvm::Type::getDoubleTy(builder->getContext());
            if(other_w > this_w) return builder->CreateFAdd(other, builder->CreateFPExt(fN, dbl, "fexttmp"), "faddtmp");
            return builder->CreateFAdd(fN, builder->CreateFPExt(other, dbl, "fexttmp"), "faddtmp");
        }
        return nullptr;
    }

    llvm::Value* ExpressionEvaluator::doAddition(BinaryOperation* op)
    {
        auto l_as_var = op->lhs->toVariant();
        auto r_as_var = op->rhs->toVariant();

        auto left_type = std::visit(ExpressionTypeChecker{irgen}, l_as_var);
        auto right_type = std::visit(ExpressionTypeChecker{irgen}, r_as_var);

        auto rhs = std::visit(*this, r_as_var);
        auto lhs = std::visit(*this, l_as_var);
        if(left_type->is_integral())
            return addIN(lhs, rhs, irgen->builder, *right_type, *left_type);
        if(left_type->is_floating_point())
            return addFN(lhs, rhs, irgen->builder, *right_type, *left_type);
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
        switch(op->op.type)
        {
            using enum TokenType;
        case Plus: return doAddition(op);
        }
    }
    llvm::Value* ExpressionEvaluator::operator()(GroupingExpression*) {}
    llvm::Value* ExpressionEvaluator::operator()(LogicalOperation*) {}
    llvm::Value* ExpressionEvaluator::operator()(PostfixOperation*) {}
    llvm::Value* ExpressionEvaluator::operator()(CallOperation*) {}
    llvm::Value* ExpressionEvaluator::operator()(SubscriptOperation*) {}
}