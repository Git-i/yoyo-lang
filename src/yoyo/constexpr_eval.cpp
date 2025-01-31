#include "ir_gen.h"
namespace Yoyo
{
    llvm::Constant* ConstantEvaluator::operator()(IntegerLiteral* lit)
    {
        return llvm::ConstantInt::get(llvm::IntegerType::getInt64Ty(irgen->context), std::stoull(lit->text));
    }
    llvm::Constant* ConstantEvaluator::operator()(BooleanLiteral* lit)
    {
        return llvm::ConstantInt::getTrue(irgen->context);
    }
    llvm::Constant* ConstantEvaluator::operator()(RealLiteral* lit)
    {
        return llvm::ConstantFP::get(llvm::Type::getDoubleTy(irgen->context), lit->token.text);
    }
    llvm::Constant* ConstantEvaluator::operator()(NameExpression* nexpr)
    {
        auto [_, dets] = irgen->module->findConst(irgen->block_hash, nexpr->text);
        if (!dets)
        {
            irgen->error(Error(nexpr, nexpr->text + " does not name a constant")); return nullptr;
        }
        return std::get<2>(*dets);
    }
    llvm::Constant* ConstantEvaluator::operator()(PrefixOperation*)
    {
        debugbreak();
        return nullptr;
    }
    llvm::Constant* ConstantEvaluator::operator()(BinaryOperation* bop)
    {
        auto type = ExpressionTypeChecker{ irgen }(bop);
        if (!type) { irgen->error(type.error()); return nullptr; }
        auto left = std::visit(*this, bop->lhs->toVariant());
        if (!left) { return nullptr; }
        auto right = std::visit(*this, bop->rhs->toVariant());
        if (!right) { return nullptr; }
        using enum llvm::Instruction::BinaryOps;
        const bool is_float = type->is_floating_point();
        auto add_isn = is_float ? FAdd : Add;
        auto sub_isn = is_float ? FSub : Sub;
        auto mul_isn = is_float ? FMul : Mul;
        auto div_isn = is_float ? FDiv : type->is_unsigned_integral() ? UDiv : SDiv;
        auto rem_isn = is_float ? FRem : type->is_unsigned_integral() ? URem : SRem;
        auto as_float = [this] (llvm::Constant* in, llvm::Type* tp) {
            if (llvm::isa<llvm::ConstantFP>(in)) return llvm::dyn_cast<llvm::Constant>(irgen->builder->CreateFPExt(in, tp));
            return llvm::dyn_cast<llvm::Constant>(irgen->builder->CreateUIToFP(in, tp));
            };
        if (is_float)
        {
            auto as_llvm = irgen->ToLLVMType(*type, false);
            left = as_float(left, as_llvm);
            right = as_float(right, as_llvm);
        }
        switch (bop->op.type)
        {
            using enum TokenType;
        case Plus: return llvm::ConstantExpr::get(add_isn, left, right);
        case Star: return llvm::ConstantExpr::get(mul_isn, left, right);
        case Minus: return llvm::ConstantExpr::get(sub_isn, left, right);
        case Slash: return llvm::ConstantExpr::get(div_isn, left, right);
        case Percent: return llvm::ConstantExpr::get(rem_isn, left, right);
        case DoubleGreater: return llvm::ConstantExpr::get(LShr, left, right);
        case DoubleLess: return llvm::ConstantExpr::get(Shl, left, right);
        /*
        case DoubleEqual: 
        case GreaterEqual:
        case LessEqual:;
        case Less:;
        case Greater:;
        case Spaceship:;
        case BangEqual:;
        case Pipe:;
        case Caret:;
        case Ampersand:;
        */
        default: irgen->error(Error(bop, "Invalid constant operation"));
        }
        return nullptr;
    }
    llvm::Constant* ConstantEvaluator::operator()(GroupingExpression* gex)
    {
        return std::visit(*this, gex->expr->toVariant());
    }
    llvm::Constant* ConstantEvaluator::operator()(LogicalOperation* lgx)
    {
        return nullptr;
    }
    bool advanceScope(Type& type, Module*& md, std::string& hash, IRGenerator* irgen, bool first);
    llvm::Constant* ConstantEvaluator::operator()(ScopeOperation* scp)
    {
        Module* md = irgen->module;
        std::string hash = irgen->block_hash;
        auto iterator = UnsaturatedTypeIterator(scp->type);
        bool first = true;
        while (!iterator.is_end())
        {
            auto type = iterator.next();
            if (!advanceScope(type, md, hash, irgen, first)) {
                irgen->error(Error(scp, "The name '" + type.name + "' does not exist in \"" + hash + "\""));
                return nullptr;
            }
            first = false;
        }
        std::string c_name = iterator.last().name;
        auto [_, dets] = md->findConst(hash, c_name);
        if (!dets)
        {
            irgen->error(Error(scp, c_name + " does not name a constant")); return nullptr;
        }
        return std::get<2>(*dets);
    }
    llvm::Constant* ConstantEvaluator::operator()(CharLiteral* ch)
    {
        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), ch->value);
    }
}