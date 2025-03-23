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
        auto [blk, dets] = irgen->module->findConst(irgen->block_hash, nexpr->text);
        if (!dets)
        {
            irgen->error(Error(nexpr, nexpr->text + " does not name a constant")); return nullptr;
        }
        //check if its recursive
        if (auto it = std::ranges::find(disallowed_consts, std::pair{ blk, std::get<1>(*dets) })
            ; it != disallowed_consts.end()) {
            irgen->error(Error(nexpr, "Constant is recursive"));
        }
        auto& val = std::get<2>(*dets);
        if (std::holds_alternative<llvm::Constant*>(val)) return std::get<llvm::Constant*>(val);
        
        irgen->block_hash.swap(blk);
        (*irgen)(std::get<ConstantDeclaration*>(val));
        irgen->block_hash.swap(blk);
        return std::get<llvm::Constant*>(val);
    }
    llvm::Constant* ConstantEvaluator::constConvert(llvm::Constant* src, const Type& source, const Type& destination) {
        if (source.is_integral() && destination.is_integral()) {
            if(destination.is_signed_integral())
                return llvm::ConstantInt::get(irgen->ToLLVMType(destination, false),
                    reinterpret_cast<llvm::ConstantInt*>(src)->getSExtValue(), true);
            else
                return llvm::ConstantInt::get(irgen->ToLLVMType(destination, false),
                    reinterpret_cast<llvm::ConstantInt*>(src)->getZExtValue(), true);
        }
        return nullptr;
    }
    llvm::Constant* ConstantEvaluator::operator()(ObjectLiteral* lit)
    {
        auto t = ExpressionTypeChecker{ irgen }(lit);
        if (!t) { irgen->error(t.error());return nullptr; }
        auto as_llvm_type = irgen->ToLLVMType(*t, false);
        auto decl = t->get_decl_if_class(irgen);
        std::vector<llvm::Constant*> args;
        args.reserve(lit->values.size());
        for (size_t i = 0; i < decl->vars.size(); i++)
        {
            auto& var = decl->vars[i];
            auto val_ty = std::visit(ExpressionTypeChecker{ irgen, var.type }, lit->values[var.name]->toVariant());
            if (!val_ty) { irgen->error(val_ty.error()); continue; }
            auto val = std::visit(*this, lit->values[var.name]->toVariant());

            args.push_back(constConvert(val, val_ty.value(), var.type));
        }
        return new llvm::GlobalVariable(as_llvm_type, true, llvm::GlobalValue::ExternalLinkage,
            llvm::ConstantStruct::get( reinterpret_cast<llvm::StructType*>(as_llvm_type), args ));
    }
    llvm::Constant* ConstantEvaluator::operator()(StringLiteral* lit) {
        if (lit->literal.size() != 1) {
            irgen->error(Error(lit, "String interpolation is not supported in constant strings"));
            return nullptr;
        }
        if (std::holds_alternative<std::string>(lit->literal[0]))
        {
            auto& as_str = std::get<std::string>(lit->literal[0]);
            auto gvar = irgen->builder->CreateGlobalString(as_str);

            auto str_size = llvm::ConstantInt::get(llvm::Type::getInt64Ty(irgen->context), as_str.size());

            auto llvm_t = irgen->ToLLVMType(Type{ "str" }, false);
            auto string = new llvm::GlobalVariable(llvm_t, true, llvm::GlobalValue::ExternalLinkage,
                llvm::ConstantStruct::get(reinterpret_cast<llvm::StructType*>(llvm_t), { gvar, str_size, str_size }));

            return string;
        }
        else {
            irgen->error(Error(lit, "String interpolation is not supported in constant strings"));
            return nullptr;
        }
    }
    llvm::Constant* ConstantEvaluator::operator()(MacroInvocation* invc)
    {
        ExpressionTypeChecker{ irgen }(invc);
        return std::visit(*this, invc->result->toVariant());
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
        auto [blk, dets] = md->findConst(hash, c_name);
        if (!dets)
        {
            irgen->error(Error(scp, c_name + " does not name a constant")); return nullptr;
        }

        if (auto it = std::ranges::find(disallowed_consts, std::pair{ blk, std::get<1>(*dets) })
            ; it != disallowed_consts.end()) {
            irgen->error(Error(scp, "Constant is recursive"));
        }
        auto& val = std::get<2>(*dets);

        if (md != irgen->module) {
            auto glb = irgen->code->getOrInsertGlobal(hash + c_name, irgen->ToLLVMType(std::get<0>(*dets), false));
            reinterpret_cast<llvm::GlobalVariable*>(glb)->setExternallyInitialized(true);
            return glb;
        }
        if (std::holds_alternative<llvm::Constant*>(val)) {
            
            return std::get<llvm::Constant*>(val);
        }

        irgen->block_hash.swap(blk);
        std::swap(md, irgen->module);
        (*irgen)(std::get<ConstantDeclaration*>(val));
        std::swap(md, irgen->module);
        irgen->block_hash.swap(blk);
        return std::get<llvm::Constant*>(val);
    }
    llvm::Constant* ConstantEvaluator::operator()(CharLiteral* ch)
    {
        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), ch->value);
    }
}