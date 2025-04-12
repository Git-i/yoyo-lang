#include "ir_gen.h"
#include "constant.h"
namespace Yoyo
{
    Constant ConstantEvaluator::operator()(IntegerLiteral* lit)
    {
        return std::stoull(lit->text);
    }
    Constant ConstantEvaluator::operator()(BooleanLiteral* lit)
    {
        return lit->token.text == "true";
    }
    Constant ConstantEvaluator::operator()(RealLiteral* lit)
    {
        return std::stod(std::string(lit->token.text));
    }
    Constant ConstantEvaluator::operator()(NameExpression* nexpr)
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
        if (std::holds_alternative<Constant>(val)) return std::get<Constant>(val);
        
        irgen->block_hash.swap(blk);
        (*irgen)(std::get<ConstantDeclaration*>(val));
        irgen->block_hash.swap(blk);
        return std::get<Constant>(val);
    }
    Constant ConstantEvaluator::constConvert(const Constant& src, const Type& source, const Type& destination) {
        if (source.is_integral() && destination.is_integral()) {
            if (destination.is_signed_integral())
                return int64_t{ source.is_unsigned_integral() ? src.internal_repr.u64 : src.internal_repr.i64 };
            else
                return uint64_t{ source.is_unsigned_integral() ? src.internal_repr.u64 : src.internal_repr.i64 };
        }
        return nullptr;
    }
    Constant ConstantEvaluator::operator()(ObjectLiteral* lit)
    {
        auto t = ExpressionTypeChecker{ irgen }(lit);
        if (!t) { irgen->error(t.error());return nullptr; }
        auto decl = t->get_decl_if_class(irgen);
        std::vector<Constant> args;
        args.reserve(lit->values.size());
        for (size_t i = 0; i < decl->vars.size(); i++)
        {
            auto& var = decl->vars[i];
            auto val_ty = std::visit(ExpressionTypeChecker{ irgen, var.type }, lit->values[var.name]->toVariant());
            if (!val_ty) { irgen->error(val_ty.error()); continue; }
            auto val = std::visit(*this, lit->values[var.name]->toVariant());

            args.push_back(constConvert(val, val_ty.value(), var.type));
        }
        return irgen->module->engine->createGlobalConstant(*t, args, irgen->module);
    }
    Constant ConstantEvaluator::operator()(StringLiteral* lit) {
        if (lit->literal.size() != 1) {
            irgen->error(Error(lit, "String interpolation is not supported in constant strings"));
            return nullptr;
        }
        if (std::holds_alternative<std::string>(lit->literal[0]))
        {
            auto& as_str = std::get<std::string>(lit->literal[0]);
            return irgen->module->engine->createGlobalConstant(Type{ "str" }, { as_str.c_str() }, irgen->module);
        }
        else {
            irgen->error(Error(lit, "String interpolation is not supported in constant strings"));
            return nullptr;
        }
    }
    Constant ConstantEvaluator::operator()(MacroInvocation* invc)
    {
        ExpressionTypeChecker{ irgen }(invc);
        return std::visit(*this, invc->result->toVariant());
    }
    Constant ConstantEvaluator::operator()(PrefixOperation*)
    {
        debugbreak();
        return {};
    }
    Constant ConstantEvaluator::operator()(BinaryOperation* bop)
    {
        auto type = ExpressionTypeChecker{ irgen }(bop);
        if (!type) { irgen->error(type.error()); return nullptr; }

        auto left = std::visit(*this, bop->lhs->toVariant());
        auto right = std::visit(*this, bop->rhs->toVariant());

        auto is_float = type->is_floating_point();
        auto& left_repr = left.internal_repr;
        auto& right_repr = right.internal_repr;
        switch (bop->op.type)
        {
            using enum TokenType;
            //Implicit requirement for the machine / compiler to use twos complement
        case Plus: return is_float ? Constant{ left_repr.f64 + right_repr.f64 } : Constant{ left_repr.u64 + right_repr.u64 };
        case Star: return is_float ? Constant{ left_repr.f64 * right_repr.f64 } : Constant{ left_repr.u64 * right_repr.u64 };
        case Minus: return is_float ? Constant{ left_repr.f64 - right_repr.f64 } : Constant{ left_repr.u64 - right_repr.u64 };
        case Slash: 
        {
            if (is_float) return left_repr.f64 / right_repr.f64;
            else if (type->is_signed_integral()) return left_repr.i64 / right_repr.i64;
            else return left_repr.u64 / right_repr.u64;
        }
        case Percent: 
        {
            if (type->is_signed_integral()) return left_repr.i64 % right_repr.i64;
            else return left_repr.u64 % right_repr.u64;
        }
        case DoubleGreater: return left_repr.u64 >> right_repr.u64;
        case DoubleLess: return left_repr.u64 << right_repr.u64;
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
    Constant ConstantEvaluator::operator()(GroupingExpression* gex)
    {
        return std::visit(*this, gex->expr->toVariant());
    }
    Constant ConstantEvaluator::operator()(LogicalOperation* lgx)
    {
        return nullptr;
    }
    bool advanceScope(Type& type, Module*& md, std::string& hash, IRGenerator* irgen, bool first);
    Constant ConstantEvaluator::operator()(ScopeOperation* scp)
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

        // TODO: functionality to register constant with llvm
        if (std::holds_alternative<Constant>(val)) {
            
            return std::get<Constant>(val);
        }

        irgen->block_hash.swap(blk);
        std::swap(md, irgen->module);
        (*irgen)(std::get<ConstantDeclaration*>(val));
        std::swap(md, irgen->module);
        irgen->block_hash.swap(blk);
        return std::get<Constant>(val);
    }
    Constant ConstantEvaluator::operator()(CharLiteral* ch)
    {
        return uint64_t{ ch->value };
    }
}