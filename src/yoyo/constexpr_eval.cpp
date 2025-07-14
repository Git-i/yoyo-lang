#include "ir_gen.h"
#include "constant.h"
namespace Yoyo
{
    Constant ConstantEvaluator::operator()(IntegerLiteral* lit)
    {
        if (target && target->is_signed_integral()) return std::stoll(lit->text);
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
        ModuleBase* module = irgen->module;
        std::string hash = irgen->block_hash;
        Type tp{ .name = nexpr->text };
        irgen->apply_using(tp, module, hash);
        auto [blk, dets] = module->findConst(hash, nexpr->text);
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
        std::swap(irgen->module, module);
        irgen->doConst(std::get<ConstantDeclaration*>(val));
        std::swap(irgen->module, module);
        irgen->block_hash.swap(blk);
        return std::get<Constant>(val);
    }
    Constant ConstantEvaluator::constConvert(const Constant& src, const Type& source, const Type& destination) {
        if (source.is_integral() && destination.is_integral()) {
            if (destination.is_signed_integral())
                return int64_t{ source.is_unsigned_integral() ? 
                    static_cast<int64_t>(std::get<uint64_t>(src.internal_repr)) : 
                    std::get<int64_t>(src.internal_repr) };
            else
                return uint64_t{ source.is_unsigned_integral() ?
                    std::get<uint64_t>(src.internal_repr) :
                    std::get<int64_t>(src.internal_repr) };
        }
        return nullptr;
    }
    
    Constant ConstantEvaluator::operator()(ObjectLiteral* lit)
    {
        if (!is_initial) irgen->error(Error(lit, "Structural constants are only allowed as top level constants"));
        is_initial = false;
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
            target = &val_ty.value();
            auto val = std::visit(*this, lit->values[var.name]->toVariant());

            args.push_back(constConvert(val, val_ty.value(), var.type));
        }
        return irgen->module->engine->createGlobalConstant(*t, args, irgen);
    }
    Constant ConstantEvaluator::operator()(StringLiteral* lit) {
        if (lit->literal.size() != 1) {
            irgen->error(Error(lit, "String interpolation is not supported in constant strings"));
            return nullptr;
        }
        if (std::holds_alternative<std::string>(lit->literal[0]))
        {
            auto& as_str = std::get<std::string>(lit->literal[0]);
            return irgen->module->engine->createGlobalConstant(Type{ "str" }, { as_str.c_str() }, irgen);
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
    Constant ConstantEvaluator::operator()(Expression* expr)
    {
        irgen->error(Error(expr, "Expression cannot be constant evaluated"));
        return Constant();
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
        case Plus: return is_float ? 
            Constant{ std::get<double>(left_repr) + std::get<double>(right_repr) } : 
            Constant{ std::get<uint64_t>(left_repr) + std::get<uint64_t>(right_repr) };
        case Star: return is_float ? Constant{ std::get<double>(left_repr) * std::get<double>(right_repr) } : Constant{ std::get<uint64_t>(left_repr) * std::get<uint64_t>(right_repr) };
        case Minus: return is_float ? Constant{ std::get<double>(left_repr) - std::get<double>(right_repr) } : Constant{ std::get<uint64_t>(left_repr) - std::get<uint64_t>(right_repr) };
        case Slash: 
        {
            if (is_float) return std::get<double>(left_repr) / std::get<double>(right_repr);
            else if (type->is_signed_integral()) return std::get<int64_t>(left_repr) / std::get<int64_t>(right_repr);
            else return std::get<uint64_t>(left_repr) / std::get<uint64_t>(right_repr);
        }
        case Percent: 
        {
            if (type->is_signed_integral()) return std::get<int64_t>(left_repr) % std::get<int64_t>(right_repr);
            else return std::get<uint64_t>(left_repr) % std::get<uint64_t>(right_repr);
        }
        case DoubleGreater: return std::get<uint64_t>(left_repr) >> std::get<uint64_t>(right_repr);
        case DoubleLess: return std::get<uint64_t>(left_repr) << std::get<uint64_t>(right_repr);
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
    bool advanceScope(Type& type, ModuleBase*& md, std::string& hash, IRGenerator* irgen, bool first);
    Constant ConstantEvaluator::operator()(ScopeOperation* scp)
    {
        ModuleBase* md = irgen->module;
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
        irgen->doConst(std::get<ConstantDeclaration*>(val));
        std::swap(md, irgen->module);
        irgen->block_hash.swap(blk);
        return std::get<Constant>(val);
    }
    Constant ConstantEvaluator::operator()(CharLiteral* ch)
    {
        return uint64_t{ ch->value };
    }
}