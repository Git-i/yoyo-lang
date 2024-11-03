#include <csignal>

#include "ir_gen.h"
#include "fn_type.h"
namespace Yoyo
{
    std::optional<Type> canBinOpLiteral(const Type &a, const Type &b)
    {
        if(a.name == "ilit")
        {
            if(b.is_integral() || b.is_floating_point()) return b;
        }
        if(a.name == "flit")
        {
            if(b.is_floating_point()) return b;
        }
        if(b.name == "ilit" || b.name == "flit")
            return canBinOpLiteral(b, a);
        return std::nullopt;
    }
    static std::optional<Type> checkAddition(const Type &a, const Type &b)
    {
        if(a.is_integral() || a.is_floating_point())
        {
            if(a.is_equal(b)) return a.strip_lvalue();
            auto res = canBinOpLiteral(a, b);
            if(res) return res->strip_lvalue();
            return std::nullopt;
        }
        return std::nullopt;
    }
    static std::optional<Type> checkAssign(const Type &a, const Type &b)
    {
        if(!a.is_mutable) return std::nullopt;
        if(!a.is_assignable_from(b))
        {
            return std::nullopt;
        }
        return Type{"void"};
    }
    static std::optional<Type> checkMinus(const Type &a, const Type &b)
    {
        if(a.is_integral() || a.is_floating_point())
        {
            if(a.is_equal(b)) return a.strip_lvalue();
            auto res = canBinOpLiteral(a, b);
            if(res) return res->strip_lvalue();
            return std::nullopt;
        }
        return std::nullopt;
    }
    static std::optional<Type> checkStar(const Type &a, const Type &b)
    {
        if(a.is_integral() || a.is_floating_point())
        {
            if(a.is_equal(b)) return a.strip_lvalue();
            auto res = canBinOpLiteral(a, b);
            if(res) return res->strip_lvalue();
            return std::nullopt;
        }
        return std::nullopt;
    }
    static std::optional<Type> checkDivide(const Type &a, const Type &b)
    {
        if(a.is_integral() || a.is_floating_point())
        {
            if(a.is_equal(b)) return a.strip_lvalue();
            auto res = canBinOpLiteral(a, b);
            if(res) return res->strip_lvalue();
            return std::nullopt;
        }
        return std::nullopt;
    }
    static std::optional<Type> checkPercent(const Type &a, const Type &b)
    {
        if(a.is_integral())
        {
            if(a.is_equal(b)) return a.strip_lvalue();
            auto res = canBinOpLiteral(a, b);
            if(res && res->is_floating_point()) return std::nullopt;
            if(res) return res->strip_lvalue();
            return std::nullopt;
        }
        return std::nullopt;
    }
    static std::optional<Type> checkCmp(const Type& a, const Type& b)
    {
        if(a.is_integral() || a.is_floating_point())
        {
            if(a.is_equal(b)) return Type{"bool", {}};
            auto res = canBinOpLiteral(a, b);
            if(res) return Type{"bool", {}};
            return std::nullopt;
        }
        return std::nullopt;
    }
    static std::optional<Type> checkBitOr(const Type &a, const Type &b)
    {
        if(a.is_integral())
        {
            if(a.is_equal(b)) return a.strip_lvalue();
            return std::nullopt;
        }
        return std::nullopt;
    }
    static std::optional<Type> checkBitXor(const Type &a, const Type &b)
    {
        if(a.is_integral())
        {
            if(a.is_equal(b)) return a.strip_lvalue();
            return std::nullopt;
        }
        return std::nullopt;
    }
    static std::optional<Type> checkBitAnd(const Type &a, const Type &b)
    {
        if(a.is_integral())
        {
            if(a.is_equal(b)) return a.strip_lvalue();
            return std::nullopt;
        }
        return std::nullopt;
    }
    static std::optional<Type> unaryNegateResult(Module* mod, const Type& typ)
    {
        if(typ.is_unsigned_integral()) return std::nullopt;
        if(typ.is_signed_integral() || typ.is_floating_point()) return typ.strip_lvalue();
        if(auto fn = mod->findFunction("__op_unary_negate__" + typ.name))
            return fn->returnType;
        return std::nullopt;
    }
    static std::optional<Type> unaryNotResult(Module* module, const Type& type)
    {
        if(type.is_boolean()) return type;
        if(auto fn = module->findFunction("__op_unary_not__" + type.name))
            return fn->returnType;
        return std::nullopt;
    }

    std::optional<FunctionType> ExpressionTypeChecker::operator()(ArrayLiteral* lit)
    {
        Type t{.name = "__arr", .subtypes = {}};
        Type subtype;
        for(auto& elem : lit->elements)
        {
            auto sub_t = std::visit(*this, elem->toVariant());
            if(!sub_t) return std::nullopt;
            subtype = Type::variant_merge(std::move(subtype), std::move(sub_t).value());
        }
        return subtype;
    }

    std::optional<FunctionType> checkDot(BinaryOperation* expr, const Type& lhs, IRGenerator* irgen)
    {
        //called and stored functions can be called via object.invoke()
        if(lhs.name == "__called_fn")
        {
            if(auto nm = dynamic_cast<NameExpression*>(expr->rhs.get()))
            {
                if(nm->token.text == "invoke")
                {
                    return FunctionType{*lhs.signature, false};
                }
            }
            return std::nullopt;
        }
        if(lhs.name == "__tup")
        {
            if(auto idx = dynamic_cast<IntegerLiteral*>(expr->rhs.get()))
            {
                return lhs.subtypes[std::stol(std::string{idx->token.text})];
            }
            return std::nullopt;
        }
        if(auto cls = lhs.get_decl_if_class(irgen))
        {
            if(auto* name_expr = dynamic_cast<NameExpression*>(expr->rhs.get()))
            {
                std::string name(name_expr->token.text);
                if(auto var = std::ranges::find_if(cls->vars, [&name](ClassVariable& v)
                {
                    return name == v.name;
                }); var != cls->vars.end())
                {
                    //accessing an l-value struct yields an l-value
                    return Type{
                        .name = var->type.name, .subtypes = var->type.subtypes, .is_mutable = lhs.is_mutable,
                        .is_lvalue = lhs.is_lvalue
                    };
                }
                if(auto var = std::ranges::find_if(cls->methods, [&name](ClassMethod& m)
                {
                    return name == m.name;
                }); var != cls->methods.end())
                {
                    auto decl = reinterpret_cast<FunctionDeclaration*>(var->function_decl.get());
                    if(decl->signature.parameters[0].type.name != "This")
                        return std::nullopt;
                    if(decl->signature.parameters[0].convention == ParamType::InOut && !lhs.is_mutable)
                        return std::nullopt;
                    return FunctionType{decl->signature, true};
                }
            }
        }
        auto rhs = std::visit(ExpressionTypeChecker{irgen}, expr->rhs->toVariant());
        if(!rhs || !rhs->is_function()) return std::nullopt;
        auto& as_function = reinterpret_cast<FunctionType&>(*rhs);
        if(as_function.is_bound) return std::nullopt;
        if(!as_function.sig.parameters[0].type.is_equal(lhs))
        {
            return std::nullopt;
        }
        if(as_function.sig.parameters[0].convention == ParamType::InOut && !lhs.is_mutable) return std::nullopt;
        as_function.is_bound = true;
        return as_function;
    }

    std::optional<FunctionType> ExpressionTypeChecker::operator()(BinaryOperation* expr)
    {
        auto lhs = std::visit(*this, expr->lhs->toVariant());
        std::optional<Type> rhs;
        if(expr->op.type != TokenType::Dot) rhs = std::visit(*this, expr->rhs->toVariant());
        //builtin operators
        if(expr->op.type != TokenType::Dot && (!lhs || !rhs)) return std::nullopt;
        switch(expr->op.type)
        {
            using enum TokenType;
        case Plus: return checkAddition(*lhs, *rhs);
        case Star: return checkStar(*lhs, *rhs);
        case Minus: return checkMinus(*lhs, *rhs);
        case Slash: return checkDivide(*lhs, *rhs);
        case Percent: return checkPercent(*lhs, *rhs);
        case DoubleEqual: [[fallthrough]];
        case GreaterEqual: [[fallthrough]];
        case LessEqual: [[fallthrough]];
        case Less: [[fallthrough]];
        case Greater: [[fallthrough]];
        case BangEqual: return checkCmp(*lhs, *rhs);
        case Pipe: return checkBitOr(*lhs, *rhs);
        case Caret: return checkBitXor(*lhs, *rhs);
        case Ampersand: return checkBitAnd(*lhs, *rhs);
        case Dot: return checkDot(expr, *lhs, irgen);
        case Equal: return checkAssign(*lhs, *rhs);
        }
        return std::nullopt;
    }

    std::optional<FunctionType> ExpressionTypeChecker::operator()(LogicalOperation*)
    {
        //TODO
    }

    std::optional<FunctionType> ExpressionTypeChecker::operator()(NameExpression* expr)
    {
        std::string name(expr->token.text);
        for(size_t i = irgen->variables.size(); i > 0; --i)
        {
            size_t idx = i - 1;
            if(auto var = irgen->variables[idx].find(name); var != irgen->variables[idx].end())
            {
                auto decl = var->second.second;
                auto t = decl->type ? decl->type.value() : std::visit(*this, decl->initializer->toVariant());
                if(t->is_lambda())
                {
                    return std::visit(*this, decl->initializer->toVariant());
                }
                t->is_mutable = decl->is_mut;
                t->is_lvalue = true;
                return t;
            }
        }
        if(auto fn = irgen->module->findFunction(irgen->module->module_hash + name))
        {
            return FunctionType{*fn, false};
        }
        return std::nullopt;
    }

    std::optional<FunctionType> ExpressionTypeChecker::operator()(PostfixOperation*)
    {

    }

    std::optional<FunctionType> ExpressionTypeChecker::operator()(SubscriptOperation*)
    {

    }

    std::optional<FunctionType> ExpressionTypeChecker::operator()(LambdaExpression* lmd)
    {
        auto fn_t = FunctionType(lmd->sig, false);
        fn_t.name = "__lambda" + lmd->hash;
        return fn_t;
    }

    std::optional<FunctionType> ExpressionTypeChecker::operator()(TupleLiteral* tup)
    {
        //target type can modify the type of tuple literals
        bool consider_target = target && target->is_tuple() && target->subtypes.size() == tup->elements.size();
        Type tp{"__tup"};
        for(size_t i = 0; i < tup->elements.size(); ++i)
        {
            auto type_i = std::visit(ExpressionTypeChecker{irgen}, tup->elements[i]->toVariant());
            if(!type_i) return std::nullopt;
            //if we can implicit convert to the target type we use that
            if(consider_target && target->subtypes[i].is_assignable_from(*type_i))
            {
                tp.subtypes.push_back(target->subtypes[i]);
                continue;
            }
            //IMPORTANT: Normally we cant evaluate expression when type checking, but tuples need to resolve literals
            if(type_i->name == "ilit" || type_i->name == "flit")
            {
                auto val = std::visit(ExpressionEvaluator{irgen}, tup->elements[i]->toVariant());
                type_i = irgen->reduceLiteral(*type_i, val);
            }
            tp.subtypes.push_back(*type_i);
        }
        return tp;
    }


    std::optional<FunctionType> ExpressionTypeChecker::operator()(BooleanLiteral*)
    {
        return Type{.name = "bool", .subtypes = {}};
    }

    std::optional<FunctionType> ExpressionTypeChecker::operator()(GroupingExpression* expr)
    {
        return std::visit(*this, expr->toVariant());
    }

    std::optional<FunctionType> ExpressionTypeChecker::operator()(IntegerLiteral* lit)
    {
        return Type{.name = "ilit", .subtypes = {}};
    }


    std::optional<FunctionType> ExpressionTypeChecker::operator()(PrefixOperation* op)
    {
        auto op_type_opt = std::visit(*this, op->operand->toVariant());
        if(!op_type_opt) return std::nullopt;
        Type& op_type = op_type_opt.value();
        switch(op->op.type)
        {
            using enum TokenType;
        case Minus: return unaryNegateResult(irgen->module, op_type);
        case Bang: return unaryNotResult(irgen->module, op_type);
        default: return std::nullopt;
        }

    }

    std::optional<FunctionType> ExpressionTypeChecker::operator()(CallOperation* op)
    {
        auto callee_ty = std::visit(*this, op->callee->toVariant());
        if(!callee_ty) return std::nullopt;
        if(!callee_ty->is_function() && !callee_ty->is_lambda()) return std::nullopt;
        auto& as_fn = reinterpret_cast<FunctionType&>(*callee_ty);
        //If the function is bound (something.function()) we skip checking the first args type
        if(op->arguments.size() + callee_ty->is_bound != as_fn.sig.parameters.size()) return std::nullopt;
        for(size_t i = callee_ty->is_bound; i < as_fn.sig.parameters.size(); ++i)
        {
            //inout parameters cannot be implicitly converted
            if(as_fn.sig.parameters[i].convention == ParamType::InOut)
            {
                auto arg = std::visit(*this, op->arguments[i]->toVariant());
                bool is_valid = arg && arg->is_lvalue && arg->is_equal(as_fn.sig.parameters[i].type);
                if(!is_valid) return std::nullopt;
            }
            else if(!as_fn.sig.parameters[i].type.is_assignable_from(*std::visit(*this, op->arguments[i]->toVariant()))) return std::nullopt;
        }
        return as_fn.sig.returnType;
    }

    std::optional<FunctionType> ExpressionTypeChecker::operator()(RealLiteral*)
    {
        return Type{.name = "flit", .subtypes = {}};
    }

    std::optional<FunctionType> ExpressionTypeChecker::operator()(StringLiteral*)
    {
        return Type{.name = "string", .subtypes = {}};
    }


}