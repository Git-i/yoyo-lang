#include <csignal>
#include <overload_resolve.h>
#include <ranges>

#include "ir_gen.h"
#include "fn_type.h"
namespace Yoyo
{
    std::optional<Type> peerResolve(std::ranges::forward_range auto types)
    {
        std::optional<Type> result = (*types.begin()).first;
        for(auto type :
            std::ranges::subrange(types.begin() + 1, types.end()) | std::views::keys)
        {
            if (!result->is_assignable_from(type))
            {
                if (type.is_assignable_from(*result)) result = type;
                else return std::nullopt;
            }
        }
        return result;
    }
    //defined in type.cpp
    extern std::vector<std::string_view> split(std::string_view str, std::string_view delim);
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
        if(a.name == "ilit" && b.name == "ilit") return a;
        auto ty = resolveAdd(a, b);
        if(ty) return ty->result; return std::nullopt;
    }
    static std::optional<Type> checkAssign(const Type &a, const Type &b)
    {
        if(!a.is_mutable) return std::nullopt;
        if(!a.is_assignable_from(b))
        {
            return std::nullopt;
        }
        return Type{.name="void", .module = a.module->engine->modules.at("__builtin").get()};
    }
    static std::optional<Type> checkMinus(const Type &a, const Type &b)
    {
        if(a.name == "ilit" && b.name == "ilit") return a;
        auto ty = resolveSub(a, b);
        if(ty) return ty->result; return std::nullopt;
    }
    static std::optional<Type> checkStar(const Type &a, const Type &b)
    {
        if(a.name == "ilit" && b.name == "ilit") return a;
        auto ty = resolveMul(a, b);
        if(ty) return ty->result; return std::nullopt;
    }
    static std::optional<Type> checkDivide(const Type &a, const Type &b)
    {
        if(a.name == "ilit" && b.name == "ilit") return a;
        auto ty = resolveDiv(a, b);
        if(ty) return ty->result; return std::nullopt;
    }
    static std::optional<Type> checkPercent(const Type &a, const Type &b)
    {
        if(a.name == "ilit" && b.name == "ilit") return a;
        auto ty = resolveRem(a, b);
        if(ty) return ty->result; return std::nullopt;
    }
    static std::optional<Type> checkCmp(const Type& a, const Type& b)
    {
        if(a.is_integral() || a.is_floating_point())
        {
            if(a.is_equal(b)) return Type{.name="bool", .module = a.module->engine->modules.at("__builtin").get()};
            auto res = canBinOpLiteral(a, b);
            if(res) return Type{.name="bool", .module = a.module->engine->modules.at("__builtin").get()};
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

        return std::nullopt;
    }
    static std::optional<Type> unaryNotResult(Module* module, const Type& type)
    {
        if(type.is_boolean()) return type;
        return std::nullopt;
    }
    std::optional<FunctionType> ExpressionTypeChecker::operator()(ArrayLiteral* lit)
    {
        auto subtype = peerResolve(lit->elements | std::views::transform([this](std::unique_ptr<Expression>& elem)
        {
            return std::make_pair(std::visit(*this, elem->toVariant()).value_or(Type{}), elem.get());
        }));
        if(!subtype) return std::nullopt;

        return Type{.name = "__arr" + std::to_string(lit->elements.size()), .subtypes = {*subtype},
            .module = subtype->module->engine->modules.at("__builtin").get()};
    }

    std::optional<FunctionType> checkDot(BinaryOperation* expr, const Type& lhs, IRGenerator* irgen)
    {
        //called and stored functions can be called via object.invoke()
        if(lhs.name == "__called_fn")
        {
            if(auto nm = dynamic_cast<NameExpression*>(expr->rhs.get()))
            {
                if(nm->text == "invoke")
                {
                    return FunctionType{*lhs.signature, false};
                }
            }
            return std::nullopt;
        }
        if(lhs.deref().is_tuple())
        {
            if(auto idx = dynamic_cast<IntegerLiteral*>(expr->rhs.get()))
            {
                return lhs.deref().subtypes[std::stol(std::string{idx->text})].take_mutability_characteristics(lhs.deref());
            }
            return std::nullopt;
        }
        if(auto cls = lhs.deref().get_decl_if_class(irgen))
        {
            if(auto* name_expr = dynamic_cast<NameExpression*>(expr->rhs.get()))
            {
                std::string name(name_expr->text);
                if(auto var = std::ranges::find_if(cls->vars, [&name](ClassVariable& v)
                {
                    return name == v.name;
                }); var != cls->vars.end())
                {
                    var->type.saturate(irgen->module, irgen);
                    //accessing an l-value struct yields an l-value
                    Type t = var->type;
                    t.is_mutable = lhs.is_mutable;
                    t.is_lvalue = lhs.is_lvalue;
                    return t;
                }
                if(auto var = std::ranges::find_if(cls->methods, [&name](ClassMethod& m)
                {
                    return name == m.name;
                }); var != cls->methods.end())
                {
                    auto decl = reinterpret_cast<FunctionDeclaration*>(var->function_decl.get());
                    if(decl->signature.parameters[0].name != "this")
                        return std::nullopt;
                    return FunctionType{decl->signature, true};
                }
            }
        }
        auto rhs = std::visit(ExpressionTypeChecker{irgen}, expr->rhs->toVariant());
        if(!rhs || !rhs->is_function()) return std::nullopt;
        auto& as_function = reinterpret_cast<FunctionType&>(*rhs);
        if(as_function.is_bound) return std::nullopt;
        if(!as_function.sig.parameters[0].type.can_accept_as_arg(lhs))
            return std::nullopt;
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
        default: ;//TODO
        }
        return std::nullopt;
    }

    std::optional<FunctionType> ExpressionTypeChecker::operator()(LogicalOperation*)
    {
        //TODO
        return std::nullopt;
    }

    std::optional<FunctionType> ExpressionTypeChecker::operator()(NameExpression* expr)
    {
        std::string name(expr->text);
        for(size_t i = irgen->variables.size(); i > 0; --i)
        {
            size_t idx = i - 1;
            if(auto var = irgen->variables[idx].find(name); var != irgen->variables[idx].end())
            {
                auto& type = var->second.second;
                //its only lvalue if its not last use
                bool is_last_use = irgen->function_cfgs.back().last_uses.at(name).contains(expr);
                type.is_lvalue = !is_last_use;
                type.saturate(irgen->module, irgen);
                return type;
            }
        }
        if(auto [name_prefix, fn] = irgen->module->findFunction(irgen->module->module_hash, name); fn)
        {
            irgen->saturateSignature(fn->sig, irgen->module);
            return FunctionType{fn->sig, false};
        }
        return std::nullopt;
    }
    void generic_replace(Type& type, const std::string& generic, const Type& other)
    {
        if(type.name == generic)
            type = other;
        for(auto& sub: type.subtypes)
            generic_replace(sub, generic, other);
    }
    std::optional<FunctionType> ExpressionTypeChecker::operator()(GenericNameExpression* expr)
    {
        // todo: inline generic functions (probably disallow those)
        if(auto[hash, decl] = irgen->module->findGenericFn(irgen->block_hash, expr->text); decl)
        {
            if(expr->arguments.size() != decl->clause.types.size()) return std::nullopt;
            FunctionSignature new_sig = decl->signature;
            for(size_t i = 0; i < decl->clause.types.size(); ++i)
            {
                expr->arguments[i].saturate(irgen->module, irgen);
                generic_replace(new_sig.returnType, decl->clause.types[i], expr->arguments[i]);
                for(auto& param : new_sig.parameters)
                    generic_replace(param.type, decl->clause.types[i], expr->arguments[i]);
            }
            return FunctionType{std::move(new_sig), false};
        }
        return std::nullopt;
    }

    std::optional<FunctionType> ExpressionTypeChecker::operator()(PostfixOperation*)
    {
        return std::nullopt;
    }

    std::optional<FunctionType> ExpressionTypeChecker::operator()(SubscriptOperation*)
    {
        return std::nullopt;
    }

    std::optional<FunctionType> ExpressionTypeChecker::operator()(LambdaExpression* lmd)
    {
        auto fn_t = FunctionType(lmd->sig, false);
        fn_t.name = "__lambda" + lmd->hash;
        return fn_t;
    }
    bool advanceScope(Type& type, Module*& md, std::string& hash, IRGenerator* irgen)
    {
        if(md->modules.contains(type.name))
        {
            md = md->modules.at(type.name);
            hash = md->module_hash;
            return true;
        }
        if(auto dets = md->findType(hash, type.name))
        {
            hash = std::get<0>(*dets);
            return true;
        }
        if(auto [name,fn] = md->findFunction(hash, type.name); fn)
        {
            hash = name + type.name + "__";
            return true;
        }
        if(auto [this_hash, fn] = md->findGenericFn(hash, type.name); fn)
        {
            if(type.subtypes.size() != fn->clause.types.size()) return false;
            for(auto& sub : type.subtypes) sub.saturate(md, irgen);
            auto mangled_name = fn->name + IRGenerator::mangleGenericArgs(type.subtypes);
            if(auto [_, exists] = md->findFunction(this_hash, mangled_name); !exists)
                ExpressionEvaluator{irgen}.generateGenericFunction(md, this_hash, fn, type.subtypes);
            hash = this_hash + mangled_name + "__";
            return true;
        }
        if(auto alias = md->findAlias(hash, type.name); alias)
        {
            advanceScope(*alias, md, hash, irgen);
        }
        return false;
    }
    std::optional<FunctionType> ExpressionTypeChecker::operator()(ScopeOperation* scp)
    {
        Module* md = irgen->module;
        std::string hash = irgen->block_hash;
        Module::ClassDetails* det = nullptr;
        auto iterator = UnsaturatedTypeIterator(scp->type);
        std::vector<Type> tps;
        while(!iterator.is_end())
        {
            auto type = iterator.next();
            if(!advanceScope(type, md, hash, irgen)) return std::nullopt;
        }
        auto last = iterator.last();
        if(auto [name, fn] = md->findFunction(hash, last.name); fn)
        {
            irgen->saturateSignature(fn->sig, md);
            auto t = FunctionType{fn->sig, false};
            t.block_hash = hash;
            t.module = md;
            return t;
        }
        return std::nullopt;
    }

    std::optional<FunctionType> ExpressionTypeChecker::operator()(ObjectLiteral* obj)
    {
        obj->t.saturate(irgen->module, irgen);
        auto decl = obj->t.get_decl_if_class(irgen);
        if(!decl) return std::nullopt;
        if(obj->values.size() != decl->vars.size()) return std::nullopt;
        //TODO: access specifier checking
        for(auto& var : decl->vars)
        {
            if(!obj->values.contains(var.name)) return std::nullopt;
            auto& expr = obj->values.at(var.name);
            auto expr_t = std::visit(ExpressionTypeChecker{irgen, var.type}, expr->toVariant());
            if(!expr_t) return std::nullopt;
            auto as_mut = var.type;
            as_mut.is_mutable = true; as_mut.is_lvalue = true;
            if(!as_mut.is_assignable_from(*expr_t)) return std::nullopt;
        }
        return obj->t;
    }

    std::optional<FunctionType> ExpressionTypeChecker::operator()(NullLiteral* null)
    {
        return Type{.name="__null",.module = irgen->module->engine->modules.at("__builtin").get()};
    }

    std::optional<FunctionType> ExpressionTypeChecker::operator()(AsExpression* expr)
    {
        auto to = std::visit(*this, expr->expr->toVariant());
        if(!to || to->is_void()) return std::nullopt;
        if(to->is_integral())
        {

        }
    }

    std::optional<FunctionType> ExpressionTypeChecker::operator()(CharLiteral*)
    {
        return Type{.name = "char", .module = irgen->module->engine->modules.at("__builtin").get()};
    }

    std::optional<FunctionType> ExpressionTypeChecker::operator()(TupleLiteral* tup)
    {
        //target type can modify the type of tuple literals
        bool consider_target = target && target->is_tuple() && target->subtypes.size() == tup->elements.size();
        Type tp{.name="__tup", .module = irgen->module->engine->modules.at("__builtin").get()};
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
        return Type{.name = "bool", .module = irgen->module->engine->modules.at("__builtin").get()};
    }

    std::optional<FunctionType> ExpressionTypeChecker::operator()(GroupingExpression* expr)
    {
        return std::visit(*this, expr->expr->toVariant());
    }


    bool ExpressionTypeChecker::hasToStr(const Type& t)
    {
        if(t.is_builtin()) return true;
        if(t.is_tuple())
        {
            for(const auto& t : t.subtypes) if(!hasToStr(t)) return false;
            return true;
        }
        if(t.is_enum()) return true;
        if(t.is_optional()) return hasToStr(t.subtypes[0]);
        if(t.is_char()) return true;
        return false;
    }

    std::optional<FunctionType> ExpressionTypeChecker::operator()(IntegerLiteral*)
    {
        return Type{.name = "ilit", .module = irgen->module->engine->modules.at("__builtin").get()};
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
        case Star:
            {
                if(op_type.is_mutable_reference())  return op_type.deref().make_mut();
                if(op_type.is_reference()) return op_type.deref();
                return std::nullopt;
            }
        case Ampersand:
            {
                //ref to ref is invalid
                if(op_type.is_reference()) return std::nullopt;
                return Type{.name="__ref", .subtypes={std::move(op_type)},
                    .module = irgen->module->engine->modules.at("__builtin").get()};
            }
        case RefMut:
            {
                if(op_type.is_reference()) return std::nullopt;
                if(!op_type.is_mutable) return std::nullopt;
                return Type{.name="__ref_mut", .subtypes={std::move(op_type)},
                    .module = irgen->module->engine->modules.at("__builtin").get()};
            }
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
        for(size_t i = 0; i < op->arguments.size(); ++i)
        {
            auto tp = std::visit(*this, op->arguments[i]->toVariant());
            if(!tp) return std::nullopt;
            if(!as_fn.sig.parameters[i + callee_ty->is_bound].type.can_accept_as_arg(*tp)) return std::nullopt;
        }
        return as_fn.sig.returnType;
    }

    std::optional<FunctionType> ExpressionTypeChecker::operator()(RealLiteral*)
    {
        return Type{.name = "flit", .module = irgen->module->engine->modules.at("__builtin").get()};
    }

    std::optional<FunctionType> ExpressionTypeChecker::operator()(StringLiteral* lit)
    {
        for(auto& val: lit->literal)
        {
            if(std::holds_alternative<std::unique_ptr<Expression>>(val))
            {
                auto& expr = std::get<std::unique_ptr<Expression>>(val);
                auto ty = std::visit(*this, expr->toVariant());
                if(!ty) return std::nullopt;
                if(!hasToStr(*ty)) return std::nullopt;
            }
        }
        return Type{.name = "str",
            .module = irgen->module->engine->modules.at("__builtin").get()};
    }


}