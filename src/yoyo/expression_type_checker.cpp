#include <csignal>

#include "ir_gen.h"
#include "fn_type.h"
namespace Yoyo
{
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
        //auto deref is not done at type level, because composite types with references are not assignable to thier
        //value counterparts ( i32? = {&i32}? is an error )
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
                    //accessing an l-value struct yields an l-value
                    return Type{
                        .name = var->type.name, .subtypes = var->type.subtypes, .module = var->type.module,
                        .is_mutable = lhs.is_mutable,
                        .is_lvalue = lhs.is_lvalue,
                    };
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
                auto decl = var->second.second;
                auto t = decl->type ? decl->type.value() : std::visit(*this, decl->initializer->toVariant());
                if(t->is_lambda())
                {
                    return std::visit(*this, decl->initializer->toVariant());
                }
                t->is_mutable = decl->is_mut;
                t->is_lvalue = true;
                t->saturate(irgen->module);
                return t;
            }
        }
        if(auto fn = irgen->module->findFunction(irgen->module->module_hash + name))
        {
            irgen->saturateSignature(*fn, irgen->module);
            return FunctionType{*fn, false};
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

    std::optional<FunctionType> ExpressionTypeChecker::operator()(ScopeOperation* scp)
    {
        //Everything till the second to last must be a module
        Module* md = irgen->module;
        if(!scp->scope.empty())
        {
            auto split_name = split(scp->scope, "::");
            for(size_t i = 0; i < split_name.size() - 2; i++)
            {
                auto str = std::string{split_name[i]};
                if(!md->modules.contains(str)) irgen->error();
                md = md->modules.at(str);
            }

        }
        ClassDeclaration* decl = nullptr;
        if(md->modules.contains(scp->type.name))
            md = md->modules.at(scp->type.name);
        else if(md->classes.contains(scp->type.name))
            decl = std::get<2>(md->classes.at(scp->type.name)).get();
        else if(md->enums.contains(scp->type.name))
        {
            if(!md->enums.at(scp->type.name)->values.contains(scp->name)) return std::nullopt;
            Type ty = scp->type;
            ty.module = md;
            return ty;
        }

        //check types in scope
        else if(md == irgen->module)
        {
            //TODO
        }

        return checkNameWithinClassOrModule(md, decl,scp->name);
    }

    std::optional<FunctionType> ExpressionTypeChecker::operator()(ObjectLiteral* obj)
    {
        obj->t.saturate(irgen->module);
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
        return Type{"__null"};
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
        return Type{"char"};
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

    std::optional<FunctionType> ExpressionTypeChecker::checkNameWithinClassOrModule(Module* module, ClassDeclaration* type,
        std::string_view name)
    {
        if(type)
        {
            for(auto& method : type->methods)
            {
                if(method.name == name)
                {
                    auto decl = reinterpret_cast<FunctionDeclaration*>(method.function_decl.get());
                    irgen->saturateSignature(decl->signature, module);
                    auto ret_val = FunctionType{decl->signature, false};
                    ret_val.module = module;
                    return std::move(ret_val);
                }
            }
            return std::nullopt;
        }
        if(auto fn = module->findFunction(module->module_hash + std::string{name}))
        {
            //it should already be saturated at this point
            irgen->saturateSignature(*fn, module);
            auto ret_val = FunctionType{*fn, false};
            ret_val.module = module;
            return std::move(ret_val);
        }
        return std::nullopt;
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
                return Type{"__ref", {std::move(op_type)}};
            }
        case RefMut:
            {
                if(op_type.is_reference()) return std::nullopt;
                if(!op_type.is_mutable) return std::nullopt;
                return Type{"__ref_mut", {std::move(op_type)}};
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
        return Type{.name = "flit", .subtypes = {}};
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
        return Type{.name = "str", .subtypes = {}};
    }


}