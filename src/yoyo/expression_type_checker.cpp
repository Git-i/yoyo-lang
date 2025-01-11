#include <csignal>
#include <overload_resolve.h>
#include <ranges>

#include "ir_gen.h"
#include "fn_type.h"
namespace Yoyo
{
    ExpressionTypeChecker::Result peerResolve(std::ranges::forward_range auto types)
    {
        FunctionType result = (*types.begin()).first.value_or_error();
        for(const auto& type :
            std::ranges::subrange(types.begin() + 1, types.end()))
        {
            auto tp = type.first.value_or_error();
            if (!result.is_assignable_from(tp))
            {
                if (tp.is_assignable_from(result)) result = tp;
                else return { Error(type.second, "") };
            }
        }
        return { result };
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
        if(!a.is_mutable || !a.is_lvalue) return std::nullopt;
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
    ExpressionTypeChecker::Result ExpressionTypeChecker::operator()(ArrayLiteral* lit)
    {
        auto subtype = peerResolve(lit->elements | std::views::transform([this](std::unique_ptr<Expression>& elem)
        {
            return std::make_pair(std::visit(*this, elem->toVariant()), elem.get());
        }));
        if (!subtype)
        {
            auto err = Error(lit, "Cannot determine array type");
            err.markers.emplace_back(subtype.error().span, "This element is not convertible to the others encountered");
            return { err };
        }
        return { Type{.name = "__arr_s" + std::to_string(lit->elements.size()), .subtypes = {*subtype},
            .module = subtype->module->engine->modules.at("__builtin").get()} };
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
        auto cls = lhs.deref().get_decl_if_class();
        if(cls)
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
        if (!rhs) return std::nullopt;
        if (rhs->is_interface_function() && cls)
        {
            using namespace std::string_view_literals;
            auto pos = rhs->name.find_first_of('$');
            std::string name = rhs->name.substr("__interface_fn"sv.size(), pos - "__interface_fn"sv.size());
            std::string fn_name = rhs->name.substr(pos + 1);
            auto it = std::ranges::find_if(cls->impls, [&name](auto& tp) {
                return tp.impl_for.full_name() == name;
                });
            if (it == cls->impls.end()) return std::nullopt;
            auto method = std::ranges::find_if(it->methods, [&fn_name](auto& tp) {
                return tp->name == fn_name;
                });
            return FunctionType{ (*method)->signature, true };
        }
        if (!rhs->is_function()) return std::nullopt;
        auto& as_function = reinterpret_cast<FunctionType&>(*rhs);
        if(as_function.is_bound) return std::nullopt;
        if(!as_function.sig.parameters[0].type.can_accept_as_arg(lhs))
            return std::nullopt;
        as_function.is_bound = true;
        return as_function;
    }

    ExpressionTypeChecker::Result ExpressionTypeChecker::operator()(BinaryOperation* expr)
    {
        auto lhs = std::visit(*this, expr->lhs->toVariant()).value_or_error();
        Type rhs;
        if(expr->op.type != TokenType::Dot) rhs = std::visit(*this, expr->rhs->toVariant()).value_or_error();

        if (lhs.is_error_ty()) return { lhs };
        if (rhs.is_error_ty()) return { rhs };

        std::optional<FunctionType> result;
        switch(expr->op.type)
        {
            using enum TokenType;
        case Plus: result = checkAddition(lhs, rhs); break;
        case Star: result = checkStar(lhs, rhs); break;
        case Minus: result = checkMinus(lhs, rhs); break;
        case Slash: result = checkDivide(lhs, rhs); break;
        case Percent: result = checkPercent(lhs, rhs); break;
        case DoubleEqual: [[fallthrough]];
        case GreaterEqual: [[fallthrough]];
        case LessEqual: [[fallthrough]];
        case Less: [[fallthrough]];
        case Greater: [[fallthrough]];
        case BangEqual: result = checkCmp(lhs, rhs); break;
        case Pipe: result = checkBitOr(lhs, rhs); break;
        case Caret: result = checkBitXor(lhs, rhs); break;
        case Ampersand: result = checkBitAnd(lhs, rhs); break;
        case Dot: result = checkDot(expr, lhs, irgen); break;
        case Equal: result = checkAssign(lhs, rhs); break;
        default: ;//TODO
        }
        if (result) return { std::move(result).value() };
        if (!expr->op.is_assignment())
        {
            Error err(expr, "No operator '" + std::string{ expr->op.text } + "' exists for the given operands");
            err.markers.emplace_back(SourceSpan{ expr->lhs->beg, expr->lhs->end }, "Expression is of type: '" + lhs.pretty_name(irgen->block_hash) + "'");
            err.markers.emplace_back(SourceSpan{ expr->rhs->beg, expr->rhs->end }, "Expression is of type: '" + rhs.pretty_name(irgen->block_hash) + "'");
            return { err };
        } 
        else
        {
            if (!lhs.is_mutable)
            {
                Error err(expr, "Attempt to assign to immutable expression");
                err.markers.emplace_back(SourceSpan{ expr->lhs->beg, expr->lhs->end }, "Expression is immutable");
                return { err };
            }
            Error err(expr, "Attempt to assign between incompatible types");
            err.markers.emplace_back(SourceSpan{ expr->lhs->beg, expr->lhs->end }, "Expression is of type");
            err.markers.emplace_back(SourceSpan{ expr->rhs->beg, expr->rhs->end }, "Expression is of type");
            return { err };
        }
    }

    ExpressionTypeChecker::Result ExpressionTypeChecker::operator()(LogicalOperation*)
    {
        //TODO
        return {};
    }

    ExpressionTypeChecker::Result ExpressionTypeChecker::operator()(NameExpression* expr)
    {
        std::string name(expr->text);
        for(size_t i = irgen->variables.size(); i > 0; --i)
        {
            size_t idx = i - 1;
            if(auto var = irgen->variables[idx].find(name); var != irgen->variables[idx].end())
            {
                auto& type = std::get<1>(var->second);
                //its only lvalue if its not last use
                bool is_last_use = irgen->function_cfgs.back().last_uses.at(name).contains(expr);
                type.is_lvalue = !is_last_use;
                type.saturate(irgen->module, irgen);
                return { type };
            }
        }
        if(auto [name_prefix, fn] = irgen->module->findFunction(irgen->module->module_hash, name); fn)
        {
            irgen->saturateSignature(fn->sig, irgen->module);
            return { FunctionType{fn->sig, false} };
        }
        return { Error(expr, "Use of undeclared identifier '" + name + "'") };
    }
    void generic_replace(Type& type, const std::string& generic, const Type& other)
    {
        if(type.name == generic)
            type = other;
        for(auto& sub: type.subtypes)
            generic_replace(sub, generic, other);
    }
    ExpressionTypeChecker::Result ExpressionTypeChecker::operator()(GenericNameExpression* expr)
    {
        // todo: inline generic functions (probably disallow those)
        if(auto[hash, decl] = irgen->module->findGenericFn(irgen->block_hash, expr->text); decl)
        {
            if (expr->arguments.size() != decl->clause.types.size()) {
                return { Error(expr, "Generic argument count mismatch",
                    std::format("'{}' was declared with {} arguments here: @yspn[{}:{},{}:{}]", expr->text, 
                        decl->clause.types.size(), 
                        decl->beg.line, 
                        decl->beg.column, 
                        decl->body->beg.line, 
                        decl->body->beg.column)) };
            }
            FunctionSignature new_sig = decl->signature;
            for(size_t i = 0; i < decl->clause.types.size(); ++i)
            {
                expr->arguments[i].saturate(irgen->module, irgen);
                generic_replace(new_sig.returnType, decl->clause.types[i], expr->arguments[i]);
                for(auto& param : new_sig.parameters)
                    generic_replace(param.type, decl->clause.types[i], expr->arguments[i]);
            }
            return { FunctionType{std::move(new_sig), false} };
        }
        return { Error(expr, "Use of undeclared identifier '" + expr->text + "'") };
    }

    ExpressionTypeChecker::Result ExpressionTypeChecker::operator()(PostfixOperation*)
    {
        return {};
    }

    ExpressionTypeChecker::Result ExpressionTypeChecker::operator()(SubscriptOperation* op)
    {
        auto tp = std::visit(*this, op->object->toVariant()).value_or_error();
        if (tp.is_array())
            return { tp.is_mutable ? tp.subtypes[0].mutable_reference_to() : tp.subtypes[0].reference_to() };
        if (tp.is_mut_slice())
            return { tp.subtypes[0].mutable_reference_to() };
        if (tp.is_slice())
            return { tp.subtypes[0].reference_to() };
        if (tp.is_error_ty())
            return { tp };
        return { Error(op, "Operator [] is not defined for type tp") };
    }

    ExpressionTypeChecker::Result ExpressionTypeChecker::operator()(LambdaExpression* lmd)
    {
        auto fn_t = FunctionType(lmd->sig, false);
        fn_t.name = "__lambda" + lmd->hash;
        return { fn_t };
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
            hash = name + type.name + "__%";
            return true;
        }
        if (auto [hsh, interface] = md->findInterface(hash, type.name); interface)
        {
            hash = hsh + "%%" + type.name + "%%interface"; //interfaces are terminal and cannot have subtyes
            return true;
        }
        if(auto [this_hash, fn] = md->findGenericFn(hash, type.name); fn)
        {
            if(type.subtypes.size() != fn->clause.types.size()) return false;
            for(auto& sub : type.subtypes) sub.saturate(md, irgen);
            auto mangled_name = fn->name + IRGenerator::mangleGenericArgs(type.subtypes);
            if(auto [_, exists] = md->findFunction(this_hash, mangled_name); !exists)
                ExpressionEvaluator{irgen}.generateGenericFunction(md, this_hash, fn, type.subtypes);
            hash = this_hash + mangled_name + "__%";
            return true;
        }
        if(auto alias = md->findAlias(hash, type.name); alias)
        {
            advanceScope(*alias, md, hash, irgen);
        }
        return false;
    }
    ExpressionTypeChecker::Result ExpressionTypeChecker::operator()(ScopeOperation* scp)
    {
        using namespace std::string_view_literals;
        Module* md = irgen->module;
        std::string hash = irgen->block_hash;
        Module::ClassDetails* det = nullptr;
        auto iterator = UnsaturatedTypeIterator(scp->type);
        while(!iterator.is_end())
        {
            auto type = iterator.next();
            if (!advanceScope(type, md, hash, irgen)) {
                return { Error(scp, "The name '" + type.name + "' does not exist in \"" + hash + "\"") };
            }
        }
        auto last = iterator.last();
        if (hash.ends_with("%%interface"))
        {
            auto pos = hash.find_last_of('%', hash.size() - "%%interface"sv.size() - 1);
            auto name = hash.substr(pos + 1, hash.size() - "%%interface"sv.size() - pos - 1);
            auto [actual_hash, interface] = md->findInterface(hash, name);
            auto it = std::ranges::find_if(interface->methods, [&last](auto& mth) {
                return mth->name == last.name;
                });
            if (it == interface->methods.end()) return { Error(scp, "No method name '" + last.name + "' in the specified interface") };
            return { Type{"__interface_fn" + actual_hash + interface->name + "$" + last.name }};
        }
        if(auto [name, fn] = md->findFunction(hash, last.name); fn)
        {
            irgen->saturateSignature(fn->sig, md);
            auto t = FunctionType{fn->sig, false};
            t.block_hash = hash;
            t.module = md;
            return { t };
        }
        return { Error(scp, "The name '" + last.name + "' does not exist in \"" + hash + "\"") };
    }

    ExpressionTypeChecker::Result ExpressionTypeChecker::operator()(ObjectLiteral* obj)
    {
        obj->t.saturate(irgen->module, irgen);
        auto decl = obj->t.get_decl_if_class();
        if (!decl) return { Error(obj, "The type tp does not exist or is not a class/struct") };
        if (obj->values.size() != decl->vars.size())
        {
            //TODO
            std::string text = "The fields: ";
            for (auto& var : decl->vars)
                if (!obj->values.contains(var.name)) text.append(var.name + ", ");
            return { Error(obj, text) };
        }
        //TODO: access specifier checking
        for(auto& var : decl->vars)
        {
            if (!obj->values.contains(var.name)) return { Error(obj, "Type tp has no member named '" + var.name + "'") };
            auto& expr = obj->values.at(var.name);
            auto expr_t = std::visit(ExpressionTypeChecker{irgen, var.type}, expr->toVariant()).value_or_error();
            auto as_mut = var.type;
            as_mut.is_mutable = true; as_mut.is_lvalue = true;
            if(!as_mut.is_assignable_from(expr_t)) 
                return { Error(expr.get(), "Cannot assign tp1 to tp2")};
        }
        return { obj->t };
    }

    ExpressionTypeChecker::Result ExpressionTypeChecker::operator()(NullLiteral* null)
    {
        return { Type{.name = "__null",.module = irgen->module->engine->modules.at("__builtin").get()} };
    }

    ExpressionTypeChecker::Result ExpressionTypeChecker::operator()(AsExpression* expr)
    {
        auto from = std::visit(*this, expr->expr->toVariant()).value_or_error();
        expr->dest.saturate(irgen->module, irgen);
        if(from.is_variant())
        {
            for(auto& sub :  from.subtypes)
            {
                if(sub.is_equal(expr->dest))
                    return { Type{
                    "__conv_result_ref", {std::move(sub)}, nullptr,
                    irgen->module->engine->modules.at("__builtin").get(),from.is_mutable,from.is_lvalue
                } };
            }
            return { Error(expr, "The type tp must be one of the variant subtypes") };
        }
        if (from.is_error_ty()) return { from };
        return { Error(expr, "The as operator is undefined for type tp") };
    }

    ExpressionTypeChecker::Result ExpressionTypeChecker::operator()(CharLiteral*)
    {
        return { Type{.name = "char", .module = irgen->module->engine->modules.at("__builtin").get()} };
    }

    ExpressionTypeChecker::Result ExpressionTypeChecker::operator()(TupleLiteral* tup)
    {
        //target type can modify the type of tuple literals
        bool consider_target = target && target->is_tuple() && target->subtypes.size() == tup->elements.size();
        Type tp{.name="__tup", .module = irgen->module->engine->modules.at("__builtin").get()};
        for(size_t i = 0; i < tup->elements.size(); ++i)
        {
            auto type_i = std::visit(ExpressionTypeChecker{irgen}, tup->elements[i]->toVariant()).value_or_error();
            //if we can implicit convert to the target type we use that
            if(consider_target && target->subtypes[i].is_assignable_from(type_i))
            {
                tp.subtypes.push_back(target->subtypes[i]);
                continue;
            }
            //IMPORTANT: Normally we cant evaluate expression when type checking, but tuples need to resolve literals
            if(type_i.name == "ilit" || type_i.name == "flit")
            {
                auto val = std::visit(ExpressionEvaluator{irgen}, tup->elements[i]->toVariant());
                type_i = irgen->reduceLiteral(type_i, val);
            }
            tp.subtypes.push_back(type_i);
        }
        return { tp };
    }


    ExpressionTypeChecker::Result ExpressionTypeChecker::operator()(BooleanLiteral*)
    {
        return { Type{.name = "bool", .module = irgen->module->engine->modules.at("__builtin").get()} };
    }

    ExpressionTypeChecker::Result ExpressionTypeChecker::operator()(GroupingExpression* expr)
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

    ExpressionTypeChecker::Result ExpressionTypeChecker::operator()(IntegerLiteral*)
    {
        return { Type{.name = "ilit", .module = irgen->module->engine->modules.at("__builtin").get()} };
    }


    ExpressionTypeChecker::Result ExpressionTypeChecker::operator()(PrefixOperation* op)
    {
        auto op_type_opt = std::visit(*this, op->operand->toVariant());
        if (!op_type_opt) return op_type_opt;
        auto op_type = std::move(op_type_opt).value();
        switch(op->op.type)
        {
            using enum TokenType;
        case Minus: {
            auto res = unaryNegateResult(irgen->module, op_type);
            if (!res) return { Error(op, "Operator '-' is not defined for type tp") };
            return { std::move(res).value() };
        }
        case Bang: {
            auto res =  unaryNotResult(irgen->module, op_type);
            if (!res) return { Error(op, "Operator '!' is not defined for type tp") };
            return { std::move(res).value() };
        }
        case Star:
            {
            if (op_type.is_mutable_reference())  return { op_type.deref().make_mut() };
            if (op_type.is_reference()) return { op_type.deref() };
                return { Error(op, "Cannot dereference non-reference type") };
            }
        case Ampersand:
            {
                //ref to ref is invalid
                //if(op_type.is_reference()) return std::nullopt;
            return { Type{.name = "__ref", .subtypes = {std::move(op_type)},
                .module = irgen->module->engine->modules.at("__builtin").get()} };
            }
        case RefMut:
            {
                //if(op_type.is_reference()) return std::nullopt;
                if (!op_type.is_mutable) return { Error(op, "Cannot mutably reference an immutable expression") };
                return { Type{.name = "__ref_mut", .subtypes = {std::move(op_type)},
                    .module = irgen->module->engine->modules.at("__builtin").get()} };
            }
        default: break;
        }

    }

    ExpressionTypeChecker::Result ExpressionTypeChecker::operator()(CallOperation* op)
    {
        auto callee_ty = std::visit(*this, op->callee->toVariant()).value_or_error();
        if (callee_ty.is_error_ty()) return { std::move(callee_ty) };
        if (!callee_ty.is_function() && !callee_ty.is_lambda()) return { Error(op->callee.get(), "Attempt to call non-function expression")};
        auto& as_fn = reinterpret_cast<FunctionType&>(callee_ty);
        //If the function is bound (something.function()) we skip checking the first args type
        if (op->arguments.size() + callee_ty.is_bound != as_fn.sig.parameters.size()) {
            Error err(op, "Function argument count mismatch");
            err.markers.emplace_back(SourceSpan{ op->callee->beg, op->callee->end }, "This function was defined as: '" + as_fn.sig.pretty_name(irgen->block_hash) + "'");
            return { err };
        }
        for(size_t i = 0; i < op->arguments.size(); ++i)
        {
            auto tp = std::visit(*this, op->arguments[i]->toVariant()).value_or_error();
            if (!as_fn.sig.parameters[i + callee_ty.is_bound].type.can_accept_as_arg(tp))
            {
                auto& type = as_fn.sig.parameters[i + callee_ty.is_bound].type;
                Error err(op->arguments[i].get(), "Cannot convert argument to expected parameter type");
                err.markers.emplace_back(SourceSpan{ op->callee->beg, op->callee->end },
                    "Expected argument of type '" + type.pretty_name(irgen->block_hash) + "' becuase function is of type '" +
                    as_fn.sig.pretty_name(irgen->block_hash) + "'");
                err.markers.emplace_back(SourceSpan{ op->arguments[i]->beg, op->arguments[i]->end }, "Expression of type '" +
                    tp.pretty_name(irgen->block_hash) + "'");
                return { err };
            }
        }
        return { as_fn.sig.returnType };
    }

    ExpressionTypeChecker::Result ExpressionTypeChecker::operator()(RealLiteral*)
    {
        return { Type{.name = "flit", .module = irgen->module->engine->modules.at("__builtin").get()} };
    }

    ExpressionTypeChecker::Result ExpressionTypeChecker::operator()(StringLiteral* lit)
    {
        for(auto& val: lit->literal)
        {
            if(std::holds_alternative<std::unique_ptr<Expression>>(val))
            {
                auto& expr = std::get<std::unique_ptr<Expression>>(val);
                auto ty = std::visit(*this, expr->toVariant()).value_or_error();
                if (!hasToStr(ty)) 
                    return { Error(expr.get(), "Expression cannot be converted to string") };
            }
        }
        return { Type{.name = "str",
            .module = irgen->module->engine->modules.at("__builtin").get()} };
    }

    ExpressionTypeChecker::Result ExpressionTypeChecker::operator()(GCNewExpression* expr)
    {
        using namespace std::ranges;
        using namespace std::views;
        auto internal_ty = std::visit(*this, expr->target_expression->toVariant()).value_or_error();
        if (internal_ty.is_error_ty()) return { internal_ty };
        if (internal_ty.is_non_owning())
        {
            Error err(expr, "Cannot store non-owning type with the garbage collector");
            err.markers.emplace_back(SourceSpan{ expr->target_expression->beg, expr->target_expression->end },
                "Expression is of type '" + internal_ty.pretty_name(irgen->block_hash) + "' which is non owning");
            return { err };
        }
        auto tp = internal_ty.reference_to();
        tp.name = "__gcref";
        return { tp };
    }
}