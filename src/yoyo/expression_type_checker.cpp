#include <csignal>
#include <overload_resolve.h>
#include <ranges>

#include "ir_gen.h"
#include "fn_type.h"
namespace Yoyo
{
    ExpressionTypeChecker::Result peerResolve(std::ranges::forward_range auto types, IRGenerator* irgen)
    {
        FunctionType result = (*types.begin()).first.value_or_error();
        for(const auto& type :
            std::ranges::subrange(types.begin() + 1, types.end()))
        {
            auto tp = type.first.value_or_error();
            if (!result.is_assignable_from(tp, irgen))
            {
                if (tp.is_assignable_from(result, irgen)) result = tp;
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
    static std::optional<Type> checkAddition(const Type &a, const Type &b, IRGenerator* irgen)
    {
        if(a.name == "ilit" && b.name == "ilit") return a;
        auto ty = resolveAdd(a, b, irgen);
        if(ty) return ty->result; return std::nullopt;
    }
    static std::optional<Type> checkAssign(const Type &a, const Type &b, IRGenerator* irgen)
    {
        if (!a.is_mutable)
            return std::nullopt;
        if(!a.is_assignable_from(b, irgen))
        {
            __debugbreak();
            return std::nullopt;
        }
        return Type{.name="void", .module = a.module->engine->modules.at("core").get()};
    }
    static std::optional<Type> checkMinus(const Type &a, const Type &b, IRGenerator* irgen)
    {
        if(a.name == "ilit" && b.name == "ilit") return a;
        auto ty = resolveSub(a, b, irgen);
        if(ty) return ty->result; return std::nullopt;
    }
    static std::optional<Type> checkStar(const Type &a, const Type &b, IRGenerator* irgen)
    {
        if(a.name == "ilit" && b.name == "ilit") return a;
        auto ty = resolveMul(a, b, irgen);
        if(ty) return ty->result; return std::nullopt;
    }
    static std::optional<Type> checkDivide(const Type &a, const Type &b, IRGenerator* irgen)
    {
        if(a.name == "ilit" && b.name == "ilit") return a;
        auto ty = resolveDiv(a, b, irgen);
        if(ty) return ty->result; return std::nullopt;
    }
    static std::optional<Type> checkPercent(const Type &a, const Type &b, IRGenerator* irgen)
    {
        if(a.name == "ilit" && b.name == "ilit") return a;
        auto ty = resolveRem(a, b, irgen);
        if(ty) return ty->result; return std::nullopt;
    }
    static std::optional<Type> checkCmp(const Type& a, const Type& b)
    {
        if(a.is_integral() || a.is_floating_point())
        {
            if(a.is_equal(b)) return Type{.name="bool", .module = a.module->engine->modules.at("core").get()};
            auto res = canBinOpLiteral(a, b);
            if(res) return Type{.name="bool", .module = a.module->engine->modules.at("core").get()};
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
        }), irgen);
        if (!subtype)
        {
            auto err = Error(lit, "Cannot determine array type");
            err.markers.emplace_back(subtype.error().span, "This element is not convertible to the others encountered");
            return { err };
        }
        return { Type{.name = "__arr_s" + std::to_string(lit->elements.size()), .subtypes = {*subtype},
            .module = subtype->module->engine->modules.at("core").get()} };
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
        auto cls = lhs.deref().get_decl_if_class(irgen);
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
                    t.is_mutable = lhs.is_mutable || lhs.is_mutable_reference();
                    t.is_lvalue = lhs.is_reference() ? true : lhs.is_lvalue;
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
        if (lhs.deref().is_view())
        {
            auto& viewed = lhs.deref().subtypes[0];
            auto [hsh, interface] = viewed.module->findInterface(viewed.block_hash, viewed.name);
            if (auto* name_expr = dynamic_cast<NameExpression*>(expr->rhs.get()); name_expr && interface)
            {
                auto it = std::ranges::find_if(interface->methods, [name_expr](auto& mth) {
                    return mth->name == name_expr->text;
                    });
                if (it != interface->methods.end())
                {
                    FunctionSignature sig = (*it)->signature;
                    sig.parameters[0].type = lhs;
                    return FunctionType{ std::move(sig), true};
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
                return (tp.impl_for.block_hash + tp.impl_for.name + IRGenerator::mangleGenericArgs(tp.impl_for.subtypes)) == name;
                });
            if (it == cls->impls.end()) return std::nullopt;
            auto method = std::ranges::find_if(it->methods, [&fn_name](auto& tp) {
                return tp->name == fn_name;
                });
            return FunctionType{ (*method)->signature, true };
        }
        if (rhs->name.starts_with("__generic_fn")) {
            Type bound_ty;
            if (rhs->sig.parameters[0].type.is_reference() && !lhs.is_reference())
                bound_ty = Type{ rhs->sig.parameters[0].type.name, {lhs} };
            else bound_ty = lhs;
            rhs->subtypes.emplace_back(std::move(bound_ty));
            rhs->is_bound = true; return *rhs;
        }
        if (!rhs->is_function()) return std::nullopt;
        auto& as_function = reinterpret_cast<FunctionType&>(*rhs);
        if(as_function.is_bound) return std::nullopt;
        if(!as_function.sig.parameters[0].type.can_accept_as_arg(lhs, irgen))
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
        case Plus: result = checkAddition(lhs, rhs, irgen); break;
        case Star: result = checkStar(lhs, rhs, irgen); break;
        case Minus: result = checkMinus(lhs, rhs, irgen); break;
        case Slash: result = checkDivide(lhs, rhs, irgen); break;
        case Percent: result = checkPercent(lhs, rhs, irgen); break;
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
        case Equal: result = checkAssign(lhs, rhs, irgen); break;
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
        if (auto [name_prefix, fn] = irgen->module->findGenericFn(irgen->module->module_hash, name); fn)
        {
            auto ty = FunctionType(fn->signature, false);
            ty.name = "__generic_fn" + name;
            ty.module = irgen->module;
            ty.block_hash = name_prefix;
            return { std::move(ty) };
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
            
            ExpressionEvaluator{ irgen }.generateGenericFunction(irgen->module, hash, decl, expr->arguments);
            if (auto [_, fn_decl] = irgen->module->findFunction(hash,
                expr->text + IRGenerator::mangleGenericArgs(expr->arguments)); fn_decl)
            {
                return { FunctionType(fn_decl->sig, false) };
            }
            return { Error(expr, "Generic Initialization failed") };
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
        if (md->engine->modules.contains(type.name))
        {
            md = md->engine->modules.at(type.name).get();
            hash = md->module_hash;
            return true;
        }
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
        if (auto [hsh, decl] = md->findGenericClass(hash, type.name); decl)
        {
            if (type.subtypes.size() != decl->clause.types.size()) return false;
            for (auto& sub : type.subtypes) sub.saturate(md, irgen);
            auto mangled_name = decl->name + IRGenerator::mangleGenericArgs(type.subtypes);
            ExpressionEvaluator{ irgen }.generateGenericClass(md, hsh, decl, std::span{ type.subtypes });
            if (auto dets = md->findType(hash, mangled_name))
            {
                hash = std::get<0>(*dets); return true;
            }
            return false;
        }
        if(auto [name,fn] = md->findFunction(hash, type.name); fn)
        {
            hash = name + type.name + "::";
            return true;
        }
        if (auto [hsh, interface] = md->findInterface(hash, type.name); interface)
        {
            hash = hsh + "%%" + type.name + "%%interface"; //interfaces are terminal and cannot have subtyes
            return true;
        }
        if (auto [hsh, interface] = md->findGenericInterface(hash, type.name); interface)
        {
            if (type.subtypes.size() != interface->clause.types.size()) return false;
            for (auto& sub : type.subtypes) sub.saturate(irgen->module, irgen);
            ExpressionEvaluator{ irgen }.generateGenericInterface(md, hsh, interface, type.subtypes);
            std::string name = interface->name + IRGenerator::mangleGenericArgs(type.subtypes);
            if (auto [_, exists] = md->findInterface(hsh, name); exists)
            {
                hash = hsh + "%%" + name + "%%interface";
                return true;
            }
            return false;
        }
        if (auto [hsh, enm] = md->findEnum(hash, type.name); enm)
        {
            hash = hsh + "%%" + type.name + "%%enum"; //enums are also terminal and don't have subtypes
        }
        if(auto [this_hash, fn] = md->findGenericFn(hash, type.name); fn)
        {
            if(type.subtypes.size() != fn->clause.types.size()) return false;
            for(auto& sub : type.subtypes) sub.saturate(md, irgen);
            auto mangled_name = fn->name + IRGenerator::mangleGenericArgs(type.subtypes);
            if(auto [_, exists] = md->findFunction(this_hash, mangled_name); !exists)
                ExpressionEvaluator{irgen}.generateGenericFunction(md, this_hash, fn, type.subtypes);
            hash = this_hash + mangled_name + "::";
            return true;
        }
        if(auto alias = md->findAlias(hash, type.name); alias)
        {
            advanceScope(*alias, md, hash, irgen);
        }
        if (auto [hsh, galias] = md->findGenericAlias(hash, type.name); galias)
        {
            __debugbreak();
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
        while (!iterator.is_end())
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
            return { Type{"__interface_fn" + actual_hash + interface->name + "$" + last.name } };
        }
        if (hash.ends_with("%%enum"))
        {
            if (!last.subtypes.empty()) return { Error(scp, "Enum child cannot have subtypes") };
            auto pos = hash.find_last_of('%', hash.size() - "%%enum"sv.size() - 1);
            auto name = hash.substr(pos + 1, hash.size() - "%%enum"sv.size() - pos - 1);
            auto [actual_hash, enm] = md->findEnum(hash, name);
            if (enm->values.contains(last.name)) return { Type{.name = last.name, .module = md, .block_hash = actual_hash } };
            return { Error(scp, "Enum doesn't contain specified value") };
        }
        if (auto [name, fn] = md->findFunction(hash, last.name); fn)
        {
            irgen->saturateSignature(fn->sig, md);
            auto t = FunctionType{ fn->sig, false };
            t.block_hash = hash;
            t.module = md;
            return { t };
        }
        return { Error(scp, "The name '" + last.name + "' does not exist in \"" + hash + "\"") };
    }
    ExpressionTypeChecker::Result ExpressionTypeChecker::operator()(ObjectLiteral* obj)
    {
        obj->t.saturate(irgen->module, irgen);
        auto decl = obj->t.get_decl_if_class(irgen);
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
            if(!as_mut.is_assignable_from(expr_t, irgen)) 
                return { Error(expr.get(), "Cannot assign tp1 to tp2")};
        }
        return { obj->t };
    }

    ExpressionTypeChecker::Result ExpressionTypeChecker::operator()(NullLiteral* null)
    {
        return { Type{.name = "__null",.module = irgen->module->engine->modules.at("core").get()} };
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
                    irgen->module->engine->modules.at("core").get(),from.is_mutable,from.is_lvalue
                } };
            }
            return { Error(expr, "The type tp must be one of the variant subtypes") };
        }
        if (expr->dest.is_view())
        {
            //interface casts for dynamic dispatch
            auto& viewed = expr->dest.subtypes[0];
            if (!from.is_reference()) return { Error(expr, "Interface cast must be from a reference type") };
            if (auto cls = from.deref().get_decl_if_class(irgen))
            {
                auto it = std::ranges::find_if(cls->impls, [&viewed](auto& impl) {
                    return impl.impl_for.is_equal(viewed);
                    });
                if (it != cls->impls.end())
                {
                    if (expr->dest.is_mut_view())
                        if(from.is_mutable_reference() || from.is_gc_reference()) return { expr->dest };
                    if (expr->dest.is_gc_view())
                        if (from.is_gc_reference()) return { expr->dest };
                    if (expr->dest.is_view())
                        return { expr->dest };
                    return { Error(expr, "Reference characteristic mismatch") };
                }
                return { Error(expr, "Cannot perform interface cast as class does not implement interface") };
            }
        }
        if (from.is_error_ty()) return { from };
        return { Error(expr, "The as operator is undefined for type tp") };
    }

    ExpressionTypeChecker::Result ExpressionTypeChecker::operator()(CharLiteral*)
    {
        return { Type{.name = "char", .module = irgen->module->engine->modules.at("core").get()} };
    }

    ExpressionTypeChecker::Result ExpressionTypeChecker::operator()(TupleLiteral* tup)
    {
        //target type can modify the type of tuple literals
        bool consider_target = target && target->is_tuple() && target->subtypes.size() == tup->elements.size();
        Type tp{.name="__tup", .module = irgen->module->engine->modules.at("core").get()};
        for(size_t i = 0; i < tup->elements.size(); ++i)
        {
            auto type_i = std::visit(ExpressionTypeChecker{irgen}, tup->elements[i]->toVariant()).value_or_error();
            //if we can implicit convert to the target type we use that
            if(consider_target && target->subtypes[i].is_assignable_from(type_i, irgen))
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
        return { Type{.name = "bool", .module = irgen->module->engine->modules.at("core").get()} };
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
        if (t.is_str()) return true;
        return false;
    }

    ExpressionTypeChecker::Result ExpressionTypeChecker::operator()(IntegerLiteral*)
    {
        return { Type{.name = "ilit", .module = irgen->module->engine->modules.at("core").get()} };
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
                .module = irgen->module->engine->modules.at("core").get()} };
            }
        case RefMut:
            {
                //if(op_type.is_reference()) return std::nullopt;
                if (!op_type.is_mutable) return { Error(op, "Cannot mutably reference an immutable expression") };
                return { Type{.name = "__ref_mut", .subtypes = {std::move(op_type)},
                    .module = irgen->module->engine->modules.at("core").get()} };
            }
        default: break;
        }

    }

    //takes two types that are (supposed to be) equal but one is generic and returns a list of substitutions
    std::optional<std::unordered_map<std::string, Type>> 
        genericMatch(const Type& generic, const Type& resolved, std::span<std::string> generics, IRGenerator* irgen) 
    {
        if (auto it = std::ranges::find_if(generics, [&generic](const std::string& val)
            {return val == generic.name;}); it != generics.end())
        {
            return {{{ *it, resolved }}};
        }
        std::unordered_map<std::string, Type> results;
        auto it = UnsaturatedTypeIterator(generic);
        if (!it.is_end())
        {
            auto first = std::string(generic.name.begin(), generic.name.begin() + generic.name.find_first_of(':'));
            auto a = irgen->module->hashOf(irgen->block_hash, first);
            if (!a) return std::nullopt;
            auto gblock = Type{ *a };
            auto res_block = Type{ resolved.block_hash };
            UnsaturatedTypeIterator generic_it(gblock);
            UnsaturatedTypeIterator resolved_it(res_block);
            while (!generic_it.is_end())
            {
                auto gtp = generic_it.next();
                auto rtp = resolved_it.next();
                if (gtp != rtp) __debugbreak();
            }
            if (resolved_it.is_end())
            {
                if(!generic_it.is_end()) __debugbreak();
            }
            else
            {
                Type rtp = resolved_it.next();
                Type gtp = generic_it.last();
                if (rtp != gtp) __debugbreak();
            }
            while (!it.is_end())
            {
                Type gtp = it.next();
                if (resolved_it.is_end() && !it.is_end()) __debugbreak();
                Type rtp = resolved_it.is_end() ? resolved_it.last(true) : resolved_it.next();
                if (gtp.name != rtp.name) return std::nullopt;
                if (gtp.subtypes.size() != rtp.subtypes.size()) return std::nullopt;
                for (size_t i = 0; i < gtp.subtypes.size(); i++)
                {
                    rtp.subtypes[i].saturate(irgen->module, irgen);
                    auto match = genericMatch(gtp.subtypes[i], rtp.subtypes[i], generics, irgen);
                    if (!match) return std::nullopt;
                    for (auto& [name, tp] : *match) {
                        if (!results.contains(name)) {
                            results[name] = tp; continue;
                        }
                        if (!results.at(name).is_equal(tp)) return std::nullopt;
                    }
                }
            }
        }
        if (it.is_end())
        {
            auto tp = it.last();
            if (tp.name != resolved.name) return std::nullopt;
            if (tp.subtypes.size() != resolved.subtypes.size()) return std::nullopt;
            for (size_t i = 0; i < tp.subtypes.size(); i++)
            {
                auto match = genericMatch(tp.subtypes[i], resolved.subtypes[i], generics, irgen);
                if (!match) return std::nullopt;
                for (auto& [name, tp] : *match) {
                    if (!results.contains(name)) {
                        results[name] = tp; continue;
                    }
                    if (!results.at(name).is_equal(tp)) return std::nullopt;
                }
            }
        }
        return results;
    }
    struct ConstraintInferenceChecker
    {
        std::unordered_map<std::string, Type>& results;
        std::string type;
        std::span<std::string> generics;
        IRGenerator* irgen;
        bool operator()(const ImplConstraint& impl_con) {
            //If the type hasn't been resolved yet we skip it, this does mean that we may require more passes
            if (!results.contains(type)) return true;
            auto cls = results.at(type).get_decl_if_class(irgen);
            if (!cls) return true;
            for (auto& intf : cls->impls)
            {
                auto match = genericMatch(impl_con.other, intf.impl_for, generics, irgen);
                if (match)
                {
                    for (auto& [name, tp] : *match) {
                        if (!results.contains(name)) {
                            results[name] = tp; continue;
                        }
                        if (!results.at(name).is_equal(tp)) return false;
                    }
                }
            }
            return true;
        }
        bool operator()(const SatisfyConstraint&) {
            Yoyo::debugbreak();
            return true;
        }
    };
    std::optional<std::vector<Type>> deduceTypeArgs(const FunctionType& generic_fn, std::span<Type> input, IRGenerator* irgen)
    {
        if (generic_fn.sig.parameters.size() == 0) return std::nullopt;
        constexpr std::string_view gfn = "__generic_fn";
        std::string name(generic_fn.name.begin() + gfn.size(), generic_fn.name.end());
        auto decl = generic_fn.module->findGenericFn(generic_fn.block_hash, name).second;
        std::unordered_map<std::string, Type> results;
        //we try to infer substitutions from parameter types first
        for (size_t i = 0; i < input.size(); i++)
        {
            auto match = genericMatch(generic_fn.sig.parameters[i].type, input[i], decl->clause.types, irgen);
            if (!match) return std::nullopt;
            for (auto& [name, tp] : *match) {
                if (!results.contains(name)) {
                    results[name] = tp; continue;
                }
                if (!results.at(name).is_equal(tp)) return std::nullopt;
            }
        }
        //after the above not all types may have substitutions, so we try to infer other substitution from constrinats
        //we return if all types have already been inferred
        if (results.size() == decl->clause.types.size())
        {
            std::vector<Type> ret;
            for (auto& param : decl->clause.types)
                ret.emplace_back(std::move(results.at(param)));
            return ret;
        }
        for (auto [type, constraints] : decl->clause.constraints) {
            for (auto& con : constraints) {
                auto success = std::visit(ConstraintInferenceChecker{ results, type, decl->clause.types, irgen }, con);
                if (!success) return std::nullopt;
            }
        }
        if (results.size() != decl->clause.types.size()) return std::nullopt;

        std::vector<Type> ret;
        for (auto& param : decl->clause.types)
            ret.emplace_back(std::move(results.at(param)));
        return ret;
    }

    ExpressionTypeChecker::Result ExpressionTypeChecker::operator()(CallOperation* op)
    {
        if(!op->callee) return { Error(op, "Could not deduce generic args") };
        auto callee_ty = std::visit(*this, op->callee->toVariant()).value_or_error();
        if (callee_ty.is_error_ty()) return { std::move(callee_ty) };
        if (!callee_ty.is_function() && !callee_ty.is_lambda() && !callee_ty.name.starts_with("__generic_fn"))
            return { Error(op->callee.get(), "Attempt to call non-function expression")};
        auto& as_fn = reinterpret_cast<FunctionType&>(callee_ty);
        //If the function is bound (something.function()) we skip checking the first args type
        if (op->arguments.size() + callee_ty.is_bound != as_fn.sig.parameters.size()) {
            Error err(op, "Function argument count mismatch");
            err.markers.emplace_back(SourceSpan{ op->callee->beg, op->callee->end }, "This function was defined as: '" + as_fn.sig.pretty_name(irgen->block_hash) + "'");
            return { err };
        }
        if (callee_ty.name.starts_with("__generic_fn"))
        {
            std::vector<Type> inputs;
            inputs.reserve(op->arguments.size() + callee_ty.is_bound);
            if (callee_ty.is_bound) inputs.emplace_back(callee_ty.subtypes[0]);
            for (auto& arg : op->arguments) inputs.emplace_back(std::visit(*this, arg->toVariant()).value_or_error());
            auto args = deduceTypeArgs(callee_ty, inputs, irgen);
            if (!args)
            {
                op->callee = nullptr; // we null the calle as a marker that we've done this before
                return { Error(op, "Could not deduce generic args") };
            }
            //modify the node if possible to prevent doing this gymnastics over and over
            if (auto as_bin = dynamic_cast<BinaryOperation*>(op->callee.get()))
            {
                if (auto as_name = dynamic_cast<NameExpression*>(as_bin->rhs.get()))
                {
                    auto beg = as_name->beg, end = as_name->end;
                    as_bin->rhs = std::make_unique<GenericNameExpression>(as_name->text, *args);
                    as_bin->rhs->beg = beg; as_bin->rhs->end = end;
                }
            }
            else if (auto as_name = dynamic_cast<NameExpression*>(op->callee.get()))
            {
                auto beg = as_name->beg, end = as_name->end;
                op->callee = std::make_unique<GenericNameExpression>(as_name->text, *args);
                op->callee->beg = beg; op->callee->end = end;
            }
            return (*this)(op);
        }
        for(size_t i = 0; i < op->arguments.size(); ++i)
        {
            auto tp = std::visit(*this, op->arguments[i]->toVariant()).value_or_error();
            if (!as_fn.sig.parameters[i + callee_ty.is_bound].type.can_accept_as_arg(tp, irgen))
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
        return { Type{.name = "flit", .module = irgen->module->engine->modules.at("core").get()} };
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
            .module = irgen->module->engine->modules.at("core").get()} };
    }

    ExpressionTypeChecker::Result ExpressionTypeChecker::operator()(GCNewExpression* expr)
    {
        using namespace std::ranges;
        using namespace std::views;
        auto internal_ty = std::visit(*this, expr->target_expression->toVariant()).value_or_error();
        if (internal_ty.is_error_ty()) return { internal_ty };
        if (internal_ty.is_non_owning(irgen))
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