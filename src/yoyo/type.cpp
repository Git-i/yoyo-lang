#include "type.h"

#include <csignal>
#include <set>

#include "ir_gen.h"
#include "statement.h"
#include "fn_type.h"
namespace Yoyo
{
    //TODO: move to util header !??
    std::vector<std::string_view> split(std::string_view str, std::string_view delim)
    {
        // ab::lol
        std::vector<std::string_view> result;
        size_t left = 0;
        for (size_t it = 0; it < str.size() - delim.size(); ++it)
        {
            std::string_view curr(str.begin() + it, str.begin() + it + delim.size());
            if (curr == delim)
            {
                result.emplace_back(str.begin() + left, str.begin() + it);
                it += delim.size();
                left = it;
            }
        }
        result.emplace_back(str.begin() + left, str.end());
        return result;
    }

    size_t Type::conversion_friction(const Type& other) const
    {
        constexpr size_t max = std::numeric_limits<size_t>::max();
        if(is_equal(other)) return 0;
        if(!is_assignable_from(other)) return max;
        if(is_unsigned_integral())
        {
            if(other.name == "ilit") return 0;
            return 1;
        }
        if(is_signed_integral())
        {
            if(other.name == "ilit") return 0;
            if(other.is_unsigned_integral()) return 2;
            return 1;
        }
        if(is_floating_point())
        {
            if(other.name == "ilit") return 0;
            if(other.name == "flit") return 0;
            if(other.is_floating_point()) return 1;
            return 2;
        }
        if(name == "__called_fn" && (other.is_function() || other.is_lambda()))
        {
            return 0;
        }
        if(is_optional())
        {
            if(other.name == "__null") return 0;
            return 1 + subtypes[0].conversion_friction(other);
        }
        //for variants, the rhs must be assignable to only one of the subtypes
        if(is_variant())
        {
            for(auto& subtype : subtypes)
            {
                if(subtype.is_assignable_from(other))
                {
                    return 1 + subtype.conversion_friction(other);
                }
            }
        }
        return max;
    }

    bool Type::is_assignable_from(const Type& other) const
    {
        if(is_equal(other)) return true;
        if(is_unsigned_integral())
        {
            if(other.name == "ilit") return true;
            if(!other.is_unsigned_integral()) return false;
            if(*other.integer_width() > *integer_width()) return false;
            return true;
        }
        if(is_signed_integral())
        {
            if(other.name == "ilit") return true;
            if(other.is_unsigned_integral()) return *integer_width() > *other.integer_width();
            if(!other.is_signed_integral()) return false;
            if(*other.integer_width() > *integer_width()) return false;
            return true;
        }
        if(is_floating_point())
        {
            if(other.name == "ilit") return true;
            if(other.name == "flit") return true;
            if(other.is_floating_point()) return *float_width() >= *other.float_width();
            if(!other.is_integral()) return false;
            return *float_width() > *other.integer_width();
        }
        if(name == "__called_fn" && (other.is_function() || other.is_lambda()))
        {
            const auto& as_fn = reinterpret_cast<const FunctionType&>(other);
            if(signature->parameters.size() != as_fn.sig.parameters.size()) return false;
            for(size_t i = 0; i < signature->parameters.size(); ++i)
            {
                if(signature->parameters[i].type != as_fn.sig.parameters[i].type) return false;
            }
            if(signature->return_is_ref != as_fn.sig.return_is_ref) return false;
            if(signature->returnType != as_fn.sig.returnType) return false;
            return true;
        }
        if(is_optional())
        {
            return other.name == "__null" || subtypes[0].is_assignable_from(other);
        }
        //for variants, the rhs must be assignable to only one of the subtypes
        if(is_variant())
        {
            bool is_valid = false;
            for(auto& subtype : subtypes)
            {
                if(subtype.is_assignable_from(other))
                {
                    if(is_valid) return false;
                    is_valid = true;
                }
            }
            return is_valid;
        }

        return false;
    }

    bool Type::can_accept_as_arg(const Type& other) const
    {
        if(is_equal(other)) return true;
        if(is_mutable_reference())
        {
            return deref().is_equal(other) && other.is_mutable;
        }
        if(is_reference())
        {
            return deref().is_equal(other.deref());
        }
        return is_assignable_from(other);
    }

    bool Type::is_equal(const Type& other) const
    {
        if(is_variant() && other.is_variant())
        {
            //variant ordering doesn't matter TODO consider sorting variant subtypes in `saturate`
            return
                std::set(subtypes.begin(), subtypes.end()) == std::set(other.subtypes.begin(), other.subtypes.end());
        }
        return name == other.name && subtypes == other.subtypes && module == other.module;
    }

    bool Type::is_non_owning(IRGenerator* irgen) const
    {
        if(is_reference()) return true;
        if(is_optional() || is_variant() || is_tuple())
        {
            for(auto& subtype : subtypes)
                if(subtype.is_non_owning(irgen)) return true;
        }
        if(auto decl = get_decl_if_class(irgen))
            return decl->ownership == Ownership::NonOwning || decl->ownership == Ownership::NonOwningMut;
        return false;
    }

    bool Type::is_non_owning_mut(IRGenerator* irgen) const
    {
        if(is_mutable_reference()) return true;
        if(is_optional() || is_variant() || is_tuple())
        {
            for(auto& subtype : subtypes)
                if(subtype.is_non_owning_mut(irgen)) return true;
        }
        if(auto decl = get_decl_if_class(irgen))
            return decl->ownership == Ownership::NonOwningMut;
        return false;
    }

    bool Type::is_reference() const
    {
        return name == "__ref" || name == "__ref_mut";
    }

    const Type& Type::deref() const
    {
        if(is_reference()) return subtypes[0];
        return *this;
    }

    Type Type::saturated(Module* src) const
    {
        Type tp = *this;
        tp.saturate(src);
        return tp;
    }

    void Type::saturate(Module* src)
    {
        if(module) return; //avoid double saturation
        auto module_path = split(name, "::");

        if(module_path.size() == 1)
        {
            if(!(is_char() || is_builtin() || is_tuple() || is_str() || name == "__called_fn" || is_optional() || is_variant() || is_reference()))
                module = src;
            else
                module = src->engine->modules.at("__builtin").get();
        }
        if(is_mutable_reference())
            subtypes[0].is_mutable = true;
        if(is_reference())
            subtypes[0].is_lvalue = true;
        if(module_path.size() > 1)
        {
            Module* mod = src->modules.at(std::string(module_path[0]));
            for(size_t i = 1; i < module_path.size() - 1; ++i)
            {
                mod = mod->modules.at(std::string(module_path[i]));
            }
            name = module_path.back();
            module = mod;
        }
        for(auto& sub: subtypes) sub.saturate(src);
    }

    Type Type::make_mut() const
    {
        Type t = *this;
        t.is_lvalue = true; t.is_mutable = true;
        return t;
    }

    Type Type::make_lvalue() const
    {
        Type t = *this;
        t.is_lvalue = true;
        return t;
    }

    Type Type::take_mutability_characteristics(const Type& other) const
    {
        Type t = *this;
        t.is_lvalue = other.is_lvalue; t.is_mutable = other.is_mutable;
        return t;
    }

    Type Type::variant_merge(Type a, Type b)
    {
        if(a.is_equal(b)) return a;
        if(a.name == "__var")
        {
            if(b.name == "__var")
            {
                a.subtypes.insert(a.subtypes.end(), b.subtypes.begin(), b.subtypes.end());
                std::vector<Type> unduplicated;
                for(auto& subtype : a.subtypes)
                {
                    if(std::ranges::find_if(unduplicated, [&subtype](const Type& elem){return elem.is_equal(subtype);}
                    ) == unduplicated.end())
                        unduplicated.emplace_back(subtype);
                }
                a.subtypes.swap(unduplicated);
                return a;
            }
            if(std::ranges::find_if(a.subtypes, [&b](const Type& elem){return elem.is_equal(b);}) == a.subtypes.end())
                a.subtypes.push_back(std::move(b));
            return a;
        }
        if(b.name == "__var")
        {
            return variant_merge(std::move(b), std::move(a));
        }
        return {.name = "__var", .subtypes = {std::move(a), std::move(b)}};
    }

    ClassDeclaration* Type::get_decl_if_class(IRGenerator* gen) const
    {
        if(module && module != gen->module)
        {
            if(module->classes.contains(name))
                return std::get<2>(module->classes.at(name)).get();
            return nullptr;
        }
        for(size_t i = gen->types.size(); i > 0; i--)
        {
            auto idx = i - 1;
            if(auto t = gen->types[idx].find(name); t != gen->types[idx].end())
            {
                return std::get<2>(t->second).get();
            }
        }
        if(auto t = gen->module->classes.find(name); t != gen->module->classes.end()) return std::get<2>(t->second).get();
        return nullptr;
    }

    bool Type::is_builtin() const
    {
        return name == "i8"
            || name == "i16"
            || name == "i32"
            || name == "i64"
            || name == "u8"
            || name == "u16"
            || name == "u32"
            || name == "u64"
            || name == "f32"
            || name == "f64"
            || name == "bool"
            || name == "ilit"
            || name == "flit";
    }

    bool Type::is_signed_integral() const
    {
        return name == "i8"
            || name == "i16"
            || name == "i32"
            || name == "i64";
    }

    bool Type::is_unsigned_integral() const
    {
        return name == "u8"
            || name == "u16"
            || name == "u32"
            || name == "u64";
    }

    bool Type::is_boolean() const
    {
        return name == "bool";
    }

    bool Type::is_enum() const
    {
        if(!module) return false;
        return module->enums.contains(name);
    }
    bool Type::should_sret() const
    {
        return !is_primitive() && !is_enum() && !is_reference() && !is_char();
    }

    bool Type::is_integral() const
    {
        return is_signed_integral() || is_unsigned_integral() || name == "ilit";
    }

    bool Type::is_floating_point() const
    {
        return name == "f32"
            || name == "f64"
            || name == "flit";
    }

    std::optional<uint32_t> Type::integer_width() const
    {
        if(!is_integral()) return std::nullopt;
        std::string size(name.begin() + 1, name.end());
        return std::stoul(size);
    }

    std::optional<uint32_t> Type::float_width() const
    {
        if(!is_floating_point()) return std::nullopt;
        std::string size(name.begin() + 1, name.end());
        return std::stoul(size);
    }

    std::string Type::full_name() const
    {
        return module->module_hash + name;
    }

    size_t Type::bitsize(IRGenerator* irgen) const
    {
        if(auto int_w = integer_width()) return *int_w;
        if(auto float_w = float_width()) return *float_w;
        if(is_void()) return 0;
        if(is_boolean()) return 1;
        if(auto decl = get_decl_if_class(irgen))
        {
            size_t sz = 0;
            for(auto& var : decl->vars)
            {
                sz += var.type.bitsize(irgen);
            }
            return sz;
        }
        return 0;
    }
}
