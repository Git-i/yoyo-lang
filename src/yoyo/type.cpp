#include "type.h"
#include "ir_gen.h"
#include "statement.h"
namespace Yoyo
{
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
        for(size_t i = gen->types.size(); i > 0; i--)
        {
            auto idx = i - 1;
            if(auto t = gen->types[idx].find(name); t != gen->types[idx].end())
            {
                return t->second.second;
            }
        }
        if(auto t = gen->module->classes.find(name); t != gen->module->classes.end()) return t->second.second;
        return nullptr;
    }

    bool Type::is_builtin() const
    {
        return
            name == "__arr"
            || name == "__var"
            || name == "__tup"
            || name == "i8"
            || name == "i16"
            || name == "i32"
            || name == "i64"
            || name == "u8"
            || name == "u16"
            || name == "u32"
            || name == "u64"
            || name == "f32"
            || name == "f64"
            || name == "bool";
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

    bool Type::is_integral() const
    {
        return is_signed_integral() || is_unsigned_integral();
    }

    bool Type::is_floating_point() const
    {
        return name == "f32"
            || name == "f64";
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
}
