#pragma once
#include <algorithm>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace Yoyo
{
    struct Type
    {
        std::string name;
        std::vector<Type> subtypes;
        bool operator==(const Type& other) const
        {
            return is_equal(other);
        }
        [[nodiscard]] bool is_equal(const Type& other) const
        {
            return name == other.name && subtypes == other.subtypes;
        }
        [[nodiscard]] bool is_shallow_equal(const Type& other) const
        {
            return name == other.name;
        }
        /// Merges the types into one variant type or returns one of them if they're equal
        /// if @c a is already a variant it adds @c b to the list(if @c b is not already there)
        /// if @c b is a variant is does the same
        /// if not it makes @c a a variant with subtypes @c a and @c b
        static Type variant_merge(Type a, Type b)
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
        [[nodiscard]] bool is_builtin() const
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
        [[nodiscard]] bool is_signed_integral() const
        {
            return name == "i8"
            || name == "i16"
            || name == "i32"
            || name == "i64";
        }
        [[nodiscard]] bool is_unsigned_integral() const
        {
            return name == "u8"
            || name == "u16"
            || name == "u32"
            || name == "u64";
        }
        [[nodiscard]] bool is_boolean() const
        {
            return name == "bool";
        }
        [[nodiscard]] bool is_integral() const
        {
            return is_signed_integral() || is_unsigned_integral();
        }
        [[nodiscard]] bool is_floating_point() const
        {
            return name == "f32"
            || name == "f64";
        }
        [[nodiscard]] std::optional<uint32_t> integer_width() const
        {
            if(!is_integral()) return std::nullopt;
            std::string size(name.begin() + 1, name.end());
            return std::stoul(size);
        }
        [[nodiscard]] std::optional<uint32_t> float_wwdth() const
        {
            if(!is_floating_point()) return std::nullopt;
            std::string size(name.begin() + 1, name.end());
            return std::stoul(size);
        }
    };
}
