#pragma once
#include <algorithm>
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <vector>


namespace Yoyo
{
    struct FunctionSignature;
    class ClassDeclaration;
    class IRGenerator;
    struct Type
    {
        std::string name;
        std::vector<Type> subtypes;
        //shared ptr because I want the object to be copyable, its mostly nullptr anyway
        //this is to store the signature for callable and storable functions
        std::shared_ptr<FunctionSignature> signature;
        bool is_lvalue = false;
        bool operator==(const Type& other) const
        {
            return is_equal(other);
        }
        [[nodiscard]] bool is_assignable_from(const Type& other) const;
        [[nodiscard]] bool is_equal(const Type& other) const;

        [[nodiscard]] bool is_shallow_equal(const Type& other) const
        {
            return name == other.name;
        }

        [[nodiscard]] bool is_tuple() const {return name == "__tup";}
        /// Merges the types into one variant type or returns one of them if they're equal
        /// if @c a is already a variant it adds @c b to the list(if @c b is not already there)
        /// if @c b is a variant is does the same
        /// if not it makes @c a a variant with subtypes @c a and @c b
        static Type variant_merge(Type a, Type b);
        ClassDeclaration* get_decl_if_class(IRGenerator*) const;
        [[nodiscard]] bool is_builtin() const;
        [[nodiscard]] bool is_signed_integral() const;

        [[nodiscard]] bool is_unsigned_integral() const;

        [[nodiscard]] bool is_boolean() const;

        [[nodiscard]] bool is_integral() const;

        [[nodiscard]] bool is_floating_point() const;

        [[nodiscard]] bool is_void() const {return name == "void"; }

        [[nodiscard]] std::optional<uint32_t> integer_width() const;

        [[nodiscard]] std::optional<uint32_t> float_width() const;

        [[nodiscard]] bool is_function() const {return name == "__fn";}
        [[nodiscard]] bool is_primitive() const {return is_integral() || is_floating_point() ||  is_boolean() || is_void();}
        [[nodiscard]] bool is_opaque_pointer() const {return name == "__ptr";}
        [[nodiscard]] bool is_lambda() const {return name.starts_with("__lambda");}

        [[nodiscard]] size_t bitsize(IRGenerator* irgen) const;

    };



}
