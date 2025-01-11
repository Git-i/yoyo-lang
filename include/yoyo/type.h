#pragma once
#include <algorithm>
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <vector>


namespace Yoyo
{
    struct Module;
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
        Module* module;
        mutable bool is_mutable = false;
        bool is_lvalue = false;
        std::string block_hash;
        bool operator==(const Type& other) const
        {
            return is_equal(other);
        }
        [[nodiscard]] size_t conversion_friction(const Type& other) const;
        [[nodiscard]] Type strip_lvalue() const {return {.name = name, .signature = signature, .is_mutable = false, .is_lvalue = false};}
        [[nodiscard]] bool is_assignable_from(const Type& other) const;
        [[nodiscard]] bool can_accept_as_arg(const Type& other) const;
        [[nodiscard]] bool is_equal(const Type& other) const;
        [[nodiscard]] bool is_non_owning() const;
        [[nodiscard]] bool is_non_owning_mut() const;
        [[nodiscard]] bool is_reference() const;
        [[nodiscard]] bool is_gc_reference() const;
        [[nodiscard]] bool is_trivially_destructible() const;
        [[nodiscard]] bool can_be_stored() const;
        [[nodiscard]] bool is_conversion_result() const;
        [[nodiscard]] bool is_value_conversion_result() const;
        [[nodiscard]] bool is_ref_conversion_result() const;
        [[nodiscard]] bool is_slice() const;
        [[nodiscard]] bool is_mut_slice() const;
        [[nodiscard]] bool is_error_ty() const;
        [[nodiscard]] bool is_interface_function() const;
        /// returns a dereferenced version of a reference type or itself if not a reference
        [[nodiscard]] const Type& deref() const;
        //reduce a type to a pure string and a Module*(also resolves aliases)
        [[nodiscard]] Type saturated(Module* src, IRGenerator* irgen) const;
        [[nodiscard]] Type reference_to() const;
        [[nodiscard]] Type mutable_reference_to() const;
        void saturate(Module* src, IRGenerator* irgen);

        [[nodiscard]] bool is_shallow_equal(const Type& other) const
        {
            return name == other.name;
        }

        [[nodiscard]] bool is_tuple() const {return name == "__tup";}
        [[nodiscard]] bool is_optional() const {return name == "__opt";}
        [[nodiscard]] bool is_str() const {return name == "str";}
        [[nodiscard]] bool is_variant() const {return name == "__var";}
        [[nodiscard]] bool is_char() const {return name == "char";}
        [[nodiscard]] bool is_mutable_reference() const {return name == "__ref_mut";}
        [[nodiscard]] bool is_static_array() const;
        [[nodiscard]] uint32_t static_array_size() const;
        [[nodiscard]] bool is_dynamic_array() const;
        [[nodiscard]] bool is_array() const;
        [[nodiscard]] Type make_mut() const;
        [[nodiscard]] Type make_lvalue() const;
        [[nodiscard]] Type take_mutability_characteristics(const Type&) const;
        /// Merges the types into one variant type or returns one of them if they're equal
        /// if @c a is already a variant it adds @c b to the list(if @c b is not already there)
        /// if @c b is a variant is does the same
        /// if not it makes @c a a variant with subtypes @c a and @c b
        static Type variant_merge(Type a, Type b);
        ClassDeclaration* get_decl_if_class() const;
        [[nodiscard]] bool is_builtin() const;
        [[nodiscard]] bool is_signed_integral() const;

        [[nodiscard]] bool is_unsigned_integral() const;

        [[nodiscard]] bool is_boolean() const;

        [[nodiscard]] bool is_enum() const;

        [[nodiscard]] bool should_sret() const;

        [[nodiscard]] bool is_integral() const;

        [[nodiscard]] bool is_floating_point() const;

        [[nodiscard]] bool is_void() const {return name == "void"; }

        [[nodiscard]] std::optional<uint32_t> integer_width() const;

        [[nodiscard]] std::optional<uint32_t> float_width() const;

        [[nodiscard]] bool is_function() const {return name == "__fn";}
        [[nodiscard]] bool is_primitive() const {return is_integral() || is_floating_point() ||  is_boolean() || is_void();}
        [[nodiscard]] bool is_opaque_pointer() const {return name == "__ptr";}
        [[nodiscard]] bool is_lambda() const {return name.starts_with("__lambda");}
        [[nodiscard]] std::string full_name() const;
        [[nodiscard]] std::string pretty_name(const std::string& block) const;

        [[nodiscard]] size_t bitsize(IRGenerator* irgen) const;
        //there's no actual sorting, just to fulfill `set` requirements
        bool operator<(const Type& other) const
        {
            if(module == other.module) return name < other.name;
            return module < other.module;

        }

    };

    struct UnsaturatedTypeIterator
    {
        const Type& type;
        explicit UnsaturatedTypeIterator(const Type& type);
        [[nodiscard]] bool is_end() const;
        [[nodiscard]] Type next();
        Type last();
        size_t pos = 0;
        std::vector<std::string_view> split_cache;
    };

}
