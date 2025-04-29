#include "type.h"

#include <csignal>
#include <ranges>
#include <set>

#include "ir_gen.h"
#include "statement.h"
#include "fn_type.h"
#include <cassert>
namespace Yoyo
{
    std::string Type::full_name_no_block() const
    {
        std::string final = name;
        if (!subtypes.empty())
        {
            final += "::<";
            final += subtypes[0].full_name();
            for (auto& sub : std::ranges::subrange(subtypes.begin() + 1, subtypes.end()))
                final += "," + sub.full_name();
            final += ">";
        }
        return final;
    }
    //TODO: move to util header !??
    std::vector<std::string_view> split(std::string_view str, std::string_view delim)
    {
        if(str.size() <= delim.size()) return {str};
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
        result.emplace_back(str.begin() + left, str.ends_with(delim) ? str.end() - delim.size() : str.end());
        return result;
    }

    size_t Type::conversion_friction(const Type& other, IRGenerator* irgen) const
    {
        constexpr size_t max = std::numeric_limits<size_t>::max();
        if(is_equal(other)) return 0;
        if(!is_assignable_from(other, irgen)) return max;
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
            return 1 + subtypes[0].conversion_friction(other, irgen);
        }
        //for variants, the rhs must be assignable to only one of the subtypes
        if(is_variant())
        {
            for(auto& subtype : subtypes)
            {
                if(subtype.is_assignable_from(other, irgen))
                {
                    return 1 + subtype.conversion_friction(other, irgen);
                }
            }
        }
        return max;
    }
    bool Type::is_interface_function() const
    {
        return name.starts_with("__interface_fn");
    }
    bool Type::is_assignable_from(const Type& other, IRGenerator* irgen) const
    {
        if(is_equal(other)) return true;
        if (is_error_ty() || other.is_error_ty()) return true;
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
            const FunctionSignature* as_fn;
            if(other.is_function()); as_fn = &reinterpret_cast<const FunctionType&>(other).sig;
            //if (other.is_lambda()) as_fn = &other.module->lambdas[other.name].second->sig;
            if(signature->parameters.size() != as_fn->parameters.size()) return false;
            for(size_t i = 0; i < signature->parameters.size(); ++i)
            {
                if(signature->parameters[i].type != as_fn->parameters[i].type) return false;
            }
            if(signature->return_is_ref != as_fn->return_is_ref) return false;
            if(signature->returnType != as_fn->returnType) return false;
            return true;
        }
        if(is_optional())
        {
            return other.name == "__null" || subtypes[0].is_assignable_from(other, irgen);
        }
        if (is_gc_reference()) return false;
        if (is_mutable_reference()) other.is_gc_reference() && other.is_mutable && deref().is_equal(other.deref());
        if (is_reference()) return other.is_reference() && deref().is_equal(other.deref());
        //for variants, the rhs must be assignable to only one of the subtypes
        if(is_variant())
        {
            bool is_valid = false;
            for(auto& subtype : subtypes)
            {
                if(subtype.is_assignable_from(other, irgen))
                {
                    if(is_valid) return false;
                    is_valid = true;
                }
            }
            return is_valid;
        }
        if (is_mut_slice())
        {
            if (other.is_mutable_reference())
                if (other.deref().is_array()) return other.deref().subtypes[0].is_equal(subtypes[0]);
            return false;
        }
        if (is_slice())
        {
            if (other.is_reference())
                if (other.deref().is_array()) return other.deref().subtypes[0].is_equal(subtypes[0]);
            if (other.is_mutable_reference())
                return other.subtypes[0].is_equal(subtypes[0]);
            return false;
        }
        if (is_view())
        {
            if (is_mut_view()) {
                if (other.is_gc_view() && subtypes[0].is_equal(other.subtypes[0])) return true;
            } else if (is_gc_view()) {}
            else {
                if (other.is_view() && subtypes[0].is_equal(other.subtypes[0])) return true;
            }
                

            auto& viewed = subtypes[0];
            if (!other.module) return false;
            if (!other.is_reference()) return false;
            if (auto cls = other.deref().get_decl_if_class(irgen))
            {
                auto it = std::ranges::find_if(cls->impls, [&viewed](auto& impl) {
                    return impl.impl_for.is_equal(viewed);
                    });
                if (it != cls->impls.end())
                {
                    if (is_mut_view())
                        if (other.is_mutable_reference() || other.is_gc_reference()) return true;
                    if (is_gc_view())
                        if (other.is_gc_reference()) return true;
                    return true;
                }
            }
        }
        return false;
    }
    bool Type::is_error_ty() const
    {
        return name == "__error_type";
    }
    bool Type::can_accept_as_arg(const Type& other, IRGenerator* irgen) const
    {
        if(is_equal(other)) return true;
        if (is_assignable_from(other, irgen)) return true;
        if(is_mutable_reference())
        {
            return deref().is_equal(other) && other.is_mutable;
        }
        if(is_reference())
        {
            return deref().is_equal(other.deref());
        }
        return false;
    }
    bool Type::is_view() const
    {
        return name.starts_with("__view");
    }
    bool Type::is_mut_view() const
    {
        return name == "__view_mut";
    }
    bool Type::is_gc_view() const
    {
        return name == "__view_gc";
    }
    bool Type::is_equal(const Type& other) const
    {
        if(is_variant() && other.is_variant())
        {
            //variant ordering doesn't matter TODO consider sorting variant subtypes in `saturate`
            return block_hash == other.block_hash &&
                std::set(subtypes.begin(), subtypes.end()) == std::set(other.subtypes.begin(), other.subtypes.end());
        }
        return name == other.name && subtypes == other.subtypes && module == other.module && block_hash == other.block_hash;
    }

    bool Type::is_non_owning(IRGenerator* irgen) const
    {
        if((is_reference() && !is_gc_reference()) || is_slice()) return true;
        if (is_view() && !is_gc_view()) return true;
        if(is_optional() || is_variant() || is_tuple())
        {
            for(auto& subtype : subtypes)
                if(subtype.is_non_owning(irgen)) return true;
        }
        if(name == "__conv_result_ref") return true;
        if(auto decl = get_decl_if_class(irgen))
            return decl->ownership == Ownership::NonOwning || decl->ownership == Ownership::NonOwningMut;
        return false;
    }

    bool Type::is_non_owning_mut(IRGenerator* irgen) const
    {
        if(is_mutable_reference() || is_mut_slice()) return true;
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
        return name == "__ref" || name == "__ref_mut" || name == "__gcref";
    }
    bool Type::is_gc_reference() const
    {
        return name == "__gcref";
    }
    bool Type::is_slice() const
    {
        return name == "__slice" || name == "__slice_mut";
    }
    bool Type::is_mut_slice() const
    {
        return name == "__slice_mut";
    }
    void evaluateDestructability(ClassDeclaration* decl, IRGenerator* irgen)
    {
        if(!decl->destructor_name.empty())
        {
            decl->is_trivially_destructible = false; return;
        }
        for(auto& var : decl->vars)
        {
            if(!var.type.is_trivially_destructible(irgen))
            {
                decl->is_trivially_destructible = false; return;
            }
        }
        decl->is_trivially_destructible = true;
    }
    bool Type::is_trivially_destructible(IRGenerator* irgen, bool consider_no) const
    {
        //if we're not on llvm, lifetime extension are handled differently
        if(consider_no && is_non_owning(irgen)) return false;
        if (is_error_ty()) return true;
        if(is_builtin() || is_opaque_pointer()) return true;
        if (is_void()) return true;
        if(is_tuple() || is_optional() || is_variant())
        {
            bool is_not_trivially_destructible = std::ranges::any_of(subtypes, [irgen](auto& subtype)
            {
                return !subtype.is_trivially_destructible(irgen);
            });
            return !is_not_trivially_destructible;
        }
        if (name.starts_with("__gc_refcell_borrow")) 
            return false;
        if(is_str()) return false;
        if(auto dets = module->findClass(block_hash, name); dets.second)
        {
            auto decl = dets.second->second.get();
            if(decl->is_trivially_destructible) return *decl->is_trivially_destructible;
            //declaration is probably not properly saturated here
            irgen->block_hash.swap(dets.second->first);
            for (auto& var : decl->vars) var.type.saturate(module, irgen);
            irgen->block_hash.swap(dets.second->first);
            evaluateDestructability(decl, irgen);
            return *decl->is_trivially_destructible;
        }
        return true;
    }

    bool Type::can_be_stored() const
    {
        return name != "void" &&
            !name.starts_with("__conv_result");
    }

    bool Type::is_conversion_result() const
    {
        return name.starts_with("__conv_result");
    }

    bool Type::is_value_conversion_result() const
    {
        return name == "__conv_result_val";
    }

    bool Type::is_ref_conversion_result() const
    {
        return name == "__conv_result_ref";
    }


    const Type& Type::deref() const
    {
        if(is_reference()) return subtypes[0];
        return *this;
    }

    Type Type::saturated(ModuleBase* src, IRGenerator* irgen) const
    {
        Type tp = *this;
        tp.saturate(src, irgen);
        return tp;
    }

    Type Type::reference_to() const
    {
        if(is_reference()) return *this;
        auto tp = Type{"__ref", {*this}};
        tp.saturate(module, nullptr);
        return tp;
    }
    Type Type::mutable_reference_to() const
    {
        if(is_reference()) return *this;
        auto tp = Type{"__ref_mut", {*this}};
        tp.saturate(module, nullptr);
        return tp;
    }
    inline static bool from_builtins(const Type& tp) {
        return (tp.is_conversion_result() ||
            tp.is_array() ||
            tp.is_char() ||
            tp.is_void() ||
            tp.is_builtin() ||
            tp.is_tuple() ||
            tp.is_str() ||
            tp.name == "__called_fn" ||
            tp.name == "impl" ||
            tp.is_opaque_pointer() ||
            tp.is_optional() ||
            tp.is_variant() ||
            tp.is_reference() ||
            tp.is_slice() ||
            tp.is_view());
    }
    bool advanceScope(Type& type, ModuleBase*& md, std::string& hash, IRGenerator* irgen, bool);
    void Type::saturate(ModuleBase* src, IRGenerator* irgen, bool do_verify)
    {
        if(module) return; //avoid double saturation
        auto module_path = split(name, "::");
        auto it = UnsaturatedTypeIterator(*this);
        if(it.is_end())
        {
            //if we have no subtyes chances are that they're in the last as a string
            auto last = it.last(subtypes.empty());
            if (subtypes.empty()) subtypes = last.subtypes;
            name = last.name;
            if(!from_builtins(*this))
            {
                module = src;
                block_hash = irgen ? irgen->block_hash : src->module_hash;
                auto hsh = module->hashOf(block_hash, name);
                if(hsh) block_hash = std::move(hsh).value();
            }
            else
                module = src->engine->modules.at("core").get();
        }
        if(is_mutable_reference())
            subtypes[0].is_mutable = true;
        if(is_reference())
            subtypes[0].is_lvalue = true;
        if(!it.is_end())
        {
            ModuleBase* md = src;
            std::string hash = irgen ? irgen->block_hash : src->module_hash;
            bool first = true;
            while(!it.is_end())
            {
                auto type = it.next();
                if (!advanceScope(type, md, hash, irgen, first))
                    debugbreak();
                first = false;
            }
            name = it.last().name;
            module = md;
            block_hash = std::move(hash);
        }
        if (auto alias = module->findAlias(block_hash, name))
        {
            auto old_is_lvalue = is_lvalue;
            auto old_is_mutable = is_mutable;
            *this = *alias;
            this->is_lvalue = old_is_lvalue;
            this->is_mutable = old_is_mutable;
        }
        else if(auto [blk, galias] = module->findGenericAlias(block_hash, name); galias)
        {
            irgen->generateGenericAlias(module, blk, galias, subtypes);
            auto alias = module->findAlias(blk, name + IRGenerator::mangleGenericArgs(subtypes));
            auto old_is_lvalue = is_lvalue;
            auto old_is_mutable = is_mutable;
            *this = *alias;
            this->is_lvalue = old_is_lvalue;
            this->is_mutable = old_is_mutable;
        }
        // since "This" is one word it tends to skip through the right modules
        // so we resaturate with the new block hash
        else if (name == "This")
        {
            if (block_hash.empty()) debugbreak();
            name = block_hash + name;
            block_hash = ""; module = nullptr;
            saturate(src, irgen);
        }
        
        if (do_verify && !verify()) debugbreak();

        for(auto& sub: subtypes) sub.saturate(src, irgen, do_verify);
        if (name == "__arr_s_uneval") {
            auto size_expr = reinterpret_cast<Expression*>(signature.get());
            auto size = std::visit(ConstantEvaluator{ irgen }, size_expr->toVariant());
            if (!std::holds_alternative<uint64_t>(size.internal_repr) && !std::holds_alternative<int64_t>(size.internal_repr)) debugbreak();
            size_t val = std::holds_alternative<uint64_t>(size.internal_repr) ? std::get<uint64_t>(size.internal_repr)
                : std::get<int64_t>(size.internal_repr);
            name = "__arr_s" + std::to_string(val);
            signature = nullptr;
            
        }
        if (signature)
        {
            signature->returnType.saturate(src, irgen, do_verify);
            for (auto& tp : signature->parameters) tp.type.saturate(src, irgen, do_verify);
        }
    }
    bool Type::verify() const {
        if (from_builtins(*this)) return true;
        auto blk = module->hashOf(block_hash, name);
        if (!blk) 
            return false;
        if (block_hash != blk) 
            return false;
        return true;
    }
    bool Type::is_static_array() const
    {
        return name.starts_with("__arr_s");
    }

    uint32_t Type::static_array_size() const
    {
        if(!is_static_array()) return 0;
        // __arr_s is 7 chars
        std::string number(name.begin() + 7, name.end());
        return std::stoi(number);
    }

    bool Type::is_dynamic_array() const
    {
        return name == "__arr_d";
    }

    bool Type::is_array() const
    {
        return is_dynamic_array() || is_static_array();
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

    ClassDeclaration* Type::get_decl_if_class(IRGenerator* irgen) const
    {
        if(module)
        {
            if (auto decl = module->findClass(block_hash, full_name_no_block()); decl.second)
                return decl.second->second.get();
            if (auto [hsh, decl] = module->findGenericClass(block_hash, name); decl)
            {
                if (subtypes.size() != decl->clause.types.size()) { debugbreak(); return nullptr; }
                irgen->generateGenericClass(module, hsh, decl, subtypes);
                if (auto decl = module->findClass(block_hash, full_name_no_block()); decl.second)
                    return decl.second->second.get();
            }
            return nullptr;
        }
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

    EnumDeclaration* Type::get_decl_if_enum() const
    {
        if(!module) return nullptr;
        auto [hsh, enm] = module->findEnum(block_hash, name);
        return enm;
    }
    UnionDeclaration* Type::get_decl_if_union() const
    {
        if (!module) return nullptr;
        auto [hsh, unn] = module->findUnion(block_hash, name);
        return unn;
    }
    bool Type::should_sret() const
    {
        return !is_primitive() && (get_decl_if_enum() == nullptr) && !is_reference() && !is_char() && !is_opaque_pointer();
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
        std::string final = block_hash + name;
        if(!subtypes.empty())
        {
            final += "::<";
            final += subtypes[0].full_name();
            for(auto& sub : std::ranges::subrange(subtypes.begin() + 1, subtypes.end()))
                final +=  "," + sub.full_name();
            final += ">";
        }
        return final;
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
    std::vector<std::string_view> unsaturated_type_split(std::string_view str)
    {
        std::string_view delim = "::";
        if (str.size() <= 2) return { str };
        std::vector<std::string_view> result;
        size_t left = 0;
        uint32_t generic_depth = 0;
        for (size_t it = 0; it < str.size() - delim.size(); ++it)
        {
            std::string_view curr(str.begin() + it, str.begin() + it + delim.size());
            //we dont split in the special case of ::<
            if (curr == delim)
                if (str[it + delim.size()] != '<')
                {
                    if (generic_depth == 0)
                    {
                        result.emplace_back(str.begin() + left, str.begin() + it);
                        it += delim.size();
                        left = it;
                    }
                }
                else generic_depth++;
            if (str[it] == '>') generic_depth--;
        }
        result.emplace_back(str.begin() + left, str.ends_with(delim) ? str.end() - delim.size() : str.end());
        return result;
    }
    UnsaturatedTypeIterator::UnsaturatedTypeIterator(const Type& type) : type(type)
    {
        split_cache = unsaturated_type_split(type.name);
    }

    bool UnsaturatedTypeIterator::is_end() const
    {
        return pos >= split_cache.size() - 1;
    }
    struct MangleScanner
    {
        enum Type
        {
            SubOpen, Name, SubClose, AtAt, Scope
        };
        struct Token
        {
            Type type; std::string_view text;
        };
        std::string_view text;
        size_t pos = 0;
        std::vector<Token> buffer;
        std::optional<Token> next(bool update_pos = true)
        {
            if(!buffer.empty())
            {
                auto back = buffer.back();
                buffer.pop_back();
                return back;
            }
            if(pos == text.size()) return std::nullopt;
            auto next_at_at = text.find_first_of(":>,", pos);
            if(next_at_at == std::string_view::npos) next_at_at = text.size();
            std::string_view from_pos{text.begin() + pos, text.end()};
            if(next_at_at == pos)
            {
                if(from_pos.starts_with("::<"))
                {
                    if(update_pos)pos += 3; return Token{SubOpen, {from_pos.begin(), from_pos.begin() + 2}};
                }
                if(from_pos.starts_with("::"))
                {
                    if(update_pos)pos += 2; return Token{ Scope, {from_pos.begin(), from_pos.begin() + 2} };
                }
                if(from_pos.starts_with(">"))
                {
                    if(update_pos)pos += 1; return Token{SubClose, {from_pos.begin(), from_pos.begin() + 1}};
                }
                if(update_pos)pos += 1; return Token{AtAt, {from_pos.begin(), from_pos.begin() + 2}};
            }
            auto ret_val = std::string_view{from_pos.begin(), from_pos.begin() + (next_at_at - pos)};
            if(update_pos) pos = next_at_at;
            return Token{Name, ret_val};
        }
    };
    std::string pretty_name_suffix(const Type& tp)
    {
        if (tp.is_mutable_reference())
            return "&mut " + pretty_name_suffix(tp.subtypes[0]);
        if (tp.is_gc_reference())
            return "^" + pretty_name_suffix(tp.subtypes[0]);
        if (tp.is_reference())
            return "&" + pretty_name_suffix(tp.subtypes[0]);
        if (tp.is_static_array())
            return "[" + pretty_name_suffix(tp.subtypes[0]) + "; " + std::to_string(tp.static_array_size()) + "]";
        if (tp.is_mut_slice())
            return "[" + pretty_name_suffix(tp.subtypes[0]) + ":&mut]";
        if (tp.is_slice())
            return "[" + pretty_name_suffix(tp.subtypes[0]) + ":&]";
        if (tp.name == "__called_fn")
            return "called " + tp.signature->pretty_name("");
        return tp.name;
    }
    std::string Type::pretty_name(const std::string& block) const
    {
        if (block_hash.empty()) return pretty_name_suffix(*this);
        std::string name_pf;
        auto this_split = split(block_hash, "%");
        auto other_split = split(block, "%");
        size_t start_idx = 0;
        while (true)
        {
            if (start_idx >= this_split.size() || start_idx >= other_split.size()) break;
            if (this_split[start_idx] == other_split[start_idx]) start_idx++;
            else break;
        }
        for (auto& spl : std::ranges::subrange(this_split.begin() + start_idx, this_split.end()))
        {
            for(auto&[md_name, module] : module->engine->modules)
            {
                if (module->module_hash == spl) name_pf.append(md_name + "::");
                continue;
            }
            if (spl.starts_with("__class__")) spl = std::string_view(spl.begin() + 9, spl.end());
            spl = std::string_view(spl.begin(), spl.end() - 3);
            name_pf.append(std::string(spl) + "::");
        }
        return name_pf + pretty_name_suffix(*this);
    }
    std::string FunctionSignature::pretty_name(const std::string& block_hash) const
    {
        std::string final_str = "fn(";
        if (parameters.size() >= 1) final_str.append(parameters[0].type.pretty_name(block_hash));
        for (auto& param : std::ranges::subrange(parameters.begin() + 1, parameters.end()))
        {
            final_str.append(", " + param.type.pretty_name(block_hash));
        }
        final_str.append(")");
        return final_str;
    }
    // for this we assume 100% correctness because its only called in that case
    Type parseType(MangleScanner& scanner)
    {

        //if (scanner.text.starts_with("gwrap_other")) __debugbreak();
        Type output;
        auto token = scanner.next().value();
        assert(token.type == MangleScanner::Name);
        output.name = token.text;
        output.module = nullptr;
        auto next = scanner.next(false);
        if(!next) return output;
        if(next->type == MangleScanner::SubOpen)
        {
            scanner.next();
            output.subtypes.emplace_back(parseType(scanner));
            while(scanner.next(false)->type == MangleScanner::AtAt)
            {
                scanner.next();
                output.subtypes.emplace_back(parseType(scanner));
            }
            assert(scanner.next()->type == MangleScanner::SubClose);
        }
        else scanner.buffer.push_back(next.value());
        while (auto next = scanner.next(false))
        {
            if (next->type != MangleScanner::Scope) break;
            scanner.next();
            auto tp = parseType(scanner);
            tp.name = output.full_name() + "::" + tp.name;
            output = std::move(tp);
        }
        return output;
    }
    Type UnsaturatedTypeIterator::next()
    {
        if(pos >= split_cache.size() - 1) return Type{};
        auto scn = MangleScanner(split_cache[pos]);
        pos++;
        return parseType(scn);
    }
    Type UnsaturatedTypeIterator::last(bool parse)
    {
            if(parse)
            {
                MangleScanner scn(split_cache.back());
                return parseType(scn);
            }
            Type tp = type;
            tp.name = split_cache.back();
            pos++;
            return tp;
    }
}
