#include "ir_gen.h"

#include <csignal>
#include <list>
#include <ranges>
#include <set>
#include <iostream>
#include "tree_cloner.h"
#include "yvm/fwd_decl.h"
namespace Yoyo
{
    void debugbreak()
    {
#if _MSC_VER
        __debugbreak();
#endif
    }
    
    void IRGenerator::saturateSignature(FunctionSignature& sig, ModuleBase* module)
    {
        sig.returnType.saturate(module, this);
        for(auto& param: sig.parameters)
        {
            param.type.saturate(module, this);
        }
    }
    
    bool IRGenerator::isShadowing(const std::string& name) const
    {
        //for(auto& map : variables)
        //    if(map.contains(name)) return true;
        return false;
    }
    bool isValidCloneMethod(const Type& tp, const FunctionSignature& sig)
    {
        if(sig.parameters.size() != 1) return false;
        if(sig.parameters[0].type.name != "__ref") return false;
        if(!sig.parameters[0].type.subtypes[0].is_equal(tp)) return false;
        if(!sig.returnType.is_equal(tp)) return false;
        return true;
    }
    void IRGenerator::annotateClass(ClassDeclaration* decl)
    {
        bool has_no_clone = std::ranges::find_if(decl->attributes, [](Attribute& attr) {
            return attr.name == "no_clone";
        }) != decl->attributes.end();
        
        FunctionDeclaration* clone_ptr = nullptr;
        FunctionDeclaration* destructor = nullptr;
        for(auto& method: decl->stats)
        {
            if (!method) continue;
            if(auto it = std::ranges::find_if(method->attributes, [](Attribute& attr) {
                return attr.name == "clone";
            }); it != method->attributes.end())
            {
                if(clone_ptr) error(Error(method.get(), "Multiple methods marked with #(clone)")); //mutiple clone methods
                if(has_no_clone) error(Error(method.get(), "Clone method specified for class with #(no_clone)"));
                clone_ptr = dynamic_cast<FunctionDeclaration*>(method.get());
            }
            if(auto it = std::ranges::find_if(method->attributes, [](Attribute& attr) {
                return attr.name == "destructor";
            }); it != method->attributes.end())
            {
                if(destructor) error(Error(method.get(), "Multiple destructors specified for class"));
                destructor = dynamic_cast<FunctionDeclaration*>(method.get());
            }
        }
        if(has_no_clone) decl->has_clone = false;
        else decl->has_clone = true;
        if(clone_ptr)
        {
            auto fn_decl = clone_ptr;
            if(!isValidCloneMethod(this_t, fn_decl->signature)) error(Error(fn_decl, "Invalid clone method signature"));
        }
        if (destructor)decl->destructor_name = destructor->name;
    }
    void IRGenerator::checkClass(ClassDeclaration* decl)
    {
        using namespace std::string_view_literals;
        using namespace std::ranges;
        //TODO: check that all decl->interfaces is unique
        for (auto& impl : decl->impls)
        {
            impl.impl_for.saturate(module, this);
            Yoyo::InterfaceDeclaration* decl;
            if (!impl.impl_for.subtypes.empty())
            {
                auto [hash, generic] = impl.impl_for.module->findGenericInterface(impl.impl_for.block_hash, impl.impl_for.name);
                generateGenericInterface(impl.impl_for.module, hash, generic, impl.impl_for.subtypes);
                decl = impl.impl_for.module->findInterface(impl.impl_for.block_hash, impl.impl_for.name + mangleGenericArgs(impl.impl_for.subtypes)).second;
            }
            else decl = impl.impl_for.module->findInterface(impl.impl_for.block_hash, impl.impl_for.name).second;
            if (!decl)
            {
                SourceLocation beg, end;
                beg.line = impl.location.begin.line;
                size_t off = impl.location.begin.column + "impl"sv.size();
                for (auto& line : subrange(view->lines.begin() + beg.line - 1, view->lines.end()))
                {
                    if (auto pos = line.find_first_not_of(" \n\t\r", off); pos != std::string_view::npos)
                    {
                        beg.column = pos + 1; break;
                    }
                    beg.line++; off = 0;
                }
                end.line = beg.line;
                off = beg.column - 1;
                for (auto& line : subrange(view->lines.begin() + beg.line - 1, view->lines.end()))
                {
                    if (auto pos = line.find_first_of('{', off); pos != std::string_view::npos)
                    {
                        off = pos; break;
                    }
                    end.line++; off = 0;
                }
                for (auto& line : subrange(view->lines.begin(), view->lines.begin() + end.line) | views::reverse)
                {
                    if (off == 0) off = line.size();
                    if (auto pos = line.find_last_not_of("{} \n\r\t", off); pos != std::string_view::npos)
                    {
                        end.column = pos + 2; break;
                    }
                    end.line--; off = 0;
                }
                Error err(impl.location.begin, impl.location.end, "Attempt to implement non-existent interface");
                err.markers.emplace_back(SourceSpan{ beg, end }, "Unrecognized interface type");
                error(err);
            }
        }
        annotateClass(decl);
    }
    
    bool implementsInterfaceMethod(const FunctionSignature& cls, const FunctionSignature& interface)
    {
        if (cls.parameters.size() != interface.parameters.size()) return false;
        if (!cls.returnType.is_equal(interface.returnType)) return false;
        for (size_t i = 0; i < cls.parameters.size(); i++)
        {
            auto& cls_param = cls.parameters[i];
            auto& intf_param = interface.parameters[i];
            if (!cls_param.type.is_equal(intf_param.type))
                if (cls_param.name != "this" || intf_param.name != "this") return false;
        }
        return true;
    }
    
    template <std::input_iterator It>
    void IRGenerator::pushScopeWithConstLock(It begin, It end)
    {
        /*pushScope();
        for(const std::string& str: std::ranges::subrange(begin, end))
        {
            for(auto& i : variables | std::views::reverse)
            {
                if(!i.contains(str)) continue;
                Type new_tp = std::get<1>(i.at(str));
                new_tp.is_mutable = false;
                variables.back()[str] = {std::get<0>(i.at(str)), std::move(new_tp), nullptr};
                break;
            }
        }*/
    }
   
    void IRGenerator::error(const Error& e)
    {
        auto str = e.to_string(*view, true);
        std::cout << str << std::endl;
        has_error = true;
        debugbreak();
    }

    std::string IRGenerator::reset_hash()
    {
        auto old = std::move(block_hash);
        block_hash = module->module_hash;
        return old;
    }

    FunctionDeclaration* IRGenerator::GetParentFunction(ASTNode* node)
    {
        auto parent = node->parent;
        while(!dynamic_cast<FunctionDeclaration*>(parent))
        {
            parent = parent->parent;
        }
        return reinterpret_cast<FunctionDeclaration*>(parent);
    }

  
    bool canReturn(Statement* stat)
    {
        if(dynamic_cast<ReturnStatement*>(stat))
            return true;
        if(auto res = dynamic_cast<IfStatement*>(stat))
            return canReturn(res->then_stat.get()) || (res->else_stat && canReturn(res->else_stat.get()));
        if(auto res = dynamic_cast<WhileStatement*>(stat))
            return canReturn(res->body.get());
        if(auto res = dynamic_cast<ForStatement*>(stat))
            return canReturn(res->body.get());
        if(auto res = dynamic_cast<BlockStatement*>(stat))
        {
            for(auto& sub_stat : res->statements)
            {
                if(canReturn(sub_stat.get())) return true;
            }
        }
        return false;
    }

   
    //The resultant return type is the type of the first return statement encountered
    std::optional<Type> IRGenerator::inferReturnType(Statement* stat)
    {
        if(!canReturn(stat)) return Type{.name = "void"};
        if(auto res = dynamic_cast<ReturnStatement*>(stat))
            return std::visit(ExpressionTypeChecker{this}, res->expression->toVariant()).to_optional();
        if(auto res = dynamic_cast<IfStatement*>(stat))
        {
            if(canReturn(res->then_stat.get())) return inferReturnType(res->then_stat.get());
            else return inferReturnType(res->else_stat.get());
        }
        if(auto res = dynamic_cast<WhileStatement*>(stat))
            return inferReturnType(res->body.get());
        if(auto res = dynamic_cast<BlockStatement*>(stat))
        {
            for(auto& sub_stat : res->statements)
            {
                if(canReturn(sub_stat.get())) return inferReturnType(sub_stat.get());
            }
        }
        return std::nullopt;
    }

    void IRGenerator::generateGenericFunction(ModuleBase* mod, const std::string& hash, GenericFunctionDeclaration* fn, std::span<Type> types)
    {
        for (auto& type : types) type.saturate(mod, this);
        std::string name = fn->name + IRGenerator::mangleGenericArgs(types);
        if (auto [_, exists] = mod->findFunction(hash, name); exists) return;
        auto module = this->module;
        this->module = mod;

        for (size_t i = 0; i < types.size(); i++)
            this->module->aliases[hash + name + "::"].emplace(fn->clause.types[i], types[i]);

        auto ptr = StatementTreeCloner::copy_stat_specific(static_cast<FunctionDeclaration*>(fn));
        auto new_decl = reinterpret_cast<FunctionDeclaration*>(ptr.get());
        new_decl->name = name;
        auto old_hash = this->reset_hash();
        this->block_hash = hash + name + "::";
        this->saturateSignature(new_decl->signature, mod);
        this->block_hash = hash;

        std::visit(ForwardDeclaratorPass1{ reinterpret_cast<YVMModule*>(module), ptr, block_hash }, new_decl->toVariant());
        doFunction(new_decl);

        this->block_hash = std::move(old_hash);
        this->module = module;
    }
    void IRGenerator::generateGenericClass(ModuleBase* mod, const std::string& hash, GenericClassDeclaration* decl, std::span<Type> types)
    {
        for (auto& type : types) type.saturate(mod, this);
        generateGenericClass(mod, hash, decl, std::span<const Type>{types});
    }
    void IRGenerator::generateGenericClass(ModuleBase* mod, const std::string& hash, GenericClassDeclaration* decl, std::span<const Type> types)
    {
        std::string name = decl->name + IRGenerator::mangleGenericArgs(types);
        if (auto exists = mod->findClass(hash, name); exists.second) return;
        auto module = this->module;
        this->module = mod;

        for (size_t i = 0; i < types.size(); i++)
            this->module->aliases[hash + name + "::"].emplace(decl->clause.types[i], types[i]);

        auto ptr = StatementTreeCloner::copy_stat_specific(static_cast<ClassDeclaration*>(decl));
        auto new_decl = reinterpret_cast<ClassDeclaration*>(ptr.get());
        new_decl->name = name;
        auto old_hash = this->reset_hash();

        this->current_Statement = &ptr;
        std::visit(ForwardDeclaratorPass1{ reinterpret_cast<YVMModule*>(module), ptr, block_hash }, new_decl->toVariant());
        doClass(new_decl);

        this->block_hash = std::move(old_hash);
        this->module = module;
    }
    void IRGenerator::generateGenericAlias(ModuleBase* mod, const std::string& block, GenericAliasDeclaration* decl,
        std::span<Type> types)
    {
        for (auto& type : types) type.saturate(mod, this);
        std::string name = decl->name + IRGenerator::mangleGenericArgs(types);
        if (auto exists = mod->findAlias(block, name)) return;
        auto old_hash = std::move(this->block_hash);
        this->block_hash = block;
        auto module = this->module;
        this->module = mod;
        for (size_t i = 0; i < types.size(); i++)
            this->module->aliases[block + name + "__"].emplace(decl->clause.types[i], types[i]);
        auto new_statement = StatementTreeCloner::copy_stat_specific(static_cast<AliasDeclaration*>(decl));
        auto alias = reinterpret_cast<AliasDeclaration*>(new_statement.get());
        alias->name = name;
        doAlias(alias);
        this->block_hash = std::move(old_hash);
        this->module = module;
    }
    void IRGenerator::generateGenericInterface(ModuleBase* md, const std::string& block, GenericInterfaceDeclaration* decl, std::span<Type> types)
    {
        for (auto& type : types) type.saturate(md, this);
        std::string name = decl->name + IRGenerator::mangleGenericArgs(types);
        if (auto [_, exists] = md->findInterface(block, name); exists) return;
        auto new_interface = StatementTreeCloner::copy_stat_specific(static_cast<InterfaceDeclaration*>(decl));
        auto itf = reinterpret_cast<InterfaceDeclaration*>(new_interface.get());
        itf->name = name;
        //TODO interface visitors to automatically saturate signatures
        for (size_t i = 0; i < types.size(); i++)
            md->aliases[block + name + "::"].emplace(decl->clause.types[i], types[i]);
        auto old_mod = this->module;
        auto old_hash = this->reset_hash();

        this->module = md;
        this->block_hash = block + name + "::";
        std::visit(ForwardDeclaratorPass1{ reinterpret_cast<YVMModule*>(module), new_interface, block }, itf->toVariant());
        for (auto& fn : itf->methods)
            this->saturateSignature(fn->signature, md);
        this->block_hash.swap(old_hash);
        this->module = old_mod;
    }

    bool advanceScope(Type& type, ModuleBase*& md, std::string& hash, IRGenerator* irgen, bool first);
    std::optional<Error> IRGenerator::apply_using(Type& tp, ModuleBase*& md, std::string& hash)
    {
        ModuleBase* new_module = md;
        std::string new_hash = hash;
        // check if name already exists
        bool exists = false;
        if (md) {
            if (auto hsh = md->hashOf(hash, tp.name)) { 
                new_hash = *hsh + tp.name + "::";
                exists = true;
                new_module = md;
            }
            else if (tp.is_integral() || tp.is_floating_point()) {
                new_module = md->engine->modules.at("core").get();
                new_hash = tp.name + "::";
                exists = true;
            }
            else if (md->engine->modules.contains(tp.name))
            {
                new_module = md->engine->modules.at(tp.name).get();
                new_hash = new_module->module_hash;
                exists = true;
            }
        }
        // maybe we should store this along side the statement pointer
        auto get_details = [&tp, this](UsingStatement* stt) -> std::tuple<ModuleBase*, std::string> {
            std::string& block = std::visit([]<typename T>(T& ct)->std::string& {
                if constexpr (std::is_same_v<T, UsingStatement::UsingSingle>) return ct.block;
                else if constexpr (std::is_same_v<T, UsingStatement::UsingMultiple>) return ct.block;
                else if constexpr (std::is_same_v<T, UsingStatement::UsingAll>) return ct.block;
                else static_assert(false);
                }, stt->content);
                ModuleBase* md = module;
                std::string hash = block_hash;
                Type type{ .name = block + tp.name };

                UnsaturatedTypeIterator iter(type);
                auto inner_type = iter.next();
                // validation is done outside
                advanceScope(inner_type, md, hash, this, true);
                while (!iter.is_end()) {
                    inner_type = iter.next();
                    advanceScope(inner_type, md, hash, this, false);
                }
                return { md, std::move(hash) };
            };
        auto contains = [](UsingStatement* stt, const std::string& name) -> std::optional<std::string> {
            struct OperatorStruct {
                const std::string& name;
                std::string block_pfx;
                std::optional<std::string> operator()(UsingStatement::UsingSingle& ct) {
                    return ct.entity == name ? std::optional(block_pfx + ct.block) : std::nullopt;
                }
                std::optional<std::string> operator()(UsingStatement::UsingMultiple& ct) {
                    auto orig_block = (block_pfx + ct.block);
                    for (auto& entity : ct.entities) {
                        block_pfx = orig_block;
                        if(auto vs = std::visit(*this, entity)) return vs;
                    }
                    return std::nullopt;
                }
                std::optional<std::string> operator()(UsingStatement::UsingAll& ct) {
                    return (block_pfx + ct.block);
                }
            };
            return std::visit(OperatorStruct{name, ""}, stt->content);
            };
        UsingStatement* contributing_stat = nullptr;
        for (auto& use_list : used_types | std::views::reverse) {
            for (auto stat : use_list) {
                auto [this_md, this_hsh] = get_details(stat);
                if (auto hs = contains(stat, tp.name)) {
                    auto actual_hash = this_md->hashOf(*hs, tp.name);
                    if (!actual_hash || *actual_hash != *hs) continue;
                    //ambiguous
                    if (contributing_stat) {
                        Error err(SourceSpan{}, "The name \"" + tp.name + "\" is ambiguous in this context");
                        err.markers.emplace_back(SourceSpan{ contributing_stat->beg, contributing_stat->end }, "Can be accessed from here");
                        err.markers.emplace_back(SourceSpan{ stat->beg, stat->end }, "Can also be accessed from here");
                        return err;
                    }
                    contributing_stat = stat;
                    new_module = this_md;
                    new_hash = *hs;
                }
            }
            // remove this if ambiguity is resolved be how deep the declaration is
            //if (contributing_stat) break;
        }
        if (exists && contributing_stat) {
            Error err(SourceSpan{}, "The name \"" + tp.name + "\" exists in this context but is imported");
            err.markers.emplace_back(SourceSpan{ contributing_stat->beg, contributing_stat->end }, "Import happens here");
            return err;
        }
        else if (exists || contributing_stat) {
            md = new_module;
            hash = new_hash;
            return std::nullopt;
        }
        // this branch can hit on valid code in some cases like aliases
        else return std::nullopt;
    }


    
}
