#pragma once
#include "statement.h"
#include "yvm_module.h"
namespace Yoyo 
{
    //Given a declaration, forward declare it and all the other sustatements
    struct ForwardDeclaratorPass1
    {
        YVMModule* md;
        std::unique_ptr<Statement>& stmt;
        std::string block;
        bool operator()(FunctionDeclaration* decl) const
        {
            md->functions[block].emplace_back(decl->name, decl->signature, decl->attributes);
            std::string new_blk = block + decl->name + "::";
            std::visit(ForwardDeclaratorPass1{ md, decl->body, new_blk }, decl->body->toVariant());
            return true;
        }
        bool operator()(ConstantDeclaration* decl) const
        {
            //constants should never be moved out of or destroyed
            decl->type.is_lvalue = true;
            decl->type.is_mutable = false;
            md->constants[block].emplace_back(decl->type, decl->name, decl);
            return true;
        }
        bool operator()(GenericFunctionDeclaration* decl) const
        {
            md->generic_fns[block].emplace_back(decl);
            return true;
        }
        bool operator()(ClassDeclaration* decl) const
        {
            std::string mangled_name_prefix = block + decl->name + "::";

            for (auto& stt : decl->stats)
            {
                std::visit(ForwardDeclaratorPass1{ md, stt, mangled_name_prefix }, stt->toVariant());
            }
            md->aliases[mangled_name_prefix]["This"] = Type{
                .name = decl->name
            };
            auto old_hash = std::move(md->module_hash);
            md->module_hash = block;
            md->aliases[mangled_name_prefix]["This"].saturate(md, nullptr);
            md->module_hash = std::move(old_hash);
            md->classes[block].emplace_back(
                mangled_name_prefix,
                std::unique_ptr<ClassDeclaration>{decl}
            );
            md->classes_types[decl] = nullptr;

            return true;
        }
        bool operator()(GenericClassDeclaration* decl) const
        {
            md->generic_classes[block].emplace_back(decl);
            return true;
        }
        bool operator()(ModuleImport* imp)
        {
            reinterpret_cast<YVMEngine*>(md->engine)->addModule(imp->module_path, "");
            md->modules[imp->module_name] = md->engine->modules[imp->module_path].get();
            return true;
        }
        bool operator()(EnumDeclaration* decl)
        {
            std::string new_blk = block + decl->identifier + "::";

            for (auto& stt : decl->stats)
            {
                std::visit(ForwardDeclaratorPass1{ md, stt, new_blk }, stt->toVariant());
            }
            md->aliases[new_blk]["This"] = Type{
                .name = decl->identifier,
                .module = md,
                .block_hash = block,
            };
            md->enums[block].emplace_back(decl);
            return true;
        }
        bool operator()(OperatorOverload*) { return true; }
        bool operator()(AliasDeclaration* decl)
        {
            md->aliases[block].emplace(decl->name, decl->type);
            return true;
        }
        bool operator()(GenericAliasDeclaration* decl)
        {
            md->generic_aliases[block].emplace_back(decl);
            return true;
        }
        bool operator()(InterfaceDeclaration* decl)
        {
            std::ignore = stmt.release();
            md->interfaces[block].emplace_back(decl);
            auto mangled_name_prefix = block + decl->name + "::";
            md->aliases[mangled_name_prefix]["This"] = Type{
                .name = "impl",
                .subtypes = { Type{ .name = decl->name } }
            };
            auto old_hash = std::move(md->module_hash);
            md->module_hash = block;
            md->aliases[mangled_name_prefix]["This"].saturate(md, nullptr);
            md->module_hash = std::move(old_hash);
            return true;
        }
        bool operator()(GenericInterfaceDeclaration* decl)
        {
            md->generic_interfaces[block].emplace_back(decl);
            return true;
        }
        bool operator()(UnionDeclaration* decl)
        {
            std::string new_blk = block + decl->name + "::";

            for (auto& stt : decl->sub_stats)
            {
                std::visit(ForwardDeclaratorPass1{ md, stt, new_blk }, stt->toVariant());
            }
            md->aliases[new_blk]["This"] = Type{
                .name = decl->name,
                .module = md,
                .block_hash = block,
            };
            md->unions[block].emplace_back(decl);
            md->union_types[decl] = nullptr;

            return true;
        }
        bool operator()(BlockStatement* stat) {
            for (auto& sub : stat->statements) {
                std::visit(ForwardDeclaratorPass1{ md, sub, block }, sub->toVariant());
            }
            return false;
        }
        bool operator()(MacroDeclaration* decl) {
            md->macros[block].emplace_back(decl);
            return true;
        }
        bool operator()(Statement*) const { return false; };
    };
}