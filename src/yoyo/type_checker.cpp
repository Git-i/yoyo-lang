#include "ir_gen.h"
#include "overload_details.h"
#include "overload_resolve.h"
#include "type_checker.h"
#include <ranges>
#define core_module irgen->module->engine->modules.at("core").get()
namespace Yoyo
{
    static bool is_type_variable(const Type& tp) {
        return tp.name[0] == '?';
    }
    static Statement* get_type_stat(const Type& type, const std::string& hash, ModuleBase* const md, bool add_non_generics = false) {
        if (auto [hsh, decl] = md->findGenericClass(hash, type.name); decl) {
            return decl;
        }
        else if (auto [this_hash, fn] = md->findGenericFn(hash, type.name); fn) {
            return fn;
        }
        else if (auto [hsh, itf] = md->findGenericInterface(hash, type.name); itf) {
            return itf;
        }
        if (add_non_generics) {
            if (auto [hsh, decl] = md->findClass(hash, type.name); decl) {
                return decl->second.get();
            }
            else if (auto [hsh, decl] = md->findEnum(hash, type.name); decl) {
                return decl;
            }
            else if (auto [hsh, decl] = md->findUnion(hash, type.name); decl) {
                return decl;
            }
            else if (auto [hsh, decl] = md->findInterface(hash, type.name); decl) {
                return decl;
            }
        }
        return nullptr;
    }
    template<typename T>
    std::string get_decl_name(T* arg) {
        if constexpr (std::derived_from<T, ClassDeclaration>) {
            return arg->name;
        }
        else if constexpr (std::derived_from<T, FunctionDeclaration>) {
            return arg->name;
        }
        else if constexpr (std::derived_from<T, UnionDeclaration>) {
            return arg->name;
        }
        else if constexpr (std::same_as<T, EnumDeclaration>) {
            return arg->identifier;
        }
        else if constexpr (std::derived_from<T, InterfaceDeclaration>) {
            return arg->name;
        }
        else if constexpr (std::derived_from<T, AliasDeclaration>) {
            return arg->name;
        }
        else if constexpr (std::derived_from<T, ConstantDeclaration>) {
            return arg->name;
        }
        return "__not_a_declaration__";
    }
    Statement* get_next_stat(Statement* stat, const std::string_view name) {
        if (auto cls = dynamic_cast<ClassDeclaration*>(stat)) { // Class and generic class declarations
            auto it = std::ranges::find_if(cls->stats, [name](auto& elem) {
                return std::visit([](auto* arg) { return get_decl_name(arg); }, elem->toVariant()) == name;
                });
            if (it != cls->stats.end()) {
                return it->get();
            }
        }
        else if (auto fn = dynamic_cast<FunctionDeclaration*>(stat)) {
            return get_next_stat(fn->body.get(), name); // maybe its defined in the function body
        }
        else if (auto enm = dynamic_cast<EnumDeclaration*>(stat)) {
            auto it = std::ranges::find_if(enm->stats, [name](auto& elem) {
                return std::visit([](auto* arg) { return get_decl_name(arg); }, elem->toVariant()) == name;
                });
            if (it != enm->stats.end()) {
                return it->get();
            }
        }
        // interfaces cannot have sub-definitions (i.e cant define struct within interface)
        else if (auto unn = dynamic_cast<UnionDeclaration*>(stat)) {
            auto it = std::ranges::find_if(unn->sub_stats, [name](auto& elem) {
                return std::visit([](auto* arg) { return get_decl_name(arg); }, elem->toVariant()) == name;
                });
            if (it != unn->sub_stats.end()) {
                return it->get();
            }
        }
        // within function body
        //else if (auto blk = dynamic_cast<BlockStatement*>(stat)) {
        //    for (auto& stat : blk->statements) {
        //        auto stat_name = std::visit([](auto* arg) { return get_decl_name(arg); }, stat->toVariant());
        //        if (stat_name == name) {
        //            return stat.get();
        //        }
        //        // we can look deeper as this statement is not a declaration
        //        else if (stat_name == "__not_a_declaration__") {
        //            auto res = get_next_stat(stat.get(), name);
        //            if (res) return res;
        //        }
        //    }
        //}
        else if (auto whl = dynamic_cast<WhileStatement*>(stat))
            return get_next_stat(whl->body.get(), name);
        else if (auto fr = dynamic_cast<ForStatement*>(stat))
            return get_next_stat(fr->body.get(), name);
        //else if (auto if_stat = dynamic_cast<IfStatement*>(stat)) {
        //    auto exists = get_next_stat(if_stat->then_stat.get(), name);
        //    if (exists) return exists;
        //    if (if_stat->else_stat) return get_next_stat(if_stat->else_stat.get(), name);
        //}
        else if (auto cond = dynamic_cast<ConditionalExtraction*>(stat)) {
            auto exists = get_next_stat(cond->body.get(), name);
            if (exists) return exists;
            if (cond->else_body) return get_next_stat(cond->else_body.get(), name);
        }
        else if (auto with = dynamic_cast<WithStatement*>(stat)) {
            return get_next_stat(with->body.get(), name);
        }
        return nullptr;
    }
    std::pair<Statement*, std::unordered_map<std::string, Type>> 
        normalize_type(Type& tp, TypeCheckerState* stt, IRGenerator* irgen, std::unordered_map<std::string, Type>);
    // ignore this huge function
    template<class T>
    concept has_clause = requires(T obj) {
        { obj.clause } -> std::same_as<GenericClause&>;
    };
    template<typename T>
    GenericClause* get_generic_clause(T* arg) {
        if constexpr (has_clause<T>) {
            return &arg->clause;
        }
        else return static_cast<GenericClause*>(nullptr);
    };
    template<class T>
    concept has_impl = requires(T obj) {
        { obj.impls } -> std::same_as<std::vector<InterfaceImplementation>&>;
    };
    template<typename T>
    std::vector<InterfaceImplementation>* get_impls(T* arg) {
        if constexpr (has_impl<T>) return &arg->impls;
        else return nullptr;
    }
    // from type.cpp
    bool from_builtins(const Type& tp);
    bool advanceScope(Type& type, ModuleBase*& md, std::string& hash, IRGenerator* irgen, bool first);
    std::optional<Error> advanceScopeNormalize(
        Statement*& stat, 
        ModuleBase*& md, 
        std::string& hash, 
        Type& type,
        TypeCheckerState* stt,
        IRGenerator* irgen, 
        std::unordered_map<std::string, Type>& generic_instantiations) 
    {
        if (stat) {
            // in generic mode
            
            // instead of searching the module tree, we search the AST directly because
            // the parent has not been monomorphized
            stat = get_next_stat(stat, type.name);
            if (!stat) {
                return Error(SourceSpan{}, "No entity named " + type.name);
            }
            auto clause = std::visit([](auto* arg) { return get_generic_clause(arg); }, stat->toVariant());
            auto num_types_in_clause = clause ? clause->types.size() : 0;
            if (type.subtypes.size() > num_types_in_clause) {
                return Error(SourceSpan{}, "Too many generic types specified");
            }
            std::vector<Type> subtypes;
            for (auto i : std::views::iota(0u, type.subtypes.size())) {
                normalize_type(type.subtypes[i], stt, irgen, {});
                subtypes.push_back(type.subtypes[i]);
                generic_instantiations[clause->types[i]] = type.subtypes[i];
            }
            for (auto i : std::views::iota(type.subtypes.size(), num_types_in_clause)) {
                generic_instantiations[clause->types[i]] = subtypes.emplace_back(stt->new_type_var());
            }
            hash += type.name + IRGenerator::mangleGenericArgs(subtypes) + "::";
            subtypes.clear();
        }
        else {
            if (auto dets = md->findClass(hash, type.name); dets.second)
            {
                if (std::get<0>(*dets.second) != hash + type.name + "::") return Error(SourceSpan{}, "Internal error");
                hash = std::get<0>(*dets.second);
            }
            if (auto [hsh, decl] = md->findGenericClass(hash, type.name); decl)
            {
                stat = decl;
                auto clause = std::visit([](auto* arg) { return get_generic_clause(arg); }, stat->toVariant());
                auto num_types_in_clause = clause ? clause->types.size() : 0;
                if (type.subtypes.size() > num_types_in_clause) {
                    return Error(SourceSpan{}, "Too many generic types specified");
                }
                std::vector<Type> subtypes;
                for (auto i : std::views::iota(0u, type.subtypes.size())) {
                    normalize_type(type.subtypes[i], stt, irgen, {});
                    subtypes.push_back(type.subtypes[i]);
                    generic_instantiations[clause->types[i]] = type.subtypes[i];
                }
                for (auto i : std::views::iota(type.subtypes.size(), num_types_in_clause)) {
                    generic_instantiations[clause->types[i]] = subtypes.emplace_back(stt->new_type_var());
                }
                hash = hsh + type.name + IRGenerator::mangleGenericArgs(subtypes) + "::";
                return std::nullopt;
            }
            if (auto [name, fn] = md->findFunction(hash, type.name); fn)
            {
                hash = name + type.name + "::";
            }
            if (auto [hsh, interface] = md->findInterface(hash, type.name); interface)
            {
                hash = hsh + "%%" + type.name + "%%interface"; //interfaces are terminal and cannot have subtyes
            }
            if (auto alias = md->findAlias(hash, type.name); alias)
            {
                advanceScope(*alias, md, hash, irgen, false);
            }
            if (auto [hsh, interface] = md->findGenericInterface(hash, type.name); interface)
            {
                stat = interface;
                hash = hsh;
                auto clause = std::visit([](auto* arg) { return get_generic_clause(arg); }, stat->toVariant());
                auto num_types_in_clause = clause ? clause->types.size() : 0;
                if (type.subtypes.size() > num_types_in_clause) {
                    return Error(SourceSpan{}, "Too many generic types specified");
                }
                std::vector<Type> subtypes;
                for (auto i : std::views::iota(0u, type.subtypes.size())) {
                    normalize_type(type.subtypes[i], stt, irgen, {});
                    subtypes.push_back(type.subtypes[i]);
                    generic_instantiations[clause->types[i]] = type.subtypes[i];
                }
                for (auto i : std::views::iota(type.subtypes.size(), num_types_in_clause)) {
                    generic_instantiations[clause->types[i]] = subtypes.emplace_back(stt->new_type_var());
                }
                hash = hsh + type.name + IRGenerator::mangleGenericArgs(subtypes) + "::";
                return std::nullopt;
            }
            else if (auto [this_hash, fn] = md->findGenericFn(hash, type.name); fn)
            {
                stat = fn;
                hash = hsh;
                auto clause = std::visit([](auto* arg) { return get_generic_clause(arg); }, stat->toVariant());
                auto num_types_in_clause = clause ? clause->types.size() : 0;
                if (type.subtypes.size() > num_types_in_clause) {
                    return Error(SourceSpan{}, "Too many generic types specified");
                }
                std::vector<Type> subtypes;
                for (auto i : std::views::iota(0u, type.subtypes.size())) {
                    normalize_type(type.subtypes[i], stt, irgen, {});
                    subtypes.push_back(type.subtypes[i]);
                    generic_instantiations[clause->types[i]] = type.subtypes[i];
                }
                for (auto i : std::views::iota(type.subtypes.size(), num_types_in_clause)) {
                    generic_instantiations[clause->types[i]] = subtypes.emplace_back(stt->new_type_var());
                }
                hash = hsh + type.name + IRGenerator::mangleGenericArgs(subtypes) + "::";
                return std::nullopt;
            }
            return std::nullopt;
        }
    }
    /// Similar to saturate but can switch to the AST for incomplete generic types
    /// also substitutes generics given an already existing context
    /// also returns the statement that the type belongs to if we followed that path
    auto normalize_type(
        Type& tp,
        TypeCheckerState* stt,
        IRGenerator* irgen,
        std::unordered_map<std::string, Type> generic_instantiations
    ) -> std::pair<Statement*, std::unordered_map<std::string, Type>>
    {
        if (is_type_variable(tp)) {
            return { nullptr, generic_instantiations };
        }
        if (tp.name == "_") {
            tp = stt->new_type_var();
            return { nullptr, generic_instantiations };
        }
        if (generic_instantiations.contains(tp.name)) {
            tp = generic_instantiations[tp.name];
            return { nullptr, generic_instantiations };
        }
        ModuleBase* md = irgen->module;
        std::string hash = irgen->block_hash;
        std::string second_to_last = "";
        auto iterated_type = tp;
        iterated_type.name = iterated_type.full_name_no_generics();
        auto iterator = UnsaturatedTypeIterator(iterated_type);
        Statement* current_stat = nullptr;
        if (iterator.is_end()) {
            //if we have no subtyes chances are that they're in the last as a string
            auto last = iterator.last(tp.subtypes.empty());
            if (tp.subtypes.empty()) tp.subtypes = last.subtypes;
            tp.name = last.name;
            if (!from_builtins(tp))
            {
                tp.module = irgen->module;
                tp.block_hash = irgen->block_hash;
                auto err = irgen->apply_using(tp, tp.module, tp.block_hash);
                if (err) debugbreak();
                if (auto as_alias = tp.module->findAlias(tp.block_hash, tp.name)) {
                    tp = *as_alias;
                };
                // replace if its an 
                auto hsh = tp.module->hashOf(tp.block_hash, tp.name);
                if (hsh) tp.block_hash = std::move(hsh).value();

                // introduce generic substitutions
                current_stat = get_type_stat(tp, tp.block_hash, tp.module);
                if (current_stat) {
                    auto clause = std::visit([](auto* a) { return get_generic_clause(a); }, current_stat->toVariant());
                    if (clause) {
                        for (auto i : std::views::iota(0u, tp.subtypes.size())) {
                            normalize_type(tp.subtypes[i], stt, irgen, generic_instantiations);
                            generic_instantiations[clause->types[i]] = tp.subtypes[i];
                        }
                        for (auto i : std::views::iota(tp.subtypes.size(), clause->types.size())) {
                            auto var = stt->new_type_var();
                            generic_instantiations[clause->types[i]] = tp.subtypes.emplace_back(var);
                        }
                    }
                }
            }
            else
                tp.module = core_module;
        }
        else {
            auto type = iterator.next();
            auto err = irgen->apply_using(type, md, hash);
            if (err) {
                irgen->error(*err);
            }
            // add generics to the resolved name
            Statement* current_stat = get_type_stat(type, hash, md);
            auto clause = current_stat ? std::visit([](auto* arg) { return get_generic_clause(arg); }, current_stat->toVariant()) : nullptr;
            // add all generic types
            std::vector<Type> subtypes;
            auto num_subtypes = clause ? clause->types.size() : 0;
            if (type.subtypes.size() > num_subtypes) {
                debugbreak();
                irgen->error(Error(nullptr, "Provided more types than schema allows"));
            }
            for (auto i : std::views::iota(0u, type.subtypes.size())) {
                normalize_type(type.subtypes[i], stt, irgen, generic_instantiations);
                subtypes.push_back(type.subtypes[i]);
                generic_instantiations[clause->types[i]] = type.subtypes[i];
            }
            for (auto i : std::views::iota(type.subtypes.size(), num_subtypes)) {
                auto var = stt->new_type_var();
                generic_instantiations[clause->types[i]] = subtypes.emplace_back(var);
                //for (auto& con : clause->constraints[clause->types[i]]) {
                //    auto& as_impl = std::get<ImplConstraint>(con);
                //    auto tp = as_impl.other;
                //    normalize_type(tp, state, irgen, {});
                //    state->add_constraint(ImplInterfaceConstraint{ var, tp, scp });
                //}
            }
            hash.erase(hash.size() - 2, 2);
            hash += IRGenerator::mangleGenericArgs(subtypes) + "::";
            
            while (!iterator.is_end())
            {
                type = iterator.next();
                if (auto err = advanceScopeNormalize(current_stat, md, hash, type, stt, irgen, generic_instantiations))
                {
                    debugbreak();
                }
            }
            tp.name = iterator.last().name;
            tp.block_hash = hash;
            tp.module = md;
            // Current stat is empty but we were successful
            current_stat = current_stat ? current_stat : get_type_stat(tp, tp.block_hash, tp.module, true);
            if (!current_stat) {
                debugbreak();
            }
            // update instantiations for last type
            clause = std::visit([](auto* arg) { return get_generic_clause(arg);  }, current_stat->toVariant());
            num_subtypes = clause ? clause->types.size() : 0;
            if (tp.subtypes.size() > num_subtypes) {
                debugbreak();
                irgen->error(Error(nullptr, "Provided more types than schema allows"));
            }
            for (auto i : std::views::iota(0u, tp.subtypes.size())) {
                normalize_type(tp.subtypes[i], stt, irgen, generic_instantiations);
                generic_instantiations[clause->types[i]] = tp.subtypes[i];
            }
            for (auto i : std::views::iota(tp.subtypes.size(), num_subtypes)) {
                generic_instantiations[clause->types[i]] = stt->new_type_var();
            }
        }
        for (auto& sub : tp.subtypes) {
            if (sub.module && !sub.block_hash.empty()) continue;
            normalize_type(sub, stt, irgen, generic_instantiations);
        }
        current_stat = current_stat ? current_stat : get_type_stat(tp, tp.block_hash, tp.module, true);
        if (tp.name == "__arr_s_uneval") {
            auto size_expr = reinterpret_cast<Expression*>(tp.signature.get());
            auto size = std::visit(ConstantEvaluator{ irgen }, size_expr->toVariant());
            if (!std::holds_alternative<uint64_t>(size.internal_repr) && !std::holds_alternative<int64_t>(size.internal_repr)) debugbreak();
            size_t val = std::holds_alternative<uint64_t>(size.internal_repr) ? std::get<uint64_t>(size.internal_repr)
                : std::get<int64_t>(size.internal_repr);
            tp.name = "__arr_s" + std::to_string(val);
            tp.signature = nullptr;

        }
        return { current_stat, generic_instantiations };
    }
    enum class NoOperatorReason {
        MulipleMatches,
        NoMatches
    };
    std::optional<Error> generic_match(Type tp1, Type tp2, ASTNode* loc, TypeCheckerState* solver);
    // The return value is a pair if the match was found and if not its either that
    // there's multiple matches or no match at all (indicated by NoOperatorReason)
    std::variant<
        std::pair<Type, std::vector<TypeCheckerConstraint>>,
        NoOperatorReason
    > unary_operator_result(
        TokenType op, 
        const Type& subject, 
        // this not necessary but it can help prevent redundant work
        std::unordered_map<OperatorOverload*, std::unordered_map<std::string, Type>>& substitution_cache,
        TypeCheckerState* state, IRGenerator* irgen
    ) {
        // unary operators can be defined at most once per type
        // and it must be done so in the type's module
        std::vector<TypeCheckerConstraint> collector;
        std::vector<OverloadDetailsUnary*> matches;
        Type result_type;
        // we can safely deref here because references don't have any unary operators defined on them by default
        // and some operators allow &T while being defined in T's module
        for (auto& [blk, ovl] : subject.deref().module->overloads.unary_datails_for(op)) {
            Type ovl_subject = ovl.obj_type;
            Type ovl_result = ovl.result;
            std::unordered_map<std::string, Type> generic_subs;
            if (ovl.statement && !ovl.statement->clause.types.empty()) {
                if (!substitution_cache.contains(ovl.statement))
                    for (auto& tp : ovl.statement->clause.types)
                        substitution_cache[ovl.statement][tp] = state->new_type_var();
                generic_subs = substitution_cache[ovl.statement];
            }
            irgen->block_hash.swap(blk);
            normalize_type(ovl_subject, state, irgen, generic_subs);
            normalize_type(ovl_result, state, irgen, generic_subs);
            irgen->block_hash.swap(blk);

            auto collector_ptr = &collector;
            std::swap(collector_ptr, state->write_new_constraints_to);
            // if we see more than one we should break out
            // we .deref() because the operator is defined &T
            if (generic_match(subject, ovl_subject, nullptr, state) == std::nullopt) {
                matches.push_back(&ovl);
                result_type = std::move(ovl_result);
            }
            std::swap(collector_ptr, state->write_new_constraints_to);
        }
        // exactly one match means we add it
        if (matches.size() == 1) {
            return std::make_pair(std::move(result_type), std::move(collector));
        }
        else if (matches.size() > 1) {
            return NoOperatorReason::MulipleMatches;
        }
        else if (matches.size() == 0) {
            return NoOperatorReason::NoMatches;
        }
    }
    Type mutable_reference_to (const Type& tp, IRGenerator* irgen) {
        return Type{ .name = "__ref_mut", .subtypes = {tp}, .module = core_module };
    };
    Type reference_to(const Type& tp, IRGenerator* irgen) {
        return Type{ .name = "__ref", .subtypes = {tp}, .module = core_module };
    };
    // This is like half the type checker, the explanation is in the definition
    
    void TypeChecker::operator()(VariableDeclaration* decl)
    {
        Type tp;
        if (decl->type) {
            normalize_type(*decl->type, state, irgen, {});
            tp = *decl->type;
        }
        if (decl->initializer) {
            auto init = std::visit(targetless(), decl->initializer->toVariant());
            if (decl->type)
                state->add_constraint(EqualConstraint{ tp, init });
            else
                tp = init;
        }
        tp.is_mutable = decl->is_mut;
        decl->type = tp;
        //state->add_constraint(OwningConstraint{tp, decl});
        state->create_variable(std::string(decl->identifier.text), tp);
    }
    FunctionType TypeChecker::operator()(IfExpression* stat) const
	{
        state->add_constraint(EqualConstraint{ 
            std::visit(targetless(), stat->condition->toVariant()), 
            Type{.name = "bool", .module = core_module} 
        });
        auto if_type = std::visit(*this, stat->then_expr->toVariant());
        if (stat->else_expr) {
            auto else_type = std::visit(*this, stat->else_expr->toVariant());
            stat->evaluated_type = state->new_type_var();
            state->add_constraint(IfStatementConstraint{
                if_type,
                else_type,
                stat->then_transfers_control,
                stat->else_transfers_control,
                stat->evaluated_type,
                stat
                });
        }
        else stat->evaluated_type = Type{ .name = "void", .module = core_module };
        return stat->evaluated_type;
	}
    void TypeChecker::operator()(WhileStatement* stat)
    {
        state->add_constraint(EqualConstraint{
            std::visit(targetless(), stat->condition->toVariant()),
            Type{.name = "bool", .module = core_module}
            });

        std::visit(*this, stat->body->toVariant());
    }
    void TypeChecker::operator()(ForStatement* stat)
    {
        auto iter = std::visit(targetless(), stat->iterable->toVariant());
        auto item = state->new_type_var();

        state->add_constraint(ImplInterfaceConstraint{
                iter,
                Type{.name = "Iterator",  .subtypes = {item}, .module = core_module, .block_hash = "core::",},
                stat->iterable.get()
            });

        // new variable block
        state->push_variable_block();
        state->create_variable(std::string(stat->names[0].text), std::move(item));
        std::visit(*this, stat->body->toVariant());
        state->pop_variable_block();
    }
    FunctionType TypeChecker::operator()(BlockExpression* stat) const
    {
        state->push_variable_block();
        for (auto& stt : stat->statements) {
            std::visit(targetless(), stt->toVariant());
        }
        if (stat->expr) stat->evaluated_type = std::visit(targetless(), stat->expr->toVariant());
        else stat->evaluated_type = Type{ .name = "void", .module = core_module };
        state->pop_variable_block();
        return stat->evaluated_type;
    }
    FunctionType TypeChecker::operator()(TryExpression* exp) const
    {
        auto ret_success = state->new_type_var();
        auto ret_fail = state->new_type_var();

        exp->evaluated_type = state->new_type_var();
        state->add_constraint(EqualConstraint{
                Type{.name = "__res", .subtypes = {exp->evaluated_type, ret_fail }, .module = core_module},
                std::visit(targetless(), exp->expression->toVariant())
            });
        state->add_constraint(EqualConstraint{
                Type{.name = "__res", .subtypes = {ret_success, ret_fail}, .module = core_module},
                state->return_type
            });
        return exp->evaluated_type;
    }
    void TypeChecker::operator()(ReturnStatement* stat)
    {
        // if its not a void function there must be a return expression
        if (!state->return_type.is_void() && !stat->expression) {
            irgen->error(Error(stat, "Expected expression in return statement of non-void function"));
            return;
        }
        if (stat->expression) {
            // `target` should handle it but add equality constraint for safety
            state->add_constraint(EqualConstraint{
                std::visit(new_target(state->return_type), stat->expression->toVariant()),
                state->return_type
                });
        }
    }
    void TypeChecker::operator()(ExpressionStatement* stat)
    {
        std::visit(targetless(), stat->expression->toVariant());
    }
    void TypeChecker::operator()(ConditionalExtraction* stat)
    {
        auto tp = std::visit(targetless(), stat->condition->toVariant());
        auto obj_ty = state->new_type_var();
        Type else_ty;
        if (stat->is_ref) {
            // value_conversion_result::<T> extracts to T
            // optional extracts to T, ref extracts to &T or &mut T
            // ref_conversion_result::<T> extracts to T, ref extracts to &T or &mut T
            state->add_constraint(RefExtractsToConstraint{
                    tp,
                    obj_ty
                });
        }
        else {
            state->add_constraint(ExtractsToConstraint{ tp, obj_ty });
        }
        if (!stat->else_capture.empty()) {
            else_ty = state->new_type_var();
            if (stat->else_is_ref)
                state->add_constraint(ElseRefExtractsToConstraint{ tp, else_ty });
            else
                state->add_constraint(ElseExtractsToConstraint{ tp, else_ty });
        }
        state->push_variable_block();
        state->create_variable(stat->captured_name, obj_ty);
        std::visit(*this, stat->body->toVariant());
        state->pop_variable_block();

        if (stat->else_body) {
            if (!stat->else_capture.empty()) {
                state->push_variable_block();
                state->create_variable(stat->else_capture, else_ty);
            }
            std::visit(*this, stat->else_body->toVariant());
            if (!stat->else_capture.empty()) state->pop_variable_block();
        }
    }
    void TypeChecker::operator()(WithStatement* stat)
    {
        auto tp = std::visit(targetless(), stat->expression->toVariant());
        state->add_constraint(NonOwningConstraint{ tp, stat->expression.get() });
        state->push_variable_block();
        state->create_variable(stat->name, tp);
        std::visit(*this, stat->body->toVariant());
        state->pop_variable_block();
    }
    FunctionType TypeChecker::operator()(IntegerLiteral* lit) const {
        if (target)
            if (target->is_integral()) return { *target };
            else {
                irgen->error(Error(lit, "Cannot convert integer literal to " + target->full_name())); 
                return Type{ "__error_type" };
            }
        
        lit->evaluated_type = state->new_type_var();
        state->add_constraint(IsIntegerConstraint{lit->evaluated_type});
        state->add_constraint(CanStoreIntegerConstraint{lit->evaluated_type, std::stoull(lit->text)});
        return lit->evaluated_type;
    }
    FunctionType TypeChecker::operator()(BooleanLiteral* lit) const {
        lit->evaluated_type = Type{ .name = "bool", .module = core_module };
        return lit->evaluated_type;
    }
    FunctionType TypeChecker::operator()(TupleLiteral* lit) const {
        if (target) {
            if (!target->is_tuple()) {
                irgen->error(Error(lit, "Cannot convert tuple literal to " + target->full_name()));
                return Type{ "__error_type" };
            }
            if (target->subtypes.size() != lit->elements.size()) {
                irgen->error(Error(lit, "Tuple element count mismatch"));
                return Type{ "__error_type" };
            }
        }
        lit->evaluated_type = Type{ .name = "__tup", .module = irgen->module->engine->modules.at("core").get() };
        for (auto i : std::views::iota(0u, lit->elements.size())) {
            auto elem_ty = 
                std::visit(target ? new_target(target->subtypes[i]) : *this, lit->elements[i]->toVariant());
            lit->evaluated_type.subtypes.push_back(elem_ty);
        }

        return lit->evaluated_type;
    }
    FunctionType TypeChecker::operator()(ArrayLiteral* lit) const { 
        auto sub_type = state->new_type_var();
        uint32_t array_size = 0;
        // [<expr>, <expr> ...]
        if (auto exprs = std::get_if<std::vector<std::unique_ptr<Expression>>>(&lit->elements)) {
            array_size = exprs->size();
            for (auto& expr : *exprs) {
                expr->evaluated_type = sub_type;
                state->add_constraint(EqualConstraint{
                        sub_type,
                        std::visit(targetless(), expr->toVariant())
                    });
            }
        }
        // [<expr>; <size>]
        else {
            auto& [expr, size] = std::get<std::pair<std::unique_ptr<Expression>, std::unique_ptr<Expression>>>(lit->elements);
            auto size_as_const = std::visit(ConstantEvaluator{ irgen }, size->toVariant());
            if (auto as_uint = std::get_if<uint64_t>(&size_as_const.internal_repr)) {
                if (*as_uint > std::numeric_limits<uint32_t>::max())
                    irgen->error(Error(lit, "Array size too large", std::format("Evaluated size is {}", *as_uint)));
                else if (*as_uint == 0)
                    irgen->error(Error(lit, "Array size must be a positive integer", std::format("Evaluated size is {}", *as_uint)));
                array_size = static_cast<uint32_t>(*as_uint);
            }
            else if (auto as_int = std::get_if<int64_t>(&size_as_const.internal_repr)) {
                if (*as_int <= 0)
                    irgen->error(Error(lit, "Array size must be a positive integer", std::format("Evaluated size is {}", *as_int)));
                else if (*as_int > std::numeric_limits<uint32_t>::max())
                    irgen->error(Error(lit, "Array size too large", std::format("Evaluated size is {}", *as_int)));
                array_size = static_cast<uint32_t>(*as_int);
            }
            expr->evaluated_type = sub_type;
            state->add_constraint(EqualConstraint{
                    sub_type,
                    std::visit(targetless(), expr->toVariant())
                });
            // TODO expr type must implement clone too
        }
        lit->evaluated_type = Type{ .name = "__arr_s" + std::to_string(array_size), .subtypes = {sub_type}, .module = core_module};
        return lit->evaluated_type;
    }
    FunctionType TypeChecker::operator()(RealLiteral* lit) const {
        if (target)
            if (target->is_floating_point()) return { *target };
            else {
                irgen->error(Error(lit, "Cannot convert real literal to " + target->full_name()));
                return Type{ "__error_type" };
            }

        lit->evaluated_type = state->new_type_var();
        state->add_constraint(IsFloatConstraint{ lit->evaluated_type });
        state->add_constraint(CanStoreRealConstraint{ lit->evaluated_type, std::stod(std::string(lit->token.text)) });
        return lit->evaluated_type;
    }
    FunctionType TypeChecker::operator()(StringLiteral* str) const {
        if (target && !target->is_str()) {
            irgen->error(Error(str, "Cannot convert string literal to " + target->full_name()));
        }
        str->evaluated_type = Type{ .name = "str",
            .module = irgen->module->engine->modules.at("core").get() };
        // check all the string interpolation parameters
        for (auto& entry : str->literal) {
            if (std::holds_alternative<std::unique_ptr<Expression>>(entry)) {
                auto& interp = std::get<std::unique_ptr<Expression>>(entry);
                state->add_constraint(ToStringConstraint{ std::visit(targetless(), interp->toVariant())});
            }
        }
        return str->evaluated_type;
    }
    FunctionType TypeChecker::operator()(NameExpression* ex) const {
        for (auto& block : state->variables | std::views::reverse) {
            if (block.contains(ex->text)) {
                ex->evaluated_type = block.at(ex->text);
                return ex->evaluated_type;
            }
        }
        // if not a variable it may be a function
        ModuleBase* module = irgen->module;
        std::string hash = irgen->block_hash;
        Type tp{ .name = ex->text };
        if (auto err = irgen->apply_using(tp, module, hash)) {
            err->span = { ex->beg, ex->end };
            irgen->error(*err);
            return Type{ "__error_type" };
        }
        if (auto [name_prefix, fn] = module->findFunction(hash, ex->text); fn)
        {
            irgen->saturateSignature(fn->sig, irgen->module);
            tp.name = "__fn";
            tp.block_hash = name_prefix + ex->text + "::";
            tp.module = module;
            std::ranges::copy(fn->sig.parameters | std::views::transform([](auto& e) { return e.type; }), std::back_inserter(tp.subtypes));
            tp.subtypes.push_back(fn->sig.returnType);
            ex->evaluated_type = tp;
            return tp;
        }
        if (auto [name_prefix, fn] = module->findGenericFn(hash, ex->text); fn)
        {
            auto clause = &fn->clause;
            tp.name = "__fn";
            tp.module = module;
            tp.block_hash = name_prefix;
            std::vector<Type> subtypes;
            std::unordered_map<std::string, Type> generic_instantiations;
            auto num_subtypes = clause ? clause->types.size() : 0;
            // register all generics as type variables
            for (auto i : std::views::iota(0u, num_subtypes)) {
                generic_instantiations[clause->types[i]] = subtypes.emplace_back(state->new_type_var());
            }
            // separate loop to apply generic constraints, because the 
            // interface might reference other type parameters e.g ::<O, T: Iterator::<O>>
            for (auto i : std::views::iota(0u, num_subtypes)) { // for each type
                for (auto& con : clause->constraints[clause->types[i]]) { // for each constraint on type
                    auto& as_impl = std::get<ImplConstraint>(con);
                    auto tp = as_impl.other;
                    normalize_type(tp, state, irgen, generic_instantiations);
                    state->add_constraint(ImplInterfaceConstraint{ 
                        generic_instantiations[clause->types[i]], 
                        tp, 
                        ex}
                    );
                }
            }
            tp.block_hash += fn->name + IRGenerator::mangleGenericArgs(subtypes) + "::";
            std::ranges::move(fn->signature.parameters | std::views::transform([&generic_instantiations, this](auto& e) {
                Type tp = e.type;
                normalize_type(tp, state, irgen, generic_instantiations);
                return tp;
                }), std::back_inserter(tp.subtypes));
            tp.subtypes.push_back(fn->signature.returnType);
            normalize_type(tp.subtypes.back(), state, irgen, generic_instantiations);
            ex->evaluated_type = tp;
            return tp;
        }
        if (auto [name_pf, c] = module->findConst(hash, ex->text); c)
        {
            return { std::get<0>(*c) };
        }
        return Type{ "__error_type" };
    }
    FunctionType TypeChecker::operator()(GenericNameExpression* ex) const {
        irgen->error(Error(ex, "Not implemented"));
        return Type{ "__error_type" };
    }
    FunctionType TypeChecker::operator()(PrefixOperation* op) const {
        // I could add an equality constraint with the result
        switch (op->op.type)
        {
            using enum TokenType;
        case Minus: {
            // -<expr>
            op->evaluated_type = state->new_type_var();
            state->add_constraint(HasUnaryMinusConstraint{ std::visit(targetless(), op->operand->toVariant()), op->evaluated_type });
            break;
        }
        case Bang: {
            // !<expr>
            op->evaluated_type = state->new_type_var();
            state->add_constraint(HasUnaryNotConstraint{ std::visit(targetless(), op->operand->toVariant()), op->evaluated_type });
            break;
        }
        case Star:
        {
            op->evaluated_type = state->new_type_var();
            state->add_constraint(IsReferenceToConstraint{ std::visit(targetless(), op->operand->toVariant()), op->evaluated_type });
            break;
        }
        case Ampersand:
        {
            // `&<expr>` is defined for every type except for references
            auto inner = std::visit(targetless(), op->operand->toVariant());
            op->evaluated_type = state->new_type_var();
            state->add_constraint(BorrowResultConstraint{ std::move(inner), op->evaluated_type });
            break;
        }
        case RefMut:
        {
            auto inner = std::visit(targetless(), op->operand->toVariant());
            op->evaluated_type = state->new_type_var();
            state->add_constraint(BorrowResultMutConstraint{ std::move(inner), op->evaluated_type });
            break;
        }
        default: break; // unreachable
        }

        if (target) {
            state->add_constraint(EqualConstraint{ op->evaluated_type, *target });
            //op->type = *target; // the node is going to be converted
        }
        return op->evaluated_type;
    }
    FunctionType TypeChecker::operator()(BinaryOperation* expr) const {
        using enum TokenType;
        auto do_overloadable_explicit_token = [expr, this](TokenType tk) { 
            auto left = std::visit(targetless(), expr->lhs->toVariant());
            auto right = std::visit(targetless(), expr->rhs->toVariant());
            expr->evaluated_type = state->new_type_var();
            state->add_constraint(BinaryOperableConstraint{ left, right, expr->evaluated_type, tk, {}, expr });
            // constrain the return type for comparison operators
            // == and != support any return type
            if (expr->op.type == DoubleEqual || expr->op.type == BangEqual) {
                expr->evaluated_type = Type{ .name = "bool",.module = core_module };
            }
            else if (expr->op.type == Spaceship) {
                state->add_constraint(ComparableConstraint{ expr->evaluated_type });
                expr->evaluated_type = Type{ .name = "bool", .module = core_module };
            }
            if (target) {
                state->add_constraint(EqualConstraint{ expr->evaluated_type, *target });
                //op->type = *target; // the node is going to be converted
            }
            return expr->evaluated_type;
            };
        auto do_overloadable = [expr, this, &do_overloadable_explicit_token]() {
            return do_overloadable_explicit_token(expr->op.type);
            };
        switch (expr->op.type) {
        case Minus: [[fallthrough]];
        case Star: [[fallthrough]];
        case Slash: [[fallthrough]];
        case Percent: [[fallthrough]];
        case DoubleGreater: [[fallthrough]];
        case DoubleLess: [[fallthrough]];
        case Plus: return do_overloadable();
        case Less: [[fallthrough]];
        case GreaterEqual: [[fallthrough]];
        case LessEqual: [[fallthrough]];
        case BangEqual: [[fallthrough]];
        case DoubleEqual: [[fallthrough]];
        case Spaceship: [[fallthrough]];
        case Greater: return do_overloadable_explicit_token(TokenType::Spaceship); // comparisons are all overloaded with <=>
        case Dot: {
            expr->evaluated_type = state->new_type_var();
            state->add_constraint(BinaryDotCompatibleConstraint{
                std::visit(targetless(), expr->lhs->toVariant()),
                expr->rhs.get(),
                expr->evaluated_type,
                expr
                });
            return expr->evaluated_type;
        }
        case Equal: {
            state->add_constraint(EqualConstraint{
                std::visit(targetless(true), expr->lhs->toVariant()),
                std::visit(targetless(), expr->rhs->toVariant())
                });
            expr->evaluated_type = Type{ .name = "void", .module = core_module };
            return expr->evaluated_type;
        }
        case DoubleDotEqual: irgen->error(Error(expr, "Not implemented yet")); return {};
        case DoubleDot: irgen->error(Error(expr, "Not implemented yet")); return {};
        }
    }
    FunctionType TypeChecker::operator()(GroupingExpression* expr) const {
        expr->evaluated_type = std::visit(*this, expr->expr->toVariant());
        return expr->evaluated_type;
    }
    FunctionType TypeChecker::operator()(LogicalOperation* expr) const {
        if (target && !target->is_boolean()) {
            irgen->error(Error(expr, "Cannot convert bool to " + target->full_name()));
            return Type{ "__error_type" };
        }
        auto left = std::visit(targetless(), expr->lhs->toVariant());
        auto right = std::visit(targetless(), expr->rhs->toVariant());
        // nothing can implicitly convert to bool and logical operations only work for and return bools
        state->add_constraint(EqualConstraint{ left, Type{.name = "bool", .module = core_module } });
        state->add_constraint(EqualConstraint{ right, Type{ .name = "bool", .module = core_module }});
        expr->evaluated_type = Type{ .name = "bool", .module = core_module };
        return expr->evaluated_type;
    }
    FunctionType TypeChecker::operator()(PostfixOperation* op) const { return {}; }
    FunctionType TypeChecker::operator()(CallOperation* op) const {
        auto callee_ty = std::visit(targetless(), op->callee->toVariant());
        if (callee_ty.name.starts_with("__union_var")) {
            // not a function call, actually Union initialization
            // TODO
            return Type{ "__error_type" };
        }
        state->add_constraint(IsInvocableConstraint{ callee_ty });
        // is of the form <expr>.<expr>() not <expr>()
        auto is_bound = callee_ty.is_bound;
        if (is_bound) {
            irgen->error(Error(op, "Not implemented"));
            return Type{ "__error_type" };
        }
        for (auto i  : std::views::iota(0u, op->arguments.size())) {
            state->add_constraint(ValidAsFunctionArgConstraint{
                std::visit(targetless(), op->arguments[i]->toVariant()),
                callee_ty,
                i + is_bound,
                op->arguments[i].get()
                });
        }
        op->evaluated_type = state->new_type_var();
        state->add_constraint(IsReturnOfConstraint{ op->evaluated_type, callee_ty, op });
        if (target) {
            state->add_constraint(EqualConstraint{ op->evaluated_type, *target, op });
        }
        return op->evaluated_type;
    }
    // TODO
    FunctionType TypeChecker::operator()(SubscriptOperation* subs) const {
        auto obj = std::visit(targetless(), subs->object->toVariant());
        auto idx = std::visit(targetless(), subs->index->toVariant());
        subs->evaluated_type = state->new_type_var();
        state->add_constraint(IndexOperableConstraint{ obj, idx, subs->evaluated_type, in_mutable_ctx, subs });
        return subs->evaluated_type;
    }
    FunctionType TypeChecker::operator()(LambdaExpression*) const { return Type{}; }

    bool advanceScopeModified(Type& type, ModuleBase*& md, std::string& hash, IRGenerator* irgen) {
        if (md->modules.contains(type.name))
        {
            md = md->modules.at(type.name);
            hash = md->module_hash;
            return true;
        }
        if (auto dets = md->findClass(hash, type.name); dets.second)
        {
            if (std::get<0>(*dets.second) != hash + type.name + "::") return false;
            hash = std::get<0>(*dets.second);
            return true;
        }
        if (auto [hsh, decl] = md->findGenericClass(hash, type.name); decl)
        {
            if (type.subtypes.size() != decl->clause.types.size()) return false;
            for (auto& sub : type.subtypes) sub.saturate(md, irgen);
            auto mangled_name = decl->name + IRGenerator::mangleGenericArgs(type.subtypes);
            irgen->generateGenericClass(md, hsh, decl, std::span{ type.subtypes });
            if (auto dets = md->findClass(hash, mangled_name); dets.second)
            {
                hash = std::get<0>(*dets.second); return true;
            }
            return false;
        }
        if (auto [name, fn] = md->findFunction(hash, type.name); fn)
        {
            hash = name + type.name + "::";
            return true;
        }
        if (auto [hsh, interface] = md->findInterface(hash, type.name); interface)
        {
            hash = hsh + "%%" + type.name + "%%interface"; //interfaces are terminal and cannot have subtyes
            return true;
        }
        if (auto alias = md->findAlias(hash, type.name); alias)
        {
            advanceScopeModified(*alias, md, hash, irgen);
        }
        return false;
    }
    
    FunctionType TypeChecker::operator()(ScopeOperation* scp) const {
        // we have 2 modes for scope checking
        // the first we check the module table and keep traversing
        // till we occur an incomplete generic (subtypes not monomorphized into module table)
        // we switch to looking directly at the generic's syntax tree
        ModuleBase* md = irgen->module;
        std::string hash = irgen->block_hash;
        std::string second_to_last = "";
        auto iterator = UnsaturatedTypeIterator(scp->type);
        auto type = iterator.next();
        // handle the first element [first]::Other::...::end;
        auto err = irgen->apply_using(type, md, hash);
        if (err) {
            err->span = SourceSpan{ scp->beg, scp->end };
        }
        std::unordered_map<std::string, Type> generic_instantiations;
        
        Statement* current_stat = get_type_stat(type, hash, md);
        auto clause = current_stat ? std::visit([](auto* arg) { return get_generic_clause(arg); }, current_stat->toVariant()) : nullptr;
        // add all generic types
        std::vector<Type> subtypes;
        auto num_subtypes = clause ? clause->types.size() : 0;
        if (type.subtypes.size() > num_subtypes) { 
            irgen->error(Error(scp, "Provided more types than schema allows")); 
            return Type{ "__error_type" }; 
        }
        for (auto i : std::views::iota(0u, type.subtypes.size())) {
            normalize_type(type.subtypes[i], state, irgen, {});
            subtypes.push_back(type.subtypes[i]);
            generic_instantiations[clause->types[i]] = type.subtypes[i];
        }
        for (auto i : std::views::iota(type.subtypes.size(), num_subtypes)) {
            auto var = state->new_type_var();
            generic_instantiations[clause->types[i]] = subtypes.emplace_back(var);
            for (auto& con : clause->constraints[clause->types[i]]) {
                auto& as_impl = std::get<ImplConstraint>(con);
                auto tp = as_impl.other;
                normalize_type(tp, state, irgen, {});
                state->add_constraint(ImplInterfaceConstraint{ var, tp, scp });
            }
        }
        hash.erase(hash.size() - 2, 2);
        hash += IRGenerator::mangleGenericArgs(subtypes) + "::";

        second_to_last.swap(type.name);

        while (!iterator.is_end())
        {
            type = iterator.next();
            if (auto err = advanceScopeNormalize(current_stat, md, hash, type, state, irgen, generic_instantiations))
            {
                debugbreak();
            }
            second_to_last.swap(type.name);
        }
        if (current_stat) {
            
            // at this point we are at the last element of the iter ( foo::bar::...::[end] )
            auto last = iterator.last();
            // scope expressions must refer to values not types
            // so the last element must be a function, union variant, constant, enum variant
            // and the function can be from an interface too so thats a slightly special case
            Statement* next_stat = get_next_stat(current_stat, last.name);
            // there is no next valid statement so its either an enum variant, interface function or union variant
            if (!next_stat) {
                if (auto intf = dynamic_cast<InterfaceDeclaration*>(current_stat)) {
                    auto it = std::ranges::find_if(intf->methods, [&last](auto& mth) {
                        return mth->name == last.name;
                        });
                    if (it == intf->methods.end()) {
                        irgen->error(Error(scp, "No method name '" + last.name + "' in the specified interface"));
                        return Type{ "__error_type" };
                    }
                    // return { Type{"__interface_fn" + actual_hash + interface->name + "$" + last.name } };
                }
                // enums
                if (auto enm = dynamic_cast<EnumDeclaration*>(current_stat)) {
                    // not valid enum variant
                    if (!enm->values.contains(last.name)) {
                        irgen->error(Error(scp, "Enum doesn't contain specified value"));
                    }
                    if (!last.subtypes.empty()) {
                        irgen->error(Error(scp, "Enum child cannot have subtypes"));
                        return Type{ "__error_type" };
                    }
                    // TODO enum hash
                    scp->evaluated_type = Type{ .name = second_to_last, .module = md, .block_hash = hash };
                }
                if (auto unn = dynamic_cast<UnionDeclaration*>(current_stat)) {
                    if (!unn->fields.contains(last.name)) {
                        irgen->error(Error(scp, "Union doesn't contain specified value"));
                    }
                    if (!last.subtypes.empty()) {
                        irgen->error(Error(scp, "Union child cannot have subtypes"));
                        return Type{ "__error_type" };
                    }
                    Type this_tp = unn->fields.at(last.name);
                    normalize_type(this_tp, state, irgen, generic_instantiations);
                    // TODO union hash
                    scp->evaluated_type = Type{
                        .name = "__union_var" + unn->name + "$" + last.name,
                        .subtypes = { this_tp },
                        .module = md,
                        .block_hash = hash,
                    };
                }
            }
            auto clause = std::visit([](auto* arg) { return get_generic_clause(arg); }, next_stat->toVariant());
            if (auto fn = dynamic_cast<FunctionDeclaration*>(next_stat)) {
                scp->evaluated_type.name = "__fn";
                scp->evaluated_type.block_hash = hash + fn->name + "::";
                scp->evaluated_type.module = md;
                // function may be generic so we add its types to our generic instantiations
                if (auto generic = dynamic_cast<GenericFunctionDeclaration*>(fn)) {
                    for (auto i : std::views::iota(0u, last.subtypes.size())) {
                        generic_instantiations[clause->types[i]] = last.subtypes[i];
                    }
                    // initialize unspecified generics to type variables
                    for (auto i : std::views::iota(last.subtypes.size(), clause->types.size())) {
                        generic_instantiations[clause->types[i]] = state->new_type_var();
                    }
                }
                // function signature is stored in subtypes
                for (auto& param : fn->signature.parameters) {
                    scp->evaluated_type.subtypes.push_back(param.type);
                    normalize_type(scp->evaluated_type.subtypes.back(), state, irgen, generic_instantiations);
                }
                scp->evaluated_type.subtypes.push_back(fn->signature.returnType);
                normalize_type(scp->evaluated_type.subtypes.back(), state, irgen, generic_instantiations);
            }
            if (auto constant = dynamic_cast<ConstantDeclaration*>(next_stat)) {
                scp->evaluated_type = constant->type;
                normalize_type(scp->evaluated_type, state, irgen, generic_instantiations);
            }
        }
        // no generics were encountered at all and we've reached the last entry
        else {
            auto last = iterator.last();
            if (auto [actual_hash, enm] = md->findEnum(hash, second_to_last); enm)
            {
                if (md != irgen->module && enm->is_private()) {
                    irgen->error(Error(scp, "The enum type " + actual_hash + enm->identifier + " is private"));
                    return Type{ "__error_type" };
                }
                if (!last.subtypes.empty()) {
                    irgen->error(Error(scp, "Enum child cannot have subtypes"));
                    return Type{ "__error_type" };
                }
                if (enm->values.contains(last.name)) { 
                    scp->evaluated_type = Type{ .name = second_to_last, .module = md, .block_hash = actual_hash };
                } else {
                    irgen->error(Error(scp, "Enum does not contain specified value"));
                    return Type{ "__error_type" };
                }
            }
            else if (auto [actual_hash, unn] = md->findUnion(hash, second_to_last); unn)
            {
                if (md != irgen->module && unn->is_private()) {
                    irgen->error(Error(scp, "The union is private"));
                    return Type{ "__error_type" };
                }
                if (!last.subtypes.empty()) {
                    irgen->error(Error(scp, "Union child cannot have subtypes"));
                    return Type{ "__error_type" };
                }
                if (unn->fields.contains(last.name)) {
                    scp->evaluated_type = Type{ .name = "__union_var" + unn->name + "$" + last.name, .module = md, .block_hash = actual_hash };
                }
                else {
                    irgen->error(Error(scp, "Union does not contain specified variant"));
                    return Type{ "__error_type" };
                }
            }
            else if (auto [name, fn] = md->findFunction(hash, last.name); fn)
            {
                if (md != irgen->module && fn->is_private()) {
                    irgen->error(Error(scp, "The function type " + name + fn->name + " is private"));
                    return Type{ "__error_type" };
                }

                irgen->block_hash.swap(hash);
                irgen->saturateSignature(fn->sig, md);
                irgen->block_hash.swap(hash);

                scp->evaluated_type.block_hash = hash + last.name + "::";
                scp->evaluated_type.name = "__fn";
                scp->evaluated_type.module = md;
                std::ranges::copy(fn->sig.parameters | std::views::transform([](auto& elem) { return elem.type; }), 
                    std::back_inserter(scp->evaluated_type.subtypes));
                scp->evaluated_type.subtypes.push_back(fn->sig.returnType);
            }
            else if (auto [name, c] = md->findConst(hash, last.name); c)
            {
                irgen->block_hash.swap(hash);
                std::get<0>(*c).saturate(md, irgen);
                irgen->block_hash.swap(hash);
                return { std::get<0>(*c) };
            }
            else {
                irgen->error(Error(scp, last.name + " does not exist in " + hash));
                return Type{ "__error_type" };
            }
        }
        return scp->evaluated_type;
    }
    FunctionType TypeChecker::operator()(ObjectLiteral* lit) const {
        lit->evaluated_type = lit->t;
        auto stat = normalize_type(lit->evaluated_type, state, irgen, {});
        
        for (auto& [field, elem] : lit->values) {
            Type result = state->new_type_var();
            state->add_constraint(HasFieldConstraint{ lit->evaluated_type, field, result, lit });
            Type other;
            if (elem) { other = std::visit(targetless(), elem->toVariant()); }
            else {
                elem = std::make_unique<NameExpression>(field);
                other = std::visit(targetless(), elem->toVariant());
            }
            state->add_constraint(EqualConstraint{ result, other, lit });
        }
        auto names = lit->values | std::views::keys;
        auto fields = std::vector<std::string>{names.begin(), names.end()};
        state->add_constraint(AllFieldsConstraint{ fields, lit->evaluated_type, lit });
        return lit->evaluated_type;
    }
    FunctionType TypeChecker::operator()(NullLiteral* lit) const {
        if (target) {
            if (!target->is_optional()) {
                irgen->error(Error(lit, "Cannot convert from 'null' to non-optional type"));
                return Type{ "__error_type" };
            }
            lit->evaluated_type = *target;
            return *target;
        }
        auto inner = state->new_type_var();
        lit->evaluated_type = Type{ .name = "__opt", .subtypes = { inner }, .module = core_module };
        return lit->evaluated_type;
    }
    FunctionType TypeChecker::operator()(AsExpression* ex) const {
        auto src = std::visit(targetless(), ex->expr->toVariant());
        normalize_type(ex->dest, state, irgen, {});
        state->add_constraint(ConvertibleToConstraint{
                src, ex->dest, ex
            });
        if (target) {
            state->add_constraint(EqualConstraint{
                    ex->dest,
                    *target, ex
                });
        }
        ex->evaluated_type = ex->dest;
        return ex->evaluated_type;
    }
    FunctionType TypeChecker::operator()(CharLiteral* ch) const {
        if (target) {
            if (!target->is_char()) {
                irgen->error(Error(ch, "Cannot convert char to " + target->full_name()));
            }
        }
        ch->evaluated_type = Type{ .name = "char", .module = core_module };
        return ch->evaluated_type;
    }
    FunctionType TypeChecker::operator()(GCNewExpression* gcn) const {
        auto sub_type = std::visit(
            target ?
                new_target(target->subtypes[0]) :
                *this,
            gcn->target_expression->toVariant());
        state->add_constraint(OwningConstraint{ sub_type }); // cannot store non owning value in gc
        gcn->evaluated_type = Type{ .name = "__gcref", .subtypes = { sub_type }, .module = core_module };
        return gcn->evaluated_type;
    }
    FunctionType TypeChecker::operator()(MacroInvocation* mcr) const {
        MacroEvaluator{ irgen }.eval(mcr);
        mcr->evaluated_type = std::visit(*this, mcr->result->toVariant());
        return mcr->evaluated_type;
    }
    FunctionType TypeChecker::operator()(SpawnExpression* ex) const {
        irgen->error(Error(ex, "Not implemented"));
        return Type{ "__error_type" };
    }

    TypeChecker TypeChecker::new_target(std::optional<Type> tgt, bool in_mutable_ctx) const
    {
        return TypeChecker{ tgt, irgen, state, in_mutable_ctx };
    }

    TypeChecker TypeChecker::targetless(bool in_mutable_ctx) const
    {
        return TypeChecker{ std::nullopt, irgen, state, in_mutable_ctx };
    }

    
    bool has_type_variable(const Type& tp) {
        if (is_type_variable(tp)) return true;
        if (auto pos = tp.block_hash.find('?'); pos != std::string::npos)
            // the "?" must be followed by a number
            return tp.block_hash.size() > pos + 1 && std::isdigit(static_cast<unsigned char>(tp.block_hash[pos + 1]));
        for (auto& sub : tp.subtypes) {
            if (has_type_variable(sub)) return true;
        }
        return false;
    }
    //======================================//
    //========CONSTRAINT SOLVING============//
    //======================================//

    // TODO 
    // any constraint that just checks should also
    // reduce the domain but still return false (eg to string constraint)
    bool ConstraintSolver::operator()(IsIntegerConstraint& con)
    {
        auto type = state->best_repr(con.type);
        if (is_type_variable(type)) {
            auto domain = state->get_type_domain(type);
            auto grp = Domain::Group{};
            grp.add_type(Type{ .name = "i8", .module = core_module });
            grp.add_type(Type{ .name = "i16", .module = core_module });
            grp.add_type(Type{ .name = "i32", .module = core_module });
            grp.add_type(Type{ .name = "i64", .module = core_module });

            grp.add_type(Type{ .name = "u8", .module = core_module });
            grp.add_type(Type{ .name = "u16", .module = core_module });
            grp.add_type(Type{ .name = "u32", .module = core_module });
            grp.add_type(Type{ .name = "u64", .module = core_module });
            if(auto error = domain->add_and_intersect(std::move(grp), state))
                irgen->error(*error);
        }
        else {
            if (!type.is_integral()) irgen->error(Error(con.expr, "This type must be an integer"));
            // this constraint is done
        }
        return true;
    }
    bool ConstraintSolver::operator()(CanStoreIntegerConstraint& con)
    {
        auto type = state->best_repr(con.type);
        if (is_type_variable(type)) {
            if (auto as_u64 = std::get_if<uint64_t>(&con.value))
                state->get_type_domain(type)->constrain_to_store(*as_u64);
            else
                state->get_type_domain(type)->constrain_to_store(std::get<int64_t>(con.value));
            return true;
        }
        else {
            // TODO
        }
        return true;
    }
    bool ConstraintSolver::operator()(IsFloatConstraint& con)
    {
        auto type = state->best_repr(con.type);
        if (is_type_variable(type)) {
            auto domain = state->get_type_domain(type);
            auto grp = Domain::Group{};
            grp.add_type(Type{ .name = "f32", .module = core_module });
            grp.add_type(Type{ .name = "f64", .module = core_module });
            if (auto error = domain->add_and_intersect(std::move(grp), state))
                irgen->error(*error);
        }
        else {
            if (!type.is_floating_point()) irgen->error(Error(con.expr, "This type must be an floating point"));
            // this constraint is done
        }
        return true;
    }
    bool ConstraintSolver::operator()(CanStoreRealConstraint& con)
    {
        auto type = state->best_repr(con.type);
        if (is_type_variable(type)) {
            state->get_type_domain(type)->constrain_to_store(con.value);
            return false; // we return false because the domain may get populated again and we need to still filter
        }
        else {
            // TODO
        }
        return true;
    }
    bool ConstraintSolver::operator()(ToStringConstraint& con)
    {
        auto type = state->best_repr(con.type);
        if (is_type_variable(type)) {
            return false;
        }
        else if (has_type_variable(type)) {
            // we can technically check the class definition
            return false;
        }
        else {
            // TODO Type::has_to_str()
            return true;
        }
    }
    /// Compares 2 (possibly generic) types with (or without) type parameters
    /// to see if they're compatible and generates equality constraints for nested type variables
    /// the equality constraint is always ordered as "type1 elem = type2 elem"
    std::optional<Error> generic_match(Type tp1, Type tp2, ASTNode* loc, TypeCheckerState* solver) {
        // include the block in the name
        // because type.subtypes only occurs generics at the end ─────────────────╮
        // so we can check cases where the generic occurs earlier ──╮             │
        //                        ╭─────────────────────────────────╯ like here   │
        //                       vvv                                              │
        // eg Module::Something::<T>::OtherStuff::Type[::<U>] <───────────────────╯
        tp1.name = tp1.full_name_no_generics();
        tp2.name = tp2.full_name_no_generics();
        auto tp1_iter = UnsaturatedTypeIterator(tp1);
        auto tp2_iter = UnsaturatedTypeIterator(tp2);
        if (tp1_iter.num_iters() != tp2_iter.num_iters()) {
            return Error(loc, std::format("The types {} and {} are incompatible", tp1.name, tp2.name));
        }
        while (!tp1_iter.is_end()) {
            auto this1 = tp1_iter.next();
            auto this2 = tp2_iter.next();
            // we now have an atomic framgent of the type path that isn't the end
            // using the the earlier example we have    ╭ one of these individually with correct subtype representation
            //   ╭────────────┬─────────────┬───────────╯        
            // vvvvvv  vvvvvvvvvvvvvv  vvvvvvvvvv
            // Module::Something::<T>::OtherStuff::Type::<U>
            if (this1.name != this2.name) {
                return Error(loc, std::format("The types {} and {} are incompatible", tp1.name, tp2.name));
            }
            if (this1.subtypes.size() != this2.subtypes.size()) {
                return Error(loc, std::format("The types {} and {} are incompatible", tp1.name, tp2.name));
            }
            for (auto i : std::views::iota(0u, this1.subtypes.size())) {
                // this is the T in Something::<T> hence it could be a varaible
                // or another full on type
                if (is_type_variable(this1.subtypes[i]) || is_type_variable(this2.subtypes[i])) {
                    solver->add_constraint(EqualConstraint{ this1.subtypes[i], this2.subtypes[i], loc });
                }
                else {
                    if(auto failed = generic_match(this1.subtypes[i], this2.subtypes[i], loc, solver))
                        return failed;
                }
            }
        }
        auto end1 = tp1_iter.last(); // using the example this is `Type` with proper subtypes
        auto end2 = tp2_iter.last();
        if (end1.name != end2.name) {
            return Error(loc, std::format("The types {} and {} are incompatible", tp1.name, tp2.name));
        }
        if (end1.subtypes.size() != end2.subtypes.size()) {
            return Error(loc, std::format("The types {} and {} are incompatible", tp1.name, tp2.name));
        }
        for (auto i : std::views::iota(0u, end1.subtypes.size())) {
            if (is_type_variable(end1.subtypes[i]) || is_type_variable(end2.subtypes[i])) {
                solver->add_constraint(EqualConstraint{ end1.subtypes[i], end2.subtypes[i], loc });
            }
            else {
                if (auto failed = generic_match(end1.subtypes[i], end2.subtypes[i], loc, solver))
                    return failed;
            }
        }
        return std::nullopt;
    }
    bool can_match(Type tp1, Type tp2) {
        tp1.name = tp1.full_name_no_generics();
        tp2.name = tp2.full_name_no_generics();
        auto tp1_iter = UnsaturatedTypeIterator(tp1);
        auto tp2_iter = UnsaturatedTypeIterator(tp2);
        if (tp1_iter.num_iters() != tp2_iter.num_iters()) {
            return false;
        }
        while (!tp1_iter.is_end()) {
            auto this1 = tp1_iter.next();
            auto this2 = tp2_iter.next();
            // this iteration is explained in `generic_match`
            if (this1.name != this2.name) {
                return false;
            }
            if (this1.subtypes.size() != this2.subtypes.size()) {
                return false;
            }
            for (auto i : std::views::iota(0u, this1.subtypes.size())) {
                // if one of the subtypes is a variable, it matches
                // else we recurse
                if (is_type_variable(this1.subtypes[i]) || is_type_variable(this2.subtypes[i])) {
                    continue;
                }
                else {
                    if (auto pass = can_match(this1.subtypes[i], this2.subtypes[i]))
                        continue;
                    else return false;
                }
            }
        }
        auto end1 = tp1_iter.last(); // using the example this is `Type` with proper subtypes
        auto end2 = tp2_iter.last();
        if (end1.name != end2.name) {
            return false;
        }
        if (end1.subtypes.size() != end2.subtypes.size()) {
            return false;
        }
        for (auto i : std::views::iota(0u, end1.subtypes.size())) {
            if (is_type_variable(end1.subtypes[i]) || is_type_variable(end2.subtypes[i])) {
                continue;
            }
            else {
                if (auto pass = can_match(end1.subtypes[i], end2.subtypes[i]))
                    continue;
                else return false;
            }
        }
        return true;
    }
    bool ConstraintSolver::operator()(EqualConstraint& con)
    {
        auto type1 = state->best_repr(con.type1);
        auto type2 = state->best_repr(con.type2);
        if (is_type_variable(type1)) {
            // ?1 and ?2 <case 1>
            if (is_type_variable(type2)) {
                state->unify_types(type1, type2, irgen);
            }
            // type 2 can be a Type::<?1> or a Path::To::<?1>::Type::Val
            // but not exaclty a type variable, what to do here?
            // ?1 and Something::<?2>::Type <case 2>
            else if (has_type_variable(type2)) {
                // Here we substitute into the domain
                // even though domain was originally designed to contain
                // concrete types the validation can be handled by the state
                if (auto err = state->get_type_domain(type1)->equal_constrain(std::move(type2)))
                    irgen->error(*err);
            }
            // ?1 and concrete <case 3>
            else {
                if(auto err = state->get_type_domain(type1)->equal_constrain(std::move(type2)))
                    irgen->error(*err);
            }
        }
        else if (has_type_variable(type1)) {
            // same as <case 2>
            if (is_type_variable(type2)) {
                if (auto err = state->get_type_domain(type2)->equal_constrain(std::move(type1)))
                    irgen->error(*err);
            }
            // Something::<?1>::Type and Something::<?1>::Type <case 4>
            else if (has_type_variable(type2)) {
                // dissect and match to generate  new constraints
                // this can come in three forms 
                // the block string can have the variables ...::Something::<?1>::...
                // the actual subtypes can have the variables (much easier)
                // the actual subtypes match either form 1 or 2 (recursive) (actual hell)
                // or both
                // luckily we have a neat routine for that
                //begin checking subtypes
                if(auto err = generic_match(std::move(type1), std::move(type2), con.expr, state))
                    irgen->error(*err);
            }
            // Something::<?1>::Type and concrete <case 5>
            else {
                // generic match can also work here probably
                if (auto err = generic_match(std::move(type1), std::move(type2), con.expr, state))
                    irgen->error(*err);
            }
        }
        else {
            // type 1 is concrete
            
            // concrete and ?2 (same as <case 3>)
            if (is_type_variable(type2)) {
                if (auto err = state->get_type_domain(type2)->equal_constrain(std::move(type1)))
                    irgen->error(*err);
            }
            // concrete and Something::<?2>::Type (same as <case 5>)
            else if (has_type_variable(type2)) {
                if (auto err = generic_match(std::move(type1), std::move(type2), con.expr, state))
                    irgen->error(*err);
            }
            // concrete and concrete
            else {
                if (!type1.is_equal(type2)) {
                    irgen->error(Error(con.expr, std::format("The types {} and {} have to be equal", 
                        type1.pretty_name(irgen->block_hash),
                        type2.pretty_name(irgen->block_hash))));
                    return true;
                }
            }
        }
        return true; //remove this constraint
    }
    bool ConstraintSolver::operator()(OwningConstraint& con)
    {
        auto type = state->best_repr(con.type);
        if (has_type_variable(type)) {
            return false;
        }

        if (type.is_non_owning(irgen))
            irgen->error(Error(con.expr, std::format("They type {} must be owning", type.pretty_name(irgen->block_hash))));
        return true;
    }
    bool ConstraintSolver::operator()(IsInvocableConstraint& con)
    {
        auto callee = state->best_repr(con.type);
        if (callee.is_error_ty()) return true;
        if (is_type_variable(callee)) {
            return false;
        }
        if (callee.name != "__fn" && callee.name != "__bound_fn") {
            irgen->error(Error(con.expr, "Type is not callable"));
            return true;
        }
        return true;
    }
    bool ConstraintSolver::operator()(ValidAsFunctionArgConstraint& con)
    {
        auto callee = state->best_repr(con.function);
        auto arg = state->best_repr(con.type);

        if (is_type_variable(callee)) {
            return false;
        }
        bool is_bound = callee.name == "__bound_fn";
        if (callee.name != "__fn" && !is_bound) {
            irgen->error(Error(con.expr, "Type is not callable"));
            return true;
        }
        // checking the bound expr
        if (is_bound && con.arg_no == uint32_t(-1)) {
            // ^this is only compatible with gc references
            if (callee.subtypes[0].is_gc_reference()) {
                add_new_constraint(EqualConstraint{ callee.subtypes[0], arg });
            }
            // &mut this works with mutable references and mutable arguments
            else if (callee.subtypes[0].is_mutable_reference()) {
                if (is_type_variable(arg)) {
                    // this type is either &mut This or This
                    Domain::Group gp;
                    gp.add_type(Type(callee.subtypes[0]));
                    gp.add_type(Type(callee.subtypes[0].deref()));
                    auto domain = state->get_type_domain(arg);
                    if (auto err = domain->add_and_intersect(std::move(gp), state)) {
                        irgen->error(err.value());
                    }
                }
                else {
                    if (arg.is_mutable_reference() || arg.is_mutable) {
                        add_new_constraint(EqualConstraint{ callee.subtypes[0].deref(), arg.deref() });
                    }
                    else {
                        irgen->error(Error(con.expr, "Tried to bind to mutable reference with immutable parameter"));
                    }
                }
            }
            // &this works for arguments of type Type, &Type, &mut Type and ^Type
            else if (callee.subtypes[0].is_reference()) {
                if (is_type_variable(arg)) {
                    // this type is either &mut This or This
                    Domain::Group gp;
                    gp.add_type(Type(callee.subtypes[0].deref()));
                    gp.add_type(Type(callee.subtypes[0].deref().reference_to()));
                    gp.add_type(Type(callee.subtypes[0].deref().mutable_reference_to()));
                    gp.add_type(Type(callee.subtypes[0].deref().gc_reference_to()));
                    auto domain = state->get_type_domain(arg);
                    if (auto err = domain->add_and_intersect(std::move(gp), state)) {
                        irgen->error(err.value());
                    }
                }
                else {
                    add_new_constraint(EqualConstraint{ callee.subtypes[0].deref(), arg.deref() });
                }
            }
            return true;
        }
        if ((callee.subtypes.size() - 1) <= con.arg_no) {
            irgen->error(Error(con.expr, "Too many function args"));
            return true;
        }

        add_new_constraint(EqualConstraint{ callee.subtypes[con.arg_no + is_bound], arg, con.expr });
        return true;
    }
    bool ConstraintSolver::operator()(IsReturnOfConstraint& con)
    {
        auto callee = state->best_repr(con.function);
        auto arg = state->best_repr(con.type);
        if(callee.is_error_ty() || arg.is_error_ty()) return true;
        if (is_type_variable(callee)) {
            return false;
        }

        if (callee.name != "__fn" && callee.name != "__bound_fn") {
            irgen->error(Error(con.expr, "Type is not callable"));
            return true;
        }

        add_new_constraint(EqualConstraint{ callee.subtypes.back(), arg, con.expr });
        return true;
    }
    bool ConstraintSolver::operator()(ImplInterfaceConstraint& con)
    {
        using namespace std::views;
        // An interface can be implemented in 2 places(and what it means for generics):
        // - In the type definition:
        // ```
        // Generic: struct::<T, E> = {
        //     impl Interface {
        //         ...
        //     }
        // }
        // all of Generic::<T, E> implement Interface even with enable_if and generic constraints
        // (the easy one)
        // 
        // - In the interface definition
        // Interface: interface = {
        //     impl::<T> for Generic::<T, f32> { ... }
        // }
        // this is the hard one Interface is only implemented for some
        // specializations of Generic
        // (note that specializations are not allowed to overlap so 
        // Generic::<T> and Generic::<i32> can not both be defined)
        // and also Interfaces can never actually be generic directly
        // - Interface: interface::<T> defines an interface with type parameters T
        // (i.e a type cannot implement Interface::<i32> and Interface::<f32> because they are the same
        // you can be generic with a bit of hackery:
        // ```
        // GenericInterface: struct::<T> = {
        //     Interface: interface = {}
        // }
        // a type can implement 
        // GenericInterface::<i32>::Interface and GenericInterface::<f32>::Interface
        // ```

        // The approach is:
        // If we have a type a type variable:
        // - Infinite Domain: return false
        // - Finite Domain: 
        //      remove all types that don't implement the interface,
        //      return true (we can return true because the domain can never grow)
        // If we have a type that doesnt have a type variable:
        // - we check if type impl under the type (the easy implementation)
        // - we check if interface is implemented for the type
        // all while generating appropriate constraints
        
        // "type" must not be a type variable
        bool ret_val = true;
        auto check_type_against_interface =
            // take_action means whether to go ahead and constrain the type(s) or to just
            // check if there's a possibility
            [this, &con, &ret_val](Type& intf, Type& type, InterfaceDeclaration* decl, bool take_action) {
                auto [type_decl, generics] = normalize_type(type, state, irgen, {});
                auto impls = std::visit([](auto* arg) { return get_impls(arg);}, type_decl->toVariant());
                std::vector<TypeCheckerConstraint> collector;
                auto collector_ptr = &collector;
                std::swap(state->write_new_constraints_to, collector_ptr);
                // this is to see if we can constrain the subject type
                // we can only constrain it if we encounter 1 valid impl (if not we may introduce false constraints)
                std::unordered_map<uint32_t, Domain> subject_constraints;
                uint32_t num_matches = 0;
                // we don't intersect domains instantly, we collect all possible types into a group
                // then intersect the domains at once
                std::unordered_map<uint32_t, Domain::Group> itf_generic_groups;
                // these are only used when we get exactly one match
                std::unordered_map<uint32_t, Domain::Group> type_generic_groups;
                // when we match itf as variable and type variable
                std::vector<EqualConstraint> var_to_var_constraints;
                if (impls) {
                    for (auto& impl : *impls) {
                        // this copy is on purpose
                        auto impl_for = impl.impl_for;
                        auto reference_block = type.block_hash;
                        std::swap(irgen->block_hash, reference_block);
                        normalize_type(impl_for, state, irgen, generics);
                        std::swap(irgen->block_hash, reference_block);
                        // this can actually match multiple times
                        // Type: struct = {
                        //     impl::<T> path::<T, i32>::Interface {}
                        //     impl::<T> path::<T, f32>::Interface {}
                        // }
                        // matches with path::<?T, ?T2>::Interface twice
                        if (auto err = generic_match(impl_for, intf, con.expr, state); !err) {
                            if (!take_action) {
                                std::swap(state->write_new_constraints_to, collector_ptr);
                                return true;
                            }
                            num_matches++;
                            
                            for (auto& con : collector | transform([](auto& con) -> auto& { 
                                return std::get<EqualConstraint>(con); 
                            })) {
                                // type1 is always from type and type2 is from interface
                                // and one of either must be a type variable
                                // if the interface is a type variable we constrain its domain
                                // like ?T2 in the above example can be {i32, f32}
                                // if the type is a type variable we constrain its domain to be exactly from the interface
                                // but we can only constrain this if this is the right implementation
                                // so we have to return false for further info
                                if (is_type_variable(con.type2) && !is_type_variable(con.type1)) {
                                    itf_generic_groups[state->tbl.type_to_id(con.type2)].add_type(
                                        std::move(con.type1)
                                    );
                                }
                                else if (is_type_variable(con.type1) && !is_type_variable(con.type2)) {
                                    type_generic_groups[state->tbl.type_to_id(con.type1)].add_type(
                                        std::move(con.type2)
                                    );
                                }
                                // I don't actually know this is the right thing to do here
                                else {
                                    var_to_var_constraints.push_back(std::move(con));
                                }
                            }
                            // TODO: see what happens if we move back up
                            collector.clear();
                        };
                    }
                }
                
                // we still need to check the interface
                std::swap(irgen->block_hash, intf.block_hash);
                // here we check all type the interface is implemented for (2nd syntax)
                // we normalize each of the types and check if they can match our input type
                // if so
                for (const auto& elem : decl->impl_fors
                    | transform(
                        [this](const ImplBlock& blk) {
                            std::unordered_map<std::string, Type> substitutions = {};
                            auto impl_for = blk.impl.impl_for;
                            for (auto& type : blk.clause.types) {
                                // need a way to make type vars without requiring them
                                // to be solved
                                substitutions[type] = state->new_type_var();
                            }
                            normalize_type(impl_for, state, irgen, std::move(substitutions));
                            return impl_for;
                        })
                    | filter(
                        [&type](const Type& tp) {
                            // we get all the implemented types that can match
                            // multiple can match but can't overlap 
                            // e.g (Type::<?, i32> Type::<?, u32>)
                            return can_match(type, tp);
                        }))
                {
                    if (!take_action) {
                        std::swap(irgen->block_hash, intf.block_hash);
                        std::swap(state->write_new_constraints_to, collector_ptr);
                        return true;
                    }
                    num_matches++;
                    generic_match(type, elem, con.expr, state);
                    for (auto& con : collector | transform([](auto& con) -> auto& {
                        return std::get<EqualConstraint>(con);
                        }))
                    {
                        if (is_type_variable(con.type2) && !is_type_variable(con.type1)) {
                            itf_generic_groups[state->tbl.type_to_id(con.type2)].add_type(
                                std::move(con.type1)
                            );
                        }
                        else if (is_type_variable(con.type1) && !is_type_variable(con.type2)) {
                            type_generic_groups[state->tbl.type_to_id(con.type1)].add_type(
                                std::move(con.type2)
                            );
                        }
                        else {
                            var_to_var_constraints.push_back(std::move(con));
                        }
                    }
                    collector.clear();
                }
                std::swap(state->write_new_constraints_to, collector_ptr);
                std::swap(irgen->block_hash, intf.block_hash);
                for (auto& [id, group] : itf_generic_groups) {
                    state->get_type_domain(state->tbl.id_to_type(id))->add_and_intersect(std::move(group), state);
                }
                // If we have only 1 match we constrain the subject types
                if (num_matches == 1) {
                    for (auto& [id, group] : type_generic_groups) {
                        subject_constraints[id].add_and_intersect(std::move(group), state);
                    }
                    for (auto& [id, domain] : subject_constraints) {
                        state->get_type_domain(state->tbl.id_to_type(id))
                            ->merge_intersect(std::move(domain), state);
                    }
                    for (auto& con : var_to_var_constraints) {
                        add_new_constraint(std::move(con));
                    }
                }
                else if (num_matches == 0) {
                    irgen->error(Error(con.expr, "Type doesn not implement interface"));
                }
                // if we have more than one match we probably need more info (or are wrong idk)
                else {
                    ret_val = false;
                }
                return true;
            };
        auto type = state->best_repr(con.type);
        auto intf = state->best_repr(con.interface);
        // Importnat Assumption: intf cannot be a type variable
        if (is_type_variable(intf)) return false;
        auto [intf_decl, b] = normalize_type(intf, state, irgen, {});
        if (intf_decl == nullptr) intf_decl = get_type_stat(intf, intf.block_hash, intf.module, true);        
        if (intf_decl == nullptr) {
            return true; // there was an error, but it was reported by normalize type (hopefully)
        }
        auto as_intf = dynamic_cast<InterfaceDeclaration*>(intf_decl);
        if (!as_intf) {
            irgen->error(Error(con.expr, "The type " + con.interface.full_name() + " is not an interface"));
            return true;
        }
        // If its a type variable with an infinite domain we come back later
        // other wise we prune it
        if (is_type_variable(type)) {
            auto domain = state->get_type_domain(type);
            // empty domain
            if (domain->concrete_types.types.empty()) {
                return false;
            }
            std::erase_if(domain->concrete_types.types, [&intf, as_intf, &check_type_against_interface](Type& tp) { 
                return !check_type_against_interface(intf, tp, as_intf, false); 
                });
            return true;
        }
        else {
            check_type_against_interface(intf, type, as_intf, true);
        }
        return ret_val;
    }
    bool ConstraintSolver::operator()(HasUnaryMinusConstraint& con) {
        auto type = state->best_repr(con.type);
        auto rets = state->best_repr(con.ret);

        if(is_type_variable(type)) {
            return false;
        }
        // TODO
        return false;
    }
    bool ConstraintSolver::operator()(HasUnaryNotConstraint& con)
    {
        // TODO
        return false;
    }
    bool ConstraintSolver::operator()(IsReferenceToConstraint& con)
    {
        auto type = state->best_repr(con.type);
        auto other = state->best_repr(con.other);
        add_new_constraint(EqualConstraint{
                type,
                reference_to(other, irgen)
            });
        return true;
    }
    bool ConstraintSolver::operator()(IsNotReferenceConstraint& con)
    {
        auto type = state->best_repr(con.type);
        if (is_type_variable(type)) {
            return false;
        }
        if (type.is_reference()) {
            irgen->error(Error(con.expr, std::format("The type {} must not a reference", type.pretty_name(irgen->block_hash))));
        }
        return true;
    }
    std::pair<std::string, OverloadDetailsBinary*> resolveBin(const Type& lhs, const Type& rhs, TokenType t, IRGenerator* irgen);
    bool ConstraintSolver::operator()(BinaryOperableConstraint& con) {
        auto left = state->best_repr(con.left);
        auto right = state->best_repr(con.right);
        auto result = state->best_repr(con.result);
        auto is_compatible = [&result, this, &con](const Type& left, const Type& right, TokenType tt, bool take_action)
            {
                using namespace std::views;
                std::vector<ModuleBase*> valid_modules;
                if (left.module && right.module) {
                    valid_modules.reserve(4);
                    std::array<ModuleBase*, 4> modules{ left.module, left.deref().module, right.module, right.deref().module };
                    for (auto module : modules) {
                        auto it = std::ranges::find(valid_modules, module);
                        if (it == valid_modules.end()) {
                            valid_modules.push_back(module);
                        }
                    }
                }
                else {
                    auto all_modules = irgen->module->engine->modules | transform([](auto& in) { return in.second.get(); });
                    valid_modules.insert(valid_modules.end(), all_modules.begin(), all_modules.end());
                }
                std::vector<TypeCheckerConstraint> l_collector;
                std::vector<TypeCheckerConstraint> r_collector;
                std::vector<TypeCheckerConstraint> intermediate;
                auto intermediate_ptr = &intermediate;
                std::swap(state->write_new_constraints_to, intermediate_ptr);
                std::vector<std::pair<ModuleBase*, OverloadDetailsBinary*>> matches;
                for (auto module : valid_modules) {
                    for (auto& [block, ovl] : module->overloads.binary_details_for(tt)) {
                        std::unordered_map<std::string, Type> generic_subs;
                        if (ovl.statement && !ovl.statement->clause.types.empty()) {
                            if (!con.substitution_cache.contains(ovl.statement))
                                for (auto& tp : ovl.statement->clause.types)
                                    con.substitution_cache[ovl.statement][tp] = state->new_type_var();
                            generic_subs = con.substitution_cache[ovl.statement];
                        }
                        Type expected_left = ovl.left;
                        Type expected_right = ovl.right;
                        irgen->block_hash.swap(block);
                        std::swap(irgen->module, module);
                        normalize_type(expected_left, state, irgen, generic_subs);
                        normalize_type(expected_right, state, irgen, std::move(generic_subs));
                        std::swap(irgen->module, module);
                        irgen->block_hash.swap(block);
                        // much like for interfaces, if both sides can match we take it into consideration
                        // and if there's only one in consideration thats our answer, if there's zero its an error
                        // and if its more than one we have to wait a bit
                        auto left_old_size = l_collector.size();
                        intermediate.clear();
                        if (generic_match(left, expected_left, nullptr, state) == std::nullopt) {
                            std::ranges::move(std::move(intermediate), std::back_inserter(l_collector));
                        }
                        else continue;
                        intermediate.clear();
                        if (generic_match(right, expected_right, nullptr, state) == std::nullopt) {
                            // they both successfuly match
                            std::ranges::move(std::move(intermediate), std::back_inserter(r_collector));
                            matches.emplace_back(module, &ovl);
                            // we found a match if we're not taking action we can abort we can return here
                            if (!take_action) {
                                std::swap(state->write_new_constraints_to, intermediate_ptr);
                                return true;
                            }
                        }
                        // on failure revert left to its original state
                        else {
                            l_collector.erase(l_collector.begin() + left_old_size, l_collector.end());
                        }
                    };
                }
                std::swap(state->write_new_constraints_to, intermediate_ptr);
                // the correct match
                if (matches.size() == 1) {
                    if (take_action)
                    {
                        std::ranges::move(std::move(l_collector), std::back_inserter(temp_constraints));
                        std::ranges::move(std::move(r_collector), std::back_inserter(temp_constraints));

                        auto this_result = matches[0].second->result;
                        normalize_type(this_result, state, irgen, con.substitution_cache[matches[0].second->statement]);
                        std::visit([&matches, &con]<typename T>(T * expr) {
                            if constexpr (std::same_as<T, BinaryOperation> || std::same_as<T, SubscriptOperation>) {
                                expr->selected = matches[0].second;
                                expr->module = matches[0].first;
                                if (matches[0].second->statement) {
                                    for (auto& generic : matches[0].second->statement->clause.types) {
                                        auto& subs = con.substitution_cache[matches[0].second->statement];
                                        expr->subtypes.push_back(subs[generic]);
                                    }
                                }
                            }
                            // this can probably be replaced with reinterpret_cast
                        }, dynamic_cast<Expression*>(con.expr)->toVariant());
                        
                        add_new_constraint(EqualConstraint{ result, this_result, con.expr });
                    }
                    return true;
                }
                else if (matches.size() == 0 && take_action) {
                    irgen->error(Error(con.expr, "Overload does not exist between types"));
                }
                return false;
            };
        if (is_type_variable(left) && is_type_variable(right)) {
            return false;
        }
        // only left is a variable
        if (is_type_variable(left)) {
            auto domain = state->get_type_domain(left);
            // we should probably generate all possible types to populate the domain
            if (domain->concrete_types.types.empty()) return false;
            std::erase_if(domain->concrete_types.types, [&con, &is_compatible, &right](const Type& tp) 
                { 
                    return !is_compatible(tp, right, con.op, false);
                });
            return false;
        }
        else if (is_type_variable(right)) {
            auto domain = state->get_type_domain(right);
            // we should probably generate all possible types to populate the domain (TODO)
            if (domain->concrete_types.types.empty()) return false;
            std::erase_if(domain->concrete_types.types, [&con, &is_compatible, &left](const Type& tp)
                {
                    return !is_compatible(left, tp, con.op, false);
                });
            return false;
        }
        else {
            return is_compatible(left, right, con.op, true);
        }
        return true;
    }
    bool ConstraintSolver::operator()(ComparableConstraint& con)
    {
        auto type = state->best_repr(con.type);
        if (has_type_variable(type)) {
            return false;
        }

        if (
            !((type.name == "CmpOrd" || type.name == "CmpPartOrd")
                && type.module == core_module)
            )
        {
            irgen->error(Error(con.expr, std::format("{} does not support <, >, >= or <=", type.pretty_name(irgen->block_hash))));
        }
        return true;
    }
    uint32_t UnificationTable::unite(uint32_t id, uint32_t id2, IRGenerator *irgen, TypeCheckerState* state) {
        auto finda = find(id);
        auto findb = find(id2);
        if (finda == findb)
            return finda;
        if (rank[finda] < rank[findb])
            std::swap(finda, findb);
        parents[findb] = finda;
        if (rank[finda] == rank[findb])
            rank[finda]++;

        if (auto produced_error =
                domains[finda].merge_intersect(std::move(domains[findb]), state))
          irgen->error(*produced_error);
        domains.erase(findb);
        return finda;
    }
    void TypeCheckerState::resolve_function(FunctionDeclaration* decl, IRGenerator* irgen, const FunctionSignature& sig)
    {
        push_variable_block();
        for (const auto& param : sig.parameters) {
            variables.back()[param.name] = param.type;
        }
        return_type = sig.returnType;
        write_new_constraints_to = &constraints;
        // generate constraints
        std::visit(TypeChecker{std::nullopt, irgen, this}, decl->body->toVariant());
        // solve constraints
        ConstraintSolver sv{ false, irgen, this };
        // any generated constraints are written into a temporary buffer
        write_new_constraints_to = &sv.temp_constraints;
        bool has_change = true;
        while (has_change) {
            has_change = std::erase_if(constraints, [&sv](TypeCheckerConstraint& elem) {
                return std::visit(sv, elem);
                }) != 0;
            // add newly generated constraints here
            std::ranges::move(sv.temp_constraints, std::back_inserter(constraints));
            sv.temp_constraints.clear();
        }
        // not enough information given
        if (!constraints.empty()) {
            // This is for debugging constraints that didn't resolve
            debugbreak();
            for (auto& con : constraints)
                std::visit(sv, con);
            // TODO actual reporting
            irgen->error(Error(decl, "Not enough info to resolve types"));
        }
        // substitute types in every ast node??
        // maybe we can do this in the borrow checker ?
        pop_variable_block();
    }
    bool TypeCheckerState::is_non_owning(const Type& tp, IRGenerator* irgen)
    {
        if ((tp.is_reference() && !tp.is_gc_reference()) || tp.is_slice()) return true;
        if (tp.is_view() && !tp.is_gc_view()) return true;
        if (tp.is_optional() || tp.is_variant() || tp.is_tuple() || tp.is_result())
        {
            for (auto& subtype : tp.subtypes)
                if (is_non_owning(subtype, irgen)) return true;
            return false;
        }
        if (tp.name == "__conv_result_ref") return true;
        if (auto decl = get_type_stat(tp, tp.block_hash, tp.module))
            // unions can also be non-owning but its not implemented yet
            if(auto as_cls = dynamic_cast<ClassDeclaration*>(decl))
                return as_cls->ownership == Ownership::NonOwning || as_cls->ownership == Ownership::NonOwningMut;
        return false;
    }
    bool ConstraintSolver::operator()(ExtractsToConstraint& con)
    {
        // only 3 kinds of types satisfy this
        // T? (__opt::<T>) extracts to T
        // __conv_result_ref::<T> extracts to T (special internal type returned by variant conversion)
        // __conv_result_val::<T> extracts to T              ^^^^^^----same case as above

        auto target = state->best_repr(con.dst);
        auto type = state->best_repr(con.type);

        if (is_type_variable(type)) {
            return false; // we can't know
        }

        if (
            type.name != "__opt" && 
            type.name != "__conv_result_ref" && 
            type.name != "__conv_result_val" &&
            type.name != "__res"
            ) {
            irgen->error(Error(con.expr, "Invalid type for conditional extraction"));
            return true;
        }
        add_new_constraint(EqualConstraint{
                type.subtypes[0],
                target
            });
        return true;
    }
    bool ConstraintSolver::operator()(RefExtractsToConstraint& con)
    {
        // only 2 kinds of types satisfy this
        // T? (__opt::<T>) extracts to &T or &mut T
        // T \ E (__res::<T, E>)
        // __conv_result_ref::<T> extracts to T (special internal type returned by variant conversion)

        auto target = state->best_repr(con.dst);
        auto type = state->best_repr(con.type);

        if (is_type_variable(type)) {
            return false; // we can't know
        }

        if (type.name != "__opt" && 
            type.name != "__conv_result_ref" &&
            type.name != "__res") {
            irgen->error(Error(con.expr, "Invalid type for reference conditional extraction"));
            return true;
        }
        add_new_constraint(EqualConstraint{
                type.is_mutable ? type.subtypes[0].mutable_reference_to() : type.subtypes[0].reference_to(),
                target,
                con.expr
            });
        return true;
    }
    bool ConstraintSolver::operator()(NonOwningConstraint& con)
    {
        auto type = state->best_repr(con.type);
        if (has_type_variable(type)) {
            return false;
        }

        if (!type.is_non_owning(irgen))
            irgen->error(Error(con.expr, std::format("They type {} must be non owning", type.pretty_name(irgen->block_hash))));
        return true;
    }
    bool ConstraintSolver::operator()(ConvertibleToConstraint& con)
    {
        auto src = state->best_repr(con.from);
        auto dst = state->best_repr(con.to);

        // TODO other cases
        if (is_type_variable(dst)) {
            if (!has_type_variable(src)) {
                // src can convert to src? and depending on the type other things
                Domain::Group gp;
                gp.add_type(Type{ .name = "__opt", .subtypes{src}, .module = core_module });
                if (auto err = state->get_type_domain(dst)->add_and_intersect(std::move(gp), state)) irgen->error(err.value());
                return true;
            }
        }
        else {
            // anything can convert to optional
            if (dst.name == "__opt") {
                add_new_constraint(EqualConstraint{ dst.subtypes[0], src });
                return true;
            }
        }
        return false;
    }
    /// What exactly does
    /// <expr1>.<expr2>
    /// mean / do?
    /// 
    /// It evaluates to the first one of these that is satisfied:
    /// 1 - if <expr1> is (functionally equivalent to) a tuple and <expr2> is an integer constant expression
    /// then the expression is a tuple access
    /// 
    /// 2 - if <expr1> is a struct type and <expr2> is one of the field names of that class, then the expression is
    /// a struct field access
    /// 
    /// 3 - if <expr2> is one of the inherent methods of <expr1> the expression is binding <expr1> to the first arg of
    /// the function call
    /// 
    /// 4 - if <expr2> is one of the inherent methods of operator&(<expr>) the expression is binding operator&(<expr1>) to the first
    /// arg of the function call
    /// 
    /// 5 - if <expr2> is a method of an interface that is implemented by <expr1>, the expression is binding <expr1> to the interface
    /// function call
    /// 
    /// 6 - if <expr2> evaluates to a function, the expression binds <expr1> as the first arg of that function call
    /// 
    /// The expression is ill-formed
    bool ConstraintSolver::operator()(BinaryDotCompatibleConstraint& con)
    {
        auto left = state->best_repr(con.tp);
        auto result = state->best_repr(con.result);
        auto as_name = dynamic_cast<NameExpression*>(con.right);
        // if left is type variable and right is name, we could have ambiguity
        if ((is_type_variable(left) || is_type_variable(left.deref())) && as_name) {
            return false;
        }
        // check tuple member access <tuple>.<number> [case 1]
        if (left.deref().is_tuple()) {
            // this should Ideally be any integer constant expression
            // we can use the conditional constraint do some magic here
            if (auto as_int = dynamic_cast<IntegerLiteral*>(con.right)) {
                auto return_type = left
                    .deref() // remove reference to get the underlying tuple
                    .subtypes[std::stol(std::string{ as_int->text })] // get the right subtype
                    .take_mutability_characteristics(left.deref());
                add_new_constraint(EqualConstraint{ result, return_type });
                return true;
            }
        }
        auto no_reference = left.deref();
        // TODO probably remove this functionality from normalize_type and change it to something else
        auto [stat, gctx] = normalize_type(no_reference, state, irgen, {});
        if (as_name) {
            // check class member access [case 2]
            if (auto cls = dynamic_cast<ClassDeclaration*>(stat)) {
                auto it = std::ranges::find_if(cls->vars, [as_name](ClassVariable& var) {
                    return var.name == as_name->text;
                    });
                if (it != cls->vars.end()) {
                    // we copy out because we may be in a generic declaration
                    // and we don't modify the AST to not affect future instantiations
                    auto type = it->type;
                    normalize_type(type, state, irgen, gctx);
                    add_new_constraint(EqualConstraint{ result, type });
                    return true;
                }
            }
            auto check_inherent_submethod = [this, &result](
                //--gotten from normalize_type--
                std::remove_cvref_t<decltype(gctx)> const& gctx,
                Statement* stat,
                //------------------------------
                const Type& type, std::string_view name, Type& right,
                ASTNode* expr
                ) {
                    const Type& no_reference = type.deref();
                    // in checking subfunctions, if the type is not generic we can search the module directly
                    // in the block of the type
                    if (gctx.empty()) {
                        std::string function_block = no_reference.full_name() + "::";
                        // we could use "findFunction" but we know the exact block
                        if (no_reference.module->functions.contains(function_block)) {
                            auto& funs = no_reference.module->functions.at(function_block);
                            if (auto it = std::ranges::find_if(funs,
                                [name](ModuleBase::FunctionDetails& dets) {
                                    return dets.name == name;
                                }); it != funs.end())
                            {
                                if (!it->sig.parameters.empty() && it->sig.parameters[0].name == "this") {
                                    right.name = "__fn";
                                    right.block_hash = function_block + std::string(name) + "::";
                                    right.module = type.module;

                                    irgen->block_hash.swap(function_block);
                                    std::ranges::copy(it->sig.parameters |
                                        std::views::transform([](auto& param) { return param.type; }) |
                                        std::views::transform([this, gctx](Type param) { normalize_type(param, state, irgen, {}); return param; }),
                                        std::back_inserter(right.subtypes));
                                    right.subtypes.push_back(it->sig.returnType);
                                    normalize_type(right.subtypes.back(), state, irgen, gctx);
                                    irgen->block_hash.swap(function_block);

                                    auto& first_type = right.subtypes[0];
                                    // first_type name is "This" we edit it to left.deref()
                                    if (!first_type.is_reference()) first_type = no_reference;
                                    else first_type.subtypes[0] = no_reference;

                                    auto result_type = Type{
                                            .name = "__bound_fn", // the first parameter
                                            .subtypes = right.subtypes,
                                            .module = type.module,
                                            .block_hash = right.block_hash
                                    };
                                    add_new_constraint(EqualConstraint{ result, result_type, expr });
                                    add_new_constraint(ValidAsFunctionArgConstraint{ type, std::move(result_type), uint32_t(-1), expr });
                                    return true;
                                }
                            }
                        }
                    }
                    // For generic types we search the AST directly because they don't exist yet
                    else if (auto fn = dynamic_cast<FunctionDeclaration*>(get_next_stat(stat, name))) {
                        if (!fn->signature.parameters.empty() && fn->signature.parameters[0].name == "this") {
                            // correct the right type
                            right.name = "__fn";
                            right.block_hash = type.block_hash + type.name + "::" + std::string(name) + "::";
                            right.module = type.module;
                            std::ranges::copy(fn->signature.parameters |
                                std::views::transform([](auto& param) { return param.type; }) |
                                std::views::transform([this, gctx](Type param) { normalize_type(param, state, irgen, gctx); return param; }),
                                std::back_inserter(right.subtypes));
                            auto& first_type = right.subtypes[0];
                            // first_type name is This we edit it to left
                            if (!first_type.is_reference()) first_type = no_reference;
                            else first_type.subtypes[0] = no_reference;
                            right.subtypes.push_back(fn->signature.returnType);
                            normalize_type(right.subtypes.back(), state, irgen, gctx);
                            auto result_type = Type{
                                    .name = "__bound_fn", // the first parameter
                                    .subtypes = right.subtypes,
                                    .module = type.module,
                                    .block_hash = right.block_hash
                            };
                            add_new_constraint(EqualConstraint{ result, result_type, expr });
                            add_new_constraint(ValidAsFunctionArgConstraint{ type, std::move(result_type), uint32_t(-1), expr });
                            return true;
                        }
                    }
                    return false;
                };
                // [case 3]
                // if the right is a name it could also be a function binding i.e `vec.push`
                // we check the type for subfunctions whose first parameter is `this`, `&this`, or `&mut this`

                // the type itslef has a inherent method named "<expr2>"
                if (check_inherent_submethod(gctx, stat, left, as_name->text, con.right->evaluated_type, con.expr))
                    return true;
                // else we check the types "deref" [case 4]
                // worth noting that they type may not be fully resolved so the deref may have multiple results
                // in that case we exit and also operator &mut and operator & must return the same thing

                // if weve already checked before and had a result don't check again
                if (!con.deref_result_opt) {
                    auto op_result = unary_operator_result(TokenType::RefMut, mutable_reference_to(no_reference, irgen), con.substitution_cache, state, irgen);
                    if (auto invalid = std::get_if<NoOperatorReason>(&op_result)) {
                        // in this case we can't be sure
                        if (*invalid == NoOperatorReason::MulipleMatches) return false;
                        // if there's no &mut we check for &
                        else if (*invalid == NoOperatorReason::NoMatches) {
                            auto op_result_immutable = unary_operator_result(TokenType::Ampersand, reference_to(no_reference, irgen), con.substitution_cache, state, irgen);
                            if (auto invalid_immutable = std::get_if<NoOperatorReason>(&op_result_immutable)) {
                                if (*invalid == NoOperatorReason::MulipleMatches) return false;
                                else if (*invalid_immutable == NoOperatorReason::NoMatches); // fall through to UCFS
                            }
                            else {
                                auto& [result_tp, constraints] = std::get<0>(op_result_immutable);
                                // we cache this incase we have to revisit this constraint
                                con.deref_result_opt = std::move(result_tp);
                                std::ranges::move(std::move(constraints), std::back_inserter(temp_constraints));
                            }
                        }
                    }
                    else {
                        auto& [result_tp, constraints] = std::get<0>(op_result);
                        con.deref_result_opt = std::move(result_tp);
                        std::ranges::move(std::move(constraints), std::back_inserter(temp_constraints));
                    }
                }
                else { con.deref_result_opt = state->best_repr(con.deref_result_opt.value()); }

                // deref_result might be a type variable here 
                if (con.deref_result_opt) {
                    auto& deref_result = con.deref_result_opt.value();

                    // come back later this should be relatively cheap because the result is cached
                    if (is_type_variable(deref_result.deref())) return false;

                    auto [stat2, gctx2] = normalize_type(deref_result, state, irgen, {});
                    if (check_inherent_submethod(gctx2, stat2, deref_result, as_name->text, con.right->evaluated_type, con.expr))
                        return true;
                }
                
                

                // case 5 todo
            
        }
        // [case 6]
        auto right_t = std::visit(TypeChecker{ std::nullopt, irgen, state }, con.right->toVariant());
        auto right = state->best_repr(right_t);
        if (right.name != "__fn") {
            irgen->error(Error(con.expr, "Cannot bind to non function"));
        }
        auto result_type = Type{
            .name = "__bound_fn", // the first parameter
            .subtypes = right.subtypes,
            .module = left.module,
            .block_hash = right.block_hash
        };
        add_new_constraint(EqualConstraint{ result, result_type, con.expr });
        add_new_constraint(ValidAsFunctionArgConstraint{ left, std::move(result_type), uint32_t(-1), con.expr });
        return true;
    }
    bool ConstraintSolver::operator()(ElseRefExtractsToConstraint& con)
    {
        // this is only satisfied by the result type
        // T??E (__res::<T, E>) and it extracts to &E
        auto target = state->best_repr(con.dst);
        auto type = state->best_repr(con.type);
        auto mutable_reference_to = [base = core_module](const Type& tp) {
            return Type{ .name = "__ref_mut", .subtypes = {tp}, .module = base };
            };
        auto reference_to = [base = core_module](const Type& tp) {
            return Type{ .name = "__ref", .subtypes = {tp}, .module = base };
            };
        if (is_type_variable(type)) {
            // Domain supports partial types
            Domain::Group grp;
            auto error_type = state->new_type_var();
            grp.add_type(Type{ .name = "__res", .subtypes = { state->new_type_var(), error_type }});
            auto err = state->get_type_domain(type)->add_and_intersect(std::move(grp), state);
            if (err) irgen->error(err.value());
            add_new_constraint(EqualConstraint{
                 type.is_mutable ? mutable_reference_to(error_type) : reference_to(error_type),
                 target,
                 con.expr
                });
            return true;
        }

        if (type.name != "__res") {
            irgen->error(Error(con.expr, "Conditional extraction can also have else branch capture with results"));
            return true;
        }
        add_new_constraint(EqualConstraint{
                type.is_mutable ? mutable_reference_to(type.subtypes[1]) : mutable_reference_to(type.subtypes[1]),
                target,
                con.expr
            });
        return true;
    }
    bool ConstraintSolver::operator()(ElseExtractsToConstraint& con)
    {
        auto target = state->best_repr(con.dst);
        auto type = state->best_repr(con.type);

        if (is_type_variable(type)) {
            // Domain supports partial types
            Domain::Group grp;
            auto error_type = state->new_type_var();
            grp.add_type(Type{ .name = "__res", .subtypes = { state->new_type_var(), error_type } });
            auto err = state->get_type_domain(type)->add_and_intersect(std::move(grp), state);
            if (err) irgen->error(err.value());
            add_new_constraint(EqualConstraint{
                 error_type,
                 target,
                 con.expr
                });
            return true;
        }

        if (type.name != "__res") {
            irgen->error(Error(con.expr, "Conditional extraction can also have else branch capture with results"));
            return true;
        }
        add_new_constraint(EqualConstraint{
                type.subtypes[1],
                target,
                con.expr
            });
        return true;
    }
    bool ConstraintSolver::operator()(IfStatementConstraint& con)
    {
        auto then_tp = state->best_repr(con.then_type);
        auto else_tp = state->best_repr(con.else_type);
        auto result = state->best_repr(con.result);
        if (con.then_transfers_control && con.else_transfers_control) {
            // no constraints they're free to be any valid type
            add_new_constraint(EqualConstraint{ result, Type{.name = "void", .module = core_module}, con.expr });
        }
        else if (!con.then_transfers_control && con.else_transfers_control) {
            // (then = else) or (else is void) and (result = then)
            add_new_constraint(EqualConstraint{ then_tp, result, con.expr });
            add_new_constraint(EqualOrIsVoidConstraint{ else_tp, then_tp, con.expr });
        }
        else if (con.then_transfers_control && !con.else_transfers_control) {
            // (then = else) or (then is void) and (result = else)
            add_new_constraint(EqualConstraint{ else_tp, result, con.expr });
            add_new_constraint(EqualOrIsVoidConstraint{ then_tp, else_tp, con.expr });
        }
        // they both don't transfer control
        else {
            //(then = else) and result = then(or else)
            add_new_constraint(EqualConstraint{ else_tp, then_tp, con.expr });
            add_new_constraint(EqualConstraint{ else_tp, result, con.expr });
        }
        return true;
    }
    bool ConstraintSolver::operator()(EqualOrIsVoidConstraint& con)
    {
        auto tp1 = state->best_repr(con.type1);
        auto tp2 = state->best_repr(con.type2);

        if (is_type_variable(tp1)) {
            auto domain = state->get_type_domain(tp1);
            // if tp1 cannot be void we can go ahead and add the equal constraint

            if (domain->concrete_types.types.empty()) return false;
            for (auto& type : domain->concrete_types.types) {
                if (type.is_void()) return false;
            }
            add_new_constraint(EqualConstraint{ tp1, tp2, con.expr });
        }
        if (tp1.is_void()) return true;
        add_new_constraint(EqualConstraint{ tp1, tp2, con.expr });
        return true;
    }
    bool ConstraintSolver::operator()(IndexOperableConstraint& con)
    {
        // We need to check the special case of static arrays ([T; n])
        // they aren't registered with the default operator registry so
        // if it can be a static array and the right can be a u64
        // we defer to make sure, before sending off
        auto left = state->best_repr(con.left);
        auto right = state->best_repr(con.right);
        auto token_type = con.is_mutable ? TokenType::SquarePairMut : TokenType::SquarePair;
        bool can_be_is_concrete = true;
        auto can_be = [this, &can_be_is_concrete](const Type& type, auto pred) {
                if (is_type_variable(type)) {
                    auto domain = state->get_type_domain(type);
                    // if we get here it might be a different overload
                    can_be_is_concrete = false;
                    // infinite domain or one element satisfies the constraint
                    return domain->concrete_types.types.empty() || std::ranges::any_of(domain->concrete_types.types, pred);
                }
                return pred(type);
            };
        if (
            can_be(left, [&can_be](const Type& tp) { return
                tp.is_static_array() ||
                (tp.is_reference() && can_be(tp.subtypes[0], [](const Type& tp2) { return tp2.is_static_array();  }));
                }) &&
            can_be(right, [](const Type& tp) { return tp.is_unsigned_integral() && tp.integer_width() == 64; })
                    )
        {
            if (can_be_is_concrete) {
                // the expression evaluator should already have a special case for this
                add_new_constraint(
                    EqualConstraint{ left.deref().subtypes[0], con.result, con.expr }
                );
                return true;
            }
            else return false;
        }
        else {
            // for index operations left is valid as T or &T so we need to stall until we can tell its not a reference
            if (is_type_variable(left)) return false;
            Type new_left = Type{ .subtypes = {left.deref()}, .module = core_module };
            if (token_type == TokenType::SquarePair) new_left.name = "__ref";
            else {
                if (!(left.is_mutable_reference() || left.is_mutable)) {
                    irgen->error(Error(con.expr, std::format("{} is immutable but used in mutable context", left.pretty_name(irgen->block_hash))));
                }
                new_left.name = "__ref_mut";
            }
            // automatic dereference by matching it to a reference to the result
            Type new_result = Type{
                .name = token_type == TokenType::SquarePair ? "__ref" : "__ref_mut",
                .subtypes = { state->best_repr(con.result) },
                .module = core_module
            };
            add_new_constraint(BinaryOperableConstraint{ 
                new_left, 
                right, 
                new_result, 
                token_type, {}, con.expr });
            return true;
        }
    }
    bool ConstraintSolver::operator()(HasFieldConstraint& con)
    {
        auto subject = state->best_repr(con.subject);
        auto result = state->best_repr(con.result);
        if (is_type_variable(subject)) return false;
        auto [stat, gctx] = normalize_type(subject, state, irgen, {});
        if (auto cls = dynamic_cast<ClassDeclaration*>(stat)) {
            auto it = std::ranges::find_if(cls->vars, [con](ClassVariable& var) {
                return var.name == con.field_name;
                });
            if (it != cls->vars.end()) {
                auto type = it->type;
                normalize_type(type, state, irgen, std::move(gctx));
                add_new_constraint(EqualConstraint{ result, type });
                return true;
            }
        }
        irgen->error(Error(con.expr, "Type does not have field"));
    }
    bool ConstraintSolver::operator()(AllFieldsConstraint& con)
    {
        auto subject = state->best_repr(con.subject);
        if (is_type_variable(subject)) return false;
        auto [stat, gctx] = normalize_type(subject, state, irgen, {});
        if (auto cls = dynamic_cast<ClassDeclaration*>(stat)) {
            auto& first_range = con.fields;
            auto second_range = cls->vars | std::views::transform([this, &gctx](const ClassVariable& var) {
                    return var.name;
                });
            auto first_set = std::set<std::string>{ first_range.begin(), first_range.end() };
            auto second_set = std::set<std::string>{ second_range.begin(), second_range.end() };
            if (first_set.size() != first_range.size()) irgen->error(Error(con.expr, "Duplicate entries"));
            if (first_set != second_set) irgen->error(Error(con.expr, "All fields must be initialized"));
            return true;
        }
        irgen->error(Error(con.expr, "Internal Error"));
    }
    bool ConstraintSolver::operator()(BorrowResultConstraint& con)
    {
        // Borrowing a type can lead to multiple outcomes
        // the first being (T -> &T) and the second being defined by operator&
        // str -> &str, &view str
        // [T; N] -> &[T; N], &[T]
        // [T; *] -> &[T; *], &[T]
        // T -> &T, &view (Interfaces implemented by T), & (Borrow operator result(if defined))
        auto subject = state->best_repr(con.subject);
        auto result = state->best_repr(con.result);
        // maybe we should tell the domain to default to *result
        if (is_type_variable(subject) && is_type_variable(result)) {
            return false;
        }
        else if (is_type_variable(result)) {
            Domain::Group possible_types;
            //------------The base T -> &T mapping-------------
            possible_types.add_type(Type{ .name = "__ref", .subtypes = {subject}, .module = core_module });
            //--------------operator&-------------------
            if (subject.is_str()) possible_types.add_type(Type{ .name = "__ref", .subtypes = {
                Type{.name = "__string_view", .module = core_module}
                }, .module = core_module });
            else if (subject.is_static_array() || subject.is_dynamic_array()) possible_types.add_type(Type{ .name = "__ref", .subtypes = {
                Type{.name = "__slice", .subtypes = { subject.subtypes[0] }, .module = core_module} },
                .module = core_module
                });
            else {
                auto op_result = unary_operator_result(TokenType::Ampersand, reference_to(subject, irgen), con.substitution_cache, state, irgen);
                if (auto invalid = std::get_if<NoOperatorReason>(&op_result)) {
                    if (*invalid == NoOperatorReason::MulipleMatches) return false;
                    else if (*invalid == NoOperatorReason::NoMatches); 
                }
                else {
                    auto& [type, extra_constraints] = std::get<0>(op_result);
                    possible_types.add_type(Type(type));
                    if (!extra_constraints.empty()) {
                        add_new_constraint(IfEqualThenConstrain{
                            std::move(type),
                            result,
                            std::move(extra_constraints)
                         });
                    }
                }
            }
            if (auto error = state->get_type_domain(result)->add_and_intersect(std::move(possible_types), state)) {
                irgen->error(error.value());
            }
            return true;
        }
        else if (is_type_variable(subject)) {
            if (result.name != "__ref") {
                irgen->error(Error(con.expr, "Operator & always yields a reference"));
            }
            // filter the domain of "subject"
            if (auto domain = state->get_type_domain(subject); !domain->concrete_types.types.empty()) {
                std::erase_if(domain->concrete_types.types, [&result](const Type& type) {
                    if (can_match(type, result.deref())) return false;
                    if (result.deref().name == "__string_view" && type.is_str()) return false;
                    if (result.deref().name == "__slice" && type.is_array()) return false;
                    return true;
                    // check if it can match
                });
                if (domain->is_solved()) return true;
                return false;
            }
            else {
                // many types can return &T as thier borrow result
                // we don't want to have to check all modules for that
                // this means code like this will fail to typecheck
                // Source: struct = {
                //     x: f32,
                //     perform: fn(&this) -> i32 = { return 10; }
                // }
                // produce: fn::<T> -> T = { /* get the value somehow */ }
                // operator: &(obj: &Struct) -> &f32 = { return &obj.x; }
                // main: fn = {
                //     b := produce();
                //     with(a: &f32 as &b) {
                //         b.perform();
                //     }
                // }
                return false;
            }
            
        }

        
    }
    bool ConstraintSolver::operator()(BorrowResultMutConstraint& con)
    {
        return false;
    }
    bool ConstraintSolver::operator()(IfEqualThenConstrain& con)
    {
        auto type1 = state->best_repr(con.type1);
        auto type2 = state->best_repr(con.type2);
        // have to revise this equality scheme but it should
        // be sufficient for normalized types
        if (has_type_variable(type1) || has_type_variable(type2)) {
            return false;
        }
        if (type1.full_name() == type2.full_name()) {
            std::ranges::move(std::move(con.apply_if_true), std::back_inserter(temp_constraints));
        } 
        return true;
    }
    void ConstraintSolver::add_new_constraint(TypeCheckerConstraint con)
    {
        temp_constraints.push_back(std::move(con));
    }
    Type TypeCheckerState::new_type_var()
    {
        auto idx = tbl.parents.size();
        tbl.parents.push_back(idx);
        tbl.rank.push_back(0);
        return Type{ .name = std::format("?{}", idx) };
    }
    void TypeCheckerState::add_constraint(TypeCheckerConstraint c)
    {
        write_new_constraints_to->push_back(std::move(c));
    }
    void TypeCheckerState::unify_types(const Type& t1, const Type& t2, IRGenerator* irgen)
    {
        if (is_type_variable(t1) && is_type_variable(t2)) {
            tbl.unite(tbl.type_to_id(t1), tbl.type_to_id(t2), irgen, this);
        }
    }
    void TypeCheckerState::push_variable_block()
    {
        variables.emplace_back();
    }
    void TypeCheckerState::pop_variable_block()
    {
        variables.pop_back();
    }
    void TypeCheckerState::create_variable(std::string name, Type type)
    {
        variables.back()[name] = std::move(type);
    }
    Type TypeCheckerState::best_repr(const Type& tp)
    {
        // convert to parent from
        if (is_type_variable(tp)) {
            auto actual_id = tbl.find(tbl.type_to_id(tp));
            auto domain = tbl.domain_of(actual_id);
            if (domain->is_solved()) {
                // this solution may be parametrized
                if (has_type_variable(domain->concrete_types.types[0])) {
                    // see if we have already solved what its parametrized to
                    // should probably prevent recusrion by checking if the solution contains the variable
                    domain->concrete_types.types[0] = best_repr(domain->concrete_types.types[0]);
                }
                return domain->get_solution().take_mutability_characteristics(tp);
            }
            return tbl.id_to_type(actual_id).take_mutability_characteristics(tp);
        }
        else if (has_type_variable(tp)) {
            // decompose tp and rebuild but substitute all type vars with best repr
            std::string actual_block;
            Type buffer = tp;
            buffer.name = buffer.full_name_no_generics();
            auto it = UnsaturatedTypeIterator(buffer);
            while (!it.is_end()) {
                auto elem = it.next();
                actual_block += elem.name + IRGenerator::mangleGenericArgs(elem.subtypes | std::views::transform([this](auto& elem) {
                    return best_repr(elem);
                    })) + "::";
            }
            Type final_t = it.last();
            for (auto& elem : final_t.subtypes) { elem = best_repr(elem); }
            final_t.block_hash = actual_block;
            final_t.module = tp.module;
            return final_t.take_mutability_characteristics(tp);
        }
        else {
            // concrete already
            return tp;
        }
    }
    Domain* TypeCheckerState::get_type_domain(const Type& type)
    {
        return tbl.domain_of(tbl.find(tbl.type_to_id(type)));
    }
    void Domain::Group::add_type(Type&& tp)
    {
        types.push_back(tp);
    }
    std::optional<Error> Domain::add_and_intersect(Group&& grp, TypeCheckerState* state)
    {
        if (concrete_types.types.empty()) {
            concrete_types.types = std::move(grp.types);
            return std::nullopt;
        }
        // merge with the infinite set
        if (grp.types.empty()) {
            return std::nullopt;
        }
        // We could have non concrete types so the intersection process is a bit more delicate
        std::vector<Type> out;
        std::vector<TypeCheckerConstraint> cons;
        auto cons_ptr = &cons;
        std::swap(state->write_new_constraints_to, cons_ptr);
        for (auto& type : grp.types) {
            // find matching type and generate equality constraints
            for (auto& current_type : concrete_types.types) {
                // we redirect generated constraints from generic match to a different vector
                // so that if they don't match we don't have wrong constraints in our solver
                auto res = generic_match(type, current_type, nullptr, state);
                if (!res.has_value()) {
                    // std::move(current_type) should also work
                    // the generated constraints should cover any blind spots
                    out.push_back(std::move(type)); 
                    // cons_ptr should contain our original constraint buffer
                    std::ranges::move(cons, std::back_inserter(*cons_ptr));
                    cons.clear();
                    break;
                }
                cons.clear();
            }
        }
        if (out.empty()) {
            std::swap(state->write_new_constraints_to, cons_ptr);
            return Error(nullptr, "Constradictory type");
        }
        concrete_types.types = std::move(out);
        std::swap(state->write_new_constraints_to, cons_ptr);
        return std::nullopt;
    }
    int64_t getIntMinOf(const Type& type)
    {
        if (type.is_signed_integral())
        {
            switch (*type.integer_width())
            {
            case 8: return std::numeric_limits<int8_t>::min();
            case 16: return std::numeric_limits<int16_t>::min();
            case 32: return std::numeric_limits<int32_t>::min();
            case 64: return std::numeric_limits<int64_t>::min();
            default: return 0;/*unreachable*/
            }
        }
        if (type.is_unsigned_integral())
        {
            switch (*type.integer_width())
            {
            case 8: return std::numeric_limits<uint8_t>::min();
            case 16: return std::numeric_limits<uint16_t>::min();
            case 32: return std::numeric_limits<uint32_t>::min();
            case 64: return std::numeric_limits<uint64_t>::min();
            default: return 0;/*unreachable*/
            }
        }
        if (type.is_floating_point())
        {
            if (*type.float_width() == 32) return -std::pow(2, 24);
            if (*type.float_width() == 64) return -std::pow(2, 53);
        }
        return 0;
    }
    uint64_t getIntMaxOf(const Type& type)
    {
        if (type.is_signed_integral())
        {
            switch (*type.integer_width())
            {
            case 8: return std::numeric_limits<int8_t>::max();
            case 16: return std::numeric_limits<int16_t>::max();
            case 32: return std::numeric_limits<int32_t>::max();
            case 64: return std::numeric_limits<int64_t>::max();
            default: return 0;/*unreachable*/
            }
        }
        if (type.is_unsigned_integral())
        {
            switch (*type.integer_width())
            {
            case 8: return std::numeric_limits<uint8_t>::max();
            case 16: return std::numeric_limits<uint16_t>::max();
            case 32: return std::numeric_limits<uint32_t>::max();
            case 64: return std::numeric_limits<uint64_t>::max();
            default: return 0;/*unreachable*/
            }
        }
        if (type.is_floating_point())
        {
            if (*type.float_width() == 32) return std::pow(2, 24);
            if (*type.float_width() == 64) return std::pow(2, 53);
        }
        return 0;
    }
    double getFloatMinOf(const Type& type)
    {
        if (type.is_floating_point())
        {
            if (*type.float_width() == 32) return std::numeric_limits<float>::min();
            if (*type.float_width() == 64) return std::numeric_limits<double>::min();
        }
        return 0;
    }

    double getFloatMaxOf(const Type& type)
    {
        if (type.is_floating_point())
        {
            if (*type.float_width() == 32) return std::numeric_limits<float>::max();
            if (*type.float_width() == 64) return std::numeric_limits<double>::max();
        }
        return 0;
    }
    std::optional<Error> Domain::constrain_to_store(uint64_t val)
    {
        if (concrete_types.types.empty()) {
            return std::nullopt;
        }
        std::erase_if(concrete_types.types, [val](Type& elem) { 
            if (!elem.is_integral()) { return true; }
            return !((elem.is_signed_integral() || getIntMinOf(elem) <= val) && getIntMaxOf(elem) >= val);
            });
        if (concrete_types.types.empty()) {
            return Error(nullptr, "Type cannot store " + std::to_string(val));
        }
        return std::nullopt;
    }
    std::optional<Error> Domain::constrain_to_store(int64_t val)
    {
        if (val < 0) {
            if (concrete_types.types.empty()) {
                return std::nullopt;
            }
            std::erase_if(concrete_types.types, [val](Type& elem) {
                if (!elem.is_signed_integral()) { return true; }
                return !(getIntMinOf(elem) <= val);
                });
            if (concrete_types.types.empty()) {
                return Error(nullptr, "Type cannot store " + std::to_string(val));
            }
            return std::nullopt;
        }
        else {
            return constrain_to_store(static_cast<uint64_t>(val));
        }
    }
    std::optional<Error> Domain::constrain_to_store(double val)
    {
        if (concrete_types.types.empty()) {
            return std::nullopt;
        }
        std::erase_if(concrete_types.types, [val](Type& elem) {
            if (!elem.is_floating_point()) { return true; }
            return !(getFloatMinOf(elem) < val && getFloatMaxOf(elem) > val);
            });
        if (concrete_types.types.empty()) {
            return Error(nullptr, "Type cannot store " + std::to_string(val));
        }
        return std::nullopt;
    }
    std::optional<Error> Domain::merge_intersect(Domain&& dmn, TypeCheckerState* state)
    {
        return add_and_intersect(std::move(dmn.concrete_types), state);
    }
    std::optional<Error> Domain::equal_constrain(Type other)
    {
        if (concrete_types.types.empty()) {
            concrete_types.types.push_back(std::move(other));
            return std::nullopt;
        }
        // other must not be concrete
        // so instead of replacing the entire domain we remove elements that can't match
        // if we empty the domain then there's an error
        std::erase_if(concrete_types.types, [this, &other](auto& elem) {
            return !can_match(other, elem);
            });
        if (!concrete_types.types.empty())
        {
            return std::nullopt;
        }
        else {
            return Error(nullptr, "Type use contradiction");
        }
    }
    bool Domain::is_solved()
    {
        return concrete_types.types.size() == 1;
    }
    Type Domain::get_solution()
    {
        return concrete_types.types[0];
    }
} // namespace Yoyo
