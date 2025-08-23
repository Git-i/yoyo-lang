#include "ir_gen.h"
#include "overload_details.h"
#include "overload_resolve.h"
#include "type_checker.h"
#include <ranges>
#define core_module irgen->module->engine->modules.at("core").get()
namespace Yoyo
{
    static Statement* get_type_stat(const Type& type, const std::string& hash, ModuleBase* const md) {
        if (auto [hsh, decl] = md->findGenericClass(hash, type.name); decl) {
            return decl;
        }
        else if (auto [this_hash, fn] = md->findGenericFn(hash, type.name); fn) {
            return fn;
        }
        else if (auto [hsh, itf] = md->findGenericInterface(hash, type.name); itf) {
            return itf;
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
    Statement* get_next_stat(Statement* stat, const std::string& name) {
        if (auto cls = dynamic_cast<ClassDeclaration*>(stat)) { // Class and generic class declarations
            auto it = std::ranges::find_if(cls->stats, [&name](auto& elem) {
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
            auto it = std::ranges::find_if(enm->stats, [&name](auto& elem) {
                return std::visit([](auto* arg) { return get_decl_name(arg); }, elem->toVariant()) == name;
                });
            if (it != enm->stats.end()) {
                return it->get();
            }
        }
        // interfaces cannot have sub-definitions (i.e cant define struct within interface)
        else if (auto unn = dynamic_cast<UnionDeclaration*>(stat)) {
            auto it = std::ranges::find_if(unn->sub_stats, [&name](auto& elem) {
                return std::visit([](auto* arg) { return get_decl_name(arg); }, elem->toVariant()) == name;
                });
            if (it != unn->sub_stats.end()) {
                return it->get();
            }
        }
        // within function body
        else if (auto blk = dynamic_cast<BlockStatement*>(stat)) {
            for (auto& stat : blk->statements) {
                auto stat_name = std::visit([](auto* arg) { return get_decl_name(arg); }, stat->toVariant());
                if (stat_name == name) {
                    return stat.get();
                }
                // we can look deeper as this statement is not a declaration
                else if (stat_name == "__not_a_declaration__") {
                    auto res = get_next_stat(stat.get(), name);
                    if (res) return res;
                }
            }
        }
        else if (auto whl = dynamic_cast<WhileStatement*>(stat))
            return get_next_stat(whl->body.get(), name);
        else if (auto fr = dynamic_cast<ForStatement*>(stat))
            return get_next_stat(fr->body.get(), name);
        else if (auto if_stat = dynamic_cast<IfStatement*>(stat)) {
            auto exists = get_next_stat(if_stat->then_stat.get(), name);
            if (exists) return exists;
            if (if_stat->else_stat) return get_next_stat(if_stat->else_stat.get(), name);
        }
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
    Statement* normalize_type(Type& tp, TypeCheckerState* stt, IRGenerator* irgen, std::unordered_map<std::string, Type>);
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
    Statement* normalize_type(Type& tp, TypeCheckerState* stt, IRGenerator* irgen, std::unordered_map<std::string, Type> generic_instantiations) {
        if (tp.name == "_") {
            tp = stt->new_type_var();
            return nullptr;
        }
        if (generic_instantiations.contains(tp.name)) {
            tp = generic_instantiations[tp.name];
            return nullptr;
        }
        ModuleBase* md = irgen->module;
        std::string hash = irgen->block_hash;
        std::string second_to_last = "";
        auto iterator = UnsaturatedTypeIterator(tp);
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

                auto hsh = tp.module->hashOf(tp.block_hash, tp.name);
                if (hsh) tp.block_hash = std::move(hsh).value();
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
        }
        for (auto& sub : tp.subtypes) normalize_type(sub, stt, irgen, generic_instantiations);
        
        return current_stat;
    }
    void TypeChecker::operator()(VariableDeclaration* decl)
    {
        Type tp;
        if (decl->type) {
            normalize_type(*decl->type, state, irgen, {});
            tp = *decl->type;
        }
        if (decl->initializer) {
            tp = std::visit(new_target(decl->type), decl->initializer->toVariant());
        }
        decl->type = tp;
        state->create_variable(std::string(decl->identifier.text), tp);
    }
    void TypeChecker::operator()(IfStatement* stat)
	{
        state->add_constraint(EqualConstraint{ 
            std::visit(targetless(), stat->condition->toVariant()), 
            Type{.name = "bool", .module = core_module} 
        });
        std::visit(*this, stat->then_stat->toVariant());
        if (stat->else_stat) std::visit(*this, stat->else_stat->toVariant());
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
                Type{.name = "Iterator",  .subtypes = {item}, .module = core_module, .block_hash = "core::",}
            });

        // new variable block
        state->push_variable_block();
        state->create_variable(std::string(stat->names[0].text), std::move(item));
        std::visit(*this, stat->body->toVariant());
        state->pop_variable_block();
    }
    void TypeChecker::operator()(BlockStatement* stat)
    {
        state->push_variable_block();
        for (auto& stt : stat->statements) {
            std::visit(*this, stt->toVariant());
        }
        state->pop_variable_block();
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
        state->push_variable_block();
        state->create_variable(stat->captured_name, obj_ty);
        std::visit(*this, stat->body->toVariant());
        state->pop_variable_block();

        if (stat->else_body) std::visit(*this, stat->else_body->toVariant());
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
    FunctionType TypeChecker::operator()(BooleanLiteral*) const {
        return Type{ "bool" };
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
        /*TODO*/ 
        return Type{};
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
            tp.block_hash = name_prefix + ex->text;
            std::ranges::copy(fn->sig.parameters | std::views::transform([](auto& e) { return e.type; }), std::back_inserter(tp.subtypes));
            tp.subtypes.push_back(fn->sig.returnType);
            ex->evaluated_type = tp;
            return tp;
        }
        if (auto [name_prefix, fn] = module->findGenericFn(hash, ex->text); fn)
        {
            auto ty = FunctionType(fn->signature, false);
            ty.name = "__generic_fn" + ex->text;
            ty.module = irgen->module;
            ty.block_hash = name_prefix;
            return { std::move(ty) };
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
            op->evaluated_type = Type{ .name = "__ref", .subtypes = { inner }, .module = core_module };
            state->add_constraint(IsNotReferenceConstraint{ std::move(inner) });
            break;
        }
        case RefMut:
        {
            auto inner = std::visit(targetless(), op->operand->toVariant());
            op->evaluated_type = Type{ .name = "__ref_mut", .subtypes = { inner }, .module = core_module };
            state->add_constraint(IsNotReferenceConstraint{ std::move(inner) });
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
            state->add_constraint(BinaryOperableConstraint{ left, right, expr->evaluated_type, tk });
            // constrain the return type for comparison operators
            // == and != support any return type
            if (expr->op.type == DoubleEqual || expr->op.type == BangEqual) {
                expr->evaluated_type = Type{ .name = "bool",.module = core_module };
            }
            else if (expr->op.type != Spaceship) {
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
        case Dot: // TODO;
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
        // state->add_constraint();
        return Type{};
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
            generic_instantiations[clause->types[i]] = subtypes.emplace_back(state->new_type_var());
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
                scp->evaluated_type.block_hash = hash;
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
            if (auto [actual_hash, unn] = md->findUnion(hash, second_to_last); unn)
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
            if (auto [name, fn] = md->findFunction(hash, last.name); fn)
            {
                if (md != irgen->module && fn->is_private()) {
                    irgen->error(Error(scp, "The function type " + name + fn->name + " is private"));
                    return Type{ "__error_type" };
                }

                irgen->block_hash.swap(hash);
                irgen->saturateSignature(fn->sig, md);
                irgen->block_hash.swap(hash);

                scp->evaluated_type.block_hash = hash + last.name;
                scp->evaluated_type.name = "__fn";
                scp->evaluated_type.module = md;
                std::ranges::copy(fn->sig.parameters | std::views::transform([](auto& elem) { return elem.type; }), 
                    std::back_inserter(scp->evaluated_type.subtypes));
                scp->evaluated_type.subtypes.push_back(fn->sig.returnType);
            }
            if (auto [name, c] = md->findConst(hash, last.name); c)
            {
                irgen->block_hash.swap(hash);
                std::get<0>(*c).saturate(md, irgen);
                irgen->block_hash.swap(hash);
                return { std::get<0>(*c) };
            }
        }
        return scp->evaluated_type;
    }
    FunctionType TypeChecker::operator()(ObjectLiteral* lit) const {
        auto stat = normalize_type(lit->t, state, irgen, {});
        return Type{};
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

    TypeChecker TypeChecker::new_target(std::optional<Type> tgt) const
    {
        return TypeChecker{ tgt, irgen, state };
    }

    TypeChecker TypeChecker::targetless() const
    {
        return TypeChecker{ std::nullopt, irgen, state };
    }

    static bool is_type_variable(const Type& tp) {
        return tp.name[0] == '?';
    }
    static bool has_type_variable(const Type& tp) {
        if (is_type_variable(tp)) return true;
        if (tp.block_hash.find('?') != std::string::npos) 
            return true;
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
            if(auto error = domain->add_and_intersect(std::move(grp)))
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
            return false;
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
            if (auto error = domain->add_and_intersect(std::move(grp)))
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
    std::optional<Error> generic_match(Type tp1, Type tp2, Expression* loc, ConstraintSolver* solver) {
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
                    solver->add_new_constraint(EqualConstraint{ this1.subtypes[i], this2.subtypes[i], loc });
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
                solver->add_new_constraint(EqualConstraint{ end1.subtypes[i], end2.subtypes[i], loc });
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
                if(auto err = generic_match(std::move(type1), std::move(type2), con.expr, this))
                    irgen->error(*err);
            }
            // Something::<?1>::Type and concrete <case 5>
            else {
                // generic match can also work here probably
                if (auto err = generic_match(std::move(type1), std::move(type2), con.expr, this))
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
                if (auto err = generic_match(std::move(type1), std::move(type2), con.expr, this))
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
        if (is_type_variable(callee)) {
            return false;
        }
        if (callee.name != "__fn") {
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

        if (callee.name != "__fn") {
            irgen->error(Error(con.expr, "Type is not callable"));
            return true;
        }

        if ((callee.subtypes.size() - 1) <= con.arg_no) {
            irgen->error(Error(con.expr, "Too many function args"));
            return true;
        }

        add_new_constraint(EqualConstraint{ callee.subtypes[con.arg_no], arg, con.expr });
        return true;
    }
    bool ConstraintSolver::operator()(IsReturnOfConstraint& con)
    {
        auto callee = state->best_repr(con.function);
        auto arg = state->best_repr(con.type);

        if (is_type_variable(callee)) {
            return false;
        }

        if (callee.name != "__fn") {
            irgen->error(Error(con.expr, "Type is not callable"));
            return true;
        }

        add_new_constraint(EqualConstraint{ callee.subtypes.back(), arg, con.expr });
        return true;
    }
    bool ConstraintSolver::operator()(ImplInterfaceConstraint& con)
    {
        // we use this to restrict the domain of a type
        // it can also technically work for type matching but gets iffy with enable_if
        auto type = state->best_repr(con.type);
        auto intf = state->best_repr(con.interface);

        // intf cannot be a type variable

        ClassDeclaration* decl = nullptr;
        
        if (is_type_variable(type)) {
            state->get_type_domain(type); // constrain to impl interface
        }
        else if (has_type_variable(type)) {
            decl = dynamic_cast<ClassDeclaration*>(normalize_type(type, state, irgen, {}));
        }
        else {
            decl = type.get_decl_if_class(irgen);
        }

        if (decl) {
            bool did = false;
            for (auto& impl : decl->impls) {
                if (impl.impl_for.name == intf.name) {
                    std::swap(irgen->block_hash, type.block_hash);
                    auto as_sat = impl.impl_for.saturated(type.module, irgen);
                    std::swap(irgen->block_hash, type.block_hash);

                    if (as_sat.block_hash == intf.block_hash) {
                        did = true;
                        add_new_constraint(EqualConstraint{ std::move(as_sat), intf });
                        break;
                    }
                }
            }
            if (!did) {
                irgen->error(Error(con.expr, "Type does not implement interface"));
            }
        }
        else {
            // TODO check for automatic interfaces
            irgen->error(Error(con.expr, "Type does not implement interface"));
        }
        return true;
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
                other.reference_to()
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
        if(is_type_variable(left)) {
            if(is_type_variable(right)) {
                return false;
            } else if(has_type_variable(right)) {
                return false;
            } else {
                // constrain left to all possible types
                // right has a binary operation with
                auto grp = Domain::Group{};
                for(auto&[hash, detail] : 
                    right.module->overloads.binary_details_for(con.op) | 
                    std::views::filter([&right, this](auto& elem) { 
                        std::swap(elem.first, irgen->block_hash);
                        elem.second.left.saturate(right.module, irgen);
                        elem.second.right.saturate(right.module, irgen);
                        std::swap(elem.first, irgen->block_hash);
                        return elem.second.right.is_equal(right); 
                    }))
                {
                    auto obj = detail.left;
                    grp.add_type(std::move(obj));
                }
                state->get_type_domain(left)->add_and_intersect(std::move(grp));
            }
        } else if (has_type_variable(left)) {
            if(is_type_variable(right)) {
                return false;
            } else if(has_type_variable(right)) {
                // this can actually work
                return false;
            } else {
                // this can also work
                return false;
                /*auto grp = Domain::Group{};
                for(auto&[hash, detail] : 
                    right.module->overloads.binary_details_for(con.op) | 
                    std::views::filter([&right, this](auto& elem) { 
                        std::swap(elem.first, irgen->block_hash);
                        elem.second.left.saturate(right.module, irgen);
                        elem.second.right.saturate(right.module, irgen);
                        std::swap(elem.first, irgen->block_hash);
                        return elem.second.right.is_equal(right); 
                    }))
                {
                    if (can_match(detail.left, left)) {
                        auto obj = detail.left;
                        grp.add_type(std::move(obj));
                    }
                }
                state->get_type_domain(left)->add_and_intersect(std::move(grp));*/
            }
        } else {
            //left is concrete
            if(is_type_variable(right)) {
                auto grp = Domain::Group{};
                for (auto& [hash, detail] :
                    right.module->overloads.binary_details_for(con.op) |
                    std::views::filter([&left, this](auto& elem) {
                        std::swap(elem.first, irgen->block_hash);
                        elem.second.left.saturate(left.module, irgen);
                        elem.second.right.saturate(left.module, irgen);
                        std::swap(elem.first, irgen->block_hash);
                        return elem.second.left.is_equal(left);
                        }))
                {
                    auto obj = detail.right;
                    grp.add_type(std::move(obj));
                }
                state->get_type_domain(right)->add_and_intersect(std::move(grp));
            } else if(has_type_variable(right)) {
                // this can also work
                return false;
            } else { // both are concrete
                auto[block, dets] = resolveBin(left, right, con.op, irgen);
                if (!dets) {
                    irgen->error(Error(con.expr, "No operator exists between these 2 types"));
                }
            }
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
    uint32_t UnificationTable::unite(uint32_t id, uint32_t id2, IRGenerator *irgen) {
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
                domains[finda].merge_intersect(std::move(domains[findb])))
          irgen->error(*produced_error);
        domains.erase(findb);
        return finda;
    }
    void TypeCheckerState::resolve_function(FunctionDeclaration* decl, IRGenerator* irgen)
    {
        push_variable_block();
        return_type = decl->signature.returnType;
        // generate constraints
        std::visit(TypeChecker{std::nullopt, irgen, this}, decl->body->toVariant());
        // solve constraints
        ConstraintSolver sv{ false, irgen, this };
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
            // TODO actual reporting
            irgen->error(Error(decl, "Not enough info to resolve types"));
        }
        // substitute types in every ast node??
        // maybe we can do this in the borrow checker ?
        pop_variable_block();
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

        if (type.name != "__opt" && type.name != "__conv_result_ref" && type.name != "__conv_result_val") {
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
        // __conv_result_ref::<T> extracts to T (special internal type returned by variant conversion)

        auto target = state->best_repr(con.dst);
        auto type = state->best_repr(con.type);

        if (is_type_variable(type)) {
            return false; // we can't know
        }

        if (type.name != "__opt" && type.name != "__conv_result_ref") {
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
                state->get_type_domain(dst)->add_and_intersect(std::move(gp));
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
        constraints.push_back(std::move(c));
    }
    void TypeCheckerState::unify_types(const Type& t1, const Type& t2, IRGenerator* irgen)
    {
        if (is_type_variable(t1) && is_type_variable(t2)) {
            tbl.unite(tbl.type_to_id(t1), tbl.type_to_id(t2), irgen);
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
                return domain->get_solution();
            }
            return tbl.id_to_type(actual_id);
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
            return final_t;
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
    std::optional<Error> Domain::add_and_intersect(Group&& grp)
    {
        if (concrete_types.types.empty()) {
            concrete_types = grp;
            return std::nullopt;
        }
        std::vector<Type> out;
        std::ranges::set_intersection(concrete_types.types, grp.types, std::back_inserter(out));
        concrete_types.types = std::move(out);
        if (concrete_types.types.empty()) {
            return Error(nullptr, "Intersection error");
        }
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
    std::optional<Error> Domain::merge_intersect(Domain&& dmn)
    {
        return add_and_intersect(std::move(dmn.concrete_types));
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
