#include "tree_cloner.h"

#include <memory>

#include "ast_node.h"
#include "borrow_checker.h"

namespace Yoyo {
std::unique_ptr<Expression> ExpressionTreeCloner::operator()(
    IntegerLiteral* expr) {
    return std::make_unique<IntegerLiteral>(expr->text);
}

std::unique_ptr<Expression> ExpressionTreeCloner::operator()(
    BooleanLiteral* lit) {
    return std::make_unique<BooleanLiteral>(lit->token);
}

std::unique_ptr<Expression> ExpressionTreeCloner::operator()(
    TupleLiteral* lit) {
    std::vector<std::unique_ptr<Expression>> children;
    auto result = std::make_unique<TupleLiteral>(std::move(children));
    for (auto& child : lit->elements)
        result->elements.emplace_back(copy_expr(child, result.get()));
    return result;
}

std::unique_ptr<Expression> ExpressionTreeCloner::operator()(
    ArrayLiteral* lit) {
    if (std::holds_alternative<std::pair<std::unique_ptr<Expression>,
                                         std::unique_ptr<Expression>>>(
            lit->elements)) {
        auto& notation =
            std::get<std::pair<std::unique_ptr<Expression>,
                               std::unique_ptr<Expression>>>(lit->elements);
        std::remove_reference_t<decltype(notation)> clone;
        clone.first = copy_expr(notation.first, nullptr);
        clone.second = copy_expr(notation.second, nullptr);
        auto result = std::make_unique<ArrayLiteral>(std::move(clone));
        auto& result_inside = std::get<decltype(clone)>(result->elements);
        result_inside.first->parent = result.get();
        result_inside.second->parent = result.get();
        return result;
    }
    std::vector<std::unique_ptr<Expression>> children_og;
    auto result = std::make_unique<ArrayLiteral>(std::move(children_og));
    auto& children = std::get<decltype(children_og)>(result->elements);
    for (auto& child : std::get<decltype(children_og)>(lit->elements))
        children.emplace_back(copy_expr(child, result.get()));
    return result;
}

std::unique_ptr<Expression> ExpressionTreeCloner::operator()(RealLiteral* lit) {
    return std::make_unique<RealLiteral>(lit->token);
}

std::unique_ptr<Expression> ExpressionTreeCloner::operator()(
    StringLiteral* lit) {
    decltype(lit->literal) children;
    auto string = std::make_unique<StringLiteral>(std::move(children));
    for (auto& child : lit->literal)
        if (std::holds_alternative<std::string>(child))
            children.emplace_back(std::get<0>(child));
        else
            children.emplace_back(copy_expr(std::get<1>(child), string.get()));
    string->literal = std::move(children);
    return string;
}

std::unique_ptr<Expression> ExpressionTreeCloner::operator()(
    NameExpression* nm) {
    return std::make_unique<NameExpression>(nm->text);
}

std::unique_ptr<Expression> ExpressionTreeCloner::operator()(
    GenericNameExpression* nm) {
    return std::make_unique<GenericNameExpression>(*nm);
}

std::unique_ptr<Expression> ExpressionTreeCloner::operator()(
    PrefixOperation* op) {
    auto ret_val = std::make_unique<PrefixOperation>(
        op->op, copy_expr(op->operand, nullptr));
    ret_val->operand->parent = ret_val.get();
    return ret_val;
}

std::unique_ptr<Expression> ExpressionTreeCloner::operator()(
    BinaryOperation* op) {
    auto ret_val = std::make_unique<BinaryOperation>(
        op->op, copy_expr(op->lhs, nullptr), copy_expr(op->rhs, nullptr));
    ret_val->lhs->parent = ret_val->rhs->parent = ret_val.get();
    return ret_val;
}

std::unique_ptr<Expression> ExpressionTreeCloner::operator()(
    GroupingExpression* expr) {
    auto ret_val =
        std::make_unique<GroupingExpression>(copy_expr(expr->expr, nullptr));
    ret_val->expr->parent = ret_val.get();
    return ret_val;
}

std::unique_ptr<Expression> ExpressionTreeCloner::operator()(
    LogicalOperation* op) {
    auto ret_val = std::make_unique<LogicalOperation>(
        op->op, copy_expr(op->lhs, nullptr), copy_expr(op->rhs, nullptr));
    ret_val->lhs->parent = ret_val->rhs->parent = ret_val.get();
    return ret_val;
}

std::unique_ptr<Expression> ExpressionTreeCloner::operator()(
    PostfixOperation* op) {
    auto ret_val = std::make_unique<PostfixOperation>(
        op->op, copy_expr(op->operand, nullptr));
    ret_val->operand->parent = ret_val.get();
    return ret_val;
}

std::unique_ptr<Expression> ExpressionTreeCloner::operator()(
    CallOperation* op) {
    auto ret_val = std::make_unique<CallOperation>(
        copy_expr(op->callee, nullptr),
        std::vector<std::unique_ptr<Expression>>{});
    ret_val->callee->parent = ret_val.get();
    for (auto& child : op->arguments)
        ret_val->arguments.emplace_back(copy_expr(child, ret_val.get()));
    return ret_val;
}

std::unique_ptr<Expression> ExpressionTreeCloner::operator()(
    SubscriptOperation* op) {
    auto ret_val = std::make_unique<SubscriptOperation>(
        copy_expr(op->object, nullptr), copy_expr(op->index, nullptr));
    ret_val->object->parent = ret_val.get();
    ret_val->index->parent = ret_val.get();
    return ret_val;
}

std::unique_ptr<Expression> ExpressionTreeCloner::operator()(
    LambdaExpression* lmbd) {
    return std::make_unique<LambdaExpression>(
        lmbd->captures, lmbd->sig,
        StatementTreeCloner::copy_stat(lmbd->body, nullptr));
}

std::unique_ptr<Expression> ExpressionTreeCloner::operator()(
    ScopeOperation* other) {
    return std::make_unique<ScopeOperation>(other->type);
}

std::unique_ptr<Expression> ExpressionTreeCloner::operator()(
    ObjectLiteral* obj) {
    auto ret_val = std::make_unique<ObjectLiteral>(
        obj->t, std::unordered_map<std::string, std::unique_ptr<Expression>>{});
    for (auto& [name, expr] : obj->values)
        ret_val->values.emplace(name, copy_expr(expr, ret_val.get()));
    return ret_val;
}

std::unique_ptr<Expression> ExpressionTreeCloner::operator()(NullLiteral* nl) {
    return std::make_unique<NullLiteral>();
}

std::unique_ptr<Expression> ExpressionTreeCloner::operator()(
    AsExpression* expr) {
    auto ret_val = std::make_unique<AsExpression>(
        copy_expr(expr->expr, nullptr), expr->dest);
    ret_val->expr->parent = ret_val.get();
    return ret_val;
}

std::unique_ptr<Expression> ExpressionTreeCloner::operator()(
    GCNewExpression* expr) {
    return std::make_unique<GCNewExpression>(
        copy_expr(expr->target_expression, nullptr));
}
std::unique_ptr<Expression> ExpressionTreeCloner::operator()(
    MacroInvocation* invc) {
    auto new_name = copy_expr(invc->macro_name, nullptr);
    auto right = invc->right ? copy_expr(invc->right, nullptr) : nullptr;
    if (std::holds_alternative<std::unique_ptr<Expression>>(invc->left)) {
        auto left = copy_expr(std::get<std::unique_ptr<Expression>>(invc->left),
                              nullptr);
        auto ret_val = std::make_unique<MacroInvocation>(
            std::move(new_name), std::move(left), std::move(right));
        if (ret_val->right) ret_val->right->parent = ret_val.get();
        ret_val->macro_name->parent = ret_val.get();
        std::get<0>(ret_val->left)->parent = ret_val.get();
        return ret_val;
    } else {
        // TODO: handle clonging for other cases
        debugbreak();
        return nullptr;
    }
}
std::unique_ptr<Expression> ExpressionTreeCloner::operator()(
    SpawnExpression* exr) {
    auto ret =
        std::make_unique<SpawnExpression>(copy_expr(exr->call_expr, nullptr));
    ret->call_expr->parent = ret.get();
    return ret;
}
std::unique_ptr<Expression> ExpressionTreeCloner::operator()(
    TryExpression* tr) {
    auto ret =
        std::make_unique<TryExpression>(copy_expr(tr->expression, nullptr));
    ret->expression->parent = ret.get();
    return ret;
}
std::unique_ptr<Expression> ExpressionTreeCloner::operator()(CharLiteral* lit) {
    auto lt = std::make_unique<CharLiteral>();
    lt->value = lit->value;
    return lt;
}

std::unique_ptr<Expression> ExpressionTreeCloner::copy_expr(Expression* e,
                                                            ASTNode* parent) {
    if (e == nullptr) return nullptr;
    auto ce = std::visit(ExpressionTreeCloner{}, e->toVariant());
    ce->beg = e->beg;
    ce->end = e->end;
    ce->parent = parent;
    ce->evaluated_type = e->evaluated_type;
    return ce;
}

std::unique_ptr<Expression> ExpressionTreeCloner::copy_expr(
    std::unique_ptr<Expression>& e, ASTNode* parent) {
    return copy_expr(e.get(), parent);
}

std::unique_ptr<Statement> StatementTreeCloner::operator()(
    FunctionDeclaration* decl) {
    auto ret = std::make_unique<FunctionDeclaration>(
        decl->name, decl->signature, copy_stat(decl->body, nullptr));
    ret->body->parent = ret.get();
    return ret;
}

std::unique_ptr<Statement> StatementTreeCloner::operator()(
    ClassDeclaration* decl) {
    std::vector<std::unique_ptr<Statement>> new_methods;
    for (auto& method : decl->stats) {
        new_methods.emplace_back(copy_stat(method, nullptr));
    }
    std::vector<InterfaceImplementation> new_impls;
    for (auto& impl : decl->impls) {
        decltype(InterfaceImplementation::methods) mth;
        for (auto& method : impl.methods) {
            auto ptr = copy_stat(method.get(), nullptr);
            mth.emplace_back(
                reinterpret_cast<FunctionDeclaration*>(ptr.release()));
        }
        new_impls.emplace_back(impl.impl_for, impl.location, std::move(mth));
    }
    auto ret = std::make_unique<ClassDeclaration>(
        Token{.text = decl->name}, decl->vars, std::move(new_methods),
        decl->ownership, std::move(new_impls), decl->domains);
    for (auto& m : ret->stats) m->parent = ret.get();
    for (auto& i : ret->impls)
        for (auto& m : i.methods) m->parent = ret.get();
    return ret;
}
std::unique_ptr<Statement> StatementTreeCloner::operator()(
    CImportDeclaration* dcl) {
    return std::make_unique<CImportDeclaration>(dcl->function_name);
}
std::unique_ptr<Statement> StatementTreeCloner::operator()(
    VariableDeclaration* decl) {
    auto ret = std::make_unique<VariableDeclaration>(
        decl->identifier, decl->type, copy_expr(decl->initializer, nullptr),
        decl->is_mut);
    if (ret->initializer) ret->initializer->parent = ret.get();
    return ret;
}

std::unique_ptr<Expression> ExpressionTreeCloner::operator()(
    IfExpression* stat) {
    auto ret =
        std::make_unique<IfExpression>(copy_expr(stat->condition, nullptr),
                                       copy_expr(stat->then_expr, nullptr),
                                       copy_expr(stat->else_expr, nullptr));
    ret->condition->parent = ret->then_expr->parent = ret.get();
    if (ret->else_expr) ret->else_expr->parent = ret.get();
    return ret;
}
std::unique_ptr<Statement> StatementTreeCloner::operator()(BreakStatement*) {
    return std::make_unique<BreakStatement>();
}
std::unique_ptr<Statement> StatementTreeCloner::operator()(ContinueStatement*) {
    return std::make_unique<ContinueStatement>();
}
std::unique_ptr<Statement> StatementTreeCloner::operator()(
    WhileStatement* stat) {
    auto ret = std::make_unique<WhileStatement>(
        copy_expr(stat->condition, nullptr), copy_stat(stat->body, nullptr));
    ret->condition->parent = ret->body->parent = ret.get();
    return ret;
}

std::unique_ptr<Statement> StatementTreeCloner::operator()(ForStatement* stat) {
    auto ret = std::make_unique<ForStatement>(
        stat->names, copy_expr(stat->iterable, nullptr),
        copy_stat(stat->body, nullptr));
    ret->iterable->parent = ret->body->parent = nullptr;
}

std::unique_ptr<Expression> ExpressionTreeCloner::operator()(
    BlockExpression* stat) {
    auto ret = std::make_unique<BlockExpression>(
        std::vector<std::unique_ptr<Statement>>{},
        copy_expr(stat->expr, nullptr));
    if (stat->expr) ret->expr->parent = ret.get();
    for (auto& stat : stat->statements)
        ret->statements.emplace_back(
            StatementTreeCloner::copy_stat(stat, ret.get()));
    return ret;
}

std::unique_ptr<Statement> StatementTreeCloner::operator()(
    ReturnStatement* stat) {
    auto ret = std::make_unique<ReturnStatement>(nullptr);
    if (stat->expression)
        ret->expression = copy_expr(stat->expression, ret.get());
    return ret;
}

std::unique_ptr<Statement> StatementTreeCloner::operator()(
    ExpressionStatement* stat) {
    auto ret = std::make_unique<ExpressionStatement>(
        copy_expr(stat->expression, nullptr));
    ret->expression->parent = ret.get();
    return ret;
}

std::unique_ptr<Statement> StatementTreeCloner::operator()(
    EnumDeclaration* decl) {
    auto ret = std::make_unique<EnumDeclaration>(
        decl->identifier, decl->values,
        std::vector<std::unique_ptr<Statement>>{});
    for (auto& stat : decl->stats)
        ret->stats.emplace_back(copy_stat(stat, ret.get()));
}

std::unique_ptr<Statement> StatementTreeCloner::operator()(ModuleImport* imp) {
    return std::make_unique<ModuleImport>(*imp);
}

std::unique_ptr<Expression> ExpressionTreeCloner::operator()(
    ConditionalExtraction* stat) {
    auto ret = std::make_unique<ConditionalExtraction>(
        stat->captured_name, stat->then_capture_tp,
        copy_expr(stat->condition, nullptr), copy_expr(stat->body, nullptr),
        copy_expr(stat->else_body, nullptr), stat->else_capture,
        stat->else_capture_tp);
    ret->condition->parent = ret->body->parent = ret.get();
    if (ret->else_body) ret->else_body->parent = ret.get();
    return ret;
}

std::unique_ptr<Statement> StatementTreeCloner::operator()(
    WithStatement* stat) {
    auto ret = std::make_unique<WithStatement>(
        stat->name, copy_expr(stat->expression, nullptr),
        copy_stat(stat->body, nullptr));
    ret->expression->parent = ret->body->parent = ret.get();
    return ret;
}

std::unique_ptr<Statement> StatementTreeCloner::operator()(
    OperatorOverload* ov) {
    auto ret = std::make_unique<OperatorOverload>(
        ov->tok, ov->signature, copy_stat(ov->body, nullptr), ov->clause);
    ret->body->parent = ret.get();
    return ret;
}

std::unique_ptr<Statement> StatementTreeCloner::operator()(
    GenericFunctionDeclaration* decl) {
    auto ret = std::make_unique<GenericFunctionDeclaration>(
        decl->clause, FunctionDeclaration(decl->name, decl->signature,
                                          copy_stat(decl->body, nullptr)));
    ret->body->parent = ret.get();
    return ret;
}

std::unique_ptr<Statement> StatementTreeCloner::operator()(
    GenericAliasDeclaration* decl) {
    return std::make_unique<GenericAliasDeclaration>(decl->name, decl->type,
                                                     decl->clause);
}

std::unique_ptr<Statement> StatementTreeCloner::operator()(
    AliasDeclaration* decl) {
    return std::make_unique<AliasDeclaration>(decl->name, decl->type);
}
std::unique_ptr<Statement> StatementTreeCloner::operator()(
    InterfaceDeclaration* stat) {
    auto final = std::make_unique<InterfaceDeclaration>();
    final->name = stat->name;
    for (auto& method : stat->methods) {
        auto mth = std::make_unique<FunctionDeclaration>(
            method->name, method->signature, nullptr);
        final->methods.emplace_back(
            reinterpret_cast<FunctionDeclaration*>(mth.release()));
    }
    return final;
}
std::unique_ptr<Statement> StatementTreeCloner::operator()(
    GenericInterfaceDeclaration* stat) {
    auto final = std::make_unique<GenericInterfaceDeclaration>();
    final->name = stat->name;
    for (auto& method : stat->methods) {
        auto mth = std::make_unique<FunctionDeclaration>(
            method->name, method->signature, nullptr);
        final->methods.emplace_back(
            reinterpret_cast<FunctionDeclaration*>(mth.release()));
    }
    final->clause = stat->clause;
    return final;
}
std::unique_ptr<Statement> StatementTreeCloner::operator()(
    UnionDeclaration* decl) {
    std::vector<std::unique_ptr<Statement>> stats;
    for (auto& stat : decl->sub_stats) {
        stats.push_back(copy_stat(stat, nullptr));
    }
    auto ret = std::make_unique<UnionDeclaration>(decl->name, decl->fields,
                                                  std::move(stats), decl->domains);
    for (auto& s : ret->sub_stats) s->parent = ret.get();
    return ret;
}
std::unique_ptr<Statement> StatementTreeCloner::operator()(
    MacroDeclaration* decl) {
    auto new_decl = std::make_unique<MacroDeclaration>();
    new_decl->body = copy_stat(decl->body, new_decl.get());
    new_decl->first_param = decl->first_param;
    new_decl->name = decl->name;
    return new_decl;
}
std::unique_ptr<Statement> StatementTreeCloner::operator()(
    UsingStatement* decl) {
    return std::make_unique<UsingStatement>(decl->content);
}
std::unique_ptr<Statement> StatementTreeCloner::operator()(
    GenericClassDeclaration* decl) {
    std::vector<std::unique_ptr<Statement>> new_methods;
    for (auto& stt : decl->stats) {
        new_methods.emplace_back(copy_stat(stt, nullptr));
    }
    std::vector<InterfaceImplementation> new_impls;
    for (auto& impl : decl->impls) {
        decltype(InterfaceImplementation::methods) mth;
        for (auto& method : impl.methods) {
            auto ptr = copy_stat(method.get(), nullptr);
            mth.emplace_back(
                reinterpret_cast<FunctionDeclaration*>(ptr.release()));
        }
        new_impls.emplace_back(impl.impl_for, impl.location, std::move(mth));
    }
    auto ret = std::make_unique<GenericClassDeclaration>(
        Token{.text = decl->name}, decl->vars, std::move(new_methods),
        decl->ownership, std::move(new_impls), decl->clause, decl->domains);
    for (auto& m : ret->stats) m->parent = ret.get();
    for (auto& i : ret->impls)
        for (auto& m : i.methods) m->parent = ret.get();
    return ret;
}
std::unique_ptr<Statement> StatementTreeCloner::operator()(
    ConstantDeclaration* decl) {
    auto ret = std::make_unique<ConstantDeclaration>(
        decl->name, decl->type, copy_expr(decl->expr, nullptr));
    ret->expr->parent = ret.get();
}
std::unique_ptr<Statement> StatementTreeCloner::copy_stat(Statement* s,
                                                          ASTNode* parent) {
    if (s == nullptr) return nullptr;
    auto ns = std::visit(StatementTreeCloner{}, s->toVariant());
    ns->beg = s->beg;
    ns->end = s->end;
    ns->parent = parent;
    return ns;
}

std::unique_ptr<Statement> StatementTreeCloner::copy_stat(
    std::unique_ptr<Statement>& s, ASTNode* parent) {
    return copy_stat(s.get(), parent);
}

std::unique_ptr<Expression> StatementTreeCloner::copy_expr(Expression* e,
                                                           ASTNode* parent) {
    return ExpressionTreeCloner::copy_expr(e, parent);
}
std::unique_ptr<Expression> StatementTreeCloner::copy_expr(
    std::unique_ptr<Expression>& e, ASTNode* parent) {
    return ExpressionTreeCloner::copy_expr(e, parent);
}
}  // namespace Yoyo
