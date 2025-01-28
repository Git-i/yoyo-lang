#include "tree_cloner.h"

namespace Yoyo
{
    std::unique_ptr<Expression> ExpressionTreeCloner::operator()(IntegerLiteral* expr)
    {
        return std::make_unique<IntegerLiteral>(expr->text);
    }

    std::unique_ptr<Expression> ExpressionTreeCloner::operator()(BooleanLiteral* lit)
    {
        return std::make_unique<BooleanLiteral>(lit->token);
    }

    std::unique_ptr<Expression> ExpressionTreeCloner::operator()(TupleLiteral* lit)
    {
        std::vector<std::unique_ptr<Expression>> children;
        for(auto& child : lit->elements)
            children.emplace_back(copy_expr(child));
        return std::make_unique<TupleLiteral>(std::move(children));
    }

    std::unique_ptr<Expression> ExpressionTreeCloner::operator()(ArrayLiteral* lit)
    {
        std::vector<std::unique_ptr<Expression>> children;
        for(auto& child : lit->elements)
            children.emplace_back(copy_expr(child));
        return std::make_unique<ArrayLiteral>(std::move(children));
    }

    std::unique_ptr<Expression> ExpressionTreeCloner::operator()(RealLiteral* lit)
    {
        return std::make_unique<RealLiteral>(lit->token);
    }

    std::unique_ptr<Expression> ExpressionTreeCloner::operator()(StringLiteral* lit)
    {
        decltype(lit->literal) children;
        for(auto& child : lit->literal)
            if(std::holds_alternative<std::string>(child)) children.emplace_back(std::get<0>(child));
            else children.emplace_back(copy_expr(std::get<1>(child)));
        return std::make_unique<StringLiteral>(std::move(children));
    }

    std::unique_ptr<Expression> ExpressionTreeCloner::operator()(NameExpression* nm)
    {
        return std::make_unique<NameExpression>(nm->text);
    }

    std::unique_ptr<Expression> ExpressionTreeCloner::operator()(GenericNameExpression* nm)
    {
        return std::make_unique<GenericNameExpression>(*nm);
    }

    std::unique_ptr<Expression> ExpressionTreeCloner::operator()(PrefixOperation* op)
    {
        return std::make_unique<PrefixOperation>(op->op, copy_expr(op->operand));
    }

    std::unique_ptr<Expression> ExpressionTreeCloner::operator()(BinaryOperation* op)
    {
        return std::make_unique<BinaryOperation>(op->op, copy_expr(op->lhs), copy_expr(op->rhs));
    }

    std::unique_ptr<Expression> ExpressionTreeCloner::operator()(GroupingExpression* expr)
    {
        return std::make_unique<GroupingExpression>(copy_expr(expr->expr));
    }

    std::unique_ptr<Expression> ExpressionTreeCloner::operator()(LogicalOperation* op)
    {
        return std::make_unique<LogicalOperation>(op->op, copy_expr(op->lhs), copy_expr(op->rhs));
    }

    std::unique_ptr<Expression> ExpressionTreeCloner::operator()(PostfixOperation* op)
    {
        return std::make_unique<PostfixOperation>(op->op, copy_expr(op->operand));
    }

    std::unique_ptr<Expression> ExpressionTreeCloner::operator()(CallOperation* op)
    {
        std::vector<std::unique_ptr<Expression>> children;
        for(auto& child : op->arguments)
            children.emplace_back(copy_expr(child));
        return std::make_unique<CallOperation>(copy_expr(op->callee), std::move(children));
    }

    std::unique_ptr<Expression> ExpressionTreeCloner::operator()(SubscriptOperation* op)
    {
        return std::make_unique<SubscriptOperation>(copy_expr(op->object), copy_expr(op->index));
    }

    std::unique_ptr<Expression> ExpressionTreeCloner::operator()(LambdaExpression* lmbd)
    {
        return std::make_unique<LambdaExpression>(lmbd->captures, lmbd->sig,
            StatementTreeCloner::copy_stat(lmbd->body));
    }

    std::unique_ptr<Expression> ExpressionTreeCloner::operator()(ScopeOperation* other)
    {
        return std::make_unique<ScopeOperation>(other->type);
    }

    std::unique_ptr<Expression> ExpressionTreeCloner::operator()(ObjectLiteral* obj)
    {
        std::unordered_map<std::string, std::unique_ptr<Expression>> children;
        for(auto&[name, expr] : obj->values)
            children.emplace(name, copy_expr(expr));
        return std::make_unique<ObjectLiteral>(obj->t, std::move(children));
    }

    std::unique_ptr<Expression> ExpressionTreeCloner::operator()(NullLiteral* nl)
    {
        return std::make_unique<NullLiteral>();
    }

    std::unique_ptr<Expression> ExpressionTreeCloner::operator()(AsExpression* expr)
    {
        return std::make_unique<AsExpression>(copy_expr(expr->expr), expr->dest);
    }

    std::unique_ptr<Expression> ExpressionTreeCloner::operator()(GCNewExpression* expr)
    {
        return std::make_unique<GCNewExpression>(copy_expr(expr->target_expression));
    }
    std::unique_ptr<Expression> ExpressionTreeCloner::operator()(CharLiteral* lit)
    {
        auto lt =  std::make_unique<CharLiteral>();
        lt->value = lit->value;
        return lt;
    }

    std::unique_ptr<Expression> ExpressionTreeCloner::copy_expr(Expression* e)
    {
        auto ce = std::visit(ExpressionTreeCloner{}, e->toVariant());
        ce->beg = e->beg;
        ce->end = e->end;
        return ce;
    }

    std::unique_ptr<Expression> ExpressionTreeCloner::copy_expr(std::unique_ptr<Expression>& e)
    {
        auto ce = std::visit(ExpressionTreeCloner{}, e->toVariant());
        ce->beg = e->beg;
        ce->end = e->end;
        return ce;
    }


    std::unique_ptr<Statement> StatementTreeCloner::operator()(FunctionDeclaration* decl)
    {
        return std::make_unique<FunctionDeclaration>(decl->name, decl->signature, copy_stat(decl->body));
    }

    std::unique_ptr<Statement> StatementTreeCloner::operator()(ClassDeclaration* decl)
    {
        std::vector<ClassMethod> new_methods;
        for(auto& method : decl->methods)
        {
            new_methods.emplace_back(method.name, copy_stat(method.function_decl), method.access);
        }
        std::vector<InterfaceImplementation> new_impls;
        for (auto& impl : decl->impls)
        {
            decltype(InterfaceImplementation::methods) mth;
            for (auto& method : impl.methods)
            {
                auto ptr = copy_stat(method.get());
                mth.emplace_back(reinterpret_cast<FunctionDeclaration*>(ptr.release()));
            }
            new_impls.emplace_back(impl.impl_for, impl.location, std::move(mth));
        }
        return std::make_unique<ClassDeclaration>(
            Token{.text = decl->name },
            decl->vars,
            std::move(new_methods),
            decl->ownership,
            decl->interfaces,
            std::move(new_impls));
    }
    std::unique_ptr<Statement> StatementTreeCloner::operator()(CImportDeclaration* dcl)
    {
        return std::make_unique<CImportDeclaration>(dcl->function_name);
    }
    std::unique_ptr<Statement> StatementTreeCloner::operator()(VariableDeclaration* decl)
    {
        return std::make_unique<VariableDeclaration>(decl->identifier, decl->type,copy_expr(decl->initializer), decl->is_mut);
    }

    std::unique_ptr<Statement> StatementTreeCloner::operator()(IfStatement* stat)
    {
        return std::make_unique<IfStatement>(
            copy_expr(stat->condition),
            copy_stat(stat->then_stat),
            copy_stat(stat->else_stat));
    }
    std::unique_ptr<Statement> StatementTreeCloner::operator()(BreakStatement*)
    {
        return std::make_unique<BreakStatement>();
    }
    std::unique_ptr<Statement> StatementTreeCloner::operator()(ContinueStatement*)
    {
        return std::make_unique<ContinueStatement>();
    }
    std::unique_ptr<Statement> StatementTreeCloner::operator()(WhileStatement* stat)
    {
        return std::make_unique<WhileStatement>(copy_expr(stat->condition),copy_stat(stat->body));
    }

    std::unique_ptr<Statement> StatementTreeCloner::operator()(ForStatement* stat)
    {
        return std::make_unique<ForStatement>(stat->names, copy_expr(stat->iterable), copy_stat(stat->body));
    }

    std::unique_ptr<Statement> StatementTreeCloner::operator()(BlockStatement* stat)
    {
        std::vector<std::unique_ptr<Statement>> statements;
        for(auto& stat : stat->statements)
            statements.emplace_back(copy_stat(stat));
        return std::make_unique<BlockStatement>(std::move(statements));
    }

    std::unique_ptr<Statement> StatementTreeCloner::operator()(ReturnStatement* stat)
    {
        return std::make_unique<ReturnStatement>(copy_expr(stat->expression));
    }

    std::unique_ptr<Statement> StatementTreeCloner::operator()(ExpressionStatement* stat)
    {
        return std::make_unique<ExpressionStatement>(copy_expr(stat->expression));
    }

    std::unique_ptr<Statement> StatementTreeCloner::operator()(EnumDeclaration* decl)
    {
        return std::make_unique<EnumDeclaration>(*decl);
    }

    std::unique_ptr<Statement> StatementTreeCloner::operator()(ModuleImport* imp)
    {
        return std::make_unique<ModuleImport>(*imp);
    }

    std::unique_ptr<Statement> StatementTreeCloner::operator()(ConditionalExtraction* stat)
    {
        return std::make_unique<ConditionalExtraction>(stat->captured_name,
            stat->is_ref,
            copy_expr(stat->condition),
            copy_stat(stat->body),
            copy_stat(stat->else_body),
            stat->else_capture, stat->else_is_ref);
    }

    std::unique_ptr<Statement> StatementTreeCloner::operator()(WithStatement* stat)
    {
        return std::make_unique<WithStatement>(stat->name, copy_expr(stat->expression), copy_stat(stat->body));
    }

    std::unique_ptr<Statement> StatementTreeCloner::operator()(OperatorOverload* ov)
    {
        return std::make_unique<OperatorOverload>(ov->tok, ov->signature, copy_stat(ov->body));
    }

    std::unique_ptr<Statement> StatementTreeCloner::operator()(GenericFunctionDeclaration* decl)
    {
        return std::make_unique<GenericFunctionDeclaration>(decl->clause,
            FunctionDeclaration(decl->name, decl->signature, copy_stat(decl->body)));
    }

    std::unique_ptr<Statement> StatementTreeCloner::operator()(GenericAliasDeclaration* decl)
    {
        return std::make_unique<GenericAliasDeclaration>(decl->name, decl->type, decl->clause);
    }

    std::unique_ptr<Statement> StatementTreeCloner::operator()(AliasDeclaration* decl)
    {
        return std::make_unique<AliasDeclaration>(decl->name, decl->type);
    }
    std::unique_ptr<Statement> StatementTreeCloner::operator()(InterfaceDeclaration* stat)
    {
        auto final = std::make_unique<InterfaceDeclaration>();
        final->name = stat->name;
        for (auto& method : stat->methods)
        {
            auto mth = std::make_unique<FunctionDeclaration>(method->name, method->signature, nullptr);
            final->methods.emplace_back(reinterpret_cast<FunctionDeclaration*>(mth.release()));
        }
        return final;
    }
    std::unique_ptr<Statement> StatementTreeCloner::operator()(GenericInterfaceDeclaration* stat)
    {
        auto final = std::make_unique<GenericInterfaceDeclaration>();
        final->name = stat->name;
        for (auto& method : stat->methods)
        {
            auto mth = std::make_unique<FunctionDeclaration>(method->name, method->signature, nullptr);
            final->methods.emplace_back(reinterpret_cast<FunctionDeclaration*>(mth.release()));
        }
        final->clause = stat->clause;
        return final;
    }
    std::unique_ptr<Statement> StatementTreeCloner::operator()(GenericClassDeclaration* decl)
    {
        std::vector<ClassMethod> new_methods;
        for (auto& method : decl->methods)
        {
            new_methods.emplace_back(method.name, copy_stat(method.function_decl), method.access);
        }
        std::vector<InterfaceImplementation> new_impls;
        for (auto& impl : decl->impls)
        {
            decltype(InterfaceImplementation::methods) mth;
            for (auto& method : impl.methods)
            {
                auto ptr = copy_stat(method.get());
                mth.emplace_back(reinterpret_cast<FunctionDeclaration*>(ptr.release()));
            }
            new_impls.emplace_back(impl.impl_for, impl.location, std::move(mth));
        }
        return std::make_unique<GenericClassDeclaration>(
            Token{ .text = decl->name },
            decl->vars,
            std::move(new_methods),
            decl->ownership,
            decl->interfaces,
            std::move(new_impls),
            decl->clause);
    }
    std::unique_ptr<Statement> StatementTreeCloner::copy_stat(Statement* s)
    {
        if (s == nullptr) return nullptr;
        auto ns = std::visit(StatementTreeCloner{}, s->toVariant());
        ns->beg = s->beg;
        ns->end = s->end;
        return ns;
    }

    std::unique_ptr<Statement> StatementTreeCloner::copy_stat(std::unique_ptr<Statement>& s)
    {
        if (s == nullptr) return nullptr;
        auto ns =  std::visit(StatementTreeCloner{}, s->toVariant());
        ns->beg = s->beg;
        ns->end = s->end;
        return ns;
    }

    std::unique_ptr<Expression> StatementTreeCloner::copy_expr(Expression* e)
    {
        return ExpressionTreeCloner::copy_expr(e);
    }
    std::unique_ptr<Expression> StatementTreeCloner::copy_expr(std::unique_ptr<Expression>& e)
    {
        return ExpressionTreeCloner::copy_expr(e);
    }
}

