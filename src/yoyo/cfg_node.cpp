#include "cfg_node.h"

#include <cassert>
#include <format>
#include <iostream>
#include <memory>
#include <ranges>
#include <set>

#include "statement.h"
namespace Yoyo
{

    void YOYO_API debugbreak();
    struct CFGPreparator
    {
        CFGNode* node;
        CFGNode* exit;
        uint32_t depth;

        CFGNode* break_to = nullptr;
        CFGNode* continue_to = nullptr;
        void operator()(ExpressionStatement* stat) const{ node->statements.push_back(stat); }
        void operator()(VariableDeclaration* decl) const{ node->statements.push_back(decl); }
        void operator()(FunctionDeclaration* stat) const {}
        void operator()(InterfaceDeclaration* stat) const {}
        void operator()(ConstantDeclaration* stat) const {}
        void operator()(UnionDeclaration* stat) const {}
        void operator()(MacroDeclaration* stat) const {}
        void operator()(IfStatement* stat)
        {
            node->statements.push_back(stat);
            auto then = node->manager->newNode(depth, "if_then");
            auto else_node = stat->else_stat ? node->manager->newNode(depth, "if_else") : nullptr;
            auto cont = node->manager->newNode(depth, "if_cont");
            if(!else_node) node->addChild(cont);
            node->addChild(then);
            auto then_prep = CFGPreparator{then,exit, depth};
            std::visit(then_prep, stat->then_stat->toVariant());
            if(then_prep.node != exit) then_prep.node->addChild(cont);
            if(else_node)
            {
                node->addChild(else_node);
                auto else_prep = CFGPreparator{else_node,exit, depth};
                std::visit(else_prep, stat->else_stat->toVariant());
                if(else_prep.node != exit) else_prep.node->addChild(cont);
            }
            node = cont->parents.empty() ? exit : cont;
        }
        void operator()(ReturnStatement* stat)
        {
            node->statements.push_back(stat);
            node->addChild(exit);
            node = exit;
        }
        void operator()(WhileStatement* stat)
        {
            auto cond = node->manager->newNode(depth, "while_cond");
            auto cont = node->manager->newNode(depth, "while_cont");
            cond->addChild(cont);
            cond->statements.push_back(stat);
            node->addChild(cond);
            auto then = node->manager->newNode(depth, "while_then");
            cond->addChild(then);
            auto while_prep = CFGPreparator{then,exit, depth, cont, cond};
            std::visit(while_prep, stat->body->toVariant());
            if(while_prep.node != exit && while_prep.node != cont && while_prep.node != cond) while_prep.node->addChild(cond);
            if(while_prep.node == exit) node = exit;
            else node = cont;
        }
        void operator()(BlockStatement* stat)
        {
            auto body = node->manager->newNode(depth + 1, "block_body");
            node->addChild(body);
            auto visistor = CFGPreparator{body, exit, depth + 1};
            for(auto& sub : stat->statements)
            {
                std::visit(visistor, sub->toVariant());
                if(visistor.node == exit || visistor.node == break_to || visistor.node == continue_to) { node = visistor.node; return; }
            }
            node = visistor.node;
        }
        void operator()(CImportDeclaration*) const {}
        void operator()(AliasDeclaration*) const {}
        void operator()(ClassDeclaration* stat) const {}
        void operator()(ForStatement* stat){
            node->statements.push_back(stat);
            auto check = node->manager->newNode(depth, "for_check");
            auto then = node->manager->newNode(depth, "for_then");
            auto cont = node->manager->newNode(depth, "for_cont");
            node->addChild(check);
            check->addChild(then);
            check->addChild(cont);
            auto then_prep = CFGPreparator{ then,exit, depth, cont, check };
            std::visit(then_prep, stat->body->toVariant());
            if (then_prep.node != exit && then_prep.node != cont && then_prep.node != check) then_prep.node->addChild(check);
            if (then_prep.node == exit) node = exit;
            else node = cont;
        }
        void operator()(ModuleImport* stat) const {}
        void operator()(EnumDeclaration* stat) const {}
        void operator()(OperatorOverload*) const{}
        void operator()(GenericFunctionDeclaration*) const{}
        void operator()(ConditionalExtraction* stat)
        {
            node->statements.push_back(stat);

            auto then = node->manager->newNode(depth + 1, "cond_extract_then");
            auto else_node = stat->else_body ? node->manager->newNode(depth + 1, "cond_extract_else") : nullptr;
            auto cont = node->manager->newNode(depth, "cond_extract_cont");
            if(!else_node) node->addChild(cont);
            node->addChild(then);
            auto then_prep = CFGPreparator{then,exit, depth + 1};
            std::visit(then_prep, stat->body->toVariant());
            if (then_prep.node != exit) then_prep.node->addChild(cont);
            if(else_node)
            {
                node->addChild(else_node);
                auto else_prep = CFGPreparator{else_node, exit,depth + 1};
                std::visit(else_prep, stat->else_body->toVariant());
                if (else_prep.node != exit) else_prep.node->addChild(cont);
            }
            node = cont->parents.empty() ? exit : cont;
        }
        void operator()(WithStatement* stat)
        {
            auto with = node->manager->newNode(depth + 1, "with body");
            auto cont = node->manager->newNode(depth, "with_cont");
            node->addChild(with);
            auto with_prep = CFGPreparator{with, exit, depth + 1};
            std::visit(with_prep, stat->body->toVariant());
            with_prep.node->addChild(cont);
            node = cont;
        }
        void operator()(BreakStatement* stat)
        {
            if (break_to) {
                node->addChild(break_to);
                node = break_to;
            }
        }
        void operator()(ContinueStatement* stat)
        {
            if (continue_to) {
                node->addChild(continue_to);
                node = continue_to;
            }
        }
    };
    struct UsedVariablesExpression
    {
        bool is_first;
        std::unordered_map<std::string, Expression*> operator()(Expression*) {return {};}
        std::unordered_map<std::string, Expression*> operator()(TupleLiteral* lit)
        {
            std::unordered_map<std::string, Expression*> vars;
            for(auto& expr: lit->elements)
            {
                auto st = std::visit(*this, expr->toVariant());
                for(auto&[name, use] : st)
                {
                    if(is_first && vars.contains(name)) continue;
                    vars[name] = use;
                }
            }
            return vars;
        }
        std::unordered_map<std::string, Expression*> operator()(ArrayLiteral* lit)
        {
            std::unordered_map<std::string, Expression*> vars;
            if (std::holds_alternative<std::vector<std::unique_ptr<Expression>>>(lit->elements)) {
                for (auto& expr : std::get<std::vector<std::unique_ptr<Expression>>>(lit->elements))
                {
                    auto st = std::visit(*this, expr->toVariant());
                    for (auto& [name, use] : st)
                    {
                        if (is_first && vars.contains(name)) continue;
                        vars[name] = use;
                    }
                }
            }
            
            return vars;
        }
        std::unordered_map<std::string, Expression*> operator()(StringLiteral* lit)
        {
            std::unordered_map<std::string, Expression*> vars;
            for(auto& expr : lit->literal)
                if(std::holds_alternative<std::unique_ptr<Expression>>(expr))
                {
                    auto st = std::visit(*this, std::get<1>(expr)->toVariant());
                    for(auto&[name, use] : st)
                    {
                        if(is_first && vars.contains(name)) continue;
                        vars[name] = use;
                    }
                }
            return vars;
        }
        std::unordered_map<std::string, Expression*> operator()(NameExpression* nm)
        {
            return {{nm->text, nm}};
        }
        std::unordered_map<std::string, Expression*> operator()(GenericNameExpression*) { return {}; } //??
        std::unordered_map<std::string, Expression*> operator()(PrefixOperation* pf)
        {
            return std::visit(*this, pf->operand->toVariant());
        }
        std::unordered_map<std::string, Expression*> operator()(BinaryOperation* bop)
        {
            auto left_uses = std::visit(*this, bop->lhs->toVariant());
            if(bop->op.type != TokenType::Dot)
            {
                auto right_uses = std::visit(*this, bop->rhs->toVariant());
                for(auto&[name, use] : right_uses)
                {
                    if(is_first && left_uses.contains(name)) continue;
                    left_uses[name] = use;
                }
            }
            return left_uses;
        }
        std::unordered_map<std::string, Expression*> operator()(GroupingExpression* expr)
        {
            return std::visit(*this, expr->expr->toVariant());
        }
        std::unordered_map<std::string, Expression*> operator()(LogicalOperation* expr)
        {
            auto left_uses = std::visit(*this, expr->lhs->toVariant());
            auto right_uses = std::visit(*this, expr->rhs->toVariant());
            for(auto&[name, use] : right_uses)
            {
                if(is_first && left_uses.contains(name)) continue;
                left_uses[name] = use;
            }
            return left_uses;
        }
        std::unordered_map<std::string, Expression*> operator()(PostfixOperation* pop)
        {
            return std::visit(*this, pop->operand->toVariant());
        }
        std::unordered_map<std::string, Expression*> operator()(CallOperation* op)
        {
            auto callee_uses = std::visit(*this, op->callee->toVariant());
            for(auto& arg: op->arguments)
            {
                auto arg_uses = std::visit(*this, arg->toVariant());
                for(auto&[name, use] : arg_uses)
                {
                    if(is_first && callee_uses.contains(name)) continue;
                    callee_uses[name] = use;
                }
            }
            return callee_uses;
        }
        std::unordered_map<std::string, Expression*> operator()(SubscriptOperation* op)
        {
            auto obj =  std::visit(*this, op->object->toVariant());
            auto idx = std::visit(*this, op->index->toVariant());
            for(auto&[name, use] : idx)
            {
                if(is_first && obj.contains(name)) continue;
                obj[name] = use;
            }
            return obj;
        }
        std::unordered_map<std::string, Expression*> operator()(LambdaExpression* lmbd){ return {}; }
        std::unordered_map<std::string, Expression*> operator()(ScopeOperation*) { return {}; } //???
        std::unordered_map<std::string, Expression*> operator()(ObjectLiteral* lit)
        {
            std::unordered_map<std::string, Expression*> vars;
            for(auto& [_,expr]: lit->values)
            {
                auto st = std::visit(*this, expr->toVariant());
                for(auto&[name, use] : st)
                {
                    if(is_first && vars.contains(name)) continue;
                    vars[name] = use;
                }
            }
            return vars;
        }
        std::unordered_map<std::string, Expression*> operator()(AsExpression* lit)
        {
            return std::visit(*this, lit->expr->toVariant());
        }
        std::unordered_map<std::string, Expression*> operator()(GCNewExpression* lit)
        {
            return std::visit(*this, lit->target_expression->toVariant());
        }
    };
    struct UsedVariables
    {
        bool is_first;
        std::unordered_map<std::string, Expression*> operator()(Statement*) { return {}; }
        std::unordered_map<std::string, Expression*> operator()(ExpressionStatement* stat)
        {
            return std::visit(UsedVariablesExpression{is_first}, stat->expression->toVariant());
        }
        std::unordered_map<std::string, Expression*> operator()(VariableDeclaration* decl)
        {
            if(decl->initializer)
            {
                auto uses = std::visit(UsedVariablesExpression{is_first}, decl->initializer->toVariant());
                uses.emplace(decl->identifier.text, decl->initializer.get());
                return uses;
            }
            return {};
        }
        std::unordered_map<std::string, Expression*> operator()(IfStatement* stat)
        {
            return std::visit(UsedVariablesExpression{is_first}, stat->condition->toVariant());
        }
        std::unordered_map<std::string, Expression*> operator()(ReturnStatement* stat)
        {
            if(!stat->expression) return {};
            return std::visit(UsedVariablesExpression{is_first}, stat->expression->toVariant());
        }
        std::unordered_map<std::string, Expression*> operator()(WhileStatement* stat)
        {
            return std::visit(UsedVariablesExpression{is_first}, stat->condition->toVariant());
        }
        std::unordered_map<std::string, Expression*> operator()(ForStatement* stat){ 
            return std::visit(UsedVariablesExpression{ is_first }, stat->iterable->toVariant());
        }
        std::unordered_map<std::string, Expression*> operator()(ConditionalExtraction* stat)
        {
            return std::visit(UsedVariablesExpression{is_first}, stat->condition->toVariant());
        }
        std::unordered_map<std::string, Expression*> operator()(WithStatement* stat)
        {
            return std::visit(UsedVariablesExpression{is_first}, stat->expression->toVariant());
        }
    };
    struct FirstUsedVariables : UsedVariables
    {
        FirstUsedVariables() : UsedVariables(true){}
    };
    struct LastUsedVariables : UsedVariables
    {
        LastUsedVariables() : UsedVariables(false){}
    };
    CFGNode* CFGNode::prepareFromFunction(CFGNodeManager& mgr, FunctionDeclaration* decl)
    {
        auto entry = mgr.newNode(0, "entry");
        auto exit = mgr.newNode(0, "return");
        std::visit(CFGPreparator{entry, exit, 0}, decl->body->toVariant());
        if (entry->children.empty()) entry->addChild(exit);
        //for(auto& child : exit->children)
        //    child->parents.erase(std::ranges::find(child->parents, exit));
        //exit->children.clear();
        for(int64_t i = 0; i < mgr.nodes.size(); i++)
            if(mgr.nodes[i]->parents.empty() && mgr.nodes[i]->children.empty())
                mgr.nodes.erase(mgr.nodes.begin() + i++);
        mgr.root_node = entry;
        return entry;
    }

    void CFGNode::addChild(CFGNode* child)
    {
        children.push_back(child);
        child->parents.push_back(this);
    }
    //std::vector<std::set<std::string>>
    std::unordered_map<std::string, std::set<Expression*>> findFirstUsesInternal(CFGNode* node, std::vector<std::set<std::string>> vars)
    {
        std::unordered_map<std::string, std::set<Expression*>> out;
        std::set<std::string>* found_here = nullptr;
        std::set<std::string> init_here;
        //size is either depth + 1, more or one less(same as depth)
        if(node->depth == vars.size()) found_here = &vars.emplace_back();
        else if(vars.size() >= node->depth + 1) found_here = &vars[node->depth];
        else debugbreak();
        for(auto stat: node->statements)
        {
            if(auto dcast = dynamic_cast<VariableDeclaration*>(stat))
                found_here->emplace(dcast->identifier.text);
            auto uses = std::visit(FirstUsedVariables{}, stat->toVariant());
            for(auto&[var, use] : uses)
                if(!out.contains(var))
                {
                    out.emplace(var, std::set{use});
                    found_here->emplace(var);
                    init_here.emplace(var);
                }
        }
        auto exists_before = [&vars](const std::string& name, uint32_t scope)
        {
            return std::ranges::any_of(std::ranges::subrange(vars.begin(), vars.begin() + scope), [&name](auto& set)
            {
               return set.contains(name);
            });
        };
        if(node->visited == true) return out;
        node->visited = true;
        for(auto child: node->children)
        {
            //if the child used it, but we used it too:
            //  We probably used our own local copy (child is less deep than us and the variable is defined before)
            //     In this case we keep the child's use
            //  Child used our own var (child is deeper or same level)
            //  None of these and child is first use
            auto uses = findFirstUsesInternal(child, vars);
            for(auto&[var, use] : uses)
                //child is deeper, or it's shallow but the var is defined before so its only first use if we don't use it
                if(child->depth >= node->depth ||
                    child->depth < node->depth && exists_before(var, node->depth))
                {
                    if(!init_here.contains(var))
                        out[var].insert(std::make_move_iterator(use.begin()), std::make_move_iterator(use.end()));
                }
                //child is shallow and var doesn't exist before
                else
                    out[var].insert(std::make_move_iterator(use.begin()), std::make_move_iterator(use.end()));
        }
        return out;
    }
    std::unordered_map<std::string, std::set<Expression*>> findLastUsesInternal(CFGNode* node, std::vector<std::set<std::string>> vars)
    {
        std::unordered_map<std::string, std::set<Expression*>> out;
        std::set<std::string>* found_here = nullptr;
        //size is either depth + 1, more or one less(same as depth)
        if(node->depth == vars.size()) found_here = &vars.emplace_back();
        else if(vars.size() >= node->depth + 1) found_here = &vars[node->depth];
        else debugbreak();
        for(auto stat: node->statements)
        {
            if(auto dcast = dynamic_cast<VariableDeclaration*>(stat))
                found_here->emplace(dcast->identifier.text);
            auto uses = std::visit(LastUsedVariables{}, stat->toVariant());
            for(auto&[var, use] : uses)
            {
                out[var] = std::set{use};
                found_here->emplace(var);
            }
        }
        auto exists_before = [&vars](const std::string& name, uint32_t scope)
        {
            return std::ranges::any_of(std::ranges::subrange(vars.begin(), vars.begin() + scope), [&name](auto& set)
            {
               return set.contains(name);
            });
        };
        if(node->visited)
        {
            node->looped = true;
            return out;
        }
        node->visited = true;
        std::unordered_map<std::string, std::vector<std::pair<CFGNode*, std::set<Expression*>>>> child_uses;
        for(auto child: node->children)
        {
            node->looped = false;
            auto uses = findLastUsesInternal(child, vars);
            //if we looped then last uses of variables on this depth and lower are not guaranteed
            if(node->looped)
            {
                for(auto& var_set: std::ranges::subrange(vars.begin(), vars.begin() + node->depth + 1))
                {
                    for(auto& var : var_set)
                        out[var].clear();
                }
            }
            for(auto&[var, use] : uses)
                //child is deeper, or it's shallow but the var is defined before so its only first use if we don't use it
                if(child->depth >= node->depth ||
                    child->depth < node->depth && exists_before(var, node->depth))
                {
                    //if the node looped we only add variables we don't know
                    out[var].clear();
                    if(!node->looped || node->looped && !exists_before(var, node->depth + 1))
                        child_uses[var].emplace_back(child, std::move(use));
                }
                //child is shallow and var doesn't exist before
                else
                    out[var].insert(std::make_move_iterator(use.begin()), std::make_move_iterator(use.end()));
        }
        //if we can reach any of the uses with no branching from one use, that use is not a last use
        for(auto&[var, uses] : child_uses)
        {
            for(size_t i = 0; i < uses.size(); i++)
            {
                auto node = uses[i].first;
                while(node->children.size() == 1)
                {
                    auto child = node->children.front();
                    auto node_view = uses | std::views::keys;
                    auto it = std::ranges::find(node_view, child);
                    if(it != node_view.end())
                    {
                        uses.erase(uses.begin() + static_cast<int64_t>(i++));
                        break;
                    }
                    node = child;
                }
            }
        }
        for(auto&[var, uses] : child_uses)
        {
            for(auto& use : uses)
                out[var].insert(std::make_move_iterator(use.second.begin()), std::make_move_iterator(use.second.end()));
        }

        node->visited = false;
        return out;
    }

    std::unordered_map<std::string, std::set<Expression*>> CFGNodeManager::findFirstUses()
    {
        return findFirstUsesInternal(root_node, {});
    }

    std::unordered_map<std::string, std::set<Expression*>> CFGNodeManager::findLastUses()
    {
        return findLastUsesInternal(root_node, {});
    }

    CFGNode* CFGNodeManager::newNode(uint32_t depth, std::string name)
    {
        auto node = std::unique_ptr<CFGNode>(new CFGNode());
        node->depth = depth;
        node->manager = this;
        node->debug_name = name;
        auto node_ptr = node.get();
        nodes.push_back(std::move(node));
        return node_ptr;
    }

    void CFGNodeManager::annotate()
    {
        first_uses = findFirstUses();
        for(auto& node : nodes) node->visited = false;
        last_uses = findLastUses();
    }
}

