#include "cfg_node.h"

#include <memory>
#include <ranges>
#include <set>

#include "statement.h"
namespace Yoyo
{
    struct CFGPreparator
    {
        CFGNode* node;
        CFGNode* exit;
        uint32_t depth;
        void operator()(ExpressionStatement* stat) const{ node->statements.push_back(stat); }
        void operator()(VariableDeclaration* decl) const{ node->statements.push_back(decl); }
        void operator()(FunctionDeclaration* stat) const {}
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
            node->statements.push_back(stat);
            auto then = node->manager->newNode(depth, "while_then");
            node->addChild(then);
            auto while_prep = CFGPreparator{then,exit, depth};
            std::visit(while_prep, stat->body->toVariant());
            if(while_prep.node != exit) while_prep.node->addChild(node);
        }
        void operator()(BlockStatement* stat)
        {
            auto body = node->manager->newNode(depth + 1, "block_body");
            node->addChild(body);
            auto visistor = CFGPreparator{body, exit, depth + 1};
            for(auto& sub : stat->statements)
            {
                std::visit(visistor, sub->toVariant());
                if(visistor.node == exit) { node = visistor.node; return; }
            }
            node = visistor.node;
        }
        void operator()(AliasDeclaration*) const {}
        void operator()(ClassDeclaration* stat) const {}
        void operator()(ForStatement* stat){} //TODO
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
            then_prep.node->addChild(cont);
            if(else_node)
            {
                node->addChild(else_node);
                auto else_prep = CFGPreparator{else_node, exit,depth + 1};
                std::visit(else_prep, stat->else_body->toVariant());
                else_prep.node->addChild(cont);
            }
            node = cont;
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
    };
    struct UsedVariablesExpression
    {
        std::set<std::string> operator()(Expression*) {return {};}
        std::set<std::string> operator()(TupleLiteral* lit)
        {
            std::set<std::string> vars;
            for(auto& expr: lit->elements)
            {
                auto st = std::visit(*this, expr->toVariant());
                vars.insert(std::make_move_iterator(st.begin()), std::make_move_iterator(st.end()));
            }
            return vars;
        }
        std::set<std::string> operator()(ArrayLiteral* lit)
        {
            std::set<std::string> vars;
            for(auto& expr: lit->elements)
            {
                auto st = std::visit(*this, expr->toVariant());
                vars.insert(std::make_move_iterator(st.begin()), std::make_move_iterator(st.end()));
            }
            return vars;
        }
        std::set<std::string> operator()(StringLiteral* lit)
        {
            std::set<std::string> vars;
            for(auto& expr : lit->literal)
                if(std::holds_alternative<std::unique_ptr<Expression>>(expr))
                {
                    auto st = std::visit(*this, std::get<1>(expr)->toVariant());
                    vars.insert(std::make_move_iterator(st.begin()), std::make_move_iterator(st.end()));
                }
            return vars;
        }
        std::set<std::string> operator()(NameExpression* nm)
        {
            return {nm->text};
        }
        std::set<std::string> operator()(GenericNameExpression*) { return {}; } //??
        std::set<std::string> operator()(PrefixOperation* pf)
        {
            return std::visit(*this, pf->operand->toVariant());
        }
        std::set<std::string> operator()(BinaryOperation* bop)
        {
            auto left_uses = std::visit(*this, bop->lhs->toVariant());
            if(bop->op.type != TokenType::Dot)
            {
                auto right_uses = std::visit(*this, bop->rhs->toVariant());
                left_uses.insert(std::make_move_iterator(right_uses.begin()), std::make_move_iterator(right_uses.end()));
            }
            return left_uses;
        }
        std::set<std::string> operator()(GroupingExpression* expr)
        {
            return std::visit(*this, expr->expr->toVariant());
        }
        std::set<std::string> operator()(LogicalOperation* expr)
        {
            std::set left = std::visit(*this, expr->lhs->toVariant());
            std::set right = std::visit(*this, expr->rhs->toVariant());
            left.insert(std::make_move_iterator(right.begin()), std::make_move_iterator(right.end()));
            return left;
        }
        std::set<std::string> operator()(PostfixOperation* pop)
        {
            return std::visit(*this, pop->operand->toVariant());
        }
        std::set<std::string> operator()(CallOperation* op)
        {
            auto callee_uses = std::visit(*this, op->callee->toVariant());
            for(auto& arg: op->arguments)
            {
                auto arg_uses = std::visit(*this, arg->toVariant());
                callee_uses.insert(std::make_move_iterator(arg_uses.begin()), std::make_move_iterator(arg_uses.end()));
            }
            return callee_uses;
        }
        std::set<std::string> operator()(SubscriptOperation* op)
        {
            auto obj =  std::visit(*this, op->object->toVariant());
            auto idx = std::visit(*this, op->index->toVariant());
            obj.insert(std::make_move_iterator(idx.begin()), std::make_move_iterator(idx.end()));
            return obj;
        }
        std::set<std::string> operator()(LambdaExpression* lmbd){}
        std::set<std::string> operator()(ScopeOperation*) {} //???
        std::set<std::string> operator()(ObjectLiteral* lit)
        {
            std::set<std::string> vars;
            for(auto& [_,expr]: lit->values)
            {
                auto st = std::visit(*this, expr->toVariant());
                vars.insert(std::make_move_iterator(st.begin()), std::make_move_iterator(st.end()));
            }
            return vars;
        }
        std::set<std::string> operator()(AsExpression* lit)
        {
            return std::visit(*this, lit->expr->toVariant());
        }
    };
    struct UsedVariables
    {
        std::set<std::string> operator()(Statement*) { return {}; }
        std::set<std::string> operator()(ExpressionStatement* stat)
        {
            return std::visit(UsedVariablesExpression{}, stat->expression->toVariant());
        }
        std::set<std::string> operator()(VariableDeclaration* decl)
        {
            if(decl->initializer)
            {
                std::set used_in_expr = {std::string(decl->identifier.text)};
                std::visit(UsedVariablesExpression{}, decl->initializer->toVariant());
                return used_in_expr;
            }
            return {};
        }
        std::set<std::string> operator()(IfStatement* stat)
        {
            return std::visit(UsedVariablesExpression{}, stat->condition->toVariant());
        }
        std::set<std::string> operator()(ReturnStatement* stat)
        {
            return std::visit(UsedVariablesExpression{}, stat->expression->toVariant());
        }
        std::set<std::string> operator()(WhileStatement* stat)
        {
            return std::visit(UsedVariablesExpression{}, stat->condition->toVariant());
        }
        std::set<std::string> operator()(ForStatement* stat){}
        std::set<std::string> operator()(ConditionalExtraction* stat)
        {
            return std::visit(UsedVariablesExpression{}, stat->condition->toVariant());
        }
        std::set<std::string> operator()(WithStatement* stat)
        {
            return std::visit(UsedVariablesExpression{}, stat->expression->toVariant());
        }
    };

    CFGNode* CFGNode::prepareFromFunction(CFGNodeManager& mgr, FunctionDeclaration* decl)
    {
        auto entry = mgr.newNode(0, "entry");
        auto exit = mgr.newNode(0, "return");
        std::visit(CFGPreparator{entry, exit, 0}, decl->body->toVariant());
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
    auto CFGNodeManager::annotate_internal(
        CFGNode* node) -> std::unordered_map<std::string, std::vector<std::pair<
                                                 CFGNode*, UsageDetails>>>
    {
        std::unordered_map<std::string, std::vector<std::pair<CFGNode*, UsageDetails>>> details;
        for(auto& stat : node->statements)
        {
            auto vars = std::visit(UsedVariables{}, stat->toVariant());
            for(auto& var : vars)
            {
                //if it doesn't exist it's a first use else it's the new "latest" use
                if(!details.contains(var))
                {
                    auto& bck = details[var].emplace_back();
                    bck.first = node;
                    bck.second.first.emplace_back(stat);
                    bck.second.second.emplace_back(stat);
                }
                auto& dets = details.at(var);
                dets[0].second.second[0] = stat;
            }
        }
        if(node->visited) return details;
        node->visited = true;
        std::unordered_map<std::string, std::vector<std::pair<CFGNode*, UsageDetails>>> rpdetails;
        for(auto& child : node->children)
        {
            auto results = annotate_internal(child);
            for(auto&[var, det] : results)
            {

                //new variable encountered
                if(!details.contains(var))
                {
                    details.emplace(var, std::move(det));
                    continue;
                }

                //we have same name (but maybe not the same variable)
                auto& this_det = details.at(var);
                //this_it is a pointer to the pair that belongs to us
                auto this_it = std::ranges::find_if(this_det, [node](auto& thing)
                {
                    return thing.first == node;
                });
                auto* this_ptr = this_it == this_det.end() ? nullptr : &*this_it;
                //other_it is the pair that belongs to our child that we can reposses
                auto repossesable = [node](decltype(det)::reference thing)
                {
                    //we reverse traverse to parents with lower or same depth values
                    CFGNode* parent = thing.first;
                    while(parent && parent != node)
                    {
                        auto it = std::ranges::find_if(parent->parents, [parent](auto& p)
                        {
                            return parent->depth >= p->depth;
                        });
                        if(it == parent->parents.end())
                            parent = nullptr;
                        else parent = *it;
                    }
                    return static_cast<bool>(parent);
                };

                for(auto& ls : det)
                {
                    if(this_ptr && repossesable(ls))
                    {
                        auto& repossess_able_details = rpdetails[var];
                        repossess_able_details.push_back(std::move(ls));
                    }
                    else
                        details[var].emplace_back(std::move(ls));
                }
            }
        }
        //before we steal the content of rpdetails, we need to filter it to remove non-last uses
        for(auto&[_, detail] : rpdetails)
        {
            for(size_t i = 0; i < detail.size(); i++)
            {
                auto& ls = detail[i];
                //if there is a straight path between ls and any of the nodes ls is not a last use
                auto child = ls.first;
                while(child->children.size() == 1)
                {
                    auto keys = detail | std::views::keys;
                    auto it = std::ranges::find(keys, child->children[0]);
                    if(it == keys.end()) child = child->children[0];
                    //we reached it in a straight path
                    else
                    {
                        detail.erase(detail.begin() + static_cast<int64_t>(i++));
                        break;
                    }
                }
            }
        }
        //finally we repossess
        for(auto& [k, det]: rpdetails)
        {
            auto& this_det = details.at(k);
            auto this_it = std::ranges::find_if(this_det, [node](auto& thing)
                {
                    return thing.first == node;
                });
            this_it->second.second.clear();
            for(auto& ls : det)
                this_it->second.second.insert(this_it->second.second.end(),
                    std::make_move_iterator(ls.second.second.begin()),
                    std::make_move_iterator(ls.second.second.end()));

        }
        return details;
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
        uses = annotate_internal(root_node);
    }
}

