#include "cfg_node.h"

#include <memory>
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
            then_prep.node->addChild(cont);
            if(else_node)
            {
                node->addChild(else_node);
                auto else_prep = CFGPreparator{else_node,exit, depth};
                std::visit(else_prep, stat->else_stat->toVariant());
                else_prep.node->addChild(cont);
            }
            node = cont;
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
            while_prep.node->addChild(node);
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
    CFGNode* CFGNode::prepareFromFunction(CFGNodeManager& mgr, FunctionDeclaration* decl)
    {
        auto entry = mgr.newNode(0, "entry");
        auto exit = mgr.newNode(0, "return");
        std::visit(CFGPreparator{entry, exit, 0}, decl->body->toVariant());
        for(auto& child : exit->children)
            child->parents.erase(std::ranges::find(child->parents, exit));
        exit->children.clear();
        for(int64_t i = 0; i < mgr.nodes.size(); i++)
            if(mgr.nodes[i]->parents.empty() && mgr.nodes[i]->children.empty())
                mgr.nodes.erase(mgr.nodes.begin() + i++);
        return entry;
    }

    void CFGNode::addChild(CFGNode* child)
    {
        children.push_back(child);
        child->parents.push_back(this);
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
}
