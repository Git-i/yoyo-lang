#include "cfg_node.h"

#include <memory>
#include "statement.h"
namespace Yoyo
{
    struct CFGPreparator
    {
        CFGNode* node;
        uint32_t depth;
        void operator()(ExpressionStatement* stat) const{ node->statements.push_back(stat); }
        void operator()(VariableDeclaration* decl) const{ node->statements.push_back(decl); }
        void operator()(FunctionDeclaration* stat) const {}
        void operator()(IfStatement* stat)
        {
            node->statements.push_back(stat);

            auto then = node->manager->newNode(depth);
            auto else_node = stat->else_stat ? node->manager->newNode(depth) : nullptr;
            auto cont = node->manager->newNode(depth);

            node->addChild(then);
            std::visit(CFGPreparator{then, depth}, stat->then_stat->toVariant());
            then->addChild(cont);
            if(else_node)
            {
                node->addChild(else_node);
                std::visit(CFGPreparator{else_node, depth}, stat->else_stat->toVariant());
                else_node->addChild(cont);
            }
            node = cont;
        }
        void operator()(ReturnStatement* stat) const { node->statements.push_back(stat); }
        void operator()(WhileStatement* stat)
        {
            node->statements.push_back(stat);
            auto then = node->manager->newNode(depth);
            auto cont = node->manager->newNode(depth);
            node->addChild(then);
            node->addChild(cont);
            then->addChild(node);
            std::visit(CFGPreparator{then, depth}, stat->body->toVariant());
            node = cont;
        }
        void operator()(BlockStatement* stat)
        {
            auto visistor = CFGPreparator{node, depth + 1};
            for(auto& sub : stat->statements)
            {
                std::visit(visistor, sub->toVariant());
                if(dynamic_cast<ReturnStatement*>(sub.get())) return;
            }
        }
        void operator()(ClassDeclaration* stat) const {}
        void operator()(ForStatement* stat){} //TODO
        void operator()(ModuleImport* stat) const {}
        void operator()(EnumDeclaration* stat) const {}
        void operator()(ConditionalExtraction* stat)
        {
            node->statements.push_back(stat);

            auto then = node->manager->newNode(depth + 1);
            auto else_node = stat->else_body ? node->manager->newNode(depth + 1) : nullptr;
            auto cont = node->manager->newNode(depth);

            node->addChild(then);
            std::visit(CFGPreparator{then, depth + 1}, stat->body->toVariant());
            then->addChild(cont);
            if(else_node)
            {
                node->addChild(else_node);
                std::visit(CFGPreparator{else_node, depth + 1}, stat->else_body->toVariant());
                else_node->addChild(cont);
            }
            node = cont;
        }
        void operator()(WithStatement* stat)
        {
            auto with = node->manager->newNode(depth + 1);
            auto cont = node->manager->newNode(depth);
            node->addChild(with);
            with->addChild(cont);
            std::visit(CFGPreparator{with, depth + 1}, stat->body->toVariant());
            node = cont;
        }

    };
    CFGNode* CFGNode::prepareFromFunction(CFGNodeManager& mgr, FunctionDeclaration* decl)
    {
        auto entry = mgr.newNode(0);
        std::visit(CFGPreparator{entry, 0}, decl->body->toVariant());
        return entry;
    }

    void CFGNode::addChild(CFGNode* child)
    {
        children.push_back(child);
        child->parents.push_back(this);
    }

    CFGNode* CFGNodeManager::newNode(uint32_t depth)
    {
        auto node = std::unique_ptr<CFGNode>(new CFGNode());
        node->depth = depth;
        node->manager = this;
        auto node_ptr = node.get();
        nodes.push_back(std::move(node));
        return node_ptr;
    }
}
