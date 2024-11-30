#pragma once
#include <memory>
#include <vector>

namespace Yoyo
{
    class Statement;
    class FunctionDeclaration;
    class CFGNodeManager;

    class CFGNode
    {
        CFGNode() = default;
        friend class CFGNodeManager;
    public:
        std::string debug_name;
        static CFGNode* prepareFromFunction(CFGNodeManager& mgr, FunctionDeclaration* decl);
        std::vector<CFGNode*> children;
        std::vector<CFGNode*> parents;
        uint32_t depth = 0;
        void addChild(CFGNode* child);
        std::vector<Statement*> statements;
        CFGNodeManager* manager = nullptr;
    };
    class CFGNodeManager
    {
    public:
        std::vector<std::unique_ptr<CFGNode>> nodes;
        CFGNode* newNode(uint32_t depth, std::string name = "");
    };
}
