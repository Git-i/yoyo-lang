#pragma once
#include <memory>
#include <unordered_map>
#include <vector>

namespace Yoyo
{
    class Statement;
    class FunctionDeclaration;
    class CFGNodeManager;

    class CFGNode
    {
        CFGNode() = default;
        bool visited = false;
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
    struct VarInfo
    {
        std::string name;
        uint32_t depth;
    };
    class CFGNodeManager
    {
        //pair of first and last uses
        using UsageDetails = std::pair<std::vector<Statement*>, std::vector<Statement*>>;
        std::unordered_map<std::string, std::vector<std::pair<CFGNode*, UsageDetails>>> annotate_internal(CFGNode* node);
    public:
        std::vector<std::unique_ptr<CFGNode>> nodes;
        ///this stores the first and last uses of every local variable and is filled after @c annotate is called
        std::unordered_map<std::string, std::vector<std::pair<CFGNode*, UsageDetails>>> uses;
        CFGNode* root_node = nullptr;
        CFGNode* newNode(uint32_t depth, std::string name = "");
        void annotate();
    };
}
