#pragma once
#include <memory>
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace Yoyo
{
    class Expression;
}

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
        bool visited = false;
        bool looped = false;
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
        std::unordered_map<std::string, std::set<Expression*>> findFirstUses();
        std::unordered_map<std::string, std::set<Expression*>> findLastUses();
    public:
        std::vector<std::unique_ptr<CFGNode>> nodes;
        ///this stores the first and last uses of every local variable and is filled after @c annotate is called
        std::unordered_map<std::string, std::set<Expression*>> first_uses;
        std::unordered_map<std::string, std::set<Expression*>> last_uses;
        CFGNode* root_node = nullptr;
        CFGNode* newNode(uint32_t depth, std::string name = "");
        void annotate();
    };
}
