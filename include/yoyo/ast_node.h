#pragma once
#include "source_location.h"
namespace Yoyo {
    class ASTNode {
    public:
        SourceLocation beg;
        SourceLocation end;
        ASTNode* parent = nullptr;
        virtual ~ASTNode() = default;
    };
}