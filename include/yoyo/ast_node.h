#pragma once
#include "source_location.h"
#include "common.h"
namespace Yoyo {
    class YOYO_API ASTNode {
    public:
        SourceLocation beg;
        SourceLocation end;
        ASTNode* parent = nullptr;
        virtual ~ASTNode() = default;
    };
}