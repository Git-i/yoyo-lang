#pragma once
#include "common.h"
#include "source_location.h"
namespace Yoyo {
class YOYO_API ASTNode {
public:
    SourceLocation beg;
    SourceLocation end;
    ASTNode* parent = nullptr;
    virtual ~ASTNode() = default;
};
}  // namespace Yoyo