#pragma once
#include <optional>
#include <string>

#include "token.h"

namespace Yoyo
{
    class IRGenerator;
    class Engine;
    struct Type;
    struct OverloadDetailsBinary;
    std::pair<std::string, OverloadDetailsBinary*> resolveAdd(const Type&, const Type&, IRGenerator*);
    std::pair<std::string, OverloadDetailsBinary*> resolveSub(const Type&, const Type&, IRGenerator*);
    std::pair<std::string, OverloadDetailsBinary*> resolveMul(const Type&, const Type&, IRGenerator*);
    std::pair<std::string, OverloadDetailsBinary*> resolveDiv(const Type&, const Type&, IRGenerator*);
    std::pair<std::string, OverloadDetailsBinary*> resolveRem(const Type&, const Type&, IRGenerator*);
    std::pair<std::string, OverloadDetailsBinary*> resolveCmp(const Type&, const Type&, IRGenerator*);
    std::pair<std::string, OverloadDetailsBinary*> resolveShl(const Type&, const Type&, IRGenerator*);
    std::pair<std::string, OverloadDetailsBinary*> resolveShr(const Type&, const Type&, IRGenerator*);
    std::pair<std::string, OverloadDetailsBinary*> resolveIdx(const Type&, const Type&, IRGenerator*);
    std::pair<std::string, OverloadDetailsBinary*> resolveIdxMut(const Type&, const Type&, IRGenerator*);
}
