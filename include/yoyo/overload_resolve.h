#pragma once
#include <optional>
#include <string>



namespace Yoyo
{
    class IRGenerator;
    class Engine;
    class Type;
    class OverloadDetailsBinary;
    OverloadDetailsBinary* resolveAdd(const Type&, const Type&, IRGenerator*);
    OverloadDetailsBinary* resolveSub(const Type&, const Type&, IRGenerator*);
    OverloadDetailsBinary* resolveMul(const Type&, const Type&, IRGenerator*);
    OverloadDetailsBinary* resolveDiv(const Type&, const Type&, IRGenerator*);
    OverloadDetailsBinary* resolveRem(const Type&, const Type&, IRGenerator*);
    OverloadDetailsBinary* resolveCmp(const Type&, const Type&, IRGenerator*);

}
