#pragma once
#include <optional>
#include <string>



namespace Yoyo
{
    class Engine;
    class Type;
    class OverloadDetailsBinary;
    OverloadDetailsBinary* resolveAdd(const Type&, const Type&);
    OverloadDetailsBinary* resolveSub(const Type&, const Type&);
    OverloadDetailsBinary* resolveMul(const Type&, const Type&);
    OverloadDetailsBinary* resolveDiv(const Type&, const Type&);
    OverloadDetailsBinary* resolveRem(const Type&, const Type&);

}
