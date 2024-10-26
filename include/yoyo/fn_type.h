#include "type.h"
#include "func_sig.h"
namespace Yoyo
{
    struct FunctionType : Type
    {
        FunctionSignature sig;
        bool is_bound = false;
        FunctionType() = default;
        FunctionType(FunctionSignature decl, bool is_bound)
            : Type{.name = "__fn", .subtypes = {}}, sig(decl), is_bound(is_bound) {}
    };
}