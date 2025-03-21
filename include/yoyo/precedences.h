#pragma once
#include <cstdint>
namespace Yoyo
{
    struct Precedences
    {
        static constexpr uint32_t Assignment = 1;
        static constexpr uint32_t RangeOp = 2;
        static constexpr uint32_t LogicOr = RangeOp + 1;
        static constexpr uint32_t LogicAnd = LogicOr + 1;
        static constexpr uint32_t BitOr = LogicAnd + 1;
        static constexpr uint32_t BitXor = BitOr + 1;
        static constexpr uint32_t BitAnd = BitXor + 1;
        static constexpr uint32_t Equality = BitAnd + 1;
        static constexpr uint32_t Relational = Equality + 1;
        static constexpr uint32_t BitShift = Relational + 1;
        static constexpr uint32_t Sum = BitShift + 1;
        static constexpr uint32_t Product = Sum + 1;
        static constexpr uint32_t As = Product + 1;
        static constexpr uint32_t Prefix = As;
        static constexpr uint32_t MemberAccess = Prefix + 1;
        static constexpr uint32_t InvalidPropagate = MemberAccess;
        static constexpr uint32_t Call = MemberAccess;
        static constexpr uint32_t ScopeResolve = Call + 1;
    };
}