#pragma once
#include <cstdint>
namespace Yoyo
{
    struct Precedences
    {
        static constexpr uint32_t Assignment = 1;
        static constexpr uint32_t LogicOr = 2;
        static constexpr uint32_t LogicAnd = 3;
        static constexpr uint32_t BitOr = 4;
        static constexpr uint32_t BitXor = 5;
        static constexpr uint32_t BitAnd = 6;
        static constexpr uint32_t Equality = 7;
        static constexpr uint32_t Relational = 8;
        static constexpr uint32_t BitShift = 9;
        static constexpr uint32_t Sum = 10;
        static constexpr uint32_t Product = 11;
        static constexpr uint32_t As = 12;
        static constexpr uint32_t Prefix = 12;
        static constexpr uint32_t MemberAccess = 13;
        static constexpr uint32_t InvalidPropagate = 13;
        static constexpr uint32_t Call = 13;
        static constexpr uint32_t ScopeResolve = 14;
    };
}