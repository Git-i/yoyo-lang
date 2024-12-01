#pragma once
namespace Yoyo
{
    enum class OverloadType
    {
        Invalid,
        Plus, Minus, Mul, Div, Mod,
        BitAnd, BitOr, BitXor, Shl, Shr,
        Index, IndexMut,
        UnNeg, UnNot,
        SpaceShip, Equal
    };

    struct OverloadDetailsBinary
    {
        Type left; Type right; Type result;
    };

    struct OverloadDetailsUnary
    {
        Type obj_type; Type result;
    };

    struct ModuleOverloadDetails
    {
        std::vector<OverloadDetailsBinary> plus;
        std::vector<OverloadDetailsBinary> minus;
        std::vector<OverloadDetailsBinary> mul;
        std::vector<OverloadDetailsBinary> div;
        std::vector<OverloadDetailsBinary> mod;
        std::vector<OverloadDetailsBinary> bit_and;
        std::vector<OverloadDetailsBinary> bit_or;
        std::vector<OverloadDetailsBinary> bit_xor;
        std::vector<OverloadDetailsBinary> shl;
        std::vector<OverloadDetailsBinary> shr;
        std::vector<OverloadDetailsBinary> idx;
        std::vector<OverloadDetailsBinary> idx_mut;
        std::vector<OverloadDetailsUnary> un_neg;
        std::vector<OverloadDetailsUnary> un_not;
        std::vector<OverloadDetailsBinary> spaceship;
        std::vector<OverloadDetailsBinary> equal;
    };
}