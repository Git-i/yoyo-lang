#pragma once
#include <csignal>

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
        std::string mangled_name(const TokenType t) const
        {
            std::string middle;
            switch (t)
            {
            case TokenType::Plus: middle = "plus__"; break;
            case TokenType::Minus: middle = "minus__"; break;
            case TokenType::Star: middle = "mul__"; break;
            case TokenType::Slash: middle = "div__"; break;
            default: raise(SIGTRAP);
            }
            return "__operator__" + middle + left.full_name() + "__" + right.full_name();
        }
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
        std::vector<OverloadDetailsBinary>* binary_details_for(TokenType t)
        {
            switch (t)
            {
            case TokenType::Plus: return &plus;
            case TokenType::Minus: return &minus;
            case TokenType::Star: return &mul;
            case TokenType::Slash: return &div;
            case TokenType::Percent: return &mod;
            default: return nullptr;
            }
        }
    };
}
