#pragma once
#include <csignal>
#include <span>
namespace Yoyo
{
    void YOYO_API debugbreak();
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
            case TokenType::Spaceship: middle = "cmp__"; break;
            default: debugbreak();
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
        std::vector<OverloadDetailsBinary> bin_overloads;
        std::vector<OverloadDetailsUnary> un_overloads;
        // offset order
        //minus;
        //mul;
        //div;
        //mod;
        //cmp;
        //bit_and;
        //bit_or;
        //bit_xor;
        //shl;
        //shr;
        //idx;
        //idx_mut;
        //spaceship;
        //equal;
        std::array<size_t, 12> offsets;
        std::array<size_t, 1> un_offsets;
        void add_binary_detail_for(TokenType t, OverloadDetailsBinary bin)
        {
            size_t off = 0;
            switch (t)
            {
            case TokenType::Plus: break;
            case TokenType::Minus: off = 1; break;
            case TokenType::Star: off = 2; break;
            case TokenType::Slash: off = 3; break;
            case TokenType::Percent: off = 4; break;
            case TokenType::Spaceship: off = 5; break;
            }
            bin_overloads.insert(bin_overloads.begin() + off, std::move(bin));
            for (auto& elem : std::ranges::subrange(offsets.begin() + off, offsets.end())) elem++;
        }
        void add_binary_detail_for(TokenType t, Type l, Type r, Type res)
        {
            size_t off = 0;
            switch (t)
            {
            case TokenType::Plus: break;
            case TokenType::Minus: off = 1; break;
            case TokenType::Star: off = 2; break;
            case TokenType::Slash: off = 3; break;
            case TokenType::Percent: off = 4; break;
            case TokenType::Spaceship: off = 5; break;
            }
            size_t actual_off = off == 0 ? 0 : offsets[off];
            bin_overloads.emplace(bin_overloads.begin() + actual_off, std::move(l), std::move(r), std::move(res));
            for (auto& elem : std::ranges::subrange(offsets.begin() + off, offsets.end())) elem++;
        }
        std::span<OverloadDetailsBinary> binary_details_for(TokenType t)
        {
            switch (t)
            {
            case TokenType::Plus: return std::span{ bin_overloads.begin(), bin_overloads.begin() + offsets[0] };
            case TokenType::Minus: return std::span{ bin_overloads.begin() + offsets[0], bin_overloads.begin() + offsets[1] };
            case TokenType::Star: return std::span{ bin_overloads.begin() + offsets[1], bin_overloads.begin() + offsets[2] };
            case TokenType::Slash: return std::span{ bin_overloads.begin() + offsets[2], bin_overloads.begin() + offsets[3] };
            case TokenType::Percent: return std::span{ bin_overloads.begin() + offsets[3], bin_overloads.begin() + offsets[4] };
            case TokenType::Spaceship: return std::span{ bin_overloads.begin() + offsets[4], bin_overloads.begin() + offsets[5] };
            default: return {};
            }
        }
    };
}
