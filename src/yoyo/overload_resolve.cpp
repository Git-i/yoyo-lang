#include "overload_resolve.h"

#include <ranges>

#include "engine.h"

namespace Yoyo
{
    OverloadDetailsBinary* resolveBin(const Type& lhs, const Type& rhs, TokenType t, IRGenerator* irgen)
    {
        constexpr size_t max_freq = std::numeric_limits<size_t>::max();
        std::vector<std::pair<size_t, OverloadDetailsBinary*>> overloads;
        auto module = lhs.module;
        while(module)
        {
            for(auto& pl_def : module->overloads.binary_details_for(t))
            {
                size_t fric = 0;
                if(auto l_fric = pl_def.left.conversion_friction(lhs, irgen); l_fric != max_freq)
                    fric += l_fric;
                else continue;
                if(auto r_firc = pl_def.right.conversion_friction(rhs, irgen); r_firc != max_freq)
                    fric += r_firc;
                else continue;
                overloads.emplace_back(fric, &pl_def);
            }
            if (module == lhs.module) module = rhs.module;
            if (module == rhs.module) module = nullptr;
        }
        if(overloads.empty()) return nullptr;
        std::pair<size_t, OverloadDetailsBinary*>* result = &overloads.front();
        for(auto& ovl : std::ranges::subrange(overloads.begin()+1, overloads.end()))
        {
            if(ovl.first == result->first) return nullptr; //ambiguous
            if(ovl.first < result->first)
                result = &ovl;
        }
        return result->second;
    }
    OverloadDetailsBinary* resolveAdd(const Type& lhs, const Type& rhs, IRGenerator* irgen)
    {
        return resolveBin(lhs, rhs, TokenType::Plus, irgen);
    }

    OverloadDetailsBinary* resolveSub(const Type& lhs, const Type& rhs, IRGenerator* irgen)
    {
        return resolveBin(lhs, rhs, TokenType::Minus, irgen);
    }

    OverloadDetailsBinary* resolveMul(const Type& lhs, const Type& rhs, IRGenerator* irgen)
    {
        return resolveBin(lhs, rhs, TokenType::Star, irgen);
    }

    OverloadDetailsBinary* resolveDiv(const Type& lhs, const Type& rhs, IRGenerator* irgen)
    {
        return resolveBin(lhs, rhs, TokenType::Slash, irgen);
    }

    OverloadDetailsBinary* resolveRem(const Type& lhs, const Type& rhs, IRGenerator* irgen)
    {
        return resolveBin(lhs, rhs, TokenType::Percent, irgen);
    }
    OverloadDetailsBinary* resolveCmp(const Type& lhs, const Type& rhs, IRGenerator* irgen)
    {
        return resolveBin(lhs, rhs, TokenType::Spaceship, irgen);
    }
}
