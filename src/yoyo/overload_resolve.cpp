#include "overload_resolve.h"

#include <ranges>

#include "engine.h"

namespace Yoyo
{
    template<typename Fn>
    OverloadDetailsBinary* resolveBin(const Type& lhs, const Type& rhs, Fn&& get_vec)
    {
        constexpr size_t max_freq = std::numeric_limits<size_t>::max();
        std::vector<std::pair<size_t, OverloadDetailsBinary*>> overloads;
        for(auto module : lhs.module == rhs.module ?
            std::initializer_list{lhs.module} :
            std::initializer_list{lhs.module, rhs.module})
        {
            for(auto& pl_def : get_vec(module->overloads))
            {
                size_t fric = 0;
                if(auto l_fric = pl_def.left.conversion_friction(lhs); l_fric != max_freq)
                    fric += l_fric;
                else continue;
                if(auto r_firc = pl_def.right.conversion_friction(rhs); r_firc != max_freq)
                    fric += r_firc;
                else continue;
                overloads.emplace_back(fric, &pl_def);
            }
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
    OverloadDetailsBinary* resolveAdd(const Type& lhs, const Type& rhs)
    {
        return resolveBin(lhs, rhs, [](ModuleOverloadDetails& det)->auto& {return det.plus;});
    }

    OverloadDetailsBinary* resolveSub(const Type& lhs, const Type& rhs)
    {
        return resolveBin(lhs, rhs, [](ModuleOverloadDetails& det)->auto& {return det.minus;});
    }

    OverloadDetailsBinary* resolveMul(const Type& lhs, const Type& rhs)
    {
        return resolveBin(lhs, rhs, [](ModuleOverloadDetails& det)->auto& {return det.mul;});
    }

    OverloadDetailsBinary* resolveDiv(const Type& lhs, const Type& rhs)
    {
        return resolveBin(lhs, rhs, [](ModuleOverloadDetails& det)->auto& {return det.div;});
    }

    OverloadDetailsBinary* resolveRem(const Type& lhs, const Type& rhs)
    {
        return resolveBin(lhs, rhs, [](ModuleOverloadDetails& det)->auto& {return det.mod;});
    }
}
