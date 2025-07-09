#include "overload_resolve.h"

#include <ranges>

#include "engine.h"
#include <token.h>
#include "module.h"
#include "ir_gen.h"
namespace Yoyo
{
    std::pair<std::string, OverloadDetailsBinary*> resolveBin(const Type& lhs, const Type& rhs, TokenType t, IRGenerator* irgen)
    {
        constexpr size_t max_freq = std::numeric_limits<size_t>::max();
        std::vector<std::pair<size_t, std::pair<std::string, OverloadDetailsBinary*>>> overloads;
        std::array<ModuleBase*, 4> modules{ lhs.module, lhs.deref().module, rhs.module, rhs.deref().module };
        std::array<ModuleBase*, 4> encountered{};
        size_t enc_idx = 0;
        auto md = modules | std::views::transform([&encountered, &enc_idx](auto& module) {
            auto it = std::ranges::find(encountered, module);
            if (it == encountered.end()) {
                encountered[enc_idx++] = module; return module;
            }
            return static_cast<ModuleBase*>(nullptr);
            });
        for(auto module : md)
        {
            if (module == nullptr) continue;
            // TODO: replace with better visibility mechanism
            for (auto& [hash, pl_def] : module->overloads.binary_details_for(t) | std::views::filter([irgen](auto& in) { 
                return irgen->block_hash.starts_with(in.first);
                }))
            {
                std::swap(hash, irgen->block_hash);
                pl_def.left.saturate(module, irgen);
                pl_def.right.saturate(module, irgen);
                std::swap(hash, irgen->block_hash);

                size_t fric = 0;
                if(auto l_fric = pl_def.left.conversion_friction(lhs, irgen); l_fric != max_freq)
                    fric += l_fric;
                else continue;
                if(auto r_firc = pl_def.right.conversion_friction(rhs, irgen); r_firc != max_freq)
                    fric += r_firc;
                else continue;
                overloads.emplace_back(fric, std::pair<std::string, OverloadDetailsBinary*>{ hash, &pl_def });
            }
        }
        if (overloads.empty()) return { "",nullptr };
        std::pair<size_t, std::pair<std::string, OverloadDetailsBinary*>>* result = &overloads.front();
        // find the minimum friction overload
        for(auto& ovl : std::ranges::subrange(overloads.begin()+1, overloads.end()))
        {
            if(ovl.first < result->first)
                result = &ovl;
        }
        // check if multiple overloads have the minimum friction
        for (auto& ovl : overloads) if (ovl.first == result->first && result != &ovl) return { "", nullptr };//ambiguos
        return result->second;
    }
    std::pair<std::string, OverloadDetailsBinary*> resolveAdd(const Type& lhs, const Type& rhs, IRGenerator* irgen)
    {
        return resolveBin(lhs, rhs, TokenType::Plus, irgen);
    }

    std::pair<std::string, OverloadDetailsBinary*> resolveSub(const Type& lhs, const Type& rhs, IRGenerator* irgen)
    {
        return resolveBin(lhs, rhs, TokenType::Minus, irgen);
    }

    std::pair<std::string, OverloadDetailsBinary*> resolveMul(const Type& lhs, const Type& rhs, IRGenerator* irgen)
    {
        return resolveBin(lhs, rhs, TokenType::Star, irgen);
    }

    std::pair<std::string, OverloadDetailsBinary*> resolveDiv(const Type& lhs, const Type& rhs, IRGenerator* irgen)
    {
        return resolveBin(lhs, rhs, TokenType::Slash, irgen);
    }

    std::pair<std::string, OverloadDetailsBinary*> resolveRem(const Type& lhs, const Type& rhs, IRGenerator* irgen)
    {
        return resolveBin(lhs, rhs, TokenType::Percent, irgen);
    }
    std::pair<std::string, OverloadDetailsBinary*> resolveCmp(const Type& lhs, const Type& rhs, IRGenerator* irgen)
    {
        return resolveBin(lhs, rhs, TokenType::Spaceship, irgen);
    }
    std::pair<std::string, OverloadDetailsBinary*> resolveShl(const Type& lhs, const Type& rhs, IRGenerator* irgen)
    {
        return resolveBin(lhs, rhs, TokenType::DoubleLess, irgen);
    }
    std::pair<std::string, OverloadDetailsBinary*> resolveShr(const Type& lhs, const Type& rhs, IRGenerator* irgen)
    {
        return resolveBin(lhs, rhs, TokenType::DoubleGreater, irgen);
    }
    std::pair<std::string, OverloadDetailsBinary*> resolveIdx(const Type& lhs, const Type& rhs, IRGenerator* irgen)
    {
        return resolveBin(lhs, rhs, TokenType::SquarePair, irgen);
    }
    std::pair<std::string, OverloadDetailsBinary*> resolveIdxMut(const Type& lhs, const Type& rhs, IRGenerator* irgen)
    {
        return resolveBin(lhs, rhs, TokenType::SquarePairMut, irgen);
    }
}
