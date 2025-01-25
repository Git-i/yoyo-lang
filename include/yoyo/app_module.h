#pragma once
#include "module.h"
#include "parser.h"
#include <func_sig.h>
namespace Yoyo
{
    enum class Result
    {
        Success,
        NullParameter,
        InvalidSignature,
        ParameterCountMismatch,
        ReturnTypeMismatch,
        ParameterTypeMismatch,
        DuplicateEnumValue,
    };
    class YOYO_API AppModule : public Module
    {
        void addFunction(FunctionSignature sig, void* func, std::string name);
    public:
        template<typename R, typename... Args>
        Result addFunction(std::string sig_str, R(*func)(Args...), std::string name)
        {
            if (func == nullptr) return Result::NullParameter;
            Parser p(sig_str);
            auto sig_opt = p.parseFunctionSignature();
            if (p.failed()) return Result::InvalidSignature;
            FunctionSignature sig = std::move(sig_opt).value();
            sig.returnType.saturate(this, nullptr);
            for (auto& param : sig.parameters) param.type.saturate(this, nullptr);
            size_t param_size = sig.returnType.should_sret() + sig.parameters.size();
            if (param_size != sizeof...(Args))
                Result::ParameterCountMismatch;
            if (sig.returnType.should_sret())
            {
                if (!std::is_same_v<R, void>) return Result::ReturnTypeMismatch;
            }
            addFunction(std::move(sig), reinterpret_cast<void*>(func), std::move(name));
            return Result::Success;
        }
        Result addEnum(std::string enum_name, std::unordered_map<std::string, int32_t> values);
    };
}