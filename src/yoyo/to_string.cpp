#include <charconv>
#include <csignal>

#include "ir_gen.h"

namespace Yoyo
{
    extern "C" {
        char* YOYO_to_string_signed_integer_dont_use_name(int64_t value, size_t* buffer_length)
        {
            size_t buffer_size = std::snprintf(nullptr, 0, "%lld", value);
            auto buffer = static_cast<char*>(malloc(buffer_size));
            std::to_chars(buffer, buffer + buffer_size + 1, value);
            return buffer;
        }
        char* YOYO_to_string_unsigned_integer_dont_use_name(uint64_t value, size_t* buffer_length)
        {
            size_t buffer_size = std::snprintf(nullptr, 0, "%llu", value);
            auto buffer = static_cast<char*>(malloc(buffer_size));
            std::to_chars(buffer, buffer + buffer_size + 1, value);
            return buffer;
        }
    }
    std::pair<llvm::Value*, llvm::Value*> sint_to_str(llvm::Value* integer, IRGenerator* irgen)
    {
        auto fn = irgen->code->getFunction("YOYO_to_string_signed_integer_dont_use_name");
        auto int64_ty = llvm::Type::getInt64Ty(irgen->context);
        auto ptr_ty = llvm::PointerType::get(irgen->context, 0);
        auto size = irgen->Alloca("int_as_str_size", int64_ty);
        if(!fn)
        {
            auto type = llvm::FunctionType::get(ptr_ty,{int64_ty, ptr_ty}, false);
            llvm::Function::Create(type, llvm::GlobalValue::ExternalLinkage, "YOYO_to_string_signed_integer_dont_use_name", irgen->code);
        }
        auto mem = irgen->builder->CreateCall(fn, {integer, size});
        return {mem, irgen->builder->CreateLoad(int64_ty, size)};
    }
    std::pair<llvm::Value*, llvm::Value*> ExpressionEvaluator::doToStr(llvm::Value* val, const Type& tp)
    {
        if(tp.is_signed_integral())
        {
            auto as64 = irgen->builder->CreateSExt(val, llvm::Type::getInt64Ty(irgen->context));
            return sint_to_str(as64, irgen);
        }
        raise(SIGTRAP);
    }
}