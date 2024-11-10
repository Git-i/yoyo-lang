#include <charconv>
#include <csignal>

#include "ir_gen.h"

namespace Yoyo
{
    extern "C" {
        char* YOYO_to_string_signed_integer_dont_use_name(int64_t value, size_t* buffer_length)
        {
            *buffer_length = std::snprintf(nullptr, 0, "%ld", value);
            auto buffer = static_cast<char*>(malloc(*buffer_length));
            std::to_chars(buffer, buffer + *buffer_length + 1, value);
            return buffer;
        }
        char* YOYO_to_string_unsigned_integer_dont_use_name(uint64_t value, size_t* buffer_length)
        {
            *buffer_length = std::snprintf(nullptr, 0, "%lu", value);
            auto buffer = static_cast<char*>(malloc(*buffer_length));
            std::to_chars(buffer, buffer + *buffer_length + 1, value);
            return buffer;
        }
        char* YOYO_to_string_enum_dont_use_name(int32_t value, EnumDeclaration* decl, size_t* buffer_length)
        {
            auto& size = *buffer_length;
            std::string str = "<unknown>";
            auto it = std::ranges::find_if(decl->values,
                [value](std::pair<const std::string, int32_t>& val){ return val.second ==  value; });
            if (it != decl->values.end()) str = it->first;
            size = str.size();
            auto buffer = static_cast<char*>(malloc(size));
            memcpy(buffer, str.data(), size);
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
            fn = llvm::Function::Create(type, llvm::GlobalValue::ExternalLinkage, "YOYO_to_string_signed_integer_dont_use_name", irgen->code);
        }
        auto mem = irgen->builder->CreateCall(fn, {integer, size});
        return {mem, irgen->builder->CreateLoad(int64_ty, size)};
    }
    std::pair<llvm::Value*, llvm::Value*> uint_to_str(llvm::Value* integer, IRGenerator* irgen)
    {
        auto fn = irgen->code->getFunction("YOYO_to_string_unsigned_integer_dont_use_name");
        auto int64_ty = llvm::Type::getInt64Ty(irgen->context);
        auto ptr_ty = llvm::PointerType::get(irgen->context, 0);
        auto size = irgen->Alloca("int_as_str_size", int64_ty);
        if(!fn)
        {
            auto type = llvm::FunctionType::get(ptr_ty,{int64_ty, ptr_ty}, false);
            fn = llvm::Function::Create(type, llvm::GlobalValue::ExternalLinkage, "YOYO_to_string_unsigned_integer_dont_use_name", irgen->code);
        }
        auto mem = irgen->builder->CreateCall(fn, {integer, size});
        return {mem, irgen->builder->CreateLoad(int64_ty, size)};
    }
    llvm::Function* enum_to_string_fn(IRGenerator* irgen)
    {
        auto fn = irgen->code->getFunction("YOYO_to_string_enum_dont_use_name");
        if(!fn)
        {
            auto int32_ty = llvm::Type::getInt32Ty(irgen->context);
            auto ptr_ty = llvm::PointerType::get(irgen->context, 0);
            auto type = llvm::FunctionType::get(ptr_ty, {int32_ty, ptr_ty, ptr_ty}, false);
            fn = llvm::Function::Create(type, llvm::GlobalValue::ExternalLinkage, "YOYO_to_string_enum_dont_use_name", irgen->code);
        }
        return fn;
    }
    std::pair<llvm::Value*, llvm::Value*> ExpressionEvaluator::doToStr(llvm::Value* val, const Type& tp)
    {   
        if(tp.is_signed_integral())
        {
            auto as64 = irgen->builder->CreateSExt(val, llvm::Type::getInt64Ty(irgen->context));
            return sint_to_str(as64, irgen);
        }
        if(tp.is_unsigned_integral())
        {
            auto as64 = irgen->builder->CreateZExt(val, llvm::Type::getInt64Ty(irgen->context));
            return uint_to_str(as64, irgen);
        }
        if(tp.is_enum())
        {
            auto decl = tp.module->enums[tp.name].get();
            auto size = irgen->Alloca("enum_to_str_size", llvm::Type::getInt64Ty(irgen->context));
            auto fn = enum_to_string_fn(irgen);
            auto llvm_decl = irgen->builder->CreateIntToPtr(
                llvm::ConstantInt::get(llvm::Type::getInt64Ty(irgen->context), reinterpret_cast<std::uintptr_t>(decl)),
                llvm::PointerType::get(irgen->context, 0)
            );
            return {
                irgen->builder->CreateCall(fn, {val, llvm_decl, size}),
                irgen->builder->CreateLoad(llvm::Type::getInt64Ty(irgen->context), size)
            };
        }
        raise(SIGTRAP);
    }
}