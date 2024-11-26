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
        if(tp.is_optional())
        {
            auto fn = irgen->builder->GetInsertBlock()->getParent();
            auto as_llvm = irgen->ToLLVMType(tp, false);
            auto if_null = llvm::BasicBlock::Create(irgen->context, "opt_is_null", fn, irgen->returnBlock);
            auto if_valid = llvm::BasicBlock::Create(irgen->context, "opt_is_valid", fn, irgen->returnBlock);
            auto cont = llvm::BasicBlock::Create(irgen->context, "opt_str_cont", fn, irgen->returnBlock);

            auto is_valid = irgen->builder->CreateLoad(llvm::Type::getInt1Ty(irgen->context),
                irgen->builder->CreateStructGEP(as_llvm, val, 1));


            irgen->builder->CreateCondBr(is_valid, if_valid, if_null);
            irgen->builder->SetInsertPoint(if_valid);
            auto sub_val = irgen->builder->CreateStructGEP(as_llvm, val, 0);
            if(!tp.subtypes[0].should_sret())
            {
                auto subtype = llvm::dyn_cast<llvm::StructType>(as_llvm)->getElementType(0);
                sub_val = irgen->builder->CreateLoad(subtype, sub_val);
            }
            auto [ptr_v, size_v] = doToStr(sub_val, tp.subtypes[0]);
            irgen->builder->CreateBr(cont);

            irgen->builder->SetInsertPoint(if_null);
            static auto null_str = irgen->builder->CreateGlobalString("null");
            auto size = llvm::ConstantInt::get(llvm::Type::getInt64Ty(irgen->context), 4);
            auto ptr = irgen->Malloc("null_str", size);
            irgen->builder->CreateMemCpy(ptr, std::nullopt, null_str, std::nullopt, size);
            irgen->builder->CreateBr(cont);

            irgen->builder->SetInsertPoint(cont);
            auto ptr_phi = irgen->builder->CreatePHI(llvm::PointerType::get(irgen->context, 0), 2);
            auto value_phi = irgen->builder->CreatePHI(llvm::Type::getInt64Ty(irgen->context), 2);

            ptr_phi->addIncoming(ptr_v, if_valid);
            ptr_phi->addIncoming(ptr, if_null);

            value_phi->addIncoming(size_v, if_valid);
            value_phi->addIncoming(size, if_null);

            return {ptr_phi, value_phi};
        }
        if(tp.is_tuple())
        {
            auto as_llvm = irgen->ToLLVMType(tp, false);
            std::vector<std::pair<llvm::Value*, llvm::Value*>> args;
            size_t idx = 0;
            for(auto& sub: tp.subtypes)
            {
                auto sub_val = irgen->builder->CreateStructGEP(as_llvm, val, idx);
                if(!sub.should_sret()) sub_val = irgen->builder->CreateLoad(irgen->ToLLVMType(sub, false), sub_val);
                args.push_back(doToStr(sub_val, sub));
                idx++;
            }
            // target output is (val, val2, val3...),
            // hence the total size is the size of inputs plus 2(, ) for every input other than the first plus 2 for the
            // open and close bracket
            auto total_size = irgen->builder->CreateAdd(args[0].second,
                llvm::ConstantInt::get(llvm::Type::getInt64Ty(irgen->context), 2));
            for(const auto& arg : std::ranges::subrange(args.begin() + 1, args.end()))
            {
                total_size = irgen->builder->CreateAdd(total_size, irgen->builder->CreateAdd(
                    llvm::ConstantInt::get(llvm::Type::getInt64Ty(irgen->context), 2),
                    arg.second));
            }
            auto memory = irgen->Malloc("tuple_to_string", total_size);
            irgen->builder->CreateStore(
                llvm::ConstantInt::get(llvm::Type::getInt8Ty(irgen->context), '('), memory);
            llvm::Value* offset = llvm::ConstantInt::get(llvm::Type::getInt64Ty(irgen->context), 1);
            auto i8_ty = llvm::Type::getInt8Ty(irgen->context);

            auto current_ptr = irgen->builder->CreateGEP(i8_ty, memory, offset);
            irgen->builder->CreateMemCpy(current_ptr, std::nullopt, args[0].first, std::nullopt, args[0].second);
            irgen->Free(args[0].first);
            offset = irgen->builder->CreateAdd(offset, args[0].second);

            auto const_one = llvm::ConstantInt::get(llvm::Type::getInt64Ty(irgen->context), 1);
            for(auto& arg : std::ranges::subrange(args.begin() + 1, args.end()))
            {
                current_ptr = irgen->builder->CreateGEP(i8_ty, memory, offset);
                irgen->builder->CreateStore(llvm::ConstantInt::get(i8_ty, ','), current_ptr);
                offset = irgen->builder->CreateAdd(offset, const_one);
                current_ptr = irgen->builder->CreateGEP(i8_ty, memory, offset);
                irgen->builder->CreateStore(llvm::ConstantInt::get(i8_ty, ' '), current_ptr);
                offset = irgen->builder->CreateAdd(offset, const_one);
                current_ptr = irgen->builder->CreateGEP(i8_ty, memory, offset);
                irgen->builder->CreateMemCpy(current_ptr, std::nullopt, arg.first, std::nullopt, arg.second);
                offset = irgen->builder->CreateAdd(offset, arg.second);
                irgen->Free(arg.first);
            }
            current_ptr = irgen->builder->CreateGEP(i8_ty, memory, offset);
            irgen->builder->CreateStore(llvm::ConstantInt::get(i8_ty, ')'), current_ptr);
            return {memory, total_size};
        }
        raise(SIGTRAP);
    }
}