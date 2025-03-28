#include <app_module.h>
#include <csignal>
#include <ranges>
#include <parser.h>
#include <llvm/IR/IRBuilder.h>
#include "engine.h"
namespace Yoyo
{
    llvm::FunctionType* toLLVMSignature(const FunctionSignature& sig, AppModule* module)
    {
        llvm::LLVMContext& ctx = *module->engine->llvm_context.getContext();
        std::vector<llvm::Type*> argTypes;
        //sret
        auto return_t = module->ToLLVMType(sig.returnType, module->module_hash, nullptr, {});
        if(sig.returnType.should_sret())
        {
            argTypes.push_back(llvm::PointerType::get(ctx, 0));
            return_t = llvm::Type::getVoidTy(ctx);
        }
        for(const auto& p : sig.parameters)
        {
            if(p.type.should_sret())
                argTypes.push_back(llvm::PointerType::get(ctx, 0));
            else argTypes.push_back(module->ToLLVMType(p.type, module->module_hash, nullptr, {}));
        }
        return llvm::FunctionType::get(return_t, argTypes, false);
    }
    void AppModule::addFunction(FunctionSignature sig, void* func, std::string name)
    {
        auto return_as_llvm = ToLLVMType(sig.returnType, module_hash, nullptr, {});
        llvm::LLVMContext& ctx = *engine->llvm_context.getContext();
        auto llvm_sig = toLLVMSignature(sig, this);


        bool uses_sret = sig.returnType.should_sret();
        std::string mangled_name = module_hash + name;
        functions[module_hash].emplace_back(name, std::move(sig), std::vector{ Attribute{"public"} });

        auto fn = llvm::Function::Create(llvm_sig, llvm::GlobalValue::ExternalLinkage, mangled_name, code.getModuleUnlocked());
        if(uses_sret)
            fn->addAttributeAtIndex(1, llvm::Attribute::get(ctx, llvm::Attribute::StructRet, return_as_llvm));
        auto bb = llvm::BasicBlock::Create(ctx, "entry", fn);
        llvm::IRBuilder<> builder(bb);
        auto addr = llvm::ConstantInt::get(llvm::Type::getIntNTy(ctx, sizeof func * 8), reinterpret_cast<std::uintptr_t>(func));
        auto addr_as_ptr = builder.CreateIntToPtr(addr, llvm::PointerType::get(ctx, 0));
        std::vector<llvm::Value*> args;
        for(auto& arg : fn->args())
        {
            args.push_back(&arg);
        }
        auto ret = builder.CreateCall(llvm_sig, addr_as_ptr, args);
        if(llvm_sig->getReturnType()->isVoidTy())
            builder.CreateRetVoid();
        else
            builder.CreateRet(ret);
    }

    Result AppModule::addEnum(std::string enum_name, std::unordered_map<std::string, int32_t> values)
    {
        //auto st = std::set{ values | std::views::values };
        //if (st.size() != values.size()) return Result::DuplicateEnumValue;
        enums[module_hash].emplace_back(std::make_unique<EnumDeclaration>(std::move(enum_name), std::move(values), std::vector<std::unique_ptr<Statement>>{}));
        return Result::Success;
    }
    Result AppModule::addClass(std::string class_name, std::vector<ClassVariable> variables, Ownership sh, std::string block_hash)
    {
        auto hash = module_hash + block_hash;
        classes[hash].emplace_back(hash + class_name + "::", nullptr, 
            std::make_unique<ClassDeclaration>(
                Token{ .text = class_name },
                std::move(variables), 
                std::vector<std::unique_ptr<Statement>>{}, 
                sh, 
                std::vector<InterfaceImplementation>{}
            ));
        return Result::Success;;
    }
}
