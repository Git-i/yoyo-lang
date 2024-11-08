#include <app_module.h>
#include <csignal>
#include <parser.h>
#include <llvm/IR/IRBuilder.h>
#include "engine.h"
namespace Yoyo
{
    llvm::FunctionType* toLLVMSignature(const FunctionSignature& sig, AppModule* module)
    {
        llvm::LLVMContext& ctx = *static_cast<llvm::LLVMContext*>(module->engine->llvm_context);
        std::vector<llvm::Type*> argTypes;
        //sret
        auto return_t = module->ToLLVMType(sig.returnType, false, {});
        if(sig.returnType.should_sret())
        {
            argTypes.push_back(llvm::PointerType::get(ctx, 0));
            return_t = llvm::Type::getVoidTy(ctx);
        }
        for(const auto& p : sig.parameters)
        {
            if(!p.type.is_primitive() || p.convention == ParamType::InOut)
                argTypes.push_back(llvm::PointerType::get(ctx, 0));
            else argTypes.push_back(module->ToLLVMType(p.type, false, {}));
        }
        return llvm::FunctionType::get(return_t, argTypes, false);
    }
    void AppModule::addFunction(std::string signature, void* func, std::string name)
    {
        Parser p(std::move(signature));
        auto sig = *p.parseFunctionSignature();
        if(p.failed()) raise(SIGTRAP);
        auto return_as_llvm = ToLLVMType(sig.returnType, false, {});
        llvm::LLVMContext& ctx = *static_cast<llvm::LLVMContext*>(engine->llvm_context);
        auto llvm_sig = toLLVMSignature(sig, this);

        if(functions.contains(name)) return; //TODO: fail on this?
        bool uses_sret = sig.returnType.should_sret();
        functions[name] = std::move(sig);
        std::string mangled_name = module_hash + name;

        auto fn = llvm::Function::Create(llvm_sig, llvm::GlobalValue::ExternalLinkage, mangled_name, code.get());
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


}
