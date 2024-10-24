#include "ir_gen.h"

namespace Yoyo
{
    llvm::Type* ToLLVMType(const Type& type, bool is_ref)
    {

    }
    llvm::FunctionType* ToLLVMSignature(const FunctionSignature& sig)
    {
        std::vector<llvm::Type*> args(sig.parameters.size());
        std::transform(sig.parameters.begin(), sig.parameters.end(), args.begin(),
            [](const FunctionParameter& p)
            {
                return ToLLVMType(p.type, p.convention == ParamType::InOut);
            });
        for(auto arg : args) if(!arg) return nullptr;
        auto return_t = ToLLVMType(sig.returnType, sig.return_is_ref);
        return llvm::FunctionType::get(return_t, args, false);
    }
    void IRGenerator::operator()(FunctionDeclaration* decl)
    {
        auto name = block_hash + std::string{decl->identifier.text};
        if(module->getFunction(name)) return;
        llvm::Function* func = llvm::Function::Create(ToLLVMSignature(decl->signature), llvm::GlobalValue::ExternalLinkage, name, module);
        auto bb = llvm::BasicBlock::Create(context, "entry", func);
        builder->SetInsertPoint(bb);
        auto old_hash = block_hash;
        block_hash = name + "__";
        std::visit(*this, decl->body->toVariant());
    }
    void IRGenerator::operator()(ClassDeclaration* decl)
    {
        std::vector<llvm::Type*> args(decl->vars.size());
        std::transform(decl->vars.begin(), decl->vars.end(), args.begin(),
            [](const ClassVariable& p)
            {
                return ToLLVMType(p.type, false);
            });
        llvm::StructType::create()
    }

    void IRGenerator::operator()(IfStatement* stat)
    {
        auto fn = builder->GetInsertBlock()->getParent();
        auto then_block = llvm::BasicBlock::Create(context, "then", fn);
        builder->CreateCondBr()
    }


    Module IRGenerator::GenerateIR(std::string_view name, std::vector<std::unique_ptr<Statement>> statements)
    {
        Module md
        {
            .code = std::make_unique<llvm::Module>(name, context)
        };
        module = &md;
        code = md.code.get();
        auto bld = std::make_unique<llvm::IRBuilder<>>(context);
        builder = bld.get();
        for(auto& stat : statements)
        {
            std::visit(*this, stat->toVariant());
        }
        builder = nullptr;
        return md;
    }

}