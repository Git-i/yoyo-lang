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

    bool IRGenerator::isShadowing(const std::string& name) const
    {
        for(auto& map : variables)
            if(map.contains(name)) return true;
        for(auto& map : types)
            if(map.contains(name)) return true;
        return false;
    }

    void IRGenerator::operator()(FunctionDeclaration* decl)
    {
        auto name = block_hash + std::string{decl->identifier.text};
        if(code->getFunction(name))
        {
            error();
            return;
        }
        llvm::Function* func = llvm::Function::Create(ToLLVMSignature(decl->signature), llvm::GlobalValue::ExternalLinkage, name, code);
        auto bb = llvm::BasicBlock::Create(context, "entry", func);
        builder->SetInsertPoint(bb);
        auto old_hash = block_hash;
        block_hash = name + "__";
        std::visit(*this, decl->body->toVariant());
    }
    void IRGenerator::operator()(ExpressionStatement* stat)
    {
        std::visit(ExpressionEvaluator{this}, stat->expression->toVariant());
    }

    void IRGenerator::operator()(ClassDeclaration* decl)
    {
        std::string name(decl->identifier.text);
        std::vector<llvm::Type*> args(decl->vars.size());
        if(isShadowing(name))
        {
            error();
            return;
        }
        std::transform(decl->vars.begin(), decl->vars.end(), args.begin(),
            [](const ClassVariable& p)
            {
                return ToLLVMType(p.type, false);
            });
        types.back()[name] = {llvm::StructType::get(context, args), decl};
    }
    void IRGenerator::operator()(VariableDeclaration* decl)
    {
        std::string name(decl->identifier.text);
        if(isShadowing(name))
        {
            error(); return;
        }
        auto entry_block = &builder->GetInsertBlock()->getParent()->getEntryBlock();
        llvm::IRBuilder<> temp(entry_block,
                 entry_block->begin());
        auto type = decl->type ? decl->type.value() : std::visit(ExpressionTypeChecker{this}, decl->initializer->toVariant());
        if(!type)
        {
            error();
            return;
        }
        auto alloc = temp.CreateAlloca(ToLLVMType(type.value(), false), nullptr, decl->identifier.text);
        variables.back()[name] = {alloc, decl};
    }
    void IRGenerator::operator()(BlockStatement* stat)
    {
        pushScope();
        for(auto& stat : stat->statements)
        {
            std::visit(*this, stat->toVariant());
        }
    }
    void IRGenerator::operator()(ForStatement*)
    {

    }
    void IRGenerator::operator()(WhileStatement* expr)
    {
        if(!std::visit(ExpressionTypeChecker{this}, expr->condition->toVariant())->is_boolean())
        {
            error();
            return;
        }
        auto while_bb = llvm::BasicBlock::Create(context, "while", builder->GetInsertBlock()->getParent());
        auto then_bb = llvm::BasicBlock::Create(context, "loopthen");
        auto cont_bb = llvm::BasicBlock::Create(context, "loopcont");
        builder->SetInsertPoint(while_bb);
        auto value = std::visit(ExpressionEvaluator{this}, expr->condition->toVariant());
        builder->CreateCondBr(value, then_bb, cont_bb);
        builder->SetInsertPoint(then_bb);
        std::visit(*this, expr->body->toVariant());
        builder->CreateBr(while_bb);
        builder->SetInsertPoint(cont_bb);
    }
    void IRGenerator::error()
    {
        throw std::runtime_error("IRGenerator error");
    }

    void IRGenerator::operator()(ReturnStatement* stat)
    {
        if(stat->expression)
            builder->CreateRet(std::visit(ExpressionEvaluator{this}, stat->expression->toVariant()));
        else
            builder->CreateRetVoid();
    }

    void IRGenerator::operator()(IfStatement* stat)
    {
        auto fn = builder->GetInsertBlock()->getParent();
        auto expr_type = std::visit(ExpressionTypeChecker{this}, stat->condition->toVariant());
        if(!expr_type->is_boolean())
        {
            error(); return;
        }
        auto then_bb = llvm::BasicBlock::Create(context, "then", fn);
        llvm::BasicBlock* else_bb = nullptr;
        if(stat->else_stat) else_bb = llvm::BasicBlock::Create(context, "else");
        auto merge_bb = llvm::BasicBlock::Create(context, "ifcont");
        builder->CreateCondBr(
            std::visit(ExpressionEvaluator{this}, stat->condition->toVariant()), then_bb, else_bb);
        builder->SetInsertPoint(then_bb);
        std::visit(*this, stat->then_stat->toVariant());
        builder->CreateBr(merge_bb);
        if(stat->else_stat)
        {
            builder->SetInsertPoint(else_bb);
            std::visit(*this, stat->else_stat->toVariant());
            builder->CreateBr(merge_bb);
        }
        builder->SetInsertPoint(merge_bb);
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
        variables.emplace_back();
        for(auto& stat : statements)
        {
            std::visit(*this, stat->toVariant());
        }
        builder = nullptr;
        return md;
    }

}