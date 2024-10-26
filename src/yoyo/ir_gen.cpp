#include "ir_gen.h"

#include <set>

namespace Yoyo
{
    llvm::Type* IRGenerator::ToLLVMType(const Type& type, bool is_ref)
    {
        if(type.is_integral())
            return llvm::Type::getIntNTy(context, *type.integer_width());
        if(type.is_floating_point())
            return *type.float_width() == 32 ? llvm::Type::getFloatTy(context) : llvm::Type::getDoubleTy(context);
        if(type.is_boolean())
            return llvm::Type::getInt1Ty(context);
        if(type.name == "void")
            return llvm::Type::getVoidTy(context);
        for(size_t i = types.size(); i > 0; i--)
        {
            auto idx = i - 1;
            if(auto t = types[idx].find(type.name); t != types[idx].end())
            {
                return t->second.first;
            }
        }
        if(auto t = module->classes.find(type.name); t != module->classes.end()) return t->second.first;
        error();
        return nullptr;
    }
    llvm::FunctionType* IRGenerator::ToLLVMSignature(const FunctionSignature& sig)
    {
        std::vector<llvm::Type*> args(sig.parameters.size());
        std::transform(sig.parameters.begin(), sig.parameters.end(), args.begin(),
            [this](const FunctionParameter& p)
            {
                return ToLLVMType(p.type, p.convention == ParamType::InOut);
            });
        for(auto arg : args) if(!arg) return nullptr;
        auto return_t = ToLLVMType(sig.returnType, sig.return_is_ref);
        return llvm::FunctionType::get(return_t, args, false);
    }
    llvm::AllocaInst* IRGenerator::Alloca(std::string_view name, llvm::Type* type)
    {
        auto entry_block = &builder->GetInsertBlock()->getParent()->getEntryBlock();
        llvm::IRBuilder<> temp(entry_block,
                 entry_block->begin());
        return temp.CreateAlloca(type, nullptr, name);
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
        pushScope();
        std::visit(*this, decl->body->toVariant());
        popScope();
        block_hash = old_hash;
    }
    void IRGenerator::operator()(ExpressionStatement* stat)
    {
        std::visit(ExpressionEvaluator{this}, stat->expression->toVariant());
    }

    void IRGenerator::operator()(ClassDeclaration* decl)
    {
        std::string name(decl->identifier.text);
        if(isShadowing(name))
        {
            error();
            return;
        }
        types.back()[name] = {hanldeClassDeclaration(decl, true), decl};
    }
    void IRGenerator::operator()(VariableDeclaration* decl)
    {
        //TODO implicit conversion and validation
        std::string name(decl->identifier.text);
        if(isShadowing(name))
        {
            error(); return;
        }
        auto type = decl->type ? decl->type.value() : std::visit(ExpressionTypeChecker{this}, decl->initializer->toVariant());
        if(!type)
        {
            error();
            return;
        }
        auto alloc = Alloca(decl->identifier.text, ToLLVMType(type.value(), false));
        if(decl->initializer)
        {
            //TODO actual initialization
            builder->CreateStore(std::visit(ExpressionEvaluator{this}, decl->initializer->toVariant()), alloc);
        }
        variables.back()[name] = {alloc, decl};
    }
    void IRGenerator::operator()(BlockStatement* stat)
    {
        pushScope();
        for(auto& sub_stat : stat->statements)
        {
            std::visit(*this, sub_stat->toVariant());
        }
        popScope();
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
        if(stat->else_stat) else_bb = llvm::BasicBlock::Create(context, "else", fn);
        auto merge_bb = llvm::BasicBlock::Create(context, "ifcont", fn);
        builder->CreateCondBr(
            std::visit(ExpressionEvaluator{this}, stat->condition->toVariant()), then_bb,
            else_bb ? else_bb : merge_bb);
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

    llvm::StructType* IRGenerator::hanldeClassDeclaration(ClassDeclaration* decl, bool is_anon)
    {
        std::vector<std::string> var_names(decl->vars.size());
        std::vector<std::string> fn_names(decl->methods.size());
        std::ranges::transform(decl->vars, var_names.begin(), [](ClassVariable& var)
        {
            return var.name;
        });
        std::ranges::transform(decl->methods, fn_names.begin(), [](ClassMethod& method)
        {
            return method.name;
        });
        for(const auto& name: var_names)
            if(std::ranges::find(fn_names, name) != fn_names.end()){error(); return nullptr;}

        for(const auto& name: fn_names)
            if(std::ranges::find(var_names, name) != var_names.end()){error(); return nullptr;}
        for(size_t i = 0; i < var_names.size(); ++i)
        {
            for(size_t j = 0; j < var_names.size(); ++j)
            {
                if(j == i) continue;
                if(var_names[i] == var_names[j]) {error(); return nullptr;}
            }
        }
        for(size_t i = 0; i < fn_names.size(); ++i)
        {
            for(size_t j = 0; j < fn_names.size(); ++j)
            {
                if(j == i) continue;
                if(fn_names[i] == fn_names[j]) {error(); return nullptr;}
            }
        }

        std::vector<llvm::Type*> args(decl->vars.size());
        std::transform(decl->vars.begin(), decl->vars.end(), args.begin(),
            [this](const ClassVariable& p)
            {
                return ToLLVMType(p.type, false);
            });
        if(is_anon)
        {
            return llvm::StructType::get(context, args);
        }
        std::string name(decl->identifier.text);
        return llvm::StructType::create(context, args, name);
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
        pushScope();
        for(auto& stat : statements)
        {
            std::variant<ClassDeclaration*, FunctionDeclaration*> vnt;
            if(auto ptr = dynamic_cast<ClassDeclaration*>(stat.get())) vnt = ptr;
            if(auto ptr = dynamic_cast<FunctionDeclaration*>(stat.get())) vnt = ptr;
            std::visit(TopLevelVisitor{this}, vnt);
        }
        builder = nullptr;
        return md;
    }

}
