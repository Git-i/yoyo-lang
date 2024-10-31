#include "ir_gen.h"

#include <csignal>
#include <list>
#include <set>

namespace Yoyo
{
    std::tuple<std::string, llvm::StructType*, ClassDeclaration*>* IRGenerator::findType(const std::string& name)
    {
        for(size_t i = types.size(); i > 0; i--)
        {
            auto idx = i - 1;
            if(auto t = types[idx].find(name); t != types[idx].end())
            {
                return &t->second;
            }
        }
        if(auto t = module->classes.find(name); t != module->classes.end()) return &t->second;
        return nullptr;
    }
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
        if(type.is_opaque_pointer())
            return llvm::PointerType::get(context, 0);
        if(type.is_lambda())
        {
            if(auto t = lambdas.find(type.name); t != lambdas.end())
                return t->second.second;
        }
        if(in_class && type.name == "This") return ToLLVMType(this_t, is_ref);
        for(size_t i = types.size(); i > 0; i--)
        {
            auto idx = i - 1;
            if(auto t = types[idx].find(type.name); t != types[idx].end())
            {
                return std::get<1>(t->second);
            }
        }
        if(auto t = module->classes.find(type.name); t != module->classes.end()) return std::get<1>(t->second);
        error();
        return nullptr;
    }
    llvm::FunctionType* IRGenerator::ToLLVMSignature(const FunctionSignature& sig)
    {
        bool use_sret = !sig.returnType.is_primitive();
        std::vector<llvm::Type*> args(sig.parameters.size() + use_sret);
        //Non primitives are passed via pointers and returned via pointers(sret)
        auto return_t = ToLLVMType(sig.returnType, sig.return_is_ref);
        if(use_sret)
        {
            args[0] = return_t->getPointerTo();
            return_t = llvm::Type::getVoidTy(context);
        }
        std::ranges::transform(sig.parameters, args.begin() + use_sret, [this](const FunctionParameter& p)
        {
            auto t = ToLLVMType(p.type, p.convention == ParamType::InOut);
            if(!p.type.is_primitive() || p.convention == ParamType::InOut)
                t = t->getPointerTo();
            return t;
        });

        for(auto arg : args) if(!arg) return nullptr;

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
        std::unique_ptr<llvm::IRBuilder<>> oldBuilder;
        auto old_return = return_t;
        auto old_ret_addr = currentReturnAddress;
        auto old_ret_block = returnBlock;

        if(builder->GetInsertBlock()) oldBuilder = std::make_unique<llvm::IRBuilder<>>(builder->GetInsertBlock(), builder->GetInsertPoint());
        auto name = block_hash + std::string{decl->identifier.text};
        if(code->getFunction(name))
        {
            error();
            return;
        }
        llvm::Function* func = llvm::Function::Create(ToLLVMSignature(decl->signature), llvm::GlobalValue::ExternalLinkage, name, code);
        return_t = decl->signature.returnType;
        auto return_as_llvm_type = ToLLVMType(return_t, false);
        return_t.is_lvalue = true;
        bool uses_sret = !decl->signature.returnType.is_primitive();
        if(uses_sret)
            func->addAttributeAtIndex(1, llvm::Attribute::get(context, llvm::Attribute::StructRet, return_as_llvm_type));
        auto bb = llvm::BasicBlock::Create(context, "entry", func);
        returnBlock = llvm::BasicBlock::Create(context, "return", func);
        builder->SetInsertPoint(bb);
        if(!return_t.is_void()) currentReturnAddress = uses_sret ? static_cast<llvm::Value*>(func->getArg(0)) :
            static_cast<llvm::Value*>(Alloca("return_address", return_as_llvm_type));
        auto old_hash = block_hash;
        block_hash = name + "__";
        pushScope();
        std::list<VariableDeclaration> declarations;
        size_t idx = 0;
        for(auto& param : decl->signature.parameters)
        {
            if(!param.name.empty())
            {
                auto param_type = func->getFunctionType()->getFunctionParamType(idx);
                auto type = param.type;
                if(in_class && type.name == "This") type = this_t;
                declarations.emplace_back(Token{}, type, nullptr,param.convention == ParamType::InOut);
                llvm::Value* var;
                if(type.is_primitive() && param.convention != ParamType::InOut)
                {
                    var = Alloca(param.name, param_type);
                    builder->CreateStore(func->getArg(idx + uses_sret), var);
                }
                else
                {
                    var = func->getArg(idx + uses_sret);
                }
                variables.back()[param.name] = {
                    var,
                    &declarations.back()
                };
            }
            //make lambda context visible
            if(param.type.is_lambda())
            {
                if(!lambdas.contains(param.type.name)) {error(); return;}
                auto llvm_type = lambdas[param.type.name].second;
                auto caps = lambdas[param.type.name].first;
                size_t capture_idx = 0;
                auto zero_const = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0);
                for(auto& capture: *caps)
                {
                    auto idx_const = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), capture_idx);
                    Token tk{.type = TokenType::Identifier, .text = capture.first};
                    NameExpression nexpr(tk);
                    auto type = ExpressionTypeChecker{this}(&nexpr);
                    declarations.emplace_back(Token{}, *type, nullptr, capture.second == ParamType::InOut);
                    auto var = builder->CreateGEP(llvm_type, func->getArg(idx + uses_sret), {zero_const, idx_const}, capture.first);
                    if(type->is_primitive() && capture.second == ParamType::InOut)
                        var = builder->CreateLoad(llvm::PointerType::get(context, 0), var);
                    variables.back()[capture.first] = {
                        var,
                        &declarations.back()
                    };
                    capture_idx++;
                }
            }
            idx++;
        }
        std::visit(*this, decl->body->toVariant());
        popScope();
        builder->SetInsertPoint(returnBlock);
        if(uses_sret) builder->CreateRetVoid();
        else builder->CreateRet(builder->CreateLoad(reinterpret_cast<llvm::AllocaInst*>(currentReturnAddress)->getAllocatedType(), currentReturnAddress));
        block_hash = old_hash;
        if(oldBuilder) builder.swap(oldBuilder);
        return_t = old_return;
        currentReturnAddress = old_ret_addr;
        returnBlock = old_ret_block;
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
        types.back()[name] = {block_hash, hanldeClassDeclaration(decl, true), decl};
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
        decl->type = type;
        auto alloc = Alloca(decl->identifier.text, ToLLVMType(type.value(), false));
        if(decl->initializer)
        {
            auto expr_type = std::visit(ExpressionTypeChecker{this}, decl->initializer->toVariant());
            type->is_lvalue = true;
            ExpressionEvaluator{this}.doAssign(alloc, std::visit(ExpressionEvaluator{this}, decl->initializer->toVariant()), *type, *expr_type);
        }
        variables.back()[name] = {alloc, decl};
    }
    void IRGenerator::operator()(BlockStatement* stat)
    {
        pushScope();
        for(auto& sub_stat : stat->statements)
        {
            std::visit(*this, sub_stat->toVariant());
            if(dynamic_cast<ReturnStatement*>(sub_stat.get())) break;
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
        if(builder->GetInsertBlock()->back().getOpcode() != llvm::Instruction::Br) builder->CreateBr(while_bb);
        builder->SetInsertPoint(cont_bb);
    }
    void IRGenerator::error()
    {
        raise(SIGTRAP);
    }

    void IRGenerator::operator()(ReturnStatement* stat)
    {
        if(stat->expression)
        {
            auto t = std::visit(ExpressionTypeChecker{this}, stat->expression->toVariant());
            if(!t) {error(); return;}
            auto value = std::visit(ExpressionEvaluator{this}, stat->expression->toVariant());
            ExpressionEvaluator{this}.doAssign(currentReturnAddress, value, return_t, *t);
            builder->CreateBr(returnBlock);
        }
        else
            builder->CreateBr(returnBlock);
    }

    void IRGenerator::operator()(IfStatement* stat)
    {
        auto fn = builder->GetInsertBlock()->getParent();
        auto expr_type = std::visit(ExpressionTypeChecker{this}, stat->condition->toVariant());
        if(!expr_type->is_boolean())
        {
            error(); return;
        }
        auto then_bb = llvm::BasicBlock::Create(context, "then", fn, returnBlock);
        llvm::BasicBlock* else_bb = nullptr;
        if(stat->else_stat) else_bb = llvm::BasicBlock::Create(context, "else", fn, returnBlock);
        auto merge_bb = llvm::BasicBlock::Create(context, "ifcont", fn, returnBlock);
        builder->CreateCondBr(
            std::visit(ExpressionEvaluator{this}, stat->condition->toVariant()), then_bb,
            else_bb ? else_bb : merge_bb);
        builder->SetInsertPoint(then_bb);
        std::visit(*this, stat->then_stat->toVariant());
        if(builder->GetInsertBlock()->back().getOpcode() != llvm::Instruction::Br) builder->CreateBr(merge_bb);
        if(stat->else_stat)
        {
            builder->SetInsertPoint(else_bb);
            std::visit(*this, stat->else_stat->toVariant());
            if(builder->GetInsertBlock()->back().getOpcode() != llvm::Instruction::Br) builder->CreateBr(merge_bb);
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
        builder = std::make_unique<llvm::IRBuilder<>>(context);
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
