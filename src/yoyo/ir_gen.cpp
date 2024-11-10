#include "ir_gen.h"

#include <csignal>
#include <list>
#include <set>

namespace Yoyo
{
    std::tuple<std::string, llvm::StructType*, std::unique_ptr<ClassDeclaration>>* IRGenerator::findType(
        const std::string& name)
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
        //type is not required not have a module (built-ins)
        auto t = type.module ?
            type.module->ToLLVMType(type, is_ref, {}):
            module->ToLLVMType(type, is_ref, {});
        if(t) return t;
        if(type.is_lambda())
        {
            if(auto t = lambdas.find(type.name); t != lambdas.end())
                return t->second.second;
        }
        if(type.is_tuple())
        {
            std::vector<llvm::Type*> args;
            for(auto& subtype : type.subtypes)
            {
                args.push_back(ToLLVMType(subtype, false));
            }
            return llvm::StructType::get(context, args);
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
    void IRGenerator::saturateSignature(FunctionSignature& sig, Module* module)
    {
        sig.returnType.saturate(module);
        for(auto& param: sig.parameters)
        {
            param.type.saturate(module);
        }
    }
    llvm::FunctionType* IRGenerator::ToLLVMSignature(const FunctionSignature& sig)
    {
        bool use_sret = sig.returnType.should_sret();
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
            if((!p.type.is_primitive() && !p.type.is_enum())  || p.convention == ParamType::InOut)
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

    llvm::Value* IRGenerator::Malloc(std::string_view name, llvm::Value* size)
    {
        auto malloc_fn = code->getFunction("malloc");
        if(!malloc_fn)
        {
            auto malloc_sig = llvm::FunctionType::get(llvm::PointerType::get(context, 0), {llvm::Type::getInt64Ty(context)}, false);
            malloc_fn = llvm::Function::Create(malloc_sig, llvm::GlobalValue::ExternalLinkage, "malloc", code);
        }
        return builder->CreateCall(malloc_fn, {size});
    }

    void IRGenerator::Free(llvm::Value* value)
    {
        auto free_fn = code->getFunction("free");
        if(!free_fn)
        {
            auto free_sig = llvm::FunctionType::get(llvm::Type::getVoidTy(context), {llvm::PointerType::get(context, 0)}, false);
            free_fn = llvm::Function::Create(free_sig, llvm::GlobalValue::ExternalLinkage, "free", code);
        }
        builder->CreateCall(free_fn, {value});
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
        saturateSignature(decl->signature, module);
        llvm::Function* func = llvm::Function::Create(ToLLVMSignature(decl->signature), llvm::GlobalValue::ExternalLinkage, name, code);
        return_t = decl->signature.returnType;
        auto return_as_llvm_type = ToLLVMType(return_t, false);
        return_t.is_mutable = true;
        bool uses_sret = decl->signature.returnType.should_sret();
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
                    NameExpression nexpr(std::string(tk.text));
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
        current_Statement = &decl->body;
        std::visit(*this, decl->body->toVariant());
        popScope();
        if(builder->GetInsertBlock()->back().getOpcode() != llvm::Instruction::Br)
            builder->CreateBr(returnBlock);
        builder->SetInsertPoint(returnBlock);
        if(uses_sret || return_t.is_void()) builder->CreateRetVoid();
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
        auto ptr = current_Statement->release();
        assert(ptr == decl);
        types.back()[name] = {block_hash, hanldeClassDeclaration(decl, true), std::unique_ptr<ClassDeclaration>{decl}};
    }
    Type IRGenerator::reduceLiteral(const Type& src, llvm::Value* val)
    {
        if(src.name == "ilit")
        {
            auto as_int = llvm::dyn_cast<llvm::ConstantInt>(val);
            if(as_int->isNegative())
            {
                int64_t int_val = as_int->getSExtValue();
                if(int_val >= std::numeric_limits<int32_t>::min() && int_val <= std::numeric_limits<int32_t>::max())
                    return Type{.name="i32"};
                return Type{.name="i64"};
            }
            uint64_t int_val = as_int->getZExtValue();
            if(int_val <= std::numeric_limits<int32_t>::max()) return Type{.name="i32"};
            if(int_val <= std::numeric_limits<uint32_t>::max()) return Type{.name="u32"};
            if(int_val <= std::numeric_limits<int64_t>::max()) return Type{.name="i64"};
            return Type{.name="u64"};
        }
        if(src.name == "flit") return Type{.name = "f64"};
        //unreachble
        return Type{};
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
        //TODO probably consider copying lambda contexts??
        llvm::Value* alloc = nullptr;
        if(decl->initializer)
        {
            auto expr_type = std::visit(ExpressionTypeChecker{this, type}, decl->initializer->toVariant());
            auto eval = ExpressionEvaluator{this, type};
            auto init = std::visit(eval, decl->initializer->toVariant());
            //instead of copying we move
            if(type->is_lambda() || (!expr_type->is_lvalue && !expr_type->is_primitive() && !expr_type->is_enum() && expr_type->is_equal(*type)))
            {
                init->setName(decl->identifier.text);
                alloc = init;
            }
            else
            {
                //at this point the type may be a literal
                if(decl->type->name == "ilit" || decl->type->name == "flit")
                {
                    decl->type = reduceLiteral(*decl->type, init);
                    type = decl->type;
                }
                alloc = Alloca(decl->identifier.text, ToLLVMType(type.value(), false));
                type->is_mutable = true;
                ExpressionEvaluator{this}.doAssign(alloc, init, *type, *expr_type);
            }

        }
        type->saturate(module);
        if(!alloc) alloc = Alloca(decl->identifier.text, ToLLVMType(type.value(), false));
        variables.back()[name] = {alloc, decl};
    }
    void IRGenerator::operator()(BlockStatement* stat)
    {
        pushScope();
        for(auto& sub_stat : stat->statements)
        {
            current_Statement = &sub_stat;
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

        current_Statement = &expr->body;
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
            auto t = std::visit(ExpressionTypeChecker{this, return_t}, stat->expression->toVariant());
            if(!t) {error(); return;}
            auto value = std::visit(ExpressionEvaluator{this, return_t}, stat->expression->toVariant());
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

        current_Statement = &stat->then_stat;
        std::visit(*this, stat->then_stat->toVariant());

        if(builder->GetInsertBlock()->back().getOpcode() != llvm::Instruction::Br) builder->CreateBr(merge_bb);
        if(stat->else_stat)
        {
            builder->SetInsertPoint(else_bb);

            current_Statement = &stat->else_stat;
            std::visit(*this, stat->else_stat->toVariant());

            if(builder->GetInsertBlock()->back().getOpcode() != llvm::Instruction::Br) builder->CreateBr(merge_bb);
        }
        builder->SetInsertPoint(merge_bb);
    }

    bool canReturn(Statement* stat)
    {
        if(dynamic_cast<ReturnStatement*>(stat))
            return true;
        if(auto res = dynamic_cast<IfStatement*>(stat))
            return canReturn(res->then_stat.get()) || (res->else_stat && canReturn(res->else_stat.get()));
        if(auto res = dynamic_cast<WhileStatement*>(stat))
            return canReturn(res->body.get());
        if(auto res = dynamic_cast<ForStatement*>(stat))
            return canReturn(res->body.get());
        if(auto res = dynamic_cast<BlockStatement*>(stat))
        {
            for(auto& sub_stat : res->statements)
            {
                if(canReturn(sub_stat.get())) return true;
            }
        }
        return false;
    }
    //The resultant return type is the type of the first return statement encountered
    std::optional<Type> IRGenerator::inferReturnType(Statement* stat)
    {
        if(!canReturn(stat)) return Type{.name = "void"};
        if(auto res = dynamic_cast<ReturnStatement*>(stat))
            return std::visit(ExpressionTypeChecker{this}, res->expression->toVariant());
        if(auto res = dynamic_cast<IfStatement*>(stat))
        {
            if(canReturn(res->then_stat.get())) return inferReturnType(res->then_stat.get());
            else return inferReturnType(res->else_stat.get());
        }
        if(auto res = dynamic_cast<WhileStatement*>(stat))
            return inferReturnType(res->body.get());
        if(auto res = dynamic_cast<BlockStatement*>(stat))
        {
            for(auto& sub_stat : res->statements)
            {
                if(canReturn(sub_stat.get())) return inferReturnType(sub_stat.get());
            }
        }
        return std::nullopt;
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

    void IRGenerator::GenerateIR(std::string_view name, std::vector<std::unique_ptr<Statement>> statements, Module* md)
    {
        block_hash = md->module_hash;
        md->code = std::make_unique<llvm::Module>(name, context);
        module = md;
        code = md->code.get();
        builder = std::make_unique<llvm::IRBuilder<>>(context);
        pushScope();
        for(auto& stat : statements)
        {
            std::variant<std::unique_ptr<ClassDeclaration>, std::unique_ptr<FunctionDeclaration>> vnt;
            if(auto ptr = dynamic_cast<ClassDeclaration*>(stat.get()))
            {
                std::ignore = stat.release();
                vnt = std::unique_ptr<ClassDeclaration>(ptr);
            }
            else if(auto fn_ptr = dynamic_cast<FunctionDeclaration*>(stat.get()))
            {
                std::ignore = stat.release();
                vnt = std::unique_ptr<FunctionDeclaration>(fn_ptr);
            }
            else continue;
            std::visit(TopLevelVisitor{this}, std::move(vnt));
        }
        builder = nullptr;
    }

}
