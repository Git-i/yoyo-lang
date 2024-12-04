#include "ir_gen.h"

#include <csignal>
#include <list>
#include <ranges>
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
        if(type.is_optional())
        {
            std::array<llvm::Type*, 2> args{};
            args[0] = ToLLVMType(type.subtypes[0], false);
            args[1] = llvm::Type::getInt1Ty(context);
            return llvm::StructType::get(context, args);
        }
        if(type.is_variant())
        {
            std::array<llvm::Type*, 2> args{};
            size_t size = 0;
            auto& layout = code->getDataLayout();
            for(auto& subtype : type.subtypes)
            {
                auto sub_t = ToLLVMType(subtype, false);
                auto as_struct = llvm::dyn_cast_or_null<llvm::StructType>(sub_t);
                size_t sz = 0;
                if(!as_struct) sz = sub_t->getPrimitiveSizeInBits() / 8;
                else sz = layout.getStructLayout(as_struct)->getSizeInBytes();
                if(sz > size) size = sz;
            }
            args[0] = llvm::ArrayType::get(llvm::Type::getInt8Ty(context), size);
            args[1] = llvm::Type::getInt32Ty(context); // 2^32 is a reasonable amount of variant subtypes
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
            auto t = ToLLVMType(p.type, false);
            if(!p.type.is_primitive() && !p.type.is_enum())
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
        llvm::Function* func = nullptr;
        bool uses_sret;

        llvm::Type* return_as_llvm_type;
        if((func = code->getFunction(name)))
        {
            if(!func->empty()) { error(); return; }
            uses_sret = func->hasStructRetAttr();
            return_as_llvm_type = func->getParamStructRetType(0);
        }
        else
        {
            saturateSignature(decl->signature, module);
            return_as_llvm_type = ToLLVMType(decl->signature.returnType, false);
            func = llvm::Function::Create(ToLLVMSignature(decl->signature), llvm::GlobalValue::ExternalLinkage, name, code);
            uses_sret = decl->signature.returnType.should_sret();
            if(uses_sret)
                func->addAttributeAtIndex(1, llvm::Attribute::get(context, llvm::Attribute::StructRet, return_as_llvm_type));
        }

        return_t = decl->signature.returnType;
        return_t.is_mutable = true;

        auto bb = llvm::BasicBlock::Create(context, "entry", func);
        returnBlock = llvm::BasicBlock::Create(context, "return", func);
        builder->SetInsertPoint(bb);
        if(!return_t.is_void()) currentReturnAddress = uses_sret ? static_cast<llvm::Value*>(func->getArg(0)) :
            static_cast<llvm::Value*>(Alloca("return_address", return_as_llvm_type));
        auto old_hash = block_hash;
        block_hash = name + "__";
        pushScope();
        size_t idx = 0;
        for(auto& param : decl->signature.parameters)
        {
            if(!param.name.empty())
            {
                auto param_type = func->getFunctionType()->getFunctionParamType(idx + uses_sret);
                auto type = param.type;
                if(in_class && type.name == "This") type = this_t;
                llvm::Value* var;
                if(!type.should_sret())
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
                    std::move(type)
                };
            }
            //make lambda context visible TODO: fix lambdas
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
                    Token tk{.type = TokenType::Identifier, .text = capture};
                    NameExpression nexpr(std::string(tk.text));
                    auto type = ExpressionTypeChecker{this}(&nexpr);
                    auto var = builder->CreateGEP(llvm_type, func->getArg(idx + uses_sret), {zero_const, idx_const}, capture);
                    if(type->is_primitive())
                        var = builder->CreateLoad(llvm::PointerType::get(context, 0), var);
                    variables.back()[capture] = {
                        var,
                        std::move(type).value()
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
        auto as_var = stat->expression->toVariant();
        auto ty = std::visit(ExpressionTypeChecker{this}, as_var);
        if(!ty) error();
        validate_expression_borrows(stat->expression.get(), this);
        std::visit(ExpressionEvaluator{this}, as_var);
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
                    return Type{.name="i32", .module = src.module};
                return Type{.name="i64",.module = src.module};
            }
            uint64_t int_val = as_int->getZExtValue();
            if(int_val <= std::numeric_limits<int32_t>::max()) return Type{.name="i32",.module = src.module};
            if(int_val <= std::numeric_limits<uint32_t>::max()) return Type{.name="u32",.module = src.module};
            if(int_val <= std::numeric_limits<int64_t>::max()) return Type{.name="i64",.module = src.module};
            return Type{.name="u64",.module = src.module};
        }
        if(src.name == "flit") return Type{.name = "f64",.module = src.module};
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
        if(decl->type) decl->type->saturate(module);
        auto type = decl->type ? decl->type.value() : std::visit(ExpressionTypeChecker{this}, decl->initializer->toVariant());
        if(!type)
        {
            error();
            return;
        }
        if(type->is_non_owning(this)) { error(); return;}
        type->is_mutable = decl->is_mut;
        decl->type = type;
        //TODO probably consider copying lambda contexts??
        llvm::Value* alloc = nullptr;
        if(decl->initializer)
        {
            auto expr_type = std::visit(ExpressionTypeChecker{this, type}, decl->initializer->toVariant());
            if(!expr_type) error();
            validate_expression_borrows(decl->initializer.get(), this);
            auto eval = ExpressionEvaluator{this, type};
            auto init = std::visit(eval, decl->initializer->toVariant());
            //instead of copying we move
            if(type->is_lambda() || (!expr_type->is_lvalue && expr_type->should_sret() && expr_type->is_equal(*type)))
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
        variables.back()[name] = {alloc, std::move(type).value()};
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
        auto fn = builder->GetInsertBlock()->getParent();
        if(!std::visit(ExpressionTypeChecker{this}, expr->condition->toVariant())->is_boolean())
        {
            error();
            return;
        }
        validate_expression_borrows(expr->condition.get(), this);
        auto while_bb = llvm::BasicBlock::Create(context, "while", fn, returnBlock);
        auto then_bb = llvm::BasicBlock::Create(context, "loopthen", fn, returnBlock);
        auto cont_bb = llvm::BasicBlock::Create(context, "loopcont", fn, returnBlock);
        builder->CreateBr(while_bb);
        builder->SetInsertPoint(while_bb);
        auto value = std::visit(ExpressionEvaluator{this}, expr->condition->toVariant());
        builder->CreateCondBr(value, then_bb, cont_bb);
        builder->SetInsertPoint(then_bb);

        current_Statement = &expr->body;
        std::visit(*this, expr->body->toVariant());
        if(builder->GetInsertBlock()->back().getOpcode() != llvm::Instruction::Br) builder->CreateBr(while_bb);
        builder->SetInsertPoint(cont_bb);
    }

    void IRGenerator::operator()(ConditionalExtraction* stat)
    {
        auto tp = std::visit(ExpressionTypeChecker{this}, stat->condition->toVariant());
        if(!tp || !tp->is_optional()) { error(); return;}
        if(!stat->else_capture.empty()) {error(); return;}

        std::array<std::pair<Expression*, BorrowResult::borrow_result_t>, 1> borrow_res;
        borrow_res[0].first = stat->condition.get();
        borrow_res[0].second = tp->is_mutable ?
            std::visit(BorrowResult::LValueBorrowResult{this}, stat->condition->toVariant()):
            std::visit(BorrowResult{this}, stat->condition->toVariant());
        validate_borrows(borrow_res, this);

        if(isShadowing(stat->captured_name)) {error(); return;}

        auto llvm_t = ToLLVMType(tp.value(), false);
        auto optional = std::visit(ExpressionEvaluator{this}, stat->condition->toVariant());

        auto is_valid = builder->CreateLoad(llvm::Type::getInt1Ty(context), builder->CreateStructGEP(llvm_t, optional, 1));

        auto fn = builder->GetInsertBlock()->getParent();
        auto then_bb = llvm::BasicBlock::Create(context, "then", fn, returnBlock);
        llvm::BasicBlock* else_bb = nullptr;
        if(stat->else_body) else_bb = llvm::BasicBlock::Create(context, "else", fn, returnBlock);
        auto merge_bb = llvm::BasicBlock::Create(context, "ifcont", fn, returnBlock);
        builder->CreateCondBr(
            is_valid, then_bb,
            else_bb ? else_bb : merge_bb);
        builder->SetInsertPoint(then_bb);

        pushScope();
        lifetimeExtensions[stat->captured_name] = std::move(borrow_res[0].second);

        auto ptr = builder->CreateStructGEP(llvm_t, optional, 0, stat->captured_name);
        tp->subtypes[0].is_lvalue = true; tp->subtypes[0].is_mutable = tp->is_mutable;
        variables.back()[stat->captured_name] = {ptr, std::move(tp->subtypes[0])};
        current_Statement = &stat->body;
        std::visit(*this, stat->body->toVariant());

        lifetimeExtensions.erase(stat->captured_name);
        popScope();

        if(builder->GetInsertBlock()->back().getOpcode() != llvm::Instruction::Br) builder->CreateBr(merge_bb);

        if(stat->else_body)
        {
            builder->SetInsertPoint(else_bb);
            pushScope();
            current_Statement = &stat->else_body;
            std::visit(*this, stat->else_body->toVariant());
            popScope();
            if(builder->GetInsertBlock()->back().getOpcode() != llvm::Instruction::Br) builder->CreateBr(merge_bb);
        }
        builder->SetInsertPoint(merge_bb);
    }
    template <std::input_iterator It>
    void IRGenerator::pushScopeWithConstLock(It begin, It end)
    {
        pushScope();
        for(const std::string& str: std::ranges::subrange(begin, end))
        {
            for(auto& i : variables | std::views::reverse)
            {
                if(!i.contains(str)) continue;
                Type new_tp = i.at(str).second;
                new_tp.is_mutable = false;
                variables.back()[str] = {i.at(str).first, std::move(new_tp)};
                break;
            }
        }
    }
    void IRGenerator::operator()(WithStatement* stat)
    {
        auto ty = std::visit(ExpressionTypeChecker{this}, stat->expression->toVariant());
        if(!ty) {error(); return;}
        if(!ty->is_non_owning(this)) {error(); return;}
        if(isShadowing(stat->name)) {error(); return;}

        ty->is_mutable = ty->is_non_owning(this);

        std::array<std::pair<Expression*, BorrowResult::borrow_result_t>, 1> borrow_res;
        borrow_res[0].first = stat->expression.get();
        borrow_res[0].second = ty->is_mutable ?
            std::visit(BorrowResult::LValueBorrowResult{this}, stat->expression->toVariant()):
            std::visit(BorrowResult{this}, stat->expression->toVariant());
        validate_borrows(borrow_res, this);

        auto expr = std::visit(ExpressionEvaluator{this}, stat->expression->toVariant());
        llvm::Value* val;
        auto names = borrow_res[0].second | std::views::keys;
        pushScopeWithConstLock(names.begin(), names.end());
        lifetimeExtensions[stat->name] = std::move(borrow_res[0].second);

        if(ty->should_sret())
        {
            expr->setName(stat->name);
            val = expr;
        }
        else
        {
            val = Alloca(stat->name, ToLLVMType(*ty, false));
            ty->is_mutable = true;
            ExpressionEvaluator{this}.doAssign(val, expr, *ty, *ty);
        }
        variables.back()[stat->name] = {val, std::move(ty).value()};
        current_Statement = &stat->body;
        std::visit(*this, stat->body->toVariant());
        lifetimeExtensions.erase(stat->name);
        popScope();
    }

    void IRGenerator::operator()(OperatorOverload*)
    {
        error();
    }

    void IRGenerator::error()
    {
        raise(SIGTRAP);
    }
    FunctionDeclaration* IRGenerator::GetParentFunction(ASTNode* node)
    {
        auto parent = node->parent;
        while(!dynamic_cast<FunctionDeclaration*>(parent))
        {
            parent = parent->parent;
        }
        return reinterpret_cast<FunctionDeclaration*>(parent);
    }
    void IRGenerator::operator()(ReturnStatement* stat)
    {
        if(stat->expression)
        {
            auto t = std::visit(ExpressionTypeChecker{this, return_t}, stat->expression->toVariant());
            if(!t) {error(); return;}
            if(!return_t.is_assignable_from(*t)) {error(); return;}
            // `doAssign` for reference types works like c++ (dereference and assign) rather than rebind the ref, because
            // references cannot be rebound, except in return statements, which is why we handle them specially
            if(return_t.is_non_owning(this))
                if(!std::visit(LifetimeExceedsFunctionChecker{this}, stat->expression->toVariant())) {error(); return;}
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
            [this, decl](const ClassVariable& p)
            {
                if(decl->ownership == Ownership::Owning && p.type.is_non_owning(this)) error();
                if(decl->ownership == Ownership::NonOwning && p.type.is_non_owning_mut(this)) error();
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
            std::variant<
                std::unique_ptr<ClassDeclaration>,
                std::unique_ptr<FunctionDeclaration>,
                std::unique_ptr<OperatorOverload>> vnt;
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
            else if(auto ovl_ptr = dynamic_cast<OperatorOverload*>(stat.get()))
            {
                std::ignore = stat.release();
                vnt = std::unique_ptr<OperatorOverload>(ovl_ptr);
            }
            else continue;
            std::visit(TopLevelVisitor{this}, std::move(vnt));
        }
        builder = nullptr;
    }

}
