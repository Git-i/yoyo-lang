#include "yvm/yvm_irgen.h"

#include <csignal>
#include <list>
#include <ranges>
#include <set>
#include <iostream>
#include "gc/gc.h"
#include <cassert>
using enum Yvm::OpCode;
namespace Yoyo
{
    NativeTy* YVMIRGenerator::toNativeType(const Type& type)
    {
        //type is not required not have a module (built-ins)
        if (type.name.starts_with("__gc_refcell_borrow")) return nullptr;
        auto t = type.module ?
            reinterpret_cast<LLModule*>(type.module)->ToLLVMType(type, block_hash, this, {}):
            reinterpret_cast<LLModule*>(module)->ToLLVMType(type, block_hash, this, {});
        if(t) return t;
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
        //this may be a generic instantiation that doesn't exist yet
        if (type.module)
        {
            if (auto [_, exists] = type.module->findGenericClass(type.block_hash, type.name); exists)
            {
                generateGenericClass(
                    type.module,
                    type.block_hash,
                    exists,
                    type.subtypes); //this should generally be safe(hopefully)
                std::string name = type.name + mangleGenericArgs(type.subtypes);
                if (auto det = std::get<1>(reinterpret_cast<LLModule*>(type.module)->findClassWithType(type.block_hash, name).second))
                    return det;
            }
                
        }
        if(in_class && type.name == "This") return ToLLVMType(this_t, is_ref);
        error(Error({1, 1}, {1, 1}, "Encountered unexpected type", ""));
        return nullptr;
    }
    extern "C"
    {
        YOYO_API  void* Yoyo_malloc_wrapper_dont_use_name(size_t size);
        YOYO_API  void Yoyo_compiler_print(const char* string);
        void* Yoyo_malloc_wrapper_dont_use_name(size_t size)
        {
            return GC_malloc(size);
        }
    }
    
    void YVMIRGenerator::operator()(FunctionDeclaration* decl)
    {
        auto new_builder = std::make_unique<Yvm::Emitter>();
        builder.swap(new_builder);

        auto fn_name = block_hash + decl->name;
        builder->create_function(fn_name);
        
        CFGNode::prepareFromFunction(function_cfgs.emplace_back(), decl);
        function_cfgs.back().annotate();
        decltype(this->variables) new_fn_vars;
        new_fn_vars.emplace_back();

        auto this_entry = module->findFunction(block_hash, decl->name).second;
        saturateSignature(this_entry->sig, module);
        size_t idx = 0;
        uint8_t uses_sret = this_entry->sig.returnType.should_sret();
        size_t highest_param = this_entry->sig.parameters.size();
        for (auto& param : this_entry->sig.parameters) {
            if (!param.name.empty()) {
                size_t stack_addr;
                // param is passed by value
                if (!param.type.should_sret()) {
                    stack_addr = uses_sret + highest_param++;
                    builder->write_alloca(NativeType::get_size(toNativeType(param.type)));
                    builder->write_2b_inst(StackAddr, uses_sret + idx);
                    builder->write_2b_inst(StackAddr, stack_addr);
                    builder->write_2b_inst(Store, toTypeEnum(param.type));
                }
                else {
                    stack_addr = uses_sret + idx;
                    if (!param.type.is_trivially_destructible(this))
                    {
                        builder->write_2b_inst(StackAddr, uses_sret + idx);
                        builder->write_fn_addr("__destructor_for" + param.type.full_name());
                        builder->write_1b_inst(RegObj);
                        builder->write_1b_inst(Pop);
                    }
                }
                new_fn_vars.back().emplace_back(param.name, std::pair{stack_addr, param.type});
            }
            idx++;
        }

        variables.swap(new_fn_vars);
        current_Statement = &decl->body;
        std::visit(*this, decl->body->toVariant());
        variables.swap(new_fn_vars);
        function_cfgs.pop_back();
        builder->close_function();
        builder.swap(new_builder);
    }
    void YVMIRGenerator::operator()(ExpressionStatement* stat)
    {
        auto as_var = stat->expression->toVariant();
        auto ty = std::visit(ExpressionTypeChecker{this}, as_var).value_or_error();
        if(!ty.is_error_ty()) validate_expression_borrows(stat->expression.get(), this);
        auto eval = YVMExpressionEvaluator{this};
        std::visit(eval, as_var);
        if(!ty.is_lvalue)
            eval.destroy(ty);
    }
    void YVMIRGenerator::operator()(ConstantDeclaration* decl)
    {
        auto [_, constant] = module->findConst(block_hash, decl->name);
        std::get<0>(*constant).saturate(module, this);
        auto val = std::visit(ConstantEvaluator{ this }, decl->expr->toVariant());
        
        std::get<2>(*constant) = val;
    }
    void YVMIRGenerator::operator()(InterfaceDeclaration* decl)
    {
        __debugbreak();
    }
    bool implementsInterfaceMethod(const FunctionSignature& cls, const FunctionSignature& interface)
    {
        if (cls.parameters.size() != interface.parameters.size()) return false;
        if (!cls.returnType.is_equal(interface.returnType)) return false;
        for (size_t i = 0; i < cls.parameters.size(); i++)
        {
            auto& cls_param = cls.parameters[i];
            auto& intf_param = interface.parameters[i];
            if (!cls_param.type.is_equal(intf_param.type))
                if (cls_param.name != "this" || intf_param.name != "this") return false;
        }
        return true;
    }
    void YVMIRGenerator::operator()(EnumDeclaration* decl)
    {
        auto ptr = current_Statement->release();
        assert(ptr == decl);

        auto old_in_class = in_class;
        in_class = true;
        auto old_this = std::move(this_t);
        this_t = Type{ .name = decl->identifier, .subtypes = {} };
        this_t.saturate(module, this);
        
        std::string this_hash = block_hash + decl->identifier + "::";
        block_hash.swap(this_hash);

        for (auto& stat : decl->stats) {
            current_Statement = &stat;
            std::visit(*this, stat->toVariant());
        }
        block_hash.swap(this_hash);
        this_t = std::move(old_this);
        in_class = old_in_class;
    }

    void YVMIRGenerator::operator()(UnionDeclaration* decl)
    {
        std::string name = decl->name;
        if (isShadowing(name))
        {
            error(Error(decl, "The name '" + name + "' is already defined"));
            return;
        }
        auto ptr = current_Statement->release();
        assert(ptr = decl);
        std::string unn_hash = block_hash + name + "::";
        auto curr_hash = reset_hash();
        block_hash = unn_hash;
        for (auto& var : decl->fields | std::views::values) var.saturate(module, this);
        size_t biggest_size = 0;
        auto& layout = code->getDataLayout();
        for (auto& var : decl->fields | std::views::values) {
            auto as_llvm = ToLLVMType(var, false);
            size_t this_size = layout.getTypeAllocSize(as_llvm);
            biggest_size = this_size > biggest_size ? this_size : biggest_size;
        }
        auto llvm_t = llvm::StructType::get(context, { 
            llvm::ArrayType::get(llvm::Type::getInt8Ty(context), biggest_size),
            llvm::Type::getInt32Ty(context)
        });
        module->unions[curr_hash].emplace_back(std::unique_ptr<UnionDeclaration>{decl}, llvm_t);
        auto old_in_class = in_class;
        in_class = true;
        auto old_this = std::move(this_t);
        this_t = Type{ .name = name, .subtypes = {} };
        this_t.saturate(module, this);
        for (auto& stat : decl->sub_stats) {
            current_Statement = &stat;
            std::visit(*this, stat->toVariant());
        }
        block_hash.swap(curr_hash);
        in_class = old_in_class;
        this_t = std::move(old_this);
    }
    void YVMIRGenerator::operator()(MacroDeclaration* decl)
    {
        assert(current_Statement->get() == decl);
        current_Statement->release();
    }
    std::optional<Type> YVMIRGenerator::getVariableType(const std::string& name, Expression* expr)
    {
        for (size_t i = variables.size(); i > 0; --i)
        {
            size_t idx = i - 1;
            if (auto var = std::ranges::find_if(variables[idx], [&name](const std::pair<std::string, std::pair<size_t, Type>>& dets) {
                return dets.first == name;
                }); var != variables[idx].end())
            {
                auto& type = var->second.second;
                //its only lvalue if its not last use
                bool is_last_use = function_cfgs.back().last_uses.at(name).contains(expr);
                type.is_lvalue = !is_last_use;
                type.saturate(module, this);
                return { type };
            }
        }
    }
    void YVMIRGenerator::operator()(ClassDeclaration* decl)
    {
        std::string name = decl->name;
        if(isShadowing(name))
        {
            error(Error(decl, "The name '" + name + "' is already defined"));
            return;
        }
        auto ptr = current_Statement->release();
        assert(ptr == decl);
        std::string class_hash = block_hash + name + "::";
        auto curr_hash = reset_hash();
        block_hash = class_hash;
        auto cls = module->findClass(curr_hash, decl->name);
        for(auto& var : decl->vars) var.type.saturate(module, this);
        reinterpret_cast<LLModule*>(module)->classes_types[cls.second->second.get()] = hanldeClassDeclaration(decl->vars, decl->ownership, "");
        auto old_in_class = in_class;
        in_class = true;
        auto old_this = std::move(this_t);
        this_t = Type{ .name = name, .subtypes = {} };
        this_t.saturate(module, this);
        checkClass(decl);
        for(auto& stt: decl->stats)
        {
            current_Statement = &stt;
            std::visit(*this, stt->toVariant());
        }
        block_hash = std::move(curr_hash);
        for (auto& impl : decl->impls)
        {
            if (!impl.impl_for.module) continue;
            std::pair<std::string, InterfaceDeclaration*> pair;
            if (!impl.impl_for.subtypes.empty())
                pair = impl.impl_for.module->findInterface(impl.impl_for.block_hash, impl.impl_for.name + IRGenerator::mangleGenericArgs(impl.impl_for.subtypes));
            else pair = impl.impl_for.module->findInterface(impl.impl_for.block_hash, impl.impl_for.name);
            auto [hash, interface] = std::move(pair);
            if (!interface) continue;
            if (impl.methods.size() != interface->methods.size())
            {
                continue;
            }
            auto curr_hash = std::move(block_hash);
            block_hash = class_hash + hash + interface->name + "::";
            for (auto& mth : impl.methods)
            {
                auto it = std::ranges::find_if(interface->methods, [&mth](auto& method) {
                    return method->name == mth->name;
                    });
                if (it == interface->methods.end())
                    error(Error(mth.get(), "Function does not exist as part of the interface"));
                else
                {
                    saturateSignature(mth->signature, module);
                    in_class = false;
                    auto interface_method_hash = reset_hash();
                    block_hash = hash + interface->name + "::";
                    saturateSignature((*it)->signature, impl.impl_for.module);
                    block_hash = std::move(interface_method_hash);
                    in_class = true;
                    if (!implementsInterfaceMethod(mth->signature, (*it)->signature))
                        error(Error(mth.get(), "Provided function is not a valid implementation of the interface"));
                }
                (*this)(mth.get());
            }
            block_hash = std::move(curr_hash);
        }
        this_t = std::move(old_this);
        in_class = old_in_class;
    }
    void YVMIRGenerator::operator()(VariableDeclaration* decl)
    {
        //TODO implicit conversion and validation
        std::string name(decl->identifier.text);
        if(isShadowing(name))
        {
            error(Error(decl, "The name '" + name + "' already exists")); return;
        }
        if(decl->type) decl->type->saturate(module, this);
        FunctionType type = decl->type ? decl->type.value() : std::visit(ExpressionTypeChecker{this}, decl->initializer->toVariant()).value_or_error();
        if(!type.can_be_stored()) { error(Error(decl, "The type tp cannot be stored")); return; }
        if(type.is_non_owning(this)) { error(Error(decl, "Variable types must not be non-owning")); return; }
        type.is_mutable = decl->is_mut;
        type.is_lvalue = true;
        decl->type = type;
        uint32_t type_size = NativeType::get_size(toNativeType(type));
        if(decl->initializer)
        {
            auto expr_type = std::visit(ExpressionTypeChecker{this, type}, decl->initializer->toVariant()).value_or_error();
            if(!expr_type.is_error_ty()) validate_expression_borrows(decl->initializer.get(), this);
            auto eval = YVMExpressionEvaluator{this, type};
            std::visit(eval, decl->initializer->toVariant());
            // TODO: reduce literal
            if(!type.should_sret())
            {
                builder->write_alloca(type_size);
                eval.implicitConvert(decl->initializer.get(), expr_type, type, true, false);
            } eval.implicitConvert(decl->initializer.get(), expr_type, type, false, false);
            if (!type.is_trivially_destructible(this))
            {
                // register the object for destruction on ret/panic
                builder->write_fn_addr("__destructor_for" + type.full_name());
                builder->write_1b_inst(RegObj);
            }
        }
        else
        {
            builder->write_alloca(type_size);
        }
        // TODO: stack addr of variables
        variables.back().emplace_back(name, std::pair{0, type});
    }
    void YVMIRGenerator::operator()(BlockStatement* stat)
    {
        pushScope();
        for(auto& sub_stat : stat->statements)
        {
            current_Statement = &sub_stat;
            std::visit(*this, sub_stat->toVariant());
            if(dynamic_cast<ReturnStatement*>(sub_stat.get()))
                return;
        }
        popScope();
    }
    void YVMIRGenerator::operator()(ForStatement* stat)
    {
        auto ty = std::visit(ExpressionTypeChecker{ this }, stat->iterable->toVariant()).value_or_error();
        if (!ty.is_mutable && ty.is_lvalue) error(Error(stat->iterable.get(), "Iterator object must be mutable or a temporary"));
        //check if it implements iterator interface
        if (auto cls = ty.get_decl_if_class(this))
        {
            auto decl = cls;
            auto hash = ty.module->findClass(ty.block_hash, cls->name).second->first;
            Yoyo::InterfaceImplementation* impl = nullptr;
            for (auto& im : decl->impls)
            {
                if (im.impl_for.module == module->engine->modules.at("core").get() && im.impl_for.name == "Iterator")
                {
                    if (impl) {
                        error(Error(stat, "Type implements multiple iterator interfaces"));
                        break;
                    }
                    impl = &im;
                }
            }
            if (!impl) {
                error(Error(stat->iterable.get(), "Expression does not evaluate to an iterable type"));
                return;
            }
            std::string fn_name = hash + "core::Iterator" + mangleGenericArgs(impl->impl_for.subtypes) + "::next";
            auto memory_ty = reinterpret_cast<StructNativeTy*>(toNativeType(impl->methods[0]->signature.returnType));
            std::visit(YVMExpressionEvaluator{ this }, stat->iterable->toVariant());
            if (!ty.is_lvalue)
            {
                builder->write_fn_addr("__destructor_for" + ty.full_name());
                builder->write_1b_inst(RegObj);
            }
            builder->write_alloca(NativeType::get_size(memory_ty));
            //------------- Iterator::next() args
            builder->write_2b_inst(StackAddr, nextKnownAddr() + 1); //< return addr
            builder->write_2b_inst(StackAddr, nextKnownAddr()); //< iterable object
            //--------------------------------------------
            pushScope();
            auto for_bb = builder->create_label("for_begin");
            auto for_cont = builder->unq_label_name("for_cont");
            //auto then_bb = llvm::BasicBlock::Create(context, "forthen", fn, returnBlock);
            //auto cont_bb = llvm::BasicBlock::Create(context, "forcont", fn, returnBlock);
            builder->write_fn_addr(fn_name);
            builder->write_2b_inst(Call, 2);
            builder->write_1b_inst(Pop); // discard the return value
            // bring the return value to the top of the stack
            builder->write_2b_inst(StackAddr, nextKnownAddr() + 1);
            // register this for destruction
            builder->write_fn_addr("__destructor_for" + impl->methods[0]->signature.returnType.full_name());
            builder->write_1b_inst(RegObj);
            builder->write_ptr_off(NativeType::getElementOffset(memory_ty, 1));
            builder->write_2b_inst(Load, Yvm::Type::u8);

            builder->create_jump(JumpIfFalse, for_cont);
            //for loop body
            variables.back().emplace_back(stat->names[0].text, std::pair{nextKnownAddr() + 1, impl->impl_for.subtypes[0]});
            current_Statement = &stat->body;
            auto old_break_to = break_to; auto old_cont_to = continue_to;
            break_to = for_cont; continue_to = for_bb;
            std::visit(*this, stat->body->toVariant());
            break_to = old_break_to; continue_to = old_cont_to;
            popScope();
            //the actual loop
            builder->create_jump(Jump, for_bb);
            builder->create_label(for_cont);
            if (!ty.is_lvalue)
            {
                YVMExpressionEvaluator{ this }.destroy(ty);
            }
        }
        
    }
    void YVMIRGenerator::operator()(BreakStatement* s)
    {
        if (break_to.empty()) error(Error(s, "'break' must be used within a loop"));
        else
        {
            // TODO: call destructors/pop scope
            builder->create_jump(Jump, break_to);
        }
    }
    void YVMIRGenerator::operator()(ContinueStatement* s)
    {
        if (continue_to.empty()) error(Error(s, "'continue' statement must be used within a loop"));
        else
        {
            // TODO: call destructors/pop scope
            builder->create_jump(Jump, continue_to);
        }
    }
    void YVMIRGenerator::operator()(WhileStatement* expr)
    {
        auto cond_ty = std::visit(ExpressionTypeChecker{ this }, expr->condition->toVariant());
        if (!cond_ty) error(cond_ty.error());
        if(cond_ty && !cond_ty->is_boolean() && !cond_ty->is_error_ty())
        {
            error(Error(expr->condition.get(), "Condition in 'while' must evaluate to a boolean"));
            return;
        }
        validate_expression_borrows(expr->condition.get(), this);
        auto while_bb = builder->create_label("while_begin");
        auto while_cont = builder->unq_label_name("while_cont");
        
        std::visit(YVMExpressionEvaluator{this}, expr->condition->toVariant());
        builder->create_jump(JumpIfFalse, while_cont);

        current_Statement = &expr->body;
        pushScope();
        auto old_break_to = break_to; auto old_cont_to = continue_to;
        break_to = while_cont; continue_to = while_bb;
        std::visit(*this, expr->body->toVariant());
        break_to = old_break_to; continue_to = old_cont_to;
        popScope();
        builder->create_jump(Jump, while_bb);
        builder->create_label(while_cont);
    }

    void YVMIRGenerator::operator()(ConditionalExtraction* stat)
    {
        auto tp_e = std::visit(ExpressionTypeChecker{this}, stat->condition->toVariant());
        auto expression = std::visit(LLVMExpressionEvaluator{ this }, stat->condition->toVariant());
        auto tp = tp_e.value_or_error();
        if(!tp.is_optional() && !tp.is_conversion_result()) { error(Error(stat->condition.get(), "Expression cannot be extracted")); return; }
        if (!stat->else_capture.empty()) { debugbreak(); return; }
        if (stat->is_ref && tp.is_value_conversion_result()) { error(Error({}, {}, "Expanded expression cannot be borrowed", "")); return; }
        std::array<std::pair<Expression*, BorrowResult::borrow_result_t>, 1> borrow_res;
        borrow_res[0].first = stat->condition.get();
        borrow_res[0].second = tp.is_mutable ?
            std::visit(BorrowResult::LValueBorrowResult{this}, stat->condition->toVariant()):
            std::visit(BorrowResult{this}, stat->condition->toVariant());
        validate_borrows(borrow_res, this);

        if (isShadowing(stat->captured_name)) { error(Error({}, {}, "Name is already in use")); return; }

        auto llvm_t = ToLLVMType(tp, false);

        auto is_valid = builder->CreateLoad(llvm::Type::getInt1Ty(context), builder->CreateStructGEP(llvm_t, expression, 1));

        auto fn = builder->GetInsertBlock()->getParent();
        auto then_bb = llvm::BasicBlock::Create(context, "then", fn, returnBlock);
        llvm::BasicBlock* else_bb = nullptr;
        if(stat->else_body) else_bb = llvm::BasicBlock::Create(context, "else", fn, returnBlock);
        auto merge_bb = llvm::BasicBlock::Create(context, "ifcont", fn, returnBlock);
        builder->CreateCondBr(
            is_valid, then_bb,
            else_bb ? else_bb : merge_bb);
        builder->SetInsertPoint(then_bb);

        auto names = borrow_res[0].second | std::views::keys;
        if(stat->is_ref)
        {
            pushScopeWithConstLock(names.begin(), names.end());
            lifetimeExtensions[stat->captured_name] = std::move(borrow_res[0].second);
        }
        else pushScope();

        auto ptr = builder->CreateStructGEP(llvm_t, expression, 0, stat->captured_name);
        if(!tp.is_value_conversion_result())
        {
            //if it's not a `ref` we clone the value
            if(!stat->is_ref)
            {
                if (tp.is_ref_conversion_result())
                    ptr = builder->CreateLoad(llvm::PointerType::get(context, 0), ptr);
                llvm::Value* into = nullptr;
                if(!tp.subtypes[0].should_sret())
                {
                    auto as_llvm = ToLLVMType(tp.subtypes[0], false);
                    into = Alloca("", as_llvm);
                    ptr = builder->CreateLoad(as_llvm, ptr);
                }
                ptr = LLVMExpressionEvaluator{this}.clone(stat->condition.get(), ptr, tp.subtypes[0], into);
                if(into) ptr = into;
            }
        }
        Type variable_type = stat->is_ref ?
            Type{tp.is_mutable ? "__ref_mut" : "__ref", {tp.subtypes[0]}} :
            tp.subtypes[0];
        variable_type.saturate(module, this);
        auto flg = prepareValidDropFlagFor(this, variable_type);
        variables.back()[stat->captured_name] = {ptr,
            std::move(variable_type),
            flg
        };
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
        LLVMExpressionEvaluator{this}.destroy(expression, tp);
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
                Type new_tp = std::get<1>(i.at(str));
                new_tp.is_mutable = false;
                variables.back()[str] = {std::get<0>(i.at(str)), std::move(new_tp), nullptr};
                break;
            }
        }
    }
    void YVMIRGenerator::operator()(WithStatement* stat)
    {
        auto ty = std::visit(ExpressionTypeChecker{this}, stat->expression->toVariant()).value_or_error();
        if (!ty.is_non_owning(this)) { error(Error(stat->expression.get(), "'with' statement can only capture non-owning types")); return; }
        if (isShadowing(stat->name)) { error(Error({}, {}, "Name is already in use")); return; }

        ty.is_mutable = ty.is_non_owning_mut(this);

        std::array<std::pair<Expression*, BorrowResult::borrow_result_t>, 1> borrow_res;
        borrow_res[0].first = stat->expression.get();
        borrow_res[0].second = ty.is_mutable ?
            std::visit(BorrowResult::LValueBorrowResult{this}, stat->expression->toVariant()):
            std::visit(BorrowResult{this}, stat->expression->toVariant());
        validate_borrows(borrow_res, this);

        auto expr = std::visit(LLVMExpressionEvaluator{this}, stat->expression->toVariant());
        llvm::Value* val;
        auto names = borrow_res[0].second | std::views::keys;
        pushScopeWithConstLock(names.begin(), names.end());
        lifetimeExtensions[stat->name] = std::move(borrow_res[0].second);

        if(!ty.should_sret())
        {
            val = Alloca(stat->name, ToLLVMType(ty, false));
            LLVMExpressionEvaluator{this}.clone(stat->expression.get(), expr, ty, val);
        } else val = LLVMExpressionEvaluator{this}.clone(stat->expression.get(), expr, ty);

        if(!ty.is_lvalue && expr->getName().starts_with("__del_parents"))
        {
            std::string name(16 + sizeof(void*), 'c');
            memcpy(name.data(), expr->getName().data(), 16 + sizeof(void*));
            val->setName(name);
        }
        ty.is_lvalue = true;
        variables.back()[stat->name] = {val, std::move(ty), prepareValidDropFlagFor(this, ty)}; //TODO: capture parents
        current_Statement = &stat->body;
        std::visit(*this, stat->body->toVariant());
        lifetimeExtensions.erase(stat->name);
        popScope();
    }

    void YVMIRGenerator::operator()(OperatorOverload*)
    {
        debugbreak();
    }

    void YVMIRGenerator::operator()(GenericFunctionDeclaration*)
    {
        current_Statement->release();
    }
    void YVMIRGenerator::operator()(GenericClassDeclaration*)
    {
        current_Statement->release();
    }
    void YVMIRGenerator::operator()(AliasDeclaration* decl)
    {
        auto type = module->findAlias(block_hash, decl->name);
        type->saturate(module, this);
    }

    void YVMIRGenerator::operator()(GenericAliasDeclaration*)
    {
        current_Statement->release();
    }

    void YVMIRGenerator::operator()(ReturnStatement* stat)
    {
        if (stat->expression)
        {
            auto t = std::visit(ExpressionTypeChecker{ this, return_t }, stat->expression->toVariant()).value_or_error();
            if (!return_t.is_assignable_from(t, this)) { error(Error(stat, "Type is not convertible to return type")); return; }
            if (return_t.is_non_owning(this))
                if (!std::visit(LifetimeExceedsFunctionChecker{ this }, stat->expression->toVariant())) debugbreak();
            std::visit(YVMExpressionEvaluator{ this, return_t }, stat->expression->toVariant());
            if (!return_t.should_sret())
            {
                YVMExpressionEvaluator{ this }.implicitConvert(stat->expression.get(), t, return_t, false, true);
                builder->write_1b_inst(Ret);
            }
            else {
                // push return address
                YVMExpressionEvaluator{ this }.implicitConvert(stat->expression.get(), t, return_t, true, false);
                builder->write_1b_inst(RetVoid);
            }
        }
        else builder->write_1b_inst(RetVoid);
    }

    void YVMIRGenerator::operator()(IfStatement* stat)
    {
        auto expr_type = std::visit(ExpressionTypeChecker{this}, stat->condition->toVariant()).value_or_error();
        if(!expr_type.is_boolean() && !expr_type.is_error_ty())
        {
            error(Error(stat->condition.get(), "'if' condition must evaluate to a boolean")); return;
        }
        std::visit(YVMExpressionEvaluator{ this }, stat->condition->toVariant());
        auto cont_block = builder->unq_label_name("if_cont");
        std::string else_block;
        if (stat->else_stat) {
            else_block = builder->unq_label_name("if_else");
            builder->create_jump(JumpIfFalse, else_block);
        }
        else {
            builder->create_jump(JumpIfFalse, cont_block);
        }

        current_Statement = &stat->then_stat;
        std::visit(*this, stat->then_stat->toVariant());

        if (stat->else_stat) {
            builder->create_jump(Jump, cont_block);
            builder->create_label(else_block);
            current_Statement = &stat->else_stat;
            std::visit(*this, stat->else_stat->toVariant());
        }
        
        builder->create_label(cont_block);
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

    void YVMIRGenerator::popScope()
    {
        callDestructors();
        variables.pop_back();
    }
    void YVMIRGenerator::callDestructors(size_t depth)
    {
        //if there's a `br` dont steal it and assume destructors have already been called
        llvm::Instruction* term = nullptr;
        if (builder->GetInsertBlock()->back().getOpcode() == llvm::Instruction::Br)
        {
            return;
            term = &builder->GetInsertBlock()->back();
            term->removeFromParent();
        }
        //call destructors
        auto fn = builder->GetInsertBlock()->getParent();
        for (auto& [name, var] : *(variables.end() - depth - 1))
        {
            auto drop_flag = std::get<2>(var);
            auto& type = std::get<1>(var);
            if (!drop_flag) continue;
            auto drop = llvm::BasicBlock::Create(context, "drop_var" + name, fn, returnBlock);
            auto drop_cont = llvm::BasicBlock::Create(context, "drop_cont" + name, fn, returnBlock);
            builder->CreateCondBr(
                builder->CreateLoad(llvm::Type::getInt1Ty(context), drop_flag),
                drop, drop_cont);
            builder->SetInsertPoint(drop);
            auto to_drop = std::get<0>(var);
            if (!type.should_sret())
            {
                std::string name(16 + sizeof(void*), 'c');
                if (to_drop->getName().starts_with("__del_parents"))
                    memcpy(name.data(), to_drop->getName().data(), 16 + sizeof(void*));
                to_drop = builder->CreateLoad(ToLLVMType(type, false), to_drop, name);
            }
            LLVMExpressionEvaluator{ this }.destroy(to_drop, std::get<1>(var));
            builder->CreateBr(drop_cont);
            builder->SetInsertPoint(drop_cont);
        }
        if (term)
            term->insertInto(builder->GetInsertBlock(), builder->GetInsertPoint());
    }



    bool YVMIRGenerator::GenerateIR(std::string_view name, std::vector<std::unique_ptr<Statement>> statements, LLModule* md, Engine* eng)
    {
        block_hash = md->module_hash;
        module = md;
        code = md->code.getModuleUnlocked();
        builder = std::make_unique<Yvm::Emitter>();
        pushScope();
        for (auto& stat : statements) {
            current_Statement = &stat;
            std::visit(*this, stat->toVariant());
        }
        builder = nullptr;
        return !has_error;
    }

    
}
