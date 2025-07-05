#include "yvm/yvm_irgen.h"

#include <csignal>
#include <list>
#include <ranges>
#include <set>
#include <iostream>
#include "gc/gc.h"
#include <cassert>
#include <yvm/fwd_decl.h>
using enum Yvm::OpCode;
namespace Yoyo
{
    NativeTy* YVMIRGenerator::toNativeType(const Type& type)
    {
        //type is not required not have a module (built-ins)
        if (type.name.starts_with("__gc_refcell_borrow")) return nullptr;
        auto t = type.module ?
            reinterpret_cast<YVMModule*>(type.module)->toNativeType(type, block_hash, this, {}):
            reinterpret_cast<YVMModule*>(module)->toNativeType(type, block_hash, this, {});
        if (t) return t;
        error(Error({1, 1}, {1, 1}, "Encountered unexpected type", ""));
        return nullptr;
    }
    Yvm::Type YVMIRGenerator::toTypeEnum(const Type& type) {
        auto nt = toNativeType(type);
        if (nt == NativeType::getI8()) return Yvm::Type::i8;
        else if (nt == NativeType::getI16()) return Yvm::Type::i16;
        else if (nt == NativeType::getI32()) return Yvm::Type::i32;
        else if (nt == NativeType::getI64()) return Yvm::Type::i64;
        else if (nt == NativeType::getU8()) return Yvm::Type::u8;
        else if (nt == NativeType::getU16()) return Yvm::Type::u16;
        else if (nt == NativeType::getU32()) return Yvm::Type::u32;
        else if (nt == NativeType::getU64()) return Yvm::Type::u64;
        else if (nt == NativeType::getF32()) return Yvm::Type::f32;
        else if (nt == NativeType::getF64()) return Yvm::Type::f64;
        else if (nt == NativeType::getPtrTy()) return Yvm::Type::ptr;
        return static_cast<Yvm::Type>(-1);
    }
    void handleCImport(YVMIRGenerator* irgen, ModuleBase::FunctionDetails* dets, CImportDeclaration* body, const std::string& fn_name) {
        auto mod = reinterpret_cast<YVMModule*>(irgen->module);
        auto eng = reinterpret_cast<YVMEngine*>(mod->engine);
        Yvm::Emitter em(false);
        std::vector<NativeTy*> args;
        for (auto& param : dets->sig.parameters) {
            args.push_back(irgen->toNativeType(param.type));
        }
        auto proto = eng->fi_manager.get_proto(args, irgen->toNativeType(dets->sig.returnType));
        auto uses_sret = dets->sig.returnType.should_sret();
        //if we use sret we supply a pointer at the stack top for the native function handler
        if (uses_sret) em.write_2b_inst(RevStackAddr, dets->sig.parameters.size());
        auto func_ptr = eng->findNativeFunction(body->function_name);
        if (!func_ptr) {
            irgen->error(Error(body, "Unable to find native function: " + body->function_name));
        }
        em.write_const(proto);
        em.write_const(func_ptr);
        em.write_2b_inst(NativeCall, dets->sig.parameters.size() + uses_sret);
        em.write_1b_inst(Yvm::OpCode::Ret);
        em.close_function(&mod->code, fn_name);
    }
    void YVMIRGenerator::doFunctionInternal(std::string fn_name, const FunctionSignature& sig, FunctionDeclaration* decl) {
        auto old_return = return_t;
        return_t = sig.returnType;

        auto old_hash = block_hash;
        block_hash = fn_name + "::";

        auto new_builder = std::make_unique<Yvm::Emitter>();
        builder.swap(new_builder);

        CFGNode::prepareFromFunction(function_cfgs.emplace_back(), decl);
        function_cfgs.back().annotate();
        decltype(this->variables) new_fn_vars;
        new_fn_vars.emplace_back();

        size_t idx = 0;
        uint8_t uses_sret = sig.returnType.should_sret();
        builder->add_function_params(sig.parameters.size() + uses_sret);
        for (auto& param :  sig.parameters) {
            if (!param.name.empty()) {
                size_t stack_addr;
                // param is passed by value
                if (!param.type.should_sret()) {
                    stack_addr = builder->write_alloca(NativeType::get_size(toNativeType(param.type)));
                    builder->write_2b_inst(StackAddr, uses_sret + idx);
                    builder->write_2b_inst(StackAddr, stack_addr);
                    builder->write_2b_inst(Store, toTypeEnum(param.type));
                }
                else {
                    stack_addr = uses_sret + idx;
                    if (!param.type.is_trivially_destructible(this))
                    {
                        builder->write_2b_inst(StackAddr, uses_sret + idx);
                        builder->write_fn_addr("__destructor_for_" + param.type.full_name());
                        builder->write_1b_inst(RegObj);
                        builder->write_1b_inst(Pop);
                    }
                }
                new_fn_vars.back().emplace_back(param.name, std::pair{ stack_addr, param.type });
            }
            idx++;
        }

        variables.swap(new_fn_vars);
        current_Statement = &decl->body;
        std::visit(*this, decl->body->toVariant());
        variables.swap(new_fn_vars);
        function_cfgs.pop_back();
        builder->close_function(&reinterpret_cast<YVMModule*>(module)->code, fn_name);
        block_hash = old_hash;
        std::swap(return_t, old_return);
        builder.swap(new_builder);
    }
    void YVMIRGenerator::operator()(FunctionDeclaration* decl)
    {
        auto fn_name = block_hash + decl->name;

        auto this_entry = module->findFunction(block_hash, decl->name).second;
        saturateSignature(this_entry->sig, module);

        if (auto cimport = dynamic_cast<CImportDeclaration*>(decl->body.get())) {
            handleCImport(this, this_entry, cimport, fn_name);
            return;
        }
        doFunctionInternal(fn_name, this_entry->sig, decl);
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
    bool implementsInterfaceMethod(const FunctionSignature& cls, const FunctionSignature& interface);
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
        
        
        //module->unions[curr_hash].emplace_back(std::unique_ptr<UnionDeclaration>{decl}, llvm_t);
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
        return std::nullopt;
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
        //reinterpret_cast<YVMModule*>(module)->classes_types[cls.second->second.get()] = hanldeClassDeclaration(decl->vars, decl->ownership, "");
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
                std::unique_ptr<Statement> stat_ptr(mth.release());
                std::visit(ForwardDeclaratorPass1{ reinterpret_cast<YVMModule*>(module), stat_ptr, block_hash }, stat_ptr->toVariant());
                mth.reset(reinterpret_cast<FunctionDeclaration*>(stat_ptr.release()));
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
            } else eval.implicitConvert(decl->initializer.get(), expr_type, type, false, false);
            if (!type.is_trivially_destructible(this))
            {
                // register the object for destruction on ret/panic
                builder->write_fn_addr("__destructor_for" + type.full_name());
                builder->write_1b_inst(RegObj);
            }
            builder->write_1b_inst(Pop); //alloca does a stack addr automatically
        }
        else
        {
            builder->write_alloca(type_size);
        }
        // TODO: stack addr of variables
        variables.back().emplace_back(name, std::pair{builder->last_alloc_addr(), type});
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
        auto tye = std::visit(ExpressionTypeChecker{ this }, stat->iterable->toVariant());
        if (!tye) {
            error(tye.error()); return;
        }
        auto& ty = *tye;
        if (!ty.is_mutable && ty.is_lvalue) error(Error(stat->iterable.get(), "Iterator object must be mutable or a temporary"));
        //check if it implements iterator interface
        if (ty.is_error_ty()) std::visit(YVMExpressionEvaluator{ this }, stat->iterable->toVariant());
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
                builder->write_fn_addr("__destructor_for_" + ty.full_name());
                builder->write_1b_inst(RegObj);
            }
            builder->write_alloca(NativeType::get_size(memory_ty));
            //------------- Iterator::next() args
            auto for_bb = builder->create_label("for_begin");
            builder->write_1b_inst(Dup); //< return addr
            builder->write_2b_inst(RevStackAddr, 2); //< iterable object
            //--------------------------------------------
            pushScope();
            auto for_cont = builder->unq_label_name("for_cont");
            builder->write_fn_addr(fn_name);
            builder->write_2b_inst(Call, 2);
            builder->write_1b_inst(Pop); // discard the return value
            // bring the return value to the top of the stack
            builder->write_1b_inst(Dup);
            // register this for destruction
            builder->write_fn_addr("__destructor_for_" + impl->methods[0]->signature.returnType.full_name());
            builder->write_1b_inst(RegObj);
            builder->write_ptr_off(NativeType::getElementOffset(memory_ty, 1));
            builder->write_2b_inst(Load, Yvm::Type::u8);

            builder->create_jump(JumpIfFalse, for_cont);
            //for loop body
            variables.back().emplace_back(stat->names[0].text, std::pair{builder->last_alloc_addr(), impl->impl_for.subtypes[0]});
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
        // since we're in a statement the current stack offset is guaranteed to be
        // number of fn params + number of allocas
        auto tp_e = std::visit(ExpressionTypeChecker{ this }, stat->condition->toVariant());
        if (!tp_e) {
            error(tp_e.error()); return;
        }
        if (!tp_e->is_optional() && !tp_e->is_conversion_result()) { error(Error(stat->condition.get(), "Expression cannot be extracted")); return; }
        if (!stat->else_capture.empty()) { debugbreak(); return; }
        if (stat->is_ref && tp_e->is_value_conversion_result()) { error(Error(stat->condition.get(), "Expanded expression cannot be borrowed", "")); return; }

        std::array<std::pair<Expression*, BorrowResult::borrow_result_t>, 1> borrow_res;
        borrow_res[0].first = stat->condition.get();
        borrow_res[0].second = tp_e->is_mutable ?
            std::visit(BorrowResult::LValueBorrowResult{ this }, stat->condition->toVariant()) :
            std::visit(BorrowResult{ this }, stat->condition->toVariant());
        validate_borrows(borrow_res, this);

        if (isShadowing(stat->captured_name)) { error(Error({}, {}, "Name is already in use")); return; }

        // all conditional extracntion types (optional types) all have a valid flag as thier second field
        auto as_native = reinterpret_cast<StructNativeTy*>(toNativeType(*tp_e));
        auto expr_eval = YVMExpressionEvaluator{ this };
        
        auto extensions = std::visit(expr_eval, stat->condition->toVariant());
        builder->write_1b_inst(Dup);
        builder->write_ptr_off(NativeType::getElementOffset(as_native, 1));
        builder->write_2b_inst(Load, Yvm::Type::u8);


        auto cont_block = builder->unq_label_name("cond_extract_cont");
        std::string else_block;
        if (stat->else_body) {
            else_block = builder->unq_label_name("cond_extract_else");
            builder->create_jump(JumpIfFalse, else_block);
        }
        else {
            builder->create_jump(JumpIfFalse, cont_block);
        }

        pushScope();

        builder->write_ptr_off(NativeType::getElementOffset(as_native, 0));
        // if its not a ref we have to clone it
        if (!tp_e->is_value_conversion_result() && !stat->is_ref) {
            if (tp_e->is_ref_conversion_result())
                builder->write_2b_inst(Load, Yvm::Type::ptr);
            if (!tp_e->subtypes[0].should_sret())
                builder->write_2b_inst(Load, toTypeEnum(tp_e->subtypes[0]));
            expr_eval.clone(stat->condition.get(), tp_e->subtypes[0], false, false);
        }
        Type variable_type = stat->is_ref ?
            Type{ tp_e->is_mutable ? "__ref_mut" : "__ref", {tp_e->subtypes[0]} } :
            tp_e->subtypes[0];
        variable_type.saturate(module, this);
        variables.back().emplace_back(stat->captured_name, std::pair{ builder->last_alloc_addr(), std::move(variable_type) });
        current_Statement = &stat->body;
        std::visit(*this, stat->body->toVariant());

        popScope();
        if (stat->else_body) {
            builder->create_jump(Jump, cont_block);
            builder->create_label(else_block);
            current_Statement = &stat->else_body;
            std::visit(*this, stat->else_body->toVariant());
        }

        builder->create_label(cont_block);
        

    }
    void YVMIRGenerator::operator()(WithStatement* stat)
    {
       
    }

    void YVMIRGenerator::operator()(OperatorOverload* ovl)
    {
        if (ovl->signature.parameters.size() == 2)
        {
            ovl->signature.parameters[0].type.saturate(module, this);
            ovl->signature.parameters[1].type.saturate(module, this);
            for (auto& det : module->overloads.binary_details_for(ovl->tok) | std::views::filter([this](auto& arg) {
                return arg.first == block_hash;
               })) {
                det.second.left.saturate(module, this);
                det.second.right.saturate(module, this);
                // generate the underlying function
                if (det.second.left.is_equal(ovl->signature.parameters[0].type) &&
                    det.second.right.is_equal(ovl->signature.parameters[1].type))
                {
                    FunctionDeclaration decl{ "", {}, std::move(ovl->body)};
                    doFunctionInternal(block_hash + det.second.mangled_name(ovl->tok), ovl->signature, &decl);
                    break;
                }
            }
        }
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
                builder->write_2b_inst(StackAddr, 0);
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

    bool canReturn(Statement* stat);

    void YVMIRGenerator::popScope()
    {
        callDestructors();
        variables.pop_back();
    }
    void YVMIRGenerator::callDestructors(size_t depth)
    {
        
    }

    void YVMIRGenerator::pushScope()
    {
        variables.emplace_back();
    }



    bool YVMIRGenerator::GenerateIR(std::string_view name, std::vector<std::unique_ptr<Statement>> statements, YVMModule* md, Engine* eng)
    {
        block_hash = md->module_hash;
        module = md;
        builder = std::make_unique<Yvm::Emitter>();
        pushScope();
        for (auto& stat : statements) {
            current_Statement = &stat;
            std::visit(*this, stat->toVariant());
        }
        builder = nullptr;
        return !has_error;
    }

    size_t YVMIRGenerator::nextKnownAddr()
    {
        return std::ranges::max(variables.back() | std::views::values | std::views::keys);
    }

    
}
