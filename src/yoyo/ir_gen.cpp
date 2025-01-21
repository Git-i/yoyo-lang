#include "ir_gen.h"

#include <csignal>
#include <list>
#include <ranges>
#include <set>
#include <iostream>
#include "gc/gc.h"
namespace Yoyo
{
    void debugbreak()
    {
#if _MSC_VER
        __debugbreak();
#endif
    }
    void dumpModule(IRGenerator* irgen)
    {
        irgen->code->print(llvm::outs(), nullptr);
    }
    llvm::Type* IRGenerator::ToLLVMType(const Type& type, bool is_ref)
    {
        //type is not required not have a module (built-ins)
        auto t = type.module ?
            type.module->ToLLVMType(type, block_hash, {}):
            module->ToLLVMType(type, block_hash, {});
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
        error(Error({1, 1}, {1, 1}, "Encountered unexpected type", ""));
        return nullptr;
    }
    extern "C"
    {
        YOYO_API  void* Yoyo_malloc_wrapper_dont_use_name(size_t size);
        void* Yoyo_malloc_wrapper_dont_use_name(size_t size)
        {
            return GC_malloc(size);
        }
    }
    
    llvm::Function* getMallocFunction(IRGenerator* irgen)
    {
        auto fn = irgen->code->getFunction("Yoyo_malloc_wrapper_dont_use_name");
        if (fn) return fn;
        auto type = llvm::FunctionType::get(llvm::PointerType::get(irgen->context, 0),
            { llvm::Type::getInt64Ty(irgen->context) }, false);
        fn = llvm::Function::Create(type, llvm::GlobalValue::ExternalLinkage, "Yoyo_malloc_wrapper_dont_use_name", irgen->code);
        return fn;
    }
    llvm::Value* IRGenerator::GCMalloc(size_t size)
    {
        auto fn = getMallocFunction(this);
        auto value = llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), size);
        return builder->CreateCall(fn, {value});
    }
    void IRGenerator::saturateSignature(FunctionSignature& sig, Module* module)
    {
        sig.returnType.saturate(module, this);
        for(auto& param: sig.parameters)
        {
            param.type.saturate(module, this);
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
        return false;
    }
    bool isValidCloneMethod(const Type& tp, const FunctionSignature& sig)
    {
        if(sig.parameters.size() != 1) return false;
        if(sig.parameters[0].type.name != "__ref") return false;
        if(!sig.parameters[0].type.subtypes[0].is_equal(tp)) return false;
        if(!sig.returnType.is_equal(tp)) return false;
        return true;
    }
    void IRGenerator::annotateClass(ClassDeclaration* decl)
    {
        bool has_no_clone = std::ranges::find_if(decl->attributes, [](Attribute& attr) {
            return attr.name == "no_clone";
        }) != decl->attributes.end();
        
        ClassMethod* clone_ptr = nullptr;
        ClassMethod* destructor = nullptr;
        for(auto& method: decl->methods)
        {
            if(auto it = std::ranges::find_if(method.function_decl->attributes, [](Attribute& attr) {
                return attr.name == "clone";
            }); it != method.function_decl->attributes.end())
            {
                if(clone_ptr) error(Error(method.function_decl.get(), "Multiple methods marked with #(clone)")); //mutiple clone methods
                if(has_no_clone) error(Error(method.function_decl.get(), "Clone method specified for class with #(no_clone)"));
                clone_ptr = &method;
            }
            if(auto it = std::ranges::find_if(method.function_decl->attributes, [](Attribute& attr) {
                return attr.name == "destructor";
            }); it != method.function_decl->attributes.end())
            {
                if(destructor) error(Error(method.function_decl.get(), "Multiple destructors specified for class"));
                destructor = &method;
            }
        }
        if(has_no_clone) decl->has_clone = false;
        else decl->has_clone = true;
        if(clone_ptr)
        {
            auto fn_decl = reinterpret_cast<FunctionDeclaration*>(clone_ptr->function_decl.get());
            if(!isValidCloneMethod(this_t, fn_decl->signature)) error(Error(fn_decl, "Invalid clone method signature"));
        }
        if (destructor)decl->destructor_name = destructor->name;
    }
    void IRGenerator::checkClass(ClassDeclaration* decl)
    {
        using namespace std::string_view_literals;
        using namespace std::ranges;
        for (auto& tp : decl->interfaces) tp.saturate(module, this);
        //TODO: check that all decl->interfaces is unique
        for (auto& impl : decl->impls)
        {
            impl.impl_for.saturate(module, this);
            auto it = std::ranges::find(decl->interfaces, impl.impl_for);
            if (it == decl->interfaces.end())
            {
                Error err(impl.location.begin, impl.location.end,
                    "Implementation is either a reimplementation or does not implement a previously declared interface");
                error(err);
                continue;
            }
            decl->interfaces.erase(it);
            Yoyo::InterfaceDeclaration* decl;
            if (!impl.impl_for.subtypes.empty())
            {
                auto [hash, generic] = impl.impl_for.module->findGenericInterface(impl.impl_for.block_hash, impl.impl_for.name);
                ExpressionEvaluator{ this }.generateGenericInterface(impl.impl_for.module, hash, generic, impl.impl_for.subtypes);
                decl = impl.impl_for.module->findInterface(impl.impl_for.block_hash, impl.impl_for.name + mangleGenericArgs(impl.impl_for.subtypes)).second;
            }
            else decl = impl.impl_for.module->findInterface(impl.impl_for.block_hash, impl.impl_for.name).second;
            if (!decl)
            {
                SourceLocation beg, end;
                beg.line = impl.location.begin.line;
                size_t off = impl.location.begin.column + "impl"sv.size();
                for (auto& line : subrange(view->lines.begin() + beg.line - 1, view->lines.end()))
                {
                    if (auto pos = line.find_first_not_of(" \n\t\r", off); pos != std::string_view::npos)
                    {
                        beg.column = pos + 1; break;
                    }
                    beg.line++; off = 0;
                }
                end.line = beg.line;
                off = beg.column - 1;
                for (auto& line : subrange(view->lines.begin() + beg.line - 1, view->lines.end()))
                {
                    if (auto pos = line.find_first_of('{', off); pos != std::string_view::npos)
                    {
                        off = pos; break;
                    }
                    end.line++; off = 0;
                }
                for (auto& line : subrange(view->lines.begin(), view->lines.begin() + end.line) | views::reverse)
                {
                    if (off == 0) off = line.size();
                    if (auto pos = line.find_last_not_of("{} \n\r\t", off); pos != std::string_view::npos)
                    {
                        end.column = pos + 2; break;
                    }
                    end.line--; off = 0;
                }
                Error err(impl.location.begin, impl.location.end, "Attempt to implement non-existent interface");
                err.markers.emplace_back(SourceSpan{ beg, end }, "Unrecognized interface type");
                error(err);
            }
        }
        annotateClass(decl);
    }
    llvm::Value* prepareValidDropFlagFor(IRGenerator* irgen, const Type& tp)
    {
        if(tp.is_trivially_destructible()) return nullptr;
        auto flag = irgen->Alloca("drop_flag", llvm::Type::getInt1Ty(irgen->context));
        irgen->builder->CreateStore(llvm::ConstantInt::getTrue(irgen->context), flag);
        return flag;
    }
    void IRGenerator::operator()(FunctionDeclaration* decl)
    {
        std::unique_ptr<llvm::IRBuilder<>> oldBuilder;
        auto old_return = return_t;
        auto old_ret_addr = currentReturnAddress;
        auto old_ret_block = returnBlock;

        if(builder->GetInsertBlock()) oldBuilder = std::make_unique<llvm::IRBuilder<>>(builder->GetInsertBlock(), builder->GetInsertPoint());
        auto name = block_hash + decl->name;
        llvm::Function* func = nullptr;
        bool uses_sret;

        llvm::Type* return_as_llvm_type;
        if((func = code->getFunction(name)))
        {
            if(!func->empty()) { error(Error(decl, "Function already exists")); return; }
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
            module->functions[block_hash].emplace_back(decl->name, decl->signature);
        }

        return_t = decl->signature.returnType;
        return_t.is_mutable = true;

        auto bb = llvm::BasicBlock::Create(context, "entry", func);
        returnBlock = llvm::BasicBlock::Create(context, "return", func);
        builder->SetInsertPoint(bb);
        if(!return_t.is_void()) currentReturnAddress = uses_sret ? static_cast<llvm::Value*>(func->getArg(0)) :
            static_cast<llvm::Value*>(Alloca("return_address", return_as_llvm_type));
        auto old_hash = block_hash;
        block_hash = name + "::";
        pushScope();
        CFGNode::prepareFromFunction(function_cfgs.emplace_back(CFGNodeManager{}), decl);
        function_cfgs.back().annotate();
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
                auto flag = prepareValidDropFlagFor(this, type);
                variables.back()[param.name] = {
                    var,
                    std::move(type),
                    flag
                };
            }
            //make lambda context visible TODO: really fix lambdas
            if(param.type.is_lambda())
            {
                //if(!lambdas.contains(param.type.name)) {error(); return;}
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
                        std::move(type).value(),
                        nullptr
                    };
                    capture_idx++;
                }
            }
            idx++;
        }
        current_Statement = &decl->body;
        std::visit(*this, decl->body->toVariant());
        function_cfgs.pop_back();
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
        auto ty = std::visit(ExpressionTypeChecker{this}, as_var).value_or_error();
        if(!ty.is_error_ty()) validate_expression_borrows(stat->expression.get(), this);
        auto eval = ExpressionEvaluator{this};
        auto val = std::visit(eval, as_var);
        if(!ty.is_lvalue)
            eval.destroy(val, ty);
    }
    void IRGenerator::operator()(InterfaceDeclaration* decl)
    {
        __debugbreak();
    }
    bool implementsInterfaceMethod(const FunctionSignature& cls, const FunctionSignature& interface);
    void IRGenerator::operator()(ClassDeclaration* decl)
    {
        std::string name(decl->identifier.text);
        if(isShadowing(name))
        {
            error(Error(decl, "The name '" + name + "' is already defined"));
            return;
        }
        auto ptr = current_Statement->release();
        assert(ptr == decl);
        std::string class_hash = block_hash + name + "::";
        for(auto& var : decl->vars) var.type.saturate(module, this);
        module->classes[block_hash].emplace_back(class_hash, hanldeClassDeclaration(decl->vars, decl->ownership, ""), std::unique_ptr<ClassDeclaration>{decl});

        in_class = true;
        auto old_this = std::move(this_t);
        this_t = Type{ .name = name, .subtypes = {} };
        this_t.saturate(module, this);
        checkClass(decl);
        for(auto& fn: decl->methods)
        {
            auto fn_decl = reinterpret_cast<FunctionDeclaration*>(fn.function_decl.get());
            auto curr_hash = reset_hash();
            block_hash = class_hash;
            current_Statement = &fn.function_decl;
            (*this)(fn_decl);
            block_hash = std::move(curr_hash);
        }
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
        in_class = false;
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
                    return Type{.name="i32", .module = src.module,.is_mutable = src.is_mutable,.is_lvalue = src.is_lvalue};
                return Type{.name="i64",.module = src.module,.is_mutable = src.is_mutable,.is_lvalue = src.is_lvalue};
            }
            uint64_t int_val = as_int->getZExtValue();
            if(int_val <= std::numeric_limits<int32_t>::max())
                return Type{.name="i32",.module = src.module,.is_mutable = src.is_mutable,.is_lvalue = src.is_lvalue};
            if(int_val <= std::numeric_limits<uint32_t>::max())
                return Type{.name="u32",.module = src.module,.is_mutable = src.is_mutable,.is_lvalue = src.is_lvalue};
            if(int_val <= std::numeric_limits<int64_t>::max())
                return Type{.name="i64",.module = src.module,.is_mutable = src.is_mutable,.is_lvalue = src.is_lvalue};
            return Type{.name="u64",.module = src.module, .is_mutable = src.is_mutable,.is_lvalue = src.is_lvalue,};
        }
        if(src.name == "flit") return Type{.name = "f64",.module = src.module,.is_mutable = src.is_mutable,.is_lvalue = src.is_lvalue};
        //unreachble
        return Type{};
    }
    void IRGenerator::operator()(VariableDeclaration* decl)
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
        if(type.is_non_owning()) { error(Error(decl, "Variable types must not be non-owning")); return; }
        type.is_mutable = decl->is_mut;
        type.is_lvalue = true;
        decl->type = type;
        //TODO probably consider copying lambda contexts??
        llvm::Value* alloc = nullptr;
        llvm::Value* drop_flag = nullptr;
        if(!type.is_trivially_destructible())
        {
            std::string name = "drop_flag_for_" + std::string{decl->identifier.text};
            drop_flag = Alloca(name, llvm::Type::getInt1Ty(context));
        }
        if(decl->initializer)
        {
            auto expr_type = std::visit(ExpressionTypeChecker{this, type}, decl->initializer->toVariant()).value_or_error();
            if(!expr_type.is_error_ty()) validate_expression_borrows(decl->initializer.get(), this);
            auto eval = ExpressionEvaluator{this, type};
            auto init = std::visit(eval, decl->initializer->toVariant());
            if(decl->type->name == "ilit" || decl->type->name == "flit")
            {
                decl->type = reduceLiteral(*decl->type, init);
                type = decl->type.value();
            }
            if(!type.should_sret())
            {
                alloc = Alloca(decl->identifier.text, ToLLVMType(type, false));
                ExpressionEvaluator{this}.implicitConvert(decl->initializer.get(), init, expr_type, type, alloc);
            } else alloc = ExpressionEvaluator{this}.implicitConvert(decl->initializer.get(), init, expr_type, type, nullptr);
            alloc->setName(decl->identifier.text);
            if(drop_flag) builder->CreateStore(llvm::ConstantInt::getTrue(context), drop_flag);
        }
        else
            if(drop_flag) builder->CreateStore(llvm::ConstantInt::getFalse(context), drop_flag);
        type.saturate(module, this);
        if(!alloc) alloc = Alloca(decl->identifier.text, ToLLVMType(type, false));

        variables.back()[name] = {alloc, std::move(type), drop_flag};
    }
    void IRGenerator::operator()(BlockStatement* stat)
    {
        pushScope();
        for(auto& sub_stat : stat->statements)
        {
            current_Statement = &sub_stat;
            std::visit(*this, sub_stat->toVariant());
            if(dynamic_cast<ReturnStatement*>(sub_stat.get()))
            {
                popScope(); return;
            }
        }
        popScope();
    }
    void IRGenerator::operator()(ForStatement* stat)
    {
        auto fn = builder->GetInsertBlock()->getParent();
        auto ty = std::visit(ExpressionTypeChecker{ this }, stat->iterable->toVariant()).value_or_error();
        if (!ty.is_mutable && ty.is_lvalue) error(Error(stat->iterable.get(), "Iterator object must be mutable or a temporary"));
        //check if it implements iterator interface
        auto value = std::visit(ExpressionEvaluator{ this }, stat->iterable->toVariant());
        if (auto cls = ty.module->findType(ty.block_hash, ty.name))
        {
            auto decl = std::get<2>(*cls).get();
            auto hash = std::get<0>(*cls);
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
            std::string fn_name = hash + "coreIterator" + mangleGenericArgs(impl->impl_for.subtypes) + "::next";
            auto next_fn = code->getFunction(fn_name);
            auto memory_ty = ToLLVMType(impl->methods[0]->signature.returnType, false);
            auto memory = Alloca("for_obj", memory_ty);
            auto flg = prepareValidDropFlagFor(this, impl->impl_for.subtypes[0]);
            pushScope();
            auto for_bb = llvm::BasicBlock::Create(context, "for", fn, returnBlock);
            auto then_bb = llvm::BasicBlock::Create(context, "forthen", fn, returnBlock);
            auto cont_bb = llvm::BasicBlock::Create(context, "forcont", fn, returnBlock);
            builder->CreateBr(for_bb);
            builder->SetInsertPoint(for_bb);
            builder->CreateCall(next_fn, {memory, value});
            auto has_next = builder->CreateStructGEP(memory_ty, memory, 1);
            has_next = builder->CreateLoad(llvm::Type::getInt1Ty(context), has_next);
            builder->CreateCondBr(has_next, then_bb, cont_bb);
            builder->SetInsertPoint(then_bb);
            if (flg) builder->CreateStore(llvm::ConstantInt::getTrue(context), flg);
            auto val = builder->CreateStructGEP(memory_ty, memory, 0);
            variables.back()[std::string{ stat->names[0].text }] = {val , impl->impl_for.subtypes[0], flg};

            current_Statement = &stat->body;
            std::visit(*this, stat->body->toVariant());
            popScope();
            if (builder->GetInsertBlock()->back().getOpcode() != llvm::Instruction::Br) builder->CreateBr(for_bb);
            builder->SetInsertPoint(cont_bb);
        }
        
    }
    void IRGenerator::operator()(WhileStatement* expr)
    {
        auto fn = builder->GetInsertBlock()->getParent();
        if(!std::visit(ExpressionTypeChecker{this}, expr->condition->toVariant())->is_boolean())
        {
            error(Error(expr->condition.get(), "Condition in 'while' must evaluate to a boolean"));
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
        auto tp_e = std::visit(ExpressionTypeChecker{this}, stat->condition->toVariant());
        auto expression = std::visit(ExpressionEvaluator{this}, stat->condition->toVariant());
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
            if(tp.is_ref_conversion_result())
                ptr = builder->CreateLoad(llvm::PointerType::get(context, 0), ptr);
            //if it's not a `ref` we clone the value
            if(!stat->is_ref)
            {
                llvm::Value* into = nullptr;
                if(!tp.subtypes[0].should_sret())
                {
                    auto as_llvm = ToLLVMType(tp.subtypes[0], false);
                    into = Alloca("", as_llvm);
                    ptr = builder->CreateLoad(as_llvm, ptr);
                }
                ptr = ExpressionEvaluator{this}.clone(stat->condition.get(), ptr, tp.subtypes[0], into);
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
        ExpressionEvaluator{this}.destroy(expression, tp);
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
    void IRGenerator::operator()(WithStatement* stat)
    {
        auto ty = std::visit(ExpressionTypeChecker{this}, stat->expression->toVariant()).value_or_error();
        if (!ty.is_non_owning()) { error(Error(stat->expression.get(), "'with' statement can only capture non-owning types")); return; }
        if (isShadowing(stat->name)) { error(Error({}, {}, "Name is already in use")); return; }

        ty.is_mutable = ty.is_non_owning_mut();

        std::array<std::pair<Expression*, BorrowResult::borrow_result_t>, 1> borrow_res;
        borrow_res[0].first = stat->expression.get();
        borrow_res[0].second = ty.is_mutable ?
            std::visit(BorrowResult::LValueBorrowResult{this}, stat->expression->toVariant()):
            std::visit(BorrowResult{this}, stat->expression->toVariant());
        validate_borrows(borrow_res, this);

        auto expr = std::visit(ExpressionEvaluator{this}, stat->expression->toVariant());
        llvm::Value* val;
        auto names = borrow_res[0].second | std::views::keys;
        pushScopeWithConstLock(names.begin(), names.end());
        lifetimeExtensions[stat->name] = std::move(borrow_res[0].second);

        if(!ty.should_sret())
        {
            val = Alloca(stat->name, ToLLVMType(ty, false));
            ExpressionEvaluator{this}.clone(stat->expression.get(), expr, ty, val);
        } else val = ExpressionEvaluator{this}.clone(stat->expression.get(), expr, ty);

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

    void IRGenerator::operator()(OperatorOverload*)
    {
        debugbreak();
    }

    void IRGenerator::operator()(GenericFunctionDeclaration*)
    {
        debugbreak();
    }

    void IRGenerator::operator()(AliasDeclaration* decl)
    {
        auto hash = block_hash;
        block_hash += decl->name + "::"; //in the case of generics
        decl->type.saturate(module, this);
        block_hash = std::move(hash);
        module->aliases[block_hash].emplace(decl->name, decl->type);
    }

    void IRGenerator::operator()(GenericAliasDeclaration*)
    {
        debugbreak();
    }

    void IRGenerator::error(const Error& e)
    {
        auto str = e.to_string(*view, true);
        std::cout << str << std::endl;
        has_error = true;
        debugbreak();
    }

    std::string IRGenerator::reset_hash()
    {
        auto old = std::move(block_hash);
        block_hash = module->module_hash;
        return old;
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

    std::string IRGenerator::mangleGenericArgs(std::span<const Type> list)
    {
        if (list.empty()) return "";
        std::string final = "::<" + list[0].full_name();
        for(auto& tp : std::ranges::subrange(list.begin() + 1, list.end()))
            final += "," + tp.full_name();
        final += ">";
        return final;
    }

    void IRGenerator::operator()(ReturnStatement* stat)
    {
        if(stat->expression)
        {
            auto t = std::visit(ExpressionTypeChecker{this, return_t}, stat->expression->toVariant()).value_or_error();
            if(!return_t.is_assignable_from(t)) { error(Error(stat, "Type is not convertible to return type")); return; }
            // `doAssign` for reference types works like c++ (dereference and assign) rather than rebind the ref, because
            // references cannot be rebound, except in return statements, which is why we handle them specially
            if (return_t.is_non_owning())
                if (!std::visit(LifetimeExceedsFunctionChecker{ this }, stat->expression->toVariant())) debugbreak();
            auto value = std::visit(ExpressionEvaluator{this, return_t}, stat->expression->toVariant());
            ExpressionEvaluator{ this }.implicitConvert(stat->expression.get(), value, t, return_t, currentReturnAddress);
            builder->CreateBr(returnBlock);
        }
        else
            builder->CreateBr(returnBlock);
    }

    void IRGenerator::operator()(IfStatement* stat)
    {
        auto fn = builder->GetInsertBlock()->getParent();
        auto expr_type = std::visit(ExpressionTypeChecker{this}, stat->condition->toVariant()).value();
        if(!expr_type.is_boolean())
        {
            error(Error(stat->condition.get(), "'if' condition must evaluate to a boolean")); return;
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

    void IRGenerator::popScope()
    {
        //if there's a `br` we steal it and add it after calling destructors
        llvm::Instruction* term = nullptr;
        if(builder->GetInsertBlock()->back().getOpcode() == llvm::Instruction::Br)
        {
            term = &builder->GetInsertBlock()->back();
            term->removeFromParent();
        }
        //call destructors
        auto fn = builder->GetInsertBlock()->getParent();
        for(auto& var : variables.back() | std::views::values)
        {
            auto drop_flag = std::get<2>(var);
            auto& type = std::get<1>(var);
            if(!drop_flag) continue;
            auto drop = llvm::BasicBlock::Create(context, "drop_var", fn, returnBlock);
            auto drop_cont = llvm::BasicBlock::Create(context, "drop_cont", fn, returnBlock);
            builder->CreateCondBr(
                builder->CreateLoad(llvm::Type::getInt1Ty(context), drop_flag),
                drop, drop_cont);
            builder->SetInsertPoint(drop);
            auto to_drop = std::get<0>(var);
            if(!type.should_sret())
            {
                std::string name(16 + sizeof(void*), 'c');
                if(to_drop->getName().starts_with("__del_parents"))
                    memcpy(name.data(), to_drop->getName().data(), 16 + sizeof(void*));
                to_drop = builder->CreateLoad(ToLLVMType(type, false), to_drop, name);
            }
            ExpressionEvaluator{this}.destroy(to_drop, std::get<1>(var));
            builder->CreateBr(drop_cont);
            builder->SetInsertPoint(drop_cont);
        }
        if (term)
            term->insertInto(builder->GetInsertBlock(), builder->GetInsertPoint());
        variables.pop_back();
    }

    //The resultant return type is the type of the first return statement encountered
    std::optional<Type> IRGenerator::inferReturnType(Statement* stat)
    {
        if(!canReturn(stat)) return Type{.name = "void"};
        if(auto res = dynamic_cast<ReturnStatement*>(stat))
            return std::visit(ExpressionTypeChecker{this}, res->expression->toVariant()).to_optional();
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

    llvm::StructType* IRGenerator::hanldeClassDeclaration(std::span<const ClassVariable> vars, Ownership own, std::string_view name)
    {
        std::vector<std::string> var_names(vars.size());

        std::vector<llvm::Type*> args(vars.size());
        std::ranges::transform(vars, args.begin(), [this, own](const ClassVariable& p)
        {
                if (own == Ownership::Owning && p.type.is_non_owning()) error(Error({}, {}, "Non owning type in owning class"));
                if (own == Ownership::NonOwning && p.type.is_non_owning_mut()) error(Error({}, {}, "Mutably non owning type in non owning class"));
            return ToLLVMType(p.type, false);
        });
        if(name.empty())
        {
            return llvm::StructType::get(context, args);
        }
        return llvm::StructType::create(context, args, name);
    }

    bool IRGenerator::GenerateIR(std::string_view name, std::vector<std::unique_ptr<Statement>> statements, Module* md, Engine* eng)
    {
        block_hash = md->module_hash;
        md->code = llvm::orc::ThreadSafeModule(std::make_unique<llvm::Module>(name, context), eng->llvm_context);
        module = md;
        code = md->code.getModuleUnlocked();
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
        return !has_error;
    }

}
