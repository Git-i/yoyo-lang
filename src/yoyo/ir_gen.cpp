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
            type.module->ToLLVMType(type, block_hash, this, {}):
            module->ToLLVMType(type, block_hash, this, {});
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
                ExpressionEvaluator{ this }.generateGenericClass(
                    type.module,
                    type.block_hash,
                    exists,
                    type.subtypes); //this should generally be safe(hopefully)
                std::string name = type.name + mangleGenericArgs(type.subtypes);
                if (auto det = type.module->findType(type.block_hash, name))
                    return std::get<1>(*det);
            }
                
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
            if(p.type.should_sret())
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
        
        FunctionDeclaration* clone_ptr = nullptr;
        FunctionDeclaration* destructor = nullptr;
        for(auto& method: decl->stats)
        {
            if (!method) continue;
            if(auto it = std::ranges::find_if(method->attributes, [](Attribute& attr) {
                return attr.name == "clone";
            }); it != method->attributes.end())
            {
                if(clone_ptr) error(Error(method.get(), "Multiple methods marked with #(clone)")); //mutiple clone methods
                if(has_no_clone) error(Error(method.get(), "Clone method specified for class with #(no_clone)"));
                clone_ptr = dynamic_cast<FunctionDeclaration*>(method.get());
            }
            if(auto it = std::ranges::find_if(method->attributes, [](Attribute& attr) {
                return attr.name == "destructor";
            }); it != method->attributes.end())
            {
                if(destructor) error(Error(method.get(), "Multiple destructors specified for class"));
                destructor = dynamic_cast<FunctionDeclaration*>(method.get());
            }
        }
        if(has_no_clone) decl->has_clone = false;
        else decl->has_clone = true;
        if(clone_ptr)
        {
            auto fn_decl = clone_ptr;
            if(!isValidCloneMethod(this_t, fn_decl->signature)) error(Error(fn_decl, "Invalid clone method signature"));
        }
        if (destructor)decl->destructor_name = destructor->name;
    }
    void IRGenerator::checkClass(ClassDeclaration* decl)
    {
        using namespace std::string_view_literals;
        using namespace std::ranges;
        //TODO: check that all decl->interfaces is unique
        for (auto& impl : decl->impls)
        {
            impl.impl_for.saturate(module, this);
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
        if(tp.is_trivially_destructible(irgen)) return nullptr;
        auto flag = irgen->Alloca("drop_flag", llvm::Type::getInt1Ty(irgen->context));
        irgen->builder->CreateStore(llvm::ConstantInt::getTrue(irgen->context), flag);
        return flag;
    }
    bool should_sret_C(llvm::Type* tp, Module* md)
    { 
        if (tp->isVoidTy()) return false;
        const auto& target = md->engine->jit->getTargetTriple();
        constexpr auto npos = std::string::npos;
        if (target.getArch() == llvm::Triple::x86_64 && target.getEnvironment() == llvm::Triple::MSVC)
        {
            size_t sz = md->engine->jit->getDataLayout().getTypeSizeInBits(tp);
            sz = sz / 8; //convert to bytes
            //only sret if its a small type that's not a power of 2
            if (sz > sizeof(uint64_t) || (sz & (sz - 1)) != 0) return true;
            return false;
        }
        debugbreak();
    }
    bool should_sptr_C(llvm::Type* tp, Module* md)
    {
        const auto& target = md->engine->jit->getTargetTriple();
        if (target.getArch() == llvm::Triple::x86_64 && target.getEnvironment() == llvm::Triple::MSVC)
        {
            size_t sz = md->engine->jit->getDataLayout().getTypeSizeInBits(tp);
            sz = sz / 8; //convert to bytes
            //only sret if its a small type that's not a power of 2
            if (sz > sizeof(uint64_t) || (sz & (sz - 1)) != 0) return true;
            return false;
        }
    }
    llvm::FunctionType* toCSignature(IRGenerator* irgen, const FunctionSignature& sig)
    {
        llvm::Type* return_t = nullptr;
        auto return_as_llvm = irgen->ToLLVMType(sig.returnType, false);
        bool should_sret = should_sret_C(return_as_llvm, irgen->module);
        std::vector<llvm::Type*> arg_tys;
        if (!return_as_llvm->isVoidTy()) {
            if (should_sret)
            {
                return_t = llvm::Type::getVoidTy(irgen->context);
                arg_tys.push_back(llvm::PointerType::get(irgen->context, 0));
            }
            //use an integer register win64 only
            else return_t = llvm::IntegerType::get(irgen->context,
                irgen->module->engine->jit->getDataLayout().getTypeSizeInBits(return_as_llvm));
        }
        else return_t = return_as_llvm;
        
        for (auto& param : sig.parameters) {
            auto as_llvm = irgen->ToLLVMType(param.type, false);
            if (should_sptr_C(as_llvm, irgen->module)) {
                arg_tys.push_back(llvm::PointerType::get(irgen->context, 0));
            }
            else {
                arg_tys.push_back(as_llvm->isPointerTy() ? as_llvm : llvm::IntegerType::get(irgen->context,
                    irgen->module->engine->jit->getDataLayout().getTypeSizeInBits(as_llvm)));
            }
        }
        return llvm::FunctionType::get(return_t, arg_tys, false);
    }
    
    void handleCImport(IRGenerator* irgen, FunctionDeclaration* decl, llvm::Function* func)
    {
        irgen->code->getTargetTriple();
        std::vector<llvm::Value*> args;
        bool return_sret = false;
        size_t offset = decl->signature.returnType.should_sret();
        if (decl->signature.returnType.should_sret())
        {
            auto as_llvm = irgen->ToLLVMType(decl->signature.returnType, false);
            return_sret = should_sret_C(as_llvm, irgen->module);
            if (return_sret) args.push_back(func->getArg(0));
        }
        auto c_import = reinterpret_cast<CImportDeclaration*>(decl->body.get());
        auto sig = toCSignature(irgen, decl->signature);
        auto c_fn = llvm::Function::Create(sig, llvm::GlobalValue::ExternalLinkage, c_import->function_name, irgen->code);
        size_t param_idx = 0;
        for (auto arg : std::ranges::views::iota(offset, func->arg_size()))
        {
            auto arg_val = func->getArg(arg);

            if (decl->signature.parameters[param_idx].type.should_sret()) {
                
                auto as_llvm = irgen->ToLLVMType(decl->signature.parameters[param_idx].type, false);
                if (should_sptr_C(as_llvm, irgen->module)) {
                    args.push_back(arg_val);
                }
                else {
                    auto ty = llvm::IntegerType::get(irgen->context,
                        irgen->module->engine->jit->getDataLayout().getTypeSizeInBits(as_llvm));
                    args.push_back(irgen->builder->CreateLoad(ty, arg_val));
                }
            }
            else args.push_back(arg_val);
            param_idx++;
        }
        auto call_val = irgen->builder->CreateCall(c_fn, args);
        if (decl->signature.returnType.is_void()) irgen->builder->CreateRetVoid();
        else if (decl->signature.returnType.should_sret())
        {
            if (!return_sret)
                irgen->builder->CreateStore(call_val, func->getArg(0));
            irgen->builder->CreateRetVoid();
        }
        else
        {
            irgen->builder->CreateRet(call_val);
        }
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
        auto this_entry = module->findFunction(block_hash, decl->name).second;
        if((func = code->getFunction(name)))
        {
            if(!func->empty()) { error(Error(decl, "Function already exists")); return; }
            uses_sret = func->hasStructRetAttr();
            return_as_llvm_type = func->getParamStructRetType(0);
        }
        else
        {
            saturateSignature(this_entry->sig, module);
            return_as_llvm_type = ToLLVMType(this_entry->sig.returnType, false);
            func = llvm::Function::Create(ToLLVMSignature(this_entry->sig), llvm::GlobalValue::ExternalLinkage, name, code);
            uses_sret = decl->signature.returnType.should_sret();
            if(uses_sret)
                func->addAttributeAtIndex(1, llvm::Attribute::get(context, llvm::Attribute::StructRet, return_as_llvm_type));
        }


        auto bb = llvm::BasicBlock::Create(context, "entry", func);
        builder->SetInsertPoint(bb);

        if (dynamic_cast<CImportDeclaration*>(decl->body.get()))
        {
            saturateSignature(decl->signature, module);
            handleCImport(this, decl, func);
            if (oldBuilder) builder.swap(oldBuilder);
            return;

        }
        return_t = this_entry->sig.returnType;
        return_t.is_mutable = true;

        returnBlock = llvm::BasicBlock::Create(context, "return", func);
        if(!return_t.is_void()) currentReturnAddress = uses_sret ? static_cast<llvm::Value*>(func->getArg(0)) :
            static_cast<llvm::Value*>(Alloca("return_address", return_as_llvm_type));
        auto old_hash = block_hash;
        block_hash = name + "::";
        pushScope();
        CFGNode::prepareFromFunction(function_cfgs.emplace_back(CFGNodeManager{}), decl);
        function_cfgs.back().annotate();
        size_t idx = 0;
        decltype(this->variables) new_fn_vars;
        new_fn_vars.emplace_back();
        for(auto& param : this_entry->sig.parameters)
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
                new_fn_vars.back()[param.name] = {
                    var,
                    std::move(type),
                    flag
                };
            }
            if(param.type.is_lambda())
            {
                auto& entry = param.type.module->lambdas[param.type.name];
                auto llvm_type = entry.first;
                auto& caps = entry.second->captures;
                size_t capture_idx = 0;
                auto arg = func->getArg(idx + uses_sret);
                for(auto& capture: caps)
                {
                    auto var = builder->CreateStructGEP(llvm_type, arg, capture_idx);
                    NameExpression nexpr(capture.name);
                    auto type = ExpressionTypeChecker{this}(&nexpr);
                    Type final_type;
                    if (capture.cp_type == Ownership::NonOwning) final_type = type->reference_to();
                    else if (capture.cp_type == Ownership::NonOwningMut) final_type = type->mutable_reference_to();
                    else final_type = std::move(type).value();
                    new_fn_vars.back()[capture.name] = {
                        var,
                        std::move(final_type),
                        nullptr
                    };
                    capture_idx++;
                }
            }
            idx++;
        }
        
        //evaluate body we give a new scope whatever so that the function can't access outside vars
        variables.swap(new_fn_vars);
        current_Statement = &decl->body;
        std::visit(*this, decl->body->toVariant());
        variables.swap(new_fn_vars);

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
    void IRGenerator::operator()(ConstantDeclaration* decl)
    {
        auto [_, constant] = module->findConst(block_hash, decl->name);
        std::get<0>(*constant).saturate(module, this);
        auto val = std::visit(ConstantEvaluator{ this }, decl->expr->toVariant());
        if (auto gv = llvm::dyn_cast_or_null<llvm::GlobalVariable>(val)) {
            val->setName(block_hash + decl->name);
            code->insertGlobalVariable(gv);
        }
        std::get<2>(*constant) = val;
    }
    void IRGenerator::operator()(InterfaceDeclaration* decl)
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
    void IRGenerator::operator()(EnumDeclaration* decl)
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

    void IRGenerator::operator()(UnionDeclaration* decl)
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
    };
    void IRGenerator::operator()(ClassDeclaration* decl)
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
        auto cls = module->findType(curr_hash, decl->name);
        for(auto& var : decl->vars) var.type.saturate(module, this);
        std::get<1>(*cls) = hanldeClassDeclaration(decl->vars, decl->ownership, "");
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
        if(type.is_non_owning(this)) { error(Error(decl, "Variable types must not be non-owning")); return; }
        type.is_mutable = decl->is_mut;
        type.is_lvalue = true;
        decl->type = type;
        //TODO probably consider copying lambda contexts??
        llvm::Value* alloc = nullptr;
        llvm::Value* drop_flag = nullptr;
        if(!type.is_trivially_destructible(this))
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
        if (auto cls = ty.get_decl_if_class(this))
        {
            auto decl = cls;
            auto hash = std::get<0>(*ty.module->findType(ty.block_hash, cls->name));
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
            auto old_break_to = break_to; auto old_cont_to = continue_to;
            break_to = cont_bb; continue_to = for_bb;
            std::visit(*this, stat->body->toVariant());
            break_to = old_break_to; continue_to = old_cont_to;
            popScope();
            if (builder->GetInsertBlock()->back().getOpcode() != llvm::Instruction::Br) builder->CreateBr(for_bb);
            builder->SetInsertPoint(cont_bb);
        }
        
    }
    void IRGenerator::operator()(BreakStatement* s)
    {
        if (!break_to) error(Error(s, "'break' must be used within a loop"));
        else
        {
            callDestructors();
            builder->CreateBr(break_to);
        }
    }
    void IRGenerator::operator()(ContinueStatement* s)
    {
        if (!continue_to) error(Error(s, "'continue' statement must be used within a loop"));
        else
        {
            callDestructors();
            builder->CreateBr(continue_to);
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
        pushScope();
        auto old_break_to = break_to; auto old_cont_to = continue_to;
        break_to = cont_bb; continue_to = while_bb;
        std::visit(*this, expr->body->toVariant());
        break_to = old_break_to; continue_to = old_cont_to;
        popScope();
        if(builder->GetInsertBlock()->back().getOpcode() != llvm::Instruction::Br) builder->CreateBr(while_bb);
        builder->SetInsertPoint(cont_bb);
    }

    void IRGenerator::operator()(ConditionalExtraction* stat)
    {
        auto tp_e = std::visit(ExpressionTypeChecker{this}, stat->condition->toVariant());
        auto expression = std::visit(ExpressionEvaluator{ this }, stat->condition->toVariant());
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
        if (!ty.is_non_owning(this)) { error(Error(stat->expression.get(), "'with' statement can only capture non-owning types")); return; }
        if (isShadowing(stat->name)) { error(Error({}, {}, "Name is already in use")); return; }

        ty.is_mutable = ty.is_non_owning_mut(this);

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
    void IRGenerator::operator()(GenericClassDeclaration*)
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
            if(!return_t.is_assignable_from(t, this)) { error(Error(stat, "Type is not convertible to return type")); return; }
            // `doAssign` for reference types works like c++ (dereference and assign) rather than rebind the ref, because
            // references cannot be rebound, except in return statements, which is why we handle them specially
            if (return_t.is_non_owning(this))
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
        auto expr_type = std::visit(ExpressionTypeChecker{this}, stat->condition->toVariant()).value_or_error();
        if(!expr_type.is_boolean() && !expr_type.is_error_ty())
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
        callDestructors();
        variables.pop_back();
    }
    void IRGenerator::callDestructors()
    {
        //if there's a `br` we steal it and add it after calling destructors
        llvm::Instruction* term = nullptr;
        if (builder->GetInsertBlock()->back().getOpcode() == llvm::Instruction::Br)
        {
            term = &builder->GetInsertBlock()->back();
            term->removeFromParent();
        }
        //call destructors
        auto fn = builder->GetInsertBlock()->getParent();
        for (auto& var : variables.back() | std::views::values)
        {
            auto drop_flag = std::get<2>(var);
            auto& type = std::get<1>(var);
            if (!drop_flag) continue;
            auto drop = llvm::BasicBlock::Create(context, "drop_var", fn, returnBlock);
            auto drop_cont = llvm::BasicBlock::Create(context, "drop_cont", fn, returnBlock);
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
            ExpressionEvaluator{ this }.destroy(to_drop, std::get<1>(var));
            builder->CreateBr(drop_cont);
            builder->SetInsertPoint(drop_cont);
        }
        if (term)
            term->insertInto(builder->GetInsertBlock(), builder->GetInsertPoint());
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
                if (own == Ownership::Owning && p.type.is_non_owning(this)) error(Error({}, {}, "Non owning type in owning class"));
                if (own == Ownership::NonOwning && p.type.is_non_owning_mut(this)) error(Error({}, {}, "Mutably non owning type in non owning class"));
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
        for (auto& stat : statements) {
            current_Statement = &stat;
            std::visit(*this, stat->toVariant());
        }
        builder = nullptr;
        return !has_error;
    }

}
