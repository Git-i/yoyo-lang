#include <cmath>
#include <csignal>
#include <overload_resolve.h>
#include <ranges>
#include <set>
#include <tree_cloner.h>
#include <llvm/Support/Error.h>

#include "llvm/llvm_irgen.h"
#include "fn_type.h"
namespace Yoyo
{
    struct ExtendedLifetimes
    {
        std::vector<std::pair<Type, llvm::Value*>> objects;
    };
    void stealUsages(std::span<llvm::Value*> args, llvm::Value* out)
    {
        ExtendedLifetimes* ext = nullptr;
        for(auto value: args)
        {
            if(value->getName().starts_with("__del_parents___"))
            {
                if(!ext) ext = new ExtendedLifetimes;
                auto nm = value->getName();
                auto& lifetimes = **reinterpret_cast<ExtendedLifetimes* const*>(nm.data() + 16);
                ext->objects.insert(ext->objects.end(),
                    std::make_move_iterator(lifetimes.objects.begin()),
                    std::make_move_iterator(lifetimes.objects.end()));
                delete &lifetimes;
            }
        }
        if(ext)
        {
            std::string name = "__del_parents___aaaaaaaa";
            memcpy(name.data() + 16, &ext, sizeof(void*));
            out->setName(name);
        }
    }
    void clearUsages(std::vector<llvm::Value*>& args, const LLVMExpressionEvaluator& eval)
    {
        for(auto value: args)
        {
            if (!value) continue;
            if(value->getName().starts_with("__del_parents___"))
            {
                auto nm = value->getName();
                auto& lifetimes = **reinterpret_cast<ExtendedLifetimes* const*>(nm.data() + 16);
                for(auto&[type, obj]: lifetimes.objects)
                    eval.destroy(obj, type);
                delete &lifetimes;
            }
        }
    }
    llvm::Function* declareFunction(const std::string& mangled_name, LLVMIRGenerator* irgen, FunctionSignature& fn_sig)
    {
        llvm::Function* fn = nullptr;
        irgen->saturateSignature(fn_sig, irgen->module);
        fn = llvm::Function::Create(irgen->ToLLVMSignature(fn_sig), llvm::GlobalValue::ExternalLinkage, mangled_name,
            irgen->code);
        if(fn_sig.returnType.should_sret())
        {
            const auto return_as_llvm = irgen->ToLLVMType(fn_sig.returnType, false);
            fn->addAttributeAtIndex(1, llvm::Attribute::get(irgen->context, llvm::Attribute::StructRet, return_as_llvm));
        }
        return fn;
    }
    int64_t getIntMinOf(const Type& type)
    {
        if(type.is_signed_integral())
        {
            switch (*type.integer_width())
            {
            case 8: return std::numeric_limits<int8_t>::min();
            case 16: return std::numeric_limits<int16_t>::min();
            case 32: return std::numeric_limits<int32_t>::min();
            case 64: return std::numeric_limits<int64_t>::min();
            default: return 0;/*unreachable*/
            }
        }
        if(type.is_unsigned_integral())
        {
            switch (*type.integer_width())
            {
            case 8: return std::numeric_limits<uint8_t>::min();
            case 16: return std::numeric_limits<uint16_t>::min();
            case 32: return std::numeric_limits<uint32_t>::min();
            case 64: return std::numeric_limits<uint64_t>::min();
            default: return 0;/*unreachable*/
            }
        }
        if(type.is_floating_point())
        {
            if(*type.float_width() == 32) return -std::pow(2, 24);
            if(*type.float_width() == 64) return -std::pow(2, 53);
        }
        return 0;
    }
    uint64_t getIntMaxOf(const Type& type)
    {
        if(type.is_signed_integral())
        {
            switch (*type.integer_width())
            {
            case 8: return std::numeric_limits<int8_t>::max();
            case 16: return std::numeric_limits<int16_t>::max();
            case 32: return std::numeric_limits<int32_t>::max();
            case 64: return std::numeric_limits<int64_t>::max();
            default: return 0;/*unreachable*/
            }
        }
        if(type.is_unsigned_integral())
        {
            switch (*type.integer_width())
            {
            case 8: return std::numeric_limits<uint8_t>::max();
            case 16: return std::numeric_limits<uint16_t>::max();
            case 32: return std::numeric_limits<uint32_t>::max();
            case 64: return std::numeric_limits<uint64_t>::max();
            default: return 0;/*unreachable*/
            }
        }
        if(type.is_floating_point())
        {
            if(*type.float_width() == 32) return std::pow(2, 24);
            if(*type.float_width() == 64) return std::pow(2, 53);
        }
        return 0;
    }
    double getFloatMinOf(const Type& type)
    {
        if(type.is_floating_point())
        {
            if(*type.float_width() == 32) return std::numeric_limits<float>::min();
            if(*type.float_width() == 64) return std::numeric_limits<double>::min();
        }
        return 0;
    }

    double getFloatMaxOf(const Type& type)
    {
        if(type.is_floating_point())
        {
            if(*type.float_width() == 32) return std::numeric_limits<float>::max();
            if(*type.float_width() == 64) return std::numeric_limits<double>::max();
        }
        return 0;
    }

    llvm::Value* LLVMExpressionEvaluator::implicitConvert(Expression* xp, llvm::Value* val, const Type& src, const Type& dst, llvm::Value* out) const
    {
        llvm::Type* int_ty = llvm::Type::getInt32Ty(irgen->context);
        if(dst.is_equal(src)) { return clone(xp, val, src, out); }
        if(!dst.is_assignable_from(src, irgen)) { 
            irgen->error(Error(xp, std::format("Expression of type {} cannot be converted to type {}", src.pretty_name(irgen->block_hash), dst.pretty_name(irgen->block_hash)))); 
            return nullptr; }
        auto dst_as_llvm = irgen->ToLLVMType(dst, false);
        if(!out) out = irgen->Alloca("implicit_convert", dst_as_llvm);
        if (src.is_error_ty() || dst.is_error_ty()) return out;
        if(src.name == "ilit")
        {
            auto as_int = llvm::dyn_cast<llvm::ConstantInt>(val);
            int64_t min_bound = getIntMinOf(dst);
            uint64_t max_bound = getIntMaxOf(dst);
            if(dst.is_integral())
            {
                if(as_int->isNegative())
                {
                    if(as_int->getSExtValue() >= min_bound && as_int->getSExtValue() <= max_bound)
                    {
                        val = irgen->builder->CreateSExtOrTrunc(val, dst_as_llvm);
                        irgen->builder->CreateStore(val, out);
                        return val;
                    }
                    irgen->error(Error(xp, "Literal out of bounds for destination type"));
                    return nullptr;
                }
                if(as_int->getZExtValue() <= max_bound)
                {
                    val = irgen->builder->CreateZExtOrTrunc(val, dst_as_llvm);
                    irgen->builder->CreateStore(val, out);
                    return val;
                }
                irgen->error(Error(xp, "Literal out of bound for destination type"));
                return nullptr;
            }
            if(dst.is_floating_point())
            {
                if(as_int->isNegative())
                {
                    if(as_int->getSExtValue() >= min_bound && as_int->getSExtValue() <= max_bound)
                    {
                        val = irgen->builder->CreateSIToFP(val, dst_as_llvm);
                        irgen->builder->CreateStore(val, out);
                        return val;
                    }
                    irgen->error(Error(xp, "Literal out of bound for destination type"));
                    return nullptr;
                }
                if(as_int->getZExtValue() <= max_bound)
                {
                    val = irgen->builder->CreateUIToFP(val, dst_as_llvm);
                    irgen->builder->CreateStore(val, out);
                    return val;
                }
                irgen->error(Error(xp, "Literal out of bound for destination type"));
                return nullptr;
            }
        }
        if(src.name == "flit")
        {
            auto as_float = llvm::dyn_cast<llvm::ConstantFP>(val);
            double min_bound = getFloatMinOf(dst);
            double max_bound = getFloatMaxOf(dst);
            double value = as_float->getValue().convertToDouble();
            if(dst.is_floating_point())
            {
                if(*dst.float_width() == 64)
                {
                    irgen->builder->CreateStore(val, out);
                    return val;
                }
                if(value >= min_bound && value <= max_bound)
                {
                    val = irgen->builder->CreateFPTrunc(val, dst_as_llvm);
                    irgen->builder->CreateStore(val, out);
                    return val;
                }
            }
        }
        if(dst.is_unsigned_integral())
        {
            if(!dst.is_equal(src))
                val = irgen->builder->CreateZExt(val, dst_as_llvm, "assign_zext");
            irgen->builder->CreateStore(val, out);
            return val;
        }
        if(dst.is_signed_integral())
        {
            if(!dst.is_equal(src))
                if(src.is_signed_integral()) val = irgen->builder->CreateSExt(val, dst_as_llvm, "assign_sext");
                else if(src.is_unsigned_integral()) val = irgen->builder->CreateZExt(val, dst_as_llvm, "assign_zext");
            irgen->builder->CreateStore(val, out);
            return val;
        }
        if(dst.is_floating_point())
        {
            if(!dst.is_equal(src))
                if(src.is_floating_point()) val = irgen->builder->CreateFPExt(val, dst_as_llvm, "assign_fpext");
                else if(src.is_unsigned_integral()) val = irgen->builder->CreateUIToFP(val, dst_as_llvm, "assign_uitofp");
                else if(src.is_signed_integral()) val = irgen->builder->CreateSIToFP(val, dst_as_llvm, "assign_uitofp");
            irgen->builder->CreateStore(val, out);
            return val;
        }
        //gc borrows are checked at runtime
        //so gc -> ref conversion increments a counter
        if (src.is_gc_reference())
        {
            std::string error_str = std::format("Borrow check failed at {}:{}:{}", irgen->view->filename, xp->beg.line, xp->beg.column);
            //the borrow count is -1 if theres a living mutable borrow
            auto i64_ty = llvm::Type::getInt64Ty(irgen->context);
            auto const_zero = llvm::ConstantInt::get(i64_ty, 0);
            auto const_min_1 = llvm::ConstantInt::get(i64_ty, -1, true);
            auto gcref_subtype = llvm::StructType::get(irgen->context, {irgen->ToLLVMType(src.deref(), false), i64_ty}, false);
            auto gc_borrow_count_ptr = irgen->builder->CreateStructGEP(gcref_subtype, val, 1);
            auto gc_object_ptr = irgen->builder->CreateStructGEP(gcref_subtype, val, 0);
            auto gc_borrow_count = irgen->builder->CreateLoad(i64_ty, gc_borrow_count_ptr);
            std::string type_name = "__gc_refcell_borrow";
            auto fn = irgen->builder->GetInsertBlock()->getParent();
            llvm::Value* new_count;
            //borrow count must be zero to borrow mutably
            auto invalid_borrow = llvm::BasicBlock::Create(irgen->context, "borrow_check_fail", fn, irgen->returnBlock);
            auto valid_borrow = llvm::BasicBlock::Create(irgen->context, "borrow_check_pass", fn, irgen->returnBlock);
            if (dst.is_mutable_reference()) {
                type_name += "_mut";
                irgen->builder->CreateCondBr(irgen->builder->CreateICmpNE(const_zero, gc_borrow_count), invalid_borrow, valid_borrow);
                new_count = const_min_1;
            }
            else {
                new_count = irgen->builder->CreateAdd(gc_borrow_count, llvm::ConstantInt::get(i64_ty, 1));
                irgen->builder->CreateCondBr(irgen->builder->CreateICmpEQ(const_min_1, gc_borrow_count), invalid_borrow, valid_borrow);
            }
            irgen->builder->SetInsertPoint(invalid_borrow);
            irgen->printString(error_str.c_str());
            irgen->builder->CreateBr(irgen->returnBlock);
            irgen->builder->SetInsertPoint(valid_borrow);
            irgen->builder->CreateStore(new_count, gc_borrow_count_ptr);
            irgen->builder->CreateStore(gc_object_ptr, out);
            auto lf = new ExtendedLifetimes;
            lf->objects.emplace_back(Type{type_name}, gc_borrow_count_ptr);
            std::string name = "__del_parents___aaaaaaaa";
            memcpy(name.data() + 16, &lf, sizeof(void*));
            gc_object_ptr->setName(name);
            return gc_object_ptr;
        }
        if(dst.is_reference())
        {
            //other is a mutable reference
            irgen->builder->CreateStore(val, out);
            return val;
        }
        if(dst.is_optional())
        {
            //opt is {data, bool}
            //TODO destroy data if exists
            if(src.name == "__null")
            {
                irgen->builder->CreateStore(llvm::ConstantInt::getFalse(irgen->context),
                    irgen->builder->CreateStructGEP(dst_as_llvm, out, 1));
                return out;
            }

            //subtype implicit conversion
            implicitConvert(xp, val, src, dst.subtypes[0], irgen->builder->CreateStructGEP(dst_as_llvm, out, 0));
            irgen->builder->CreateStore(llvm::ConstantInt::getTrue(irgen->context), irgen->builder->CreateStructGEP(dst_as_llvm, out, 1));
        }
        if(dst.is_variant())
        {
            const std::set subtypes(dst.subtypes.begin(), dst.subtypes.end());
            const auto type_idx_ptr = irgen->builder->CreateStructGEP(dst_as_llvm, out, 1);
            uint32_t i = 0;

            for(auto& sub : subtypes)
            {
                if(!sub.is_assignable_from(src, irgen)) { i++; continue; }
                implicitConvert(xp, val, src, sub, irgen->builder->CreateStructGEP(dst_as_llvm, out, 0));
                irgen->builder->CreateStore(llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), i),
                    type_idx_ptr);
                break;
            }
        }
        if (dst.is_slice())
        {
            if (src.is_reference())
            {
                auto ptr = irgen->builder->CreateStructGEP(dst_as_llvm, out, 0);
                auto size = irgen->builder->CreateStructGEP(dst_as_llvm, out, 1);
                if (src.deref().is_static_array())
                {
                    irgen->builder->CreateStore(val, ptr);
                    auto sz = llvm::ConstantInt::get(llvm::Type::getInt64Ty(irgen->context), src.deref().static_array_size());
                    irgen->builder->CreateStore(sz, size);
                }
                
                if (!src.is_lvalue)
                {
                    auto arr = std::array{ val };
                    stealUsages(arr, out);
                }
            }
        }
        if (dst.name == "__called_fn")
        {
            if (src.is_function())
            {
                irgen->builder->CreateStore(llvm::ConstantPointerNull::get(llvm::PointerType::get(irgen->context, 0)),
                    irgen->builder->CreateStructGEP(dst_as_llvm, out, 0));
                irgen->builder->CreateStore(val,
                    irgen->builder->CreateStructGEP(dst_as_llvm, out, 1));
                return out;
            }
            if (src.is_lambda())
            {
                irgen->builder->CreateStore(val,
                    irgen->builder->CreateStructGEP(dst_as_llvm, out, 0));
                irgen->builder->CreateStore(irgen->code->getFunction(src.module->module_hash + src.name),
                    irgen->builder->CreateStructGEP(dst_as_llvm, out, 1));
                return out;
            }
        }
        if (dst.is_view())
        {
            if (src.is_view())
            {
                auto& layout = irgen->code->getDataLayout();
                size_t size = layout.getTypeAllocSize(dst_as_llvm);
                irgen->builder->CreateMemCpy(out, std::nullopt, val, std::nullopt, size);
                return out;
            }
            auto& viewed = dst.subtypes[0];
            auto [name, intf] = viewed.module->findInterface(viewed.block_hash, viewed.name);
            std::string full_name = name + intf->name;
            if (auto cls = src.deref().get_decl_if_class(irgen))
            {
                auto details = src.deref().module->findType(src.deref().block_hash, src.deref().name);
                auto this_ptr = irgen->builder->CreateStructGEP(dst_as_llvm, out, 0);
                irgen->builder->CreateStore(val, this_ptr);
                size_t idx = 0;
                for (auto& method : intf->methods)
                {
                    idx++;
                    auto fn_ptr = irgen->builder->CreateStructGEP(dst_as_llvm, out, idx);
                    std::string method_name = std::get<0>(*details) + full_name + "::" + method->name;
                    auto fn = irgen->code->getFunction(method_name);
                    irgen->builder->CreateStore(fn, fn_ptr);
                }
                return out;
            }
        }
        return out;
    }

    /// user defined types can create a @c clone function.\n
    /// The @c clone function's signature looks like @code clone: (&this) -> This @endcode \n
    /// Without it the type is cannot be copied
    /// I think it should be implicit for union types
    llvm::Value* LLVMExpressionEvaluator::clone(Expression* xp, llvm::Value* value, const Type& left_type, llvm::Value* into) const
    {
        if(!left_type.should_sret())
        {
            if(into) irgen->builder->CreateStore(value, into);
            return value;
        }
        auto as_llvm = irgen->ToLLVMType(left_type, false);
        if(!left_type.is_lvalue)
        {
            //move
            if(!into) return value;
            auto& layout = irgen->code->getDataLayout();
            size_t size = layout.getTypeAllocSize(as_llvm);
            irgen->builder->CreateMemCpy(into, std::nullopt, value, std::nullopt, size);
            return into;
        }
        if(!into) into = irgen->Alloca("clone", as_llvm);
        if(left_type.is_tuple())
        {
            size_t idx = 0;
            for(auto& sub : left_type.subtypes)
            {
                auto sub_as_llvm = irgen->ToLLVMType(sub, false);
                auto ptr = irgen->builder->CreateStructGEP(as_llvm, into, idx);
                auto val = irgen->builder->CreateStructGEP(as_llvm, value, idx);
                if(!sub.should_sret()) val = irgen->builder->CreateLoad(sub_as_llvm, val);
                clone(xp, val, sub, ptr);
                idx++;
            }
        }
        else if(left_type.is_optional())
        {
            auto opt_ty = irgen->ToLLVMType(left_type, false);
            //opt is {data, bool}
            auto has_value = irgen->builder->CreateLoad(llvm::Type::getInt1Ty(irgen->context),
                irgen->builder->CreateStructGEP(opt_ty, value, 1));
            auto fn = irgen->builder->GetInsertBlock()->getParent();
            auto is_valid = llvm::BasicBlock::Create(irgen->context, "opt_valid_assign", fn, irgen->returnBlock);
            auto opt_assign_cont = llvm::BasicBlock::Create(irgen->context, "opt_assign_cont", fn, irgen->returnBlock);
            irgen->builder->CreateCondBr(has_value, is_valid, opt_assign_cont);

            irgen->builder->SetInsertPoint(is_valid);
            auto sub_value = irgen->builder->CreateStructGEP(opt_ty, value, 0);
            if(!left_type.subtypes[0].should_sret())
            {
                auto subtype = llvm::dyn_cast<llvm::StructType>(opt_ty)->getElementType(0);
                sub_value = irgen->builder->CreateLoad(subtype, sub_value);
            }
            clone(xp, sub_value, left_type.subtypes[0], irgen->builder->CreateStructGEP(opt_ty, into, 0));
            irgen->builder->CreateBr(opt_assign_cont);

            irgen->builder->SetInsertPoint(opt_assign_cont);
            irgen->builder->CreateStore(has_value, irgen->builder->CreateStructGEP(opt_ty, into , 1));
        }
        else if (left_type.is_str())
        {
            auto size = irgen->builder->CreateStructGEP(as_llvm, value, 1);
            size = irgen->builder->CreateLoad(llvm::Type::getInt64Ty(irgen->context), size);
            auto mem = irgen->builder->CreateStructGEP(as_llvm, value, 0);
            mem = irgen->builder->CreateLoad(llvm::PointerType::get(irgen->context, 0), mem);
            auto dst_ptr_ptr = irgen->builder->CreateStructGEP(as_llvm, into, 0);
            auto dst_size_ptr = irgen->builder->CreateStructGEP(as_llvm, into, 1);
            auto dst_cap_ptr = irgen->builder->CreateStructGEP(as_llvm, into, 2);
            auto ptr = irgen->Malloc("", size);
            irgen->builder->CreateStore(size, dst_size_ptr);
            irgen->builder->CreateStore(size, dst_cap_ptr);
            irgen->builder->CreateStore(ptr, dst_ptr_ptr);
            irgen->builder->CreateMemCpy(ptr, std::nullopt, mem, std::nullopt, size);
        }
        else if(left_type.is_variant())
        {
            std::set subtypes(left_type.subtypes.begin(), left_type.subtypes.end());
            auto fn = irgen->builder->GetInsertBlock()->getParent();
            auto llvm_ty = irgen->ToLLVMType(left_type, false);
            auto type_idx_ptr = irgen->builder->CreateStructGEP(llvm_ty, value, 1);

            auto type_idx = irgen->builder->CreateLoad(
                    llvm::Type::getInt32Ty(irgen->context),
                    irgen->builder->CreateStructGEP(llvm_ty, value, 1));
            llvm::BasicBlock* def = llvm::BasicBlock::Create(irgen->context, "variant_default", fn, irgen->returnBlock);
            auto sw = irgen->builder->CreateSwitch(type_idx, def, left_type.subtypes.size());
            uint32_t idx = 0;
            for(auto& sub : subtypes)
            {
                llvm::BasicBlock* blk = llvm::BasicBlock::Create(irgen->context, sub.name, fn, def);
                sw->addCase(llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), idx), blk);

                auto sub_as_llvm = irgen->ToLLVMType(sub, false);
                irgen->builder->SetInsertPoint(blk);
                auto sub_value = irgen->builder->CreateStructGEP(llvm_ty, value, 0);
                if(!sub.should_sret()) sub_value = irgen->builder->CreateLoad(sub_as_llvm, sub_value);

                clone(xp, sub_value, sub, irgen->builder->CreateStructGEP(llvm_ty, into, 1));

                irgen->builder->CreateBr(def);
                idx++;
            }
            irgen->builder->SetInsertPoint(def);
            irgen->builder->CreateStore(type_idx, type_idx_ptr);
        }
        else if(left_type.is_reference())
        {
            irgen->builder->CreateStore(value, into);
        }
        //class types can define custom clone
        else if(auto decl_tup = left_type.module->findType(left_type.block_hash, left_type.name))
        {
            auto decl = std::get<2>(*decl_tup).get();
            if(!decl->has_clone) { irgen->error(Error(xp, "Expression cannot be cloned")); return nullptr; }
            auto candidate = std::ranges::find_if(decl->stats, [](auto& meth)
            {
                auto end = meth->attributes.end();
                return std::ranges::find_if(meth->attributes, [](auto& attr) {
                    return static_cast<Attribute&>(attr).name == "clone";
                }) != end;
            });
            if(candidate != decl->stats.end() && dynamic_cast<FunctionDeclaration*>(candidate->get()))
            {
                auto decl = reinterpret_cast<FunctionDeclaration*>(candidate->get());
                auto& sig = decl->signature;
                std::string fn_name = std::get<0>(*decl_tup) + decl->name;
                auto fn = irgen->code->getFunction(fn_name);
                if(!fn) fn = declareFunction(fn_name, irgen, sig);
                irgen->builder->CreateCall(fn, {into, value});
            }
            else
            {
                //if no custom clone is provided, use memberwise cloning
                size_t idx = 0;
                for(auto& var : decl->vars)
                {
                    auto sub_val_ptr = irgen->builder->CreateStructGEP(as_llvm, value, idx);
                    auto sub_into_ptr = irgen->builder->CreateStructGEP(as_llvm, into, idx);
                    if(!var.type.should_sret())
                        sub_val_ptr = irgen->builder->CreateLoad(irgen->ToLLVMType(var.type, false), sub_val_ptr);
                    clone(xp, sub_val_ptr, var.type, sub_into_ptr);
                    idx++;
                }
            }

        }
        return into;
    }
    void destroyNonOwningParents(const LLVMExpressionEvaluator& eval, llvm::Value* value, const Type& tp)
    {
        if(value->getName().starts_with("__del_parents___"))
        {
            auto nm = value->getName();
            auto& lifetimes = **reinterpret_cast<ExtendedLifetimes* const*>(nm.data() + 16);
            for(auto& [type, obj] : lifetimes.objects)
                eval.destroy(obj, type);
            delete &lifetimes;
        }
    }
    extern "C" void string_destroy_debug()
    {
        printf("string destroyed\n");
    }
    llvm::Function* getStringDestroy(LLVMIRGenerator* irgen)
    {
        auto fn = irgen->code->getFunction("string_destroy_debug");
        if(fn) return fn;
        auto ty = llvm::FunctionType::get(llvm::Type::getVoidTy(irgen->context), false);
        return llvm::Function::Create(ty, llvm::GlobalValue::ExternalLinkage, "string_destroy_debug", irgen->code);
    }
    void LLVMExpressionEvaluator::destroy(llvm::Value* value, const Type& type) const
    {
        if(type.is_trivially_destructible(irgen)) return;

        auto as_llvm = irgen->ToLLVMType(type, false);
        auto parent = irgen->builder->GetInsertBlock()->getParent();
        if(type.is_optional())
        {
            auto is_valid = irgen->builder->CreateStructGEP(as_llvm, value, 1);
            auto should_destroy = llvm::BasicBlock::Create(irgen->context, "destroy_opt", parent, irgen->returnBlock);
            auto destroy_cont = llvm::BasicBlock::Create(irgen->context, "destroy_cont", parent, should_destroy);
            irgen->builder->CreateCondBr(
                irgen->builder->CreateLoad(llvm::Type::getInt1Ty(irgen->context), is_valid), should_destroy, destroy_cont);
            irgen->builder->SetInsertPoint(should_destroy);
            destroy(value, type.subtypes[0]);
            irgen->builder->CreateBr(destroy_cont);
            irgen->builder->SetInsertPoint(destroy_cont);
        }
        else if (type.name == "__gc_refcell_borrow") {
            auto i64_ty = llvm::Type::getInt64Ty(irgen->context);
            auto new_val = irgen->builder->CreateLoad(i64_ty, value);
            irgen->builder->CreateStore(irgen->builder->CreateSub(new_val, llvm::ConstantInt::get(i64_ty, 1)), value);
        }
        else if (type.name == "__gc_refcell_borrow_mut") {
            irgen->builder->CreateStore(llvm::ConstantInt::get(llvm::Type::getInt64Ty(irgen->context), 0), value);
        }
        else if(type.is_tuple())
        {
            for(auto idx : std::views::iota(0u, type.subtypes.size()))
            {
                auto sub = irgen->builder->CreateStructGEP(as_llvm, value, idx);
                destroy(sub, type.subtypes[idx]);
            }
        }
        else if(type.is_variant())
        {
            std::set subtypes(type.subtypes.begin(), type.subtypes.end());
            auto fn = irgen->builder->GetInsertBlock()->getParent();
            auto type_idx_ptr = irgen->builder->CreateStructGEP(as_llvm, value, 1);

            auto type_idx = irgen->builder->CreateLoad(
                    llvm::Type::getInt32Ty(irgen->context),
                    irgen->builder->CreateStructGEP(as_llvm, value, 1));
            llvm::BasicBlock* def = llvm::BasicBlock::Create(irgen->context, "variant_default", fn, irgen->returnBlock);
            auto sw = irgen->builder->CreateSwitch(type_idx, def, type.subtypes.size());
            uint32_t idx = 0;
            for(auto& sub : subtypes)
            {
                llvm::BasicBlock* blk = llvm::BasicBlock::Create(irgen->context, sub.name, fn, def);
                sw->addCase(llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), idx), blk);

                irgen->builder->SetInsertPoint(blk);
                auto sub_value = irgen->builder->CreateStructGEP(as_llvm, value, 0);
                destroy(sub_value, type.subtypes[idx]);
                irgen->builder->CreateBr(def);
                idx++;
            }
            irgen->builder->SetInsertPoint(def);
        }
        else if(type.is_str())
        {
            auto mem_ptr = irgen->builder->CreateStructGEP(as_llvm, value, 0);
            irgen->Free(irgen->builder->CreateLoad(llvm::PointerType::get(irgen->context,0), mem_ptr));
            //irgen->builder->CreateCall(getStringDestroy(irgen));
        }
        else if(auto dets = type.module->findType(type.block_hash, type.name))
        {
            auto decl = std::get<2>(*dets).get();
            if(!decl->destructor_name.empty())
            {
                auto fn = irgen->code->getFunction(std::get<0>(*dets) + decl->destructor_name);
                irgen->builder->CreateCall(fn, {value});
            }
            //even after calling the destructor, we still do member wise destruction
            size_t idx = 0;
            for(auto& var : decl->vars)
            {
                auto sub = irgen->builder->CreateStructGEP(as_llvm, value, idx);
                destroy(sub, var.type);
                idx++;
            }
        }

        if(type.is_non_owning(irgen))
            destroyNonOwningParents(*this, value, type);
    }

    llvm::Value* LLVMExpressionEvaluator::doDot(Expression* lhs, Expression* rhs, const Type& left_type, bool load_primitive)
    {
        auto left_val = std::visit(*this, lhs->toVariant());;
        if(left_type.deref().is_tuple())
        {
            if(auto idx = dynamic_cast<IntegerLiteral*>(rhs))
            {
                auto idx_int = std::stoul(std::string{idx->text});
                auto out_type = left_type.deref().subtypes[idx_int];

                auto llvm_t = irgen->ToLLVMType(left_type.deref(), false);
                auto left_ptr = left_val;
                auto ptr = irgen->builder->CreateStructGEP(llvm_t, left_ptr, idx_int);
                if(load_primitive && !out_type.should_sret())
                    return irgen->builder->CreateLoad(irgen->ToLLVMType(out_type, false), ptr);
                //no partial move allowed, so rvalue binary operations clone the field and destroy the object
                if (!left_type.is_lvalue)
                {
                    if(!left_type.is_trivially_destructible(irgen))
                    {
                        auto ret = clone(lhs, ptr, left_type.subtypes[idx_int]);
                        destroy(left_ptr, left_type);
                        return ret;
                    }
                }
                return ptr;
            }
        }
        if(auto cls = left_type.deref().get_decl_if_class(irgen))
        {
            if(auto* name_expr = dynamic_cast<NameExpression*>(rhs))
            {
                std::string name(name_expr->text);
                if(auto var = std::ranges::find_if(cls->vars, [&name](ClassVariable& v)
                {
                    return name == v.name;
                }); var != cls->vars.end())
                {
                    auto llvm_t = irgen->ToLLVMType(left_type.deref(), false);
                    auto out_t = irgen->ToLLVMType(var->type, false);
                    llvm::Value* ptr;

                    auto lalloc = left_val;
                    ptr = irgen->builder->CreateStructGEP(llvm_t, lalloc, std::distance(cls->vars.begin(), var));


                    if(load_primitive && !var->type.should_sret()) return irgen->builder->CreateLoad(out_t, ptr, var->name);
                    if (!left_type.is_lvalue)
                    {
                        if(!left_type.is_trivially_destructible(irgen))
                        {
                            auto ret = clone(lhs, ptr, var->type);
                            destroy(lalloc, left_type);
                            return ret;
                        }
                    }
                    return ptr;
                }
            }
        }
        return nullptr;
    }


    /*
     * Implicit conversion is not done in expression except literals
     * ilit op ilit = ilit
     * ilit op flit = flit
     * ilit op int = int
     * ilit op float = float
     *
     * flit op float = float
     * flit op flit = flit
     */
    void convertLiterals(llvm::Value** lhs,
        llvm::Value** rhs,
        const Type& left_type,
        const Type& right_type, LLVMIRGenerator* irgen, const LLVMExpressionEvaluator* eval)
    {
        if(left_type.name == "ilit")
        {
            auto as_int = llvm::dyn_cast<llvm::ConstantInt>(*lhs);
            if(right_type.name == "ilit") return;
            if(right_type.name == "filt")
            {
                if(as_int->isNegative()) *lhs = irgen->builder->CreateSIToFP(*lhs, llvm::Type::getDoubleTy(irgen->context));
                else *lhs = irgen->builder->CreateUIToFP(*lhs, llvm::Type::getDoubleTy(irgen->context));
                return;
            }
            *lhs = eval->implicitConvert(nullptr, *lhs, left_type, right_type);
            return;
        }
        if(left_type.name == "flit")
        {
            if(right_type.name == "flit") return;
            *lhs = eval->implicitConvert(nullptr, *lhs, left_type, right_type);
            return;
        }
        if(right_type.name == "ilit" || right_type.name == "flit")
            convertLiterals(rhs, lhs, right_type, left_type, irgen, eval);
    }
    llvm::FunctionType* BinOverloadToLLVMSig(LLVMIRGenerator* irgen, OverloadDetailsBinary* bin)
    {
        bool should_sret = bin->result.should_sret();
        auto ptr_ty = llvm::PointerType::get(irgen->context, 0);
        llvm::Type* res = should_sret ? llvm::Type::getVoidTy(irgen->context)
            : irgen->ToLLVMType(bin->result, false);
        std::vector<llvm::Type*> args;
        args.reserve(2 + should_sret);
        if(should_sret) args.push_back(ptr_ty);
        args.push_back(bin->left.should_sret() ? ptr_ty: irgen->ToLLVMType(bin->left, false));
        args.push_back(bin->right.should_sret() ? ptr_ty: irgen->ToLLVMType(bin->right, false));
        return llvm::FunctionType::get(res, args, false);
    }
    llvm::Function* getOperatorFunction(TokenType t, LLVMIRGenerator* irgen, OverloadDetailsBinary* target)
    {
        auto fn_name = target->mangled_name(t);
        auto fn = irgen->code->getFunction(fn_name);
        if(!fn)
        {
            fn = llvm::Function::Create(BinOverloadToLLVMSig(irgen, target), llvm::GlobalValue::ExternalLinkage,
                fn_name, irgen->code);
            if(target->result.should_sret()) fn->addAttributeAtIndex(1,
                llvm::Attribute::get(irgen->context,llvm::Attribute::StructRet, irgen->ToLLVMType(target->result, false)));
        }
        return fn;
    }
    llvm::Value* LLVMExpressionEvaluator::doAddition(
        Expression* lhs,
        Expression* rhs,
        const Type& left_type,
        const Type& right_type)
    {
        auto lhs_e = std::visit(*this, lhs->toVariant());
        auto rhs_e = std::visit(*this, rhs->toVariant());
        if (left_type.name == "ilit" && right_type.name == "ilit") return irgen->builder->CreateAdd(lhs_e, rhs_e);
        auto target = resolveAdd(left_type, right_type, irgen);
        auto fn = getOperatorFunction(TokenType::Plus, irgen, target);
        std::vector<llvm::Value*> args;
        if(target->result.should_sret())
            args.push_back(irgen->Alloca("ret_temp", irgen->ToLLVMType(target->result, false)));
        args.push_back(implicitConvert(lhs, lhs_e, left_type, target->left));
        args.push_back(implicitConvert(rhs, rhs_e, right_type, target->right));
        auto ret = irgen->builder->CreateCall(fn, args);
        auto final  =target->result.should_sret() ? args[0] : ret;
        if(target->result.is_non_owning(irgen))
            stealUsages(args, final);
        else
            clearUsages(args, *this);
        return final;
    }
    llvm::Value* LLVMExpressionEvaluator::doMinus(
        Expression* lhs,
        Expression* rhs,
        const Type& left_type,
        const Type& right_type)
    {
        auto lhs_e = std::visit(*this, lhs->toVariant());
        auto rhs_e = std::visit(*this, rhs->toVariant());
        if (left_type.name == "ilit" && right_type.name == "ilit") return irgen->builder->CreateSub(lhs_e, rhs_e);
        auto target = resolveSub(left_type, right_type, irgen);
        auto fn = getOperatorFunction(TokenType::Minus, irgen, target);
        std::vector<llvm::Value*> args;
        if(target->result.should_sret())
            args.push_back(irgen->Alloca("ret_temp", irgen->ToLLVMType(target->result, false)));
        args.push_back(implicitConvert(lhs, lhs_e, left_type, target->left));
        args.push_back(implicitConvert(rhs, rhs_e, right_type, target->right));
        auto ret = irgen->builder->CreateCall(fn, args);
        auto final  =target->result.should_sret() ? args[0] : ret;
        if(target->result.is_non_owning(irgen))
            stealUsages(args, final);
        else
            clearUsages(args, *this);
        return final;
    }
    llvm::Value* LLVMExpressionEvaluator::doMult(
        Expression* lhs,
        Expression* rhs,
        const Type& left_type,
        const Type& right_type)
    {
        auto lhs_e = std::visit(*this, lhs->toVariant());
        auto rhs_e = std::visit(*this, rhs->toVariant());
        if (left_type.name == "ilit" && right_type.name == "ilit") return irgen->builder->CreateMul(lhs_e, rhs_e);
        auto target = resolveMul(left_type, right_type, irgen);
        auto fn = getOperatorFunction(TokenType::Star, irgen, target);
        std::vector<llvm::Value*> args;
        if(target->result.should_sret())
            args.push_back(irgen->Alloca("ret_temp", irgen->ToLLVMType(target->result, false)));
        args.push_back(implicitConvert(lhs, lhs_e, left_type, target->left));
        args.push_back(implicitConvert(rhs, rhs_e, right_type, target->right));
        auto ret = irgen->builder->CreateCall(fn, args);
        auto final  =target->result.should_sret() ? args[0] : ret;
        if(target->result.is_non_owning(irgen))
            stealUsages(args, final);
        else
            clearUsages(args, *this);
        return final;
    }
    llvm::Value* LLVMExpressionEvaluator::doRange(
        Expression* lhs,
        Expression* rhs,
        const Type& left_type,
        const Type& right_type,
        const Type& result)
    {
        using namespace std::string_view_literals;
        auto subtype = Type{ .name = std::string(result.name.begin() + "range_"sv.size(), result.name.end()) };
        auto type = irgen->ToLLVMType(result, false);
        auto ptr = irgen->Alloca("range_literal", type);
        implicitConvert(lhs, std::visit(*this, lhs->toVariant()), left_type, subtype,
            irgen->builder->CreateStructGEP(type, ptr, 0));
        implicitConvert(lhs, std::visit(*this, rhs->toVariant()), right_type, subtype,
            irgen->builder->CreateStructGEP(type, ptr, 1));
        return ptr;
    }
    llvm::Value* LLVMExpressionEvaluator::doDiv(
        Expression* lhs,
        Expression* rhs,
        const Type& left_type,
        const Type& right_type)
    {
        auto lhs_e = std::visit(*this, lhs->toVariant());
        auto rhs_e = std::visit(*this, rhs->toVariant());
        if (left_type.name == "ilit" && right_type.name == "ilit") return irgen->builder->CreateSDiv(lhs_e, rhs_e);
        auto target = resolveDiv(left_type, right_type, irgen);
        auto fn = getOperatorFunction(TokenType::Slash, irgen, target);
        std::vector<llvm::Value*> args;
        if(target->result.should_sret())
            args.push_back(irgen->Alloca("ret_temp", irgen->ToLLVMType(target->result, false)));
        args.push_back(implicitConvert(lhs, lhs_e, left_type, target->left));
        args.push_back(implicitConvert(rhs, rhs_e, right_type, target->right));
        auto ret = irgen->builder->CreateCall(fn, args);
        auto final  =target->result.should_sret() ? args[0] : ret;
        if(target->result.is_non_owning(irgen))
            stealUsages(args, final);
        else
            clearUsages(args, *this);
        return final;
    }
    llvm::Value* LLVMExpressionEvaluator::doRem(
        Expression* lhs,
        Expression* rhs,
        const Type& left_type,
        const Type& right_type)
    {
        auto lhs_e = std::visit(*this, lhs->toVariant());
        auto rhs_e = std::visit(*this, rhs->toVariant());
        if (left_type.name == "ilit" && right_type.name == "ilit") return irgen->builder->CreateSRem(lhs_e, rhs_e);
        auto target = resolveRem(left_type, right_type, irgen);
        auto fn = getOperatorFunction(TokenType::Percent, irgen, target);
        std::vector<llvm::Value*> args;
        if(target->result.should_sret())
            args.push_back(irgen->Alloca("ret_temp", irgen->ToLLVMType(target->result, false)));
        args.push_back(implicitConvert(lhs, lhs_e, left_type, target->left));
        args.push_back(implicitConvert(rhs, rhs_e, right_type, target->right));
        auto ret = irgen->builder->CreateCall(fn, args);
        auto final  =target->result.should_sret() ? args[0] : ret;
        if(target->result.is_non_owning(irgen))
            stealUsages(args, final);
        else
            clearUsages(args, *this);
        return final;
    }
    llvm::Value* LLVMExpressionEvaluator::doShl(Expression* lhs, Expression* rhs, const Type& left_type, const Type& right_type)
    {
        auto lhs_e = std::visit(*this, lhs->toVariant());
        auto rhs_e = std::visit(*this, rhs->toVariant());
        if (left_type.name == "ilit" && right_type.name == "ilit") return irgen->builder->CreateShl(lhs_e, rhs_e);
        auto target = resolveShl(left_type, right_type, irgen);
        auto fn = getOperatorFunction(TokenType::DoubleLess, irgen, target);
        std::vector<llvm::Value*> args;
        if (target->result.should_sret())
            args.push_back(irgen->Alloca("ret_temp", irgen->ToLLVMType(target->result, false)));
        args.push_back(implicitConvert(lhs, lhs_e, left_type, target->left));
        args.push_back(implicitConvert(rhs, rhs_e, right_type, target->right));
        auto ret = irgen->builder->CreateCall(fn, args);
        auto final = target->result.should_sret() ? args[0] : ret;
        if (target->result.is_non_owning(irgen))
            stealUsages(args, final);
        else
            clearUsages(args, *this);
        return final;
    }
    llvm::Value* LLVMExpressionEvaluator::doShr(Expression* lhs, Expression* rhs, const Type& left_type, const Type& right_type)
    {
        auto lhs_e = std::visit(*this, lhs->toVariant());
        auto rhs_e = std::visit(*this, rhs->toVariant());
        if (left_type.name == "ilit" && right_type.name == "ilit") return irgen->builder->CreateLShr(lhs_e, rhs_e);
        auto target = resolveShr(left_type, right_type, irgen);
        auto fn = getOperatorFunction(TokenType::DoubleGreater, irgen, target);
        std::vector<llvm::Value*> args;
        if (target->result.should_sret())
            args.push_back(irgen->Alloca("ret_temp", irgen->ToLLVMType(target->result, false)));
        args.push_back(implicitConvert(lhs, lhs_e, left_type, target->left));
        args.push_back(implicitConvert(rhs, rhs_e, right_type, target->right));
        auto ret = irgen->builder->CreateCall(fn, args);
        auto final = target->result.should_sret() ? args[0] : ret;
        if (target->result.is_non_owning(irgen))
            stealUsages(args, final);
        else
            clearUsages(args, *this);
        return final;
    }
    llvm::Value* LLVMExpressionEvaluator::doCmp(
        ComparisonPredicate p,
        Expression* lhs,
        Expression* rhs,
        const Type& left_type,
        const Type& right_type,
        const Type& result_type)
    {
        constexpr int32_t eq = 1;
        constexpr int32_t ne = 0;
        constexpr int32_t less = 2;
        constexpr int32_t greater = 3;
        constexpr int32_t unord = 4;
        auto i32 = llvm::Type::getInt32Ty(irgen->context);
        auto lhs_e = std::visit(*this, lhs->toVariant());
        auto rhs_e = std::visit(*this, rhs->toVariant());
        if ((p == EQ || p == NE) && left_type.is_equal(right_type)) {
            if (left_type.get_decl_if_enum()) {
                return
                    (p == EQ) ? irgen->builder->CreateICmpEQ(lhs_e, rhs_e, "enum_cmp")
                    : irgen->builder->CreateICmpNE(lhs_e, rhs_e, "enum_cmp");
            }
        }
        auto target = resolveCmp(left_type, right_type, irgen);
        auto fn = getOperatorFunction(TokenType::Spaceship, irgen, target);
        std::array<llvm::Value*, 2> args;
        args[0] = implicitConvert(lhs, lhs_e, left_type, target->left);
        args[1] = implicitConvert(rhs, rhs_e, right_type, target->right);
        auto ret = irgen->builder->CreateCall(fn, args);
        if (p == SPACE) return ret;
        if (p == EQ) return irgen->builder->CreateICmpEQ(ret, llvm::ConstantInt::get(i32, eq));
        if (p == GT) return irgen->builder->CreateICmpEQ(ret, llvm::ConstantInt::get(i32, greater));
        if (p == LT) return irgen->builder->CreateICmpEQ(ret, llvm::ConstantInt::get(i32, less));
        if (p == NE)
        {
            return irgen->builder->CreateICmpNE(ret, llvm::ConstantInt::get(i32, eq));
        }
        if (p == EQ_GT)
        {
            return irgen->builder->CreateOr(
                irgen->builder->CreateICmpEQ(ret, llvm::ConstantInt::get(i32, greater)),
                irgen->builder->CreateICmpEQ(ret, llvm::ConstantInt::get(i32, eq)));
        }
        if (p == EQ_LT)
        {
            return irgen->builder->CreateOr(
                irgen->builder->CreateICmpEQ(ret, llvm::ConstantInt::get(i32, less)),
                irgen->builder->CreateICmpEQ(ret, llvm::ConstantInt::get(i32, eq)));
        }
    }
    llvm::Value* LLVMExpressionEvaluator::doUnionVar(CallOperation* op, Type& t)
    {
        using namespace std::string_view_literals;
        size_t dollar_off = t.name.find_first_of('$');
        std::string variant = std::string(t.name.begin() + dollar_off + 1, t.name.end());
        std::string type_name = std::string(t.name.begin() + "__union_var"sv.size(), t.name.begin() + dollar_off);
        t.name = type_name;
        auto as_llvm = irgen->ToLLVMType(t, false);
        auto ret = irgen->Alloca("union_obj", as_llvm);
        auto decl = t.get_decl_if_union();
        auto arg = std::visit(LLVMExpressionEvaluator{ irgen, decl->fields.at(variant) }, op->arguments[0]->toVariant());
        auto arg_ty = std::visit(ExpressionTypeChecker{ irgen, decl->fields.at(variant) }, op->arguments[0]->toVariant()).value_or_error();
        implicitConvert(op->arguments[0].get(), arg, arg_ty, decl->fields.at(variant), irgen->builder->CreateStructGEP(as_llvm, ret, 0));
        irgen->builder->CreateStore(llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context),
            std::distance(decl->fields.begin(), decl->fields.find(variant))), irgen->builder->CreateStructGEP(as_llvm, ret, 1));
        return ret;
    }

    llvm::Value* LLVMExpressionEvaluator::LValueEvaluator::operator()(NameExpression*nm)
    {
        std::string name(nm->text);
        for(size_t i = irgen->variables.size(); i > 0; --i)
        {
            size_t idx = i - 1;
            if(auto var = irgen->variables[idx].find(name); var != irgen->variables[idx].end())
            {
                //if(!std::get<1>(var->second).is_mutable) return nullptr;
                return std::get<0>(var->second);
            }
        }
    }

    llvm::Value* LLVMExpressionEvaluator::LValueEvaluator::operator()(BinaryOperation* bop)
    {
        //Dot operations can also be lvalue'd
        if(bop->op.type != TokenType::Dot) return nullptr;
        auto left_t = std::visit(ExpressionTypeChecker{irgen}, bop->lhs->toVariant());
        if(!left_t) return nullptr;
        if(!left_t->is_mutable && !left_t->is_reference()) return nullptr;
        return LLVMExpressionEvaluator{irgen}.doDot(bop->lhs.get(), bop->rhs.get(), *left_t, false);
    }

    llvm::Value* LLVMExpressionEvaluator::LValueEvaluator::operator()(PrefixOperation* op)
    {
        if(op->op.type == TokenType::Star)
        {
            return std::visit(LLVMExpressionEvaluator{irgen}, op->operand->toVariant());
        }
    }

    llvm::Value* LLVMExpressionEvaluator::LValueEvaluator::operator()(Expression* expr)
    {
        //TODO
        return nullptr;
    }

    llvm::Value* LLVMExpressionEvaluator::LValueEvaluator::operator()(GroupingExpression* gre)
    {
        return std::visit(*this, gre->expr->toVariant());
    }


    llvm::Value* LLVMExpressionEvaluator::operator()(IntegerLiteral* lit) {
        const auto ul = std::stoull(std::string{lit->text});
        llvm::Type* llvm_ty = llvm::Type::getInt64Ty(irgen->context);
        if (target && target->is_integral()) llvm_ty = irgen->ToLLVMType(*target, false);
        return llvm::ConstantInt::get(llvm_ty, ul);
    }
    llvm::Value* LLVMExpressionEvaluator::operator()(BooleanLiteral* lit)
    {
        if(lit->token.text == "true") return llvm::ConstantInt::getTrue(irgen->context);
        return llvm::ConstantInt::getFalse(irgen->context);
    }
    llvm::Value* LLVMExpressionEvaluator::operator()(TupleLiteral* tup)
    {
        auto tuple_t = ExpressionTypeChecker{irgen, target}(tup);
        if(!tuple_t) { irgen->error(tuple_t.error()); return nullptr; }
        auto llvm_t = irgen->ToLLVMType(*tuple_t, false);
        auto tuple_tmp = irgen->Alloca("tuple_lit", llvm_t);
        auto zero_const = llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 0);
        size_t idx = 0;
        for(auto& expr: tup->elements)
        {
            auto idx_const = llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), idx);
            auto idx_ptr = irgen->builder->CreateGEP(llvm_t, tuple_tmp, {zero_const, idx_const}, "tuple_elem");
            auto type = std::visit(ExpressionTypeChecker{irgen}, expr->toVariant());
            implicitConvert(expr.get(), std::visit(*this, expr->toVariant()), *type, tuple_t->subtypes[idx], idx_ptr);
            idx++;
        }
        return tuple_tmp;
    }
    llvm::Value* LLVMExpressionEvaluator::operator()(ArrayLiteral* lit)
    {
        using repeat_notation = std::pair<std::unique_ptr<Expression>, std::unique_ptr<Expression>>;
        using list_notation = std::vector<std::unique_ptr<Expression>>;
        auto tp_check = ExpressionTypeChecker{irgen};
        auto type = ExpressionTypeChecker{ irgen, target }(lit);
        if (!type) { irgen->error(type.error()); return nullptr; }
        auto as_llvm = irgen->ToLLVMType(type.value(), false);
        auto val = irgen->Alloca("array_literal", as_llvm);
        if(type->is_static_array())
        {
            if (std::holds_alternative<repeat_notation>(lit->elements)) {
                auto& rpn = std::get<repeat_notation>(lit->elements);
                llvm::Value* elem = nullptr;
                if (target && target->is_array())
                    elem = std::visit(LLVMExpressionEvaluator{ irgen, target->subtypes[0] }, rpn.first->toVariant());
                else
                    elem = std::visit(*this, rpn.first->toVariant());
                for (size_t i = 0; i < type->static_array_size(); i++)
                {
                    auto ptr = irgen->builder->CreateConstGEP2_32(as_llvm, val, 0, i);
                    clone(rpn.first.get(), elem, type->subtypes[0], ptr);
                }
                return val;
            }
            auto& elements = std::get<list_notation>(lit->elements);
            for(size_t i = 0; i < elements.size(); ++i)
            {
                auto elem = irgen->builder->CreateConstGEP2_32(as_llvm, val, 0, i);
                auto tp = std::visit(tp_check, elements[i]->toVariant());
                if (!tp) {irgen->error(tp.error()); continue;}
                implicitConvert(elements[i].get(),
                    std::visit(*this, elements[i]->toVariant()),
                    tp.value(),
                    type->subtypes[0],
                    elem);
            }
        }
        return val;
    }
    llvm::Value* LLVMExpressionEvaluator::operator()(RealLiteral* lit)
    {
        return llvm::ConstantFP::get(llvm::Type::getDoubleTy(irgen->context), lit->token.text);
    }
    //str is of type {ptr, int64, int64} for buffer, size, capacity
    llvm::Value* LLVMExpressionEvaluator::operator()(StringLiteral* lit)
    {
        ExpressionTypeChecker type_checker{irgen};
        if (auto tp = type_checker(lit); !tp) { irgen->error(tp.error()); return nullptr; }
        std::vector<std::pair<llvm::Value*, llvm::Value*>> substrings;
        for(auto& substr: lit->literal)
        {
            if(std::holds_alternative<std::string>(substr))
            {
                auto& as_str = std::get<std::string>(substr);
                auto gvar = irgen->builder->CreateGlobalString(as_str);

                auto str_size = llvm::ConstantInt::get(llvm::Type::getInt64Ty(irgen->context) ,as_str.size());
                substrings.emplace_back(gvar, str_size);

            }
            else //if(std::holds_alternative<std::unique_ptr<Expression>>)
            {
                auto& as_expr = std::get<1>(substr);
                auto type = std::visit(type_checker, as_expr->toVariant());
                auto value = std::visit(*this, as_expr->toVariant());
                substrings.push_back(doToStr(value, *type));
            }
        }
        llvm::Value* final_len = substrings.front().second;
        //combine all the strings into one buffer
        for(auto& pair : std::ranges::subrange(substrings.begin() + 1, substrings.end()))
            final_len = irgen->builder->CreateAdd(final_len, pair.second);
        //add 1 for null termination
        llvm::Value* final_buffer = irgen->Malloc("string_buffer", irgen->builder->CreateAdd(
            final_len, llvm::ConstantInt::get(llvm::Type::getInt64Ty(irgen->context), 1)
            ));
        llvm::Value* offset = llvm::ConstantInt::get(llvm::Type::getInt64Ty(irgen->context), 0);
        auto i8_ty = llvm::Type::getInt8Ty(irgen->context);
        for(auto& pair : substrings)
        {
            auto current_pointer = irgen->builder->CreateGEP(i8_ty, final_buffer, {offset});
            irgen->builder->CreateMemCpy(current_pointer, std::nullopt, pair.first, std::nullopt, pair.second);
            if(!llvm::isa<llvm::GlobalVariable>(pair.first)) irgen->Free(pair.first);
            offset = irgen->builder->CreateAdd(offset, pair.second);
        }
        irgen->builder->CreateStore(llvm::ConstantInt::get(i8_ty, 0),
            irgen->builder->CreateGEP(i8_ty, final_buffer, { offset }));

        auto llvm_t = irgen->ToLLVMType(Type{"str"}, false);
        auto string = irgen->Alloca("str_obj", llvm_t);

        auto buffer_ptr = irgen->builder->CreateStructGEP(llvm_t, string, 0);
        auto size_ptr = irgen->builder->CreateStructGEP(llvm_t, string, 1);
        auto cap_ptr = irgen->builder->CreateStructGEP(llvm_t, string, 2);

        irgen->builder->CreateStore(final_buffer, buffer_ptr);
        irgen->builder->CreateStore(final_len, size_ptr);
        irgen->builder->CreateStore(final_len, cap_ptr);

        return string;

    }
    llvm::Value* LLVMExpressionEvaluator::operator()(NameExpression* nm)
    {
        std::string name(nm->text);
        auto ret_type = ExpressionTypeChecker{irgen}(nm);
        if (!ret_type) { irgen->error(ret_type.error()); return nullptr; }
        for(size_t i = irgen->variables.size(); i > 0; --i)
        {
            size_t idx = i - 1;
            if(auto var = irgen->variables[idx].find(name); var != irgen->variables[idx].end())
            {
                if(!std::get<1>(var->second).should_sret())
                {
                    return irgen->builder->CreateLoad(
                        irgen->ToLLVMType(std::get<1>(var->second), false), std::get<0>(var->second), name);
                }
                if(!ret_type->is_lvalue)
                {
                    //mark the drop flag
                    auto drop_flag = std::get<2>(var->second);
                    if(drop_flag) irgen->builder->CreateStore(llvm::ConstantInt::getFalse(irgen->context), drop_flag);
                }
                return std::get<0>(var->second);
            }
        }
        if(auto [name_prefix, fn] = irgen->module->findFunction(irgen->block_hash, name); fn)
        {
            auto llvm_fn = irgen->code->getFunction(name_prefix + name);
            if(!llvm_fn)
                llvm_fn = declareFunction(name_prefix + name, irgen, fn->sig);
            return llvm_fn;
        }
        if (ret_type->is_error_ty()) return nullptr;
        auto cnst = ConstantEvaluator{ irgen }(nm);
        auto as_llvm = irgen->ToLLVMType(*ret_type, false);
        if (ret_type->is_integral() || ret_type->is_char() || ret_type->is_boolean() || ret_type->get_decl_if_enum())
            return std::visit([as_llvm]<typename T>(T & val) {
            if constexpr (std::is_integral_v<T>) return llvm::ConstantInt::get(as_llvm, val);
            else return static_cast<llvm::ConstantInt*>(nullptr);
        }, cnst.internal_repr);
        else return reinterpret_cast<llvm::Constant*>(std::get<void*>(cnst.internal_repr));
    }

    llvm::Value* LLVMExpressionEvaluator::operator()(GenericNameExpression* nm)
    {
        if(auto[hash, fn] = irgen->module->findGenericFn(irgen->block_hash, nm->text); fn)
        {
            std::string mangled_name_suffix = nm->text + IRGenerator::mangleGenericArgs(nm->arguments);
            //it has not already been instantiated
            if(auto[name_prefx, ifn] = irgen->module->findFunction(irgen->block_hash, mangled_name_suffix); !ifn)
                generateGenericFunction(irgen->module, hash, fn, nm->arguments);
            return irgen->code->getFunction(hash + mangled_name_suffix);
        }
    }

    llvm::Value* LLVMExpressionEvaluator::operator()(PrefixOperation* op)
    {
        auto target = ExpressionTypeChecker{irgen}(op);
        if(!target) {irgen->error(target.error()); return nullptr;}

        switch(op->op.type)
        {
            //dereference
        case TokenType::Star:
            {
                auto operand = std::visit(*this, op->operand->toVariant());
                if(!target->should_sret())
                    return irgen->builder->CreateLoad(irgen->ToLLVMType(*target, false), operand);
                return operand;
            }
        case TokenType::Ampersand:
            {
                auto operand = std::visit(*this, op->operand->toVariant());
                if(!target->subtypes[0].should_sret())
                {
                    auto val = irgen->Alloca("temp", irgen->ToLLVMType(target->subtypes[0], false));
                    irgen->builder->CreateStore(operand, val);
                    return val;
                }
                //taking the address of the lvalue causes the lifetime to be extended till the ref dies
                if(!target->deref().is_trivially_destructible(irgen) && !target->deref().is_lvalue)
                {
                    auto lf = new ExtendedLifetimes;
                    lf->objects.emplace_back(std::move(target->subtypes[0]), operand);
                    std::string name = "__del_parents___aaaaaaaa";
                    memcpy(name.data() + 16, &lf, sizeof(void*));
                    operand->setName(name);
                }
                return operand;
            }
        case TokenType::RefMut:
            {
                return std::visit(LValueEvaluator{irgen}, op->operand->toVariant());
            }
        case TokenType::Bang:
        {
            auto operand = std::visit(*this, op->operand->toVariant());
            return irgen->builder->CreateNot(operand);
        }
        }
    }
    llvm::Value* LLVMExpressionEvaluator::operator()(BinaryOperation* op)
    {
        auto type_checker = ExpressionTypeChecker{irgen};
        auto res =  type_checker(op);
        if(!res) {irgen->error(res.error()); return nullptr;}

        auto l_as_var = op->lhs->toVariant();
        auto r_as_var = op->rhs->toVariant();

        auto left_t = std::visit(type_checker, l_as_var);
        auto right_t = std::visit(type_checker, r_as_var);

        if (!left_t) { irgen->error(left_t.error()); return nullptr; }
        if(op->op.type != TokenType::Dot)
            if (!right_t) { irgen->error(right_t.error()); return nullptr; }

        auto lhs = op->lhs.get(); auto rhs = op->rhs.get();
        switch(op->op.type)
        {
            using enum TokenType;
        case Plus: return doAddition(lhs, rhs, *left_t, *right_t);
        case Minus: return doMinus(lhs, rhs, *left_t, *right_t);
        case Star: return doMult(lhs, rhs, *left_t, *right_t);
        case Slash: return doDiv(lhs, rhs, *left_t, *right_t);
        case Percent: return doRem(lhs, rhs, *left_t, *right_t);
        case DoubleGreater: return doShr(lhs, rhs, *left_t, *right_t);
        case DoubleLess: return doShl(lhs, rhs, *left_t, *right_t);
        case Greater: return doCmp(GT, lhs, rhs, *left_t, *right_t, *res);
        case Less: return doCmp(LT, lhs, rhs, *left_t, *right_t, *res);
        case GreaterEqual: return doCmp(EQ_GT, lhs, rhs, *left_t, *right_t, *res);
        case LessEqual: return doCmp(EQ_LT, lhs, rhs, *left_t, *right_t, *res);
        case BangEqual: return doCmp(NE, lhs, rhs, *left_t, *right_t, *res);
        case DoubleEqual: return doCmp(EQ, lhs, rhs, *left_t, *right_t, *res);
        case Spaceship: return doCmp(SPACE, lhs, rhs, *left_t, *right_t, *res);
        case Dot: return doDot(op->lhs.get(), op->rhs.get(), *left_t);
        case DoubleDotEqual: irgen->error(Error(op, "Not implemented yet")); return nullptr;
        case DoubleDot: return doRange(lhs, rhs, *left_t, *right_t, *res);
        case Equal:
            return implicitConvert(rhs, std::visit(*this, r_as_var), *right_t, *left_t, std::visit(LValueEvaluator{ irgen }, l_as_var));
        default:; //TODO
        }
        return nullptr;
    }
    llvm::Value* LLVMExpressionEvaluator::operator()(GroupingExpression* op)
    {
        return std::visit(*this, op->expr->toVariant());
    }

    llvm::Value* LLVMExpressionEvaluator::operator()(LogicalOperation* op) { 
        auto fn = irgen->builder->GetInsertBlock()->getParent();
        auto type_checker = ExpressionTypeChecker{ irgen };
        auto ovrl_type = type_checker(op);
        if (!ovrl_type) { irgen->error(ovrl_type.error()); return nullptr; }
        auto l_as_var = op->lhs->toVariant();
        auto r_as_var = op->rhs->toVariant();
        auto left_t = std::visit(type_checker, l_as_var);
        auto right_t = std::visit(type_checker, r_as_var);
        
        if (!left_t) { irgen->error(left_t.error()); return nullptr; }
        if (!right_t) { irgen->error(right_t.error()); return nullptr; }

        if (ovrl_type->is_error_ty()) return nullptr;

        auto left = std::visit(*this, l_as_var);
        auto orig_block = irgen->builder->GetInsertBlock();
        if (!left) return left;
        auto left_ext = llvm::BasicBlock::Create(irgen->context, "lft_ext", fn, irgen->returnBlock);
        auto cont = llvm::BasicBlock::Create(irgen->context, "logi_cont", fn, irgen->returnBlock);
        if (op->op.type == TokenType::DoubleAmpersand) {
            // if left is true we need to verify id right is true
            irgen->builder->CreateCondBr(left, left_ext, cont);
            irgen->builder->SetInsertPoint(left_ext);
            // here we evaluate the right side
            auto right = std::visit(*this, r_as_var);
            // we go to cont whatever is the value of right is our answer
            // we need to retrieve block because it can be changed
            auto current_block = irgen->builder->GetInsertBlock();
            irgen->builder->CreateBr(cont);
            irgen->builder->SetInsertPoint(cont);
            auto result = irgen->builder->CreatePHI(llvm::Type::getInt1Ty(irgen->context), 2, "logi_result");
            result->addIncoming(llvm::ConstantInt::getFalse(irgen->context), orig_block);
            result->addIncoming(right, current_block);
            return result;
        }
        if (op->op.type == TokenType::DoublePipe) {
            irgen->builder->CreateCondBr(left, cont, left_ext);
            irgen->builder->SetInsertPoint(left_ext);
            auto right = std::visit(*this, r_as_var);
            auto current_block = irgen->builder->GetInsertBlock();
            irgen->builder->CreateBr(cont);
            irgen->builder->SetInsertPoint(cont);
            auto result = irgen->builder->CreatePHI(llvm::Type::getInt1Ty(irgen->context), 2, "logi_result");
            result->addIncoming(llvm::ConstantInt::getTrue(irgen->context), orig_block);
            result->addIncoming(right, current_block);
            return result;
        }
        return nullptr;
    }
    llvm::Value* LLVMExpressionEvaluator::operator()(PostfixOperation*) { return nullptr; }
    llvm::Value* LLVMExpressionEvaluator::fillArgs(bool uses_sret,
        const FunctionSignature& sig,
        std::vector<llvm::Value*>& args, llvm::Value* first, std::vector<std::unique_ptr<Expression>>& exprs)
    {
        llvm::Value* return_value = nullptr;
        if(uses_sret)
        {
            auto ret_t = irgen->ToLLVMType(sig.returnType, false);
            return_value = irgen->Alloca("return_temp", ret_t);
            args[0] = return_value;
        }
        if(first) args[uses_sret] = first;
        bool is_bound = first != nullptr;
        for(size_t i = 0; i < exprs.size(); i++)
        {
            auto arg = std::visit(*this, exprs[i]->toVariant());
            auto tp = std::visit(ExpressionTypeChecker{irgen}, exprs[i]->toVariant());
            if (!tp) continue;
            
            args[i + is_bound + uses_sret] = implicitConvert(exprs[i].get(), arg, *tp, sig.parameters[i + is_bound].type);
            
        }
        return return_value;
    }

    llvm::Value* LLVMExpressionEvaluator::doInvoke(CallOperation* op, const Type& left_t)
    {
        auto expr = reinterpret_cast<BinaryOperation*>(op->callee.get());
        auto fn_store = std::visit(*this, expr->lhs->toVariant());
        auto const_zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 0);
        auto as_llvm_t = irgen->ToLLVMType(left_t, false);
        auto ctx_ptr = irgen->builder->CreateGEP(as_llvm_t, fn_store, {const_zero,const_zero}, "fn_context_ptr_ptr");
        auto fn_ptr = irgen->builder->CreateGEP(as_llvm_t, fn_store, {const_zero,
            llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 1)}, "fn_ptr_ptr");

        auto ptr_ty = llvm::PointerType::get(irgen->context, 0);
        auto ctx = irgen->builder->CreateLoad(ptr_ty, ctx_ptr, "fn_ctx");
        auto fn = irgen->builder->CreateLoad(ptr_ty, fn_ptr, "fn_ptr");

        bool uses_sret = left_t.signature->returnType.should_sret();
        std::vector<llvm::Value*> args(op->arguments.size() + uses_sret);
        auto return_val = fillArgs(uses_sret, *left_t.signature, args, nullptr, op->arguments);
        auto args_w_lambda = args;
        args_w_lambda.push_back(ctx);
        auto ctx_as_int = irgen->builder->CreatePtrToInt(ctx, llvm::Type::getInt64Ty(irgen->context), "fn_ctx_as_int");
        auto zero64 = llvm::ConstantInt::get(llvm::Type::getInt64Ty(irgen->context), 0);
        auto is_lambda = irgen->builder->CreateICmpEQ(ctx_as_int, zero64, "is_lambda");
        auto current_fn = irgen->builder->GetInsertBlock()->getParent();
        auto then_bb = llvm::BasicBlock::Create(irgen->context, "lambda_call", current_fn, irgen->returnBlock);
        auto else_bb = llvm::BasicBlock::Create(irgen->context, "fn_call", current_fn, irgen->returnBlock);
        auto cont_bb = llvm::BasicBlock::Create(irgen->context, "after_call", current_fn, irgen->returnBlock);
        irgen->builder->CreateCondBr(is_lambda, then_bb, else_bb);

        auto fn_t = irgen->ToLLVMSignature(*left_t.signature);
        FunctionSignature sig = *left_t.signature;
        sig.parameters.push_back(FunctionParameter{.type = Type{"__ptr"}});
        auto lambda_t = irgen->ToLLVMSignature(sig);

        auto return_as_llvm = irgen->ToLLVMType(left_t.signature->returnType, false);

        irgen->builder->SetInsertPoint(then_bb);
        auto call_resl = irgen->builder->CreateCall(lambda_t, fn, args_w_lambda);

        irgen->builder->CreateBr(cont_bb);

        irgen->builder->SetInsertPoint(else_bb);
        auto call_resf = irgen->builder->CreateCall(fn_t, fn, args);
        irgen->builder->CreateBr(cont_bb);

        irgen->builder->SetInsertPoint(cont_bb);
        if(!uses_sret && !left_t.signature->returnType.is_void())
        {
            auto PN = irgen->builder->CreatePHI(return_as_llvm, 2, "invoke_result");
            PN->addIncoming(call_resl, then_bb);
            PN->addIncoming(call_resf, else_bb);
            return_val = PN;
        }


        return return_val;
    }

    void LLVMExpressionEvaluator::generateGenericFunction(Module* mod, const std::string& hash, GenericFunctionDeclaration* fn, std::span<Type> types)
    {
        for(auto& type: types) type.saturate(mod, irgen);
        std::string name = fn->name + IRGenerator::mangleGenericArgs(types);
        if(auto[_,exists] = mod->findFunction(hash, name); exists) return;
        auto module = irgen->module;
        irgen->module = mod;

        for(size_t i = 0; i < types.size(); i++)
            irgen->module->aliases[hash + name + "::"].emplace(fn->clause.types[i], types[i]);

        auto ptr = StatementTreeCloner::copy_stat_specific(static_cast<FunctionDeclaration*>(fn));
        auto new_decl = reinterpret_cast<FunctionDeclaration*>(ptr.get());
        new_decl->name = name;
        auto old_hash = irgen->reset_hash();
        irgen->block_hash = hash + name + "::";
        irgen->saturateSignature(new_decl->signature, mod);
        irgen->block_hash = hash;

        (*irgen)(new_decl);
        
        irgen->block_hash = std::move(old_hash);
        irgen->module = module;
    }
    void LLVMExpressionEvaluator::generateGenericClass(Module* mod, const std::string& hash, GenericClassDeclaration* decl, std::span<Type> types)
    {
        for (auto& type : types) type.saturate(mod, irgen);
        generateGenericClass(mod, hash, decl, std::span<const Type>{types});
    }
    void LLVMExpressionEvaluator::generateGenericClass(Module* mod, const std::string& hash, GenericClassDeclaration* decl, std::span<const Type> types)
    {
        std::string name = decl->name + IRGenerator::mangleGenericArgs(types);
        if (auto exists = mod->findType(hash, name); exists) return;
        auto module = irgen->module;
        irgen->module = mod;

        for (size_t i = 0; i < types.size(); i++)
            irgen->module->aliases[hash + name + "::"].emplace(decl->clause.types[i], types[i]);

        auto ptr = StatementTreeCloner::copy_stat_specific(static_cast<ClassDeclaration*>(decl));
        auto new_decl = reinterpret_cast<ClassDeclaration*>(ptr.get());
        new_decl->name = name;
        auto old_hash = irgen->reset_hash();

        irgen->current_Statement = &ptr;
        (*irgen)(new_decl);

        irgen->block_hash = std::move(old_hash);
        irgen->module = module;
    }

    void LLVMExpressionEvaluator::generateGenericAlias(Module* mod, const std::string& block, GenericAliasDeclaration* decl,
        std::span<Type> types)
    {
        for(auto& type: types) type.saturate(mod, irgen);
        std::string name = decl->name + IRGenerator::mangleGenericArgs(types);
        if(auto exists = mod->findAlias(block, name)) return;
        auto old_hash = std::move(irgen->block_hash);
        irgen->block_hash = block;
        auto module = irgen->module;
        irgen->module = mod;
        for(size_t i = 0; i < types.size(); i++)
            irgen->module->aliases[block + name + "__"].emplace(decl->clause.types[i], types[i]);
        auto new_statement = StatementTreeCloner::copy_stat_specific(static_cast<AliasDeclaration*>(decl));
        auto alias = reinterpret_cast<AliasDeclaration*>(new_statement.get());
        alias->name = name;
        (*irgen)(alias);
        irgen->block_hash = std::move(old_hash);
        irgen->module = module;
    }
    void LLVMExpressionEvaluator::generateGenericInterface(Module* md, const std::string& block, GenericInterfaceDeclaration* decl, std::span<Type> types)
    {
        for (auto& type : types) type.saturate(md, irgen);
        std::string name = decl->name + IRGenerator::mangleGenericArgs(types);
        if (auto [_, exists] = md->findInterface(block, name); exists) return;
        auto new_interface = StatementTreeCloner::copy_stat_specific(static_cast<InterfaceDeclaration*>(decl));
        auto itf = reinterpret_cast<InterfaceDeclaration*>(new_interface.release());
        itf->name = name;
        //TODO interface visitors to automatically saturate signatures
        for (size_t i = 0; i < types.size(); i++)
            md->aliases[block + name + "__"].emplace(decl->clause.types[i], types[i]);
        auto old_mod = irgen->module;
        auto old_hash = irgen->reset_hash();

        irgen->module = md;
        irgen->block_hash = block + name + "__";
        for (auto& fn : itf->methods)
            irgen->saturateSignature(fn->signature, md);
        irgen->block_hash.swap(old_hash);
        irgen->module = old_mod;
        md->interfaces[block].emplace_back(itf);
    }
    llvm::Value* LLVMExpressionEvaluator::operator()(CallOperation* op)
    {
        ExpressionTypeChecker type_checker{irgen};
        //type checker is allowed to modify generic function nodes to do argument deduction
        auto return_t = type_checker(op);
        if (!return_t) { irgen->error(return_t.error()); return nullptr; }
        auto t = std::visit(type_checker, op->callee->toVariant());
        if (t && t->is_error_ty())
        {
            std::visit(*this, op->callee->toVariant());
            debugbreak();
        }
        if (t && t->name.starts_with("__union_var"))
        {
            return doUnionVar(op, *t);
        }
        if(!t || !(t->is_function() || t->is_lambda())) {irgen->error(t.error()); return nullptr;}
        if(!return_t)
            {irgen->error(return_t.error()); return nullptr;}
        bool is_lambda = t->is_lambda();
        auto fn = reinterpret_cast<FunctionType&>(*t);
        auto expr = dynamic_cast<BinaryOperation*>(op->callee.get());
        ExpressionTypeChecker::Result left_ty;
        if(expr) left_ty = std::visit(type_checker, expr->lhs->toVariant());
        if(left_ty && left_ty->name == "__called_fn")
        {
            return doInvoke(op, *left_ty);
        }
        if(fn.is_bound)
        {
            //expr is guaranteed to be valid if the function is bound
            //callee is a binary dot expr
            auto left_t = std::visit(ExpressionTypeChecker{irgen}, expr->lhs->toVariant());
            auto cls = left_t->deref().get_decl_if_class(irgen);
            if(auto rhs = dynamic_cast<NameExpression*>(expr->rhs.get()))
            {
                auto this_block = left_t->deref().full_name() + "::";
                auto [block, fn] =
                    left_t->deref().module->findFunction(this_block, rhs->text);
                if (block == this_block && fn->sig.parameters[0].name == "this")
                {
                    std::string function_name = block + rhs->text;
                    auto callee = irgen->code->getFunction(function_name);
                    if (!callee)
                    {
                        irgen->block_hash.swap(block);
                        callee = declareFunction(function_name, irgen, fn->sig);
                        irgen->block_hash.swap(block);
                    }
                    bool uses_sret = callee->hasStructRetAttr();

                    std::vector<llvm::Value*> args(op->arguments.size() + 1 + uses_sret); // +1 because its bound
                    auto first = std::visit(*this, expr->lhs->toVariant());
                    if(left_t->deref() != fn->sig.parameters[0].type.deref())
                        first = implicitConvert(expr->lhs.get(), first, *left_t, fn->sig.parameters[0].type);
                    llvm::Value* return_value = fillArgs(uses_sret, fn->sig, args, first, op->arguments);
                    auto call_val = irgen->builder->CreateCall(callee, args);
                    if (!uses_sret) return_value = call_val;
                    if (return_t->is_non_owning(irgen))
                        stealUsages(args, return_value);
                    else
                        clearUsages(args, *this);
                    return return_value;
                } 
            }
            if (left_t->deref().is_view())
            {
                auto& viewed = left_t->deref().subtypes[0];
                auto [hsh, interface] = viewed.module->findInterface(viewed.block_hash, viewed.name);
                if (auto* name_expr = dynamic_cast<NameExpression*>(expr->rhs.get()); name_expr && interface)
                {
                    auto it = std::ranges::find_if(interface->methods, [name_expr](auto& mth) {
                        return mth->name == name_expr->text;
                        });
                    auto fty = irgen->ToLLVMSignature((*it)->signature);
                    bool uses_sret = (*it)->signature.returnType.should_sret();
                    std::vector<llvm::Value*> args(op->arguments.size() + 1 + uses_sret);
                    auto as_llvm_type = irgen->ToLLVMType(left_ty->deref(), false);
                    auto value = std::visit(*this, expr->lhs->toVariant());

                    auto pointer_type = llvm::PointerType::get(irgen->context, 0);

                    auto param = irgen->builder->CreateLoad(pointer_type,
                        irgen->builder->CreateStructGEP(as_llvm_type, value, 0));
                    auto callee = irgen->builder->CreateLoad(pointer_type, 
                        irgen->builder->CreateStructGEP(as_llvm_type, value, 1 + std::distance(interface->methods.begin(), it)));

                    llvm::Value* return_value = fillArgs(uses_sret, t->sig, args, param, op->arguments);
                    auto call_val = irgen->builder->CreateCall(fty, callee, args);
                    if (!uses_sret) return_value = call_val;
                    if (return_t->is_non_owning(irgen))
                        stealUsages(args, return_value);
                    else
                        clearUsages(args, *this);
                    return return_value;
                }
            }
            auto right_t = std::visit(ExpressionTypeChecker{ irgen }, expr->rhs->toVariant());
            if (right_t->is_interface_function() && cls)
            {
                using namespace std::string_view_literals;
                auto pos = right_t->name.find_first_of('$');
                std::string name = right_t->name.substr("__interface_fn"sv.size(), pos - "__interface_fn"sv.size());
                std::string fn_name = right_t->name.substr(pos + 1);
                auto dets = irgen->module->findType(left_t->deref().block_hash, left_t->deref().full_name_no_block());
                fn_name = std::get<0>(*dets) + name + "::" + fn_name;
                auto callee = irgen->code->getFunction(fn_name);
                bool uses_sret = callee->hasStructRetAttr();
                std::vector<llvm::Value*> args(op->arguments.size() + 1 + uses_sret);

                llvm::Value* return_value = fillArgs(uses_sret, t->sig, args, std::visit(*this, expr->lhs->toVariant()), op->arguments);
                auto call_val = irgen->builder->CreateCall(callee, args);
                if (!uses_sret) return_value = call_val;
                if (return_t->is_non_owning(irgen))
                    stealUsages(args, return_value);
                else
                    clearUsages(args, *this);
                return return_value;
                
            }
            auto* callee = is_lambda ?
                irgen->code->getFunction(irgen->block_hash + right_t->name) :
                llvm::dyn_cast_or_null<llvm::Function>(std::visit(*this, expr->rhs->toVariant()));
            if(!callee) { /* return an llvm invalid value*/ return nullptr; }
            const auto* sig = is_lambda ? &right_t->module->lambdas[right_t->name].second->sig
                : &right_t->sig;
            bool uses_sret = callee->hasStructRetAttr();
            std::vector<llvm::Value*> args(op->arguments.size() + 1 + uses_sret);
            auto first = std::visit(*this, expr->lhs->toVariant());
            if (left_t->deref() != sig->parameters[0].type.deref())
            {
                first = implicitConvert(expr->lhs.get(), first, *left_t, sig->parameters[0].type, nullptr);
            }
            llvm::Value* return_value = fillArgs(uses_sret, fn.sig, args, first, op->arguments);
            if(is_lambda) args.push_back(std::visit(*this, expr->rhs->toVariant()));
            auto call_val = irgen->builder->CreateCall(callee->getFunctionType(), callee, args);
            if(!uses_sret) return_value = call_val;
            if(return_t->is_non_owning(irgen))
                stealUsages(args, return_value);
            else
                clearUsages(args, *this);
            return return_value;
        }

        auto callee_val = std::visit(*this, op->callee->toVariant());
        auto* callee = is_lambda ?
            irgen->code->getFunction(irgen->block_hash + t->name) :
            llvm::dyn_cast_or_null<llvm::Function>(callee_val);
        if(!callee) { return nullptr; }
        bool uses_sret = callee->hasStructRetAttr();
        std::vector<llvm::Value*> args(op->arguments.size() + uses_sret);
        llvm::Value* return_value = fillArgs(uses_sret, fn.sig, args, nullptr, op->arguments);
        if(is_lambda) args.push_back(callee_val);
        auto call_val = irgen->builder->CreateCall(callee->getFunctionType(), callee, args);
        if(!uses_sret) return_value = call_val;
        if(return_t->is_non_owning(irgen))
            stealUsages(args, return_value);
        else
            clearUsages(args, *this);
        return return_value;
    }
    llvm::Value* LLVMExpressionEvaluator::operator()(SubscriptOperation* op)
    {
        auto obj_ty = std::visit(ExpressionTypeChecker{irgen}, op->object->toVariant());
        if (!obj_ty) { irgen->error(obj_ty.error()); return nullptr; }
        auto llvm_t = irgen->ToLLVMType(obj_ty->deref(), false);
        auto obj = std::visit(*this, op->object->toVariant());
        auto idx = std::visit(*this, op->index->toVariant());
        if(obj_ty->deref().is_static_array())
        {
            auto const_zero = llvm::ConstantInt::get(idx->getType(), 0);
            return irgen->builder->CreateGEP(llvm_t, obj, {const_zero, idx});
        }
        if (obj_ty->is_slice())
        {
            auto data_ptr = irgen->builder->CreateStructGEP(llvm_t, obj, 0);
            data_ptr = irgen->builder->CreateLoad(llvm::PointerType::get(irgen->context, 0), data_ptr);
            auto sub_ty = irgen->ToLLVMType(obj_ty->subtypes[0], false);
            return irgen->builder->CreateGEP(sub_ty, data_ptr, { idx });
        }
    }
    llvm::Value* LLVMExpressionEvaluator::operator()(GCNewExpression* expr)
    {
        auto gc_tp = std::visit(ExpressionTypeChecker{ irgen }, expr->toVariant());
        if (!gc_tp) { irgen->error(gc_tp.error()); return nullptr; }
        auto tp = &gc_tp.value();
        auto value = std::visit(*this, expr->target_expression->toVariant());
        if (tp->is_error_ty()) return nullptr;
        auto internal_ty = irgen->ToLLVMType((*tp).subtypes[0], false);
        auto llvm_ty = llvm::StructType::get(irgen->context, {
            internal_ty,
            llvm::Type::getInt64Ty(irgen->context)
        }, false);
        size_t type_size = irgen->code->getDataLayout().getTypeAllocSize(llvm_ty);
        size_t internal_size = irgen->code->getDataLayout().getTypeAllocSize(internal_ty);
        auto memory = irgen->GCMalloc(type_size);
        auto copy_to = irgen->builder->CreateStructGEP(llvm_ty, memory, 0);
        irgen->builder->CreateMemCpy(copy_to, std::nullopt, value, std::nullopt, internal_size);
        irgen->builder->CreateStore(llvm::ConstantInt::get(llvm::Type::getInt64Ty(irgen->context), 0), irgen->builder->CreateStructGEP(llvm_ty, memory, 1));
        return memory;
    }
    llvm::Value* LLVMExpressionEvaluator::operator()(MacroInvocation* invc)
    {
        ExpressionTypeChecker{ irgen }(invc);
        return std::visit(*this, invc->result->toVariant());
    }
    llvm::Value* LLVMExpressionEvaluator::operator()(LambdaExpression* expr)
    {
        auto t = ExpressionTypeChecker{ irgen }(expr);
        if (!t) { irgen->error(t.error()); return nullptr; }
        auto context = t->module->lambdas.at(t->name).first;
        auto ctx_object = irgen->Alloca("lambda_context", context);
        //copy types into the context
        size_t idx = 0;
        for(auto& capture : expr->captures)
        {
            NameExpression name(capture.name);
            auto type = ExpressionTypeChecker{irgen}(&name);
            bool is_last_use = irgen->function_cfgs.back().last_uses.at(name.text).contains(expr);
            type->is_lvalue = !is_last_use;
            llvm::Value* lhs = irgen->builder->CreateStructGEP(context, ctx_object, idx);
            if (capture.cp_type != Ownership::Owning)
            {
                llvm::Value* val = LValueEvaluator{ irgen }(&name);
                irgen->builder->CreateStore(val, lhs);
            }
            else
            {
                llvm::Value* val = (*this)(&name);
                clone(&name, val, *type, lhs);
            }
            idx++;
        }
        std::string name = "__lambda" + expr->hash;
        
        FunctionSignature sig = expr->sig;
        //insert a pointer to the context at the end of the signature
        sig.parameters.push_back(FunctionParameter{.type = Type{.name = name, .module = irgen->module}});
        FunctionDeclaration decl(name,std::move(sig), std::move(expr->body));
        auto old_hash = irgen->reset_hash();
        (*irgen)(&decl);
        irgen->block_hash.swap(old_hash);
        return ctx_object;
    }

    llvm::Value* LLVMExpressionEvaluator::operator()(ScopeOperation* op)
    {
        auto ty = ExpressionTypeChecker{irgen}(op);
        if (!ty) { irgen->error(ty.error()); return nullptr; }
        if(ty->is_function())
        {
            std::string mangled_name = ty->block_hash + UnsaturatedTypeIterator(op->type).last().name;
            auto fn = irgen->code->getFunction(mangled_name);
            if(!fn) fn = declareFunction(mangled_name, irgen, ty->sig);
            return fn;
        }
        if (auto enm = ty->get_decl_if_enum())
        {
            auto name = UnsaturatedTypeIterator(op->type).last().name;
            return llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), enm->values[name]);
        }
        if (ty->is_error_ty()) return nullptr;
        // try to eval as const
        auto cnst = ConstantEvaluator{ irgen }(op);
        
        auto as_llvm = irgen->ToLLVMType(*ty, false);
        if (ty->is_integral() || ty->is_char() || ty->is_boolean() || ty->get_decl_if_enum())
            return std::visit([as_llvm]<typename T>(T & val) {
            if constexpr (std::is_integral_v<T>) return llvm::ConstantInt::get(as_llvm, val);
            else return static_cast<llvm::ConstantInt*>(nullptr);
        }, cnst.internal_repr);
        else return reinterpret_cast<llvm::Constant*>(std::get<void*>(cnst.internal_repr));
    }

    llvm::Value* LLVMExpressionEvaluator::operator()(ObjectLiteral* lit)
    {
        auto t = ExpressionTypeChecker{irgen}(lit);
        if(!t) {irgen->error(t.error());return nullptr;}
        auto as_llvm_type = irgen->ToLLVMType(*t, false);
        auto decl = t->get_decl_if_class(irgen);
        auto value = irgen->Alloca("obj_lit",as_llvm_type);

        auto zero_const = llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 0);
        for(size_t i = 0; i < decl->vars.size(); i++)
        {
            auto idx_const = llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), i);
            auto& var = decl->vars[i];
            auto val_ty = std::visit(ExpressionTypeChecker{irgen, var.type}, lit->values[var.name]->toVariant());
            if (!val_ty) { irgen->error(val_ty.error()); continue; }
            auto val = std::visit(LLVMExpressionEvaluator{irgen, var.type}, lit->values[var.name]->toVariant());

            auto mem_ptr = irgen->builder->CreateGEP(as_llvm_type, value, {zero_const, idx_const});
            implicitConvert(lit->values[var.name].get(), val, *val_ty, var.type, mem_ptr);
        }
        return value;
    }

    llvm::Value* LLVMExpressionEvaluator::operator()(NullLiteral*)
    {
        return nullptr;
    }

    llvm::Value* LLVMExpressionEvaluator::operator()(AsExpression* expr)
    {
        auto ty = std::visit(ExpressionTypeChecker{irgen}, expr->expr->toVariant());
        auto final_ty = std::visit(ExpressionTypeChecker{irgen}, expr->toVariant());
        if (!final_ty) { irgen->error(final_ty.error()); return nullptr; }
        if (!ty) { irgen->error(ty.error()); return nullptr; }
        auto as_llvm = irgen->ToLLVMType(*final_ty, false);
        auto internal_as_llvm = irgen->ToLLVMType(*ty, true);
        auto internal = std::visit(*this, expr->expr->toVariant());
        if(ty->is_variant())
        {
            const std::set subtypes(ty->subtypes.begin(), ty->subtypes.end());
            auto val = irgen->Alloca("variant_conv", as_llvm);
            auto ptr = irgen->builder->CreateStructGEP(as_llvm, val, 0);
            auto internal_ptr = irgen->builder->CreateStructGEP(internal_as_llvm, internal, 0);
            irgen->builder->CreateStore(internal_ptr, ptr);
            auto is_valid_ptr = irgen->builder->CreateStructGEP(as_llvm, val, 1);
            auto idx_ptr = irgen->builder->CreateStructGEP(internal_as_llvm, internal, 1);

            auto i32_ty = llvm::Type::getInt32Ty(irgen->context);
            auto this_idx = llvm::ConstantInt::get(i32_ty, std::distance(subtypes.begin(), subtypes.find(expr->dest)));
            auto correct_idx = irgen->builder->CreateLoad(i32_ty, idx_ptr);

            auto is_valid = irgen->builder->CreateICmpEQ(this_idx, correct_idx);
            irgen->builder->CreateStore(is_valid, is_valid_ptr);
            if(!ty->deref().is_trivially_destructible(irgen) && !ty->deref().is_lvalue)
            {
                auto lf = new ExtendedLifetimes;
                lf->objects.emplace_back(std::move(ty).value(), internal);
                std::string name = "__del_parents___aaaaaaaa";
                memcpy(name.data() + 16, &lf, sizeof(void*));
                val->setName(name);
            }
            return val;
        }
        if (final_ty->is_assignable_from(*ty, irgen))
        {
            return implicitConvert(expr->expr.get(), internal, *ty, *final_ty);
        }
        if (final_ty->is_view())
        {
            auto& viewed = expr->dest.subtypes[0];
            auto [name, intf] = viewed.module->findInterface(viewed.block_hash, viewed.name);
            std::string full_name = name + intf->name;
            if (auto cls = ty->deref().get_decl_if_class(irgen))
            {
                auto details = ty->deref().module->findType(ty->deref().block_hash, ty->deref().name);
                auto result = irgen->Alloca("interface_cast", as_llvm);
                auto this_ptr = irgen->builder->CreateStructGEP(as_llvm, result, 0);
                irgen->builder->CreateStore(internal, this_ptr);
                size_t idx = 0;
                for (auto& method : intf->methods)
                {
                    idx++;
                    auto fn_ptr = irgen->builder->CreateStructGEP(as_llvm, result, idx);
                    std::string method_name = std::get<0>(*details) + "__interface" + full_name + "__%" + method->name;
                    auto fn = irgen->code->getFunction(method_name);
                    irgen->builder->CreateStore(fn, fn_ptr);
                }
                return result;
            }
        }
        if (auto decl = ty->get_decl_if_union())
        {
            auto it = decl->fields.find(expr->dest.name);
            size_t idx = std::distance(decl->fields.begin(), it);
            auto val = irgen->Alloca("union_conv", as_llvm);
            auto ptr = irgen->builder->CreateStructGEP(as_llvm, val, 0);
            auto internal_ptr = irgen->builder->CreateStructGEP(internal_as_llvm, internal, 0);
            irgen->builder->CreateStore(internal_ptr, ptr);

            auto is_valid_ptr = irgen->builder->CreateStructGEP(as_llvm, val, 1);
            auto idx_ptr = irgen->builder->CreateStructGEP(internal_as_llvm, internal, 1);
            auto i32_ty = llvm::Type::getInt32Ty(irgen->context);
            auto this_idx = llvm::ConstantInt::get(i32_ty, idx);
            auto correct_idx = irgen->builder->CreateLoad(i32_ty, idx_ptr);

            auto is_valid = irgen->builder->CreateICmpEQ(this_idx, correct_idx);
            irgen->builder->CreateStore(is_valid, is_valid_ptr);
            if (!ty->deref().is_trivially_destructible(irgen) && !ty->deref().is_lvalue)
            {
                auto lf = new ExtendedLifetimes;
                lf->objects.emplace_back(std::move(ty).value(), internal);
                std::string name = "__del_parents___aaaaaaaa";
                memcpy(name.data() + 16, &lf, sizeof(void*));
                val->setName(name);
            }
            return val;
        }
        return nullptr;
    }

    llvm::Value* LLVMExpressionEvaluator::operator()(CharLiteral* lit)
    {
        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), lit->value);
    }
}
