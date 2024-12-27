#include <cmath>
#include <csignal>
#include <overload_resolve.h>
#include <ranges>
#include <set>
#include <tree_cloner.h>
#include <llvm/Support/Error.h>

#include "ir_gen.h"
#include "fn_type.h"
namespace Yoyo
{
    struct ExtendedLifetimes
    {
        std::vector<std::pair<Type, llvm::Value*>> objects;
    };
    llvm::Function* declareFunction(const std::string& mangled_name, IRGenerator* irgen, FunctionSignature& fn_sig)
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
    
    llvm::Value* ExpressionEvaluator::implicitConvert(llvm::Value* val, const Type& src, const Type& dst, llvm::Value* out) const
    {
        if(dst.is_equal(src)) { return clone(val, src, out); }if(dst.is_equal(src)) { return clone(val, src, out); }
        if(!dst.is_assignable_from(src)) { irgen->error(); return nullptr; }
        auto dst_as_llvm = irgen->ToLLVMType(dst, false);
        if(!out) out = irgen->Alloca("implicit_convert", dst_as_llvm);
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
                    irgen->error();
                    return nullptr;
                }
                if(as_int->getZExtValue() <= max_bound)
                {
                    val = irgen->builder->CreateZExtOrTrunc(val, dst_as_llvm);
                    irgen->builder->CreateStore(val, out);
                    return val;
                }
                irgen->error();
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
                    irgen->error();
                    return nullptr;
                }
                if(as_int->getZExtValue() <= max_bound)
                {
                    val = irgen->builder->CreateUIToFP(val, dst_as_llvm);
                    irgen->builder->CreateStore(val, out);
                    return val;
                }
                irgen->error();
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
            implicitConvert(val, src, dst.subtypes[0], irgen->builder->CreateStructGEP(dst_as_llvm, out, 0));
            irgen->builder->CreateStore(llvm::ConstantInt::getTrue(irgen->context), irgen->builder->CreateStructGEP(dst_as_llvm, out, 1));
        }
        if(dst.is_variant())
        {
            const std::set subtypes(dst.subtypes.begin(), dst.subtypes.end());
            const auto type_idx_ptr = irgen->builder->CreateStructGEP(dst_as_llvm, out, 1);
            uint32_t i = 0;

            for(auto& sub : subtypes)
            {
                if(!sub.is_assignable_from(src)) { i++; continue; }
                implicitConvert(val, src, sub, irgen->builder->CreateStructGEP(dst_as_llvm, out, 0));
                irgen->builder->CreateStore(llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), i),
                    type_idx_ptr);
                break;
            }
        }
        return out;
    }
    /// IMPORTANT:\n
    /// I don't know where to place this, but all structural types are pointers
    llvm::Value* ExpressionEvaluator::doAssign(llvm::Value* lhs, llvm::Value* rhs, const Type& left_type, const Type& right_type)
    {
        if(!left_type.is_mutable) {irgen->error(); return nullptr;}
        if(!left_type.is_assignable_from(right_type)) {irgen->error(); return nullptr;}
        return implicitConvert(rhs, right_type, left_type, lhs);
    }
    /// user defined types can create a @c clone function.\n
    /// The @c clone function's signature looks like @code clone: (&this) -> This @endcode \n
    /// Without it the type is cannot be copied
    /// I think it should be implicit for union types
    llvm::Value* ExpressionEvaluator::clone(llvm::Value* value, const Type& left_type, llvm::Value* into) const
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
            size_t size = layout.getStructLayout(llvm::dyn_cast<llvm::StructType>(as_llvm))->getSizeInBytes();
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
                clone(val, sub, ptr);
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
            clone(sub_value, left_type.subtypes[0], irgen->builder->CreateStructGEP(opt_ty, into, 0));
            irgen->builder->CreateBr(opt_assign_cont);

            irgen->builder->SetInsertPoint(opt_assign_cont);
            irgen->builder->CreateStore(has_value, irgen->builder->CreateStructGEP(opt_ty, into , 1));
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

                clone(sub_value, sub, irgen->builder->CreateStructGEP(llvm_ty, into, 1));

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
            if(!decl->has_clone) { irgen->error(); return nullptr; }
            auto candidate = std::ranges::find_if(decl->methods, [](auto& meth)
            {
                auto end = meth.function_decl->attributes.end();
                return std::ranges::find_if(meth.function_decl->attributes, [](auto& attr) {
                    return static_cast<Attribute&>(attr).name == "clone";
                }) != end;
            });
            if(candidate != decl->methods.end())
            {
                auto& sig = reinterpret_cast<FunctionDeclaration*>(candidate->function_decl.get())->signature;
                std::string fn_name = std::get<0>(*decl_tup) + candidate->name;
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
                    clone(sub_val_ptr, var.type, sub_into_ptr);
                    idx++;
                }
            }

        }
        return into;
    }
    void destroyNonOwningParents(const ExpressionEvaluator& eval, llvm::Value* value, const Type& tp)
    {
        if(value->getName().starts_with("__del_this"))
            eval.destroy(value, tp);
        if(value->getName().starts_with("__del_parents___"))
        {
            auto nm = value->getName();
            auto& lifetimes = **reinterpret_cast<ExtendedLifetimes* const*>(nm.data() + 16);
            for(auto& [type, obj] : lifetimes.objects)
                eval.destroy(obj, tp);
            delete &lifetimes;
        }
    }
    void ExpressionEvaluator::destroy(llvm::Value* value, const Type& type) const
    {
        if(type.is_trivially_destructible()) return;

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
            irgen->Free(mem_ptr);
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

        if(type.is_non_owning())
            destroyNonOwningParents(*this, value, type);
    }

    llvm::Value* ExpressionEvaluator::doDot(Expression* lhs, Expression* rhs, const Type& left_type, bool load_primitive)
    {
        if(left_type.deref().is_tuple())
        {
            if(auto idx = dynamic_cast<IntegerLiteral*>(rhs))
            {
                auto idx_int = std::stoul(std::string{idx->text});
                auto out_type = left_type.deref().subtypes[idx_int];

                auto llvm_t = irgen->ToLLVMType(left_type.deref(), false);
                auto left_ptr = std::visit(*this, lhs->toVariant());
                auto ptr = irgen->builder->CreateStructGEP(llvm_t, left_ptr, idx_int);
                if(load_primitive && !out_type.should_sret())
                    return irgen->builder->CreateLoad(irgen->ToLLVMType(out_type, false), ptr);
                //no partial move allowed, so rvalue binary operations clone the field and destroy the object
                if (!left_type.is_lvalue)
                {
                    if(!left_type.is_trivially_destructible())
                    {
                        auto ret = clone(ptr, left_type.subtypes[idx_int]);
                        destroy(left_ptr, left_type);
                        return ret;
                    }
                }
                return ptr;
            }
        }
        if(auto cls = left_type.deref().get_decl_if_class())
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

                    auto lalloc = std::visit(*this, lhs->toVariant());
                    ptr = irgen->builder->CreateStructGEP(llvm_t, lalloc, std::distance(cls->vars.begin(), var));


                    if(load_primitive && !var->type.should_sret()) return irgen->builder->CreateLoad(out_t, ptr, var->name);
                    if (!left_type.is_lvalue)
                    {
                        if(!left_type.is_trivially_destructible())
                        {
                            auto ret = clone(ptr, var->type);
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
        const Type& right_type, IRGenerator* irgen, const ExpressionEvaluator* eval)
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
            *lhs = eval->implicitConvert(*lhs, left_type, right_type);
            return;
        }
        if(left_type.name == "flit")
        {
            if(right_type.name == "flit") return;
            *lhs = eval->implicitConvert(*lhs, left_type, right_type);
            return;
        }
        if(right_type.name == "ilit" || right_type.name == "flit")
            convertLiterals(rhs, lhs, right_type, left_type, irgen, eval);
    }
    llvm::FunctionType* BinOverloadToLLVMSig(IRGenerator* irgen, OverloadDetailsBinary* bin)
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
    llvm::Function* getOperatorFunction(TokenType t, IRGenerator* irgen, OverloadDetailsBinary* target)
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
    llvm::Value* ExpressionEvaluator::doAddition(
        llvm::Value* lhs,
        llvm::Value* rhs,
        const Type& left_type,
        const Type& right_type) const
    {
        auto target = resolveAdd(left_type, right_type);
        auto fn = getOperatorFunction(TokenType::Plus, irgen, target);
        std::vector<llvm::Value*> args;
        if(target->result.should_sret())
            args.push_back(irgen->Alloca("ret_temp", irgen->ToLLVMType(target->result, false)));
        args.push_back(implicitConvert(lhs, left_type, target->left));
        args.push_back(implicitConvert(rhs, right_type, target->right));
        auto ret = irgen->builder->CreateCall(fn, args);
        return target->result.should_sret() ? args[0] : ret;
    }
    llvm::Value* ExpressionEvaluator::doMinus(
        llvm::Value* lhs,
        llvm::Value* rhs,
        const Type& left_type,
        const Type& right_type) const
    {
        auto target = resolveSub(left_type, right_type);
        auto fn = getOperatorFunction(TokenType::Minus, irgen, target);
        std::vector<llvm::Value*> args;
        if(target->result.should_sret())
            args.push_back(irgen->Alloca("ret_temp", irgen->ToLLVMType(target->result, false)));
        args.push_back(implicitConvert(lhs, left_type, target->left));
        args.push_back(implicitConvert(rhs, right_type, target->right));
        auto ret = irgen->builder->CreateCall(fn, args);
        return target->result.should_sret() ? args[0] : ret;
    }
    llvm::Value* ExpressionEvaluator::doMult(
        llvm::Value* lhs,
        llvm::Value* rhs,
        const Type& left_type,
        const Type& right_type) const
    {
        auto target = resolveMul(left_type, right_type);
        auto fn = getOperatorFunction(TokenType::Star, irgen, target);
        std::vector<llvm::Value*> args;
        if(target->result.should_sret())
            args.push_back(irgen->Alloca("ret_temp", irgen->ToLLVMType(target->result, false)));
        args.push_back(implicitConvert(lhs, left_type, target->left));
        args.push_back(implicitConvert(rhs, right_type, target->right));
        auto ret = irgen->builder->CreateCall(fn, args);
        return target->result.should_sret() ? args[0] : ret;
    }
    llvm::Value* ExpressionEvaluator::doDiv(
        llvm::Value* lhs,
        llvm::Value* rhs,
        const Type& left_type,
        const Type& right_type) const
    {

        auto target = resolveDiv(left_type, right_type);
        auto fn = getOperatorFunction(TokenType::Slash, irgen, target);
        std::vector<llvm::Value*> args;
        if(target->result.should_sret())
            args.push_back(irgen->Alloca("ret_temp", irgen->ToLLVMType(target->result, false)));
        args.push_back(implicitConvert(lhs, left_type, target->left));
        args.push_back(implicitConvert(rhs, right_type, target->right));
        auto ret = irgen->builder->CreateCall(fn, args);
        return target->result.should_sret() ? args[0] : ret;
    }
    llvm::Value* ExpressionEvaluator::doRem(
        llvm::Value* lhs,
        llvm::Value* rhs,
        const Type& left_type,
        const Type& right_type) const
    {
        auto target = resolveRem(left_type, right_type);
        auto fn = getOperatorFunction(TokenType::Percent, irgen, target);
        std::vector<llvm::Value*> args;
        if(target->result.should_sret())
            args.push_back(irgen->Alloca("ret_temp", irgen->ToLLVMType(target->result, false)));
        args.push_back(implicitConvert(lhs, left_type, target->left));
        args.push_back(implicitConvert(rhs, right_type, target->right));
        auto ret = irgen->builder->CreateCall(fn, args);
        return target->result.should_sret() ? args[0] : ret;
    }
    llvm::Value* ExpressionEvaluator::doCmp(
        ComparisonPredicate p,
        llvm::Value* lhs,
        llvm::Value* rhs,
        const Type& left_type,
        const Type& right_type,
        const Type& result_type) const
    {
        llvm::CmpInst::Predicate pred{};
        auto get_int_cmp_pred = [](const bool is_signed, ComparisonPredicate p)
        {
            if(is_signed)
                switch(p)
                {
                case EQ: return llvm::CmpInst::ICMP_EQ;
                case GT: return llvm::CmpInst::ICMP_SGT;
                case LT: return llvm::CmpInst::ICMP_SLT;
                case EQ_GT: return llvm::CmpInst::ICMP_SGE;
                case EQ_LT: return llvm::CmpInst::ICMP_SLE;
                case NE: return llvm::CmpInst::ICMP_NE;
                }
            else
                switch(p)
                {
                case EQ: return llvm::CmpInst::ICMP_EQ;
                case GT: return llvm::CmpInst::ICMP_UGT;
                case LT: return llvm::CmpInst::ICMP_ULT;
                case EQ_GT: return llvm::CmpInst::ICMP_UGE;
                case EQ_LT: return llvm::CmpInst::ICMP_ULE;
                case NE: return llvm::CmpInst::ICMP_NE;
                }
            return llvm::CmpInst::BAD_ICMP_PREDICATE;
        };
        if(left_type.is_signed_integral() && right_type.is_integral())
        {
            convertLiterals(&lhs, &rhs, left_type, right_type, irgen, this);
            pred = get_int_cmp_pred(true, p);
            return irgen->builder->CreateICmp(pred, lhs, rhs, "cmptmp");
        }
        if(left_type.is_unsigned_integral() && right_type.is_integral())
        {
            convertLiterals(&lhs, &rhs, left_type, right_type, irgen, this);
            pred = get_int_cmp_pred(false, p);
            return irgen->builder->CreateICmp(pred, lhs, rhs, "cmptmp");
        }
        if(left_type.is_integral() && right_type.is_integral())
        {
            convertLiterals(&lhs, &rhs, left_type, right_type, irgen, this);
            auto lhs_int = llvm::dyn_cast<llvm::ConstantInt>(lhs);
            auto rhs_int = llvm::dyn_cast<llvm::ConstantInt>(rhs);
            pred = get_int_cmp_pred(lhs_int->isNegative() || rhs_int->isNegative(), p);
            return irgen->builder->CreateICmp(pred, lhs, rhs, "divtmp");
        }
        if(left_type.is_floating_point() && (left_type.is_integral() || left_type.is_floating_point()))
        {
            convertLiterals(&lhs, &rhs, left_type, right_type, irgen, this);
            switch(p)
            {
            case EQ: pred = llvm::CmpInst::FCMP_OEQ; break;
            case GT: pred = llvm::CmpInst::FCMP_OGT; break;
            case LT: pred = llvm::CmpInst::FCMP_OLT; break;
            case EQ_GT: pred = llvm::CmpInst::FCMP_OGE; break;
            case EQ_LT: pred = llvm::CmpInst::FCMP_OLE; break;
            case NE: pred = llvm::CmpInst::FCMP_ONE; break;
            }
            return irgen->builder->CreateFCmp(pred, lhs, rhs, "cmptmp");
        }
        return nullptr;
    }


    llvm::Value* ExpressionEvaluator::LValueEvaluator::operator()(NameExpression*nm)
    {
        std::string name(nm->text);
        for(size_t i = irgen->variables.size(); i > 0; --i)
        {
            size_t idx = i - 1;
            if(auto var = irgen->variables[idx].find(name); var != irgen->variables[idx].end())
            {
                if(!std::get<1>(var->second).is_mutable) return nullptr;
                return std::get<0>(var->second);
            }
        }
    }

    llvm::Value* ExpressionEvaluator::LValueEvaluator::operator()(BinaryOperation* bop)
    {
        //Dot operations can also be lvalue'd
        if(bop->op.type != TokenType::Dot) return nullptr;
        auto left_t = std::visit(ExpressionTypeChecker{irgen}, bop->lhs->toVariant());
        if(!left_t) return nullptr;
        if(!left_t->is_lvalue && !left_t->is_reference()) return nullptr;
        return ExpressionEvaluator{irgen}.doDot(bop->lhs.get(), bop->rhs.get(), *left_t, false);
    }

    llvm::Value* ExpressionEvaluator::LValueEvaluator::operator()(PrefixOperation* op)
    {
        if(op->op.type == TokenType::Star)
        {
            return std::visit(ExpressionEvaluator{irgen}, op->operand->toVariant());
        }
    }

    llvm::Value* ExpressionEvaluator::LValueEvaluator::operator()(Expression* expr)
    {
        //TODO
    }

    llvm::Value* ExpressionEvaluator::LValueEvaluator::operator()(GroupingExpression* gre)
    {
        return std::visit(*this, gre->expr->toVariant());
    }


    llvm::Value* ExpressionEvaluator::operator()(IntegerLiteral* lit) {
        const auto ul = std::stoull(std::string{lit->text});
        return llvm::ConstantInt::get(llvm::Type::getInt64Ty(irgen->context), ul);
    }
    llvm::Value* ExpressionEvaluator::operator()(BooleanLiteral* lit)
    {
        if(lit->token.text == "true") return llvm::ConstantInt::getTrue(irgen->context);
        return llvm::ConstantInt::getFalse(irgen->context);
    }
    llvm::Value* ExpressionEvaluator::operator()(TupleLiteral* tup)
    {
        auto tuple_t = ExpressionTypeChecker{irgen, target}(tup);
        if(!tuple_t) { irgen->error(); return nullptr; }
        auto llvm_t = irgen->ToLLVMType(*tuple_t, false);
        auto tuple_tmp = irgen->Alloca("tuple_lit", llvm_t);
        auto zero_const = llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 0);
        size_t idx = 0;
        for(auto& expr: tup->elements)
        {
            auto idx_const = llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), idx);
            auto idx_ptr = irgen->builder->CreateGEP(llvm_t, tuple_tmp, {zero_const, idx_const}, "tuple_elem");
            auto type = std::visit(ExpressionTypeChecker{irgen}, expr->toVariant());
            tuple_t->subtypes[idx].is_mutable = true;
            doAssign(idx_ptr, std::visit(*this, expr->toVariant()), tuple_t->subtypes[idx], *type);
            idx++;
        }
        return tuple_tmp;
    }
    llvm::Value* ExpressionEvaluator::operator()(ArrayLiteral*) {}
    llvm::Value* ExpressionEvaluator::operator()(RealLiteral* lit)
    {
        return llvm::ConstantFP::get(llvm::Type::getDoubleTy(irgen->context), lit->token.text);
    }
    //str is of type {ptr, int64, int64} for buffer, size, capacity
    llvm::Value* ExpressionEvaluator::operator()(StringLiteral* lit)
    {
        ExpressionTypeChecker type_checker{irgen};
        if(!type_checker(lit)) {irgen->error(); return nullptr;}
        std::vector<std::pair<llvm::Value*, llvm::Value*>> substrings;
        for(auto& substr: lit->literal)
        {
            if(std::holds_alternative<std::string>(substr))
            {
                auto& as_str = std::get<std::string>(substr);
                auto gvar = irgen->builder->CreateGlobalString(as_str);

                auto str_size = llvm::ConstantInt::get(llvm::Type::getInt64Ty(irgen->context) ,as_str.size());
                substrings.emplace_back(irgen->Malloc("tmp_string_buffer", str_size), str_size);

                irgen->builder->CreateMemCpy(substrings.back().first, std::nullopt, gvar, std::nullopt, str_size);
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
        llvm::Value* final_buffer = irgen->Malloc("string_buffer", final_len);
        llvm::Value* offset = llvm::ConstantInt::get(llvm::Type::getInt64Ty(irgen->context), 0);
        for(auto& pair : substrings)
        {
            auto current_pointer = irgen->builder->CreateGEP(llvm::Type::getInt8Ty(irgen->context), final_buffer, {offset});
            irgen->builder->CreateMemCpy(current_pointer, std::nullopt, pair.first, std::nullopt, pair.second);
            irgen->Free(pair.first);
            offset = irgen->builder->CreateAdd(offset, pair.second);
        }
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
    llvm::Value* ExpressionEvaluator::operator()(NameExpression* nm)
    {
        std::string name(nm->text);
        auto ret_type = ExpressionTypeChecker{irgen}(nm);
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
        return nullptr;
    }

    llvm::Value* ExpressionEvaluator::operator()(GenericNameExpression* nm)
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

    llvm::Value* ExpressionEvaluator::operator()(PrefixOperation* op)
    {
        auto target = ExpressionTypeChecker{irgen}(op);
        if(!target) {irgen->error(); return nullptr;}

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
                if(!target->deref().is_trivially_destructible() && !target->deref().is_lvalue)
                {
                    std::string name = "__del_this" + std::string(operand->getName());
                    operand->setName(name);
                }
                return operand;
            }
        case TokenType::RefMut:
            {
                return std::visit(LValueEvaluator{irgen}, op->operand->toVariant());
            }
        }
    }
    llvm::Value* ExpressionEvaluator::operator()(BinaryOperation* op)
    {
        auto type_checker = ExpressionTypeChecker{irgen};
        auto res =  type_checker(op);
        if(!res) {irgen->error(); return nullptr;}

        auto l_as_var = op->lhs->toVariant();
        auto r_as_var = op->rhs->toVariant();

        llvm::Value* lhs;
        llvm::Value* rhs;
        if(op->op.type != TokenType::Dot && !op->op.is_assignment())
        {
            lhs = std::visit(*this, l_as_var);
            rhs = std::visit(*this, r_as_var);
        }
        auto left_t = std::visit(type_checker, l_as_var);
        auto right_t = std::visit(type_checker, r_as_var);

        switch(op->op.type)
        {
            using enum TokenType;
        case Plus: return doAddition(lhs, rhs, *left_t, *right_t);
        case Minus: return doMinus(lhs, rhs, *left_t, *right_t);
        case Star: return doMult(lhs, rhs, *left_t, *right_t);
        case Slash: return doDiv(lhs, rhs, *left_t, *right_t);
        case Percent: return doRem(lhs, rhs, *left_t, *right_t);
        case Greater: return doCmp(GT, lhs, rhs, *left_t, *right_t, *res);
        case Less: return doCmp(LT, lhs, rhs, *left_t, *right_t, *res);
        case GreaterEqual: return doCmp(EQ_GT, lhs, rhs, *left_t, *right_t, *res);
        case LessEqual: return doCmp(EQ_LT, lhs, rhs, *left_t, *right_t, *res);
        case BangEqual: return doCmp(NE, lhs, rhs, *left_t, *right_t, *res);
        case DoubleEqual: return doCmp(EQ, lhs, rhs, *left_t, *right_t, *res);
        case Dot: return doDot(op->lhs.get(), op->rhs.get(), *left_t);

        case Equal:
                return doAssign(std::visit(LValueEvaluator{irgen}, l_as_var), std::visit(*this, r_as_var), *left_t, *right_t);
        default:; //TODO
        }
        return nullptr;
    }
    llvm::Value* ExpressionEvaluator::operator()(GroupingExpression* op)
    {
        return std::visit(*this, op->expr->toVariant());
    }
    llvm::Value* ExpressionEvaluator::operator()(LogicalOperation*) {}
    llvm::Value* ExpressionEvaluator::operator()(PostfixOperation*) {}
    llvm::Value* ExpressionEvaluator::fillArgs(bool uses_sret,
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
            if(sig.parameters[i + is_bound].type.name == "__called_fn")
            {
                auto llvm_t = irgen->ToLLVMType(sig.parameters[i + is_bound].type, false);
                auto buffer = irgen->Alloca("called_fn_buffer", llvm_t);
                if(tp->is_function())
                {
                    irgen->builder->CreateStore(llvm::ConstantPointerNull::get(llvm::PointerType::get(irgen->context, 0)),
                        irgen->builder->CreateGEP(llvm_t, buffer, {
                            llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 0),
                            llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 0),
                        }));
                    irgen->builder->CreateStore(arg,
                        irgen->builder->CreateGEP(llvm_t, buffer, {
                            llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 0),
                            llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 1),
                        }));
                }
                if(tp->is_lambda())
                {
                    irgen->builder->CreateStore(arg,
                        irgen->builder->CreateGEP(llvm_t, buffer, {
                            llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 0),
                            llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 0),
                        }));
                    irgen->builder->CreateStore(irgen->code->getFunction(irgen->block_hash + tp->name),
                        irgen->builder->CreateGEP(llvm_t, buffer, {
                            llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 0),
                            llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 1),
                        }));
                }
                args[i + is_bound + uses_sret] = buffer;
            }
            else
            {
                args[i + is_bound + uses_sret] = implicitConvert(arg, *tp, sig.parameters[i + is_bound].type);
            }
        }
        return return_value;
    }

    llvm::Value* ExpressionEvaluator::doInvoke(CallOperation* op, const Type& left_t)
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
        auto args_w_lambda = args;
        args_w_lambda.push_back(ctx);
        auto return_val = fillArgs(uses_sret, *left_t.signature, args, nullptr, op->arguments);
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

    void ExpressionEvaluator::generateGenericFunction(Module* mod, const std::string& hash, GenericFunctionDeclaration* fn, std::span<Type> types)
    {
        for(auto& type: types) type.saturate(mod, irgen);
        std::string name = fn->name + IRGenerator::mangleGenericArgs(types);
        if(auto[_,exists] = mod->findFunction(hash, name); exists) return;
        auto module = irgen->module;
        irgen->module = mod;

        for(size_t i = 0; i < types.size(); i++)
            irgen->module->aliases[hash + name + "__"].emplace(fn->clause.types[i], types[i]);

        auto old_hash = irgen->reset_hash();
        irgen->block_hash = hash;
        std::string old_name = std::move(fn->name);
        fn->name = name;
        auto old_sig = fn->signature; //signature will be modified during saturation
        auto ptr = StatementTreeCloner{}(static_cast<FunctionDeclaration*>(fn));
        (*irgen)(reinterpret_cast<FunctionDeclaration*>(ptr.get()));
        fn->signature = std::move(old_sig);
        fn->name = std::move(old_name);
        irgen->block_hash = std::move(old_hash);
        irgen->module = module;
    }

    void ExpressionEvaluator::generateGenericAlias(Module* mod, const std::string& block, GenericAliasDeclaration* decl,
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
        auto new_statement = StatementTreeCloner{}(static_cast<AliasDeclaration*>(decl));
        auto alias = reinterpret_cast<AliasDeclaration*>(new_statement.get());
        alias->name = name;
        (*irgen)(alias);
        irgen->block_hash = std::move(old_hash);
        irgen->module = module;
    }

    llvm::Value* ExpressionEvaluator::operator()(CallOperation* op)
    {
        ExpressionTypeChecker type_checker{irgen};
        auto t = std::visit(type_checker, op->callee->toVariant());
        if(!t || !(t->is_function() || t->is_lambda())) {irgen->error(); return nullptr;}
        if(!type_checker(op))
            {irgen->error(); return nullptr;}
        bool is_lambda = t->is_lambda();
        auto fn = reinterpret_cast<FunctionType&>(*t);
        auto expr = dynamic_cast<BinaryOperation*>(op->callee.get());
        std::optional<FunctionType> left_ty;
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
            if(auto cls = left_t->get_decl_if_class())
            {
                //handle member functions
                if(auto rhs = dynamic_cast<NameExpression*>(expr->rhs.get()))
                {
                    std::string name(rhs->text);
                    if(auto var = std::ranges::find_if(cls->methods, [&name](ClassMethod& m)
                        {
                            return name == m.name;
                        }); var != cls->methods.end())
                    {
                        auto decl = reinterpret_cast<FunctionDeclaration*>(var->function_decl.get());
                        auto found = irgen->module->findType(irgen->block_hash, left_t->name);
                        auto function_name  = std::get<0>(*found) + name;
                        auto callee = irgen->code->getFunction(function_name);
                        bool uses_sret = callee->hasStructRetAttr();
                        std::vector<llvm::Value*> args(op->arguments.size() + 1 + uses_sret); // +1 because its bound
                        llvm::Value* return_value = fillArgs(uses_sret, decl->signature, args, std::visit(*this, expr->lhs->toVariant()), op->arguments);
                        auto call_val = irgen->builder->CreateCall(callee, args);
                        if(!uses_sret) return_value = call_val;
                        return return_value;
                    }
                }
            }
            auto right_t = std::visit(ExpressionTypeChecker{irgen}, expr->rhs->toVariant());
            auto* callee = is_lambda ?
                irgen->code->getFunction(irgen->block_hash + right_t->name) :
                llvm::dyn_cast_or_null<llvm::Function>(std::visit(*this, expr->rhs->toVariant()));
            if(!callee) { irgen->error(); return nullptr; }
            bool uses_sret = callee->hasStructRetAttr();
            std::vector<llvm::Value*> args(op->arguments.size() + 1 + uses_sret);
            llvm::Value* return_value = fillArgs(uses_sret, fn.sig, args, std::visit(*this, expr->lhs->toVariant()), op->arguments);
            if(is_lambda) args.push_back(std::visit(*this, expr->rhs->toVariant()));
            auto call_val = irgen->builder->CreateCall(callee->getFunctionType(), callee, args);
            if(!uses_sret) return_value = call_val;
            return return_value;
        }

        auto callee_val = std::visit(*this, op->callee->toVariant());
        auto* callee = is_lambda ?
            irgen->code->getFunction(irgen->block_hash + t->name) :
            llvm::dyn_cast_or_null<llvm::Function>(callee_val);
        if(!callee) { irgen->error(); return nullptr; }
        bool uses_sret = callee->hasStructRetAttr();
        std::vector<llvm::Value*> args(op->arguments.size() + uses_sret);
        llvm::Value* return_value = fillArgs(uses_sret, fn.sig, args, nullptr, op->arguments);
        if(is_lambda) args.push_back(callee_val);
        auto call_val = irgen->builder->CreateCall(callee->getFunctionType(), callee, args);
        if(!uses_sret) return_value = call_val;
        return return_value;
    }
    llvm::Value* ExpressionEvaluator::operator()(SubscriptOperation*) {}

    llvm::Value* ExpressionEvaluator::operator()(LambdaExpression* expr)
    {
        //TODO: check for capture/parameter duplicates
        if(expr->sig.returnType.name == "__inferred")
        {
            auto t = irgen->inferReturnType(expr->body.get());;
            expr->sig.returnType = *t;
        }
        std::vector<llvm::Type*> context_types;
        for(auto& capture : expr->captures)
        {
            Token tk{.type = TokenType::Identifier, .text = capture};
            NameExpression name(std::string(tk.text));
            auto type = ExpressionTypeChecker{irgen}(&name);
            if(type->is_function()) { irgen->error(); return nullptr; }
            llvm::Type* tp = irgen->ToLLVMType(*type, false);
            context_types.push_back(tp->getPointerTo());
        }
        auto context = llvm::StructType::get(irgen->context, context_types);
        auto ctx_object = irgen->Alloca("lambda_context", context);
        //copy types into the context
        size_t idx = 0;
        auto zero_const = llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 0);
        for(auto& capture : expr->captures)
        {
            auto idx_const = llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), idx);
            Token tk{.type = TokenType::Identifier, .text = capture};
            NameExpression name(std::string(tk.text));
            auto type = ExpressionTypeChecker{irgen}(&name);
            type->is_mutable = true;
            llvm::Value* val = LValueEvaluator{irgen}(&name);
            llvm::Value* lhs = irgen->builder->CreateGEP(context, ctx_object, {zero_const, idx_const});
            irgen->builder->CreateStore(val, lhs);
            idx++;
        }
        std::string name = "__lambda" + expr->hash;
        Token tk{.type = TokenType::Identifier, .text = name};
        irgen->lambdas[name] = {&expr->captures, context};
        irgen->lambdaSigs[name] = expr->sig;
        FunctionSignature sig = expr->sig;
        //insert a pointer to the context at the end of the signature
        sig.parameters.push_back(FunctionParameter{.type = Type{name}});
        FunctionDeclaration decl(std::string{tk.text},std::move(sig), std::move(expr->body));
        (*irgen)(&decl);
        return ctx_object;
    }

    llvm::Value* ExpressionEvaluator::operator()(ScopeOperation* op)
    {
        auto ty = ExpressionTypeChecker{irgen}(op);
        if(ty->is_function())
        {
            std::string mangled_name = ty->block_hash + UnsaturatedTypeIterator(op->type).last().name;
            auto fn = irgen->code->getFunction(mangled_name);
            if(!fn) fn = declareFunction(mangled_name, irgen, ty->sig);
            return fn;
        }
        return nullptr;
    }

    llvm::Value* ExpressionEvaluator::operator()(ObjectLiteral* lit)
    {
        auto t = ExpressionTypeChecker{irgen}(lit);
        if(!t) {irgen->error();return nullptr;}
        auto as_llvm_type = irgen->ToLLVMType(*t, false);
        auto decl = t->get_decl_if_class();
        auto value = irgen->Alloca("obj_lit",as_llvm_type);

        auto zero_const = llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), 0);
        for(size_t i = 0; i < decl->vars.size(); i++)
        {
            auto idx_const = llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), i);
            auto& var = decl->vars[i];
            auto val_ty = std::visit(ExpressionTypeChecker{irgen, var.type}, lit->values[var.name]->toVariant());
            auto val = std::visit(ExpressionEvaluator{irgen, var.type}, lit->values[var.name]->toVariant());

            auto mem_ptr = irgen->builder->CreateGEP(as_llvm_type, value, {zero_const, idx_const});
            auto as_mut = var.type;
            as_mut.is_mutable = true; as_mut.is_lvalue = true;
            doAssign(mem_ptr, val, as_mut, *val_ty);
        }
        return value;
    }

    llvm::Value* ExpressionEvaluator::operator()(NullLiteral*)
    {
        return nullptr;
    }

    llvm::Value* ExpressionEvaluator::operator()(CharLiteral* lit)
    {
        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(irgen->context), lit->value);
    }
}
