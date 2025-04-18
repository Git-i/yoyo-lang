#include <cmath>
#include <csignal>
#include <overload_resolve.h>
#include <ranges>
#include <set>
#include <tree_cloner.h>

#include "yvm/yvm_irgen.h"
#include "fn_type.h"
#include <format>
#include <yvm/yvm_module.h>
using Yvm::OpCode;
namespace Yoyo
{
    void clearUsages(std::vector<Type>& args, const YVMExpressionEvaluator& eval)
    {
        for(auto& value: args)
        {
            eval.irgen->builder->write_1b_inst(OpCode::Switch);
            eval.destroy(value);
        }
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

    void YVMExpressionEvaluator::implicitConvert(Expression* xp, const Type& src, const Type& dst, bool on_stack, bool do_load) const
    {
        if(dst.is_equal(src)) { return clone(xp, src, on_stack, do_load); }
        if(!dst.is_assignable_from(src, irgen)) { 
            irgen->error(Error(xp, std::format("Expression of type {} cannot be converted to type {}", src.pretty_name(irgen->block_hash), dst.pretty_name(irgen->block_hash)))); 
            return; 
        }
        if (src.is_error_ty() || dst.is_error_ty()) return;

        if(src.name == "ilit")
        {
            // There's no way to check the value of the constant till folding is implemented
            // in Yvm::Emitter
            /*int64_t min_bound = getIntMinOf(dst);
            uint64_t max_bound = getIntMaxOf(dst);*/
            if(dst.is_integral())
            {
                irgen->builder->write_const(0);
            }
            if(dst.is_floating_point())
            {
                irgen->builder->write_const(0.0);
            }
        }
        if(src.name == "flit")
        {
            double min_bound = getFloatMinOf(dst);
            double max_bound = getFloatMaxOf(dst);
            if(dst.is_floating_point())
            {
                if(*dst.float_width() != 64)
                    irgen->builder->write_3b_inst(OpCode::FpConv, 64, 32);
                if (do_load && on_stack) {
                    irgen->error(Error(xp, "Internal, error do_load and on_stack set"));
                }
                else if (on_stack) irgen->builder->write_2b_inst(OpCode::Store, irgen->toTypeEnum(dst));
                else if (!do_load && !on_stack) {
                    irgen->builder->write_alloca(*dst.float_width()/8);
                    irgen->builder->write_2b_inst(OpCode::Store, irgen->toTypeEnum(dst));
                }
                else if (do_load && !on_stack);
                return;
            }
        }
        if(dst.is_unsigned_integral())
        {
            //unsigned conversion only happen between unsigned vales
            irgen->builder->write_3b_inst(OpCode::UConv, *src.integer_width(), *dst.integer_width());
            if (do_load && on_stack) {
                irgen->error(Error(xp, "Internal, error do_load and on_stack set"));
            }
            else if (on_stack) irgen->builder->write_2b_inst(OpCode::Store, irgen->toTypeEnum(dst));
            else if (!do_load && !on_stack) {
                irgen->builder->write_alloca(*dst.integer_width() / 8);
                irgen->builder->write_2b_inst(OpCode::Store, irgen->toTypeEnum(dst));
            }
            else if (do_load && !on_stack);
            return;
        }
        if(dst.is_signed_integral())
        {
            if (src.is_signed_integral())
            {
                irgen->builder->write_3b_inst(OpCode::SConv, *src.integer_width(), *dst.integer_width());
                if (do_load && on_stack) {
                    irgen->error(Error(xp, "Internal, error do_load and on_stack set"));
                }
                else if (on_stack) irgen->builder->write_2b_inst(OpCode::Store, irgen->toTypeEnum(dst));
                else if (!do_load && !on_stack) {
                    irgen->builder->write_alloca(*dst.integer_width() / 8);
                    irgen->builder->write_2b_inst(OpCode::Store, irgen->toTypeEnum(dst));
                }
                else if (do_load && !on_stack);
                return;
            }
            else if (src.is_unsigned_integral())
            {
                irgen->builder->write_3b_inst(OpCode::UConv, *src.integer_width(), *dst.integer_width());
                if (do_load && on_stack) {
                    irgen->error(Error(xp, "Internal, error do_load and on_stack set"));
                }
                else if (on_stack) irgen->builder->write_2b_inst(OpCode::Store, irgen->toTypeEnum(dst));
                else if (!do_load && !on_stack) {
                    irgen->builder->write_alloca(*dst.integer_width() / 8);
                    irgen->builder->write_2b_inst(OpCode::Store, irgen->toTypeEnum(dst));
                }
                else if (do_load && !on_stack);
                return;
            }
        }
        if(dst.is_floating_point())
        {
            if (src.is_floating_point())
            {
                irgen->builder->write_3b_inst(OpCode::FpConv, *src.float_width(), *dst.float_width());
                if (do_load && on_stack) {
                    irgen->error(Error(xp, "Internal, error do_load and on_stack set"));
                }
                else if (on_stack) irgen->builder->write_2b_inst(OpCode::Store, irgen->toTypeEnum(dst));
                else if (!do_load && !on_stack) {
                    irgen->builder->write_alloca(*dst.float_width() / 8);
                    irgen->builder->write_2b_inst(OpCode::Store, irgen->toTypeEnum(dst));
                }
                else if (do_load && !on_stack);
            }
            else if (src.is_unsigned_integral())
            {

            }
            else if (src.is_signed_integral())
            {

            }
            return;
        }
        //gc borrows are checked at runtime
        //so gc -> ref conversion increments a counter
        if (src.is_gc_reference())
        {
            std::string error_str = std::format("Borrow check failed at {}:{}:{}", irgen->view->filename, xp->beg.line, xp->beg.column);
            //the borrow count is -1 if theres a living mutable borrow
            // we get the borrow count
            irgen->builder->write_ptr_off(NativeType::getElementOffset(reinterpret_cast<StructNativeTy*>(irgen->toNativeType(src)), 1));
            irgen->builder->write_1b_inst(OpCode::Dup); // keep a copy of the pointer to the count to update it
            irgen->builder->write_2b_inst(OpCode::Load, Yvm::Type::i64);
            std::string type_name = "__gc_refcell_borrow";
            //borrow count must be zero to borrow mutably
            auto valid_borrow = irgen->builder->unq_label_name("valid borrow");
            if (dst.is_mutable_reference()) {
                type_name += "_mut";
                irgen->builder->write_const(int64_t{ 0 });
                irgen->builder->write_1b_inst(OpCode::CmpNe);
                irgen->builder->create_jump(OpCode::JumpIfFalse, valid_borrow);
                irgen->builder->write_1b_inst(OpCode::Panic);
                irgen->builder->create_label(valid_borrow);
                irgen->builder->write_const(int64_t{ -1 });
                // store requires the pointer to be at the top of the stack
                irgen->builder->write_1b_inst(OpCode::Switch);
                irgen->builder->write_2b_inst(OpCode::Store, Yvm::Type::i64);
            }
            else {
                // since we're going to add one to the borrow value we can just duplicate it rather than load again
                irgen->builder->write_1b_inst(OpCode::Dup);
                irgen->builder->write_const(int64_t{ -1 });
                irgen->builder->write_1b_inst(OpCode::CmpEq);
                irgen->builder->create_jump(OpCode::JumpIfFalse, valid_borrow);
                irgen->builder->write_1b_inst(OpCode::Panic);
                irgen->builder->create_label(valid_borrow);
                irgen->builder->write_const(int64_t{ 1 });
                irgen->builder->write_1b_inst(OpCode::Add64);
                irgen->builder->write_1b_inst(OpCode::Switch);
                irgen->builder->write_2b_inst(OpCode::Store, Yvm::Type::i64);
            }
            // todo extended lifetimes
            return;
        }
        if(dst.is_reference())
        {
            if (do_load && on_stack) {
                irgen->error(Error(xp, "Internal, error do_load and on_stack set"));
            }
            else if (on_stack) irgen->builder->write_2b_inst(OpCode::Store, irgen->toTypeEnum(dst));
            else if (!do_load && !on_stack) {
                irgen->builder->write_alloca(sizeof(void*));
                irgen->builder->write_2b_inst(OpCode::Store, irgen->toTypeEnum(dst));
            }
            else if (do_load && !on_stack);
        }
        // from here on out we require a pointer to our out data
        auto native = irgen->toNativeType(dst);
        auto struct_native = reinterpret_cast<StructNativeTy*>(native);
        if (!on_stack) irgen->builder->write_alloca(NativeType::get_size(native));
        if(dst.is_optional())
        {
            //opt is {data, bool}
            //TODO destroy data if exists
            bool is_null = src.name == "__null";
            irgen->builder->write_1b_inst(OpCode::Dup);
            irgen->builder->write_ptr_off(NativeType::getElementOffset(struct_native, 1));
            irgen->builder->write_const(uint8_t{ is_null });
            irgen->builder->write_1b_inst(OpCode::Switch);
            irgen->builder->write_2b_inst(OpCode::Store, Yvm::Type::u8);
            if(is_null) return;

            //place a pointer to the data on the stack top and do implicit conversion
            irgen->builder->write_1b_inst(OpCode::Dup);
            irgen->builder->write_ptr_off(NativeType::getElementOffset(struct_native, 0));
            implicitConvert(xp, src, dst.subtypes[0], true, false);
            irgen->builder->write_1b_inst(OpCode::Pop);
            return;
        }
        if(dst.is_variant())
        {
            const std::set subtypes(dst.subtypes.begin(), dst.subtypes.end());
            uint32_t i = 0;

            // TODO: I should reall use `find` and `std::distance`, and do so in the llvm backend
            for(auto& sub : subtypes)
            {
                if(!sub.is_assignable_from(src, irgen)) { i++; continue; }
                irgen->builder->write_1b_inst(OpCode::Dup);
                irgen->builder->write_ptr_off(NativeType::getElementOffset(struct_native, 0));
                implicitConvert(xp, src, sub, true, false);
                irgen->builder->write_1b_inst(OpCode::Pop);
                irgen->builder->write_1b_inst(OpCode::Dup);
                irgen->builder->write_ptr_off(NativeType::getElementOffset(struct_native, 1));
                irgen->builder->write_const(i);
                irgen->builder->write_1b_inst(OpCode::Switch);
                irgen->builder->write_2b_inst(OpCode::Store, Yvm::Type::u32);
                return;
            }
        }
        if (dst.is_slice())
        {
            if (src.is_reference())
            {
                if (src.deref().is_static_array())
                {
                    // currently the stack looks like 
                    // [array, slice pointer]
                    // we can get the slice data pointer pointer and store array into it with reverse stack addressing
                    // dup, ptr_off => [array, slice pointer, slice data pointer pointer]
                    // rev_stack_addr 2 => [array, slice pointer, slice data pointer pointer, array]
                    // switch => [array, slice pointer, array, slice data pointer pointer]
                    // store ptr => [array, slice pointer]
                    // constant => [array, slice pointer, array size]
                    // dup, ptr_off => [array, slice pointer, array size, slice size pointer] 
                    // store u64 => [array, slice pointer]
                    // top_consume => [slice pointer]
                    irgen->builder->write_1b_inst(OpCode::Dup);
                    irgen->builder->write_ptr_off(NativeType::getElementOffset(struct_native, 0));
                    irgen->builder->write_2b_inst(OpCode::RevStackAddr, 2);
                    irgen->builder->write_1b_inst(OpCode::Switch);
                    irgen->builder->write_2b_inst(OpCode::Store, Yvm::Type::ptr);
                    irgen->builder->write_const<uint64_t>(src.deref().static_array_size());
                    irgen->builder->write_1b_inst(OpCode::Dup);
                    irgen->builder->write_ptr_off(NativeType::getElementOffset(struct_native, 1));
                    irgen->builder->write_2b_inst(OpCode::Store, Yvm::Type::u64);
                    irgen->builder->write_1b_inst(OpCode::TopConsume);
                    return;
                }
                
            }
        }
        if (dst.name == "__called_fn")
        {
            if (src.is_function())
            {
                /*irgen->builder->CreateStore(llvm::ConstantPointerNull::get(llvm::PointerType::get(irgen->context, 0)),
                    irgen->builder->CreateStructGEP(dst_as_llvm, out, 0));
                irgen->builder->CreateStore(val,
                    irgen->builder->CreateStructGEP(dst_as_llvm, out, 1));
                return out;*/
            }
            if (src.is_lambda())
            {
                /*irgen->builder->CreateStore(val,
                    irgen->builder->CreateStructGEP(dst_as_llvm, out, 0));
                irgen->builder->CreateStore(irgen->code->getFunction(src.module->module_hash + src.name),
                    irgen->builder->CreateStructGEP(dst_as_llvm, out, 1));
                return out;*/
            }
        }
        if (dst.is_view())
        {
            /*if (src.is_view())
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
                auto details = src.deref().module->findClass(src.deref().block_hash, src.deref().name).second;
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
            }*/
        }
    }

    void YVMExpressionEvaluator::clone(Expression* xp, const Type& left_type, bool on_stack, bool perform_load) const
    {
        if(!left_type.should_sret())
        {
            if (on_stack)
            {
                irgen->builder->write_1b_inst(OpCode::Switch);
                irgen->builder->write_2b_inst(OpCode::Store, irgen->toTypeEnum(left_type));
            }
        }
        auto native = irgen->toNativeType(left_type);
        auto struct_native = reinterpret_cast<StructNativeTy*>(native);
        if(!left_type.is_lvalue)
        {
            //move
            if(!on_stack) return;
            // TODO: memcpy and instrinsic system
        }
        if(!on_stack) irgen->builder->write_alloca(NativeType::get_size(native));
        if(left_type.is_tuple())
        {
            // for each element we:
            // [
            //      rev_stack_addr 1 => [src, dest, src]
            //      ptr_off i => [src, dest, src elem ptr]
            //      if(!should sret) load <type> => [src, dest, src elem]
            //      rev_stack_addr 1 => [src, dest, src elem, dest elem ptr]
            //      clone()
            // ]
            // top_consume
            size_t idx = 0;
            for(auto& sub : left_type.subtypes)
            {
                auto sub_yvm = irgen->toTypeEnum(sub);
                auto elem_off = NativeType::getElementOffset(struct_native, idx);
                irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1);
                irgen->builder->write_ptr_off(elem_off);
                if(!sub.should_sret()) irgen->builder->write_2b_inst(OpCode::Load, sub_yvm);
                irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1);
                clone(xp, sub, true, false);
                idx++;
            }
            irgen->builder->write_1b_inst(OpCode::TopConsume);
        }
        else if(left_type.is_optional())
        {
            //---------check if the optional has a value----------------------
            irgen->builder->write_1b_inst(OpCode::Switch);
            irgen->builder->write_1b_inst(OpCode::Dup);

            irgen->builder->write_ptr_off(NativeType::getElementOffset(struct_native, 1));
            irgen->builder->write_2b_inst(OpCode::Load, Yvm::Type::u8);
            irgen->builder->write_const(uint8_t{ 0 });
            irgen->builder->write_1b_inst(OpCode::CmpNe);
            auto assign_cont = irgen->builder->unq_label_name("opt_assign_cont");
            // if its not valid we skip the copy
            irgen->builder->create_jump(OpCode::JumpIfFalse, assign_cont);
            irgen->builder->write_1b_inst(OpCode::Dup);
            irgen->builder->write_ptr_off(NativeType::getElementOffset(struct_native, 0));
            if (!left_type.subtypes[0].should_sret()) 
                irgen->builder->write_2b_inst(OpCode::Load, irgen->toTypeEnum(left_type.subtypes[0]));
            irgen->builder->write_2b_inst(OpCode::RevStackAddr, 2);
            clone(xp, left_type.subtypes[0], true, false);
            irgen->builder->write_1b_inst(OpCode::Pop);
            irgen->builder->create_label(assign_cont);
            // duplicate the validity flag
            irgen->builder->write_ptr_off(NativeType::getElementOffset(struct_native, 0));
            irgen->builder->write_2b_inst(OpCode::Load, Yvm::Type::u8);
            irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1);
            irgen->builder->write_2b_inst(OpCode::Store, Yvm::Type::u8);
        }
        else if (left_type.is_str())
        {
            /*  No memory operations yet */
        }
        else if(left_type.is_variant())
        {
            // requires jump table implementation
        }
        //class types can define custom clone
        else if(auto decl_tup = left_type.module->findClass(left_type.block_hash, left_type.name).second)
        {
            auto decl = decl_tup->second.get();
            if(!decl->has_clone) {
                irgen->error(Error(xp, "Expression of type " + left_type.pretty_name(irgen->block_hash) + " cannot be cloned")); 
                return;
            }
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
                irgen->builder->write_1b_inst(OpCode::Dup); // dest for sret
                irgen->builder->write_2b_inst(OpCode::RevStackAddr, 2); // src
                irgen->builder->write_fn_addr(fn_name);
                irgen->builder->write_2b_inst(OpCode::Call, 2);
                irgen->builder->write_1b_inst(OpCode::Pop); // Pop the zero call returns
                irgen->builder->write_1b_inst(OpCode::TopConsume);
            }
            else
            {
                //if no custom clone is provided, use memberwise cloning
                size_t idx = 0;
                for(auto& var : decl->vars)
                {
                    auto sub_yvm = irgen->toTypeEnum(var.type);
                    auto elem_off = NativeType::getElementOffset(struct_native, idx);
                    irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1);
                    irgen->builder->write_ptr_off(elem_off);
                    if (!var.type.should_sret()) irgen->builder->write_2b_inst(OpCode::Load, sub_yvm);
                    irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1);
                    clone(xp, var.type, true, false);
                    idx++;
                }
            }

        }
    }
    void YVMExpressionEvaluator::destroy(const Type& type) const
    {
        if(type.is_trivially_destructible(irgen, false)) return;
        irgen->builder->write_fn_addr("__destructor_for" + type.full_name());
        irgen->builder->write_2b_inst(OpCode::Call, 2);
        irgen->builder->write_1b_inst(OpCode::Pop);
    }

    std::vector<Type> YVMExpressionEvaluator::doDot(Expression* lhs, Expression* rhs, const Type& left_type, bool load_primitive)
    {
        auto left_val = std::visit(*this, lhs->toVariant());
        if(left_type.deref().is_tuple())
        {
            if(auto idx = dynamic_cast<IntegerLiteral*>(rhs))
            {
                auto idx_int = std::stoul(std::string{idx->text});
                auto out_type = left_type.deref().subtypes[idx_int];

                auto native_t = reinterpret_cast<StructNativeTy*>(irgen->toNativeType(left_type.deref()));
                irgen->builder->write_ptr_off(NativeType::getElementOffset(native_t, idx_int));
                if (load_primitive && !out_type.should_sret())
                {
                    irgen->builder->write_2b_inst(OpCode::Load, irgen->toTypeEnum(out_type));
                    return {};
                }
                //no partial move allowed, so rvalue binary operations clone the field and destroy the object
                if (!left_type.is_lvalue)
                {
                    if(!left_type.is_trivially_destructible(irgen))
                    {
                        clone(lhs, left_type.subtypes[idx_int], false, true);
                        irgen->builder->write_2b_inst(OpCode::RevStackAddr, 2);
                        destroy(left_type);
                        return {};
                    }
                }
                return {};
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
                    auto native_t = reinterpret_cast<StructNativeTy*>(irgen->toNativeType(left_type.deref()));
                    irgen->builder->write_ptr_off(NativeType::getElementOffset(native_t, std::distance(cls->vars.begin(), var)));


                    if (load_primitive && !var->type.should_sret())
                    {
                        irgen->builder->write_2b_inst(OpCode::Load, irgen->toTypeEnum(var->type));
                        return {};
                    }
                    if (!left_type.is_lvalue)
                    {
                        if(!left_type.is_trivially_destructible(irgen))
                        {
                            clone(lhs, var->type, false, true);
                            destroy(left_type);
                            return {};
                        }
                    }
                    return {};
                }
            }
        }
        return {};
    }
    std::string getOperatorFunction(TokenType t, YVMIRGenerator* irgen, OverloadDetailsBinary* target)
    {
        return target->mangled_name(t);
    }
    std::vector<Type> YVMExpressionEvaluator::doAddition(
        Expression* lhs,
        Expression* rhs,
        const Type& left_type,
        const Type& right_type)
    {
        auto lhs_e = std::visit(*this, lhs->toVariant());
        auto rhs_e = std::visit(*this, rhs->toVariant());
        if (left_type.name == "ilit" && right_type.name == "ilit") {
            irgen->builder->write_1b_inst(OpCode::Add64);
            return {};
        }
        auto target = resolveAdd(left_type, right_type, irgen);
        auto fn = getOperatorFunction(TokenType::Plus, irgen, target);
        
        if (target->result.should_sret())
        {
            irgen->builder->write_alloca(NativeType::get_size(irgen->toNativeType(target->result)));
            irgen->builder->write_1b_inst(OpCode::Dup);
        }
        irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1 + rhs_e.size());
        implicitConvert(lhs, left_type, target->left, false, true);
        irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1);
        implicitConvert(rhs, right_type, target->right, false, true);
        irgen->builder->write_fn_addr(fn);
        irgen->builder->write_2b_inst(OpCode::Call, 2 + target->result.should_sret());
        if (target->result.should_sret()) irgen->builder->write_1b_inst(OpCode::Pop);
        if (target->result.is_non_owning(irgen))
        {
            // You can't extend lifetimes if you're an rvalue, I believe
            irgen->builder->write_1b_inst(OpCode::Switch);
            irgen->builder->write_1b_inst(OpCode::Pop);
            rhs_e.push_back(Type{ .name = "void" });
            rhs_e.insert(rhs_e.end(), lhs_e.begin(), lhs_e.end());
            return rhs_e;
        }
        else
        {
            irgen->builder->write_1b_inst(OpCode::Switch);
            irgen->builder->write_1b_inst(OpCode::Pop);
            clearUsages(rhs_e, *this);

            irgen->builder->write_1b_inst(OpCode::Pop);
            clearUsages(lhs_e, *this);
            return {};
        }
    }
    std::vector<Type> YVMExpressionEvaluator::doMinus(
        Expression* lhs,
        Expression* rhs,
        const Type& left_type,
        const Type& right_type)
    {
        auto lhs_e = std::visit(*this, lhs->toVariant());
        auto rhs_e = std::visit(*this, rhs->toVariant());
        if (left_type.name == "ilit" && right_type.name == "ilit") {
            irgen->builder->write_1b_inst(OpCode::Sub64);
            return {};
        }
        auto target = resolveAdd(left_type, right_type, irgen);
        auto fn = getOperatorFunction(TokenType::Minus, irgen, target);

        if (target->result.should_sret())
        {
            irgen->builder->write_alloca(NativeType::get_size(irgen->toNativeType(target->result)));
            irgen->builder->write_1b_inst(OpCode::Dup);
        }
        irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1 + rhs_e.size());
        implicitConvert(lhs, left_type, target->left, false, true);
        irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1);
        implicitConvert(rhs, right_type, target->right, false, true);
        irgen->builder->write_fn_addr(fn);
        irgen->builder->write_2b_inst(OpCode::Call, 2 + target->result.should_sret());
        if (target->result.should_sret()) irgen->builder->write_1b_inst(OpCode::Pop);
        if (target->result.is_non_owning(irgen))
        {
            // You can't extend lifetimes if you're an rvalue, I believe
            irgen->builder->write_1b_inst(OpCode::Switch);
            irgen->builder->write_1b_inst(OpCode::Pop);
            rhs_e.push_back(Type{ .name = "void" });
            rhs_e.insert(rhs_e.end(), lhs_e.begin(), lhs_e.end());
            return rhs_e;
        }
        else
        {
            irgen->builder->write_1b_inst(OpCode::Switch);
            irgen->builder->write_1b_inst(OpCode::Pop);
            clearUsages(rhs_e, *this);

            irgen->builder->write_1b_inst(OpCode::Pop);
            clearUsages(lhs_e, *this);
            return {};
        }
    }
    std::vector<Type> YVMExpressionEvaluator::doMult(
        Expression* lhs,
        Expression* rhs,
        const Type& left_type,
        const Type& right_type)
    {
        auto lhs_e = std::visit(*this, lhs->toVariant());
        auto rhs_e = std::visit(*this, rhs->toVariant());
        if (left_type.name == "ilit" && right_type.name == "ilit") {
            irgen->builder->write_1b_inst(OpCode::Mul64);
            return {};
        }
        auto target = resolveAdd(left_type, right_type, irgen);
        auto fn = getOperatorFunction(TokenType::Star, irgen, target);

        if (target->result.should_sret())
        {
            irgen->builder->write_alloca(NativeType::get_size(irgen->toNativeType(target->result)));
            irgen->builder->write_1b_inst(OpCode::Dup);
        }
        irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1 + rhs_e.size());
        implicitConvert(lhs, left_type, target->left, false, true);
        irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1);
        implicitConvert(rhs, right_type, target->right, false, true);
        irgen->builder->write_fn_addr(fn);
        irgen->builder->write_2b_inst(OpCode::Call, 2 + target->result.should_sret());
        if (target->result.should_sret()) irgen->builder->write_1b_inst(OpCode::Pop);
        if (target->result.is_non_owning(irgen))
        {
            // You can't extend lifetimes if you're an rvalue, I believe
            irgen->builder->write_1b_inst(OpCode::Switch);
            irgen->builder->write_1b_inst(OpCode::Pop);
            rhs_e.push_back(Type{ .name = "void" });
            rhs_e.insert(rhs_e.end(), lhs_e.begin(), lhs_e.end());
            return rhs_e;
        }
        else
        {
            irgen->builder->write_1b_inst(OpCode::Switch);
            irgen->builder->write_1b_inst(OpCode::Pop);
            clearUsages(rhs_e, *this);

            irgen->builder->write_1b_inst(OpCode::Pop);
            clearUsages(lhs_e, *this);
            return {};
        }
    }
    std::vector<Type> YVMExpressionEvaluator::doRange(
        Expression* lhs,
        Expression* rhs,
        const Type& left_type,
        const Type& right_type,
        const Type& result)
    {
        using namespace std::string_view_literals;
        auto subtype = Type{ .name = std::string(result.name.begin() + "range_"sv.size(), result.name.end()) };
        auto type = reinterpret_cast<StructNativeTy*>(irgen->toNativeType(subtype));
        irgen->builder->write_alloca(NativeType::get_size(type));
        irgen->builder->write_1b_inst(OpCode::Dup);
        irgen->builder->write_ptr_off(NativeType::getElementOffset(type, 0));
        std::visit(*this, lhs->toVariant());
        irgen->builder->write_1b_inst(OpCode::Switch);
        implicitConvert(lhs, left_type, subtype, true, false);
        irgen->builder->write_1b_inst(OpCode::Pop);
        irgen->builder->write_1b_inst(OpCode::Dup);
        irgen->builder->write_ptr_off(NativeType::getElementOffset(type, 1));
        std::visit(*this, rhs->toVariant());
        irgen->builder->write_1b_inst(OpCode::Switch);
        implicitConvert(rhs, right_type, subtype, true, false);
        return {};
    }
    std::vector<Type> YVMExpressionEvaluator::doDiv(
        Expression* lhs,
        Expression* rhs,
        const Type& left_type,
        const Type& right_type)
    {
        auto lhs_e = std::visit(*this, lhs->toVariant());
        auto rhs_e = std::visit(*this, rhs->toVariant());
        //if (left_type.name == "ilit" && right_type.name == "ilit") return irgen->builder->CreateSDiv(lhs_e, rhs_e);
        auto target = resolveDiv(left_type, right_type, irgen);
        auto fn = getOperatorFunction(TokenType::Slash, irgen, target);
        if (target->result.should_sret())
        {
            irgen->builder->write_alloca(NativeType::get_size(irgen->toNativeType(target->result)));
            irgen->builder->write_1b_inst(OpCode::Dup);
        }
        irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1 + rhs_e.size());
        implicitConvert(lhs, left_type, target->left, false, true);
        irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1);
        implicitConvert(rhs, right_type, target->right, false, true);
        irgen->builder->write_fn_addr(fn);
        irgen->builder->write_2b_inst(OpCode::Call, 2 + target->result.should_sret());
        if (target->result.should_sret()) irgen->builder->write_1b_inst(OpCode::Pop);
        if (target->result.is_non_owning(irgen))
        {
            // You can't extend lifetimes if you're an rvalue, I believe
            irgen->builder->write_1b_inst(OpCode::Switch);
            irgen->builder->write_1b_inst(OpCode::Pop);
            rhs_e.push_back(Type{ .name = "void" });
            rhs_e.insert(rhs_e.end(), lhs_e.begin(), lhs_e.end());
            return rhs_e;
        }
        else
        {
            irgen->builder->write_1b_inst(OpCode::Switch);
            irgen->builder->write_1b_inst(OpCode::Pop);
            clearUsages(rhs_e, *this);

            irgen->builder->write_1b_inst(OpCode::Pop);
            clearUsages(lhs_e, *this);
            return {};
        }
    }
    std::vector<Type> YVMExpressionEvaluator::doRem(
        Expression* lhs,
        Expression* rhs,
        const Type& left_type,
        const Type& right_type)
    {
        auto lhs_e = std::visit(*this, lhs->toVariant());
        auto rhs_e = std::visit(*this, rhs->toVariant());
        //if (left_type.name == "ilit" && right_type.name == "ilit") return irgen->builder->CreateSRem(lhs_e, rhs_e);
        auto target = resolveRem(left_type, right_type, irgen);
        auto fn = getOperatorFunction(TokenType::Percent, irgen, target);
        if (target->result.should_sret())
        {
            irgen->builder->write_alloca(NativeType::get_size(irgen->toNativeType(target->result)));
            irgen->builder->write_1b_inst(OpCode::Dup);
        }
        irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1 + rhs_e.size());
        implicitConvert(lhs, left_type, target->left, false, true);
        irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1);
        implicitConvert(rhs, right_type, target->right, false, true);
        irgen->builder->write_fn_addr(fn);
        irgen->builder->write_2b_inst(OpCode::Call, 2 + target->result.should_sret());
        if (target->result.should_sret()) irgen->builder->write_1b_inst(OpCode::Pop);
        if (target->result.is_non_owning(irgen))
        {
            // You can't extend lifetimes if you're an rvalue, I believe
            irgen->builder->write_1b_inst(OpCode::Switch);
            irgen->builder->write_1b_inst(OpCode::Pop);
            rhs_e.push_back(Type{ .name = "void" });
            rhs_e.insert(rhs_e.end(), lhs_e.begin(), lhs_e.end());
            return rhs_e;
        }
        else
        {
            irgen->builder->write_1b_inst(OpCode::Switch);
            irgen->builder->write_1b_inst(OpCode::Pop);
            clearUsages(rhs_e, *this);

            irgen->builder->write_1b_inst(OpCode::Pop);
            clearUsages(lhs_e, *this);
            return {};
        }
    }
    std::vector<Type> YVMExpressionEvaluator::doShl(Expression* lhs, Expression* rhs, const Type& left_type, const Type& right_type)
    {
        auto lhs_e = std::visit(*this, lhs->toVariant());
        auto rhs_e = std::visit(*this, rhs->toVariant());
        if (left_type.name == "ilit" && right_type.name == "ilit") { 
            irgen->builder->write_2b_inst(OpCode::Shl, 64); 
            return;
        }
        auto target = resolveShl(left_type, right_type, irgen);
        auto fn = getOperatorFunction(TokenType::DoubleLess, irgen, target);
        if (target->result.should_sret())
        {
            irgen->builder->write_alloca(NativeType::get_size(irgen->toNativeType(target->result)));
            irgen->builder->write_1b_inst(OpCode::Dup);
        }
        irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1 + rhs_e.size());
        implicitConvert(lhs, left_type, target->left, false, true);
        irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1);
        implicitConvert(rhs, right_type, target->right, false, true);
        irgen->builder->write_fn_addr(fn);
        irgen->builder->write_2b_inst(OpCode::Call, 2 + target->result.should_sret());
        if (target->result.should_sret()) irgen->builder->write_1b_inst(OpCode::Pop);
        if (target->result.is_non_owning(irgen))
        {
            // You can't extend lifetimes if you're an rvalue, I believe
            irgen->builder->write_1b_inst(OpCode::Switch);
            irgen->builder->write_1b_inst(OpCode::Pop);
            rhs_e.push_back(Type{ .name = "void" });
            rhs_e.insert(rhs_e.end(), lhs_e.begin(), lhs_e.end());
            return rhs_e;
        }
        else
        {
            irgen->builder->write_1b_inst(OpCode::Switch);
            irgen->builder->write_1b_inst(OpCode::Pop);
            clearUsages(rhs_e, *this);

            irgen->builder->write_1b_inst(OpCode::Pop);
            clearUsages(lhs_e, *this);
            return {};
        }
    }
    std::vector<Type> YVMExpressionEvaluator::doShr(Expression* lhs, Expression* rhs, const Type& left_type, const Type& right_type)
    {
        auto lhs_e = std::visit(*this, lhs->toVariant());
        auto rhs_e = std::visit(*this, rhs->toVariant());
        if (left_type.name == "ilit" && right_type.name == "ilit") {
            irgen->builder->write_2b_inst(OpCode::Shr, 64);
            return;
        }
        auto target = resolveShl(left_type, right_type, irgen);
        auto fn = getOperatorFunction(TokenType::DoubleGreater, irgen, target);
        if (target->result.should_sret())
        {
            irgen->builder->write_alloca(NativeType::get_size(irgen->toNativeType(target->result)));
            irgen->builder->write_1b_inst(OpCode::Dup);
        }
        irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1 + rhs_e.size());
        implicitConvert(lhs, left_type, target->left, false, true);
        irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1);
        implicitConvert(rhs, right_type, target->right, false, true);
        irgen->builder->write_fn_addr(fn);
        irgen->builder->write_2b_inst(OpCode::Call, 2 + target->result.should_sret());
        if (target->result.should_sret()) irgen->builder->write_1b_inst(OpCode::Pop);
        if (target->result.is_non_owning(irgen))
        {
            // You can't extend lifetimes if you're an rvalue, I believe
            irgen->builder->write_1b_inst(OpCode::Switch);
            irgen->builder->write_1b_inst(OpCode::Pop);
            rhs_e.push_back(Type{ .name = "void" });
            rhs_e.insert(rhs_e.end(), lhs_e.begin(), lhs_e.end());
            return rhs_e;
        }
        else
        {
            irgen->builder->write_1b_inst(OpCode::Switch);
            irgen->builder->write_1b_inst(OpCode::Pop);
            clearUsages(rhs_e, *this);

            irgen->builder->write_1b_inst(OpCode::Pop);
            clearUsages(lhs_e, *this);
            return {};
        }
    }
    std::vector<Type> YVMExpressionEvaluator::doCmp(
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
        auto lhs_e = std::visit(*this, lhs->toVariant());
        auto rhs_e = std::visit(*this, rhs->toVariant());
        // enum comparisons
        if ((p == EQ || p == NE) && left_type.is_equal(right_type)) {
            if (left_type.get_decl_if_enum()) {
                if (p == EQ) irgen->builder->write_2b_inst(OpCode::CmpEq, 32);
                else irgen->builder->write_2b_inst(OpCode::CmpNe, 32);
                return {};
            }
        }
        auto target = resolveCmp(left_type, right_type, irgen);
        auto fn = getOperatorFunction(TokenType::Spaceship, irgen, target);
        irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1 + rhs_e.size());
        implicitConvert(lhs, left_type, target->left, false, true);
        irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1);
        implicitConvert(rhs, right_type, target->right, false, true);
        irgen->builder->write_fn_addr(fn);
        irgen->builder->write_2b_inst(OpCode::Call, 2);
        if (p == EQ) {
            irgen->builder->write_const(eq); irgen->builder->write_2b_inst(OpCode::CmpEq, 32);
        }
        else if (p == GT) {
            irgen->builder->write_const(greater); irgen->builder->write_2b_inst(OpCode::CmpEq, 32);
        }
        else if (p == LT) {
            irgen->builder->write_const(less); irgen->builder->write_2b_inst(OpCode::CmpEq, 32);
        }
        else if (p == NE) {
            irgen->builder->write_const(eq); irgen->builder->write_2b_inst(OpCode::CmpNe, 32);
        }
        else if (p == EQ_GT)
        {
            irgen->builder->write_1b_inst(OpCode::Dup);
            irgen->builder->write_const(greater);
            irgen->builder->write_2b_inst(OpCode::CmpEq, 32);
            irgen->builder->write_1b_inst(OpCode::Switch);
            irgen->builder->write_const(eq);
            irgen->builder->write_2b_inst(OpCode::CmpEq, 32);
            irgen->builder->write_1b_inst(OpCode::Or);
        }
        else if (p == EQ_LT)
        {
            irgen->builder->write_1b_inst(OpCode::Dup);
            irgen->builder->write_const(less);
            irgen->builder->write_2b_inst(OpCode::CmpEq, 32);
            irgen->builder->write_1b_inst(OpCode::Switch);
            irgen->builder->write_const(eq);
            irgen->builder->write_2b_inst(OpCode::CmpEq, 32);
            irgen->builder->write_1b_inst(OpCode::Or);
        }
        return {};
    }
    std::vector<Type> YVMExpressionEvaluator::doUnionVar(CallOperation* op, Type& t)
    {
        using namespace std::string_view_literals;
        size_t dollar_off = t.name.find_first_of('$');
        std::string variant = std::string(t.name.begin() + dollar_off + 1, t.name.end());
        std::string type_name = std::string(t.name.begin() + "__union_var"sv.size(), t.name.begin() + dollar_off);
        t.name = type_name;
        auto as_native = reinterpret_cast<StructNativeTy*>(irgen->toNativeType(t));
        irgen->builder->write_alloca(NativeType::get_size(as_native));
        auto decl = t.get_decl_if_union();
        auto arg = std::visit(YVMExpressionEvaluator{ irgen, decl->fields.at(variant) }, op->arguments[0]->toVariant());
        auto arg_ty = std::visit(ExpressionTypeChecker{ irgen, decl->fields.at(variant) }, op->arguments[0]->toVariant()).value_or_error();
        irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1);
        irgen->builder->write_ptr_off(NativeType::getElementOffset(as_native, 0));
        implicitConvert(op->arguments[0].get(), arg_ty, decl->fields.at(variant), true, false);
        irgen->builder->write_1b_inst(OpCode::Dup);
        irgen->builder->write_ptr_off(NativeType::getElementOffset(as_native, 1));
        irgen->builder->write_const<uint32_t>(std::distance(decl->fields.begin(), decl->fields.find(variant)));
        irgen->builder->write_1b_inst(OpCode::Switch);
        irgen->builder->write_2b_inst(OpCode::Store, Yvm::Type::i32);
        return {};
    }

    std::vector<Type> YVMExpressionEvaluator::LValueEvaluator::operator()(NameExpression*nm)
    {
        std::string name(nm->text);
        for (size_t i = irgen->variables.size(); i > 0; --i)
        {
            size_t idx = i - 1;
            for (auto& var : irgen->variables[idx])
            {
                if (var.first != nm->text) continue;
                irgen->builder->write_2b_inst(OpCode::StackAddr, var.second.first);
                return {};
            }
        }
    }

    std::vector<Type> YVMExpressionEvaluator::LValueEvaluator::operator()(BinaryOperation* bop)
    {
        //Dot operations can also be lvalue'd
        if (bop->op.type != TokenType::Dot) {
            irgen->error(Error(bop, "Expression cannot evaluate to lvalue"));
            return {};
        }
        auto left_t = std::visit(ExpressionTypeChecker{irgen}, bop->lhs->toVariant());
        if (!left_t) {
            irgen->error(left_t.error());
            return {};
        }
        if (!left_t->is_mutable && !left_t->is_reference()) {
            irgen->error(Error(bop, "Expression cannot evaluate to lvalue"));
            return {};
        }
        return YVMExpressionEvaluator{irgen}.doDot(bop->lhs.get(), bop->rhs.get(), *left_t, false);

    }

    std::vector<Type> YVMExpressionEvaluator::LValueEvaluator::operator()(PrefixOperation* op)
    {
        if(op->op.type == TokenType::Star)
        {
            return std::visit(YVMExpressionEvaluator{irgen}, op->operand->toVariant());
        }
    }

    std::vector<Type> YVMExpressionEvaluator::LValueEvaluator::operator()(Expression* expr)
    {
        //TODO
        return {};
    }

    std::vector<Type> YVMExpressionEvaluator::LValueEvaluator::operator()(GroupingExpression* gre)
    {
        return std::visit(*this, gre->expr->toVariant());
    }


    std::vector<Type> YVMExpressionEvaluator::operator()(IntegerLiteral* lit) {
        const auto ul = std::stoull(std::string{lit->text});
        irgen->builder->write_const(ul);
        return {};
    }
    std::vector<Type> YVMExpressionEvaluator::operator()(BooleanLiteral* lit)
    {
        if (lit->token.text == "true") irgen->builder->write_const(uint8_t{ 1 });
        else irgen->builder->write_const(uint8_t{ 0 });
        return {};
    }
    std::vector<Type> YVMExpressionEvaluator::operator()(TupleLiteral* tup)
    {
        auto tuple_t = ExpressionTypeChecker{irgen, target}(tup);
        if (!tuple_t) { irgen->error(tuple_t.error()); return {}; }
        auto llvm_t = reinterpret_cast<StructNativeTy*>(irgen->toNativeType(*tuple_t));
        irgen->builder->write_alloca(NativeType::get_size(llvm_t));
        size_t idx = 0;
        for(auto& expr: tup->elements)
        {
            irgen->builder->write_1b_inst(OpCode::Dup);
            irgen->builder->write_ptr_off(NativeType::getElementOffset(llvm_t, idx));
            auto type = std::visit(ExpressionTypeChecker{irgen}, expr->toVariant());
            std::visit(*this, expr->toVariant());
            irgen->builder->write_1b_inst(OpCode::Switch);
            implicitConvert(expr.get(), *type, tuple_t->subtypes[idx], true, false);
            irgen->builder->write_1b_inst(OpCode::Pop);
            idx++;
        }
        return {};
    }
    std::vector<Type> YVMExpressionEvaluator::operator()(ArrayLiteral* lit)
    {
        using repeat_notation = std::pair<std::unique_ptr<Expression>, std::unique_ptr<Expression>>;
        using list_notation = std::vector<std::unique_ptr<Expression>>;
        auto tp_check = ExpressionTypeChecker{irgen};
        auto type = ExpressionTypeChecker{ irgen, target }(lit);
        if (!type) { irgen->error(type.error()); return {}; }
        auto as_native = reinterpret_cast<StructNativeTy*>(irgen->toNativeType(*type));
        irgen->builder->write_alloca(NativeType::get_size(as_native));
        if(type->is_static_array())
        {
            if (std::holds_alternative<repeat_notation>(lit->elements)) {
                auto& rpn = std::get<repeat_notation>(lit->elements);
                if (target && target->is_array())
                    std::visit(YVMExpressionEvaluator{ irgen, target->subtypes[0] }, rpn.first->toVariant());
                else
                    std::visit(*this, rpn.first->toVariant());
                for (size_t i = 0; i < type->static_array_size(); i++)
                {
                    irgen->builder->write_1b_inst(OpCode::Dup);
                    irgen->builder->write_2b_inst(OpCode::RevStackAddr, 2);
                    irgen->builder->write_ptr_off(NativeType::getElementOffset(as_native, i));
                    clone(rpn.first.get(), type->subtypes[0], true, false);
                    irgen->builder->write_1b_inst(OpCode::Pop);
                }
                return {};
            }
            auto& elements = std::get<list_notation>(lit->elements);
            for(size_t i = 0; i < elements.size(); ++i)
            {
                std::visit(*this, elements[i]->toVariant());
                irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1);
                auto tp = std::visit(tp_check, elements[i]->toVariant());
                if (!tp) {irgen->error(tp.error()); continue;}
                implicitConvert(elements[i].get(),
                    tp.value(),
                    type->subtypes[0],
                    true, false);
            }
        }
        return {};
    }
    std::vector<Type> YVMExpressionEvaluator::operator()(RealLiteral* lit)
    {
        double val = std::strtod(lit->token.text.data(), nullptr);
        irgen->builder->write_const(val);
        return {};
    }
    //str is of type {ptr, int64, int64} for buffer, size, capacity
    std::vector<Type> YVMExpressionEvaluator::operator()(StringLiteral* lit)
    {
        /* strings don't exist yet sadly */
    }
    std::vector<Type> YVMExpressionEvaluator::operator()(NameExpression* nm)
    {
        auto ret_type = ExpressionTypeChecker{irgen}(nm);
        if (!ret_type) { irgen->error(ret_type.error()); return {}; }
        for(size_t i = irgen->variables.size(); i > 0; --i)
        {
            size_t idx = i - 1;
            for(auto& var : irgen->variables[idx])
            {
                if (var.first != nm->text) continue;
                irgen->builder->write_2b_inst(OpCode::StackAddr, var.second.first);
                if(!var.second.second.should_sret())
                {
                    irgen->builder->write_2b_inst(OpCode::Load, irgen->toTypeEnum(var.second.second));
                }
                if(!ret_type->is_lvalue)
                {
                    irgen->builder->write_1b_inst(OpCode::PopReg);
                }
                return {};
            }
        }
        if(auto [name_prefix, fn] = irgen->module->findFunction(irgen->block_hash, nm->text); fn)
        {
            irgen->builder->write_fn_addr(name_prefix + nm->text);
            return {};
        }
        if (ret_type->is_error_ty()) return {};
        auto cnst = ConstantEvaluator{ irgen }(nm);
        if (ret_type->is_integral() || ret_type->is_char() || ret_type->is_boolean() || ret_type->get_decl_if_enum());
        //    return std::visit([as_llvm]<typename T>(T & val) {
        //    if constexpr (std::is_integral_v<T>) {
        //        return llvm::ConstantInt::get(as_llvm, val);
        //    }
        //    else return std::vector<Type>{};
        //}, cnst.internal_repr);
        else {
            irgen->builder->write_const(std::get<void*>(cnst.internal_repr));
            return {};
        }
    }

    std::vector<Type> YVMExpressionEvaluator::operator()(GenericNameExpression* nm)
    {
        if(auto[hash, fn] = irgen->module->findGenericFn(irgen->block_hash, nm->text); fn)
        {
            std::string mangled_name_suffix = nm->text + IRGenerator::mangleGenericArgs(nm->arguments);
            //it has not already been instantiated
            if(auto[name_prefx, ifn] = irgen->module->findFunction(irgen->block_hash, mangled_name_suffix); !ifn)
                irgen->generateGenericFunction(irgen->module, hash, fn, nm->arguments);
            irgen->builder->write_fn_addr(hash + mangled_name_suffix);
            return {};
        }
    }

    std::vector<Type> YVMExpressionEvaluator::operator()(PrefixOperation* op)
    {
        auto target = ExpressionTypeChecker{irgen}(op);
        if (!target) { irgen->error(target.error()); return {}; }

        switch(op->op.type)
        {
            //dereference
        case TokenType::Star:
            {
                std::visit(*this, op->operand->toVariant());
                if(!target->should_sret())
                    irgen->builder->write_2b_inst(OpCode::Load, irgen->toTypeEnum(*target));
                return {};
            }
        case TokenType::Ampersand:
            {
                auto operand = std::visit(*this, op->operand->toVariant());
                if(!target->subtypes[0].should_sret())
                {
                    irgen->builder->write_alloca(NativeType::get_size(irgen->toNativeType(target->subtypes[0])));
                    irgen->builder->write_1b_inst(OpCode::Switch);
                    irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1);
                    irgen->builder->write_2b_inst(OpCode::Store, irgen->toTypeEnum(target->subtypes[0]));
                    return {};
                }
                //taking the address of the rvalue causes the lifetime to be extended till the ref dies
                if(!target->deref().is_trivially_destructible(irgen) && !target->deref().is_lvalue)
                {
                }
                return operand;
            }
        case TokenType::RefMut:
            {
                return std::visit(LValueEvaluator{irgen}, op->operand->toVariant());
            }
        case TokenType::Bang:
        {
            std::visit(*this, op->operand->toVariant());
            irgen->builder->write_1b_inst(OpCode::Not);
        }
        }
    }
    std::vector<Type> YVMExpressionEvaluator::operator()(BinaryOperation* op)
    {
        auto type_checker = ExpressionTypeChecker{irgen};
        auto res =  type_checker(op);
        if (!res) { irgen->error(res.error()); return {}; }

        auto l_as_var = op->lhs->toVariant();
        auto r_as_var = op->rhs->toVariant();

        auto left_t = std::visit(type_checker, l_as_var);
        auto right_t = std::visit(type_checker, r_as_var);

        if (!left_t) { irgen->error(left_t.error()); return {}; }
        if(op->op.type != TokenType::Dot)
            if (!right_t) { irgen->error(right_t.error()); return {}; }

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
        case DoubleDotEqual: irgen->error(Error(op, "Not implemented yet")); return {};
        case DoubleDot: return doRange(lhs, rhs, *left_t, *right_t, *res);
        case Equal:
        {
            std::visit(*this, r_as_var);
            std::visit(LValueEvaluator{ irgen }, l_as_var);
            implicitConvert(rhs, *right_t, *left_t, true, false);
            return {};
        }
        default:; //TODO
        }
        return {};
    }
    std::vector<Type> YVMExpressionEvaluator::operator()(GroupingExpression* op)
    {
        return std::visit(*this, op->expr->toVariant());
    }

    std::vector<Type> YVMExpressionEvaluator::operator()(LogicalOperation* op) { 
        
    }
    std::vector<Type> YVMExpressionEvaluator::operator()(PostfixOperation*) { return {}; }
    void YVMExpressionEvaluator::fillArgs(bool uses_sret,
        const FunctionSignature& sig, const std::unique_ptr<Expression>& first_expr,
        std::vector<std::unique_ptr<Expression>>& exprs)
    {
        if(uses_sret)
        {
            irgen->builder->write_alloca(NativeType::get_size(irgen->toNativeType(sig.returnType)));
            irgen->builder->write_1b_inst(OpCode::Dup);
        }
        //if(first) args[uses_sret] = first;
        //bool is_bound = first != nullptr;
        //for(size_t i = 0; i < exprs.size(); i++)
        //{
        //    auto arg = std::visit(*this, exprs[i]->toVariant());
        //    auto tp = std::visit(ExpressionTypeChecker{irgen}, exprs[i]->toVariant());
        //    if (!tp) continue;
        //    
        //    args[i + is_bound + uses_sret] = implicitConvert(exprs[i].get(), arg, *tp, sig.parameters[i + is_bound].type);
        //    
        //}
        //return return_value;
    }

    std::vector<Type> YVMExpressionEvaluator::doInvoke(CallOperation* op, const Type& left_t)
    {
        return {};
    }

    

    
    std::vector<Type> YVMExpressionEvaluator::operator()(CallOperation* op)
    {
        ExpressionTypeChecker type_checker{irgen};
        //type checker is allowed to modify generic function nodes to do argument deduction
        auto return_t = type_checker(op);
        if (!return_t) { irgen->error(return_t.error()); return {}; }
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
        if (!t || !(t->is_function() || t->is_lambda())) { irgen->error(t.error()); return {}; }
        if(!return_t)
        {
            irgen->error(return_t.error()); return {};
        }
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
                    
                    bool uses_sret = fn->sig.returnType.should_sret();

                    auto first = std::visit(*this, expr->lhs->toVariant());
                    fillArgs(uses_sret, fn->sig, expr->lhs, op->arguments);
                    irgen->builder->write_fn_addr(function_name);
                    irgen->builder->write_2b_inst(OpCode::Call, op->arguments.size() + 1 + uses_sret);
                    if (uses_sret) irgen->builder->write_1b_inst(OpCode::Pop);
                    if (return_t->is_non_owning(irgen));
                    //stealUsages(args, return_value);
                    else;
                        //clearUsages(args, *this);
                    return {};
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
                    bool uses_sret = (*it)->signature.returnType.should_sret();
                    auto value = std::visit(*this, expr->lhs->toVariant());

                    //auto pointer_type = llvm::PointerType::get(irgen->context, 0);

                    //auto param = irgen->builder->CreateLoad(pointer_type,
                    //    irgen->builder->CreateStructGEP(as_llvm_type, value, 0));
                    //auto callee = irgen->builder->CreateLoad(pointer_type, 
                    //    irgen->builder->CreateStructGEP(as_llvm_type, value, 1 + std::distance(interface->methods.begin(), it)));

                    //llvm::Value* return_value = fillArgs(uses_sret, t->sig, args, param, op->arguments);
                    irgen->builder->write_2b_inst(OpCode::Call, op->arguments.size() + 1);
                    if (return_t->is_non_owning(irgen));
                    //stealUsages(args, return_value);
                    else;
                        //clearUsages(args, *this);
                    return {};
                }
            }
            auto right_t = std::visit(ExpressionTypeChecker{ irgen }, expr->rhs->toVariant());
            if (right_t->is_interface_function() && cls)
            {
                using namespace std::string_view_literals;
                auto pos = right_t->name.find_first_of('$');
                std::string name = right_t->name.substr("__interface_fn"sv.size(), pos - "__interface_fn"sv.size());
                std::string fn_name = right_t->name.substr(pos + 1);
                auto dets = irgen->module->findClass(left_t->deref().block_hash, left_t->deref().full_name_no_block()).second;
                fn_name = std::get<0>(*dets) + name + "::" + fn_name;
                
                bool uses_sret = t->sig.returnType.should_sret();

                fillArgs(uses_sret, t->sig, expr->lhs, op->arguments);
                irgen->builder->write_fn_addr(fn_name);
                irgen->builder->write_2b_inst(OpCode::Call, 2);
                if (uses_sret) irgen->builder->write_1b_inst(OpCode::Pop);
                if (return_t->is_non_owning(irgen));
                //stealUsages(args, return_value);
                else;
                    //clearUsages(args, *this);
                return {};
                
            }
            

            const auto* sig = is_lambda ? &reinterpret_cast<YVMModule*>(right_t->module)->lambdas[right_t->name].second->sig
                : &right_t->sig;
            bool uses_sret = sig->returnType.should_sret();

            fillArgs(uses_sret, fn.sig, expr->lhs, op->arguments);
            if (is_lambda) irgen->builder->write_fn_addr(irgen->block_hash + right_t->name);
            else std::visit(*this, expr->rhs->toVariant());
            //if(is_lambda) args.push_back(std::visit(*this, expr->rhs->toVariant()));
            irgen->builder->write_2b_inst(OpCode::Call, op->arguments.size() + uses_sret);
            if (uses_sret) irgen->builder->write_1b_inst(OpCode::Pop);
            if (return_t->is_non_owning(irgen));
            //stealUsages(args, return_value);
            else;
              //  clearUsages(args, *this);
            return {};
        }

        
        bool uses_sret = return_t->should_sret();
        fillArgs(uses_sret, fn.sig, {}, op->arguments);
        irgen->builder->write_fn_addr(irgen->block_hash + t->name);

        irgen->builder->write_2b_inst(OpCode::Call, op->arguments.size());
        if (uses_sret) irgen->builder->write_1b_inst(OpCode::Pop);
        if (return_t->is_non_owning(irgen));
        //stealUsages(args, return_value);
        else;
            //clearUsages(args, *this);
        return {};
    }
    std::vector<Type> YVMExpressionEvaluator::operator()(SubscriptOperation* op)
    {
        /*auto obj_ty = std::visit(ExpressionTypeChecker{irgen}, op->object->toVariant());
        if (!obj_ty) { irgen->error(obj_ty.error()); return {}; }
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
        }*/
    }
    std::vector<Type> YVMExpressionEvaluator::operator()(GCNewExpression* expr)
    {
        // TODO
    }
    std::vector<Type> YVMExpressionEvaluator::operator()(MacroInvocation* invc)
    {
        ExpressionTypeChecker{ irgen }(invc);
        return std::visit(*this, invc->result->toVariant());
    }
    std::vector<Type> YVMExpressionEvaluator::operator()(LambdaExpression* expr)
    {
        return {};
    }

    std::vector<Type> YVMExpressionEvaluator::operator()(ScopeOperation* op)
    {
        auto ty = ExpressionTypeChecker{irgen}(op);
        if (!ty) { irgen->error(ty.error()); return {}; }
        if(ty->is_function())
        {
            std::string mangled_name = ty->block_hash + UnsaturatedTypeIterator(op->type).last().name;
            irgen->builder->write_fn_addr(mangled_name);
            return {};
        }
        if (auto enm = ty->get_decl_if_enum())
        {
            auto name = UnsaturatedTypeIterator(op->type).last().name;
            irgen->builder->write_const(enm->values[name]);
        }
        if (ty->is_error_ty()) return {};
        // try to eval as const
        auto cnst = ConstantEvaluator{ irgen }(op);
        
        //auto as_llvm = irgen->ToLLVMType(*ty, false);
        //if (ty->is_integral() || ty->is_char() || ty->is_boolean() || ty->get_decl_if_enum())
        //    return std::visit([as_llvm]<typename T>(T & val) {
        //    if constexpr (std::is_integral_v<T>) return llvm::ConstantInt::get(as_llvm, val);
        //    else return static_cast<llvm::ConstantInt*>(nullptr);
        //}, cnst.internal_repr);
        //else return reinterpret_cast<llvm::Constant*>(std::get<void*>(cnst.internal_repr));
    }

    std::vector<Type> YVMExpressionEvaluator::operator()(ObjectLiteral* lit)
    {
        auto t = ExpressionTypeChecker{irgen}(lit);
        if (!t) { irgen->error(t.error());return {}; }
        auto as_native_type = reinterpret_cast<StructNativeTy*>(irgen->toNativeType(*t));
        auto decl = t->get_decl_if_class(irgen);
        irgen->builder->write_alloca(NativeType::get_size(as_native_type));

        for(size_t i = 0; i < decl->vars.size(); i++)
        {
            auto& var = decl->vars[i];
            auto val_ty = std::visit(ExpressionTypeChecker{irgen, var.type}, lit->values[var.name]->toVariant());
            if (!val_ty) { irgen->error(val_ty.error()); continue; }
            std::visit(YVMExpressionEvaluator{irgen, var.type}, lit->values[var.name]->toVariant());
            irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1);
            irgen->builder->write_ptr_off(NativeType::getElementOffset(as_native_type, i));
            implicitConvert(lit->values[var.name].get(), *val_ty, var.type, true, false);
            irgen->builder->write_1b_inst(OpCode::Pop);
        }
        return {};
    }

    std::vector<Type> YVMExpressionEvaluator::operator()(NullLiteral*)
    {
        return {};
    }

    std::vector<Type> YVMExpressionEvaluator::operator()(AsExpression* expr)
    {
        /*auto ty = std::visit(ExpressionTypeChecker{irgen}, expr->expr->toVariant());
        auto final_ty = std::visit(ExpressionTypeChecker{irgen}, expr->toVariant());
        if (!final_ty) { irgen->error(final_ty.error()); return {}; }
        if (!ty) { irgen->error(ty.error()); return {}; }
        auto as_native = reinterpret_cast<StructNativeTy*>(irgen->toNativeType(*final_ty));
        auto internal_as_native = reinterpret_cast<StructNativeTy*>(irgen->toNativeType(*ty));
        std::visit(*this, expr->expr->toVariant());
        if(ty->is_variant())
        {
            const std::set subtypes(ty->subtypes.begin(), ty->subtypes.end());
            irgen->builder->write_alloca(NativeType::get_size(as_native));
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
            implicitConvert(expr->expr.get(), *ty, *final_ty, false, false);
            return {};
        }
        if (final_ty->is_view())
        {
            auto& viewed = expr->dest.subtypes[0];
            auto [name, intf] = viewed.module->findInterface(viewed.block_hash, viewed.name);
            std::string full_name = name + intf->name;
            if (auto cls = ty->deref().get_decl_if_class(irgen))
            {
                auto details = ty->deref().module->findClass(ty->deref().block_hash, ty->deref().name).second;
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
        return nullptr;*/
        return {};
    }

    std::vector<Type> YVMExpressionEvaluator::operator()(CharLiteral* lit)
    {
        irgen->builder->write_const(lit->value);
        return {};
    }
}
