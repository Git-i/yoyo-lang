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

    void YVMExpressionEvaluator::implicitConvert(Expression* xp, const Type& src, const Type& dst, bool on_stack, bool do_load)
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
        if (src.is_gc_reference() && dst.is_reference())
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
                irgen->builder->write_2b_inst(OpCode::CmpNe, 64);
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
                irgen->builder->write_2b_inst(OpCode::CmpEq, 64);
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
        auto ret_alloc = 0;
        if (!on_stack) {
            irgen->builder->write_alloca(NativeType::get_size(native)); ret_alloc = irgen->builder->last_alloc_addr();
        }
        if(dst.is_optional())
        {
            //opt is {data, bool}
            //TODO destroy data if exists
            bool is_null = src.name == "__null";
            irgen->builder->write_1b_inst(OpCode::Dup);
            irgen->builder->write_ptr_off(NativeType::getElementOffset(struct_native, 1));
            irgen->builder->write_const(uint8_t{ !is_null });
            irgen->builder->write_1b_inst(OpCode::Switch);
            irgen->builder->write_2b_inst(OpCode::Store, Yvm::Type::u8);
            if (is_null) {
                // null literals push a rubbish value onto the stack
                irgen->builder->write_1b_inst(OpCode::TopConsume);
                return;
            }

            //place a pointer to the data on the stack top and do implicit conversion
            auto data_off = NativeType::getElementOffset(struct_native, 0);
            if (data_off == 0) {
                implicitConvert(xp, src, dst.subtypes[0], true, false);
                returned_alloc_addr = ret_alloc;
                return;
            }
            debugbreak();
            returned_alloc_addr = ret_alloc;
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
                returned_alloc_addr = ret_alloc;
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
                    returned_alloc_addr = ret_alloc;
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

    void YVMExpressionEvaluator::clone(Expression* xp, const Type& left_type, bool on_stack, bool perform_load)
    {
        if(!left_type.should_sret())
        {
            if (on_stack)
            {
                irgen->builder->write_1b_inst(OpCode::Switch);
                irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1);
                irgen->builder->write_2b_inst(OpCode::Store, irgen->toTypeEnum(left_type));
                return;
            }
            else {
                if (perform_load) return;
                auto as_native = irgen->toNativeType(left_type);
                irgen->builder->write_alloca(NativeType::get_size(as_native));
                irgen->builder->write_1b_inst(OpCode::Switch);
                irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1);
                irgen->builder->write_2b_inst(OpCode::Store, irgen->toTypeEnum(left_type));
            }
            return;
        }
        auto native = irgen->toNativeType(left_type);
        auto struct_native = reinterpret_cast<StructNativeTy*>(native);
        auto ret_alloc = 0;
        if(!left_type.is_lvalue)
        {
            //move
            if(!on_stack) 
                return;
            // [ rvalue, memory ]
            
            irgen->builder->write_const<uint64_t>(NativeType::get_size(native));
            irgen->builder->write_2b_inst(OpCode::RevStackAddr, 2); // get the source object
            irgen->builder->write_2b_inst(OpCode::RevStackAddr, 2); // get the memory pointer
            irgen->builder->write_1b_inst(OpCode::MemCpy);
            irgen->builder->write_1b_inst(OpCode::TopConsume);
            returned_alloc_addr = ret_alloc;
            return;
        }
        if (!on_stack) {
            irgen->builder->write_alloca(NativeType::get_size(native)); ret_alloc = irgen->builder->last_alloc_addr();
        }
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
            irgen->builder->write_2b_inst(OpCode::CmpNe, 8);
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
            irgen->builder->write_ptr_off(NativeType::getElementOffset(struct_native, 1));
            irgen->builder->write_2b_inst(OpCode::Load, Yvm::Type::u8);
            irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1);
            irgen->builder->write_ptr_off(NativeType::getElementOffset(struct_native, 1));
            irgen->builder->write_2b_inst(OpCode::Store, Yvm::Type::u8);
        }
        else if (left_type.is_str())
        {
            irgen->builder->write_1b_inst(OpCode::TopConsume);
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
        returned_alloc_addr = ret_alloc;
    }
    void YVMExpressionEvaluator::destroy(const Type& type) const
    {
        if (type.is_trivially_destructible(irgen, false)) {
            irgen->builder->write_1b_inst(OpCode::Pop);
            return;
        }
        irgen->builder->write_fn_addr("__destructor_for_" + type.full_name());
        irgen->builder->write_2b_inst(OpCode::Call, 1);
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
                    if (!left_type.deref().is_lvalue)
                    {
                        if(!left_type.deref().is_trivially_destructible(irgen))
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
    std::vector<Type> doBasicBinaryOp(
        YVMExpressionEvaluator* eval,
        OverloadDetailsBinary* target_ovl,
        TokenType toktp,
        Expression* lhs,
        Expression* rhs,
        const Type& left_type_og,
        const Type& right_type_og,
        std::string block_hash)
    {
        auto left_type = *std::visit(ExpressionTypeChecker{ eval->irgen, target_ovl->left }, lhs->toVariant());
        eval->target = target_ovl->left;
        auto lhs_e = toktp == TokenType::SquarePairMut ?
            std::visit(YVMExpressionEvaluator::LValueEvaluator{eval->irgen}, lhs->toVariant()) :
            std::visit(*eval, lhs->toVariant());
        if (toktp == TokenType::SquarePair) {
            eval->to_reference(left_type);
            left_type = left_type.reference_to();
        }
        else if (toktp == TokenType::SquarePairMut) {
            left_type = left_type.mutable_reference_to();
        }
        eval->implicitConvert(lhs, left_type, target_ovl->left, false, true);

        auto right_type = *std::visit(ExpressionTypeChecker{ eval->irgen, target_ovl->right }, rhs->toVariant());
        eval->target = target_ovl->right;
        auto rhs_e = std::visit(*eval, rhs->toVariant());
        eval->implicitConvert(rhs, right_type, target_ovl->right, false, true);

        auto fn = block_hash + getOperatorFunction(toktp, eval->irgen, target_ovl);

        if (target_ovl->result.should_sret())
        {
            eval->irgen->builder->write_alloca(NativeType::get_size(eval->irgen->toNativeType(target_ovl->result)));
            eval->irgen->builder->write_1b_inst(OpCode::Dup);
        }
        if (!rhs_e.empty()) {
            eval->irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1 + rhs_e.size());
            eval->irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1);
        }

        eval->irgen->builder->write_fn_addr(fn);
        eval->irgen->builder->write_2b_inst(OpCode::Call, 2 + target_ovl->result.should_sret());
        if (target_ovl->result.should_sret()) eval->irgen->builder->write_1b_inst(OpCode::Pop);
        if (target_ovl->result.is_non_owning(eval->irgen))
        {
            // You can't extend lifetimes if you're an rvalue, I believe
            eval->irgen->builder->write_1b_inst(OpCode::Switch);
            eval->irgen->builder->write_1b_inst(OpCode::Pop);
            rhs_e.push_back(Type{ .name = "void" });
            rhs_e.insert(rhs_e.end(), lhs_e.begin(), lhs_e.end());
            return rhs_e;
        }
        else
        {
            if (!rhs_e.empty()) {
                eval->irgen->builder->write_1b_inst(OpCode::Switch);
                eval->irgen->builder->write_1b_inst(OpCode::Pop);
                clearUsages(rhs_e, *eval);
                eval->irgen->builder->write_1b_inst(OpCode::Pop);
                clearUsages(lhs_e, *eval);
            }
            return {};
        }
    }
    std::vector<Type> YVMExpressionEvaluator::doAddition(
        Expression* lhs,
        Expression* rhs,
        const Type& left_type_og,
        const Type& right_type_og)
    {
        if (left_type_og.name == "ilit" && right_type_og.name == "ilit") {
            irgen->builder->write_1b_inst(OpCode::Add64);
            return {};
        }
        auto [block, target_ovl] = resolveAdd(left_type_og, right_type_og, irgen);
        return doBasicBinaryOp(this, target_ovl, TokenType::Plus, lhs, rhs, left_type_og, right_type_og, block);
    }
    std::vector<Type> YVMExpressionEvaluator::doMinus(
        Expression* lhs,
        Expression* rhs,
        const Type& left_type,
        const Type& right_type)
    {
        if (left_type.name == "ilit" && right_type.name == "ilit") {
            irgen->builder->write_1b_inst(OpCode::Sub64);
            return {};
        }
        auto [block,target] = resolveSub(left_type, right_type, irgen);
        return doBasicBinaryOp(this, target, TokenType::Minus, lhs, rhs, left_type, right_type, block);
    }
    std::vector<Type> YVMExpressionEvaluator::doMult(
        Expression* lhs,
        Expression* rhs,
        const Type& left_type,
        const Type& right_type)
    {
        if (left_type.name == "ilit" && right_type.name == "ilit") {
            irgen->builder->write_1b_inst(OpCode::Mul64);
            return {};
        }
        auto [block, target] = resolveMul(left_type, right_type, irgen);
        return doBasicBinaryOp(this, target, TokenType::Star, lhs, rhs, left_type, right_type, block);
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
        auto [block, target] = resolveDiv(left_type, right_type, irgen);
        return doBasicBinaryOp(this, target, TokenType::Slash, lhs, rhs, left_type, right_type, block);
    }
    std::vector<Type> YVMExpressionEvaluator::doRem(
        Expression* lhs,
        Expression* rhs,
        const Type& left_type,
        const Type& right_type)
    {
        auto [block, target] = resolveRem(left_type, right_type, irgen);
        return doBasicBinaryOp(this, target, TokenType::Percent, lhs, rhs, left_type, right_type, block);
    }
    std::vector<Type> YVMExpressionEvaluator::doShl(Expression* lhs, Expression* rhs, const Type& left_type, const Type& right_type)
    {
        if (left_type.name == "ilit" && right_type.name == "ilit") { 
            irgen->builder->write_2b_inst(OpCode::Shl, 64); 
            return {};
        }
        auto [block, target] = resolveShl(left_type, right_type, irgen);
        return doBasicBinaryOp(this, target, TokenType::DoubleLess, lhs, rhs, left_type, right_type, block);
    }
    std::vector<Type> YVMExpressionEvaluator::doShr(Expression* lhs, Expression* rhs, const Type& left_type, const Type& right_type)
    {
        if (left_type.name == "ilit" && right_type.name == "ilit") {
            irgen->builder->write_2b_inst(OpCode::Shr, 64);
            return {};
        }
        auto [block, target] = resolveShr(left_type, right_type, irgen);
        return doBasicBinaryOp(this, target, TokenType::DoubleGreater, lhs, rhs, left_type, right_type, block);
    }
    std::vector<Type> YVMExpressionEvaluator::doCmp(
        ComparisonPredicate p,
        Expression* lhs,
        Expression* rhs,
        const Type& left_type_og,
        const Type& right_type_og,
        const Type& result_type)
    {
        constexpr int32_t eq = 1;
        constexpr int32_t ne = 0;
        constexpr int32_t less = 2;
        constexpr int32_t greater = 3;
        constexpr int32_t unord = 4;
        // enum comparisons
        if ((p == EQ || p == NE) && left_type_og.is_equal(right_type_og)) {
            if (left_type_og.get_decl_if_enum()) {
                std::visit(*this, lhs->toVariant());
                std::visit(*this, rhs->toVariant());
                if (p == EQ) irgen->builder->write_2b_inst(OpCode::CmpEq, 32);
                else irgen->builder->write_2b_inst(OpCode::CmpNe, 32);
                return {};
            }
        }
        auto [block, target_ovl] = resolveCmp(left_type_og, right_type_og, irgen);
        auto left_type = *std::visit(ExpressionTypeChecker{ irgen, target_ovl->left }, lhs->toVariant());
        target = target_ovl->left;
        auto lhs_e = std::visit(*this, lhs->toVariant());
        implicitConvert(lhs, left_type, target_ovl->left, false, true);

        auto right_type = *std::visit(ExpressionTypeChecker{ irgen, target_ovl->right }, rhs->toVariant());
        target = target_ovl->right;
        auto rhs_e = std::visit(*this, rhs->toVariant());
        implicitConvert(rhs, right_type, target_ovl->right, false, true);
        
        auto fn = getOperatorFunction(TokenType::Spaceship, irgen, target_ovl);
        if (!rhs_e.empty()) {
            irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1 + rhs_e.size());
            irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1);
        }
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
    std::vector<Type> YVMExpressionEvaluator::doSingleStringLiteral(const std::string& text, StructNativeTy* str_type)
    {
        irgen->builder->write_alloca(NativeType::get_size(str_type));
        irgen->builder->write_const<uint64_t>(text.size()); // capacity to store in the string
        irgen->builder->write_1b_inst(OpCode::Dup); // size to store in the string
        irgen->builder->write_1b_inst(OpCode::Dup); // size for the memcpy
        irgen->builder->write_1b_inst(OpCode::Dup); // size for the malloc
        irgen->builder->write_1b_inst(OpCode::Malloc);
        irgen->builder->write_1b_inst(OpCode::Switch);
        irgen->builder->write_const_string(irgen->builder->create_const_string(text, &reinterpret_cast<YVMEngine*>(irgen->module->engine)->vm));
        irgen->builder->write_2b_inst(OpCode::RevStackAddr, 2);
        irgen->builder->write_1b_inst(OpCode::MemCpy);
        // bring the obj and write the values into it, its at addr 3 because we have the pointer and 2 numbers on stack
        irgen->builder->write_2b_inst(OpCode::RevStackAddr, 3); 
        irgen->builder->write_ptr_off(NativeType::getElementOffset(str_type, 0));
        irgen->builder->write_2b_inst(OpCode::Store, Yvm::Type::ptr);

        irgen->builder->write_2b_inst(OpCode::RevStackAddr, 2);
        irgen->builder->write_ptr_off(NativeType::getElementOffset(str_type, 1));
        irgen->builder->write_2b_inst(OpCode::Store, Yvm::Type::u64);

        irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1);
        irgen->builder->write_ptr_off(NativeType::getElementOffset(str_type, 2));
        irgen->builder->write_2b_inst(OpCode::Store, Yvm::Type::u64);
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
        auto alloc_addr = irgen->builder->last_alloc_addr();
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
        irgen->builder->write_2b_inst(OpCode::Store, Yvm::Type::u32);
        returned_alloc_addr = alloc_addr;
        return {};
    }

    void YVMExpressionEvaluator::to_reference(const Type& type)
    {
        if (type.is_reference()) return;
        if (type.should_sret()) return;

        // for non sret types
        // we have to allocate memory and copy it there
        irgen->builder->write_alloca(NativeType::get_size(irgen->toNativeType(type)));
        irgen->builder->write_1b_inst(OpCode::Switch);
        irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1);
        irgen->builder->write_2b_inst(OpCode::Store, irgen->toTypeEnum(type));
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
                if(var.second.first.type == YVMIRGenerator::VariableIndex::Checkpoint)
                    irgen->builder->write_2b_inst(OpCode::StackCheckpoint, var.second.first.index);
                else 
                    irgen->builder->write_2b_inst(OpCode::StackAddr, var.second.first.index);
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
        if (target && target->is_integral()) {
            auto width = *target->integer_width();
            target = std::nullopt;
            switch (width) {
            case 8: irgen->builder->write_const(static_cast<uint8_t>(ul)); break;
            case 16: irgen->builder->write_const(static_cast<uint16_t>(ul)); break;
            case 32: irgen->builder->write_const(static_cast<uint32_t>(ul)); break;
            case 64: irgen->builder->write_const(static_cast<uint64_t>(ul)); break;
            }
            return {};
        }
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
        auto type = ExpressionTypeChecker{ irgen, target }(lit);
        auto tp_check = (target && target->is_array()) ? ExpressionTypeChecker{irgen, target->subtypes[0]}
        : ExpressionTypeChecker{ irgen };
        if (!type) { irgen->error(type.error()); return {}; }
        auto as_native = reinterpret_cast<ArrayNativeTy*>(irgen->toNativeType(*type));
        irgen->builder->write_alloca(NativeType::get_size(as_native));
        size_t ret_addr = irgen->builder->last_alloc_addr();
        if(type->is_static_array())
        {
            if (std::holds_alternative<repeat_notation>(lit->elements)) {
                auto& rpn = std::get<repeat_notation>(lit->elements);
                if (target && target->is_array())
                    std::visit(YVMExpressionEvaluator{ irgen, type->subtypes[0] }, rpn.first->toVariant());
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
                returned_alloc_addr = ret_addr;
                return {};
            }
            auto& elements = std::get<list_notation>(lit->elements);
            *target = type->subtypes[0];
            for(size_t i = 0; i < elements.size(); ++i)
            {
                std::visit(*this, elements[i]->toVariant());
                irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1);
                irgen->builder->write_ptr_off(NativeType::getElementOffset(as_native, i));
                auto tp = std::visit(tp_check, elements[i]->toVariant());
                if (!tp) {irgen->error(tp.error()); continue;}
                implicitConvert(elements[i].get(),
                    tp.value(),
                    type->subtypes[0],
                    true, false);
                irgen->builder->write_1b_inst(OpCode::Pop);
            }
        }
        returned_alloc_addr = ret_addr;
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
        ExpressionTypeChecker type_checker{ irgen };
        auto tp = type_checker(lit);
        if (!tp) { irgen->error(tp.error()); return {}; }
        auto str_type = reinterpret_cast<StructNativeTy*>(irgen->toNativeType(*tp));
        // creating a string is way too many instructions for a fairly common task
        // so we have this special optimized path for the common case of a string without interpolation
        if (lit->literal.size() == 1 && std::holds_alternative<std::string>(lit->literal[0]))
            return doSingleStringLiteral(std::get<std::string>(lit->literal[0]), str_type);

        irgen->builder->write_alloca(NativeType::get_size(str_type));
        // for each string segment we write:
        // - the pointer
        // - the size
        // - the offsets which they must be copied to
        // we can get the final allocation size by adding the values at the stack top
        // we can get the offset by adding the previous offset to the previous size
        irgen->builder->write_const(uint64_t{ 0 });
        irgen->builder->write_1b_inst(OpCode::Dup);
        std::vector<bool> should_free_vec;
        for (auto& substr : lit->literal) {
            if (std::holds_alternative<std::string>(substr)) {
                auto str = irgen->builder->create_const_string(std::get<std::string>(substr), &reinterpret_cast<YVMEngine*>(irgen->module->engine)->vm);
                irgen->builder->write_const_string(str);
                irgen->builder->write_const(std::get<std::string>(substr).size());
                irgen->builder->write_2b_inst(OpCode::RevStackAddr, 2);
                irgen->builder->write_2b_inst(OpCode::RevStackAddr, 4);
                irgen->builder->write_1b_inst(OpCode::Add64);
                should_free_vec.push_back(false);
            }
            else {
                auto& expr = std::get<std::unique_ptr<Expression>>(substr);
                auto expr_tp = std::visit(ExpressionTypeChecker{ irgen }, expr->toVariant());
                if (!expr_tp) { irgen->error(expr_tp.error()); return {}; }
                if (expr_tp->is_integral() || expr_tp->is_floating_point()) {
                    std::visit(*this, expr->toVariant());
                    uint8_t intrinsic_idx = 0;
                    // we can probably do something fancy like
                    // checking the first character for i u or f and then using adding
                    // log2 of integer width - 3, we could even use countl_zero over log
                    if (expr_tp->name == "i8") intrinsic_idx = 1;
                    else if (expr_tp->name == "i16") intrinsic_idx = 2;
                    else if (expr_tp->name == "i32") intrinsic_idx = 3;
                    else if (expr_tp->name == "i64") intrinsic_idx = 4;
                    else if (expr_tp->name == "u8") intrinsic_idx = 5;
                    else if (expr_tp->name == "u16") intrinsic_idx = 6;
                    else if (expr_tp->name == "u32") intrinsic_idx = 7;
                    else if (expr_tp->name == "u64") intrinsic_idx = 8;
                    else if (expr_tp->name == "f32") intrinsic_idx = 9;
                    else if (expr_tp->name == "f64") intrinsic_idx = 10;
                    irgen->builder->write_2b_inst(OpCode::ExternalIntrinsic, intrinsic_idx);
                    irgen->builder->write_2b_inst(OpCode::RevStackAddr, 2);
                    irgen->builder->write_2b_inst(OpCode::RevStackAddr, 4);
                    irgen->builder->write_1b_inst(OpCode::Add64);
                    should_free_vec.push_back(true);
                }
            }
        }
        irgen->builder->write_2b_inst(OpCode::RevStackAddr, 0); // offset of last string
        irgen->builder->write_2b_inst(OpCode::RevStackAddr, 2); // size of last string
        irgen->builder->write_1b_inst(OpCode::Add64);
        //here we write the size and offset into the string object-------------
        // bring the string object to the stack top
        // each element in the should_free_vec has 3 stack items associated, 
        // and +2 for the total size on the stack and its duplicate
        // and another +2 for the 2 zeros we pushed on the stack earlier
        irgen->builder->write_1b_inst(OpCode::Dup);
        irgen->builder->write_2b_inst(OpCode::RevStackAddr, should_free_vec.size() * 3 + 2 + 2);
        irgen->builder->write_ptr_off(NativeType::getElementOffset(str_type, 1));
        irgen->builder->write_2b_inst(OpCode::Store, Yvm::Type::u64);
        irgen->builder->write_1b_inst(OpCode::Dup);
        irgen->builder->write_2b_inst(OpCode::RevStackAddr, should_free_vec.size() * 3 + 2 + 2);
        irgen->builder->write_ptr_off(NativeType::getElementOffset(str_type, 2));
        irgen->builder->write_2b_inst(OpCode::Store, Yvm::Type::u64);
        //---------------------------------------------------------------------
        irgen->builder->write_1b_inst(OpCode::Malloc);
        for (auto should_free : should_free_vec | std::views::reverse) {
            irgen->builder->write_2b_inst(OpCode::RevStackAddr, 2); // size
            irgen->builder->write_2b_inst(OpCode::RevStackAddr, 4); // pointer
            irgen->builder->write_2b_inst(OpCode::RevStackAddr, 2); // destinations
            irgen->builder->write_2b_inst(OpCode::RevStackAddr, 4); // the offset of the current string
            irgen->builder->write_1b_inst(OpCode::PtrOff);
            irgen->builder->write_1b_inst(OpCode::MemCpy);

            // remove the offset, size and pointer of the current string
            irgen->builder->write_1b_inst(OpCode::Switch); // bring offset forward
            irgen->builder->write_1b_inst(OpCode::Pop); // pop offset
            irgen->builder->write_1b_inst(OpCode::Switch); // bring size forward
            irgen->builder->write_1b_inst(OpCode::Pop); // pop size
            irgen->builder->write_1b_inst(OpCode::Switch); // bring the pointer forward
            if (should_free)
                irgen->builder->write_1b_inst(OpCode::Free);
            else
                irgen->builder->write_1b_inst(OpCode::Pop);
        }
        irgen->builder->write_2b_inst(OpCode::RevStackAddr, 3);
        irgen->builder->write_ptr_off(NativeType::getElementOffset(str_type, 0));
        irgen->builder->write_2b_inst(OpCode::Store, Yvm::Type::ptr);
        irgen->builder->write_1b_inst(OpCode::Pop); irgen->builder->write_1b_inst(OpCode::Pop);
        return {};
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
                if (var.second.first.type == YVMIRGenerator::VariableIndex::Checkpoint)
                    irgen->builder->write_2b_inst(OpCode::StackCheckpoint, var.second.first.index);
                else
                    irgen->builder->write_2b_inst(OpCode::StackAddr, var.second.first.index);
                if(!var.second.second.should_sret())
                {
                    irgen->builder->write_2b_inst(OpCode::Load, irgen->toTypeEnum(var.second.second));
                }
                if(!ret_type->is_lvalue && !ret_type->is_trivially_destructible(irgen))
                {
                    irgen->builder->write_1b_inst(OpCode::PopReg);
                    irgen->builder->write_1b_inst(OpCode::Pop);
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
                    // we use a dup because lifetime extensions expect on the stack top
                    // and one object below that for each element in the extension vector
                    irgen->builder->write_1b_inst(OpCode::Dup);
                    return { target->deref() };
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
        return {};
    }
    std::vector<Type> YVMExpressionEvaluator::operator()(PostfixOperation*) { return {}; }
    std::vector<Type> YVMExpressionEvaluator::fillArgs(bool uses_sret,
        const FunctionSignature& sig, const std::unique_ptr<Expression>& first_expr,
        std::vector<std::unique_ptr<Expression>>& exprs)
    {
        if(uses_sret)
        {
            irgen->builder->write_alloca(NativeType::get_size(irgen->toNativeType(sig.returnType)));
            irgen->builder->write_1b_inst(OpCode::Dup);
        }
        // assume there is no lifetime extension
        
        bool is_bound = first_expr != nullptr;
        std::vector<std::vector<Type>> extended_lifetimes;
        extended_lifetimes.reserve(exprs.size() + is_bound);
        bool using_elf = false;
        if (first_expr) {
            auto tp = std::visit(ExpressionTypeChecker{ irgen }, first_expr->toVariant());
            if (!tp) {
                irgen->error(tp.error());
            }
            else {
                using_elf = !extended_lifetimes.emplace_back(std::visit(*this, first_expr->toVariant())).empty();
                // if we are targeting a reference as the first argument, we don't need to implicit convert
                if(!
                    (sig.parameters[0].type.is_reference() && 
                        !sig.parameters[0].type.is_gc_reference() && 
                        tp->is_equal(sig.parameters[0].type.deref())
                    ))
                    implicitConvert(first_expr.get(), *tp, sig.parameters[0].type, false, true);
            }   
        }
        decltype(this->target) old_tgt = std::nullopt;
        this->target.swap(old_tgt);
        for(size_t i = 0; i < exprs.size(); i++)
        {
            auto tp = std::visit(ExpressionTypeChecker{irgen, sig.parameters[i + is_bound].type }, exprs[i]->toVariant());
            this->target = sig.parameters[i + is_bound].type;
            auto this_uses_elf = !extended_lifetimes.emplace_back(std::visit(*this, exprs[i]->toVariant())).empty();
            using_elf = using_elf ? true : this_uses_elf;
            if (!tp) {
                irgen->error(tp.error()); continue;
            }
            
            implicitConvert(exprs[i].get(), *tp, sig.parameters[i + is_bound].type, false, true);
            
        }
        this->target.swap(old_tgt);
        if (!using_elf) {
            return {};
        }
        // at least one function argument is performing temporary lifetime extension
        // so there's gaps in the stack and we need to bring all args together
        // we also need to return all the extended lifetimes in order
        // ex ex ex p ex ex p ex p ex ex p
        for (size_t i = extended_lifetimes.size(); i > 0; i--) {
            auto idx = i - 1;
            auto rev_view = extended_lifetimes | std::views::reverse;
            // we add idx for the amount of parameters on the stack
            // we add extended_lifetimes.size() - i for the amount of new values we added via this process
            // i.e the rev stack addr
            auto s_addr = idx + extended_lifetimes.size() - i;
            for (const auto& elf : std::ranges::subrange(rev_view.begin(), rev_view.begin() + idx)) {
                s_addr += elf.size();
            }
            irgen->builder->write_2b_inst(OpCode::RevStackAddr, s_addr);
        }
        std::vector<Type> final_types;
        for (auto& elf : extended_lifetimes | std::views::reverse) {
            final_types.push_back(Type{ "void" });
            final_types.insert(final_types.end(), elf.begin(), elf.end());
        }
        return final_types;
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

            auto extensions = fillArgs(uses_sret, fn.sig, expr->lhs, op->arguments);
            if (is_lambda) irgen->builder->write_fn_addr(irgen->block_hash + right_t->name);
            else std::visit(*this, expr->rhs->toVariant());
            //if(is_lambda) args.push_back(std::visit(*this, expr->rhs->toVariant()));
            irgen->builder->write_2b_inst(OpCode::Call, op->arguments.size() + 1 + uses_sret);
            if (uses_sret) irgen->builder->write_1b_inst(OpCode::Pop);
            if (return_t->is_non_owning(irgen))
            {
                return extensions;                
            }
            else
            {
                for (auto& ext : extensions) {
                    irgen->builder->write_1b_inst(OpCode::Switch);
                    destroy(ext);
                }
                return {};
            }
        }

        
        bool uses_sret = return_t->should_sret();
        auto extensions = fillArgs(uses_sret, fn.sig, {}, op->arguments);
        std::visit(*this, op->callee->toVariant());

        irgen->builder->write_2b_inst(OpCode::Call, op->arguments.size() + uses_sret);
        if (uses_sret) irgen->builder->write_1b_inst(OpCode::Pop);
        if (return_t->is_non_owning(irgen))
            return extensions;
        else
        {
            for (auto& ext : extensions) {
                irgen->builder->write_1b_inst(OpCode::Switch);
                destroy(ext);
            }
            return {};
        }
    }
    std::vector<Type> YVMExpressionEvaluator::operator()(SubscriptOperation* op)
    {
        ExpressionTypeChecker tp{ irgen };
        auto final_type = std::visit(tp, op->toVariant());
        if (!final_type) irgen->error(final_type.error());

        auto expr_ty = std::visit(tp, op->object->toVariant());
        if (!expr_ty) irgen->error(expr_ty.error());

        auto idx_ty = std::visit(tp, op->index->toVariant());
        if (!idx_ty) irgen->error(idx_ty.error());

        OverloadDetailsBinary* ovl = nullptr;
        std::string block = "";
        TokenType tok = TokenType::SquarePair;
        if (expr_ty->is_mutable || expr_ty->is_mutable_reference()) {
            tok = TokenType::SquarePairMut;
            std::tie(block, ovl) = resolveIdxMut(expr_ty->mutable_reference_to(), *idx_ty, irgen);
        }

        // if there is no mutable overload still default to the non mutable overload
        if (!ovl) std::tie(block, ovl) = resolveIdx(expr_ty->reference_to(), *idx_ty, irgen);
        return doBasicBinaryOp(this, ovl, tok, op->object.get(), op->index.get(), *expr_ty, *idx_ty, block);
    }
    std::vector<Type> YVMExpressionEvaluator::operator()(GCNewExpression* expr)
    {
        auto gc_tp = std::visit(ExpressionTypeChecker{ irgen }, expr->toVariant());
        if (!gc_tp) { irgen->error(gc_tp.error()); return {}; }
        
        auto gc_native = reinterpret_cast<StructNativeTy*>(reinterpret_cast<YVMEngine*>(irgen->module->engine)->struct_manager.get_struct_type({ {
                irgen->toNativeType(gc_tp->deref()),
                NativeType::getI64()
            } }));
        irgen->builder->write_const(NativeType::get_size(gc_native));
        irgen->builder->write_2b_inst(OpCode::ExternalIntrinsic, 0); //gcmalloc instrinsic
        std::visit(*this, expr->target_expression->toVariant());
        irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1);
        irgen->builder->write_ptr_off(NativeType::getElementOffset(gc_native, 0));
        auto internal_ty = *std::visit(ExpressionTypeChecker{ irgen }, expr->target_expression->toVariant());
        implicitConvert(expr->target_expression.get(), internal_ty, gc_tp->subtypes[0], true, false); // write the object into the gc memory
        irgen->builder->write_1b_inst(OpCode::Pop);
        irgen->builder->write_const<uint64_t>(0);
        irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1);
        irgen->builder->write_ptr_off(NativeType::getElementOffset(gc_native, 1));
        irgen->builder->write_2b_inst(OpCode::Store, Yvm::Type::i64);
        return {};
    }
    std::vector<Type> YVMExpressionEvaluator::operator()(MacroInvocation* invc)
    {
        ExpressionTypeChecker{ irgen }(invc);
        return std::visit(*this, invc->result->toVariant());
    }
    std::vector<Type> YVMExpressionEvaluator::operator()(SpawnExpression* expr)
    {
        auto call_op = reinterpret_cast<CallOperation*>(expr->call_expr.get());
        ExpressionTypeChecker type_checker{ irgen };
        auto return_t = type_checker(expr);
        auto t = std::visit(type_checker, call_op->callee->toVariant());
        if (!return_t) { irgen->error(return_t.error()); return {}; }
        if (t && t->is_error_ty())
        {
            std::visit(*this, call_op->callee->toVariant());
            debugbreak();
        }
        if (!t || !(t->is_function() || t->is_lambda())) { irgen->error(t.error()); return {}; }
        
        bool is_lambda = t->is_lambda();
        auto fn = reinterpret_cast<FunctionType&>(*t);
        auto bexpr = dynamic_cast<BinaryOperation*>(call_op->callee.get()); 
        StructNativeTy* params_ty;
        std::vector<NativeTy*> types;
        for (auto& param : fn.sig.parameters) {
            types.push_back(irgen->toNativeType(param.type));
        }
        params_ty = reinterpret_cast<StructNativeTy*>(reinterpret_cast<YVMEngine*>(irgen->module->engine)->struct_manager.get_struct_type(types));
        NativeTy* ret_ty = irgen->toNativeType(fn.sig.returnType);
        bool uses_sret = fn.sig.returnType.should_sret();

        auto fiber_ty = reinterpret_cast<StructNativeTy*>(irgen->toNativeType(*return_t));

        irgen->builder->write_alloca(NativeType::get_size(fiber_ty));
        irgen->builder->write_1b_inst(OpCode::Dup);
        irgen->builder->write_const<uint8_t>(uses_sret);
        irgen->builder->write_const(ret_ty);
        irgen->builder->write_const(params_ty);
        
        if (fn.is_bound)
        {
            irgen->builder->write_fn_addr("");
            irgen->builder->write_2b_inst(OpCode::ExternalIntrinsic, 12);
            auto& function_name = irgen->builder->get_last_inserted_function();
            // TODO: called and stored fns
            //bexpr is guaranteed to be valid if the function is bound as
            //callee is a binary dot expr
            auto left_t = std::visit(type_checker, bexpr->lhs->toVariant());
            auto cls = left_t->deref().get_decl_if_class(irgen);
            // method call
            if (auto rhs = dynamic_cast<NameExpression*>(bexpr->rhs.get()))
            {
                auto this_block = left_t->deref().full_name() + "::";
                auto [block, fn] =
                    left_t->deref().module->findFunction(this_block, rhs->text);
                if (block == this_block && fn->sig.parameters[0].name == "this")
                {
                    function_name = block + rhs->text;

                    irgen->builder->write_1b_inst(OpCode::Dup); // the fiber ptr
                    irgen->builder->write_ptr_off(NativeType::getElementOffset(fiber_ty, 0)); // get the parameters pointer
                    irgen->builder->write_2b_inst(OpCode::Load, Yvm::Type::ptr);

                    // the "this" parameter
                    irgen->builder->write_1b_inst(OpCode::Dup);
                    NativeType::getElementOffset(params_ty, 0);
                    std::visit(*this, bexpr->lhs->toVariant()); // no need to remove lifetime extensions because borrow is not allowed
                    irgen->builder->write_1b_inst(OpCode::Switch);
                    implicitConvert(bexpr->lhs.get(), *left_t, fn->sig.parameters[0].type, true, false);
                    irgen->builder->write_1b_inst(OpCode::Pop);

                    for (auto i : std::views::iota(1u, fn->sig.parameters.size())) {
                        if (i != fn->sig.parameters.size() - 1) irgen->builder->write_1b_inst(OpCode::Dup);
                        NativeType::getElementOffset(params_ty, i);
                        std::visit(*this, call_op->arguments[i - 1]->toVariant());
                        irgen->builder->write_1b_inst(OpCode::Switch);
                        implicitConvert(
                            call_op->arguments[i - 1].get(),
                            *std::visit(type_checker, call_op->arguments[i - 1]->toVariant()),
                            fn->sig.parameters[i].type, true, false);
                        irgen->builder->write_1b_inst(OpCode::Pop);
                    }
                    
                }
                return {};
            }
        }
        std::visit(*this, call_op->callee->toVariant());
        irgen->builder->write_2b_inst(OpCode::ExternalIntrinsic, 12);

        irgen->builder->write_1b_inst(OpCode::Dup); // the fiber ptr
        irgen->builder->write_ptr_off(NativeType::getElementOffset(fiber_ty, 0));
        irgen->builder->write_2b_inst(OpCode::Load, Yvm::Type::ptr);

        for (auto i : std::views::iota(0u, fn.sig.parameters.size())) {
            if (i != fn.sig.parameters.size() - 1) irgen->builder->write_1b_inst(OpCode::Dup);
            NativeType::getElementOffset(params_ty, i);
            std::visit(*this, call_op->arguments[i]->toVariant());
            irgen->builder->write_1b_inst(OpCode::Switch);
            implicitConvert(
                call_op->arguments[i].get(),
                *std::visit(type_checker, call_op->arguments[i]->toVariant()),
                fn.sig.parameters[i].type, true, false);
            irgen->builder->write_1b_inst(OpCode::Pop);
        }

        return {};
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
        auto last_alloc = irgen->builder->last_alloc_addr();
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
        returned_alloc_addr = last_alloc;
        return {};
    }

    std::vector<Type> YVMExpressionEvaluator::operator()(NullLiteral*)
    {
        irgen->builder->write_const(uint8_t{ 0 });
        return {};
    }

    std::vector<Type> YVMExpressionEvaluator::operator()(AsExpression* expr)
    {
        auto ty = std::visit(ExpressionTypeChecker{irgen}, expr->expr->toVariant());
        auto final_ty = std::visit(ExpressionTypeChecker{irgen}, expr->toVariant());
        if (!final_ty) { irgen->error(final_ty.error()); return {}; }
        if (!ty) { irgen->error(ty.error()); return {}; }
        auto final_as_native = reinterpret_cast<StructNativeTy*>(irgen->toNativeType(*final_ty));
        auto as_native = reinterpret_cast<StructNativeTy*>(irgen->toNativeType(*ty));
        /*
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
        return nullptr;*/
        if (auto decl = ty->get_decl_if_union())
        {
            std::visit(*this, expr->expr->toVariant()); // stack: union
            auto it = decl->fields.find(expr->dest.name);
            uint32_t idx = std::distance(decl->fields.begin(), it);
            irgen->builder->write_alloca(NativeType::get_size(final_as_native)); // stack: union, final
            irgen->builder->write_1b_inst(OpCode::Switch); // stack: final, union
            irgen->builder->write_1b_inst(OpCode::Dup); // stack: final, union, union
            irgen->builder->write_ptr_off(NativeType::getElementOffset(as_native, 1)); // stack: final, union, union_idx_ptr
            irgen->builder->write_2b_inst(OpCode::Load, Yvm::Type::u32); // stack: final, union, union_idx
            irgen->builder->write_const(idx); // stack: final, union, union_idx, target_idx
            irgen->builder->write_2b_inst(OpCode::CmpEq, 32); // stack: final, union, is_idx_valid
            irgen->builder->write_2b_inst(OpCode::RevStackAddr, 2); // stack: final, union, is_idx_valid, final
            irgen->builder->write_ptr_off(NativeType::getElementOffset(final_as_native, 1)); // stack: final, union, is_valid_idx, final_idx
            irgen->builder->write_2b_inst(OpCode::Store, Yvm::Type::u32); // stack: final, union
            irgen->builder->write_ptr_off(NativeType::getElementOffset(as_native, 0)); // stack: final, union_data_ptr
            irgen->builder->write_2b_inst(OpCode::RevStackAddr, 1); // stack: final, union_data_ptr, final
            irgen->builder->write_2b_inst(OpCode::Store, Yvm::Type::ptr);
            
            //if (!ty->deref().is_trivially_destructible(irgen) && !ty->deref().is_lvalue)
            //{
            //    auto lf = new ExtendedLifetimes;
            //    lf->objects.emplace_back(std::move(ty).value(), internal);
            //    std::string name = "__del_parents___aaaaaaaa";
            //    memcpy(name.data() + 16, &lf, sizeof(void*));
            //    val->setName(name);
            //}
            //return val;
        }
        return {};
    }

    std::vector<Type> YVMExpressionEvaluator::operator()(CharLiteral* lit)
    {
        irgen->builder->write_const(lit->value);
        return {};
    }
}
