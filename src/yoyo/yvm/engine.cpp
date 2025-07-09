#include <ir_gen.h>
#include <yvm/yvm_module.h>
#include <parser.h>
#include <ranges>
#include <statement.h>
#include <yvm/yvm_engine.h>
#include <yvm/yvm_irgen.h>
#include <gc/gc.h>
#include <yvm/app_module.h>
#include "yvm/fwd_decl.h"
#include <format>

#include "minicoro.h"
namespace Yoyo
{
    struct YoyoString {
        char* data;
        uint64_t size;
        uint64_t capacity;
    };
    YVMEngine::YVMEngine()
    {
        vm.do_native_call = [](void* function, Yvm::VM::Type* begin, size_t arg_size, void* proto) {
            return NativeType::doCall(static_cast<NativeProto*>(proto), arg_size, begin, function);
            };
        //instrinsic zero is gcnew
        vm.intrinsic_handler = [](Yvm::Stack& stack, uint8_t intrinsic, void* ex_data) {
            /*
            * gc malloc -> 0
            * i8 to str -> 1
            * i16 to str -> 2
            * i32 to str -> 3
            * i64 to str -> 4
            * 
            * u8 to str -> 5
            * u16 to str -> 6
            * u32 to str -> 7
            * u64 to str -> 8
            * 
            * f32 to str -> 9
            * f64 to str -> 10
            * 
            * sleep fiber -> 11
            * create fiber -> 12 [pops: code*, param_struct_type*, return_type*, bool should_sret, return fiber*]
            */
            auto self = reinterpret_cast<decltype(this)>(ex_data);
            auto to_str_primitive = [&stack](auto value) {
                auto length = std::formatted_size("{}", value);
                auto buffer = static_cast<char*>(malloc(length));
                std::format_to(buffer, "{}", value);
                stack.push(buffer); stack.push<uint64_t>(length);
                };
            switch (intrinsic)
            {
            case 0: stack.push(GC_MALLOC(stack.pop<32>())); break;
            case 1: to_str_primitive(stack.pops<8>()); break;
            case 2: to_str_primitive(stack.pops<16>()); break;
            case 3: to_str_primitive(stack.pops<32>()); break;
            case 4: to_str_primitive(stack.pops<64>()); break;

            case 5: to_str_primitive(stack.pop<8>()); break;
            case 6: to_str_primitive(stack.pop<16>()); break;
            case 7: to_str_primitive(stack.pop<32>()); break;
            case 8: to_str_primitive(stack.pop<64>()); break;

            case 9: to_str_primitive(stack.popf<32>()); break;
            case 10: to_str_primitive(stack.popf<64>()); break;
            case 11: self->sleep(stack.pop<64>()); break;
            case 12: {
                auto code = stack.pop_ptr<uint64_t>();
                auto param_ty = stack.pop_ptr<StructNativeTy>();
                auto ret_ty = stack.pop_ptr<NativeTy>();
                bool should_sret = stack.pop<8>();
                auto ret_fiber = stack.pop_ptr<Fiber>();
                *ret_fiber = self->createFiber(FunctionTy{ code, param_ty, ret_ty, should_sret });
                break;
            }
            }
            };
        vm.ex_data = this;
        YVMModule::makeBuiltinModule(this);
    }

    YVMEngine::~YVMEngine()
    {
        for (auto mod : external_dlls) NativeType::free_native_library(mod);
    }

    YVMAppModule* YVMEngine::addAppModule(const std::string& name)
    {
        if (modules.contains(name)) return nullptr;
        auto& md = modules[name];
        md = std::make_unique<YVMAppModule>();
        md->module_hash = name + "::";
        md->engine = this;
        auto app_md = reinterpret_cast<YVMAppModule*>(md.get());
        vm.add_module(&app_md->code);
        return app_md;
    }

    ModuleBase* YVMEngine::addModule(const std::string& module_name, std::string source)
    {
        auto p = Parser(source);
        if (modules.contains(module_name)) return modules.at(module_name).get();
        auto prog = p.parseProgram();
        if (p.failed()) return nullptr;
        auto& md = modules[module_name];
        md = std::make_unique<YVMModule>();
        md->engine = this;
        md->module_hash = module_name + "::";
        md->modules["core"] = modules.at("core").get();
        vm.add_module(&reinterpret_cast<YVMModule*>(md.get())->code);
        for (auto& stat : prog)
        {
            if (!std::visit(ForwardDeclaratorPass1{ reinterpret_cast<YVMModule*>(md.get()), stat, md->module_hash }, stat->toVariant()))
            {
                modules.erase(module_name);
                break;
            };
        }
        sources[module_name] = { std::move(source), std::move(prog) };
        return md.get();
    }

    bool YVMEngine::compile()
    {
        YVMIRGenerator irgen;
        auto keys_view = std::ranges::views::keys(modules);
        std::vector module_names(keys_view.begin(), keys_view.end());
        bool has_error = false;
        for (auto& mod : modules)
        {
            auto yvm_mod = reinterpret_cast<YVMModule*>(mod.second.get());
            if (!sources.contains(mod.first)) continue;
            auto src = sources.extract(mod.first);
            SourceView vw(src.mapped().first, mod.first);
            irgen.view = &vw;
            bool this_success = irgen.GenerateIR(mod.first, std::move(src.mapped().second), yvm_mod, this);
            has_error = !this_success || has_error;
        }
        return !has_error;
    }
    void YVMEngine::addDynamicLibrary(std::string_view path)
    {
        auto mod = NativeType::load_native_library(std::string(path));
        if (mod) external_dlls.push_back(mod);
    }
    Fiber YVMEngine::createFiber(const FunctionTy& fn)
    {
        struct CoroData {
            Yvm::VM* vm;
            uint64_t* code;
            StructNativeTy* param_type;
            NativeTy* ret_ty;
            bool should_sret;
        };
        struct ParamReturnPtrs {
            void* params; void* ret;
        };
        mco_desc desc = mco_desc_init([](mco_coro* co) {
            auto data = reinterpret_cast<CoroData*>(mco_get_user_data(co));
            auto param_data = ParamReturnPtrs{
                .params = data->param_type == nullptr ? nullptr : alloca(NativeType::get_size(data->param_type)),
                .ret = data->ret_ty == nullptr ? nullptr : alloca(NativeType::get_size(data->ret_ty)) };
            mco_push(co, &param_data, sizeof(ParamReturnPtrs));
            mco_yield(co);
            
            auto rn = data->vm->new_runner();
            auto n_elems = data->param_type == nullptr ? 0 : NativeType::getNumElements(data->param_type);
            if (data->should_sret) rn.stack_data[0].ptr = param_data.ret;
            for (auto i : std::views::iota(0u + data->should_sret, n_elems + data->should_sret)) {
                auto elem_ptr = static_cast<std::byte*>(param_data.params) + NativeType::getElementOffset(data->param_type, i);
                auto this_t = NativeType::getStructElementType(data->param_type, i);
                
                if (this_t == NativeType::getI8()) rn.stack_data[i].i8 = *reinterpret_cast<int8_t*>(elem_ptr);
                else if (this_t == NativeType::getI16()) rn.stack_data[i].i16 = *reinterpret_cast<int16_t*>(elem_ptr);
                else if (this_t == NativeType::getI32()) rn.stack_data[i].i32 = *reinterpret_cast<int32_t*>(elem_ptr);
                else if (this_t == NativeType::getI64()) rn.stack_data[i].i64 = *reinterpret_cast<int64_t*>(elem_ptr);
                else if (this_t == NativeType::getU8()) rn.stack_data[i].u8 = *reinterpret_cast<uint8_t*>(elem_ptr);
                else if (this_t == NativeType::getU16()) rn.stack_data[i].u16 = *reinterpret_cast<uint16_t*>(elem_ptr);
                else if (this_t == NativeType::getU32()) rn.stack_data[i].u32 = *reinterpret_cast<uint32_t*>(elem_ptr);
                else if (this_t == NativeType::getU64()) rn.stack_data[i].u64 = *reinterpret_cast<uint64_t*>(elem_ptr);
                else if (this_t == NativeType::getF32()) rn.stack_data[i].f32 = *reinterpret_cast<float*>(elem_ptr);
                else if (this_t == NativeType::getF64()) rn.stack_data[i].f64 = *reinterpret_cast<double*>(elem_ptr);
                else if (this_t == NativeType::getPtrTy()) rn.stack_data[i].ptr = *reinterpret_cast<void**>(elem_ptr);
                else rn.stack_data[i].ptr = elem_ptr;
            }

            //jmp_buf panic_buf;
            //if (setjmp(panic_buf)) {
            //    // panic
            //}
            rn.run_code(data->code, nullptr, n_elems + data->should_sret);
            delete data;
            }, 0);
        desc.user_data = new CoroData{ .vm = &vm, .code = fn.code, .param_type = fn.params, .ret_ty = fn.ret, .should_sret = fn.should_sret };
        desc.storage_size = sizeof(ParamReturnPtrs);

        mco_coro* co;
        auto res = mco_create(&co, &desc);
        mco_resume(co);
        ParamReturnPtrs param_ret;
        mco_pop(co, &param_ret, sizeof(ParamReturnPtrs));
        rt.add_job(co);
        return Fiber{ param_ret.params, param_ret.ret, co, &rt };
    }
    std::optional<FunctionTy> YVMEngine::findFunction(ModuleBase* mod, const std::string& function_name)
    {
        auto tp = Type{ .name = function_name };
        UnsaturatedTypeIterator it(tp);
        auto last = it.last();
        auto block = std::string{ function_name.begin(), function_name.begin() + function_name.size()  - last.name.size()};
        if (auto [blk, fn] = mod->findFunction(block, last.name); fn) {
            std::vector<NativeTy*> types;
            for (auto& param : fn->sig.parameters) {
                types.push_back(
                    reinterpret_cast<YVMModule*>(param.type.module)->toNativeType(param.type, param.type.block_hash, nullptr, {}));
            }
            return FunctionTy{
                .code = reinterpret_cast<YVMModule*>(mod)->code.code[blk + fn->name].data(),
                .params = types.empty() ? nullptr : reinterpret_cast<StructNativeTy*>(struct_manager.get_struct_type(types)),
                .ret = fn->sig.returnType.is_void() ? nullptr :
                    reinterpret_cast<YVMModule*>(fn->sig.returnType.module)->toNativeType(
                        fn->sig.returnType,
                        fn->sig.returnType.block_hash, nullptr, {}),
                .should_sret = fn->sig.returnType.should_sret()
            };
        }
        return std::nullopt;
    }
    void* YVMEngine::createGlobalConstant(const Type& type, const std::vector<Constant>& args, IRGenerator* irgen_g)
    {
        auto irgen = reinterpret_cast<YVMIRGenerator*>(irgen_g);
        auto native_t = reinterpret_cast<StructNativeTy*>(irgen->toNativeType(type));
        if (type.name == "str") {
            auto str_ptr = static_cast<char*>(std::get<void*>(args[0].internal_repr));
            auto str_sz = strlen(str_ptr);
            auto ret_val = static_cast<YoyoString*>(malloc(sizeof(YoyoString)));
            ret_val->size = str_sz;
            ret_val->capacity = str_sz;
            ret_val->data = static_cast<char*>(malloc(str_sz + 1));
            strcpy(ret_val->data, str_ptr);

            return ret_val;
        }
        size_t n = 0;
        auto cls = type.get_decl_if_class(irgen_g);
        if (!cls) return nullptr;

        auto memory = static_cast<std::byte*>(malloc(NativeType::get_size(native_t)));
        for (auto& constant : args) {
            auto off = NativeType::getElementOffset(native_t, n);
            auto elem = memory + off;
            auto& elem_type = cls->vars[n].type;
            if (elem_type.name == "i8") *reinterpret_cast<int8_t*>(elem) = std::get<int64_t>(constant.internal_repr);
            else if (elem_type.name == "i16") *reinterpret_cast<int16_t*>(elem) = std::get<int64_t>(constant.internal_repr);
            else if (elem_type.name == "i32") *reinterpret_cast<int32_t*>(elem) = std::get<int64_t>(constant.internal_repr);
            else if (elem_type.name == "i64") *reinterpret_cast<int64_t*>(elem) = std::get<int64_t>(constant.internal_repr);

            else if (elem_type.name == "u8") *reinterpret_cast<uint8_t*>(elem) = std::get<uint64_t>(constant.internal_repr);
            else if (elem_type.name == "u16") *reinterpret_cast<uint16_t*>(elem) = std::get<uint64_t>(constant.internal_repr);
            else if (elem_type.name == "u32") *reinterpret_cast<uint32_t*>(elem) = std::get<uint64_t>(constant.internal_repr);
            else if (elem_type.name == "u64") *reinterpret_cast<uint64_t*>(elem) = std::get<uint64_t>(constant.internal_repr);

            else if (elem_type.name == "f32") *reinterpret_cast<float*>(elem) = std::get<double>(constant.internal_repr);
            else if (elem_type.name == "f64") *reinterpret_cast<double*>(elem) = std::get<double>(constant.internal_repr);
            n++;
        }
        return memory;
    }
    void* YVMEngine::findNativeFunction(const std::string& name)
    {
        for (auto mod : external_dlls) {
            auto fn = NativeType::get_library_fn(mod, name);
            if (fn) return fn;
        }
        return nullptr;
    }
    void YVMEngine::prepareForExecution()
    {
        vm.link();
    }
}
