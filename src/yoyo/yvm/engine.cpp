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

#define MINICORO_IMPL
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
        vm.intrinsic_handler = [](Yvm::Stack& stack, uint8_t intrinsic) {
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
            */
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
            }
            };
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

    void YVMEngine::compile()
    {
        YVMIRGenerator irgen;
        auto keys_view = std::ranges::views::keys(modules);
        std::vector module_names(keys_view.begin(), keys_view.end());

        for (auto& mod : modules)
        {
            auto yvm_mod = reinterpret_cast<YVMModule*>(mod.second.get());
            if (!sources.contains(mod.first)) continue;
            auto src = sources.extract(mod.first);
            SourceView vw(src.mapped().first, mod.first);
            irgen.view = &vw;
            irgen.GenerateIR(mod.first, std::move(src.mapped().second), yvm_mod, this);
        }
    }
    void YVMEngine::addDynamicLibrary(std::string_view path)
    {
        auto mod = NativeType::load_native_library(std::string(path));
        if (mod) external_dlls.push_back(mod);
    }
    void* YVMEngine::createFiber(const FunctionTy& fn, void** coro_out)
    {
        struct CoroData {
            Yvm::VM* vm;
            uint64_t* code;
            StructNativeTy* param_type;
        };

        mco_desc desc = mco_desc_init([](mco_coro* co) {
            auto data = reinterpret_cast<CoroData*>(mco_get_user_data(co));
            auto ptr = alloca(NativeType::get_size(data->param_type));
            mco_push(co, &ptr, sizeof(void*));
            mco_yield(co);
            
            auto rn = data->vm->new_runner();
            auto n_elems = NativeType::getNumElements(data->param_type);
            for (auto i : std::views::iota(0u, n_elems)) {
                auto elem_ptr = static_cast<std::byte*>(ptr) + NativeType::getElementOffset(data->param_type, i);
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
            rn.run_code(data->code, nullptr, n_elems);
            delete data;
            }, 0);
        desc.user_data = new CoroData{ .vm = &vm, .code = fn.code, .param_type = fn.params };
        desc.storage_size = sizeof(void*);

        mco_coro* co;
        auto res = mco_create(&co, &desc);
        mco_resume(co);
        void* out_ptr;
        mco_pop(co, &out_ptr, sizeof(void*));
        *coro_out = co;
        return out_ptr;
    }
    void YVMEngine::runFiber(void* coro)
    {
        mco_resume(static_cast<mco_coro*>(coro));
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
                .params = reinterpret_cast<StructNativeTy*>(struct_manager.get_struct_type(types))
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
