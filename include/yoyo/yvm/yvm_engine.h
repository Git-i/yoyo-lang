#pragma once
#include "engine.h"
#include "yoyo_vm/vm.h"
#include "unique_type_selector.h"
#include <optional>
namespace Yoyo
{
    struct YVMAppModule;
    struct FunctionTy {
        uint64_t* code;
        StructNativeTy* params;
    };
    struct FiberData {

    };
	class YOYO_API YVMEngine : public Engine {
        size_t idx = 0;
    public:
        YVMEngine();
        ~YVMEngine() override;
        YVMAppModule* addAppModule(const std::string& name);
        ModuleBase* addModule(const std::string& module_name, std::string source);
        void compile();
        void prepareForExecution();
        void addDynamicLibrary(std::string_view path);
        void* createFiber(const FunctionTy& fn, void** coro_out);
        void runFiber(void* coro);
        std::optional<FunctionTy> findFunction(ModuleBase* mod, const std::string& function_name);
        void execute();
        void* createGlobalConstant(const Type& type, const std::vector<Constant>& args, IRGenerator*) override;
        void* findNativeFunction(const std::string& name);
        size_t getTypeSpawnIdx(const Type& tp);
        Yvm::VM vm;
        StructTypeSelector struct_manager;
        UniqueFISelector fi_manager;
        std::vector<NativeModule*> external_dlls;
	};
}