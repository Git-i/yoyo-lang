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
        NativeTy* ret;
        bool should_sret;
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
        bool compile();
        void prepareForExecution();
        void addDynamicLibrary(std::string_view path);
        Fiber createFiber(const FunctionTy& fn);
        std::optional<FunctionTy> findFunction(ModuleBase* mod, const std::string& function_name);
        
        void* createGlobalConstant(const Type& type, const std::vector<Constant>& args, IRGenerator*) override;
        void* findNativeFunction(const std::string& name);
        size_t getTypeSpawnIdx(const Type& tp);
        Yvm::VM vm;
        StructTypeSelector struct_manager;
        UniqueFISelector fi_manager;
        UnionTypeSelector union_manager;
        std::vector<NativeModule*> external_dlls;
	};
}