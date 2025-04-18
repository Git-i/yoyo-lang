#pragma once
#include "engine.h"
#include "yoyo_vm/vm.h"
#include "unique_type_selector.h"
namespace Yoyo
{
    struct YVMAppModule;
	class YVMEngine : public Engine {
    public:
        YVMAppModule* addAppModule(const std::string& name);
        void addModule(const std::string& module_name, std::string source);
        void compile();
        void prepareForExecution();
        void addDynamicLibrary(std::string_view path);
        virtual void* createGlobalConstant(const Type& type, const std::vector<Constant>& args, IRGenerator*) override;
        Yvm::VM vm;
        StructTypeSelector struct_manager;
	};
}