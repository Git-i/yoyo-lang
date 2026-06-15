#pragma once
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>

#include "engine.h"
namespace Yoyo {
class LLVMEngine : public Engine {
public:
    llvm::orc::ThreadSafeContext llvm_context;
    std::unique_ptr<llvm::orc::LLJIT> jit;
    LLVMAppModule* addAppModule(const std::string& name);
    void addModule(const std::string& module_name, std::string source);
    void compile();
    void prepareForExecution();
    void addStaticLibrary(std::string_view path);
    void addDynamicLibrary(std::string_view path);
    virtual void* createGlobalConstant(const Type& type,
                                       const std::vector<Constant>& args,
                                       IRGenerator*) override;
};
}  // namespace Yoyo