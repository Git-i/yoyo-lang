#pragma once
#include "engine.h"
namespace Yoyo
{
	class LLVMEngine : public Engine {
    public:
        llvm::orc::ThreadSafeContext llvm_context;
        std::unique_ptr<llvm::orc::LLJIT> jit;
        virtual std::unique_ptr<Module> make_module() override;
        virtual void* createGlobalConstant(const Type& type, const std::vector<Constant>& args, IRGenerator*) override;
	};
}