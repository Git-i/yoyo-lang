#include "yvm/app_module.h"
#include <yoyo_vm/emitter.h>
#include <ranges>
namespace Yoyo
{
	// the application still obeys yoyo calling conventions
	void YVMAppModule::addFunction(FunctionSignature sig, void* func, std::string name)
	{
		auto eng = reinterpret_cast<YVMEngine*>(engine);
		std::vector<NativeTy*> args(sig.parameters.size());
		NativeTy* ret_ty = nullptr;
		if (sig.returnType.should_sret()) {
			ret_ty = NativeType::getVoid();
			args.push_back(NativeType::getPtrTy());
		}
		else ret_ty = toNativeType(sig.returnType, module_hash, nullptr, {});

		for (auto i : std::views::iota(size_t{ 0 }, sig.parameters.size()))
			args[i] = sig.parameters[i].type.should_sret() ? NativeType::getPtrTy() : toNativeType(sig.parameters[i].type, module_hash, nullptr, {});
		auto proto = eng->fi_manager.get_proto(args, ret_ty);

		Yvm::Emitter em(false);
		em.write_const(proto);
		em.write_const(func);
		em.write_2b_inst(Yvm::OpCode::NativeCall, sig.parameters.size());
		if (!sig.returnType.should_sret()) em.write_1b_inst(Yvm::OpCode::Ret);
		em.close_function(&code, module_hash + name);
		
		functions[module_hash].emplace_back(name, std::move(sig), std::vector{ Attribute{"public"} });
	}
}

