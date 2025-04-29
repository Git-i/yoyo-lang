#include "yvm/native_type.h"
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include "Windows.h"
namespace Yoyo {
	struct NativeModule {
		HMODULE mod;
	};
	namespace NativeType
	{
		
		NativeModule* load_native_library(const std::string& lib_name) {
			return new NativeModule{ .mod = LoadLibraryA(lib_name.c_str()) };
		}
		void* get_library_fn(NativeModule* module, const std::string& fn_name) {
			return GetProcAddress(module->mod, fn_name.c_str());
		}
		void free_native_library(NativeModule* mod) {
			FreeLibrary(mod->mod);
			delete mod;
		}
	}
	
}