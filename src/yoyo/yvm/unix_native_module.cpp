#include "yvm/native_type.h"
#include <dlfcn.h>
namespace Yoyo {
	struct NativeModule {
        void* handle;
	};
	namespace NativeType
	{
		
		NativeModule* load_native_library(const std::string& lib_name) {
            // TODO handle error cases
            return new NativeModule{ .handle = dlopen(lib_name.c_str(), 0) };
		}
		void* get_library_fn(NativeModule* module, const std::string& fn_name) {
			return dlsym(module->handle, fn_name.c_str());
        }
		void free_native_library(NativeModule* mod) {
            dlclose(mod->handle);
			delete mod;
		}
	}
	
}
