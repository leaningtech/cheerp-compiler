#include "unwind.h"
#include "private_typeinfo.h"
#include <typeinfo>
#include <cassert>
#include <cstdlib>
#include <cheerpintrin.h>
#include <cheerp/client.h>

namespace [[cheerp::genericjs]] client {
	class CheerpException: public Error {
	public:
		CheerpException(const String& msg);
	};
}
namespace __cxxabiv1 {

struct __cheerp_landingpad
{
	void* val;
	int sel;
};

struct Exception
{
	void* obj;
	void* adjustedPtr;
	std::type_info* tinfo;
	void (*dest)(void*);
	Exception* next;
	Exception(void* obj, std::type_info* tinfo, void(*dest)(void*)) throw()
		: obj(obj), adjustedPtr(nullptr), tinfo(tinfo), dest(dest), next(nullptr)
	{
	}
	~Exception() noexcept
	{
		if(dest)
			dest(obj);
		free(obj);
	}
};

static Exception* current_exception = nullptr;

extern "C" {

[[noreturn]]
__attribute((noinline))
void
__cxa_throw(void *thrown_object, std::type_info *tinfo, void (*dest)(void *)) {
	Exception* ex = new Exception(thrown_object, tinfo, dest);
	ex->next = current_exception;
	current_exception = ex;

	//TODO: if I remove this statement clang crashes
	__cheerp_landingpad* lp = new __cheerp_landingpad { nullptr, 0};

	client::CheerpException* wrapper = new client::CheerpException(tinfo->name());
	__builtin_cheerp_throw(wrapper);
}

__attribute((noinline))
void*
__cxa_begin_catch(void* unwind_arg) noexcept
{
	return static_cast<Exception*>(unwind_arg)->adjustedPtr;
}

__attribute((noinline))
void*
__cxa_get_exception_ptr(void* unwind_arg) noexcept
{
	return static_cast<Exception*>(unwind_arg)->adjustedPtr;
}

__attribute((noinline))
void __cxa_end_catch() noexcept {
	Exception* ex = current_exception;
	current_exception = ex->next;
	delete ex;
}

struct __cheerp_clause
{
	std::type_info* val;
	int sel;
};
__attribute((noinline))
 __cheerp_landingpad*
__gxx_personality_v0
                    (client::Object* obj, __cheerp_clause* catches, int n) noexcept
{
	static bool reent = false;
	if(reent)
		__builtin_cheerp_throw(obj);

	reent = true;

	bool native;
	asm("%1 instanceof CheerpException" : "=r"(native) : "r"(obj));
	
	Exception* ex = current_exception;
	__cheerp_landingpad* lp = new __cheerp_landingpad { nullptr, 0};
	if(native)
	{
		lp->val = ex;
	}

	for(int i = 0; i < n; i++)
	{
		if(!native)
		{
			if(catches[i].val == nullptr)
			{
				lp->sel = catches[i].sel;
				break;
			}
			continue;
		}
		std::ptrdiff_t adjustedOffset = 0;
		__shim_type_info* catcher = static_cast<__shim_type_info*>(catches[i].val);
		__shim_type_info* thrown = static_cast<__shim_type_info*>(ex->tinfo);
		bool deref = false;
		if(catcher->can_catch(thrown, adjustedOffset, deref))
		{
			ex->adjustedPtr = ex->obj;
			if(deref)
			{
				ex->adjustedPtr = *static_cast<void**>(ex->adjustedPtr);
			}
			if(adjustedOffset != 0)
			{
				ex->adjustedPtr = __builtin_cheerp_downcast<void,void>(ex->adjustedPtr, -adjustedOffset);
			}
			lp->sel = catches[i].sel;
			break;
		}
	}
	reent = false;
	return lp;
}

}
}
