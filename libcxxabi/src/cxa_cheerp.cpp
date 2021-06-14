#include "unwind.h"
#include "private_typeinfo.h"
#include <typeinfo>
#include <cassert>
#include <cstdlib>
#include <cheerpintrin.h>
#include <cheerp/client.h>

namespace __cxxabiv1 {

struct __cheerp_landingpad
{
	void* val;
	int sel;
};

struct Exception
{
	void* obj;
	std::type_info* tinfo;
	void (*dest)(void*);
	Exception* next;
	Exception(void* obj, std::type_info* tinfo, void(*dest)(void*)) throw()
		: obj(obj), tinfo(tinfo), dest(dest), next(nullptr)
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
	__builtin_cheerp_throw( new __cheerp_landingpad{ex, 0});
}

__attribute((noinline))
void*
__cxa_begin_catch(void* unwind_arg) noexcept
{
	return static_cast<Exception*>(unwind_arg)->obj;
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
                    (__cheerp_landingpad* lp, __cheerp_clause* catches, int n) noexcept
{
	Exception* ex = static_cast<Exception*>(lp->val);
	for(int i = 0; i < n; i++)
	{
		std::ptrdiff_t adjustedOffset = 0;
		__shim_type_info* catcher = static_cast<__shim_type_info*>(catches[i].val);
		__shim_type_info* thrown = static_cast<__shim_type_info*>(ex->tinfo);
		bool deref = false;
		if(catcher->can_catch(thrown, adjustedOffset, deref))
		{
			if(deref)
			{
				void* ptr = ex->obj;
				ptr = *static_cast<void**>(ptr);
				ex->obj = ptr;
				lp->val = ex;
			}
			if(adjustedOffset != 0)
			{
				ex->obj = __builtin_cheerp_downcast<void,void>(ex->obj, -adjustedOffset);
			}
			lp->sel = catches[i].sel;
			break;
		}
	}
	return lp;
}

}
}
