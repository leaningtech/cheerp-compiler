#include "unwind.h"
#include "private_typeinfo.h"
#include <typeinfo>
#include <cassert>
#include <cstdlib>
#include <utility>
#include <cheerpintrin.h>
#include <cheerp/client.h>

namespace [[cheerp::genericjs]] client {
	class CheerpException: public Error {
	public:
		CheerpException(const String& msg) noexcept;
	};
}
namespace [[cheerp::genericjs]] cheerp {
	class JSException {
		client::Object* inner;
		public:
			JSException(client::Object* e) noexcept: inner(e)
			{
			}
			client::Object* get() noexcept
			{
				return inner;
			}
	};
}
namespace [[cheerp::genericjs]] __cxxabiv1 {

struct
#ifdef __ASMJS__
[[cheerp::wasm]]
#endif
 __cheerp_landingpad
{
	void* val;
	int sel;
#ifdef __ASMJS__
	[[cheerp::wasm]]
#endif
	__cheerp_landingpad() noexcept: val(nullptr), sel(0)
	{
	}
#ifdef __ASMJS__
	[[cheerp::wasm]]
#endif
	__cheerp_landingpad& operator=(const __cheerp_landingpad& o) noexcept
	{
		val = o.val;
		sel = o.sel;
		return *this;
	}

#ifdef __ASMJS__
	[[cheerp::genericjs]]
	void set_val(void* v) noexcept
	{
		set_val_inner(__builtin_cheerp_pointer_offset(v));
	}
	[[cheerp::wasm]]
	void set_val_inner(size_t v) noexcept
	{
		val = reinterpret_cast<void*>(v);
	}
#else
	void set_val(void* v) noexcept
	{
		val = v;
	}
#endif
};

template<typename T>
class IdAllocator
{
	T* store;
	bool* slots;
	int len;

	int find_free_id()
	{
		int id = 0;
		for (; id < len; id++)
		{
			if (!slots[id])
				break;
		}
		if (id == len)
		{
			len = len*2;
			store = static_cast<T*>(realloc(store, len));
			slots = static_cast<bool*>(realloc(slots, len));
		}
		return id;
	};
public:
	IdAllocator(): slots(new bool[16]), len(16)
	{
		store = static_cast<T*>(malloc(16*sizeof(T)));
	}
	template<typename... Args>
	T* allocate(Args&&... args)
	{
		int id = find_free_id();
		T* ret = new(&store[id]) T(cheerp::forward<Args>(args)...);
		slots[id] = true;
		return ret;
	}
	void deallocate(T* t)
	{
		int id = get_id(t);
		store[id].~T();
		slots[id] = false;
	}
	int get_id(T* t)
	{
		return t - store;
	}
	T* get_object(int id)
	{
		return &store[id];
	}
	T* get_object(void* unwind)
	{
		int id = __builtin_cheerp_pointer_offset(unwind);
		return &store[id];
	}
};

struct Exception
{
	static IdAllocator<Exception> allocator;

	client::Object* jsObj;
	void* obj;
	void* adjustedPtr;
	const std::type_info* tinfo;
	void (*dest)(void*);
	int refCount;
	int handlerCount;
	Exception* global_next;
	Exception* next;
	Exception* primary; // null if this is the primary exception
	Exception(void* obj, const std::type_info* tinfo, void(*dest)(void*), Exception* primary = nullptr) noexcept
		: obj(obj), adjustedPtr(nullptr), tinfo(tinfo), dest(dest), refCount(1), 
		  handlerCount(0), next(nullptr), primary(primary)
	{
	}
	~Exception() noexcept
	{
		run_dest();
	}
	template<typename... Args>
	static Exception* allocate(Args&&... args) noexcept
	{
		return allocator.allocate(cheerp::forward<Args>(args)...);
	}
	void deallocate() noexcept
	{
		allocator.deallocate(this);
	}
	void set_jsObj(client::Object* o) noexcept
	{
		jsObj = o;
	}
	void run_dest() noexcept
	{
		if(dest)
			dest(obj);
		dest = nullptr;
	}
	int incRef() noexcept
	{
		refCount++;
		return refCount;
	}
	int decRef() noexcept
	{
		refCount--;
		return refCount;
	}
	int incHandlerCount() noexcept
	{
		handlerCount++;
		return handlerCount;
	}
	int decHandlerCount() noexcept
	{
		handlerCount--;
		return handlerCount;
	}
	bool isDependent() noexcept
	{
		return primary != nullptr;
	}
};
IdAllocator<Exception> Exception::allocator;


// Global variable to store the currently thrown exception.
// This is needed just by the personality, which will reset it to null immediately
// Ideally this would be contained in the actual CheerpException object
static Exception* current_exception = nullptr;

// Global list of currently active (thrown or caught) exceptions
static Exception* thrown_exceptions = nullptr;

// Global counter of currently uncaught exceptions
static int uncaughtExceptions = 0;

static Exception* find_exception_from_unwind_ptr(void* unwind)
{
	return Exception::allocator.get_object(unwind);
}

extern "C" {

void __cxa_decrement_exception_refcount(void* obj) noexcept
{
	if(obj != nullptr)
	{
		Exception* ex = find_exception_from_unwind_ptr(obj);
		if(ex->decRef() == 0)
			ex->deallocate();
	}
}
void __cxa_increment_exception_refcount(void* obj) noexcept
{
	if(obj != nullptr)
	{
		find_exception_from_unwind_ptr(obj)->incRef();
	}
}


[[noreturn]]
static void do_throw(Exception* ex)
{
	current_exception = ex;
	uncaughtExceptions += 1;

	client::CheerpException* wrapper = new client::CheerpException(ex->tinfo->name());
	ex->set_jsObj(wrapper);
	__builtin_cheerp_throw(wrapper);
}

[[noreturn]]
__attribute((noinline))
void
__cxa_throw(void *thrown_object, std::type_info *tinfo, void (*dest)(void *)) {
	Exception* ex = Exception::allocate(thrown_object, tinfo, dest);
	do_throw(ex);
}

__attribute((noinline))
void*
__cxa_begin_catch(void* unwind_arg) noexcept
{
	Exception* ex = find_exception_from_unwind_ptr(unwind_arg);
	// Increment the handler count, removing the flag about being rethrown
	ex->handlerCount = ex->handlerCount < 0 ?
		-ex->handlerCount + 1 : ex->handlerCount + 1;
	//  place the exception on the top of the stack if it's not already
	//    there by a previous rethrow
	if (ex != thrown_exceptions)
	{
		ex->next = thrown_exceptions;
		thrown_exceptions = ex;
	}
	uncaughtExceptions -= 1;
	return ex->adjustedPtr;
}

__attribute((noinline))
void*
__cxa_get_exception_ptr(void* unwind_arg) noexcept
{
	return find_exception_from_unwind_ptr(unwind_arg)->adjustedPtr;
}

__attribute((noinline))
void __cxa_end_catch() noexcept {
	Exception* ex = thrown_exceptions;

	if (ex->handlerCount < 0)
	{
		//  The exception has been rethrown by __cxa_rethrow, so don't delete it
		if (0 == ex->incHandlerCount())
		{
			//  Remove from the chain of thrown exceptions
			thrown_exceptions = ex->next;
			// but don't destroy
		}
		// Keep handlerCount negative in case there are nested catch's
		//   that need to be told that this exception is rethrown.  Don't
		//   erase this rethrow flag until the exception is recaught.
	}
	else
	{
		// The native exception has not been rethrown
		if (0 == ex->decHandlerCount())
		{
			//  Remove from the chain of thrown exceptions
			thrown_exceptions = ex->next;
			// Destroy this exception, being careful to distinguish
			//    between dependent and primary exceptions
			if (ex->isDependent())
			{
				// Reset ex to primary and deallocate the dependent exception
				Exception* dep = ex;
				ex = dep->primary;
				dep->deallocate();
			}
			// Destroy the primary exception only if its refCount goes to 0
			__cxa_decrement_exception_refcount(ex);
		}
	}
}

[[noreturn]]
__attribute((noinline))
void __cxa_rethrow() {
	Exception* ex = thrown_exceptions;
	ex->handlerCount = -ex->handlerCount;
	do_throw(ex);
}

void* __cxa_current_primary_exception() noexcept
{
	Exception* ex = thrown_exceptions;
	if(ex == nullptr)
		return ex;
	if(ex->primary != nullptr)
		ex = ex->primary;
	__cxa_increment_exception_refcount(ex);
	return ex;
}

[[noreturn]]
void __cxa_rethrow_primary_exception(void* obj)
{
	Exception* ex = find_exception_from_unwind_ptr(obj);
	__cxa_increment_exception_refcount(ex);
	Exception* dep = Exception::allocate(ex->obj, ex->tinfo, nullptr, ex);
	do_throw(dep);
}

int __cxa_uncaught_exceptions()
{
	return uncaughtExceptions;
}

struct __cheerp_clause
{
	std::type_info* val;
	int sel;
};

struct
#ifdef __ASMJS__
[[cheerp::wasm]]
#endif
can_catch_ret {
	std::ptrdiff_t adjustedOffset;
	bool can_catch;
	bool deref;
};
#ifdef __ASMJS__
[[cheerp::wasm]]
#endif
can_catch_ret can_catch(const __shim_type_info* catcher, const __shim_type_info* thrown)
{
	can_catch_ret ret {0, false, false};
	ret.can_catch = catcher->can_catch(thrown, ret.adjustedOffset, ret.deref);
	return ret;
}

__attribute((noinline))
 __cheerp_landingpad*
__gxx_personality_v0
                    (client::Object* obj, __cheerp_clause* catches, int n) noexcept
{
	static bool reent = false;
#ifdef __ASMJS__
	[[cheerp::wasm]]
#endif
	static __cheerp_landingpad lp;

	if(reent)
		__builtin_cheerp_throw(obj);

	reent = true;

	bool native;
	asm("%1 instanceof CheerpException" : "=r"(native) : "r"(obj));

	if(!native)
	{
		for(int i = 0; i < n; i++)
		{
			// Is the landingpad catching a foreign exception?
			// if so, fabricate an Exception object.
			if(static_cast<std::type_info*>(catches[i].val) == &typeid(cheerp::JSException))
			{
				cheerp::JSException* foreign = new cheerp::JSException(obj);
				Exception* ex = Exception::allocate(foreign, &typeid(cheerp::JSException), nullptr);
				ex->set_jsObj(obj);
				current_exception = ex;
				uncaughtExceptions += 1;
				// From now on, we treat this as a native exception;
				native = true;
				break;
			}
		}
	}
	if(!native)
	{
		lp = __cheerp_landingpad();
		return &lp;
	}

	Exception* ex = current_exception;
	lp = __cheerp_landingpad();
	lp.set_val(ex);

	for(int i = 0; i < n; i++)
	{
		if(catches[i].val == nullptr)
		{
			// This is a catch(...) clause
			lp.sel = catches[i].sel;
			break;
		}
		const __shim_type_info* catcher = static_cast<const __shim_type_info*>(catches[i].val);
		const __shim_type_info* thrown = static_cast<const __shim_type_info*>(ex->tinfo);
		can_catch_ret cc = can_catch(catcher, thrown);
		if(cc.can_catch)
		{
			ex->adjustedPtr = ex->obj;
			if(cc.deref)
			{
				ex->adjustedPtr = *static_cast<void**>(ex->adjustedPtr);
			}
			if(cc.adjustedOffset != 0 && ex->adjustedPtr != nullptr)
			{
				ex->adjustedPtr = __builtin_cheerp_downcast<void,void>(ex->adjustedPtr, -cc.adjustedOffset);
			}
			lp.sel = catches[i].sel;
			break;
		}
	}
	reent = false;
	return &lp;
}

}
}
