#ifndef LLVM_EXECUTIONENGINE_FUNCTION_PROXY_H
#define LLVM_EXECUTIONENGINE_FUNCTION_PROXY_H


#if defined(__linux__)
#include <llvm/ExecutionEngine/Mmap32bitAllocator.h>
#endif

#include <llvm/IR/Function.h>
#include <map>
#include <tuple>

class FunctionProxy {
public:
	static FunctionProxy* getProxy(llvm::Function* func)
	{
		map_t::iterator it;
		std::tie(it,std::ignore) = proxies().emplace(func,FunctionProxy(func));
		return &it->second;
	}
	llvm::Function* getFunction()
	{
		return func;
	}
	static void clearMap()
	{
		proxies().~map_t();
		new (&proxies()) map_t();
	}
private:
	llvm::Function* func;
	explicit FunctionProxy(llvm::Function* func): func(func)
	{
	}
	typedef std::pair<llvm::Function*, FunctionProxy> elem_t;
#if defined(__linux__)
	typedef std::map<llvm::Function*, FunctionProxy, std::less<llvm::Function*>, llvm::StdMmap32bitAllocator<elem_t>> map_t;
#else
	typedef std::map<llvm::Function*, FunctionProxy, std::less<llvm::Function*>> map_t;
#endif
	static map_t& proxies()
	{
		static map_t _v;
		return _v;
	}
};
#endif
