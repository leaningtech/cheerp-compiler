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
	typedef std::pair<llvm::Function*, FunctionProxy> elem_t;
#if defined(__linux__)
	typedef std::map<llvm::Function*, FunctionProxy, std::less<llvm::Function*>, llvm::StdMmap32bitAllocator<elem_t>> ProxyMap;
#else
	typedef std::map<llvm::Function*, FunctionProxy, std::less<llvm::Function*>> ProxyMap;
#endif
	static FunctionProxy* getProxy(ProxyMap& map, llvm::Function* func)
	{
		ProxyMap::iterator it;
		std::tie(it,std::ignore) = map.emplace(func,FunctionProxy(func));
		return &it->second;
	}
	llvm::Function* getFunction()
	{
		return func;
	}
private:
	llvm::Function* func;
	explicit FunctionProxy(llvm::Function* func): func(func)
	{
	}
};
#endif
