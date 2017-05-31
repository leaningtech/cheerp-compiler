#ifndef LLVM_EXECUTIONENGINE_FUNCTION_PROXY_H
#define LLVM_EXECUTIONENGINE_FUNCTION_PROXY_H

#include <llvm/ExecutionEngine/Mmap32bitAllocator.h>

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
private:
	llvm::Function* func;
	explicit FunctionProxy(llvm::Function* func): func(func)
	{
	}
	typedef std::pair<llvm::Function*, FunctionProxy> elem_t;
	typedef std::map<llvm::Function*, FunctionProxy, std::less<llvm::Function*>, llvm::StdMmap32bitAllocator<elem_t>> map_t;
	static map_t& proxies()
	{
		static map_t _v;
		return _v;
	}
};
#endif
