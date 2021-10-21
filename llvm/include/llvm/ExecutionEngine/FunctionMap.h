#ifndef LLVM_EXECUTIONENGINE_FUNCTION_MAP_H
#define LLVM_EXECUTIONENGINE_FUNCTION_MAP_H

#include <llvm/IR/Function.h>
#include <map>
#include <tuple>

class FunctionMapBase {
public:
	 virtual llvm::Function* getFunction(void* addr) = 0;
	 virtual void* getAddress(llvm::Function* f) = 0;
	 virtual ~FunctionMapBase(){};
};

class VirtualFunctionMap: public FunctionMapBase {
public:
	 llvm::Function* getFunction(void* addr) override
	 {
		 uintptr_t addri = reinterpret_cast<uintptr_t>(addr);
		 auto it = rev_map.find(addri);
		 assert(it != rev_map.end() && "This function address does not exists");
		 return it->second;
	 }
	 void* getAddress(llvm::Function* f) override
	 {
		std::map<llvm::Function*,uint32_t>::iterator it;
		bool inserted;
		std::tie(it,inserted) = map.emplace(f,next_addr+1);
		if (inserted)
		{
			next_addr ++;
			rev_map.emplace(next_addr, f);
		}
		return reinterpret_cast<void*>(it->second);
	 }
private:
	 std::map<llvm::Function*, uint32_t> map;
	 std::map<uint32_t, llvm::Function*> rev_map;
	 uint32_t next_addr = 1;

};
class DirectFunctionMap: public FunctionMapBase {
public:
	 llvm::Function* getFunction(void* addr) override
	 {
		 return reinterpret_cast<llvm::Function*>(addr);
	 }
	 void* getAddress(llvm::Function* f) override
	 {
		return f;
	 }
};
#endif
