#ifndef LLVM_EXECUTIONENGINE_VIRTUAL_ALLOCATOR_H
#define LLVM_EXECUTIONENGINE_VIRTUAL_ALLOCATOR_H

#include <map>
#include <cstdint>

#include "llvm/Support/Allocator.h"
#include "llvm/Support/raw_ostream.h"

namespace llvm {

class VirtualAllocator : public AllocatorBase<VirtualAllocator> {
public:
	void* toReal(void* virt)
	{
		uintptr_t virti = reinterpret_cast<uintptr_t>(virt);
		auto it = virt_to_real.upper_bound(virti);
		assert(it != virt_to_real.begin() && "the requested address is out of range");
		it--;
		if(it->first + it->second.size <= virti)
		{
			llvm::errs()<<"Translating virt addr "<<virti<<":\n";
			llvm::errs()<<"it->first = "<<it->first<<"\n";
			llvm::errs()<<"it->second.start = "<<it->second.start<<"\n";
			llvm::errs()<<"it->second.size = "<<it->second.size<<"\n";
		}
		assert(it->first + it->second.size > virti && "the requested address is out of range");
		ptrdiff_t offset = virti - it->first;
		return reinterpret_cast<void*>(it->second.start+offset);
	}
	void* toVirtual(void* real)
	{
		uintptr_t reali = reinterpret_cast<uintptr_t>(real);
		auto it = real_to_virt.upper_bound(reali);
		assert(it != real_to_virt.begin() && "the requested address is out of range");
		it--;
		if(it->first + it->second.size <= reali)
		{
			llvm::errs()<<"Translating real addr "<<reali<<":\n";
			llvm::errs()<<"it->first = "<<it->first<<"\n";
			llvm::errs()<<"it->second.start = "<<it->second.start<<"\n";
			llvm::errs()<<"it->second.size = "<<it->second.size<<"\n";
		}
		assert(it->first + it->second.size > reali && "the requested address is out of range");
		ptrdiff_t offset = reali - it->first;
		return reinterpret_cast<void*>(it->second.start+offset);
	}

	LLVM_ATTRIBUTE_RETURNS_NONNULL void *Allocate(size_t size,size_t align) {
		void* ret =  malloc(size);
		uintptr_t reti = reinterpret_cast<uintptr_t>(ret);
		uint32_t padding = (align - next_virt % align) % align;
		next_virt += padding;
		virt_to_real.emplace(next_virt, Page(reti, size));
		real_to_virt.emplace(reti, Page(next_virt, size));
		next_virt += size;
		return ret;
	}
	// Pull in base class overloads.
	using AllocatorBase<VirtualAllocator>::Allocate;

	void Deallocate(const void *Ptr, size_t Size) {
		free(const_cast<void*>(Ptr));
	}

	// Pull in base class overloads.
	using AllocatorBase<VirtualAllocator>::Deallocate;

	void PrintStats() const {}

	VirtualAllocator() {
		virt_to_real.emplace(0,Page(0,8));
		real_to_virt.emplace(0,Page(0,8));
		next_virt = 8;
	}
private:
	struct Page {
		uintptr_t start;
		uint32_t size;
		Page(uintptr_t start, uintptr_t size):start(start),size(size){}
	};
	uintptr_t next_virt;
	std::map<uintptr_t, Page> virt_to_real;
	std::map<uintptr_t, Page> real_to_virt;
};
}
#endif
