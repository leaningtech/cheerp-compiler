#ifndef LLVM_EXECUTIONENGINE_VIRTUAL_ADDRESS_MAP_H
#define LLVM_EXECUTIONENGINE_VIRTUAL_ADDRESS_MAP_H

#include <map>
#include <cstdint>

#include "llvm/Support/Allocator.h"
#include "llvm/Support/raw_ostream.h"

namespace llvm {

class AddressMapBase {
public:
	virtual void* toReal(void* virt) = 0;
	virtual void* toVirtual(void* real) = 0;
	virtual void map(void* ptr, size_t size) = 0;
	virtual void unmap(void* ptr) = 0;

	virtual ~AddressMapBase(){};
};

class VirtualAddressMap : public AddressMapBase {
public:
	struct Page {
		uintptr_t start;
		uint32_t size;
		Page(uintptr_t start, uintptr_t size):start(start),size(size){}
	};

	void* toReal(void* virt) override
	{
		uintptr_t virti = reinterpret_cast<uintptr_t>(virt);
		auto it = find_start(virt_to_real, virti);
		assert(it != virt_to_real.end() && "the requested address is out of range");
		ptrdiff_t offset = virti - it->first;
		return reinterpret_cast<void*>(it->second.start+offset);
	}
	void* toVirtual(void* real) override
	{
		uintptr_t reali = reinterpret_cast<uintptr_t>(real);
		auto it = find_start(real_to_virt, reali);
		assert(it != real_to_virt.end() && "the requested address is out of range");
		ptrdiff_t offset = reali - it->first;
		return reinterpret_cast<void*>(it->second.start+offset);
	}

	void map(void* ptr, size_t size) override {
		uintptr_t reti = reinterpret_cast<uintptr_t>(ptr);
		uint32_t padding = (8 - next_virt % 8) % 8;
		next_virt += padding;
		virt_to_real.emplace(next_virt, Page(reti, size));
		real_to_virt.emplace(reti, Page(next_virt, size));
		next_virt += size;
	}

	void unmap(void* ptr) override {
		auto it = real_to_virt.find(reinterpret_cast<uintptr_t>(ptr));
		assert(it != real_to_virt.end());
		virt_to_real.erase(it->second.start);
		real_to_virt.erase(it);
	}

	VirtualAddressMap() {
		virt_to_real.emplace(0,Page(0,8));
		real_to_virt.emplace(0,Page(0,8));
		next_virt = 8;
	}
private:
	static std::map<uintptr_t, Page>::const_iterator find_start(const std::map<uintptr_t, Page>& mapping,  uintptr_t addr) {
		auto it = mapping.upper_bound(addr);
		if (it == mapping.begin())
			return mapping.end();
		it--;
		if (it->first + it->second.size <= addr) {
#if DEBUG
			llvm::errs()<<"Translating addr "<<addr<<":\n";
			llvm::errs()<<"it->first = "<<it->first<<"\n";
			llvm::errs()<<"it->second.start = "<<it->second.start<<"\n";
			llvm::errs()<<"it->second.size = "<<it->second.size<<"\n";
#endif
			return mapping.end();
		}
		return it;
	}

	uintptr_t next_virt;
	std::map<uintptr_t, Page> virt_to_real;
	std::map<uintptr_t, Page> real_to_virt;
};
class DirectAddressMap : public AddressMapBase {
public:
	void* toReal(void* virt) override
	{
		return virt;
	}
	void* toVirtual(void* real) override
	{
		return real;
	}

	void map(void* ptr, size_t size) override {
	}
	void unmap(void* ptr) override {
	}

};
}
#endif
