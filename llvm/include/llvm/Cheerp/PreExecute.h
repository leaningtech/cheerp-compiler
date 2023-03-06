//===-- Cheerp/PreExecute.h - Execute run-time init at compile time ------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2015-2023 Leaning Technologies
//
//===---------------------------------------------------------------------===//

#ifndef _CHEERP_PREEXECUTE_H
#define _CHEERP_PREEXECUTE_H

#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Cheerp/DeterministicUnorderedMap.h"
#include "llvm/Cheerp/Utility.h"

#include <map>
#include <vector>
#include <memory>

namespace cheerp
{

class AllocData
{
public:
    llvm::GlobalVariable *globalValue;
    llvm::Type *allocType;
    size_t size;
    bool hasCookie;
    bool asmjs;

    AllocData() : globalValue(nullptr), allocType(nullptr), size(0), hasCookie(false), asmjs(false) { }
};

class Allocator
{
    std::vector<std::unique_ptr<char[]>> allocations;
    llvm::AddressMapBase& mapping;
public:
    Allocator(llvm::AddressMapBase& mapping): mapping(mapping) {}
    void* allocate(size_t size)
    {
        auto memory = std::make_unique<char[]>(size);
        void* ret = memory.get();
        mapping.map(ret, size + 4);
        allocations.push_back(std::move(memory));
	  return ret;
    }
    void deallocate()
    {
        for (auto& a: allocations)
        {
            mapping.unmap(a.get());
        }
	  allocations.clear();
    }
    ~Allocator()
    {
        deallocate();
    }
};

class PreExecute
{
public:
    static PreExecute *currentPreExecutePass;

    llvm::ExecutionEngine *currentEE;
    llvm::Module *currentModule;
    std::unique_ptr<Allocator> allocator;

    DeterministicUnorderedMap<llvm::GlobalVariable *, llvm::Constant *, RestrictionsLifted::NoErasure>  modifiedGlobals;
    std::map<char *, AllocData> typedAllocations;

    explicit PreExecute(){
    }
    
    bool runOnModule(llvm::Module& m);
    bool runOnConstructor(llvm::Module& m, llvm::Function* c);

    void recordStore(void* Addr);
    void recordTypedAllocation(llvm::Type *type, size_t size, char *buf, bool hasCookie, bool asmjs) {
        AllocData data;
        data.allocType = type;
        data.size = size;
        data.hasCookie = hasCookie;
        data.asmjs = asmjs;
        typedAllocations.insert(std::make_pair(buf, data));
    };
    void releaseTypedAllocation(char* buf) {
        auto it = typedAllocations.find(buf);
        assert(it!=typedAllocations.end() && "There is no typed allocation recorded with this address");
        typedAllocations.erase(it);
    }
private:
    llvm::Constant* findPointerFromGlobal(const llvm::DataLayout* DL,
            llvm::Type* memType, llvm::GlobalValue* GV, char* GlobalStartAddr,
            char* StoredAddr, llvm::Type* Int32Ty);

    llvm::GlobalValue* getGlobalForMalloc(const llvm::DataLayout* DL,
            char* StoredAddr, char*& MallocStartAddress);

    llvm::Constant* computeInitializerFromMemory(const llvm::DataLayout* DL,
            llvm::Type* memType, char* Addr, bool asmjs);
};

class PreExecutePass : public llvm::PassInfoMixin<PreExecutePass> {
public:
	llvm::PreservedAnalyses run(llvm::Module& M, llvm::ModuleAnalysisManager& MAM)
	{
		cheerp::PreExecute inner;
		if (!inner.runOnModule(M))
			return llvm::PreservedAnalyses::all();
		return llvm::PreservedAnalyses::none();
	}
	static bool isRequired() { return true;}
};
}

#endif // _CHEERP_PREEXECUTE_H
