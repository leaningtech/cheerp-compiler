//===-- Cheerp/PreExecute.h - Execute run-time init at compile time ------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2015 Leaning Technologies
//
//===---------------------------------------------------------------------===//

#ifndef _CHEERP_PREEXECUTE_H
#define _CHEERP_PREEXECUTE_H

#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"

#include <map>

namespace cheerp
{

class AllocData
{
public:
    llvm::GlobalVariable *globalValue;
    llvm::Type *allocType;
    size_t size;

    AllocData() : globalValue(nullptr), allocType(nullptr), size(0) { }
};

class PreExecute : public llvm::ModulePass
{
public:
    static PreExecute *currentPreExecutePass;
    static char ID;

    llvm::ExecutionEngine *currentEE;
    llvm::Module *currentModule;

    std::map<llvm::GlobalVariable *, llvm::Constant *>  modifiedGlobals;
    std::map<char *, AllocData> typedAllocations;

    explicit PreExecute() : llvm::ModulePass(ID) {
    }

    const char* getPassName() const override;
    bool runOnModule(llvm::Module& m) override;
    bool runOnConstructor(const llvm::Target* target, const std::string& triple, llvm::Module& m, llvm::Function* c);

#if defined(__linux__)
    void recordStore(void* Addr);
    void recordTypedAllocation(llvm::Type *type, size_t size, char *buf) {
        AllocData data;
        data.allocType = type;
        data.size = size;
        typedAllocations.insert(std::make_pair(buf, data));
    };
private:
    llvm::Constant* findPointerFromGlobal(const llvm::DataLayout* DL,
            llvm::Type* memType, llvm::GlobalValue* GV, char* GlobalStartAddr,
            char* StoredAddr, llvm::Type* Int32Ty);

    llvm::GlobalValue* getGlobalForMalloc(const llvm::DataLayout* DL,
            char* StoredAddr, char*& MallocStartAddress);

    llvm::Constant* computeInitializerFromMemory(const llvm::DataLayout* DL,
            llvm::Type* memType, char* Addr);
#endif
};

inline llvm::ModulePass* createPreExecutePass() {
    return new PreExecute();
}

}

#endif // _CHEERP_PREEXECUTE_H
