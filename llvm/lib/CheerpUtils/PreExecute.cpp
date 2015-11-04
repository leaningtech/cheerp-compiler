//===-- PreExecute.cpp - Execute run-time init at compile time -----------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2015 Leaning Technologies
//
//===---------------------------------------------------------------------===//

#define DEBUG_TYPE "pre-execute"
#include "llvm/Cheerp/PreExecute.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/ExecutionEngine/Interpreter.h"
#include "llvm/IR/Constants.h"
#include "llvm/Pass.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

namespace cheerp {

PreExecute* PreExecute::currentPreExecutePass = NULL;

const char* PreExecute::getPassName() const
{
    return "CheerpPreExecute";
}

char PreExecute::ID = 0;

static void StoreListener(void* Addr)
{
    PreExecute::currentPreExecutePass->recordStore(Addr);
}

static GenericValue pre_execute_malloc(FunctionType *FT,
        const std::vector<GenericValue> &Args) {
    size_t size=(size_t)(Args[0].IntVal.getLimitedValue());
    void* ret=malloc(size);
    return GenericValue(ret);
}

static GenericValue pre_execute_element_distance(FunctionType *FT, const std::vector<GenericValue> &Args)
{
    Type* elementType = FT->getParamType(0)->getPointerElementType();
    uint32_t elementSize = PreExecute::currentPreExecutePass->currentModule->getDataLayout()->getTypeAllocSize(elementType);
    GenericValue GV;
    GV.IntVal = APInt(32, elementSize);
    return GV;
}

static GenericValue pre_execute_allocate(FunctionType *FT,
                                         const std::vector<GenericValue> &Args) {
  size_t size=(size_t)(Args[0].IntVal.getLimitedValue());
  // Allocate twice the space to account for pointers, which are twice as wide
  void* ret=malloc(size);
  memset(ret, 0, size);
  // Register this allocations in the pass
  PreExecute::currentPreExecutePass->recordTypedAllocation(FT->getReturnType()->getPointerElementType(), size, (char*)ret);
  return GenericValue(ret);
}

static GenericValue pre_execute_cast(FunctionType *FT,
                                     const std::vector<GenericValue> &Args) {
  return Args[0];
}

static GenericValue pre_execute_memcpy(FunctionType *FT,
                                       const std::vector<GenericValue> &Args) {
  // Support fully typed memcpy
  memcpy(GVTOP(Args[0]), GVTOP(Args[1]),
         (size_t)(Args[2].IntVal.getLimitedValue()));

  GenericValue GV;
  GV.IntVal = 0;
  return GV;
}

static GenericValue pre_execute_downcast(FunctionType *FT,
                                         const std::vector<GenericValue> &Args) {
    // We need to apply the offset in bytes using the bases metadata
    Type* derivedType = FT->getReturnType()->getPointerElementType();
    Type* baseType = FT->getParamType(0)->getPointerElementType();
    int baseOffset = Args[1].IntVal.getLimitedValue();
    uintptr_t curByteOffset=0;
    uint32_t curBaseOffset=baseOffset;
    StructType* curType=cast<StructType>(derivedType);
    while(curType!=baseType)
    {
        if(curBaseOffset==0)
            break;
        //Decrease 1 for this base
        curBaseOffset--;
        uint32_t firstBase, baseCount;
        TypeSupport::getBasesInfo(*PreExecute::currentPreExecutePass->currentModule, curType, firstBase, baseCount);
        assert(curBaseOffset < baseCount);
        uint32_t nextBaseIndex = firstBase;
        for(;nextBaseIndex<(firstBase+baseCount);nextBaseIndex++)
        {
            uint32_t subFirstBase,subBaseCount;
            StructType* nextType = cast<StructType>(curType->getElementType(nextBaseIndex));
            bool ret=TypeSupport::getBasesInfo(*PreExecute::currentPreExecutePass->currentModule, nextType,
                        subFirstBase, subBaseCount);
            assert(ret);
            if (curBaseOffset < subBaseCount)
            {
                curType = nextType;
                break;
            }
            curBaseOffset -= subBaseCount;
        }
        // Skip the offset untils the first base
        curType = cast<StructType>(curType->getElementType(nextBaseIndex));
        curByteOffset+=PreExecute::currentPreExecutePass->currentModule->getDataLayout()->
                getStructLayout(curType)->getElementOffset(firstBase);
    }
    uintptr_t AddrValue = reinterpret_cast<uintptr_t>(GVTOP(Args[0]));
    AddrValue-=curByteOffset;
    return GenericValue(reinterpret_cast<void*>(AddrValue));
}

static void* LazyFunctionCreator(const std::string& funcName)
{
    if(funcName=="malloc")
        return (void*)(void(*)())pre_execute_malloc;
    if(strncmp(funcName.c_str(), "llvm.cheerp.cast.user.", strlen("llvm.cheerp.cast.user."))==0)
        return (void*)(void(*)())pre_execute_cast;
    if(strncmp(funcName.c_str(), "llvm.cheerp.downcast.", strlen("llvm.cheerp.downcast."))==0)
        return (void*)(void(*)())pre_execute_downcast;
    if(strncmp(funcName.c_str(), "llvm.cheerp.allocate.", strlen("llvm.cheerp.allocate."))==0)
        return (void*)(void(*)())pre_execute_allocate;
    if(strncmp(funcName.c_str(), "llvm.cheerp.element.distance.", strlen("llvm.cheerp.element.distance."))==0)
        return (void*)(void(*)())pre_execute_element_distance;
    if(strncmp(funcName.c_str(), "llvm.memcpy.", strlen("llvm.memcpy."))==0)
        return (void*)(void(*)())pre_execute_memcpy;
    return NULL;
}

void PreExecute::recordStore(void* Addr)
{
    // Look for the address in the globals, if found keep note of this
    const GlobalValue* GV=currentEE->getGlobalValueAtAddress(Addr);
    if(!GV)
        return;
    if(const GlobalVariable* GVar = dyn_cast<GlobalVariable>(GV))
    {
        modifiedGlobals.insert(std::make_pair(const_cast<GlobalVariable*>(GVar),nullptr));
        return;
    }
}

bool getTypeSafeGepForAddress(SmallVector<Constant*, 4>& Indices, Type* Int32Ty, const DataLayout* DL,
                    Type* startType, Type* endType, uint32_t Offset)
{
    Type* curType = startType;
    while(Offset!=0 || curType!=endType)
    {
        // If the offset is not zero, we must deal with an aggregate
        if(ArrayType* AT=dyn_cast<ArrayType>(curType))
        {
            Type* ET = AT->getElementType();
            uint32_t elementSize = DL->getTypeAllocSize(ET);
            uint32_t elementIndex = Offset/elementSize;
            Indices.push_back(ConstantInt::get(Int32Ty, elementIndex));
            Offset %= elementSize;
            curType = ET;
        }
        else if (StructType* ST=dyn_cast<StructType>(curType))
        {
            const StructLayout* SL = DL->getStructLayout(ST);
            uint32_t elementIndex = SL->getElementContainingOffset(Offset);
            Indices.push_back(ConstantInt::get(Int32Ty, elementIndex));
            Offset -= SL->getElementOffset(elementIndex);
            curType = ST->getElementType(elementIndex);
        }
        else
        {
            return false;
        }
    }
    return true;
}

Constant* PreExecute::findPointerFromGlobal(const DataLayout* DL, Type* memType, GlobalValue* GV,
                        char* GlobalStartAddr, char* StoredAddr, Type* Int32Ty)
{
    // Build a type safe GEP to the right type at the right offset
    uintptr_t Offset = StoredAddr - GlobalStartAddr;
    llvm::SmallVector<Constant*, 4> Indices;
    // This is needed to dereference global
    Indices.push_back(ConstantInt::get(Int32Ty, 0));
    bool success=getTypeSafeGepForAddress(Indices, Int32Ty, DL, GV->getType()->getPointerElementType(),
                    memType->getPointerElementType(), Offset);
    if (!success)
        return NULL;
    // ExecutionEngine has given us a constant global value, but we need it non-const
    Constant* GEP = ConstantExpr::getGetElementPtr(GV->getType()->getPointerElementType(), GV, Indices);
    assert(GEP->getType() == memType);
    return GEP;
}

GlobalValue* PreExecute::getGlobalForMalloc(const DataLayout* DL, char* StoredAddr, char*& MallocStartAddress)
{
    auto it = typedAllocations.upper_bound(StoredAddr);
    if (it == typedAllocations.begin())
        return NULL;
    --it;
    // Verify that the address is in the memory block
    if (StoredAddr < it->first)
        return NULL;
    AllocData& allocData = it->second;
    // The edge of the allocation is a valid pointer
    if (StoredAddr > (it->first + allocData.size))
        return NULL;
    MallocStartAddress = it->first;
    if (allocData.globalValue)
        return allocData.globalValue;
    // We need to promote this memory to a globalvalue
    // Make it an array
    // TODO: Support plain objects
    uint32_t elementSize = DL->getTypeAllocSize(allocData.allocType);
    Type* newGlobalType = ArrayType::get(allocData.allocType, allocData.size/elementSize);
    // Build an initializer
    Constant* init=computeInitializerFromMemory(DL, newGlobalType, it->first);
    allocData.globalValue = new GlobalVariable(*currentModule, newGlobalType, false,
                        GlobalValue::InternalLinkage, init, "promotedMalloc");
    return allocData.globalValue;
}

Constant* PreExecute::computeInitializerFromMemory(const DataLayout* DL, Type* memType, char* Addr)
{
    if (IntegerType* IT=dyn_cast<IntegerType>(memType))
    {
        if (IT->getBitWidth() > 32 || IT->getBitWidth()%8)
            return NULL;
        // Assume little endian
        uint32_t integerVal = 0;
        memcpy(&integerVal, Addr, IT->getBitWidth()/8);
        return ConstantInt::get(IT, integerVal);
    }
    else if (memType->isFloatTy())
    {
        // Assume little endian
        float floatVal;
        memcpy(&floatVal, Addr, 4);
        return ConstantFP::get(memType, floatVal);
    }
    else if (memType->isDoubleTy())
    {
        // Assume little endian
        double doubleVal;
        memcpy(&doubleVal, Addr, 8);
        return ConstantFP::get(memType, doubleVal);
    }
    else if (StructType* ST=dyn_cast<StructType>(memType))
    {
        // Let's return a constant struct
        SmallVector<Constant*, 4> Elements;
        const StructLayout* SL = DL->getStructLayout(ST);
        for (uint32_t i=0;i<ST->getNumElements();i++)
        {
            char* elementAddr = Addr + SL->getElementOffset(i);
            Constant* elem = computeInitializerFromMemory(DL, ST->getElementType(i), elementAddr);
            Elements.push_back(elem);
        }
        return ConstantStruct::get(ST, Elements);
    }
    else if (ArrayType* AT=dyn_cast<ArrayType>(memType))
    {
        uint32_t numElements = AT->getNumElements();
        Type* elementType = AT->getElementType();
        uint32_t elementSize = DL->getTypeAllocSize(elementType);
        SmallVector<Constant*, 4> Elements;
        for(uint32_t i=0;i<numElements;i++)
        {
            char* elementAddr = Addr + i*elementSize;
            Constant* elem = computeInitializerFromMemory(DL, elementType, elementAddr);
            Elements.push_back(elem);
        }
        return ConstantArray::get(AT, Elements);
    }
    else if (PointerType* PT=dyn_cast<PointerType>(memType))
    {
        // Pointers are tricky, we need to find the global containing the pointer
        // Start with reading the pointer from memory
        char* StoredAddr = *((char**)Addr);
        if (StoredAddr==NULL)
            return ConstantPointerNull::get(PT);
        Type* Int32Ty = IntegerType::get(currentModule->getContext(), 32);
        const GlobalValue* GV = currentEE->getGlobalValueAtAddress(StoredAddr);
        if (GV)
        {
            char* GlobalStartAddr = (char*)currentEE->getPointerToGlobal(GV);
            Constant* ret=findPointerFromGlobal(DL, memType, const_cast<GlobalValue*>(GV),
                                GlobalStartAddr,  StoredAddr, Int32Ty);
            return ret;
        }
        // Look inside type safe allocated memory
        char* MallocStartAddress;
        GV = getGlobalForMalloc(DL, StoredAddr, MallocStartAddress);
        if (GV)
        {
            Constant* ret=findPointerFromGlobal(DL, memType, const_cast<GlobalValue*>(GV),
                                MallocStartAddress, StoredAddr, Int32Ty);
            return ret;
        }
        return NULL;
    }
    return NULL;
}

bool PreExecute::runOnModule(Module& m)
{
    currentPreExecutePass = this;
    currentModule = &m;
    bool Changed = false;
    std::string Error;
    const Target *TheTarget = TargetRegistry::lookupTarget(sys::getDefaultTargetTriple(), Error);
    TargetMachine* TM = TheTarget->createTargetMachine(sys::getDefaultTargetTriple(), "", "",
                    TargetOptions());
    EngineBuilder builder(nullptr);
    builder.setOptLevel(CodeGenOpt::Default);
    builder.setErrorStr(&Error);
    builder.setVerifyModules(true);

    currentEE = builder.create(TM);
    currentEE->InstallStoreListener(StoreListener);
    currentEE->InstallLazyFunctionCreator(LazyFunctionCreator);
    GlobalVariable * constructorVar = m.getGlobalVariable("llvm.global_ctors");
    if (constructorVar)
    {
        // Random things which may go boom
        if ( !constructorVar->hasInitializer() ||
            !isa<ConstantArray>( constructorVar->getInitializer() ) )
            return Changed;
        
        const ConstantArray * constructors = cast<ConstantArray>( constructorVar->getInitializer() );

        for (ConstantArray::const_op_iterator it = constructors->op_begin();
             it != constructors->op_end(); ++it)
        {
            Function* func = cast<Function>( cast<Constant>(*it)->getAggregateElement(1) );
            currentEE->runFunction(func, std::vector< GenericValue >());
        }
    }

    // Compute new initializer for the modified globals
    for(auto& it: modifiedGlobals)
    {
        GlobalVariable* GV = it.first;
        void* Addr = currentEE->getPointerToGlobal(GV);
        Constant* newInit = computeInitializerFromMemory(m.getDataLayout(),
                                GV->getType()->getPointerElementType(), (char*)Addr);
        assert(newInit);
        it.second = newInit;
        Changed = true;
    }

    // Free the module from the ExecutionEngine
    currentEE->removeModule(&m);
    delete currentEE;
    delete TM;

    // Set new initializers for the modified globals
    for(auto& it: modifiedGlobals)
    {
        assert(it.second);
        it.first->setInitializer(it.second);
    }

    // Delete global constructors
    constructorVar->eraseFromParent();

    currentPreExecutePass = NULL;
    currentModule = NULL;
    currentEE = NULL;
    return Changed;
}

}

using namespace cheerp;

INITIALIZE_PASS_BEGIN(PreExecute, "PreExecute", "Execute run-time init at compile time",
        false, false)
INITIALIZE_PASS_END(PreExecute, "PreExecute", "Execute run-time init at compile time",
        false, false)
