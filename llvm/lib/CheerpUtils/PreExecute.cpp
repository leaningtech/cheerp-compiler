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
#include "llvm/ExecutionEngine/FunctionMap.h"
#include "llvm/IR/Constants.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Target/TargetSubtargetInfo.h"
#include <string.h>
#include <algorithm>

//#define DEBUG_PRE_EXECUTE 1

using namespace llvm;

static cl::opt<bool> PreExecuteMain("cheerp-preexecute-main", cl::desc("Run main/webMain in the PreExecuter step") );

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
static void AllocaListener(Type* Ty,uint32_t Size, void* Addr)
{
    PreExecute::currentPreExecutePass->recordTypedAllocation(Ty, Size, (char*)Addr);
}
static void RetListener(const std::vector<std::unique_ptr<char[]>>& allocas)
{
    for (const auto& a: allocas)
    {
        PreExecute::currentPreExecutePass->releaseTypedAllocation(a.get());
    }
}

static GenericValue pre_execute_malloc(FunctionType *FT,
        const std::vector<GenericValue> &Args) {
    size_t size=(size_t)(Args[0].IntVal.getLimitedValue());
    ExecutionEngine *currentEE = PreExecute::currentPreExecutePass->currentEE;
    void* ret = PreExecute::currentPreExecutePass->allocator->allocate(size);
#ifdef DEBUG_PRE_EXECUTE
    llvm::errs() << "Allocating " << ret << " of size " << size << "\n";
#endif
    return currentEE->RPTOGV(ret);
}

static GenericValue pre_execute_element_distance(FunctionType *FT, const std::vector<GenericValue> &Args)
{
    Type* type = FT->getParamType(0)->getPointerElementType();
    llvm::Module *module = PreExecute::currentPreExecutePass->currentModule;
    uint32_t elementSize = module->getDataLayout()->getTypeAllocSize(type);
    GenericValue GV;
    GV.IntVal = APInt(32, elementSize);
    return GV;
}

static GenericValue pre_execute_pointer_base(FunctionType *FT,
                                         const std::vector<GenericValue> &Args) {
  ExecutionEngine *currentEE = PreExecute::currentPreExecutePass->currentEE;
  char *p = (char *)(currentEE->GVTORP(Args[0]));
  const GlobalValue* GV = currentEE->getGlobalValueAtAddress(p);
  if (GV)
  {
	  void* base = currentEE->getPointerToGlobalIfAvailable(GV);
	  return currentEE->RPTOGV(base);
  }
  auto it = PreExecute::currentPreExecutePass->typedAllocations.upper_bound(p);
  assert (it != PreExecute::currentPreExecutePass->typedAllocations.begin());
  --it;
  // Verify that the address is in the memory block
  assert (p >= it->first);
  AllocData& allocData = it->second;
  // The edge of the allocation is a valid pointer
  assert(p <= (it->first + allocData.size));
  return currentEE->RPTOGV(it->first);
}

static GenericValue pre_execute_pointer_offset(FunctionType *FT,
                                         const std::vector<GenericValue> &Args) {
  GenericValue G;
  ExecutionEngine *currentEE = PreExecute::currentPreExecutePass->currentEE;
  char *p = (char *)(currentEE->GVTORP(Args[0]));
  const GlobalValue* GV = currentEE->getGlobalValueAtAddress(p);
  if (GV)
  {
    char* base = (char*)currentEE->getPointerToGlobalIfAvailable(GV);
    G.IntVal = APInt(32, p - base);
    return G;
  }
  auto it = PreExecute::currentPreExecutePass->typedAllocations.upper_bound(p);
  assert (it != PreExecute::currentPreExecutePass->typedAllocations.begin());
  --it;
  // Verify that the address is in the memory block
  assert (p >= it->first);
  AllocData& allocData = it->second;
  // The edge of the allocation is a valid pointer
  assert(p <= (it->first + allocData.size));
  G.IntVal = APInt(32, p - it->first);
  return G;
}

static GenericValue pre_execute_allocate(FunctionType *FT,
                                         const std::vector<GenericValue> &Args) {
  size_t size=(size_t)(Args[0].IntVal.getLimitedValue());
  ExecutionEngine *currentEE = PreExecute::currentPreExecutePass->currentEE;
  void* ret = PreExecute::currentPreExecutePass->allocator->allocate(size);
  memset(ret, 0, size);

#ifdef DEBUG_PRE_EXECUTE
  llvm::errs() << "Allocating " << ret << " of size " << size << " and type " << *FT->getReturnType() << "\n";
#endif

  // Register this allocations in the pass
  llvm::Type *type = FT->getReturnType()->getPointerElementType();
  PreExecute::currentPreExecutePass->recordTypedAllocation(type, size, (char*)ret);
  return currentEE->RPTOGV(ret);
}

static GenericValue pre_execute_reallocate(FunctionType *FT,
                                         const std::vector<GenericValue> &Args) {
  ExecutionEngine *currentEE = PreExecute::currentPreExecutePass->currentEE;
  void *p = (void *)(currentEE->GVTORP(Args[0]));
  size_t size=(size_t)(Args[1].IntVal.getLimitedValue());
  void* ret = PreExecute::currentPreExecutePass->allocator->allocate(size);
  memset(ret, 0, size);
  // Find out the old size
  auto it = PreExecute::currentPreExecutePass->typedAllocations.find((char*)p);
  uint32_t oldSize = 0;
  if(it == PreExecute::currentPreExecutePass->typedAllocations.end())
  {
    // In PreExecuter context it may happen to resize a global, since it may have been from malloc before.
    const GlobalValue* GV=currentEE->getGlobalValueAtAddress((char*)p);
    assert(GV);
    const DataLayout *DL = PreExecute::currentPreExecutePass->currentModule->getDataLayout();
    oldSize = DL->getTypeAllocSize(GV->getType());
  }
  else
    oldSize = it->second.size;
  // Copy the old contents in the new buffer
  memcpy(ret, p, oldSize);

#ifdef DEBUG_PRE_EXECUTE
  llvm::errs() << "Reallocating " << ret << " of size " << size << " and type " << *FT->getReturnType() << "\n";
#endif

  // Register this allocations in the pass
  llvm::Type *type = FT->getReturnType()->getPointerElementType();
  PreExecute::currentPreExecutePass->recordTypedAllocation(type, size, (char*)ret);
  return currentEE->RPTOGV(ret);
}

static GenericValue pre_execute_deallocate(FunctionType *FT,
                                           const std::vector<GenericValue> &Args) {
#ifdef DEBUG_PRE_EXECUTE
    ExecutionEngine *currentEE = PreExecute::currentPreExecutePass->currentEE;
    void *p = (void *)(currentEE->GVTORP(Args[0]));
#endif

    // TODO: deallocate the memory

#ifdef DEBUG_PRE_EXECUTE
    llvm::errs() << "Deallocating " << p << "\n";
#endif

    return GenericValue();
}

static GenericValue pre_execute_cast(FunctionType *FT,
                                     const std::vector<GenericValue> &Args) {
    return Args[0];
}

static GenericValue pre_execute_memcpy(FunctionType *FT,
                                       const std::vector<GenericValue> &Args) {
  ExecutionEngine *currentEE = PreExecute::currentPreExecutePass->currentEE;
  // Support fully typed memcpy
  memcpy(currentEE->GVTORP(Args[0]), currentEE->GVTORP(Args[1]),
         (size_t)(Args[2].IntVal.getLimitedValue()));

  GenericValue GV;
  GV.IntVal = 0;
  return GV;
}
static StructType* most_derived_class(char* Addr)
{
  ExecutionEngine *currentEE = PreExecute::currentPreExecutePass->currentEE;

  const GlobalValue* GV = currentEE->getGlobalValueAtAddress(Addr);
  StructType* Ty;
  if (GV)
  {
    Ty = dyn_cast<StructType>(GV->getType()->getPointerElementType());
  }
  else
  {
    auto it = PreExecute::currentPreExecutePass->typedAllocations.upper_bound(Addr);
    assert(it != PreExecute::currentPreExecutePass->typedAllocations.begin());
    --it;
    // Verify that the address is in the memory block
    assert (Addr >= it->first);
    AllocData& allocData = it->second;
    // The edge of the allocation is a valid pointer
    assert(Addr <= (it->first + allocData.size));
    Ty = dyn_cast<StructType>(it->second.allocType);
  }
  return Ty;
}
static GenericValue pre_execute_downcast_current(FunctionType *FT,
                                         const std::vector<GenericValue> &Args) {

  ExecutionEngine *currentEE = PreExecute::currentPreExecutePass->currentEE;
  char* Addr = (char*)currentEE->GVTORP(Args[0]);

  StructType* objType = most_derived_class(Addr); 
  assert(objType);
  StructType* currentType = dyn_cast<StructType>(FT->getParamType(0)->getPointerElementType());
  StructType* currentBase = objType;
  while (currentBase != nullptr)
  {
    if(currentBase == currentType)
    {
      GenericValue ret;
      ret.IntVal = APInt(32, 0);
      return ret;
    }
    currentBase = currentBase->getDirectBase();
  }
  uint32_t firstBase, baseCount;
  auto &currentModule = PreExecute::currentPreExecutePass->currentModule;
  bool ret = TypeSupport::getBasesInfo(*currentModule, objType,
                firstBase, baseCount);
  assert(ret);
  for(uint32_t i = firstBase; i < firstBase+baseCount; i++)
  {
    currentBase = dyn_cast<StructType>(objType->getElementType(i));
    if(currentBase == currentType) 
    {
      GenericValue ret;
      ret.IntVal = APInt(32, i);
      return ret;
    }
  }
  llvm_unreachable("Base not found");
}
static GenericValue pre_execute_downcast(FunctionType *FT,
                                         const std::vector<GenericValue> &Args) {
    // We need to apply the offset in bytes using the bases metadata
    ExecutionEngine *currentEE = PreExecute::currentPreExecutePass->currentEE;
    char* Addr = (char*)currentEE->GVTORP(Args[0]);
    Type* derivedType = most_derived_class(Addr); 
    assert(derivedType);
    int baseOffset = Args[1].IntVal.getSExtValue();
    Type* baseType;
    if (baseOffset >= 0)
        baseType = FT->getParamType(0)->getPointerElementType();
    else
        baseType = FT->getReturnType()->getPointerElementType();
    uintptr_t curByteOffset=0;
    uint32_t curBaseOffset= baseOffset>=0 ? baseOffset : -baseOffset;
    StructType* curType=cast<StructType>(derivedType);
    auto &currentModule = PreExecute::currentPreExecutePass->currentModule;
    auto DL = currentModule->getDataLayout();
    while(curType!=baseType)
    {
        if(curBaseOffset==0)
            break;
        //Decrease 1 for this base
        curBaseOffset--;
        uint32_t firstBase, baseCount;
        bool ret = TypeSupport::getBasesInfo(*currentModule, curType,
                firstBase, baseCount);
        assert(ret);
#ifdef DEBUG_PRE_EXECUTE
        llvm::errs()
            << "curType " << *curType
            << " FIRST " << firstBase
            << " COUNT " << baseCount
            << "\n";
#endif
        assert(curBaseOffset < baseCount);
        uint32_t nextBaseIndex = firstBase;
        for(; nextBaseIndex < (firstBase + baseCount); nextBaseIndex++) {
            uint32_t subFirstBase, subBaseCount;
            auto element = curType->getElementType(nextBaseIndex);
            StructType* nextType = cast<StructType>(element);
#ifdef DEBUG_PRE_EXECUTE
            llvm::errs() << "nextType: " << *nextType << "\n";
#endif
            bool ret = TypeSupport::getBasesInfo(*currentModule, nextType,
                        subFirstBase, subBaseCount);
            if (!ret)
                break;
            if (curBaseOffset < subBaseCount) {
                curType = nextType;
                break;
            }
            curBaseOffset -= subBaseCount;
        }
        // Skip the offset untils the first base
        auto layout = DL->getStructLayout(curType);
#ifdef DEBUG_PRE_EXECUTE
        llvm::errs() << "curType: " << *curType << "\n";
        llvm::errs() << "firstBase: " << firstBase << "\n";
        llvm::errs() << "layout in bytes: " << layout->getSizeInBytes() << "\n";
#endif
        curByteOffset += layout->getElementOffset(firstBase);
    }
    if (baseOffset < 0)
	    curByteOffset = - curByteOffset;
    uintptr_t AddrValue = reinterpret_cast<uintptr_t>(GVTOP(Args[0]));
#ifdef DEBUG_PRE_EXECUTE
    llvm::errs() << "AddrValue " << AddrValue
        << " curByteOffset " << curByteOffset << "\n";
#endif
    AddrValue-=curByteOffset;
    return GenericValue(reinterpret_cast<void*>(AddrValue));
}

static GenericValue pre_execute_upcast(FunctionType *FT,
                                       const std::vector<GenericValue> &Args) {
    return Args[0]; 
}

static GenericValue assertEqualImpl(FunctionType *FT,
        const std::vector<GenericValue> &Args)
{
    ExecutionEngine *currentEE = PreExecute::currentPreExecutePass->currentEE;
    bool success = Args[0].IntVal.getZExtValue();
    const char* msg = reinterpret_cast<char *>(currentEE->GVTORP(Args[1]));
    if (success) {
        llvm::errs() << msg << ": SUCCESS\n";
    } else {
        llvm::errs() << msg << ": FAILURE\n";
	  llvm::report_fatal_error("PreExecute test failed");
    }

    return GenericValue(0);
}

static GenericValue emptyFunction(FunctionType *FT,
                                  const std::vector<GenericValue> &Args)
{
    return GenericValue(0);
}

static void* LazyFunctionCreator(const std::string& funcName)
{
    if (funcName=="malloc")
        return (void*)(void(*)())pre_execute_malloc;
    if (strncmp(funcName.c_str(), "llvm.cheerp.cast.user.", strlen("llvm.cheerp.cast.user."))==0)
        return (void*)(void(*)())pre_execute_cast;
    if (strncmp(funcName.c_str(), "llvm.cheerp.downcast.current.", strlen("llvm.cheerp.downcast.current."))==0)
        return (void*)(void(*)())pre_execute_downcast_current;
    if (strncmp(funcName.c_str(), "llvm.cheerp.downcast.", strlen("llvm.cheerp.downcast."))==0)
        return (void*)(void(*)())pre_execute_downcast;
    if (strncmp(funcName.c_str(), "llvm.cheerp.upcast.", strlen("llvm.cheerp.upcast."))==0)
        return (void*)(void(*)())pre_execute_upcast;
    if (strncmp(funcName.c_str(), "llvm.cheerp.allocate.", strlen("llvm.cheerp.allocate."))==0)
        return (void*)(void(*)())pre_execute_allocate;
    if (strncmp(funcName.c_str(), "llvm.cheerp.reallocate.", strlen("llvm.cheerp.reallocate."))==0)
        return (void*)(void(*)())pre_execute_reallocate;
    if (strncmp(funcName.c_str(), "llvm.cheerp.deallocate", strlen("llvm.cheerp.deallocate")) == 0 ||
        strncmp(funcName.c_str(), "free", strlen("free")) == 0)
        return (void*)(void(*)())pre_execute_deallocate;
    if (strncmp(funcName.c_str(), "llvm.cheerp.element.distance.", strlen("llvm.cheerp.element.distance."))==0)
        return (void*)(void(*)())pre_execute_element_distance;
    if (strncmp(funcName.c_str(), "llvm.cheerp.pointer.base.", strlen("llvm.cheerp.pointer.base."))==0)
        return (void*)(void(*)())pre_execute_pointer_base;
    if (strncmp(funcName.c_str(), "llvm.cheerp.pointer.offset.", strlen("llvm.cheerp.pointer.offset."))==0)
        return (void*)(void(*)())pre_execute_pointer_offset;
    if (strncmp(funcName.c_str(), "llvm.memcpy.", strlen("llvm.memcpy."))==0)
        return (void*)(void(*)())pre_execute_memcpy;
    if (strcmp(funcName.c_str(), "assertEqualImpl") == 0)
        return (void*)(void(*)())assertEqualImpl;
    if (strcmp(funcName.c_str(), "llvm.dbg.value") == 0)
        return (void*)(void(*)())emptyFunction;

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

static bool isTypeCompatible(Type* curType, Type* endType)
{
	if(curType == endType)
		return true;
	StructType* curTypeSt = dyn_cast<StructType>(curType);
	StructType* endTypeSt = dyn_cast<StructType>(endType);
	if(!curTypeSt || !endTypeSt)
		return false;
	while(curTypeSt->getDirectBase())
	{
		curTypeSt = curTypeSt->getDirectBase();
		if(curTypeSt == endTypeSt)
			return true;
	}
	return false;
}

llvm::Type* getTypeSafeGepForAddress(SmallVector<Constant*, 4>& Indices, Type* Int32Ty, const DataLayout* DL,
                    Type* startType, Type* endType, uint32_t Offset)
{
    Type* curType = startType;
    // Keep track of the state while ignoring all trailing zero indices
    assert(isa<PointerType>(curType));
    if (PointerType* PT=dyn_cast<PointerType>(curType))
    {
        Type* ET = PT->getElementType();
        uint32_t elementSize = DL->getTypeAllocSize(ET);
        uint32_t elementOffset = Offset/elementSize;
        Indices.push_back(ConstantInt::get(Int32Ty, elementOffset));
        Offset %= elementSize;
        curType = ET;
    }
    Type* typeAtLastNotZero = curType;
    uint32_t indicesLengthAtLastNotZero = 1;
    while(Offset!=0 || !isTypeCompatible(curType, endType))
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
            if(elementIndex != 0)
            {
                typeAtLastNotZero = curType;
                indicesLengthAtLastNotZero = Indices.size();
            }
        }
        else if (StructType* ST=dyn_cast<StructType>(curType))
        {
            const StructLayout* SL = DL->getStructLayout(ST);
            uint32_t elementIndex = SL->getElementContainingOffset(Offset);
            Indices.push_back(ConstantInt::get(Int32Ty, elementIndex));
            Offset -= SL->getElementOffset(elementIndex);
            curType = ST->getElementType(elementIndex);
            if(elementIndex != 0)
            {
                typeAtLastNotZero = curType;
                indicesLengthAtLastNotZero = Indices.size();
            }
        }
        else
        {
            // If we have not consumed the Offset we require a pointer to the middle of an element
            if(Offset)
                return NULL;
            // The Offset was valid, but we could not find the type, drop trailing zeroes from the Indices array
            Indices.resize(indicesLengthAtLastNotZero);
            return typeAtLastNotZero;
        }
    }
    // We found the desired type or a compatible one
    return curType;
}

Constant* PreExecute::findPointerFromGlobal(const DataLayout* DL,
        Type* memType, GlobalValue* GV, char* GlobalStartAddr,
        char* StoredAddr, Type* Int32Ty)
{
    // Build a type safe GEP to the right type at the right offset
    uintptr_t Offset = StoredAddr - GlobalStartAddr;
    llvm::SmallVector<Constant*, 4> Indices;
    // This is needed to dereference global
    llvm::Type* typeFound = getTypeSafeGepForAddress(Indices, Int32Ty, DL,
            GV->getType(),
            memType->getPointerElementType(), Offset);
    if (!typeFound)
        return NULL;
    Constant* GEP = ConstantExpr::getGetElementPtr(GV->getType()->getPointerElementType(), GV, Indices);
    assert(GEP->getType()->getPointerElementType() == typeFound);
    if(GEP->getType() != memType)
        return ConstantExpr::getBitCast(GEP, memType);
    return GEP;
}

GlobalValue* PreExecute::getGlobalForMalloc(const DataLayout* DL, char* StoredAddr,
        char*& MallocStartAddress, bool asmjs)
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
    // Make it an array, if it's more than 1 element long
    uint32_t elementSize = DL->getTypeAllocSize(allocData.allocType);
    uint32_t size = allocData.size / elementSize;
    Type* newGlobalType = size > 1 ? ArrayType::get(allocData.allocType, size) : allocData.allocType;

    allocData.globalValue = new GlobalVariable(*currentModule, newGlobalType,
            false, GlobalValue::InternalLinkage, nullptr, "promotedMalloc");

    if (asmjs)
        allocData.globalValue->setSection("asmjs");
    // Build an initializer
    allocData.globalValue->setInitializer(computeInitializerFromMemory(DL, newGlobalType, it->first, asmjs));

    return allocData.globalValue;
}

Constant* PreExecute::computeInitializerFromMemory(const DataLayout* DL,
        Type* memType, char* Addr, bool asmjs)
{
    if (IntegerType* IT=dyn_cast<IntegerType>(memType))
    {
        if (IT->getBitWidth() > 32)
            return NULL;
        // Assume little endian
        uint32_t integerVal = 0;
        memcpy(&integerVal, Addr, std::max(IT->getBitWidth()/8, 1u));
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
        for (uint32_t i = 0; i < ST->getNumElements(); i++)
        {
            char* elementAddr = Addr + SL->getElementOffset(i);
            Constant* elem = computeInitializerFromMemory(DL,
                    ST->getElementType(i), elementAddr, asmjs);
            Elements.push_back(elem);
        }
        return ConstantStruct::get(ST, Elements);
    }
    else if (ArrayType* AT=dyn_cast<ArrayType>(memType))
    {
        Type* elementType = AT->getElementType();
        uint32_t elementSize = DL->getTypeAllocSize(elementType);
        SmallVector<Constant*, 4> Elements;
        for(uint32_t i = 0; i < AT->getNumElements(); i++) {
            char* elementAddr = Addr + i*elementSize;
            Constant* elem = computeInitializerFromMemory(DL,
                    elementType, elementAddr, asmjs);
            Elements.push_back(elem);
        }
        return ConstantArray::get(AT, Elements);
    }
    else if (PointerType* PT=dyn_cast<PointerType>(memType))
    {
        // Pointers are tricky, we need to find the global containing the
        // pointer Start with reading the pointer from memory
        const unsigned LoadBytes = DL->getTypeStoreSize(memType);
        char* StoredAddr = nullptr;
        memcpy(&StoredAddr, Addr, LoadBytes);

        if (StoredAddr==NULL) {
            return ConstantPointerNull::get(PT);
        }

        if(PT->getElementType()->isFunctionTy())
        {
            Value* castedVal = currentEE->FunctionAddresses->getFunction(StoredAddr);
            assert(isa<Function>(castedVal));
            // Potentially also cast the function to the expected type
            if(castedVal->getType() != PT)
                return ConstantExpr::getBitCast(cast<Function>(castedVal), PT);
            return cast<Function>(castedVal);
        }

        StoredAddr = (char*) currentEE->ValueAddresses->toReal(StoredAddr);
        Type* Int32Ty = IntegerType::get(currentModule->getContext(), 32);
        const GlobalValue* GV = currentEE->getGlobalValueAtAddress(StoredAddr);
        if (GV)
        {
            char* GlobalStartAddr = (char*)currentEE->getPointerToGlobal(GV);
            Constant* ret = findPointerFromGlobal(DL, memType,
                    const_cast<GlobalValue*>(GV), GlobalStartAddr,
                    StoredAddr, Int32Ty);
            return ret;
        }

        // Look inside type safe allocated memory
        char* MallocStartAddress;
        GV = getGlobalForMalloc(DL, StoredAddr, MallocStartAddress, asmjs);
        if (GV)
        {
#ifdef DEBUG_PRE_EXECUTE
            llvm::errs() << "GV for malloc " << *GV << "\n";
#endif
            Constant* ret = findPointerFromGlobal(DL, memType,
                    const_cast<GlobalValue*>(GV), MallocStartAddress,
                    StoredAddr, Int32Ty);
            return ret;
        }
        llvm::errs() << "StoredAddr: " << (void*) StoredAddr
            << " Addr: " << (void*) Addr
            << " LoadBytes: " << LoadBytes
            << "\n";
        llvm_unreachable("Could not get pointer");
    }
    return NULL;
}

bool PreExecute::runOnConstructor( llvm::Module& m, llvm::Function* func)
{
    bool Changed = false;

    for (auto& GV: m.globals())
    {
        if (GV.hasInitializer())
            currentEE->EmitGlobalVariable(&GV);
    }
    currentEE->runFunction(func, std::vector< GenericValue >());
    if(currentEE->hasFailed())
    {
        // Execution could not be safely completed. Clean up.
        modifiedGlobals.clear();
    }
    else
        Changed = true;

    // Compute new initializer for the modified globals
    for(auto& it: modifiedGlobals)
    {
        GlobalVariable* GV = it.first;
        void* Addr = currentEE->getPointerToGlobal(GV);
        Constant* newInit;
        const DataLayout *DL = m.getDataLayout();
        Type *ptrType = GV->getType()->getPointerElementType();
        bool asmjs = GV->getSection() == StringRef("asmjs");
        newInit = computeInitializerFromMemory(DL, ptrType, (char*)Addr, asmjs);
        assert(newInit);
        it.second = newInit;
    }

    // Set new initializers for the modified globals
    for(auto& it: modifiedGlobals)
    {
        assert(it.second);
        it.first->setInitializer(it.second);
    }

    modifiedGlobals.clear();
    typedAllocations.clear();

#ifdef DEBUG_PRE_EXECUTE
    currentEE->printMemoryStats();
#endif

    allocator->deallocate();

    return Changed;
}

bool PreExecute::runOnModule(Module& m)
{
    bool Changed = false;

    currentPreExecutePass = this;
    currentModule = &m;

    std::string error;
    std::string triple = sys::getDefaultTargetTriple();
    const Target *target = TargetRegistry::lookupTarget(triple, error);

    TargetMachine* machine;
    machine = target->createTargetMachine(triple, "", "", TargetOptions());

    std::unique_ptr<Module> uniqM(&m);

    EngineBuilder builder(std::move(uniqM));
    builder.setEngineKind(llvm::EngineKind::PreExecuteInterpreter);
    builder.setOptLevel(CodeGenOpt::Default);
    builder.setErrorStr(&error);
    builder.setVerifyModules(true);

    currentEE = builder.create(machine);
    assert(currentEE && "failed to create execution engine!");
    currentEE->InstallStoreListener(StoreListener);
    currentEE->InstallAllocaListener(AllocaListener);
    currentEE->InstallRetListener(RetListener);
    currentEE->InstallLazyFunctionCreator(LazyFunctionCreator);

    allocator = make_unique<Allocator>(*currentEE->ValueAddresses);

    GlobalVariable * constructorVar = m.getGlobalVariable("llvm.global_ctors");

    std::vector<Constant*> newConstructors;

    if (constructorVar)
    {
        // Random things which may go boom
        if (!constructorVar->hasInitializer() ||
            !isa<ConstantArray>(constructorVar->getInitializer()))
            return Changed;

        const Constant *initializer = constructorVar->getInitializer();
        const ConstantArray *constructors = cast<ConstantArray>(initializer);

        for (ConstantArray::const_op_iterator it = constructors->op_begin();
             it != constructors->op_end(); ++it)
        {
            Constant *elem = cast<Constant>(*it);
            Function* func = cast<Function>(elem->getAggregateElement(1));
            if(runOnConstructor(m, func))
                Changed |= true;
            else
                newConstructors.push_back(elem);
        }
    }

    if (PreExecuteMain)
    {
        Function* mainFunc = m.getFunction("_Z7webMainv");
        if (!mainFunc)
            mainFunc = m.getFunction("main");
        assert(mainFunc && "unable to find main/webMain in module!");
        if(runOnConstructor(m, mainFunc))
        {
            Changed |= true;
            mainFunc->eraseFromParent();
        }
    }

    // Delete global constructors and remove the main body
    if (constructorVar)
    {
        // Build new constructors if neededed
        if(newConstructors.empty())
            constructorVar->eraseFromParent();
        else
        {
            Constant* newArray = ConstantArray::get(ArrayType::get(newConstructors[0]->getType(), newConstructors.size()), newConstructors);
            // Code borrowed from removeGlobalCtors
            // Create the new global and insert it next to the existing list.
            GlobalVariable *NGV = new GlobalVariable(newArray->getType(), constructorVar->isConstant(), constructorVar->getLinkage(),
                         newArray, "", constructorVar->getThreadLocalMode());
            NGV->setSection(constructorVar->getSection());
            constructorVar->getParent()->getGlobalList().insert(constructorVar, NGV);
            NGV->takeName(constructorVar);

            // Nuke the old list, replacing any uses with the new one.
            if (!constructorVar->use_empty())
            {
                Constant *V = NGV;
                if (V->getType() != constructorVar->getType())
                    V = ConstantExpr::getBitCast(V, constructorVar->getType());
                constructorVar->replaceAllUsesWith(V);
            }
            constructorVar->eraseFromParent();
        }
    }

    bool removed = currentEE->removeModule(&m);
    assert(removed && "failed to free the module from ExecutionEngine");

    allocator = nullptr;
    delete currentEE;

    currentEE = NULL;
    currentPreExecutePass = NULL;
    currentModule = NULL;

    return Changed;
}

}

using namespace cheerp;

INITIALIZE_PASS_BEGIN(PreExecute, "PreExecute",
        "Execute run-time init at compile time (linux only)", false, false)
INITIALIZE_PASS_END(PreExecute, "PreExecute",
        "Execute run-time init at compile time (linux only)", false, false)
