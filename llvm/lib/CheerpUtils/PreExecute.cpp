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
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/ExecutionEngine/Interpreter.h"
#include "llvm/ExecutionEngine/FunctionMap.h"
#include "llvm/IR/Constants.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetRegistry.h"
#include <string.h>
#include <algorithm>

//#define DEBUG_PRE_EXECUTE 1

using namespace llvm;

static cl::opt<bool> PreExecuteMain("cheerp-preexecute-main", cl::desc("Run main/webMain in the PreExecuter step") );

namespace cheerp {

PreExecute* PreExecute::currentPreExecutePass = NULL;

StringRef PreExecute::getPassName() const
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
    ExecutionEngine *currentEE = PreExecute::currentPreExecutePass->currentEE;
    bool asmjs = currentEE->getCurrentFunction()->getSection() == StringRef("asmjs") ||
                    TypeSupport::isAsmJSPointer(Ty);
    PreExecute::currentPreExecutePass->recordTypedAllocation(Ty, Size, (char*)Addr, /*hasCookie*/ false, asmjs);
}
static void RetListener(const std::vector<std::unique_ptr<char[]>>& allocas)
{
    for (const auto& a: allocas)
    {
        PreExecute::currentPreExecutePass->releaseTypedAllocation(a.get());
    }
}

static GenericValue pre_execute_element_distance(FunctionType *FT, ArrayRef<GenericValue> Args)
{
    Type* type = FT->getParamType(0)->getPointerElementType();
    llvm::Module *module = PreExecute::currentPreExecutePass->currentModule;
    uint32_t elementSize = module->getDataLayout().getTypeAllocSize(type);
    GenericValue GV;
    GV.IntVal = APInt(32, elementSize);
    return GV;
}

static char* most_derived_pointer(char* Addr) {
  ExecutionEngine *currentEE = PreExecute::currentPreExecutePass->currentEE;
  const GlobalValue* GV = currentEE->getGlobalValueAtAddress(Addr);
  if (GV)
  {
    char* p = static_cast<char*>(currentEE->getPointerToGlobalIfAvailable(GV));
    return p;
  }
  auto it = PreExecute::currentPreExecutePass->typedAllocations.upper_bound(Addr);
  assert (it != PreExecute::currentPreExecutePass->typedAllocations.begin());
  --it;
  // Verify that the address is in the memory block
  assert (Addr >= it->first);
  AllocData& allocData = it->second;
  (void)allocData;
  // The edge of the allocation is a valid pointer
  assert(Addr <= (it->first + allocData.size));
  return it->first;
}

static GenericValue pre_execute_pointer_base(FunctionType *FT,
                                         ArrayRef<GenericValue> Args) {
  ExecutionEngine *currentEE = PreExecute::currentPreExecutePass->currentEE;
  char *p = (char *)(currentEE->GVTORP(Args[0]));
  return currentEE->RPTOGV(most_derived_pointer(p));
}

static GenericValue pre_execute_pointer_offset(FunctionType *FT,
                                         ArrayRef<GenericValue> Args) {
  GenericValue G;
  ExecutionEngine *currentEE = PreExecute::currentPreExecutePass->currentEE;
  char *p = (char *)(currentEE->GVTORP(Args[0]));
  char *derived = most_derived_pointer(p);
  G.IntVal = APInt(32, p - derived);
  return G;
}

static GenericValue pre_execute_allocate_array(FunctionType *FT,
                                         ArrayRef<GenericValue> Args) {
  ExecutionEngine *currentEE = PreExecute::currentPreExecutePass->currentEE;
  size_t size=(size_t)(Args[0].IntVal.getLimitedValue());

  llvm::Type *type = FT->getReturnType()->getPointerElementType();
  bool asmjs = currentEE->getCurrentCaller()->getSection() == StringRef("asmjs") ||
                TypeSupport::isAsmJSPointer(type->getPointerTo());
  const DataLayout *DL = &PreExecute::currentPreExecutePass->currentModule->getDataLayout();
  size_t num = size / DL->getTypeAllocSize(type);
  // Cookie
  size_t cookieSize = 0;
  if (!asmjs)
    cookieSize = cheerp::TypeSupport::getArrayCookieSizeAsmJS(*DL, type);
  size += cookieSize;
  void* ret = PreExecute::currentPreExecutePass->allocator->allocate(size);
  memset(ret, 0, size);

#ifdef DEBUG_PRE_EXECUTE
  llvm::errs() << "Allocating " << ret << " of size " << size << " and type " << *FT->getReturnType() << "\n";
#endif

  // Register this allocations in the pass
  PreExecute::currentPreExecutePass->recordTypedAllocation(type, size, (char*)ret, /*hasCookie*/ true, asmjs);

  if (!asmjs) {
    uint32_t* cookie = (uint32_t*) ret;
    *cookie = num;
  }
  ret = static_cast<char*>(ret) + cookieSize;

  return currentEE->RPTOGV(ret);
}

static GenericValue pre_execute_allocate(FunctionType *FT,
                                         ArrayRef<GenericValue> Args) {
  size_t size=(size_t)(Args[0].IntVal.getLimitedValue());
  ExecutionEngine *currentEE = PreExecute::currentPreExecutePass->currentEE;
  void* ret = PreExecute::currentPreExecutePass->allocator->allocate(size);
  memset(ret, 0, size);

#ifdef DEBUG_PRE_EXECUTE
  llvm::errs() << "Allocating " << ret << " of size " << size << " and type " << *FT->getReturnType() << "\n";
#endif

  // Register this allocations in the pass
  llvm::Type *type = FT->getReturnType()->getPointerElementType();
  bool asmjs = currentEE->getCurrentCaller()->getSection() == StringRef("asmjs") ||
                TypeSupport::isAsmJSPointer(type->getPointerTo());
  PreExecute::currentPreExecutePass->recordTypedAllocation(type, size, (char*)ret, /*hasCookie*/ false, asmjs);

  return currentEE->RPTOGV(ret);
}

static GenericValue pre_execute_reallocate(FunctionType *FT,
                                         ArrayRef<GenericValue> Args) {
  ExecutionEngine *currentEE = PreExecute::currentPreExecutePass->currentEE;
  void *p = (void *)(currentEE->GVTORP(Args[0]));
  size_t size=(size_t)(Args[1].IntVal.getLimitedValue());
  void* ret = PreExecute::currentPreExecutePass->allocator->allocate(size);
  memset(ret, 0, size);
  if(p != nullptr)
  {
    // Find out the old size
    auto it = PreExecute::currentPreExecutePass->typedAllocations.find((char*)p);
    uint32_t oldSize = 0;
    if(it == PreExecute::currentPreExecutePass->typedAllocations.end())
    {
      // In PreExecuter context it may happen to resize a global, since it may have been from malloc before.
      const GlobalValue* GV=currentEE->getGlobalValueAtAddress((char*)p);
      assert(GV);
      const DataLayout *DL = &PreExecute::currentPreExecutePass->currentModule->getDataLayout();
      oldSize = DL->getTypeAllocSize(GV->getType());
    }
    else
      oldSize = it->second.size;
    // Copy the old contents in the new buffer
    memcpy(ret, p, std::min((uint32_t)size, oldSize));
  }
#ifdef DEBUG_PRE_EXECUTE
  llvm::errs() << "Reallocating " << ret << " of size " << size << " and type " << *FT->getReturnType() << "\n";
#endif

  // Register this allocations in the pass
  llvm::Type *type = FT->getReturnType()->getPointerElementType();
  bool asmjs = currentEE->getCurrentCaller()->getSection() == StringRef("asmjs") ||
                TypeSupport::isAsmJSPointer(type);
  PreExecute::currentPreExecutePass->recordTypedAllocation(type, size, (char*)ret, /*hasCookie*/ false, asmjs);
  return currentEE->RPTOGV(ret);
}

static GenericValue pre_execute_deallocate(FunctionType *FT,
                                           ArrayRef<GenericValue> Args) {
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

static GenericValue pre_execute_get_array_len(FunctionType *FT,
                                           ArrayRef<GenericValue> Args) {
    ExecutionEngine *currentEE = PreExecute::currentPreExecutePass->currentEE;
    const DataLayout& DL = currentEE->getDataLayout();
    Type* type = FT->getParamType(0)->getPointerElementType();
    size_t cookieWords = cheerp::TypeSupport::getArrayCookieSizeAsmJS(DL, type) / sizeof(uint32_t);
    uint32_t *cookie = static_cast<uint32_t*>(currentEE->GVTORP(Args[0])) - cookieWords;

    GenericValue G;
    G.IntVal = APInt(32, *cookie);
    return G;
}

static GenericValue pre_execute_cast(FunctionType *FT,
                                     ArrayRef<GenericValue> Args) {
    return Args[0];
}

static GenericValue pre_execute_memcpy(FunctionType *FT,
                                       ArrayRef<GenericValue> Args) {
  ExecutionEngine *currentEE = PreExecute::currentPreExecutePass->currentEE;
  // Support fully typed memcpy
  memcpy(currentEE->GVTORP(Args[0]), currentEE->GVTORP(Args[1]),
         (size_t)(Args[2].IntVal.getLimitedValue()));

  GenericValue GV;
  GV.IntVal = 0;
  return GV;
}

static GenericValue pre_execute_memmove(FunctionType *FT,
                                       ArrayRef<GenericValue> Args) {
  ExecutionEngine *currentEE = PreExecute::currentPreExecutePass->currentEE;
  // Support fully typed memmove
  memmove(currentEE->GVTORP(Args[0]), currentEE->GVTORP(Args[1]),
         (size_t)(Args[2].IntVal.getLimitedValue()));

  GenericValue GV;
  GV.IntVal = 0;
  return GV;
}

static GenericValue pre_execute_memset(FunctionType *FT,
                                       ArrayRef<GenericValue> Args) {
  ExecutionEngine *currentEE = PreExecute::currentPreExecutePass->currentEE;
  // Support fully typed memset
  memset(currentEE->GVTORP(Args[0]),
         (size_t)(Args[1].IntVal.getLimitedValue()),
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
    (void)allocData;
    // The edge of the allocation is a valid pointer
    assert(Addr <= (it->first + allocData.size));
    Ty = dyn_cast<StructType>(it->second.allocType);
  }
  return Ty;
}

static bool get_subobject_base_offset(StructType* derivedType, char* derivedAddr, char* baseAddr, uint32_t& baseIndex) {
    if (derivedAddr == baseAddr)
        return true;

    if (StructType* directBase =derivedType->getDirectBase())
    {
        bool ret = get_subobject_base_offset(directBase, derivedAddr, baseAddr, baseIndex);
        if (ret)
            return true;
    }

    Module* currentModule = PreExecute::currentPreExecutePass->currentModule;
    auto DL = &currentModule->getDataLayout();
    auto layout = DL->getStructLayout(derivedType);

    uint32_t firstBase, baseCount;
    bool ret = TypeSupport::getBasesInfo(*currentModule, derivedType,
            firstBase, baseCount);
    if(!ret)
    {
        return false;
    }
    for (uint32_t curBase = firstBase; curBase < firstBase + baseCount; curBase++)
    {
        baseIndex++;
        bool ret = get_subobject_base_offset(cast<StructType>(derivedType->getElementType(curBase)),
                derivedAddr + layout->getElementOffset(curBase), baseAddr, baseIndex);
        if (ret)
            return true;
    }
    return false;
}

static bool get_subobject_byte_offset(StructType* derivedType, uint32_t baseIndex, uint32_t& curIndex, uint32_t& byteOffset) {
    if (baseIndex == curIndex)
        return true;

    if (StructType* directBase =derivedType->getDirectBase())
    {
        bool ret = get_subobject_byte_offset(directBase, baseIndex, curIndex, byteOffset);
        if (ret)
            return true;
    }
    Module* currentModule = PreExecute::currentPreExecutePass->currentModule;
    auto DL = &currentModule->getDataLayout();
    auto layout = DL->getStructLayout(derivedType);

    uint32_t firstBase, baseCount;
    bool ret = TypeSupport::getBasesInfo(*currentModule, derivedType,
            firstBase, baseCount);
    if(!ret)
    {
        return false;
    }
    for (uint32_t curBase = firstBase; curBase < firstBase + baseCount; curBase++)
    {
        uint32_t childOffset = byteOffset + layout->getElementOffset(curBase);
        curIndex++;
        ret = get_subobject_byte_offset(cast<StructType>(derivedType->getElementType(curBase)),
                baseIndex, curIndex, childOffset);
        if (ret)
        {
            byteOffset = childOffset;
            return true;
        }
    }
    return false;
}

static GenericValue pre_execute_downcast_current(FunctionType *FT,
                                         ArrayRef<GenericValue> Args) {

  ExecutionEngine *currentEE = PreExecute::currentPreExecutePass->currentEE;
  assert(currentEE->getCurrentCaller());
  bool asmjs = currentEE->getCurrentCaller()->getSection() == StringRef("asmjs");
  if (asmjs)
  {
    GenericValue GV;
    GV.IntVal = APInt(32,reinterpret_cast<uintptr_t>(Args[0].PointerVal));
    return GV;
  }

  char* Addr = (char*)currentEE->GVTORP(Args[0]);

  StructType* derivedType = most_derived_class(Addr); 
  char* derivedAddr = most_derived_pointer(Addr);

  uint32_t baseIndex = 0;
  bool check = get_subobject_base_offset(derivedType, derivedAddr, Addr, baseIndex);
  (void)check;
  assert(check && "subobject not found");

  GenericValue ret;
  ret.IntVal = APInt(32, baseIndex);

  return ret;
}

static GenericValue pre_execute_downcast(FunctionType *FT,
                                         ArrayRef<GenericValue> Args) {
    // We need to apply the offset in bytes using the bases metadata
    ExecutionEngine *currentEE = PreExecute::currentPreExecutePass->currentEE;
    char* Addr = (char*)currentEE->GVTORP(Args[0]);
    int baseOffset = Args[1].IntVal.getSExtValue();
    // NOTE: a downcast with offset 0 is equivalent to a bitcast, so it is a NOP
    // in PreExecuter. We exit early because it can happen that downcasts with
    // zero offset are performed on types not actually inside the most derived
    // object hierarchy, so the algorithm of get_subobject_base_offset would fail
    if (baseOffset == 0)
        return Args[0];
    StructType* derivedType = most_derived_class(Addr); 
    char* derivedAddr = most_derived_pointer(Addr);
    assert(derivedType);
    bool asmjs = derivedType->hasAsmJS();
    if (asmjs)
    {
        char* Addr = (char*)Args[0].PointerVal;
        int32_t Offset = Args[1].IntVal.getSExtValue();
        return GenericValue(Addr + Offset);
    }

    uint32_t baseIndex = 0;
    bool ret = get_subobject_base_offset(derivedType, derivedAddr, Addr, baseIndex);
    assert(ret && "subobject not found");
    baseIndex -= baseOffset;
    uint32_t byteOffset = 0;
    uint32_t curIndex = 0;
    ret = get_subobject_byte_offset(derivedType, baseIndex, curIndex, byteOffset);
    assert(ret);
    return currentEE->RPTOGV(derivedAddr + byteOffset);
}

static GenericValue pre_execute_virtualcast(FunctionType *FT,
                                         ArrayRef<GenericValue> Args) {
    // We need to apply the offset in bytes using the bases metadata
    ExecutionEngine *currentEE = PreExecute::currentPreExecutePass->currentEE;
    char* Addr = (char*)currentEE->GVTORP(Args[0]);
    StructType* derivedType = most_derived_class(Addr); 
    char* derivedAddr = most_derived_pointer(Addr);
    assert(derivedType);
    bool asmjs = derivedType->hasAsmJS();
    if (asmjs)
    {
        char* Addr = (char*)Args[0].PointerVal;
        int32_t Offset = Args[1].IntVal.getSExtValue();
        return GenericValue(Addr + Offset);
    }

    int baseIndex = Args[1].IntVal.getSExtValue();
    uint32_t byteOffset = 0;
    uint32_t curIndex = 0;
    bool ret = get_subobject_byte_offset(derivedType, baseIndex, curIndex, byteOffset);
    (void)ret;
    assert(ret);
    return currentEE->RPTOGV(derivedAddr + byteOffset);
}

static GenericValue pre_execute_upcast(FunctionType *FT,
                                       ArrayRef<GenericValue> Args) {
    return Args[0]; 
}

static GenericValue assertEqualImpl(FunctionType *FT,
        ArrayRef<GenericValue> Args)
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
                                  ArrayRef<GenericValue> Args)
{
    return GenericValue(0);
}

static void* LazyFunctionCreator(const std::string& funcName)
{
    if (strncmp(funcName.c_str(), "llvm.cheerp.cast.user.", strlen("llvm.cheerp.cast.user."))==0)
        return (void*)(void(*)())pre_execute_cast;
    if (strncmp(funcName.c_str(), "llvm.cheerp.downcast.current.", strlen("llvm.cheerp.downcast.current."))==0)
        return (void*)(void(*)())pre_execute_downcast_current;
    if (strncmp(funcName.c_str(), "llvm.cheerp.downcast.", strlen("llvm.cheerp.downcast."))==0)
        return (void*)(void(*)())pre_execute_downcast;
    if (strncmp(funcName.c_str(), "llvm.cheerp.virtualcast.", strlen("llvm.cheerp.virtualcast."))==0)
        return (void*)(void(*)())pre_execute_virtualcast;
    if (strncmp(funcName.c_str(), "llvm.cheerp.upcast.", strlen("llvm.cheerp.upcast."))==0)
        return (void*)(void(*)())pre_execute_upcast;
    if (strncmp(funcName.c_str(), "llvm.cheerp.allocate.array.", strlen("llvm.cheerp.allocate.array."))==0)
        return (void*)(void(*)())pre_execute_allocate_array;
    if (strncmp(funcName.c_str(), "llvm.cheerp.allocate.", strlen("llvm.cheerp.allocate."))==0)
        return (void*)(void(*)())pre_execute_allocate;
    if (strncmp(funcName.c_str(), "llvm.cheerp.reallocate.", strlen("llvm.cheerp.reallocate."))==0)
        return (void*)(void(*)())pre_execute_reallocate;
    if (strncmp(funcName.c_str(), "llvm.cheerp.deallocate", strlen("llvm.cheerp.deallocate")) == 0 ||
        strncmp(funcName.c_str(), "free", strlen("free")) == 0)
        return (void*)(void(*)())pre_execute_deallocate;
    if (strncmp(funcName.c_str(), "llvm.cheerp.get.array.len.", strlen("llvm.cheerp.get.array.len."))==0)
        return (void*)(void(*)())pre_execute_get_array_len;
    if (strncmp(funcName.c_str(), "llvm.cheerp.element.distance.", strlen("llvm.cheerp.element.distance."))==0)
        return (void*)(void(*)())pre_execute_element_distance;
    if (strncmp(funcName.c_str(), "llvm.cheerp.pointer.base.", strlen("llvm.cheerp.pointer.base."))==0)
        return (void*)(void(*)())pre_execute_pointer_base;
    if (strncmp(funcName.c_str(), "llvm.cheerp.pointer.offset.", strlen("llvm.cheerp.pointer.offset."))==0)
        return (void*)(void(*)())pre_execute_pointer_offset;
    if (strncmp(funcName.c_str(), "llvm.memcpy.", strlen("llvm.memcpy."))==0)
        return (void*)(void(*)())pre_execute_memcpy;
    if (strncmp(funcName.c_str(), "llvm.memmove.", strlen("llvm.memmove."))==0)
        return (void*)(void(*)())pre_execute_memmove;
    if (strncmp(funcName.c_str(), "llvm.memset.", strlen("llvm.memset."))==0)
        return (void*)(void(*)())pre_execute_memset;
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
        char*& MallocStartAddress)
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
    size_t cookieSize = allocData.hasCookie ? cheerp::TypeSupport::getArrayCookieSizeAsmJS(*DL, allocData.allocType) : 0;
    // We need to encode the cookie in the new global in asmjs mode, it may be needed if the memory is freed at runtime
    bool encodeCookieInType = allocData.hasCookie && allocData.asmjs;
    if(encodeCookieInType)
        MallocStartAddress = it->first;
    else
        MallocStartAddress = it->first + cookieSize;
    if (allocData.globalValue)
        return allocData.globalValue;
    // We need to promote this memory to a globalvalue
    // Make it an array, if it's more than 1 element long
    uint32_t elementSize = DL->getTypeAllocSize(allocData.allocType);
    Type* newGlobalType = nullptr;
    uint32_t size = (allocData.size-cookieSize) / elementSize;
    if (encodeCookieInType)
    {
        size_t cookieWords = cookieSize / sizeof(uint32_t);
        Type* Int32Ty = IntegerType::get(currentModule->getContext(), 32);
        Type* CookieTy = ArrayType::get(Int32Ty, cookieWords);
        Type* DataTy = ArrayType::get(allocData.allocType, size);
        Type* Tys[] = { CookieTy, DataTy };
        newGlobalType = StructType::create(currentModule->getContext(), Tys);
    }
    else
    {
        newGlobalType = size > 1 ? ArrayType::get(allocData.allocType, size) : allocData.allocType;
    }

    allocData.globalValue = new GlobalVariable(*currentModule, newGlobalType,
            false, GlobalValue::InternalLinkage, nullptr, "promotedMalloc");

    if (allocData.asmjs)
        allocData.globalValue->setSection("asmjs");
    // Build an initializer
    allocData.globalValue->setInitializer(computeInitializerFromMemory(DL, newGlobalType, MallocStartAddress, allocData.asmjs));

    return allocData.globalValue;
}

Constant* PreExecute::computeInitializerFromMemory(const DataLayout* DL,
        Type* memType, char* Addr, bool asmjs)
{
    if (IntegerType* IT=dyn_cast<IntegerType>(memType))
    {
        // Assume little endian
        uint64_t integerVal = 0;
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
        GV = getGlobalForMalloc(DL, StoredAddr, MallocStartAddress);
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


bool PreExecute::runOnConstructor( const llvm::Target* target, const std::string& triple, llvm::Module& m, llvm::Function* func)
{
    bool Changed = false;

    std::string error;
    std::unique_ptr<Module> uniqM(&m);
    TargetMachine* machine = target->createTargetMachine(triple, "", "", TargetOptions(), None);

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

    allocator = std::make_unique<Allocator>(*currentEE->ValueAddresses);

    currentEE->runFunction(func, std::vector< GenericValue >());
    if(currentEE->hasFailed())
    {
        // Execution could not be safely completed. Clean up.
        modifiedGlobals.clear();
        llvm::errs() << "warning: Could not pre-execute global constructor " << func->getName() << "\n";
    }
    else
        Changed = true;

    // Compute new initializer for the modified globals
    for(auto& it: modifiedGlobals)
    {
        GlobalVariable* GV = it.first;
        void* Addr = currentEE->getPointerToGlobal(GV);
        Constant* newInit;
        const DataLayout *DL = &m.getDataLayout();
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

    bool removed = currentEE->removeModule(&m);
    (void)removed;
    assert(removed && "failed to free the module from ExecutionEngine");

    allocator = nullptr;
    delete currentEE;

    currentEE = NULL;

    return Changed;
}

// RAII wrapper for temporarily detaching functions from a module
class FunctionDetacher {
  public:
    FunctionDetacher(Module& m): m(m) {}
    bool detach(const char* fname)
    {
        Function* f = m.getFunction(fname);
        if (f)
        {
            Function* tmp = Function::Create(f->getFunctionType(), f->getLinkage());
            tmp->getBasicBlockList().splice(tmp->begin(), f->getBasicBlockList());
            detached.push_back(std::make_pair(f,tmp));
        }
        return f != nullptr;
    }
    ~FunctionDetacher()
    {
        for (auto d: detached)
        {
            d.first->getBasicBlockList().splice(d.first->begin(), d.second->getBasicBlockList());
            delete d.second;
        }
    }
  private:
    Module& m;
    std::vector<std::pair<Function*, Function*>> detached;
};

bool PreExecute::runOnModule(Module& m)
{
    bool Changed = false;

    currentPreExecutePass = this;
    currentModule = &m;

    std::string error;
    std::string triple = sys::getProcessTriple();
    const Target *target = TargetRegistry::lookupTarget(triple, error);

    GlobalVariable * constructorVar = m.getGlobalVariable("llvm.global_ctors");
    if(constructorVar)
    {
        // Random things which may go boom
        if (!constructorVar->hasInitializer() || !isa<ConstantArray>(constructorVar->getInitializer()))
        {
          constructorVar = nullptr;
        }
    }

    std::vector<Constant*> newConstructors;

    // Detach malloc, free, and realloc, so the interpreter will fail if it will
    // encounter them
    FunctionDetacher FD(m);
    FD.detach("calloc");
    FD.detach("malloc");
    FD.detach("free");
    FD.detach("realloc");
    FD.detach("cALLOc");
    FD.detach("mALLOc");
    FD.detach("fREe");
    FD.detach("rEALLOc");

    if (constructorVar)
    {
        const Constant *initializer = constructorVar->getInitializer();
        const ConstantArray *constructors = cast<ConstantArray>(initializer);

        for (ConstantArray::const_op_iterator it = constructors->op_begin();
             it != constructors->op_end(); ++it)
        {
            Constant *elem = cast<Constant>(*it);
            Function* func = cast<Function>(elem->getAggregateElement(1));
            if(runOnConstructor(target, triple, m, func))
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
        if(runOnConstructor(target, triple, m, mainFunc))
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
            GlobalVariable *NGV = new GlobalVariable(m, newArray->getType(), constructorVar->isConstant(), constructorVar->getLinkage(),
                         newArray, "", constructorVar, constructorVar->getThreadLocalMode());
            NGV->setSection(constructorVar->getSection());
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
