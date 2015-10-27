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
#include "llvm/Target/TargetSubtargetInfo.h"
#include <string.h>
#include <algorithm>

//#define DEBUG_PRE_EXECUTE 1

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
    Type* type = FT->getParamType(0)->getPointerElementType();
    llvm::Module *module = PreExecute::currentPreExecutePass->currentModule;
    uint32_t elementSize = module->getDataLayout()->getTypeAllocSize(type);
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
  llvm::Type *type = FT->getReturnType()->getPointerElementType();
  PreExecute::currentPreExecutePass->recordTypedAllocation(type, size, (char*)ret);
  return GenericValue(ret);
}

static GenericValue pre_execute_deallocate(FunctionType *FT,
                                           const std::vector<GenericValue> &Args) {
#ifdef DEBUG_PRE_EXECUTE
    void *p = (void *)(GVTOP(Args[0]));
#endif

    return GenericValue();
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
    // We need to apply the offset in bytes using the bases metadata
#ifdef DEBUG_PRE_EXECUTE
    llvm::errs()
        << "Upcast from " << *FT->getReturnType()->getPointerElementType()
        << " to " << *FT->getParamType(0)->getPointerElementType()
        << "\n";
#endif
    uintptr_t AddrValue = reinterpret_cast<uintptr_t>(GVTOP(Args[0]));
    return GenericValue(reinterpret_cast<void*>(AddrValue));
}

template<class T>
static GenericValue assertEqual(FunctionType *FT,
        const std::vector<GenericValue> &Args)
{
    const T value = *reinterpret_cast<T*>(GVTOP(Args[0]));
    const T expected = *reinterpret_cast<T*>(GVTOP(Args[1]));
    const char* msg = reinterpret_cast<char *>(GVTOP(Args[2]));
    if (value == expected) {
        llvm::errs() << msg << ": SUCCESS\n";
    } else {
        llvm::errs() << msg << ": FAILURE\n";
    }

    return GenericValue(0);
}

static GenericValue assertAlmostEqual(FunctionType *FT,
                                      const std::vector<GenericValue> &Args)
{
    const double value = Args[0].DoubleVal;
    const double expected = Args[1].DoubleVal;
    const double epsilon = Args[2].DoubleVal;
    const char* msg = reinterpret_cast<char *>(GVTOP(Args[3]));
    if (value >= expected - epsilon && value <= expected + epsilon) {
        llvm::errs() << msg << ": SUCCESS\n";
    } else {
        llvm::errs() << msg << ": FAILURE\n";
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
    if (strncmp(funcName.c_str(), "llvm.cheerp.downcast.", strlen("llvm.cheerp.downcast."))==0)
        return (void*)(void(*)())pre_execute_downcast;
    if (strncmp(funcName.c_str(), "llvm.cheerp.upcast.", strlen("llvm.cheerp.upcast."))==0)
        return (void*)(void(*)())pre_execute_upcast;
    if (strncmp(funcName.c_str(), "llvm.cheerp.allocate.", strlen("llvm.cheerp.allocate."))==0)
        return (void*)(void(*)())pre_execute_allocate;
    if (strcmp(funcName.c_str(), "llvm.cheerp.deallocate") == 0)
        return (void*)(void(*)())pre_execute_deallocate;
    if (strncmp(funcName.c_str(), "llvm.cheerp.element.distance.", strlen("llvm.cheerp.element.distance."))==0)
        return (void*)(void(*)())pre_execute_element_distance;
    if (strncmp(funcName.c_str(), "llvm.memcpy.", strlen("llvm.memcpy."))==0)
        return (void*)(void(*)())pre_execute_memcpy;
    if (strcmp(funcName.c_str(), "_Z11assertEqualIcEvRKT_S2_PKc") == 0)
        return (void*)(void(*)())assertEqual<char>;
    if (strcmp(funcName.c_str(), "_Z11assertEqualIsEvRKT_S2_PKc") == 0)
        return (void*)(void(*)())assertEqual<short>;
    if (strcmp(funcName.c_str(), "_Z11assertEqualItEvRKT_S2_PKc") == 0)
        return (void*)(void(*)())assertEqual<unsigned short>;
    if (strcmp(funcName.c_str(), "_Z11assertEqualIiEvRKT_S2_PKc") == 0)
        return (void*)(void(*)())assertEqual<int>;
    if (strcmp(funcName.c_str(), "_Z11assertEqualIjEvRKT_S2_PKc") == 0)
        return (void*)(void(*)())assertEqual<unsigned int>;
    if (strcmp(funcName.c_str(), "_Z11assertEqualIlEvRKT_S2_PKc") == 0)
        return (void*)(void(*)())assertEqual<long>;
    if (strcmp(funcName.c_str(), "_Z11assertEqualIxEvRKT_S2_PKc") == 0)
        return (void*)(void(*)())assertEqual<long long>;
    if (strcmp(funcName.c_str(), "_Z11assertEqualIbEvRKT_S2_PKc") == 0)
        return (void*)(void(*)())assertEqual<bool>;
    if (strcmp(funcName.c_str(), "_Z11assertEqualIfEvRKT_S2_PKc") == 0)
        return (void*)(void(*)())assertEqual<float>;
    if (strcmp(funcName.c_str(), "_Z11assertEqualIdEvRKT_S2_PKc") == 0)
        return (void*)(void(*)())assertEqual<double>;
    if (strcmp(funcName.c_str(), "_Z11assertEqualdddPKc") == 0)
        return (void*)(void(*)())assertAlmostEqual;
    if (strncmp(funcName.c_str(), "_Z11assertEqualIP",
                strlen("_Z11assertEqualIP")) == 0)
        return (void*)(void(*)())assertEqual<void *>;

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

Constant* PreExecute::findPointerFromGlobal(const DataLayout* DL,
        Type* memType, GlobalValue* GV, char* GlobalStartAddr,
        char* StoredAddr, Type* Int32Ty)
{
    // Build a type safe GEP to the right type at the right offset
    uintptr_t Offset = StoredAddr - GlobalStartAddr;
    llvm::SmallVector<Constant*, 4> Indices;
    // This is needed to dereference global
    Indices.push_back(ConstantInt::get(Int32Ty, 0));
    bool success = getTypeSafeGepForAddress(Indices, Int32Ty, DL,
            GV->getType()->getPointerElementType(),
            memType->getPointerElementType(), Offset);
    if (!success)
    {
        // TODO print warning of forced bitcast
#ifdef DEBUG_PRE_EXECUTE
        llvm::errs() << "Cast: " << *ConstantExpr::getBitCast(GV, memType) << "\n";
#endif
        return ConstantExpr::getBitCast(GV, memType);
    }
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
    uint32_t size = allocData.size / elementSize;
    Type* newGlobalType = ArrayType::get(allocData.allocType, size);

    // Build an initializer
    Constant* init=computeInitializerFromMemory(DL, newGlobalType, it->first);

    allocData.globalValue = new GlobalVariable(*currentModule, newGlobalType,
            false, GlobalValue::InternalLinkage, init, "promotedMalloc");
    return allocData.globalValue;
}

Constant* PreExecute::computeInitializerFromMemory(const DataLayout* DL,
        Type* memType, char* Addr)
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
                    ST->getElementType(i), elementAddr);
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
                    elementType, elementAddr);
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
        llvm_unreachable("Could not get pointer");
    }
    return NULL;
}

bool PreExecute::runOnModule(Module& m)
{
    bool Changed = false;

    std::string error;
    std::string triple = sys::getDefaultTargetTriple();
    const Target *target = TargetRegistry::lookupTarget(triple, error);
    TargetMachine* machine;
    machine = target->createTargetMachine(triple, "", "", TargetOptions());

    std::unique_ptr<Module> uniqM(&m);

    currentPreExecutePass = this;
    currentModule = &m;

    EngineBuilder builder(std::move(uniqM));
    builder.setEngineKind(llvm::EngineKind::PreExecuteInterpreter);
    builder.setOptLevel(CodeGenOpt::Default);
    builder.setErrorStr(&error);
    builder.setVerifyModules(true);

    currentEE = builder.create(machine);
    assert(currentEE && "failed to create execution engine!");
    currentEE->InstallStoreListener(StoreListener);
    currentEE->InstallLazyFunctionCreator(LazyFunctionCreator);
    GlobalVariable * constructorVar = m.getGlobalVariable("llvm.global_ctors");

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
            currentEE->runFunction(func, std::vector< GenericValue >());
        }
    }

#if 0
    Function* mainFunc = m.getFunction("_Z7webMainv");
    assert(mainFunc && "unable to find function webMain in module!");
    currentEE->runFunction(mainFunc, std::vector<GenericValue>());
#endif

    // Compute new initializer for the modified globals
    for(auto& it: modifiedGlobals)
    {
        GlobalVariable* GV = it.first;
        void* Addr = currentEE->getPointerToGlobal(GV);
        Constant* newInit;
        const DataLayout *DL = m.getDataLayout();
        Type *ptrType = GV->getType()->getPointerElementType();
        newInit = computeInitializerFromMemory(DL, ptrType, (char*)Addr);
        assert(newInit);
        it.second = newInit;
        Changed = true;
    }

    // Set new initializers for the modified globals
    for(auto& it: modifiedGlobals)
    {
        assert(it.second);
        it.first->setInitializer(it.second);
    }

#ifdef DEBUG_PRE_EXECUTE
    currentEE->printMemoryStats();
#endif

    // Delete global constructors and remove the main body
    if (constructorVar)
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
