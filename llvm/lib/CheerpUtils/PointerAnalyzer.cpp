//===-- PointerAnalyzer.cpp - The Cheerp JavaScript generator -------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2011-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/InitializePasses.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/PointerAnalyzer.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Operator.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/Debug.h"
#include <numeric>

using namespace llvm;

namespace cheerp {

bool PointerAnalyzer::runOnModule(Module& M)
{
	return false;
}

static POINTER_KIND getPointerKindForType(const Type* t) {
	if(!t->isPointerTy())
		t->dump();
	assert(t->isPointerTy());
	switch(getCheerpAS(cast<PointerType>(t))) {
		case CheerpAS::Default: {
			t->dump();
			report_fatal_error("default as");
		}
		case CheerpAS::Client: {
			return COMPLETE_OBJECT;
		}
		case CheerpAS::GenericJS: {
			return REGULAR;
		}
		case CheerpAS::ByteLayout: {
			return BYTE_LAYOUT;
		}
		case CheerpAS::Wasm: {
			return RAW;
		}
	}
}
POINTER_KIND PointerAnalyzer::getPointerKindAssert(const Value* p) const
{
	auto r = getPointerKind(p);
	assert(r != CONSTANT);
	return r;
}
POINTER_KIND PointerAnalyzer::getPointerKind(const Value* p) const
{
	// TODO: do something better for nullptr
	if (isa<ConstantPointerNull>(p)) {
		return CONSTANT;
	}
	if (auto* SI = dyn_cast<StoreInst>(p)) {
		return getPointerKindForType(SI->getPointerOperandType());
	}
	if(p->getType()->getPointerAddressSpace()==0) {
		p->dump();
		if (auto* I = dyn_cast<Instruction>(p)) {
			I->getFunction()->dump();
		}
	}
	assert(p->getType()->getPointerAddressSpace()!=0);
	POINTER_KIND ret =  getPointerKindForType(p->getType());
	if (ret == REGULAR && isa<CallBase>(p)) {
		const CallBase* CB = cast<CallBase>(p);
		if (!(CB->getCalledFunction() && CB->getCalledFunction()->getIntrinsicID()))
			ret = SPLIT_REGULAR;
	}
	return ret;
}

POINTER_KIND PointerAnalyzer::getPointerKindForReturn(const Function* F) const
{
	POINTER_KIND ret = getPointerKindForType(F->getReturnType());
	if (ret == REGULAR)
		return SPLIT_REGULAR;
	return ret;
}

POINTER_KIND PointerAnalyzer::getPointerKindForStoredType(Type* pointerType) const
{
	return getPointerKindForType(pointerType);
}

POINTER_KIND PointerAnalyzer::getPointerKindForArgumentTypeAndIndex( const TypeAndIndex& argTypeAndIndex ) const
{
	POINTER_KIND ret = getPointerKindForType(argTypeAndIndex.type);
	if (ret == REGULAR)
		return SPLIT_REGULAR;
	return ret;
}

POINTER_KIND PointerAnalyzer::getPointerKindForJSExportedType (Type* jsexportedType) const
{
	StructType* Ty = cast<StructType>(jsexportedType);
	if (Ty->hasByteLayout()) {
		return BYTE_LAYOUT;
	} else if (Ty->hasAsmJS()) {
		return RAW;
	} else {
		return REGULAR;
	}
}

POINTER_KIND PointerAnalyzer::getPointerKindForArgument( const llvm::Argument* A ) const
{
	POINTER_KIND ret = getPointerKindForType(A->getType());
	if (ret == REGULAR)
		return SPLIT_REGULAR;
	return ret;
}

POINTER_KIND PointerAnalyzer::getPointerKindForMemberPointer(const TypeAndIndex& baseAndIndex) const
{
	return getPointerKindForType(cast<StructType>(baseAndIndex.type)->getElementType(baseAndIndex.index));
}

POINTER_KIND PointerAnalyzer::getPointerKindForMember(const TypeAndIndex& baseAndIndex) const
{
	Type* memberTy = cast<StructType>(baseAndIndex.type)->getElementType(baseAndIndex.index);
	if (StructType* STy = dyn_cast<StructType>(memberTy)) {
		if (STy->hasByteLayout())
			return BYTE_LAYOUT;
	}
	return REGULAR;
}

const ConstantInt* PointerAnalyzer::getConstantOffsetForPointer(const Value * v) const
{
	return nullptr;
}

const llvm::ConstantInt* PointerAnalyzer::getConstantOffsetForMember( const TypeAndIndex& baseAndIndex ) const
{
	return nullptr;
}

void PointerAnalyzer::invalidate(const Value * v)
{
	assert(status == MODIFIABLE);
}

void PointerAnalyzer::fullResolve()
{
	if (status == FULLY_RESOLVED)
		return;
	status = CACHING_STARTED;
	status = FULLY_RESOLVED;
}

void PointerAnalyzer::computeConstantOffsets(const Module& M)
{
	assert(status == FULLY_RESOLVED);
}

PreservedAnalyses PointerAnalyzerPass::run(Module& M, ModuleAnalysisManager& MAM)
{
	PointerAnalyzer& inner = MAM.getResult<PointerAnalysis>(M).getInner();
	bool res = inner.runOnModule(M);
	(void)res;
	assert(!res);
	return PreservedAnalyses::all();
}

AnalysisKey PointerAnalysis::Key;
PointerAnalyzer* PointerAnalysisWrapper::innerPtr{nullptr};

}
