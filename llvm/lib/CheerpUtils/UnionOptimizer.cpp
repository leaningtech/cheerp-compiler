//===-- CallConstructors.cpp - Cheerp backend pass ------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2022-2023 Leaning Technologies
//
// This pass optimizes away calls to cheerp_typed_ptrcast when the result is immediately
// used as the pointer argument for a load, store, or GEP. This avoids materializing
// a typed array for unions unless the pointer escapes.

//===----------------------------------------------------------------------===//


#include "llvm/Cheerp/UnionOptimizer.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instructions.h"
#include <vector>

#define DEBUG_TYPE "unionoptimizer"

using namespace llvm;

namespace cheerp
{

static bool runOnCandidate(Instruction* I) {
	Value* Orig = I->getOperand(0);
	for (Use& U: make_early_inc_range(I->uses())) {
		if (auto* SI = dyn_cast<StoreInst>(U.getUser())) {
			if (SI->getPointerOperand() != U.get())
				continue;
		} else if (auto* LI = dyn_cast<StoreInst>(U.getUser())) {
			if (LI->getPointerOperand() != U.get())
				continue;
		} else if (auto* GEP = dyn_cast<GetElementPtrInst>(U.getUser())) {
			if (GEP->getPointerOperand() != U.get())
				continue;
			GEP->mutateType(GEP->getResultElementType()->getPointerTo(Orig->getType()->getPointerAddressSpace()));
		} else {
			continue;
		}
		U.set(Orig);
	}
	return true;
}

PreservedAnalyses UnionOptimizerPass::run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM)
{
	for (auto& I: instructions(F)) {
		IntrinsicInst* II = dyn_cast<IntrinsicInst>(&I);
		if (II && II->getIntrinsicID() == Intrinsic::cheerp_typed_ptrcast) {

			runOnCandidate(&I);
		}
	}

	return PreservedAnalyses::all();
}

}
