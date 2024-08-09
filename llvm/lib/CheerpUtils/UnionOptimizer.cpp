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
#include "llvm/ADT/Twine.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instructions.h"
#include <vector>

#define DEBUG_TYPE "unionoptimizer"

using namespace llvm;

namespace cheerp
{

static bool runOnCandidate(Instruction* I, Value* Orig) {
	bool Changed = false;
	for (Use& U: make_early_inc_range(I->uses())) {
		if (auto* SI = dyn_cast<StoreInst>(U.getUser())) {
			if (SI->getPointerOperand() == U.get()) {
				U.set(Orig);
				Changed = true;
			}
		} else if (auto* LI = dyn_cast<StoreInst>(U.getUser())) {
			if (LI->getPointerOperand() == U.get()) {
				U.set(Orig);
				Changed = true;
			}
		} else if (auto* GEP = dyn_cast<GetElementPtrInst>(U.getUser())) {
			if (GEP->getPointerOperand() == U.get()) {
				// Tentatively create a new GEP, but keep the old one in case it has uses
				// that are not store/load of a basic type
				auto* NewGEP = GEP->clone();
				NewGEP->insertBefore(GEP);
				NewGEP->setName(Twine(GEP->getName(), ".uniongepopt"));
				NewGEP->mutateType(GEP->getResultElementType()->getPointerTo(Orig->getType()->getPointerAddressSpace()));
				NewGEP->setOperand(U.getOperandNo(), Orig);
				// Recurse and replace store/loads from the GEP with NewGEP
				Changed |= runOnCandidate(GEP, NewGEP);
			}
		}
	}
	return Changed;
}

PreservedAnalyses UnionOptimizerPass::run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM)
{
	bool Changed = false;
	for (auto& I: instructions(F)) {
		IntrinsicInst* II = dyn_cast<IntrinsicInst>(&I);
		if (II && II->getIntrinsicID() == Intrinsic::cheerp_typed_ptrcast) {

			Changed |= runOnCandidate(II, II->getOperand(0));
		}
	}

	return Changed? PreservedAnalyses::none() : PreservedAnalyses::all();
}

}
