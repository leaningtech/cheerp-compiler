//===-- BitCastLowering.cpp - Cheerp helper -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/BitCastLowering.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/Cheerp/Utility.h"

using namespace llvm;

namespace cheerp
{

PreservedAnalyses BitCastLoweringPass::run(Function& F, FunctionAnalysisManager& FAM)
{
	if (LinearOutput != AsmJs || F.getSection() != "asmjs")
		return PreservedAnalyses::all();

	bool Changed = false;
	Value* BitCastSlot = F.getParent()->getGlobalVariable("cheerpBitCastSlot");
	for(Instruction& I: make_early_inc_range(instructions(F))) 
	{
		if (BitCastInst* BI = dyn_cast<BitCastInst>(&I)) {
			Type* RetTy = BI->getType();
			Value* Arg = BI->getOperand(0);
			Type* ArgTy = Arg->getType();
			if(!RetTy->isFloatingPointTy() && !RetTy->isIntegerTy())
				continue;
			if(!ArgTy->isFloatingPointTy() && !ArgTy->isIntegerTy())
				continue;

			IRBuilder<> Builder(&I);

			Value* CastSrc = Builder.CreateBitCast(BitCastSlot, ArgTy->getPointerTo());
			Builder.CreateStore(Arg, CastSrc);
			Value* CastDst = Builder.CreateBitCast(BitCastSlot, RetTy->getPointerTo());
			Value* Ret = Builder.CreateLoad(RetTy, CastDst);

			I.replaceAllUsesWith(Ret);
			I.eraseFromParent();
			Changed = true;
		}
	}
	if (!Changed)
		return PreservedAnalyses::all(); 
	PreservedAnalyses PA;
	PA.preserveSet<CFGAnalyses>();
	return PA;
}

}
