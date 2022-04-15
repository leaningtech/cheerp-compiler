//===-- ByValLowering.cpp - Cheerp optimization pass --------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017-2022 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/InitializePasses.h"
#include "llvm/Cheerp/ByValLowering.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Support/raw_ostream.h"

#define DEBUG_TYPE "CheerpByValLowering"
STATISTIC(NumNewAllocas, "Number of new allocas created");

namespace llvm {

static bool ExpandCall(const DataLayout& DL, CallBase* Call)
{
	bool Modify = false;
	AttributeList Attrs = Call->getAttributes();
	for (unsigned ArgIdx = 0; ArgIdx < Call->arg_size(); ++ArgIdx)
	{
		unsigned AttrIdx = ArgIdx + 1;
		if (Attrs.hasAttributeAtIndex(AttrIdx, Attribute::ByVal))
		{
			Modify = true;
			Value *ArgPtr = Call->getArgOperand(ArgIdx);
			Type *ArgType = ArgPtr->getType()->getPointerElementType();
			ConstantInt *ArgSize = ConstantInt::get(
				Call->getContext(), APInt(64, DL.getTypeStoreSize(ArgType)));
			unsigned AllocAlignment = DL.getABITypeAlignment(ArgType);
			auto Alignment = Attrs.getParamAlignment(ArgIdx);
			if(Alignment.hasValue())
			{
				Align a = Alignment.getValue();
				if(a.value() > AllocAlignment)
					AllocAlignment = a.value();
			}
			// Make a copy of the byval argument.
			Instruction *CopyBuf = new AllocaInst(ArgType, 0, 0, Align(AllocAlignment),
				ArgPtr->getName() + ".byval_copy");
			NumNewAllocas++;
			Function *Func = Call->getParent()->getParent();
			Func->getEntryBlock().getInstList().push_front(CopyBuf);
			IRBuilder<> Builder(Call);
			// TODO: Support lifetime for invokes
			if(isa<CallInst>(Call))
				Builder.CreateLifetimeStart(CopyBuf, ArgSize);
			// Using the argument's alignment attribute for the memcpy
			// should be OK because the LLVM Language Reference says that
			// the alignment attribute specifies "the alignment of the stack
			// slot to form and the known alignment of the pointer specified
			// to the call site".
			Instruction *MemCpy = Builder.CreateMemCpy(CopyBuf, MaybeAlign(AllocAlignment), ArgPtr, MaybeAlign(AllocAlignment), ArgSize,
				false, nullptr, nullptr, nullptr, nullptr, false);
			MemCpy->setDebugLoc(Call->getDebugLoc());
			Call->setArgOperand(ArgIdx, CopyBuf);
			// Mark the argument copy as unused using llvm.lifetime.end.
			// TODO: handle invoke
			if(isa<CallInst>(Call))
			{
				Builder.SetInsertPoint(Call->getNextNode());
				Builder.CreateLifetimeEnd(CopyBuf, ArgSize);
			}
			Call->removeAttributeAtIndex(AttrIdx, Attribute::ByVal);
			Call->addAttributeAtIndex(AttrIdx, Attribute::NoAlias);
		}
	}
	if (Modify && isa<CallInst>(Call))
	{
		// This is no longer a tail call because the callee references
		// memory alloca'd by the caller.
		cast<CallInst>(Call)->setTailCall(false);
	}
	return Modify;
}

bool ByValLowering::runOnModule(Module& M)
{
	return ByValLoweringPass::runOnModule(M);
}

bool ByValLoweringPass::runOnModule(Module& M)
{
	const DataLayout& DL = M.getDataLayout();
	bool Modified = false;
	for (auto& Func: M.functions())
	{
		AttributeList Attrs = Func.getAttributes();
		for (unsigned ArgIdx = 0; ArgIdx < Func.arg_size(); ++ArgIdx)
		{
			unsigned AttrIdx = ArgIdx + 1;
			if (Attrs.hasAttributeAtIndex(AttrIdx, Attribute::ByVal))
			{
				Func.removeAttributeAtIndex(AttrIdx, Attribute::ByVal);
				Func.addAttributeAtIndex(AttrIdx, Attribute::get(M.getContext(), Attribute::NoAlias));
				Modified |= true;
			}
		}
		for (auto BB = Func.begin(), E = Func.end(); BB != E; ++BB)
		{
			for (auto Inst = BB->begin(), E = BB->end(); Inst != E; ++Inst)
			{
				if (CallBase *Call = dyn_cast<CallBase>(Inst))
					Modified |= ExpandCall(DL, Call);
			}
		}
	}
	return Modified;
}

PreservedAnalyses ByValLoweringPass::run(Module& Module, ModuleAnalysisManager&)
{
  if (runOnModule(Module))
    return PreservedAnalyses::none();
  return PreservedAnalyses::all();
}

}
