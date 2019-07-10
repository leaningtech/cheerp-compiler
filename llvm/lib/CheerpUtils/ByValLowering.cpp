//===-- ByValLowering.cpp - Cheerp optimization pass --------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "CheerpByValLowering"
#include "llvm/Cheerp/ByValLowering.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Support/raw_ostream.h"

STATISTIC(NumByValLowered, "Number of byval arguments lowered");
STATISTIC(NumNewAllocas, "Number of new allocas created");

namespace llvm {

static AttributeSet ChangeAttrs(LLVMContext& Context, AttributeSet Attrs)
{
	SmallVector<AttributeSet, 8> AttrList;
	for (unsigned Slot = 0; Slot < Attrs.getNumSlots(); ++Slot)
	{
		unsigned Index = Attrs.getSlotIndex(Slot);
		AttrBuilder AB;
		for (auto Attr = Attrs.begin(Slot), E = Attrs.end(Slot); Attr != E; ++Attr)
		{
			if (Attr->isEnumAttribute()
				&& Attr->getKindAsEnum() != Attribute::ByVal)
			{
				AB.addAttribute(*Attr);
			}
			// IR semantics require that ByVal implies NoAlias.
			if (Attr->isEnumAttribute()
				&& Attr->getKindAsEnum() == Attribute::ByVal)
			{
				++NumByValLowered;
				AB.addAttribute(Attribute::get(Context, Attribute::NoAlias));
				//AB.addAttribute(Attribute::get(Context, Attribute::NoCapture));
			}
		}
		AttrList.push_back(AttributeSet::get(Context, Index, AB));
	}
	return AttributeSet::get(Context, AttrList);
}

static bool ExpandCall(const DataLayout& DL, CallInst* Call)
{
	bool Modify = false;
	AttributeSet Attrs = Call->getAttributes();
	for (unsigned ArgIdx = 0; ArgIdx < Call->getNumArgOperands(); ++ArgIdx)
	{
		unsigned AttrIdx = ArgIdx + 1;
		if (Attrs.hasAttribute(AttrIdx, Attribute::ByVal))
		{
			Modify = true;
			Value *ArgPtr = Call->getArgOperand(ArgIdx);
			Type *ArgType = ArgPtr->getType()->getPointerElementType();
			ConstantInt *ArgSize = ConstantInt::get(
				Call->getContext(), APInt(64, DL.getTypeStoreSize(ArgType)));
			unsigned Alignment = Attrs.getParamAlignment(AttrIdx);
			unsigned AllocAlignment =
					std::max(Alignment, DL.getABITypeAlignment(ArgType));
			// Make a copy of the byval argument.
			Instruction *CopyBuf = new AllocaInst(ArgType, 0, AllocAlignment,
				ArgPtr->getName() + ".byval_copy");
			NumNewAllocas++;
			Function *Func = Call->getParent()->getParent();
			Func->getEntryBlock().getInstList().push_front(CopyBuf);
			IRBuilder<> Builder(Call);
			Builder.CreateLifetimeStart(CopyBuf, ArgSize);
			// Using the argument's alignment attribute for the memcpy
			// should be OK because the LLVM Language Reference says that
			// the alignment attribute specifies "the alignment of the stack
			// slot to form and the known alignment of the pointer specified
			// to the call site".
			Instruction *MemCpy = Builder.CreateMemCpy(CopyBuf, ArgPtr, ArgSize,
				Alignment, false, nullptr, nullptr, nullptr, nullptr, false);
			MemCpy->setDebugLoc(Call->getDebugLoc());
			Call->setArgOperand(ArgIdx, CopyBuf);
			// Mark the argument copy as unused using llvm.lifetime.end.
			// TODO: handle invoke
			assert(isa<CallInst>(Call));
			Builder.SetInsertPoint(Call->getNextNode());
			Builder.CreateLifetimeEnd(CopyBuf, ArgSize);
		}
	}
	if (Modify)
	{
		Call->setAttributes(ChangeAttrs(Call->getContext(), Attrs));
		if (CallInst *CI = dyn_cast<CallInst>(Call))
		{
			// This is no longer a tail call because the callee references
			// memory alloca'd by the caller.
			CI->setTailCall(false);
		}
	}
	return Modify;
}

bool ByValLowering::runOnModule(Module& M)
{
	const DataLayout& DL = M.getDataLayout();
	bool Modified = false;
	for (auto& Func: M.functions())
	{
		AttributeSet Attrs = Func.getAttributes();
		AttributeSet NewAttrs = ChangeAttrs(Func.getContext(), Attrs);
		Modified |= (NewAttrs != Attrs);
		if (Modified)
			Func.setAttributes(NewAttrs);
		for (auto BB = Func.begin(), E = Func.end(); BB != E; ++BB)
		{
			for (auto Inst = BB->begin(), E = BB->end(); Inst != E; ++Inst)
			{
				if (CallInst *Call = dyn_cast<CallInst>(Inst))
					Modified |= ExpandCall(DL, Call);
				// TODO: handle Invoke
			}
		}
	}
	return Modified;
}

const char* ByValLowering::getPassName() const
{
	return "ByValLowering";
}

char ByValLowering::ID = 0;

void ByValLowering::getAnalysisUsage(AnalysisUsage & AU) const
{
	llvm::Pass::getAnalysisUsage(AU);
}

ModulePass *createByValLoweringPass() { return new ByValLowering(); }

}

using namespace llvm;
INITIALIZE_PASS_BEGIN(ByValLowering, "ByValLowering",
        "Lower byval arguments to alloca and memcpy", false, false)
INITIALIZE_PASS_END(ByValLowering, "ByValLowering",
        "Lower byval arguments to alloca and memcpy", false, false)