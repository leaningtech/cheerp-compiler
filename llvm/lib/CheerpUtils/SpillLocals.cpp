//===-- SpillLocals.cpp - Push locals to the reserved stack ---------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2025 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/InitializePasses.h"
#include "llvm/Cheerp/SpillLocals.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/IntrinsicInst.h"

using namespace llvm;

namespace cheerp
{

bool SpillLocals::runOnFunction(Module& M, Function& F, const PointerAnalyzer& PA)
{
	// For the CheerpOS target we conventionally need to spill all locals to the stack space
	// just below the stack pointer at function entry. This space sits above the space allocated
	// for regular stack memory.
	//
	// Spilling happens by adding an intrinsic call after every instruction that has a local assigned,
	// this is somewhat fragile as we need to make sure this extra instruction does not cause any change
	// in the decisions about a local being required.
	//
	// The spilled locals have two functions.
	// 1. They make local values observable to application implementing consevative garbage	collection via stack scanning
	// 2. They are used when re-entering execution in the CheerpOs interpreter
	//
	// TODO: We could significantly reduce the overhead here by using the lifetime information computed
	//       by registerize to only spill locals that are live across a function call. It might even be
	//       possible to ignore some classes of functions.
	auto AddLocalStore = [](llvm::Module& M, llvm::Value* savedStack, llvm::Value* I, llvm::Instruction* insertionPoint)
	{
		Function* localStore = Intrinsic::getDeclaration(&M, Intrinsic::cheerp_local_store, { I->getType() });
		CallInst::Create(localStore, { savedStack, I }, "", insertionPoint);
	};
	// We need to delay adding stores until we find the saved stack pointer
	std::vector<llvm::Instruction*> delayedLocals;
	llvm::Value* reservedStack = nullptr;
	for ( BasicBlock & BB : F )
	{
		// Be mindful of the manual increment of the iterator
		for ( BasicBlock::iterator it = BB.begin(); it != BB.end(); ++it)
		{
			Instruction* I = &*it;
			if(!I->hasNUsesOrMore(1))
			{
				// Skip values that cannot have a local assigned. Void is included.
				continue;
			}
			if(cheerp::isInlineable(*I, PA))
			{
				// Inlineable values do not have a local
				continue;
			}
			if(llvm::IntrinsicInst* ii = dyn_cast<llvm::IntrinsicInst>(I))
			{
				// Store away the reserved stack
				if(ii->getIntrinsicID() == llvm::Intrinsic::cheerp_locals_stack)
				{
					assert(reservedStack == nullptr);
					reservedStack = ii;
					// Store function arguments
					for(Argument& arg : F.args())
					{
						AddLocalStore(M, reservedStack, &arg, I->getNextNode());
					}
					// Store delayed locals
					for(Instruction* delayedLocal : delayedLocals)
					{
						AddLocalStore(M, reservedStack, delayedLocal, I->getNextNode());
					}
					delayedLocals.clear();
				}
			}
			// This value must have a local assigned
			if(reservedStack == nullptr)
			{
				// Delay storing the local until we identify the saved stack pointer
				delayedLocals.push_back(I);
				continue;
			}
			llvm::Instruction* insertionPoint = I->getNextNode();
			if(isa<PHINode>(insertionPoint))
				insertionPoint = BB.getFirstNonPHI();
			AddLocalStore(M, reservedStack, I, insertionPoint);
		}
	}
	return false;
}

llvm::PreservedAnalyses SpillLocalsPass::run(llvm::Module& M, llvm::ModuleAnalysisManager& MAM)
{
	cheerp::PointerAnalyzer &PA = MAM.getResult<cheerp::PointerAnalysis>(M);

	SpillLocals inner;
	for (Function &F : M)
	{
		if(F.empty() || F.getSection() != "asmjs")
			continue;
		inner.runOnFunction(M, F, PA);
	}

	return PreservedAnalyses::all();
}

}
