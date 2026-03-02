//===-- CheckLoadStore.cpp - Check addresses before loads/stores ----------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2026 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/InitializePasses.h"
#include "llvm/Cheerp/CheckedLoadStore.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"

using namespace llvm;

namespace cheerp
{

bool CheckLoadStore::runOnFunction(Module& M, Function& F)
{
	// Inject the cheerp_checked_load and cheerp_checked_store intrinsics
	// in place of all plain loads and stores. The backend will insert
	// conditional calls to imported helpers to access memory mapped
	// in the negative space.
	std::vector<Instruction*> toDelete;
	for ( BasicBlock & BB : F )
	{
		// Be mindful of the manual increment of the iterator
		for ( BasicBlock::iterator it = BB.begin(); it != BB.end(); ++it)
		{
			Instruction* I = &*it;
			if(LoadInst* LI = dyn_cast<LoadInst>(I))
			{
				if(LI->isAtomic())
					continue;
				// Constant pointers, including globals, can never be mapped
				Value* pointerOperand = LI->getPointerOperand();
				if(isa<Constant>(pointerOperand))
					continue;
				Type* loadType = LI->getType();
				Type* pointerType = pointerOperand->getType();
				Function* checkedLoad = Intrinsic::getDeclaration(&M, Intrinsic::cheerp_checked_load, { loadType, pointerType, pointerType } );
				CallInst* CI = CallInst::Create(checkedLoad, { pointerOperand, pointerOperand }, "", LI);
				LI->replaceAllUsesWith(CI);
				toDelete.push_back(LI);
			}
			else if(StoreInst* SI = dyn_cast<StoreInst>(I))
			{
				if(SI->isAtomic())
					continue;
				// Constant pointers, including globals, can never be mapped
				Value* pointerOperand = SI->getPointerOperand();
				if(isa<Constant>(pointerOperand))
					continue;
				Value* valueOperand = SI->getValueOperand();
				Type* storeType = valueOperand->getType();
				Type* pointerType = pointerOperand->getType();
				Function* checkedStore = Intrinsic::getDeclaration(&M, Intrinsic::cheerp_checked_store, { pointerType, pointerType, storeType, storeType });
				CallInst::Create(checkedStore, { pointerOperand, pointerOperand, valueOperand, valueOperand }, "", SI);
				// No need to replace users, stores are void
				toDelete.push_back(SI);
			}
		}
	}
	for(Instruction* I : toDelete)
	{
		I->eraseFromParent();
	}
	return false;
}

llvm::PreservedAnalyses CheckLoadStorePass::run(llvm::Module& M, llvm::ModuleAnalysisManager& MAM)
{
	CheckLoadStore inner;
	for (Function &F : M)
	{
		if(F.empty() || F.getSection() != "asmjs")
			continue;
		inner.runOnFunction(M, F);
	}

	return PreservedAnalyses::all();
}

}
