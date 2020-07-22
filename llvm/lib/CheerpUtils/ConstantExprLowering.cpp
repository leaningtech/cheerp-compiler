//===-- ConstantExprLowering.cpp - Cheerp helper -------------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2020 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/ConstantExprLowering.h"
#include "llvm/Cheerp/I64Lowering.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"

using namespace llvm;



namespace cheerp
{

StringRef ConstantExprLowering::getPassName() const {
	return "ConstantExprLowering";
}


static bool runOnInstruction(Instruction* I, bool& hasI64)
{
	bool Changed = false;
	std::vector<Instruction*> Stack;
	Stack.push_back(I);
	while(!Stack.empty())
	{
		Instruction* Cur = Stack.back();
		Stack.pop_back();
		SmallDenseMap<BasicBlock*, Instruction*> PHICache;
		for (auto& O: Cur->operands())
		{
			if (ConstantExpr* CE = dyn_cast<ConstantExpr>(O.get()))
			{
				if (CE->getType()->isIntegerTy(64))
					hasI64 |= true;

				Instruction* Conv;

				if (PHINode* P = dyn_cast<PHINode>(Cur))
				{
					BasicBlock* Incoming = P->getIncomingBlock(O);
					auto it = PHICache.find(Incoming);
					if (it != PHICache.end())
					{
						Conv = it->second;
					}
					else
					{
						Conv = CE->getAsInstruction();
						Conv->insertBefore(Incoming->getTerminator());
						PHICache.insert(std::make_pair(Incoming, Conv));
					}
				}
				else
				{
					Conv = CE->getAsInstruction();
					Conv->insertBefore(Cur);
				}
				Stack.push_back(Conv);
				O.set(Conv);
				Changed = true;
			}
		}
	}
	return Changed;
}
bool ConstantExprLowering::runOnFunction(Function& F)
{
	bool Changed = false;
	bool hasI64 = false;

	for (BasicBlock& BB: F)
	{
		for (Instruction& I: BB)
		{
			Changed = runOnInstruction(&I, hasI64);
		}
	}
	if (hasI64)
	{
		I64Lowering I64Low;
		Changed |= I64Low.runOnFunction(F);
	}

	return Changed;
}

char ConstantExprLowering::ID = 0;

FunctionPass *createConstantExprLoweringPass() { return new ConstantExprLowering(); }

}

using namespace cheerp;

INITIALIZE_PASS_BEGIN(ConstantExprLowering, "ConstantExprLowering", "Converts ConstExpr into regular instructions",
                      false, false)
INITIALIZE_PASS_END(ConstantExprLowering, "ConstantExprLowering", "Converts ConstExpr into regular instructions",
                    false, false)
