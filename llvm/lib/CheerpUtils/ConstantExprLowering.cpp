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
		for (auto& O: Cur->operands())
		{
			if (ConstantExpr* CE = dyn_cast<ConstantExpr>(O.get()))
			{
				Instruction* Conv = CE->getAsInstruction();
				if (Conv->getType()->isIntegerTy(64))
					hasI64 |= true;
				if (PHINode* P = dyn_cast<PHINode>(Cur))
				{
					BasicBlock* Incoming = P->getIncomingBlock(O);
					Conv->insertBefore(Incoming->getTerminator());
				}
				else
				{
					Conv->insertBefore(Cur);
				}
				O.set(Conv);
				Stack.push_back(Conv);
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
