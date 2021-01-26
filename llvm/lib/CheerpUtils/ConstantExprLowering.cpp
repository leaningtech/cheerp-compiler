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

#include "llvm/InitializePasses.h"
#include "llvm/Cheerp/ConstantExprLowering.h"
#include "llvm/Cheerp/I64Lowering.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Analysis/ConstantFolding.h"

using namespace llvm;

namespace cheerp
{

StringRef ConstantExprLowering::getPassName() const {
	return "ConstantExprLowering";
}

Constant* ConstantExprLowering::visitConstantExpr(const ConstantExpr *CE, SmallDenseMap<Constant *, Constant *> &FoldedOps)
{
	SmallVector<Constant *, 8> Ops;
	for (const Use &NewU : CE->operands())
	{
		auto *NewC = cast<Constant>(&NewU);
		// Recursively fold the ConstantExpr's operands. If we have already folded
		// a ConstantExpr, we don't have to process it again.
		if (auto *NewCE = dyn_cast<ConstantExpr>(NewC))
		{
			auto It = FoldedOps.find(NewC);
			if (It == FoldedOps.end())
			{
				if (auto *FoldedC = visitConstantExpr(NewCE, FoldedOps))
				{
					FoldedOps.insert({NewC, FoldedC});
					NewC = FoldedC;
				}
				else
				{
					FoldedOps.insert({NewC, NewC});
				}
			}
			else
			{
				NewC = It->second;
			}
		}
		else if (auto *GV = dyn_cast<GlobalVariable>(NewC))
		{
			// In asmjs, addresses of globals  are just integers
			// Ask LinearMemoryHelper for the value and cast to the pointer type
			if (GV->GlobalValue::getSection() == StringRef("asmjs"))
			{
				auto *CI = ConstantInt::get(IntegerType::get(CE->getContext(), 32), LH->getGlobalVariableAddress(GV));
				NewC = ConstantExpr::getIntToPtr(CI, NewC->getType());
			}
		}
		else if (isa<UndefValue>(NewC) && NewC->getType()->isFloatingPointTy())
		{
			// For some reason, undef floating points are not folded
			// So we replace them with NaNs
			NewC = ConstantFP::getNaN(NewC->getType());
		}
		Ops.push_back(NewC);
	}
	auto Opcode = CE->getOpcode();
	// Handle easy binops first.
	if (Instruction::isBinaryOp(Opcode))
		return ConstantFoldBinaryOpOperands(Opcode, Ops[0], Ops[1], *DL);
	if (CE->isCompare())
		return ConstantFoldCompareInstOperands(CE->getPredicate(), Ops[0], Ops[1], *DL);
	if (Instruction::isCast(Opcode))
		return ConstantFoldCastOperand(Opcode, Ops[0], CE->getType(), *DL);

	// Manually fold GEPs of globals. Maybe we can let llvm do this too
	if (auto *GEP = dyn_cast<GEPOperator>(CE)) {
		ArrayRef<Constant*> Indices = Ops;
		Indices = Indices.slice(1);
		ConstantExpr* ITP = dyn_cast<ConstantExpr>(Ops[0]);
		if (ITP && ITP->getOpcode() == Instruction::IntToPtr)
		{
			if (auto *Base = dyn_cast<ConstantInt>(ITP->getOperand(0)))
			{
				uint32_t Addr = Base->getValue().getZExtValue();
				Type* curTy = CE->getOperand(0)->getType();

				for (Constant* idx: Indices)
				{
					int64_t index = cast<ConstantInt>(idx)->getZExtValue();
					//curTy is modifyed by partialOffset
					Addr += partialOffset(curTy, *DL, index);
				}
				auto *CI = ConstantInt::get(IntegerType::get(CE->getContext(), 32), Addr);
				return ConstantExpr::getIntToPtr(CI, CE->getType());
			}
		}
		return ConstantExpr::getGetElementPtr(GEP->getSourceElementType(), Ops[0],
			Indices, GEP->isInBounds(),
			GEP->getInRangeIndex());
	}

	return nullptr;
}

bool ConstantExprLowering::runOnInstruction(Instruction* I, bool& hasI64)
{
	bool Changed = false;
	std::vector<Instruction*> Stack;
	Stack.push_back(I);
	while(!Stack.empty())
	{
		Instruction* Cur = Stack.back();
		Stack.pop_back();
		SmallDenseMap<BasicBlock*, Instruction*> PHICache;
		SmallDenseMap<Constant*, Constant*> FoldedOps;
		for (auto& O: Cur->operands())
		{
			if (ConstantExpr* CE = dyn_cast<ConstantExpr>(O.get()))
			{
				if (CE->getType()->isIntegerTy(64))
					hasI64 |= true;

				if (Constant* C = visitConstantExpr(CE, FoldedOps))
				{
					if (isa<ConstantExpr>(C))
						CE = cast<ConstantExpr>(C);
					else
					{
						O.set(C);
						Changed = true;
						continue;
					}
				}

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

	LH = &getAnalysis<cheerp::LinearMemoryHelper>();
	DL = &F.getParent()->getDataLayout();

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

void ConstantExprLowering::getAnalysisUsage(llvm::AnalysisUsage & AU) const
{
	AU.addRequired<cheerp::LinearMemoryHelper>();
	AU.addPreserved<cheerp::LinearMemoryHelper>();
	AU.addPreserved<Registerize>();
	AU.addPreserved<cheerp::GlobalDepsAnalyzer>();
	AU.setPreservesCFG();
	llvm::Pass::getAnalysisUsage(AU);
}

char ConstantExprLowering::ID = 0;

FunctionPass *createConstantExprLoweringPass() { return new ConstantExprLowering(); }

}

using namespace cheerp;

INITIALIZE_PASS_BEGIN(ConstantExprLowering, "ConstantExprLowering", "Converts ConstExpr into regular instructions",
                      false, false)
INITIALIZE_PASS_END(ConstantExprLowering, "ConstantExprLowering", "Converts ConstExpr into regular instructions",
                    false, false)
