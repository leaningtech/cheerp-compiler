//===-- ConstantExprLowering.cpp - Cheerp helper -------------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2020-2022 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/InitializePasses.h"
#include "llvm/Cheerp/ConstantExprLowering.h"
#include "llvm/Cheerp/I64Lowering.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/InvokeWrapping.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Analysis/ConstantFolding.h"

using namespace llvm;

namespace cheerp
{

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
	if(isa<LandingPadInst>(I))
		return false;
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
				// Leave alone casts of globals. We don't gain anything
				// and it is harder to track globals later (e.g. to extract type info)
				if (isa<GlobalVariable>(CE->stripPointerCastsSafe()))
				{
					continue;
				}
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
bool ConstantExprLowering::runOnFunction(Function& F, bool& hasI64)
{
	bool Changed = false;
	hasI64 = false;

	DL = &F.getParent()->getDataLayout();

	for (BasicBlock& BB: F)
	{
		for (Instruction& I: BB)
		{
			Changed = runOnInstruction(&I, hasI64);
		}
	}
	return Changed;
}

llvm::PreservedAnalyses ConstantExprLoweringPass::run(llvm::Module& M, llvm::ModuleAnalysisManager& MAM)
{
const LinearMemoryHelper& AR = MAM.getResult<LinearMemoryAnalysis>(M);
FunctionAnalysisManager& FAM = MAM.getResult<FunctionAnalysisManagerModuleProxy>(M).getManager();
	ConstantExprLowering inner(&AR);

		FunctionPassManager FPM;
		FPM.addPass(I64LoweringPass());
bool moduleChanged = false;

		for (Function& F : M)
		{
			if (F.isDeclaration())
				continue;

			bool hasI64 = false;
			bool Changed = inner.runOnFunction(F, hasI64);
			if (Changed)
			{
				FAM.invalidate(F, PreservedAnalyses::none());
				moduleChanged = true;
			}
			if (hasI64)
			{
				PreservedAnalyses PA = FPM.run(F, FAM);
				if (!PA.areAllPreserved())
					moduleChanged = true;
				FAM.invalidate(F, PA);
			}
		}

		if (!moduleChanged)
			return PreservedAnalyses::all();

	PreservedAnalyses PA;
	PA.preserve<LinearMemoryAnalysis>();
	PA.preserve<RegisterizeAnalysis>();
	PA.preserve<GlobalDepsAnalysis>();
	PA.preserve<RegisterizeAnalysis>();
	PA.preserve<InvokeWrappingAnalysis>();
	PA.preserveSet<CFGAnalyses>();
	return PA;
}

}
