//===-- ConstantExprLowering.cpp - Cheerp helper -------------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2020-2023 Leaning Technologies
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
#include "llvm/IR/InstIterator.h"
#include "llvm/Analysis/ConstantFolding.h"

using namespace llvm;

namespace cheerp
{

bool ConstantExprLowering::runOnFunction(Function& F, bool& hasI64, const TargetLibraryInfo& TLI)
{
	bool Changed = false;
	hasI64 = false;

	DL = &F.getParent()->getDataLayout();

	std::set<ConstantExpr*> visitedCE;
	std::deque<ConstantExpr*> toBeInstructionized;
	auto recursivelyVisitOperands = [&visitedCE, &toBeInstructionized](ConstantExpr* CE)
	{
		std::deque<ConstantExpr*> workList;

		if (visitedCE.insert(CE).second)
			workList.push_back(CE);

		for (uint32_t i=0; i<workList.size(); i++)
		{
			ConstantExpr* CE = workList[i];
			for (const Use &NewU : CE->operands())
			{
				// Recursively visit the ConstantExpr's operands. If we have already visited
				// a ConstantExpr, we don't have to process it again.
				if (auto *NewCE = dyn_cast<ConstantExpr>(&NewU))
				{
					if (visitedCE.insert(NewCE).second)
						workList.push_back(NewCE);
				}
				// Do the same for the vectors of ConstantExpr
				else if (ConstantVector* CV = dyn_cast<ConstantVector>(&NewU))
				{
					const unsigned num = CV->getType()->getNumElements();
					for (unsigned i = 0; i < num; i++)
					{
						Constant* element = CV->getAggregateElement(i);
						if (ConstantExpr* CVE = dyn_cast<ConstantExpr>(element))
						{
							if (visitedCE.insert(CVE).second)
								workList.push_back(CVE);
						}
					}
				}
			}
		}
		std::reverse(workList.begin(), workList.end());

		for (ConstantExpr* CE : workList)
			toBeInstructionized.push_back(CE);
	};
	Instruction* insertionPoint = F.getEntryBlock().getFirstNonPHI();

	// 1. Iterate on instructions, collecting referenced ConstantExpr
	for (Instruction& I : instructions(F))
	{
		for (auto& O: I.operands())
		{
			if (ConstantExpr* CE = dyn_cast<ConstantExpr>(O.get()))
				recursivelyVisitOperands(CE);
			// If this is a vector of ConstantExpr, loop over it and add the ConstExpr's
			else if (ConstantVector* CV = dyn_cast<ConstantVector>(O.get()))
			{
				const unsigned num = CV->getType()->getNumElements();
				for (unsigned i = 0; i < num; i++)
				{
					Constant* element = CV->getAggregateElement(i);
					if (ConstantExpr* CE = dyn_cast<ConstantExpr>(element))
						recursivelyVisitOperands(CE);
				}
			}
		}
	}

	std::reverse(toBeInstructionized.begin(), toBeInstructionized.end());

	std::map<ConstantExpr*, Value*> mapCEToValue;
	std::map<GlobalValue*, Instruction*> mapGVToInst;

	// 2. Iterate on the collected ConstantExpr, converting them to Instructions
	// 	Note that given the specific order of the visit, operands will be processed later (= will end up dominating their users)
	for (ConstantExpr* CE : toBeInstructionized)
	{
		Instruction* Conv = CE->getAsInstruction();
		for (auto& O: Conv->operands())
		{
			Value* NewC = O.get();
			if (auto *GV = dyn_cast<GlobalVariable>(NewC))
			{
				// In asmjs, addresses of globals  are just integers
				// Ask LinearMemoryHelper for the value and cast to the pointer type
				if (GV->GlobalValue::getSection() == StringRef("asmjs"))
				{
					if (mapGVToInst.count(GV) == 0)
					{
						auto *CI = ConstantInt::get(IntegerType::get(Conv->getContext(), 32), LH->getGlobalVariableAddress(GV));
						mapGVToInst[GV] = CastInst::Create(Instruction::IntToPtr, CI, NewC->getType());
					}

					NewC = mapGVToInst.at(GV);
				}
			}
			else if (isa<UndefValue>(NewC) && NewC->getType()->isFloatingPointTy())
			{
				// For some reason, undef floating points are not folded
				// So we replace them with NaNs
				NewC = ConstantFP::getNaN(NewC->getType());
			}
			O.set(NewC);
		}
		Conv->insertBefore(F.getEntryBlock().getFirstNonPHI());
		Changed = true;
		mapCEToValue[CE] = Conv;
	}

	// 3. Insert also PtrToIntInst that has been created while folding GV
	for (auto& pair : mapGVToInst)
	{
		pair.second->insertBefore(F.getEntryBlock().getFirstNonPHI());
	}

	// 4. Actually substitute any ConstantExpr operand with the mapped Instruction (that will be in the Function entry BB)
	std::map<ConstantVector*, Value*> mapCVToValue;
	for (Instruction& I : instructions(F))
	{
		if (isa<LandingPadInst>(I))
			continue;
		if (I.getType()->isIntegerTy(64))
			hasI64 |= true;
		for (auto& O: I.operands())
		{
			if (O.get()->getType()->isIntegerTy(64))
				hasI64 |= true;

			if (ConstantExpr* CE = dyn_cast<ConstantExpr>(O.get()))
			{
				assert(mapCEToValue.count(CE));
				O.set(mapCEToValue.at(CE));
				Changed = true;
			}
			if (ConstantVector* CV = dyn_cast<ConstantVector>(O.get()))
			{
				auto it = mapCVToValue.find(CV);
				if (it != mapCVToValue.end())
				{
					O.set(it->second);
					Changed = true;
					continue;
				}
				// First, collect the elements in a vector and keep track of whether we find a ConstantExpr in there.
				SmallVector<Value *, 16> values;
				bool vectorHasConstantExpr = false;
				Value* currentElement;
				const unsigned num = CV->getType()->getNumElements();
				for (unsigned i = 0; i < num; i++)
				{
					currentElement = CV->getAggregateElement(i);
					if (ConstantExpr* CEelement = dyn_cast<ConstantExpr>(currentElement))
					{
						// If the element is a ConstantExpr, replace it with the mapped value.
						values.push_back(mapCEToValue.at(CEelement));
						vectorHasConstantExpr = true;
					}
					else
						values.push_back(currentElement);
				}
				if (!vectorHasConstantExpr)
					continue;

				IRBuilder<> Builder(insertionPoint);
				Value* newVector;
				if (Value* splatElement = CV->getSplatValue())
				{
					// If this is a splat vector, create with a splat.
					std::vector<Type *> argTypes = {CV->getType(), splatElement->getType()};
					ConstantExpr* CEelement = dyn_cast<ConstantExpr>(splatElement);
					Function *splatIntrinsic = Intrinsic::getDeclaration(I.getModule(), Intrinsic::cheerp_wasm_splat, argTypes);
					newVector = Builder.CreateCall(splatIntrinsic, {mapCEToValue.at(CEelement)});
				}
				else
				{
					// Otherwise, create the vector by inserting elements into an empty vector.
					newVector = UndefValue::get(CV->getType());
					for (unsigned i = 0; i < num; i++)
						newVector = Builder.CreateInsertElement(newVector, values[i], i);
				}
				mapCVToValue[CV] = newVector;
				O.set(newVector);
				Changed = true;
			}
		}
	}

	std::vector<Instruction*> deleteList;

	// 5. Optimization: Fold Instruction into Constants (but NOT ConstantExpr)
	for (Instruction& I: instructions(F))
	{
		if (Constant *C = ConstantFoldInstruction(&I, F.getParent()->getDataLayout(), &TLI))
		{
			if (isa<ConstantExpr>(C))
			{
				//Avoid adding CE back
				continue;
			}
			I.replaceAllUsesWith(C);
			deleteList.push_back(&I);
		}
	}

	for (Instruction* D : deleteList)
		D->eraseFromParent();

	// 6. Check for I64 instructions
	for (Instruction& I : instructions(F))
	{
		if (I.getType()->isIntegerTy(64))
			hasI64 |= true;
		for (auto& O: I.operands())
		{
			if (O.get()->getType()->isIntegerTy(64))
				hasI64 |= true;
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
		const llvm::TargetLibraryInfo& TLI = FAM.getResult<TargetLibraryAnalysis>(F);
		bool Changed = inner.runOnFunction(F, hasI64, TLI);
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
	PA.preserve<PointerAnalysis>();
	return PA;
}

}
