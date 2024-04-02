//===-- JSStringLiteralLowering.cpp - Cheerp helper -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Analysis/ValueTracking.h"
#include "llvm/Cheerp/JSStringLiteralLowering.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/Cheerp/Utility.h"

using namespace llvm;

namespace cheerp
{

PreservedAnalyses JSStringLiteralLoweringPass::run(Function& F, FunctionAnalysisManager& FAM)
{
	// Skip this pass for wasm functions, because we still need to call the
	// String constructor to get an anyref object.
	if (F.getSection() == "asmjs")
		return PreservedAnalyses::all();
	bool Changed = false;
	for(Instruction& I: instructions(F))
	{
		CallBase* CB = dyn_cast<CallBase>(&I);
		if (CB == nullptr)
			continue;
		Function* F = CB->getCalledFunction();
		if (F == nullptr)
			continue;
		if (F->getName() != "cheerpCreate_ZN6client6StringC2EPKc")
			continue;
		if (CB->arg_size() != 1)
			continue;
		Value* op = CB->getArgOperand(0);
		StringRef str;
		if(!llvm::getConstantStringInfo(op, str, /*TrimAtNul*/true))
			continue;
		// Replace the argument with null and attach a metadata
		op = llvm::ConstantPointerNull::get(cast<PointerType>(op->getType()));
		CB->setArgOperand(0, op);
		llvm::MDString* md = llvm::MDString::get(F->getContext(), str);
		assert(md);
		CB->setMetadata("jsliteral", llvm::MDNode::get(F->getContext(), { md }));
	}
	if (!Changed)
		return PreservedAnalyses::all(); 
	PreservedAnalyses PA;
	PA.preserveSet<CFGAnalyses>();
	return PA;
}

}
