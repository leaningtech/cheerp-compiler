//===-- CallConstructors.cpp - Cheerp backend pass ------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2022 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/CallConstructors.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/InitializePasses.h"
#include "llvm/Pass.h"

#define DEBUG_TYPE "callconstructors"

using namespace llvm;

namespace cheerp
{

PreservedAnalyses CallConstructorsPass::run(llvm::Module &M, llvm::ModuleAnalysisManager &MPA)
{
	FunctionType* Ty = FunctionType::get(Type::getVoidTy(M.getContext()), false);
	Function* Ctors = cast<Function>(M.getOrInsertFunction("_start", Ty).getCallee());
	if (!Ctors->empty())
		return PreservedAnalyses::all();

	BasicBlock* Entry = BasicBlock::Create(M.getContext(),"entry", Ctors);
	IRBuilder<> Builder(Entry);

	for (Constant* C: cheerp::getGlobalConstructors(M))
	{
		Builder.CreateCall(Ty, cast<Function>(C->getAggregateElement(1)));
	}
	Function* entry = M.getFunction("webMain");
	if (!entry)
		entry = M.getFunction("main");
	if (entry)
	{
		SmallVector<Value*, 4> Args;
		for (auto& a: entry->args())
		{
			auto* ArgTy = a.getType();
			if (auto* PTy = dyn_cast<PointerType>(ArgTy))
				Args.push_back(ConstantPointerNull::get(PTy));
			else if (auto* ITy = dyn_cast<IntegerType>(ArgTy))
				Args.push_back(ConstantInt::get(ITy, 0));
			else
				llvm::report_fatal_error("main function has a strange signature");
		}
		Builder.CreateCall(entry->getFunctionType(), entry, Args);
	}

	Builder.CreateRetVoid();

	PreservedAnalyses PA = PreservedAnalyses::none();
	PA.preserveSet(CFGAnalyses::ID());
	return PA;
}

}
