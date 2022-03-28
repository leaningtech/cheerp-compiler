//===-- CheerpLowerInvoke.cpp - Cheerp backend pass ------------------------===//
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

static std::vector<Function*> getConstructors(Module& module)
{
	std::vector<Function*> ret;
	//Process constructors
	const ConstantArray* constructors = cheerp::ModuleGlobalConstructors(module);
	if (!constructors)
		return ret;

	ret.reserve(constructors->getNumOperands());
	for (auto& el: constructors->operands())
	{
		Constant* C = cast<Constant>(el);
		Function* func = cast<Function>(C->getAggregateElement(1));
		ret.push_back(func);
	}
	return ret;
}

PreservedAnalyses CallConstructorsPass::run(llvm::Module &M, llvm::ModuleAnalysisManager &MPA)
{
	FunctionType* Ty = FunctionType::get(Type::getVoidTy(M.getContext()), false);
	Function* Ctors = cast<Function>(M.getOrInsertFunction("__cheerp_constructors", Ty).getCallee());
	if (!Ctors->empty())
		return PreservedAnalyses::all();

	BasicBlock* Entry = BasicBlock::Create(M.getContext(),"entry", Ctors);
	IRBuilder<> Builder(Entry);

	for (Function* C: getConstructors(M))
	{
		Builder.CreateCall(Ty, C);
	}
	Builder.CreateRetVoid();

	PreservedAnalyses PA = PreservedAnalyses::none();
	PA.preserveSet(CFGAnalyses::ID());
	return PA;
}

}
