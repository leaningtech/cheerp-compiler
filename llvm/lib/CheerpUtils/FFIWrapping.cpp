//===-- FFIWrapping.cpp - Cheerp utility function --------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2020 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"

namespace cheerp {

using namespace llvm;

static Function* wrapImport(Module& M, const Function* Orig)
{
	FunctionType* Ty = Orig->getFunctionType();
	Function* Wrapper = cast<Function>(M.getOrInsertFunction(Twine("__wrapper__",Orig->getName()).str(), Ty));
	BasicBlock* Entry = BasicBlock::Create(M.getContext(),"entry", Wrapper);
	IRBuilder<> Builder(Entry);

	llvm::SmallVector<Value*, 4> params;
	for(auto& arg: Wrapper->args())
		params.push_back(&arg);
	CallInst* ForwardCall = Builder.CreateCall(const_cast<Function*>(Orig), params);
	Value* Ret = ForwardCall->getType()->isVoidTy() ? nullptr : ForwardCall;
	Builder.CreateRet(Ret);

	Wrapper->setSection("asmjs");


	// Replace all uses inside the 
	return Wrapper;
}

static bool needsWrapping(const Function* F)
{
	for (const auto& arg: F->args())
	{
		if (arg.getType()->isPointerTy())
			return true;
	}

	return false;
}

static void replaceAllUsesWithFiltered(Value* Old, Value* New,
		const DeterministicFunctionSet& whitelist)
{
	auto UI = Old->use_begin(), E = Old->use_end();
	for (; UI != E;)
	{
		Use &U = *UI;
		++UI;
		auto *Usr = dyn_cast<Instruction>(U.getUser());
		if (!Usr)
			continue;
		if (!whitelist.count(Usr->getParent()->getParent()))
			continue;
		U.set(New);
	}
}

void FFIWrapping::run()
{
	std::unordered_set<const Function*> newImports;
	for (auto* F: imports)
	{
		if (needsWrapping(F))
		{
			Function* W = wrapImport(M, F);
			newImports.insert(W);
			outsideModule.insert(W);
			replaceAllUsesWithFiltered(const_cast<Function*>(F), W, insideModule);
		}
		else
		{
			newImports.insert(F);
		}
	}
	imports = std::move(newImports);
}

}
