//===-- FixFunctionCasts.cpp - Cheerp optimization pass --------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2019 Leaning Technologies
//
//===----------------------------------------------------------------------===//
/// Detect function pointer casts and create a forwarder function with the casted
/// type signature that calls the original one, if possible. In particular in the
/// common case of calling a function with more arguments (which is fine in X86,
/// but not in wasm).

#include "llvm/InitializePasses.h"
#include "llvm/Cheerp/CFGPasses.h"
#include "llvm/IR/Constants.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Cheerp/LinearMemoryHelper.h"

using namespace llvm;

namespace {

class FixFunctionCasts: public ModulePass {
public:
	FixFunctionCasts(): ModulePass(ID) {}
	StringRef getPassName() const override {
		return "FixFunctionCasts";
	}
	bool runOnModule(Module &M) override;
	static char ID;
};

}

static Function* addCastWrapper(Module& M, Function& F, FunctionType* NewTy)
{
	auto TableName = cheerp::LinearMemoryHelper::getFunctionTableName(NewTy);
	SmallVector<char, 16> NewNameBuf;
	StringRef NewName = Twine(F.getName(), TableName.c_str()).toStringRef(NewNameBuf);
	if (Function* NewF = M.getFunction(NewName))
		return NewF;
	Function* NewF = Function::Create(NewTy, Function::InternalLinkage, NewName, &M);
	NewF->setSection(F.getSection());
	NewF->copyAttributesFrom(&F);
	// Fill the new function
	BasicBlock * Body = BasicBlock::Create( NewF->getContext(), "entry", NewF);

	auto ArgIt = NewF->arg_begin();
	SmallVector<Value*, 4> Args;
	for (uint32_t i = 0; i < F.arg_size(); ++i)
	{
		Args.push_back(&*ArgIt++);
	}
	CallInst * Call = CallInst::Create(&F, Args, "", Body);

	if (NewTy->isVoidTy())
		ReturnInst::Create(NewF->getContext(), Body);
	else
		ReturnInst::Create(NewF->getContext(), Call, Body);
	return NewF;
}

bool FixFunctionCasts::runOnModule(Module& M)
{
	bool Changed = false;

	for (auto& F: M.functions())
	{
		if (F.getSection() != StringRef("asmjs"))
			continue;
		if (!F.hasAddressTaken())
			continue;
		for (User* U: F.users())
		{
			if (auto BC = dyn_cast<ConstantExpr>(U))
			{
				if (!BC->isCast())
					continue;
				if (!BC->getType()->isPointerTy() || !BC->getType()->getPointerElementType()->isFunctionTy())
					continue;
				FunctionType* Dst = dyn_cast<FunctionType>(BC->getType()->getPointerElementType());
				FunctionType* Src = dyn_cast<FunctionType>(F.getType()->getPointerElementType());
				if (!Dst || !Src)
					continue;
				if (Dst->getNumParams() <= Src->getNumParams())
					continue;
				auto SrcIt = Src->param_begin();
				auto DstIt = Dst->param_begin();
				auto cmp = cheerp::LinearMemoryHelper::FunctionSignatureCmp<>();
				for (; SrcIt != Src->param_end(); ++SrcIt, ++DstIt)
				{
				}
				SmallVector<Type*, 4> Params{Src->param_begin(), Src->param_end()};
				Params.append(DstIt, Dst->param_end());
				FunctionType* New = FunctionType::get(Src->getReturnType(), Params, Src->isVarArg());
				if (!cmp(Dst, New))
					continue;
				Function* Wrapper = addCastWrapper(M, F, New);
				Constant* C = ConstantExpr::getBitCast(Wrapper, BC->getType());
				U->replaceAllUsesWith(C);
			}
		}
	}
	return Changed;
}

char FixFunctionCasts::ID = 0;

namespace llvm {

ModulePass* createFixFunctionCastsPass()
{
	return new FixFunctionCasts();
}

}

INITIALIZE_PASS_BEGIN(FixFunctionCasts, "FixFunctionCasts", "Create wrapper functions for bitcast to function types with more arguments",
                      false, false)
INITIALIZE_PASS_END(FixFunctionCasts, "FixFunctionCasts", "Create wrapper functions for bitcast to function types with more arguments",
                    false, false)
