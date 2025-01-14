//===-- ThreadLocalLowering.cpp - Cheerp helper -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2024-2025 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/ThreadLocalLowering.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/LinearMemoryHelper.h"
#include "llvm/Cheerp/InvokeWrapping.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/IRBuilder.h"

using namespace llvm;

namespace cheerp
{

static Function* getOrCreateThreadLocalWrapper(Module* M, GlobalDepsAnalyzer& GDA)
{
	Type* i8Ty = IntegerType::getInt8Ty(M->getContext());
	Type* i8PtrTy = PointerType::get(i8Ty, 0);
	Type* i32Ty = IntegerType::getInt32Ty(M->getContext());
	Type* argTy[] = {i32Ty};
	FunctionType* fTy = FunctionType::get(i8PtrTy,ArrayRef<Type*>(argTy, 1), false);
	Function* wrapper = cast<Function>(M->getOrInsertFunction("__getThreadLocalAddress", fTy).getCallee());
	if (!wrapper->empty())
		return wrapper;

	BasicBlock* entry = BasicBlock::Create(M->getContext(),"entry", wrapper);
	IRBuilder<> Builder(entry);
	// Get the thread local address.
	Function* threadPointerIntrinsic = Intrinsic::getDeclaration(M, Intrinsic::cheerp_get_thread_pointer);
	Value* threadPointer = Builder.CreateCall(threadPointerIntrinsic);
	// Add the offset argument
	Value* offset = wrapper->getArg(0);
	Value* address = Builder.CreateAdd(threadPointer, offset);
	// Bitcast to a pointer
	address = Builder.CreateIntToPtr(address, i8PtrTy);
	Builder.CreateRet(address);

	wrapper->setSection("asmjs");
	GDA.insertAsmJSExport(wrapper);
	return wrapper;
}

bool replaceThreadLocalIntrinsicWithFunction(Function& F, GlobalDepsAnalyzer& GDA)
{
	Module* M = F.getParent();
	bool changed = false;
	SmallVector<Instruction*, 8> deleteList;

	for (BasicBlock& BB: F)
	{
		for (Instruction& I: BB)
		{
			if (isa<IntrinsicInst>(I))
			{
				IntrinsicInst& II = cast<IntrinsicInst>(I);
				Intrinsic::ID intrId = II.getIntrinsicID();
				if (intrId == Intrinsic::threadlocal_address)
				{
					IRBuilder<> Builder(&II);
					// Replace call to intrinsic with function
					// 1. Use an intrinsic that will be the offset for the threadlocal.
					Type* argTy[] = {II.getOperand(0)->getType()};
					Function* offsetIntrinsic = Intrinsic::getDeclaration(M, Intrinsic::cheerp_get_threadlocal_offset, argTy);
					Value* offset = Builder.CreateCall(offsetIntrinsic, II.getOperand(0));
					// 2. Pass this offset to the wasm function that will calculate the address from the thread pointer.
					Function* newFunc = getOrCreateThreadLocalWrapper(M, GDA);
					Value* newCall = Builder.CreateCall(newFunc, offset);
					// 3. Bitcast return code from this function to required type.
					Type* origType = II.getType();
					if (origType != newCall->getType())
						newCall = Builder.CreateBitCast(newCall, origType);
					I.replaceAllUsesWith(newCall);
					deleteList.push_back(&I);
					changed = true;
				}
			}
		}
	}
	for (Instruction* I: deleteList)
		I->eraseFromParent();
	return changed;
}

PreservedAnalyses ThreadLocalLoweringInnerPass::run(Function& F, FunctionAnalysisManager& FAM)
{
	if (F.getSection() == "asmjs")
		return PreservedAnalyses::all();

	// Find calls to threadlocal.address intrinsic, replace with calls to function.
	bool changed = replaceThreadLocalIntrinsicWithFunction(F, GDA);
	if (!changed)
		return PreservedAnalyses::all();
	PreservedAnalyses PA;
	PA.preserve<PointerAnalysis>();
	PA.preserve<RegisterizeAnalysis>();
	PA.preserve<GlobalDepsAnalysis>();
	PA.preserve<DominatorTreeAnalysis>();
	PA.preserve<InvokeWrappingAnalysis>();
	return PA;
}

PreservedAnalyses ThreadLocalLoweringPass::run(Module& M, ModuleAnalysisManager& MAM)
{
	FunctionPassManager FPM;

	GlobalDepsAnalyzer& GDA = MAM.getResult<GlobalDepsAnalysis>(M);
	FPM.addPass(ThreadLocalLoweringInnerPass(GDA));

	ModulePassManager MPM;

	MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
	PreservedAnalyses PA = MPM.run(M, MAM);
	return PA;
}

}
