//===-- CallConstructors.cpp - Cheerp backend pass ------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2022-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/CallConstructors.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/ADT/Triple.h"
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
	Function* Main = getMainFunction(M);
	bool Wasi = Triple(M.getTargetTriple()).getOS() == Triple::WASI;
	if (Wasi || (Main && Main->getSection() == "asmjs"))
		Ctors->setSection("asmjs");
	if (Main)
	{
		Value* ExitCode = nullptr;
		if (Main->arg_size())
		{
			if (Main->arg_size() != 2)
				llvm::report_fatal_error("main function has a strange signature");
			Type* ArgcTy = Main->getArg(0)->getType();
			Type* ArgvTy = Main->getArg(1)->getType();
			Value* Argc = nullptr;
			Value* Argv = nullptr;
			Function* GetArgs = M.getFunction("__syscall_main_args");
			if (GetArgs)
			{
				if (GetArgs->getSection() != Main->getSection())
					llvm::report_fatal_error("__syscall_main_args must have the same section as main");

				Value* ArgcA = Builder.CreateAlloca(ArgcTy);
				Value* ArgvA = Builder.CreateAlloca(ArgvTy);
				ArrayRef<Value*> ArgsA = { ArgcA, ArgvA };
				Builder.CreateCall(GetArgs->getFunctionType(), GetArgs, { ArgcA, ArgvA});
				Argc = Builder.CreateLoad(ArgcTy, ArgcA);
				Argv = Builder.CreateLoad(ArgvTy, ArgvA);
			}
			else
			{
				Argc = ConstantInt::get(ArgcTy, 0);
				Argv = ConstantPointerNull::get(cast<PointerType>(ArgvTy));
			}
			ExitCode = Builder.CreateCall(Main->getFunctionType(), Main, { Argc, Argv });
		}
		else
		{
			ExitCode = Builder.CreateCall(Main->getFunctionType(), Main);
		}
		if (Wasi)
		{
			Function* Exit = M.getFunction("__syscall_exit");
			if (ExitCode->getType() != Builder.getInt32Ty())
				ExitCode = ConstantInt::get(Builder.getInt32Ty(), 0);
			Builder.CreateCall(Exit->getFunctionType(), Exit, ExitCode);
		}
	}

	Builder.CreateRetVoid();

	PreservedAnalyses PA = PreservedAnalyses::none();
	PA.preserveSet(CFGAnalyses::ID());
	return PA;
}

}
