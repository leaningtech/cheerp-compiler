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
	Triple triple = Triple(M.getTargetTriple());
	bool isWasmStandalone = triple.isCheerpWasmStandalone();
	bool isCheerpOS = triple.isCheerpOS();
	bool useUtilityThread = !LowerAtomics && !isWasmStandalone && !isCheerpOS;
	FunctionType* Ty = FunctionType::get(Type::getVoidTy(M.getContext()), false);
	Function* StartFunction = cast<Function>(M.getOrInsertFunction("_start", Ty).getCallee());
	if (!StartFunction->empty())
		return PreservedAnalyses::all();

	BasicBlock* StartEntry = BasicBlock::Create(M.getContext(),"entry", StartFunction);
	IRBuilder<> Builder(StartEntry);

	Function* StartPreThread = nullptr;
	if (useUtilityThread)
	{
		StartPreThread = cast<Function>(M.getOrInsertFunction("_startPreThread", Ty).getCallee());
		BasicBlock* StartPreThreadEntry = BasicBlock::Create(M.getContext(),"entry", StartPreThread);
		Builder.SetInsertPoint(StartPreThreadEntry);
	}

	if (LinearOutput == LinearOutputTy::Wasm)
	{
		// Here we declare the memory_init function, which will be called by _start.
		Function* memoryInit = cast<Function>(M.getOrInsertFunction("__memory_init", Ty).getCallee());
		memoryInit->setSection("asmjs");
		Builder.CreateCall(Ty, memoryInit);
	}
	// Add a call to initialise the tls to _start.
	Function* cheerpInitTls = cast<Function>(M.getOrInsertFunction("__cheerp_init_tls", Ty).getCallee());
	Builder.CreateCall(Ty, cheerpInitTls);

	Function* GetEnviron = M.getFunction("__syscall_main_environ");
	if (GetEnviron)
		Builder.CreateCall(GetEnviron->getFunctionType(), GetEnviron);

	for (Constant* C: cheerp::getGlobalConstructors(M))
		Builder.CreateCall(Ty, cast<Function>(C->getAggregateElement(1)->stripPointerCastsSafe()));
	removeGlobalConstructorsGlobal(M);

	if (useUtilityThread)
	{
		// If -pthread is passed, add a call to spawnUtility to setup the utility thread.
		Function* spawnUtility = cast<Function>(M.getOrInsertFunction("spawnUtility", Ty).getCallee());
		Builder.CreateCall(Ty, spawnUtility);
		Builder.CreateRetVoid();
		Builder.SetInsertPoint(StartEntry);
	}

	Function* Main = getMainFunction(M);
	if (isWasmStandalone || (Main && Main->getSection() == "asmjs"))
	{
		StartFunction->setSection("asmjs");
		if (useUtilityThread)
			StartPreThread->setSection("asmjs");
	}
	if (Main)
	{
		Value* ExitCode = nullptr;
		if (Main->arg_size())
		{
			if (Main->arg_size() != 2 && Main->arg_size() != 3)
				llvm::report_fatal_error("main function has a strange signature");
			Type* ArgcTy = Main->getArg(0)->getType();
			Type* ArgvTy = Main->getArg(1)->getType();
			Value* Argc = nullptr;
			Value* Argv = nullptr;
			Function* GetArgs = M.getFunction("__syscall_main_args");
			if (GetArgs && GetArgs->getSection() == Main->getSection())
			{
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

			if (Main->arg_size() == 3)
			{
				Type* EnvTy = Main->getArg(2)->getType();
				Value* Env = nullptr;

				if (GetEnviron->getSection() == Main->getSection())
				{
					Value* Environ = M.getNamedValue("environ");
					assert(Environ && "environ not present");
					Env = Builder.CreateLoad(EnvTy, Environ);
				}
				else
				{
					Env = Builder.CreateAlloca(Builder.getInt8Ty()->getPointerTo(0));
					Builder.CreateStore(ConstantPointerNull::get(Builder.getInt8Ty()->getPointerTo(0)), Env);
				}


				ExitCode = Builder.CreateCall(Main->getFunctionType(), Main, { Argc, Argv, Env });
			}
			else
			{
				ExitCode = Builder.CreateCall(Main->getFunctionType(), Main, { Argc, Argv });
			}
		}
		else
		{
			ExitCode = Builder.CreateCall(Main->getFunctionType(), Main);
		}
		if (!LowerAtomics || isWasmStandalone)
		{
			// In standalone mode, or if -pthread has been passed, we call exit after main, which will run global destructors
			Function* Exit = M.getFunction("exit");
			assert(Exit != nullptr);
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
