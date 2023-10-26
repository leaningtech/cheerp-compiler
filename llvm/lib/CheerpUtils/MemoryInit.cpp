//===-- MemoryInit.cpp - Populate the __memory_init function with init intrinsics------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2022-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/MemoryInit.h"
#include "llvm/Cheerp/LinearMemoryHelper.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Intrinsics.h"

using namespace llvm;

namespace cheerp
{

PreservedAnalyses MemoryInitPass::run(Module& M, ModuleAnalysisManager& MAM)
{
	cheerp::LinearMemoryHelper& linearHelper = MAM.getResult<LinearMemoryAnalysis>(M);

	Function* initFunc = M.getFunction("__memory_init");
	if (!initFunc)
	{
		// We print a warning and return. This is to satisfy some llvm tests.
		llvm::errs() << "warning: __memory_init function point not found\n";
		return PreservedAnalyses::all();
	}
	if (!initFunc->empty())
		llvm::report_fatal_error("__memory_init function already has a body.");

	BasicBlock* Entry = BasicBlock::Create(M.getContext(), "entry", initFunc);
	IRBuilder<> Builder(Entry);

	// Loop over the chunks and populate the function.
	Function* memoryInit = Intrinsic::getDeclaration(&M, Intrinsic::cheerp_memory_init);
	Function* dataDrop = Intrinsic::getDeclaration(&M, Intrinsic::cheerp_data_drop);

	uint32_t amountOfChunks = linearHelper.getAmountChunks();
	for (uint32_t i = 0; i < amountOfChunks; i++)
	{
		auto chunk = linearHelper.getGlobalDataChunk(i);
		Builder.CreateCall(memoryInit, {Builder.getInt32(i), Builder.getInt32(chunk.address), Builder.getInt32(0), Builder.getInt32(chunk.view.size())});
		Builder.CreateCall(dataDrop, {Builder.getInt32(i)});
	}

	Builder.CreateRetVoid();
	return PreservedAnalyses::all();
}

}
