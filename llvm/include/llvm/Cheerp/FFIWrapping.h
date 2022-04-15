//===-- Cheerp/FFIWrapping.h ------------------------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2021-2022 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_FFI_WRAPPING_H
#define _CHEERP_FFI_WRAPPING_H

#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/IR/PassManager.h"

namespace cheerp {

// Converts 64-bit integer operations into 32-bit ones
class FFIWrapping
{
public:
	explicit FFIWrapping()
	{ }

	bool runOnModule(llvm::Module &M, cheerp::GlobalDepsAnalyzer& GDA);
};

class FFIWrappingPass : public llvm::PassInfoMixin<FFIWrappingPass> {
public:
	llvm::PreservedAnalyses run(llvm::Module& M, llvm::ModuleAnalysisManager& MAM);
	static bool isRequired() { return true;}
};
}

#endif //_CHEERP_FFI_WRAPPING_H

