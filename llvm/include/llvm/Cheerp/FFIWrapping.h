//===-- Cheerp/FFIWrapping.h ------------------------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2021 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_FFI_WRAPPING_H
#define _CHEERP_FFI_WRAPPING_H

#include "llvm/Pass.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"

namespace cheerp {

// Converts 64-bit integer operations into 32-bit ones
class FFIWrapping: public llvm::ModulePass
{
public:
	static char ID;

	explicit FFIWrapping()
		: ModulePass(ID)
	{ }

	virtual bool runOnModule(llvm::Module &M) override;
	virtual void getAnalysisUsage(llvm::AnalysisUsage & AU) const override;
	virtual llvm::StringRef getPassName() const override
	{
		return "FFIWrapping";
	}
};

llvm::ModulePass *createFFIWrappingPass();
}

#endif //_CHEERP_FFI_WRAPPING_H

