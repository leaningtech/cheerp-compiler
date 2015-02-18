//===-- Cheerp/AllocaMerging.h - Cheerp alloca elision code ---------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_ALLOCA_MERGING_H
#define _CHEERP_ALLOCA_MERGING_H

#include "llvm/Cheerp/Registerize.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Pass.h"
#include <list>

namespace llvm {

class AllocaMergingBase: public FunctionPass
{
protected:
	AllocaMergingBase(char& ID):FunctionPass(ID)
	{
	}
	typedef std::list<std::pair<AllocaInst*, cheerp::Registerize::LiveRange>> AllocaInfos;
	void analyzeBlock(const cheerp::Registerize& registerize, llvm::BasicBlock& BB,
				AllocaInfos& allocaInfos);
};

// This class is resposible for recycling allocas. We can use lifetime intrinsics to know
// about the lifetime of an alloca
class AllocaMerging: public AllocaMergingBase
{
private:
	static bool areTypesEquivalent(Type* a, Type* b);
public:
	static char ID;
	explicit AllocaMerging() : AllocaMergingBase(ID) { }
	bool runOnFunction(Function &F);
	const char *getPassName() const;
	void getAnalysisUsage(AnalysisUsage & AU) const;
};

class AllocaArraysMerging: public AllocaMergingBase
{
private:
	bool checkUsesForArrayMerging(AllocaInst* alloca);
public:
	static char ID;
	explicit AllocaArraysMerging() : AllocaMergingBase(ID) { }
	bool runOnFunction(Function &F);
	const char *getPassName() const;
	void getAnalysisUsage(AnalysisUsage & AU) const;
};

//===----------------------------------------------------------------------===//
//
// AllocaMerging - This pass merges allocas which are not used at the same time
//
FunctionPass *createAllocaMergingPass();
FunctionPass *createAllocaArraysMergingPass();
}

#endif //_CHEERP_ALLOCA_MERGING_H

