//===-- Cheerp/AllocaMerging.h - Cheerp alloca elision code ---------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014-2018 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_ALLOCA_MERGING_H
#define _CHEERP_ALLOCA_MERGING_H

#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Pass.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include <list>

namespace cheerp {

class AllocaMergingBase: public llvm::FunctionPass
{
protected:
	AllocaMergingBase(char& ID):FunctionPass(ID)
	{
	}
	typedef std::pair<llvm::AllocaInst*, Registerize::LiveRange> AllocaInfo;
	template<typename Container>
	void analyzeBlock(const Registerize& registerize, llvm::BasicBlock& BB,
				Container& allocaInfos);
};

// This class is resposible for recycling allocas. We can use lifetime intrinsics to know
// about the lifetime of an alloca
class AllocaMerging: public AllocaMergingBase
{
private:
	static bool areTypesEquivalent(const TypeSupport& types, PointerAnalyzer& PA, llvm::Type* a, llvm::Type* b, bool asmjs);
public:
	static char ID;
	explicit AllocaMerging() : AllocaMergingBase(ID) { }
	bool runOnFunctionLegacy(llvm::Function &F);
	bool runOnFunction(llvm::Function &F);
	llvm::StringRef getPassName() const;
	void getAnalysisUsage(llvm::AnalysisUsage & AU) const;
};

class AllocaArraysMerging: public AllocaMergingBase
{
private:
	bool checkUsesForArrayMerging(llvm::AllocaInst* alloca) const;
	llvm::Type* collectUniformAlloca(std::vector<llvm::AllocaInst*>& uniformAllocaArrays, std::list<AllocaInfo>& allocaInfos) const;
public:
	static char ID;
	explicit AllocaArraysMerging() : AllocaMergingBase(ID) { }
	bool runOnFunction(llvm::Function &F);
	llvm::StringRef getPassName() const;
	void getAnalysisUsage(llvm::AnalysisUsage & AU) const;
};

//===----------------------------------------------------------------------===//
//
// AllocaMerging - This pass merges allocas which are not used at the same time
//
llvm::FunctionPass *createAllocaMergingPass();
llvm::FunctionPass *createAllocaArraysMergingPass();

// NOTE: This is a ModulePass only to make LLVM happy, it actually only work at the block level
class AllocaStoresExtractor: public llvm::ModulePass
{
public:
	typedef std::unordered_map<uint32_t, llvm::Value*> OffsetToValueMap;
private:
	const llvm::DataLayout* DL;
	std::unordered_map<const llvm::AllocaInst*, OffsetToValueMap> allocaStores;
	std::vector<llvm::Instruction*> instsToRemove;
	bool runOnBasicBlock(llvm::BasicBlock &BB, const llvm::Module& module);
	static bool validType(llvm::Type* t, const llvm::Module& module);
public:
	static char ID;
	explicit AllocaStoresExtractor() : llvm::ModulePass(ID), DL(nullptr) { }
	bool runOnModule(llvm::Module& M);
	llvm::StringRef getPassName() const;
	void getAnalysisUsage(llvm::AnalysisUsage & AU) const;
	const OffsetToValueMap* getValuesForAlloca(const llvm::AllocaInst* AI) const;
	// Removes the extracted stores, and clean up instructions which become dead afterwards
	void destroyStores();
};

//===----------------------------------------------------------------------===//
//
// AllocaStoresExtractor - This pass removes stores to just allocated memory and keeps track of the values separately
//
llvm::ModulePass* createAllocaStoresExtractor();
}

#endif //_CHEERP_ALLOCA_MERGING_H

