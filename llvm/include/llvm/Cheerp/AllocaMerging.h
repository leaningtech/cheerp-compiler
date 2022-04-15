//===-- Cheerp/AllocaMerging.h - Cheerp alloca elision code ---------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014-2022 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_ALLOCA_MERGING_H
#define _CHEERP_ALLOCA_MERGING_H

#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include <list>

namespace cheerp {

class GlobalDepsAnalyzer;

class AllocaMergingBase
{
protected:
	AllocaMergingBase()
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
	explicit AllocaMerging() : AllocaMergingBase() { }
	bool runOnFunctionLegacy(llvm::Function& F, cheerp::PointerAnalyzer& PA, llvm::DominatorTree* DT, cheerp::Registerize& registerize);
	bool runOnFunction(llvm::Function& F, cheerp::PointerAnalyzer& PA, llvm::DominatorTree* DT, cheerp::Registerize& registerize);
};

class AllocaArraysMerging: public AllocaMergingBase
{
private:
	bool checkUsesForArrayMerging(llvm::AllocaInst* alloca) const;
	llvm::Type* collectUniformAlloca(std::vector<llvm::AllocaInst*>& uniformAllocaArrays, std::list<AllocaInfo>& allocaInfos) const;
public:
	explicit AllocaArraysMerging() : AllocaMergingBase() { }
	bool runOnFunction(llvm::Function& F, cheerp::PointerAnalyzer& PA, llvm::DominatorTree* DT, cheerp::Registerize& registerize, cheerp::GlobalDepsAnalyzer & GDA);
};

//===----------------------------------------------------------------------===//
//
// AllocaMerging - This pass merges allocas which are not used at the same time
//
class AllocaMergingPass : public llvm::PassInfoMixin<AllocaMergingPass> {
public:
	llvm::PreservedAnalyses run(llvm::Module& M, llvm::ModuleAnalysisManager& MAM);
	static bool isRequired() { return true;}
};

class AllocaArraysMergingPass : public llvm::PassInfoMixin<AllocaArraysMergingPass> {
public:
	llvm::PreservedAnalyses run(llvm::Module& M, llvm::ModuleAnalysisManager& MAM);
	static bool isRequired() { return true;}
};

class AllocaStoresExtractorPass;

// NOTE: This is a ModulePass only to make LLVM happy, it actually only work at the block level
class AllocaStoresExtractor
{
public:
	typedef std::unordered_map<uint32_t, llvm::Value*> OffsetToValueMap;
private:
	const llvm::TargetLibraryInfo* TLI;
	llvm::ModuleAnalysisManager* MAM;
	const llvm::DataLayout* DL;
	std::unordered_map<const llvm::AllocaInst*, OffsetToValueMap> allocaStores;
	std::vector<llvm::Instruction*> instsToRemove;
	bool runOnBasicBlock(llvm::BasicBlock &BB, const llvm::Module& module);
	static bool validType(llvm::Type* t, const llvm::Module& module);
public:
	explicit AllocaStoresExtractor() : DL(nullptr)
	{}
	bool runOnModule(llvm::Module& M);
	const OffsetToValueMap* getValuesForAlloca(const llvm::AllocaInst* AI) const;
	// Removes the extracted stores, and clean up instructions which become dead afterwards
	void unlinkStores();
	void destroyStores();
	friend AllocaStoresExtractorPass;
};

class AllocaStoresExtractorWrapper
{
public:
	operator AllocaStoresExtractor&()
	{
		static AllocaStoresExtractor* innerPtr;
		if (innerPtr)
			delete innerPtr;
		innerPtr = new AllocaStoresExtractor();
		return *innerPtr;
	}
};

//===----------------------------------------------------------------------===//
//
// AllocaStoresExtractor - This pass removes stores to just allocated memory and keeps track of the values separately
//
//
class AllocaStoresExtractorPass : public llvm::PassInfoMixin<AllocaStoresExtractorPass> {
public:
	llvm::PreservedAnalyses run(llvm::Module& M, llvm::ModuleAnalysisManager& MAM);
	static bool isRequired() { return true;}
};

class AllocaStoresExtractorAnalysis : public llvm::AnalysisInfoMixin<AllocaStoresExtractorAnalysis> {
	friend llvm::AnalysisInfoMixin<AllocaStoresExtractorAnalysis>;
	static llvm::AnalysisKey Key;
public:
	using Result = AllocaStoresExtractorWrapper;
	static Result run(llvm::Module& M, llvm::ModuleAnalysisManager&);
};

}

#endif //_CHEERP_ALLOCA_MERGING_H

