//===-- Cheerp/InvokeWrapping.h ------------------------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2021-2022 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_INVOKE_WRAPPING_H
#define _CHEERP_INVOKE_WRAPPING_H

#include "llvm/IR/Instruction.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"

namespace cheerp {

class LandingPadTable
{
public:
	struct Entry
	{
		llvm::Value* start;
		llvm::Value* n;
	};
	class LocalTypeIdMap
	{
	private:
		llvm::DenseMap<llvm::GlobalValue*, int> typeIdMap;
	public:
		int getTypeIdFor(llvm::Value* V);
		int getTypeIdFor(llvm::Value* V) const;
	};

	LandingPadTable(): table(nullptr)
	{
	};
	void populate(llvm::Module& M, cheerp::GlobalDepsAnalyzer& GDA);
	Entry getEntry(const llvm::LandingPadInst* lpad) const
	{
		auto it = entries.find(lpad);
		assert(it != entries.end() && "missing landing pad");
		return it->second;
	}
	void addEntry(const llvm::LandingPadInst* lpad, Entry e)
	{
		entries.insert(std::make_pair(lpad, e));
	}
	LocalTypeIdMap& getLocalTypeIdMap(const llvm::Function* F)
	{
		return localTypeIdMaps[F];
	}
	const LocalTypeIdMap& getLocalTypeIdMap(const llvm::Function* F) const
	{
		auto it = localTypeIdMaps.find(F);
		assert(it != localTypeIdMaps.end());
		return it->getSecond();
	}

private:
	llvm::GlobalVariable* table;
	llvm::DenseMap<const llvm::LandingPadInst*, Entry> entries;
	llvm::DenseMap<const llvm::Function*, LocalTypeIdMap> localTypeIdMaps;
};

class InvokeWrappingAnalysis;

class InvokeWrapping
{
public:
	explicit InvokeWrapping()
	{ }

	bool runOnModule(llvm::Module &M, cheerp::GlobalDepsAnalyzer& GDA);
	const LandingPadTable& getLandingPadTable() const
	{
		return table;
	}
	bool invalidate(llvm::Module& M, const llvm::PreservedAnalyses& PA, llvm::ModuleAnalysisManager::Invalidator&)
	{
		auto PAC = PA.getChecker<InvokeWrappingAnalysis>();
		if (PAC.preserved())
		{
			return false;
		}
		llvm::errs() << "-------\tinvalidate on InvokeWrapping\n";
		//SHOULD NEVER BE CALLED!
		return true;
	}

private:
	LandingPadTable table;
};

class InvokeWrappingAnalysis;

class InvokeWrappingWrapper {
	static InvokeWrapping* innerPtr;
public:
	static InvokeWrapping& getInner()
	{
		if (innerPtr)
			delete innerPtr;
		innerPtr = new InvokeWrapping();
		return *innerPtr;
	}
	operator InvokeWrapping&()
	{
		assert(innerPtr);
		return *innerPtr;
	}
	bool invalidate(llvm::Module& M, const llvm::PreservedAnalyses& PA, llvm::ModuleAnalysisManager::Invalidator&)
	{
		auto PAC = PA.getChecker<InvokeWrappingAnalysis>();
		return !PAC.preserved();
	}
};

class InvokeWrappingPass : public llvm::PassInfoMixin<InvokeWrappingPass> {
public:
	llvm::PreservedAnalyses run(llvm::Module& M, llvm::ModuleAnalysisManager& MAM);
	static bool isRequired() { return true;}
};

class InvokeWrappingAnalysis : public llvm::AnalysisInfoMixin<InvokeWrappingAnalysis> {
	friend llvm::AnalysisInfoMixin<InvokeWrappingAnalysis>;
	static llvm::AnalysisKey Key;
public:
	using Result = InvokeWrappingWrapper;
	static Result run(llvm::Module& M, llvm::ModuleAnalysisManager&)
	{
		static llvm::Module* modulePtr = nullptr;
		assert(modulePtr != &M);
		modulePtr = &M;
		return InvokeWrappingWrapper();
	}
};

}

#endif //_CHEERP_INVOKE_WRAPPING_H

