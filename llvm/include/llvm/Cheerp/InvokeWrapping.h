//===-- Cheerp/InvokeWrapping.h ------------------------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2021 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_INVOKE_WRAPPING_H
#define _CHEERP_INVOKE_WRAPPING_H

#include "llvm/Pass.h"
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
	void populate(llvm::Module& M);
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

class InvokeWrapping: public llvm::ModulePass
{
public:
	static char ID;

	explicit InvokeWrapping()
		: ModulePass(ID)
	{ }

	virtual bool runOnModule(llvm::Module &M) override;
	virtual void getAnalysisUsage(llvm::AnalysisUsage & AU) const override;
	virtual llvm::StringRef getPassName() const override
	{
		return "InvokeWrapping";
	}
	const LandingPadTable& getLandingPadTable() const
	{
		return table;
	}

private:
	LandingPadTable table;
};

llvm::ModulePass *createInvokeWrappingPass();
}

#endif //_CHEERP_INVOKE_WRAPPING_H

