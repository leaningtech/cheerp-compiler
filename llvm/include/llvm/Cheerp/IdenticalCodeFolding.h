//===-- Cheerp/IdenticalCodeFolding.cpp - Remove duplicate functions/globals --===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#pragma once

#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Module.h"
#include "llvm/Cheerp/LinearMemoryHelper.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/SmallSet.h"

#include <unordered_map>

namespace cheerp
{

struct function_pair_hash {
	template <class T1, class T2>
	std::size_t operator () (const std::pair<T1,T2> &p) const {
		auto h1 = std::hash<T1>{}(p.first);
		auto h2 = std::hash<T2>{}(p.second);
		return llvm::hashing::detail::hash_16_bytes(h1, h2);
	}
};

/**
 * Determine which globals (variables and functions) can be folded since they
 * are identical.
 */
class IdenticalCodeFolding : public llvm::ModulePass
{
public:
	static char ID;

	IdenticalCodeFolding();

	bool runOnModule(llvm::Module& ) override;

	void getAnalysisUsage(llvm::AnalysisUsage& ) const override;

private:
	const char* getPassName() const override;
	uint64_t hashFunction(llvm::Function& F);

	bool equivalentFunction(const llvm::Function* A, const llvm::Function* B);
	bool equivalentBlock(const llvm::BasicBlock* A, const llvm::BasicBlock* B);
	bool equivalentInstruction(const llvm::Instruction* A, const llvm::Instruction* B);
	bool equivalentOperand(const llvm::Value* A, const llvm::Value* B);
	bool equivalentConstant(const llvm::Constant* A, const llvm::Constant* B);
	bool equivalentType(const llvm::Type* A, const llvm::Type* B);
	bool equivalentGep(const llvm::GetElementPtrInst* A, const llvm::GetElementPtrInst* B);
	bool ignoreInstruction(const llvm::Instruction* I);
	bool hasSameIntegerBitWidth(const llvm::Type* A, const llvm::Type* B);
	bool isStaticIndirectFunction(const llvm::Value* A);

	void mergeTwoFunctions(llvm::Function* F, llvm::Function* G);

	const llvm::DataLayout *DL;

	llvm::SmallSet<const llvm::PHINode*, 16> visitedPhis;
	std::unordered_map<std::pair<const llvm::Function*, const llvm::Function*>, bool, function_pair_hash> functionEquivalence;
};

inline llvm::Pass* createIdenticalCodeFoldingPass()
{
	return new IdenticalCodeFolding;
}

class HashAccumulator64 {
	uint64_t hash;
public:
	HashAccumulator64() {
		// Initialize to random constant, so the state isn't zero.
		hash = 0x6acaa36bef8325c5ULL;
	}
	void add(uint64_t value) {
		hash = llvm::hashing::detail::hash_16_bytes(hash, value);
	}
	uint64_t getHash() {
		return hash;
	}
};

}
