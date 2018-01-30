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

#include "llvm/IR/Module.h"
#include "llvm/Cheerp/LinearMemoryHelper.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/SmallSet.h"

namespace cheerp
{

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

	bool equivalentFunction(llvm::Function* A, llvm::Function* B);
	bool equivalentBlock(llvm::BasicBlock* A, llvm::BasicBlock* B);
	bool equivalentInstruction(const llvm::Instruction* A, const llvm::Instruction* B);
	bool equivalentOperand(const llvm::Value* A, const llvm::Value* B);
	bool equivalentConstant(const llvm::Constant* A, const llvm::Constant* B);
	bool equivalentType(const llvm::Type* A, const llvm::Type* B);
	bool equivalentIndices(const llvm::GetElementPtrInst* A, const llvm::GetElementPtrInst* B);
	bool ignoreInstruction(const llvm::Instruction* I);
	bool hasSameIntegerBitWidth(const llvm::Type* A, const llvm::Type* B);

	bool mergeTwoFunctions(llvm::Function* F, llvm::Function* G);

	llvm::SmallSet<const llvm::PHINode*, 16> visitedPhis;
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
