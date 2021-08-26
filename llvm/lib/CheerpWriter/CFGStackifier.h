//===-- Cheerp/CFGStackifier.h - Cheerp rendering helper ---------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2018 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_CFG_STACKIFIER_NEW_H
#define _CHEERP_CFG_STACKIFIER_NEW_H

#include "llvm/Cheerp/TokenList.h"

#include "llvm/IR/Module.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/Dominators.h"

#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/PointerAnalyzer.h"

namespace cheerp
{

class CFGStackifier
{
public:
	enum Mode {
		GenericJS,
		AsmJS,
		Wasm,
	};
	CFGStackifier(const llvm::Function &F, const llvm::LoopInfo& LI,
		const llvm::DominatorTree& DT, const Registerize& R,
		const PointerAnalyzer& PA, Mode M);

	TokenList Tokens;
	std::vector<const llvm::BasicBlock*> selectBasicBlocksWithPossibleIncomingResult() const;
	void addResultToTokens(const std::map<const llvm::BasicBlock*, const llvm::PHINode*>& specialPHINodes, const Registerize& registerize);
};


}

#endif
