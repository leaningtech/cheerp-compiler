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

#include "Relooper.h"
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
	CFGStackifier(const llvm::Function &F, const llvm::LoopInfo& LI,
		const llvm::DominatorTree& DT, const Registerize& R, const PointerAnalyzer& PA);

	TokenList Tokens;
};


}

#endif
