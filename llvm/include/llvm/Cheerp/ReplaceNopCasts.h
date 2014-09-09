//===-- Cheerp/ReplaceNopCasts.h - Replace nop casts with bitcasts ---------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_REPLACE_NOP_CASTS_H
#define _CHEERP_REPLACE_NOP_CASTS_H

#include "llvm/IR/Instructions.h"
#include "llvm/Pass.h"

namespace cheerp {

// Replace all NopCasts with BitCast, and report warnings for existing unsafe bitcasts
class ReplaceNopCasts: public llvm::FunctionPass
{
public:
	static char ID;

	explicit ReplaceNopCasts() : FunctionPass(ID) { }

	virtual bool runOnFunction(llvm::Function &F) override;
	
	virtual const char *getPassName() const override;
	
private:
	bool processBasicBlock(llvm::BasicBlock & BB);
	
	void reportUnsafeCast( const llvm::User * u ) const;
};

//===----------------------------------------------------------------------===//
//
// ReplaceNopCasts
//
llvm::FunctionPass *createReplaceNopCastsPass();
}

#endif //_CHEERP_REPLACE_NOP_CASTS_H

