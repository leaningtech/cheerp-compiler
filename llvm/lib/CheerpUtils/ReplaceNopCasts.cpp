//===-- ReplaceNopCasts.cpp - The Cheerp JavaScript generator ---------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/ReplaceNopCasts.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Support/FormattedStream.h"

namespace cheerp {

using namespace llvm;

bool ReplaceNopCasts::runOnFunction(Function & F)
{
	if ( F.empty() )
		return false;

	bool Changed = false;

	for ( BasicBlock & BB : F )
		Changed |= processBasicBlock(BB);
	
	assert( ! verifyFunction(F, &llvm::errs()) );

	return Changed;
}

bool ReplaceNopCasts::processBasicBlock(BasicBlock& BB)
{
	bool Changed = false;
	
	/**
	 * First pass: replace nopCasts with bitcasts and report warning for invalid type casts
	 */
	for ( BasicBlock::iterator it = BB.begin(); it != BB.end(); )
	{
		Instruction * Inst = it++;
		
		if (isNopCast(Inst) )
		{
			assert( isa<CallInst>(Inst) );
			
			CallInst * call = cast<CallInst>(Inst);
			
			if ( TypeSupport::isClientType( call->getType()) )
			{
				llvm::errs() << "Cast of client type: " << *call << "\n";
				continue;
			}
			if ( TypeSupport::isClientType( call->getArgOperand(0)->getType()) )
			{
				llvm::errs() << "Cast of client type: " << *call->getArgOperand(0) << "\n";
				continue;
			}

			ReplaceInstWithInst( call,  BitCastInst::Create( Instruction::CastOps::BitCast, call->getArgOperand(0), call->getType() ) );

			Changed = true;
		}
	}
	
	/**
	 * Second pass: collapse bitcasts of bitcasts.
	 * 
	 * Note: this might leave some dead instruction around, but we don't care since bitcasts are inlined anyway
	 */
	for ( BasicBlock::iterator it = BB.begin(); it != BB.end(); ++it )
	{
		if ( isa<BitCastInst>(it) ) 
		{
			while ( BitCastInst * src = dyn_cast<BitCastInst>(it->getOperand(0) ) )
			{
				it->setOperand(0, src->getOperand(0) );
				Changed = true;
			}
		}
	}

	return Changed;
}

void ReplaceNopCasts::reportUnsafeCast(const User* u) const
{
	cast<Instruction>(u)->getParent()->getParent()->dump();
	llvm::errs() << "warning in instruction: " << *u << "\n";
	llvm::errs() << "\t Type conversion between: \t" << *u->getOperand(0)->getType() << " and " << *u->getType() << "\n\tis not safe. ";
	llvm::errs() << "Expect issues. And report a bug.\n\n";
}

const char *ReplaceNopCasts::getPassName() const {
	return "ReplaceNopCasts";
}

char ReplaceNopCasts::ID = 0;

FunctionPass *createReplaceNopCastsPass() { return new ReplaceNopCasts(); }

}
