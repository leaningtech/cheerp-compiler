//===-- ReplaceNopCastsAndByteSwaps.cpp - The Cheerp JavaScript generator -===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014-2016 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/InitializePasses.h"
#include "llvm/Cheerp/ReplaceNopCastsAndByteSwaps.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Support/FormattedStream.h"
#include "memory"

namespace cheerp {

using namespace llvm;

bool ReplaceNopCastsAndByteSwaps::runOnFunction(Function & F)
{
	// We can use the same IntrinsicLowering over and over again
	if ( !IL )
	{
		const DataLayout* DL = &F.getParent()->getDataLayout();
		assert(DL);
		IL = std::unique_ptr<IntrinsicLowering>(new IntrinsicLowering(*DL));
	}

	if ( F.empty() )
		return false;

	bool Changed = false;

	for ( BasicBlock & BB : F )
		Changed |= processBasicBlock(BB);
	
	assert( ! verifyFunction(F, &llvm::errs()) );

	return Changed;
}

bool ReplaceNopCastsAndByteSwaps::processBasicBlock(BasicBlock& BB)
{
	bool Changed = false;
	
	/**
	 * First pass: replace nopCasts with bitcasts and bswap intrinsics with logic operations
	
	 */
	for ( BasicBlock::iterator it = BB.begin(); it != BB.end(); )
	{
		Instruction * Inst = &*it++;
		
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
		else if( IntrinsicInst* II = dyn_cast<IntrinsicInst>(Inst) )
		{
			if(II->getIntrinsicID() == Intrinsic::bswap)
			{
				IL->LowerIntrinsicCall(II);
				Changed = true;
			}
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

StringRef ReplaceNopCastsAndByteSwaps::getPassName() const {
	return "ReplaceNopCastsAndByteSwaps";
}

char ReplaceNopCastsAndByteSwaps::ID = 0;

FunctionPass *createReplaceNopCastsAndByteSwapsPass() { return new ReplaceNopCastsAndByteSwaps(); }

}

using namespace cheerp;

INITIALIZE_PASS_BEGIN(ReplaceNopCastsAndByteSwaps, "ReplaceNopCasts", "Replace type safe cast intrinsics with bitcasts and bswap intrinsics with logical operations",
                      false, false)
INITIALIZE_PASS_END(ReplaceNopCastsAndByteSwaps, "ReplaceNopCastsAndByteSwaps", "Replace type safe cast intrinsics with bitcasts and bswap intrinsics with logical operations",
                    false, false)
