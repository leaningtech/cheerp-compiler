//===-- Cheerp/EdgeContext.h - Cheerp utitily class ---------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2019 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_EDGE_CONTEXT_H
#define _CHEERP_EDGE_CONTEXT_H

#include "llvm/IR/BasicBlock.h"

namespace cheerp
{
// Context used to disambiguate temporary values used in PHI resolution
struct EdgeContext
{
	const llvm::BasicBlock* fromBB;
	const llvm::BasicBlock* toBB;
	uint32_t assigmentIndex;
	constexpr EdgeContext()
		: fromBB(NULL), toBB(NULL), assigmentIndex(-1u)
	{
	}
	bool isNull() const
	{
		return fromBB==NULL;
	}
	void clear()
	{
		fromBB=NULL;
		toBB=NULL;
		assigmentIndex=-1u;
	}
	void setEdgeContext(const llvm::BasicBlock* fromBB_, const llvm::BasicBlock* toBB_)
	{
		assert(isNull());
		fromBB=fromBB_;
		toBB=toBB_;
		assigmentIndex=1;
	}
	void processAssigment()
	{
		++assigmentIndex;
	}
	void undoAssigment()
	{
		--assigmentIndex;
	}
	constexpr static EdgeContext emptyContext()
	{
		return EdgeContext();
	}
};

}

#endif
