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
	constexpr EdgeContext():fromBB(NULL), toBB(NULL)
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
	}
	void setEdgeContext(const llvm::BasicBlock* fromBB_, const llvm::BasicBlock* toBB_)
	{
		assert(isNull());
		fromBB=fromBB_;
		toBB=toBB_;
	}
	constexpr static EdgeContext emptyContext()
	{
		return EdgeContext();
	}
};

}

#endif
