//===-- Cheerp/PassUtility.h - Pass manager utility ---==//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2022 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef CHEERP_PASS_UTILITY_H
#define CHEERP_PASS_UTILITY_H

#include "llvm/IR/PassManager.h"

template <class P>
class RequiredPassWrapper : public P
{
	public:
	static bool isRequired() {return true;}
};

#endif

