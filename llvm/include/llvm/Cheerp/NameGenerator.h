//===-- Cheerp/NameGenerator.h - Cheerp name generator code ----------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2013 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_NAME_GENERATOR_H
#define _CHEERP_NAME_GENERATOR_H

#include "llvm/IR/Value.h"
#include "llvm/IR/Instructions.h"
#include <map>
#include <set>

namespace cheerp {

// This class is responsible for generate unique indices for a llvm::Value
/**
 * At the moment the implementation is very simple, it is supposed to become
 * much more smarter (i.e. recicle names depending on the scope) in the future.
 */
class NameGenerator
{
public:
	uint32_t getUniqueIndexForValue(const llvm::Value* v) const;
	uint32_t getUniqueIndexForPHI(const llvm::Function * f);

private:
	typedef std::map<const llvm::Value*, uint32_t> UnnamedMap;

	mutable std::map<const llvm::Function*, UnnamedMap> unnamedValueMap;
	std::map<const llvm::Function *, uint32_t> currentUniqueIndex;
};

}

#endif //_CHEERP_NAME_GENERATOR_H
