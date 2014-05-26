//===-- Opcodes.cpp - The Duetto JavaScript generator ---------------------===//
//
//                     Duetto: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Duetto/NameGenerator.h"
#include "llvm/IR/Function.h"

using namespace llvm;

namespace duetto {

uint32_t NameGenerator::getUniqueIndexForValue(const Value* v) const
{
	const Function * f = nullptr;
	if ( const Instruction * Instr = dyn_cast<Instruction>(v) )
		f = Instr->getParent()->getParent();
	
	UnnamedMap & fMap = unnamedValueMap[f];

	UnnamedMap::iterator it = fMap.find(v);

	if( it == fMap.end() )
		it = fMap.emplace(v, fMap.size() ).first;

	return it->second;
}

uint32_t NameGenerator::getUniqueIndexForPHI(const llvm::Function * f)
{
	return currentUniqueIndex[f]++;
}

}
