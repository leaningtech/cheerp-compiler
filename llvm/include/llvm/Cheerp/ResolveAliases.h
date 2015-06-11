//===-- Cheerp/ResolveAliases.h - Cheerp utility code ---------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_RESOLVE_ALIASES_H
#define _CHEERP_RESOLVE_ALIASES_H

#include "llvm/IR/Module.h"

namespace llvm
{

/**
 * ResolveAliases - Replace all the GlobalAliases with the respective aliasee, 
 *  even if they are weak aliases.
 */
class ResolveAliases : public llvm::ModulePass
{
public:
	static char ID;
	
	explicit ResolveAliases() : ModulePass(ID) { }
	
	bool runOnModule( llvm::Module & ) override;
	
	const char *getPassName() const override;
};

llvm::ModulePass *createResolveAliasesPass();

}

#endif
