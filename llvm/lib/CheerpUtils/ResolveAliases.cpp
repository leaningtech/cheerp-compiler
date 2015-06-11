//===-- ResolveAliases.cpp - Replace aliases with aliasees -----------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "CheerpResolveAliases"
#include "llvm/ADT/Statistic.h"
#include "llvm/Cheerp/ResolveAliases.h"

STATISTIC(NumAliasRemoved, "Number of global aliases removed");

namespace llvm {

const char* ResolveAliases::getPassName() const
{
	return "CheerpResolveAliases";
}

char ResolveAliases::ID = 0;

bool ResolveAliases::runOnModule(Module & m)
{
	bool Changed = false;
	
	for (Module::alias_iterator it = m.alias_begin(); it != m.alias_end(); )
	{
		GlobalAlias * A = it++;
		A->replaceAllUsesWith( A->getAliasee() );
		A->eraseFromParent();
		
		NumAliasRemoved++;
		
		Changed = true;
	}
	
	assert( m.alias_empty() );
	
	return Changed;
}

ModulePass* createResolveAliasesPass()
{
	return new ResolveAliases();
}

}
