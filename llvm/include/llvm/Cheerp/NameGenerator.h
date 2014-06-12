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

#include "llvm/ADT/SmallString.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Instructions.h"
#include <unordered_map>

namespace cheerp {

class GlobalDepsAnalyzer;

// This class is responsible for generate unique names for a llvm::Value
class NameGenerator
{
public:
	/**
	 * This initialize the namegenerator by collecting
	 * all the global variable names
	 */
	explicit NameGenerator( const GlobalDepsAnalyzer &, bool makeReadableNames = true );

	/**
	 * Return the computed name for the given variable.
	 * This function can be called only if the passed value is not an inlined instruction
	 */
	llvm::StringRef getName(const llvm::Value* v) const
	{
		assert(namemap.count(v) );
		assert(! namemap.at(v).empty() );
		return namemap.at(v);
	}

	// This will be removed when we will entirely get rid of PHIs.
	uint32_t getUniqueIndexForPHI(const llvm::Function * f);

	// Filter the original string so that it no longer contains invalid JS characters.
	static llvm::SmallString<4> filterLLVMName( llvm::StringRef, bool isGlobalName );

private:
	void generateCompressedNames( const GlobalDepsAnalyzer & );
	void generateReadableNames( const GlobalDepsAnalyzer & );
	
	// Determine if an instruction actually needs a name
	bool needsName(const llvm::Instruction &) const;

	std::unordered_map<const llvm::Value*, llvm::SmallString<4> > namemap;
	std::unordered_map<const llvm::Function *, uint32_t> currentUniqueIndexForPHI;
};

}

#endif //_CHEERP_NAME_GENERATOR_H
