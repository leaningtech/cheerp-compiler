//===-- Cheerp/GlobalDepsAnalyzer.h - Cheerp utility code ---------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_GLOBAL_DEPS_ANALYZER_H
#define _CHEERP_GLOBAL_DEPS_ANALYZER_H

#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Module.h"
#include "llvm/Cheerp/Utility.h"
#include <string>
#include <unordered_map>
#include <unordered_set>

namespace cheerp
{

/**
 * Determine which globals (variables and functions) need to be compiled.
 * 
 * It also computes a proper ordering between variables and functions to satisfy dependencies
 */
class GlobalDepsAnalyzer
{
public:
	typedef llvm::SmallVector<const llvm::Use *, 8> SubExprVec;
	typedef std::unordered_multimap< const llvm::GlobalVariable *, SubExprVec > FixupMap;

	/**
	 * Run the filter. Notice that it does not modify the module.
	 */
	GlobalDepsAnalyzer( const llvm::Module & );
	
	/**
	 * Get the ordered list of all the global values
	 */
	const std::vector< const llvm::GlobalValue * > globalOrderedList() const
	{
		return globalsOrder;
	}
	
	/**
	 * Get the ordered list of all the functions
	 */
	const std::vector< const llvm::Function * > functionOrderedList() const
	{
		return functionOrder;
	}
	
	/**
	 * Get the ordered list of all the global variables
	 */
	const std::vector< const llvm::GlobalVariable * > varsOrderedList() const
	{
		return varsOrder;
	}
	
	/**
	 * Determine if a given global value is reachable
	 * 
	 * Notice that this is the only function in this class which accepts a GlobalAlias as input.
	 */
	bool isReachable(const llvm::GlobalValue * val) const
	{
		return reachableGlobals.count(val);
	}
	
	/**
	 * Get the map of global variables whose definitions triggers the completion of the definition of other global vars
	 */
	const FixupMap & fixupVars() const { return varsFixups; }
	
	/**
	 * Get a list of the classes which require bases info
	 */
	const std::unordered_set<llvm::StructType*> & classesWithBaseInfo() const { return classesNeeded; }
	
	/**
	 * Get a list of the arrays which are dynamically allocated with unknown size
	 */
	const std::unordered_set<llvm::StructType*> & dynAllocArrays() const { return arraysNeeded; }
	
	/**
	 * Get the list of constructors (static initializers) required by the program
	 */
	const std::vector<const llvm::Function*> & constructors() const { return constructorsNeeded; }
	
	/**
	 * Determine if we need to compile a cheerpCreateClosure function
	 */
	bool needCreateClosure() const { return hasCreateClosureUsers; }
	
	/**
	 * Determine if we need to compile an handleVAArg function
	 */
	bool needHandleVAArg() const { return hasVAArgs; }
	
	/**
	 * Determine if we need to compile a createPointerArrays function
	 */
	bool needCreatePointerArray() const { return hasPointerArrays; }
	
private:
	typedef llvm::SmallSet<const llvm::GlobalValue*, 8> VisitedSet;
	
	/**
	 * Propagate the search across globalvalues (i.e. Functions, GlobalVariables and GlobalAliases)
	 * 
	 * visited contains the nodes which are being currently visited.
	 * the subexpr argument is required whenever we are calling this from a visitConstant. 
	 * It is used only in the circular dependency case, to fill up the fixup map
	 */
	void visitGlobal( const llvm::GlobalValue *, 
			  VisitedSet & visited, 
			  const SubExprVec & subexpr = SubExprVec() );
	
	/** 
	 * Propagate the search across constant expressions.
	 * 
	 * visited is just forwarded to visitGlobal
	 * subexpr is filled with the list of uses followed to get to the current constant being processed
	 */
	void visitConstant( const llvm::Constant *, 
			    VisitedSet & visited, 
			    SubExprVec & subexpr );
	
	/**
	 * Visit every instruction inside a function.
	 */
	void visitFunction( const llvm::Function * F, VisitedSet & visited );
	
	/**
	 * Helper functions to push a variable/function in the right list
	 */
	void pushFunction( const llvm::Function * F )
	{
		globalsOrder.push_back(F);
		functionOrder.push_back(F);
	}
	
	void pushVar( const llvm::GlobalVariable * V )
	{
		globalsOrder.push_back(V);
		varsOrder.push_back(V);
	}

	const llvm::Module & module;
	TypeSupport types;
	std::unordered_set< const llvm::GlobalValue * > reachableGlobals; // Set of all the reachable globals
	
	FixupMap varsFixups;
	std::unordered_set<llvm::StructType* > classesNeeded;
	std::unordered_set<llvm::StructType* > arraysNeeded;
	std::vector< const llvm::Function* > constructorsNeeded;
		
	std::vector< const llvm::GlobalValue * > globalsOrder;
	std::vector< const llvm::Function * > functionOrder;
	std::vector< const llvm::GlobalVariable * > varsOrder;
	
	bool hasCreateClosureUsers = false;
	bool hasVAArgs = false;
	bool hasPointerArrays = false;
};

/**
 * Use a GlobalDepsAnalyzer object to actually remove all the unused
 * function/variables from a module.
 * 
 * This also sorts the functions and the global variables in the module
 * according to the ordering provided by the GlobalDepsAnalyzer object.
 * 
 * Return the number of globals removed from the module.
 * 
 * \warning Even if this returns 0, it does *not* mean that the module has 
 * not been modified. This function also *reorders* the variables inside the module.
 */
int filterModule( llvm::Module &, const GlobalDepsAnalyzer & fg );

}

#endif
