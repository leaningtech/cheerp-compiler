//===-- Cheerp/GlobalDepsAnalyzer.h - Cheerp utility code ---------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2022 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_GLOBAL_DEPS_ANALYZER_H
#define _CHEERP_GLOBAL_DEPS_ANALYZER_H

#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/Cheerp/DeterministicUnorderedSet.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Cheerp/BuiltinInstructions.h"
#include <array>
#include <string>
#include <unordered_map>
#include <unordered_set>

namespace cheerp
{

// Handy alias for a deterministic set of const llvm::Function* with no erasures
using DeterministicFunctionSet = cheerp::DeterministicUnorderedSet<const llvm::Function*, RestrictionsLifted::NoPointerStability>;

extern const char* wasmNullptrName;

/**
 * Determine which globals (variables and functions) need to be compiled.
 * 
 * It also computes a proper ordering between variables and functions to satisfy dependencies
 */
class GlobalDepsAnalyzerWrapper;

class GlobalDepsAnalyzer 
{
	llvm::ModuleAnalysisManager* MAM;
	friend GlobalDepsAnalyzerWrapper;
public:
	/**
	 * Select how to deal with math functions which are provided natively by JS or Wasm
	 */
	enum MATH_MODE { NO_BUILTINS = 0, JS_BUILTINS, WASM_BUILTINS };
	typedef llvm::SmallVector<const llvm::Use *, 8> SubExprVec;
	typedef std::unordered_multimap< const llvm::GlobalVariable *, SubExprVec > FixupMap;

	GlobalDepsAnalyzer(MATH_MODE mathMode = NO_BUILTINS, bool llcPass = false, bool wasmStart = false);
	
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
	const std::unordered_set<llvm::StructType*> & classesWithBaseInfo() const { return classesWithBaseInfoNeeded; }
	
	/**
	 * Get a list of the classes which are allocated in the code
	 */
	const std::unordered_set<llvm::StructType*> & classesUsed() const { return classesNeeded; }

	/**
	 * Get a list of the arrays which are dynamically allocated with unknown size
	 */
	const std::unordered_set<llvm::Type*> & dynAllocArrays() const { return arraysNeeded; }

	/**
	 * Get a list of the arrays which are dynamically resized
	 */
	const std::unordered_set<llvm::Type*> & dynResizeArrays() const { return arrayResizesNeeded; }
	
	/**
	 * Get a list of the asm.js functions called from genericjs
	 */
	const DeterministicFunctionSet & asmJSExports() const { return asmJSExportedFuncions; }

	/**
	 * Get a list of the genericjs functions called from asm.js
	 */
	const DeterministicFunctionSet & asmJSImports() const { return asmJSImportedFuncions; }

	/**
	 * Get the entry point of the program, might be webMain or main
	 */
	const llvm::Function* getEntryPoint() const { return entryPoint; }
	
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

	/**
	 * Determine if we need to compile the asm.js module
	 */
	bool needAsmJSCode() const { return hasAsmJSCode; }
	/**
	 * Determine if we need to compile the asm.js module
	 */
	bool needAsmJSMemory() const { return hasAsmJSMemory; }
	/**
	 * Determine if we need to compile the definition of CheerpException
	 */
	bool needCheerpException() const { return hasCheerpException; }
	
	/**
	 * Determine if linear memory malloc is ever used
	 */
	bool usesAsmJSMalloc() const { return hasAsmJSMalloc; }

	bool runOnModule( llvm::Module & );

	void visitType( llvm::Type* t, bool forceTypedArray );

	// Visit the `derived` struct and all its bases recursively, looking for `base`.
	// If there is a match, add `derived` to the classesWithBasesInfoNeeded set.
	// Memoize the result in visitedClasses, to avoid traversing multiple times.
	// NOTE: In presence of virtual bases, the virtual base type will have a `.base`
	// suffix, and will be different from the type represented by `base`.
	// We are going to IGNORE this case, for the following reason:
	// the point of this function is to generate the downcast array for the operands
	// of virtualcasts. In order to get a pointer to a virtual base, and use it as an
	// argument to virtualcast, we necesssarily did a virtualcast in the reverse direction
	// first somewhere. So the `derived` class (and all classes derived from it) will
	// be added to the set at that point, and it is fine to miss it when dealing with
	// the `.base` type.
	void visitVirtualcastBases(llvm::StructType* derived, llvm::StructType* base, std::unordered_map<llvm::StructType*, bool>& visitedClasses);

	llvm::StructType* needsDowncastArray(llvm::StructType* t) const;

	// Utility function to get the name of the asm.js function table for the
	// given function type
	std::string getFunctionTableName(const llvm::FunctionType* ft);

	// When folding identical functions, it is possible that the original
	// function is exported while the replacement is not. This function is used
	// to re-insert the replacement function into the exported function list.
	// It is also used in FFIWrapping.
	void insertAsmJSExport(const llvm::Function* F);
	// This is used in FFIWrapping to add the newly created wrappers as imports
	void insertAsmJSImport(const llvm::Function* F);
	// This is used in FFIWrapping to remove a wrapped function as import.
	void removeAsmJSImport(const llvm::Function* F);

	// This is used in InvokeWrapping to insert the array of __cheerp_clause
	// if it is longer than 8 elements.
	void insertDynAllocArray(llvm::Type* t);

	// Remove function from GDA's function list.
	void eraseFunction(llvm::Function* F);

private:
	typedef llvm::SmallSet<const llvm::GlobalValue*, 8> VisitedSet;
	
	void logUndefinedSymbol(const llvm::GlobalValue* GV);

	/**
	 * Propagate the search across globalvalues (i.e. Functions, GlobalVariables and GlobalAliases)
	 * 
	 * visited contains the nodes which are being currently visited.
	 * the subexpr argument is required whenever we are calling this from a visitConstant. 
	 * It is used only in the circular dependency case, to fill up the fixup map
	 */
	void visitGlobal( const llvm::GlobalValue *, 
			  VisitedSet & visited, 
			  const SubExprVec & subexpr );
	
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
	 * Visit every sub-structure inside this struct
	 */
	void visitStruct( llvm::StructType* ST );
	/**
	 * Remove all the unused function/variables from a module.
	 * 
	 * This also sorts the global variables to minimize unresolved dependencies
	 * 
	 * Return the number of globals removed from the module.
	 * 
	 * \warning Even if this returns 0, it does *not* mean that the module has 
	 * not been modified. This function also *reorders* the variables inside the module.
	 */
	int filterModule( const llvm::DenseSet<const llvm::Function*>&, llvm::Module & );

	static bool isMathIntrinsic(const llvm::Function* F);

	//Insert Function on execution queue
	void enqueueFunction(const llvm::Function* F);

	//Process Functions in the execution queue
	void processEnqueuedFunctions();

	//Pre-process direct calls to possibly simplify them
	void simplifyCalls(llvm::Module & module) const;

	//Extend lifetime of function, visiting them and declaring external
	void extendLifetime(llvm::Function* F);

	std::unordered_set< const llvm::GlobalValue * > reachableGlobals; // Set of all the reachable globals
	
	FixupMap varsFixups;
	std::unordered_set<llvm::StructType* > classesWithBaseInfoNeeded;
	std::unordered_set<llvm::StructType* > classesNeeded;
	std::unordered_set<llvm::Type* > arraysNeeded;
	std::unordered_set<llvm::Type* > arrayResizesNeeded;
	DeterministicFunctionSet asmJSExportedFuncions;
	DeterministicFunctionSet asmJSImportedFuncions;

	std::vector< const llvm::GlobalVariable * > varsOrder;
	std::vector< llvm::GlobalValue * > externals;
	std::vector< const llvm::Function* > functionsQueue;

	std::unordered_map<llvm::StructType*, uint32_t> basesInfo;

	std::array<bool, BuiltinInstr::numGenericBuiltins()> hasBuiltin;

	MATH_MODE mathMode;

	const llvm::DataLayout* DL;

	const llvm::Function* entryPoint;
	
	bool hasCreateClosureUsers;
	bool hasVAArgs;
	bool hasPointerArrays;
	bool hasAsmJSCode;
	bool hasAsmJSMemory;
	bool hasAsmJSMalloc;
	bool hasCheerpException;
	bool mayNeedAsmJSFree;

	bool llcPass;
	bool wasmStart;
	bool hasUndefinedSymbolErrors;
public:
	bool forceTypedArrays;
	bool needsBuiltin(BuiltinInstr::BUILTIN b)
	{
		return hasBuiltin[b];
	}
	MATH_MODE getMathMode() const
	{
		return mathMode;
	}

	/**
	 * Visit dynamic-sized allocation
	 */
	void visitDynSizedAlloca( llvm::Type* pointedType );
};

class GlobalDepsAnalysis;

struct GlobalDepsInitializer
{
	cheerp::GlobalDepsAnalyzer::MATH_MODE mathMode;
	bool resolveAliases;
	bool WasmOnly;
};


class GlobalDepsAnalyzerWrapper {
	static GlobalDepsAnalyzer* innerPtr;
public:
	static GlobalDepsAnalyzer& getInner(llvm::ModuleAnalysisManager& MAM, GlobalDepsInitializer& data)
	{
		if (innerPtr)
			delete innerPtr;
		innerPtr = new GlobalDepsAnalyzer(data.mathMode, data.resolveAliases, data.WasmOnly);
		innerPtr->MAM = &MAM;
		return *innerPtr;
	}
	operator GlobalDepsAnalyzer&()
	{
		assert(innerPtr);
		return *innerPtr;
	}
	bool invalidate(llvm::Module& M, const llvm::PreservedAnalyses& PA, llvm::ModuleAnalysisManager::Invalidator&)
	{
		auto PAC = PA.getChecker<GlobalDepsAnalysis>();
		return !PAC.preserved();
	}
};

class GlobalDepsAnalyzerPass : public llvm::PassInfoMixin<GlobalDepsAnalyzerPass> {
	GlobalDepsInitializer data;
public:
	llvm::PreservedAnalyses run(llvm::Module &M, llvm::ModuleAnalysisManager&);
	GlobalDepsAnalyzerPass(GlobalDepsAnalyzer::MATH_MODE mathMode=GlobalDepsAnalyzer::MATH_MODE::NO_BUILTINS, bool llcPass = false, bool wasmStart = false) : data({mathMode, llcPass, wasmStart}) {}
	static bool isRequired() { return true; }
};

class GlobalDepsAnalysis : public llvm::AnalysisInfoMixin<GlobalDepsAnalysis> {
	friend llvm::AnalysisInfoMixin<GlobalDepsAnalysis>;
	static llvm::AnalysisKey Key;
public:
	using Result = GlobalDepsAnalyzerWrapper;
	GlobalDepsAnalyzerWrapper run(llvm::Module& M, llvm::ModuleAnalysisManager&);
};

} //cheerp
#endif
