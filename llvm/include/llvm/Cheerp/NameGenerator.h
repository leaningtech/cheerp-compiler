//===-- Cheerp/NameGenerator.h - Cheerp name generator code ----------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2019 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_NAME_GENERATOR_H
#define _CHEERP_NAME_GENERATOR_H

#include "llvm/ADT/SmallString.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Cheerp/LinearMemoryHelper.h"
#include "llvm/Cheerp/EdgeContext.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/PointerAnalyzer.h"
#include "llvm/Cheerp/Utility.h"
#include <string>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <array>

namespace cheerp {

class GlobalDepsAnalyzer;

// This class is responsible for generate unique names for a llvm::Value
// The class is dependent on registerize to work properly
class NameGenerator
{
public:
	/**
	 * This enum represents the builtin functions that we directly write from
	 * the backend
	 */
	enum Builtin
	{
		IMUL = 0,
		FROUND,
		ABS,
		ACOS,
		ASIN,
		ATAN,
		ATAN2,
		CEIL,
		COS,
		EXP,
		FLOOR,
		LOG,
		POW,
		SIN,
		SQRT,
		TAN,
		CLZ32,
		CREATE_CLOSURE,
		CREATE_CLOSURE_SPLIT,
		CREATE_POINTER_ARRAY,
		LABEL,
		STACKPTR,
		GROW_MEM,
		ASSIGN_HEAPS,
		DUMMY,
		MEMORY,
		HANDLE_VAARG,
		FETCHBUFFER,
		HEAP8,
		HEAP16,
		HEAP32,
		HEAP64,
		HEAPF32,
		HEAPF64,
		END // This is used to get the number of builtins, keep it last
	};
	/**
	 * This initialize the namegenerator by collecting
	 * all the global variable names
	 */
	explicit NameGenerator( const llvm::Module&, const GlobalDepsAnalyzer &, Registerize &, const PointerAnalyzer& PA, LinearMemoryHelper& linearHelper,
		const std::vector<std::string>& reservedNames, bool makeReadableNames = true );

	/**
	 * Return the computed name for the given variable.
	 * This function can be called only if the passed value is not an inlined instruction
	 */
	llvm::StringRef getName(const llvm::Value* v) const
	{
		if(const llvm::Instruction* I = llvm::dyn_cast<llvm::Instruction>(v))
			return getName(I->getParent()->getParent(), registerize.getRegisterId(I, EdgeContext::emptyContext()));
		assert(namemap.count(v) );
		assert(! namemap.at(v).empty() );
		return namemap.at(v);
	}

	llvm::StringRef getName(const llvm::Function* F, uint32_t regId) const
	{
		std::pair<const llvm::Function*, uint32_t> valData = std::make_pair(F, regId);
		assert(regNamemap.count(valData));
		assert(!regNamemap.at(valData).empty());
		return regNamemap.at(valData);
	}

	/**
	 * Some values, such as arguments which are REGULAR pointers needs two names
	 */
	llvm::StringRef getSecondaryName(const llvm::Value* v) const
	{
		if(const llvm::Instruction* I = llvm::dyn_cast<llvm::Instruction>(v))
			return getSecondaryName(I->getParent()->getParent(), registerize.getRegisterId(I, EdgeContext::emptyContext()));
		assert(secondaryNamemap.count(v) );
		assert(!secondaryNamemap.at(v).empty());
		return secondaryNamemap.at(v);
	}

	llvm::StringRef getSecondaryName(const llvm::Function* F, uint32_t regId) const
	{
		std::pair<const llvm::Function*, uint32_t> valData = std::make_pair(F, regId);
		assert(regSecondaryNamemap.count(valData));
		assert(!regSecondaryNamemap.at(valData).empty());
		return regSecondaryNamemap.at(valData);
	}

	/**
	 * Return a JS compatible name for the StructType constructor, potentially minimized
	 * A name is guaranteed also for literal structs which have otherwise no name
	 */
	llvm::StringRef getClassName(llvm::Type* T) const
	{
		assert(classmap.count(T));
		return classmap.at(T);
	}
	/**
	 * Return a JS compatible name for the StructType class, potentially minimized
	 * A name is guaranteed also for literal structs which have otherwise no name
	 */
	llvm::StringRef getConstructorName(llvm::Type* T) const
	{
		assert(constructormap.count(T));
		return constructormap.at(T);
	}
	/**
	 * Return a JS compatible name for the StructType array, potentially minimized
	 * A name is guaranteed also for literal structs which have otherwise no name
	 */
	llvm::StringRef getArrayName(llvm::Type* T) const
	{
		assert(arraymap.count(T));
		return arraymap.at(T);
	}
	llvm::StringRef getArrayResizeName(llvm::Type* T) const
	{
		assert(resizemap.count(T));
		return resizemap.at(T);
	}
	/**
	 * Return a name for the requested builtin, potentially minimized
	 */
	llvm::StringRef getBuiltinName(Builtin b) const
	{
		return builtins.at(b);
	}

	/**
	 * Same as getName, but supports the required temporary variables in edges between blocks
	 * It uses the current edge context.
	*/
	llvm::StringRef getNameForEdge(const llvm::Value* v, const EdgeContext& edgeContext) const;
	llvm::StringRef getSecondaryNameForEdge(const llvm::Value* v, const EdgeContext& edgeContext) const;
	std::string getShortestLocalName() const
	{
		if (shortestLocalName.size() == 0)
		{
			//TODO: add a solution even if there are no locals AND no arguments
			llvm_unreachable("There are no locals");
		}
		return shortestLocalName;
	}

	enum NAME_FILTER_MODE { GLOBAL = 0, GLOBAL_SECONDARY, LOCAL, LOCAL_SECONDARY };
	// Filter the original string so that it no longer contains invalid JS characters.
	static llvm::SmallString<4> filterLLVMName( llvm::StringRef, NAME_FILTER_MODE filterMode );

	// Determine if an instruction actually needs a name
	bool needsName(const llvm::Instruction &, const PointerAnalyzer& PA) const;

private:
	void assignLocalName(llvm::StringRef name)
	{
		if (shortestLocalName.size() == 0 || name.size() < shortestLocalName.size())
			shortestLocalName = std::string(name);
	}
	void generateCompressedNames( const llvm::Module& M, const GlobalDepsAnalyzer &, LinearMemoryHelper& linearHelper);
	void generateReadableNames( const llvm::Module& M, const GlobalDepsAnalyzer &, LinearMemoryHelper& linearHelper );

	Registerize& registerize;
	const PointerAnalyzer& PA;
	std::unordered_map<const llvm::Value*, llvm::SmallString<4> > namemap;
	std::unordered_map<const llvm::Value*, llvm::SmallString<4> > secondaryNamemap;
	std::unordered_map<std::pair<const llvm::Function*, uint32_t>, llvm::SmallString<4>, PairHash<const llvm::Function*, uint32_t> > regNamemap;
	std::unordered_map<std::pair<const llvm::Function*, uint32_t>, llvm::SmallString<4>, PairHash<const llvm::Function*, uint32_t> > regSecondaryNamemap;
	std::unordered_map<llvm::Type*, llvm::SmallString<4> > classmap;
	std::unordered_map<llvm::Type*, llvm::SmallString<4> > constructormap;
	std::unordered_map<llvm::Type*, llvm::SmallString<4> > arraymap;
	std::unordered_map<llvm::Type*, llvm::SmallString<4> > resizemap;
	std::array<llvm::SmallString<4>, Builtin::END> builtins;
	const std::vector<std::string> reservedNames;
	std::string shortestLocalName;
};

std::vector<std::string> buildReservedNamesList(const llvm::Module& M, const std::vector<std::string>& fromOption);
std::vector<std::string> buildJsExportedNamesList(const llvm::Module& M);
std::unordered_set<std::string> buildAsmClobberNamesList(const llvm::Module& M);

}

#endif //_CHEERP_NAME_GENERATOR_H
