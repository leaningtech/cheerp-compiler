//===-- Cheerp/NameGenerator.h - Cheerp name generator code ----------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2018 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_NAME_GENERATOR_H
#define _CHEERP_NAME_GENERATOR_H

#include "llvm/ADT/SmallString.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/PointerAnalyzer.h"
#include "llvm/Cheerp/Utility.h"
#include <unordered_map>
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
		CREATE_POINTER_ARRAY,
		HANDLE_VAARG,
		HEAP8,
		HEAP16,
		HEAP32,
		HEAPF32,
		HEAPF64,
		END // This is used to get the number of builtins, keep it last
	};
	/**
	 * This initialize the namegenerator by collecting
	 * all the global variable names
	 */
	explicit NameGenerator( const llvm::Module&, const GlobalDepsAnalyzer &, Registerize &, const PointerAnalyzer& PA,
		const std::vector<std::string>& reservedNames, bool makeReadableNames = true );

	/**
	 * Return the computed name for the given variable.
	 * This function can be called only if the passed value is not an inlined instruction
	 */
	llvm::StringRef getName(const llvm::Value* v) const
	{
		if(const llvm::Instruction* I = llvm::dyn_cast<llvm::Instruction>(v))
			return getName(I->getParent()->getParent(), registerize.getRegisterId(I));
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
			return getSecondaryName(I->getParent()->getParent(), registerize.getRegisterId(I));
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
	llvm::StringRef getNameForEdge(const llvm::Value* v, const llvm::BasicBlock* fromBB, const llvm::BasicBlock* toBB) const;
	llvm::StringRef getSecondaryNameForEdge(const llvm::Value* v, const llvm::BasicBlock* fromBB, const llvm::BasicBlock* toBB) const;

	enum NAME_FILTER_MODE { GLOBAL = 0, GLOBAL_SECONDARY, LOCAL, LOCAL_SECONDARY };
	// Filter the original string so that it no longer contains invalid JS characters.
	static llvm::SmallString<4> filterLLVMName( llvm::StringRef, NAME_FILTER_MODE filterMode );

	// Determine if an instruction actually needs a name
	bool needsName(const llvm::Instruction &, const PointerAnalyzer& PA) const;

private:
	void generateCompressedNames( const llvm::Module& M, const GlobalDepsAnalyzer & );
	void generateReadableNames( const llvm::Module& M, const GlobalDepsAnalyzer & );
	static std::vector<std::string> buildReservedNamesList(const llvm::Module& M, const std::vector<std::string>& fromOption);
	
	Registerize& registerize;
	const PointerAnalyzer& PA;
	std::unordered_map<const llvm::Value*, llvm::SmallString<4> > namemap;
	std::unordered_map<const llvm::Value*, llvm::SmallString<4> > secondaryNamemap;
	std::unordered_map<std::pair<const llvm::Function*, uint32_t>, llvm::SmallString<4>, PairHash<const llvm::Function*, uint32_t> > regNamemap;
	std::unordered_map<std::pair<const llvm::Function*, uint32_t>, llvm::SmallString<4>, PairHash<const llvm::Function*, uint32_t> > regSecondaryNamemap;
	std::unordered_map<llvm::Type*, llvm::SmallString<4> > classmap;
	std::unordered_map<llvm::Type*, llvm::SmallString<4> > constructormap;
	std::unordered_map<llvm::Type*, llvm::SmallString<4> > arraymap;
	std::array<llvm::SmallString<4>, Builtin::END> builtins;
	const std::vector<std::string> reservedNames;
};

}

#endif //_CHEERP_NAME_GENERATOR_H
