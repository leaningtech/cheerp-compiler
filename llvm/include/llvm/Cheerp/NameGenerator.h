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
		END // This is used to get the number of builtins, keep it last
	};
	/**
	 * This initialize the namegenerator by collecting
	 * all the global variable names
	 */
	explicit NameGenerator( const llvm::Module&, const GlobalDepsAnalyzer &, const Registerize &, const PointerAnalyzer& PA,
		const std::vector<std::string>& reservedNames, bool makeReadableNames = true );

	/**
	 * Return the computed name for the given variable.
	 * This function can be called only if the passed value is not an inlined instruction
	 */
	llvm::StringRef getName(const llvm::Value* v) const
	{
		if(!edgeContext.isNull())
			return getNameForEdge(v);
		if(const llvm::Instruction* I = llvm::dyn_cast<llvm::Instruction>(v))
		{
			std::pair<const llvm::Function*, uint32_t> valData = std::make_pair(I->getParent()->getParent(), registerize.getRegisterId(I));
			assert(regNamemap.count(valData));
			assert(!regNamemap.at(valData).empty());
			return regNamemap.at(valData);
		}
		assert(namemap.count(v) );
		assert(! namemap.at(v).empty() );
		return namemap.at(v);
	}

	/**
	 * Some values, such as arguments which are REGULAR pointers needs two names
	 */
	llvm::StringRef getSecondaryName(const llvm::Value* v) const
	{
		if(!edgeContext.isNull())
			return getSecondaryNameForEdge(v);
		if(const llvm::Instruction* I = llvm::dyn_cast<llvm::Instruction>(v))
		{
			std::pair<const llvm::Function*, uint32_t> valData = std::make_pair(I->getParent()->getParent(), registerize.getRegisterId(I));
			assert(regSecondaryNamemap.count(valData));
			assert(!regSecondaryNamemap.at(valData).empty());
			return regSecondaryNamemap.at(valData);
		}
		assert(secondaryNamemap.count(v) );
		assert(!secondaryNamemap.at(v).empty());
		return secondaryNamemap.at(v);
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
	llvm::StringRef getNameForEdge(const llvm::Value* v) const;
	llvm::StringRef getSecondaryNameForEdge(const llvm::Value* v) const;

	void setEdgeContext(const llvm::BasicBlock* fromBB, const llvm::BasicBlock* toBB)
	{
		assert(edgeContext.isNull());
		edgeContext.fromBB=fromBB;
		edgeContext.toBB=toBB;
	}

	void clearEdgeContext()
	{
		edgeContext.clear();
	}

	enum NAME_FILTER_MODE { GLOBAL = 0, GLOBAL_SECONDARY, LOCAL, LOCAL_SECONDARY };
	// Filter the original string so that it no longer contains invalid JS characters.
	static llvm::SmallString<4> filterLLVMName( llvm::StringRef, NAME_FILTER_MODE filterMode );

	// Determine if an instruction actually needs a name
	bool needsName(const llvm::Instruction &, const PointerAnalyzer& PA) const;
	bool needsSecondaryName(const llvm::Value*, const PointerAnalyzer& PA) const;

private:
	void generateCompressedNames( const llvm::Module& M, const GlobalDepsAnalyzer & );
	void generateReadableNames( const llvm::Module& M, const GlobalDepsAnalyzer & );
	
	const Registerize& registerize;
	const PointerAnalyzer& PA;
	std::unordered_map<const llvm::Value*, llvm::SmallString<4> > namemap;
	std::unordered_map<const llvm::Value*, llvm::SmallString<4> > secondaryNamemap;
	std::unordered_map<std::pair<const llvm::Function*, uint32_t>, llvm::SmallString<4>, PairHash<const llvm::Function*, uint32_t> > regNamemap;
	std::unordered_map<std::pair<const llvm::Function*, uint32_t>, llvm::SmallString<4>, PairHash<const llvm::Function*, uint32_t> > regSecondaryNamemap;
	std::unordered_map<llvm::Type*, llvm::SmallString<4> > classmap;
	std::unordered_map<llvm::Type*, llvm::SmallString<4> > constructormap;
	std::unordered_map<llvm::Type*, llvm::SmallString<4> > arraymap;
	std::array<llvm::SmallString<4>, Builtin::END> builtins;
	struct InstOnEdge
	{
		const llvm::BasicBlock* fromBB;
		const llvm::BasicBlock* toBB;
		uint32_t registerId;
		InstOnEdge(const llvm::BasicBlock* f, const llvm::BasicBlock* t, uint32_t r):fromBB(f),toBB(t),registerId(r)
		{
		}
		bool operator==(const InstOnEdge& r) const
		{
			return fromBB==r.fromBB && toBB==r.toBB && registerId==r.registerId;
		}
		struct Hash
		{
			size_t operator()(const InstOnEdge& i) const
			{
				return std::hash<const llvm::BasicBlock*>()(i.fromBB) ^
					std::hash<const llvm::BasicBlock*>()(i.toBB) ^
					std::hash<uint32_t>()(i.registerId);
			}
		};
	};
	typedef std::unordered_map<InstOnEdge, llvm::SmallString<8>, InstOnEdge::Hash > EdgeNameMapTy;
	EdgeNameMapTy edgeNamemap;
	EdgeNameMapTy edgeSecondaryNamemap;
	struct EdgeContext
	{
		const llvm::BasicBlock* fromBB;
		const llvm::BasicBlock* toBB;
		EdgeContext():fromBB(NULL), toBB(NULL)
		{
		}
		bool isNull() const
		{
			return fromBB==NULL;
		}
		void clear()
		{
			fromBB=NULL;
			toBB=NULL;
		}
	};
	EdgeContext edgeContext;
	const std::vector<std::string>& reservedNames;
};

}

#endif //_CHEERP_NAME_GENERATOR_H
