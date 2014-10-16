//===-- Cheerp/PointerAnalyzer.h - Cheerp pointer analyzer code --------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2013 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_POINTER_ANALYZER_H
#define _CHEERP_POINTER_ANALYZER_H

#include "llvm/Pass.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/Timer.h"

namespace cheerp {

enum POINTER_KIND {
	COMPLETE_OBJECT,
	REGULAR,
	BYTE_LAYOUT,
	LAST_POINTER_KIND=BYTE_LAYOUT
};

class PointerKindWrapper
{
private:
	uint32_t kind;
	bool hasConstraints() const
	{
		return !(returnConstraints.empty() && argsConstraints.empty() && storedTypeConstraints.empty());
	}
	void clearConstraints()
	{
		returnConstraints.clear();
		argsConstraints.clear();
		storedTypeConstraints.clear();
	}
public:
	enum WRAPPED_POINTER_KIND { UNKNOWN=LAST_POINTER_KIND+1, INDIRECT=LAST_POINTER_KIND+2 };
	std::vector<const llvm::Function*> returnConstraints;
	std::vector<std::pair<const llvm::Function*, uint32_t>> argsConstraints;
	std::vector<llvm::Type*> storedTypeConstraints;
	PointerKindWrapper():kind(COMPLETE_OBJECT)
	{
	}
	PointerKindWrapper(uint32_t k):kind(k)
	{
	}
	PointerKindWrapper(const llvm::Function* f):kind(INDIRECT)
	{
		returnConstraints.push_back(f);
	}
	PointerKindWrapper(const llvm::Function* f, uint32_t arg):kind(INDIRECT)
	{
		argsConstraints.push_back(std::make_pair(f, arg));
	}
	PointerKindWrapper(llvm::Type* t):kind(INDIRECT)
	{
		storedTypeConstraints.push_back(t);
	}
	bool operator==(uint32_t rhs) const
	{
		return kind==rhs;
	}
	bool operator!=(uint32_t rhs) const
	{
		return kind!=rhs;
	}
	PointerKindWrapper operator|(const PointerKindWrapper & rhs);
	PointerKindWrapper& operator|=(const PointerKindWrapper& rhs);
	bool isKnown() const
	{
		return kind!=UNKNOWN;
	}
	explicit operator POINTER_KIND() const
	{
		return (POINTER_KIND)kind;
	}
	void makeKnown()
	{
		//If all the uses are unknown no use is REGULAR, we can return CO
		if(kind!=UNKNOWN)
			return;
		if(hasConstraints())
			kind = INDIRECT;
		else
			kind = COMPLETE_OBJECT;
	}
	void dump() const;
};

class PointerAnalyzer : public llvm::ModulePass
{
public:
	PointerAnalyzer() : 
		ModulePass(ID)
#ifndef NDEBUG
		,fullyResolved(false),
		timerGroup("Pointer Analyzer"),
		gpkTimer("getPointerKind",timerGroup),
		gpkfrTimer("getPointerKindForReturn",timerGroup)
#endif //NDEBUG
	{}

	void prefetch( const llvm::Module & ) const;
	static char ID;

	bool runOnModule( llvm::Module & ) override;

	const char *getPassName() const override;

	POINTER_KIND getPointerKind(const llvm::Value* v) const;
	POINTER_KIND getPointerKindForReturn(const llvm::Function* F) const;
	POINTER_KIND getPointerKindForStoredType( llvm::Type * pointerType ) const;
	POINTER_KIND getPointerKindForArgumentType( llvm::Type * pointerType ) const;
	PointerKindWrapper getFinalPointerKindWrapper(const llvm::Value* v ) const;
	PointerKindWrapper getFinalPointerKindWrapperForReturn(const llvm::Function* F) const;

	/**
	 * Functions to manually invalidate the cache
	 */

	// Notify that a value has been invalidated
	void invalidate( const llvm::Value * );

	// Fully resolve indirect pointer kinds. After you call this function you should not call invalidate anymore.
	void fullResolve() const;

#ifndef NDEBUG
	mutable bool fullyResolved;
	// Dump a pointer value info
	void dumpPointer(const llvm::Value * v, bool dumpOwnerFuncion = true) const;
#endif //NDEBUG

	typedef llvm::DenseMap<const llvm::Value*, PointerKindWrapper> ValueKindMap;
	typedef llvm::DenseMap<llvm::Type*, PointerKindWrapper> TypeKindMap;
	struct AddressTakenMap: public llvm::DenseMap<const llvm::Function*, bool>
	{
		bool checkAddressTaken(const llvm::Function* F)
		{
			auto it=find(F);
			if(it==end())
			{
				bool ret=F->hasAddressTaken();
				insert(std::make_pair(F, ret));
				return ret;
			}
			else
				return it->second;
		}
	};
	struct CacheBundle
	{
		ValueKindMap valueCache;
		TypeKindMap typeCache;
		AddressTakenMap addressTakenCache;
	};

private:

	mutable CacheBundle cacheBundle;

#ifndef NDEBUG
	mutable llvm::TimerGroup timerGroup;
	mutable llvm::Timer gpkTimer, gpkfrTimer;
#endif //NDEBUG
};

#ifndef NDEBUG

void dumpAllPointers(const llvm::Function &, const PointerAnalyzer & );
void writePointerDumpHeader();

#endif //NDEBUG

inline llvm::Pass * createPointerAnalyzerPass()
{
	return new PointerAnalyzer;
}

}

#endif //_CHEERP_POINTER_ANALYZER_H
