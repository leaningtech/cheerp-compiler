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
	UNKNOWN,
	INDIRECT
};

enum INDIRECT_POINTER_KIND_CONSTRAINT { RETURN_CONSTRAINT, DIRECT_ARG_CONSTRAINT, STORED_TYPE_CONSTRAINT, RETURN_TYPE_CONSTRAINT};

struct IndirectPointerKindConstraint
{
	INDIRECT_POINTER_KIND_CONSTRAINT kind;
	union
	{
		const llvm::Function* funcPtr;
		llvm::Type* typePtr;
		const void* ptr;
	};
	uint32_t i;
	IndirectPointerKindConstraint(INDIRECT_POINTER_KIND_CONSTRAINT k, const void* p, uint32_t i):kind(k),ptr(p),i(i)
	{
	}
	bool operator<(const IndirectPointerKindConstraint& rhs) const
	{
		if (kind == rhs.kind)
		{
			if(ptr == rhs.ptr)
				return i < rhs.i;
			else
				return ptr < rhs.ptr;
		}
		else
			return kind < rhs.kind;
	}
	void dump() const;
};

class PointerKindWrapper
{
private:
	POINTER_KIND kind;
	bool hasConstraints() const
	{
		return !constraints.empty();
	}
	void clearConstraints()
	{
		constraints.clear();
	}
public:
	std::vector<IndirectPointerKindConstraint> constraints;
	PointerKindWrapper():kind(COMPLETE_OBJECT)
	{
	}
	PointerKindWrapper(POINTER_KIND k):kind(k)
	{
	}
	PointerKindWrapper(INDIRECT_POINTER_KIND_CONSTRAINT constraint, const void* ptr, uint32_t i=0):kind(INDIRECT)
	{
		constraints.emplace_back(constraint, ptr, i);
	}
	bool operator==(uint32_t rhs) const
	{
		return kind==rhs;
	}
	bool operator!=(uint32_t rhs) const
	{
		return kind!=rhs;
	}
	PointerKindWrapper& operator=(const PointerKindWrapper& rhs)
	{
		assert(this != &rhs);
		kind = rhs.kind;
		constraints = rhs.constraints;
		return *this;
	}
	PointerKindWrapper& operator|=(const PointerKindWrapper& rhs);
	bool isKnown() const
	{
		return kind!=UNKNOWN;
	}
	POINTER_KIND getPointerKind() const
	{
		return kind;
	}
	POINTER_KIND getPointerKindForKnown() const
	{
		//If all the uses are unknown, no use is REGULAR and we can return CO
		if(kind!=UNKNOWN)
			return kind;
		if(hasConstraints())
			return INDIRECT;
		else
			return COMPLETE_OBJECT;
	}
	void makeKnown()
	{
		kind = getPointerKindForKnown();
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

	void prefetchFunc( const llvm::Function & ) const;
	static char ID;

	void getAnalysisUsage(llvm::AnalysisUsage & AU) const;

	bool runOnModule( llvm::Module & ) override;

	const char *getPassName() const override;

	POINTER_KIND getPointerKind(const llvm::Value* v) const;
	POINTER_KIND getPointerKindForReturn(const llvm::Function* F) const;
	POINTER_KIND getPointerKindForStoredType( llvm::Type * pointerType ) const;
	POINTER_KIND getPointerKindForArgumentType( llvm::Type * pointerType ) const;
	const PointerKindWrapper& getFinalPointerKindWrapper(const llvm::Value* v ) const;

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
	typedef llvm::DenseMap<llvm::Type*, PointerKindWrapper> ReturnTypeKindMap;
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
	struct PointerKindData
	{
		ValueKindMap valueCache;
		TypeKindMap storedTypeMap;
		ReturnTypeKindMap returnTypeMap;
		AddressTakenMap addressTakenCache;
	};

	static PointerKindWrapper staticCompleteObjectKind;
private:
	const PointerKindWrapper& getFinalPointerKindWrapperForReturn(const llvm::Function* F) const;
	mutable PointerKindData pointerKindData;

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
