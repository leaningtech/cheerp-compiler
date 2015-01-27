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
#include "llvm/IR/Constants.h"
#include "llvm/Support/Timer.h"

namespace cheerp {

enum POINTER_KIND {
	COMPLETE_OBJECT,
	REGULAR,
	BYTE_LAYOUT,
	UNKNOWN,
	INDIRECT
};

enum INDIRECT_POINTER_KIND_CONSTRAINT { RETURN_CONSTRAINT, DIRECT_ARG_CONSTRAINT, STORED_TYPE_CONSTRAINT, RETURN_TYPE_CONSTRAINT, BASE_AND_INDEX_CONSTRAINT};

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
	bool operator==(POINTER_KIND rhs) const
	{
		return kind==rhs;
	}
	bool operator!=(POINTER_KIND rhs) const
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

	static PointerKindWrapper staticDefaultValue;
};

class PointerConstantOffsetWrapper
{
public:
	enum STATUS { UNINITALIZED = 0, VALID, INVALID };
private:
	const llvm::ConstantInt* offset;
	STATUS status;
	void clearConstraints()
	{
		constraints.clear();
	}
public:
	std::vector<IndirectPointerKindConstraint> constraints;
	PointerConstantOffsetWrapper():offset(NULL),status(UNINITALIZED)
	{
	}
	PointerConstantOffsetWrapper(const llvm::ConstantInt* o, STATUS s = VALID):offset(o),status(s)
	{
		if(o == NULL && s == VALID)
			status = INVALID;
	}
	PointerConstantOffsetWrapper(INDIRECT_POINTER_KIND_CONSTRAINT constraint, const void* ptr, uint32_t i=0):offset(NULL),status(UNINITALIZED)
	{
		constraints.emplace_back(constraint, ptr, i);
	}
	PointerConstantOffsetWrapper& operator|=(const PointerConstantOffsetWrapper& rhs);
	bool isInvalid() const
	{
		return status == INVALID;
	}
	bool isValid() const
	{
		return status == VALID;
	}
	bool isUninitialized() const
	{
		return status == UNINITALIZED;
	}
	bool hasConstraints() const
	{
		return !constraints.empty();
	}
	const llvm::ConstantInt* getPointerOffset() const
	{
		assert(status == VALID);
		return offset;
	}
	void dump() const;

	static PointerConstantOffsetWrapper staticDefaultValue;
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

	struct TypeAndIndex
	{
		llvm::Type* type;
		uint32_t index;
		TypeAndIndex(llvm::Type* t, uint32_t i):type(t),index(i)
		{
			if(!t)
				return;
			// Find if a direct base is the actual owner of the field
			if(llvm::StructType* st=llvm::dyn_cast<llvm::StructType>(t))
			{
				while(st->getDirectBase() && st->getDirectBase()->getNumElements() > i)
					st = st->getDirectBase();
				type = st;
			}
		}
		bool operator<(const TypeAndIndex& rhs) const
		{
			if(type==rhs.type)
				return index < rhs.index;
			else
				return type < rhs.type;
		}
		operator bool() const
		{
			return type != NULL;
		}
	};

	void prefetchFunc( const llvm::Function & ) const;
	static char ID;

	void getAnalysisUsage(llvm::AnalysisUsage & AU) const;

	bool runOnModule( llvm::Module & ) override;

	const char *getPassName() const override;

	POINTER_KIND getPointerKind(const llvm::Value* v) const;
	POINTER_KIND getPointerKindForReturn(const llvm::Function* F) const;
	POINTER_KIND getPointerKindForStoredType( llvm::Type * pointerType ) const;
	POINTER_KIND getPointerKindForArgumentType( llvm::Type * pointerType ) const;
	POINTER_KIND getPointerKindForMemberPointer( const TypeAndIndex& baseAndIndex ) const;
	POINTER_KIND getPointerKindForMember( const TypeAndIndex& baseAndIndex ) const;
	const PointerKindWrapper& getFinalPointerKindWrapper(const llvm::Value* v ) const;
	static TypeAndIndex getBaseStructAndIndexFromGEP( const llvm::Value* v );
	static bool hasNonLoadStoreUses ( const llvm::Value* v );
	const llvm::ConstantInt* getConstantOffsetForPointer( const llvm::Value* ) const;
	const llvm::ConstantInt* getConstantOffsetForMember( const TypeAndIndex& baseAndIndex ) const;

	/**
	 * Functions to manually invalidate the cache
	 */

	// Notify that a value has been invalidated
	void invalidate( const llvm::Value * );

	// Fully resolve indirect pointer kinds. After you call this function you should not call invalidate anymore.
	void fullResolve();
	// Compute all the offsets for REGULAR pointer which may be assumed constant
	void computeConstantOffsets(const llvm::Module& M );

#ifndef NDEBUG
	mutable bool fullyResolved;
	// Dump a pointer value info
	void dumpPointer(const llvm::Value * v, bool dumpOwnerFuncion = true) const;
#endif //NDEBUG
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
	template<class T>
	struct PointerData
	{
		typedef llvm::DenseMap<const llvm::Value*, T> ValueKindMap;
		typedef llvm::DenseMap<llvm::Type*, T> TypeKindMap;
		typedef llvm::DenseMap<llvm::Type*, T> ReturnTypeKindMap;
		typedef std::map<TypeAndIndex, T> TypeAndIndexMap;
		ValueKindMap valueMap;
		TypeKindMap storedTypeMap;
		// This map stores constraints about pointer to members
		TypeAndIndexMap baseStructAndIndexMapForMembers;
		// This map stores constraints about pointers stored and loaded from a member which is a pointer
		TypeAndIndexMap baseStructAndIndexMapForPointers;
		ReturnTypeKindMap returnTypeMap;
	};

	typedef PointerData<PointerKindWrapper> PointerKindData;
	typedef PointerData<PointerConstantOffsetWrapper> PointerOffsetData;
private:
	const PointerKindWrapper& getFinalPointerKindWrapperForReturn(const llvm::Function* F) const;
	const PointerConstantOffsetWrapper& getFinalPointerConstantOffsetWrapper(const llvm::Value*) const;
	mutable PointerKindData pointerKindData;
	mutable PointerOffsetData pointerOffsetData;
	mutable AddressTakenMap addressTakenCache;

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
