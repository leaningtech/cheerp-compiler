//===-- Cheerp/PointerAnalyzer.h - Cheerp pointer analyzer code --------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2015 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_POINTER_ANALYZER_H
#define _CHEERP_POINTER_ANALYZER_H

#include "llvm/Pass.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Constants.h"
#include "llvm/Support/Timer.h"
#include <unordered_map>
#include <unordered_set>

namespace cheerp {

enum POINTER_KIND {
	COMPLETE_OBJECT,  // object with aN, iN and dN fields.
	SPLIT_REGULAR,    // two local vars "X" and "Xo".
	REGULAR,          // pointer object {o: ..., d: ...}.
	BYTE_LAYOUT,      // slow, and mostly used for unions.
	UNKNOWN,
	INDIRECT,
	RAW               // very fast, and used in asm.js mode.
};

enum REGULAR_POINTER_PREFERENCE { PREF_NONE, PREF_SPLIT_REGULAR, PREF_REGULAR };

struct TypeAndIndex
{
	llvm::Type* type;
	uint32_t index;
	enum CANONICALIZE_TYPE { STRUCT_MEMBER = 0, ARGUMENT };
	TypeAndIndex(llvm::Type* t, uint32_t i, CANONICALIZE_TYPE c):type(t),index(i)
	{
		if(!t)
			return;
		assert(c == STRUCT_MEMBER || c == ARGUMENT);
		if (c == STRUCT_MEMBER)
		{
			// Find if a direct base is the actual owner of the field
			if(llvm::StructType* st=llvm::dyn_cast<llvm::StructType>(t))
			{
				while(st->getDirectBase() && st->getDirectBase()->getNumElements() > i)
					st = st->getDirectBase();
				type = st;
			}
		}
		else if(c == ARGUMENT)
		{
			// Collapse every argument to the least derived one
			if(llvm::StructType* st=llvm::dyn_cast<llvm::StructType>(t))
			{
				while(st->getDirectBase())
					st = st->getDirectBase();
				type = st;
			}
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

enum INDIRECT_POINTER_KIND_CONSTRAINT { RETURN_CONSTRAINT, DIRECT_ARG_CONSTRAINT, STORED_TYPE_CONSTRAINT, RETURN_TYPE_CONSTRAINT, BASE_AND_INDEX_CONSTRAINT,
	INDIRECT_ARG_CONSTRAINT, DIRECT_ARG_CONSTRAINT_IF_ADDRESS_TAKEN };

struct IndirectPointerKindConstraint
{
	union
	{
		const llvm::Function* funcPtr;
		llvm::Type* typePtr;
		const llvm::Argument* argPtr;
		const void* ptr;
	};
	uint32_t i;
	uint8_t kind;
	mutable bool isBeingVisited;
	IndirectPointerKindConstraint(INDIRECT_POINTER_KIND_CONSTRAINT k, const void* p):ptr(p),i(0xffffffff),kind(k),isBeingVisited(false)
	{
		assert(k == RETURN_CONSTRAINT || k == DIRECT_ARG_CONSTRAINT || k == STORED_TYPE_CONSTRAINT ||
			k == RETURN_TYPE_CONSTRAINT || k == DIRECT_ARG_CONSTRAINT_IF_ADDRESS_TAKEN);
	}
	IndirectPointerKindConstraint(INDIRECT_POINTER_KIND_CONSTRAINT k, const TypeAndIndex& typeAndIndex):ptr(typeAndIndex.type),i(typeAndIndex.index),
														kind(k),isBeingVisited(false)
	{
		assert(k == BASE_AND_INDEX_CONSTRAINT || k == INDIRECT_ARG_CONSTRAINT);
	}
	bool operator==(const IndirectPointerKindConstraint& rhs) const
	{
		return kind == rhs.kind && ptr == rhs.ptr && i == rhs.i;
	}
	void dump() const;
	struct Hash
	{
		size_t operator()(const IndirectPointerKindConstraint& c) const
		{
			return std::hash<uint32_t>()((uint32_t)c.kind) ^ std::hash<const void*>()(c.ptr) ^ std::hash<uint32_t>()(c.i);
		}
	};
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
	// We can store pointers to constraint as they are made unique by PointerData::getConstraintPtr
	llvm::DenseSet<const IndirectPointerKindConstraint*> constraints;
	PointerKindWrapper():kind(COMPLETE_OBJECT),regularCause(NULL)
	{
	}
	PointerKindWrapper(POINTER_KIND k, const llvm::Value* regularCause = NULL):kind(k),regularCause(regularCause)
	{
		assert((k!=REGULAR && k!=SPLIT_REGULAR) || regularCause);
	}
	PointerKindWrapper(const IndirectPointerKindConstraint* constraint):kind(INDIRECT),regularCause(NULL)
	{
		constraints.insert(constraint);
	}
	PointerKindWrapper(const PointerKindWrapper& rhs):kind(rhs.kind),constraints(rhs.constraints),regularCause(rhs.regularCause)
	{
		assert(this != &rhs);
	}
	void swap(PointerKindWrapper& rhs)
	{
		std::swap(kind, rhs.kind);
		constraints.swap(rhs.constraints);
		std::swap(regularCause, rhs.regularCause);
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
		regularCause = rhs.regularCause;
		return *this;
	}
	PointerKindWrapper& operator|=(const PointerKindWrapper& rhs);
	PointerKindWrapper& operator|=(const IndirectPointerKindConstraint* rhs);
	bool isKnown() const
	{
		return kind!=UNKNOWN;
	}
	POINTER_KIND getPointerKind(REGULAR_POINTER_PREFERENCE p) const
	{
		if(kind==REGULAR && p==PREF_SPLIT_REGULAR)
			return SPLIT_REGULAR;
		else if(kind==SPLIT_REGULAR && p==PREF_REGULAR)
			return REGULAR;
		else
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
	void applyRegularPreference(REGULAR_POINTER_PREFERENCE p)
	{
		kind = getPointerKind(p);
	}
	void makeKnown()
	{
		kind = getPointerKindForKnown();
	}
	void dump() const;
	const llvm::Value* regularCause;

	static PointerKindWrapper staticDefaultValue;
};

class PointerConstantOffsetWrapper
{
public:
	enum STATUS { UNINITALIZED = 0, VALID, INVALID, UNKNOWN };
private:
	const llvm::ConstantInt* offset;
	STATUS status;
	void clearConstraints()
	{
		constraints.clear();
	}
public:
	std::unordered_set<const IndirectPointerKindConstraint*> constraints;
	PointerConstantOffsetWrapper():offset(NULL),status(UNINITALIZED)
	{
	}
	PointerConstantOffsetWrapper(STATUS s):offset(NULL),status(s)
	{
	}
	PointerConstantOffsetWrapper(const llvm::ConstantInt* o):offset(o),status(VALID)
	{
		assert(o);
	}
	PointerConstantOffsetWrapper(const IndirectPointerKindConstraint* constraint):offset(NULL),status(UNINITALIZED)
	{
		constraints.insert(constraint);
	}
	void swap(PointerConstantOffsetWrapper& rhs)
	{
		std::swap(offset, rhs.offset);
		std::swap(status, rhs.status);
		constraints.swap(rhs.constraints);
	}
	PointerConstantOffsetWrapper& operator|=(const PointerConstantOffsetWrapper& rhs);
	PointerConstantOffsetWrapper& operator|=(const IndirectPointerKindConstraint* rhs);
	PointerConstantOffsetWrapper& operator|=(const llvm::ConstantInt* rhs);
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
	bool isUnknown() const
	{
		return status == UNKNOWN;
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
	void makeKnown()
	{
		if(status == UNKNOWN)
		{
			if(offset == NULL)
				status = UNINITALIZED;
			else
				status = VALID;
		}
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

	void prefetchFunc( const llvm::Function & ) const;
	static char ID;

	void getAnalysisUsage(llvm::AnalysisUsage & AU) const override;

	bool runOnModule( llvm::Module & ) override;

	const char *getPassName() const override;

	POINTER_KIND getPointerKind(const llvm::Value* v) const;
	POINTER_KIND getPointerKindForReturn(const llvm::Function* F) const;
	POINTER_KIND getPointerKindForStoredType( llvm::Type * pointerType ) const;
	POINTER_KIND getPointerKindForMemberPointer( const TypeAndIndex& baseAndIndex ) const;
	POINTER_KIND getPointerKindForMember( const TypeAndIndex& baseAndIndex ) const;
	POINTER_KIND getPointerKindForArgumentTypeAndIndex( const TypeAndIndex& argTypeAndIndex ) const;
	const PointerKindWrapper& getFinalPointerKindWrapper(const llvm::Value* v ) const;
	static TypeAndIndex getBaseStructAndIndexFromGEP( const llvm::Value* v );
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
		typedef std::map<TypeAndIndex, T> TypeAndIndexMap;
		typedef std::unordered_map<IndirectPointerKindConstraint, T, IndirectPointerKindConstraint::Hash> ConstraintsMap;
		ConstraintsMap constraintsMap;
		// Helper function to make constraints unique, they are stored as the key field into constraintsMap
		// and may or may not hold any actual pointer data as the corresponding valiue
		const IndirectPointerKindConstraint* getConstraintPtr(const IndirectPointerKindConstraint& c)
		{
			return &constraintsMap.insert(std::make_pair(c, T())).first->first;
		}

		ValueKindMap valueMap;
		// This map stores constraints about pointer to members
		TypeAndIndexMap baseStructAndIndexMapForMembers;
		ValueKindMap argsMap;
	};

	typedef PointerData<PointerKindWrapper> PointerKindData;
	typedef PointerData<PointerConstantOffsetWrapper> PointerOffsetData;

	static REGULAR_POINTER_PREFERENCE getRegularPreference(const IndirectPointerKindConstraint& c, PointerKindData& pointerKindData, AddressTakenMap& addressTakenCache);
	static POINTER_KIND getPointerKindForMemberImpl(const TypeAndIndex& baseAndIndex, PointerKindData& pointerKindData, AddressTakenMap& addressTakenCache);
private:
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
