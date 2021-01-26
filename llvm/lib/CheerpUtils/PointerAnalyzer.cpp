//===-- PointerAnalyzer.cpp - The Cheerp JavaScript generator -------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2019 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/InitializePasses.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/PointerAnalyzer.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/Debug.h"
#include <numeric>

using namespace llvm;

namespace cheerp {

PointerKindWrapper PointerKindWrapper::staticDefaultValue(COMPLETE_OBJECT);
PointerConstantOffsetWrapper PointerConstantOffsetWrapper::staticDefaultValue(PointerConstantOffsetWrapper::INVALID);

void IndirectPointerKindConstraint::dump() const
{
	switch(kind)
	{
		case RETURN_CONSTRAINT:
			dbgs() << "\tDepends on return value of: " << funcPtr->getName() << "\n";
			break;
		case DIRECT_ARG_CONSTRAINT:
			dbgs() << "\tDepends on argument " << argPtr->getArgNo() << " of " << argPtr->getParent()->getName() << "\n";
			break;
		case STORED_TYPE_CONSTRAINT:
			dbgs() << "Depends on stored type " << *typePtr << "\n";
			break;
		case RETURN_TYPE_CONSTRAINT:
			dbgs() << "Depends on returned type " << *typePtr << "\n";
			break;
		case BASE_AND_INDEX_CONSTRAINT:
			dbgs() << "Depends on index " << i << " of struct " << *typePtr << "\n";
			break;
		case INDIRECT_ARG_CONSTRAINT:
			dbgs() << "Depends on argument " << i << " of type pointer to " << *typePtr << "\n";
			break;
		case DIRECT_ARG_CONSTRAINT_IF_ADDRESS_TAKEN:
			dbgs() << "\tDepends on argument " << argPtr->getArgNo() << " of " << argPtr->getParent()->getName() << ", if it is used indirectly\n";
			break;
	}
}

PointerKindWrapper& PointerKindWrapper::operator|=(const PointerKindWrapper& rhs)
{
	// 1) REGULAR | Any = REGULAR
	// 2) SPLIT_REGULAR | Any = SPLIT_REGULAR
	// 3) COMPLETE_OBJECT | Any = Any
	// 4) UNKNOWN | INDIRECT = UNKNOWN with constraints
	// 5) UNKNOWN | UNKNOWN = UNKNOWN with all constraints
	// 6) INDIRECT | INDIRECT = INDIRECT with all constraints
	PointerKindWrapper& lhs=*this;

	assert(lhs!=BYTE_LAYOUT && rhs!=BYTE_LAYOUT);
	assert(lhs!=RAW && rhs!=RAW);
	assert(lhs!=CONSTANT && rhs!=CONSTANT);
	
	// Handle 1
	if (lhs==REGULAR || rhs==REGULAR)
	{
		lhs.kind = REGULAR;
		lhs.clearConstraints();
		if(rhs.regularCause)
			lhs.regularCause = rhs.regularCause;
		return lhs;
	}

	// Handle 2
	if (lhs==SPLIT_REGULAR || rhs==SPLIT_REGULAR)
	{
		lhs.kind = SPLIT_REGULAR;
		lhs.clearConstraints();
		if(rhs.regularCause)
			lhs.regularCause = rhs.regularCause;
		return lhs;
	}

	// Handle 3
	if (lhs==COMPLETE_OBJECT)
		return *this = rhs;
	if (rhs==COMPLETE_OBJECT)
		return *this;

	// Handle 4, 5, 6
	if (lhs==UNKNOWN || rhs==UNKNOWN)
		lhs.kind = UNKNOWN;

	lhs.constraints.insert(rhs.constraints.begin(), rhs.constraints.end());
	return *this;
}

PointerKindWrapper& PointerKindWrapper::operator|=(const IndirectPointerKindConstraint* rhs)
{
	// 1) REGULAR | Rhs = REGULAR
	// 2) SPLIT_REGULAR | Rhs = REGULAR
	// 3) COMPLETE_OBJECT | Rhs = Rhs
	// 4) UNKNOWN | Rhs = UNKNOWN with Rhs
	// 5) INDIRECT | Rhs = INDIRECT with Rhs
	PointerKindWrapper& lhs=*this;

	assert(lhs!=BYTE_LAYOUT);
	assert(lhs!=RAW);
	assert(lhs!=CONSTANT);

	// Handle 1
	if (lhs==REGULAR)
		return lhs;

	// Handle 2
	if (lhs==SPLIT_REGULAR)
		return lhs;

	// Handle 3
	if (lhs==COMPLETE_OBJECT)
	{
		lhs.kind = INDIRECT;
		assert(lhs.constraints.empty());
	}

	// Handle 4, 5
	lhs.constraints.insert(rhs);
	return *this;
}

void PointerKindWrapper::dump() const
{
	if(kind==UNKNOWN)
		dbgs() << "Unknown kind\n";
	else if(kind==INDIRECT)
		dbgs() << "Indirect kind\n";
	else
		dbgs() << "Wraps plain kind " << int(kind) << "\n";
	if(regularCause)
	{
		dbgs() << "Reason for REGULAR is " << *regularCause;
		if(const Instruction* I = dyn_cast<Instruction>(regularCause))
		{
			dbgs() << " in function " << I->getParent()->getParent()->getName();
		}
		dbgs() << "\n";
	}

	for(const IndirectPointerKindConstraint* c: constraints)
		c->dump();
}

PointerConstantOffsetWrapper& PointerConstantOffsetWrapper::operator|=(const PointerConstantOffsetWrapper & rhs)
{
	PointerConstantOffsetWrapper& lhs=*this;

	if(lhs.status == INVALID || rhs.status == INVALID)
	{
		lhs.clearConstraints();
		lhs.status = INVALID;
		return lhs;
	}

	// Merge the constraints, if any
	if (rhs.hasConstraints())
		lhs.constraints.insert(rhs.constraints.begin(), rhs.constraints.end());
	else
		assert(rhs.status != UNINITALIZED);

	// From now on both offsets can only be UNINITALIZED, VALID or UNKNOWN
	if(rhs.status == UNINITALIZED)
		return lhs;

	if(lhs.status == UNINITALIZED)
	{
		lhs.status = rhs.status;
		lhs.offset = rhs.offset;
		return lhs;
	}

	// Both are VALID or UNKNOWN, check if the are the same constant
	if(rhs.offset != NULL && lhs.offset != NULL)
	{
		if(rhs.offset != lhs.offset)
		{
			lhs.offset = NULL;
			lhs.status = INVALID;
			lhs.clearConstraints();
			return lhs;
		}
	}
	else if(rhs.offset != NULL)
	{
		assert(lhs.status == UNKNOWN);
		assert(lhs.offset == NULL);
		lhs.offset = rhs.offset;
		return lhs;
	}

	// Both are VALID or UNKNOWN, with the same constant or NULL
	if(rhs.status == UNKNOWN)
		lhs.status = UNKNOWN;
	return lhs;
}

PointerConstantOffsetWrapper& PointerConstantOffsetWrapper::operator|=(const IndirectPointerKindConstraint* rhs)
{
	PointerConstantOffsetWrapper& lhs=*this;

	if(lhs.status == INVALID)
		return lhs;

	// Merge the constraint
	lhs.constraints.insert(rhs);
	return lhs;
}

PointerConstantOffsetWrapper& PointerConstantOffsetWrapper::operator|=(const ConstantInt* rhs)
{
	PointerConstantOffsetWrapper& lhs=*this;

	if(lhs.status == INVALID || rhs == NULL)
	{
		lhs.clearConstraints();
		lhs.status = INVALID;
		return lhs;
	}

	assert(rhs);
	if(lhs.status == UNINITALIZED)
	{
		lhs.status = VALID;
		lhs.offset = rhs;
		return lhs;
	}

	// lhs is VALID or UNKNOWN
	if(lhs.status == VALID)
	{
		if(rhs != lhs.offset)
		{
			lhs.offset = NULL;
			lhs.status = INVALID;
			lhs.clearConstraints();
			return lhs;
		}
	}
	lhs.offset = rhs;
	return lhs;
}

void PointerConstantOffsetWrapper::dump() const
{
	if(status==INVALID)
		dbgs() << "Invalid constant offset\n";
	else if(status==UNINITALIZED)
		dbgs() << "Uninitialized constant offset\n";
	else
	{
		if(status==UNKNOWN)
			dbgs() << "Unknown constant offset\n";
		if(offset)
			dbgs() << "Constant offset " << *offset << "\n";
	}
	for(const IndirectPointerKindConstraint* c: constraints)
		c->dump();
}

char PointerAnalyzer::ID = 0;

StringRef PointerAnalyzer::getPassName() const
{
	return "CheerpPointerAnalyzer";
}

void PointerAnalyzer::getAnalysisUsage(AnalysisUsage& AU) const
{
	AU.setPreservesAll();

	llvm::Pass::getAnalysisUsage(AU);
}

bool PointerAnalyzer::runOnModule(Module& M)
{
	for(const Function & F : M)
		prefetchFunc(F);

	llvm::SmallVector<const User*, 4> globalsUsersQueue;
	for(const GlobalVariable & GV : M.getGlobalList())
	{
		if(GV.getType()->isPointerTy())
			getFinalPointerKindWrapper(&GV);
		if(!GV.hasInitializer())
			continue;
		for(const User* u: GV.users())
		{
			if(!u->getType()->isPointerTy())
				continue;
			if(!isa<Constant>(u))
				continue;
			globalsUsersQueue.push_back(u);
		}
	}

	SmallVector<ConstantExpr*, 4> ConstantGEPs;
	ConstantExpr::getAllFromOpcode(ConstantGEPs, M.getContext(), Instruction::GetElementPtr);
	for(ConstantExpr* GEP: ConstantGEPs)
	{
		globalsUsersQueue.push_back(GEP);
	}

	while(!globalsUsersQueue.empty())
	{
		const User* u = globalsUsersQueue.pop_back_val();
		getFinalPointerKindWrapper(u);
		for(const User* v: u->users())
		{
			if(!v->getType()->isPointerTy())
				continue;
			if(!isa<Constant>(v))
				continue;
			if(PACache.pointerKindData.valueMap.count(v))
				continue;
			globalsUsersQueue.push_back(v);
		}
	}

	return false;
}

struct PointerUsageVisitor
{
	PointerUsageVisitor( PointerAnalyzer::PointerAnalyzerCache& cache) :
			pointerKindData(cache.pointerKindData), addressTakenCache(cache.addressTakenCache) {}

	PointerKindWrapper& visitValue(PointerKindWrapper& ret, const Value* v, bool first);
	PointerKindWrapper& visitUse(PointerKindWrapper& ret, const Use* U);
	static bool visitRawChain ( const Value * v );
	static bool visitByteLayoutChain ( const Value * v );
	static POINTER_KIND getKindForType(Type*);

	PointerKindWrapper& visitAllUses(PointerKindWrapper& ret, const Value* v)
	{
		for(const Use& u : v->uses())
		{
			visitUse(ret, &u);
			if (ret==REGULAR)
				break;
		}
		return ret;
	}

	Type * realType( const Value * v ) const
	{
		assert( v->getType()->isPointerTy() );
		if ( isBitCast(v) )
			v = cast<User>(v)->getOperand(0);
		return v->getType()->getPointerElementType();
	}

	PointerAnalyzer::PointerKindData& pointerKindData;
	PointerAnalyzer::AddressTakenMap& addressTakenCache;
	llvm::DenseSet< const llvm::Value* > closedset;
};

template<class T>
struct PointerResolverBaseVisitor
{
	PointerResolverBaseVisitor(PointerAnalyzer::PointerAnalyzerCache& cache,
				PointerAnalyzer::PointerData<T>& pointerData,
				PointerAnalyzer::AddressTakenMap& addressTakenCache ) :
					cache(cache), pointerData(pointerData), addressTakenCache(addressTakenCache){}
	~PointerResolverBaseVisitor()
	{
		for(const IndirectPointerKindConstraint* c: closedset)
			c->isBeingVisited = false;
		for(const PointerKindWrapper* c: visitedPKW)
			c->isBeingVisited = false;
	}

	const T& resolveConstraint(const IndirectPointerKindConstraint& c);

	PointerAnalyzer::PointerAnalyzerCache& cache;
	PointerAnalyzer::PointerData<T>& pointerData;
	PointerAnalyzer::AddressTakenMap& addressTakenCache;
	std::vector< const IndirectPointerKindConstraint* > closedset;
	std::vector<const PointerKindWrapper*> visitedPKW;
};

struct PointerResolverForKindVisitor: public PointerResolverBaseVisitor<PointerKindWrapper>
{
	PointerResolverForKindVisitor(PointerAnalyzer::PointerAnalyzerCache& cache) :
				PointerResolverBaseVisitor<PointerKindWrapper>(cache, cache.pointerKindData, cache.addressTakenCache){}
	const PointerKindWrapper& resolvePointerKind(const PointerKindWrapper& k);
	const PointerKindWrapper& resolvePointerKindImpl(const PointerKindWrapper& k, bool& mayCache);
	void cacheResolvedConstraint(const IndirectPointerKindConstraint& c, const PointerKindWrapper& d);
};

bool PointerUsageVisitor::visitRawChain( const Value * p)
{
	const Value * origP = p;
	while(true)
	{
		if (TypeSupport::isAsmJSPointer(p->getType()))
			return true;
		if (TypeSupport::isGenericJSPointer(p->getType()))
			return false;
		// ignore geps and bitcasts
		if ( isGEP(p) || isBitCast(p) )
		{
			const User* u = cast<User>(p);
			p = u->getOperand(0);
			continue;
		}
		break;
	}
	const GlobalValue* top = nullptr;
	if (isa<Instruction>(p))
	{
		if (const CallInst* ci = dyn_cast<CallInst>(p))
		{

			Function* callee = ci->getCalledFunction();
			bool getStackPtrCall = callee && callee->getName() == StringRef("__getStackPtr");
			if (getStackPtrCall)
				return true;
		}
		else if (isa<IntToPtrInst>(p))
		{
			return true;
		}
		top = cast<Instruction>(p)->getParent()->getParent();
	}
	else if (isa<Argument>(p) )
	{
		top = cast<Argument>(p)->getParent();
		if (!TypeSupport::isRawPointer(p->getType(), top->getSection() == StringRef("asmjs")))
			return false;
	}
	else if (isa<ConstantExpr>(p) && cast<ConstantExpr>(p)->getOpcode() == Instruction::IntToPtr)
	{
		return true;
	}
	else if (isa<GlobalValue>(p))
	{
		top = cast<GlobalValue>(p);
	}
	else if (isa<Constant>(p))
	{
		// By following geps and bitcast we reached a null, make the kind depend on the container function
		if (isa<Instruction>(origP))
			top = cast<Instruction>(origP)->getParent()->getParent();
		else if (isa<GlobalValue>(origP))
			top = cast<GlobalValue>(origP);
	}
	if (top && top->getSection() == StringRef("asmjs"))
		return true;
	return false;
}

bool PointerUsageVisitor::visitByteLayoutChain( const Value * p )
{
	if ( TypeSupport::hasByteLayout(p->getType()->getPointerElementType()) )
		return true;
	if ( isGEP(p))
	{
		const User* u = cast<User>(p);
		// We need to find out if the base element or any element accessed by the GEP is byte layout
		if (visitByteLayoutChain(u->getOperand(0)))
			return true;
		Type* curType = u->getOperand(0)->getType();
		for (uint32_t i=1;i<u->getNumOperands();i++)
		{
			if (StructType* ST = dyn_cast<StructType>(curType))
			{
				if (ST->hasByteLayout())
					return true;
				uint32_t index = cast<ConstantInt>( u->getOperand(i) )->getZExtValue();
				curType = ST->getElementType(index);
			}
			else
			{
				// This case also handles the first index
				curType = getElementType(curType);
			}
		}
		return false;
	}

	if ( isBitCast(p) || (isa<IntrinsicInst>(p) && cast<IntrinsicInst>(p)->getIntrinsicID() == Intrinsic::cheerp_cast_user))
	{
		const User* u = cast<User>(p);
		if (TypeSupport::hasByteLayout(u->getOperand(0)->getType()->getPointerElementType()))
			return true;
		if (visitByteLayoutChain(u->getOperand(0)))
			return true;
		return false;
	}

	return false;
}

PointerKindWrapper& PointerUsageVisitor::visitValue(PointerKindWrapper& ret, const Value* p, bool first)
{
	if (p->getType()->isPointerTy())
	{
		if (isa<ConstantPointerNull>(p) || isa<UndefValue>(p))
		{
			return pointerKindData.valueMap.insert( std::make_pair(p, CONSTANT ) ).first->second;
		}

		if (visitRawChain(p))
		{
			if (isa<Argument>(p))
				pointerKindData.argsMap.insert( std::make_pair(p, RAW ) );
			return pointerKindData.valueMap.insert( std::make_pair(p, RAW ) ).first->second;
		}
		if (visitByteLayoutChain(p))
			return pointerKindData.valueMap.insert( std::make_pair(p, BYTE_LAYOUT ) ).first->second;
		else if(getKindForType(p->getType()->getPointerElementType()) == COMPLETE_OBJECT)
			return pointerKindData.valueMap.insert( std::make_pair(p, COMPLETE_OBJECT ) ).first->second;
	}

	auto existingValueIt = pointerKindData.valueMap.find(p);
	if(existingValueIt != pointerKindData.valueMap.end())
		return ret |= existingValueIt->second;

	if(!closedset.insert(p).second)
		return ret |= UNKNOWN;

	bool mayCache = first || ret == COMPLETE_OBJECT;
	PointerKindWrapper oldRet;
	if(!mayCache)
		oldRet.swap(ret);
	auto CacheAndReturn = [&](PointerKindWrapper& k) -> PointerKindWrapper&
	{
		assert(first || &k==&ret);
		// Do not recurse below here
		closedset.erase(p);
		if(first)
		{
			k.makeKnown();
			return pointerKindData.valueMap.insert( std::make_pair(p, k ) ).first->second;
		}
		else if(k.isKnown())
			pointerKindData.valueMap.insert( std::make_pair(p, k ) );

		if(mayCache)
			return k;
		else
			return ret |= oldRet;
	};

	TypeAndIndex baseAndIndex = PointerAnalyzer::getBaseStructAndIndexFromGEP(p);
	if(baseAndIndex)
	{
		if(TypeSupport::isAsmJSPointer(baseAndIndex.type))
			return pointerKindData.valueMap.insert( std::make_pair(p, RAW ) ).first->second;
		// If the pointer inside a structure has uses which are not just loads and stores
		// we have merge the BASE_AND_INDEX_CONSTRAINT with the STORED_TYPE_CONSTRAINT
		// We do this by making both indirectly dependent on the other
		if(p->getType()->getPointerElementType()->isPointerTy() && cheerp::hasNonLoadStoreUses(p))
		{
			Type* pointedType = cast<StructType>(baseAndIndex.type)->getElementType(baseAndIndex.index)->getPointerElementType();
			IndirectPointerKindConstraint baseAndIndexContraint(BASE_AND_INDEX_CONSTRAINT, baseAndIndex);
			IndirectPointerKindConstraint storedTypeConstraint(STORED_TYPE_CONSTRAINT, pointedType);
			pointerKindData.constraintsMap[baseAndIndexContraint] |= pointerKindData.getConstraintPtr(storedTypeConstraint);
			pointerKindData.constraintsMap[storedTypeConstraint] |= pointerKindData.getConstraintPtr(baseAndIndexContraint);
		}

		// For constant expression we need to handle the case immediately as we won't have another chance
		if(first || isa<ConstantExpr>(p))
		{
			PointerKindWrapper& k = visitAllUses(ret, p);
			k.makeKnown();
			// In general, keep track of the contraints on elements of structures
			// Warning, do not add the kindForType to the members, otherwise all scalars will require a wrapper array
			pointerKindData.baseStructAndIndexMapForMembers[baseAndIndex] |= k;
			return CacheAndReturn(k);
		}
	}

	if((isa<AllocaInst>(p) || isa<GlobalVariable>(p)) && !cheerp::hasNonLoadStoreUses(p))
		return CacheAndReturn(ret |= COMPLETE_OBJECT);

	if(const StoreInst* SI=dyn_cast<StoreInst>(p))
	{
		Type* valueType = SI->getValueOperand()->getType();
		assert(valueType->isPointerTy());
		Type* pointedValueType = valueType->getPointerElementType();
		bool asmjs = SI->getParent()->getParent()->getSection() == StringRef("asmjs");
		if(TypeSupport::hasByteLayout(pointedValueType))
			return CacheAndReturn(ret = BYTE_LAYOUT);
		else if (TypeSupport::isRawPointer(valueType, asmjs))
			return CacheAndReturn(ret = RAW);
		else if(TypeAndIndex baseAndIndex = PointerAnalyzer::getBaseStructAndIndexFromGEP(SI->getPointerOperand()))
			ret |= pointerKindData.getConstraintPtr(IndirectPointerKindConstraint( BASE_AND_INDEX_CONSTRAINT, baseAndIndex ));
		else
			ret |= pointerKindData.getConstraintPtr(IndirectPointerKindConstraint( STORED_TYPE_CONSTRAINT, pointedValueType));
		// Only cache the result for SI when not recursing, otherwise we would poison the kind of SI with the other uses of valOp
		return CacheAndReturn(ret);
	}

	bool isIntrinsic = false;
	if ( const IntrinsicInst * intrinsic = dyn_cast<IntrinsicInst>(p) )
	{
		isIntrinsic = true;
		switch ( intrinsic->getIntrinsicID() )
		{
		case Intrinsic::cheerp_virtualcast:
		case Intrinsic::cheerp_downcast:
		case Intrinsic::cheerp_upcast_collapsed:
		case Intrinsic::cheerp_cast_user:
			break;
		case Intrinsic::cheerp_allocate:
		case Intrinsic::cheerp_allocate_array:
		case Intrinsic::cheerp_reallocate:
			break;
		case Intrinsic::cheerp_pointer_base:
		case Intrinsic::cheerp_create_closure:
		case Intrinsic::cheerp_make_complete_object:
		{
			llvm::Type * rType = realType(p);
			PointerKindWrapper testRet;
			if(getKindForType(rType) != COMPLETE_OBJECT && visitAllUses(testRet, p).getPointerKindForKnown() != COMPLETE_OBJECT)
			{
				llvm::errs() << "Result of " << *intrinsic << " used as REGULAR: " << *p << "\n";
				llvm::report_fatal_error("Unsupported code found, please report a bug", false);
			}
			return CacheAndReturn(ret |= COMPLETE_OBJECT);
		}
		case Intrinsic::cheerp_make_regular:
			return CacheAndReturn(ret |= PointerKindWrapper(REGULAR, p));
		case Intrinsic::memmove:
		case Intrinsic::memcpy:
		case Intrinsic::memset:
			return CacheAndReturn(visitValue(ret, intrinsic->getArgOperand(0), /*first*/ false));
		case Intrinsic::cheerp_pointer_offset:
		case Intrinsic::cheerp_is_linear_heap:
		case Intrinsic::invariant_start:
			return CacheAndReturn(visitValue(ret, intrinsic->getArgOperand(1), /*first*/ false));
		case Intrinsic::invariant_end:
		case Intrinsic::vastart:
		case Intrinsic::vaend:
		case Intrinsic::flt_rounds:
		default:
			SmallString<128> str("Unreachable code in cheerp::PointerAnalyzer::visitValue, unhandled intrinsic: ");
			str+=intrinsic->getCalledFunction()->getName();
			llvm::report_fatal_error(StringRef(str),false);
		}
	}

	if(const Argument* arg = dyn_cast<Argument>(p))
	{
		Type* argPointedType = arg->getType()->getPointerElementType();
		assert(first);
		PointerKindWrapper& k = visitAllUses(ret, p);
		k.makeKnown();
		pointerKindData.argsMap[arg] = k;

		// Keep track of the constraint for each argument/type pair, but only if the function is indirectly used
		// If the kind is COMPLETE_OBJECT it is ok to omit it from the constraints for this argument index and type
		if(addressTakenCache.checkAddressTaken(arg->getParent()) && k != COMPLETE_OBJECT)
		{
			TypeAndIndex typeAndIndex(argPointedType, arg->getArgNo(), TypeAndIndex::ARGUMENT);
			pointerKindData.constraintsMap[IndirectPointerKindConstraint(INDIRECT_ARG_CONSTRAINT, typeAndIndex)] |=
					pointerKindData.getConstraintPtr(IndirectPointerKindConstraint( DIRECT_ARG_CONSTRAINT_IF_ADDRESS_TAKEN, arg ));
		}
		return CacheAndReturn(ret = PointerKindWrapper(pointerKindData.getConstraintPtr(IndirectPointerKindConstraint(DIRECT_ARG_CONSTRAINT, arg))));
	}

	// TODO this is not really necessary,
	// but we need to modify the writer so that CallInst and InvokeInst
	// perform a demotion in place.
	if(auto cs = ImmutableCallSite(p))
	{
		assert(cs.getCaller()->getSection()!=StringRef("asmjs"));
		if (!isIntrinsic)
		{
			assert(first);
			PointerKindWrapper& k = visitAllUses(ret, p);
			k.makeKnown();
			if(const Function* F = cs.getCalledFunction())
			{
				if (TypeSupport::isAsmJSPointer(F->getReturnType()))
					return CacheAndReturn(ret = PointerKindWrapper(RAW));
				bool calleeAsmJS = F->getSection() == StringRef("asmjs");
				if (calleeAsmJS)
				{
					return CacheAndReturn(ret = PointerKindWrapper(SPLIT_REGULAR, F));
					
				}
				else
				{
					IndirectPointerKindConstraint c(RETURN_CONSTRAINT, F);
					pointerKindData.constraintsMap[c] |= k;
					// We want to override the ret value, not add a constraint
					return CacheAndReturn(ret = PointerKindWrapper(pointerKindData.getConstraintPtr(c)));
				}
			}
			else
			{
				// There may be implicit casts to direct bases, so collapse all types to the least derived one
				llvm::Type* retType = p->getType();
				if(retType->isPointerTy() && retType->getPointerElementType()->isStructTy())
				{
					StructType* st = cast<StructType>(retType->getPointerElementType());
					while(st->getDirectBase())
						st = st->getDirectBase();
					retType = st->getPointerTo();
				}
				IndirectPointerKindConstraint c(RETURN_TYPE_CONSTRAINT, retType);
				pointerKindData.constraintsMap[c] |= k;
				// We want to override the ret value, not add a constraint
				return CacheAndReturn(ret = PointerKindWrapper(pointerKindData.getConstraintPtr(c)));
			}
		}
	}

	// Keep track of the constraints for loaded pointers of this type
	if(const LoadInst* LI = dyn_cast<LoadInst>(p))
	{
		assert(first);
		PointerKindWrapper& k = visitAllUses(ret, p);
		k.makeKnown();
		// We want to override the ret value, not add a constraint
		if (TypeAndIndex b = PointerAnalyzer::getBaseStructAndIndexFromGEP(LI->getOperand(0)))
		{
			IndirectPointerKindConstraint baseAndIndexContraint(BASE_AND_INDEX_CONSTRAINT, b);
			pointerKindData.constraintsMap[baseAndIndexContraint] |= k;
			return CacheAndReturn(ret = PointerKindWrapper( pointerKindData.getConstraintPtr(baseAndIndexContraint) ) );
		}
		else
		{
			Type* curType = p->getType()->getPointerElementType();
			IndirectPointerKindConstraint storedTypeConstraint(STORED_TYPE_CONSTRAINT, curType);
			pointerKindData.constraintsMap[storedTypeConstraint] |= k;
			return CacheAndReturn(ret = PointerKindWrapper( pointerKindData.getConstraintPtr(storedTypeConstraint) ));
		}
	}

	if(isa<VAArgInst>(p))
	{
		// This behaves like a load to memory for the specific type
		assert(first);
		PointerKindWrapper& k = visitAllUses(ret, p);
		k.makeKnown();
		// We want to override the ret value, not add a constraint
		Type* curType = p->getType()->getPointerElementType();
		// Collapse the type to the most base one
		if(StructType* st = dyn_cast<StructType>(curType))
		{
			while(st->getDirectBase())
				st = st->getDirectBase();
			curType = st;
		}
		IndirectPointerKindConstraint storedTypeConstraint(STORED_TYPE_CONSTRAINT, curType);
		pointerKindData.constraintsMap[storedTypeConstraint] |= k;
		return CacheAndReturn(ret = PointerKindWrapper( pointerKindData.getConstraintPtr(storedTypeConstraint) ));
	}

	return CacheAndReturn(visitAllUses(ret, p));
}

PointerKindWrapper& PointerUsageVisitor::visitUse(PointerKindWrapper& ret, const Use* U)
{
	const User * p = U->getUser();
	if ( isGEP(p) )
	{
		const Constant * constOffset = dyn_cast<Constant>( p->getOperand(1) );
		
		if ( constOffset && constOffset->isNullValue() )
		{
			if ( p->getNumOperands() == 2 )
				return visitValue( ret, p, /*first*/ false );
			return ret |= COMPLETE_OBJECT;
		}
		return ret |= PointerKindWrapper(SPLIT_REGULAR, p);
	}

	// Constant data in memory is considered stored
	if (isa<ConstantArray>(p) || isa<GlobalVariable>(p))
		return ret |= pointerKindData.getConstraintPtr(IndirectPointerKindConstraint( STORED_TYPE_CONSTRAINT, U->get()->getType()->getPointerElementType() ));

	if (isa<ConstantStruct>(p))
	{
		// Build a TypeAndIndex struct to get the normalized type
		TypeAndIndex baseAndIndex(p->getType(), U->getOperandNo(), TypeAndIndex::STRUCT_MEMBER);
		return ret |= pointerKindData.getConstraintPtr(IndirectPointerKindConstraint(BASE_AND_INDEX_CONSTRAINT, baseAndIndex));
	}

	// We need to check for basic type kinds here, because visitValue may have not done it,
	// for example when computing kinds for members
	Type* pointedType = U->get()->getType()->getPointerElementType();
	POINTER_KIND kindForType = getKindForType(pointedType);

	bool isFromStruct = PointerAnalyzer::getBaseStructAndIndexFromGEP(U->get());

	if ( isa<StoreInst>(p) )
	{
		// If the pointer is being stored, use the store logic in visitValue
		if (U->getOperandNo() == 0)
			return visitValue(ret, p, /*first*/ false);
		// If the pointer is the memory location we need to make sure that pointer to immutables are REGULAR
		if (!isFromStruct && kindForType != UNKNOWN)
			return ret |= PointerKindWrapper(kindForType, p);
	}

	if ( !isFromStruct && isa<LoadInst>(p) && kindForType != UNKNOWN )
		return ret |= PointerKindWrapper(kindForType, p);

	if ( isa<PtrToIntInst>(p) || ( isa<ConstantExpr>(p) && cast<ConstantExpr>(p)->getOpcode() == Instruction::PtrToInt) )
		return ret |= PointerKindWrapper(SPLIT_REGULAR, p);

	if ( const CmpInst * I = dyn_cast<CmpInst>(p) )
	{
		if (kindForType != UNKNOWN)
			return ret |= PointerKindWrapper(kindForType, p);
		if ( !I->isEquality() )
			return ret |= PointerKindWrapper(SPLIT_REGULAR, p);
		else
			return ret |= COMPLETE_OBJECT;
	}

	if ( const IntrinsicInst * intrinsic = dyn_cast<IntrinsicInst>(p) )
	{
		switch ( intrinsic->getIntrinsicID() )
		{
		case Intrinsic::memmove:
		case Intrinsic::memcpy:
		{
			if (TypeSupport::hasByteLayout(intrinsic->getOperand(0)->getType()->getPointerElementType()))
				return ret |= COMPLETE_OBJECT;
			else
				return ret |= PointerKindWrapper(SPLIT_REGULAR, p);
		}
		case Intrinsic::cheerp_get_array_len:
			return ret |= PointerKindWrapper(SPLIT_REGULAR, p);
		case Intrinsic::cheerp_pointer_kind:
			return ret |= COMPLETE_OBJECT;
		case Intrinsic::invariant_start:
		case Intrinsic::invariant_end:
		case Intrinsic::lifetime_start:
		case Intrinsic::lifetime_end:
		case Intrinsic::cheerp_make_regular:
		case Intrinsic::cheerp_make_complete_object:
		case Intrinsic::cheerp_downcast_current:
			return ret |= COMPLETE_OBJECT;
		case Intrinsic::cheerp_upcast_collapsed:
		case Intrinsic::cheerp_cast_user:
			return visitValue( ret, p, /*first*/ false );
		case Intrinsic::cheerp_virtualcast:
			return ret |= COMPLETE_OBJECT;
		case Intrinsic::cheerp_downcast:
		{
			// Behaves like a cast if the offset is constant 0
			const Value* offset = intrinsic->getOperand(1);
			if(isa<ConstantInt>(offset) && cast<ConstantInt>(offset)->isNullValue())
				return visitValue( ret, p, /*first*/ false );
			else
				return ret |= COMPLETE_OBJECT;
		}
		case Intrinsic::cheerp_reallocate:
		case Intrinsic::cheerp_pointer_base:
		case Intrinsic::cheerp_pointer_offset:
		case Intrinsic::cheerp_is_linear_heap:
		case Intrinsic::vastart:
		case Intrinsic::vacopy:
		case Intrinsic::vaend:
			return ret |= PointerKindWrapper(SPLIT_REGULAR, p);
		case Intrinsic::cheerp_create_closure:
			if ( U->getOperandNo() == 0)
				return ret |= COMPLETE_OBJECT;
			else if ( isa<Function>( p->getOperand(0) ) )
				return ret |= PointerKindWrapper(SPLIT_REGULAR, p);
			else
				llvm::report_fatal_error("Unreachable code in cheerp::PointerAnalyzer::visitUse, cheerp_create_closure");
		case Intrinsic::cheerp_deallocate:
		{
			if (TypeSupport::isTypedArrayType(U->get()->getType()->getPointerElementType(), true))
			{
				return ret |= PointerKindWrapper(SPLIT_REGULAR, p);
			}
			return ret |= COMPLETE_OBJECT;
		}
		case Intrinsic::flt_rounds:
		case Intrinsic::cheerp_allocate:
		case Intrinsic::cheerp_allocate_array:
		case Intrinsic::memset:
		default:
			SmallString<128> str("Unreachable code in cheerp::PointerAnalyzer::visitUse, unhandled intrinsic: ");
			str+=intrinsic->getCalledFunction()->getName();
			llvm::report_fatal_error(StringRef(str),false);
		}
		return ret |= PointerKindWrapper(SPLIT_REGULAR, p);
	}

	if ( auto cs = ImmutableCallSite(p) )
	{
		if ( cs.isCallee(U) )
			return ret |= COMPLETE_OBJECT;

		const Function * calledFunction = cs.getCalledFunction();
		// TODO: Use function type
		if ( !calledFunction )
		{
			TypeAndIndex typeAndIndex(pointedType, U->getOperandNo(), TypeAndIndex::ARGUMENT);
			return ret |= pointerKindData.getConstraintPtr(IndirectPointerKindConstraint(INDIRECT_ARG_CONSTRAINT, typeAndIndex));
		}

		if (isFreeFunctionName(calledFunction->getName()))
		{
			if (TypeSupport::isTypedArrayType(U->get()->getType()->getPointerElementType(), true))
			{
				return ret |= PointerKindWrapper(SPLIT_REGULAR, p);
			}
			return ret |= COMPLETE_OBJECT;
		}

		unsigned argNo = cs.getArgumentNo(U);

		if ( argNo >= calledFunction->arg_size() )
		{
			// Passed as a variadic argument, behave like it was stored in memory
			if(StructType* st = dyn_cast<StructType>(pointedType))
			{
				while(st->getDirectBase())
					st = st->getDirectBase();
				pointedType = st;
			}
			return ret |= pointerKindData.getConstraintPtr(IndirectPointerKindConstraint( STORED_TYPE_CONSTRAINT, pointedType ));
		}

		Function::const_arg_iterator arg = calledFunction->arg_begin();
		std::advance(arg, argNo);
		return ret |= pointerKindData.getConstraintPtr(IndirectPointerKindConstraint(DIRECT_ARG_CONSTRAINT, &*arg));
	}

	if ( const ReturnInst * retInst = dyn_cast<ReturnInst>(p) )
		return ret |= pointerKindData.getConstraintPtr(IndirectPointerKindConstraint(RETURN_CONSTRAINT, retInst->getParent()->getParent()));

	// Bitcasts from byte layout types require COMPLETE_OBJECT, and generate BYTE_LAYOUT
	// Bitcasts to RAW force SPLIT_REGULAR or REGULAR
	if(isBitCast(p))
	{
		if (TypeSupport::isRawPointer(p->getType(), false))
			return ret |= PointerKindWrapper(SPLIT_REGULAR, p);
		else if (TypeSupport::hasByteLayout(p->getOperand(0)->getType()->getPointerElementType()))
			return ret |= COMPLETE_OBJECT;
		else
			return visitValue( ret, p, /*first*/ false );
	}

	if(isa<SelectInst> (p) || isa <PHINode>(p) || (isa<ConstantExpr>(p) && cast<ConstantExpr>(p)->getOpcode() == Instruction::Select) )
		return visitValue(ret, p, /*first*/ false);

	return ret |= COMPLETE_OBJECT;
}

POINTER_KIND PointerUsageVisitor::getKindForType(Type * tp)
{
	if ( tp->isFunctionTy() ||
		TypeSupport::isClientType( tp ) )
		return COMPLETE_OBJECT;

	if ( TypeSupport::isImmutableType( tp ) )
		return SPLIT_REGULAR;

	return UNKNOWN;
}

template<class T>
const T& PointerResolverBaseVisitor<T>::resolveConstraint(const IndirectPointerKindConstraint& c)
{
	switch(c.kind)
	{
		case DIRECT_ARG_CONSTRAINT:
		{
			// If the function has its address taken we need to do the indirect arg check
			if (addressTakenCache.checkAddressTaken(c.argPtr->getParent()))
			{
				Type* argPointedType = c.argPtr->getType()->getPointerElementType();
				TypeAndIndex typeAndIndex(argPointedType, c.argPtr->getArgNo(), TypeAndIndex::ARGUMENT);
				return resolveConstraint(IndirectPointerKindConstraint( INDIRECT_ARG_CONSTRAINT, typeAndIndex));
			}
			else
			{
				assert(pointerData.argsMap.count(c.argPtr));
				return pointerData.argsMap.find(c.argPtr)->second;
			}
		}
		case RETURN_CONSTRAINT:
		case STORED_TYPE_CONSTRAINT:
		case RETURN_TYPE_CONSTRAINT:
		case BASE_AND_INDEX_CONSTRAINT:
		case INDIRECT_ARG_CONSTRAINT:
		{
			const auto& it=pointerData.constraintsMap.find(c);
			if(it==pointerData.constraintsMap.end())
				return T::staticDefaultValue;
			return it->second;
		}
		case DIRECT_ARG_CONSTRAINT_IF_ADDRESS_TAKEN:
		{
			// If the function address is not taken we can ignore this constraint
			if (!addressTakenCache.checkAddressTaken(c.argPtr->getParent()))
				return T::staticDefaultValue;
			else
			{
				assert(pointerData.argsMap.count(c.argPtr));
				return pointerData.argsMap.find(c.argPtr)->second;
			}
		}
	}
	llvm_unreachable("All constraints kind should be addressed by the switch");
}

void PointerResolverForKindVisitor::cacheResolvedConstraint(const IndirectPointerKindConstraint& c, const PointerKindWrapper& t)
{
	//It's possible that this is called without the global state allowing caching
	if (!cache.mayCache())
		return;
	switch(c.kind)
	{
		case DIRECT_ARG_CONSTRAINT:
			if (addressTakenCache.checkAddressTaken(c.argPtr->getParent()))
			{
				Type* argPointedType = c.argPtr->getType()->getPointerElementType();
				TypeAndIndex typeAndIndex(argPointedType, c.argPtr->getArgNo(), TypeAndIndex::ARGUMENT);
				return cacheResolvedConstraint(IndirectPointerKindConstraint( INDIRECT_ARG_CONSTRAINT, typeAndIndex), t);
			}
			else
			{
				assert(pointerData.argsMap.count(c.argPtr));
				(pointerData.argsMap.find(c.argPtr)->second = t).applyRegularPreference(PointerAnalyzer::getRegularPreference(c, cache));
			}
			break;
		case RETURN_CONSTRAINT:
		case STORED_TYPE_CONSTRAINT:
		case RETURN_TYPE_CONSTRAINT:
		case BASE_AND_INDEX_CONSTRAINT:
		case INDIRECT_ARG_CONSTRAINT:
		{
			const auto& it=pointerData.constraintsMap.find(c);
			if(it!=pointerData.constraintsMap.end())
				(it->second = t).applyRegularPreference(PointerAnalyzer::getRegularPreference(c, cache));
			break;
		}
		case DIRECT_ARG_CONSTRAINT_IF_ADDRESS_TAKEN:
			if (addressTakenCache.checkAddressTaken(c.argPtr->getParent()))
			{
				assert(pointerData.argsMap.count(c.argPtr));
				(pointerData.argsMap.find(c.argPtr)->second = t).applyRegularPreference(PointerAnalyzer::getRegularPreference(c, cache));
			}
			break;
	}
}

const PointerKindWrapper& PointerResolverForKindVisitor::resolvePointerKind(const PointerKindWrapper& k)
{
	bool mayCache = cache.mayCache();
	return resolvePointerKindImpl(k, mayCache);
}

const PointerKindWrapper& PointerResolverForKindVisitor::resolvePointerKindImpl(const PointerKindWrapper& k, bool& mayCache)
{
	assert(k==INDIRECT);

	if (k.isBeingVisited)
	{
		mayCache = false;
		return PointerKindWrapper::staticDefaultValue;
	}
	k.isBeingVisited = true;
	visitedPKW.push_back(&k);

	// If mayCache is initially false we can't cache anything
	bool initialMayCache = mayCache;
	// Temporary value to store a SPLIT_REGULAR kind, as we can't stop immediately like we do for REGULAR
	const PointerKindWrapper* tmpRet = NULL;
	for(const IndirectPointerKindConstraint* constraint: k.constraints)
	{
		if(constraint->isBeingVisited)
		{
			mayCache = false;
			continue;
		}
		constraint->isBeingVisited = true;
		closedset.push_back(constraint);
		const PointerKindWrapper& retKind=resolveConstraint(*constraint);
		assert(retKind.isKnown());
		if (retKind.isBeingVisited)
		{
			mayCache = false;
			continue;
		}
		if(retKind==REGULAR || retKind==BYTE_LAYOUT)
			return retKind;
		else if(retKind == SPLIT_REGULAR)
			tmpRet = &retKind;
		else if(retKind==INDIRECT)
		{
			bool subMayCache = initialMayCache;
			const PointerKindWrapper& resolvedKind=resolvePointerKindImpl(retKind, subMayCache);
			if(resolvedKind==REGULAR)
			{
				cacheResolvedConstraint(*constraint, resolvedKind);
				mayCache = true;
				return resolvedKind;
			}

			if(subMayCache)
				cacheResolvedConstraint(*constraint, resolvedKind);
			else
				mayCache = false;

			if(resolvedKind==SPLIT_REGULAR)
				tmpRet = &resolvedKind;
		}
	}
	if(tmpRet)
		return *tmpRet;

	return PointerKindWrapper::staticDefaultValue;
}

struct PointerConstantOffsetVisitor
{
	PointerConstantOffsetVisitor( PointerAnalyzer::PointerAnalyzerCache& cache) :
				pointerOffsetData(cache.pointerOffsetData), addressTakenCache(cache.addressTakenCache) {}

	PointerConstantOffsetWrapper& visitValue(PointerConstantOffsetWrapper& ret, const Value* v, bool first);
	static const llvm::ConstantInt* getPointerOffsetFromGEP( const llvm::Value* v );
	void visitConstantStruct(const ConstantStruct* CS);

	PointerAnalyzer::PointerOffsetData& pointerOffsetData;
	PointerAnalyzer::AddressTakenMap& addressTakenCache;
	llvm::DenseSet< const llvm::Value* > closedset;
};

struct PointerResolverForOffsetVisitor: public PointerResolverBaseVisitor<PointerConstantOffsetWrapper>
{
	PointerResolverForOffsetVisitor(PointerAnalyzer::PointerAnalyzerCache& cache) :
				PointerResolverBaseVisitor<PointerConstantOffsetWrapper>(cache, cache.pointerOffsetData, cache.addressTakenCache){}
	PointerConstantOffsetWrapper resolvePointerOffset(const PointerConstantOffsetWrapper& o);
};

const ConstantInt* PointerConstantOffsetVisitor::getPointerOffsetFromGEP(const Value* p)
{
	if(!isGEP(p))
		return NULL;
	const User* gep=cast<User>(p);
	if (gep->getNumOperands() == 2)
		return NULL;
	SmallVector<Value*, 4> indexes;
	for(uint32_t i=1;i<gep->getNumOperands()-1;i++)
		indexes.push_back(*(gep->op_begin()+i));
	Type* containerType = GetElementPtrInst::getIndexedType(
				(*gep->op_begin())->getType()->getPointerElementType(), indexes);
	if (containerType->isStructTy() || TypeSupport::hasByteLayout(containerType))
		return NULL;
	return dyn_cast<ConstantInt>(*std::prev(gep->op_end()));
}

void PointerConstantOffsetVisitor::visitConstantStruct(const ConstantStruct* CS)
{
	Type* structType = CS->getType();
	// We need to keep track of all offsets for each member
	for(uint32_t i=0;i<CS->getNumOperands();i++)
	{
		// TODO: We might need to handle ConstantArray and ConstantAggregateZero here
		const Value* op = CS->getOperand(i);
		if(op->getType()->isPointerTy())
		{
			PointerConstantOffsetWrapper localRet;
			TypeAndIndex typeAndIndex(structType, i, TypeAndIndex::STRUCT_MEMBER);
			PointerConstantOffsetWrapper& memberRet = visitValue(localRet, op, false);
			assert(!memberRet.isUnknown());
			pointerOffsetData.constraintsMap[IndirectPointerKindConstraint(BASE_AND_INDEX_CONSTRAINT, typeAndIndex)] |= memberRet;
		}
		else if(isa<ConstantStruct>(op))
		{
			visitConstantStruct(cast<ConstantStruct>(op));
		}
	}
}

PointerConstantOffsetWrapper& PointerConstantOffsetVisitor::visitValue(PointerConstantOffsetWrapper& ret, const Value* v, bool first)
{
	auto existingValueIt = pointerOffsetData.valueMap.find(v);
	if(existingValueIt != pointerOffsetData.valueMap.end())
		return ret |= existingValueIt->second;

	if(!closedset.insert(v).second)
		return ret |= PointerConstantOffsetWrapper::UNKNOWN;

	bool mayCache = first || (ret.isUninitialized() && !ret.hasConstraints());
	std::unique_ptr<PointerConstantOffsetWrapper> oldRet;
	if(!mayCache)
	{
		oldRet = std::unique_ptr<PointerConstantOffsetWrapper>(new PointerConstantOffsetWrapper());
		oldRet->swap(ret);
	}
	// Find out if the pointer offset can be a constant
	auto CacheAndReturn = [&](PointerConstantOffsetWrapper& o) -> PointerConstantOffsetWrapper&
	{
		assert(first || &o==&ret);
		// Do not recurse below here
		closedset.erase(v);
		if(first)
		{
			ret.makeKnown();
			return pointerOffsetData.valueMap.insert( std::make_pair(v, o ) ).first->second;
		}
		if(!mayCache)
			*oldRet |= ret;
		if(!o.isUnknown())
		{
			assert(!o.isUninitialized() || o.hasConstraints());
			if(mayCache)
				pointerOffsetData.valueMap.insert( std::make_pair(v, o ) );
			else
			{
				assert(!pointerOffsetData.valueMap.count(v));
				pointerOffsetData.valueMap[v].swap(ret);
			}
		}
		if(!mayCache)
			ret.swap(*oldRet);
		return o;
	};

	if (PointerUsageVisitor::visitRawChain(v))
	{
		return CacheAndReturn(ret |= PointerConstantOffsetWrapper::INVALID);
	}

	if ( isGEP(v) )
	{
		if(TypeAndIndex b = PointerAnalyzer::getBaseStructAndIndexFromGEP(v))
		{
			if(cheerp::hasNonLoadStoreUses(v))
				pointerOffsetData.constraintsMap[IndirectPointerKindConstraint(BASE_AND_INDEX_CONSTRAINT, b)] |= PointerConstantOffsetWrapper::INVALID;
		}
		return CacheAndReturn(ret |= getPointerOffsetFromGEP(v));
	}

	if(const StoreInst* SI=dyn_cast<StoreInst>(v))
	{
		assert(SI->getValueOperand()->getType()->isPointerTy());
		PointerConstantOffsetWrapper& o = visitValue(ret, SI->getValueOperand(), true);

		if (TypeAndIndex baseAndIndex = PointerAnalyzer::getBaseStructAndIndexFromGEP(SI->getPointerOperand()))
		{
			IndirectPointerKindConstraint baseAndIndexContraint(BASE_AND_INDEX_CONSTRAINT, baseAndIndex);
			assert(!o.isUnknown());
			pointerOffsetData.constraintsMap[baseAndIndexContraint] |= o;
			return CacheAndReturn(ret |= pointerOffsetData.getConstraintPtr(baseAndIndexContraint));
		}
		else if(isa<GlobalVariable>(SI->getPointerOperand()))
		{
			visitValue(ret, SI->getPointerOperand(), false);
			return CacheAndReturn(ret);
		}
		else
			return CacheAndReturn(ret |= PointerConstantOffsetWrapper::INVALID);
	}

	if(const LoadInst* LI=dyn_cast<LoadInst>(v))
	{
		if (PointerUsageVisitor::visitRawChain(LI->getPointerOperand()))
		{
			return CacheAndReturn(ret |= PointerConstantOffsetWrapper::INVALID);
		}
		else if (TypeAndIndex baseAndIndex = PointerAnalyzer::getBaseStructAndIndexFromGEP(LI->getPointerOperand()))
			return CacheAndReturn(ret |= pointerOffsetData.getConstraintPtr(IndirectPointerKindConstraint( BASE_AND_INDEX_CONSTRAINT, baseAndIndex)));
		else if(isa<GlobalVariable>(LI->getPointerOperand()))
		{
			visitValue(ret, LI->getPointerOperand(), false);
			return CacheAndReturn(ret);
		}
	}

	if(isBitCast(v))
		return CacheAndReturn(visitValue(ret, cast<User>(v)->getOperand(0), false));

	if(const PHINode* phi=dyn_cast<PHINode>(v))
	{
		for(uint32_t i=0;i<phi->getNumIncomingValues();i++)
		{
			const Value* incoming = phi->getIncomingValue(i);
			visitValue(ret, incoming, false);
		}
		return CacheAndReturn(ret);
	}

	if(const ConstantStruct* CS=dyn_cast<ConstantStruct>(v))
	{
		visitConstantStruct(CS);
		return CacheAndReturn(ret |= PointerConstantOffsetWrapper::INVALID);
	}

	Type* Int32Ty=IntegerType::get(v->getContext(), 32);
	ConstantInt* Zero = cast<ConstantInt>(ConstantInt::get(Int32Ty, 0));
	if(isa<ConstantPointerNull>(v))
		return CacheAndReturn(ret |= Zero);

	if(const Argument* arg = dyn_cast<Argument>(v))
	{
		const Function* parentFunc = arg->getParent();
		if(!parentFunc->hasExternalLinkage() && !addressTakenCache.checkAddressTaken(parentFunc))
		{
			const Function* parentFunc = arg->getParent();
			for(const User* U: parentFunc->users())
			{
				assert(isa<CallInst>(U));
				const CallInst& CI = cast<CallInst>(*U);
				llvm::Value* op = CI.getOperand(arg->getArgNo());
				PointerConstantOffsetWrapper localRet;
				PointerConstantOffsetWrapper& callerRet = visitValue(localRet, op, false);
				ret |= callerRet;
			}
			return CacheAndReturn(ret);
		}
	}

	if(const CallInst * CI = dyn_cast<CallInst>(v))
	{
		Function* F = CI->getCalledFunction();
		if(!F)
			return CacheAndReturn(ret |= PointerConstantOffsetWrapper::INVALID);
		if(F->getIntrinsicID()==Intrinsic::cheerp_allocate ||
			F->getIntrinsicID()==Intrinsic::cheerp_allocate_array ||
			F->getIntrinsicID()==Intrinsic::cheerp_reallocate)
		{
			return CacheAndReturn(ret |= Zero);
		}
		if(F->getName() == "calloc" ||
			F->getName() == "malloc" ||
			F->getName() == "_Znwj" ||
			F->getName() == "_Znaj" ||
			F->getName() == "realloc")
		{
			return CacheAndReturn(ret |= Zero);
		}
		
	}

	// Handle global pointers
	if(const GlobalVariable* GV = dyn_cast<GlobalVariable>(v))
	{
		if(GV->hasInitializer() && GV->getType()->getPointerElementType()->isPointerTy())
		{
			visitValue(ret, GV->getInitializer(), false);
			for(const Use& u: GV->uses())
			{
				const User* user = u.getUser();
				if(isa<StoreInst>(user) && u.getOperandNo() == 1)
					visitValue(ret, user->getOperand(0), false);
				else if(!isa<LoadInst>(user))
					return CacheAndReturn(ret |= PointerConstantOffsetWrapper::INVALID);
			}
			return CacheAndReturn(ret);
		}
	}

	return CacheAndReturn(ret |= PointerConstantOffsetWrapper::INVALID);
}

PointerConstantOffsetWrapper PointerResolverForOffsetVisitor::resolvePointerOffset(const PointerConstantOffsetWrapper& o)
{
	assert(!o.isInvalid());
	assert(o.hasConstraints());
	// 'o' may be VALID (which means it contains a valid offset) or UNINITALIZED
	const llvm::ConstantInt* offset = o.isValid() ? o.getPointerOffset() : NULL;
	for(const IndirectPointerKindConstraint* constraint: o.constraints)
	{
		if(constraint->isBeingVisited)
			continue;
		constraint->isBeingVisited = true;
		closedset.push_back(constraint);
		const PointerConstantOffsetWrapper& c = resolveConstraint(*constraint);
		if(c.isInvalid())
			return PointerConstantOffsetWrapper::INVALID;
		// 'c' is VALID or UNINITALIZED
		if(c.isValid())
		{
			if(offset == NULL)
				offset = c.getPointerOffset();
			else if(offset != c.getPointerOffset())
				return PointerConstantOffsetWrapper::INVALID;
		}
		if(c.hasConstraints())
		{
			const PointerConstantOffsetWrapper& resolved = resolvePointerOffset(c);
			// No constrains allowed below here!
			assert(!resolved.hasConstraints());
			if(resolved.isInvalid())
				return PointerConstantOffsetWrapper::INVALID;
			if(resolved.isUninitialized())
				continue;
			assert(resolved.isValid());
			if(offset == NULL)
				offset = resolved.getPointerOffset();
			else if(offset != resolved.getPointerOffset())
				return PointerConstantOffsetWrapper::INVALID;
		}
	}
	// No constrains were INVALID, so if offset is still NULL it means we should be UNINITALIZED
	// Otherwise we found a valid value
	if (offset == NULL)
		return PointerConstantOffsetWrapper::UNINITALIZED;
	else
		return offset;
}

void PointerAnalyzer::prefetchFunc(const Function& F) const
{
	for(const Argument & arg : F.args())
		if(arg.getType()->isPointerTy())
			getFinalPointerKindWrapper(&arg);
	for(const BasicBlock & BB : F)
	{
		for(auto it=BB.rbegin();it != BB.rend();++it)
		{
			if(it->getType()->isPointerTy() ||
				(isa<StoreInst>(*it) && it->getOperand(0)->getType()->isPointerTy()))
			{
				getFinalPointerKindWrapper(&(*it));
			}
		}
	}
	if(PACache.addressTakenCache.checkAddressTaken(&F))
	{
		IndirectPointerKindConstraint returnConstraint(RETURN_CONSTRAINT, &F);
		llvm::Type* retType = F.getReturnType();
		if(retType->isPointerTy() && retType->getPointerElementType()->isStructTy())
		{
			StructType* st = cast<StructType>(retType->getPointerElementType());
			while(st->getDirectBase())
				st = st->getDirectBase();
			retType = st->getPointerTo();
		}
		IndirectPointerKindConstraint returnTypeConstaint(RETURN_TYPE_CONSTRAINT, retType);
		PACache.pointerKindData.constraintsMap[returnConstraint] |= PACache.pointerKindData.getConstraintPtr(returnTypeConstaint);
	}
}

const PointerKindWrapper& PointerAnalyzer::getFinalPointerKindWrapper(const Value* p) const
{
	// If the values is already cached just return it
	auto it = PACache.pointerKindData.valueMap.find(p);
	if(it!=PACache.pointerKindData.valueMap.end())
	{
		assert(it->second.isKnown());
		return it->second;
	}

	PointerKindWrapper ret;
	PointerKindWrapper& k = PointerUsageVisitor(PACache).visitValue(ret, p, /*first*/ true);
#ifndef NDEBUG
	it = PACache.pointerKindData.valueMap.find(p);
	assert(it!=PACache.pointerKindData.valueMap.end());
	assert(&it->second == &k);
	assert(k.isKnown());
#endif
	return k;
}

const PointerConstantOffsetWrapper& PointerAnalyzer::getFinalPointerConstantOffsetWrapper(const Value* p) const
{
	auto& pointerOffsetData = PACache.pointerOffsetData;

	// If the values is already cached just return it
	auto it = pointerOffsetData.valueMap.find(p);
	if(it!=pointerOffsetData.valueMap.end())
	{
		assert(!it->second.isUnknown());
		return it->second;
	}

	PointerConstantOffsetWrapper ret;
	PointerConstantOffsetWrapper& o = PointerConstantOffsetVisitor(PACache).visitValue(ret, p, /*first*/ true);
#ifndef NDEBUG
	it = PACache.pointerOffsetData.valueMap.find(p);
	assert(it!=PACache.pointerOffsetData.valueMap.end());
	assert(&it->second == &o);
	assert(!o.isUnknown());
#endif
	return o;
}

POINTER_KIND PointerAnalyzer::getPointerKindAssert(const Value* p) const
{
	auto r = getPointerKind(p);
	assert(r != CONSTANT);
	return r;
}
POINTER_KIND PointerAnalyzer::getPointerKind(const Value* p) const
{
	const PointerKindWrapper& k = getFinalPointerKindWrapper(p);

	if (k!=INDIRECT)
		return k.getPointerKind(PREF_NONE);

	// Got an indirect value, we need to resolve it now
	return PointerResolverForKindVisitor(PACache).resolvePointerKind(k).getPointerKind(PREF_NONE);
}

POINTER_KIND PointerAnalyzer::getPointerKindForReturn(const Function* F) const
{
	if(TypeSupport::hasByteLayout(F->getReturnType()->getPointerElementType()))
		return BYTE_LAYOUT;

	assert(F->getReturnType()->isPointerTy());
	if (TypeSupport::isAsmJSPointer(F->getReturnType()))
		return RAW;
	if (F->getSection() == StringRef("asmjs"))
	{
		if (F->getName() == StringRef("__getStackPtr"))
			return RAW;
		if (TypeSupport::isClientType(F->getReturnType()->getPointerElementType()))
			return COMPLETE_OBJECT;
		return SPLIT_REGULAR;
	}
	IndirectPointerKindConstraint c(RETURN_CONSTRAINT, F);
	const PointerKindWrapper& k=PointerResolverForKindVisitor(PACache).resolveConstraint(c);
	assert(k.isKnown());
	REGULAR_POINTER_PREFERENCE regularPreference = getRegularPreference(c, PACache);

	if (k!=INDIRECT)
		return k.getPointerKind(regularPreference);

	return PointerResolverForKindVisitor(PACache).resolvePointerKind(k).getPointerKind(regularPreference);
}

POINTER_KIND PointerAnalyzer::getPointerKindForStoredType(Type* pointerType) const
{
	IndirectPointerKindConstraint c(STORED_TYPE_CONSTRAINT, pointerType->getPointerElementType());
	auto it=PACache.pointerKindData.constraintsMap.find(c);
	if(it==PACache.pointerKindData.constraintsMap.end())
	{
		if (TypeSupport::isRawPointer(pointerType, false))
			return RAW;
		else
			return COMPLETE_OBJECT;
	}
	REGULAR_POINTER_PREFERENCE regularPreference = getRegularPreference(c, PACache);

	const PointerKindWrapper& k = it->second;
	assert(k.isKnown());
	if (k!=INDIRECT)
		return k.getPointerKind(regularPreference);

	return PointerResolverForKindVisitor(PACache).resolvePointerKind(k).getPointerKind(regularPreference);
}

POINTER_KIND PointerAnalyzer::getPointerKindForArgumentTypeAndIndex( const TypeAndIndex& argTypeAndIndex ) const
{
	if(TypeSupport::hasByteLayout(argTypeAndIndex.type))
		return BYTE_LAYOUT;

	IndirectPointerKindConstraint c(INDIRECT_ARG_CONSTRAINT, argTypeAndIndex);
	const PointerKindWrapper& k=PointerResolverForKindVisitor(PACache).resolveConstraint(c);
	assert(k.isKnown());
	REGULAR_POINTER_PREFERENCE regularPreference = getRegularPreference(c, PACache);

	if (k!=INDIRECT)
		return k.getPointerKind(regularPreference);

	return PointerResolverForKindVisitor(PACache).resolvePointerKind(k).getPointerKind(regularPreference);
}

POINTER_KIND PointerAnalyzer::getPointerKindForMemberPointer(const TypeAndIndex& baseAndIndex) const
{
	if(TypeSupport::hasByteLayout(cast<StructType>(baseAndIndex.type)->getElementType(baseAndIndex.index)->getPointerElementType()))
		return BYTE_LAYOUT;

	IndirectPointerKindConstraint c(BASE_AND_INDEX_CONSTRAINT, baseAndIndex);
	auto it=PACache.pointerKindData.constraintsMap.find(c);
	if(it==PACache.pointerKindData.constraintsMap.end())
		return COMPLETE_OBJECT;

	const PointerKindWrapper& k = it->second;
	assert(k.isKnown());
	REGULAR_POINTER_PREFERENCE regularPreference = getRegularPreference(c, PACache);

	if (k!=INDIRECT)
		return k.getPointerKind(regularPreference);

	return PointerResolverForKindVisitor(PACache).resolvePointerKind(k).getPointerKind(regularPreference);
}

POINTER_KIND PointerAnalyzer::getPointerKindForMemberImpl(const TypeAndIndex& baseAndIndex, PointerAnalyzerCache& cache)
{
	auto it=cache.pointerKindData.baseStructAndIndexMapForMembers.find(baseAndIndex);
	if(it==cache.pointerKindData.baseStructAndIndexMapForMembers.end())
		return COMPLETE_OBJECT;

	const PointerKindWrapper& k = it->second;
	assert(k!=UNKNOWN);

	if (k!=INDIRECT)
		return k.getPointerKind(PREF_REGULAR);

	return PointerResolverForKindVisitor(cache).resolvePointerKind(k).getPointerKind(PREF_REGULAR);
}

POINTER_KIND PointerAnalyzer::getPointerKindForMember(const TypeAndIndex& baseAndIndex) const
{
	return getPointerKindForMemberImpl(baseAndIndex, PACache);
}

TypeAndIndex PointerAnalyzer::getBaseStructAndIndexFromGEP(const Value* p)
{
	if(!isGEP(p))
		return TypeAndIndex(NULL, 0, TypeAndIndex::STRUCT_MEMBER);
	const User* gep=cast<User>(p);
	if (gep->getNumOperands() == 2)
		return TypeAndIndex(NULL, 0, TypeAndIndex::STRUCT_MEMBER);

	SmallVector<Value*, 4> indexes;
	for(uint32_t i=1;i<gep->getNumOperands()-1;i++)
		indexes.push_back(*(gep->op_begin()+i));
	Type* containerType = GetElementPtrInst::getIndexedType(
				(*gep->op_begin())->getType()->getPointerElementType(), indexes);

	if (containerType->isStructTy())
	{
		Value* lastIndex = *std::prev(gep->op_end());
		assert(isa<ConstantInt>(lastIndex));
		uint32_t lastOffsetConstant =  cast<ConstantInt>(lastIndex)->getZExtValue();
		return TypeAndIndex(containerType, lastOffsetConstant, TypeAndIndex::STRUCT_MEMBER);
	}
	return TypeAndIndex(NULL, 0, TypeAndIndex::STRUCT_MEMBER);
}

REGULAR_POINTER_PREFERENCE PointerAnalyzer::getRegularPreference(const IndirectPointerKindConstraint& c, PointerAnalyzerCache& cache)
{
	switch(c.kind)
	{
		case BASE_AND_INDEX_CONSTRAINT:
		{
			// A pointer which requires a wrapping array can't be SPLIT_REGULAR
			TypeAndIndex tai(c.typePtr, c.i, TypeAndIndex::STRUCT_MEMBER);
			if(PointerAnalyzer::getPointerKindForMemberImpl(tai, cache) == REGULAR)
				return PREF_REGULAR;
			return PREF_SPLIT_REGULAR;
		}
		case DIRECT_ARG_CONSTRAINT:
		case RETURN_CONSTRAINT:
		case RETURN_TYPE_CONSTRAINT:
		case INDIRECT_ARG_CONSTRAINT:
		case DIRECT_ARG_CONSTRAINT_IF_ADDRESS_TAKEN:
			return PREF_SPLIT_REGULAR;
		case STORED_TYPE_CONSTRAINT:
			return PREF_REGULAR;
	}
	llvm_unreachable("Switch should support all kind of pointers");
}

const ConstantInt* PointerAnalyzer::getConstantOffsetForPointer(const Value * v) const
{
	auto it=PACache.pointerOffsetData.valueMap.find(v);
	if(it==PACache.pointerOffsetData.valueMap.end())
		return NULL;

	if(!it->second.hasConstraints())
	{
		if(it->second.isInvalid() || it->second.isUninitialized())
			return NULL;
		else if(it->second.isValid())
			return it->second.getPointerOffset();
	}
	assert(!it->second.isInvalid() && !it->second.isUnknown());
	const PointerConstantOffsetWrapper& ret=PointerResolverForOffsetVisitor(PACache).resolvePointerOffset(it->second);
	if(ret.isInvalid())
		return NULL;
	else if(ret.isUninitialized())
	{
		Type* Int32Ty=IntegerType::get(v->getContext(), 32);
		return cast<ConstantInt>(ConstantInt::get(Int32Ty, 0));
	}
	assert(ret.isValid());
	assert(ret.getPointerOffset());
	return ret.getPointerOffset();
}

const llvm::ConstantInt* PointerAnalyzer::getConstantOffsetForMember( const TypeAndIndex& baseAndIndex ) const
{
	auto it=PACache.pointerOffsetData.constraintsMap.find(IndirectPointerKindConstraint(BASE_AND_INDEX_CONSTRAINT, baseAndIndex));
	if(it==PACache.pointerOffsetData.constraintsMap.end())
		return NULL;

	if(!it->second.hasConstraints())
	{
		if(it->second.isInvalid() || it->second.isUninitialized())
			return NULL;
		else if(it->second.isValid())
			return it->second.getPointerOffset();
	}
	assert(!it->second.isInvalid() && !it->second.isUnknown());
	const PointerConstantOffsetWrapper& ret=PointerResolverForOffsetVisitor(PACache).resolvePointerOffset(it->second);
	if(ret.isInvalid())
		return NULL;
	else if(ret.isUninitialized())
	{
		Type* Int32Ty=IntegerType::get(baseAndIndex.type->getContext(), 32);
		return cast<ConstantInt>(ConstantInt::get(Int32Ty, 0));
	}
	assert(ret.isValid());
	assert(ret.getPointerOffset());
	return ret.getPointerOffset();
}

void PointerAnalyzer::invalidate(const Value * v)
{
	assert(status == MODIFIABLE);

	if ( PACache.pointerKindData.valueMap.erase(v) )
	{
		const User * u = dyn_cast<User>(v);

		// Go on and invalidate all the operands
		// Users should not be invalidated since they are independent
		if ( u && u->getType()->isPointerTy() )
		{
			for ( const Use & U : u->operands() )
			{
				if ( U->getType()->isPointerTy() )
					invalidate(U.get());
			}
		}
		// TODO: Recompute?
	}
	// If v is a function invalidate also all its call and arguments
	if ( const Function * F = dyn_cast<Function>(v) )
	{
		for ( const Argument & arg : F->args() )
			if (arg.getType()->isPointerTy())
				invalidate(&arg);
		PACache.addressTakenCache.erase(F);
		prefetchFunc(*F);
	}
	// Constant offsets should only be computed at the end, so they should be never invalidated
	assert( !PACache.pointerOffsetData.valueMap.count(v) );
}

void PointerAnalyzer::fullResolve()
{
	if (status == FULLY_RESOLVED)
		return;
	status = CACHING_STARTED;

	auto& pointerKindData = PACache.pointerKindData;

	for(auto& it: pointerKindData.argsMap)
	{
		if(it.second!=INDIRECT)
		{
			it.second.applyRegularPreference(PREF_SPLIT_REGULAR);
			continue;
		}
		const PointerKindWrapper& k=PointerResolverForKindVisitor(PACache).resolvePointerKind(it.second);
		assert(k==COMPLETE_OBJECT || k==BYTE_LAYOUT || k==SPLIT_REGULAR || k==REGULAR);
		it.second = k;
		it.second.applyRegularPreference(PREF_SPLIT_REGULAR);
	}
	for(auto& it: pointerKindData.baseStructAndIndexMapForMembers)
	{
		if(it.second!=INDIRECT)
		{
			it.second.applyRegularPreference(PREF_REGULAR);
			continue;
		}
		const PointerKindWrapper& k=PointerResolverForKindVisitor(PACache).resolvePointerKind(it.second);
		// BYTE_LAYOUT is not expected for the kind of pointers to member
		assert(k==COMPLETE_OBJECT || k==SPLIT_REGULAR || k==REGULAR);
		it.second = k;
		it.second.applyRegularPreference(PREF_REGULAR);
	}
	for(auto& it: pointerKindData.constraintsMap)
	{
		REGULAR_POINTER_PREFERENCE pref = getRegularPreference(it.first, PACache);
		assert(pref != PREF_NONE);
		if(it.second!=INDIRECT)
		{
			it.second.applyRegularPreference(pref);
			continue;
		}
		const PointerKindWrapper& k=PointerResolverForKindVisitor(PACache).resolvePointerKind(it.second);
		assert(k==COMPLETE_OBJECT || k==BYTE_LAYOUT || k==REGULAR || k==SPLIT_REGULAR);
		it.second = k;
		it.second.applyRegularPreference(pref);
	}
	for(auto& it: pointerKindData.valueMap)
	{
		if(it.second!=INDIRECT)
			continue;
		const PointerKindWrapper& k=PointerResolverForKindVisitor(PACache).resolvePointerKind(it.second);
		assert(k==COMPLETE_OBJECT || k==BYTE_LAYOUT || k==REGULAR || k==SPLIT_REGULAR);
		it.second = k;
	}
	status = FULLY_RESOLVED;
}

void PointerAnalyzer::computeConstantOffsets(const Module& M)
{
	assert(status == FULLY_RESOLVED);

	for(const Function & F : M)
	{
		for(const BasicBlock & BB : F)
		{
			for(auto it=BB.begin();it != BB.end();++it)
			{
				if(it->getType()->isPointerTy() ||
					(isa<StoreInst>(*it) && it->getOperand(0)->getType()->isPointerTy()))
				{
					getFinalPointerConstantOffsetWrapper(&(*it));
				}
			}
		}
	}
	llvm::SmallVector<const User*, 4> globalsUsersQueue;

	for(const GlobalVariable & GV : M.getGlobalList())
	{
		if(!GV.hasInitializer())
			continue;
		if(GV.getInitializer()->getType()->isStructTy())
		{
			for(const User* u: GV.users())
			{
				if(!u->getType()->isPointerTy())
					continue;
				if(!isa<Constant>(u))
					continue;
				globalsUsersQueue.push_back(u);
			}
			getFinalPointerConstantOffsetWrapper(GV.getInitializer());
		}
		else if(GV.getInitializer()->getType()->isPointerTy())
			getFinalPointerConstantOffsetWrapper(&GV);
	}

	while(!globalsUsersQueue.empty())
	{
		const User* u = globalsUsersQueue.pop_back_val();
		getFinalPointerConstantOffsetWrapper(u);
		for(const User* v: u->users())
		{
			if(!v->getType()->isPointerTy())
				continue;
			if(!isa<Constant>(v))
				continue;
			globalsUsersQueue.push_back(v);
		}
	}
}

#ifndef NDEBUG
void PointerAnalyzer::dumpPointer(const Value* v, bool dumpOwnerFunc) const
{
	llvm::formatted_raw_ostream fmt( llvm::errs() );

	fmt.changeColor( llvm::raw_ostream::RED, false, false );
	v->printAsOperand( fmt );
	fmt.resetColor();

	if (dumpOwnerFunc)
	{
		if ( const Instruction * I = dyn_cast<Instruction>(v) )
			fmt << " in function: " << I->getParent()->getParent()->getName();
		else if ( const Argument * A = dyn_cast<Argument>(v) )
			fmt << " arg of function: " << A->getParent()->getName();
	}

	if (v->getType()->isPointerTy())
	{
		fmt.PadToColumn(92);
		switch (getPointerKind(v))
		{
			case COMPLETE_OBJECT: fmt << "COMPLETE_OBJECT"; break;
			case REGULAR: fmt << "REGULAR"; break;
			case SPLIT_REGULAR: fmt << "SPLIT_REGULAR"; break;
			case BYTE_LAYOUT: fmt << "BYTE_LAYOUT"; break;
			case RAW: fmt << "RAW"; break;
			case CONSTANT: fmt << "CONSTANT"; break;
			default:
				assert(false && "Unexpected pointer kind");
		}
		fmt.PadToColumn(112) << (TypeSupport::isImmutableType( v->getType()->getPointerElementType() ) ? "true" : "false" );
	}
	else
		fmt << " is not a pointer";
	fmt << '\n';
}

void dumpAllPointers(const Function & F, const PointerAnalyzer & analyzer)
{
	llvm::errs() << "Function: " << F.getName();
	if ( F.hasAddressTaken() )
		llvm::errs() << " (with address taken)";
	if ( F.getReturnType()->isPointerTy() )
	{
		llvm::errs() << " [";
		switch (analyzer.getPointerKindForReturn(&F))
		{
			case COMPLETE_OBJECT: llvm::errs() << "COMPLETE_OBJECT"; break;
			case REGULAR: llvm::errs() << "REGULAR"; break;
			case SPLIT_REGULAR: llvm::errs() << "SPLIT_REGULAR"; break;
			case BYTE_LAYOUT: llvm::errs() << "BYTE_LAYOUT"; break;
			default:
				assert(false && "Unexpected pointer kind");
		}
		llvm::errs() << ']';
	}

	llvm::errs() << "\n";

	for ( const Argument & arg : F.args() )
		analyzer.dumpPointer(&arg, false);

	for ( const BasicBlock & BB : F )
	{
		for ( const Instruction & I : BB )
		{
			if ( I.getType()->isPointerTy() )
				analyzer.dumpPointer(&I, false);
		}
	}
	llvm::errs() << "\n";
}

void writePointerDumpHeader()
{
	llvm::formatted_raw_ostream fmt( llvm::errs() );
	fmt.PadToColumn(0) << "Name";
	fmt.PadToColumn(92) << "Kind";
	fmt.PadToColumn(112) << "UsageFlags";
	fmt.PadToColumn(132) << "UsageFlagsComplete";
	fmt.PadToColumn(152) << "IsImmutable";
	fmt << '\n';
}

#endif //NDEBUG

}

using namespace cheerp;

INITIALIZE_PASS_BEGIN(PointerAnalyzer, "PointerAnalyzer", "Analyze the requirements of each pointers in the module",
			false, false)
INITIALIZE_PASS_END(PointerAnalyzer, "PointerAnalyzer", "Analyze the requirements of each pointers in the module",
			false, false)
