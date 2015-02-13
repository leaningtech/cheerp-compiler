//===-- PointerAnalyzer.cpp - The Cheerp JavaScript generator -------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

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
PointerConstantOffsetWrapper PointerConstantOffsetWrapper::staticDefaultValue(NULL, PointerConstantOffsetWrapper::INVALID);

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
	// 2) COMPLETE_OBJECT | Any = Any
	// 3) UNKNOWN | INDIRECT = UNKNOWN with constraints
	// 4) UNKNOWN | UNKNOWN = UNKNOWN with all constraints
	// 5) INDIRECT | INDIRECT = INDIRECT with all constraints
	PointerKindWrapper& lhs=*this;

	assert(lhs!=BYTE_LAYOUT && rhs!=BYTE_LAYOUT);
	
	// Handle 1
	if (lhs==REGULAR || rhs==REGULAR)
	{
		lhs.kind = REGULAR;
		lhs.clearConstraints();
		return lhs;
	}

	// Handle 2
	if (lhs==COMPLETE_OBJECT)
		return *this = rhs;
	if (rhs==COMPLETE_OBJECT)
		return *this;

	// Handle 3, 4, 5
	if (lhs==UNKNOWN || rhs==UNKNOWN)
		lhs.kind = UNKNOWN;

	lhs.constraints.insert(lhs.constraints.end(), rhs.constraints.begin(), rhs.constraints.end());
	return *this;
}

void PointerKindWrapper::dump() const
{
	if(kind==UNKNOWN)
		dbgs() << "Unknown kind\n";
	else if(kind==INDIRECT)
		dbgs() << "Indirect kind\n";
	else
		dbgs() << "Wraps plain kind " << kind << "\n";

	for(const IndirectPointerKindConstraint& c: constraints)
		c.dump();
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
		lhs.constraints.insert(lhs.constraints.end(), rhs.constraints.begin(), rhs.constraints.end());
	else
		assert(rhs.status != UNINITALIZED);

	// From now on both offsets can only be UNINITALIZED or VALID
	if(rhs.status == UNINITALIZED)
		return lhs;

	if(lhs.status == UNINITALIZED)
	{
		lhs.status = rhs.status;
		lhs.offset = rhs.offset;
		return lhs;
	}

	// Both are VALID, check if the are the same constant
	assert(rhs.offset != NULL && lhs.offset != NULL);

	if(rhs.offset != lhs.offset)
	{
		lhs.offset = NULL;
		lhs.status = INVALID;
		lhs.clearConstraints();
		return lhs;
	}

	// Both are valid and contain the same constant
	return lhs;
}

void PointerConstantOffsetWrapper::dump() const
{
	if(status==VALID)
		dbgs() << "Constant offset " << *offset << "\n";

	for(const IndirectPointerKindConstraint& c: constraints)
		c.dump();
}

char PointerAnalyzer::ID = 0;

const char* PointerAnalyzer::getPassName() const
{
	return "CheerpPointerAnalyzer";
}

void PointerAnalyzer::getAnalysisUsage(AnalysisUsage& AU) const
{
	AU.addPreserved<cheerp::GlobalDepsAnalyzer>();
	AU.addPreserved<cheerp::Registerize>();

	llvm::Pass::getAnalysisUsage(AU);
}

bool PointerAnalyzer::runOnModule(Module& M)
{
	for(const Function & F : M)
		prefetchFunc(F);

	for(const GlobalVariable & GV : M.getGlobalList())
	{
		if(!GV.hasInitializer())
			continue;
		if(!GV.getInitializer()->getType()->isStructTy())
			continue;
		getFinalPointerConstantOffsetWrapper(GV.getInitializer());
	}
	return false;
}

struct PointerUsageVisitor
{
	PointerUsageVisitor( PointerAnalyzer::PointerKindData& pointerKindData, PointerAnalyzer::AddressTakenMap& addressTakenCache ) :
			pointerKindData(pointerKindData), addressTakenCache(addressTakenCache) {}

	PointerKindWrapper& visitValue(PointerKindWrapper& ret, const Value* v, bool first);
	PointerKindWrapper& visitUse(PointerKindWrapper& ret, const Use* U);
	PointerKindWrapper& visitReturn(PointerKindWrapper& ret, const Function* F, bool first);
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
	PointerResolverBaseVisitor( const PointerAnalyzer::PointerData<T>& pointerData, PointerAnalyzer::AddressTakenMap& addressTakenCache ) :
				pointerData(pointerData) , addressTakenCache(addressTakenCache){}

	const T& resolveConstraint(const IndirectPointerKindConstraint& c);

	const PointerAnalyzer::PointerData<T>& pointerData;
	PointerAnalyzer::AddressTakenMap& addressTakenCache;
	std::unordered_set< IndirectPointerKindConstraint, IndirectPointerKindConstraint::Hash > closedset;
};

struct PointerResolverForKindVisitor: public PointerResolverBaseVisitor<PointerKindWrapper>
{
	PointerResolverForKindVisitor(const PointerAnalyzer::PointerKindData& pointerData, PointerAnalyzer::AddressTakenMap& addressTakenCache) :
				PointerResolverBaseVisitor<PointerKindWrapper>(pointerData, addressTakenCache){}
	const PointerKindWrapper& resolvePointerKind(const PointerKindWrapper& k);
};

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
				curType = curType->getSequentialElementType();
			}
		}
		return false;
	}

	if ( isBitCast(p))
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
	if (p->getType()->isPointerTy() && visitByteLayoutChain(p))
		return pointerKindData.valueMap.insert( std::make_pair(p, BYTE_LAYOUT ) ).first->second;

	if(pointerKindData.valueMap.count(p))
		return ret |= pointerKindData.valueMap.find(p)->second;

	if(!closedset.insert(p).second)
		return ret |= UNKNOWN;

	auto CacheAndReturn = [&](PointerKindWrapper& k) -> PointerKindWrapper&
	{
		// Do not recurse below here
		closedset.erase(p);
		if(first)
			k.makeKnown();
		if(!k.isKnown())
			return k;
		else
			return pointerKindData.valueMap.insert( std::make_pair(p, k ) ).first->second;
	};

	PointerAnalyzer::TypeAndIndex baseAndIndex = PointerAnalyzer::getBaseStructAndIndexFromGEP(p);
	if(baseAndIndex)
	{
		// If the pointer inside a structure has uses which are not just loads and stores
		// we have merge the BASE_AND_INDEX_CONSTRAINT with the STORED_TYPE_CONSTRAINT
		// We do this by making both indirectly dependent on the other
		if(p->getType()->getPointerElementType()->isPointerTy() && PointerAnalyzer::hasNonLoadStoreUses(p))
		{
			Type* pointedType = cast<StructType>(baseAndIndex.type)->getElementType(baseAndIndex.index)->getPointerElementType();
			pointerKindData.baseStructAndIndexMapForPointers[baseAndIndex] |= PointerKindWrapper( STORED_TYPE_CONSTRAINT, pointedType );
			pointerKindData.storedTypeMap[pointedType] |= PointerKindWrapper( BASE_AND_INDEX_CONSTRAINT, baseAndIndex.type, baseAndIndex.index );
		}

		if(first)
		{
			PointerKindWrapper& k = visitAllUses(ret, p);
			k.makeKnown();
			// In general, keep track of the contraints on elements of structures
			pointerKindData.baseStructAndIndexMapForMembers[baseAndIndex] |= k;
			return CacheAndReturn(k);
		}
	}

	if((isa<AllocaInst>(p) || isa<GlobalVariable>(p)) && !PointerAnalyzer::hasNonLoadStoreUses(p))
		return CacheAndReturn(ret |= COMPLETE_OBJECT);

	if(const StoreInst* SI=dyn_cast<StoreInst>(p))
	{
		assert(SI->getValueOperand()->getType()->isPointerTy());
		if(getKindForType(SI->getValueOperand()->getType()->getPointerElementType()) != UNKNOWN)
			return CacheAndReturn(ret |= getKindForType(SI->getValueOperand()->getType()->getPointerElementType()));
		if(PointerAnalyzer::TypeAndIndex baseAndIndex = PointerAnalyzer::getBaseStructAndIndexFromGEP(SI->getPointerOperand()))
			ret |= PointerKindWrapper( BASE_AND_INDEX_CONSTRAINT, baseAndIndex.type, baseAndIndex.index);
		else
			ret |= PointerKindWrapper( STORED_TYPE_CONSTRAINT, SI->getValueOperand()->getType()->getPointerElementType());
		// Only cache the result for SI when not recursing, otherwise we would poison the kind of SI with the other uses of valOp
		if(first)
			return CacheAndReturn(ret);
		else
			return ret;
	}

	llvm::Type * type = p->getType()->getPointerElementType();

	bool isIntrinsic = false;
	if ( const IntrinsicInst * intrinsic = dyn_cast<IntrinsicInst>(p) )
	{
		isIntrinsic = true;
		switch ( intrinsic->getIntrinsicID() )
		{
		case Intrinsic::cheerp_downcast:
		case Intrinsic::cheerp_upcast_collapsed:
		case Intrinsic::cheerp_cast_user:
			break;
		case Intrinsic::cheerp_allocate:
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
			return CacheAndReturn(ret |= REGULAR);
		case Intrinsic::memmove:
		case Intrinsic::memcpy:
		case Intrinsic::memset:
			return CacheAndReturn(visitValue(ret, intrinsic->getArgOperand(0), /*first*/ false));
		case Intrinsic::cheerp_pointer_offset:
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

	if(getKindForType(type) != UNKNOWN)
		return CacheAndReturn(ret |= getKindForType(type));

	if(const Argument* arg = dyn_cast<Argument>(p))
	{
		Type* argPointedType = arg->getType()->getPointerElementType();
		assert(first);
		PointerKindWrapper& k = visitAllUses(ret, p);
		k.makeKnown();
		pointerKindData.argsMap[arg] = k;

		// Keep track of the constraint for each argument/type pair, but only if the function is indirectly used
		if(addressTakenCache.checkAddressTaken(arg->getParent()))
		{
			// If the kind is COMPLETE_OBJECT it is ok to omit it from the constraints for this argument index and type
			if(k != COMPLETE_OBJECT)
			{
				pointerKindData.paramTypeMap[PointerAnalyzer::TypeAndIndex(argPointedType, arg->getArgNo(), PointerAnalyzer::TypeAndIndex::ARGUMENT)] |=
						PointerKindWrapper( DIRECT_ARG_CONSTRAINT_IF_ADDRESS_TAKEN, arg );
			}
			return CacheAndReturn(ret = PointerKindWrapper( DIRECT_ARG_CONSTRAINT, arg ) );
		}
		else
			return CacheAndReturn(k);
	}

	// TODO this is not really necessary,
	// but we need to modify the writer so that CallInst and InvokeInst
	// perform a demotion in place.
	if(ImmutableCallSite cs = p)
	{
		if (!isIntrinsic)
		{
			if(cs.getCalledFunction())
				return CacheAndReturn(ret |= PointerKindWrapper( RETURN_CONSTRAINT, cs.getCalledFunction() ));
			else
			{
				assert(first);
				PointerKindWrapper& k = visitAllUses(ret, p);
				k.makeKnown();
				pointerKindData.returnTypeMap[p->getType()] |= k;
				// We want to override the ret value, not add a constraint
				return CacheAndReturn(ret = PointerKindWrapper( RETURN_TYPE_CONSTRAINT, p->getType() ));
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
		if (PointerAnalyzer::TypeAndIndex b = PointerAnalyzer::getBaseStructAndIndexFromGEP(LI->getOperand(0)))
		{
			pointerKindData.baseStructAndIndexMapForPointers[b] |= k;
			return CacheAndReturn(ret = PointerKindWrapper( BASE_AND_INDEX_CONSTRAINT, b.type, b.index ) );
		}
		else
		{
			Type* curType = p->getType()->getPointerElementType();
			pointerKindData.storedTypeMap[curType] |= k;
			return CacheAndReturn(ret = PointerKindWrapper( STORED_TYPE_CONSTRAINT, curType ));
		}
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
		return ret |= REGULAR;
	}

	// Constant data in memory is considered stored
	if (isa<ConstantArray>(p) || isa<GlobalVariable>(p))
		return ret |= PointerKindWrapper( STORED_TYPE_CONSTRAINT, U->get()->getType()->getPointerElementType() );

	if (isa<ConstantStruct>(p))
	{
		// Build a TypeAndIndex struct to get the normalized type
		PointerAnalyzer::TypeAndIndex baseAndIndex(p->getType(), U->getOperandNo(), PointerAnalyzer::TypeAndIndex::STRUCT_MEMBER);
		return ret |= PointerKindWrapper( BASE_AND_INDEX_CONSTRAINT, baseAndIndex.type, baseAndIndex.index );
	}

	if ( (isa<StoreInst>(p) && U->getOperandNo() == 0 ))
		return visitValue(ret, p, /*first*/ false);

	if ( isa<PtrToIntInst>(p) || ( isa<ConstantExpr>(p) && cast<ConstantExpr>(p)->getOpcode() == Instruction::PtrToInt) )
		return ret |= REGULAR;

	if ( const CmpInst * I = dyn_cast<CmpInst>(p) )
	{
		if ( !I->isEquality() )
			return ret |= REGULAR;
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
				return ret |= REGULAR;
		}
		case Intrinsic::invariant_start:
		case Intrinsic::invariant_end:
		case Intrinsic::vastart:
		case Intrinsic::vaend:
		case Intrinsic::lifetime_start:
		case Intrinsic::lifetime_end:
		case Intrinsic::cheerp_element_distance:
		case Intrinsic::cheerp_deallocate:
		case Intrinsic::cheerp_make_regular:
		case Intrinsic::cheerp_make_complete_object:
			return ret |= COMPLETE_OBJECT;
		case Intrinsic::cheerp_downcast:
		case Intrinsic::cheerp_upcast_collapsed:
		case Intrinsic::cheerp_cast_user:
		{
			return visitValue( ret, p, /*first*/ false );
		}
		case Intrinsic::cheerp_reallocate:
		case Intrinsic::cheerp_pointer_base:
		case Intrinsic::cheerp_pointer_offset:
			return ret |= REGULAR;
		case Intrinsic::cheerp_create_closure:
			if ( U->getOperandNo() == 0)
				return ret |= COMPLETE_OBJECT;
			else if ( isa<Function>( p->getOperand(0) ) )
				return ret |= REGULAR;
			else
				llvm::report_fatal_error("Unreachable code in cheerp::PointerAnalyzer::visitUse, cheerp_create_closure");
		case Intrinsic::flt_rounds:
		case Intrinsic::cheerp_allocate:
		case Intrinsic::memset:
		default:
			SmallString<128> str("Unreachable code in cheerp::PointerAnalyzer::visitUse, unhandled intrinsic: ");
			str+=intrinsic->getCalledFunction()->getName();
			llvm::report_fatal_error(StringRef(str),false);
		}
		return ret |= REGULAR;
	}

	if ( ImmutableCallSite cs = p )
	{
		if ( cs.isCallee(U) )
			return ret |= COMPLETE_OBJECT;

		Type* pointedType = U->get()->getType()->getPointerElementType();

		// We need to check for basic type kinds here, because visitValue may have not done it,
		// for example when computing kinds for members
		if(getKindForType(pointedType) != UNKNOWN)
			return ret |= getKindForType(pointedType);

		const Function * calledFunction = cs.getCalledFunction();
		// TODO: Use function type
		if ( !calledFunction )
			return ret |= PointerKindWrapper(INDIRECT_ARG_CONSTRAINT, pointedType, U->getOperandNo());

		unsigned argNo = cs.getArgumentNo(U);

		if ( argNo >= calledFunction->arg_size() )
		{
			// Passed as a variadic argument
			return ret |= REGULAR;
		}

		Function::const_arg_iterator arg = calledFunction->arg_begin();
		std::advance(arg, argNo);
		return ret |= PointerKindWrapper(DIRECT_ARG_CONSTRAINT, arg);
	}

	if ( const ReturnInst * retInst = dyn_cast<ReturnInst>(p) )
	{
		return ret |= PointerKindWrapper(RETURN_CONSTRAINT, retInst->getParent()->getParent());
	}

	// Bitcasts from byte layout types require COMPLETE_OBJECT, and generate BYTE_LAYOUT
	if(isBitCast(p))
	{
		if (TypeSupport::hasByteLayout(p->getOperand(0)->getType()->getPointerElementType()))
			return ret |= COMPLETE_OBJECT;
		else
			return visitValue( ret, p, /*first*/ false );
	}

	if(isa<SelectInst> (p) || isa <PHINode>(p))
		return visitValue(ret, p, /*first*/ false);

	return ret |= COMPLETE_OBJECT;
}

PointerKindWrapper& PointerUsageVisitor::visitReturn(PointerKindWrapper& ret, const Function* F, bool first)
{
	assert(F);

	/**
	 * Note:
	 * we can not use F as the cache key here,
	 * since F is a pointer to function which might be used elsewhere.
	 * Hence we store the entry basic block.
	 */

	if(pointerKindData.valueMap.count(F->begin()))
		return ret |= pointerKindData.valueMap.find(F->begin())->second;

	if(!closedset.insert(F->begin()).second)
		return ret |= UNKNOWN;

	auto CacheAndReturn = [&](PointerKindWrapper& k) -> PointerKindWrapper&
	{
		closedset.erase(F);
		if(first)
			k.makeKnown();

		if (k.isKnown())
			return pointerKindData.valueMap.insert( std::make_pair(F->begin(), k ) ).first->second;
		return k;
	};

	Type* returnPointedType = F->getReturnType()->getPointerElementType();

	if(getKindForType(returnPointedType) != UNKNOWN)
		return CacheAndReturn(ret |= getKindForType(returnPointedType));

	if(addressTakenCache.checkAddressTaken(F))
		return CacheAndReturn(ret |= PointerKindWrapper(RETURN_TYPE_CONSTRAINT, F->getReturnType()));

	for(const Use& u : F->uses())
	{
		ImmutableCallSite cs = u.getUser();
		if(cs && cs.isCallee(&u))
			visitAllUses(ret, cs.getInstruction());

		if (ret==REGULAR)
			break;
	}
	return CacheAndReturn(ret);
}

POINTER_KIND PointerUsageVisitor::getKindForType(Type * tp)
{
	if ( tp->isFunctionTy() ||
		TypeSupport::isClientType( tp ) )
		return COMPLETE_OBJECT;

	if ( TypeSupport::isImmutableType( tp ) )
		return REGULAR;

	return UNKNOWN;
}

template<class T>
const T& PointerResolverBaseVisitor<T>::resolveConstraint(const IndirectPointerKindConstraint& c)
{
	switch(c.kind)
	{
		case RETURN_CONSTRAINT:
		{
			assert(pointerData.valueMap.count(c.funcPtr->begin()));
			return pointerData.valueMap.find(c.funcPtr->begin())->second;
		}
		case DIRECT_ARG_CONSTRAINT:
		{
			// If the function has its address taken we need to do the indirect arg check
			if (addressTakenCache.checkAddressTaken(c.argPtr->getParent()))
			{
				Type* argPointedType = c.argPtr->getType()->getPointerElementType();
				return resolveConstraint(IndirectPointerKindConstraint( INDIRECT_ARG_CONSTRAINT, argPointedType, c.argPtr->getArgNo()));
			}
			else
			{
				assert(pointerData.argsMap.count(c.argPtr));
				return pointerData.argsMap.find(c.argPtr)->second;
			}
		}
		case STORED_TYPE_CONSTRAINT:
		{
			// We will resolve this constraint indirectly through the storedTypeMap map
			const auto& it=pointerData.storedTypeMap.find(c.typePtr);
			if(it==pointerData.storedTypeMap.end())
				return T::staticDefaultValue;
			return it->second;
		}
		case RETURN_TYPE_CONSTRAINT:
		{
			// We will resolve this constraint indirectly through the returnTypeMap map
			const auto& it=pointerData.returnTypeMap.find(c.typePtr);
			if(it==pointerData.returnTypeMap.end())
				return T::staticDefaultValue;
			return it->second;
		}
		case BASE_AND_INDEX_CONSTRAINT:
		{
			// We will resolve this constraint indirectly through the baseStructAndIndexMapForPointers map
			const auto& it=pointerData.baseStructAndIndexMapForPointers.find(PointerAnalyzer::TypeAndIndex( c.typePtr, c.i,
											PointerAnalyzer::TypeAndIndex::STRUCT_MEMBER ));
			if(it==pointerData.baseStructAndIndexMapForPointers.end())
				return T::staticDefaultValue;
			return it->second;
		}
		case INDIRECT_ARG_CONSTRAINT:
		{
			Type* argPointedType = c.typePtr;
			const auto& it=pointerData.paramTypeMap.find(PointerAnalyzer::TypeAndIndex( argPointedType, c.i,
												PointerAnalyzer::TypeAndIndex::ARGUMENT ));
			if(it==pointerData.paramTypeMap.end())
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
	assert(false);
}

const PointerKindWrapper& PointerResolverForKindVisitor::resolvePointerKind(const PointerKindWrapper& k)
{
	assert(k==INDIRECT);
	for(uint32_t i=0;i<k.constraints.size();i++)
	{
		const PointerKindWrapper& retKind=resolveConstraint(k.constraints[i]);
		assert(retKind.isKnown());
		if(retKind==REGULAR || retKind==BYTE_LAYOUT)
			return retKind;
		else if(retKind==INDIRECT)
		{
			if(!closedset.insert(k.constraints[i]).second)
				continue;
			const PointerKindWrapper& resolvedKind=resolvePointerKind(retKind);
			if(resolvedKind==REGULAR)
				return resolvedKind;
		}
	}
	return PointerKindWrapper::staticDefaultValue;
}

struct PointerConstantOffsetVisitor
{
	PointerConstantOffsetVisitor( PointerAnalyzer::PointerOffsetData& pointerOffsetData ) : pointerOffsetData(pointerOffsetData) {}

	PointerConstantOffsetWrapper& visitValue(PointerConstantOffsetWrapper& ret, const Value* v);
	static const llvm::ConstantInt* getPointerOffsetFromGEP( const llvm::Value* v );

	PointerAnalyzer::PointerOffsetData& pointerOffsetData;
	llvm::DenseSet< const llvm::Value* > closedset;
};

struct PointerResolverForOffsetVisitor: public PointerResolverBaseVisitor<PointerConstantOffsetWrapper>
{
	PointerResolverForOffsetVisitor(const PointerAnalyzer::PointerOffsetData& pointerData, PointerAnalyzer::AddressTakenMap& addressTakenCache) :
				PointerResolverBaseVisitor<PointerConstantOffsetWrapper>(pointerData, addressTakenCache){}
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
				(*gep->op_begin())->getType(), indexes);
	if (containerType->isStructTy())
		return NULL;
	return dyn_cast<ConstantInt>(*std::prev(gep->op_end()));
}

PointerConstantOffsetWrapper& PointerConstantOffsetVisitor::visitValue(PointerConstantOffsetWrapper& ret, const Value* v)
{
	if(!closedset.insert(v).second)
		return ret;

	// Find out if the pointer offset can be a constant
	auto CacheAndReturn = [&](PointerConstantOffsetWrapper& o) -> PointerConstantOffsetWrapper&
	{
		// Do not recurse below here
		closedset.erase(v);
		if(o.isUninitialized() && !o.hasConstraints())
			return o;
		return pointerOffsetData.valueMap.insert( std::make_pair(v, o ) ).first->second;
	};

	if ( isGEP(v) )
	{
		if(PointerAnalyzer::TypeAndIndex b = PointerAnalyzer::getBaseStructAndIndexFromGEP(v))
		{
			if(PointerAnalyzer::hasNonLoadStoreUses(v))
				pointerOffsetData.baseStructAndIndexMapForPointers[b] |= PointerConstantOffsetWrapper( NULL, PointerConstantOffsetWrapper::INVALID );
		}
		return CacheAndReturn(ret |= getPointerOffsetFromGEP(v));
	}

	if(const StoreInst* SI=dyn_cast<StoreInst>(v))
	{
		assert(SI->getValueOperand()->getType()->isPointerTy());
		PointerConstantOffsetWrapper& o = visitValue(ret, SI->getValueOperand());

		if (PointerAnalyzer::TypeAndIndex baseAndIndex = PointerAnalyzer::getBaseStructAndIndexFromGEP(SI->getPointerOperand()))
		{
			pointerOffsetData.baseStructAndIndexMapForPointers[baseAndIndex] |= o;
			return CacheAndReturn(ret |= PointerConstantOffsetWrapper( BASE_AND_INDEX_CONSTRAINT, baseAndIndex.type, baseAndIndex.index));
		}
		else
			return CacheAndReturn(ret |= PointerConstantOffsetWrapper(NULL, PointerConstantOffsetWrapper::INVALID));
	}

	if(const LoadInst* LI=dyn_cast<LoadInst>(v))
	{
		if (PointerAnalyzer::TypeAndIndex baseAndIndex = PointerAnalyzer::getBaseStructAndIndexFromGEP(LI->getPointerOperand()))
			return CacheAndReturn(ret |= PointerConstantOffsetWrapper( BASE_AND_INDEX_CONSTRAINT, baseAndIndex.type, baseAndIndex.index));
	}

	if(isBitCast(v))
		return CacheAndReturn(visitValue(ret, cast<User>(v)->getOperand(0)));

	if(const PHINode* phi=dyn_cast<PHINode>(v))
	{
		for(uint32_t i=0;i<phi->getNumIncomingValues();i++)
		{
			const Value* incoming = phi->getIncomingValue(i);
			visitValue(ret, incoming);
		}
		return CacheAndReturn(ret);
	}

	if(const ConstantStruct* CS=dyn_cast<ConstantStruct>(v))
	{
		Type* structType = CS->getType();
		// We need to keep track of all offsets for each member
		for(uint32_t i=0;i<CS->getNumOperands();i++)
		{
			const Value* op = CS->getOperand(i);
			if(!op->getType()->isPointerTy())
				continue;
			PointerConstantOffsetWrapper localRet;
			PointerAnalyzer::TypeAndIndex typeAndIndex(structType, i, PointerAnalyzer::TypeAndIndex::STRUCT_MEMBER);
			pointerOffsetData.baseStructAndIndexMapForPointers[typeAndIndex] |= visitValue(localRet, op);
		}
		return CacheAndReturn(ret |= PointerConstantOffsetWrapper(NULL, PointerConstantOffsetWrapper::INVALID));
	}

	Type* Int32Ty=IntegerType::get(v->getContext(), 32);
	ConstantInt* Zero = cast<ConstantInt>(ConstantInt::get(Int32Ty, 0));
	if(isa<ConstantPointerNull>(v))
		return CacheAndReturn(ret |= Zero);

	if(const CallInst * CI = dyn_cast<CallInst>(v))
	{
		Function* F = CI->getCalledFunction();
		if(!F)
			return CacheAndReturn(ret |= PointerConstantOffsetWrapper(NULL, PointerConstantOffsetWrapper::INVALID));
		if(F->getIntrinsicID()==Intrinsic::cheerp_allocate ||
			F->getIntrinsicID()==Intrinsic::cheerp_reallocate)
		{
			return CacheAndReturn(ret |= Zero);
		}
	}

	return CacheAndReturn(ret |= PointerConstantOffsetWrapper(NULL, PointerConstantOffsetWrapper::INVALID));
}

PointerConstantOffsetWrapper PointerResolverForOffsetVisitor::resolvePointerOffset(const PointerConstantOffsetWrapper& o)
{
	assert(!o.isInvalid());
	assert(o.hasConstraints());
	// 'o' may be VALID (which means it contains a valid offset) or UNINITALIZED
	const llvm::ConstantInt* offset = o.isValid() ? o.getPointerOffset() : NULL;
	for(uint32_t i=0;i<o.constraints.size();i++)
	{
		if(!closedset.insert(o.constraints[i]).second)
			continue;
		const PointerConstantOffsetWrapper& c = resolveConstraint(o.constraints[i]);
		if(c.isInvalid())
			return PointerConstantOffsetWrapper(NULL, PointerConstantOffsetWrapper::INVALID);
		// 'c' is VALID or UNINITALIZED
		if(c.isValid())
		{
			if(offset == NULL)
				offset = c.getPointerOffset();
			else if(offset != c.getPointerOffset())
				return PointerConstantOffsetWrapper(NULL, PointerConstantOffsetWrapper::INVALID);
		}
		if(c.hasConstraints())
		{
			const PointerConstantOffsetWrapper& resolved = resolvePointerOffset(c);
			// No constrains allowed below here!
			assert(!resolved.hasConstraints());
			if(resolved.isInvalid())
				return PointerConstantOffsetWrapper(NULL, PointerConstantOffsetWrapper::INVALID);
			if(resolved.isUninitialized())
				continue;
			assert(resolved.isValid());
			if(offset == NULL)
				offset = resolved.getPointerOffset();
			else if(offset != resolved.getPointerOffset())
				return PointerConstantOffsetWrapper(NULL, PointerConstantOffsetWrapper::INVALID);
		}
	}
	// No constrains were INVALID, so if offset is still NULL it means we should be UNINITALIZED
	// Otherwise we found a valid value
	if (offset == NULL)
		return PointerConstantOffsetWrapper(NULL, PointerConstantOffsetWrapper::UNINITALIZED);
	else
		return PointerConstantOffsetWrapper(offset, PointerConstantOffsetWrapper::VALID);
}

struct TimerGuard
{
	TimerGuard(Timer & timer) : timer(timer)
	{
		timer.startTimer();
	}
	~TimerGuard()
	{
		timer.stopTimer();
	}

	Timer & timer;
};

void PointerAnalyzer::prefetchFunc(const Function& F) const
{
	for(const Argument & arg : F.getArgumentList())
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
	if(F.getReturnType()->isPointerTy())
		getFinalPointerKindWrapperForReturn(&F);
}

const PointerKindWrapper& PointerAnalyzer::getFinalPointerKindWrapper(const Value* p) const
{
#ifndef NDEBUG
	TimerGuard guard(gpkTimer);
#endif //NDEBUG

	// If the values is already cached just return it
	auto it = pointerKindData.valueMap.find(p);
	if(it!=pointerKindData.valueMap.end())
		return it->second;

	PointerKindWrapper ret;
	PointerKindWrapper& k = PointerUsageVisitor(pointerKindData, addressTakenCache).visitValue(ret, p, /*first*/ true);
#ifndef NDEBUG
	it = pointerKindData.valueMap.find(p);
	assert(it!=pointerKindData.valueMap.end());
	assert(&it->second == &k);
#endif
	return k;
}

const PointerKindWrapper& PointerAnalyzer::getFinalPointerKindWrapperForReturn(const Function* F) const
{
#ifndef NDEBUG
	TimerGuard guard(gpkfrTimer);
#endif //NDEBUG
	// If the values is already cached just return it
	auto it = pointerKindData.valueMap.find(F->begin());
	if(it!=pointerKindData.valueMap.end())
		return it->second;

	PointerKindWrapper ret;
	PointerKindWrapper& k = PointerUsageVisitor(pointerKindData, addressTakenCache).visitReturn(ret, F, /* first*/ true);
#ifndef NDEBUG
	it = pointerKindData.valueMap.find(F->begin());
	assert(it!=pointerKindData.valueMap.end());
	assert(&it->second == &k);
#endif
	return k;
}

const PointerConstantOffsetWrapper& PointerAnalyzer::getFinalPointerConstantOffsetWrapper(const Value* p) const
{
	// If the values is already cached just return it
	auto it = pointerOffsetData.valueMap.find(p);
	if(it!=pointerOffsetData.valueMap.end())
		return it->second;

	PointerConstantOffsetWrapper ret;
	PointerConstantOffsetWrapper& o = PointerConstantOffsetVisitor(pointerOffsetData).visitValue(ret, p);
#ifndef NDEBUG
	it = pointerOffsetData.valueMap.find(p);
	assert(it!=pointerOffsetData.valueMap.end());
	assert(&it->second == &o);
#endif
	return o;
}

POINTER_KIND PointerAnalyzer::getPointerKind(const Value* p) const
{
#ifndef NDEBUG
	TimerGuard guard(gpkTimer);
#endif //NDEBUG
	const PointerKindWrapper& k = getFinalPointerKindWrapper(p);

	if (k!=INDIRECT)
		return k.getPointerKind();

	// Got an indirect value, we need to resolve it now
	return PointerResolverForKindVisitor(pointerKindData, addressTakenCache).resolvePointerKind(k).getPointerKind();
}

POINTER_KIND PointerAnalyzer::getPointerKindForReturn(const Function* F) const
{
#ifndef NDEBUG
	TimerGuard guard(gpkfrTimer);
#endif //NDEBUG
	const PointerKindWrapper& k = getFinalPointerKindWrapperForReturn(F);
	if (k!=INDIRECT)
		return k.getPointerKind();

	return PointerResolverForKindVisitor(pointerKindData, addressTakenCache).resolvePointerKind(k).getPointerKind();
}

POINTER_KIND PointerAnalyzer::getPointerKindForStoredType(Type* pointerType) const
{
	POINTER_KIND ret=PointerUsageVisitor::getKindForType(pointerType->getPointerElementType());
	if(ret!=UNKNOWN)
		return ret;

	auto it=pointerKindData.storedTypeMap.find(pointerType->getPointerElementType());
	if(it==pointerKindData.storedTypeMap.end())
		return COMPLETE_OBJECT;

	const PointerKindWrapper& k = it->second;
	assert(k!=UNKNOWN);
	if (k!=INDIRECT)
		return k.getPointerKind();

	return PointerResolverForKindVisitor(pointerKindData, addressTakenCache).resolvePointerKind(k).getPointerKind();
}

POINTER_KIND PointerAnalyzer::getPointerKindForArgumentType(Type* pointerType) const
{
	return PointerUsageVisitor::getKindForType(pointerType->getPointerElementType());
}

POINTER_KIND PointerAnalyzer::getPointerKindForArgumentTypeAndIndex( const TypeAndIndex& argTypeAndIndex ) const
{
	Type* pointedType = argTypeAndIndex.type;
	uint32_t argNo = argTypeAndIndex.index;
	POINTER_KIND ret=PointerUsageVisitor::getKindForType(pointedType);
	if(ret!=UNKNOWN)
		return ret;

	const PointerKindWrapper& k=PointerResolverForKindVisitor(pointerKindData, addressTakenCache).resolveConstraint(
										IndirectPointerKindConstraint(INDIRECT_ARG_CONSTRAINT, pointedType, argNo));
	assert(k!=UNKNOWN);

	if (k!=INDIRECT)
		return k.getPointerKind();

	return PointerResolverForKindVisitor(pointerKindData, addressTakenCache).resolvePointerKind(k).getPointerKind();
}

POINTER_KIND PointerAnalyzer::getPointerKindForMemberPointer(const TypeAndIndex& baseAndIndex) const
{
	Type* elementType = cast<StructType>(baseAndIndex.type)->getElementType(baseAndIndex.index);
	POINTER_KIND ret=PointerUsageVisitor::getKindForType(elementType->getPointerElementType());
	if(ret!=UNKNOWN)
		return ret;

	auto it=pointerKindData.baseStructAndIndexMapForPointers.find(baseAndIndex);
	if(it==pointerKindData.baseStructAndIndexMapForPointers.end())
		return COMPLETE_OBJECT;

	const PointerKindWrapper& k = it->second;
	assert(k!=UNKNOWN);

	if (k!=INDIRECT)
		return k.getPointerKind();

	return PointerResolverForKindVisitor(pointerKindData, addressTakenCache).resolvePointerKind(k).getPointerKind();
}

POINTER_KIND PointerAnalyzer::getPointerKindForMember(const TypeAndIndex& baseAndIndex) const
{
	auto it=pointerKindData.baseStructAndIndexMapForMembers.find(baseAndIndex);
	if(it==pointerKindData.baseStructAndIndexMapForMembers.end())
		return COMPLETE_OBJECT;

	const PointerKindWrapper& k = it->second;
	assert(k!=UNKNOWN);

	if (k!=INDIRECT)
		return k.getPointerKind();

	return PointerResolverForKindVisitor(pointerKindData, addressTakenCache).resolvePointerKind(k).getPointerKind();
}

PointerAnalyzer::TypeAndIndex PointerAnalyzer::getBaseStructAndIndexFromGEP(const Value* p)
{
	if(!isGEP(p))
		return PointerAnalyzer::TypeAndIndex(NULL, 0, PointerAnalyzer::TypeAndIndex::STRUCT_MEMBER);
	const User* gep=cast<User>(p);
	if (gep->getNumOperands() == 2)
		return PointerAnalyzer::TypeAndIndex(NULL, 0, PointerAnalyzer::TypeAndIndex::STRUCT_MEMBER);

	SmallVector<Value*, 4> indexes;
	for(uint32_t i=1;i<gep->getNumOperands()-1;i++)
		indexes.push_back(*(gep->op_begin()+i));
	Type* containerType = GetElementPtrInst::getIndexedType(
				(*gep->op_begin())->getType(), indexes);

	if (containerType->isStructTy())
	{
		Value* lastIndex = *std::prev(gep->op_end());
		assert(isa<ConstantInt>(lastIndex));
		uint32_t lastOffsetConstant =  cast<ConstantInt>(lastIndex)->getZExtValue();
		return PointerAnalyzer::TypeAndIndex(containerType, lastOffsetConstant, PointerAnalyzer::TypeAndIndex::STRUCT_MEMBER);
	}
	return PointerAnalyzer::TypeAndIndex(NULL, 0, PointerAnalyzer::TypeAndIndex::STRUCT_MEMBER);
}

bool PointerAnalyzer::hasNonLoadStoreUses( const Value* v)
{
	for(const Use& U: v->uses())
	{
		const User* user = U.getUser();
		if (isa<LoadInst>(user))
			continue;
		if (isa<StoreInst>(user) && U.getOperandNo()==1)
			continue;
		return true;
	}
	return false;
}

const ConstantInt* PointerAnalyzer::getConstantOffsetForPointer(const Value * v) const
{
	auto it=pointerOffsetData.valueMap.find(v);
	if(it==pointerOffsetData.valueMap.end())
		return NULL;

	if(!it->second.hasConstraints())
	{
		assert(!it->second.isUninitialized());
		if(it->second.isInvalid())
			return NULL;
		else if(it->second.isValid())
			return it->second.getPointerOffset();
	}
	assert(!it->second.isInvalid());
	const PointerConstantOffsetWrapper& ret=PointerResolverForOffsetVisitor(pointerOffsetData, addressTakenCache).resolvePointerOffset(it->second);
	if(ret.isInvalid() || ret.isUninitialized())
		return NULL;
	assert(ret.isValid());
	assert(ret.getPointerOffset());
	return ret.getPointerOffset();
}

const llvm::ConstantInt* PointerAnalyzer::getConstantOffsetForMember( const TypeAndIndex& baseAndIndex ) const
{
	auto it=pointerOffsetData.baseStructAndIndexMapForPointers.find(baseAndIndex);
	if(it==pointerOffsetData.baseStructAndIndexMapForPointers.end())
		return NULL;

	if(!it->second.hasConstraints())
	{
		assert(!it->second.isUninitialized());
		if(it->second.isInvalid())
			return NULL;
		else if(it->second.isValid())
			return it->second.getPointerOffset();
	}
	assert(!it->second.isInvalid());
	const PointerConstantOffsetWrapper& ret=PointerResolverForOffsetVisitor(pointerOffsetData, addressTakenCache).resolvePointerOffset(it->second);
	if(ret.isInvalid() || ret.isUninitialized())
		return NULL;
	assert(ret.isValid());
	assert(ret.getPointerOffset());
	return ret.getPointerOffset();
}

void PointerAnalyzer::invalidate(const Value * v)
{
#ifndef NDEBUG
	assert(!fullyResolved);
#endif
	if ( pointerKindData.valueMap.erase(v) )
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
		for ( const Argument & arg : F->getArgumentList() )
			if (arg.getType()->isPointerTy())
				invalidate(&arg);
		addressTakenCache.erase(F);
		prefetchFunc(*F);
		//TODO: Return
	}
	// Constant offsets should only be computed at the end, so they should be never invalidated
	assert( !pointerOffsetData.valueMap.count(v) );
}

void PointerAnalyzer::fullResolve()
{
	for(auto& it: pointerKindData.valueMap)
	{
		if(it.second!=INDIRECT)
			continue;
		const PointerKindWrapper& k=PointerResolverForKindVisitor(pointerKindData, addressTakenCache).resolvePointerKind(it.second);
		assert(k==COMPLETE_OBJECT || k==BYTE_LAYOUT || k==REGULAR);
		it.second=k;
	}
	for(auto& it: pointerKindData.argsMap)
	{
		if(it.second!=INDIRECT)
			continue;
		const PointerKindWrapper& k=PointerResolverForKindVisitor(pointerKindData, addressTakenCache).resolvePointerKind(it.second);
		assert(k==COMPLETE_OBJECT || k==BYTE_LAYOUT || k==REGULAR);
		it.second=k;
	}
	for(auto& it: pointerKindData.storedTypeMap)
	{
		if(it.second!=INDIRECT)
			continue;
		const PointerKindWrapper& k=PointerResolverForKindVisitor(pointerKindData, addressTakenCache).resolvePointerKind(it.second);
		assert(k==COMPLETE_OBJECT || k==BYTE_LAYOUT || k==REGULAR);
		it.second=k;
	}
	for(auto& it: pointerKindData.baseStructAndIndexMapForPointers)
	{
		if(it.second!=INDIRECT)
			continue;
		const PointerKindWrapper& k=PointerResolverForKindVisitor(pointerKindData, addressTakenCache).resolvePointerKind(it.second);
		assert(k==COMPLETE_OBJECT || k==BYTE_LAYOUT || k==REGULAR);
		it.second=k;
	}
	for(auto& it: pointerKindData.baseStructAndIndexMapForMembers)
	{
		if(it.second!=INDIRECT)
			continue;
		const PointerKindWrapper& k=PointerResolverForKindVisitor(pointerKindData, addressTakenCache).resolvePointerKind(it.second);
		// BYTE_LAYOUT is not expected for the kind of pointers to member
		assert(k==COMPLETE_OBJECT || k==REGULAR);
		it.second=k;
	}
	for(auto& it: pointerKindData.paramTypeMap)
	{
		if(it.second!=INDIRECT)
			continue;
		const PointerKindWrapper& k=PointerResolverForKindVisitor(pointerKindData, addressTakenCache).resolvePointerKind(it.second);
		assert(k==COMPLETE_OBJECT || k==BYTE_LAYOUT || k==REGULAR);
		it.second=k;
	}
#ifndef NDEBUG
	fullyResolved = true;
#endif
}

void PointerAnalyzer::computeConstantOffsets(const Module& M)
{
#ifndef NDEBUG
	assert(fullyResolved);
#endif
	for(const Function & F : M)
	{
		for(const BasicBlock & BB : F)
		{
			for(auto it=BB.rbegin();it != BB.rend();++it)
			{
				if(it->getType()->isPointerTy() ||
					(isa<StoreInst>(*it) && it->getOperand(0)->getType()->isPointerTy()))
				{
					getFinalPointerConstantOffsetWrapper(&(*it));
				}
			}
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
			case BYTE_LAYOUT: fmt << "BYTE_LAYOUT"; break;
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
			case BYTE_LAYOUT: llvm::errs() << "BYTE_LAYOUT"; break;
			default:
				assert(false && "Unexpected pointer kind");
		}
		llvm::errs() << ']';
	}

	llvm::errs() << "\n";

	for ( const Argument & arg : F.getArgumentList() )
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
