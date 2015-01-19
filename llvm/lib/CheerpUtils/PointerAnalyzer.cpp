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

PointerKindWrapper PointerAnalyzer::staticCompleteObjectKind(COMPLETE_OBJECT);

void IndirectPointerKindConstraint::dump() const
{
	switch(kind)
	{
		case RETURN_CONSTRAINT:
			dbgs() << "\tDepends on return value of: " << funcPtr->getName() << "\n";
			break;
		case DIRECT_ARG_CONSTRAINT:
			dbgs() << "\tDepends on argument " << i << " of " << funcPtr->getName() << "\n";
			break;
		case STORED_TYPE_CONSTRAINT:
			dbgs() << "Depends on stored type " << *typePtr << "\n";
			break;
		case RETURN_TYPE_CONSTRAINT:
			dbgs() << "Depends on returned type " << *typePtr << "\n";
			break;
		default:
			assert(false);
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
	return false;
}

struct PointerUsageVisitor
{
	PointerUsageVisitor( PointerAnalyzer::PointerKindData& pointerKindData ) : pointerKindData(pointerKindData) {}

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
	llvm::DenseSet< const llvm::Value* > closedset;
};

struct PointerResolverVisitor
{
	PointerResolverVisitor( const PointerAnalyzer::PointerKindData& pointerKindData ) : pointerKindData(pointerKindData) {}

	const PointerKindWrapper& resolvePointerKind(const PointerKindWrapper& k);
	const PointerKindWrapper& resolveConstraint(const IndirectPointerKindConstraint& c);

	const PointerAnalyzer::PointerKindData& pointerKindData;
	std::set< IndirectPointerKindConstraint > closedset;
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
		return pointerKindData.valueCache.insert( std::make_pair(p, BYTE_LAYOUT ) ).first->second;

	if(pointerKindData.valueCache.count(p))
		return ret |= pointerKindData.valueCache.find(p)->second;

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
			return pointerKindData.valueCache.insert( std::make_pair(p, k ) ).first->second;
	};

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
		if(pointerKindData.addressTakenCache.checkAddressTaken(arg->getParent()))
			return CacheAndReturn(ret |= REGULAR);
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
	if(isa<LoadInst>(p))
	{
		assert(first);
		PointerKindWrapper& k = visitAllUses(ret, p);
		k.makeKnown();
		Type* curType = p->getType()->getPointerElementType();
		pointerKindData.storedTypeMap[curType] |= k;
		// We want to override the ret value, not add a constraint
		return CacheAndReturn(ret = PointerKindWrapper( STORED_TYPE_CONSTRAINT, curType ));
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

	// Constant data in memory is equivalent to store
	if ( (isa<StoreInst>(p) && U->getOperandNo() == 0 ) ||
		isa<ConstantStruct>(p) || isa<ConstantArray>(p))
	{
		return ret |= PointerKindWrapper( STORED_TYPE_CONSTRAINT, U->get()->getType()->getPointerElementType() );
	}

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
			assert( U->getOperandNo() == 1 );
			if ( const Function * f = dyn_cast<Function>(p->getOperand(0) ) )
			{
				return ret |= REGULAR;
			}
			else
				llvm::report_fatal_error("Unreachable code in cheerp::PointerAnalyzer::visitUse, cheerp_create_closure");
		case Intrinsic::cheerp_make_complete_object:
			return ret |= COMPLETE_OBJECT;
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

		const Function * calledFunction = cs.getCalledFunction();
		if ( !calledFunction )
			return ret |= REGULAR;

		unsigned argNo = cs.getArgumentNo(U);

		if ( argNo >= calledFunction->arg_size() )
		{
			// Passed as a variadic argument
			return ret |= REGULAR;
		}

		return ret |= PointerKindWrapper(DIRECT_ARG_CONSTRAINT, calledFunction, argNo);
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

	if ( isa<Constant>(p) )
		return ret |= REGULAR;

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

	if(pointerKindData.valueCache.count(F->begin()))
		return ret |= pointerKindData.valueCache.find(F->begin())->second;

	if(!closedset.insert(F->begin()).second)
		return ret |= UNKNOWN;

	auto CacheAndReturn = [&](PointerKindWrapper& k) -> PointerKindWrapper&
	{
		closedset.erase(F);
		if(first)
			k.makeKnown();

		if (k.isKnown())
			return pointerKindData.valueCache.insert( std::make_pair(F->begin(), k ) ).first->second;
		return k;
	};

	Type* returnPointedType = F->getReturnType()->getPointerElementType();

	if(getKindForType(returnPointedType) != UNKNOWN)
		return CacheAndReturn(ret |= getKindForType(returnPointedType));

	if(pointerKindData.addressTakenCache.checkAddressTaken(F))
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

	return (POINTER_KIND)UNKNOWN;
}

const PointerKindWrapper& PointerResolverVisitor::resolveConstraint(const IndirectPointerKindConstraint& c)
{
	switch(c.kind)
	{
		case RETURN_CONSTRAINT:
		{
			assert(pointerKindData.valueCache.count(c.funcPtr->begin()));
			return pointerKindData.valueCache.find(c.funcPtr->begin())->second;
		}
		case DIRECT_ARG_CONSTRAINT:
		{
			Function::const_arg_iterator arg = c.funcPtr->arg_begin();
			std::advance(arg, c.i);
			assert(pointerKindData.valueCache.count(arg));
			return pointerKindData.valueCache.find(arg)->second;

		}
		case STORED_TYPE_CONSTRAINT:
		{
			// We will resolve this constraint indirectly through the storedTypeMap map
			const auto& it=pointerKindData.storedTypeMap.find(c.typePtr);
			if(it==pointerKindData.storedTypeMap.end())
				return PointerAnalyzer::staticCompleteObjectKind;
			return it->second;
		}
		case RETURN_TYPE_CONSTRAINT:
		{
			// We will resolve this constraint indirectly through the returnTypeMap map
			const auto& it=pointerKindData.returnTypeMap.find(c.typePtr);
			if(it==pointerKindData.returnTypeMap.end())
				return PointerAnalyzer::staticCompleteObjectKind;
			return it->second;
		}
	}
	assert(false);
}

const PointerKindWrapper& PointerResolverVisitor::resolvePointerKind(const PointerKindWrapper& k)
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
	return PointerAnalyzer::staticCompleteObjectKind;
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
			if(it->getType()->isPointerTy())
				getFinalPointerKindWrapper(&(*it));
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
	auto it = pointerKindData.valueCache.find(p);
	if(it!=pointerKindData.valueCache.end())
		return it->second;

	PointerKindWrapper ret;
	PointerKindWrapper& k = PointerUsageVisitor(pointerKindData).visitValue(ret, p, /*first*/ true);
#ifndef NDEBUG
	it = pointerKindData.valueCache.find(p);
	assert(it!=pointerKindData.valueCache.end());
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
	auto it = pointerKindData.valueCache.find(F->begin());
	if(it!=pointerKindData.valueCache.end())
		return it->second;

	PointerKindWrapper ret;
	PointerKindWrapper& k = PointerUsageVisitor(pointerKindData).visitReturn(ret, F, /* first*/ true);
#ifndef NDEBUG
	it = pointerKindData.valueCache.find(F->begin());
	assert(it!=pointerKindData.valueCache.end());
	assert(&it->second == &k);
#endif
	return k;
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
	return PointerResolverVisitor(pointerKindData).resolvePointerKind(k).getPointerKind();
}

POINTER_KIND PointerAnalyzer::getPointerKindForReturn(const Function* F) const
{
#ifndef NDEBUG
	TimerGuard guard(gpkfrTimer);
#endif //NDEBUG
	const PointerKindWrapper& k = getFinalPointerKindWrapperForReturn(F);
	if (k!=INDIRECT)
		return k.getPointerKind();

	return PointerResolverVisitor(pointerKindData).resolvePointerKind(k).getPointerKind();
}

POINTER_KIND PointerAnalyzer::getPointerKindForStoredType(Type* pointerType) const
{
	POINTER_KIND ret=PointerUsageVisitor(pointerKindData).getKindForType(pointerType->getPointerElementType());
	if(ret!=UNKNOWN)
		return ret;

	auto it=pointerKindData.storedTypeMap.find(pointerType->getPointerElementType());
	if(it==pointerKindData.storedTypeMap.end())
		return COMPLETE_OBJECT;

	const PointerKindWrapper& k = it->second;
	assert(k!=UNKNOWN);
	if (k!=INDIRECT)
		return k.getPointerKind();

	return PointerResolverVisitor(pointerKindData).resolvePointerKind(k).getPointerKind();
}

POINTER_KIND PointerAnalyzer::getPointerKindForArgumentType(Type* pointerType) const
{
	return PointerUsageVisitor(pointerKindData).getKindForType(pointerType->getPointerElementType());
}

void PointerAnalyzer::invalidate(const Value * v)
{
#ifndef NDEBUG
	assert(!fullyResolved);
#endif
	if ( pointerKindData.valueCache.erase(v) )
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
		pointerKindData.addressTakenCache.erase(F);
		prefetchFunc(*F);
		//TODO: Return
	}
}

void PointerAnalyzer::fullResolve()
{
	for(auto& it: pointerKindData.valueCache)
	{
		if(it.second!=INDIRECT)
			continue;
		const PointerKindWrapper& k=PointerResolverVisitor(pointerKindData).resolvePointerKind(it.second);
		assert(k==COMPLETE_OBJECT || k==BYTE_LAYOUT || k==REGULAR);
		it.second=k;
	}
	for(auto& it: pointerKindData.storedTypeMap)
	{
		if(it.second!=INDIRECT)
			continue;
		const PointerKindWrapper& k=PointerResolverVisitor(pointerKindData).resolvePointerKind(it.second);
		assert(k==COMPLETE_OBJECT || k==REGULAR);
		it.second=k;
	}
#ifndef NDEBUG
	fullyResolved = true;
#endif
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
