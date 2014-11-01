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

#include "llvm/Cheerp/PointerAnalyzer.h"
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

PointerKindWrapper PointerKindWrapper::operator|(const PointerKindWrapper & rhs)
{
	PointerKindWrapper ret=*this;
	ret |= rhs;
	return ret;
}

PointerKindWrapper& PointerKindWrapper::operator|=(const PointerKindWrapper & rhs)
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

	lhs.returnConstraints.insert(lhs.returnConstraints.end(), rhs.returnConstraints.begin(), rhs.returnConstraints.end());
	lhs.argsConstraints.insert(lhs.argsConstraints.end(), rhs.argsConstraints.begin(), rhs.argsConstraints.end());
	lhs.storedTypeConstraints.insert(lhs.storedTypeConstraints.end(), rhs.storedTypeConstraints.begin(), rhs.storedTypeConstraints.end());
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

	for(const llvm::Function* f: returnConstraints)
		dbgs() << "\tDepends on return value of: " << f->getName() << "\n";
	for(auto& it: argsConstraints)
		llvm::errs() << "\tDepends on argument " << it.second << " of " << it.first->getName() << "\n";
	for(llvm::Type* t: storedTypeConstraints)
		llvm::errs() << "Depends on stored type " << *t << "\n";
}

char PointerAnalyzer::ID = 0;

const char* PointerAnalyzer::getPassName() const
{
	return "CheerpPointerAnalyzer";
}

bool PointerAnalyzer::runOnModule(Module& M)
{
	prefetch(M);
	return false;
}

enum VISITED_KIND { RETURN_CONSTRAINT, DIRECT_ARG_CONSTRAINT, STORED_TYPE_CONSTRAINT };

struct PointerUsageVisitor
{
	typedef llvm::DenseSet< const llvm::Value* > visited_set_t;
	struct ResolveVisitedItem
	{
		VISITED_KIND kind;
		const void* ptr;
		uint32_t i;
		bool operator<(const ResolveVisitedItem& rhs) const
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
	};
	typedef std::set< ResolveVisitedItem > resolve_visited_set_t;
	typedef PointerAnalyzer::ValueKindMap value_kind_map_t;
	typedef PointerAnalyzer::TypeKindMap type_kind_map_t;
	typedef PointerAnalyzer::AddressTakenMap address_taken_map_t;

	PointerUsageVisitor( value_kind_map_t & cache, type_kind_map_t & typeCache, address_taken_map_t & cachedAddressTaken ) : cachedValues(cache), cachedTypes(typeCache), cachedAddressTaken( cachedAddressTaken ) {}

	PointerKindWrapper visitValue(const Value* v);
	PointerKindWrapper visitUse(const Use* U);
	PointerKindWrapper visitReturn(const Function* F);
	POINTER_KIND resolvePointerKind(const PointerKindWrapper& k, resolve_visited_set_t& closedset);
	bool visitByteLayoutChain ( const Value * v );
	POINTER_KIND getKindForType(Type*) const;

	PointerKindWrapper visitAllUses(const Value* v)
	{
		PointerKindWrapper result = COMPLETE_OBJECT;
		for(const Use& u : v->uses())
		{
			result = result | visitUse(&u);
			if (result==REGULAR)
				break;
		}
		return result;
	}

	Type * realType( const Value * v ) const
	{
		assert( v->getType()->isPointerTy() );
		if ( isBitCast(v) )
			v = cast<User>(v)->getOperand(0);
		return v->getType()->getPointerElementType();
	}

	value_kind_map_t & cachedValues;
	type_kind_map_t & cachedTypes;
	address_taken_map_t & cachedAddressTaken;
	visited_set_t closedset;
};

bool PointerUsageVisitor::visitByteLayoutChain( const Value * p )
{
	if ( TypeSupport::hasByteLayout(p->getType()->getPointerElementType()) && visitValue(p) != COMPLETE_OBJECT)
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

PointerKindWrapper PointerUsageVisitor::visitValue(const Value* p)
{
	if( cachedValues.count(p) )
		return cachedValues.find(p)->second;

	if(!closedset.insert(p).second)
		return {PointerKindWrapper::UNKNOWN};

	auto CacheAndReturn = [&](const PointerKindWrapper& k)
	{
		closedset.erase(p);
		if (k.isKnown())
			return (const PointerKindWrapper&)cachedValues.insert( std::make_pair(p, k ) ).first->second;
		return k;
	};

	llvm::Type * type = realType(p);

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
			if(getKindForType(type) != COMPLETE_OBJECT && visitAllUses(p) != COMPLETE_OBJECT)
			{
				llvm::errs() << "Result of " << *intrinsic << " used as REGULAR: " << *p << "\n";
				llvm::report_fatal_error("Unsupported code found, please report a bug", false);
			}
			return CacheAndReturn(COMPLETE_OBJECT);
		case Intrinsic::cheerp_make_regular:
			return CacheAndReturn(REGULAR);
		case Intrinsic::memmove:
		case Intrinsic::memcpy:
		case Intrinsic::memset:
			return CacheAndReturn(visitValue(intrinsic->getArgOperand(0)));
		case Intrinsic::cheerp_pointer_offset:
		case Intrinsic::invariant_start:
			return CacheAndReturn(visitValue(intrinsic->getArgOperand(1)));
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

	if(getKindForType(type) == COMPLETE_OBJECT)
	{
		return CacheAndReturn(COMPLETE_OBJECT);
	}

	if(TypeSupport::isImmutableType(type))
	{
		return CacheAndReturn(REGULAR);
	}

	if(isa<LoadInst>(p))
		return CacheAndReturn(REGULAR);

	if(const Argument* arg = dyn_cast<Argument>(p))
	{
		if(cachedAddressTaken.checkAddressTaken(arg->getParent()))
			return CacheAndReturn(REGULAR);
	}

	// TODO this is not really necessary,
	// but we need to modify the writer so that CallInst and InvokeInst
	// perform a demotion in place.
	if(ImmutableCallSite cs = p)
	{
		if (!isIntrinsic)
			return CacheAndReturn(cs.getCalledFunction());
	}

	return CacheAndReturn(visitAllUses(p));
}

PointerKindWrapper PointerUsageVisitor::visitUse(const Use* U)
{
	const User * p = U->getUser();
	if ( isGEP(p) )
	{
		const Constant * constOffset = dyn_cast<Constant>( p->getOperand(1) );
		
		if ( constOffset && constOffset->isNullValue() )
		{
			if ( p->getNumOperands() == 2 )
				return visitValue( p );
			return COMPLETE_OBJECT;
		}
		
		return REGULAR;
	}

	if ( isa<StoreInst>(p) && U->getOperandNo() == 0 )
		return REGULAR;

	if ( isa<PtrToIntInst>(p) || ( isa<ConstantExpr>(p) && cast<ConstantExpr>(p)->getOpcode() == Instruction::PtrToInt) )
		return REGULAR;

	if ( const CmpInst * I = dyn_cast<CmpInst>(p) )
	{
		if ( !I->isEquality() )
			return REGULAR;
		else
			return COMPLETE_OBJECT;
	}

	if ( const IntrinsicInst * intrinsic = dyn_cast<IntrinsicInst>(p) )
	{
		switch ( intrinsic->getIntrinsicID() )
		{
		case Intrinsic::memmove:
		case Intrinsic::memcpy:
		{
			if (TypeSupport::hasByteLayout(intrinsic->getOperand(0)->getType()->getPointerElementType()))
				return COMPLETE_OBJECT;
			else
				return REGULAR;
		}
		case Intrinsic::invariant_start:
		case Intrinsic::invariant_end:
		case Intrinsic::vastart:
		case Intrinsic::vaend:
		case Intrinsic::lifetime_start:
		case Intrinsic::lifetime_end:
		case Intrinsic::cheerp_element_distance:
			return COMPLETE_OBJECT;
		case Intrinsic::cheerp_downcast:
		case Intrinsic::cheerp_upcast_collapsed:
		case Intrinsic::cheerp_cast_user:
			return visitValue( p );
		case Intrinsic::cheerp_reallocate:
		case Intrinsic::cheerp_pointer_base:
		case Intrinsic::cheerp_pointer_offset:
			return REGULAR;
		case Intrinsic::cheerp_create_closure:
			assert( U->getOperandNo() == 1 );
			if ( const Function * f = dyn_cast<Function>(p->getOperand(0) ) )
			{
				return { f, 0 };
			}
			else
				llvm::report_fatal_error("Unreachable code in cheerp::PointerAnalyzer::visitUse, cheerp_create_closure");
		case Intrinsic::cheerp_make_complete_object:
			return COMPLETE_OBJECT;
		case Intrinsic::flt_rounds:
		case Intrinsic::cheerp_allocate:
		case Intrinsic::memset:
		default:
			SmallString<128> str("Unreachable code in cheerp::PointerAnalyzer::visitUse, unhandled intrinsic: ");
			str+=intrinsic->getCalledFunction()->getName();
			llvm::report_fatal_error(StringRef(str),false);
		}
		return REGULAR;
	}

	if ( ImmutableCallSite cs = p )
	{
		if ( cs.isCallee(U) )
			return COMPLETE_OBJECT;

		const Function * calledFunction = cs.getCalledFunction();
		if ( !calledFunction )
			return REGULAR;

		unsigned argNo = cs.getArgumentNo(U);

		if ( argNo >= calledFunction->arg_size() )
		{
			// Passed as a variadic argument
			return REGULAR;
		}

		return { calledFunction, argNo };
	}

	if ( const ReturnInst * ret = dyn_cast<ReturnInst>(p) )
	{
		return { ret->getParent()->getParent() };
	}

	// Bitcasts from byte layout types require COMPLETE_OBJECT, and generate BYTE_LAYOUT
	if(isBitCast(p))
	{
		if (TypeSupport::hasByteLayout(p->getOperand(0)->getType()->getPointerElementType()))
			return COMPLETE_OBJECT;
		else
			return visitValue( p );
	}

	if(isa<SelectInst> (p) || isa <PHINode>(p))
		return visitValue(p);

	if ( isa<Constant>(p) )
		return REGULAR;

	return COMPLETE_OBJECT;
}

PointerKindWrapper PointerUsageVisitor::visitReturn(const Function* F)
{
	if(!F)
		return REGULAR;

	/**
	 * Note:
	 * we can not use F as the cache key here,
	 * since F is a pointer to function which might be used elsewhere.
	 * Hence we store the entry basic block.
	 */

	if(cachedValues.count(F->begin()))
		return cachedValues.find(F->begin())->second;

	if(!closedset.insert(F->begin()).second)
		return {PointerKindWrapper::UNKNOWN};

	auto CacheAndReturn = [&](const PointerKindWrapper& k)
	{
		closedset.erase(F);
		if (k.isKnown())
			return (const PointerKindWrapper&)cachedValues.insert( std::make_pair(F->begin(), k ) ).first->second;
		return k;
	};

	Type* returnPointedType = F->getReturnType()->getPointerElementType();

	if((PointerKindWrapper::WRAPPED_POINTER_KIND)getKindForType(returnPointedType) != PointerKindWrapper::UNKNOWN)
		return CacheAndReturn(getKindForType(returnPointedType));

	if(cachedAddressTaken.checkAddressTaken(F))
		return CacheAndReturn(REGULAR);

	PointerKindWrapper result = COMPLETE_OBJECT;
	for(const Use& u : F->uses())
	{
		ImmutableCallSite cs = u.getUser();
		if(cs && cs.isCallee(&u))
			result = result | visitAllUses(cs.getInstruction());

		if (result==REGULAR)
			break;
	}
	return CacheAndReturn(result);
}

POINTER_KIND PointerUsageVisitor::getKindForType(Type * tp) const
{
	if ( tp->isFunctionTy() ||
		TypeSupport::isClientType( tp ) )
		return COMPLETE_OBJECT;

	if ( TypeSupport::isImmutableType( tp ) )
		return REGULAR;

	return (POINTER_KIND)PointerKindWrapper::UNKNOWN;
}

POINTER_KIND PointerUsageVisitor::resolvePointerKind(const PointerKindWrapper& k, resolve_visited_set_t& closedset)
{
	assert(k==PointerKindWrapper::INDIRECT);
	for(uint32_t i=0;i<k.returnConstraints.size();i++)
	{
		const Function* f=k.returnConstraints[i];
		PointerKindWrapper retKind=visitReturn(f);
		retKind.makeKnown();
		assert(retKind!=BYTE_LAYOUT);
		if(retKind==REGULAR)
			return REGULAR;
		else if(retKind==PointerKindWrapper::INDIRECT)
		{
			if(!closedset.insert({ RETURN_CONSTRAINT, f, 0}).second)
				continue;
			POINTER_KIND resolvedKind=resolvePointerKind(retKind, closedset);
			if(resolvedKind==REGULAR)
				return REGULAR;
		}
	}
	for(uint32_t i=0;i<k.argsConstraints.size();i++)
	{
		auto& it=k.argsConstraints[i];
		Function::const_arg_iterator arg = it.first->arg_begin();
		std::advance(arg, it.second);
		PointerKindWrapper retKind=visitValue( arg );
		retKind.makeKnown();
		if(retKind==REGULAR || retKind==BYTE_LAYOUT)
			return (POINTER_KIND)retKind;
		else if(retKind==PointerKindWrapper::INDIRECT)
		{
			if(!closedset.insert({ DIRECT_ARG_CONSTRAINT, arg, 0}).second)
				continue;
			POINTER_KIND resolvedKind=resolvePointerKind(retKind, closedset);
			if(resolvedKind==REGULAR)
				return REGULAR;
		}
	}
	for(uint32_t i=0;i<k.storedTypeConstraints.size();i++)
	{
		Type* t=k.storedTypeConstraints[i];
		// We will resolve this contraint indirectly through the cachedTypes map
		auto it=cachedTypes.find(t);
		if(it==cachedTypes.end())
			continue;
		const PointerKindWrapper& retKind=it->second;
		assert(retKind!=BYTE_LAYOUT);
		if(retKind==REGULAR)
			return REGULAR;
		else if(retKind==PointerKindWrapper::INDIRECT)
		{
			if(!closedset.insert({ STORED_TYPE_CONSTRAINT, t, 0}).second)
				continue;
			POINTER_KIND resolvedKind=resolvePointerKind(retKind, closedset);
			if(resolvedKind==REGULAR)
				return REGULAR;
		}
	}
	return COMPLETE_OBJECT;
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

void PointerAnalyzer::prefetch(const Module& m) const
{
#ifndef NDEBUG
	Timer t( "prefetch", timerGroup);
	TimerGuard guard(t);
#endif //NDEBUG

	for(const Function & F : m)
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
}

PointerKindWrapper PointerAnalyzer::getFinalPointerKindWrapper(const Value* p) const
{
#ifndef NDEBUG
	TimerGuard guard(gpkTimer);
#endif //NDEBUG
	if (PointerUsageVisitor(cache, typeCache, addressTakenCache).visitByteLayoutChain(p))
	{
		cache.insert( std::make_pair(p, BYTE_LAYOUT) );
		return BYTE_LAYOUT;
	}
	PointerKindWrapper k = PointerUsageVisitor(cache, typeCache, addressTakenCache).visitValue(p);

	k.makeKnown();
	return k;
}

POINTER_KIND PointerAnalyzer::getPointerKind(const Value* p) const
{
#ifndef NDEBUG
	TimerGuard guard(gpkTimer);
#endif //NDEBUG
	PointerKindWrapper k = getFinalPointerKindWrapper(p);

	if (k!=PointerKindWrapper::INDIRECT)
		return (POINTER_KIND)k;

	// Got an indirect value, we need to resolve it now
	PointerUsageVisitor::resolve_visited_set_t closedset;
	return PointerUsageVisitor(cache, typeCache, addressTakenCache).resolvePointerKind(k, closedset);
}

PointerKindWrapper PointerAnalyzer::getFinalPointerKindWrapperForReturn(const Function* F) const
{
#ifndef NDEBUG
	TimerGuard guard(gpkfrTimer);
#endif //NDEBUG
	PointerKindWrapper k = PointerUsageVisitor(cache, typeCache, addressTakenCache).visitReturn(F);

	k.makeKnown();
	return k;
}

POINTER_KIND PointerAnalyzer::getPointerKindForReturn(const Function* F) const
{
#ifndef NDEBUG
	TimerGuard guard(gpkfrTimer);
#endif //NDEBUG
	PointerKindWrapper k = getFinalPointerKindWrapperForReturn(F);
	if (k!=PointerKindWrapper::INDIRECT)
		return (POINTER_KIND)k;

	PointerUsageVisitor::resolve_visited_set_t closedset;
	return PointerUsageVisitor(cache, typeCache, addressTakenCache).resolvePointerKind(k, closedset);
}

POINTER_KIND PointerAnalyzer::getPointerKindForType(Type* pointerType) const
{
	POINTER_KIND ret=PointerUsageVisitor(cache, typeCache, addressTakenCache).getKindForType(pointerType->getPointerElementType());
	if(ret!=REGULAR)
		return ret;
	// If REGULAR by default, try harder using the typeCache map
	auto it=typeCache.find(pointerType->getPointerElementType());
	if(it==typeCache.end())
		return REGULAR;

	const PointerKindWrapper& k = it->second;
	if (k!=PointerKindWrapper::INDIRECT)
		return (POINTER_KIND)k;

	PointerUsageVisitor::resolve_visited_set_t closedset;
	return PointerUsageVisitor(cache, typeCache, addressTakenCache).resolvePointerKind(k, closedset);
}

void PointerAnalyzer::invalidate(const Value * v)
{
#ifndef NDEBUG
	assert(!fullyResolved);
#endif
	if ( cache.erase(v) )
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
	}
	// If v is a function invalidate also all its call and arguments
	if ( const Function * F = dyn_cast<Function>(v) )
	{
		for ( const Argument & arg : F->getArgumentList() )
			if (arg.getType()->isPointerTy())
				invalidate(&arg);
		addressTakenCache.erase(F);
	}
}

void PointerAnalyzer::fullResolve() const
{
	for(auto& it: cache)
	{
		if(it.second!=PointerKindWrapper::INDIRECT)
			continue;
		PointerUsageVisitor::resolve_visited_set_t closedset;
		POINTER_KIND k=PointerUsageVisitor(cache, typeCache, addressTakenCache).resolvePointerKind(it.second, closedset);
		assert(k==COMPLETE_OBJECT || k==REGULAR);
		it.second=PointerKindWrapper(k);
	}
	for(auto& it: typeCache)
	{
		if(it.second!=PointerKindWrapper::INDIRECT)
			continue;
		PointerUsageVisitor::resolve_visited_set_t closedset;
		POINTER_KIND k=PointerUsageVisitor(cache, typeCache, addressTakenCache).resolvePointerKind(it.second, closedset);
		assert(k==COMPLETE_OBJECT || k==REGULAR);
		it.second=PointerKindWrapper(k);
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
