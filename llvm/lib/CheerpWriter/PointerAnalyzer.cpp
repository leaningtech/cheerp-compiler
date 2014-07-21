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
#include "llvm/ADT/SmallSet.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/Support/FormattedStream.h"

using namespace llvm;

namespace cheerp {

struct PointerUsageVisitor
{
	typedef llvm::SmallSet< const llvm::Value *, 8> visited_set_t;

	POINTER_KIND visitValue( const Value * v );
	POINTER_KIND visitUse( const Use * U );
	POINTER_KIND visitReturn( const Function * F);
	POINTER_KIND getKindForType( Type * ) const;

	bool allUsesAreCO( const Value * U )
	{
		return std::all_of( U->use_begin(), U->use_end(),
				[this]( const Use & u )
				{
					return visitUse( &u ) == COMPLETE_OBJECT;
				} );
	}

	Type * realType( const Value * v ) const
	{
		assert( v->getType()->isPointerTy() );
		if ( isBitCast(v) )
			v = cast<User>(v)->getOperand(0);
		return v->getType()->getPointerElementType();
	}

	visited_set_t closedset;
};

POINTER_KIND PointerUsageVisitor::visitValue( const Value * p )
{
	if ( !closedset.insert(p).second )
		return COMPLETE_OBJECT;

	llvm::Type * type = realType(p);

	if ( getKindForType(type) == COMPLETE_OBJECT )
		return COMPLETE_OBJECT;

	if ( TypeSupport::isImmutableType( type ) )
	{
		return REGULAR;
	}

	if ( isa<LoadInst>(p) )
		return REGULAR;

	if ( const Argument * arg = dyn_cast<Argument>(p) )
	{
		if ( arg->getParent()->hasAddressTaken() )
			return REGULAR;
	}

	if ( const IntrinsicInst * intrinsic = dyn_cast<IntrinsicInst>(p) )
	{
		switch ( intrinsic->getIntrinsicID() )
		{
		case Intrinsic::cheerp_downcast:
		case Intrinsic::cheerp_upcast_collapsed:
		case Intrinsic::cheerp_cast_user:
			break;
		case Intrinsic::cheerp_allocate:
			break;
		case Intrinsic::cheerp_pointer_base:
		case Intrinsic::cheerp_create_closure:
		case Intrinsic::cheerp_make_complete_object:
			if ( ! allUsesAreCO(p) )
			{
				llvm::errs() << "Result of " << intrinsic->getName() << " used as REGULAR: " << *p << "\n";
				llvm::report_fatal_error("Unsupported code found, please report a bug", false);
			}
			return COMPLETE_OBJECT;
		case Intrinsic::cheerp_pointer_offset:
		case Intrinsic::memmove:
		case Intrinsic::memcpy:
		case Intrinsic::memset:
		case Intrinsic::invariant_start:
		case Intrinsic::invariant_end:
		case Intrinsic::vastart:
		case Intrinsic::vaend:
		case Intrinsic::flt_rounds:
		default:
			llvm::report_fatal_error("Unreachable code in cheerp::PointerAnalyzer::visitValue, unhandled intrinsic");
		}
	}
	else
	// TODO this is not really necessary,
	// but we need to modify the writer so that CallInst and InvokeInst 
	// perform a demotion in place.
	if ( ImmutableCallSite cs = p )
	{
		if ( visitReturn( cs.getCalledFunction() ) == REGULAR )
			return REGULAR;
// 		else
// 		{
// 			assert( allUsesAreCO(p) );
// 			return COMPLETE_OBJECT;
// 		}
	}
	
	return allUsesAreCO(p) ? COMPLETE_OBJECT : REGULAR;
}

POINTER_KIND PointerUsageVisitor::visitUse(const Use* U)
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

	if ( const IntrinsicInst * intrinsic = dyn_cast<IntrinsicInst>(p) )
	{
		switch ( intrinsic->getIntrinsicID() )
		{
		case Intrinsic::memmove:
		case Intrinsic::memcpy:
		case Intrinsic::memset:
			return REGULAR;
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
		case Intrinsic::cheerp_pointer_base:
		case Intrinsic::cheerp_pointer_offset:
			return REGULAR;
		case Intrinsic::cheerp_create_closure:
			assert( U->getOperandNo() == 1 );
			if ( const Function * f = dyn_cast<Function>(p->getOperand(0) ) )
			{
				return visitValue( f->arg_begin() );
			}
			else
				llvm::report_fatal_error("Unreachable code in cheerp::PointerAnalyzer::visitUse, cheerp_create_closure");
		case Intrinsic::cheerp_make_complete_object:
			return COMPLETE_OBJECT;
		case Intrinsic::flt_rounds:
		case Intrinsic::cheerp_allocate:
		default:
			llvm::report_fatal_error("Unreachable code in cheerp::PointerAnalyzer::visitUse, unhandled intrinsic");
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

		Function::const_arg_iterator arg = calledFunction->arg_begin();
		std::advance(arg, argNo);

		return visitValue( arg );
	}

	if ( const ReturnInst * ret = dyn_cast<ReturnInst>(p) )
	{
		return visitReturn( ret->getParent()->getParent() );
	}

	if ( isBitCast(p) || isa< SelectInst > (p) || isa < PHINode >(p) )
		return visitValue( p );

	if ( isa<Constant>(p) )
		return REGULAR;

	return COMPLETE_OBJECT;
}

POINTER_KIND PointerUsageVisitor::visitReturn(const Function * F)
{
	if ( !F || F->hasAddressTaken() )
		return REGULAR;

	if ( std::all_of( F->use_begin(), F->use_end(),
		[this]( const Use & u )
		{
			ImmutableCallSite cs = u.getUser();
			if ( cs && cs.isCallee(&u) )
			{
				return visitValue(cs.getInstruction()) == COMPLETE_OBJECT;
			}

			return true;
		} ) )
		return COMPLETE_OBJECT;

	return REGULAR;
}

POINTER_KIND PointerUsageVisitor::getKindForType(Type * tp) const
{
	if ( tp->isFunctionTy() ||
		TypeSupport::isClientType( tp ) )
		return COMPLETE_OBJECT;
	return REGULAR;
}

POINTER_KIND PointerAnalyzer::getPointerKind(const Value* p) const
{
	return PointerUsageVisitor().visitValue(p);
}

POINTER_KIND PointerAnalyzer::getPointerKindForReturn(const Function* F) const
{
	return PointerUsageVisitor().visitReturn(F);
}

POINTER_KIND PointerAnalyzer::getPointerKindForType(Type* tp) const
{
	return PointerUsageVisitor().getKindForType(tp);
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
