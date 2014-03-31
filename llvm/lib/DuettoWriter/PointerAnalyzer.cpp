//===-- Analyzer.cpp - The Duetto JavaScript generator --------------------===//
//
//                     Duetto: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include <iomanip>
#include <sstream>
#include "llvm/Duetto/PointerAnalyzer.h"
#include "llvm/Duetto/Utility.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Operator.h"
#include "llvm/Support/FormattedStream.h"

using namespace llvm;

namespace duetto {


/*
 * The map is used to handle cyclic PHI nodes
 */
POINTER_KIND DuettoPointerAnalyzer::getPointerKind(const Value* v) const
{
	assert(v->getType()->isPointerTy());

	pointer_kind_map_t::iterator iter = pointerKindMap.find(v);

	if (pointerKindMap.end() != iter)
		return iter->second;
	iter = pointerKindMap.insert( std::make_pair(v, UNDECIDED) ).first;
	
	#ifdef DUETTO_DEBUG_POINTERS
	debugAllPointersSet.insert(v);
	#endif
 
	PointerType * pt = cast<PointerType>(v->getType());
	if(isClientArrayType(pt->getElementType()))
	{
		//Handle client arrays like COMPLETE_ARRAYs, so the right 0 offset
		//is used when doing GEPs
		return iter->second = COMPLETE_ARRAY;
	}
	if(isClientType(pt->getElementType()))
	{
		//Pointers to client type are complete objects, and are never expanded to
		//regular ones since an array of client objects does not exists.
		//NOTE: An array of pointer to client objects exists, not an array of objects.
		return iter->second = COMPLETE_OBJECT;
	}
	if(AllocaInst::classof(v) || GlobalVariable::classof(v))
	{
		if(isImmutableType(pt->getElementType()) && !isNoWrappingArrayOptimizable(v))
			return iter->second = COMPLETE_ARRAY;
		else
			return iter->second = COMPLETE_OBJECT;
	}
	//Follow bitcasts
	if(isBitCast(v))
	{
		const User* bi=static_cast<const User*>(v);
		//Casts from unions return regular pointers
		if(isUnion(bi->getOperand(0)->getType()->getPointerElementType()))
		{
			//Special case arrays
			if(ArrayType::classof(pt->getElementType()))
				return iter->second = COMPLETE_OBJECT;
			else
				return iter->second = COMPLETE_ARRAY;
		}
		return iter->second = getPointerKind(bi->getOperand(0));
	}
	if(isNopCast(v))
	{
		const User* bi=static_cast<const User*>(v);
		return iter->second = getPointerKind(bi->getOperand(0));
	}
	//Follow select
	if(const SelectInst* s=dyn_cast<SelectInst>(v))
	{
		POINTER_KIND k1 = getPointerKind(s->getTrueValue());
		if(k1 == REGULAR)
			return iter->second = REGULAR;
		
		POINTER_KIND k2 = getPointerKind(s->getFalseValue());
		//If the type is different we need to collapse to REGULAR
		if(k1!=k2)
			return iter->second = REGULAR;
		//The type is the same
		return iter->second = k1;
	}
	if(isComingFromAllocation(v))
		return iter->second = COMPLETE_ARRAY;

	if ( const Argument * arg = dyn_cast<const Argument>(v) )
	{
		const Function * F = arg->getParent();
		
		//TODO properly handle varargs
		if (!F || canBeCalledIndirectly(F) || F->isVarArg())
			return iter->second = REGULAR;
	}
	
	if (isa<const PHINode>(v) || isa<const Argument>(v))
	{
		if (isImmutableType( pt->getElementType() ) )
		{
			return iter->second = (isNoWrappingArrayOptimizable(v) ? COMPLETE_OBJECT : REGULAR);
		}
		else
		{
			return iter->second = (isNoSelfPointerOptimizable(v) ? COMPLETE_OBJECT : REGULAR);
		}
	}
	return iter->second = REGULAR;
}

uint32_t DuettoPointerAnalyzer::getPointerUsageFlagsComplete(const Value * v) const
{
	assert(v->getType()->isPointerTy());

	pointer_usage_map_t::const_iterator iter = pointerCompleteUsageMap.find(v);

	if (pointerCompleteUsageMap.end() == iter)
	{
		std::set<const Value *> openset;
		iter = pointerCompleteUsageMap.insert( std::make_pair(v,dfsPointerUsageFlagsComplete(v, openset) ) ).first;
	}

	return iter->second;
}

void DuettoPointerAnalyzer::dumpPointer(const Value* v) const
{
	std::ostringstream fmt;
	fmt << std::setw(96) << std::left;

	{
		std::ostringstream tmp;
		if (v->hasName())
			tmp << v->getName().data();
		else
			tmp << "tmp" << namegen.getUniqueIndexForValue(v);
		
		if (const Argument * arg = dyn_cast<const Argument>(v))
		{
			tmp << " arg of function: " << arg->getParent()->getName().data();
		}
		fmt << tmp.str();
	}
	
	if (v->getType()->isPointerTy())
	{
		fmt << std::setw(18) << std::left;
		switch (getPointerKind(v))
		{
			case COMPLETE_OBJECT: fmt << "COMPLETE_OBJECT"; break;
			case COMPLETE_ARRAY: fmt << "COMPLETE_ARRAY"; break;
			case REGULAR: fmt << "REGULAR"; break;
			default: fmt << "UNDECIDED"; break;
		}
		
		fmt << std::setw(18) << std::left << getPointerUsageFlags(v);
		fmt << std::setw(18) << std::left << getPointerUsageFlagsComplete(v);
		fmt << std::setw(18) << std::left << std::boolalpha << isImmutableType( v->getType()->getPointerElementType() );
	}
	else
		fmt << "Is not a pointer";

	llvm::errs() << fmt.str() << "\n";
}

#ifdef DUETTO_DEBUG_POINTERS

void DuettoPointerAnalyzer::dumpAllPointers() const
{
	llvm::errs() << "Dumping all pointers\n";
	
	llvm::errs() << "Name" << std::string(92,' ') << "Kind              UsageFlags        UsageFlagsComplete IsImmutable?\n";
	
	for (auto ptr : debugAllPointersSet)
		dumpPointer(ptr);
	
// 	llvm::errs() << "Debug indirect function calls:\n";
// 	for (function_indirect_call_map_t::iterator iter = functionIndirectCallMap.begin(); iter != functionIndirectCallMap.end(); ++iter)
// 	{
// 		llvm::errs() << iter->first->getName() << ": " << iter->second << "\n";
// 	}
}

#endif //DUETTO_DEBUG_POINTERS

std::string DuettoPointerAnalyzer::valueObjectName(const Value* v)
{
	std::ostringstream os;
	if (const Instruction * p = dyn_cast<const Instruction>(v) )
		os << " instruction " << p->getOpcodeName() << "\n";
	else if (const Constant * p = dyn_cast<const Constant>(v) )
	{
		os << " constant " << p->getName().str() << "(";
		
		// Feel free to find a way to avoid this obscenity
		if (isa<const BlockAddress>(p))
			os << "BlockAddress";
		else if (isa<const ConstantAggregateZero>(p))
			os << "ConstantAggregateZero";
		else if (isa<const ConstantArray>(p))
			os << "ConstantArray";
		else if (isa<const ConstantDataSequential>(p))
			os << "ConstantDataSequential";
		else if (const ConstantExpr * pc = dyn_cast<const ConstantExpr>(p))
		{
			os << "ConstantExpr [" << pc->getOpcodeName() <<"]";
		}
		else if (isa<const ConstantFP>(p))
			os << "ConstantFP";
		else if (isa<const ConstantInt>(p))
			os << "ConstantInt";
		else if (isa<const ConstantPointerNull>(p))
			os << "ConstantPointerNull";
		else if (isa<const ConstantStruct>(p))
			os << "ConstantStruct";
		else if (isa<const ConstantVector>(p))
			os << "ConstantVector";
		else if (isa<const GlobalValue>(p))
			os << "GlobalValue";
		else if (isa<const UndefValue>(p))
			os << "UndefValue";
		else
			os << "Unknown";
		os << ")\n";
	}
	else if ( isa<const Operator>(p) )
		os << " operator " << p->getName().str() << "\n";
	return os.str();
}

uint32_t DuettoPointerAnalyzer::getPointerUsageFlags(const llvm::Value * v) const
{
	assert(v->getType()->isPointerTy());

	pointer_usage_map_t::const_iterator iter = pointerUsageMap.find(v);

	if (iter == pointerUsageMap.end())
	{
		uint32_t ans = 0;
		for (Value::const_use_iterator it = v->use_begin(); it != v->use_end(); ++it)
		{
			const User* U = it->getUser();
			// Check if the pointer "v" is used as "ptr" for a StoreInst. 
			if (const StoreInst * I = dyn_cast<const StoreInst>(U) )
			{
				if (I->getPointerOperand() == v)
					ans |= POINTER_NONCONST_DEREF; 
			}
			
			// Check if the pointer "v" is used as lhs or rhs of a comparison operation
			else if (const CmpInst * I = dyn_cast<const CmpInst>(U) )
			{
				if (!I->isEquality())
					ans |= POINTER_ORDINABLE;
				else
					ans |= POINTER_EQUALITY_COMPARABLE;
			}
			
			// Check if the pointer is casted to int
			else if (isa<const PtrToIntInst>(U) )
			{
				ans |= POINTER_CASTABLE_TO_INT;
			}
			
			// Pointer used as a base to a getElementPtr
			else if (isGEP(U) )
			{
				const User * I = cast<const User>(U);
  				const ConstantInt * p = dyn_cast<const ConstantInt>(I->getOperand(1));
				if (!p || !p->isZero())
					ans |= POINTER_ARITHMETIC;
			}
			/** TODO deal with all use cases and remove the following 2 blocks **/
			else if (
				isa<const PHINode>(U) || 
				isa<const SelectInst>(U) ||
				isa<const LoadInst>(U) ||
				isa<const CallInst>(U) ||
				isa<const InvokeInst>(U) ||
				isa<const ReturnInst>(U) || 
				isa<const GlobalValue>(U) ||
				isa<const ConstantArray>(U) ||
				isa<const ConstantStruct>(U) ||
				isBitCast(U) ||
				isNopCast(U) )
			{
				continue;
			}
			else
			{
#ifdef DUETTO_DEBUG_POINTERS
				llvm::errs() << "Adding POINTER_UNKNOWN in getPointerUsageFlags due to instruction: " << valueObjectName(U) << "\n";
#endif //DUETTO_DEBUG_POINTERS
				ans |= POINTER_UNKNOWN;
			}
		}
		
		iter = pointerUsageMap.insert( std::make_pair(v, ans) ).first;
	}

	return iter->second;
}

uint32_t DuettoPointerAnalyzer::dfsPointerUsageFlagsComplete(const Value * v, std::set<const Value *> & openset) const
{
	if ( !openset.insert(v).second )
	{
		return 0;
	}

	uint32_t f = getPointerUsageFlags(v);
	
	if (isBitCast(v) || isNopCast(v) || isa<const PHINode>(v) || isa<const Argument>(v) || isa<const SelectInst>(v))
		f |= POINTER_IS_NOT_UNIQUE_OWNER;

	for (Value::const_use_iterator it = v->use_begin(); it != v->use_end(); ++it)
	{
		const User * U = it->getUser();
		// Check if "v" is used as a operand in a phi node
		if (isa<const PHINode>(U) ||
			isa<const SelectInst>(U) ||
			isBitCast(U) ||
			isNopCast(U))
		{
			f |= dfsPointerUsageFlagsComplete(U, openset) | POINTER_IS_NOT_UNIQUE_OWNER;
		}
		else if (const CallInst * I = dyn_cast<const CallInst>(U))
		{
			// Indirect calls require a finer analysis
			f |= usageFlagsForStoreAndInvoke(v,I,openset);
		}
		else if (const InvokeInst * I = dyn_cast<const InvokeInst>(U))
		{
			f |= usageFlagsForStoreAndInvoke(v,I,openset);
		}
		else if (isa<const ReturnInst>(U))
		{
			//TODO deal with me properly
			f |= POINTER_UNKNOWN;
		}
		else if (const StoreInst * I = dyn_cast<const StoreInst>(U) )
		{
			if (I->getValueOperand() == v)
			{
				// Tracking the stores is almost impossible
				/** But we can do this - in a conservative way - by checking the type of the pointed object.
				 * This should be implemented in future
				 */
				f |= POINTER_UNKNOWN;
			}
		}
		else if (
			isa<const ConstantStruct>(U) ||
			isa<const ConstantArray>(U) ||
			isa<const GlobalValue>(U) )
		{
			f |= POINTER_UNKNOWN;
		}
		else if ( // Things we know are ok
			isa<const CmpInst>(U) ||
			isa<const LoadInst>(U) ||
			isa<const PtrToIntInst>(U) ||
			isGEP(U) )
		{
			continue;
		}
		else
		{
#ifdef DUETTO_DEBUG_POINTERS
			llvm::errs() << "Adding POINTER_UNKNOWN in dfsPointerUsageFlagsComplete due to instruction: " << valueObjectName(U) << "\n";
#endif //DUETTO_DEBUG_POINTERS

			//NOTE no need to add POINTER_IS_NOT_UNIQUE_OWNER to PtrToIntInst since IntToPtr are disabled anyway
			f |= POINTER_UNKNOWN;
		}
	}
	return f;
}

template<class CallOrInvokeInst>
uint32_t DuettoPointerAnalyzer::usageFlagsForStoreAndInvoke(const Value * v, const CallOrInvokeInst * I, std::set<const Value *> & openset) const
{
	const Function * f = I->getCalledFunction();
	if ( !f || canBeCalledIndirectly(f) || f->isVarArg())
		return POINTER_UNKNOWN;

	Function::const_arg_iterator iter = f->arg_begin();
	
	assert( f->arg_size() == I->getNumArgOperands() );
	
	for (unsigned int argNo = 0; iter != f->arg_end(); ++iter, ++argNo)
		if ( I->getArgOperand(argNo) == v ) break;

	assert( iter != f->arg_end() );
	
	return dfsPointerUsageFlagsComplete( &(*iter), openset ) | POINTER_IS_NOT_UNIQUE_OWNER;
}

bool DuettoPointerAnalyzer::isNoSelfPointerOptimizable(const llvm::Value * v) const
{
	assert( v->getType()->isPointerTy() );
	return ! (getPointerUsageFlagsComplete(v) & (POINTER_ARITHMETIC | POINTER_ORDINABLE | POINTER_CASTABLE_TO_INT | POINTER_EQUALITY_COMPARABLE) );
}

bool DuettoPointerAnalyzer::isNoWrappingArrayOptimizable(const llvm::Value * v) const
{
	assert( v->getType()->isPointerTy() );
	
	return isImmutableType(v->getType()->getPointerElementType()) && // This type of optimization makes sense only for immutable types
		!(getPointerUsageFlagsComplete(v) & (POINTER_ARITHMETIC | POINTER_ORDINABLE | POINTER_CASTABLE_TO_INT | POINTER_IS_NOT_UNIQUE_OWNER | POINTER_EQUALITY_COMPARABLE) );
}

bool DuettoPointerAnalyzer::canBeCalledIndirectly(const Function * f) const 
{
	assert(f);
	return  f->hasAddressTaken() ||
			f->empty(); //TODO: atm intrinsic functions are assumed to always be called indirectly
}

}
