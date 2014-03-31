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

POINTER_KIND DuettoPointerAnalyzer::getPointerKind(const llvm::Value* v) const
{
	assert(v->getType()->isPointerTy());

	pointer_kind_map_t::const_iterator iter = pointerKindMap.find(v);

	if (pointerKindMap.end() == iter)
	{
		std::map<const Value*, POINTER_KIND> visitedPhis;
		iter = pointerKindMap.insert( std::make_pair(v,dfsPointerKind(v, visitedPhis)) ).first;
		
		assert(iter->second != UNDECIDED);
	}
	return iter->second;
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


/*
 * The map is used to handle cyclic PHI nodes
 */
POINTER_KIND DuettoPointerAnalyzer::dfsPointerKind(const Value* v, std::map<const Value*, POINTER_KIND>& visitedPhisOrArguments) const
{
#ifdef DUETTO_DEBUG_POINTERS
	debugAllPointersSet.insert(v);
#endif
	assert(v->getType()->isPointerTy());
	PointerType* pt=cast<PointerType>(v->getType());
	if(isClientArrayType(pt->getElementType()))
	{
		//Handle client arrays like COMPLETE_ARRAYs, so the right 0 offset
		//is used when doing GEPs
		return COMPLETE_ARRAY;
	}
	if(isClientType(pt->getElementType()))
	{
		//Pointers to client type are complete objects, and are never expanded to
		//regular ones since an array of client objects does not exists.
		//NOTE: An array of pointer to client objects exists, not an array of objects.
		return COMPLETE_OBJECT;
	}
	if(AllocaInst::classof(v) || GlobalVariable::classof(v))
	{
		if(isImmutableType(pt->getElementType()) && !isNoWrappingArrayOptimizable(v))
			return COMPLETE_ARRAY;
		else
			return COMPLETE_OBJECT;
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
				return COMPLETE_OBJECT;
			else
				return COMPLETE_ARRAY;
		}
		return dfsPointerKind(bi->getOperand(0), visitedPhisOrArguments);
	}
	if(isNopCast(v))
	{
		const User* bi=static_cast<const User*>(v);
		return dfsPointerKind(bi->getOperand(0), visitedPhisOrArguments);
	}
	//Follow select
	if(const SelectInst* s=dyn_cast<SelectInst>(v))
	{
		POINTER_KIND k1=dfsPointerKind(s->getTrueValue(), visitedPhisOrArguments);
		if(k1==REGULAR)
			return REGULAR;
		POINTER_KIND k2=dfsPointerKind(s->getFalseValue(), visitedPhisOrArguments);
		//If the type is different we need to collapse to REGULAR
		if(k1!=k2)
			return REGULAR;
		//The type is the same
		return k1;
	}
	if(isComingFromAllocation(v))
		return COMPLETE_ARRAY;

	//Follow PHIs
	if( const PHINode * newPHI = dyn_cast<const PHINode>(v) )
	{
		std::map<const Value*, POINTER_KIND>::iterator alreadyVisited=visitedPhisOrArguments.find(newPHI);
		if(alreadyVisited!=visitedPhisOrArguments.end())
		{
			//Assume true, if needed it will become false later on
			return alreadyVisited->second;
		}
		//Intialize the PHI with undecided
		std::map<const Value*, POINTER_KIND>::iterator current=
			visitedPhisOrArguments.insert(std::make_pair(newPHI, UNDECIDED)).first;
		for(unsigned i=0;i<newPHI->getNumIncomingValues() && current->second!=REGULAR;i++)
		{
			POINTER_KIND k=dfsPointerKind(newPHI->getIncomingValue(i), visitedPhisOrArguments);
			if(current->second == UNDECIDED)
				current->second = k;
			// COMPLETE_OBJECT can't change to COMPLETE_ARRAY for the "self" optimization
			// so switch directly to REGULAR
			else if (k != current->second)
				current->second = REGULAR;
		}
		return current->second;
	}
	
	if ( const Argument * arg = dyn_cast<const Argument>(v) )
	{
		std::map<const Value*, POINTER_KIND>::iterator alreadyVisited=visitedPhisOrArguments.find(arg);
		if(alreadyVisited!=visitedPhisOrArguments.end())
		{
			//Assume true, if needed it will become false later on
			return alreadyVisited->second;
		}

		//Intialize the arg with undecided
		std::map<const Value*, POINTER_KIND>::iterator current=
			visitedPhisOrArguments.insert(std::make_pair(arg, UNDECIDED)).first;

		const Function * F = arg->getParent();
		
		//TODO properly handle varargs
		if (!F || canBeCalledIndirectly(F) || F->isVarArg())
			return REGULAR;
		
		// Compute the argument index
		unsigned argIndex = arg->getArgNo();
		assert(argIndex < F->arg_size());
		
		//TODO: this is used co check if the function F is called at least once in the code, it should be removed in the future
		int nCall = 0;
		
		//We do roughly the same thing we do with phi-nodes, but with function calls
		for( Function::const_use_iterator iter = F->use_begin(); iter != F->use_end() && current->second != REGULAR; ++iter)
		{
			const Value * arg = 0;
			const User* U = iter->getUser();
			
			// Set arg to the argIndex-th argument of the function call, or leave it to 0 if this use is not a function call
			if (const CallInst * call = dyn_cast<const CallInst>(U) )
			{
				if (call->getCalledFunction() == F)
				{
					assert (F->getArgumentList().size() == call->getNumArgOperands() || (F->dump(),false) );
					assert (argIndex < call->getNumArgOperands());
					
					arg = call->getArgOperand(argIndex);
				}
			}
			else if (const InvokeInst * call = dyn_cast<const InvokeInst>(U) )
			{
				if (call->getCalledFunction() == F)
				{
					assert (F->getArgumentList().size() == call->getNumArgOperands() || (F->dump(),false) );
					assert (argIndex < call->getNumArgOperands());
					
					arg = call->getArgOperand(argIndex);
				}
			}
			
			if (arg != 0)
			{
				++nCall;
				POINTER_KIND k = dfsPointerKind(arg, visitedPhisOrArguments );
				if(current->second == UNDECIDED)
					current->second = k;
				else if (k != current->second)
					current->second = REGULAR;
			}
		}
		if (nCall == 0)
		{
			//llvm::errs() << "warning: function " << F->getName() << " seems to never be called\n";
			current->second = REGULAR;
		}
		return current->second;
	}
	
	return REGULAR;
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

	uint32_t f = getPointerUsageFlags(v) | (
		(isBitCast(v) || isa<const PHINode>(v) || isa<const SelectInst>(v)) ? POINTER_IS_NOT_UNIQUE_OWNER : 0);

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
			f |= POINTER_UNKNOWN;
		}
		else if (isa<const InvokeInst>(U))
		{
			//TODO same thing as CallInst.
			f |= POINTER_UNKNOWN;
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

bool DuettoPointerAnalyzer::isNoSelfPointerOptimizable(const llvm::Value * v) const
{
	assert( v->getType()->isPointerTy() );
	return ! (getPointerUsageFlagsComplete(v) & (POINTER_ARITHMETIC | POINTER_ORDINABLE | POINTER_CASTABLE_TO_INT) );
}

bool DuettoPointerAnalyzer::isNoWrappingArrayOptimizable(const llvm::Value * v) const
{
	assert( v->getType()->isPointerTy() );
	
	return isImmutableType(v->getType()->getPointerElementType()) && // This type of optimization makes sense only for immutable types
		!(getPointerUsageFlagsComplete(v) & (POINTER_ARITHMETIC | POINTER_ORDINABLE | POINTER_CASTABLE_TO_INT | POINTER_IS_NOT_UNIQUE_OWNER) );
}

bool DuettoPointerAnalyzer::canBeCalledIndirectly(const Function * f) const 
{
	assert(f);
	return  f->hasAddressTaken() ||
			f->empty(); //TODO: atm intrinsic functions are assumed to always be called indirectly
}

}
