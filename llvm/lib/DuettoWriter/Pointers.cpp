//===-- Pointers.cpp - The Duetto JavaScript C pointers implementation ----===//
//
//                     Duetto: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Duetto/Utils.h"
#include "llvm/Duetto/Writer.h"
#include <llvm/IR/Operator.h>

using namespace llvm;
using namespace duetto;

#ifdef DUETTO_DEBUG_POINTERS
static void print_debug_pointer_uknown(const llvm::Value * v, const llvm::User * u,const char * f)
{
	llvm::errs() << "Adding POINTER_UNKNOWN in \"" << f << "\", pointer: " << v->getName() << " being used by: ";

	if (const Instruction * p = dyn_cast<const Instruction>(u) )
		llvm::errs() << " instruction " << p->getOpcodeName() << "\n";
	else if (const Constant * p = dyn_cast<const Constant>(u) )
	{
		llvm::errs() << " constant " << p->getName() << ":\n";
		llvm::errs() << "Object dump: "; p->dump(); llvm::errs() << "\n";
		llvm::errs() << "Yields type: "; p->getType()->dump(); llvm::errs() << "\n";
		llvm::errs() << "\n";
	}
	else if (const Operator * p = dyn_cast<const Operator>(u) )
		llvm::errs() << " operator " << p->getName() << "\n";
}
#endif //DUETTO_DEBUG_POINTERS

//TODO this is a workaround for missing getElementPtrConstantExpr
static const ConstantExpr* dyn_cast_to_constant_gep(const Value * v)
{
	const ConstantExpr * I = dyn_cast<const ConstantExpr>(v);
	return (I && I->getOpcode() == Instruction::GetElementPtr) ? I : 0;
}

uint32_t DuettoWriter::getPointerUsageFlags(const llvm::Value * v)
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
			else if (const GetElementPtrInst * I = dyn_cast<const GetElementPtrInst>(U) )
			{
				const ConstantInt * p = dyn_cast<const ConstantInt>(I->getOperand(1));
				if (!p || !p->isZero())
					ans |= POINTER_ARITHMETIC;
			}
			else if (const ConstantExpr * I = dyn_cast_to_constant_gep(U) )
			{
				// Same as GEP, constant
				//TODO use getElementPtrConstantExpr after rebase with main llvm tree
				const ConstantInt * p = cast<const ConstantInt>(I->getOperand(1));
				assert(p && "First argument to GEP is not a constant int");
				if (!p->isZero())
					ans |= POINTER_ARITHMETIC;
			}
			/** TODO deal with all use cases and remove the following 2 blocks **/
			else if (isa<const PHINode>(U) || 
					isa<const BitCastInst>(U) || 
					isa<const SelectInst>(U) ||
					isa<const LoadInst>(U) ||
					isa<const CallInst>(U) ||
					isa<const ReturnInst>(U) )
			{
				continue;
			}
			else
			{
#ifdef DUETTO_DEBUG_POINTERS
				print_debug_pointer_uknown(v,it->getUser(),"getPointerUsageFlags");
#endif //DUETTO_DEBUG_POINTERS
				ans |= POINTER_UNKNOWN;
			}
		}
		
		iter = pointerUsageMap.insert( std::make_pair(v, ans) ).first;
	}

	return iter->second;
}

uint32_t DuettoWriter::dfsPointerUsageFlagsComplete(const Value * v, std::set<const Value *> & openset)
{
	if ( !openset.insert(v).second )
	{
		return 0;
	}

	uint32_t f = getPointerUsageFlags(v);

	for (Value::const_use_iterator it = v->use_begin(); it != v->use_end(); ++it)
	{
		const User* U = it->getUser();
		// Check if "v" is used as a operand in a phi node
		if (isa<const PHINode>(U) ||
			isa<const BitCastInst>(U) ||
			isa<const SelectInst>(U) )
		{
			f |= dfsPointerUsageFlagsComplete(U, openset);
		} 
		else if (isa<const CallInst>(U))
		{
			//TODO deal with me properly
			f |= POINTER_UNKNOWN;
		}
		else if (isa<const ReturnInst>(U))
		{
			//TODO deal with me properly
			f |= POINTER_UNKNOWN;
		}
		else if (const StoreInst * I = dyn_cast<const StoreInst>(U) )
		{
			if (I->getOperand(0) == v)
			{
				// Tracking the stores is almost impossible
				/** But we can do this - in a conservative way - by checking the type of the pointed object.
				 * This should be implemented in future
				 */
				f |= POINTER_UNKNOWN;
			}
		}
		else if ( // Things we know are ok
			isa<const CmpInst>(U) ||
 			isa<const GetElementPtrInst>(U) ||
			isa<const LoadInst>(U) ||
			isa<const PtrToIntInst>(U) || 
			(dyn_cast_to_constant_gep(U) != 0) )
		{
			continue;
		}
		else
		{
#ifdef DUETTO_DEBUG_POINTERS
			print_debug_pointer_uknown(v,it->getUser(),"dfsPointerUsageFlagsComplete");
#endif //DUETTO_DEBUG_POINTERS

			f |= POINTER_UNKNOWN;
		}
	}
	return f;
}

uint32_t DuettoWriter::getPointerUsageFlagsComplete(const Value * v)
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
