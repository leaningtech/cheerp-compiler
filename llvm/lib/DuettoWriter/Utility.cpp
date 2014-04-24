//===-- Utility.cpp - The Duetto JavaScript generator --------------------===//
//
//                     Duetto: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Duetto/Utility.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/Support/FormattedStream.h"

using namespace llvm;

namespace duetto {

bool isValidTypeCast(const Value* castI, const Value* castOp, Type* srcPtr, Type* dstPtr)
{
	//Only pointer casts are possible anyway
	assert(srcPtr->isPointerTy() && dstPtr->isPointerTy());
	Type* src=cast<PointerType>(srcPtr)->getElementType();
	Type* dst=cast<PointerType>(dstPtr)->getElementType();
	//Conversion between client objects is free
	if(isClientType(src) && isClientType(dst))
		return true;
	//Conversion between any function pointer is ok
	if(src->isFunctionTy() && dst->isFunctionTy())
		return true;
	//Allow conversions between equivalent struct types
	if(src->isStructTy() && dst->isStructTy())
	{
		StructType* srcSt = cast<StructType>(src);
		StructType* dstSt = cast<StructType>(dst);
		if(srcSt->isLayoutIdentical(dstSt))
			return true;
	}
	if(dst->isIntegerTy(8))
		return true;
	//Support getting functions back from the Vtable
	if(src->isPointerTy() && dst->isPointerTy())
	{
		Type* innerSrc=cast<PointerType>(src)->getElementType();
		Type* innerDst=cast<PointerType>(dst)->getElementType();
		if(innerSrc->isIntegerTy(8) || innerDst->isFunctionTy())
		{
			const ConstantExpr* constGep=dyn_cast<const ConstantExpr>(castOp);
			if(constGep && constGep->getOpcode()==Instruction::GetElementPtr)
			{
				const Value* sourceVal = constGep->getOperand(0);
				if(sourceVal->hasName() &&
					strncmp(sourceVal->getName().data(),"_ZTV",4)==0)
				{
					//This casts ultimately comes from a VTable, it's ok
					return true;
				}
			}
		}
		if(innerSrc->isFunctionTy() && innerDst->isFunctionTy())
			return true;
	}
	//Also allow the unsafe cast from i8* in a few selected cases
	if(src->isIntegerTy(8))
	{
		bool comesFromNew = isValidVoidPtrSource(castOp);
		bool allowedRawUsages = true;
		Value::const_use_iterator it=castOp->use_begin();
		Value::const_use_iterator itE=castOp->use_end();
		for(;it!=itE;++it)
		{
			const User* U = it->getUser();
			//Check that the other use is a memset or an icmp
			if(U==castI)
				continue;
			const CallInst* ci=dyn_cast<const CallInst>(U);
			if(!(ICmpInst::classof(U) || safeCallForNewedMemory(ci)))
				allowedRawUsages = false;
		}
		if(comesFromNew && allowedRawUsages)
			return true;
	}
	if(isUnion(src) && (ArrayType::classof(dst) || isTypedArrayType(dst)))
		return true;
	//Allow changing the size of an array
	if (ArrayType::classof(src) && ArrayType::classof(dst) &&
		src->getSequentialElementType() == dst->getSequentialElementType())
	{
		return true;
	}
	return false;
}

bool isClientType(const Type* t)
{
	return (t->isStructTy() && cast<StructType>(t)->hasName() &&
		strncmp(t->getStructName().data(), "class._ZN6client", 16)==0);
}

bool isClientArrayType(const Type* t)
{
	return (t->isStructTy() && cast<StructType>(t)->hasName() &&
		strcmp(t->getStructName().data(), "class._ZN6client5ArrayE")==0);
}

bool isClientGlobal(const char* mangledName)
{
	return strncmp(mangledName,"_ZN6client",10)==0;
}

bool isI32Type(Type* t)
{
	return t->isIntegerTy() && static_cast<IntegerType*>(t)->getBitWidth()==32;
}

bool isTypedArrayType(Type* t)
{
	return t->isIntegerTy(8) || t->isIntegerTy(16) || t->isIntegerTy(32) ||
		t->isFloatTy() || t->isDoubleTy();
}

bool isComingFromAllocation(const Value* val)
{
	const CallInst* newCall=dyn_cast<const CallInst>(val);
	if(newCall && newCall->getCalledFunction())
	{
		return newCall->getCalledFunction()->getName()=="_Znwj"
			|| newCall->getCalledFunction()->getName()=="_Znaj"
			|| newCall->getCalledFunction()->getName()=="realloc"
			|| newCall->getCalledFunction()->getName()=="malloc"
			|| newCall->getCalledFunction()->getName()=="calloc"
			|| newCall->getCalledFunction()->getIntrinsicID() == Intrinsic::duetto_allocate;
	}
	//Try invoke as well
	const InvokeInst* newInvoke=dyn_cast<const InvokeInst>(val);
	if(newInvoke && newInvoke->getCalledFunction())
	{
		//TODO: Disable throw in new, it's nonsense in JS context
		return newInvoke->getCalledFunction()->getName()=="_Znwj"
			|| newInvoke->getCalledFunction()->getName()=="_Znaj"
			|| newInvoke->getCalledFunction()->getName()=="realloc"
			|| newInvoke->getCalledFunction()->getName()=="malloc"
			|| newInvoke->getCalledFunction()->getName()=="calloc"
			|| newInvoke->getCalledFunction()->getIntrinsicID() == Intrinsic::duetto_allocate;
	}
	return false;
}

bool isNopCast(const Value* val)
{
	const CallInst* newCall=dyn_cast<const CallInst>(val);
	if(newCall && newCall->getCalledFunction())
		return newCall->getCalledFunction()->getIntrinsicID() == Intrinsic::duetto_upcast_collapsed
			|| newCall->getCalledFunction()->getIntrinsicID() == Intrinsic::duetto_cast_user;
	return false;
}

bool isValidVoidPtrSource(const Value* val)
{
	std::set<const PHINode*> visitedPhis;
	return isValidVoidPtrSource(val, visitedPhis);
}

bool isValidVoidPtrSource(const Value* val, std::set<const PHINode*>& visitedPhis)
{
	if(isComingFromAllocation(val))
		return true;
	const PHINode* newPHI=dyn_cast<const PHINode>(val);
	if(newPHI)
	{
		if(visitedPhis.count(newPHI))
		{
			//Assume true, if needed it will become false later on
			return true;
		}
		visitedPhis.insert(newPHI);
		for(unsigned i=0;i<newPHI->getNumIncomingValues();i++)
		{
			if(!isValidVoidPtrSource(newPHI->getIncomingValue(i),visitedPhis))
			{
				visitedPhis.erase(newPHI);
				return false;
			}
		}
		visitedPhis.erase(newPHI);
		return true;
	}
	return false;
}

bool isInlineable(const Instruction& I)
{
	//Inlining a variable used by a PHI it's unsafe
	//When the phi's are computed the result
	//correctness may depend on the order they are
	//computed. Check all uses
	Value::const_use_iterator it=I.use_begin();
	Value::const_use_iterator itE=I.use_end();
	for(;it!=itE;++it)
	{
		if(PHINode::classof(it->getUser()))
			return false;
	}
	//Beside a few cases, instructions with a single use may be inlined
	//TODO: Find out a better heuristic for inlining, it seems that computing
	//may be faster even on more than 1 use
	if(I.getOpcode()==Instruction::GetElementPtr)
	{
		//Special case GEPs. They should always be inline since creating the object is really slow
		return true;
	}
	else if(I.getOpcode()==Instruction::BitCast)
	{
		//Inline casts which are not unions
		llvm::Type* src=I.getOperand(0)->getType();
		if(!src->isPointerTy() || !isUnion(src->getPointerElementType()))
			return true;
		Type* pointedType=src->getPointerElementType();
		//Do not inline union casts to array
		if(ArrayType::classof(pointedType))
			return false;
		//Inline if the only uses are load and stores
		Value::const_use_iterator it=I.use_begin();
		Value::const_use_iterator itE=I.use_end();
		for(;it!=itE;++it)
		{
			if(!LoadInst::classof(it->getUser()) && !StoreInst::classof(it->getUser()))
				return false;
		}
		return true;
	}
	else if(I.hasOneUse())
	{
		//A few opcodes needs to be executed anyway as they
		//do not operated on registers
		switch(I.getOpcode())
		{
			case Instruction::Call:
			case Instruction::Invoke:
			case Instruction::Ret:
			case Instruction::LandingPad:
			case Instruction::PHI:
			case Instruction::Load:
			case Instruction::Store:
			case Instruction::InsertValue:
			case Instruction::Resume:
			case Instruction::Br:
			case Instruction::Alloca:
			case Instruction::Switch:
			case Instruction::Unreachable:
			case Instruction::VAArg:
				return false;
			case Instruction::Add:
			case Instruction::Sub:
			case Instruction::Mul:
			case Instruction::And:
			case Instruction::Or:
			case Instruction::Xor:
			case Instruction::Trunc:
			case Instruction::FPToSI:
			case Instruction::SIToFP:
			case Instruction::SDiv:
			case Instruction::SRem:
			case Instruction::Shl:
			case Instruction::AShr:
			case Instruction::LShr:
			case Instruction::FAdd:
			case Instruction::FDiv:
			case Instruction::FSub:
			case Instruction::FPTrunc:
			case Instruction::FPExt:
			case Instruction::FMul:
			case Instruction::FCmp:
			case Instruction::ICmp:
			case Instruction::ZExt:
			case Instruction::SExt:
			case Instruction::Select:
			case Instruction::ExtractValue:
			case Instruction::URem:
			case Instruction::UDiv:
			case Instruction::UIToFP:
			case Instruction::FPToUI:
			case Instruction::PtrToInt:
				return true;
			default:
				llvm::report_fatal_error(Twine("Unsupported opcode: ",StringRef(I.getOpcodeName())), false);
				return true;
		}
	}
	return false;
}

bool isBitCast(const Value* v)
{
	const User* b=static_cast<const User*>(v);
	if(isa<BitCastInst>(v))
	{
		bool validCast = isValidTypeCast(v, b->getOperand(0), b->getOperand(0)->getType(), v->getType());
		if(!validCast)
		{
			llvm::errs() << "Error while handling cast " << *v << "\n";
			llvm::report_fatal_error("Unsupported code found, please report a bug", false);
			return false;
		}
		return true;
	}
	const ConstantExpr* ce=dyn_cast<const ConstantExpr>(v);
	if(ce && ce->getOpcode()==Instruction::BitCast)
	{
		bool validCast = isValidTypeCast(v, b->getOperand(0), b->getOperand(0)->getType(), v->getType());
		if(!validCast)
		{
			llvm::errs() << "Error while handling cast " << *v << "\n";
			llvm::report_fatal_error("Unsupported code found, please report a bug", false);
			return false;
		}
		return true;
	}
	return false;
}

bool isGEP(const Value* v)
{
	if(GetElementPtrInst::classof(v))
		return true;
	const ConstantExpr* ce=dyn_cast<const ConstantExpr>(v);
	if(ce && ce->getOpcode()==Instruction::GetElementPtr)
		return true;
	return false;
}

bool isImmutableType(const Type* t)
{
	if(t->isIntegerTy() || t->isFloatTy() || t->isDoubleTy() || t->isPointerTy())
		return true;
	return false;
}

bool isUnion(const Type* t)
{
	return (t->isStructTy() && cast<StructType>(t)->hasName() &&
		t->getStructName().startswith("union."));
}

bool safeUsagesForNewedMemory(const Value* v)
{
	Value::const_use_iterator it=v->use_begin();
	Value::const_use_iterator itE=v->use_end();
	for(;it!=itE;++it)
	{
		const PHINode* p=dyn_cast<const PHINode>(it->getUser());
		//If the usage is a PHI node, recursively check its usages
		if(p)
		{
			if(!safeUsagesForNewedMemory(p))
				return false;
		}
		else
		{
			const CallInst* ci=dyn_cast<const CallInst>(*it);
			if(!safeCallForNewedMemory(ci))
				return false;
		}
	}
	return true;
}

bool safeCallForNewedMemory(const CallInst* ci)
{
	//We allow the unsafe cast to i8* only
	//if the usage is memcpy, memset, free or delete
	//or one of the lifetime/invariant intrinsics
	return (ci && ci->getCalledFunction() &&
		(ci->getCalledFunction()->getIntrinsicID()==Intrinsic::memcpy ||
		ci->getCalledFunction()->getIntrinsicID()==Intrinsic::memset ||
		ci->getCalledFunction()->getIntrinsicID()==Intrinsic::memmove ||
		ci->getCalledFunction()->getName()=="free" ||
		ci->getCalledFunction()->getName()=="_ZdaPv" ||
		ci->getCalledFunction()->getName()=="_ZdlPv" ||
		ci->getCalledFunction()->getIntrinsicID()==Intrinsic::lifetime_start ||
		ci->getCalledFunction()->getIntrinsicID()==Intrinsic::lifetime_end ||
		ci->getCalledFunction()->getIntrinsicID()==Intrinsic::invariant_start ||
		ci->getCalledFunction()->getIntrinsicID()==Intrinsic::invariant_end ||
		//Allow unsafe casts for a limited number of functions that accepts callback args
		//TODO: find a nicer approach for this
		ci->getCalledFunction()->getName()=="__cxa_atexit"));
}

}
