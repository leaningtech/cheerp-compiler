//===-- Utility.cpp - The Cheerp JavaScript generator --------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include <sstream>
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalAlias.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Operator.h"
#include "llvm/Support/FormattedStream.h"

using namespace llvm;

namespace cheerp {

bool isNopCast(const Value* val)
{
	const CallInst * newCall = dyn_cast<const CallInst>(val);
	if(newCall && newCall->getCalledFunction())
	{
		unsigned int id = newCall->getCalledFunction()->getIntrinsicID();
		
		if ( Intrinsic::cheerp_upcast_collapsed == id ||
			Intrinsic::cheerp_cast_user == id )
			return true;
		
		if ( Intrinsic::cheerp_downcast == id )
		{
			Type* t = newCall->getArgOperand(0)->getType()->getPointerElementType();

			if ( TypeSupport::isClientType(t) ||
				getIntFromValue( newCall->getArgOperand(1) ) == 0 )
				return true;
		}
		
	}
	return false;
}

bool isValidVoidPtrSource(const Value* val, std::set<const PHINode*>& visitedPhis)
{
	if (DynamicAllocInfo::getAllocType(val) != DynamicAllocInfo::not_an_alloc )
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

bool isInlineable(const Instruction& I, const PointerAnalyzer& PA)
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
		if(PA.getPointerKind(&I)==COMPLETE_OBJECT && PA.getPointerKind(I.getOperand(0))==REGULAR)
			return false;
		return true;
	}
	else if(I.hasOneUse())
	{
		switch(I.getOpcode())
		{
			// A few opcodes if immediately used in a store or return can be inlined
			case Instruction::Call:
			case Instruction::Load:
			{
				const Instruction* nextInst=I.getNextNode();
				assert(nextInst);
				return I.user_back()==nextInst && (isa<StoreInst>(nextInst) || isa<ReturnInst>(nextInst));
			}
			case Instruction::Invoke:
			case Instruction::Ret:
			case Instruction::LandingPad:
			case Instruction::Store:
			case Instruction::InsertValue:
			case Instruction::PHI:
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
			case Instruction::IntToPtr:
				return true;
			default:
				llvm::report_fatal_error(Twine("Unsupported opcode: ",StringRef(I.getOpcodeName())), false);
				return true;
		}
	}
	return false;
}

uint32_t getIntFromValue(const Value* v)
{
	if(!ConstantInt::classof(v))
	{
		llvm::errs() << "Expected constant int found " << *v << "\n";
		llvm::report_fatal_error("Unsupported code found, please report a bug", false);
		return 0;
	}

	const ConstantInt* i=cast<const ConstantInt>(v);
	return i->getZExtValue();
}

std::string valueObjectName(const Value* v)
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
		else if (isa<const GlobalAlias>(p))
			os << "GlobalAlias";
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

bool TypeSupport::isDerivedStructType(StructType* derivedType, StructType* baseType)
{
	if(derivedType->getNumElements() < baseType->getNumElements())
		return false;
	// If a type is derived should begin with the same fields as the base type
	for(uint32_t i=0;i<baseType->getNumElements();i++)
	{
		if(derivedType->getElementType(i)!=baseType->getElementType(i))
			return false;
	}
	return true;
}

bool TypeSupport::isValidTypeCast(const Value * castOp, Type * dstPtr)
{
	Type * srcPtr = castOp->getType();

	//Only pointer casts are possible anyway
	assert(srcPtr->isPointerTy() && dstPtr->isPointerTy());
	Type * src = cast<PointerType>(srcPtr)->getElementType();
	Type * dst = cast<PointerType>(dstPtr)->getElementType();

	//Conversion between client objects is free
	if(isClientType(src) && isClientType(dst))
		return true;

	//Conversion between any function pointer is ok
	if(src->isFunctionTy() && dst->isFunctionTy())
		return true;

	//Allow conversions between equivalent struct types and base/derived types
	if(src->isStructTy() && dst->isStructTy())
	{
		StructType* srcSt = cast<StructType>(src);
		StructType* dstSt = cast<StructType>(dst);
		if(srcSt->isLayoutIdentical(dstSt))
			return true;
		if(isDerivedStructType(srcSt, dstSt))
			return true;
		if(isDerivedStructType(dstSt, srcSt))
			return true;
	}
	if(dst->isIntegerTy(8) && !src->isFunctionTy())
		return true;

	//Also allow the unsafe cast from i8* in a few selected cases
	if(src->isIntegerTy(8))
	{
		if ( isValidVoidPtrSource(castOp) )
		{
			bool allowedRawUsages = std::all_of(
				castOp->user_begin(),
				castOp->user_end(),
				[](const User * U) -> bool
				{
					//Check that the other use is a memset or an icmp
					if ( const ConstantExpr * ce = dyn_cast<ConstantExpr>(U) )
						return ce->getOpcode() == Instruction::BitCast;
					return isa<ICmpInst>(U) || isa<CastInst>(U) || safeCallForNewedMemory(dyn_cast<CallInst>(U));
				});

			if(allowedRawUsages)
				return true;
		}
	}
	if(hasByteLayout(src) && (ArrayType::classof(dst) || isTypedArrayType(dst) || hasByteLayout(dst)))
		return true;

	//Allow changing the size of an array
	if (ArrayType::classof(src) && ArrayType::classof(dst) &&
		src->getSequentialElementType() == dst->getSequentialElementType())
	{
		return true;
	}
	return false;
}

bool TypeSupport::getBasesInfo(const Module& module, const StructType* t, uint32_t& firstBase, uint32_t& baseCount)
{
	const NamedMDNode* basesNamedMeta = getBasesMetadata(t, module);
	if(!basesNamedMeta)
		return false;

	MDNode* basesMeta=basesNamedMeta->getOperand(0);
	assert(basesMeta->getNumOperands()==2);
	firstBase=getIntFromValue(cast<ConstantAsMetadata>(basesMeta->getOperand(0))->getValue());
	int32_t baseMax=getIntFromValue(cast<ConstantAsMetadata>(basesMeta->getOperand(1))->getValue())-1;
	baseCount=0;

	StructType::element_iterator E=t->element_begin()+firstBase;
	StructType::element_iterator EE=t->element_end();
	for(;E!=EE;++E)
	{
		baseCount++;
		StructType* baseT=cast<StructType>(*E);
		NamedMDNode* baseNamedMeta=module.getNamedMetadata(Twine(baseT->getName(),"_bases"));
		if(baseNamedMeta)
			baseMax-=getIntFromValue(cast<ConstantAsMetadata>(baseNamedMeta->getOperand(0)->getOperand(1))->getValue());
		else
			baseMax--;
		assert(baseMax>=0);
		if(baseMax==0)
			break;
	}
	return true;
}

bool TypeSupport::safeCallForNewedMemory(const CallInst* ci)
{
	//We allow the unsafe cast to i8* only
	//if the usage is free or delete
	//or one of the lifetime/invariant intrinsics
	return (ci && ci->getCalledFunction() &&
		(ci->getCalledFunction()->getName()=="free" ||
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

DynamicAllocInfo::DynamicAllocInfo( ImmutableCallSite callV ) : call(callV), type( getAllocType(callV) ), castedType(nullptr)
{
	if ( isValidAlloc() )
		castedType = computeCastedType();
}

DynamicAllocInfo::AllocType DynamicAllocInfo::getAllocType( ImmutableCallSite callV )
{
	if (callV.isCall() || callV.isInvoke() )
	{
		if (const Function * f = callV.getCalledFunction() )
		{
			if (f->getName() == "malloc")
				return malloc;
			else if (f->getName() == "calloc")
				return calloc;
			else if (f->getIntrinsicID() == Intrinsic::cheerp_allocate)
				return cheerp_allocate;
			else if (f->getIntrinsicID() == Intrinsic::cheerp_reallocate)
				return cheerp_reallocate;
			else if (f->getName() == "_Znwj")
				return opnew;
			else if (f->getName() == "_Znaj")
				return opnew_array;
		}
	}
	return not_an_alloc;
}

PointerType * DynamicAllocInfo::computeCastedType() const 
{
	assert(isValidAlloc() );
	
	if ( type == cheerp_allocate || type == cheerp_reallocate )
	{
		assert( call.getType()->isPointerTy() );
		return cast<PointerType>(call.getType());
	}
	
	auto getTypeForUse = [](const User * U) -> Type *
	{
		if ( isa<BitCastInst>(U) )
			return U->getType();
		else if ( const IntrinsicInst * ci = dyn_cast<IntrinsicInst>(U) )
			if ( ci->getIntrinsicID() == Intrinsic::cheerp_cast_user )
				return U->getType();
		return nullptr;
	};
	
	auto firstNonNull = std::find_if(
		call->user_begin(),
		call->user_end(),
		getTypeForUse);
	
	// If there are no casts, use i8*
	if ( call->user_end() == firstNonNull )
	{
		return cast<PointerType>(Type::getInt8PtrTy(call->getContext()));
	}
	
	assert( getTypeForUse(*firstNonNull)->isPointerTy() );
	
	PointerType * pt = cast<PointerType>( getTypeForUse(*firstNonNull) );
	
	// Check that all uses are the same
	if (! std::all_of( 
		std::next(firstNonNull),
		call->user_end(),
		[&]( const User * U ) { return getTypeForUse(U) == pt; }) )
	{
		llvm::errs() << "Can not deduce valid type for allocation instruction: " << call->getName() << '\n';
		llvm::report_fatal_error("Unsupported code found, please report a bug", false);
	}
	
	return pt;
}

const Value * DynamicAllocInfo::getByteSizeArg() const
{
	assert( isValidAlloc() );

	if ( calloc == type )
	{
		assert( call.arg_size() == 2 );
		return call.getArgument(1);
	}
	else if ( cheerp_reallocate == type )
	{
		assert( call.arg_size() == 2 );
		return call.getArgument(1);
	}

	assert( call.arg_size() == 1 );
	return call.getArgument(0);
}

const Value * DynamicAllocInfo::getNumberOfElementsArg() const
{
	assert( isValidAlloc() );
	
	if ( type == calloc )
	{
		assert( call.arg_size() == 2 );
		return call.getArgument(0);
	}
	return nullptr;
}

const Value * DynamicAllocInfo::getMemoryArg() const
{
	assert( isValidAlloc() );
	
	if ( type == cheerp_reallocate )
	{
		assert( call.arg_size() == 2 );
		return call.getArgument(0);
	}
	return nullptr;
}

bool DynamicAllocInfo::sizeIsRuntime() const
{
	assert( isValidAlloc() );
	if ( getAllocType() == calloc && isa<ConstantInt> (getNumberOfElementsArg() ) )
	{
		return false;
	}
	if ( isa<ConstantInt>(getByteSizeArg()) )
		return false;
	return true;
}

bool DynamicAllocInfo::useCreateArrayFunc() const
{
	if (getCastedType()->getElementType()->isStructTy() )
	{
		assert( !TypeSupport::isTypedArrayType( getCastedType()->getElementType() ) );
		return sizeIsRuntime() || type == cheerp_reallocate;
	}
	return false;
}

bool DynamicAllocInfo::useCreatePointerArrayFunc() const
{
	if (getCastedType()->getElementType()->isPointerTy() )
	{
		assert( !TypeSupport::isTypedArrayType( getCastedType()->getElementType() ) );
		return sizeIsRuntime() || type == cheerp_reallocate;
	}
	return false;
}

bool DynamicAllocInfo::useTypedArray() const
{
	return TypeSupport::isTypedArrayType( getCastedType()->getElementType() );
}

void EndOfBlockPHIHandler::runOnPHI(PHIRegs& phiRegs, uint32_t regId, llvm::SmallVector<const PHINode*, 4>& orderedPHIs)
{
	auto it=phiRegs.find(regId);
	if(it==phiRegs.end())
		return;
	PHIRegData& regData=it->second;
	if(regData.status==PHIRegData::VISITED)
		return;
	else if(regData.status==PHIRegData::VISITING)
	{
		// Report the recursive dependency to the user
		handleRecursivePHIDependency(regData.phiInst);
		return;
	}
	// Not yet visited
	regData.status=PHIRegData::VISITING;
	runOnPHI(phiRegs, regData.incomingReg, orderedPHIs);
	// Add the PHI to orderedPHIs only after eventual dependencies have been added
	orderedPHIs.push_back(regData.phiInst);
	regData.status=PHIRegData::VISITED;
}

void EndOfBlockPHIHandler::runOnEdge(const Registerize& registerize, const BasicBlock* fromBB, const BasicBlock* toBB)
{
	BasicBlock::const_iterator I=toBB->begin();
	BasicBlock::const_iterator IE=toBB->end();
	PHIRegs phiRegs;
	llvm::SmallVector<const PHINode*, 4> orderedPHIs;
	for(;I!=IE;++I)
	{
		// Gather the dependency graph between registers for PHIs and incoming values
		// Also add PHIs which are always safe to the orderedPHIs vector
		const PHINode* phi=dyn_cast<PHINode>(I);
		if(!phi)
			break;
		const Value* val=phi->getIncomingValueForBlock(fromBB);
		const Instruction* I=dyn_cast<Instruction>(val);
		if(!I)
		{
			orderedPHIs.push_back(phi);
			continue;
		}
		uint32_t incomingValueId = registerize.getRegisterId(I);
		uint32_t phiReg = registerize.getRegisterId(phi);
		if(incomingValueId==phiReg)
		{
			orderedPHIs.push_back(phi);
			continue;
		}
		phiRegs.insert(std::make_pair(phiReg, PHIRegData(phi, incomingValueId)));
	}
	for(auto it: phiRegs)
	{
		if(it.second.status!=PHIRegData::VISITED)
			runOnPHI(phiRegs, it.first, orderedPHIs);
	}
	// Notify the user for each PHI, in the right order to avoid accidental overwriting
	for(uint32_t i=orderedPHIs.size();i>0;i--)
	{
		const PHINode* phi=orderedPHIs[i-1];
		const Value* val=phi->getIncomingValueForBlock(fromBB);
		handlePHI(phi, val);
	}
}

}

namespace llvm
{

void initializeCheerpOpts(PassRegistry &Registry)
{
	initializeAllocaArraysPass(Registry);
	initializeAllocaMergingPass(Registry);
	initializeGlobalDepsAnalyzerPass(Registry);
	initializePointerAnalyzerPass(Registry);
	initializeRegisterizePass(Registry);
	initializeStructMemFuncLoweringPass(Registry);
}

}
