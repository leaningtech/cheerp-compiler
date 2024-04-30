//===-- Utility.cpp - Cheerp utility functions --------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2011-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include <cxxabi.h>
#include <sstream>
#include "llvm/Cheerp/JsExport.h"
#include "llvm/InitializePasses.h"
#include "llvm/Cheerp/Demangler.h"
#include "llvm/Cheerp/EdgeContext.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/Cheerp/GEPOptimizer.h"
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
			Type* t = newCall->getParamElementType(0);

			// Special case downcasts from a type to itself, they are used to support pointers to member functions
			if ( TypeSupport::isClientType(t) ||
				(isa<ConstantInt>( newCall->getArgOperand(1) ) && getIntFromValue( newCall->getArgOperand(1) ) == 0 && newCall->getArgOperand(0)->getType() != newCall->getType()))
				return true;
		}
		
	}
	return false;
}

bool isValidVoidPtrSource(const Value* val, std::set<const PHINode*>& visitedPhis)
{
	if (DynamicAllocInfo::getAllocType(cast<CallBase>(val)) != DynamicAllocInfo::not_an_alloc )
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

int32_t partialOffset(llvm::Type* & curType, llvm::Type* alternative, const llvm::DataLayout& DL, const int32_t index)
{
	int32_t partialOffset = 0;
	if(llvm::StructType* ST = dyn_cast<llvm::StructType>(curType))
	{
		const llvm::StructLayout* SL = DL.getStructLayout( ST );
		partialOffset = SL->getElementOffset(index);
		curType = ST->getElementType(index);
	}
	else
	{
		const uint32_t elementSize = DL.getTypeAllocSize(getElementType(curType, alternative));
		partialOffset = elementSize * index;
		curType = getElementType(curType, alternative);
	}
	return partialOffset;
}

bool isInlineable(const Instruction& I, const PointerAnalyzer& PA)
{
	InlineableCache cache(PA);
	return cache.isInlineableWithoutCache(I);
}

bool InlineableCache::isInlineableWithoutCache(const Instruction& I)
{
	//This version do not cache (but still needs the CacheIsInlineable object to operate on)
	//It calls isInlineableImpl, that calls back this function of recursion. It's basically a helper function since the "natural"
	//way of expressing isInlineableImpl<&CacheIsInlineable::isInlineableImpl<...??..>> would be infinitely recursive.

	//A cleaner solution would be the one proposed here: https://stackoverflow.com/questions/25078734/recursive-lambda-callbacks-without-y-combinator/25085574#25085574
	return isInlineableImpl<&InlineableCache::isInlineableWithoutCache>(I);
}

bool InlineableCache::isInlineable(const Instruction& I)
{
	//This is the function that checks whether the information is already cached, and it's called back on recursion

	auto it = cache.find(&I);
	if (it == cache.end())
	{
		it = cache.insert(std::make_pair(&I, isInlineableImpl<&InlineableCache::isInlineable>(I))).first;
	}
	assert(it->first == &I);
	return it->second;
}

template<InlineableCache::InstructionToBoolFunction recursiveCall>
bool InlineableCache::isInlineableImpl(const Instruction& I)
{
	//Take care when invoking recursiveCall: it has to be called like: (this->*recursiveCall)(paramethers)
	//This helper lamba probably keeps it easier
	auto isInlineableRecursion = [this](const Instruction& I) -> bool
		{
			return (this->*recursiveCall)(I);
		};

	//Beside a few cases, instructions with a single use may be inlined
	//TODO: Find out a better heuristic for inlining, it seems that computing
	//may be faster even on more than 1 use
	bool hasMoreThan1Use = I.hasNUsesOrMore(2);
	auto isUserAPhiInNextBlock = [](const Instruction& I)
	{
		assert(I.hasOneUse());
		const Use& U = *I.use_begin();
		const Instruction* userInst = cast<Instruction>(U.getUser());
		// Also allow PHIs in immediately following blocks
		if(const PHINode* phi = dyn_cast<PHINode>(userInst))
		{
			const BasicBlock* incomingBlock = phi->getIncomingBlock(U);
			if(incomingBlock==I.getParent())
				return true;
		}
		return false;
	};
	//Certain instructions require a given argument to be rendered more than once
	auto userRequiresMultipleRenderings = [](const Instruction& I)
	{
		assert(I.hasOneUse());
		const Use& U = *I.use_begin();
		const Instruction* userInst = cast<Instruction>(U.getUser());
		if (const FCmpInst* cmp = dyn_cast<FCmpInst>(userInst))
		{
			//Those FCmpInst will all need to be renderized as f(f(a,a),f(b,b)) or f(f(a,b),f(a,b))
			//So both arguments will be used twice
			switch (cmp->getPredicate())
			{
				case CmpInst::FCMP_ONE:
				case CmpInst::FCMP_UEQ:
				case CmpInst::FCMP_ORD:
				case CmpInst::FCMP_UNO:
					return true;
				default:
					break;
			}
		}
		if(const IntrinsicInst* II=dyn_cast<IntrinsicInst>(userInst))
		{
			if (II->getIntrinsicID() == Intrinsic::abs)
				//Abs will be rendered as (X >= 0) ? X : -X in both writers
				return true;
		}
		if(const AtomicCmpXchgInst* ai = dyn_cast<AtomicCmpXchgInst>(userInst))
		{
			if (&I == ai->getCompareOperand())
				return true;
		}
		return false;
	};
	// Do not inline the instruction if the use is in another block
	// If this happen the instruction may have been hoisted outside a loop and we want to keep it there
	auto isUserInOtherBlock = [&isUserAPhiInNextBlock](const Instruction& I)
	{
		// We should get here only if there is just 1 user
		if(I.use_empty())
			return false;
		// Easy case, is the user is in the same block as I?
		const Instruction* userInst = cast<Instruction>(I.use_begin()->getUser());
		const BasicBlock* userBlock = userInst->getParent();
		if(userBlock==I.getParent())
			return false;
		// Also allow PHIs in immediately following blocks
		return !isUserAPhiInNextBlock(I);
	};
	// On wasm it is efficient to inline constant geps, but only if the offset is positve
	// NOTE: This only checks the first index as an approximation, we would need DataLayout
	//       to compute the full offset exactly
	auto isPositiveOffsetGep = [](const GetElementPtrInst& gep)
	{
		if (!gep.hasAllConstantIndices())
			return false;
		ConstantInt* firstOffset = dyn_cast<ConstantInt>(gep.getOperand(1));
		if (!firstOffset)
			return false;
		return firstOffset->getSExtValue() >= 0;
	};
	auto isGepOnlyUsedInLoadStore = [&isPositiveOffsetGep](const Instruction& I)
	{
		auto& gep = cast<GetElementPtrInst>(I);
		if (!isPositiveOffsetGep(gep))
			return false;
		//If used only as pointerOperand for Store or Load, could be efficiently encoded in the offset of the Load/Store
		std::vector<const Instruction*> toCheck;
		toCheck.push_back(&gep);

		//Allow a single NON Load/Store use
		bool anyNonLoadStoreUse = false;

		while (!toCheck.empty())
		{
			const Instruction* I = toCheck.back();
			toCheck.pop_back();
			for(const Use& u: I->uses())
			{
				Instruction* userI = cast<Instruction>(u.getUser());
				if(isa<LoadInst>(userI))
					continue;
				else if(isa<StoreInst>(userI) && u.getOperandNo() == 1)
					continue;
				else if (isa<BitCastInst>(userI) ||
						(isa<GetElementPtrInst>(userI) && isPositiveOffsetGep(*cast<GetElementPtrInst>(userI))))
				{
					toCheck.push_back(userI);
					continue;
				}
				if (anyNonLoadStoreUse)
					return false;
				anyNonLoadStoreUse = true;
			}
		}
		return true;
	};
	if(I.getOpcode()==Instruction::GetElementPtr)
	{
		POINTER_KIND IPointerKind = PA.getPointerKind(&I);
		if(IPointerKind == RAW)
		{
			// Geps with constant indices used (in)directly only in Load or Store can be compactly encoded.
			if (isGepOnlyUsedInLoadStore(I))
				return true;
			if (hasMoreThan1Use || isUserInOtherBlock(I) || userRequiresMultipleRenderings(I))
				return false;
			return true;
		}

		// For generic JS, computing the gep in a local will not result
		// in smaller code due to the overhead of additional type casts.
		//
		// Note that geps that are used in equal pointer comparisons should
		// always be inlined. See also the assertions in
		// |CheerpWriter::compileEqualPointersComparison|.
		if (I.getNumOperands() == 2)
			return true;

		if (IPointerKind == COMPLETE_OBJECT) {
			auto type = cast<GetElementPtrInst>(I).getResultElementType();
			// Always inline geps to immutable fields of a complete object.
			if (TypeSupport::isImmutableType(type))
				return true;

			return !hasMoreThan1Use;
		}

		// Split regular, regular, and byte layout are always inlined.
		return true;
	}
	else if(I.getOpcode()==Instruction::BitCast)
	{
		if (!I.getType()->isPointerTy())
		{
			return !hasMoreThan1Use || !isa<Instruction>(I.getOperand(0)) || !isInlineableRecursion(*cast<Instruction>(I.getOperand(0)));
		}
		POINTER_KIND IPointerKind = PA.getPointerKind(&I);
		if(IPointerKind == RAW)
		{
			if(GetElementPtrInst* gep = dyn_cast<GetElementPtrInst>(I.getOperand(0)))
			{
				if (isGepOnlyUsedInLoadStore(*gep))
					return true;
			}
			return !hasMoreThan1Use || !isa<Instruction>(I.getOperand(0)) || !isInlineableRecursion(*cast<Instruction>(I.getOperand(0)));
		}

		if (IPointerKind == COMPLETE_OBJECT) {
			// Never inline if the source is REGULAR (forces conversion to CO)
			if(PA.getPointerKind(I.getOperand(0)) == REGULAR)
				return false;

			return !hasMoreThan1Use || !isa<Instruction>(I.getOperand(0)) || !isInlineableRecursion(*cast<Instruction>(I.getOperand(0)));
		}

		// Split regular, regular, and byte layout are always inlined.
		return true;
	}
	else if(I.getOpcode()==Instruction::Trunc)
	{
		//64 to 8/16/32 Trunc are not no-op, since they require explicitly to call the truncation in wasm (and in genericjs with BigInts they still require a call)
		if (I.getOperand(0)->getType()->isIntegerTy(64))
			return !hasMoreThan1Use;
		return !hasMoreThan1Use || !isa<Instruction>(I.getOperand(0)) || !isInlineableRecursion(*cast<Instruction>(I.getOperand(0)));
	}
	else if(const IntrinsicInst* II=dyn_cast<IntrinsicInst>(&I))
	{
		// Special handling for instrinsics
		switch(II->getIntrinsicID())
		{
			case Intrinsic::cheerp_cast_user:
			case Intrinsic::cheerp_upcast_collapsed:
			case Intrinsic::cheerp_make_regular:
				return true;
			default:
				break;
		}
		return false;
	}
	else if((I.getOpcode()==Instruction::FCmp || I.getOpcode()==Instruction::ICmp) && hasMoreThan1Use)
	{
		return !I.getOperand(0)->getType()->isPointerTy();
	}
	else if (I.use_empty())
	{
		return false;
	}
	else if(!hasMoreThan1Use)
	{
		if(isUserInOtherBlock(I) || userRequiresMultipleRenderings(I))
			return false;
		switch(I.getOpcode())
		{
			// A few opcodes if immediately used in a store or return can be inlined
			case Instruction::Call:
			case Instruction::Load:
			{
				if(I.getType()->isStructTy())
					return false;
				// We can only inline COMPLETE_OBJECT and RAW pointers, other kinds may actually require multiple accesses while rendering
				// NOTE: When RAW pointers are converted to REGULAR/SPLIT_REGULAR only one access (the offset part) is used, the base is a constant HEAP*
				if(I.getType()->isPointerTy())
				{
					POINTER_KIND k = PA.getPointerKind(&I);
					if(k != COMPLETE_OBJECT && k != RAW)
						return false;
				}

				const bool hasSideEffects = I.mayHaveSideEffects();
				// Skip up to N instructions, looking for the final not-inlineable user of this load/call
				// If we find no interfering instructions along the way it is safe to inline
				// TODO: Currently we assume that crossing an instruction implies reordering, but in reality this actually depends on rendering
				//       for example call(a, b) in JS guarantees that all the expression for 'a' in evaluated before 'b'
				const Instruction* curInst = &I;
				const Instruction* curUser = cast<Instruction>(I.user_back());
				if(curUser->getType()->isPointerTy() && PA.getPointerKind(curUser) == SPLIT_REGULAR)
				{
					// The user is rendered twice, do not inline
					return false;
				}
				const Instruction* nextInst = &I;
				while(1)
				{
					nextInst = nextInst->getNextNode();
					if(curUser == nextInst)
					{
						// Reached the direct user
						if(!nextInst->hasOneUse() &&
							(nextInst->getOpcode() == Instruction::BitCast ||
							nextInst->getOpcode() == Instruction::Trunc))
						{
							// Avoid interacting with the bitcast/trunc logic for now
							break;
						}
						else if(hasSideEffects &&
							(nextInst->getOpcode() == Instruction::Select ||
							(I.getType()->isIntegerTy(1) && (nextInst->getOpcode() == Instruction::And || nextInst->getOpcode() == Instruction::Or))))
						{
							// Do not inline side effects in selects and logical and/or, they don't evaluate both sides
							break;
						}
						else if(isa<IntrinsicInst>(nextInst))
						{
							// Avoid interacting with intrinsics logic for now
							break;
						}
						else if(!isInlineableRecursion(*nextInst))
						{
							// Not inlineable, it is safe to inline
							return true;
						}
						else if(nextInst->getOpcode() == Instruction::Call || nextInst->getOpcode() == Instruction::Load)
						{
							// Inlineable and this logic has already been done
							return true;
						}
						else if(!nextInst->hasOneUse())
						{
							break;
						}
						else
						{
							// It is inlineable, if it has only one user we can keep going
							curInst = nextInst;
							curUser = cast<Instruction>(curInst->user_back());
							InstElemIterator it(curUser, PA);
							if(std::next(it) != InstElemIterator::end(PA))
							{
								// The user is rendered multiple times, do not inline
								return false;
							}
						}
					}
					else if(hasSideEffects && nextInst->mayReadOrWriteMemory())
					{
						// Do not reorder side-effectfull calls over anything which may read memory
						break;
					}
					else if(nextInst->mayHaveSideEffects())
					{
						// This instruction is not the user and has side effects, give up
						break;
					}
					else if(nextInst->isTerminator())
					{
						// We have reached the end of the block without finding the final user
						// If the user is a phi directly following AND the function has no sideEffects -> good
						//		otherwise we cannot inline
						if (isUserAPhiInNextBlock(I) && !hasSideEffects)
							return true;
						break;
					}
				}
				return false;
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
			case Instruction::FRem:
			case Instruction::FNeg:
			case Instruction::FSub:
			case Instruction::FPTrunc:
			case Instruction::FPExt:
			case Instruction::FMul:
			case Instruction::FCmp:
			case Instruction::ICmp:
			case Instruction::ZExt:
			case Instruction::SExt:
			case Instruction::URem:
			case Instruction::UDiv:
			case Instruction::UIToFP:
			case Instruction::FPToUI:
			case Instruction::PtrToInt:
			case Instruction::IntToPtr:
			case Instruction::ShuffleVector:
				return true;
			case Instruction::ExtractElement:
			case Instruction::InsertElement:
			case Instruction::AtomicRMW:
			case Instruction::AtomicCmpXchg:
				return false;
			case Instruction::Select:
			{
				//TODO: allow inlining aggregates by handling select in compileAggregateElem
				return I.getType()->isStructTy()? false : true;
			}
			case Instruction::ExtractValue:
			{
				if(I.getType()->isPointerTy())
				{
					POINTER_KIND k = PA.getPointerKind(&I);
					if(k != COMPLETE_OBJECT && k != RAW)
						return false;
				}
				return true;
			}
			default:
				llvm::report_fatal_error(Twine("Unsupported opcode: ",StringRef(I.getOpcodeName())), false);
				return true;
		}
	}
	return false;
}

bool canDelayPHI(const PHINode* phi, const PointerAnalyzer& PA, const Registerize& registerize, const EdgeContext& edgeContext)
{
	// If for all incoming we have
	// 1) A not-inlineable instruction
	// 2) The same registers
	// 3) The registers are a subset of the PHI registers
	// 4) The same pointer kind
	// 5) The same constant offset
	//
	// Then we can compile this PHI only once in the destination block
	// NOTE: We require the registers to be a subset of the PHI's to make sure
	// that they are not overwritten by any other PHI

	// We only care about pointers, for other types if the incoming register and the PHI register
	// are the same then the PHI is removed completely
	if(phi->use_empty() || !phi->getType()->isPointerTy())
		return false;
	auto phiRegs = registerize.getAllRegisterIds(phi, EdgeContext::emptyContext());
	POINTER_KIND phiKind = PA.getPointerKind(phi);
	const ConstantInt* phiOffset = PA.getConstantOffsetForPointer(phi);
	// Get expected values from incoming 0
	const Instruction* incomingInst0 = getUniqueIncomingInst(phi->getIncomingValue(0), PA);
	if(!incomingInst0)
		return false;
	assert(!isInlineable(*incomingInst0, PA));
	auto incomingRegs0 = registerize.getAllRegisterIds(incomingInst0, edgeContext);
	bool isSubset = true;
	for(uint32_t ir: incomingRegs0)
	{
		auto it = std::find(phiRegs.begin(), phiRegs.end(), ir);
		if(it == phiRegs.end())
		{
			isSubset = false;
			break;
		}
	}
	if(!isSubset)
		return false;
	POINTER_KIND incomingKind0 = PA.getPointerKind(incomingInst0);
	const ConstantInt* incomingOffset0 = PA.getConstantOffsetForPointer(incomingInst0);
	if(incomingKind0 == phiKind && incomingOffset0 == phiOffset)
	{
		// Same register, same kind and same offset. This PHI will just disapper
		return false;
	}
	for(uint32_t i=1;i<phi->getNumIncomingValues();i++)
	{
		const Instruction* incomingInst = getUniqueIncomingInst(phi->getIncomingValue(i), PA);
		if(!incomingInst)
			return false;
		assert(!isInlineable(*incomingInst, PA));
		auto incomingRegs = registerize.getAllRegisterIds(incomingInst, edgeContext);
		POINTER_KIND incomingKind = PA.getPointerKind(incomingInst);
		const ConstantInt* incomingOffset = PA.getConstantOffsetForPointer(incomingInst);
		if(incomingRegs != incomingRegs0 ||
			incomingKind != incomingKind0 ||
			incomingOffset != incomingOffset0)
		{
			return false;
		}
	}
	return true;
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

bool hasNonLoadStoreUses( const Value* v)
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

Type* getGEPContainerType(const User* gep)
{
	SmallVector< const Value*, 8 > indices(std::next(gep->op_begin()), std::prev(gep->op_end()));
	assertPointerElementOrOpaque(gep->getOperand(0)->getType(), cast<GEPOperator>(gep)->getSourceElementType());
	Type* containerType = GetElementPtrInst::getIndexedType(cast<GEPOperator>(gep)->getSourceElementType(),
			makeArrayRef(const_cast<Value* const*>(indices.begin()),
				     const_cast<Value* const*>(indices.end())));
	return containerType;
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

bool TypeSupport::getBasesInfo(const Module& module, const StructType* t, uint32_t& firstBase, uint32_t& baseCount)
{
	const NamedMDNode* basesNamedMeta = getBasesMetadata(t, module);
	if(!basesNamedMeta)
		return false;

	MDNode* basesMeta=basesNamedMeta->getOperand(0);
	assert(basesMeta->getNumOperands()>=1);
	firstBase=getIntFromValue(cast<ConstantAsMetadata>(basesMeta->getOperand(0))->getValue());
	assert(firstBase < t->getNumElements());
	if (basesMeta->getNumOperands() >=2)
		baseCount=getIntFromValue(cast<ConstantAsMetadata>(basesMeta->getOperand(1))->getValue());
	else
		baseCount = t->getNumElements()-firstBase;

	return true;
}

bool TypeSupport::useWrapperArrayForMember(const PointerAnalyzer& PA, StructType* st, uint32_t memberIndex) const
{
	uint32_t firstBase, baseCount;
	if(getBasesInfo(st, firstBase, baseCount))
	{
		if(memberIndex >= firstBase && memberIndex < (firstBase+baseCount) && st->getElementType(memberIndex)->isStructTy())
			return false;
	}
	if(st->getDirectBase() && memberIndex < st->getDirectBase()->getNumElements())
		return useWrapperArrayForMember(PA, st->getDirectBase(), memberIndex);
	// We don't want to use the wrapper array if the downcast array is alredy available
	TypeAndIndex baseAndIndex(st, memberIndex, TypeAndIndex::STRUCT_MEMBER);
	assert(PA.getPointerKindForMember(baseAndIndex)!=SPLIT_REGULAR);
	return PA.getPointerKindForMember(baseAndIndex)==REGULAR;
}

char TypeSupport::getPrefixCharForMember(const PointerAnalyzer& PA, llvm::StructType* st, uint32_t memberIndex) const
{
	bool useWrapperArray = useWrapperArrayForMember(PA, st, memberIndex);
	Type* elementType = st->getElementType(memberIndex);
	if(useWrapperArray)
		return 'a';
	else if(elementType->isIntegerTy() && !elementType->isIntegerTy(64))
		return 'i';
	else if(elementType->isFloatTy() || elementType->isDoubleTy())
		return 'd';
	else
		return 'a';
}

bool TypeSupport::isJSExportedType(StructType* st, const Module& m)
{
	if (st->isLiteral())
		return false;
	if (!jsExportedTypes)
	{
		jsExportedTypes.emplace();
		for (auto record : getJsExportRecords(m))
			jsExportedTypes->insert(record.getStructName());
	}
	return jsExportedTypes->contains(st->getStructName());
}

std::optional<llvm::StringSet<>> TypeSupport::jsExportedTypes;

bool TypeSupport::isSimpleType(Type* t, bool forceTypedArrays)
{
	switch(t->getTypeID())
	{
		case Type::IntegerTyID:
		case Type::FloatTyID:
		case Type::DoubleTyID:
		case Type::PointerTyID:
			return true;
		case Type::StructTyID:
		{
			// Union are considered simple because they use a single DataView object
			if(TypeSupport::hasByteLayout(t))
				return true;
			break;
		}
		case Type::ArrayTyID:
		{
			ArrayType* at=static_cast<ArrayType*>(t);
			Type* et=at->getElementType();
			// When a single typed array object is used, we consider this array as simple
			if(isTypedArrayType(et, forceTypedArrays) && at->getNumElements()>1)
				return true;
			if(TypeSupport::hasByteLayout(t))
				return true;
			break;
		}
		default:
			assert(false);
	}
	return false;
}

uint32_t TypeSupport::getAlignmentAsmJS(const llvm::DataLayout& dl, llvm::Type* t)
{
	uint32_t alignment = 8;
	// it the type is an array, look at the element type
	while (t->isArrayTy())
	{
		t = t->getArrayElementType();
	}
	// NOTE: we could compute the real minimum alignment with a
	//       recursive scan of the struct, but instead we just
	//       align to 8 bytes
	if (t->isStructTy())
	{
		alignment = 8;
	}
	else
	{
		alignment = dl.getTypeAllocSize(t);
	}

	return alignment;
}

std::pair<std::string, std::string> TypeSupport::ClientFunctionDemangled::getClientNamespacedAndFunc(const char* identifier)
{
	cheerp::Demangler demangler(identifier);

	assert(demangler.isMangled() && demangler.isNamespaceClient());

	std::string namespaced = demangler.getJSMangling(/*doCleanup*/true);
	std::string funcName = "";

	if (demangler.isFunction())
	{
		while (!namespaced.empty() && namespaced.back() != '.')
		{
			funcName += namespaced.back();
			namespaced.pop_back();
		}

		std::reverse(funcName.begin(), funcName.end());
	}

	return std::make_pair(namespaced, funcName);
}

DynamicAllocInfo::DynamicAllocInfo( const CallBase* callV, const DataLayout* DL, bool forceTypedArrays ) : call(callV), type( getAllocType(callV) ), castedElementType(nullptr), forceTypedArrays(forceTypedArrays)
{
	if ( isValidAlloc() )
	{
		castedElementType = computeCastedElementType();
		typeSize = DL->getTypeAllocSize(castedElementType);
	}
}

DynamicAllocInfo::AllocType DynamicAllocInfo::getAllocType( const CallBase* callV )
{
	// The alloc type is always not_an_alloc in asmjs, since we don't need
	// thr DynamicAllocInfo functionality
	if (callV == nullptr || callV->getParent()->getParent()->getSection() == StringRef("asmjs"))
		return not_an_alloc;
	DynamicAllocInfo::AllocType ret = not_an_alloc;
	{
		if (const Function * f = callV->getCalledFunction() )
		{
			if (f->getName() == "malloc")
				ret = malloc;
			else if (f->getName() == "calloc")
				ret = calloc;
			else if (f->getIntrinsicID() == Intrinsic::cheerp_allocate ||
			         f->getIntrinsicID() == Intrinsic::cheerp_allocate_array)
				ret =  cheerp_allocate;
			else if (f->getIntrinsicID() == Intrinsic::cheerp_reallocate)
				ret = cheerp_reallocate;
			else if (f->getName() == "_Znwj")
				ret = opnew;
			else if (f->getName() == "_Znaj")
				ret = opnew_array;
		}
	}
	// As above, allocations of asmjs types are considered not_an_alloc
	if (ret != not_an_alloc && TypeSupport::isAsmJSPointer(callV->getType()))
		return not_an_alloc;
	return ret;
}

Type * DynamicAllocInfo::computeCastedElementType() const
{
	assert(isValidAlloc() );
	
	if ( type == cheerp_allocate || type == cheerp_reallocate )
	{
		assert( call->getType()->isPointerTy() );
		assert( call->getParamElementType(0) );
		return call->getParamElementType(0);
	}
	
	auto getTypeForUse = [](const User * U) -> Type *
	{
		if ( isa<BitCastInst>(U) )
			return U->getType()->getNonOpaquePointerElementType();
		else if ( const IntrinsicInst * ci = dyn_cast<IntrinsicInst>(U) )
			if ( ci->getIntrinsicID() == Intrinsic::cheerp_cast_user )
				return ci->getParamElementType(0);
		return nullptr;
	};
	
	auto firstNonNull = std::find_if(
		call->user_begin(),
		call->user_end(),
		getTypeForUse);
	
	// If there are no casts, use i8*
	if ( call->user_end() == firstNonNull )
	{
		return Type::getInt8Ty(call->getContext());
	}
	
	Type * pt = getTypeForUse(*firstNonNull);
	assert(pt);
	
	// Check that all uses are the same
	if (! std::all_of( 
		std::next(firstNonNull),
		call->user_end(),
		[&]( const User * U ) { return getTypeForUse(U) == pt; }) )
	{
#ifndef NDEBUG
		call->getParent()->getParent()->dump();
		llvm::errs() << "Can not deduce valid type for allocation instruction: " << call->getName() << '\n';
		llvm::errs() << "In function: " << call->getParent()->getParent()->getName() << "\n";
		llvm::errs() << "Allocation instruction: "; call->dump();
		llvm::errs() << "Pointer: "; pt->dump();
		llvm::errs() << "Usage:\n";
		for (auto u = call->user_begin(); u != call->user_end(); u++)
		{
			u->dump();
		}
#endif
		llvm::report_fatal_error("Unsupported code found, please report a bug", false);
	}
	
	return pt;
}

const Value * DynamicAllocInfo::getByteSizeArg() const
{
	assert( isValidAlloc() );
	if ( calloc == type )
	{
		assert( call->arg_size() == 2 );
		return call->getOperand(1);
	}
	else if ( cheerp_allocate == type || cheerp_reallocate == type )
	{
		assert( call->arg_size() == 2 );
		return call->getOperand(1);
	}

	assert( call->arg_size() == 1 );
	return call->getOperand(0);
}

const Value * DynamicAllocInfo::getNumberOfElementsArg() const
{
	assert( isValidAlloc() );
	
	if ( type == calloc )
	{
		assert( call->arg_size() == 2 );
		return call->getOperand(0);
	}
	return nullptr;
}

const Value * DynamicAllocInfo::getMemoryArg() const
{
	assert( isValidAlloc() );
	
	if ( type == cheerp_reallocate )
	{
		assert( call->arg_size() == 2 );
		return call->getOperand(0);
	}
	return nullptr;
}

bool DynamicAllocInfo::sizeIsRuntime() const
{
	assert( isValidAlloc() );
	if ( getAllocType() == calloc && !isa<ConstantInt> (getNumberOfElementsArg() ) )
		return true;
	if ( isa<ConstantInt>(getByteSizeArg()) )
		return false;
	return true;
}

bool DynamicAllocInfo::useCreateArrayFunc() const
{
	if( !TypeSupport::isTypedArrayType( getCastedPointedType(), forceTypedArrays ) )
	{
		if( sizeIsRuntime() || type == cheerp_reallocate)
			return true;
		// Should also use createArray if allocating many elements
		uint32_t byteSize = cast<ConstantInt>(getByteSizeArg())->getZExtValue();
		return byteSize/typeSize > 8;
	}
	return false;
}

bool DynamicAllocInfo::useCreatePointerArrayFunc() const
{
	if (getCastedPointedType()->isPointerTy() )
	{
		assert( !TypeSupport::isTypedArrayType( getCastedPointedType(), forceTypedArrays) );
		if( sizeIsRuntime() || type == cheerp_reallocate)
			return true;
		// Should also use createPointerArray if allocating many elements
		uint32_t byteSize = cast<ConstantInt>(getByteSizeArg())->getZExtValue();
		return byteSize/typeSize > 8;
	}
	return false;
}

bool DynamicAllocInfo::useTypedArray() const
{
	return TypeSupport::isTypedArrayType( getCastedPointedType(), forceTypedArrays);
}

static const ConstantArray* getConstructorsConst(Module& M)
{
	GlobalVariable* var = M.getGlobalVariable("llvm.global_ctors");
	if (!var || !var->hasInitializer())
		return nullptr;

	if (!isa<ConstantArray>(var->getInitializer()))
		return nullptr;

	return cast<ConstantArray>(var->getInitializer());
}

std::vector<Constant*> getGlobalConstructors(Module& module)
{
	//Process constructors
	const ConstantArray* constructors = getConstructorsConst(module);
	if (!constructors)
		return {};

	std::vector<Constant*> ret;
	ret.reserve(constructors->getNumOperands());
	for (auto& el: constructors->operands())
	{
		Constant* C = cast<Constant>(el);
		ret.push_back(C);
	}
	std::stable_sort(ret.begin(), ret.end(), [](auto& a, auto& b)
	{
		return cast<ConstantInt>(a->getAggregateElement(0u))->getSExtValue()
			< cast<ConstantInt>(b->getAggregateElement(0u))->getSExtValue();
	});
	return ret;
}

const llvm::Loop* findCommonLoop(const llvm::LoopInfo* LI, const llvm::BasicBlock* first, const llvm::BasicBlock* second)
{
	//Find the innermost common loop between two BB.
	//Note that nullptr is returned when there are no common loops

	LoopWithDepth A(LI->getLoopFor(first));
	LoopWithDepth B(LI->getLoopFor(second));

	//If they are in a common loop, it should have the same depth
	while (A.depth != B.depth)
	{
		if (A.depth > B.depth)
			A.stepBack();
		else
			B.stepBack();
	}

	//And should also be in the same loop
	while (A.loop && A.loop != B.loop)
	{
		A.stepBack();
		B.stepBack();
	}

	assert(A.depth == B.depth);
	assert(A.loop == B.loop);

	//Either a common loop has been found, or we stopped since they were both NULL
	return A.loop;
}

const Instruction* findCommonInsertionPoint(const Instruction* I, const DominatorTree* DT, const Instruction* currentInsertionPoint, const Instruction* user)
{
	return const_cast<const Instruction*>(findCommonInsertionPoint(I, DT, const_cast<Instruction*>(currentInsertionPoint), const_cast<Instruction*>(user)));
}

Instruction* findCommonInsertionPoint(const Instruction* I, const DominatorTree* DT, Instruction* currentInsertionPoint, Instruction* user)
{
	if(PHINode* phi = dyn_cast<PHINode>(user))
	{
		// It must dominate all incoming blocks that has the value as an incoming value
		for(unsigned i = 0; i < phi->getNumIncomingValues(); i++)
		{
			if(phi->getIncomingValue(i) != I)
				continue;
			BasicBlock* incomingBlock = phi->getIncomingBlock(i);
			currentInsertionPoint = findCommonInsertionPoint(I, DT, currentInsertionPoint, incomingBlock->getTerminator());
		}
		return currentInsertionPoint;
	}
	if(!currentInsertionPoint || DT->dominates(user, currentInsertionPoint))
		return user;
	else if(DT->dominates(currentInsertionPoint, user))
		return currentInsertionPoint;
	else if(currentInsertionPoint->getParent() == user->getParent())
	{
		// Check relative order, find it currentInsertionPoint is above user
		Instruction* it = currentInsertionPoint;
		while(it)
		{
			if(it == user)
			{
				// user is after currentInsertionPoint
				return currentInsertionPoint;
			}
			it = it->getNextNode();
		}
		// user is above currentInsertionPoint
		return user;
	}
	else // Find a common dominator
	{
		//llvm::findNearestCommonDominator should become a const function, and the const_cast could then be dropped
		BasicBlock* common = const_cast<DominatorTree*>(DT)->findNearestCommonDominator(currentInsertionPoint->getParent(),user->getParent());
		return common->getTerminator();
	}
}

const Instruction* getUniqueIncomingInst(const Value* v, const PointerAnalyzer& PA)
{
	while(const Instruction* I = dyn_cast<Instruction>(v))
	{
		if(!isInlineable(*I, PA))
			return I;
		else if(I->getOpcode() == Instruction::Trunc)
			v = I->getOperand(0);
		else if(I->getOpcode() == Instruction::BitCast && I->getType()->isPointerTy() && PA.getPointerKind(I) == RAW)
		{
			// TODO: Expand this logic to support other cases where a bitcast is a nop (when no kind conversion is required?)
			v = I->getOperand(0);
		}
		else if(I->getOpcode() == Instruction::GetElementPtr && PA.getPointerKind(I) == RAW && cast<GetElementPtrInst>(I)->hasAllZeroIndices())
		{
			v = I->getOperand(0);
		}
		else
			break;
	}
	return nullptr;
}

bool mayContainSideEffects(const Value* V, const PointerAnalyzer& PA)
{
	if (!isa<Instruction>(V))
		return false;
	const Instruction* I = cast<Instruction>(V);
	if (!isInlineable(*I, PA))
		return false;
	if (I->mayHaveSideEffects())
		return true;
	for (Value* Op: I->operands())
	{
		if(mayContainSideEffects(Op, PA))
			return true;
	}
	return false;
}

Instruction* getOrCreateNextInsertPoint(Instruction& I)
{
	//For InvokeInst this possibly creates a BB where to put instructions
	//This will possibly invalidate Analysis passes
	if (InvokeInst* invoke = dyn_cast<InvokeInst>(&I))
	{
		BasicBlock* normalDest = invoke->getNormalDest();

		if (BasicBlock* pred = normalDest->getSinglePredecessor())
		{
			Instruction* firstNonPHI = normalDest->getFirstNonPHI();
			if (firstNonPHI == &*normalDest->begin())
			{
				//Single predecessors BB without phis is a valid insertion point
				return firstNonPHI;
			}
		}

		Function& F = *invoke->getFunction();
		BasicBlock* forwardBB = BasicBlock::Create(F.getParent()->getContext());
		forwardBB->setName("invokeNextNode");
		forwardBB->insertInto(&F);

		//Rewire branches and phi
		//invoke -> forwardBB -> normalDest
		//  \
		//   -----> unwindDest
		normalDest->replacePhiUsesWith(invoke->getParent(), forwardBB);
		BranchInst* uncondBranch = BranchInst::Create(normalDest, forwardBB);
		invoke->setNormalDest(forwardBB);

		return uncondBranch;
	}

	//Other terminators are not handled
	assert(!I.isTerminator());

	return I.getNextNode();
}

bool replaceCallOfBitCastWithBitCastOfCall(CallBase& callInst, bool mayFail, bool performPtrIntConversions)
{
	auto addCast = [&performPtrIntConversions](Value* src, Type* oldType, Type* newType, Instruction* insertPoint) -> Value*
	{
		if(oldType->isIntegerTy() && newType->isPointerTy()) {
			if (PtrToIntOperator* ptrToInt = dyn_cast<PtrToIntOperator>(src))
				if (ptrToInt->getOperand(0)->getType() == newType)
					return ptrToInt->getOperand(0);
			assert(performPtrIntConversions);
			return new IntToPtrInst(src, newType, "", insertPoint);
		} else if(oldType->isPointerTy() && newType->isIntegerTy()) {
			if (IntToPtrInst* intToPtr = dyn_cast<IntToPtrInst>(src))
				if (intToPtr->getOperand(0)->getType() == newType)
					return intToPtr->getOperand(0);
			assert(performPtrIntConversions);
			return new PtrToIntInst(src, newType, "", insertPoint);
		} else if(oldType->isPointerTy() && newType->isPointerTy()) {
			return new BitCastInst(src, newType, "", insertPoint);
		} else if(oldType->isIntegerTy() && newType->isIntegerTy()) {
			if(oldType->getIntegerBitWidth() < newType->getIntegerBitWidth())
				return new ZExtInst(src, newType, "", insertPoint);
			else
				return new TruncInst(src, newType, "", insertPoint);
		} else if (oldType->isVoidTy()) {
			// NOTE: This case is only ever encountered for return values, and in that case oldType is the new one
			//       If we get here it means that we have replaced a bitcast from a void function to non-void,
			//       generate an undefined values
			return UndefValue::get(newType);
		} else {
			llvm_unreachable("Unexpected cast required");
		}
	};

	ConstantExpr* bitCast = dyn_cast<ConstantExpr>(callInst.getCalledOperand());

	if (!bitCast)
	{
		//All is already taken care of
		return false;
	}

	if (bitCast->getOpcode() != Instruction::BitCast)
	{
		assert(mayFail && "ConstantExpr BitCast expected");
		return false;
	}

	Function* F = dyn_cast<Function>(bitCast->getOperand(0));
	if (!F)
	{
		assert(mayFail && "Function expected");
		return false;
	}
	FunctionType* FTy = F->getFunctionType();

	if (FTy->getNumParams() > callInst.arg_size() || (FTy->getNumParams() < callInst.arg_size() && !FTy->isVarArg()))
	{
		assert(mayFail && "Equal number of parameters expected");
		return false;
	}

	//Add casts for each operand that needs them
	AttributeList NewAttrs = callInst.getAttributes();
	for (uint32_t i=0; i<FTy->getNumParams(); i++)
	{
		Type* originalTy = callInst.getArgOperand(i)->getType();
		Type* nextTy = FTy->getParamType(i);
		if (originalTy != nextTy)
		{
			Value* cast = addCast(callInst.getArgOperand(i), originalTy, nextTy, &callInst);
			if ((originalTy->isPointerTy() && nextTy->isIntegerTy()) || (originalTy->isIntegerTy() && nextTy->isPointerTy())) {
				NewAttrs = NewAttrs.removeParamAttributes(callInst.getContext(), i);
			}
			callInst.setArgOperand(i, cast);
		}
	}

	Type* oldReturnType = callInst.getType();
	Type* newReturnType = FTy->getReturnType();

	const bool returnTypeChanged = (oldReturnType != newReturnType);
	if (returnTypeChanged)
		callInst.mutateType(newReturnType);
	if (returnTypeChanged && !oldReturnType->isVoidTy() && !newReturnType->isVoidTy())
	{
		//getOrCreateNextInsertPoint might possibly create a forwarding BB for InvokeInst
		Value* n = addCast(&callInst, newReturnType, oldReturnType, getOrCreateNextInsertPoint(callInst));
		if ((oldReturnType->isPointerTy() && newReturnType->isIntegerTy()) || (oldReturnType->isIntegerTy() && newReturnType->isPointerTy())) {
			NewAttrs = NewAttrs.removeAttributesAtIndex(callInst.getContext(), AttributeList::ReturnIndex);
		}
		if (n != &callInst)
		{
			// Appease 'replaceAllUsesWith'
			callInst.mutateType(oldReturnType);
			callInst.replaceAllUsesWith(n);
			callInst.mutateType(newReturnType);
			// 'replaceAllUsesWith' also changes the cast, restore it
			if(isa<Instruction>(n))
				cast<Instruction>(n)->setOperand(0, &callInst);
		}
	}
	callInst.setAttributes(NewAttrs);
	// Parameters and returns are fixed, now fix the types and the called functions
	callInst.mutateFunctionType(FTy);
	callInst.setCalledFunction(F);

	return true;
}

void replaceSomeUsesWith(std::vector<Use*> uses, Value* toSubstitute)
{
	for (auto U : uses)
	{
		if (Constant* C = dyn_cast<Constant>(U->getUser()))
		{
			Value* V = U->get();
			if (V != toSubstitute)
				C->handleOperandChange(V, toSubstitute);
		}
		else
		{
			U->set(toSubstitute);
		}
	}
}

Function* getMainFunction(Module& module)
{
	if (llvm::Function* webMainOrMain = module.getFunction("_Z7webMainv"))
		return webMainOrMain;
	if (llvm::Function* webMainOrMain = module.getFunction("webMain"))
		return webMainOrMain;
	if (llvm::Function* webMainOrMain = module.getFunction("main"))
		return webMainOrMain;

	return nullptr;
}

const Function* getMainFunction(const Module& module)
{
	return getMainFunction(const_cast<Module&>(module));
}


void setForceRawAttribute(Module& M, Function* Wrapper)
{
	// Force PA to treat pointers of basic types coming in and out of this wrapper as RAW.
	AttributeList Attrs;
	for(auto& arg: Wrapper->args())
	{
		if (TypeSupport::isRawPointer(arg.getType(), true) && !TypeSupport::isAsmJSPointer(arg.getType()))
		{
			Attrs = Attrs.addParamAttribute(M.getContext(), arg.getArgNo(), "force-raw");
		}
	}
	if (TypeSupport::isRawPointer(Wrapper->getReturnType(), true) && !TypeSupport::isAsmJSPointer(Wrapper->getReturnType()))
	{
		Attrs = Attrs.addRetAttribute(M.getContext(), Attribute::get(M.getContext(), "force-raw"));
	}
	Wrapper->setAttributes(Attrs);
}

unsigned getVectorBitwidth(const FixedVectorType* vecType)
{
	unsigned elementSize;
	if (vecType->getElementType()->isPointerTy())
		elementSize = 32;
	else
		elementSize = vecType->getScalarSizeInBits();
	const unsigned vectorBitwidth = vecType->getNumElements() * elementSize;
	return vectorBitwidth;
}

bool hasSIMDAttribute(const Function* F)
{
	if (!F->hasFnAttribute("target-features"))
		return false;
	Attribute Attr = F->getFnAttribute("target-features");
	if (!Attr.isStringAttribute())
		return false;
	StringRef allFeatures = Attr.getValueAsString();
	size_t pos = 0;
	size_t len = 0;
	StringRef feature;
	while (len != std::string::npos)
	{
		len = allFeatures.find(",", pos);
		feature = allFeatures.substr(pos, len - pos);
		if (feature == "+simd128")
			return true;
		pos = len + 1;
	}
	return false;
}

void removeSIMDAttribute(Function* F)
{
	if (!F->hasFnAttribute("target-features"))
		return;
	Attribute Attr = F->getFnAttribute("target-features");
	if (!Attr.isStringAttribute())
		return;
	StringRef allFeatures = Attr.getValueAsString();
	std::vector<StringRef> features;
	size_t pos = 0;
	size_t len = 0;
	StringRef feature;
	while (len != std::string::npos)
	{
		len = allFeatures.find(",", pos);
		feature = allFeatures.substr(pos, len - pos);
		features.push_back(feature);
		pos = len + 1;
	}
	auto it = std::find(features.begin(), features.end(), "+simd128");
	if (it == features.end())
		return;
	features.erase(it);
	F->removeFnAttr("target-features");
	if (features.empty())
		return;
	F->addFnAttr("target-features", llvm::join(features, ","));
}

InstElemIterator& InstElemIterator::operator++()
{
	assert(*this != end(*PA));
	llvm::Type* Ty = inner.instruction->getType();
	if(auto* SI = llvm::dyn_cast<llvm::StoreInst>(inner.instruction))
		Ty = SI->getValueOperand()->getType();
	inner.totalIdx++;
	if(inner.ptrIdx == 0 && isTwoElems(inner.instruction, Ty, inner.structIdx, *PA))
	{
		inner.ptrIdx = 1;
		return *this;
	}
	if(!Ty->isStructTy() || inner.structIdx == Ty->getStructNumElements()-1)
	{
		inner = InstElem(nullptr);
		return *this;
	}
	inner.structIdx++;
	inner.ptrIdx = 0;
	return *this;
}

bool InstElemIterator::isTwoElems(const llvm::Instruction* I, llvm::Type* Ty, int32_t structIdx, const PointerAnalyzer& PA)
{
	if(!Ty->isStructTy())
	{
		return Ty->isPointerTy() && PA.getPointerKind(I) == SPLIT_REGULAR && !PA.getConstantOffsetForPointer(I);
	}
	auto* STy = llvm::cast<llvm::StructType>(Ty);
	if(!STy->getElementType(structIdx)->isPointerTy())
		return false;
	TypeAndIndex b(STy, structIdx, TypeAndIndex::STRUCT_MEMBER);
	auto kind = PA.getPointerKindForMemberPointer(b);
	bool hasConstantOffset = PA.getConstantOffsetForMember(b) != NULL;
	return (kind == SPLIT_REGULAR && !hasConstantOffset);
}

}
