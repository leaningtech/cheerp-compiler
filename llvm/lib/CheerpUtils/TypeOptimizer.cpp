//===-- TypeOptimizer.cpp - Cheerp helper -------------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2015 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/Utility.h"
#include "llvm/Cheerp/TypeOptimizer.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/Support/raw_ostream.h"
#include <set>

using namespace llvm;

namespace cheerp
{

const char *TypeOptimizer::getPassName() const {
	return "TypeOptimizer";
}

void TypeOptimizer::gatherAllDowncastSourceTypes(const Module& M)
{
	for(const Function& F: M)
	{
		for(const BasicBlock& BB: F)
		{
			for(const Instruction& I: BB)
			{
				if(const IntrinsicInst* II=dyn_cast<IntrinsicInst>(&I))
				{
					if(II->getIntrinsicID()!=Intrinsic::cheerp_downcast)
						continue;
					// If a source type is downcasted with an offset != 0 we can't collapse the type
					// we keep track of this by setting the mapping to an empty vector
					StructType* sourceType = cast<StructType>(II->getOperand(0)->getType()->getPointerElementType());
					if(cast<ConstantInt>(II->getOperand(1))->getZExtValue() != 0)
					{
						downcastSourceToDestinationsMapping[sourceType].clear();
						continue;
					}
					// If the offset is 0 we need to append the destination type to the mapping
					// If the source type is in the map, but the vector is empty it means that we were
					// in the case above, so we don't add the new destType
					StructType* destType = cast<StructType>(II->getType()->getPointerElementType());
					auto it=downcastSourceToDestinationsMapping.find(sourceType);
					if(it != downcastSourceToDestinationsMapping.end() && it->second.empty())
						continue;
					downcastSourceToDestinationsMapping[sourceType].insert(destType);
				}
			}
		}
	}
}

/**
	We can only collapse a downcast source if all the possible destinations collapse as well
*/
bool TypeOptimizer::isUnsafeDowncastSource(StructType* st)
{
	auto it=downcastSourceToDestinationsMapping.find(st);
	if(it == downcastSourceToDestinationsMapping.end())
		return false;
	// If the destinations set is empty it means that we have a downcast with an offset != 0
	// and we should not collapse this source
	if(it->second.empty())
		return true;
	// Finally, try to rewrite every destination type, if they all collapse the source will collapse as well
	for(StructType* destSt: it->second)
	{
		const TypeMappingInfo& destStInfo=rewriteType(destSt);
		if(destStInfo.elementMappingKind != TypeMappingInfo::COLLAPSED)
			return true;
	}
	return false;
}

TypeOptimizer::TypeMappingInfo TypeOptimizer::rewriteType(Type* t)
{
	assert(!newStructTypes.count(t));
	auto typeMappingIt=typesMapping.find(t);
	if(typeMappingIt!=typesMapping.end())
	{
		if(typeMappingIt->second.elementMappingKind == TypeMappingInfo::COLLAPSING)
			typeMappingIt->second.elementMappingKind = TypeMappingInfo::COLLAPSING_BUT_USED;
		return typeMappingIt->second;
	}
	auto CacheAndReturn = [&](Type* ret, TypeMappingInfo::MAPPING_KIND kind)
	{
		assert(!ret->isStructTy() || !cast<StructType>(ret)->isOpaque());
		return typesMapping[t] = TypeMappingInfo(ret, kind);
	};
	if(StructType* st=dyn_cast<StructType>(t))
	{
		if(TypeSupport::isClientType(st))
			return CacheAndReturn(st, TypeMappingInfo::IDENTICAL);
		if(TypeSupport::hasByteLayout(st))
			return CacheAndReturn(st, TypeMappingInfo::IDENTICAL);

		// Generate a new type inconditionally, it may end up being the same as the old one
		StructType* newStruct=StructType::create(st->getContext());
#ifndef NDEBUG
		newStructTypes.insert(newStruct);
#endif
		if(st->hasName())
		{
			SmallString<20> name=st->getName();
			st->setName("obsoletestruct");
			newStruct->setName(name);
		}
		// Tentatively map the type to the newStruct, it may be overridden if the type is collapsed
		typesMapping[t] = TypeMappingInfo(newStruct, TypeMappingInfo::IDENTICAL);

		// Forge the new element types
		SmallVector<Type*, 4> newTypes;
		if(st->getNumElements() > 1)
		{
			for(uint32_t i=0;i<st->getNumElements();i++)
			{
				Type* elementType=st->getElementType(i);
				Type* rewrittenType=rewriteType(elementType);
				// Add the new type
				newTypes.push_back(rewrittenType);
			}
		}
		else if(st->getNumElements() == 1)
		{
			// We push the original type here, below we will try to collapse the struct to this element
			newTypes.push_back(st->getElementType(0));
		}

		// newTypes may have a single element because st has a single element or because all the elements collapsed into one
		if(newTypes.size() == 1)
		{
			// Stop if the element is just a int8, we may be dealing with an empty struct
			// Empty structs are unsafe as the int8 inside is just a placeholder and will be replaced
			// by a different type in a derived class
			// TODO: If pointers could be collapsed we may have implicit casts between base classes and derived classes
			if(!newTypes[0]->isIntegerTy(8) && !newTypes[0]->isPointerTy() && !TypeSupport::isJSExportedType(newStruct, *module))
			{
				// If this type is an unsafe downcast source and can't be collapse
				// we need to fall through to correctly set the mapped element
				if(!isUnsafeDowncastSource(st))
				{
					// We must avoid using this type as it will not exist anymore
					// TODO: This does not solve A { B { C { A* } } } -> C { C* }
					typesMapping[st] = TypeMappingInfo(newStruct, TypeMappingInfo::COLLAPSING);
					Type* collapsed = rewriteType(newTypes[0]);
					if(typesMapping[st].elementMappingKind != TypeMappingInfo::COLLAPSING_BUT_USED)
					{
						assert(typesMapping[st].elementMappingKind == TypeMappingInfo::COLLAPSING);
						return CacheAndReturn(collapsed, TypeMappingInfo::COLLAPSED);
					}
					typesMapping[st] = TypeMappingInfo(newStruct, TypeMappingInfo::IDENTICAL);
				}
			}
			// Can't collapse, rewrite the member now
			Type* elementType=newTypes[0];
			Type* rewrittenType=rewriteType(elementType);
			newTypes[0]=rewrittenType;
		}

		StructType* newDirectBase = st->getDirectBase() ? dyn_cast<StructType>(rewriteType(st->getDirectBase()).mappedType) : NULL;
		newStruct->setBody(newTypes, st->isPacked(), newDirectBase);
		pendingStructTypes.erase(t);
		return CacheAndReturn(newStruct, TypeMappingInfo::IDENTICAL);
	}
	if(FunctionType* ft=dyn_cast<FunctionType>(t))
	{
		Type* newReturnType=rewriteType(ft->getReturnType());
		SmallVector<Type*, 4> newParameters;
		for(uint32_t i=0;i<ft->getNumParams();i++)
			newParameters.push_back(rewriteType(ft->getParamType(i)));
		return CacheAndReturn(FunctionType::get(newReturnType, newParameters, ft->isVarArg()), TypeMappingInfo::IDENTICAL);
	}
	if(PointerType* pt=dyn_cast<PointerType>(t))
	{
		Type* elementType = pt->getElementType();
		Type* newType = rewriteType(elementType);
		if(newType == elementType)
			return CacheAndReturn(pt, TypeMappingInfo::IDENTICAL);
		else
			return CacheAndReturn(PointerType::get(newType, 0), TypeMappingInfo::IDENTICAL);
	}
	if(ArrayType* at=dyn_cast<ArrayType>(t))
	{
		Type* elementType = at->getElementType();
		const TypeMappingInfo& newInfo = rewriteType(elementType);
		Type* newType = newInfo.mappedType;
		if(newType == elementType)
			return CacheAndReturn(at, TypeMappingInfo::IDENTICAL);
		else
			return CacheAndReturn(ArrayType::get(newType, at->getNumElements()), TypeMappingInfo::IDENTICAL);
	}
	return CacheAndReturn(t, TypeMappingInfo::IDENTICAL);
}

Constant* TypeOptimizer::rewriteConstant(Constant* C)
{
	// Immediately return for GlobalValue, we should never try to map their type as they are already rewritten
	if(isa<GlobalValue>(C))
		return C;
	TypeMappingInfo newTypeInfo = rewriteType(C->getType());
	if(ConstantExpr* CE=dyn_cast<ConstantExpr>(C))
	{
		auto getOriginalGlobalType = [&](Constant* C) -> Type*
		{
			GlobalValue* GV = dyn_cast<GlobalValue>(C);
			if(!GV)
				return C->getType();
			auto it = globalTypeMapping.find(GV);
			if(it == globalTypeMapping.end())
				return C->getType();
			else
				return it->second;
		};
		switch(CE->getOpcode())
		{
			case Instruction::GetElementPtr:
			{
				Constant* ptrOperand = CE->getOperand(0);
				Type* ptrType = getOriginalGlobalType(ptrOperand);
				ptrOperand = rewriteConstant(ptrOperand);
				SmallVector<Value*, 4> newIndexes;
				rewriteGEPIndexes(newIndexes, ptrType->getPointerElementType(), ArrayRef<Use>(CE->op_begin()+1,CE->op_end()));
				return ConstantExpr::getGetElementPtr(ptrOperand, newIndexes);
			}
			case Instruction::BitCast:
			{
				Constant* srcOperand = rewriteConstant(CE->getOperand(0));
				return ConstantExpr::getBitCast(srcOperand, newTypeInfo.mappedType);
			}
			case Instruction::IntToPtr:
			{
				return ConstantExpr::getIntToPtr(CE->getOperand(0), newTypeInfo.mappedType);
			}
			default:
			{
				// Get a cloned CE with rewritten operands
				std::vector<Constant*> newOperands;
				for(Use& op: CE->operands())
					newOperands.push_back(rewriteConstant(cast<Constant>(op)));
				return CE->getWithOperands(newOperands);
			}
		}
	}
	else if(C->getType() == newTypeInfo.mappedType)
		return C;
	else if(isa<ConstantAggregateZero>(C))
		return Constant::getNullValue(newTypeInfo.mappedType);
	else if(isa<ConstantPointerNull>(C))
		return ConstantPointerNull::get(cast<PointerType>(newTypeInfo.mappedType));
	else if(isa<UndefValue>(C))
		return UndefValue::get(newTypeInfo.mappedType);
	else if(ConstantStruct* CS=dyn_cast<ConstantStruct>(C))
	{
		if(newTypeInfo.elementMappingKind == TypeMappingInfo::COLLAPSED)
		{
			assert(cast<StructType>(CS->getType())->getNumElements()==1);
			Constant* element = CS->getOperand(0);
			return rewriteConstant(element);
		}
		assert(newTypeInfo.elementMappingKind == TypeMappingInfo::IDENTICAL);
		SmallVector<Constant*, 4> newElements;
		for(uint32_t i=0;i<CS->getNumOperands();i++)
		{
			Constant* element = CS->getOperand(i);
			Constant* newElement = rewriteConstant(element);
			newElements.push_back(newElement);
		}
		return ConstantStruct::get(cast<StructType>(newTypeInfo.mappedType), newElements);
	}
	else if(ConstantArray* CA=dyn_cast<ConstantArray>(C))
	{
		assert(newTypeInfo.mappedType->isArrayTy());
		SmallVector<Constant*, 4> newElements;
		for(uint32_t i=0;i<CA->getNumOperands();i++)
		{
			Constant* element = CA->getOperand(i);
			Constant* newElement = rewriteConstant(element);
			newElements.push_back(newElement);
		}
		return ConstantArray::get(cast<ArrayType>(newTypeInfo.mappedType), newElements);
	}
	else
		assert(false && "Unexpected constant in TypeOptimizer");
	return NULL;
}

void TypeOptimizer::rewriteIntrinsic(Function* F, FunctionType* FT)
{
	// If a type for this intrinsic is collapsed we need to use a differently named intrinsic
	// Make sure that this new intrinsic is also mapped to new types.
	// This lambda returns true if the name has not changed, as in that case we don't need a new intrinsic
	auto fixDepedendentIntrinsic = [&](Intrinsic::ID id, ArrayRef<Type*> Tys) -> bool
	{
		const std::string& intrName = Intrinsic::getName(id, Tys);
		// If the name does not change we only need to fix the type
		if(F->getName() == intrName)
		{
			F->mutateType(FT->getPointerTo());
			return true;
		}
		Function* intrF = F->getParent()->getFunction(intrName);
		// If the intrinsic with the new types is not already defined we will create a new fixed one later on
		if(!intrF || !pendingFunctions.count(intrF))
			return false;
		rewriteFunction(intrF);
		return false;
	};
	SmallVector<Type*, 3> newTys;
	switch(F->getIntrinsicID())
	{
		case Intrinsic::cheerp_upcast_collapsed:
		case Intrinsic::cheerp_cast_user:
		case Intrinsic::cheerp_downcast:
		case Intrinsic::cheerp_reallocate:
		{
			Type* localTys[] = { FT->getReturnType(), FT->getParamType(0)};
			newTys.insert(newTys.end(),localTys,localTys+2);
			break;
		}
		case Intrinsic::cheerp_element_distance:
		{
			Type* localTys[] = { FT->getParamType(0) };
			newTys.insert(newTys.end(),localTys,localTys+1);
			break;
		}
		case Intrinsic::lifetime_start:
		case Intrinsic::lifetime_end:
		{
			Type* localTys[] = { FT->getParamType(1) };
			newTys.insert(newTys.end(),localTys,localTys+1);
			break;
		}
		case Intrinsic::cheerp_allocate:
		{
			Type* localTys[] = { FT->getReturnType() };
			newTys.insert(newTys.end(),localTys,localTys+1);
			break;
		}
		case Intrinsic::cheerp_create_closure:
		{
			Type* localTys[] = { FT->getReturnType(), FT->getParamType(0), FT->getParamType(1) };
			newTys.insert(newTys.end(),localTys,localTys+3);
			break;
		}
	}
	if(!fixDepedendentIntrinsic((Intrinsic::ID)F->getIntrinsicID(), newTys))
	{
		Function* newFunc = Intrinsic::getDeclaration(F->getParent(), (Intrinsic::ID)F->getIntrinsicID(), newTys);
		assert(newFunc != F);
		while(!F->use_empty())
		{
			Use& U = *F->use_begin();
			CallInst* CI=cast<CallInst>(U.getUser());
			assert(U.getOperandNo()==CI->getNumArgOperands());
			CI->setOperand(U.getOperandNo(), newFunc);
		}
	}
}

void TypeOptimizer::rewriteGEPIndexes(SmallVector<Value*, 4>& newIndexes, Type* ptrType, ArrayRef<Use> idxs)
{
	Type* curType = ptrType;
	newIndexes.push_back(idxs[0]);
	for(uint32_t i=1;i<idxs.size();i++)
	{
		TypeMappingInfo curTypeMappingInfo = rewriteType(curType);
		switch(curTypeMappingInfo.elementMappingKind)
		{
			case TypeMappingInfo::IDENTICAL:
				newIndexes.push_back(idxs[i]);
				break;
			case TypeMappingInfo::COLLAPSED:
				break;
			case TypeMappingInfo::COLLAPSING:
			case TypeMappingInfo::COLLAPSING_BUT_USED:
				assert(false);
				break;
		}
		if(StructType* ST=dyn_cast<StructType>(curType))
			curType = ST->getElementType(cast<ConstantInt>(idxs[i])->getZExtValue());
		else
			curType = curType->getSequentialElementType();
	}
}

void TypeOptimizer::rewriteFunction(Function* F)
{
	bool erased = pendingFunctions.erase(F);
	(void)erased;
	assert(erased);
	globalTypeMapping[F] = F->getType();
	// Rewrite the type
	Type* newFuncType = rewriteType(F->getType());
	// Keep track of the original types of local instructions
	std::unordered_map<Value*, Type*> localTypeMapping;
	auto getOriginalOperandType = [&](Value* v) -> Type*
	{
		auto it = localTypeMapping.find(v);
		if(it != localTypeMapping.end())
			return it->second;
		else if(GlobalValue* GV=dyn_cast<GlobalValue>(v))
		{
			assert(globalTypeMapping.count(GV));
			return globalTypeMapping[GV];
		}
		else
			return v->getType();
	};
	auto setOriginalOperandType = [&](Value* v, Type* t) -> void
	{
		localTypeMapping[v] = t;
	};
	// Keep track of instructions which have been remapped
	std::unordered_map<Value*, Value*> localInstMapping;
	auto getMappedOperand = [&](Value* v) -> Value*
	{
		if(Constant* C=dyn_cast<Constant>(v))
			return rewriteConstant(C);
		auto it = localInstMapping.find(v);
		if(it != localInstMapping.end())
			return it->second;
		else
			return v;
	};
	auto setMappedOperand = [&](Value* v, Value* m) -> void
	{
		assert(v->getType()->isPointerTy() && m->getType()->isPointerTy());
		localInstMapping[v] = m;
	};
	if(newFuncType!=F->getType())
	{
		if(F->getIntrinsicID())
			rewriteIntrinsic(F, cast<FunctionType>(newFuncType->getPointerElementType()));
		else
			F->mutateType(newFuncType);
		// Change the types of the arguments
		for(Argument& a: F->getArgumentList())
		{
			Type* newArgType=cast<FunctionType>(newFuncType->getPointerElementType())->getParamType(a.getArgNo());
			if(newArgType==a.getType())
				continue;
			setOriginalOperandType(&a, a.getType());
			a.mutateType(newArgType);
		}
	}
	if(F->empty())
		return;
	SmallVector<BasicBlock*, 4> blocksInDFSOrder;
	std::unordered_set<BasicBlock*> usedBlocks;
	blocksInDFSOrder.push_back(&F->getEntryBlock());
	// The size of the vector will increase over time, this is by design
	for(uint32_t i=0;i<blocksInDFSOrder.size();i++)
	{
		BasicBlock* BB = blocksInDFSOrder[i];
		TerminatorInst* term = BB->getTerminator();
		for(uint32_t i=0;i<term->getNumSuccessors();i++)
		{
			BasicBlock* succ = term->getSuccessor(i);
			if(usedBlocks.count(succ))
				continue;
			usedBlocks.insert(succ);
			blocksInDFSOrder.push_back(succ);
		}
	}

	SmallVector<PHINode*, 4> delayedPHIs;
	// Rewrite instructions as needed
	for(BasicBlock* BB: blocksInDFSOrder)
	{
		for(Instruction& I: *BB)
		{
			switch(I.getOpcode())
			{
				default:
					if(I.getType()->isPointerTy())
						llvm::errs() << "INST " << I << "\n";
					assert(!I.getType()->isPointerTy() && "Unexpected instruction in TypeOptimizer");
					break;
				case Instruction::GetElementPtr:
				{
					Value* ptrOperand = I.getOperand(0);
					Type* ptrType = getOriginalOperandType(ptrOperand);
					if(rewriteType(ptrType) != ptrType || rewriteType(I.getType()) != I.getType())
					{
						Type* curType = ptrType->getPointerElementType();
						SmallVector<Value*, 4> newIndexes;
						rewriteGEPIndexes(newIndexes, curType, ArrayRef<Use>(I.op_begin()+1,I.op_end()));
						GetElementPtrInst* NewInst = GetElementPtrInst::Create(getMappedOperand(ptrOperand), newIndexes);
						NewInst->takeName(&I);
						NewInst->setIsInBounds(cast<GetElementPtrInst>(I).isInBounds());
						setMappedOperand(&I, NewInst);
						break;
					}
					// If the operand and the result types have not changed the indexes do not need any change as well
					// but we still need to check if the type of the GEP itself needs to be updated,
					// so fall through to the below cases. Please note that Call must check for IntrinsicInst for this to work.
				}
				case Instruction::Call:
				{
					// We need to handle special intrinsics here
					if(IntrinsicInst* II=dyn_cast<IntrinsicInst>(&I))
					{
						if(II->getIntrinsicID() == Intrinsic::cheerp_upcast_collapsed)
						{
							// If the return type is not a struct anymore while the source type is still a
							// struct replace the upcast with a GEP
							Value* ptrOperand = I.getOperand(0);
							Type* curType = getOriginalOperandType(ptrOperand)->getPointerElementType();
							TypeMappingInfo newRetInfo = rewriteType(I.getType()->getPointerElementType());
							TypeMappingInfo newOpInfo = rewriteType(curType);
							// TODO: Also handle MERGED_MEMBER_ARRAYS_AND_COLLAPSED
							if(newRetInfo.elementMappingKind == TypeMappingInfo::COLLAPSED &&
								newOpInfo.elementMappingKind != TypeMappingInfo::COLLAPSED)
							{
								Type* Int32 = IntegerType::get(II->getContext(), 32);
								Value* Zero = ConstantInt::get(Int32, 0);
								Value* Indexes[] = { Zero, Zero };
								Value* newPtrOperand = getMappedOperand(ptrOperand);
								Instruction* newGEP = GetElementPtrInst::Create(newPtrOperand, Indexes, "gepforupcast");
								setMappedOperand(&I, newGEP);
								break;
							}
						}
					}
					// Fall through to next case
				}
				case Instruction::Alloca:
				case Instruction::BitCast:
				case Instruction::ExtractValue:
				case Instruction::IntToPtr:
				case Instruction::Load:
				case Instruction::PHI:
				case Instruction::Ret:
				case Instruction::Select:
				case Instruction::Store:
				case Instruction::VAArg:
				{
					if(I.getType()->isVoidTy())
						break;
					TypeMappingInfo newInfo = rewriteType(I.getType());
					if(newInfo.mappedType==I.getType())
						break;
					setOriginalOperandType(&I, I.getType());
					I.mutateType(newInfo.mappedType);
					break;
				}
			}
			// We need to handle pointer PHIs later on, when all instructions are redefined
			if(PHINode* phi = dyn_cast<PHINode>(&I))
			{
				if(phi->getType()->isPointerTy())
				{
					delayedPHIs.push_back(phi);
					continue;
				}
			}
			for(uint32_t i=0;i<I.getNumOperands();i++)
			{
				Value* op=I.getOperand(i);
				I.setOperand(i, getMappedOperand(op));
			}
		}
	}
	for(PHINode* phi: delayedPHIs)
	{
		for(uint32_t i=0;i<phi->getNumIncomingValues();i++)
		{
			Value* op=phi->getIncomingValue(i);
			phi->setIncomingValue(i, getMappedOperand(op));
		}
	}
	for(auto it: localInstMapping)
	{
		// Insert new instruction
		cast<Instruction>(it.second)->insertAfter(cast<Instruction>(it.first));
		// Delete old instructions
		cast<Instruction>(it.first)->replaceAllUsesWith(UndefValue::get(it.first->getType()));
		cast<Instruction>(it.first)->eraseFromParent();
	}
}

void TypeOptimizer::rewriteGlobal(GlobalVariable* GV)
{
	Type* rewrittenType = rewriteType(GV->getType());
	globalTypeMapping[GV] = GV->getType();
	if(GV->getType()==rewrittenType)
		return;
	GV->mutateType(rewrittenType);
}

void TypeOptimizer::rewriteGlobalInit(GlobalVariable* GV)
{
	if(!GV->hasInitializer())
		return;
	Type* GVType = globalTypeMapping[GV]->getPointerElementType();
	Type* rewrittenType = rewriteType(GVType);
	if(GVType==rewrittenType)
		return;
	// We need to change type, so we have to forge a new initializer
	Constant* rewrittenInit = rewriteConstant(GV->getInitializer());
	GV->setInitializer(rewrittenInit);
}

bool TypeOptimizer::runOnModule(Module& M)
{
	// Get required auxiliary data
	module = &M;
	gatherAllDowncastSourceTypes(M);
	// Update the type for all global variables
	for(GlobalVariable& GV: M.getGlobalList())
		rewriteGlobal(&GV);
	for(Function& F: M)
		pendingFunctions.insert(&F);
	// Rewrite all functions
	while(!pendingFunctions.empty())
		rewriteFunction(*pendingFunctions.begin());
	// Now that all functions are fixes, update the global initializer
	for(GlobalVariable& GV: M.getGlobalList())
		rewriteGlobalInit(&GV);
	for(GlobalAlias& GA: M.getAliasList())
	{
		Type* rewrittenType = rewriteType(GA.getType());
		GA.mutateType(rewrittenType);
	}
	while(!pendingStructTypes.empty())
		rewriteType(*pendingStructTypes.begin());
	assert(pendingFunctions.empty());
	// Reuse pendingFunctions to store intrinsics that should be delete
	for(Function& F: M)
	{
		if(F.getIntrinsicID() && F.use_empty())
			pendingFunctions.insert(&F);
	}
	for(Function* F: pendingFunctions)
		F->eraseFromParent();
	return true;
}

char TypeOptimizer::ID = 0;

}

using namespace cheerp;

INITIALIZE_PASS_BEGIN(TypeOptimizer, "TypeOptimizer", "Optimize struct and array types",
                      false, false)
INITIALIZE_PASS_END(TypeOptimizer, "TypeOptimizer", "Optimize struct and array types",
                    false, false)
