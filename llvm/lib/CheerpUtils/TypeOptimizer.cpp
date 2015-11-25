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
#include "llvm/IR/IRBuilder.h"
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

void TypeOptimizer::addAllBaseTypesForByteLayout(StructType* st, Type* baseType)
{
	if(ArrayType* AT=dyn_cast<ArrayType>(baseType))
		addAllBaseTypesForByteLayout(st, AT->getElementType());
	else if(StructType* ST=dyn_cast<StructType>(baseType))
	{
		// TODO: This is broken for unions inside union. We would need to indirectly reference them.
		for(uint32_t i=0;i<ST->getNumElements();i++)
			addAllBaseTypesForByteLayout(st, ST->getElementType(i));
	}
	else
	{
		// If there is no base type so far, initialize it
		auto it = baseTypesForByteLayout.find(st);
		if(it == baseTypesForByteLayout.end())
			baseTypesForByteLayout.insert(std::make_pair(st, baseType));
		else if (it->second != baseType)
		{
			// The known base type is not the same as the passed one
			it->second = NULL;
		}
	}
}

void TypeOptimizer::pushAllBaseConstantElements(SmallVector<llvm::Constant*, 4>& newElements, Constant* C, Type* baseType)
{
	if(C->getType()==baseType)
		newElements.push_back(C);
	else if(ArrayType* AT=dyn_cast<ArrayType>(C->getType()))
	{
		if(ConstantArray* CA=dyn_cast<ConstantArray>(C))
		{
			for(unsigned i=0;i<AT->getNumElements();i++)
				pushAllBaseConstantElements(newElements, CA->getOperand(i), baseType);
		}
		else if(ConstantDataSequential* CDS=dyn_cast<ConstantDataSequential>(C))
		{
			for(unsigned i=0;i<AT->getNumElements();i++)
				pushAllBaseConstantElements(newElements, CDS->getElementAsConstant(i), baseType);
		}
		else
		{
			assert(isa<ConstantAggregateZero>(C));
			// TODO: Could be optimized as we know that the elements should all be of baseType
			for(unsigned i=0;i<AT->getNumElements();i++)
				pushAllBaseConstantElements(newElements, Constant::getNullValue(AT->getElementType()), baseType);
		}
	}
	else if(StructType* ST=dyn_cast<StructType>(C->getType()))
	{
		if(ConstantStruct* CS=dyn_cast<ConstantStruct>(C))
		{
			for(unsigned i=0;i<ST->getNumElements();i++)
				pushAllBaseConstantElements(newElements, CS->getOperand(i), baseType);
		}
		else
		{
			assert(isa<ConstantAggregateZero>(C));
			// TODO: Could be optimized as we know that the elements should all be of baseType
			for(unsigned i=0;i<ST->getNumElements();i++)
				pushAllBaseConstantElements(newElements, Constant::getNullValue(ST->getElementType(i)), baseType);
		}
	}
	else
	{
		// It's not an aggregate and not the baseType, something is wrong here
		assert(false);
	}
}

void TypeOptimizer::gatherAllTypesInfo(const Module& M)
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
					if(!isa<ConstantInt>(II->getOperand(1)) || cast<ConstantInt>(II->getOperand(1))->getZExtValue() != 0)
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
				else if(const BitCastInst* BC=dyn_cast<BitCastInst>(&I))
				{
					// Find out all the types that bytelayout structs are casted to
					StructType* st = dyn_cast<StructType>(BC->getSrcTy()->getPointerElementType());
					if(!st || !st->hasByteLayout())
						continue;
					addAllBaseTypesForByteLayout(st, BC->getDestTy()->getPointerElementType());
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
		{
			// When we find a COLLAPSING type, we forward the request if the contained type is a struct
			// otherwise it will set the COLLAPSING_BUT_USED flag, in which case we need to abort the rewrite
			// See also below how the COLLAPSING flag is used
			if(typeMappingIt->second.mappedType->isStructTy())
			{
				assert(typeMappingIt->second.mappedType != t);
				return rewriteType(typeMappingIt->second.mappedType);
			}
			else
				typeMappingIt->second.elementMappingKind = TypeMappingInfo::COLLAPSING_BUT_USED;
		}
		return typeMappingIt->second;
	}
	auto CacheAndReturn = [&](Type* ret, TypeMappingInfo::MAPPING_KIND kind)
	{
		return typesMapping[t] = TypeMappingInfo(ret, kind);
	};
	if(StructType* st=dyn_cast<StructType>(t))
	{
		if(TypeSupport::isClientType(st))
			return CacheAndReturn(st, TypeMappingInfo::IDENTICAL);
		if(TypeSupport::hasByteLayout(st))
		{
			addAllBaseTypesForByteLayout(st, st);
			// If the data of this byte layout struct is always accessed as the same type, we can replace it with an array of that type
			// This is useful for an idiom used by C++ graphics code to have a vector both accessible as named elements and as an array
			// union { struct { double x,y,z; }; double elemets[3]; };
			auto it=baseTypesForByteLayout.find(st);
			assert(it!=baseTypesForByteLayout.end());
			if(it->second == NULL)
				return CacheAndReturn(st, TypeMappingInfo::IDENTICAL);
			// Check that the struct fits exactly N values of the base type
			uint32_t structSize = DL->getTypeAllocSize(st);
			uint32_t elementSize = DL->getTypeAllocSize(it->second);
			if(structSize % elementSize)
				return CacheAndReturn(st, TypeMappingInfo::IDENTICAL);

			uint32_t numElements = structSize / elementSize;
			// See if we can replace it with a single element
			if(numElements==1)
				return CacheAndReturn(it->second, TypeMappingInfo::BYTE_LAYOUT_TO_ARRAY);

			// Replace this byte layout struct with an array
			Type* newType = ArrayType::get(it->second, numElements);
			return CacheAndReturn(newType, TypeMappingInfo::BYTE_LAYOUT_TO_ARRAY);
		}

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

		// Since we can merge arrays of the same type in an struct it is possible that at the end of the process a single type will remain
		TypeMappingInfo::MAPPING_KIND newStructKind = TypeMappingInfo::IDENTICAL;
		// Forge the new element types
		SmallVector<Type*, 4> newTypes;
		bool hasMergedArrays=false;
		std::vector<std::pair<uint32_t, uint32_t>> membersMapping;
		if(st->getNumElements() > 1)
		{
			// We want to merge arrays of the same type in the same object
			// So, for each element type, keep track if there is already an array
			std::unordered_map<Type*, uint32_t> arraysFound;
			uint32_t directBaseLimit=0;
			// We may need to update the bases metadata for this type
			NamedMDNode* namedBasesMetadata = TypeSupport::getBasesMetadata(newStruct, *module);
			uint32_t firstBaseBegin, firstBaseEnd, baseMax;
			if(namedBasesMetadata)
			{
				MDNode* md = namedBasesMetadata->getOperand(0);
				firstBaseBegin=getIntFromValue(cast<ConstantAsMetadata>(md->getOperand(0))->getValue());
				firstBaseEnd=firstBaseBegin;
				baseMax=getIntFromValue(cast<ConstantAsMetadata>(md->getOperand(1))->getValue());
			}
			for(uint32_t i=0;i<st->getNumElements();i++)
			{
				// We can't merge arrats across bases, so when we reach the limit of the previous direct base we
				// reset the merging state and compute a new limit
				if(i==directBaseLimit)
				{
					arraysFound.clear();
					StructType* curBase=st;
					while(curBase->getDirectBase() && curBase->getDirectBase()->getNumElements()>i)
						curBase=curBase->getDirectBase();
					directBaseLimit=curBase->getNumElements();
				}
				Type* elementType=st->getElementType(i);
				Type* rewrittenType=rewriteType(elementType);
				if(ArrayType* at=dyn_cast<ArrayType>(rewrittenType))
				{
					Type* arrayElementType=rewrittenType->getArrayElementType();
					auto arraysFoundIt=arraysFound.find(arrayElementType);
					// An array is already available for this type, just extend it
					if(arraysFoundIt!=arraysFound.end())
					{
						uint32_t typeIndex = arraysFoundIt->second;
						ArrayType* previousArrayType = cast<ArrayType>(newTypes[typeIndex]);
						newTypes[typeIndex] = ArrayType::get(arrayElementType, previousArrayType->getNumElements() + at->getNumElements());
						membersMapping.push_back(std::make_pair(typeIndex, previousArrayType->getNumElements()));
						if(i < firstBaseBegin)
							firstBaseEnd--;
						hasMergedArrays=true;
						continue;
					}
					// Insert this array in the map, we will insert it in the vector just below
					arraysFound[arrayElementType] = newTypes.size();
				}
				membersMapping.push_back(std::make_pair(newTypes.size(), 0));
				// Add the new type
				newTypes.push_back(rewrittenType);
			}
			if(hasMergedArrays)
			{
				assert(!newTypes.empty());
				membersMappingData.insert(std::make_pair(st, membersMapping));
				newStructKind = TypeMappingInfo::MERGED_MEMBER_ARRAYS;
				// Update bases metadata
				if(namedBasesMetadata)
				{
					Type* Int32 = IntegerType::get(module->getContext(), 32);
					Metadata* newBasesMeta[] = { ConstantAsMetadata::get(ConstantInt::get(Int32, firstBaseEnd)), ConstantAsMetadata::get(ConstantInt::get(Int32, baseMax)) };
					MDNode* newMD = MDNode::get(module->getContext(), newBasesMeta);
					// The bases metadata has numerous duplicated entries, so fix all of them
					// TODO: Remove duplicated entries
					for(uint32_t i=0;i<namedBasesMetadata->getNumOperands();i++)
						namedBasesMetadata->setOperand(i, newMD);
				}
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
					// To fix the following case A { B { C { A* } } } -> C { C* }
					// we prime the mapping to the contained element and use the COLLAPSING flag
					typesMapping[st] = TypeMappingInfo(newTypes[0], TypeMappingInfo::COLLAPSING);
					Type* collapsed = rewriteType(newTypes[0]);
					if(typesMapping[st].elementMappingKind != TypeMappingInfo::COLLAPSING_BUT_USED)
					{
						assert(typesMapping[st].elementMappingKind == TypeMappingInfo::COLLAPSING);
						if(newStructKind != TypeMappingInfo::MERGED_MEMBER_ARRAYS)
							return CacheAndReturn(collapsed, TypeMappingInfo::COLLAPSED);
						else
							return CacheAndReturn(collapsed, TypeMappingInfo::MERGED_MEMBER_ARRAYS_AND_COLLAPSED);
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
		return CacheAndReturn(newStruct, newStructKind);
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
		if(newType->isArrayTy())
		{
			// It's never a good idea to use pointers to array, we may end up creating wrapper arrays for arrays
			return CacheAndReturn(PointerType::get(newType->getArrayElementType(), 0), TypeMappingInfo::POINTER_FROM_ARRAY);
		}
		else if(newType == elementType)
			return CacheAndReturn(pt, TypeMappingInfo::IDENTICAL);
		else
			return CacheAndReturn(PointerType::get(newType, 0), TypeMappingInfo::IDENTICAL);
	}
	if(ArrayType* at=dyn_cast<ArrayType>(t))
	{
		Type* elementType = at->getElementType();
		const TypeMappingInfo& newInfo = rewriteType(elementType);
		Type* newType = newInfo.mappedType;
		if(ArrayType* subArray=dyn_cast<ArrayType>(newType))
		{
			// Flatten arrays of array
			return CacheAndReturn(ArrayType::get(newType->getArrayElementType(), at->getNumElements()*subArray->getNumElements()),
				TypeMappingInfo::FLATTENED_ARRAY);
		}
		else if(newType == elementType)
			return CacheAndReturn(at, TypeMappingInfo::IDENTICAL);
		else
			return CacheAndReturn(ArrayType::get(newType, at->getNumElements()), TypeMappingInfo::IDENTICAL);
	}
	return CacheAndReturn(t, TypeMappingInfo::IDENTICAL);
}

void TypeOptimizer::pushAllArrayConstantElements(SmallVector<Constant*, 4>& newElements, Constant* array)
{
	ArrayType* AT=cast<ArrayType>(array->getType());
	if(ConstantArray* CA=dyn_cast<ConstantArray>(array))
	{
		for(unsigned i=0;i<AT->getNumElements();i++)
			newElements.push_back(CA->getOperand(i));
	}
	else if(ConstantDataSequential* CDS=dyn_cast<ConstantDataSequential>(array))
	{
		for(unsigned i=0;i<AT->getNumElements();i++)
			newElements.push_back(CDS->getElementAsConstant(i));
	}
	else
	{
		assert(isa<ConstantAggregateZero>(array));
		for(unsigned i=0;i<AT->getNumElements();i++)
			newElements.push_back(Constant::getNullValue(AT->getElementType()));
	}
}

Constant* TypeOptimizer::rewriteConstant(Constant* C)
{
	// Immediately return for globals, we should never try to map their type as they are already rewritten
	if(GlobalVariable* GV=dyn_cast<GlobalVariable>(C))
	{
		assert(globalsMapping.count(GV));
		return globalsMapping[GV];
	}
	else if(isa<GlobalValue>(C))
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
				Type* targetType = rewriteType(CE->getType()->getPointerElementType());
				rewriteGEPIndexes(newIndexes, ptrType, ArrayRef<Use>(CE->op_begin()+1,CE->op_end()), targetType, NULL);
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
		if(newTypeInfo.elementMappingKind == TypeMappingInfo::BYTE_LAYOUT_TO_ARRAY)
		{
			auto baseTypeIt = baseTypesForByteLayout.find(cast<StructType>(CS->getType()));
			assert(baseTypeIt != baseTypesForByteLayout.end() && baseTypeIt->second);
			// Forge a ConstantArray
			SmallVector<Constant*, 4> newElements;
			pushAllBaseConstantElements(newElements, CS, baseTypeIt->second);
			if(newElements.size() == 1)
				return newElements[0];
			ArrayType* newArrayType = ArrayType::get(baseTypeIt->second, newElements.size());
			return ConstantArray::get(newArrayType, newElements);
		}
		else if(newTypeInfo.elementMappingKind == TypeMappingInfo::COLLAPSED)
		{
			assert(cast<StructType>(CS->getType())->getNumElements()==1);
			Constant* element = CS->getOperand(0);
			return rewriteConstant(element);
		}
		auto membersMappingIt = membersMappingData.find(CS->getType());
		bool hasMergedArrays = newTypeInfo.elementMappingKind == TypeMappingInfo::MERGED_MEMBER_ARRAYS ||
					newTypeInfo.elementMappingKind == TypeMappingInfo::MERGED_MEMBER_ARRAYS_AND_COLLAPSED;
		assert(!hasMergedArrays || membersMappingIt != membersMappingData.end());
		SmallVector<Constant*, 4> newElements;
		// Check if some of the contained constant arrays needs to be merged
		for(uint32_t i=0;i<CS->getNumOperands();i++)
		{
			Constant* element = CS->getOperand(i);
			Constant* newElement = rewriteConstant(element);
			if(hasMergedArrays && membersMappingIt->second[i].first != (newElements.size()))
			{
				// This element has been remapped to another one. It must be an array
				SmallVector<Constant*, 4> mergedArrayElements;
				Constant* oldMember = newElements[membersMappingIt->second[i].first];
				assert(oldMember->getType()->getArrayElementType() == newElement->getType()->getArrayElementType());
				// Insert all the elements of the existing member
				pushAllArrayConstantElements(mergedArrayElements, oldMember);
				pushAllArrayConstantElements(mergedArrayElements, newElement);
				// Forge a new array and replace oldMember
				ArrayType* mergedType = ArrayType::get(oldMember->getType()->getArrayElementType(), mergedArrayElements.size());
				newElements[membersMappingIt->second[i].first] = ConstantArray::get(mergedType, mergedArrayElements);
			}
			else
				newElements.push_back(newElement);
		}
		if(newTypeInfo.elementMappingKind == TypeMappingInfo::MERGED_MEMBER_ARRAYS_AND_COLLAPSED)
		{
			assert(newElements.size() == 1);
			return newElements[0];
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
			if(newTypeInfo.elementMappingKind == TypeMappingInfo::FLATTENED_ARRAY)
			{
				// Put all the operands of the element in this array
				pushAllArrayConstantElements(newElements, newElement);
			}
			else
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
		case Intrinsic::cheerp_make_complete_object:
		{
			Type* localTys[] = { FT->getReturnType(), FT->getParamType(0)};
			newTys.insert(newTys.end(),localTys,localTys+2);
			break;
		}
		case Intrinsic::cheerp_downcast_current:
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
		case Intrinsic::memcpy:
		case Intrinsic::memmove:
		{
			Type* localTys[] = { FT->getParamType(0), FT->getParamType(1), FT->getParamType(2) };
			newTys.insert(newTys.end(),localTys,localTys+3);
			break;
		}
		case Intrinsic::memset:
		{
			Type* localTys[] = { FT->getParamType(0), FT->getParamType(2) };
			newTys.insert(newTys.end(),localTys,localTys+2);
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

void TypeOptimizer::rewriteGEPIndexes(SmallVector<Value*, 4>& newIndexes, Type* ptrType, ArrayRef<Use> idxs, Type* targetType, Instruction* insertionPoint)
{
	// The addToLastIndex flag should be set to true if the following index should be added to the previouly pushed one
	bool addToLastIndex = false;
	auto AddIndex=[&](Value* V)
	{
		if(addToLastIndex)
		{
			if(insertionPoint)
				newIndexes.back() = BinaryOperator::Create(Instruction::Add, newIndexes.back(), V, "", insertionPoint);
			else
			{
				assert(isa<ConstantInt>(newIndexes.back()) && isa<ConstantInt>(V));
				newIndexes.back() = ConstantExpr::getAdd(cast<Constant>(newIndexes.back()), cast<Constant>(V));
			}
		}
		else
			newIndexes.push_back(V);
		addToLastIndex = false;
	};
	auto AddMultipliedIndex=[&](Value* V, uint32_t multiplier)
	{
		Constant* numElementsC = ConstantInt::get(V->getType(), multiplier);
		if(insertionPoint)
			AddIndex(BinaryOperator::Create(Instruction::Mul, V, numElementsC, "", insertionPoint));
		else
		{
			assert(isa<Constant>(V));
			AddIndex(ConstantExpr::getMul(cast<Constant>(V), numElementsC));
		}
	};
	Type* curType = ptrType;
	Type* Int32Ty = IntegerType::get(curType->getContext(), 32);
	for(uint32_t i=0;i<idxs.size();i++)
	{
		TypeMappingInfo curTypeMappingInfo = rewriteType(curType);
		switch(curTypeMappingInfo.elementMappingKind)
		{
			case TypeMappingInfo::IDENTICAL:
				AddIndex(idxs[i]);
				break;
			case TypeMappingInfo::COLLAPSED:
				break;
			case TypeMappingInfo::BYTE_LAYOUT_TO_ARRAY:
			{
				assert(isa<StructType>(curType));
				if(curTypeMappingInfo.mappedType == targetType)
				{
					if(targetType->isArrayTy())
					{
						// We are transforming all pointers to arrays to pointers to elements
						Value* Zero = ConstantInt::get(Int32Ty, 0);
						AddIndex(Zero);
					}
					return;
				}
				auto baseTypeIt = baseTypesForByteLayout.find(cast<StructType>(curType));
				assert(baseTypeIt != baseTypesForByteLayout.end() && baseTypeIt->second);
				if(!curTypeMappingInfo.mappedType->isArrayTy())
				{
					// If it's not an array it must be a single element and we should stop immediately
					assert(curTypeMappingInfo.mappedType == baseTypeIt->second);
					return;
				}
				uint32_t baseTypeSize = DL->getTypeAllocSize(baseTypeIt->second);
				// All the indexes needs to be flattened to a byte offset and then to an array offset
				// NOTE: We are willingly iterating over 'i' again
				for(;i<idxs.size();i++)
				{
					if(StructType* ST=dyn_cast<StructType>(curType))
					{
						assert(isa<ConstantInt>(idxs[i]));
						uint32_t elementIndex = cast<ConstantInt>(idxs[i])->getZExtValue();
						uint32_t elementOffset = DL->getStructLayout(ST)->getElementOffset(elementIndex);
						// All offsets should be multiple of the base type size
						assert(!(elementOffset % baseTypeSize));
						AddIndex(ConstantInt::get(Int32Ty, elementOffset / baseTypeSize));
						curType = ST->getElementType(elementIndex);
					}
					else
					{
						uint32_t elementSize = DL->getTypeAllocSize(curType->getArrayElementType());
						// All offsets should be multiple of the base type size
						assert(!(elementSize % baseTypeSize));
						AddMultipliedIndex(idxs[i], elementSize / baseTypeSize);
						curType = curType->getSequentialElementType();
					}
					addToLastIndex = true;
				}
				// All indexes have been consumed now, we can just return
				assert(rewriteType(curType) == targetType);
				if(targetType->isArrayTy())
				{
					// We are transforming all pointers to arrays to pointers to elements
					Value* Zero = ConstantInt::get(Int32Ty, 0);
					AddIndex(Zero);
				}
				return;
			}
			case TypeMappingInfo::POINTER_FROM_ARRAY:
			{
				// This should only happen for the first element
				assert(i==0);
				// We need to multiply the index by the right number of elements, corresponding to the size of the old type
				// Fall through, the code is identical for POINTER_FROM_ARRAY and FLATTENED_ARRAY
			}
			case TypeMappingInfo::FLATTENED_ARRAY:
			{
				// We had something like [ N x [ M x T ] ] which is now [ N*M x T ]
				uint32_t oldTypeSize = DL->getTypeAllocSize(curType->getSequentialElementType());
				uint32_t elementSize = DL->getTypeAllocSize(curTypeMappingInfo.mappedType->getSequentialElementType());
				assert(!(oldTypeSize % elementSize));
				uint32_t numElements=oldTypeSize/elementSize;
				AddMultipliedIndex(idxs[i], numElements);
				addToLastIndex = true;
				break;
			}
			case TypeMappingInfo::MERGED_MEMBER_ARRAYS:
			case TypeMappingInfo::MERGED_MEMBER_ARRAYS_AND_COLLAPSED:
			{
				assert(curType->isStructTy());
				StructType* oldStruct = cast<StructType>(curType);
				uint32_t elementIndex = cast<ConstantInt>(idxs[i])->getZExtValue();
				assert(membersMappingData.count(oldStruct));
				const std::pair<uint32_t, uint32_t>& mappedMember = membersMappingData[oldStruct][elementIndex];
				if(curTypeMappingInfo.elementMappingKind == TypeMappingInfo::MERGED_MEMBER_ARRAYS)
				{
					// The new index is mappedMember.first
					AddIndex(ConstantInt::get(Int32Ty, mappedMember.first));
				}
				else
					assert(mappedMember.first == 0);
				// If mappedMember.second is not zero, also add a new index that can be eventually incremented later
				if(mappedMember.second)
				{
					AddIndex(ConstantInt::get(Int32Ty, mappedMember.second));
					addToLastIndex = true;
				}
				break;
			}
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
	assert(rewriteType(curType) == targetType);
	if(targetType->isArrayTy())
	{
		// We are transforming all pointers to arrays to pointers to elements
		Value* Zero = ConstantInt::get(Int32Ty, 0);
		AddIndex(Zero);
	}
}

void TypeOptimizer::rewriteFunction(Function* F)
{
	bool erased = pendingFunctions.erase(F);
	(void)erased;
	assert(erased);
	FunctionType* oldFuncType = cast<FunctionType>(F->getType()->getPointerElementType());
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
			auto it=globalTypeMapping.find(GV);
			if(it==globalTypeMapping.end())
				return GV->getType();
			else
				return it->second;
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
	// Remove byval attribute from pointer to array arguments, see CallInst handling below
	bool attributesChanged = false;
	AttributeSet newAttrs=F->getAttributes();
	for(uint32_t i=0;i<F->arg_size();i++)
	{
		if(!newAttrs.hasAttribute(i+1, Attribute::ByVal))
			continue;
		Type* argType = oldFuncType->getParamType(i);
		assert(argType->isPointerTy());
		Type* rewrittenArgType = rewriteType(argType->getPointerElementType());
		if(!rewrittenArgType->isArrayTy())
			continue;
		newAttrs=newAttrs.removeAttribute(module->getContext(), i+1, Attribute::ByVal);
		attributesChanged = true;
	}
	if(attributesChanged)
		F->setAttributes(newAttrs);
	if(F->empty())
		return;
	SmallVector<BasicBlock*, 4> blocksInDFSOrder;
	std::unordered_set<BasicBlock*> usedBlocks;
	usedBlocks.insert(&F->getEntryBlock());
	blocksInDFSOrder.push_back(&F->getEntryBlock());
	// The size of the vector will increase over time, this is by design
	for(uint32_t i=0;i<blocksInDFSOrder.size();i++)
	{
		BasicBlock* BB = blocksInDFSOrder[i];
		TerminatorInst* term = BB->getTerminator();
		for(uint32_t i=0;i<term->getNumSuccessors();i++)
		{
			BasicBlock* succ = term->getSuccessor(i);
			if(!usedBlocks.insert(succ).second)
				continue;
			blocksInDFSOrder.push_back(succ);
		}
	}
	// Finally add all blocks which are not yet used
	for(BasicBlock& BB: *F)
	{
		if(usedBlocks.insert(&BB).second)
			blocksInDFSOrder.push_back(&BB);
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
					assert(!I.getType()->isPointerTy() && "Unexpected instruction in TypeOptimizer");
					break;
				case Instruction::GetElementPtr:
				{
					Value* ptrOperand = I.getOperand(0);
					Type* ptrType = getOriginalOperandType(ptrOperand);
					if(rewriteType(ptrType) != ptrType || rewriteType(I.getType()) != I.getType())
					{
						SmallVector<Value*, 4> newIndexes;
						Type* targetType = rewriteType(I.getType()->getPointerElementType());
						rewriteGEPIndexes(newIndexes, ptrType, ArrayRef<Use>(I.op_begin()+1,I.op_end()), targetType, &I);
						GetElementPtrInst* NewInst = GetElementPtrInst::Create(getMappedOperand(ptrOperand), newIndexes);
						assert(!NewInst->getType()->getPointerElementType()->isArrayTy());
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
								Type* newType = GetElementPtrInst::getIndexedType(newPtrOperand->getType(), Indexes);
								Value* newGEP = NULL;
								if(newType->isArrayTy())
								{
									Value* Indexes2[] = { Zero, Zero, Zero };
									newGEP = GetElementPtrInst::Create(newPtrOperand, Indexes2, "gepforupcast");
								}
								else
									newGEP = GetElementPtrInst::Create(newPtrOperand, Indexes, "gepforupcast");
								setMappedOperand(&I, newGEP);
								break;
							}
						}
					}
					else if(CallInst* CI=dyn_cast<CallInst>(&I))
					{
						if(CI->hasByValArgument())
						{
							// We need to make sure that no byval attribute is applied to pointers to arrays
							// as they will be rewritten to plain pointers and less memory will be copied
							// Get the original type of the called function
							AttributeSet newAttrs=CI->getAttributes();
							bool attributesChanged=false;
							Function* calledFunction = CI->getCalledFunction();
							for(uint32_t i=0;i<CI->getNumArgOperands();i++)
							{
								if(!newAttrs.hasAttribute(i+1, Attribute::ByVal))
									continue;
								Type* argType = getOriginalOperandType(CI->getArgOperand(i));
								assert(argType->isPointerTy());
								Type* rewrittenArgType = rewriteType(argType->getPointerElementType());
								if(!rewrittenArgType->isArrayTy())
									continue;
								// The pointer is to an array, we need to make an explicit copy here
								// and remove the attribute unless the called function is known and the argument is readonly
								if(!calledFunction || !calledFunction->onlyReadsMemory(i+1))
								{
									IRBuilder<> Builder(CI);
									Value* mappedOp = getMappedOperand(CI->getOperand(i));
									assert(mappedOp->getType()->isPointerTy() &&
										!mappedOp->getType()->getPointerElementType()->isArrayTy());
									// 1) Create an alloca of the right type
									Value* byValCopy=Builder.CreateAlloca(rewrittenArgType, nullptr, "byvalcopy");
									byValCopy=Builder.CreateConstGEP2_32(rewrittenArgType, byValCopy, 0, 0);
									// 2) Create a mempcy
									Builder.CreateMemCpy(byValCopy, mappedOp, DL->getTypeAllocSize(rewrittenArgType),
												/*align*/1, /*volatile*/false, nullptr, nullptr,
												nullptr, nullptr, /*byteLayout*/ false);
									// 3) Replace the argument
									CI->setOperand(i, byValCopy);
								}
								// 4) Remove the byval attribute from the call
								newAttrs=newAttrs.removeAttribute(module->getContext(), i+1, Attribute::ByVal);
								attributesChanged = true;
							}
							if(attributesChanged)
								CI->setAttributes(newAttrs);
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
					// Special handling for Alloca
					if(I.getOpcode() == Instruction::Alloca && newInfo.elementMappingKind == TypeMappingInfo::POINTER_FROM_ARRAY)
					{
						// In this case we need to rewrite the allocated type and use that directly
						// Moreover, we need to generate a GEP that will be used instead of this alloca
						Type* newAllocatedType = rewriteType(I.getType()->getPointerElementType());
						Type* newPtrType = PointerType::get(newAllocatedType, 0);
						I.mutateType(newPtrType);
						Type* Int32 = IntegerType::get(I.getType()->getContext(), 32);
						Value* Zero = ConstantInt::get(Int32, 0);
						Value* Indexes[] = { Zero, Zero };
						Instruction* newGEP = GetElementPtrInst::Create(&I, Indexes, "allocadecay");
						setMappedOperand(&I, newGEP);
						break;
					}
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
		// Insert new instruction, if necessary
		if(!cast<Instruction>(it.second)->getParent())
			cast<Instruction>(it.second)->insertAfter(cast<Instruction>(it.first));
		// Alloca are only replaced for POINTER_FROM_ARRAY, and should not be removed
		if(isa<AllocaInst>(it.first))
			continue;
		// Delete old instructions
		cast<Instruction>(it.first)->replaceAllUsesWith(UndefValue::get(it.first->getType()));
		cast<Instruction>(it.first)->eraseFromParent();
	}
}

Constant* TypeOptimizer::rewriteGlobal(GlobalVariable* GV)
{
	TypeMappingInfo newInfo = rewriteType(GV->getType());
	globalTypeMapping[GV] = GV->getType();
	if(GV->getType()==newInfo.mappedType)
	{
		assert(!GV->getType()->getPointerElementType()->isArrayTy());
		return GV;
	}
	if(newInfo.elementMappingKind == TypeMappingInfo::POINTER_FROM_ARRAY)
	{
		Type* newAllocatedType = rewriteType(GV->getType()->getPointerElementType());
		Type* newPtrType = PointerType::get(newAllocatedType, 0);
		GV->mutateType(newPtrType);
		Type* Int32 = IntegerType::get(GV->getType()->getContext(), 32);
		Value* Zero = ConstantInt::get(Int32, 0);
		Value* Indexes[] = { Zero, Zero };
		return ConstantExpr::getGetElementPtr(newAllocatedType, GV, Indexes);
	}
	else
		GV->mutateType(newInfo.mappedType);
	return GV;
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
	DL = &M.getDataLayout();
	assert(DL);
	// Do a preprocessing step to gather data that we can't get online
	gatherAllTypesInfo(M);
	// Update the type for all global variables
	for(GlobalVariable& GV: M.getGlobalList())
	{
		Constant* rewrittenGlobal=rewriteGlobal(&GV);
		globalsMapping.insert(std::make_pair(&GV, rewrittenGlobal));
	}
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
	// Reuse pendingFunctions to store intrinsics that should be deleted
	for(Function& F: M)
	{
		if(F.getIntrinsicID() && F.use_empty())
			pendingFunctions.insert(&F);
	}
	for(Function* F: pendingFunctions)
		F->eraseFromParent();
	module = NULL;
	return true;
}

char TypeOptimizer::ID = 0;

}

using namespace cheerp;

INITIALIZE_PASS_BEGIN(TypeOptimizer, "TypeOptimizer", "Optimize struct and array types",
                      false, false)
INITIALIZE_PASS_END(TypeOptimizer, "TypeOptimizer", "Optimize struct and array types",
                    false, false)
