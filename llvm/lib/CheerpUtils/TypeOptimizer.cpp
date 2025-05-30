//===-- TypeOptimizer.cpp - Cheerp helper -------------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2015-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/InitializePasses.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/Cheerp/TypeOptimizer.h"
#include "llvm/Cheerp/CommandLine.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Operator.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/ModRef.h"
#include "llvm/Support/raw_ostream.h"
#include <set>

using namespace llvm;

namespace cheerp
{

bool TypeOptimizer::isI64ToRewrite(const Type* t)
{
	return t->isIntegerTy(64) && (!UseBigInts || LinearOutput == AsmJs);
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

llvm::StructType* TypeOptimizer::isEscapingStructGEP(const User* GEP)
{
	if(GEP->getNumOperands()<3)
		return nullptr;
	// Keep track of all structure fields that "escapes" (used by more than load/stores)
	if(!hasNonLoadStoreUses(GEP))
		return nullptr;
	StructType* containerStructType = dyn_cast<StructType>(cheerp::getGEPContainerType(GEP));
	return containerStructType;
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
					if(II->getIntrinsicID()==Intrinsic::cheerp_downcast)
					{
						Type* opType = II->getParamElementType(0);
						Type* retType = II->getRetElementType();
						// In the special case of downcast from i8* to i8* we are dealing with exceptions.
						// We collect the types info from another syntetic downcast, so just skip here.
						if(opType->isIntegerTy())
						{
							assert(retType->isIntegerTy(8));
							continue;
						}
						StructType* sourceType = cast<StructType>(opType);
						if(sourceType->hasAsmJS())
							continue;
						// In the special case of downcast to i8* we are dealing with member function pointers
						// Just give up and assume that all the bases may be needed.
						// Also give up in the special case of downcast to the same type.
						// It is used for thrown types.
						if(retType->isIntegerTy(8) || retType == opType)
						{
							do
							{
								downcastSourceToDestinationsMapping[sourceType].clear();
								uint32_t firstBase;
								uint32_t baseCount;
								bool hasBases = TypeSupport::getBasesInfo(M, sourceType, firstBase, baseCount);
								if (!hasBases)
									continue;
								StructType::element_iterator el, end;
								for (el = sourceType->element_begin()+firstBase, end = sourceType->element_begin()+firstBase+baseCount; el != end; ++el)
								{
									downcastSourceToDestinationsMapping[cast<StructType>(*el)].clear();
								}
							}
							while((sourceType = sourceType->getDirectBase()));
							continue;
						}
						// If a source type is downcasted with an offset != 0 we can't collapse the type
						// we keep track of this by setting the mapping to an empty vector
						if(!isa<ConstantInt>(II->getOperand(1)) || cast<ConstantInt>(II->getOperand(1))->getZExtValue() != 0)
						{
							downcastSourceToDestinationsMapping[sourceType].clear();
							continue;
						}
						// If the offset is 0 we need to append the destination type to the mapping
						// If the source type is in the map, but the vector is empty it means that we were
						// in the case above, so we don't add the new destType
						StructType* destType = cast<StructType>(retType);
						auto it=downcastSourceToDestinationsMapping.find(sourceType);
						if(it != downcastSourceToDestinationsMapping.end() && it->second.empty())
							continue;
						downcastSourceToDestinationsMapping[sourceType].insert(destType);
					}
					else if(II->getIntrinsicID() == Intrinsic::cheerp_virtualcast)
					{
						// We can't collapse the source of a virtualcast, keep track of this by setting the mapping to an empty vector
						assert(II->getParamElementType(0));
						StructType* sourceType = cast<StructType>(II->getParamElementType(0));
						downcastSourceToDestinationsMapping[sourceType].clear();
					}
				}
				else if(const BitCastInst* BC=dyn_cast<BitCastInst>(&I))
				{
					if (!BC->getDestTy()->isPointerTy())
						continue;
					// Find out all the types that bytelayout structs are casted to
					StructType* st = dyn_cast<StructType>(BC->getSrcTy()->getNonOpaquePointerElementType());
					if(!st || !st->hasByteLayout())
						continue;
					addAllBaseTypesForByteLayout(st, BC->getDestTy()->getNonOpaquePointerElementType());
				}
				else if(const GetElementPtrInst* GEP=dyn_cast<GetElementPtrInst>(&I))
				{
					StructType* containerStructType = isEscapingStructGEP(GEP);
					if(!containerStructType)
						continue;
					uint32_t fieldIndex = cast<ConstantInt>(*std::prev(GEP->op_end()))->getZExtValue();
					escapingFields.emplace(containerStructType, fieldIndex, TypeAndIndex::STRUCT_MEMBER);
				}
			}
		}

		// Mark the function as only used by wasm if it is used only by direct calls
		// from other wasm functions. If so, we don't need to lower i64 in the
		// signature
		if (!F.hasAddressTaken() && F.getSection() == StringRef("asmjs") && LinearOutput == Wasm)
		{
			bool onlyCalledByWasm = true;
			for (auto& U: F.uses())
			{
				if (Instruction* I = dyn_cast<Instruction>(U.getUser()))
				{
					// NOTE: Invokes from wasm mean that we need js wrappers
					// (see InvokeWrapping), so they count as calls from js
					if (I->getParent()->getParent()->getSection() != StringRef("asmjs") || isa<InvokeInst>(I))
					{
						onlyCalledByWasm = false;
						break;
					}
				}
				else
				{
					onlyCalledByWasm = false;
					break;
				}
			}
			if (onlyCalledByWasm)
			{
				onlyCalledByWasmFuncs.insert(&F);
			}
		}
	}

	if (!UseBigInts)
        {
		//Special functions that may need to be reachable from genericjs
		auto markAsReachable = [this](Function* F)
		{
			if (F) {
				onlyCalledByWasmFuncs.erase(F);
			}
		};

                markAsReachable(M.getFunction("__modti3"));
                markAsReachable(M.getFunction("__umodti3"));
                markAsReachable(M.getFunction("__divti3"));
                markAsReachable(M.getFunction("__udivti3"));
        }

	// Ugly, we need to iterate over constant GEPs, but they are per-context and not per-module
	SmallVector<ConstantExpr*, 4> ConstantGEPs;
	ConstantExpr::getAllFromOpcode(ConstantGEPs, M.getContext(), Instruction::GetElementPtr);
	for(ConstantExpr* GEP: ConstantGEPs)
	{
		StructType* containerStructType = isEscapingStructGEP(GEP);
		if(!containerStructType)
			continue;
		uint32_t fieldIndex = cast<ConstantInt>(*std::prev(GEP->op_end()))->getZExtValue();
		escapingFields.emplace(containerStructType, fieldIndex, TypeAndIndex::STRUCT_MEMBER);
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

bool TypeOptimizer::canCollapseStruct(llvm::StructType* st, llvm::StructType* newStruct, llvm::Type* newType)
{
	if (newStruct == nullptr)
	{
		assert(st->isLiteral());
		return true;
	}
	// Stop if the element is just a int8, we may be dealing with an empty struct
	// Empty structs are unsafe as the int8 inside is just a placeholder and will be replaced
	// by a different type in a derived class
	// TODO: If pointers could be collapsed we may have implicit casts between base classes and derived classes
	// NOTE: We allow the collapsing of client pointers
	if(!TypeSupport::isJSExportedType(newStruct, *module) &&
		!TypeSupport::hasByteLayout(st) &&
		!newType->isIntegerTy(8) && (!newType->isPointerTy() || TypeSupport::isClientPtrType(cast<PointerType>(newType))))
	{
		// If this type is an unsafe downcast source and can't be collapse
		// we need to fall through to correctly set the mapped element
		if(!isUnsafeDowncastSource(st))
		{
			return true;
		}
	}
	return false;
}

//The results of this function are meaningless for packed Struct, since there is no clear meaning of what alignment means
//This function is ok with returning a lower bound, but should never overestimate the alignment
llvm::Align TypeOptimizer::getAlignmentAfterRewrite(llvm::Type* t)
{
	auto it = cacheAlignmentAfterRewrite.find(t);
	if (it != cacheAlignmentAfterRewrite.end())
		return it->second;

	llvm::Align align(1);

	std::set<llvm::Type*> computed;
	std::vector<llvm::Type*> queue;

	queue.push_back(t);

	while (queue.size())
	{
		llvm::Type* curr = queue.back();
		queue.pop_back();

		if (computed.count(curr))
			continue;
		if (StructType* str = dyn_cast<StructType>(curr))
		{
			if(str->isPacked())
				continue;
			//Visit all members
			for(uint32_t i=0;i<str->getNumElements();i++)
				queue.push_back(str->getElementType(i));
		}
		else if (ArrayType* at = dyn_cast<ArrayType>(curr))
		{
			queue.push_back(at->getElementType());
		}
		else if (isI64ToRewrite(curr))
		{
			align = std::max(align, Align(4));
		}
		else
		{
			align = std::max(align, DL->getPrefTypeAlign(curr));
		}
	}

	cacheAlignmentAfterRewrite[t] = align;

	return align;
}

TypeOptimizer::TypeMappingInfo TypeOptimizer::rewriteTypeWithAlignmentInfo(llvm::Type* t, TypeOptimizer::AlignmentInfo& info)
{
	info.first = DL->getPrefTypeAlign(t);
	TypeMappingInfo mappingInfo = rewriteType(t);
	info.second = getAlignmentAfterRewrite(mappingInfo.mappedType);
	return mappingInfo;
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
		if(st->isOpaque())
			return CacheAndReturn(st, TypeMappingInfo::IDENTICAL);
		while(TypeSupport::hasByteLayout(st))
		{
			addAllBaseTypesForByteLayout(st, st);
			// If the data of this byte layout struct is always accessed as the same type, we can replace it with an array of that type
			// This is useful for an idiom used by C++ graphics code to have a vector both accessible as named elements and as an array
			// union { struct { double x,y,z; }; double elemets[3]; };
			auto it=baseTypesForByteLayout.find(st);
			assert(it!=baseTypesForByteLayout.end());
			if(it->second == NULL)
				break;
			// Check that the struct fits exactly N values of the base type
			uint32_t structSize = DL->getTypeAllocSize(st);
			uint32_t elementSize = DL->getTypeAllocSize(it->second);
			if(structSize % elementSize)
				break;

			bool areSubStructsConvertible = true;
			// Every struct type inside the struct must be also convertible to array
			for(uint32_t i=0;i<st->getNumElements();i++)
			{
				StructType* subSt = dyn_cast<StructType>(st->getElementType(i));
				if(!subSt)
					continue;
				if(!subSt->hasByteLayout())
				{
					// If subSt is a struct but not bytelayout code generation is broken
					areSubStructsConvertible = false;
					break;
				}
				const TypeMappingInfo& subInfo = rewriteType(subSt);
				if(subInfo.elementMappingKind != TypeMappingInfo::BYTE_LAYOUT_TO_ARRAY)
				{
					areSubStructsConvertible = false;
					break;
				}
			}
			if(!areSubStructsConvertible)
				break;

			uint32_t numElements = structSize / elementSize;
			// See if we can replace it with a single element
			if(numElements==1)
				return CacheAndReturn(it->second, TypeMappingInfo::BYTE_LAYOUT_TO_ARRAY);

			// Replace this byte layout struct with an array
			Type* newType = ArrayType::get(it->second, numElements);
			return CacheAndReturn(newType, TypeMappingInfo::BYTE_LAYOUT_TO_ARRAY);
		}

		// Generate a new type if it's not a literal struct. It may end up being the same as the old one
		// In case of literal, it will be created as a literal at the end.
		StructType* newStruct=nullptr;
		if (!st->isLiteral())
			newStruct=StructType::create(st->getContext());
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
		if (!st->isLiteral())
			typesMapping[t] = TypeMappingInfo(newStruct, TypeMappingInfo::IDENTICAL);

		// Since we can merge arrays of the same type in an struct it is possible that at the end of the process a single type will remain
		TypeMappingInfo::MAPPING_KIND newStructKind = TypeMappingInfo::IDENTICAL;
		// Forge the new element types
		SmallVector<Type*, 4> newTypes;
		bool hasMergedArrays=false;
		std::vector<std::pair<uint32_t, uint32_t>> membersMapping;
		if (st->hasAsmJS() || (st->hasByteLayout() && st->getNumElements() > 1))
		{
			//Given that a collection of member would have currentSize, and the next member requires a given alignRequired
			//Basically, solve X = currentSize + something, with X%alignment == 0 and something as small as possible, and then return X
			auto padStruct = [](uint32_t currentSize, llvm::Align alignRequired) -> uint32_t
			{
				const uint32_t log2Align = Log2(alignRequired);         //either 0,1,2,3, depending on the required alignment
				const uint32_t toBeAlignedTo = 1u << log2Align;                 //either 1,2,4,8
				uint32_t padSize = currentSize + toBeAlignedTo - 1;     //add either 0, 1, 3, 7
				padSize >>= log2Align;
				padSize <<= log2Align;                                  //zeros the least significant 0,1,2,3 bits

				return padSize;
			};

			const auto& SL = DL->getStructLayout(st);
			uint32_t currentSize = 0;
			llvm::Align maxAlignment(1);
			for(uint32_t i=0;i<st->getNumElements();i++)
			{
				AlignmentInfo alignmentInfo;
				alignmentInfo.first = alignmentInfo.second = llvm::Align(1);
				Type* elTy = st->getElementType(i);
				Type* nextTy = st->isPacked() ?
						rewriteType(elTy) :						//packed -> rewrite type blindly
						rewriteTypeWithAlignmentInfo(elTy, alignmentInfo);		//!packed -> perform alignment checks
				assert(alignmentInfo.first >= alignmentInfo.second);
				maxAlignment = std::max(maxAlignment, alignmentInfo.second);

				assert(alignmentInfo.first >= alignmentInfo.second);
				const uint32_t originalPaddedSize = SL->getElementOffset(i);
				const uint32_t nextPaddedSize = padStruct(currentSize, alignmentInfo.second);

				const uint32_t toAdd = originalPaddedSize - currentSize;
				assert(toAdd < 32);

				//Padding bytes are inserted only if adding them actually change the size
				//eg. {i8, i32, i64} -> {i8, i32, padding?? ,[2 x i32]}
				//Here padding is not needed (originalPaddedSize 8 vs nextPaddedSize 8), so the resulting struct will be {i8, i32, [2 x i32]}
				//eg. {i8, i64} -> {i8, padding??, [2 x i32]}
				//Here padding is needed (originalPaddedSize 8 vs nextPaddedSize 4), so ther resulting struct will be {i8, [7 x i8], [2 x i32]}
				if (originalPaddedSize != nextPaddedSize && toAdd)
				{
					currentSize += toAdd;
					//An array [toAdd x i8] is added
					newTypes.push_back(ArrayType::get(IntegerType::get(module->getContext(), 8), toAdd));
					newStructKind = TypeMappingInfo::PADDING;
				}

				currentSize = padStruct(currentSize, alignmentInfo.second);     //First grow pad currentSize to the appropriate alignment
				currentSize += DL->getTypeAllocSize(elTy);                      //Then add the next type dimension

				membersMapping.push_back({newTypes.size(), 0});                 //Note that the second field is unused
				newTypes.push_back(nextTy);
			}

			const uint32_t originalSize = DL->getTypeAllocSize(st);
			const uint32_t totalPaddedSize = padStruct(currentSize, maxAlignment);
			const uint32_t toAdd = originalSize - currentSize;
			assert( toAdd < 8 );
			//Pad the struct at the end
			//The problematic test case is something like {i8, i64, i8} -> {i8, [7 x i8], [2 x i32], i8, ???}
			//In the original struct, the allocation size is 24 (17 rounded to the next multiple of 8)
			//while in the rewritten struct it will be 20 (17 rounded to the next multiple of 4), so we need to pad the end correctly
			if (originalSize != totalPaddedSize && toAdd)
			{
				currentSize += toAdd;
				//An array [toAdd x i8] is added
				newTypes.push_back(ArrayType::get(IntegerType::get(module->getContext(), 8), toAdd));
				newStructKind = TypeMappingInfo::PADDING;
			}
			currentSize = padStruct(currentSize, maxAlignment);

			//If we ever padded, memorize the member mappings
			if (newStructKind == TypeMappingInfo::PADDING)
			{
				membersMappingData.insert(std::make_pair(st, std::move(membersMapping)));
			}

			//TypeOptimizer should not change the dimension of a given structure
			assert(DL->getTypeAllocSize(st) == currentSize);
		}
		else if(st->getNumElements() > 1)
		{
			// We want to merge arrays of the same type in the same object
			// So, for each element type, keep track if there is already an array
			std::unordered_map<Type*, uint32_t> arraysFound;
			// Keep track of currently fillable integers
			std::vector<std::pair<uint32_t, uint8_t>> mergedInts;
			uint32_t directBaseLimit=0;
			// We may need to update the bases metadata for this type
			NamedMDNode* namedBasesMetadata = nullptr;
			if (!st->isLiteral())
				namedBasesMetadata = TypeSupport::getBasesMetadata(newStruct, *module);
			uint32_t firstBaseBegin, firstBaseEnd;
			if(namedBasesMetadata)
			{
				MDNode* md = namedBasesMetadata->getOperand(0);
				firstBaseBegin=getIntFromValue(cast<ConstantAsMetadata>(md->getOperand(0))->getValue());
				firstBaseEnd=firstBaseBegin;
			}
			for(uint32_t i=0;i<st->getNumElements();i++)
			{
				// We can't merge arrats across bases, so when we reach the limit of the previous direct base we
				// reset the merging state and compute a new limit
				if(i==directBaseLimit)
				{
					arraysFound.clear();
					mergedInts.clear();
					StructType* curBase=st;
					while(curBase->getDirectBase() && curBase->getDirectBase()->getNumElements()>i && isa<StructType>(rewriteType(curBase->getDirectBase()).mappedType))
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
				else if(IntegerType* it=dyn_cast<IntegerType>(rewrittenType))
				{
					TypeAndIndex typeAndIndex(st, i, TypeAndIndex::STRUCT_MEMBER);
					bool fieldEscapes = escapingFields.count(typeAndIndex);
					// Merge small integers together to reduce memory usage
					if(!fieldEscapes && it->getBitWidth() < 32)
					{
						// Look for an integer than can be filled
						bool mergedThisInt = false;
						for(size_t m=0;m<mergedInts.size();m++)
						{
							auto& mergedInt = mergedInts[m];
							if(mergedInt.second < it->getBitWidth() || (it->getBitWidth()%8 != 0))
								continue;
							// There is enough space in an integer. Promote the type and merge with this one
							IntegerType* oldType = cast<IntegerType>(newTypes[mergedInt.first]);
							newTypes[mergedInt.first] = IntegerType::get(module->getContext(), oldType->getBitWidth()+it->getBitWidth());
							membersMapping.push_back(std::make_pair(mergedInt.first, 32-mergedInt.second));
							mergedInt.second -= it->getBitWidth();
							// Remove fully used integers
							if(mergedInt.second == 0)
								mergedInts.erase(mergedInts.begin()+m);
							if(i < firstBaseBegin)
								firstBaseEnd--;
							hasMergedArrays=true;
							mergedThisInt=true;
							break;
						}
						if(mergedThisInt)
							continue;
						// Not enough space on any integer
						mergedInts.push_back(std::make_pair(newTypes.size(), 32 - it->getBitWidth()));
					}
				}
				membersMapping.push_back(std::make_pair(newTypes.size(), 0));
				// Add the new type
				newTypes.push_back(rewrittenType);
			}
			assert(!st->hasByteLayout());
			assert(membersMapping.size() == st->getNumElements());
			if(hasMergedArrays)
			{
				assert(!newTypes.empty());
				membersMappingData.insert(std::make_pair(st, std::move(membersMapping)));
				newStructKind = TypeMappingInfo::MERGED_MEMBER_ARRAYS;
				// Update bases metadata
				if(namedBasesMetadata)
				{
					Type* Int32 = IntegerType::get(module->getContext(), 32);
					Metadata* newBasesMeta[] = { ConstantAsMetadata::get(ConstantInt::get(Int32, firstBaseEnd))};
					MDNode* newMD = MDNode::get(module->getContext(), newBasesMeta);
					// The bases metadata has numerous duplicated entries, so fix all of them
					// TODO: Remove duplicated entries
					for(uint32_t i=0;i<namedBasesMetadata->getNumOperands();i++)
						namedBasesMetadata->setOperand(i, newMD);
				}
			}

			assert(!st->hasAsmJS());
			if(newTypes.size() == 1 && canCollapseStruct(st, newStruct, newTypes[0]))
			{
				Type* collapsed = newTypes[0];
				if(newStructKind != TypeMappingInfo::MERGED_MEMBER_ARRAYS)
					return CacheAndReturn(collapsed, TypeMappingInfo::COLLAPSED);
				else
					return CacheAndReturn(collapsed, TypeMappingInfo::MERGED_MEMBER_ARRAYS_AND_COLLAPSED);
			}

		}
		else if(st->getNumElements() == 1)
		{
			// Try to collapse the struct to this element
			llvm::Type* elementType = st->getElementType(0);
			assert(!st->hasAsmJS());
			if(canCollapseStruct(st, newStruct, elementType))
			{
				// To fix the following case A { B { C { A* } } } -> C { C* }
				// we prime the mapping to the contained element and use the COLLAPSING flag
				typesMapping[st] = TypeMappingInfo(elementType, TypeMappingInfo::COLLAPSING);
				Type* collapsed = rewriteType(elementType);
				if(typesMapping[st].elementMappingKind != TypeMappingInfo::COLLAPSING_BUT_USED)
				{
					assert(typesMapping[st].elementMappingKind == TypeMappingInfo::COLLAPSING);
					if(newStructKind != TypeMappingInfo::MERGED_MEMBER_ARRAYS)
						return CacheAndReturn(collapsed, TypeMappingInfo::COLLAPSED);
					else
						return CacheAndReturn(collapsed, TypeMappingInfo::MERGED_MEMBER_ARRAYS_AND_COLLAPSED);
				}
				typesMapping[st] = TypeMappingInfo(newStruct, TypeMappingInfo::IDENTICAL);
				elementType = collapsed;
			}
			else
			{
				// Can't collapse, rewrite the member now
				elementType = rewriteType(elementType);
			}
			newTypes.push_back(elementType);
		}

		StructType* newDirectBase = st->getDirectBase() ? dyn_cast<StructType>(rewriteType(st->getDirectBase()).mappedType) : NULL;
		if (st->isLiteral())
		{
			newStruct = StructType::get(st->getContext(), newTypes, st->isPacked(), newDirectBase, st->hasByteLayout(), st->hasAsmJS());
			typesMapping[t] = TypeMappingInfo(newStruct, TypeMappingInfo::IDENTICAL);
		}
		else
			newStruct->setBody(newTypes, st->isPacked(), newDirectBase, st->hasByteLayout(), st->hasAsmJS());

		return CacheAndReturn(newStruct, newStructKind);
	}
	if(FunctionType* ft=dyn_cast<FunctionType>(t))
	{
		return CacheAndReturn(rewriteFunctionType(ft, false), TypeMappingInfo::IDENTICAL);
	}
	if(PointerType* pt=dyn_cast<PointerType>(t))
	{
		Type* elementType = pt->getPointerElementType();
		Type* newType = rewriteType(elementType);
		if(newType->isArrayTy())
		{
			// It's never a good idea to use pointers to array, we may end up creating wrapper arrays for arrays
			return CacheAndReturn(PointerType::get(newType->getArrayElementType(), pt->getAddressSpace()), TypeMappingInfo::POINTER_FROM_ARRAY);
		}
		else if(newType == elementType)
			return CacheAndReturn(pt, TypeMappingInfo::IDENTICAL);
		else
			return CacheAndReturn(PointerType::get(newType, pt->getPointerAddressSpace()), TypeMappingInfo::IDENTICAL);
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
	if (isI64ToRewrite(t))
	{
		t = ArrayType::get(IntegerType::get(t->getContext(), 32), 2);
		return CacheAndReturn(t, TypeMappingInfo::IDENTICAL);
	}
	if (FixedVectorType* vt=dyn_cast<FixedVectorType>(t))
	{
		Type* elementType = vt->getElementType();
		if (elementType->isIntegerTy())
			return CacheAndReturn(vt, TypeMappingInfo::IDENTICAL);
		const TypeMappingInfo& newInfo = rewriteType(elementType);
		Type* newType = newInfo.mappedType;
		if (newType == elementType)
			return CacheAndReturn(vt, TypeMappingInfo::IDENTICAL);
		else
			return CacheAndReturn(FixedVectorType::get(newType, vt->getNumElements()), TypeMappingInfo::IDENTICAL);
	}
	return CacheAndReturn(t, TypeMappingInfo::IDENTICAL);
}

FunctionType* TypeOptimizer::rewriteFunctionType(FunctionType* ft, bool keepI64)
{
	Type* newReturnType = nullptr;
	if (isI64ToRewrite(ft->getReturnType()) && keepI64)
	{
		newReturnType = ft->getReturnType();
	}
	else if (isI64ToRewrite(ft->getReturnType()))
	{
		newReturnType = IntegerType::get(ft->getContext(), 32);
	}
	else
	{
		newReturnType = rewriteType(ft->getReturnType());
	}
	SmallVector<Type*, 4> newParameters;
	for(uint32_t i=0;i<ft->getNumParams();i++)
	{
		Type* oldP = ft->getParamType(i);
		if (isI64ToRewrite(oldP))
		{
			if (keepI64)
				newParameters.push_back(oldP);
			else
			{
				Type* Int32Ty = IntegerType::get(oldP->getContext(), 32);
				newParameters.push_back(Int32Ty);
				newParameters.push_back(Int32Ty);
			}
		}
		else
			newParameters.push_back(rewriteType(oldP));
	}
	return FunctionType::get(newReturnType, newParameters, ft->isVarArg());
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

std::pair<Constant*, uint8_t> TypeOptimizer::rewriteConstant(Constant* C, bool rewriteI64)
{
	// Immediately return for globals, we should never try to map their type as they are already rewritten
	if(isa<GlobalAlias>(C))
		return std::make_pair(C, 0);
	if(GlobalValue* GV=dyn_cast<GlobalValue>(C))
	{
		assert(globalsMapping.count(GV));
		return std::make_pair(globalsMapping[GV], 0);
	}
	TypeMappingInfo newTypeInfo = rewriteType(C->getType());
	if (ConstantExpr* CE=dyn_cast<ConstantExpr>(C))
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
				auto rewrittenOperand = rewriteConstant(ptrOperand, false);
				assert(rewrittenOperand.second==0);
				ptrOperand = rewrittenOperand.first;
				SmallVector<Value*, 4> newIndexes;
				Type* srcType = rewriteType(cast<GEPOperator>(CE)->getSourceElementType());
				Type* targetType = rewriteType(cast<GEPOperator>(CE)->getResultElementType());
				SmallVector<Value*, 2> idxs;
				for (auto Op = CE->op_begin()+1; Op != CE->op_end(); ++Op)
				{
					idxs.push_back(*Op);
				}
				if (isa<ArrayType>(srcType))
					srcType = cast<ArrayType>(srcType)->getElementType();
				uint8_t mergedIntegerOffset=rewriteGEPIndexes(newIndexes, ptrType, cast<GEPOperator>(CE)->getSourceElementType(), idxs, targetType, NULL, CE->getType());
				return std::make_pair(ConstantExpr::getGetElementPtr(srcType, ptrOperand, newIndexes), mergedIntegerOffset);
			}
			case Instruction::BitCast:
			{
				auto rewrittenOperand = rewriteConstant(CE->getOperand(0), false);
				assert(rewrittenOperand.second == 0);
				Constant* srcOperand = rewrittenOperand.first;
				return std::make_pair(ConstantExpr::getBitCast(srcOperand, newTypeInfo.mappedType), 0);
			}
			case Instruction::AddrSpaceCast:
			{
				auto rewrittenOperand = rewriteConstant(CE->getOperand(0), false);
				assert(rewrittenOperand.second == 0);
				Constant* srcOperand = rewrittenOperand.first;
				return std::make_pair(ConstantExpr::getPointerBitCastOrAddrSpaceCast(srcOperand, newTypeInfo.mappedType), 0);
			}
			case Instruction::IntToPtr:
			{
				return std::make_pair(ConstantExpr::getIntToPtr(CE->getOperand(0), newTypeInfo.mappedType), 0);
			}
			default:
			{
				// Get a cloned CE with rewritten operands
				std::vector<Constant*> newOperands;
				for(Use& op: CE->operands())
				{
					auto rewrittenOperand = rewriteConstant(cast<Constant>(op), false);
					assert(rewrittenOperand.second == 0);
					newOperands.push_back(rewrittenOperand.first);
				}
				return std::make_pair(CE->getWithOperands(newOperands), 0);
			}
		}
	}
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
				return std::make_pair(newElements[0], 0);
			ArrayType* newArrayType = ArrayType::get(baseTypeIt->second, newElements.size());
			return std::make_pair(ConstantArray::get(newArrayType, newElements), 0);
		}
		else if(newTypeInfo.elementMappingKind == TypeMappingInfo::COLLAPSED)
		{
			assert(cast<StructType>(CS->getType())->getNumElements()==1);
			Constant* element = CS->getOperand(0);
			return rewriteConstant(element, rewriteI64);
		}
		auto membersMappingIt = membersMappingData.find(CS->getType());
		bool hasMergedArrays = newTypeInfo.elementMappingKind == TypeMappingInfo::MERGED_MEMBER_ARRAYS ||
					newTypeInfo.elementMappingKind == TypeMappingInfo::MERGED_MEMBER_ARRAYS_AND_COLLAPSED;
		assert(!hasMergedArrays || membersMappingIt != membersMappingData.end());
		SmallVector<Constant*, 4> newElements;

		if(newTypeInfo.elementMappingKind == TypeMappingInfo::PADDING)
		{
			for(uint32_t i=0;i<CS->getNumOperands();i++)
			{
				//Check whether an intermediate paddign has to be added
				if (membersMappingData[CS->getType()][i].first > newElements.size())
				{
					assert(isa<StructType>(newTypeInfo.mappedType));
					newElements.push_back(UndefValue::get(dyn_cast<StructType>(newTypeInfo.mappedType)->getElementType((int)newElements.size())));
				}
				Constant* element = CS->getOperand(i);
				auto rewrittenOperand = rewriteConstant(element, rewriteI64);
				assert(rewrittenOperand.second == 0);
				Constant* newElement = rewrittenOperand.first;
				newElements.push_back(newElement);
			}
			//Check whether there is a last padding to be added
			if (dyn_cast<StructType>(newTypeInfo.mappedType)->getNumElements() > newElements.size())
			{
				assert(isa<StructType>(newTypeInfo.mappedType));
				newElements.push_back(UndefValue::get(dyn_cast<StructType>(newTypeInfo.mappedType)->getElementType((int)newElements.size())));
			}
			return std::make_pair(ConstantStruct::get(cast<StructType>(newTypeInfo.mappedType), newElements), 0);
		}

		// Check if some of the contained constant arrays needs to be merged
		for(uint32_t i=0;i<CS->getNumOperands();i++)
		{
			Constant* element = CS->getOperand(i);
			auto rewrittenOperand = rewriteConstant(element, rewriteI64);
			assert(rewrittenOperand.second == 0);
			Constant* newElement = rewrittenOperand.first;
			if(hasMergedArrays && membersMappingIt->second[i].first != (newElements.size()))
			{
				// This element has been remapped to another one. It must be an array
				SmallVector<Constant*, 4> mergedArrayElements;
				Constant* oldMember = newElements[membersMappingIt->second[i].first];
				if(isa<ArrayType>(oldMember->getType()))
				{
					assert(oldMember->getType()->getArrayElementType() == newElement->getType()->getArrayElementType());
					// Insert all the elements of the existing member
					pushAllArrayConstantElements(mergedArrayElements, oldMember);
					pushAllArrayConstantElements(mergedArrayElements, newElement);
					// Forge a new array and replace oldMember
					ArrayType* mergedType = ArrayType::get(oldMember->getType()->getArrayElementType(), mergedArrayElements.size());
					newElements[membersMappingIt->second[i].first] = ConstantArray::get(mergedType, mergedArrayElements);
				}
				else if(isa<IntegerType>(oldMember->getType()))
				{
					uint32_t oldValue = cast<ConstantInt>(oldMember)->getZExtValue();
					uint32_t newValue = cast<ConstantInt>(newElement)->getZExtValue();
					newValue <<= membersMappingIt->second[i].second;
					uint32_t finalValue = oldValue | newValue;
					Type* IntType = nullptr;
					if(newTypeInfo.elementMappingKind == TypeMappingInfo::MERGED_MEMBER_ARRAYS_AND_COLLAPSED)
						IntType = newTypeInfo.mappedType;
					else
						IntType = cast<StructType>(newTypeInfo.mappedType)->getElementType(membersMappingIt->second[i].first);
					newElements[membersMappingIt->second[i].first] = ConstantInt::get(IntType, finalValue);
				}
			}
			else
				newElements.push_back(newElement);
		}
		if(newTypeInfo.elementMappingKind == TypeMappingInfo::MERGED_MEMBER_ARRAYS_AND_COLLAPSED)
		{
			assert(newElements.size() == 1);
			return std::make_pair(newElements[0], 0);
		}
		return std::make_pair(ConstantStruct::get(cast<StructType>(newTypeInfo.mappedType), newElements), 0);
	}
	else if(ConstantArray* CA=dyn_cast<ConstantArray>(C))
	{
		assert(newTypeInfo.mappedType->isArrayTy());
		SmallVector<Constant*, 4> newElements;
		for(uint32_t i=0;i<CA->getNumOperands();i++)
		{
			Constant* element = CA->getOperand(i);
			auto rewrittenOperand = rewriteConstant(element, rewriteI64);
			assert(rewrittenOperand.second == 0);
			Constant* newElement = rewrittenOperand.first;
			if(newTypeInfo.elementMappingKind == TypeMappingInfo::FLATTENED_ARRAY)
			{
				// Put all the operands of the element in this array
				pushAllArrayConstantElements(newElements, newElement);
			}
			else
				newElements.push_back(newElement);
		}
		return std::make_pair(ConstantArray::get(cast<ArrayType>(newTypeInfo.mappedType), newElements), 0);
	}
	else if (ConstantVector* CV = dyn_cast<ConstantVector>(C))
	{
		assert(newTypeInfo.mappedType->isVectorTy());
		SmallVector<Constant*, 4> newElements;
		for (unsigned i = 0; i < CV->getType()->getNumElements(); i++)
		{
			Constant* element = CV->getAggregateElement(i);
			auto rewrittenOperand = rewriteConstant(element, false);
			assert(rewrittenOperand.second == 0);
			Constant* newElement = rewrittenOperand.first;
			newElements.push_back(newElement);
		}
		return std::make_pair(ConstantVector::get(newElements), 0);
	}
	else if (ConstantDataVector* CDV = dyn_cast<ConstantDataVector>(C))
		return std::make_pair(C, 0);
	else if(C->getType() == newTypeInfo.mappedType)
		return std::make_pair(C, 0);
	else if(ConstantDataArray* CA=dyn_cast<ConstantDataArray>(C))
	{
		assert(newTypeInfo.mappedType->isArrayTy());
		assert(CA->getElementType()->isIntegerTy(64));
		SmallVector<uint32_t, 4> newElements;
		newElements.resize(CA->getNumElements() * 2);
		for(uint32_t i=0;i<CA->getNumElements();i++)
		{
			uint64_t el = CA->getElementAsInteger(i);
			uint32_t elLow = el;
			uint32_t elHigh = el>>32;
			newElements[i*2] = elLow;
			newElements[i*2+1] = elHigh;
		}
		return std::make_pair(ConstantDataArray::get(C->getContext(), newElements), 0);
	}
	else if(isa<ConstantAggregateZero>(C))
		return std::make_pair(Constant::getNullValue(newTypeInfo.mappedType), 0);
	else if(isa<ConstantPointerNull>(C))
		return std::make_pair(ConstantPointerNull::get(cast<PointerType>(newTypeInfo.mappedType)), 0);
	else if(isI64ToRewrite(C->getType()))
	{
		if (!rewriteI64)
			return std::make_pair(C, TypeMappingInfo::IDENTICAL);
		Type* Int32Ty = IntegerType::get(C->getContext(), 32);
		Constant* Low = ConstantExpr::getTrunc(C, Int32Ty);
		Constant* High = ConstantExpr::getTrunc(ConstantExpr::getLShr(C, ConstantInt::get(C->getType(), 32)), Int32Ty);
		Constant* Arr[2] = {Low, High};
		ArrayType* ArrTy = ArrayType::get(Int32Ty, 2);
		return std::make_pair(ConstantArray::get(ArrTy, Arr), TypeMappingInfo::IDENTICAL);
	}
	else if(isa<UndefValue>(C))
		return std::make_pair(UndefValue::get(newTypeInfo.mappedType), 0);
	else
		assert(false && "Unexpected constant in TypeOptimizer");
	return std::make_pair((Constant*)NULL, 0);
}

Function* TypeOptimizer::rewriteIntrinsic(Function* F, FunctionType* FT)
{
	auto fixFuncType = [](Function* F, FunctionType* FT)
	{
		F->mutateType(FT->getPointerTo());
		F->setValueType(FT);
		auto AI = F->arg_begin();
		auto TI = FT->param_begin();
		for (; AI != F->arg_end(); ++AI, ++TI)
		{
			AI->mutateType(*TI);
		}
	};

	SmallVector<Type*, 3> newTys;
	switch(F->getIntrinsicID())
	{
		case Intrinsic::cheerp_allocate:
		case Intrinsic::cheerp_allocate_array:
		{
			Type* localTys[] = { FT->getReturnType()};
			newTys.insert(newTys.end(),localTys,localTys+1);
			break;
		}
		case Intrinsic::cheerp_upcast_collapsed:
		case Intrinsic::cheerp_cast_user:
		case Intrinsic::cheerp_downcast:
		case Intrinsic::cheerp_virtualcast:
		case Intrinsic::cheerp_make_complete_object:
		case Intrinsic::cheerp_make_regular:
		{
			Type* localTys[] = { FT->getReturnType(), FT->getParamType(0)};
			newTys.insert(newTys.end(),localTys,localTys+2);
			break;
		}
		case Intrinsic::cheerp_reallocate:
		{
			Type* localTys[] = { FT->getReturnType(), FT->getParamType(1)};
			newTys.insert(newTys.end(),localTys,localTys+2);
			break;
		}
		case Intrinsic::cheerp_downcast_current:
		case Intrinsic::cheerp_get_array_len:
		case Intrinsic::cheerp_pointer_kind:
		case Intrinsic::cheerp_throw:
		case Intrinsic::cheerp_pointer_offset:
		{
			Type* localTys[] = { FT->getParamType(0) };
			newTys.insert(newTys.end(),localTys,localTys+1);
			break;
		}
		case Intrinsic::invariant_start:
		case Intrinsic::invariant_end:
		case Intrinsic::cheerp_deallocate:
		{
			Type* localTys[] = { FT->getParamType(1) };
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
		case Intrinsic::umul_with_overflow:
		{
			Type* localTys[] = { FT->getParamType(0) };
			newTys.insert(newTys.end(),localTys,localTys+1);
			break;
		}
		default:
		{
			break;
		}
	}


	const std::string& intrName = Intrinsic::getName(F->getIntrinsicID(), newTys, module);

	// If the name does not change we only need to fix the type
	if(F->getName() == intrName)
	{
		fixFuncType(F, FT);
		return F;
	}

	// If an intrinsic with the new name already exists, just return it.
	// We will fix the type (if necessary) when we iterate on it.
	Function* newF = module->getFunction(intrName);
	if (newF)
		return newF;

	// We need a new function. Create it.
	newF = Intrinsic::getDeclaration(F->getParent(), (Intrinsic::ID)F->getIntrinsicID(), newTys);
	assert(newF != F);
	return newF;
}

Value* TypeOptimizer::getConstantForGEP(Type* Int32Ty, uint32_t constant, Type* returnType)
{
	Constant* ci = ConstantInt::get(Int32Ty, constant);
	if (!returnType->isVectorTy())
		return ci;
	const FixedVectorType* vecType = cast<FixedVectorType>(returnType);
	const unsigned num = vecType->getNumElements();
	return ConstantDataVector::getSplat(num, ci);
}

uint8_t TypeOptimizer::rewriteGEPIndexes(SmallVector<Value*, 4>& newIndexes, Type* ptrType, Type* srcType, ArrayRef<Value*> idxs, Type* targetType, Instruction* insertionPoint, Type* returnType)
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
	uint32_t integerOffset = 0;
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
			case TypeMappingInfo::PADDING:
			{
				StructType* oldStruct = cast<StructType>(curType);
				uint32_t elementIndex = cast<ConstantInt>(idxs[i])->getZExtValue();
				assert(membersMappingData.count(oldStruct));
				const uint32_t newIndex = membersMappingData[oldStruct][elementIndex].first;
				AddIndex(getConstantForGEP(Int32Ty, newIndex, returnType));
				break;
			}
			case TypeMappingInfo::COLLAPSED:
				break;
			case TypeMappingInfo::BYTE_LAYOUT_TO_ARRAY:
			{
				assert(integerOffset==0);
				assert(isa<StructType>(curType));
				if(curTypeMappingInfo.mappedType == targetType)
				{
					if(targetType->isArrayTy())
					{
						// We are transforming all pointers to arrays to pointers to elements
						Value* Zero = getConstantForGEP(Int32Ty, 0, returnType);
						AddIndex(Zero);
					}
					return 0;
				}
				auto baseTypeIt = baseTypesForByteLayout.find(cast<StructType>(curType));
				assert(baseTypeIt != baseTypesForByteLayout.end() && baseTypeIt->second);
				if(!curTypeMappingInfo.mappedType->isArrayTy())
				{
					// If it's not an array it must be a single element and we should stop immediately
					assert(curTypeMappingInfo.mappedType == baseTypeIt->second);
					return 0;
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
						AddIndex(getConstantForGEP(Int32Ty, elementOffset / baseTypeSize, returnType));
						curType = ST->getElementType(elementIndex);
					}
					else
					{
						uint32_t elementSize = DL->getTypeAllocSize(curType->getArrayElementType());
						// All offsets should be multiple of the base type size
						assert(!(elementSize % baseTypeSize));
						AddMultipliedIndex(idxs[i], elementSize / baseTypeSize);
						assert(isa<ArrayType>(curType));
						curType = curType->getArrayElementType();
					}
					addToLastIndex = true;
				}
				// All indexes have been consumed now, we can just return
				assert(rewriteType(curType) == targetType);
				if(targetType->isArrayTy())
				{
					// We are transforming all pointers to arrays to pointers to elements
					Value* Zero = getConstantForGEP(Int32Ty, 0, returnType);
					AddIndex(Zero);
				}
				return 0;
			}
			case TypeMappingInfo::POINTER_FROM_ARRAY:
			{
				// This should only happen for the first element
				assert(i==0);
				// We need to multiply the index by the right number of elements, corresponding to the size of the old type
				// Fall through, the code is identical for POINTER_FROM_ARRAY and FLATTENED_ARRAY
				[[clang::fallthrough]];
			}
			case TypeMappingInfo::FLATTENED_ARRAY:
			{
				// We had something like [ N x [ M x T ] ] which is now [ N*M x T ]
				uint32_t oldTypeSize = DL->getTypeAllocSize(rewriteType(getElementType(curType, srcType)));

				llvm::Type* mappedType = curTypeMappingInfo.mappedType;
				llvm::Type* elementType = nullptr;
				if (ArrayType* mappedArrayType = dyn_cast<ArrayType>(mappedType))
				{
					elementType = mappedArrayType->getArrayElementType();
				}
				else if (PointerType* mappedPointedType = dyn_cast<PointerType>(mappedType))
				{
					llvm::Type* rewritten = rewriteType(srcType);

					if (ArrayType* rewrittenAsArray = dyn_cast<ArrayType>(rewritten))
						elementType = rewrittenAsArray->getArrayElementType();
					else
					{
						assertPointerElementOrOpaque(mappedPointedType, rewriteType(getElementType(curType, srcType)));
						elementType = mappedPointedType->getPointerElementType();
					}
				}

				assert(elementType);
				uint32_t elementSize = DL->getTypeAllocSize(elementType);
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
					AddIndex(getConstantForGEP(Int32Ty, mappedMember.first, returnType));
				}
				else
					assert(mappedMember.first == 0);
				// We need to check if the mapped type for the element has become an integer
				Type* mappedElementType = rewriteType(oldStruct->getElementType(elementIndex));
				bool isMergedInt = mappedElementType->isIntegerTy();
				// If mappedMember.second is not zero, also add a new index that can be eventually incremented later
				// For merged integers we don't add the offset here, but return it. It will need to be applied by the following loads/stores
				if(isMergedInt)
					integerOffset += mappedMember.second;
				else if(mappedMember.second)
				{
					AddIndex(getConstantForGEP(Int32Ty, mappedMember.second, returnType));
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
			curType = getElementType(curType, srcType);
	}
	assert(rewriteType(curType) == targetType);
	if(targetType->isArrayTy())
	{
		// We are transforming all pointers to arrays to pointers to elements
		Value* Zero = getConstantForGEP(Int32Ty, 0, returnType);
		AddIndex(Zero);
	}
	return integerOffset;
}

Function* TypeOptimizer::rewriteFunctionSignature(Function* F)
{
	FunctionType* oldFuncType = F->getFunctionType();
	bool keepI64 = onlyCalledByWasmFuncs.count(F) && !F->isVarArg();
	FunctionType* newFuncType = rewriteFunctionType(oldFuncType, F->isIntrinsic() || keepI64);
	if(newFuncType==oldFuncType)
		return F;

	if(F->isIntrinsic())
	{
		return rewriteIntrinsic(F, newFuncType);
	}

	SmallVector<AttributeSet, 8> ArgAttrVec;
	AttributeList PAL = F->getAttributes();
	for(unsigned i = 0; i < F->arg_size(); ++i)
	{
		Argument* CurA = F->arg_begin()+i;
		if (isI64ToRewrite(CurA->getType()) && !keepI64)
		{
			ArgAttrVec.push_back(AttributeSet());
			ArgAttrVec.push_back(AttributeSet());
		}
		else
		{
			AttributeSet CurAttrs = PAL.getParamAttrs(i);
			if(CurAttrs.hasAttribute(Attribute::ByVal))
			{
				Type* argElemType = F->getParamByValType(i);
				PAL = PAL.removeParamAttribute(F->getContext(), i, Attribute::ByVal);
				Type* rewrittenArgType = rewriteType(argElemType);
				if(rewrittenArgType->isStructTy())
				{
					PAL = PAL.addParamAttribute(F->getContext(), i, Attribute::getWithByValType(F->getContext(), rewrittenArgType));
				}
			}
			if(CurAttrs.hasAttribute(Attribute::StructRet))
			{
				Type* argElemType = F->getParamStructRetType(i);
				PAL = PAL.removeParamAttribute(F->getContext(), i, Attribute::StructRet);
				Type* rewrittenArgType = rewriteType(argElemType);
				if(rewrittenArgType->isStructTy())
				{
					PAL = PAL.addParamAttribute(F->getContext(), i, Attribute::getWithStructRetType(F->getContext(), rewrittenArgType));
				}
			}
			if (CurAttrs.hasAttribute(Attribute::JsExportType))
			{
				Type* type = CurAttrs.getAttribute(Attribute::JsExportType).getValueAsType();
				PAL = PAL.removeParamAttribute(F->getContext(), i, Attribute::JsExportType);
				Type* rewrittenType = rewriteType(type);
				PAL = PAL.addParamAttribute(F->getContext(), i, Attribute::get(F->getContext(), Attribute::JsExportType, rewrittenType));
			}
			if (CurAttrs.hasAttribute(Attribute::ElementType))
			{
				Type* type = CurAttrs.getAttribute(Attribute::ElementType).getValueAsType();
				PAL = PAL.removeParamAttribute(F->getContext(), i, Attribute::ElementType);
				Type* rewrittenType = rewriteType(type);
				PAL = PAL.addParamAttribute(F->getContext(), i, Attribute::get(F->getContext(), Attribute::ElementType, rewrittenType));
			}
			ArgAttrVec.push_back(PAL.getParamAttrs(i));
		}
	}
	if (PAL.hasRetAttr(Attribute::JsExportType))
	{
		Type* type = PAL.getRetAttrs().getAttribute(Attribute::JsExportType).getValueAsType();
		PAL = PAL.removeRetAttribute(F->getContext(), Attribute::JsExportType);
		Type* rewrittenType = rewriteType(type);
		PAL = PAL.addRetAttribute(F->getContext(), Attribute::get(F->getContext(), Attribute::JsExportType, rewrittenType));
	}
	F->setAttributes(PAL);

	// Create the new function body and insert it into the module.
	Function *NF = Function::Create(newFuncType, F->getLinkage(), F->getAddressSpace(), F->getName());
	NF->copyAttributesFrom(F);
	NF->copyMetadata(F, 0);

	// Patch the pointer to LLVM function in debug info descriptor.
	NF->setSubprogram(F->getSubprogram());
	F->setSubprogram(nullptr);

	// Recompute the parameter attributes list based on the new arguments for
	// the function.
	NF->setAttributes(AttributeList::get(F->getContext(), PAL.getFnAttrs(),
				PAL.getRetAttrs(), ArgAttrVec));

	F->getParent()->getFunctionList().insert(F->getIterator(), NF);

	// Transfer the name
	NF->takeName(F);

	return NF;
}

static Value* AssembleI64(Value* Low, Value* High, IRBuilder<>& Builder)
{
	Type* Int64Ty = IntegerType::get(Low->getContext(), 64);

	Low = Builder.CreateZExt(Low, Int64Ty, Twine(Low->getName(), ".zext"));
	High = Builder.CreateZExt(High, Int64Ty, Twine(High->getName(), ".zext"));
	High = Builder.CreateShl(High, 32, Twine(High->getName(), ".shl"));
	return Builder.CreateOr(Low, High, Twine(Low->getName(), ".").concat(Twine(High->getName(), ".i64")));
}

static std::pair<Value*, Value*> SplitI64(Value* V, IRBuilder<>& Builder)
{
	Type* Int32Ty = IntegerType::get(V->getContext(), 32);

	Value* Low = Builder.CreateTrunc(V, Int32Ty, Twine(V->getName(),".low"));
	Value* High = Builder.CreateLShr(V, 32, Twine(V->getName(),".highShl"));
	High = Builder.CreateTrunc(High, Int32Ty, Twine(V->getName(),".high"));

	return std::make_pair(Low, High);
}

void TypeOptimizer::rewriteFunction(Function* F)
{
	bool erased = pendingFunctions.erase(F);
	(void)erased;
	assert(erased);

	Type* Int32Ty = IntegerType::get(F->getContext(), 32);
	Type* Int64Ty = IntegerType::get(F->getContext(), 64);

	// Rewrite the type
	// Keep track of the original types of local instructions
	LocalTypeMapping localTypeMapping(globalTypeMapping);
	// Keep track of instructions which have been remapped
	LocalInstMapping localInstMapping(*this);
	auto it = globalsMapping.find(F);
	assert(it != globalsMapping.end());
	if(F->empty())
		return;
	bool keepI64 = onlyCalledByWasmFuncs.count(F) && !F->isVarArg();
	if (it->first != it->second)
	{
		Function* NF = cast<Function>(it->second);
		// Transfer the body
		NF->getBasicBlockList().splice(NF->begin(), F->getBasicBlockList());
		// Loop over the argument list, mapping the old arguments to
		// the new arguments, also transferring over the names as well.
		IRBuilder<> Builder(NF->getEntryBlock().getFirstNonPHI());
		for (auto A = F->arg_begin(), AE = F->arg_end(), NA = NF->arg_begin();A != AE; ++A, ++NA)
		{
			Value* New = nullptr;
			if (isI64ToRewrite(A->getType()) && !keepI64)
			{
				Value* Low = NA++;
				Value* High = NA;
				Low->setName(Twine(A->getName(), ".low"));
				High->setName(Twine(A->getName(), ".high"));
				Value* V = AssembleI64(Low, High, Builder);
				V->takeName(A);
				localInstMapping.setMappedOperand(A, V, 0);
				New = V;
			}
			else
			{
				NA->takeName(A);
				localInstMapping.setMappedOperand(A, NA, 0);
				New = NA;
			}
			if (New->getType() == A->getType())
			{
				A->replaceAllUsesWith(New);
			}
		}

		F = NF;
	}
	// Set the updated personality function
	if(F->hasPersonalityFn())
	{
		auto* Personality = rewriteConstant(F->getPersonalityFn(), false).first;
		F->setPersonalityFn(Personality);
	}


	SmallVector<BasicBlock*, 4> blocksInDFSOrder;
	std::unordered_set<BasicBlock*> usedBlocks;
	usedBlocks.insert(&F->getEntryBlock());
	blocksInDFSOrder.push_back(&F->getEntryBlock());
	// The size of the vector will increase over time, this is by design
	for(uint32_t i=0;i<blocksInDFSOrder.size();i++)
	{
		BasicBlock* BB = blocksInDFSOrder[i];
		Instruction* term = BB->getTerminator();
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

	bool wasm = F->getSection() == StringRef("asmjs") && LinearOutput == Wasm;

	SmallVector<PHINode*, 4> delayedPHIs;
	SmallVector<Instruction*, 4> InstsToDelete;
	// Rewrite instructions as needed
	for(BasicBlock* BB: blocksInDFSOrder)
	{
		auto BBI = BB->begin();
		while(BBI != BB->end())
		{
			Instruction& I = *BBI;
			++BBI;
			bool needsDefaultHandling = true;
			switch(I.getOpcode())
			{
				default:
					assert(!I.getType()->isPointerTy() && "Unexpected instruction in TypeOptimizer");
					break;
				case Instruction::Ret:
				{
					Value* Ret = cast<ReturnInst>(I).getReturnValue();
					if (!Ret || !isI64ToRewrite(Ret->getType()) || keepI64)
						break;

					IRBuilder<> Builder(&I);
					auto V = SplitI64(localInstMapping.getMappedOperand(Ret).first, Builder);
					Value* Low = V.first;
					Value* High = V.second;

					GlobalVariable* Sret = cast<GlobalVariable>(module->getOrInsertGlobal("cheerpSretSlot", Int32Ty));
					Builder.CreateStore(High, Sret);
					Builder.CreateRet(Low);
					// Since we are writing to global memory,
					// remove attributes that say otherwise
					F->setMemoryEffects(MemoryEffects::unknown());

					InstsToDelete.push_back(&I);
					needsDefaultHandling = false;
					break;
				}
				case Instruction::VAArg:
				{
					if(!isI64ToRewrite(I.getType()))
						break;

					VAArgInst& VA = cast<VAArgInst>(I);
					IRBuilder<> Builder(&VA);
					Value* Ptr = localInstMapping.getMappedOperand(VA.getPointerOperand()).first;
					Value* Low = Builder.CreateVAArg(Ptr, Int32Ty);
					Value* High = Builder.CreateVAArg(Ptr, Int32Ty);
					Value* V = AssembleI64(Low, High, Builder);

					localInstMapping.setMappedOperand(&I, V, 0);
					needsDefaultHandling = false;
					break;
				}
				case Instruction::GetElementPtr:
				{
					Value* ptrOperand = I.getOperand(0);
					Type* ptrType = localTypeMapping.getOriginalOperandType(ptrOperand);
					if (ptrType->isVectorTy())
						ptrType = ptrType->getScalarType();
					Type* newPtrType = rewriteType(ptrType);
					if(newPtrType != ptrType || rewriteType(I.getType()) != I.getType())
					{
						SmallVector<Value*, 4> newIndexes;
						Type* targetType = rewriteType(cast<GetElementPtrInst>(I).getResultElementType());
						SmallVector<Value*, 2> idxs;
						for (auto Op = I.op_begin()+1; Op != I.op_end(); ++Op)
						{
							idxs.push_back(localInstMapping.getMappedOperand(*Op).first);
						}
						uint8_t mergedIntegerOffset=rewriteGEPIndexes(newIndexes, ptrType, cast<GEPOperator>(&I)->getSourceElementType(), idxs, targetType, &I, I.getType());
						auto rewrittenOperand = localInstMapping.getMappedOperand(ptrOperand);
						if (auto A = dyn_cast<Argument>(rewrittenOperand.first))
						{
							assert(A->getParent() == F);
						}
						assert(rewrittenOperand.second == 0);
						Type* srcElementType = rewriteType(cast<GetElementPtrInst>(I).getSourceElementType());
						if (isa<ArrayType>(srcElementType))
							srcElementType = cast<ArrayType>(srcElementType)->getElementType();

						GetElementPtrInst* NewInst = GetElementPtrInst::Create(srcElementType, rewrittenOperand.first, newIndexes);
						assert(!NewInst->getResultElementType()->isArrayTy());
						NewInst->takeName(&I);
						NewInst->setIsInBounds(cast<GetElementPtrInst>(I).isInBounds());
						localInstMapping.setMappedOperand(&I, NewInst, mergedIntegerOffset);
						// We are done with handling this case
						needsDefaultHandling = false;
					}
					break;
				}
				case Instruction::Call:
				case Instruction::Invoke:
				{
					CallBase* CI=cast<CallBase>(&I);
					Function* calledFunction = dyn_cast<Function>(CI->getCalledOperand());
					bool isIntrinsic = calledFunction && calledFunction->isIntrinsic();
					// We need to handle special intrinsics here
					if(isIntrinsic)
					{
						if(calledFunction->getIntrinsicID() == Intrinsic::cheerp_upcast_collapsed)
						{
							// If the return type is not a struct anymore while the source type is still a
							// struct replace the upcast with a GEP
							Value* ptrOperand = I.getOperand(0);
							Type* curType = CI->getParamElementType(0);
							assert(curType);
							assertPointerElementOrOpaque(localTypeMapping.getOriginalOperandType(ptrOperand), curType);
							TypeMappingInfo newRetInfo = rewriteType(I.getType()->getPointerElementType());
							TypeMappingInfo newOpInfo = rewriteType(curType);
							if(TypeMappingInfo::isCollapsedStruct(newRetInfo.elementMappingKind) &&
								!TypeMappingInfo::isCollapsedStruct(newOpInfo.elementMappingKind))
							{
								Type* Int32 = IntegerType::get(I.getContext(), 32);
								Value* Zero = ConstantInt::get(Int32, 0);
								Value* Indexes[] = { Zero, Zero };
								auto rewrittenOperand = localInstMapping.getMappedOperand(ptrOperand);
								assert(rewrittenOperand.second == 0);
								Value* newPtrOperand = rewrittenOperand.first;
								Type* newType = GetElementPtrInst::getIndexedType(rewriteType(curType), Indexes);
								Value* newGEP = NULL;
								if(newType->isArrayTy())
								{
									Value* Indexes2[] = { Zero, Zero, Zero };
									newGEP = GetElementPtrInst::Create(newOpInfo.mappedType, newPtrOperand, Indexes2, "gepforupcast");
								}
								else
									newGEP = GetElementPtrInst::Create(newOpInfo.mappedType, newPtrOperand, Indexes, "gepforupcast");
								localInstMapping.setMappedOperand(&I, newGEP, 0);
								needsDefaultHandling = false;
								continue;
							}
						}
					}
					else
					{
						if(CI->hasByValArgument() || CI->hasStructRetAttr())
						{
							// We need to make sure that no byval attribute is applied to pointers to arrays
							// as they will be rewritten to plain pointers and less memory will be copied
							// Get the original type of the called function
							AttributeList newAttrs=CI->getAttributes();
							bool attributesChanged=false;
							for(uint32_t i=0;i<CI->arg_size();i++)
							{
								if(newAttrs.hasParamAttr(i, Attribute::ByVal))
								{
									Type* argType = localTypeMapping.getOriginalOperandType(CI->getArgOperand(i));
									assert(argType->isPointerTy());
									Type* rewrittenArgType = rewriteType(CI->getParamByValType(i));
									if(rewrittenArgType->isStructTy())
									{
										newAttrs = newAttrs.removeParamAttribute(module->getContext(), i, Attribute::ByVal);
										newAttrs = newAttrs.addParamAttribute(module->getContext(), i, Attribute::getWithByValType(F->getContext(), rewrittenArgType));
										attributesChanged = true;
										continue;
									}
									// The pointer is to an array, we need to make an explicit copy here
									// and remove the attribute unless the called function is known and the argument is readonly
									if(!calledFunction || !calledFunction->hasParamAttribute(i, Attribute::NoCapture))
									{
										IRBuilder<> Builder(CI);
										auto rewrittenOperand = localInstMapping.getMappedOperand(CI->getOperand(i));
										assert(rewrittenOperand.second==0);
										Value* mappedOp = rewrittenOperand.first;
										assertPointerElementOrOpaque(mappedOp->getType(), rewrittenArgType);
										assert(mappedOp->getType()->isPointerTy() &&
											!rewrittenArgType->isArrayTy());
										// 1) Create an alloca of the right type
										Value* byValCopy=Builder.CreateAlloca(rewrittenArgType, nullptr, "byvalcopy");
										byValCopy=Builder.CreateConstGEP2_32(rewrittenArgType, byValCopy, 0, 0);
										// 2) Create a mempcy
										Builder.CreateMemCpy(byValCopy, MaybeAlign(), mappedOp, MaybeAlign(), DL->getTypeAllocSize(rewrittenArgType),
													/*volatile*/false, nullptr, nullptr,
													nullptr, nullptr, llvm::IRBuilderBase::CheerpTypeInfo(rewrittenArgType));
										// 3) Replace the argument
										CI->setOperand(i, byValCopy);
									}
									// 4) Remove the byval attribute from the call
									newAttrs=newAttrs.removeParamAttribute(module->getContext(), i, Attribute::ByVal);
									attributesChanged = true;
								}
								if(newAttrs.hasParamAttr(i, Attribute::StructRet))
								{
									Type* argType = localTypeMapping.getOriginalOperandType(CI->getArgOperand(i));
									assert(argType->isPointerTy());
									Type* rewrittenArgType = rewriteType(CI->getParamStructRetType(i));
									newAttrs=newAttrs.removeParamAttribute(module->getContext(), i, Attribute::StructRet);
									attributesChanged = true;
									if(rewrittenArgType->isStructTy())
									{
										newAttrs = newAttrs.addParamAttribute(module->getContext(), i, Attribute::getWithStructRetType(F->getContext(), rewrittenArgType));
										continue;
									}
								}
							}
							if(attributesChanged)
								CI->setAttributes(newAttrs);
						}
					}
					Type* oldType = CI->getType();
					bool calleeOnlyCalledByWasm = onlyCalledByWasmFuncs.count(calledFunction);
					// NOTE: if the function is vararg, we can never keep i64s in the signature
					// for the vararg part. For now, we don't keep them entirely
					bool keepI64 = (isIntrinsic || calleeOnlyCalledByWasm) && !CI->getFunctionType()->isVarArg();
					FunctionType* rewrittenFuncType = rewriteFunctionType(CI->getFunctionType(), keepI64);
					bool needsRewrite = rewrittenFuncType != CI->getFunctionType();
					if (!needsRewrite && !keepI64 && CI->getFunctionType()->isVarArg())
					{
						for (auto& A: CI->args())
						{
							if (isI64ToRewrite(A->getType()))
							{
								needsRewrite = true;
								break;
							}
						}
					}
					if (needsRewrite)
					{
						SmallVector<Value *, 16> Args;
						AttributeList paramAttributes;
						const AttributeList &CallPAL = CI->getAttributes();

						// Loop over the operands, unpacking i64s into i32s when necessary.
						auto AI = CI->arg_begin();
						unsigned ArgNo = 0;
						unsigned nextArgNo = 0;
						for (auto AE = CI->arg_end(); AI != AE;
							++AI, ++ArgNo)
						{
							Value* Op = localInstMapping.getMappedOperand(*AI).first;
							if (isI64ToRewrite((*AI)->getType()) && !keepI64)
							{
								IRBuilder<> Builder(CI);
								auto V = SplitI64(Op, Builder);
								Value* Low = V.first;
								Value* High = V.second;
								Args.push_back(Low);
								Args.push_back(High);
								nextArgNo += 2;
							}
							else
							{
								Args.push_back(Op); // Unmodified argument
								for (auto attr : CallPAL.getParamAttrs(ArgNo))
								{
									if (attr.getKindAsEnum() == Attribute::ElementType)
									{
										llvm::Type* newType = rewriteType(attr.getValueAsType()).mappedType;
										if (newType->isArrayTy())
											newType = newType->getArrayElementType();
										Attribute newTypedAttr = attr.getWithNewType(CI->getContext(), newType);
										paramAttributes = paramAttributes.addAttributeAtIndex(CI->getContext(), nextArgNo+1, newTypedAttr);
									}
									else
									{
										//non-typed attributes do not require rewriting
										//ByVal and StructRet have been transformed above
										paramAttributes = paramAttributes.addAttributeAtIndex(CI->getContext(), nextArgNo+1, attr);
									}
								}
								nextArgNo += 1;
							}
						}
						AttrBuilder retAttributes(CI->getContext());
						for (auto attr: CallPAL.getRetAttrs())
						{
							if (attr.getKindAsEnum() == Attribute::ElementType)
							{
								llvm::Type* newType = rewriteType(attr.getValueAsType()).mappedType;
								if (newType->isArrayTy())
									newType = newType->getArrayElementType();
								Attribute newRetAttr = attr.getWithNewType(CI->getContext(), newType);
								retAttributes.addAttribute(newRetAttr);
							}
							else
							{
								retAttributes.addAttribute(attr);
							}
						}

						SmallVector<OperandBundleDef, 1> OpBundles;
						CI->getOperandBundlesAsDefs(OpBundles);
				
						Value* Callee = localInstMapping.getMappedOperand(CI->getCalledOperand()).first;

						CallBase *NewCall;
						if (auto* CallI = dyn_cast<CallInst>(CI))
						{
							auto* NC = CallInst::Create(rewrittenFuncType, Callee, Args, OpBundles, "", CI);
							NC->setTailCallKind(CallI->getTailCallKind());
							NewCall = NC;
						}
						else if (auto* InvI = dyn_cast<InvokeInst>(CI))
						{
							auto* NI = InvokeInst::Create(rewrittenFuncType, Callee, InvI->getNormalDest(), InvI->getUnwindDest(), Args, OpBundles, "", CI);
							NewCall = NI;
						}
						else
						{
							llvm_unreachable("unhandled CallBase derived class");
						}
						NewCall->setCallingConv(CI->getCallingConv());
						NewCall->setAttributes(AttributeList::get(F->getContext(), {
							AttributeList::get(F->getContext(), CallPAL.getFnAttrs(),
								AttributeSet::get(F->getContext(), retAttributes), {}),
							paramAttributes
							}));
						NewCall->setDebugLoc(CI->getDebugLoc());
						Args.clear();
				
						Value* Ret = NewCall;
						NewCall->copyMetadata(*CI);
						if (isI64ToRewrite(CI->getType()) && !keepI64)
						{
							GlobalVariable* Sret = cast<GlobalVariable>(module->getOrInsertGlobal("cheerpSretSlot", Int32Ty));
							IRBuilder<> Builder(CI);
							if(auto* Inv = dyn_cast<InvokeInst>(NewCall))
							{
								BasicBlock* BB = BasicBlock::Create(module->getContext());
								BB->setName("invokeRetI64");
								BB->insertInto(F);
								Inv->getNormalDest()->replacePhiUsesWith(Inv->getParent(), BB);
								Builder.SetInsertPoint(BB);
								Instruction* Term = Builder.CreateBr(Inv->getNormalDest());
								Inv->setNormalDest(BB);
								Builder.SetInsertPoint(Term);
							}
							Value* Low = Ret;
							Value* High = Builder.CreateLoad(Int32Ty, Sret);
							Ret = AssembleI64(Low, High, Builder);
							// Since we are reading from global memory,
							// remove attributes that say otherwise
							F->setMemoryEffects(MemoryEffects::unknown());
							// We have alreaded the return type, drop range info
							NewCall->setMetadata(LLVMContext::MD_range, nullptr);
						}
						localInstMapping.setMappedOperand(CI, Ret, 0);
						NewCall->takeName(CI);
						CI = NewCall;
					}
					if(CI->getType() != oldType)
						localTypeMapping.setOriginalOperandType(&I, oldType);
					needsDefaultHandling = false;
					break;
				}
				case Instruction::Store:
				{
					if(!I.getOperand(0)->getType()->isIntegerTy())
						break;
					auto mappedOperand = localInstMapping.getMappedOperand(I.getOperand(1));
					auto rewritteValue = localInstMapping.getMappedOperand(I.getOperand(0));
					assert(rewritteValue.second == 0);
					llvm::Value* mappedValue = rewritteValue.first;
					bool isVolatile = cast<StoreInst>(I).isVolatile();
					if(isI64ToRewrite(mappedValue->getType()))
					{
						IRBuilder<> Builder(&I);
						Value* Base = mappedOperand.first;
						StoreInst* orig = cast<StoreInst>(&I);
						if (wasm)
						{
							Value* BC = Builder.CreateBitCast(Base, Int64Ty->getPointerTo());
							Builder.CreateAlignedStore(mappedValue, BC, orig->getAlign(), isVolatile);
						}
						else
						{
							auto V = SplitI64(mappedValue, Builder);
							Value* Low = V.first;
							Value* High = V.second;
							Value* LowPtr = Builder.CreateConstInBoundsGEP1_32(Int32Ty, Base, 0);
							Value* HighPtr = Builder.CreateConstInBoundsGEP1_32(Int32Ty, Base, 1);
							Builder.CreateAlignedStore(Low, LowPtr, orig->getAlign(), isVolatile);
							Builder.CreateAlignedStore(High, HighPtr, std::min(orig->getAlign(), Align(4)), isVolatile);
						}
						InstsToDelete.push_back(&I);
						needsDefaultHandling = false;
						break;
					}
					llvm::Type* oldType = mappedValue->getType();
					bool isMergedPointer = mappedOperand.first->getType() != I.getOperand(1)->getType();
					if(!isMergedPointer)
						break;
					I.dropUnknownNonDebugMetadata();
					// We need to load, mask, insert and store
					llvm::Instruction* load = new LoadInst(mappedOperand.first->getType()->getPointerElementType(), mappedOperand.first, "mergedload", &I);
					// Compute a mask to preserve all the not-needed bits
					uint32_t maskVal = ((1<<(cast<IntegerType>(oldType)->getBitWidth()))-1);
					maskVal <<= mappedOperand.second;
					maskVal = ~maskVal;
					llvm::Instruction* mask = BinaryOperator::Create(Instruction::And, load, ConstantInt::get(cast<IntegerType>(load->getType()), maskVal), "mergedmask", &I);
					llvm::Instruction* extend = new ZExtInst(mappedValue, load->getType(), "mergedext", &I);
					if(mappedOperand.second)
						extend = BinaryOperator::Create(Instruction::Shl, extend, ConstantInt::get(cast<IntegerType>(extend->getType()), mappedOperand.second), "mergedshift", &I);
					llvm::Instruction* insert = BinaryOperator::Create(Instruction::Or, mask, extend, "mergedinsert", &I);
					I.setOperand(0, insert);
					needsDefaultHandling = false;
					break;
				}
				case Instruction::Load:
				{
					if(!I.getType()->isIntegerTy())
						break;
					auto mappedOperand = localInstMapping.getMappedOperand(I.getOperand(0));
					llvm::Type* oldType = I.getType();
					bool isVolatile = cast<LoadInst>(I).isVolatile();
					if(isI64ToRewrite(I.getType()))
					{
						IRBuilder<> Builder(&I);
						Value* Base = mappedOperand.first;
						Value* V = nullptr;
						LoadInst* orig = cast<LoadInst>(&I);
						if (wasm)
						{
							Value* BC = Builder.CreateBitCast(Base, Int64Ty->getPointerTo());
							V = Builder.CreateAlignedLoad(Int64Ty, BC, orig->getAlign(), isVolatile);
						}
						else
						{
							Value* LowPtr = Builder.CreateConstInBoundsGEP1_32(Int32Ty, Base, 0);
							Value* HighPtr = Builder.CreateConstInBoundsGEP1_32(Int32Ty, Base, 1);
							Value* Low = Builder.CreateAlignedLoad(Int32Ty, LowPtr, orig->getAlign(), isVolatile);
							Value* High = Builder.CreateAlignedLoad(Int32Ty, HighPtr, std::min(orig->getAlign(), Align(4)), isVolatile);
							V = AssembleI64(Low, High, Builder);
						}
						I.replaceAllUsesWith(V);
						InstsToDelete.push_back(&I);
						needsDefaultHandling = false;
						break;
					}
					bool isMergedPointer = mappedOperand.first->getType() != I.getOperand(0)->getType();
					if(!isMergedPointer)
						break;
					I.mutateType(mappedOperand.first->getType()->getPointerElementType());
					I.dropUnknownNonDebugMetadata();
					llvm::Instruction* mergedValue = &I;
					if(mappedOperand.second)
					{
						mergedValue = BinaryOperator::Create(Instruction::AShr, mergedValue,
							ConstantInt::get(cast<IntegerType>(I.getType()), mappedOperand.second), "mergedshift", mergedValue->getNextNode());
					}
					llvm::Value* truncated = new TruncInst(mergedValue, oldType, "mergedtrunc", mergedValue->getNextNode());
					localInstMapping.setMappedOperand(&I, truncated, 0);
					needsDefaultHandling = false;
					break;
				}
				case Instruction::Alloca:
				{
					AllocaInst* AI = cast<AllocaInst>(&I);
					TypeMappingInfo newInfo = rewriteType(I.getType());
					if(newInfo.mappedType!=I.getType())
					{
						Type* newAllocatedType = rewriteType(AI->getAllocatedType());
						AI->setAllocatedType(newAllocatedType);
						localTypeMapping.setOriginalOperandType(&I, I.getType());
						// Special handling for Alloca
						if(newInfo.elementMappingKind == TypeMappingInfo::POINTER_FROM_ARRAY)
						{
							// In this case we need to rewrite the allocated type and use that directly
							// Moreover, we need to generate a GEP that will be used instead of this alloca
							Type* newPtrType = PointerType::get(newAllocatedType, AI->getAddressSpace());
							I.mutateType(newPtrType);
							Type* Int32 = IntegerType::get(I.getType()->getContext(), 32);
							Value* Zero = ConstantInt::get(Int32, 0);
							Value* Indexes[] = { Zero, Zero };
							Instruction* newGEP = GetElementPtrInst::Create(newAllocatedType, &I, Indexes, "allocadecay");
							localInstMapping.setMappedOperand(&I, newGEP, 0);
						}
						else
							I.mutateType(newInfo.mappedType);
					}
					needsDefaultHandling = false;
					break;
				}
				case Instruction::BitCast:
				case Instruction::AddrSpaceCast:
				case Instruction::ExtractValue:
				case Instruction::InsertValue:
				case Instruction::IntToPtr:
				case Instruction::PHI:
				case Instruction::Select:
				case Instruction::LandingPad:
				case Instruction::Resume:
				case Instruction::Freeze:
				case Instruction::ExtractElement:
				case Instruction::InsertElement:
					break;
			}
			if(needsDefaultHandling && !I.getType()->isVoidTy() && !isI64ToRewrite(I.getType()))
			{
				TypeMappingInfo newInfo = rewriteType(I.getType());
				if(newInfo.mappedType!=I.getType())
				{
					localTypeMapping.setOriginalOperandType(&I, I.getType());
					I.mutateType(newInfo.mappedType);
				}
			}
			// We need to handle PHI operands later on, when all instructions are redefined
			if(PHINode* phi = dyn_cast<PHINode>(&I))
			{
				delayedPHIs.push_back(phi);
				continue;
			}
			for(uint32_t i=0;i<I.getNumOperands();i++)
			{
				Value* op=I.getOperand(i);
				auto rewrittenOperand = localInstMapping.getMappedOperand(op).first;
				if (auto A = dyn_cast<Argument>(rewrittenOperand))
				{
					assert(A->getParent() == F);
				}
				I.setOperand(i, rewrittenOperand);
			}
		}
	}
	for(Instruction* I: InstsToDelete)
	{
		I->eraseFromParent();
	}
	for(PHINode* phi: delayedPHIs)
	{
		for(uint32_t i=0;i<phi->getNumIncomingValues();i++)
		{
			Value* op=phi->getIncomingValue(i);
			auto rewrittenOperand = localInstMapping.getMappedOperand(op);
			assert(rewrittenOperand.second == 0);
			// Work around setIncomingValue assertion by using setOperand directly
			phi->setOperand(i, rewrittenOperand.first);
		}
	}
	for(auto it: localInstMapping)
	{
		// Transfer metadata info to the mapped value
		if (auto M = ValueAsMetadata::getIfExists(it.first))
		{
			if (auto *MDV = MetadataAsValue::getIfExists(it.first->getContext(), M))
			{
				auto NM = ValueAsMetadata::get(it.second.first);
				auto NMDV = MetadataAsValue::get(it.first->getContext(), NM);
				SmallVector<Use*, 4> Uses;
				for (auto& U: MDV->uses())
				{
					Uses.push_back(&U);
				}
				for (auto U: Uses)
				{
					U->set(NMDV);
				}
				MDV->deleteValue();
			}
		}
		if (!isa<Instruction>(it.second.first))
			continue;
		if (!isa<Instruction>(it.first))
			continue;
		Instruction* oldI = cast<Instruction>(it.first);
		Instruction* newI = cast<Instruction>(it.second.first);
		// Insert new instruction, if necessary
		if(!newI->getParent())
			newI->insertAfter(oldI);
		// Alloca are only replaced for POINTER_FROM_ARRAY, and should not be removed
		// Loads are replaced when merged integers, and should not be removed
		if(isa<AllocaInst>(oldI) || isa<LoadInst>(oldI))
			continue;
		// Delete old instructions
		oldI->replaceAllUsesWith(UndefValue::get(it.first->getType()));
		oldI->eraseFromParent();
	}
	assert(!verifyFunction(*F, &errs()));
}

Constant* TypeOptimizer::rewriteGlobal(GlobalVariable* GV)
{
	TypeMappingInfo newInfo = rewriteType(GV->getType());
	globalTypeMapping[GV] = GV->getType();
	if(GV->getType()==newInfo.mappedType)
	{
		assert(!GV->getValueType()->isArrayTy());
		return GV;
	}
	if(newInfo.elementMappingKind == TypeMappingInfo::POINTER_FROM_ARRAY)
	{
		Type* newAllocatedType = rewriteType(GV->getValueType());
		Type* newPtrType = PointerType::get(newAllocatedType, GV->getAddressSpace());
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
	// We need to change type, so we have to forge a new initializer
	auto rewrittenInit = rewriteConstant(GV->getInitializer(), true);
	assert(rewrittenInit.second==0);
	GV->setInitializer(rewrittenInit.first);
}

bool TypeOptimizer::runOnModule(Module& M)
{
	// Get required auxiliary data
	module = &M;
	DL = &M.getDataLayout();
	assert(DL);
	// Do a preprocessing step to gather data that we can't get online
	gatherAllTypesInfo(M);
	std::vector<Function*> originalFuncs;
	// Queue the functions for updating
	for(Function& F: M)
	{
		pendingFunctions.insert(&F);
	}
	// Update function signatures
	for(Function* F: pendingFunctions)
	{
		Type* oldType = F->getType();
		Function* NF = rewriteFunctionSignature(F);
		globalsMapping.insert(std::make_pair(F, NF));
		if (oldType != NF->getType())
			globalTypeMapping.insert(std::make_pair(NF, oldType));
	}
	// Update the type for all global variables
	for(GlobalVariable& GV: M.getGlobalList())
	{
		Constant* rewrittenGlobal=rewriteGlobal(&GV);
		globalsMapping.insert(std::make_pair(&GV, rewrittenGlobal));
	}
	// Update aliases
	for(GlobalAlias& GA: M.getAliasList())
	{
		Type* rewrittenType = rewriteType(GA.getType());
		GA.mutateType(rewrittenType);
		GA.setValueType(rewriteType(GA.getValueType()));
		GA.setAliasee(rewriteConstant(GA.getAliasee(), false).first);
	}
	// Rewrite all functions
	while(!pendingFunctions.empty())
		rewriteFunction(*pendingFunctions.begin());
	// Now that all functions are fixes, update the global initializer
	for(GlobalVariable& GV: M.getGlobalList())
	{
		GV.setValueType(rewriteType(GV.getValueType()));
		rewriteGlobalInit(&GV);
	}
	for(auto it: globalsMapping)
	{
		if (Function* F = dyn_cast<Function>(it.first))
		{
			Function* New = cast<Function>(it.second);
			if (F != New)
			{
				F->replaceNonMetadataUsesWith(UndefValue::get(F->getType()));
				F->mutateType(New->getType());
				F->setValueType(New->getValueType());
				F->replaceAllUsesWith(New);
				F->eraseFromParent();
			}
		}
	}
	assert(!verifyModule(*module, &errs()));
	module = NULL;
	return true;
}

PreservedAnalyses TypeOptimizerPass::run(Module &M, ModuleAnalysisManager&)
{
	TypeOptimizer TO;
	if (TO.runOnModule(M))
		return PreservedAnalyses::none();
	return PreservedAnalyses::all();
}

}
