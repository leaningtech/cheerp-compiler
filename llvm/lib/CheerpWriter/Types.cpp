//===-- Types.cpp - The Cheerp JavaScript generator -----------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2015 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/Writer.h"
#include "llvm/Cheerp/Utility.h"
#include <stdio.h>

using namespace cheerp;
using namespace llvm;

void CheerpWriter::compileTypedArrayType(Type* t)
{
	if(t->isIntegerTy(8))
		stream << "Uint8Array";
	else if(t->isIntegerTy(16))
		stream << "Uint16Array";
	else if(t->isIntegerTy(32))
		stream << "Int32Array";
	else if(t->isFloatTy())
		stream << "Float32Array";
	else if(t->isDoubleTy())
		stream << "Float64Array";
	else
	{
		llvm::errs() << "Typed array requested for type " << *t << "\n";
		llvm::report_fatal_error("Unsupported code found, please report a bug", false);
	}
}

void CheerpWriter::compileSimpleType(Type* t, llvm::Value* init)
{
	assert(TypeSupport::isSimpleType(t, forceTypedArrays));
	switch(t->getTypeID())
	{
		case Type::IntegerTyID:
		{
			//We only really have 32bit integers.
			//We will allow anything shorter.
			if(init)
				compileOperand(init);
			else
			{
				//Print out a '0' to let the engine know this is an integer.
				stream << '0';
			}
			break;
		}
		case Type::FloatTyID:
		{
			if(useMathFround)
			{
				stream << namegen.getBuiltinName(NameGenerator::Builtin::FROUND) << "(";
				if(init)
					compileOperand(init);
				else
					stream << "0.";
				stream << ")";
				break;
			}
		}
		case Type::DoubleTyID:
		{
			if(init)
				compileOperand(init);
			else
			{
				// NOTE: V8 requires the `.` to identify it as a double in asm.js
				stream << "-0.";
			}
			break;
		}
		case Type::PointerTyID:
		{
			if(init)
			{
				compilePointerAs(init, PA.getPointerKindForStoredType(t));
			}
			else
			{
				if(PA.getPointerKindForStoredType(t)==COMPLETE_OBJECT)
					stream << "null";
				else
					stream << "nullObj";
			}
			break;
		}
		case Type::StructTyID:
		{
			assert(init == nullptr);
			assert(TypeSupport::hasByteLayout(t));
			uint32_t typeSize = targetData.getTypeAllocSize(t);
			stream << "new DataView(new ArrayBuffer(";
			// Round up the size to make sure that any typed array can be initialized from the buffer
			stream << ((typeSize + 7) & (~7));
			stream << "))";
			break;
		}
		case Type::ArrayTyID:
		{
			assert(init == nullptr);
			if(TypeSupport::hasByteLayout(t))
			{
				uint32_t typeSize = targetData.getTypeAllocSize(t);
				stream << "new DataView(new ArrayBuffer(";
				// Round up the size to make sure that any typed array can be initialized from the buffer
				stream << ((typeSize + 7) & (~7));
				stream << "))";
			}
			else
			{
				ArrayType* at=cast<ArrayType>(t);
				Type* et=at->getElementType();
				assert(types.isTypedArrayType(et, forceTypedArrays) && at->getNumElements()>1);
				stream << "new ";
				compileTypedArrayType(et);
				stream << '(' << at->getNumElements() << ')';
			}
			break;
		}
		default:
			assert(false);
	}
}

uint32_t CheerpWriter::compileComplexType(Type* t, COMPILE_TYPE_STYLE style, StringRef varName, uint32_t maxDepth, uint32_t totalLiteralProperties,
						const AllocaStoresExtractor::OffsetToValueMap* offsetToValueMap, uint32_t offset, uint32_t& usedValuesFromMap)
{
	assert(!TypeSupport::isSimpleType(t, forceTypedArrays));
	// Handle complex arrays and objects, they are all literals in JS
	assert(t->getTypeID() == Type::StructTyID || t->getTypeID() == Type::ArrayTyID);

	bool useVarName = !varName.empty() && style == LITERAL_OBJ;

	// We only need to split large objects with the LITERAL_OBJ style
	uint32_t numElements = 0;
	if(StructType* ST = dyn_cast<StructType>(t))
	{
		numElements = ST->getNumElements();
		if(numElements > V8MaxLiteralProperties && style!=THIS_OBJ)
		{
			assert(globalDeps.classesUsed().count(cast<StructType>(t)));
			// This is a big object, call the constructor and be done with it
			stream << "new ";
			stream << namegen.getConstructorName(t) << "()";
			return 0;
		}
	}
	else if(ArrayType* AT = dyn_cast<ArrayType>(t))
	{
		// Basic elements, such as numbers and pointers (including nullObj) do not count. Structs and array do.
		if(AT->getElementType()->isStructTy() || AT->getElementType()->isArrayTy())
			numElements = AT->getNumElements();
	}
	bool shouldReturnElementsCount = true;

	if(useVarName && (maxDepth == 0 || ((totalLiteralProperties + numElements) > V8MaxLiteralProperties)))
	{
		// If this struct have more than V8MaxLiteralProperties there is no point in splitting it anyway
		if(numElements <= V8MaxLiteralProperties)
			stream << varName << '=';
		maxDepth = V8MaxLiteralDepth;
		shouldReturnElementsCount = false;
		totalLiteralProperties = 0;
	}

	uint32_t nextMaxDepth = useVarName ? maxDepth - 1 : maxDepth;
	if (StructType* st = dyn_cast<StructType>(t))
	{
		assert(!TypeSupport::hasByteLayout(st));
		StructType* downcastArrayBase = globalDeps.needsDowncastArray(st);
		bool addDowncastArray = downcastArrayBase != NULL;
		if(style == LITERAL_OBJ)
		{
			if(addDowncastArray)
			{
				stream << namegen.getClassName(downcastArrayBase) << '(';
			}
			stream << '{';
		}
		const StructLayout* SL = targetData.getStructLayout( st );
		for(uint32_t i=0;i<st->getNumElements();i++)
		{
			Type* element = st->getElementType(i);
			if(i!=0)
			{
				if(style==THIS_OBJ)
					stream << ';' << NewLine;
				else
					stream << ',';
			}
			if(style==THIS_OBJ)
				stream << "this.";
			stream << types.getPrefixCharForMember(PA, st, i) << i;
			if(style==THIS_OBJ)
				stream << '=';
			else
				stream << ':';
			// Create a wrapper array for all members which require REGULAR pointers, if they are not already covered by the downcast array
			TypeAndIndex baseAndIndex(st, i, TypeAndIndex::STRUCT_MEMBER);
			bool restoreMaxDepth = false;
			bool useWrapperArray = types.useWrapperArrayForMember(PA, st, i);
			if (useWrapperArray)
			{
				// We need to do an extra check to break deep literals here
				if(useVarName)
				{
					if(nextMaxDepth == 0)
					{
						stream << varName << '=';
						nextMaxDepth = V8MaxLiteralDepth;
					}
					else
					{
						nextMaxDepth--;
						restoreMaxDepth=true;
						if(element->isStructTy())
							numElements++;
					}
				}
				stream << '[';
			}
			llvm::Value* init = nullptr;
			uint32_t totalOffset = offset + SL->getElementOffset(i);
			if(offsetToValueMap)
			{
				auto it = offsetToValueMap->find(totalOffset);
				if(it != offsetToValueMap->end())
					init = it->second;
			}
			if (element->isPointerTy())
			{
				if(init)
					usedValuesFromMap++;
				POINTER_KIND memberPointerKind = PA.getPointerKindForMemberPointer(baseAndIndex);
				bool hasConstantOffset = PA.getConstantOffsetForMember(baseAndIndex);
				if((memberPointerKind == REGULAR || memberPointerKind == SPLIT_REGULAR) && hasConstantOffset)
				{
					if(init)
						compilePointerBase(init);
					else
						stream << "nullArray";
				}
				else if (memberPointerKind == SPLIT_REGULAR)
				{
					if(init)
						compilePointerBase(init);
					else
						stream << "nullArray";
					if(style==THIS_OBJ)
						stream << ';' << NewLine << "this.";
					else
						stream << ',';
					stream << types.getPrefixCharForMember(PA, st, i) << i << 'o';
					if(style==THIS_OBJ)
						stream << '=';
					else
						stream << ':';
					if(init)
						compilePointerOffset(init, HIGHEST);
					else
						stream << '0';
					// FIXME: The offset member is not taken into account when deciding if the new-based constructor is required
					// so in rare cases (when the added element makes the struct larger than 8 elements) the slow literal runtime
					// call will be used on V8.
					numElements++;
				}
				else if (init)
					compilePointerAs(init, memberPointerKind);
				else if (memberPointerKind == COMPLETE_OBJECT)
					stream << "null";
				else
					stream << "nullObj";
			}
			else if(TypeSupport::isSimpleType(element, forceTypedArrays))
			{
				if(init)
					usedValuesFromMap++;
				compileSimpleType(element, init);
			}
			else if(style == THIS_OBJ)
				compileComplexType(element, LITERAL_OBJ, varName, nextMaxDepth, 0, offsetToValueMap, totalOffset, usedValuesFromMap);
			else
				numElements += compileComplexType(element, LITERAL_OBJ, varName, nextMaxDepth, totalLiteralProperties + numElements, offsetToValueMap, totalOffset, usedValuesFromMap);
			if(useWrapperArray)
			{
				if(restoreMaxDepth)
					nextMaxDepth++;
				stream << ']';
			}
		}
		if(style == LITERAL_OBJ)
		{
			stream << '}';
			if(addDowncastArray)
				stream << ')';
		}
		else if(style == THIS_OBJ)
		{
			stream << ';' << NewLine;
			if(addDowncastArray)
			{
				stream << namegen.getClassName(downcastArrayBase) << "(this)";
			}
		}
	}
	else
	{
		assert(style == LITERAL_OBJ);
		ArrayType* at=cast<ArrayType>(t);
		Type* element = at->getElementType();
		assert(!(types.isTypedArrayType(element, forceTypedArrays) && at->getNumElements()>1));
		// Work around V8 limits on literal array larger than 8 elements
		if(at->getNumElements() > 8)
		{
			if(element->isPointerTy())
			{
				assert( globalDeps.needCreatePointerArray() );
				stream << namegen.getBuiltinName(NameGenerator::Builtin::CREATE_POINTER_ARRAY) << "([],0," << at->getNumElements();
				stream << ',';
				if(PA.getPointerKindForStoredType(element)==COMPLETE_OBJECT)
					stream << "null";
				else
					stream << "nullObj";
				stream << ')';
			}
			else
			{
				assert( globalDeps.dynAllocArrays().count(element) );
				stream <<  namegen.getArrayName(element) << "([],0," << at->getNumElements() << ')';
			}
		}
		else
		{
			stream << '[';
			for(uint64_t i=0;i<at->getNumElements();i++)
			{
				if(i!=0)
					stream << ',';
				uint32_t elementSize = targetData.getTypeAllocSize(at->getElementType());
				uint32_t totalOffset = offset + elementSize * i;
				if(TypeSupport::isSimpleType(element, forceTypedArrays))
				{
					llvm::Value* init = nullptr;
					if(offsetToValueMap)
					{
						auto it = offsetToValueMap->find(totalOffset);
						if(it != offsetToValueMap->end())
							init = it->second;
					}
					if(init)
						usedValuesFromMap++;
					compileSimpleType(element, init);
				}
				else
					numElements += compileComplexType(element, LITERAL_OBJ, varName, nextMaxDepth, totalLiteralProperties + numElements, offsetToValueMap, totalOffset, usedValuesFromMap);
			}
			stream << ']';
		}
	}
	return shouldReturnElementsCount ? numElements : 0;
}

void CheerpWriter::compileType(Type* t, COMPILE_TYPE_STYLE style, StringRef varName, const AllocaStoresExtractor::OffsetToValueMap* offsetToValueMap)
{
	if(style == LITERAL_OBJ && isa<StructType>(t) && TypeSupport::isJSExportedType(cast<StructType>(t), module))
	{
		assert(offsetToValueMap == nullptr);
		StringRef mangledName = t->getStructName().drop_front(6);
		demangler_iterator demangler( mangledName );
		StringRef jsClassName = *demangler++;

		// Special argument for only allocating the object, without calling the
		// C++ constructor
		stream << "new " << jsClassName << "(undefined)";
	}
	else if(TypeSupport::isSimpleType(t, forceTypedArrays))
	{
		llvm::Value* init = nullptr;
		if(offsetToValueMap)
		{
			assert(offsetToValueMap->size() == 1);
			auto it = offsetToValueMap->find(0);
			if(it != offsetToValueMap->end())
				init = it->second;
		}
		compileSimpleType(t, init);
	}
	else
	{
		uint32_t usedValuesFromMap = 0;
		compileComplexType(t, style, varName, V8MaxLiteralDepth, 0, offsetToValueMap, 0, usedValuesFromMap);
		if(offsetToValueMap)
			assert(offsetToValueMap->size() == usedValuesFromMap);
	}
}

uint32_t CheerpWriter::compileClassTypeRecursive(const std::string& baseName, StructType* currentType, uint32_t baseCount)
{
	if(currentType->getDirectBase())
	{
		baseCount=compileClassTypeRecursive(baseName,currentType->getDirectBase(),baseCount);
		if(!TypeSupport::hasBasesInfoMetadata(currentType, module))
			return baseCount;
	}
	else
	{
		stream << "a[" << baseCount << "]=" << baseName << ';' << NewLine;
		stream << baseName << ".o=" << baseCount << ';' << NewLine;
		stream << baseName << ".a=a;" << NewLine;
		baseCount++;
	}

	uint32_t firstBase, localBaseCount;
	if(!types.getBasesInfo(currentType, firstBase, localBaseCount))
		return baseCount;
	//baseCount has been already incremented above

	for(uint32_t i=firstBase;i<(firstBase+localBaseCount);i++)
	{
		if(!currentType->getElementType(i)->isStructTy())
			continue;
		SmallString<16> buf;
		llvm::raw_svector_ostream bufStream(buf);
		bufStream << ".a" << i;
		bufStream.flush();
		baseCount=compileClassTypeRecursive(baseName+buf.c_str(), cast<StructType>(currentType->getElementType(i)), baseCount);
	}
	return baseCount;
}

void CheerpWriter::compileClassConstructor(StructType* T)
{
	assert(T->getNumElements() > V8MaxLiteralProperties);
	stream << "function ";
	stream << namegen.getConstructorName(T) << "(){" << NewLine;
	uint32_t usedValuesFromMap;
	compileComplexType(T, THIS_OBJ, "aSlot", V8MaxLiteralDepth, 0, nullptr, 0, usedValuesFromMap);
	stream << '}' << NewLine;
}

void CheerpWriter::compileClassType(StructType* T)
{
	if(!T->hasName())
	{
		llvm::errs() << "Expected name for struct " << *T << "\n";
		llvm::report_fatal_error("Unsupported code found, please report a bug", false);
		return;
	}
	stream << "function " << namegen.getClassName(T) << "(obj){" << NewLine;

	stream << "var a=[];" << NewLine;
	compileClassTypeRecursive("obj", T, 0);
	stream << "return obj;}" << NewLine;
}

void CheerpWriter::compileArrayClassType(Type* T)
{
	stream << "function ";
	stream << namegen.getArrayName(T);
	stream << "(ret,start,end){" << NewLine;
	stream << "for(var __i__=start;__i__<end;__i__++)" << NewLine;
	stream << "ret[__i__]=";
	compileType(T, LITERAL_OBJ, "ret[__i__]");
	stream << ';' << NewLine << "return ret;" << NewLine << '}' << NewLine;
}

void CheerpWriter::compileArrayPointerType()
{
	stream << "function " << namegen.getBuiltinName(NameGenerator::Builtin::CREATE_POINTER_ARRAY) << "(ret,start,end,elem){for(var __i__=start;__i__<end;__i__++)ret[__i__]=elem;return ret;}"
		<< NewLine;
}

bool CheerpWriter::needsUnsignedTruncation(std::unordered_set<const llvm::Value*> visited, const Value* v) const
{
	if(!v->getType()->isIntegerTy(8) && !v->getType()->isIntegerTy(16))
		return true;
	if(!visited.insert(v).second)
		return false;
	if(isa<ConstantInt>(v))
	{
		// Constants are compiled as zero extended
		return false;
	}
	else if(const LoadInst* LI = dyn_cast<LoadInst>(v))
	{
		Value* ptr = LI->getOperand(0);
		if(isGEP(ptr))
		{
			uint32_t numOp = cast<User>(ptr)->getNumOperands();
			if(numOp > 2)
			{
				Type* containerType = getGEPContainerType(cast<User>(ptr));
				// 1 element arrays are represented as normal JS arrays, but longer arrays are always typed arrays
				// i8 and i16 typed arrays are unsigned, so we don't need the truncation
				bool comesFromTypedArray = isa<ArrayType>(containerType) && cast<ArrayType>(containerType)->getNumElements() > 1;
				return !comesFromTypedArray;
			}
			else
			{
				ConstantInt* lastOperand = dyn_cast<ConstantInt>(cast<User>(ptr)->getOperand(numOp-1));
				return !(lastOperand && lastOperand->getSExtValue() > 0);
			}
		}
	}
	else if(const PHINode* phi = dyn_cast<PHINode>(v))
	{
		for(uint32_t i=0;i<phi->getNumIncomingValues();i++)
		{
			const Value* incoming = phi->getIncomingValue(i);
			if(needsUnsignedTruncation(visited, incoming))
				return true;
		}
		return false;
	}
	else if(const Instruction* I = dyn_cast<Instruction>(v))
	{
		if(I->getOpcode() == Instruction::And)
		{
			return needsUnsignedTruncation(visited, I->getOperand(0)) && needsUnsignedTruncation(visited, I->getOperand(1));
		}
		else if(I->getOpcode() == Instruction::Xor || I->getOpcode() == Instruction::Or)
		{
			return needsUnsignedTruncation(visited, I->getOperand(0)) || needsUnsignedTruncation(visited, I->getOperand(1));
		}
		else if(I->getOpcode() == Instruction::Select)
		{
			return needsUnsignedTruncation(visited, I->getOperand(1)) || needsUnsignedTruncation(visited, I->getOperand(2));
		}
		else if(I->getOpcode() == Instruction::ZExt || I->getOpcode() == Instruction::LShr)
			return false;
	}
	return true;
}

bool CheerpWriter::needsUnsignedTruncation(const Value* v) const
{
	std::unordered_set<const llvm::Value*> visited;
	return needsUnsignedTruncation(visited, v);
}
