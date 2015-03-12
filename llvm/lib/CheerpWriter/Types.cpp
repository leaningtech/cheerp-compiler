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

void CheerpWriter::compileSimpleType(Type* t)
{
	assert(TypeSupport::isSimpleType(t));
	switch(t->getTypeID())
	{
		case Type::IntegerTyID:
		{
			//We only really have 32bit integers.
			//We will allow anything shorter.
			//Print out a '0' to let the engine know this is an integer.
			stream << '0';
			break;
		}
		case Type::FloatTyID:
		case Type::DoubleTyID:
		{
			stream << '0';
			break;
		}
		case Type::PointerTyID:
		{
			if(PA.getPointerKindForStoredType(t)==COMPLETE_OBJECT)
				stream << "nullRef";
			else
				stream << "nullObj";
			break;
		}
		case Type::StructTyID:
		{
			assert(TypeSupport::hasByteLayout(t));
			uint32_t typeSize = targetData.getTypeAllocSize(t);
			stream << "new DataView(new ArrayBuffer(";
			stream << typeSize;
			stream << "))";
			break;
		}
		case Type::ArrayTyID:
		{
			ArrayType* at=cast<ArrayType>(t);
			Type* et=at->getElementType();
			assert(types.isTypedArrayType(et) && at->getNumElements()>1);
			stream << "new ";
			compileTypedArrayType(et);
			stream << '(' << at->getNumElements() << ')';
			break;
		}
		default:
			assert(false);
	}
}

void CheerpWriter::compileComplexType(Type* t, COMPILE_TYPE_STYLE style)
{
	assert(!TypeSupport::isSimpleType(t));
	assert(t->getTypeID() == Type::StructTyID || t->getTypeID() == Type::ArrayTyID);

	if (StructType* st = dyn_cast<StructType>(t))
	{
		assert(!TypeSupport::hasByteLayout(st));
		bool addDowncastArray = types.hasBasesInfo(t);
		if(style == LITERAL_OBJ)
		{
			if(addDowncastArray)
				stream << "create" << namegen.getTypeName(cast<StructType>(t)) << '(';
			stream << '{';
		}
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
			bool useWrapperArray = types.useWrapperArrayForMember(PA, st, i);
			if (useWrapperArray)
				stream << '[';
			if (element->isPointerTy())
			{
				if (PA.getPointerKindForMemberPointer(baseAndIndex)==COMPLETE_OBJECT)
					stream << "nullRef";
				else if (PA.getConstantOffsetForMember(baseAndIndex))
					stream << "nullArray";
				else
					stream << "nullObj";
			}
			else
				compileType(element, LITERAL_OBJ);
			if(useWrapperArray)
				stream << ']';
		}
		if(style == LITERAL_OBJ)
		{
			stream << '}';
			if(addDowncastArray)
				stream << ')';
		}
		else if(addDowncastArray)
		{
			assert(style == THIS_OBJ);
			stream << "create" << namegen.getTypeName(cast<StructType>(t)) << "(this)";
		}
	}
	else
	{
		assert(style == LITERAL_OBJ);
		ArrayType* at=cast<ArrayType>(t);
		Type* element = at->getElementType();
		assert(!(types.isTypedArrayType(element) && at->getNumElements()>1));
		stream << '[';
		for(uint64_t i=0;i<at->getNumElements();i++)
		{
			if(i!=0)
				stream << ',';
			compileType(at->getElementType(), LITERAL_OBJ);
		}
		stream << ']';
	}
}

void CheerpWriter::compileType(Type* t, COMPILE_TYPE_STYLE style)
{
	if(TypeSupport::isSimpleType(t))
		compileSimpleType(t);
	else
		compileComplexType(t, style);
}

uint32_t CheerpWriter::compileClassTypeRecursive(const std::string& baseName, StructType* currentType, uint32_t baseCount)
{
	stream << "a[" << baseCount << "]=" << baseName << ';' << NewLine;
	stream << baseName << ".o=" << baseCount << ';' << NewLine;
	stream << baseName << ".a=a;" << NewLine;
	baseCount++;

	uint32_t firstBase, localBaseCount;
	if(!types.getBasesInfo(currentType, firstBase, localBaseCount))
		return baseCount;
	//baseCount has been already incremented above

	for(uint32_t i=firstBase;i<(firstBase+localBaseCount);i++)
	{
		SmallString<16> buf;
		llvm::raw_svector_ostream bufStream(buf);
		bufStream << ".a" << i;
		bufStream.flush();
		baseCount=compileClassTypeRecursive(baseName+buf.c_str(), cast<StructType>(currentType->getElementType(i)), baseCount);
	}
	return baseCount;
}

void CheerpWriter::compileClassType(StructType* T)
{
	if(!T->hasName())
	{
		llvm::errs() << "Expected name for struct " << *T << "\n";
		llvm::report_fatal_error("Unsupported code found, please report a bug", false);
		return;
	}
	//This function is used as a constructor using the new syntax
	stream << "function create" << namegen.filterLLVMName(T->getName(), NameGenerator::GLOBAL) << "(obj){" << NewLine;

	NamedMDNode* basesNamedMeta=module.getNamedMetadata(Twine(T->getName(),"_bases"));
	assert(basesNamedMeta);
	MDNode* basesMeta=basesNamedMeta->getOperand(0);
	assert(basesMeta->getNumOperands()==2);
	uint32_t baseMax=getIntFromValue(cast<ConstantAsMetadata>(basesMeta->getOperand(1))->getValue());
	stream << "var a=new Array(" << baseMax << ");" << NewLine;
	compileClassTypeRecursive("obj", T, 0);
	stream << "return obj;}" << NewLine;
}

void CheerpWriter::compileArrayClassType(StructType* T)
{
	if(!T->hasName())
	{
		llvm::errs() << "Expected name for struct " << *T << "\n";
		llvm::report_fatal_error("Unsupported code found, please report a bug", false);
		return;
	}
	stream << "function createArray";
	stream << namegen.getTypeName(T);
	stream << "(ret,start){" << NewLine;
	stream << "for(var __i__=start;__i__<ret.length;__i__++)" << NewLine;
	stream << "ret[__i__]=";
	compileType(T, LITERAL_OBJ);
	stream << ';' << NewLine << "return ret;" << NewLine << '}' << NewLine;
}

void CheerpWriter::compileArrayPointerType()
{
	stream << "function createPointerArray(ret, start) { for(var __i__=start;__i__<ret.length;__i__++) ret[__i__]={ d: nullArray, o: 0}; return ret; }"
		<< NewLine;
}

