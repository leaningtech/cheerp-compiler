//===-- Types.cpp - The Cheerp JavaScript generator -----------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2014 Leaning Technologies
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

void CheerpWriter::compileTypeImpl(Type* t, COMPILE_TYPE_STYLE style)
{
	switch(t->getTypeID())
	{
		case Type::IntegerTyID:
		{
			//We only really have 32bit integers.
			//We will allow anything shorter.
			//NOTE: Only bit operations are allowed on shorter types
			//this is enforced on a per-operation basis
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
		case Type::StructTyID:
		{
			//Special case union first
			if(TypeSupport::isUnion(t))
			{
				uint32_t typeSize = targetData.getTypeAllocSize(t);
				stream << "new DataView(new ArrayBuffer(";
				stream << typeSize;
				stream << "))";
				break;
			}
			StructType* st=static_cast<StructType*>(t);
			if(style == LITERAL_OBJ)
				stream << "{ ";
			StructType::element_iterator E=st->element_begin();
			StructType::element_iterator EE=st->element_end();
			uint32_t offset=0;
			for(;E!=EE;++E)
			{
				if(offset!=0)
				{
					if(style==LITERAL_OBJ)
						stream << ", ";
					else
						stream << ';' << NewLine;
				}
				if(style==THIS_OBJ)
					stream << "this.";
				stream << "a" << offset;
				if(style==LITERAL_OBJ)
					stream << ": ";
				else
					stream << "= ";
				compileType(*E, LITERAL_OBJ);
				offset++;
			}
			if(style == LITERAL_OBJ)
				stream << " }";
			break;
		}
		case Type::PointerTyID:
		{
			Type* pointedType = t->getPointerElementType();
			if(types.isClientType(pointedType))
				stream << "null";
			else
				stream << "nullObj";
			break;
		}
		case Type::ArrayTyID:
		{
			ArrayType* at=static_cast<ArrayType*>(t);
			Type* et=at->getElementType();
			//For numerical types, create typed arrays
			if(types.isTypedArrayType(et))
			{
				stream << "new ";
				compileTypedArrayType(et);
				stream << '(' << at->getNumElements() << ')';
			}
			else
			{
				stream << '[';
				for(uint64_t i=0;i<at->getNumElements();i++)
				{
					compileType(at->getElementType(), LITERAL_OBJ);
					if((i+1)<at->getNumElements())
						stream << ",";
				}
				stream << ']';
			}
			break;
		}
		default:
			llvm::errs() << "Support type ";
			t->dump();
			llvm::errs() << '\n';
	}
}

void CheerpWriter::compileType(Type* t, COMPILE_TYPE_STYLE style)
{
	if(StructType* st=dyn_cast<StructType>(t))
	{
		NamedMDNode* basesMeta=NULL;
		//TODO: Verify that it makes sense to assume struct with no name has no bases
		if(st->hasName())
			basesMeta=module.getNamedMetadata(Twine(st->getName(),"_bases"));
		if(basesMeta && globalDeps.classesWithBaseInfo().count(st))
		{
			if(style==LITERAL_OBJ)
			{
				stream << "new create" << namegen.filterLLVMName(st->getName(), true) << "()";
			}
			else
			{
				stream << "create" << namegen.filterLLVMName(st->getName(), true) << ".call(this)";
			}
			return;
		}
		//Else fallthrough to base case
	}
	compileTypeImpl(t, style);
}

uint32_t CheerpWriter::compileClassTypeRecursive(const std::string& baseName, StructType* currentType, uint32_t baseCount)
{
	stream << "a[" << baseCount << "] = " << baseName << ';' << NewLine;
	stream << baseName << ".o=" << baseCount << ';' << NewLine;
	stream << baseName << ".a=a;" << NewLine;
	baseCount++;

	uint32_t firstBase, localBaseCount;
	if(!types.getBasesInfo(currentType, firstBase, localBaseCount))
		return baseCount;
	//baseCount has been already incremented above

	for(uint32_t i=firstBase;i<(firstBase+localBaseCount);i++)
	{
		char buf[12];
		snprintf(buf,12,".a%u",i);
		baseCount=compileClassTypeRecursive(baseName+buf, cast<StructType>(currentType->getElementType(i)), baseCount);
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
	stream << "function create" << namegen.filterLLVMName(T->getName(), true) << "(){" << NewLine;

	//TODO: Currently base classes are initialized also during compileTypeImpl
	//find a way to skip it. It's also necessary to initialize members that require
	//downcast support
	compileTypeImpl(T, THIS_OBJ);
	stream << NewLine;

	NamedMDNode* basesNamedMeta=module.getNamedMetadata(Twine(T->getName(),"_bases"));
	if(basesNamedMeta)
	{
		MDNode* basesMeta=basesNamedMeta->getOperand(0);
		assert(basesMeta->getNumOperands()==2);
		uint32_t baseMax=getIntFromValue(cast<ConstantAsMetadata>(basesMeta->getOperand(1))->getValue());
		stream << "var a=new Array(" << baseMax << ");" << NewLine;

		compileClassTypeRecursive("this", T, 0);
	}
	stream << '}' << NewLine;
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
	stream << namegen.filterLLVMName(T->getName(), true);
	stream << "(size){" << NewLine;
	stream << "var ret=new Array(size);" << NewLine << "for(var __i__=0;__i__<size;__i__++)" << NewLine;
	stream << "ret[__i__]=";
	compileType(T, LITERAL_OBJ);
	stream << ';' << NewLine << "return ret;" << NewLine << '}' << NewLine;
}

void CheerpWriter::compileArrayPointerType()
{
	stream << "function createPointerArray(size) { var ret=new Array(size); for(var __i__=0;__i__<size;__i__++) ret[__i__]={ d: null, o: 0}; return ret; }"
		<< NewLine;
}

