//===-- Opcodes.cpp - The Cheerp JavaScript generator ---------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014-2015 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/Utility.h"
#include "llvm/Cheerp/Writer.h"

using namespace llvm;
using namespace cheerp;

void CheerpWriter::compileIntegerComparison(const llvm::Value* lhs, const llvm::Value* rhs, CmpInst::Predicate p, PARENT_PRIORITY parentPrio)
{
	if(lhs->getType()->isPointerTy())
	{
		if(p==CmpInst::ICMP_EQ || p==CmpInst::ICMP_NE)
		{
			// LOGICAL_OR is slighly conservative
			if(parentPrio > LOGICAL_OR) stream << '(';
			compileEqualPointersComparison(lhs, rhs, p);
			if(parentPrio > LOGICAL_OR) stream << ')';
		}
		else
		{
			POINTER_KIND kind = PA.getPointerKind(lhs);
			//Comparison on different bases is anyway undefined, so ignore them
			if(parentPrio > COMPARISON) stream << '(';
			if (kind == RAW)
			{
				// Pointers must be of the same type
				assert(kind == PA.getPointerKind(rhs));
				stream << "(";
				compileRawPointer(lhs);
				stream << "|0)";
			}
			else
				compilePointerOffset( lhs, COMPARISON );
			compilePredicate(p);
			if (kind == RAW)
			{
				stream << "(";
				compileRawPointer(rhs);
				stream << "|0)";
			}
			else
				compilePointerOffset( rhs, COMPARISON );
			if(parentPrio > COMPARISON) stream << ')';
		}
	}
	else
	{
		if(parentPrio > COMPARISON) stream << '(';
		compileOperandForIntegerPredicate(lhs,p,COMPARISON);
		compilePredicate(p);
		compileOperandForIntegerPredicate(rhs,p,COMPARISON);
		if(parentPrio > COMPARISON) stream << ')';
	}
}

void CheerpWriter::compilePtrToInt(const llvm::Value* v)
{
	stream << '(';
	Type* pointedType = v->getType()->getPointerElementType();
	// Multiplying by the size is only required for pointer subtraction, which implies that the type is sized
	uint64_t typeSize = pointedType->isSized() ? targetData.getTypeAllocSize(pointedType) : 0;
	if (PA.getPointerKind(v) == RAW)
	{
		compileRawPointer(v);
		stream << "|0";
	}
	else if(typeSize>1 && PA.getPointerKind(v) != BYTE_LAYOUT)
	{
		if(useMathImul)
		{
			stream << "Math.imul(";
			compilePointerOffset(v, LOWEST);
			stream << ',' << typeSize << ')';
		}
		else
		{
			stream << '(';
			compilePointerOffset(v, LOWEST);
			stream << ')';
			stream << '*' << typeSize;
		}
	}
	else
		compilePointerOffset(v, LOWEST);
	stream << ')';
}

void CheerpWriter::compileSubtraction(const llvm::Value* lhs, const llvm::Value* rhs, PARENT_PRIORITY parentPrio)
{
	//Integer subtraction
	//TODO: optimize negation
	PARENT_PRIORITY subPrio = ADD_SUB;
	if(needsIntCoercion(Registerize::INTEGER, parentPrio))
		subPrio = BIT_OR;
	if(parentPrio > subPrio) stream << '(';
	compileOperand(lhs, ADD_SUB);
	stream << '-';
	// TODO: to avoid `--` for now we set HIGHEST priority, and
	// compileConstant adds parenthesis if the constant is negative
	compileOperand(rhs, HIGHEST);
	if(subPrio == BIT_OR)
			stream << "|0";
	if(parentPrio > subPrio) stream << ')';
}

void CheerpWriter::compileBitCast(const llvm::User* bc_inst, POINTER_KIND kind)
{
	if (kind == RAW)
	{
		compileOperand(bc_inst->getOperand(0));
		stream << "|0";
	}
	else if(kind==COMPLETE_OBJECT)
		compileCompleteObject(bc_inst->getOperand(0));
	else
	{
		if(PA.getConstantOffsetForPointer(bc_inst))
			compilePointerBase(bc_inst);
		else if(PA.getPointerKind(bc_inst->getOperand(0)) == REGULAR && !isa<Argument>(bc_inst->getOperand(0)))
			compileOperand(bc_inst->getOperand(0));
		else
		{
			stream << "{d:";
			compilePointerBase(bc_inst, true);
			stream << ",o:";
			compilePointerOffset(bc_inst, LOWEST, true);
			stream << "}";
		}
	}
}

void CheerpWriter::compileBitCastBase(const llvm::User* bi, bool forEscapingPointer)
{
	Type* dst=bi->getType();
	//Special case unions
	if(PA.getPointerKind(bi->getOperand(0)) == BYTE_LAYOUT && forEscapingPointer)
	{
		//Find the type
		llvm::Type* elementType = dst->getPointerElementType();
		bool isArray=isa<ArrayType>(elementType);
		llvm::Type* pointedType = (isArray)?elementType->getSequentialElementType():elementType;
		if(TypeSupport::isTypedArrayType(pointedType, /* forceTypedArray*/ true))
		{
			stream << "new ";
			compileTypedArrayType(pointedType);
			stream << '(';
			if(isa<AllocaInst>(bi->getOperand(0)))
				compileCompleteObject(bi->getOperand(0));
			else
				compilePointerBase(bi->getOperand(0));
			stream << ".buffer";
			if(!isa<AllocaInst>(bi->getOperand(0)))
			{
				stream << ',';
				compilePointerOffset(bi->getOperand(0), LOWEST, false);
			}
			stream << ')';
			return;
		}
	}

	compilePointerBase(bi->getOperand(0), forEscapingPointer);
}

void CheerpWriter::compileBitCastOffset(const llvm::User* bi, PARENT_PRIORITY parentPrio)
{
	Type* dst=bi->getType();
	//Special case unions
	if(PA.getPointerKind(bi->getOperand(0)) == BYTE_LAYOUT)
	{
		//Find the type
		llvm::Type* elementType = dst->getPointerElementType();
		bool isArray=isa<ArrayType>(elementType);
		llvm::Type* pointedType = (isArray)?elementType->getSequentialElementType():elementType;
		if(TypeSupport::isTypedArrayType(pointedType, /* forceTypedArray*/ true))
		{
			stream << '0';
			return;
		}
	}

	compilePointerOffset(bi->getOperand(0), parentPrio, true);
}

void CheerpWriter::compileSelect(const llvm::User* select, const llvm::Value* cond, const llvm::Value* lhs, const llvm::Value* rhs, PARENT_PRIORITY parentPrio)
{
	if(parentPrio >= TERNARY) stream << '(';
	compileOperand(cond, TERNARY, /*allowBooleanObjects*/ true);
	stream << '?';

	if(select->getType()->isPointerTy() && PA.getPointerKind(select) != RAW)
	{
		POINTER_KIND k = PA.getPointerKind(select);
		compilePointerAs(lhs, k);
		stream << ':';
		compilePointerAs(rhs, k);
	}
	else
	{
		PARENT_PRIORITY prio = TERNARY;
		bool asmjs = currentFun && currentFun->getSection() == StringRef("asmjs");
		Registerize::REGISTER_KIND regKind = registerize.getRegKindFromType(lhs->getType(),asmjs);
		if(needsIntCoercion(regKind, TERNARY))
			prio = BIT_OR;
		compileOperand(lhs, prio);
		if (prio == BIT_OR)
			stream << "|0";
		stream << ':';
		compileOperand(rhs, prio);
		if (prio == BIT_OR)
			stream << "|0";
	}

	if(parentPrio >= TERNARY) stream << ')';
}
