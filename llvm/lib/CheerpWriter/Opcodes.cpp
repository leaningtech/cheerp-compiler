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

void CheerpWriter::compileFloatComparison(const llvm::Value* lhs, const llvm::Value* rhs, CmpInst::Predicate p, PARENT_PRIORITY parentPrio, bool asmjs)
{
	//Check that the operation is JS safe
	//Special case orderedness check
	if(p==CmpInst::FCMP_ORD)
	{
		if(parentPrio > LOGICAL_AND) stream << '(';
		compileOperand(lhs,PARENT_PRIORITY::COMPARISON);
		if (asmjs)
			stream << "==";
		else
			stream << "===";
		compileOperand(lhs,PARENT_PRIORITY::COMPARISON);
		if (asmjs)
			stream << '&';
		else
			stream << "&&";
		compileOperand(rhs,PARENT_PRIORITY::COMPARISON);
		if (asmjs)
			stream << "==";
		else
			stream << "===";
		compileOperand(rhs,PARENT_PRIORITY::COMPARISON);
		if(parentPrio > LOGICAL_AND) stream << ')';
	}
	else if(p==CmpInst::FCMP_UNO)
	{
		if(parentPrio > LOGICAL_OR) stream << '(';
		stream << "!(";
		compileOperand(lhs,PARENT_PRIORITY::COMPARISON);
		if (asmjs)
			stream << "==";
		else
			stream << "===";
		compileOperand(lhs,PARENT_PRIORITY::COMPARISON);
		stream << ')';
		if (asmjs)
			stream << '|';
		else
			stream << "||";
		stream << "!(";
		compileOperand(rhs,PARENT_PRIORITY::COMPARISON);
		if (asmjs)
			stream << "==";
		else
			stream << "===";
		compileOperand(rhs,PARENT_PRIORITY::COMPARISON);
		stream << ')';
		if(parentPrio > LOGICAL_OR) stream << ')';
	}
	else
	{
		if(parentPrio > COMPARISON) stream << '(';
		// It is much more efficient to invert the predicate if we need to check for unorderedness
		bool invertForUnordered = CmpInst::isUnordered(p);
		if(invertForUnordered)
		{
			p = CmpInst::getInversePredicate(p);
			stream << "!(";
		}
		compileOperand(lhs, COMPARISON);
		compilePredicate(p);
		compileOperand(rhs, COMPARISON);
		if(invertForUnordered)
			stream << ')';
		if(parentPrio > COMPARISON) stream << ')';
	}
}

void CheerpWriter::compilePtrToInt(const llvm::Value* v, bool isInt64)
{
	Type* pointedType = v->getType()->getPointerElementType();
	if (isInt64)
	{
		stream << "BigInt(";
	}
	else
	{
		stream << '(';
	}
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
			stream << namegen.getBuiltinName(NameGenerator::Builtin::IMUL) << "(";
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

void CheerpWriter::compileSubtraction(const llvm::Value* lhs, const llvm::Value* rhs, PARENT_PRIORITY parentPrio, bool asmjs)
{
	//Integer subtraction
	bool isInt64 = lhs->getType()->isIntegerTy(64);
	PARENT_PRIORITY subPrio = ADD_SUB;
	if(isInt64 && parentPrio!=INTN)
	{
		subPrio = INTN;
		parentPrio = LOWEST;
		stream << "BigInt.asIntN(64,";
	}
	else if(!isInt64 && needsIntCoercion(parentPrio))
		subPrio = BIT_OR;

	if(parentPrio > subPrio) stream << '(';

	// Optimize negation, unless in asm.js mode
	// In asm.js, if the expression happens to be -0 the type checker will see a double and get confused
	if(asmjs || !(isa<ConstantInt>(lhs) && cast<ConstantInt>(lhs)->isZero()))
		compileOperand(lhs, ADD_SUB);
	stream << '-';
	compileOperand(rhs, nextPrio(ADD_SUB));

	if(subPrio == INTN)
		stream << ')';
	else if(subPrio == BIT_OR)
		stream << "|0";
	if(parentPrio > subPrio) stream << ')';
}

void CheerpWriter::compileDivRem(const llvm::Value* lhs, const llvm::Value* rhs, PARENT_PRIORITY parentPrio, char op, bool isSigned)
{
	//Integer signed division
	bool isInt64 = lhs->getType()->isIntegerTy(64);
	PARENT_PRIORITY myPrio = MUL_DIV;
	if(isInt64 && parentPrio!=INTN)
	{
		myPrio = INTN;
		parentPrio = LOWEST;
		stream << "BigInt.asIntN(64,";
	}
	else if(!isInt64 && needsIntCoercion(parentPrio))
		myPrio = BIT_OR;

	if(parentPrio > myPrio) stream << '(';

	if (isSigned)
		compileSignedInteger(lhs, /*forComparison*/ false, MUL_DIV);
	else
		compileUnsignedInteger(lhs, /*forComparison*/ false, MUL_DIV);

	stream << op;

	if (isSigned)
		compileSignedInteger(rhs, /*forComparison*/ false, nextPrio(MUL_DIV));
	else
		compileUnsignedInteger(rhs, /*forComparison*/ false, nextPrio(MUL_DIV));

	if(myPrio == INTN)
		stream << ')';
	else if(myPrio == BIT_OR)
		stream << "|0";

	if(parentPrio > myPrio) stream << ')';
}

void CheerpWriter::compileBitCast(const llvm::User* bc_inst, POINTER_KIND kind, PARENT_PRIORITY parentPrio)
{
	if (kind == RAW)
		compilePointerAs(bc_inst->getOperand(0), RAW, parentPrio);
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
		llvm::Type* pointedType = (isArray)?elementType->getArrayElementType():elementType;
		if(TypeSupport::isTypedArrayType(pointedType, /* forceTypedArray*/ true))
		{
			stream << "(";
			if(isa<AllocaInst>(bi->getOperand(0)))
				compileCompleteObject(bi->getOperand(0));
			else
				compilePointerBase(bi->getOperand(0));
			stream << ".";
			compileTypedArrayType(pointedType);
			stream << "||(";
			if(isa<AllocaInst>(bi->getOperand(0)))
				compileCompleteObject(bi->getOperand(0));
			else
				compilePointerBase(bi->getOperand(0));
			stream << ".";
			compileTypedArrayType(pointedType);
			stream << "=new ";
			compileTypedArrayType(pointedType);
			stream << '(';
			if(isa<AllocaInst>(bi->getOperand(0)))
				compileCompleteObject(bi->getOperand(0));
			else
				compilePointerBase(bi->getOperand(0));
			stream << ".buffer)))";
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
		llvm::Type* pointedType = (isArray)?elementType->getArrayElementType():elementType;
		if(TypeSupport::isTypedArrayType(pointedType, /* forceTypedArray*/ true))
		{
			compileByteLayoutOffset( bi->getOperand(0), BYTE_LAYOUT_OFFSET_FULL );
			uint32_t size = targetData.getTypeAllocSize(pointedType);
			if(size != 1)
				stream << ">>" << Log2_32(size);
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

	if(select->getType()->isPointerTy())
	{
		POINTER_KIND k = PA.getPointerKind(select);
		compilePointerAs(lhs, k);
		stream << ':';
		compilePointerAs(rhs, k);
	}
	else
	{
		PARENT_PRIORITY prio = TERNARY;
		bool isIntTy = lhs->getType()->isIntegerTy();
		bool isInt64Ty = lhs->getType()->isIntegerTy(64);
		if(isIntTy && !isInt64Ty)
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
