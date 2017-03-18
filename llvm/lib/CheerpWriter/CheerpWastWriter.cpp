//===-- CheerpWriter.cpp - The Cheerp JavaScript generator -------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/WastWriter.h"

using namespace llvm;
using namespace cheerp;

const char* CheerpWastWriter::getTypeString(Type* t)
{
	if(t->isIntegerTy(32))
		return "i32";
	else
	{
		llvm::errs() << "Unsupported type " << *t << "\n";
		llvm_unreachable("Unsuppored type");
	}
}

void CheerpWastWriter::compileConstant(const Constant* c)
{
	if(const ConstantInt* i=dyn_cast<ConstantInt>(c))
	{
		stream << getTypeString(i->getType()) << ".const ";
		if(i->getBitWidth()==32)
			stream << i->getSExtValue();
		else
			stream << i->getZExtValue();
	}
}

void CheerpWastWriter::compileOperand(const llvm::Value* v)
{
	if(const Constant* c=dyn_cast<Constant>(v))
		compileConstant(c);
	else
		assert(false);
}

void CheerpWastWriter::compileInstruction(const Instruction& I)
{
	switch(I.getOpcode())
	{
		case Instruction::Ret:
		{
			stream << "return (";
			compileOperand(I.getOperand(0));
			stream << ")\n";
			break;
		}
		default:
		{
			llvm::errs() << "\tImplement inst " << I.getOpcodeName() << '\n';
			return;
		}
	}
}

void CheerpWastWriter::compileBB(const BasicBlock& BB)
{
	BasicBlock::const_iterator I=BB.begin();
	BasicBlock::const_iterator IE=BB.end();
	for(;I!=IE;++I)
	{
		if(isInlineable(*I, PA))
			continue;
		if(I->getOpcode()==Instruction::PHI) //Phys are manually handled
			continue;
		if(const IntrinsicInst* II=dyn_cast<IntrinsicInst>(&(*I)))
		{
			//Skip some kind of intrinsics
			if(II->getIntrinsicID()==Intrinsic::lifetime_start ||
				II->getIntrinsicID()==Intrinsic::lifetime_end ||
				II->getIntrinsicID()==Intrinsic::dbg_declare ||
				II->getIntrinsicID()==Intrinsic::dbg_value)
			{
				continue;
			}
		}
		if(!I->getType()->isVoidTy() && !I->use_empty())
		{
			assert(false);
//			stream << namegen.getName(I) << '=';
		}
		if(I->isTerminator() || !I->use_empty() || I->mayHaveSideEffects())
		{
			compileInstruction(*I);
		}
	}
}

void CheerpWastWriter::compileMethod(const Function& F)
{
	stream << "(func ";
	// TODO: We should not export them all
	stream << "(export \"" << F.getName() << "\")";
	stream << "(result " << getTypeString(F.getReturnType()) << ')';
	stream << '(';
	assert(F.size() == 1);
	compileBB(*F.begin());
	stream << "))";
}

void CheerpWastWriter::makeWast()
{
	// Emit S-expressions for the module
	stream << "(module\n";
	
	for ( const Function & F : module.getFunctionList() )
	{
		if (!F.empty() && F.getSection() == StringRef("asmjs"))
		{
			compileMethod(F);
		}
	}
	
	stream << ')';
}
