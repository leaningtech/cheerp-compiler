//===-- LinearMemoryHelper.cpp - The Cheerp JavaScript generator ----------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/LinearMemoryHelper.h"
#include "llvm/Support/raw_ostream.h"

using namespace cheerp;
using namespace llvm;

void LinearMemoryHelper::compileConstantAsBytes(const Constant* c, bool asmjs, ByteListener* listener)
{
	if(const ConstantDataSequential* CD = dyn_cast<ConstantDataSequential>(c))
	{
		for(uint32_t i=0;i<CD->getNumElements();i++)
			compileConstantAsBytes(CD->getElementAsConstant(i), asmjs, listener);
	}
	else if(const UndefValue* U = dyn_cast<UndefValue>(c))
	{
		uint32_t size = targetData.getTypeAllocSize(U->getType());
		for (uint32_t i = 0; i < size; i++)
			listener->addByte(0);
	}
	else if(isa<ConstantArray>(c) || isa<ConstantStruct>(c))
	{
		for(uint32_t i=0;i<c->getNumOperands();i++)
			compileConstantAsBytes(cast<Constant>(c->getOperand(i)), asmjs, listener);
	}
	else if(const ConstantFP* f=dyn_cast<ConstantFP>(c))
	{
		const APFloat& flt = f->getValueAPF();
		const APInt& integerRepresentation = flt.bitcastToAPInt();
		uint64_t val = integerRepresentation.getLimitedValue();
		uint32_t bitWidth = integerRepresentation.getBitWidth();
		for(uint32_t i=0;i<bitWidth;i+=8)
			listener->addByte((val>>i)&255);
	}
	else if(const ConstantInt* i=dyn_cast<ConstantInt>(c))
	{
		const APInt& integerRepresentation = i->getValue();
		uint64_t val = integerRepresentation.getLimitedValue();
		uint32_t bitWidth = integerRepresentation.getBitWidth();
		for(uint32_t i=0;i<bitWidth;i+=8)
			listener->addByte((val>>i)&255);
	}
	else if (asmjs)
	{
		if(const ConstantAggregateZero* Z = dyn_cast<ConstantAggregateZero>(c))
		{
			uint32_t size = targetData.getTypeAllocSize(Z->getType());
			for (uint32_t i = 0; i < size; i++)
				listener->addByte(0);
		}
		else if(dyn_cast<ConstantPointerNull>(c))
		{
			listener->addByte(0);
			listener->addByte(0);
			listener->addByte(0);
			listener->addByte(0);
		}
		else if(const Function* F = dyn_cast<Function>(c))
		{
			if (!globalDeps.functionAddresses().count(F))
			{
				llvm::errs() << "function not in table: " << F->getName() <<"\n";
				llvm::report_fatal_error("please report a bug");
			}
			int32_t offset = globalDeps.functionAddresses().at(F) + functionAddrStart;
			for(uint32_t i=0;i<32;i+=8)
				listener->addByte((offset>>i)&255);
		}
		else if(isa<ConstantExpr>(c))
		{
			const ConstantExpr* ce = cast<ConstantExpr>(c);
			switch(ce->getOpcode())
			{
				case Instruction::GetElementPtr:
				{
					assert(isa<GlobalVariable>(ce->getOperand(0)));
					const GlobalVariable* g = cast<GlobalVariable>(ce->getOperand(0));
					if (!gVarsAddr.count(g))
					{
						llvm::errs() << "global variable not found:" << g->getName() << "\n";
						llvm::report_fatal_error("please report a bug");
					}
					uint32_t addr = gVarsAddr[g];

					Type* curTy = g->getType();
					SmallVector< const Value *, 8 > indices ( std::next(ce->op_begin()), ce->op_end() );
					for (uint32_t i=0; i<indices.size(); i++)
					{
						uint32_t index = cast<ConstantInt>(indices[i])->getZExtValue();
						if (StructType* ST = dyn_cast<StructType>(curTy))
						{
							const StructLayout* SL = targetData.getStructLayout( ST );
							addr += SL->getElementOffset(index);
							curTy = ST->getElementType(index);
						}
						else
						{
							addr += index*targetData.getTypeAllocSize(curTy->getSequentialElementType());
							curTy = curTy->getSequentialElementType();
						}
					}
					for(uint32_t i=0;i<32;i+=8)
						listener->addByte((addr>>i)&255);
					
					break;
				}
				case Instruction::IntToPtr:
				{
					const ConstantInt* ptr = cast<ConstantInt>(ce->getOperand(0));
					uint32_t val = ptr->getZExtValue();
					for(uint32_t i=0;i<32;i+=8)
						listener->addByte((val>>i)&255);
					break;
				}
				case Instruction::BitCast:
				{
					compileConstantAsBytes(ce->getOperand(0), asmjs, listener);
					break;
				}
				default:
					// TODO: It could make sense to emit the right number of zeroes anyway
					llvm::errs() << "warning: Unsupported constant expr in asm.js module :" << ce->getOpcodeName() << '\n';
			}
		}
		else if(isa<GlobalVariable>(c))
		{
			const GlobalVariable* g = cast<GlobalVariable>(c);
			if (gVarsAddr.count(g) != 1)
			{
				llvm::errs() << "global variable not found:" << g->getName() << "\n";
				llvm::report_fatal_error("please report a bug");
			}
			uint32_t val = gVarsAddr[g];
			for(uint32_t i=0;i<32;i+=8)
				listener->addByte((val>>i)&255);
		}
		else
		{
			// TODO: It could make sense to emit the right number of zeroes anyway
			llvm::errs() << "Unsupported constant type for bytes in asm.js module :";
			c->getType()->dump();
		}
	}
	else
	{
		// TODO: It could make sense to emit the right number of zeroes anyway
		llvm::errs() << "Unsupported constant type for bytes ";
		c->dump();
	}
}

uint32_t LinearMemoryHelper::addGlobalVariable(const GlobalVariable* G)
{
	Type* ty = G->getType();
	uint32_t size = targetData.getTypeAllocSize(ty->getPointerElementType());
	// Ensure the right alignment for the type
	uint32_t alignment = TypeSupport::getAlignmentAsmJS(targetData, ty->getPointerElementType());
	// The following is correct if alignment is a power of 2 (which it should be)
	uint32_t ret = heapStart = (heapStart + alignment - 1) & ~(alignment - 1);
	gVarsAddr.emplace(G,heapStart);
	heapStart += size;
	return ret;
}

uint32_t LinearMemoryHelper::getGlobalVariableAddress(const GlobalVariable* G) const
{
	assert(gVarsAddr.count(G));
	return gVarsAddr.find(G)->second;
}
