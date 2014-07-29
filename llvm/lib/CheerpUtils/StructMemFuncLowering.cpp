//===-- StructMemFuncLowering.cpp - Cheerp helper -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/StructMemFuncLowering.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

const char *StructMemFuncLowering::getPassName() const {
	return "StructMemFuncLowering";
}

void StructMemFuncLowering::createMemFunc(IRBuilder<>* IRB, Value* baseDst, Value* baseSrc, size_t size,
						SmallVector<Value*, 8>& indexes)
{
	Value* src = IRB->CreateGEP(baseSrc, indexes);
	Value* dst = IRB->CreateGEP(baseDst, indexes);
	assert(!src->getType()->getPointerElementType()->isArrayTy());
	// Create a type safe memcpy
	IRB->CreateMemCpy(dst, src, size, 1, false, NULL, NULL, NULL, NULL, false);
}

void StructMemFuncLowering::recursiveCopy(IRBuilder<>* IRB, Value* baseDst, Value* baseSrc, Type* curType,
						Type* indexType, SmallVector<Value*, 8>& indexes)
{
	// For aggregates we push a new index and overwrite it for each element
	if(StructType* ST=dyn_cast<StructType>(curType))
	{
		if (ST->hasByteLayout())
			return createMemFunc(IRB, baseDst, baseSrc, DL->getTypeAllocSize(curType), indexes);
		indexes.push_back(NULL);
		for(uint32_t i=0;i<ST->getNumElements();i++)
		{
			indexes.back() = ConstantInt::get(indexType, i);
			recursiveCopy(IRB, baseDst, baseSrc, ST->getElementType(i), indexType, indexes);
		}
		indexes.pop_back();
	}
	else if(ArrayType* AT=dyn_cast<ArrayType>(curType))
	{
		Type* elementType = AT->getElementType();
		indexes.push_back(NULL);
		if (elementType->isIntegerTy() || elementType->isFloatingPointTy())
		{
			indexes.back() = ConstantInt::get(indexType, 0);
			createMemFunc(IRB, baseDst, baseSrc, DL->getTypeAllocSize(curType), indexes);
		}
		else
		{
			for(uint32_t i=0;i<AT->getNumElements();i++)
			{
				indexes.back() = ConstantInt::get(indexType, i);
				recursiveCopy(IRB, baseDst, baseSrc, elementType, indexType, indexes);
			}
		}
		indexes.pop_back();
	}
	else
	{
		Value* elementSrc = IRB->CreateGEP(baseSrc, indexes);
		Value* elementDst = IRB->CreateGEP(baseDst, indexes);
		Value* element = IRB->CreateLoad(elementSrc);
		IRB->CreateStore(element, elementDst);
	}
}

void StructMemFuncLowering::recursiveReset(IRBuilder<>* IRB, Value* baseDst, Value* resetVal, Type* curType,
						Type* indexType, SmallVector<Value*, 8>& indexes)
{
	// For aggregates we push a new index and overwrite it for each element
	if(StructType* ST=dyn_cast<StructType>(curType))
	{
		indexes.push_back(NULL);
		for(uint32_t i=0;i<ST->getNumElements();i++)
		{
			indexes.back() = ConstantInt::get(indexType, i);
			recursiveReset(IRB, baseDst, resetVal, ST->getElementType(i), indexType, indexes);
		}
		indexes.pop_back();
	}
	else if(ArrayType* AT=dyn_cast<ArrayType>(curType))
	{
		Type* elementType = AT->getElementType();
		indexes.push_back(NULL);
		for(uint32_t i=0;i<AT->getNumElements();i++)
		{
			indexes.back() = ConstantInt::get(indexType, i);
			recursiveReset(IRB, baseDst, resetVal, elementType, indexType, indexes);
		}
		indexes.pop_back();
	}
	else if(IntegerType* IT=dyn_cast<IntegerType>(curType))
	{
		int bitWidth = IT->getBitWidth();
		// We need to expand the resetVal 8-bit value to the right size
		Value* expandedResetVal = IRB->CreateZExtOrTrunc(resetVal,IT);
		Value* computedResetVal = expandedResetVal;
		for(int i=8;i<bitWidth;i+=8)
		{
			computedResetVal=IRB->CreateShl(computedResetVal, 8);
			computedResetVal=IRB->CreateOr(computedResetVal, expandedResetVal);
		}
		Value* elementDst = IRB->CreateGEP(baseDst, indexes);
		IRB->CreateStore(computedResetVal, elementDst);
	}
	else if(curType->isFloatTy() || curType->isDoubleTy())
	{
		// Only constant values are supported
		// TODO: Stop non constant in the frontend
		uint8_t constResetVal = cast<ConstantInt>( resetVal )->getZExtValue();
		int bytesNum = curType->isFloatTy() ? 4 : 8;
		uint64_t floatConstant = 0;
		for(int i=0;i<bytesNum;i++)
		{
			floatConstant <<= 8;
			floatConstant |= constResetVal;
		}
		Value* floatResetVal = NULL;
		if(curType->isFloatTy())
			floatResetVal = ConstantFP::get(curType->getContext(), APFloat(APFloat::IEEEsingle, floatConstant));
		else
			floatResetVal = ConstantFP::get(curType->getContext(), APFloat(APFloat::IEEEdouble, floatConstant));
		Value* elementDst = IRB->CreateGEP(baseDst, indexes);
		IRB->CreateStore(floatResetVal, elementDst);
	}
	else if(PointerType* PT=dyn_cast<PointerType>(curType))
	{
		// Only constant NULL is supported
		// TODO: Stop non constant in the frontend
		assert(cast<ConstantInt>( resetVal )->getZExtValue() == 0);
		Value* elementDst = IRB->CreateGEP(baseDst, indexes);
		IRB->CreateStore(ConstantPointerNull::get(PT), elementDst);
	}
	else
	{
		assert(false && "Unexpected type while unrolling memset");
	}
}

// Create a forward loop, the index is incremented until elementsCount is reached. IRB already inserts inside currentBlock.
void StructMemFuncLowering::createForwardLoop(IRBuilder<>* IRB, BasicBlock* previousBlock, BasicBlock* endBlock, BasicBlock* currentBlock,
						Type* pointedType, Value* dst, Value* src, Value* elementsCount, MODE mode)
{
	Type* int32Type = IntegerType::get(previousBlock->getContext(), 32);
	// We need an index variable
	PHINode* index=IRB->CreatePHI(int32Type, 2);
	// Start from 0 if coming from the previous block, later on we will add the modified index
	index->addIncoming(ConstantInt::get(int32Type, 0), previousBlock);
	// Now, recursively descend into the object to copy all the values
	SmallVector<Value*, 8> indexes;
	indexes.push_back(index);
	if (mode == MEMSET)
		recursiveReset(IRB, dst, src, pointedType, int32Type, indexes);
	else
		recursiveCopy(IRB, dst, src, pointedType, int32Type, indexes);
	// Increment the index
	Value* incrementedIndex = IRB->CreateAdd(index, ConstantInt::get(int32Type, 1));
	// Close the loop for index
	index->addIncoming(incrementedIndex, currentBlock);
	// Check if we have finished, if not loop again
	Value* finishedLooping=IRB->CreateICmp(CmpInst::ICMP_EQ, elementsCount, incrementedIndex);
	IRB->CreateCondBr(finishedLooping, endBlock, currentBlock);
}

// Create a backward loop, the index is increment from elementsCount-1 to 0. IRB already inserts inside currentBlock.
void StructMemFuncLowering::createBackwardLoop(IRBuilder<>* IRB, BasicBlock* previousBlock, BasicBlock* endBlock, BasicBlock* currentBlock,
						Type* pointedType, Value* dst, Value* src, Value* elementsCount)
{
	Type* int32Type = IntegerType::get(previousBlock->getContext(), 32);
	// We need an index variable
	PHINode* index=IRB->CreatePHI(int32Type, 2);
	// Start from elementsCount if coming from the previous block, later on we will add the modified index
	index->addIncoming(elementsCount, previousBlock);
	// Immediately decrement by one, so that we are accessing a valid element
	Value* decrementedIndex = IRB->CreateSub(index, ConstantInt::get(int32Type, 1));
	// Now, recursively descend into the object to copy all the values
	SmallVector<Value*, 8> indexes;
	indexes.push_back(decrementedIndex);
	recursiveCopy(IRB, dst, src, pointedType, int32Type, indexes);
	// Close the loop for index
	index->addIncoming(decrementedIndex, currentBlock);
	// Check if we have finished, if not loop again
	Value* finishedLooping=IRB->CreateICmp(CmpInst::ICMP_EQ, ConstantInt::get(int32Type, 0), decrementedIndex);
	IRB->CreateCondBr(finishedLooping, endBlock, currentBlock);
}

bool StructMemFuncLowering::runOnBlock(BasicBlock& BB)
{
	BasicBlock::iterator it=BB.begin();
	BasicBlock::iterator itE=BB.end();
	for(;it!=itE;++it)
	{
		CallInst* CI=dyn_cast<CallInst>(it);
		if(!CI || !CI->getCalledFunction())
			continue;
		Function* F=CI->getCalledFunction();
		MODE mode = NONE;
		if(F->getIntrinsicID()==Intrinsic::memcpy)
			mode = MEMCPY;
		else if(F->getIntrinsicID()==Intrinsic::memmove)
			mode = MEMMOVE;
		else if(F->getIntrinsicID()==Intrinsic::memset)
			mode = MEMSET;
		if(mode==NONE)
			continue;
		Type* pointedType = F->getFunctionType()->getParamType(0)->getPointerElementType();
		//We want to decompose everything which is not an immutable type or a byte layout structure. memset is always decomposed.
		if(mode != MEMSET && (pointedType->isIntegerTy() || pointedType->isFloatingPointTy() ||
			(isa<StructType>(pointedType) && cast<StructType>(pointedType)->hasByteLayout())))
		{
			continue;
		}
		//We have a typed mem func on a struct
		//Decompose it in a loop
		Value* dst=CI->getOperand(0);
		//In MEMSET mode src is the value to be written
		Value* src=CI->getOperand(1);
		assert(dst->getType() == src->getType() || mode==MEMSET);
		Value* size=CI->getOperand(2);
		uint32_t byteSize = DL->getTypeAllocSize(pointedType);
		//First of all split the original block
		BasicBlock* endLoop = BB.splitBasicBlock(it);
		//Add the lower part of the block to be anaysized later on
		basicBlocks.push_back(endLoop);
		//Now BB is linked to endblock by an unconditional jump
		//Instead we need to check if we need to enter the loop
		Instruction* oldBranch = BB.getTerminator();
		//Delete the old branch
		oldBranch->eraseFromParent();
		IRBuilder<>* IRB = new IRBuilder<>(&BB);
		Type* int32Type = IntegerType::get(BB.getContext(), 32);
		Value* fixedSize = IRB->CreateZExtOrTrunc(size, int32Type);
		Value* elementsCount=IRB->CreateUDiv(fixedSize, ConstantInt::get(int32Type, byteSize));
		Value* countIsZero=IRB->CreateICmp(CmpInst::ICMP_EQ, elementsCount, ConstantInt::get(int32Type, 0));
		BasicBlock* memfuncBody=BasicBlock::Create(BB.getContext(), "memfunc.body", BB.getParent());
		IRB->CreateCondBr(countIsZero, endLoop, memfuncBody);
		IRB->SetInsertPoint(memfuncBody);
		if (mode == MEMMOVE)
		{
			// For memmove we need to check the relative ordering of src and dst and select a direction accordingly
			Value* srcAfterDst = IRB->CreateICmp(CmpInst::ICMP_UGE, src, dst);
			// Create two basic blocks, one is for the forward case, the other for the backward case
			BasicBlock* memmoveForward=BasicBlock::Create(BB.getContext(), "memmove.forward", BB.getParent());
			BasicBlock* memmoveBackward=BasicBlock::Create(BB.getContext(), "memmove.backward", BB.getParent());
			IRB->CreateCondBr(srcAfterDst, memmoveForward, memmoveBackward);
			// Do the forward side
			IRB->SetInsertPoint(memmoveForward);
			createForwardLoop(IRB, memfuncBody, endLoop, memmoveForward, pointedType, dst, src, elementsCount, mode);
			// Do the backward side
			IRB->SetInsertPoint(memmoveBackward);
			createBackwardLoop(IRB, memfuncBody, endLoop, memmoveBackward, pointedType, dst, src, elementsCount);
		}
		else //if(mode == MEMCPY || mode == MEMSET)
			createForwardLoop(IRB, &BB, endLoop, memfuncBody, pointedType, dst, src, elementsCount, mode);
		CI->eraseFromParent();
		return true;
	}
	return false;
}

bool StructMemFuncLowering::runOnFunction(Function& F)
{
	DL = &F.getParent()->getDataLayout();
	assert(DL);
	for(BasicBlock& BB: F)
		basicBlocks.push_back(&BB);
	while(!basicBlocks.empty())
	{
		BasicBlock* BB = basicBlocks.pop_back_val();
		runOnBlock(*BB);
	}
	return true;
}

char StructMemFuncLowering::ID = 0;

FunctionPass *llvm::createStructMemFuncLowering() { return new StructMemFuncLowering(); }
