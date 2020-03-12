//===-- StructMemFuncLowering.cpp - Cheerp helper -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014-2019 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/StructMemFuncLowering.h"
#include "llvm/Analysis/InstructionSimplify.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

const unsigned INLINE_WRITE_LOOP_MAX = 128;

StringRef StructMemFuncLowering::getPassName() const {
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
		if(AT->getNumElements() > 6)
		{
			// Create a loop instead of unrolling
			llvm::BasicBlock* prevBlock=IRB->GetInsertBlock();
			BasicBlock* arrayLoop=BasicBlock::Create(IRB->getContext(), "arrayloop", prevBlock->getParent());
			BasicBlock* afterLoop=BasicBlock::Create(IRB->getContext(), "afterloop", prevBlock->getParent());
			IRB->CreateBr(arrayLoop);
			IRB->SetInsertPoint(arrayLoop);
			llvm::PHINode* index=IRB->CreatePHI(indexType, 2);
			index->addIncoming(ConstantInt::get(indexType, 0), prevBlock);
			indexes.back() = index;
			recursiveCopy(IRB, baseDst, baseSrc, elementType, indexType, indexes);
			Value* incrementedIndex = IRB->CreateAdd(index, ConstantInt::get(indexType, 1));
			index->addIncoming(incrementedIndex, IRB->GetInsertBlock());
			Value* finishedLooping=IRB->CreateICmp(CmpInst::ICMP_EQ, ConstantInt::get(indexType, AT->getNumElements()), incrementedIndex);
			IRB->CreateCondBr(finishedLooping, afterLoop, arrayLoop);
			IRB->SetInsertPoint(afterLoop);
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
		Value* elementSrc = baseSrc;
		Value* elementDst = baseDst;
		if(indexes.size() != 1 || !isa<ConstantInt>(indexes[0]) || cast<ConstantInt>(indexes[0])->getZExtValue()!=0)
		{
			elementSrc = IRB->CreateGEP(baseSrc, indexes);
			elementDst = IRB->CreateGEP(baseDst, indexes);
		}
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
		if(AT->getNumElements() > 6)
		{
			// Create a loop instead of unrolling
			llvm::BasicBlock* prevBlock=IRB->GetInsertBlock();
			BasicBlock* arrayLoop=BasicBlock::Create(IRB->getContext(), "arrayloop", prevBlock->getParent());
			BasicBlock* afterLoop=BasicBlock::Create(IRB->getContext(), "afterloop", prevBlock->getParent());
			IRB->CreateBr(arrayLoop);
			IRB->SetInsertPoint(arrayLoop);
			llvm::PHINode* index=IRB->CreatePHI(indexType, 2);
			index->addIncoming(ConstantInt::get(indexType, 0), prevBlock);
			indexes.back() = index;
			recursiveReset(IRB, baseDst, resetVal, elementType, indexType, indexes);
			Value* incrementedIndex = IRB->CreateAdd(index, ConstantInt::get(indexType, 1));
			index->addIncoming(incrementedIndex, IRB->GetInsertBlock());
			Value* finishedLooping=IRB->CreateICmp(CmpInst::ICMP_EQ, ConstantInt::get(indexType, AT->getNumElements()), incrementedIndex);
			IRB->CreateCondBr(finishedLooping, afterLoop, arrayLoop);
			IRB->SetInsertPoint(afterLoop);
		}
		else
		{
			for(uint32_t i=0;i<AT->getNumElements();i++)
			{
				indexes.back() = ConstantInt::get(indexType, i);
				recursiveReset(IRB, baseDst, resetVal, elementType, indexType, indexes);
			}
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
			floatResetVal = ConstantFP::get(curType->getContext(), APFloat(APFloat::IEEEsingle(), floatConstant));
		else
			floatResetVal = ConstantFP::get(curType->getContext(), APFloat(APFloat::IEEEdouble(), floatConstant));
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
	bool needsLoop = !isa<ConstantInt>(elementsCount) || cast<ConstantInt>(elementsCount)->getZExtValue() != 1;
	// We need an index variable, that may be zero or a PHI
	SmallVector<Value*, 8> indexes;
	PHINode* index = NULL;
	if(needsLoop)
	{
		index=IRB->CreatePHI(int32Type, 2);
		// Start from 0 if coming from the previous block, later on we will add the modified index
		index->addIncoming(ConstantInt::get(int32Type, 0), previousBlock);
		// Now, recursively descend into the object to copy all the values
		indexes.push_back(index);
	}
	else
		indexes.push_back(ConstantInt::get(int32Type, 0));

	if (mode == MEMSET)
		recursiveReset(IRB, dst, src, pointedType, int32Type, indexes);
	else
		recursiveCopy(IRB, dst, src, pointedType, int32Type, indexes);
	if(needsLoop)
	{
		// Increment the index
		Value* incrementedIndex = IRB->CreateAdd(index, ConstantInt::get(int32Type, 1));
		// Close the loop for index
		index->addIncoming(incrementedIndex, IRB->GetInsertBlock());
		// Check if we have finished, if not loop again
		Value* finishedLooping=IRB->CreateICmp(CmpInst::ICMP_EQ, elementsCount, incrementedIndex);
		IRB->CreateCondBr(finishedLooping, endBlock, currentBlock);
	}
	else
		IRB->CreateBr(endBlock);
}

// Create a backward loop, the index is increment from elementsCount-1 to 0. IRB already inserts inside currentBlock.
void StructMemFuncLowering::createBackwardLoop(IRBuilder<>* IRB, BasicBlock* previousBlock, BasicBlock* endBlock, BasicBlock* currentBlock,
						Type* pointedType, Value* dst, Value* src, Value* elementsCount)
{
	Type* int32Type = IntegerType::get(previousBlock->getContext(), 32);
	bool needsLoop = !isa<ConstantInt>(elementsCount) || cast<ConstantInt>(elementsCount)->getZExtValue() != 1;
	// We need an index variable, that may be zero or a PHI
	PHINode* indexPHI = NULL;
	Value* index = NULL;
	if(needsLoop)
	{
		indexPHI=IRB->CreatePHI(int32Type, 2);
		// Start from elementsCount if coming from the previous block, later on we will add the modified index
		indexPHI->addIncoming(elementsCount, previousBlock);
		index = indexPHI;
	}
	else
		index = elementsCount;
		
	// Immediately decrement by one, so that we are accessing a valid element
	Value* decrementedIndex = IRB->CreateSub(index, ConstantInt::get(int32Type, 1));
	// Now, recursively descend into the object to copy all the values
	SmallVector<Value*, 8> indexes;
	indexes.push_back(decrementedIndex);
	recursiveCopy(IRB, dst, src, pointedType, int32Type, indexes);
	if(needsLoop)
	{
		// Close the loop for index
		indexPHI->addIncoming(decrementedIndex, currentBlock);
		// Check if we have finished, if not loop again
		Value* finishedLooping=IRB->CreateICmp(CmpInst::ICMP_EQ, ConstantInt::get(int32Type, 0), decrementedIndex);
		IRB->CreateCondBr(finishedLooping, endBlock, currentBlock);
	}
	else
		IRB->CreateBr(endBlock);
}

bool StructMemFuncLowering::isDoubleAggregate(llvm::Type* t)
{
	if(StructType* ST = dyn_cast<StructType>(t))
	{
		for(uint32_t i=0;i<ST->getNumElements();i++)
		{
			if(!isDoubleAggregate(ST->getElementType(i)))
				return false;
		}
		return true;
	}
	else if(ArrayType* AT = dyn_cast<ArrayType>(t))
	{
		return isDoubleAggregate(AT->getElementType());
	}
	else if(t->isDoubleTy())
		return true;
	else
		return false;
}

bool StructMemFuncLowering::createLoops(llvm::BasicBlock& BB, llvm::BasicBlock* endLoop, llvm::Type* int32Type, llvm::Value* src, llvm::Value* dst, llvm::Value* size, llvm::Type* pointedType, MODE mode)
{
	assert(dst->getType() == src->getType() || mode==MEMSET);
	uint32_t byteSize = DL->getTypeAllocSize(pointedType);
	IRBuilder<> IRB(&BB);
	Value* fixedSize = IRB.CreateZExtOrTrunc(size, int32Type);
	Value* elementsCount=IRB.CreateUDiv(fixedSize, ConstantInt::get(int32Type, byteSize));
	Value* countIsZero=IRB.CreateICmp(CmpInst::ICMP_EQ, elementsCount, ConstantInt::get(int32Type, 0));
	// Handle the cases when countIsZero is a constant. This pass is the very last one in the LTO phase and inefficient code is left otherwise.
	ConstantInt* constantCondition = dyn_cast<ConstantInt>(countIsZero);
	if(constantCondition && !constantCondition->isZeroValue())
	{
		// Nothing to do
		return false;
	}
	BasicBlock* memfuncBody=BasicBlock::Create(BB.getContext(), "memfunc.body", BB.getParent());
	if(constantCondition && constantCondition->isZeroValue())
		IRB.CreateBr(memfuncBody);
	else
		IRB.CreateCondBr(countIsZero, endLoop, memfuncBody);
	IRB.SetInsertPoint(memfuncBody);
	if (mode == MEMMOVE)
	{
		// For memmove we need to check the relative ordering of src and dst and select a direction accordingly
		Value* srcAfterDst = IRB.CreateICmp(CmpInst::ICMP_UGE, src, dst);
		// Create two basic blocks, one is for the forward case, the other for the backward case
		BasicBlock* memmoveForward=BasicBlock::Create(BB.getContext(), "memmove.forward", BB.getParent());
		BasicBlock* memmoveBackward=BasicBlock::Create(BB.getContext(), "memmove.backward", BB.getParent());
		IRB.CreateCondBr(srcAfterDst, memmoveForward, memmoveBackward);
		// Do the forward side
		IRB.SetInsertPoint(memmoveForward);
		createForwardLoop(&IRB, memfuncBody, endLoop, memmoveForward, pointedType, dst, src, elementsCount, mode);
		// Do the backward side
		IRB.SetInsertPoint(memmoveBackward);
		createBackwardLoop(&IRB, memfuncBody, endLoop, memmoveBackward, pointedType, dst, src, elementsCount);
	}
	else //if(mode == MEMCPY || mode == MEMSET)
		createForwardLoop(&IRB, &BB, endLoop, memfuncBody, pointedType, dst, src, elementsCount, mode);
	return true;
}

bool StructMemFuncLowering::runOnBlock(BasicBlock& BB, bool asmjs)
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
		//We want to decompose everything which is not a byte layout structure. memset is always decomposed.
		if(mode != MEMSET)
		{
			bool isByteLayout = isa<StructType>(pointedType) && cast<StructType>(pointedType)->hasByteLayout();
			if(isByteLayout)
				continue;
		}

		//We have a typed mem func on a struct
		//Decompose it in a loop
		Value* dst=CI->getOperand(0);
		//In MEMSET mode src is the value to be written
		Value* src=CI->getOperand(1);
		Value* size=CI->getOperand(2);
		Type* int32Type = IntegerType::get(BB.getContext(), 32);
		// Do not inline memory intrinsics with a large or non-constant size
		// argument, when in linear memory mode.
		// Also, if the memcpy is nicely aligned, use 32 bit loads and stores
		if (asmjs) {
			ConstantInt *sizeConst = dyn_cast<ConstantInt>(size);
			if (!sizeConst || sizeConst->getZExtValue() > INLINE_WRITE_LOOP_MAX)
				continue;
			uint32_t sizeInt = sizeConst->getZExtValue();
			uint32_t alignInt = cast<ConstantInt>(CI->getOperand(3))->getZExtValue();
			assert(alignInt != 0);
			uint32_t elemSize = 1;
			// Take advantage of double moves when available
			if (alignInt % 8 == 0 && sizeInt >= 8 &&
					(mode==MEMSET ?
						// For MEMSET the dst must be compatible, but also allow constant 0
						isDoubleAggregate(dst->getType()->getPointerElementType()) || (isa<Constant>(src) && cast<Constant>(src)->isNullValue()) :
						// Otherwise dst and src must be compatible
						isDoubleAggregate(dst->getType()->getPointerElementType()) && isDoubleAggregate(src->getType()->getPointerElementType()))) {
				Type* doubleType = Type::getDoubleTy(BB.getContext());
				pointedType = doubleType;
				elemSize = 8;
			} else if (alignInt % 4 == 0 && sizeInt >= 4) {
				pointedType = int32Type;
				elemSize = 4;
			} else if (alignInt % 2 == 0 && sizeInt >= 2) {
				pointedType = IntegerType::get(BB.getContext(), 16);
				elemSize = 2;
			}
			if (elemSize > 1) {
				IRBuilder<> IRB(CI);
				dst = IRB.CreateBitCast(dst, pointedType->getPointerTo());
				// In MEMSET mode src is the i8 value to write
				if(mode != MEMSET)
					src = IRB.CreateBitCast(src, pointedType->getPointerTo());
				// We have found a good alignment above, check if we need to split the intrinsic to deal with an unaligned tail
				if(uint32_t tailSize = sizeInt % elemSize) {
					IRBuilder<> IRB(CI->getNextNode());
					uint32_t skipCount = sizeInt / elemSize;
					llvm::Value* tailDst = IRB.CreateGEP(dst, ConstantInt::get(int32Type, skipCount));
					llvm::Value* tailSrc = src;
					if(mode != MEMSET)
						tailSrc = IRB.CreateGEP(src, ConstantInt::get(int32Type, skipCount));
					uint32_t newAlign = alignInt;
					// The tail operation starts at a multiple of elemSize
					while(elemSize % newAlign != 0)
						newAlign /= 2;
					if(mode == MEMCPY)
						IRB.CreateMemCpy(tailDst, tailSrc, tailSize, newAlign);
					else if(mode == MEMMOVE)
						IRB.CreateMemMove(tailDst, tailSrc, tailSize, newAlign);
					else //if(mode == MEMSET)
						IRB.CreateMemSet(tailDst, tailSrc, tailSize, newAlign);
					size = ConstantInt::get(int32Type, sizeInt - tailSize);
				}
			}
		}
		//First of all split the original block
		BasicBlock* endLoop = BB.splitBasicBlock(it);
		//Add the lower part of the block to be anaysized later on
		basicBlocks.push_back(endLoop);
		//Now BB is linked to endblock by an unconditional jump
		//Instead we need to check if we need to enter the loop
		Instruction* oldBranch = BB.getTerminator();
		if(createLoops(BB, endLoop, int32Type, src, dst, size, pointedType, mode)) {
			//Delete the old branch
			oldBranch->eraseFromParent();
		}
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
		runOnBlock(*BB, F.getSection() == StringRef("asmjs"));
	}
	return true;
}

char StructMemFuncLowering::ID = 0;

FunctionPass *llvm::createStructMemFuncLowering() { return new StructMemFuncLowering(); }

INITIALIZE_PASS_BEGIN(StructMemFuncLowering, "StructMemFuncLowering", "Lower memory intrinsics for structure types",
                      false, false)
INITIALIZE_PASS_END(StructMemFuncLowering, "StructMemFuncLowering", "Lower memory intrinsics for structure types",
                    false, false)
