//===-- Cheerp/StructMemFuncLowering.h - Cheerp utility code ---------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef CHEERP_STRUCT_MEMFUNC_LOWERING_H
#define CHEERP_STRUCT_MEMFUNC_LOWERING_H

#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"

namespace llvm
{

class StructMemFuncLowering: public FunctionPass
{
private:
	enum MODE { NONE = 0, MEMCPY, MEMMOVE, MEMSET };
	bool runOnBlock(BasicBlock& BB, bool asmjs);
	void recursiveCopy(IRBuilder<>* IRB, Value* baseDst, Value* baseSrc, Type* curType, Type* indexType, uint32_t baseAlign, SmallVector<Value*, 8>& indexes);
	void recursiveReset(IRBuilder<>* IRB, Value* baseDst, Value* resetVal, Type* curType, Type* indexType, uint32_t baseAlign, SmallVector<Value*, 8>& indexes);
	void createMemFunc(IRBuilder<>* IRB, Value* baseDst, Value* baseSrc, size_t size, SmallVector<Value*, 8>& indexes);
	void createBackwardLoop(IRBuilder<>* IRB, BasicBlock* BB, BasicBlock* endLoop, BasicBlock* memfuncBody,
				Type* pointedType, Value* dst, Value* src, Value* elementsCount, uint32_t baseAlign);
	void createForwardLoop(IRBuilder<>* IRB, BasicBlock* BB, BasicBlock* endLoop, BasicBlock* memfuncBody,
				Type* pointedType, Value* dst, Value* src, Value* elementsCount, MODE mode, uint32_t baseAlign);
	void createGenericLoop(IRBuilder<>* IRB, BasicBlock* BB, BasicBlock* endLoop, BasicBlock* memfuncBody,
				Type* pointedType, Value* dst, Value* src, Value* elementsCount, MODE, uint32_t baseAlign, const bool isForward);
	bool createLoops(BasicBlock& BB, BasicBlock* endLoop, Type* int32Type, Value* src, Value* dst, Value* size, Type* pointedType, MODE mode, uint32_t baseAlign);
	static bool isDoubleAggregate(llvm::Type* t);
	SmallVector<BasicBlock*, 10> basicBlocks;
	const DataLayout* DL;
public:
	static char ID;
	explicit StructMemFuncLowering() : FunctionPass(ID), DL(NULL) { }
	bool runOnFunction(Function &F);
	StringRef getPassName() const;
};

//===----------------------------------------------------------------------===//
//
// StructMemFuncLowering - This pass converts memcpy/memmove/memset to an explicit
// loop of instructions if the arguments are StructTypes
//
FunctionPass *createStructMemFuncLowering();

}

#endif
