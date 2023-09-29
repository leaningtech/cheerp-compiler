//===-- Cheerp/StructMemFuncLowering.h - Cheerp utility code ---------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2014-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef CHEERP_STRUCT_MEMFUNC_LOWERING_H
#define CHEERP_STRUCT_MEMFUNC_LOWERING_H

#include "llvm/IR/PassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"

namespace llvm
{

class StructMemFuncLowering
{
private:
	enum MODE { NONE = 0, MEMCPY, MEMMOVE, MEMSET };
	bool runOnBlock(BasicBlock& BB, bool asmjs);
	// If 'aliasScopes' is not null it will be used to mark stores as not-aliasing for any previously loaded element
	void recursiveCopy(IRBuilder<>* IRB, Value* baseDst, Value* baseSrc, Type* curType, Type* containingType, Type* indexType, uint32_t baseAlign, SmallVector<Value*, 8>& indexes,
				SmallVector<Metadata*, 8>& aliasScopes, MDNode* aliasDomain);
	void recursiveReset(IRBuilder<>* IRB, Value* baseDst, Value* resetVal, Type* curType, Type* containingType, Type* indexType, uint32_t baseAlign, SmallVector<Value*, 8>& indexes);
	void createMemFunc(IRBuilder<>* IRB, Value* baseDst, Value* baseSrc, Type* containingType, size_t size, SmallVector<Value*, 8>& indexes);
	void createBackwardLoop(IRBuilder<>* IRB, BasicBlock* BB, BasicBlock* endLoop, BasicBlock* memfuncBody,
				Type* pointedType, Value* dst, Value* src, Value* elementsCount, uint32_t baseAlign);
	void createForwardLoop(IRBuilder<>* IRB, BasicBlock* BB, BasicBlock* endLoop, BasicBlock* memfuncBody,
				Type* pointedType, Value* dst, Value* src, Value* elementsCount, MODE mode, uint32_t baseAlign);
	void createGenericLoop(IRBuilder<>* IRB, BasicBlock* BB, BasicBlock* endLoop, BasicBlock* memfuncBody,
				Type* pointedType, Value* dst, Value* src, Value* elementsCount, MODE, uint32_t baseAlign, const bool isForward);
	bool createLoops(BasicBlock& BB, BasicBlock* endLoop, Type* int32Type, Value* src, Value* dst, Value* size, Type* pointedType, MODE mode, uint32_t baseAlign);
	uint32_t computeElemAlign(uint32_t baseAlign, uint32_t baseOffset);
	SmallVector<BasicBlock*, 10> basicBlocks;
	const DataLayout* DL;
public:
	explicit StructMemFuncLowering() : DL(NULL) { }
	bool runOnFunction(Function &F);
};

//===----------------------------------------------------------------------===//
//
// StructMemFuncLowering - This pass converts memcpy/memmove/memset to an explicit
// loop of instructions if the arguments are StructTypes
//
class StructMemFuncLoweringPass : public PassInfoMixin<StructMemFuncLoweringPass> {
public:
	PreservedAnalyses run(Function& F, FunctionAnalysisManager& FAM);
	static bool isRequired() { return true;}
};


}

#endif
