//===-- AllocateArrayLowering.cpp - Cheerp optimization pass --------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2018 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "CheerpAllocateArrayLowering"
#include "llvm/Cheerp/AllocateArrayLowering.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Cheerp/Utility.h"

STATISTIC(NumAllocateArrayLowered, "Number of cheerp_allocate_array lowered");
STATISTIC(NumDeallocateArrayLowered, "Number of cheerp_deallocate_array lowered");
STATISTIC(NumGetArrayLenLowered, "Number of cheerp_get_array_len lowered");

namespace llvm {

bool AllocateArrayLowering::runOnFunction(Function& F)
{
	Module* M = F.getParent();
	DataLayout targetData(M);
	Type* int32Ty = IntegerType::getInt32Ty(M->getContext());
	Type* int8Ty = IntegerType::getInt8Ty(M->getContext());

	bool asmjs = F.getSection() == StringRef("asmjs");
	
	bool modified = false;
	for ( BasicBlock & BB : F )
	{
		for ( BasicBlock::iterator it = BB.begin(); it != BB.end();)
		{
			Instruction* cur = it;
			it++;
			if (CallInst * ci = dyn_cast<CallInst>(cur))
			{
				Function* called = ci->getCalledFunction();
				if (called && called->getIntrinsicID() == Intrinsic::cheerp_allocate_array) {
					NumAllocateArrayLowered++;
					PointerType* allocType = cast<PointerType>(ci->getType());
					Function* Allocate = Intrinsic::getDeclaration(M, Intrinsic::cheerp_allocate, {allocType});
					if (!asmjs && !cheerp::TypeSupport::isAsmJSPointer(ci->getType())) {
						BasicBlock::iterator ii(ci);
						Instruction* repl = CallInst::Create(Allocate, {ci->getOperand(0)});
						ReplaceInstWithInst(BB.getInstList(), ii, repl);
						continue;
					}

					Value* origSize = ci->getOperand(0);
					uint32_t typeSize = targetData.getTypeAllocSize(allocType->getPointerElementType());
					int32_t cookieSize = cheerp::TypeSupport::getArrayCookieSizeAsmJS(targetData, allocType->getPointerElementType());
					IRBuilder<> Builder(ci);
					Value* size = Builder.CreateAdd(origSize, ConstantInt::get(int32Ty, cookieSize));
					Value* alloc = Builder.CreateCall(Allocate, {size});
					Value* numElems = Builder.CreateUDiv(origSize, ConstantInt::get(int32Ty, typeSize));
					Value* cookieAddr = Builder.CreateBitCast(alloc, int32Ty->getPointerTo());
					Builder.CreateStore(numElems, cookieAddr);
					Value* ret = Builder.CreateGEP(cookieAddr, ConstantInt::get(int32Ty, cookieSize/sizeof(int32_t)));
					ret = Builder.CreateBitCast(ret, allocType);

					BasicBlock::iterator ii(ci);
					ReplaceInstWithValue(BB.getInstList(), ii, ret);

				} else if (called && called->getIntrinsicID() == Intrinsic::cheerp_deallocate_array) {
					NumDeallocateArrayLowered++;
					Value* allocStart = ci->getOperand(0);
					Type* allocType = allocStart->getType();
					Function* Deallocate = Intrinsic::getDeclaration(M, Intrinsic::cheerp_deallocate, {allocType});
					if (!asmjs && !cheerp::TypeSupport::isAsmJSPointer(allocType)) {
						BasicBlock::iterator ii(ci);
						Instruction* repl = CallInst::Create(Deallocate, ci->getOperand(0));
						ReplaceInstWithInst(BB.getInstList(), ii, repl);
						continue;
					}

					IRBuilder<> Builder(ci);
					int32_t cookieSize = cheerp::TypeSupport::getArrayCookieSizeAsmJS(targetData, allocType->getPointerElementType());
					allocStart = Builder.CreateBitCast(allocStart, int8Ty->getPointerTo());
					allocStart = Builder.CreateGEP(allocStart, ConstantInt::get(int32Ty, -cookieSize));
					allocStart = Builder.CreateBitCast(allocStart, allocType);
					Builder.CreateCall(Deallocate, {allocStart});
					ci->eraseFromParent();
				} else if (called && called->getIntrinsicID() == Intrinsic::cheerp_get_array_len) {
					if (!asmjs) continue;

					NumGetArrayLenLowered++;
					IRBuilder<> Builder(ci);
					Value* allocStart = ci->getOperand(0);
					Type* allocType = allocStart->getType();
					int32_t cookieSize = cheerp::TypeSupport::getArrayCookieSizeAsmJS(targetData, allocType->getPointerElementType());
					allocStart = Builder.CreateBitCast(allocStart, int32Ty->getPointerTo());
					Value* cookieLoc = Builder.CreateGEP(allocStart, ConstantInt::get(int32Ty, -cookieSize/sizeof(int32_t)));
					Value* cookie = Builder.CreateLoad(cookieLoc);

					BasicBlock::iterator ii(ci);
					ReplaceInstWithValue(BB.getInstList(), ii, cookie);
				}
			}
		}
	}

	return modified;
}

const char* AllocateArrayLowering::getPassName() const
{
	return "AllocateArrayLowering";
}

char AllocateArrayLowering::ID = 0;

void AllocateArrayLowering::getAnalysisUsage(AnalysisUsage & AU) const
{
	llvm::Pass::getAnalysisUsage(AU);
}

FunctionPass *createAllocateArrayLoweringPass() { return new AllocateArrayLowering(); }

}
