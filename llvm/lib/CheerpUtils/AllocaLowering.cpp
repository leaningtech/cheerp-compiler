//===-- AllocaLowering.cpp - Cheerp optimization pass --------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2017-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/AllocaLowering.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/InvokeWrapping.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/PromoteMemToReg.h"
#include "llvm/Support/raw_ostream.h"

#define DEBUG_TYPE "CheerpAllocaLowering"
STATISTIC(NumAllocasTransformedToGEPs, "Number of allocas of values transformed to GEPs in the stack");

namespace cheerp {

using namespace llvm;

static Function* getOrCreateGetStackWrapper(Module* M, cheerp::GlobalDepsAnalyzer& GDA)
{
	Type* i8Ty = IntegerType::getInt8Ty(M->getContext());
	Type* i8PtrTy = PointerType::get(i8Ty, 0);
	FunctionType* fTy = FunctionType::get(i8PtrTy,{});
	Function* wrapper = cast<Function>(M->getOrInsertFunction("__getStackPtr", fTy).getCallee());
	if (!wrapper->empty())
		return wrapper;
	BasicBlock* entry = BasicBlock::Create(M->getContext(),"entry", wrapper);
	IRBuilder<> Builder(entry);
	Function* getStackIntr = Intrinsic::getDeclaration(M, Intrinsic::stacksave);
	Value* ret = Builder.CreateCall(getStackIntr, {}, "savedStack");
	Builder.CreateRet(ret);

	wrapper->setSection("asmjs");
	GDA.insertAsmJSExport(wrapper);
	return wrapper;
}
static Function* getOrCreateSetStackWrapper(Module* M, cheerp::GlobalDepsAnalyzer& GDA)
{
	Type* i8Ty = IntegerType::getInt8Ty(M->getContext());
	Type* i8PtrTy = PointerType::get(i8Ty, 0);
	Type* argTy[] = {i8PtrTy};
	FunctionType* fTy = FunctionType::get(Type::getVoidTy(M->getContext()),ArrayRef<Type*>(argTy,1), false);
	Function* wrapper = cast<Function>(M->getOrInsertFunction("__setStackPtr", fTy).getCallee());
	if (!wrapper->empty())
		return wrapper;
	BasicBlock* entry = BasicBlock::Create(M->getContext(),"entry", wrapper);
	IRBuilder<> Builder(entry);
	Function* setStackIntr = Intrinsic::getDeclaration(M, Intrinsic::stackrestore);
	Value* arg = &*wrapper->arg_begin();
	Builder.CreateCall(setStackIntr, arg);
	Builder.CreateRetVoid();

	wrapper->setSection("asmjs");
	GDA.insertAsmJSExport(wrapper);
	return wrapper;
}

bool AllocaLowering::runOnFunction(Function& F, DominatorTree& DT, cheerp::GlobalDepsAnalyzer& GDA)
{
	Module* M = F.getParent();
	DataLayout targetData(M);
	bool asmjs = F.getSection() == StringRef("asmjs");

	SmallVector<std::pair<AllocaInst*, int32_t>, 8> allocas;
	SmallVector<std::pair<AllocaInst*, Value*>, 8> dynAllocas;
	SmallVector<AllocaInst*, 8> allocasToPromote;
	SmallVector<ReturnInst*, 8> returns;
	SmallVector<CallInst*, 8> vastarts;
	SmallVector<CallInst*, 8> varargCalls;
	Type* Int8Ty = IntegerType::getInt8Ty(M->getContext());

	uint32_t nbytes = 0;
	for ( BasicBlock & BB : F )
	{
		for ( BasicBlock::iterator it = BB.begin(); it != BB.end(); it++ )
		{
			if (AllocaInst * ai = dyn_cast<AllocaInst>(it))
			{
				// Skip if not RAW pointer
				if (!cheerp::TypeSupport::isRawPointer(ai->getType(), asmjs))
				{
					continue;
				}

				Type* allocTy = ai->getAllocatedType();
				// Frontend checks should have made sure that allocas of anyrefs are
				// always promotable to registers, so if we find one, just promote it
				if (allocTy->isPointerTy() && cheerp::TypeSupport::isClientPtrType(cast<PointerType>(allocTy)))
				{
					assert(isAllocaPromotable(ai) && "Alloca of externref not promotable to register");
					allocasToPromote.push_back(ai);
					continue;
				}
				uint32_t size = targetData.getTypeAllocSize(allocTy);

				uint64_t alignment64 = ai->getAlign().value();
				assert(alignment64 <= std::numeric_limits<uint32_t>::max());
				uint32_t asmjsAlignment = cheerp::TypeSupport::getAlignmentAsmJS(targetData, allocTy);
				uint32_t alignment = std::max(static_cast<uint32_t>(alignment64), asmjsAlignment);

				size_t num  = 1;
				if (ai->isArrayAllocation())
				{
					Value* n = ai->getArraySize();
					if (ConstantInt* constN = dyn_cast<ConstantInt>(n))
					{
						num = constN->getZExtValue();
					}
					else
					{
						dynAllocas.push_back(std::make_pair(ai, n));
						continue;
					}
				}
				assert((alignment & (alignment-1)) == 0 && "alignment must be power of 2");
				size_t offset = nbytes + size*num;
				offset = (offset + alignment - 1) & -alignment;
				allocas.push_back(std::make_pair(ai, -offset));
				nbytes = offset;
			}
			else if (ReturnInst * ret = dyn_cast<ReturnInst>(it))
			{
				returns.push_back(ret);
			}
			else if (CallInst * ci = dyn_cast<CallInst>(it))
			{
				Function* calledFunc = ci->getCalledFunction();
				// Add only `vastart`s used in asmjs functions
				if (asmjs && calledFunc && calledFunc->getIntrinsicID() == Intrinsic::vastart)
				{
					vastarts.push_back(ci);
					continue;
				}
				// Skip if both caller and callee not asmjs
				if (!asmjs && (!calledFunc || calledFunc->getSection() != StringRef("asmjs")))
					continue;
				const FunctionType* fTy = ci->getFunctionType();
				if (fTy->isVarArg())
				{
					varargCalls.push_back(ci);
				}
			}
		}
	}
	// Promote stuff
	if (allocasToPromote.size() != 0)
	{
		PromoteMemToReg(allocasToPromote, DT);
	}
	// Nothing else to do
	if (allocas.size() == 0 && dynAllocas.size() == 0 && varargCalls.size() == 0 && vastarts.size() == 0)
		return false;
	// We need to save the stack pointer if we are going to reference memory
	// relative to its position at the beginning of the function (e.g. allocas
	// and varargs)
	bool needFrame = allocas.size() != 0  || vastarts.size() != 0;

	// Keep aligned at 8 bytes
	nbytes = (nbytes + 7) & -8;

	Function *getStack, *setStack;
	if (asmjs)
	{
		getStack = Intrinsic::getDeclaration(M, Intrinsic::stacksave);
		setStack = Intrinsic::getDeclaration(M, Intrinsic::stackrestore);
	}
	else
	{
		getStack = getOrCreateGetStackWrapper(M, GDA);
		setStack = getOrCreateSetStackWrapper(M, GDA);
	}

	Type* int32Ty = IntegerType::getInt32Ty(M->getContext());
	IRBuilder<> Builder(&F.getEntryBlock().front());
	Value* savedStack = nullptr;
	Value* newStack = nullptr;
	if (needFrame)
	{
		savedStack = Builder.CreateCall(getStack, {}, "savedStack");
		newStack = Builder.CreateGEP(Int8Ty, savedStack, ConstantInt::get(int32Ty, -nbytes, true));
		Builder.CreateCall(setStack, newStack);
	}

	// Lower allocas
	for (const auto& a: allocas)
	{
		BasicBlock::iterator ii(a.first);

		Constant* offset = ConstantInt::get(int32Ty, nbytes + a.second, true);
		IRBuilder<> Builder(a.first);
		Value* gep = Builder.CreateGEP(Int8Ty, newStack, offset);
		gep  = Builder.CreateBitCast(gep, a.first->getType());
		ReplaceInstWithValue(a.first->getParent()->getInstList(), ii, gep);

		NumAllocasTransformedToGEPs++;
	}
	// Lower dynamically sized allocas
	for (const auto& a: dynAllocas)
	{
		BasicBlock::iterator ii(a.first);

		IRBuilder<> Builder(a.first);
		uint32_t typeSizeVal = targetData.getTypeAllocSize(a.first->getAllocatedType());
		Value* size = nullptr;
		if(typeSizeVal == 1)
			size = a.second;
		else
		{
			Constant* typeSize = ConstantInt::get(int32Ty,typeSizeVal,false);
			size = Builder.CreateMul(a.second,typeSize);
		}

		uint64_t alignment64 = a.first->getAlign().value();
		assert(alignment64 <= std::numeric_limits<uint32_t>::max());
		Type* allocTy = a.first->getAllocatedType();
		uint32_t asmjsAlignment = cheerp::TypeSupport::getAlignmentAsmJS(targetData, allocTy);
		uint32_t alignment = std::max(std::max(static_cast<uint32_t>(alignment64), asmjsAlignment), 8u);

		Value* stackPtr = Builder.CreateCall(getStack, {});
		Value* addr = Builder.CreateIntToPtr(Builder.CreateAnd(Builder.CreateSub(Builder.CreatePtrToInt(stackPtr, int32Ty), size), ~(alignment - 1)), stackPtr->getType());
		Builder.CreateCall(setStack, addr);
		addr = Builder.CreateBitCast(addr, a.first->getType());
		ReplaceInstWithValue(a.first->getParent()->getInstList(), ii, addr);

		NumAllocasTransformedToGEPs++;
	}

	// Pop the stack frame before rets
	if (needFrame)
	{
		for (Instruction* ret: returns)
		{
			Instruction* MustTailCall = ret->getParent()->getTerminatingMustTailCall();
			IRBuilder<> Builder(MustTailCall? MustTailCall : ret);
			Builder.CreateCall(setStack, savedStack);
		}
	}

	// Lower vastart to a store of the frame pointer
	for (const auto& va: vastarts)
	{
		BasicBlock::iterator ii(va);

		IRBuilder<> Builder(va);
		Value* valist = va->getOperand(0);
		valist = Builder.CreateBitCast(valist, savedStack->getType()->getPointerTo(0));
		Instruction* store = new StoreInst(savedStack, valist, /*isVolatile*/false, Align(1));
		ReplaceInstWithInst(va->getParent()->getInstList(), ii, store);
	}
	// Add stack handling instructions to vararg calls
	for (const auto& ci: varargCalls)
	{
		const FunctionType* fTy = ci->getFunctionType();

		size_t totalParamNum = ci->arg_size();
		size_t fixedParamsNum = fTy->getNumParams();
		size_t varargParamNum = totalParamNum - fixedParamsNum;

		IRBuilder<> Builder(ci);
		Value* stackPtr = Builder.CreateCall(getStack, {});
		// Each argument pushed is 8 bytes in size
		Constant* pushOffset = ConstantInt::get(int32Ty, -varargParamNum*8, false);
		Value* pushedStackPtr = Builder.CreateGEP(Int8Ty, stackPtr, pushOffset);
		Builder.CreateCall(setStack, pushedStackPtr);
		// Calling convention for variadic arguments in asm.js mode:
		// arguments are pushed into the stack in the reverse order
		// in which they appear.
		size_t i = 0;
		for (auto op = ci->op_begin() + totalParamNum - 1; op != ci->op_begin() + fixedParamsNum - 1; op--)
		{
			i++;
			Constant* offset = ConstantInt::get(int32Ty, -i*8, true);
			Value* loc = Builder.CreateGEP(Int8Ty, stackPtr, offset);
			loc = Builder.CreateBitCast(loc, op->get()->getType()->getPointerTo(0));
			Builder.CreateStore(op->get(), loc);
		}
		// Pop the vararg arguments after the call
		IRBuilder<> AfterBuilder(ci->getNextNode());
		AfterBuilder.CreateCall(setStack, stackPtr);
	}

	return true;
}
using namespace llvm;

PreservedAnalyses cheerp::AllocaLoweringInnerPass::run(Function& F, FunctionAnalysisManager& FAM)
{
	AllocaLowering inner;
	DominatorTree& DT = FAM.getResult<DominatorTreeAnalysis>(F);
	if (!inner.runOnFunction(F, DT, GDA))
		return PreservedAnalyses::all();
	
	PreservedAnalyses PA;
	PA.preserve<PointerAnalysis>();
	PA.preserve<RegisterizeAnalysis>();
	PA.preserve<GlobalDepsAnalysis>();
	PA.preserve<DominatorTreeAnalysis>();
	PA.preserve<InvokeWrappingAnalysis>();
	return PA;
}

PreservedAnalyses cheerp::AllocaLoweringPass::run(Module& M, ModuleAnalysisManager& MAM)
{
	FunctionPassManager FPM;

	GlobalDepsAnalyzer& GDA = MAM.getResult<GlobalDepsAnalysis>(M);
	FPM.addPass(AllocaLoweringInnerPass(GDA));

	ModulePassManager MPM;

	MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
	PreservedAnalyses PA = MPM.run(M, MAM);
	return PA;
}

}
