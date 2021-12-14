//===-- InvokeWrapping.cpp - Cheerp backend pass --------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2021 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/InvokeWrapping.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"

namespace cheerp {

using namespace llvm;

static CallInst* copyInvokeToCall(InvokeInst* IV)
{
      SmallVector<Value *, 16> CallArgs(IV->arg_begin(), IV->arg_end());
      SmallVector<OperandBundleDef, 1> OpBundles;
      IV->getOperandBundlesAsDefs(OpBundles);
      // Insert a normal call instruction...
      CallInst *NewCall =
          CallInst::Create(IV->getFunctionType(), IV->getCalledOperand(),
                           CallArgs, OpBundles, "", IV);
      NewCall->takeName(IV);
      NewCall->setCallingConv(IV->getCallingConv());
      NewCall->setAttributes(IV->getAttributes());
      NewCall->setDebugLoc(IV->getDebugLoc());
	  return NewCall;
}

static GlobalVariable* getOrInsertHelperGlobal(Module& M)
{
    auto* Ty = llvm::StructType::getTypeByName(M.getContext(), "struct._ZN10__cxxabiv119__cheerp_landingpadE");
	assert(Ty);
	GlobalVariable* G = cast<GlobalVariable>(M.getOrInsertGlobal("__cheerpExceptionHelperGlobal", Ty->getPointerTo(), [&M, Ty]()
	{
		auto* g = new GlobalVariable(M, Ty->getPointerTo(), false, GlobalVariable::ExternalLinkage, ConstantPointerNull::get(Ty->getPointerTo()));
		g->setName("__cheerpExceptionHelperGlobal");
		g->setLinkage(GlobalVariable::ExternalLinkage);
		if (Ty->hasAsmJS())
			g->setSection("asmjs");
		return g;
	}));
	return G;
}

static Function* wrapInvoke(Module& M, InvokeInst& IV, DenseSet<Instruction*>& ToRemove)
{

	FunctionType* Ty = IV.getFunctionType();
	Function* F = IV.getCalledFunction();
	assert(F);
	std::string fname = "__invoke_wrapper__";
	fname += F->getName();
	fname += "__";
	fname += IV.getParent()->getParent()->getName();
	fname += "__";
	fname += IV.getParent()->getName();
	Function* Wrapper = cast<Function>(M.getOrInsertFunction(fname.c_str(), Ty).getCallee());
	assert(Wrapper->empty());
	setForceRawAttribute(M, Wrapper);

	Wrapper->setPersonalityFn(IV.getParent()->getParent()->getPersonalityFn());
	BasicBlock* Entry = BasicBlock::Create(M.getContext(),"entry", Wrapper);
	BasicBlock* Cont = BasicBlock::Create(M.getContext(),"cont", Wrapper);
	BasicBlock* Catch = BasicBlock::Create(M.getContext(),"catch", Wrapper);
	IRBuilder<> Builder(Entry);

	llvm::SmallVector<Value*, 4> params;
	for(auto& arg: Wrapper->args())
		params.push_back(&arg);
	InvokeInst* ForwardInvoke = Builder.CreateInvoke(F, Cont, Catch, params);

	GlobalVariable* Helper = getOrInsertHelperGlobal(M);

	Builder.SetInsertPoint(Cont);
	Value* Ret = ForwardInvoke->getType()->isVoidTy() ? nullptr : ForwardInvoke;
	Builder.CreateStore(ConstantPointerNull::get(cast<PointerType>(Helper->getType()->getPointerElementType())), Helper);
	Builder.CreateRet(Ret);

	Builder.SetInsertPoint(Catch);
	// TODO handle phis for shared landing pads
	if(!IV.getUnwindDest()->getUniquePredecessor())
	{
		IV.getParent()->getParent()->dump();
	}
	assert(IV.getUnwindDest()->getUniquePredecessor());
	LandingPadInst* OldLP = IV.getUnwindDest()->getLandingPadInst();
	LandingPadInst* LP = cast<LandingPadInst>(OldLP->clone());
	Builder.Insert(LP);
	Builder.CreateStore(LP, Helper);
	Ret = ForwardInvoke->getType()->isVoidTy() ? nullptr : UndefValue::get(ForwardInvoke->getType());
	Builder.CreateRet(Ret);

	Builder.SetInsertPoint(&IV);
	CallInst* Call = copyInvokeToCall(&IV);
	Call->setCalledFunction(Wrapper);
	IV.replaceAllUsesWith(Call);
	Value* Ex = Builder.CreateLoad(Helper->getType()->getPointerElementType(), Helper);
	Value* Cond = Builder.CreateICmpEQ(Ex, ConstantPointerNull::get(cast<PointerType>(Ex->getType())));
	IV.getNormalDest()->removePredecessor(IV.getParent());
	IV.getUnwindDest()->removePredecessor(IV.getParent());
	Builder.CreateCondBr(Cond, IV.getNormalDest(), IV.getUnwindDest());

	IV.eraseFromParent();

	Builder.SetInsertPoint(OldLP);
	Ex = Builder.CreateLoad(Helper->Value::getType()->getPointerElementType(), Helper);
	OldLP->replaceAllUsesWith(Ex);
	ToRemove.insert(OldLP);
	// what about resume?

	return Wrapper;
}

static Function* wrapResume(Module& M, ResumeInst* RS, DenseSet<Instruction*>& ToRemove)
{
	Function* CxaResume = M.getFunction("__cxa_resume");
	assert(CxaResume);
	IRBuilder<> Builder(RS);
	Value* LP = RS->getOperand(0);
	if (LP->getType() != CxaResume->getFunctionType()->getParamType(0))
	{
		LP = Builder.CreateBitCast(LP, CxaResume->getFunctionType()->getParamType(0));
	}
	Value* Call = Builder.CreateCall(CxaResume->getFunctionType(), CxaResume, LP);
	RS->replaceAllUsesWith(Call);
	Builder.CreateUnreachable();
	ToRemove.insert(RS);
	return CxaResume;
}


bool InvokeWrapping::runOnModule(Module& M)
{
	GDA = &getAnalysis<cheerp::GlobalDepsAnalyzer>();
	bool Changed = false;

	DenseSet<Instruction*> ToRemove;
	for (Function& F: make_early_inc_range(M.functions()))
	{
		if (F.getSection() != "asmjs")
			continue;
		for (auto& I: make_early_inc_range(instructions(F)))
		{
			if (auto* IV = dyn_cast<InvokeInst>(&I))
			{
				Changed = true;
				bool indirect = IV->isIndirectCall();
				// TODO handle indirect calls
				assert(!indirect);
				bool asmjs = indirect || IV->getCalledFunction()->getSection() == "asmjs";
				Function* W = wrapInvoke(M, *IV, ToRemove);
				if (asmjs)
				{
					GDA->insertAsmJSExport(IV->getCalledFunction());
				}
				GDA->insertAsmJSImport(W);
			} else if(auto* RS = dyn_cast<ResumeInst>(&I)) {
				Changed = true;
				Function* W = wrapResume(M, RS, ToRemove);
				GDA->insertAsmJSImport(W);
			}
		}
	}
	for (auto* I: ToRemove)
	{
		I->eraseFromParent();
	}
	return Changed;
}

void InvokeWrapping::getAnalysisUsage(llvm::AnalysisUsage & AU) const
{
	AU.addRequired<cheerp::GlobalDepsAnalyzer>();
	AU.addPreserved<cheerp::GlobalDepsAnalyzer>();
	llvm::Pass::getAnalysisUsage(AU);
}

char InvokeWrapping::ID = 0;

ModulePass *createInvokeWrappingPass() { return new InvokeWrapping(); }
}
