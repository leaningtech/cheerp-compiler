//===-- FFIWrapping.cpp - Cheerp backend pass --------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2020-2021 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/FFIWrapping.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"

namespace cheerp {

using namespace llvm;

static Function* wrapImport(Module& M, const Function* Orig)
{
	FunctionType* Ty = Orig->getFunctionType();
	Function* Wrapper = cast<Function>(M.getOrInsertFunction(Twine("__wrapper__",Orig->getName()).str(), Ty).getCallee());

	setForceRawAttribute(M, Wrapper);

	BasicBlock* Entry = BasicBlock::Create(M.getContext(),"entry", Wrapper);
	IRBuilder<> Builder(Entry);

	llvm::SmallVector<Value*, 4> params;
	for(auto& arg: Wrapper->args())
		params.push_back(&arg);
	CallInst* ForwardCall = Builder.CreateCall(const_cast<Function*>(Orig), params);
	Type* RetTy = ForwardCall->getType();
	Value* Ret = RetTy->isVoidTy() ? nullptr : ForwardCall;
	if (Ret && RetTy->isPointerTy() && !TypeSupport::isAsmJSPointer(RetTy) && !TypeSupport::isClientType(RetTy))
	{
		Function* MakeRegular = Intrinsic::getDeclaration(&M, Intrinsic::cheerp_make_regular, { RetTy, RetTy });
		Function* PointerOffset = Intrinsic::getDeclaration(&M, Intrinsic::cheerp_pointer_offset, { RetTy });
		Value* Off = Builder.CreateCall(PointerOffset->getFunctionType(), PointerOffset, { Ret });
		Ret = Builder.CreateCall(MakeRegular->getFunctionType(), MakeRegular, { Ret, Off} );
	}
	Builder.CreateRet(Ret);

	// Replace all uses inside the 
	return Wrapper;
}

static Function* wrapGlobal(Module& M, GlobalVariable* G)
{
	Type* RetTy = G->getType();
	FunctionType* FTy = FunctionType::get(RetTy, false);
	Function* Wrapper = cast<Function>(M.getOrInsertFunction(Twine("__wrapper__",G->getName()).str(), FTy).getCallee());
	if (!Wrapper->empty())
		return Wrapper;
	BasicBlock* Entry = BasicBlock::Create(M.getContext(),"entry", Wrapper);
	IRBuilder<> Builder(Entry);
	Builder.CreateRet(G);

	return Wrapper;
}

static bool needsWrapping(const Function* F)
{
	// Client non-static methods always need wrapping
	if (TypeSupport::isClientFunc(F) && !F->hasFnAttribute(Attribute::Static))
	{
		//Attribute::Static is attached both to static methods AND free functions, so only methods do not have it
		return true;
	}
	// Client native constructors always need wrapping
	if (TypeSupport::isClientConstructorName(F->getName()))
		return true;
	// If the function is not implemented and not client, avoid the wrapper:
	// we will import the dummy function
	if (!TypeSupport::isClientFunc(F) && F->empty())
		return false;
	auto typeRequiresWrapper = [](Type* ty)
	{
		// non pointers are fine (TODO i64 eventually)
		if (!ty->isPointerTy())
			return false;
		// pointers to asmjs types are fine
		if (TypeSupport::isAsmJSPointer(ty))
			return false;
		// Excluding client pointers (which are always anyref), we support only
		// split regulars, and they always need the wrapper
		if (TypeSupport::isClientType(ty->getPointerElementType()))
			return false;
		return true;
	};
	// Check argument types
	for (const auto& arg: F->args())
	{
		Type* ty = arg.getType();
		if (typeRequiresWrapper(ty))
			return true;
	}
	// Check return value. This is only relevant for functions injected by the compiler,
	// since we forbid calls to genericjs functions that return basic pointer types
	// from wasm
	if (typeRequiresWrapper(F->getReturnType()))
		return true;

	return false;
}

template<typename T, typename F>
static void replaceAllUsesWithFiltered(Value* Old, T GetNew,
		F filter)
{
	auto UI = Old->use_begin(), E = Old->use_end();
	for (; UI != E;)
	{
		Use &U = *UI;
		++UI;
		if (Instruction* I = dyn_cast<Instruction>(U.getUser()))
		{
			if (!filter(I->getParent()->getParent()))
				continue;
			IRBuilder<> Builder(I);
			Value* New = GetNew(Builder);
			U.set(New);
		}
		else if (ConstantExpr* CE = dyn_cast<ConstantExpr>(U.getUser()))
		{
			// We can only see bitcasts here since for globals we
			// allow only client types, which are opaque, and functions
			// can only be bitcasted
			assert(CE->getOpcode() == Instruction::BitCast);
			replaceAllUsesWithFiltered<std::function<Value*(IRBuilder<>&)>>(CE, [&GetNew, CE](IRBuilder<>& Builder) {
				Value* New = GetNew(Builder);
				return Builder.CreateBitCast(New, CE->getType());
			}, filter);
		}
	}
}

bool FFIWrapping::runOnModule(Module& M)
{
	auto& GDA = getAnalysis<cheerp::GlobalDepsAnalyzer>();
	bool Changed = false;
	std::vector<const Function*> newImports;
	std::vector<const Function*> oldImports;
	auto insideModule = [](Function* f)
	{
		return f->getSection() == "asmjs";
	};
	for (auto* F: GDA.asmJSImports())
	{
		if (needsWrapping(F))
		{
			Changed = true;
			Function* W = wrapImport(M, F);
			newImports.push_back(W);
			oldImports.push_back(F);
			replaceAllUsesWithFiltered(const_cast<Function*>(F), [W](IRBuilder<>&) { return W; }, insideModule);
		}
	}
	// Replace client globals uses in asmjs with getter function wrappers
	for (GlobalVariable& G: M.globals())
	{
		if (!TypeSupport::isClientGlobal(&G))
			continue;
		Changed = true;
		replaceAllUsesWithFiltered(&G, [&G, &newImports, &M](IRBuilder<>& Builder) {
			Function* W = wrapGlobal(M, &G);
			newImports.push_back(W);
			return Builder.CreateCall(W);
		}, insideModule);
	}
	for (auto* W: newImports)
		GDA.insertAsmJSImport(W);
	for (auto* O: oldImports)
		GDA.removeAsmJSImport(O);

	return Changed;
}

void FFIWrapping::getAnalysisUsage(llvm::AnalysisUsage & AU) const
{
	AU.addRequired<cheerp::GlobalDepsAnalyzer>();
	AU.addPreserved<cheerp::GlobalDepsAnalyzer>();
	llvm::Pass::getAnalysisUsage(AU);
}

char FFIWrapping::ID = 0;

ModulePass *createFFIWrappingPass() { return new FFIWrapping(); }
}
