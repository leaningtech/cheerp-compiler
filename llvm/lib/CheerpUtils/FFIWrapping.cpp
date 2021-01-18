//===-- FFIWrapping.cpp - Cheerp utility function --------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2020 Leaning Technologies
//
//===----------------------------------------------------------------------===//

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
	BasicBlock* Entry = BasicBlock::Create(M.getContext(),"entry", Wrapper);
	IRBuilder<> Builder(Entry);

	llvm::SmallVector<Value*, 4> params;
	for(auto& arg: Wrapper->args())
		params.push_back(&arg);
	CallInst* ForwardCall = Builder.CreateCall(const_cast<Function*>(Orig), params);
	Value* Ret = ForwardCall->getType()->isVoidTy() ? nullptr : ForwardCall;
	Builder.CreateRet(Ret);

	Wrapper->setSection("asmjs");


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
	// Check argument types
	for (const auto& arg: F->args())
	{
		Type* ty = arg.getType();
		// non pointers are fine (TODO i64 eventually)
		if (!ty->isPointerTy())
			continue;
		// Excluding client pointers (which are always anyref), we support only
		// split regulars, and they always need the wrapper
		if (!TypeSupport::isClientType(ty->getPointerElementType()))
		{
			return true;
		}
	}

	return false;
}

template<typename T>
static void replaceAllUsesWithFiltered(Value* Old, T GetNew,
		const DeterministicFunctionSet& whitelist)
{
	auto UI = Old->use_begin(), E = Old->use_end();
	for (; UI != E;)
	{
		Use &U = *UI;
		++UI;
		if (Instruction* I = dyn_cast<Instruction>(U.getUser()))
		{
			if (!whitelist.count(I->getParent()->getParent()))
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
			}, whitelist);
		}
	}
}

void FFIWrapping::run()
{
	DeterministicFunctionSet newImports;
	for (auto* F: imports)
	{
		if (needsWrapping(F))
		{
			Function* W = wrapImport(M, F);
			newImports.insert(W);
			outsideModule.insert(W);
			replaceAllUsesWithFiltered(const_cast<Function*>(F), [W](IRBuilder<>&) { return W; }, insideModule);
		}
		else
		{
			newImports.insert(F);
		}
	}
	// Replace client globals uses in asmjs with getter function wrappers
	for (GlobalVariable& G: M.globals())
	{
		if (!TypeSupport::isClientGlobal(&G))
			continue;
		replaceAllUsesWithFiltered(&G, [&G, &newImports, this](IRBuilder<>& Builder) {
			Function* W = wrapGlobal(M, &G);
			newImports.insert(W);
			outsideModule.insert(W);
			return Builder.CreateCall(W);
		}, insideModule);
	}
	imports = std::move(newImports);
}

}
