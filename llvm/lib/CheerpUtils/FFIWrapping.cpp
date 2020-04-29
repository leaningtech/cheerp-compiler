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
	Function* Wrapper = cast<Function>(M.getOrInsertFunction(Twine("__wrapper__",Orig->getName()).str(), Ty));
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
	Function* Wrapper = cast<Function>(M.getOrInsertFunction(Twine("__wrapper__",G->getName()).str(), FTy));
	if (!Wrapper->empty())
		return Wrapper;
	BasicBlock* Entry = BasicBlock::Create(M.getContext(),"entry", Wrapper);
	IRBuilder<> Builder(Entry);
	Builder.CreateRet(G);

	return Wrapper;
}

static bool needsWrapping(const Function* F)
{
	// Client methods always need wrapping
	// TODO: avoid wrapper for static methods (F->hasFnAttribute(Attribute::Static))
	// It requires some special handling in the writer
	if (TypeSupport::isClientFunc(F))
	{
		// This could still be a free function. Try to xtract the class name
		// from the mangled name. If we succeed, it is a method.
		std::string className;
		std::string funcName;
		std::tie(className, funcName) = TypeSupport::getClientClassAndFunc(F->getName().data());
		bool isMethod = !className.empty();
		if (isMethod)
			return true;
	}
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
		auto *Usr = dyn_cast<Instruction>(U.getUser());
		if (!Usr)
			continue;
		if (!whitelist.count(Usr->getParent()->getParent()))
			continue;
		U.set(GetNew(U));
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
			replaceAllUsesWithFiltered(const_cast<Function*>(F), [W](const Use& U) { return W; }, insideModule);
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
		replaceAllUsesWithFiltered(&G, [&G, &newImports, this](const Use& U) {
			Function* W = wrapGlobal(M, &G);
			newImports.insert(W);
			outsideModule.insert(W);
			CallInst* C = CallInst::Create(W, "", cast<Instruction>(U.getUser()));
			return C;
		}, insideModule);
	}
	imports = std::move(newImports);
}

}
