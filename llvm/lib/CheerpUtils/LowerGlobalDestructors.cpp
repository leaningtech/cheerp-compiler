//===-- ThreadLocalLowering.h - Cheerp helper -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2025 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include <unordered_set>
#include "llvm/Cheerp/LowerGlobalDestructors.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Module.h"
#include "llvm/Transforms/Utils/LowerGlobalDtors.h"

using namespace llvm;

namespace cheerp
{

void LowerGlobalDestructorsPass::filterGenericJSDestructors(Module& M)
{
	GlobalVariable* var = M.getGlobalVariable("llvm.global_dtors");
	if (!var || !var->hasInitializer())
		return;
	ConstantArray* initList = dyn_cast<ConstantArray>(var->getInitializer());
	if (!initList)
		return;

	GlobalValue::LinkageTypes linkageTypes = var->getLinkage();
	SmallVector<Constant*, 8> validDestructors;
	Type* elementType = nullptr;

	// Loop over the global destructors, put all the ones tagged asmjs into a new list.
	for (Value* O: initList->operands())
	{
		ConstantStruct* CS = dyn_cast<ConstantStruct>(O);
		if (!CS)
			continue;

		Constant* destructor = CS->getOperand(1);
		if (destructor->isNullValue())
			break;

		elementType = O->getType();
		Function* destructorFunc = dyn_cast<Function>(destructor->stripPointerCastsSafe());
		assert(destructorFunc);
		if (destructorFunc->getSection() == "asmjs")
			validDestructors.push_back(cast<Constant>(O));
	}

	var->eraseFromParent();
	uint32_t numElements = validDestructors.size();
	// If there are no valid destructors, don't create a new global.
	if (numElements == 0)
		return;

	ArrayType* arrayType = ArrayType::get(elementType, numElements);
	Constant* newInitList = ConstantArray::get(arrayType, validDestructors);
	new GlobalVariable(M, arrayType, true, linkageTypes, newInitList, "llvm.global_dtors");
}

PreservedAnalyses LowerGlobalDestructorsPass::run(Module& M, ModuleAnalysisManager& MAM)
{
	LowerGlobalDtorsPass LGDP;

	// Collect all currently existing functions in a set.
	std::unordered_set<Function*> functionsBeforePass;
	for (Function& F: M.getFunctionList())
		functionsBeforePass.insert(&F);

	// Remove the destructors that aren't tagged asmjs.
	filterGenericJSDestructors(M);

	// Run the LowerGlobalDtorsPass.
	PreservedAnalyses PA = LGDP.run(M, MAM);

	// The functions that weren't in the list before are the new functions
	// created by the LowerGlobalDtorsPass. Tag them asmjs.
	for (Function& F: M.getFunctionList())
	{
		if (!functionsBeforePass.count(&F))
			F.setSection("asmjs");
	}

	return PA;
}

}
