#include "llvm/Cheerp/CheerpLowerAtomic.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Cheerp/CommandLine.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/Transforms/Scalar/LowerAtomicPass.h"

using namespace llvm;
using namespace cheerp;
// Module pass that invokes the LLVM LowerAtomicPass on genericjs functions.
PreservedAnalyses CheerpLowerAtomicPass::run(Module& M, ModuleAnalysisManager& MAM)
{
	FunctionAnalysisManager& FAM = MAM.getResult<FunctionAnalysisManagerModuleProxy>(M).getManager();
	FunctionPassManager FPM;
	FPM.addPass(LowerAtomicPass());

	// Loop over the functions, and only pass genericjs ones to LowerAtomicPass
	for (Function& F : M)
	{
		if (F.isDeclaration())
			continue;

		if (!LowerAtomics && F.getSection() == "asmjs")
			continue;

		FPM.run(F, FAM);
	}

	// Replace thread locals with actual globals
	// NOTE: this could go in its own pass
	for (GlobalVariable& G: M.globals())
	{
		if (!LowerAtomics && G.getSection() == "asmjs")
			continue;

		if (!G.isThreadLocal())
		{
			continue;
		}
		G.setThreadLocalMode(GlobalVariable::NotThreadLocal);
		for (auto& U: make_early_inc_range(G.uses()))
		{
			if (auto* C = dyn_cast<CallBase>(U.getUser()))
			{
				if (!C->getCalledFunction() || C->getCalledFunction()->getIntrinsicID() != Intrinsic::threadlocal_address)
				{
					continue;
				}
				C->replaceAllUsesWith(&G);
				C->eraseFromParent();
			}
		}
	}

	return PreservedAnalyses::none();
}
