#include "llvm/Cheerp/CheerpLowerAtomic.h"
#include "llvm/Cheerp/CommandLine.h"
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

	return PreservedAnalyses::none();
}
