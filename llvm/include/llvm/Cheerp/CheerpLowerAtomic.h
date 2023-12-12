#ifndef CHEERP_LOWER_ATOMIC_H
#define CHEERP_LOWER_ATOMIC_H

#include "llvm/IR/PassManager.h"

namespace cheerp
{

class CheerpLowerAtomicPass : public llvm::PassInfoMixin<CheerpLowerAtomicPass>
{
public:
	llvm::PreservedAnalyses run(llvm::Module& M, llvm::ModuleAnalysisManager& MAM);
	static bool isRequired() { return true; }
};

}

#endif
