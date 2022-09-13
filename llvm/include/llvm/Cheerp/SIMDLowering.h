#include "llvm/IR/PassManager.h"

namespace cheerp{

using namespace llvm;

class SIMDLoweringPass : public llvm::PassInfoMixin<SIMDLoweringPass> {
public:
	PreservedAnalyses run(llvm::Function& F, FunctionAnalysisManager& FAM);
	static bool isRequired() { return true;}
};

}
