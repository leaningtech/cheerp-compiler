#include "llvm/Cheerp/SIMDLowering.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/LinearMemoryHelper.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/IRBuilder.h"

using namespace llvm;

namespace cheerp
{

PreservedAnalyses SIMDLoweringPass::run(Function& F, FunctionAnalysisManager& FAM)
{
	for (auto it = F.begin(); it != F.end(); it++)
	{
		BasicBlock& BB = *it;
		for (Instruction& I: BB)
		{
			if (I.getOpcode() == Instruction::ExtractElement && !isa<ConstantInt>(I.getOperand(1)))
			{
				Value* vec = I.getOperand(0);
				IRBuilder<> Builder(&I);
				IntegerType *Int32Ty = Builder.getInt32Ty();
				BasicBlock* newBlock = BB.splitBasicBlock(&I, BB.getName() + ".afterswitch");
				BB.getTerminator()->eraseFromParent();
				Builder.SetInsertPoint(&BB);
				std::vector<BasicBlock*> switchBlocks;
				for (int i = 0; i < 4; i++)
					switchBlocks.push_back(BasicBlock::Create(Builder.getContext(), "switch", &F));
				SwitchInst* Switch = Builder.CreateSwitch(I.getOperand(1), switchBlocks[0], 4);
				for (int i = 1; i < 4; i++)
					Switch->addCase(ConstantInt::get(Int32Ty, i), switchBlocks[i]);
				std::vector<Value*> values;
				for (int i = 0; i < 4; i++)
				{
					Builder.SetInsertPoint(switchBlocks[i]);
					values.push_back(Builder.CreateExtractElement(vec, i));
					Builder.CreateBr(newBlock);
				}
				Builder.SetInsertPoint(&I);
				PHINode* phi = Builder.CreatePHI(I.getType(), 4);
				for (int i = 0; i < 4; i++)
					phi->addIncoming(values[i], switchBlocks[i]);
				I.replaceAllUsesWith(phi);
				I.eraseFromParent();
				break;
			}
		}
	}
	PreservedAnalyses PA;
	PA.preserve<cheerp::GlobalDepsAnalysis>();
	PA.preserve<cheerp::LinearMemoryAnalysis>();
	return PA;
}

}
