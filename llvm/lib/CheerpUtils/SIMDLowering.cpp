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
	// This pass will find certain instructions that do not allow variables as lane indexes and
	// instead add all the versions of these instructions with a switch.
	for (auto it = F.begin(); it != F.end(); it++)
	{
		BasicBlock& BB = *it;
		for (Instruction& I: BB)
		{
			if (I.getOpcode() == Instruction::ExtractElement && !isa<ConstantInt>(I.getOperand(1)))
			{
				Value* vec = I.getOperand(0);
				assert(vec->getType()->isVectorTy());
				const int amount = 128 / vec->getType()->getScalarSizeInBits();
				IRBuilder<> Builder(&I);
				IntegerType *Int32Ty = Builder.getInt32Ty();
				BasicBlock* newBlock = BB.splitBasicBlock(&I, BB.getName() + ".afterswitch");
				BB.getTerminator()->eraseFromParent();
				Builder.SetInsertPoint(&BB);
				std::vector<BasicBlock*> switchBlocks;
				for (int i = 0; i < amount; i++)
					switchBlocks.push_back(BasicBlock::Create(Builder.getContext(), BB.getName() + ".case", &F));
				SwitchInst* Switch = Builder.CreateSwitch(I.getOperand(1), switchBlocks[0], amount);
				for (int i = 1; i < amount; i++)
					Switch->addCase(ConstantInt::get(Int32Ty, i), switchBlocks[i]);
				std::vector<Value*> values;
				for (int i = 0; i < amount; i++)
				{
					Builder.SetInsertPoint(switchBlocks[i]);
					values.push_back(Builder.CreateExtractElement(vec, i));
					Builder.CreateBr(newBlock);
				}
				Builder.SetInsertPoint(&I);
				PHINode* phi = Builder.CreatePHI(I.getType(), amount);
				for (int i = 0; i < amount; i++)
					phi->addIncoming(values[i], switchBlocks[i]);
				I.replaceAllUsesWith(phi);
				I.eraseFromParent();
				// Since we split the current block, all instructions of this block that we haven't seen yet will be in the next block.
				// We break out of the current loop since the current instruction was erased, and start looping over the next block.
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
