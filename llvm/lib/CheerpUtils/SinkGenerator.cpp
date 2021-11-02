//===-- SinkGenerator.cpp - Optimizations on the control flow graph --------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2021 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Pass.h"
#include "llvm/InitializePasses.h"
#include "llvm/Cheerp/CFGPasses.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/PointerAnalyzer.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/LinearMemoryHelper.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include <set>
#include <map>

namespace cheerp
{

typedef std::vector<int> BlockSignature;

class BlockSignatureHelper
{
private:
	const int maxIteration = 5;
	BlockSignature signature;
	const BasicBlock* currBB;
	const BasicBlock& succBB;
public:
	BlockSignatureHelper(const BasicBlock& succBB)
		: succBB(succBB)
	{
	}
	void analyze(const Instruction& I)
	{
		//Opcodes should match
		signature.push_back(I.getOpcode() + isOpcode);
	}
	void analyze(const BasicBlock* bb)
	{
		currBB = bb;
		signature.clear();
		int currIteration = 0;
		//Iterate over the last maxIteration instructions of the BB from the terminator upward
		for (auto iter = currBB->rbegin(); iter != currBB->rend() && currIteration < maxIteration; iter++, currIteration++)
		{
			analyze(*iter);
		}
	}
	BlockSignature getSignature() const
	{
		return signature;
	}
};

static BlockSignature computeSignature(BlockSignatureHelper& helper, const BasicBlock* pred)
{
	helper.analyze(pred);

	return helper.getSignature();
}

static std::vector<std::vector<BasicBlock*>> groupIncoming(const std::vector<BasicBlock*> preds, const BasicBlock* succ)
{
	typedef std::pair<BlockSignature, BasicBlock*> BBSignature;
	std::vector<BBSignature> signatureCollection;

	BlockSignatureHelper helper(*succ);

	//For every unconditional predecessor, we compute its 'signature'
	//BB with equal signature will be grouped together
	//The details of how the signature is compute are to be found in BlockSignatureHelper logic
	for (BasicBlock* BB : preds)
		signatureCollection.push_back({computeSignature(helper, BB), BB});

	//Now we assume signature are deterministic and we group BBs with the same signature
	//Here we require that signatures are totaly ordered to simplify the algorithm to sort + group
	std::sort(signatureCollection.begin(), signatureCollection.end(),
		[](const BBSignature& left, const BBSignature& right) -> bool {
			return left.first < right.first;
		}
	);

	std::vector<std::vector<BasicBlock*>> groups;
	std::vector<BasicBlock*> curr;
	BBSignature* last = nullptr;

	for (auto& P : signatureCollection)
	{
		if (curr.empty())
			last = &P;

		if (last->first != P.first)
		{
			if (curr.size())
			{
				//We are not part of the last group
				//So add it to the list of groups
				groups.push_back(curr);
			}
			curr.clear();
		}

		last = &P;
		curr.push_back(P.second);
	}

	//Add also the last remaining group
	groups.push_back(curr);

	return groups;
}

void SinkGenerator::addSingleSinkTarget(BasicBlock& BB, const std::vector<BasicBlock*>& incomings)
{
	BasicBlock* sinkTarget = BasicBlock::Create(BB.getContext(), "sinkTarget."+BB.getName(), BB.getParent());

	for (PHINode& phi : BB.phis())
	{
		PHINode* sinkTargetPHI = PHINode::Create(phi.getType(), incomings.size(), {"sinkTarget.",phi.getName()});
		for (auto& incomingBB : incomings)
		{
			sinkTargetPHI->addIncoming(phi.getIncomingValueForBlock(incomingBB), incomingBB);
		}
		phi.addIncoming(sinkTargetPHI, sinkTarget);
		for (auto& incomingBB : incomings)
		{
			phi.removeIncomingValue(incomingBB);
			incomingBB->getTerminator()->setSuccessor(0, sinkTarget);
		}
		sinkTarget->getInstList().push_back(sinkTargetPHI);
	}

	//Unconditional branch from sinkTarget to BB
	BranchInst::Create(&BB, sinkTarget);

	for (auto& incomingBB : incomings)
	{
		//Unconditional branches from incomingBB to sinkTarget
		incomingBB->getTerminator()->setSuccessor(0, sinkTarget);
	}
}

void SinkGenerator::addSinkTargets(BasicBlock &BB, const VectorGroupOfBlocks& groups)
{
	for (const auto& incomings : groups)
		addSingleSinkTarget(BB, incomings);
}


void SinkGenerator::examineBasicBlock(BasicBlock& BB, SinkLocationToCreate& groupedIncomings)
{
	unsigned int numUnconditionalPredecessor = 0;
	std::unordered_set <BasicBlock*> preds_set;		//This is just used as filter, so determinism is not relevant
	std::vector<BasicBlock*> preds_vec;

	for (BasicBlock* pred : predecessors(&BB))
	{
		//We are interested only in predecessors that unconditionally got to BB
		if (pred->getUniqueSuccessor() != &BB)
			continue;

		//This should be ideally canonicalized out, but might possibly appear in unoptimized IR
		if (preds_set.insert(pred).second == false)
			continue;

		numUnconditionalPredecessor++;
		preds_vec.push_back(pred);
	}

	std::vector<std::vector<BasicBlock*>> grouped = groupIncoming(preds_vec, &BB);

	std::vector<std::vector<BasicBlock*>> groups;
	for (auto g : grouped)
	{
		if (g.size() > 1 && g.size() < numUnconditionalPredecessor)
			groups.push_back(g);
	}

	if (groups.size() > 0)
		groupedIncomings.push_back({&BB, groups});
}

bool SinkGenerator::runOnFunction(Function& F)
{
	SinkLocationToCreate groupedIncomings;

	//Iterate on BasicBlocks deciding how to group incomings
	//Note that since we are grouping only blocks with unconditional branches
	//The groups that we are going to make are guarantee to be disjunt
	for (BasicBlock& BB : F)
		examineBasicBlock(BB, groupedIncomings);

	bool changed = false;

	//Actually create sink targets and rewire phis
	for (auto& toModify : groupedIncomings)
	{
		changed = true;
		addSinkTargets(*toModify.first, toModify.second);
	}

	return changed;
}
StringRef SinkGenerator::getPassName() const
{
	return "SinkGenerator";
}

char SinkGenerator::ID = 0;

void SinkGenerator::getAnalysisUsage(AnalysisUsage & AU) const
{
	AU.addPreserved<cheerp::GlobalDepsAnalyzer>();
	AU.addPreserved<cheerp::LinearMemoryHelper>();
	llvm::Pass::getAnalysisUsage(AU);
}

FunctionPass *createSinkGeneratorPass() { return new SinkGenerator(); }
}
using namespace cheerp;

INITIALIZE_PASS_BEGIN(SinkGenerator, "SinkGenerator", "Converts 64-bit integer operations into 32-bit ones",
                      false, false)
INITIALIZE_PASS_END(SinkGenerator, "SinkGenerator", "Converts 64-bit integer operations into 32-bit ones",
                    false, false)
