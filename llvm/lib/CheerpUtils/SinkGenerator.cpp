//===-- SinkGenerator.cpp - Optimizations on the control flow graph --------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2021-2022 Leaning Technologies
//
//===----------------------------------------------------------------------===//

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
	const int isOpcode = 1<<28;
	const int isNonInstMap = 2<<28;
	const int isInvalid = 3<<28;
	const int isUser = 4<<28;
	std::unordered_map<const Value*, int> nonInstMap;
	int lowerNotMapped = 0;

	int getNonInstMapped(const Value* V)
	{
		const auto pair = nonInstMap.insert({V, lowerNotMapped});
		if (pair.second)
			lowerNotMapped++;

		return pair.first->second;
	}

	std::unordered_map<const Instruction*, int> useMap;
	int lowerAssignedInvalidID = 0;
	std::pair<int,int> getUseMapped(const Use& use)
	{
		const Instruction* useI = cast<Instruction>(&use);
		const auto iter = useMap.find(useI);

		if (iter != useMap.end())
		{
			if (useI->getParent() == currBB)
			{
				return {iter->second, use.getOperandNo()};
			}
			else if (useI->getParent() == &succBB)
			{
				const PHINode* phi = cast<PHINode>(useI);
				assert(phi);
				//Since phis will be rewritten into one,
				//the operand number is not relevant
				return {iter->second, 0};
			}
		}

		//We treat only the cases where the user is a lower indexed function OR a phi in the successor
		//All other cases will not be meargeable so we add a unique ID
		return {isInvalid, lowerAssignedInvalidID++};
	}
public:
	BlockSignatureHelper(const BasicBlock& succBB)
		: succBB(succBB)
	{
		int nextId = maxIteration;
		for (const PHINode& phi : succBB.phis())
		{
			useMap.insert({&phi, nextId++});
		}
	}
	void analyze(const Instruction& I)
	{
		//Opcodes should match
		signature.push_back(I.getOpcode() + isOpcode);

		//Non-Instruction operands should match
		for (uint32_t i = 0; i<I.getNumOperands(); i++)
		{
			const llvm::Value* operand = I.getOperand(i);
			if (!isa<Instruction>(operand))
			{
				signature.push_back(i + isNonInstMap);
				signature.push_back(getNonInstMapped(operand));
			}
		}

		//Signature consider the set of uses of a given Instruction
		//Only signature with matching uses are considered for merging
		std::set<std::pair<int,int> > usesRepresentation;
	        for (const Use& use : I.uses())
			usesRepresentation.insert(getUseMapped(use));

		signature.push_back(isUser);
		signature.push_back(usesRepresentation.size());
		//std::set will be iterated deterministically, so given equivalent sets always the same signature will be generated
		for (auto p : usesRepresentation)
		{
			signature.push_back(p.first);
			signature.push_back(p.second);
		}
	}
	void addInstructionToMap(const Instruction& I, const int currIteration)
	{
		//Add Instruction to useMap
		useMap.insert({&I, currIteration});
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
			addInstructionToMap(*iter, currIteration);
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

llvm::PreservedAnalyses SinkGeneratorPass::run(Function& F, FunctionAnalysisManager& FAM)
{
	SinkGenerator inner;
	if (!inner.runOnFunction(F))
		return PreservedAnalyses::all();
	PreservedAnalyses PA;
	PA.preserve<cheerp::GlobalDepsAnalysis>();
	PA.preserve<cheerp::LinearMemoryAnalysis>();
	return PA;
}
}
