//===-- GEPOptimizer.cpp - Cheerp GEP optimization passes -----------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2019 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "CheerpGEPOptimizer"

#include "llvm/InitializePasses.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/GEPOptimizer.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/GenericDomTreeConstruction.h"
#include "llvm/Support/raw_ostream.h"

//#define DEBUG_GEP_OPT_VERBOSE 1

namespace llvm {

ValidBasicBlockForestGraph* ValidBasicBlockForestGraph::current_VBBFGraph = NULL;

Value* GEPOptimizer::GEPRecursionData::getValueNthOperator(const GetElementPtrInst* gep, const uint32_t index)
{
	if (index >= gep->getNumOperands())
		return NULL;
	else
		return gep->getOperand(index);
}

void GEPOptimizer::GEPRecursionData::buildNodesOfGEPTree(OrderedGEPs::iterator begin, const OrderedGEPs::iterator end,
		Value* base, uint32_t endIndex)
{
	//This function takes a range (begin, end) of GEPs, build all the GEPs of level endIndex in the GEPTree, and then call itself again as to build nodes on levels > endIndex

	assert(begin != end);
	llvm::SmallVector<llvm::Value*, 4> newIndexes;

	if(endIndex > 1)
	{
		// This is an optimized GEP, it will start from the passed base. Add a constant 0.
		newIndexes.push_back(ConstantInt::get(llvm::Type::getInt32Ty(base->getContext()), 0));
	}
	while (begin != end)
	{
		//Initialize range
		const Value* referenceOperand = getValueNthOperator(*begin, endIndex);

		const OrderedGEPs::iterator endSubRange = find_if_not(begin, end, [endIndex, referenceOperand](GetElementPtrInst* gep)
				{
					return referenceOperand == getValueNthOperator(gep, endIndex);
				});

#if DEBUG_GEP_OPT_VERBOSE
		llvm::errs() << "Index equal until " << endIndex << ", range size: " << std::distance(begin, endSubRange) << "\n";
#endif
		for (auto& subproblem : splitIntoSubproblems(begin, endSubRange, endIndex+1))
		{
			assert(checkInvariantsOnOrderedGEPs(subproblem.GEPs));

			// Compute the insertion point to dominate all users of this GEP in the range
			Instruction* insertionPoint = findInsertionPoint(subproblem.GEPs);

			OrderedGEPs::iterator endEquals = find_if_not(subproblem.GEPs.begin(), subproblem.GEPs.end(), [endIndex](GetElementPtrInst* gep)
					{
						return getValueNthOperator(gep, endIndex+1) == NULL;
					});

			//Hoist the GEP as high as possible, will be put on the right spot by DelayInsts later
			//If the instruction is already in the highest possible block, leave it there (since it may be already higher than the terminator)
			//Otherwise move the insertion point to the terminator of the highest possible block
			if (insertionPoint->getParent() != subproblem.representative)
				insertionPoint = subproblem.representative->getTerminator();

			newIndexes.push_back((*(subproblem.GEPs.begin()))->getOperand(endIndex));
			GetElementPtrInst* newGEP = GetElementPtrInst::Create(base->getType()->getPointerElementType(), base, newIndexes, base->getName()+".optgep");
			newGEP->insertBefore(insertionPoint);
			newIndexes.pop_back();
#if DEBUG_GEP_OPT_VERBOSE
			llvm::errs() << "New GEP " << *newGEP << "\n";
#endif
			OrderedGEPs endingHere(subproblem.GEPs.begin(), endEquals);
			OrderedGEPs longerGEPs(endEquals, subproblem.GEPs.end());

			if (!endingHere.empty())
			{
				//Take care of the GEP ending here
				erasedInst.insert({newGEP, endingHere});
				// Will be later examined, checking wheter it should be merged to its single children (or kept if there are multiple ones)
			}


			if (!longerGEPs.empty())
			{
				nonTerminalGeps.push_back(newGEP);
				//Take care of a nonTerminalGeps (= has children nodes in the GEP tree)
				buildNodesOfGEPTree(longerGEPs.begin(), longerGEPs.end(), newGEP, endIndex + 1);
			}
		}

		// Reset the state for the next range
		begin = endSubRange;
	}
}

Instruction* GEPOptimizer::GEPRecursionData::findInsertionPoint(const OrderedGEPs& GEPs)
{
	Instruction* insertionPoint = NULL;
	DominatorTree* DT = passData->DT;

	for(GetElementPtrInst* curGEP : GEPs)
	{
		insertionPoint = cheerp::findCommonInsertionPoint(NULL, DT, insertionPoint, curGEP);
	}
	return insertionPoint;
}

std::vector<GEPOptimizer::GEPRecursionData::PairRepresentativeOrderedGEPs> GEPOptimizer::GEPRecursionData::splitIntoSubproblems(const GEPOptimizer::OrderedGEPs::iterator begin, const GEPOptimizer::OrderedGEPs::iterator end, const uint32_t endIndex) const
{
	//Take a ordered range of GEPs, assign every one of them to each representative root of the appropriate BasicBlockForest
	//and split them accordingly (keeping the GEPs in the same relative order as before)

	const GEPRange range = GEPRange::createGEPRange(cast<llvm::GetElementPtrInst>(*begin), endIndex);
	const BasicBlockForest& forest = passData->validGEPMap.at(range);

	struct PairRepresentativeGEP
	{
		PairRepresentativeGEP(GetElementPtrInst* GEP, const BasicBlockForest& forest)
			: representative(forest.getRepresentingRoot(GEP->getParent())), GEP(GEP)
		{}
		bool operator<(const PairRepresentativeGEP& other) const
		{
			//The comparison is only based on the BB
			//And a stable sort is needed to keep the GEPs with the same representative ordered (since they start out as ordered)
			return representative < other.representative;
		}
		llvm::BasicBlock* representative;
		llvm::GetElementPtrInst* GEP;
	};

	std::vector<PairRepresentativeGEP> auxiliaryVector;

	std::transform(begin, end, std::back_inserter(auxiliaryVector),
			[&forest](GetElementPtrInst* gep) -> PairRepresentativeGEP { return PairRepresentativeGEP(gep, forest); });

	PointerToUniqueIdentifier<BasicBlock*> BBToIdentifier;
	//Popolate BBToIdentifier
	for (const auto& x : auxiliaryVector)
	{
		BBToIdentifier.insert(x.representative);
	}

	//The sort has to be stable, otherwise we lost the proprety that smaller geps should come first
	//and the sort should only based on the representative part (a BasicBlock* basically)
	stable_sort(auxiliaryVector.begin(), auxiliaryVector.end(),
			[&BBToIdentifier](const PairRepresentativeGEP& a, const PairRepresentativeGEP&b) -> bool
			{
				return BBToIdentifier.at(a.representative) < BBToIdentifier.at(b.representative);
			});

	std::vector<PairRepresentativeOrderedGEPs> splitProblems;

	for (const auto& elem : auxiliaryVector)
	{
		if (splitProblems.empty() || splitProblems.back().representative != elem.representative)
			splitProblems.push_back(PairRepresentativeOrderedGEPs(elem.representative));

		splitProblems.back().GEPs.push_back(elem.GEP);
	}

	return splitProblems;
}



template <> struct GraphTraits<ValidBasicBlockForestGraph::Node*> {
	typedef ValidBasicBlockForestGraph::Node NodeType;
	typedef ValidBasicBlockForestGraph::SuccIterator ChildIteratorType;

	static NodeType *getEntryNode(NodeType* N) { return N; }
	static inline ChildIteratorType child_begin(NodeType *N) {
		return ChildIteratorType(N);
	}
	static inline ChildIteratorType child_end(NodeType *N) {
		return ChildIteratorType(N, true);
	}
	typedef ValidBasicBlockForestGraph::Node* NodeRef;
};
template <> struct GraphTraits<Inverse<ValidBasicBlockForestGraph::Node*>> {
	typedef ValidBasicBlockForestGraph::Node NodeType;
	typedef ValidBasicBlockForestGraph::PredIterator ChildIteratorType;

	static NodeType *getEntryNode(Inverse<NodeType*> N) { return N.Graph; }
	static inline ChildIteratorType child_begin(NodeType *N) {
		return ChildIteratorType(N);
	}
	static inline ChildIteratorType child_end(NodeType *N) {
		return ChildIteratorType(N, true);
	}
	typedef ValidBasicBlockForestGraph::Node* NodeRef;
};
template <> struct GraphTraits<ValidBasicBlockForestGraph*> : public GraphTraits<ValidBasicBlockForestGraph::Node*> {
	static NodeType *getEntryNode(ValidBasicBlockForestGraph *G) { return G->getEntryNode(); }

	typedef mapped_iterator<
		ValidBasicBlockForestGraph::NodeMap::iterator,
		std::function<ValidBasicBlockForestGraph::Node*(ValidBasicBlockForestGraph::NodeMap::iterator::reference)>
	> mapped_iterator_type;
	static NodeType* get_second_ptr(ValidBasicBlockForestGraph::NodeMap::iterator::reference pair)
	{
		return &pair.second;
	}
	 // nodes_iterator/begin/end - Allow iteration over all nodes in the graph
	typedef mapped_iterator_type nodes_iterator;
	static nodes_iterator nodes_begin(ValidBasicBlockForestGraph* G) { return nodes_iterator(G->Nodes.begin(), get_second_ptr); }
	static nodes_iterator nodes_end  (ValidBasicBlockForestGraph* G) { return nodes_iterator(G->Nodes.end(), get_second_ptr); }
	static size_t         size       (ValidBasicBlockForestGraph* G) { return G->Nodes.size(); }
};

BasicBlockForest ValidBasicBlockForestGraph::getValidBlocks()
{
	//This function takes the direct graph defined in ValidBasicBlockForestGraph, compute the post dominator tree, and build the result
	DominatorTreeBase<Node, true> PDT;
	PDT.recalculate(*this);
	SmallVector<ValidBasicBlockForestGraph::Node*, 8> ValidNodes;
	PDT.getDescendants(getOrCreate(Kind::Good), ValidNodes);

	//The valid nodes are all nodes that we knew where valid (knownValid)
	BasicBlockForest ret(knownValid);

	//and all nodes postdominates by "Good"
	for (const auto& V: ValidNodes)
	{
		if (V->BB())
		{
			ret.insert(V->BB());
		}
	}
	ret.simplify();
	assert(std::all_of(ret.getRoots().begin(), ret.getRoots().end(), [this](BasicBlock* bb) -> bool { return toBeClassified.count(bb); } ));
	return ret;
}

std::vector<BasicBlock*> findRepresentingBasicBlock(const DominatorTree* DT, const std::vector<BasicBlock*>& blocks)
{
	//Given a vector of BasicBlocks, we return for each one of which region he is a member.
	//Each region is represented by the root of the BasicBlockForest

	if (blocks.empty())
	{
		//In the costructor for ValidBasicBlockForestGraph we calculate the Function as blocks().begin()->getParent(), this implies that blocks is non-empty
		//This is not a problem, since a empty set of blocks has an empty set of representatives
		return std::vector<BasicBlock*>();
	}

	//Insert the good nodes
	BasicBlockForest goodNodes(DT);
	for (BasicBlock* BB : blocks)
	{
		goodNodes.insert(BB);
	}
	goodNodes.simplify();
	goodNodes.expandUpwards();

	ValidBasicBlockForestGraph VG(goodNodes);

	BasicBlockForest validPlacements = VG.getValidBlocks();
	validPlacements.expandUpwards();

	std::vector<BasicBlock*> result(blocks.size(), NULL);
	for (uint32_t i=0; i<blocks.size(); i++)
	{
		result[i] = validPlacements.getRepresentingRoot(blocks[i]);
		assert(result[i]);
	}
	return result;
}

Instruction* GEPOptimizer::hoistGEP(Instruction* I, const GEPRange& R) const
{
	BasicBlock* B = I->getParent();

	const auto& GEPMap = validGEPMap.at(R);

	//We loop over B, moving it every times to his immediate dominator
	while (1)
	{
		BasicBlock* iDominator = immediateDominator(B, DT);

		//Either there is no immediateDominator
		if (!iDominator)
			break;

		//Or because the current block is non valid for this GEPRange
		if (GEPMap.count(iDominator) == 0)
			break;

		//Move to the immediate dominator
		B = iDominator;

		//I needs to be updated inside the loop
		//in the future it may be called on something that isn't a terminator on his original block
		I = B->getTerminator();
	}

	return I;
}

void BasicBlockForest::keepOnlyDominatedByOperand(const Value* value)
{
	if (const Instruction* I = dyn_cast<const Instruction> (value))
	{
		keepOnlyDominated(I->getParent());
	}
}



GEPOptimizer::GEPRecursionData::GEPRecursionData(Function &F, GEPOptimizer* data) :
		passData(data)
{
	//First we do a pass to collect in which blocks a GepRange is used, this data will be later used by ValidGEPGraph::Node::isValidForGEP()
	ValidGEPLocations NoBlocks(passData->DT);
	for ( BasicBlock& BB : F )
	{
		for ( Instruction& I: BB )
		{
			if(!isa<GetElementPtrInst>(I))
				continue;
			// Only consider GEPs with at least  two indexes
			if(I.getNumOperands() < 2)
				continue;
			const GetElementPtrInst* GEP = cast<GetElementPtrInst>(&I);

			// NOTE: `i` is a size, so the end condition needs <=
			for (size_t i = 2; i <= GEP->getNumOperands(); ++i)
			{
				GEPRange Range = GEPRange::createGEPRange(GEP, i);
				auto it = passData->subsetGEPMap.find(Range);
				if(it == passData->subsetGEPMap.end())
					it = passData->subsetGEPMap.emplace(Range, NoBlocks).first;
				it->second.insert(&BB);
			}
		}
	}
	for (auto& x : passData->subsetGEPMap)
	{
		auto& blocks = x.second;
		blocks.simplify();
		blocks.expandUpwards();
	}
	//Then we do a second pass, to collect order (used to compare Instruction), the actual GEP list (in orderedGeps) and fill validGEPMap
	//order will be used to compare two Instruction. Inserting NULL here means
	//shorter subset will appear first (like in a dictionary, CAT < CATS)
	//This is required by GEPRecursionData::optimizeGEPsRecursive
	order.insert(NULL);
	assert(order.at(NULL) == 0);

	// Gather all the GEPs
	ValidGEPLocations AllBlocks(passData->DT);
	AllBlocks.insert(&F.getEntryBlock());
	for ( BasicBlock& BB : F )
	{
		for ( Instruction& I: BB )
		{
			if(!isa<GetElementPtrInst>(I))
				continue;
			// Only consider GEPs with at least  two indexes
			if(I.getNumOperands() < 2)
				continue;
			if(I.getNumOperands() == 2 && isConstantZero(I.getOperand(1)))
				continue;
			GetElementPtrInst* GEP = cast<GetElementPtrInst>(&I);
			for (size_t i = 0; i < GEP->getNumOperands(); ++i)
			{
				order.insert(GEP->getOperand(i));
			}

			orderedGeps.push_back(GEP);
			ValidGEPLocations possiblyValidBlocks = AllBlocks;
			assert(possiblyValidBlocks.getDominatorTree() == passData->DT);
			//possiblyValidBlocks contains all blocks where a GEP could still be possibly placed
			//(meaning, the only blocks excluded are the ones the could never possibly host one)
			//It starts being equal to all the blocks, then filtered by the operand of the GEP

			possiblyValidBlocks.keepOnlyDominatedByOperand(GEP->getOperand(0));

			// NOTE: `i` is a size, so the end condition needs <=
			for (size_t i = 2; i <= GEP->getNumOperands(); ++i)
			{
				GEPRange Range = GEPRange::createGEPRange(GEP, i);
				auto it = passData->validGEPMap.find(Range);
				if(it == passData->validGEPMap.end())
				{
					//Here we introduce the additional constraint that valid blocks have to be
					//dominated by every instruction used inside the GEP
					//Since basically we are doing set intersection between the original forest and a new
					//(sub-)tree in the DT, the result will still be a forest of subtree after every
					//application of keepOnlyDominated
					possiblyValidBlocks.keepOnlyDominatedByOperand(GEP->getOperand(i-1));

					//passData->subsetGEPMap.find(Range)->second contains all blocks where we are already certain a GEP could be placed
					//(this is a subset of possiblyValidBlocks)

					ValidBasicBlockForestGraph VG(passData->subsetGEPMap.find(Range)->second, possiblyValidBlocks);

					ValidGEPLocations validPlacements = VG.getValidBlocks();
					//ValidBlocks contains all the blocks that:
					//1- either leads from every possibly path to a node using a GEP
					//2- or thery are a child to such a node (in the DT)
					//They are represented as a forest (a set of (sub-)trees) in the dominator tree
					//Since property 2 guarantees that the children of a valid nodes are still valid,
					//we can keep only nodes that are roots to a sub-tree in the dominator tree

					it = passData->validGEPMap.emplace(Range, std::move(validPlacements)).first;
				}
				//For the next round, all blocks already excluded could be excluded for certain
				possiblyValidBlocks = it->second;
			}
		}
	}
}

void GEPOptimizer::GEPRecursionData::sortGEPs()
{
	//Sort the GEPs lexicographically
	//(it is not important the actual order, only two property matters:
	//-GEP with a common prefix should be close to each other
	//-a GEP that is a subset of another GEP should come earlier
	std::sort(orderedGeps.begin(), orderedGeps.end(), OrderByOperands(order));

	assert(checkInvariantsOnOrderedGEPs(orderedGeps));
}

bool GEPOptimizer::runOnFunction(Function& F)
{
	DT = &getAnalysis<DominatorTreeWrapperPass>().getDomTree();

	//Collect all the data (where GEP are located) and do some precomputations (which blocks are valid for a certain GEPRange)
	GEPRecursionData data(F, this);

	//Sort the GEPs lexicographically
	data.sortGEPs();

	//Walk the GEPtree, building a GEP for every node (both leaf and non-leaf)
	data.buildGEPTree();

	//Substitute the created GEPs in the function
	data.applyOptGEP();

	//Eliminate some non-leaf nodes (eg. X[3].a.b is only used by X[3].a.b.c, we can skip the former)
	data.compressGEPTree(ShortGEPPolicy::ALLOWED);

	//Clean up for running on the next function
	validGEPMap.clear();
	subsetGEPMap.clear();

	return data.anyChange();
}

// Look for GEPs that have a common base pointer. They should have both the
// pointer and the first index equal. As the GEPs in the map are ordered we
// know that equal objects are close.

void GEPOptimizer::GEPRecursionData::buildGEPTree()
{
	uint32_t rangeLength = 0;
	auto it=orderedGeps.begin();
	OrderedGEPs::iterator rangeStart = it;
	while (it != orderedGeps.end())
	{
		// Check that the first two operands are the same
		while (it != orderedGeps.end() &&
			(*rangeStart)->getOperand(0) == (*it)->getOperand(0) &&
			(*rangeStart)->getOperand(1) == (*it)->getOperand(1))
		{
			rangeLength++;
			++it;
		}

		// End the range here, if the range is longer than 1 we can optimize some GEPs
		if(rangeLength > 1)
		{
#if DEBUG_GEP_OPT_VERBOSE
			llvm::errs() << "Common GEP range:\n";
			for(auto it2=rangeStart;it2!=it;++it2)
				llvm::errs() << **it2 << "\n";
			llvm::errs() << "\n";
#endif
			buildNodesOfGEPTree(rangeStart, it, (*rangeStart)->getOperand(0), 1);
		}
		rangeLength = 0;
		rangeStart = it;
	}
}

void GEPOptimizer::GEPRecursionData::applyOptGEP()
{
	for(auto p: erasedInst)
	{
		p.first->takeName(p.second.front());
		for (auto I : p.second)
		{
			I -> replaceAllUsesWith(p.first);
			I -> eraseFromParent();
		}
	}
}

void GEPOptimizer::GEPRecursionData::mergeGEPs(GetElementPtrInst* a, GetElementPtrInst* b)
{
	llvm::SmallVector<llvm::Value*, 4> indexes;
	auto iter = a->idx_begin();
	while (iter != a->idx_end())
	{
		indexes.push_back(*iter);
		++iter;
	}

	iter = b->idx_begin();

	//The first index of a gep with depth > 0 has to be 0
	assert(cast<ConstantInt>(*iter)->equalsInt(0));

	++iter;
	while (iter != b->idx_end())
	{
		indexes.push_back(*iter);
		++iter;
	}

	GetElementPtrInst* newGEP = GetElementPtrInst::Create(a->getSourceElementType(), a->getPointerOperand(), indexes, b->getName()+".optgepsqueezed");
	newGEP->insertBefore(b);
	b->replaceAllUsesWith(newGEP);
	b->eraseFromParent();
}

void GEPOptimizer::GEPRecursionData::compressGEPTree(const ShortGEPPolicy shortGEPPolicy)
{
	//nonTerminalGeps holds inner nodes in the GEP tree
	//A child node always appear after its parent (by construction)
	//nonTerminalGeps is consumed backward, since otherwise we may modify the children of
	//the current node, and we need to avoid invalidating values still inside the vector
	while (!nonTerminalGeps.empty())
	{
		GetElementPtrInst* a = nonTerminalGeps.back();
		//Delete the current value, it may be invalidated
		nonTerminalGeps.pop_back();

		//If it is already used by someone, do not merge it
		if (erasedInst.count(a))
			continue;

		//If either it has only one use or it is a short GEP, merge them
		if ( (shortGEPPolicy == ShortGEPPolicy::NOT_ALLOWED && a->getNumOperands()==2)
				|| (a->getNumOperands() == 2 && isConstantZero(a->getOperand(1)))
				|| a->hasOneUse() )
		{
			//We will change a->users(), so the iteration have to be done with care
			for (auto b = a->users().begin(); b != a->users().end(); )
			{
				auto next = b;
				++next;
				//Check whether that user is a GEP
				GetElementPtrInst* bGEP = cast<GetElementPtrInst>(*b);
				mergeGEPs(a, bGEP);
				b = next;
			}
			a->eraseFromParent();
		}
	}
}

StringRef GEPOptimizer::getPassName() const
{
	return "GEPOptimizer";
}

char GEPOptimizer::ID = 0;

void GEPOptimizer::getAnalysisUsage(AnalysisUsage & AU) const
{
	AU.addRequired<DominatorTreeWrapperPass>();
	AU.addPreserved<cheerp::GlobalDepsAnalyzer>();
	llvm::Pass::getAnalysisUsage(AU);
}

FunctionPass *createGEPOptimizerPass() { return new GEPOptimizer(); }

}

using namespace llvm;

INITIALIZE_PASS_BEGIN(GEPOptimizer, "GEPOptimizer", "Rewrite GEPs in a function to remove redundant object accesses",
			false, false)
INITIALIZE_PASS_END(GEPOptimizer, "GEPOptimizer", "Rewrite GEPs in a function to remove redundant object accesses",
			false, false)
