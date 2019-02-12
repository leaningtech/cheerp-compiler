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
#include "llvm/Analysis/InstructionSimplify.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/GEPOptimizer.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/ValueMapper.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Support/GenericDomTreeConstruction.h"
#include "llvm/Support/raw_ostream.h"
#include <set>
#include <map>

//#define DEBUG_GEP_OPT_VERBOSE 1

namespace llvm {

Value* GEPOptimizer::GEPRecursionData::getValueNthOperator(const OrderedGEPs::iterator it, const uint32_t index)
{
	if (index >= (*it)->getNumOperands())
		return NULL;
	else
		return (*it)->getOperand(index);
}

void GEPOptimizer::GEPRecursionData::optimizeGEPsRecursive(OrderedGEPs::iterator begin, const OrderedGEPs::iterator end,
		Value* base, uint32_t endIndex)
{
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
		OrderedGEPs::iterator it = begin;
		const Value* referenceOperand = getValueNthOperator(it, endIndex);
		uint32_t rangeSize = 1;
		it++;

		while(it!=end &&  referenceOperand == getValueNthOperator(it, endIndex))
		{
			rangeSize++;
			it++;
		}
		// This is the first index which is different in the range.
#if DEBUG_GEP_OPT_VERBOSE
		llvm::errs() << "Index equal until " << endIndex << ", range size: " << rangeSize << "\n";
#endif

		// Compute the insertion point to dominate all users of this GEP in the range
		// (possibly moving some GEP to skippedGeps, so finding the iterators has to be done afterwards)
		Instruction* insertionPoint = findInsertionPoint(begin, it, endIndex);

		OrderedGEPs::iterator endEquals = begin;
		while (endEquals != it && getValueNthOperator(endEquals, endIndex+1) == NULL)
		{
			++endEquals;
		}
		OrderedGEPs::iterator endRange = endEquals;
		while (endRange != it)
		{
			++endRange;
		}
		const GEPRange range = GEPRange::createGEPRange(cast<llvm::GetElementPtrInst>(*begin), endIndex+1);

		//Hoist the GEP as high as possible, will be put on the right spot by DelayInsts later
		insertionPoint = passData->hoistGEP(insertionPoint, range);
		assert(insertionPoint);

		newIndexes.push_back((*begin)->getOperand(endIndex));
		GetElementPtrInst* newGEP = GetElementPtrInst::Create(base->getType()->getPointerElementType(), base, newIndexes, base->getName()+".optgep");
		newGEP->insertBefore(insertionPoint);
		newIndexes.pop_back();
#if DEBUG_GEP_OPT_VERBOSE
		llvm::errs() << "New GEP " << newGEP << "\n";
#endif

		if (begin != endEquals)
		{
			//Take care of the GEP ending here
			erasedInst.insert(std::make_pair(newGEP, std::vector<GetElementPtrInst*>(begin, endEquals)));
		}

		// Will be later examined, checking wheter it should be merged to its single use (or kept if there are multiple uses)

		if (endEquals != endRange)
		{
			nonTerminalGeps.push_back(newGEP);
			//Take care of a nonTerminalGeps (= has children nodes in the GEP tree)
			optimizeGEPsRecursive(endEquals, endRange, newGEP, endIndex + 1);
		}
		// Reset the state for the next range
		begin = endRange;
	}
}

Instruction* GEPOptimizer::GEPRecursionData::findInsertionPoint(const OrderedGEPs::iterator begin, const OrderedGEPs::iterator end, const uint32_t endIndex)
{
	Instruction* insertionPoint = NULL;
	DominatorTree* DT = passData->DT;

	for(OrderedGEPs::iterator it = begin; it != end;)
	{
		//We need to hold an iterator both to the current and the next item
		//to be able to delete the current item without invalidating iterators
		OrderedGEPs::iterator currIt = it;
		GetElementPtrInst* curGEP = *it;
		++it;
		Instruction* insertPointCandidate = cheerp::findCommonInsertionPoint(NULL, DT, insertionPoint, curGEP);

		// Make sure that insertPointCandidate is in a valid block for this GEP
		const ValidGEPLocations& validGEPLocations = passData->validGEPMap.at(GEPRange::createGEPRange(cast<const GetElementPtrInst>(curGEP),endIndex+1));
		if (!validGEPLocations.count(insertPointCandidate->getParent()))
		{
			// It is not safe to optimize this GEP, remove it from the set
			assert(curGEP != *begin);
			skippedGeps.insert(curGEP);
			orderedGeps.erase(currIt);
#if DEBUG_GEP_OPT_VERBOSE
			llvm::errs() << "Skipping GEP " << *curGEP << "\n";
#endif
		}
		else
		{
			insertionPoint = insertPointCandidate;
		}
	}
	return insertionPoint;
}


template <> struct GraphTraits<GEPOptimizer::ValidGEPGraph::Node*> {
	typedef GEPOptimizer::ValidGEPGraph::Node NodeType;
	typedef GEPOptimizer::ValidGEPGraph::SuccIterator ChildIteratorType;

	static NodeType *getEntryNode(NodeType* N) { return N; }
	static inline ChildIteratorType child_begin(NodeType *N) {
		return ChildIteratorType(N);
	}
	static inline ChildIteratorType child_end(NodeType *N) {
		return ChildIteratorType(N, true);
	}
};
template <> struct GraphTraits<Inverse<GEPOptimizer::ValidGEPGraph::Node*>> {
	typedef GEPOptimizer::ValidGEPGraph::Node NodeType;
	typedef GEPOptimizer::ValidGEPGraph::PredIterator ChildIteratorType;

	static NodeType *getEntryNode(NodeType* N) { return N; }
	static inline ChildIteratorType child_begin(NodeType *N) {
		return ChildIteratorType(N);
	}
	static inline ChildIteratorType child_end(NodeType *N) {
		return ChildIteratorType(N, true);
	}
};
template <> struct GraphTraits<GEPOptimizer::ValidGEPGraph*> : public GraphTraits<GEPOptimizer::ValidGEPGraph::Node*> {
	static NodeType *getEntryNode(GEPOptimizer::ValidGEPGraph *G) { return G->getEntryNode(); }

	typedef mapped_iterator<
		GEPOptimizer::ValidGEPGraph::NodeMap::iterator,
		std::function<GEPOptimizer::ValidGEPGraph::Node*(GEPOptimizer::ValidGEPGraph::NodeMap::iterator::reference)>
	> mapped_iterator_type;
	struct deref_mapped_iterator: public mapped_iterator_type {
		using mapped_iterator_type::mapped_iterator;
		operator NodeType*()
		{
			return **this;
		}
	};
	static NodeType* get_second_ptr(GEPOptimizer::ValidGEPGraph::NodeMap::iterator::reference pair)
	{
		return &pair.second;
	}
	 // nodes_iterator/begin/end - Allow iteration over all nodes in the graph
	typedef deref_mapped_iterator nodes_iterator;
	static nodes_iterator nodes_begin(GEPOptimizer::ValidGEPGraph* G) { return nodes_iterator(G->Nodes.begin(), get_second_ptr); }
	static nodes_iterator nodes_end  (GEPOptimizer::ValidGEPGraph* G) { return nodes_iterator(G->Nodes.end(), get_second_ptr); }
	static size_t         size       (GEPOptimizer::ValidGEPGraph* G) { return G->Nodes.size(); }
};

GEPOptimizer::ValidGEPLocations GEPOptimizer::ValidGEPGraph::getValidBlocks()
{
	//This function takes the direct graph defined in ValidGEPGraph, compute the post dominator tree, and build the result
	DominatorTreeBase<Node> PDT(true);
	PDT.recalculate(*this);
	SmallVector<ValidGEPGraph::Node*, 8> ValidNodes;
	PDT.getDescendants(getOrCreate(Kind::Good), ValidNodes);

	//The valid nodes are all nodes that we knew where valid (knownValid)
	ValidGEPLocations ret(knownValid);

	//and all nodes postdominates by "Good"
	for (auto V: ValidNodes)
	{
		if (V->BB)
		{
			ret.insert(V->BB);
		}
	}
	ret.simplify();
	return ret;
}

Instruction* GEPOptimizer::hoistGEP(Instruction* I, const GEPRange& R) const
{
	BasicBlock* B = I->getParent();

	auto GEPMap = validGEPMap.at(R);

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

void GEPOptimizer::ValidGEPLocations::keepOnlyDominatedByOperand(const Value* value)
{
	if (const Instruction* I = dyn_cast<const Instruction> (value))
	{
		keepOnlyDominated(I->getParent());
	}
}



GEPOptimizer::GEPRecursionData::GEPRecursionData(Function &F, GEPOptimizer* data) :
		order(),
		orderedGeps(OrderByOperands(&order)),
		skippedGeps(OrderByOperands(&order)),
		passData(data)
{
	//First we do a pass to collect in which blocks a GepRange is used, this data will be later used by ValidGEPGraph::Node::isValidForGEP()
	ValidGEPLocations NoBlocks;
	NoBlocks.setDominatorTree(passData->DT);
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
	order.insert({NULL, 0});

	// Gather all the GEPs
	ValidGEPLocations AllBlocks;
	AllBlocks.insert(&F.getEntryBlock());
	AllBlocks.setDominatorTree(passData->DT);
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
				order.insert({GEP->getOperand(i), order.size()});
			}

			orderedGeps.insert(GEP);
			ValidGEPLocations possiblyValidBlocks = AllBlocks;
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

					ValidGEPGraph VG(&F, passData->DT, Range, passData->subsetGEPMap.find(Range)->second, possiblyValidBlocks);

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

bool GEPOptimizer::runOnFunction(Function& F)
{
	DT = &getAnalysis<DominatorTreeWrapperPass>().getDomTree();

	//Collect all the data (where GEP are located, and sort them) and do some precomputations (which blocks are valid for a certain GEPRange)
	GEPRecursionData data(F, this);

	//Walk the GEPtree, building a GEP for every node (both leaf and non-leaf)
	data.startRecursion();

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

void GEPOptimizer::GEPRecursionData::startRecursion()
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
			optimizeGEPsRecursive(rangeStart, it, (*rangeStart)->getOperand(0), 1);
		}
		rangeLength = 0;
		rangeStart = it;
	}

	if (!skippedGeps.empty())
	{
		swap(skippedGeps, orderedGeps);
		skippedGeps.clear();
		startRecursion();
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

const char* GEPOptimizer::getPassName() const
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
