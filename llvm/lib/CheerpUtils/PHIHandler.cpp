//===-- PHIHandler.cpp - Cheerp utility functions --------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2020-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/PHIHandler.h"
#include "llvm/ADT/SCCIterator.h"

using namespace llvm;

namespace cheerp {

EndOfBlockPHIHandler::EndOfBlockPHIHandler(const PointerAnalyzer& PA, EdgeContext& edgeContext)
	: PA(PA), edgeContext(edgeContext)
{
}

EndOfBlockPHIHandler::~EndOfBlockPHIHandler()
{
	assert(edgeContext.isNull());
}

uint32_t EndOfBlockPHIHandler::countIncomingRegisters(const uint32_t current, const std::vector<uint32_t>& registerIds, const IncomingRegs& incomingRegs)
{
	uint32_t countIncoming = 0;
	assert(std::is_sorted(incomingRegs.begin(), incomingRegs.end(), [](auto&& a, auto&& b)->bool
	{
		return a.first < b.first;
	}));
	uint32_t i=0;
	uint32_t j=0;
	while (i < registerIds.size() && j < incomingRegs.size())
	{
		if (registerIds[i] == incomingRegs[j].first)
		{
			if (current != incomingRegs[j].first)
				countIncoming++;
			while (j < incomingRegs.size() && registerIds[i] == incomingRegs[j].first)
			{
				++j;
			}
			++i;
		}
		else if (registerIds[i] < incomingRegs[j].first)
			++i;
		else
			++j;
	}
	return countIncoming;
}

void EndOfBlockPHIHandler::runOnSCC(const std::vector<uint32_t>& registerIds, PHIRegs& phiRegs)
{
	assert(std::is_sorted(registerIds.begin(), registerIds.end()));

	//Find what phi to process first (the one with more incoming registers)
	//TODO: possibly it could be improved to count even itself if a temporary is needed for selfreferencing
	std::pair<uint32_t, uint32_t> best({0,0});
	uint32_t k =0;
	for (const auto& X : phiRegs)
	{
		if (k == registerIds.size() || X.first != registerIds[k])
			continue;
		++k;

		best = std::max(best, {countIncomingRegisters(X.first, registerIds, X.second.incomingRegs), X.first});
	}
	const uint32_t whoToProcess = best.second;
	const auto& regData = phiRegs.at(whoToProcess);

	//If the SCC is a multi-node loop, there is need to create a temporary
	if (registerIds.size() > 1)
	{
		const auto& incoming = regData.incomingInstElem;
		assert(incoming.instruction);
		handleRecursivePHIDependency(incoming);
		edgeContext.processAssigment();
	}

	setRegisterUsed(whoToProcess);
	const PHINode* phi = cast<PHINode>(regData.phiInstElem.instruction);
	const Value* val=phi->getIncomingValueForBlock(edgeContext.fromBB);
	// Call specialized function to process the actual assignment to the PHI
	handlePHI(regData.phiInstElem, val);
	edgeContext.processAssigment();

	for (const auto& pair : regData.incomingRegs)
	{
		removeRegisterUse(pair.first);
	}

	//If there are other nodes, solve them recursively on the graph with the current node removed
	if (registerIds.size() > 1)
	{
		PHIRegs filteredPhiRegs;
		for (uint32_t id : registerIds)
		{
			if (id == whoToProcess)
				continue;
			const auto& regData = phiRegs.at(id);
			filteredPhiRegs.emplace(id, regData);
		}
		runOnConnectionGraph(DependencyGraph(filteredPhiRegs, whoToProcess), phiRegs, /*isRecursiveCall*/true);
	}
}

void EndOfBlockPHIHandler::runOnConnectionGraph(DependencyGraph dependencyGraph, PHIRegs& phiRegs, bool isRecursiveCall)
{
	//1. Assign trivially assignable phi(eg. has a constant as incoming)
	//2. Build the dependency graph (phi with register X uses information stored into register Y to compute its value)
	//3. Find the Strongly Connected Components, and process in a standard order each SCC
	//4. Use a greedy strategy to minimize the number of temporary needed inside a given SCC
	// Note that since we process SCC in order, we are free to recycle temporary registers eventually created

	std::deque<std::vector<uint32_t>> regions;

	for (auto& SCC: make_range(scc_begin(&dependencyGraph), scc_end(&dependencyGraph)))
	{
		std::vector<uint32_t> region;
		//Collect register ids in the current Strongly Connected Components
		for (auto & node : SCC)
		{
			if (node->registerId == dependencyGraph.getEntry())
				break;
			region.push_back(node->registerId);
		}
		if (region.empty())
			continue;
		//Single nodes that do not depends on other registers should be processed last
		if (region.size() == 1 && countIncomingRegisters(region.front(), dependencyGraph.listRegisters(), phiRegs.at(region.front()).incomingRegs) == 0)
			regions.push_back(region);
		else
			regions.push_front(region);
	}

	for (std::vector<uint32_t>& registerIds : regions)
	{
		assert(!registerIds.empty());
		std::sort(registerIds.begin(), registerIds.end());

		//If using stack to resolve temporaries, do it now
		if (!isRecursiveCall)
		{
			std::vector<Registerize::InstElem> toProcessOnStack;
			for (auto id : registerIds)
			{
				auto& regData = phiRegs.at(id);
				toProcessOnStack.emplace_back(regData.phiInstElem);
			}

			handlePHIStackGroup(toProcessOnStack);
		}

		runOnSCC(registerIds, phiRegs);
	}
}

void EndOfBlockPHIHandler::runOnEdge(const Registerize& registerize, const BasicBlock* fromBB, const BasicBlock* toBB)
{
	edgeContext.setEdgeContext(fromBB, toBB);
	BasicBlock::const_iterator I=toBB->begin();
	BasicBlock::const_iterator IE=toBB->end();
	PHIRegs phiRegs;
	llvm::SmallVector<Registerize::InstElem, 4> orderedPHIs;
	std::vector<Registerize::InstElem> toProcessOnStack;
	for(;I!=IE;++I)
	{
		// Gather the dependency graph between registers for PHIs and incoming values
		// Also add PHIs which are always safe to the orderedPHIs vector
		const PHINode* phi=dyn_cast<PHINode>(I);
		if(!phi)
			break;
		if(phi->use_empty())
			continue;
		if(phiToBeSkipped.count(phi))
			continue;
		auto phiRegisters = registerize.getAllRegisterIds(phi, EdgeContext::emptyContext());
		const Value* val=phi->getIncomingValueForBlock(fromBB);
		const Instruction* I=dyn_cast<Instruction>(val);
		if(!I)
		{
			for(const auto& ie: Registerize::getInstElems(phi, PA))
			{
				toProcessOnStack.emplace_back(ie);
				orderedPHIs.push_back(ie);
			}
			continue;
		}
		Registerize::InstElemIterator it(phi, PA);
		for(uint32_t reg: phiRegisters)
		{
			phiRegs.emplace(reg, PHIRegData(*it));
			++it;
		}
		llvm::SmallVector<std::pair<const Instruction*, /*dereferenced*/bool>, 4> instQueue;
		instQueue.push_back(std::make_pair(I, false));
		bool splitRegular = phi->getType()->isPointerTy() && PA.getPointerKind(phi) == SPLIT_REGULAR && !PA.getConstantOffsetForPointer(phi);
		while(!instQueue.empty())
		{
			std::pair<const Instruction*, bool> incomingInst = instQueue.pop_back_val();
			if(!isInlineable(*incomingInst.first, PA))
			{
				auto incomingRegs = registerize.getAllRegisterIds(incomingInst.first, EdgeContext::emptyContext());
				Registerize::InstElemIterator phiIt(phi, PA);
				for(uint32_t i = 0; i < phiRegisters.size(); i++)
				{
					auto it = phiRegs.find(phiRegisters[i]);
					assert(it != phiRegs.end());
					if(incomingInst.second/*derefereced*/ || phiRegisters.size() != incomingRegs.size())
					{
						Registerize::InstElemIterator incomingIt(incomingInst.first, PA);
						for (uint32_t j = 0; j < incomingRegs.size(); j++)
						{
							it->second.incomingRegs.push_back(std::make_pair(incomingRegs[j], *incomingIt));
							++incomingIt;
						}
					}
					else
					{
						auto incomingEl = *phiIt;
						incomingEl.instruction = incomingInst.first;
						it->second.incomingRegs.push_back(std::make_pair(incomingRegs[i], incomingEl));
					}
					++phiIt;
				}
			}
			else
			{
				// TODO: Loads when inlined should go here
				bool dereferenced = incomingInst.second || (splitRegular && isa<GetElementPtrInst>(incomingInst.first) && incomingInst.first->getNumOperands() > 2);
				for(const Value* op: incomingInst.first->operands())
				{
					const Instruction* opI = dyn_cast<Instruction>(op);
					if(!opI)
						continue;
					instQueue.push_back(std::make_pair(opI, dereferenced));
				}
			}
		}
	}

	for (auto& X : phiRegs)
	{
		std::sort(X.second.incomingRegs.begin(), X.second.incomingRegs.end(), [](auto&& a, auto&& b) { return a.first < b.first; });
		//Set incomingInst AND the counter of how many times a input register is used
		for (const auto& pair : X.second.incomingRegs)
		{
			auto it = phiRegs.find(pair.first);
			if (it != phiRegs.end())
				it->second.incomingInstElem = pair.second;

			addRegisterUse(pair.first);
		}
	}
	runOnConnectionGraph(DependencyGraph(phiRegs), phiRegs, /*isRecursiveCall*/false);

	// Notify the user for each PHI, in the right order to avoid accidental overwriting
	for(int i=orderedPHIs.size()-1;i>=0;i--)
	{
		auto* phi = cast<PHINode>(orderedPHIs[i].instruction);
		const Value* val=phi->getIncomingValueForBlock(fromBB);
		// Call specialized function to process the actual assignment to the PHI
		handlePHI(orderedPHIs[i], val);
		edgeContext.processAssigment();
	}

	handlePHIStackGroup(toProcessOnStack);
	edgeContext.clear();
}

void EndOfBlockPHIHandler::skipPHI(const PHINode* toSkip)
{
	phiToBeSkipped.insert(toSkip);
}

bool EndOfBlockPHIHandler::hasToSkipPHIs() const
{
	return !phiToBeSkipped.empty();
}

}
