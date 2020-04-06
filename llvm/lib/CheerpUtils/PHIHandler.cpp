//===-- PHIHandler.cpp - Cheerp utility functions --------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2020 Leaning Technologies
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

void EndOfBlockPHIHandler::runOnPHI(PHIRegs& phiRegs, uint32_t regId, const llvm::Instruction* incoming, llvm::SmallVector<std::pair<const PHINode*, /*selfReferencing*/bool>, 4>& orderedPHIs)
{
	auto it=phiRegs.find(regId);
	if(it==phiRegs.end())
		return;
	PHIRegData& regData=it->second;
	if(regData.status==PHIRegData::VISITED)
		return;
	else if(regData.status==PHIRegData::VISITING)
	{
		// Call specialized function to process the copy to temporary
		handleRecursivePHIDependency(incoming);
		edgeContext.processAssigment();
		return;
	}
	// Not yet visited
	regData.status=PHIRegData::VISITING;
	for(auto& reg: regData.incomingRegs)
	{
		runOnPHI(phiRegs, reg.first, reg.second, orderedPHIs);
	}
	// Add the PHI to orderedPHIs only after eventual dependencies have been added
	orderedPHIs.push_back(std::make_pair(regData.phiInst, regData.selfReferencing));
	regData.status=PHIRegData::VISITED;
}

uint32_t EndOfBlockPHIHandler::countIncomingRegisters(const uint32_t current, const std::vector<uint32_t>& registerIds, const IncomingRegs& incomingRegs)
{
	uint32_t countIncoming = 0;
	assert(std::is_sorted(incomingRegs.begin(), incomingRegs.end(), [](const std::pair<uint32_t, const Instruction*>& a, const std::pair<uint32_t, const Instruction*>& b)->bool
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

void EndOfBlockPHIHandler::runOnSCC(const std::vector<uint32_t>& registerIds, PHIRegs& phiRegs, llvm::SmallVector<std::pair<const PHINode*, /*selfReferencing*/bool>, 4>& orderedPHIs)
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

	//If the SCC is a multi-node loop, there is need to create a temporary
	if (registerIds.size() > 1)
	{
		const Instruction* incoming = phiRegs.at(whoToProcess).incomingInst;
		assert(incoming);
		handleRecursivePHIDependency(incoming);
		edgeContext.processAssigment();
	}

	setRegisterUsed(whoToProcess);
	const PHINode* phi=phiRegs.at(whoToProcess).phiInst;
	const Value* val=phi->getIncomingValueForBlock(edgeContext.fromBB);
	// Call specialized function to process the actual assignment to the PHI
	handlePHI(phi, val, phiRegs.at(whoToProcess).selfReferencing);
	edgeContext.processAssigment();

	for (const auto& pair : phiRegs.at(whoToProcess).incomingRegs)
	{
		removeRegisterUse(pair.first);
	}
	if (phiRegs.at(whoToProcess).selfReferencing)
	{
		removeRegisterUse(whoToProcess);
	}

	//If there are other nodes, solve them recursively on the graph with the current node removed
	if (registerIds.size() > 1)
	{
		PHIRegs filteredPhiRegs;
		for (uint32_t id : registerIds)
		{
			if (id == whoToProcess)
				continue;
			filteredPhiRegs.insert(std::make_pair(id, PHIRegData(phiRegs.at(id).phiInst, phiRegs.at(id).incomingRegs, phiRegs.at(id).selfReferencing)));
		}
		runOnConnectionGraph(DependencyGraph(filteredPhiRegs, whoToProcess), phiRegs, orderedPHIs);
	}
}

void EndOfBlockPHIHandler::runOnConnectionGraph(DependencyGraph dependencyGraph, PHIRegs& phiRegs, llvm::SmallVector<std::pair<const PHINode*, /*selfReferencing*/bool>, 4>& orderedPHIs)
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
		sort(registerIds.begin(), registerIds.end());
		runOnSCC(registerIds, phiRegs, orderedPHIs);
	}
}

void EndOfBlockPHIHandler::runOnEdge(const Registerize& registerize, const BasicBlock* fromBB, const BasicBlock* toBB)
{
	edgeContext.setEdgeContext(fromBB, toBB);
	BasicBlock::const_iterator I=toBB->begin();
	BasicBlock::const_iterator IE=toBB->end();
	PHIRegs phiRegs;
	llvm::SmallVector<std::pair<const PHINode*, /*selfReferencing*/bool>, 4> orderedPHIs;
	for(;I!=IE;++I)
	{
		// Gather the dependency graph between registers for PHIs and incoming values
		// Also add PHIs which are always safe to the orderedPHIs vector
		const PHINode* phi=dyn_cast<PHINode>(I);
		if(!phi)
			break;
		if(phi->use_empty())
			continue;
		const Value* val=phi->getIncomingValueForBlock(fromBB);
		const Instruction* I=dyn_cast<Instruction>(val);
		if(!I)
		{
			orderedPHIs.push_back(std::make_pair(phi, /*selfReferencing*/false));
			continue;
		}
		uint32_t phiReg = registerize.getRegisterId(phi, EdgeContext::emptyContext());
		if (RegisterizeLegacy)
			setRegisterUsed(phiReg);
		// This instruction may depend on multiple registers
		llvm::SmallVector<std::pair<uint32_t, const Instruction*>, 2> incomingRegisters;
		llvm::SmallVector<std::pair<const Instruction*, /*dereferenced*/bool>, 4> instQueue;
		instQueue.push_back(std::make_pair(I, false));
		bool mayNeedSelfRef = phi->getType()->isPointerTy() && PA.getPointerKind(phi) == SPLIT_REGULAR && !PA.getConstantOffsetForPointer(phi);
		bool selfReferencing = false;
		while(!instQueue.empty())
		{
			std::pair<const Instruction*, bool> incomingInst = instQueue.pop_back_val();
			if(!isInlineable(*incomingInst.first, PA))
			{
				uint32_t incomingValueId = registerize.getRegisterId(incomingInst.first, EdgeContext::emptyContext());
				if (RegisterizeLegacy)
					setRegisterUsed(incomingValueId);
				if(incomingValueId==phiReg)
				{
					if(mayNeedSelfRef &&
						PA.getPointerKind(incomingInst.first) == SPLIT_REGULAR && // If the incoming inst is not SPLIT_REGULAR there is no collision risk
						!PA.getConstantOffsetForPointer(incomingInst.first) && // If the offset part is constant we can reorder the operation to avoid a collision
						incomingInst.second) // If the register is not dereferenced there is no conflict as base and offset are not used together
					{
						selfReferencing = true;
					}
					if (RegisterizeLegacy)
						continue;
				}
				incomingRegisters.push_back(std::make_pair(incomingValueId, incomingInst.first));
			}
			else
			{
				// TODO: Loads when inlined should go here
				bool dereferenced = incomingInst.second || (mayNeedSelfRef && isa<GetElementPtrInst>(incomingInst.first) && incomingInst.first->getNumOperands() > 2);
				for(const Value* op: incomingInst.first->operands())
				{
					const Instruction* opI = dyn_cast<Instruction>(op);
					if(!opI)
						continue;
					instQueue.push_back(std::make_pair(opI, dereferenced));
				}
			}
		}
		if(incomingRegisters.empty() && RegisterizeLegacy)
			orderedPHIs.push_back(std::make_pair(phi, selfReferencing));
		else
			phiRegs.insert(std::make_pair(phiReg, PHIRegData(phi, std::move(incomingRegisters), selfReferencing)));
	}

	if (RegisterizeLegacy)
	{
		//Legacy algorithm:
		//1. Assign trivially assignable phi(eg. has a constant as incoming)
		//2. Process the other phi in a standard order
		//3. Use a greedy strategy to minimize the number of temporary needed inside a given SCC
		for(auto it: phiRegs)
		{
			if(it.second.status==PHIRegData::VISITED)
				continue;
			runOnPHI(phiRegs, it.first, nullptr, orderedPHIs);
		}
	}
	else
	{
		for (auto& X : phiRegs)
		{
			//Set incomingInst AND the counter of how many times a input register is used
			for (const auto& pair : X.second.incomingRegs)
			{
				auto it = phiRegs.find(pair.first);
				if (it != phiRegs.end())
					it->second.incomingInst = pair.second;

				addRegisterUse(pair.first);
			}
		}
		runOnConnectionGraph(DependencyGraph(phiRegs), phiRegs, orderedPHIs);
	}

	// Notify the user for each PHI, in the right order to avoid accidental overwriting
	for(uint32_t i=orderedPHIs.size();i>0;i--)
	{
		const PHINode* phi=orderedPHIs[i-1].first;
		const Value* val=phi->getIncomingValueForBlock(fromBB);
		// Call specialized function to process the actual assignment to the PHI
		handlePHI(phi, val, orderedPHIs[i-1].second);
		edgeContext.processAssigment();
	}
	edgeContext.clear();
}

}
