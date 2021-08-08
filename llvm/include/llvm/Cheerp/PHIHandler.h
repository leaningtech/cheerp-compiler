//===-- Cheerp/PHIHandler.h - Cheerp common routines -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2020-2021 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_PHIHANDLER_H
#define _CHEERP_PHIHANDLER_H

#include <algorithm>
#include <map>
#include <vector>
#include <unordered_map>
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Cheerp/EdgeContext.h"
#include "llvm/Cheerp/PointerAnalyzer.h"
#include "llvm/Cheerp/Registerize.h"

namespace cheerp
{

class EndOfBlockPHIHandler
{
	typedef llvm::SmallVector<std::pair<uint32_t, const llvm::Instruction*>,2> IncomingRegs;
	struct PHIRegData
	{
		const llvm::PHINode* phiInst;
		const llvm::Instruction* incomingInst;
		IncomingRegs incomingRegs;
		enum STATUS { NOT_VISITED=0, VISITING, VISITED };
		STATUS status;
		bool selfReferencing;
		PHIRegData(const llvm::PHINode* p, const llvm::SmallVector<std::pair<uint32_t, const llvm::Instruction*>,2>& r, bool selfReferencing):
			phiInst(p), incomingInst(nullptr), incomingRegs(r), status(NOT_VISITED), selfReferencing(selfReferencing)
		{
			std::sort(incomingRegs.begin(), incomingRegs.end());
		}
	};
	typedef std::map<uint32_t, PHIRegData> PHIRegs;
public:
	class DependencyGraph;
	struct GraphNode {
		uint32_t registerId;
		llvm::SmallVector<uint32_t, 2> Succs;
		DependencyGraph& Graph;
		explicit GraphNode(const uint32_t id, DependencyGraph& Graph)
			: registerId(id), Graph(Graph)
		{
			if (registerId == Graph.getEntry())
			{
				//The entry node its connected to every other node
				for (const auto& x : Graph.PHIData)
				{
					Succs.push_back(x.first);
				}
			}
			else
			{
				//The other nodes to their dependencies
				for (const auto& x : Graph.PHIData.at(registerId).incomingRegs)
				{
					if (Graph.shouldBeRepresented(x.first))
						Succs.push_back(x.first);
				}
			}
			//Normalize successors
			std::sort(Succs.begin(), Succs.end());
		}
	};
	class DependencyGraph {
	public:
		typedef std::unordered_map<uint32_t, GraphNode> NodeMap;

		explicit DependencyGraph(const PHIRegs& PHIData, uint32_t toSkip = -1): PHIData(PHIData), toSkip(toSkip)
		{
		}
		uint32_t getEntry() const
		{
			return -1;
		}
		std::vector<uint32_t> listRegisters() const
		{
			std::vector<uint32_t> res;
			for (const auto& x : PHIData)
			{
				res.push_back(x.first);
			}
			assert(std::is_sorted(res.begin(), res.end()));
			return res;
		}
		bool shouldBeRepresented(const uint32_t id) const
		{
			return PHIData.count(id) && id != toSkip;
		}
	private:
		GraphNode* getOrCreate(uint32_t id)
		{
			auto it = Nodes.find(id);
			if (it == Nodes.end())
			{
				it = Nodes.emplace(id, GraphNode(id, *this)).first;
			}
			return &it->second;
		}
		friend struct llvm::GraphTraits<DependencyGraph*>;
		friend struct GraphNode;

		const PHIRegs& PHIData;
		NodeMap Nodes;
		const uint32_t toSkip;
	};
	EndOfBlockPHIHandler(const PointerAnalyzer& PA, EdgeContext& edgeContext);
	void runOnEdge(const Registerize& registerize, const llvm::BasicBlock* fromBB, const llvm::BasicBlock* toBB);
	void skipPHI(const llvm::PHINode* phiToSkip);
	bool hasToSkipPHIs() const;
protected:
	const PointerAnalyzer& PA;
	EdgeContext& edgeContext;
	virtual ~EndOfBlockPHIHandler();
private:
	static uint32_t countIncomingRegisters(const uint32_t current, const std::vector<uint32_t>& registerIds, const IncomingRegs& incomingRegs);
	void runOnSCC(const std::vector<uint32_t>& registerIds, PHIRegs& phiRegs, llvm::SmallVector<std::pair<const llvm::PHINode*, /*selfReferencing*/bool>, 4>& orderedPHIs);
	void runOnConnectionGraph(DependencyGraph dependecyGraph, PHIRegs& phiRegs, llvm::SmallVector<std::pair<const llvm::PHINode*, /*selfReferencing*/bool>, 4>& orderedPHIs, bool isRecursiveCall);
	void runOnPHI(PHIRegs& phiRegs, uint32_t phiId, const llvm::Instruction* incoming, llvm::SmallVector<std::pair<const llvm::PHINode*, /*selfReferencing*/bool>, 4>& orderedPHIs);
	// Callbacks implemented by derived classes
	virtual void handleRecursivePHIDependency(const llvm::Instruction* incoming) = 0;
	virtual void handlePHI(const llvm::PHINode* phi, const llvm::Value* incoming, bool selfReferencing) = 0;
	virtual void handlePHIStackGroup(const std::vector<const llvm::PHINode*>& phiToHandle) = 0;
	// Called for every register which is either assigned or used by PHIs in the edge
	virtual void setRegisterUsed(uint32_t reg) {};
	virtual void addRegisterUse(uint32_t reg) {};
	virtual void reportRegisterUse() const {};
	virtual void removeRegisterUse(uint32_t reg) {};
	virtual void resetRegistersState() {};
	llvm::DenseSet<const llvm::PHINode*> phiToBeSkipped;
};

class PHIHandlerUsingTemp : public EndOfBlockPHIHandler
{
public:
	PHIHandlerUsingTemp(const PointerAnalyzer& PA, EdgeContext& edgeContext) : EndOfBlockPHIHandler(PA, edgeContext)
	{
	}
	// Callbacks that have to be implemented by derived classes
	void handleRecursivePHIDependency(const llvm::Instruction* incoming) override = 0;
	void handlePHI(const llvm::PHINode* phi, const llvm::Value* incoming, bool selfReferencing) override = 0;
	// Callbacks that should NOT be implemented by derived classes
	void handlePHIStackGroup(const std::vector<const llvm::PHINode*>& phiToHandle) override {}
};

class PHIHandlerUsingStack : public EndOfBlockPHIHandler
{
	EdgeContext edgeContext;
public:
	PHIHandlerUsingStack(const PointerAnalyzer& PA) : EndOfBlockPHIHandler(PA, edgeContext)
	{
	}
	// Callbacks that should NOT be implemented by derived classes
	void handleRecursivePHIDependency(const llvm::Instruction* incoming) override {}
	void handlePHI(const llvm::PHINode* phi, const llvm::Value* incoming, bool selfReferencing) override {}
	// Callbacks that have to be implemented by derived classes
	void handlePHIStackGroup(const std::vector<const llvm::PHINode*>& phiToHandle) override = 0;
};

}
namespace llvm
{
template <> struct GraphTraits<cheerp::EndOfBlockPHIHandler::DependencyGraph*> {
	typedef cheerp::EndOfBlockPHIHandler::GraphNode NodeType;
	typedef NodeType* NodeRef;
	typedef llvm::mapped_iterator<SmallVectorImpl<uint32_t>::iterator, std::function<cheerp::EndOfBlockPHIHandler::GraphNode*(uint32_t)>> ChildIteratorType;

	static NodeType *getEntryNode(cheerp::EndOfBlockPHIHandler::DependencyGraph* G) { return G->getOrCreate(G->getEntry()); }
	static inline ChildIteratorType child_begin(NodeType *N) {
		return ChildIteratorType(N->Succs.begin(), [N](uint32_t id){ return N->Graph.getOrCreate(id);});
	}
	static inline ChildIteratorType child_end(NodeType *N) {
		return ChildIteratorType(N->Succs.end(), [](uint32_t id){ llvm_unreachable("dereferencing past-the-end iterator");return nullptr;});
	}
};
}

#endif //_CHEERP_PHIHANDLER_H
