//===-- Cheerp/CFGStackifier.h - Cheerp rendering helper ---------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2018 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_CFG_STACKIFIER_H
#define _CHEERP_CFG_STACKIFIER_H

#include "Relooper.h"

#include "llvm/IR/Module.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/Dominators.h"

#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/PointerAnalyzer.h"

#include <unordered_map>
#include <vector>
#include <list>
#include <unordered_set>

namespace cheerp
{

class CFGStackifier
{
public:
	class Block {
	public:
		enum Marker {
			LOOP,
			LOOP_END,
			BLOCK,
			BLOCK_END,
			BRANCH,
			BRANCH_END
		};
		struct Scope {
			Marker kind;
			int start;
			int end;
			bool label;
		};
		using ScopeIter = std::list<Scope>::iterator;
		explicit Block(llvm::BasicBlock *BB, int id) : BB(BB), id(id)
		{
		}
		llvm::BasicBlock* getBB() const
		{
			return BB;
		}
		std::list<Scope>& getScopes()
		{
			return scopes;
		}
		const std::list<Scope>& getScopes() const
		{
			return scopes;
		}
		const Scope* getTopEndScope() const
		{
			const Scope* ret = nullptr;
			for (const auto& s: scopes)
			{
				switch (s.kind)
				{
					case LOOP_END:
					case BLOCK_END:
					case BRANCH_END:
						if (!ret || s.start < ret->start)
							ret = &s;
						break;
					default:
						return ret;
				}
			}
			return ret;
		}
		int getId() const
		{
			return id;
		}
		ScopeIter insertScope(Scope s);
		void removeScope(ScopeIter si);
		Scope* getBranchStart()
		{
			auto it = std::find_if(scopes.begin(), scopes.end(), [](const Scope& s) {
				return s.kind == BRANCH;
			});
			if (it != scopes.end())
				return &*it;
			return nullptr;
		}
		const Scope* getBranchStart() const {
			return const_cast<Block*>(this)->getBranchStart();
		}
		void addNaturalPred(int id)
		{
			naturalPreds.insert(id);
		}
		bool isNaturalPred(int id) const
		{
			return naturalPreds.count(id);
		}
		bool operator==(const llvm::BasicBlock* Other) const { return BB == Other; }
#ifdef DEBUG_CFGSTACKIFIER
		void dump() const;
#endif
	private:
		llvm::BasicBlock *BB;
		int id;
		std::list<Scope> scopes;
		std::unordered_set<int> naturalPreds;
	};

	CFGStackifier(const llvm::Function &F, llvm::LoopInfo& LI, llvm::DominatorTree& DT);
	void render(RenderInterface& ri, const Registerize& R, const PointerAnalyzer& PA, bool asmjs);
private:
	const Block& getBlock(int id) const
	{
		assert(id >= 0 && id < (int)BlockList.size());
		return BlockList[id];
	}
	const Block& getBlock(llvm::BasicBlock* BB) const
	{
		auto it = BlockIdMap.find(BB);
		assert(it != BlockIdMap.end());
		const Block& B =  getBlock(it->second);
		assert(B.getBB() == BB);
		return B;
	}

	std::vector<Block> BlockList;
	std::unordered_map<llvm::BasicBlock*, int> BlockIdMap;
};


}

#endif
