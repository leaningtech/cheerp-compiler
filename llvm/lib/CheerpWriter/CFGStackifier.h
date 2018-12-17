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
		};
		struct BranchState {
			bool IsBranchRoot{false};
			enum RenderBranchCase {
				ONLY_FORWARDS,
				DEFAULT_FORWARD,
				DEFAULT_NESTED,
			};
			RenderBranchCase Case{ONLY_FORWARDS};
		};

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
		int getId() const {
			return id;
		}
		Scope* insertScope(Scope s);
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
		void setBranchState(BranchState::RenderBranchCase RBC)
		{
			BS.IsBranchRoot = true;
			BS.Case = RBC;
		}
		bool isBranchRoot() const
		{
			return BS.IsBranchRoot;
		}
		BranchState::RenderBranchCase getBranchCase() const
		{
			return BS.Case;
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
		BranchState BS;
	};

	CFGStackifier(const llvm::Function &F, llvm::LoopInfo& LI, llvm::DominatorTree& DT);
	void render(RenderInterface& ri, bool asmjs);
private:
	std::vector<Block> BlockList;
	std::unordered_map<llvm::BasicBlock*, int> BlockIdMap;
};


}

#endif
