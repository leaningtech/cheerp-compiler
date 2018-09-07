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

#define DEBUG_CFGSTACKIFIER 0

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
			BLOCK_END
		};
		struct Scope {
			Marker kind;
			size_t start;
			size_t end;
		};

		explicit Block(llvm::BasicBlock *BB, size_t id) : BB(BB), id(id)
		{
		}
		llvm::BasicBlock* getBB() const
		{
			return BB;
		}
		std::vector<Scope>& getScopes()
		{
			return scopes;
		}
		const std::vector<Scope>& getScopes() const
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
						if (!ret || s.start < ret->start)
							ret = &s;
						break;
					default:
						return ret;
				}
			}
			return ret;
		}
		size_t getId() const {
			return id;
		}
		void insertScope(Scope s);
		bool operator==(const llvm::BasicBlock* Other) const { return BB == Other; }
#ifdef DEBUG_CFGSTACKIFIER
		void dump() const;
#endif
	private:
		llvm::BasicBlock *BB;
		std::vector<Scope> scopes;
		size_t id;
	};

	explicit CFGStackifier(const llvm::Function &F, llvm::LoopInfo& LI, llvm::DominatorTree& DT);
	void render(RenderInterface& ri, bool asmjs);

	const std::vector<Block>& getBlockList() const { return BlockList; };
	const std::unordered_map<llvm::BasicBlock*, size_t>& getBlockIdMap() const { return BlockIdMap; };
private:
	std::vector<Block> BlockList;
	std::unordered_map<llvm::BasicBlock*, size_t> BlockIdMap;

	void orderBlocks(llvm::Function& F, llvm::LoopInfo& LI, llvm::DominatorTree& DT);
	void addLoopMarkers(Block& B, llvm::LoopInfo& LI);
	void addBlockMarkers(Block& B, llvm::DominatorTree& DT);
	size_t findLoopEnd(llvm::Loop* L, size_t start);
};


}

#endif
