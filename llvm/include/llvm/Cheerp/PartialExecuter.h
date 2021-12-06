//===-- Cheerp/PartialExecuter.h - Cheerp utility code ---------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2021 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_PARTIAL_EXECUTER_H
#define _CHEERP_PARTIAL_EXECUTER_H

#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Module.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/Cheerp/DeterministicUnorderedSet.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Cheerp/BuiltinInstructions.h"
#include <unordered_map>

namespace cheerp
{

	class ModuleData;
/**
 */
class PartialExecuter : public llvm::ModulePass
{
public:
	static char ID;
	PartialExecuter();
	bool runOnModule(llvm::Module& module) override;
	llvm::BasicBlock* getImmediateDom(llvm::BasicBlock* bb, bool status);
	llvm::BasicBlock* getOnlyOne(llvm::BasicBlock* bb);

private:
	bool runOnFunction(llvm::Function&);
	void processModule(llvm::Module & module);
	void processFunction(llvm::Function & F);
	ModuleData* moduleData;

	void getAnalysisUsage(llvm::AnalysisUsage& AU) const override;
	llvm::StringRef getPassName() const override;
	std::unordered_map<const llvm::BasicBlock*, int> groupBasicBlocks(const llvm::Function& F);

	std::map<llvm::BasicBlock*, std::set<llvm::BasicBlock*> > implementationSCC;
	std::map<llvm::BasicBlock*, std::set<llvm::BasicBlock*> > implementationGLOBAL;
	void findNextVisited(llvm::Function& F, bool status);

	void classifyFunctions(const llvm::Module& module);
	std::set<const llvm::Function*> isPreExecutableFunction;
};

inline llvm::Pass * createPartialExecuterPass()
{
	return new PartialExecuter();
}

}

#endif
