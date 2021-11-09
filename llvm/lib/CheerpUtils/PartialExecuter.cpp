//===-- PartialExecuter.cpp - Remove unused functions/globals -----------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2021 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "PartialExecuter"
#include <algorithm>
#include <llvm/Analysis/OptimizationRemarkEmitter.h>
#include "llvm/InitializePasses.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Cheerp/CommandLine.h"
#include "llvm/Cheerp/PartialExecuter.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/Cheerp/LinearMemoryHelper.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IRBuilder.h"
#include "../lib/ExecutionEngine/Interpreter/Interpreter.h"

#include "llvm/InitializePasses.h"
#include "llvm/Cheerp/PreExecute.h"
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/ExecutionEngine/Interpreter.h"
#include "llvm/ExecutionEngine/FunctionMap.h"
#include "llvm/IR/Constants.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"
#include <string.h>
#include <algorithm>


using namespace llvm;

//STATISTIC(NumRemovedGlobals, "Number of unused globals which have been removed");

namespace cheerp {

using namespace std;

char PartialExecuter::ID = 0;

StringRef PartialExecuter::getPassName() const
{
	return "PartialExecuter";
}

PartialExecuter::PartialExecuter()
	: llvm::ModulePass(ID)
{
}

void PartialExecuter::getAnalysisUsage(AnalysisUsage& AU) const
{
	AU.addPreserved<cheerp::PointerAnalyzer>();
	AU.addPreserved<cheerp::Registerize>();

	llvm::ModulePass::getAnalysisUsage(AU);
}

std::unordered_map<const BasicBlock*, int> PartialExecuter::groupBasicBlocks(const Function& F)
{
  std::unordered_map<const BasicBlock*, int> map;
  int SccNum = 0;
  for (scc_iterator<const Function *> It = scc_begin(&F); !It.isAtEnd();
       ++It, ++SccNum) {
    // Ignore single-block SCCs since they either aren't loops or LoopInfo will
    // catch them.
    const std::vector<const BasicBlock *> &Scc = *It;
    
    for (const auto *BB : Scc) {
      	map[BB] = SccNum;
    }
  }
  return map;
}

class PartialInterpreter : public llvm::Interpreter {
	std::unordered_set<const llvm::Value*> computed;
	const llvm::BasicBlock* old;
public:
	explicit PartialInterpreter(std::unique_ptr<llvm::Module> M)
		: llvm::Interpreter(std::move(M), /*preExecute*/false)
	{
		llvm::errs() << "BINGO\n";
		old = nullptr;
	}
	bool isValueComputed(const llvm::Value* V) const
	{
		if (isa<Constant>(V))
			return true;
		if (isa<Argument>(V))
		{
			//TODO: depends on the argument
			return false;
		}
		if (const PHINode* phi = dyn_cast<PHINode>(V))
		{
			return computed.count(phi->getIncomingValueForBlock(old));
		}
		if (isa<Instruction>(V))
		{
			return computed.count(V);
		}
		if (isa<BasicBlock>(V))
			return true;
		return false;
	}
	bool areOperandsComputed(const llvm::Instruction& I) const
	{
		for (auto& op : I.operands())
		{
			if (!isValueComputed(op))
				return false;
		}
		return true;
	}
	bool hasToBeSkipped(llvm::Instruction& I) const
	{
		if (isa<CallBase>(I))
			return true;
		if (isa<StoreInst>(I))
			return true;
		if (!areOperandsComputed(I))
			return true;
		return false;
	}
	void visitOuter(llvm::Instruction& I) override
	{
		const bool skip = hasToBeSkipped(I);
		old = I.getParent();

		if (skip)
			llvm::errs() << "        ";
		else
			llvm::errs() << "compute ";
		llvm::errs() << I << "\n";
		

		if (skip)
		{
			if (BranchInst* BR = dyn_cast<BranchInst>(&I))
			{
				visitBranchInst(*BR);
			}
			return; 
		}
		computed.insert(&I);
		visit(I);
	}
/*	void visitBranchInst(BranchInst &I) {
		llvm::errs() << "BING\n";
  ExecutionContext &SF = ECStack.back();
  BasicBlock *Dest;

  Dest = I.getSuccessor(0);          // Uncond branches have a fixed dest...
  if (!I.isUnconditional()) {
    Value *Cond = I.getCondition();
    if (getOperandValue(Cond, SF).IntVal == 0) // If false cond...
      Dest = I.getSuccessor(1);
  }
  SwitchToNewBasicBlock(Dest, SF);
}
*/


/// Create a new interpreter object.
///
static ExecutionEngine* create(std::unique_ptr<Module> M,
                                     std::string *ErrStr) {
  // Tell this Module to materialize everything and release the GVMaterializer.
  if (Error Err = M->materializeAll()) {
    std::string Msg;
    handleAllErrors(std::move(Err), [&](ErrorInfoBase &EIB) {
      Msg = EIB.message();
    });
    if (ErrStr)
      *ErrStr = Msg;
    // We got an error, just return 0
    return nullptr;
  }

  return new PartialInterpreter(std::move(M));
}


};


bool PartialExecuter::runOnModule( llvm::Module & module )
{
	using namespace llvm;

	for (Function& F : module)
	{
		std::unordered_map<const BasicBlock*, int> MAP = groupBasicBlocks(F);

		std::unordered_map<int, std::vector<const BasicBlock*>> INVERSE;
		for (auto& x : MAP)
		{
			INVERSE[x.second].push_back(x.first);
		}

		for (auto& x : INVERSE)
		{
			llvm::errs() << x.first << ":\t";
	//		for (auto & y : x.second)
				llvm::errs() << x.second.size() << "\n";
		}



		if (F.getName() == "printf")
		{
using namespace llvm;
    llvm::ExecutionEngine *currentEE;
    llvm::Module *currentModule;
    std::unique_ptr<Allocator> allocator;
    

std::unique_ptr<Module> uniqM(&module); 
    std::string error; 
        std::string triple = sys::getProcessTriple();
    const Target *target = TargetRegistry::lookupTarget(triple, error);

    TargetMachine* machine = target->createTargetMachine(triple, "", "", TargetOptions(), None); 
 
//    EngineBuilder builder(std::move(uniqM)); 
//    builder.setEngineKind(llvm::EngineKind::PreExecuteInterpreter); 
 //   builder.setOptLevel(CodeGenOpt::Default); 
  //  builder.setErrorStr(&error); 
  //  builder.setVerifyModules(true); 
 
  //  currentEE = builder.create(machine); 
       currentEE = PartialInterpreter::create(std::move(uniqM), &error);
    assert(currentEE && "failed to create execution engine!"); 
//    currentEE->InstallStoreListener(StoreListener); 
  //  currentEE->InstallAllocaListener(AllocaListener); 
    //currentEE->InstallRetListener(RetListener); 
 //   currentEE->InstallLazyFunctionCreator(LazyFunctionCreator); 

    allocator = std::make_unique<Allocator>(*currentEE->ValueAddresses);

    PartialInterpreter* X = (PartialInterpreter*)(currentEE);
    X->runFunction(&F, std::vector< GenericValue >(100));
 //   currentEE->runFunction(&F, std::vector< GenericValue >(100));
    
    /*
			llvm::errs() << F.getName() << "\n";
    std::unique_ptr<Module> uniqM(&module); 
			llvm::errs() << F.getName() << "\n";
			PartialInterpreter p(std::move(uniqM));
			llvm::errs() << F.getName() << "\n";

			p.runFunction(&F, ArrayRef<GenericValue>())	
*/
		}


		if (F.hasAddressTaken())
			continue;

		if (F.isDeclaration())
			continue;

/*		llvm::errs() << F.getName() << "\n";
		{
		for (auto& u : F.uses())
			llvm::errs() << *u.getUser() << "\n";
		llvm::errs() << "\n";
		}
*/	}




	bool changed = false;
	return changed;
}

}

using namespace cheerp;

INITIALIZE_PASS_BEGIN(PartialExecuter, "PartialExecuter", "Partially execute functions",
                      false, false)
INITIALIZE_PASS_END(PartialExecuter, "PartialExecuter", "Partially execute functions",
                    false, false)
