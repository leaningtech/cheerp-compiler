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
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/Analysis/CallGraphSCCPass.h"


#include "llvm/Transforms/IPO/FunctionAttrs.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SCCIterator.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/AssumptionCache.h"
#include "llvm/Analysis/BasicAliasAnalysis.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/Analysis/CGSCCPassManager.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/Analysis/CallGraphSCCPass.h"
#include "llvm/Analysis/CaptureTracking.h"
#include "llvm/Analysis/LazyCallGraph.h"
#include "llvm/Analysis/MemoryBuiltins.h"
#include "llvm/Analysis/MemoryLocation.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Use.h"
#include "llvm/IR/User.h"
#include "llvm/IR/Value.h"
#include "llvm/InitializePasses.h"
#include "llvm/Pass.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/Utils/Local.h"
#include <cassert>
#include <iterator>
#include <map>
#include <vector>

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
       ++It, --SccNum) {
    // Ignore single-block SCCs since they either aren't loops or LoopInfo will
    // catch them.
    const std::vector<const BasicBlock *> &Scc = *It;
    
    for (const auto *BB : Scc) {
      	map[BB] = SccNum;
    }
  }
  return map;
}

typedef std::pair<const llvm::Value*, GenericValue> ValueGenericValuePair;
typedef std::vector<ValueGenericValuePair> LocalState;
//typedef std::unordered_map<ValueGenericValuePair> LocalStateMAP;

class PartialInterpreter : public llvm::Interpreter {
	std::unordered_set<const llvm::Value*> computed;
	const llvm::BasicBlock* old;
public:
	void visitBasicBlock(LocalState& state, llvm::BasicBlock* BB, llvm::BasicBlock* from=nullptr);
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

void visitBasicBlock2(LocalState& state, llvm::BasicBlock* BB, llvm::BasicBlock* from=nullptr)
{
	std::unique_ptr<Module> uniqM((BB)->getParent()->getParent()); 
	std::string error; 
	PartialInterpreter* currentEE = (PartialInterpreter*)(PartialInterpreter::create(std::move(uniqM), &error));
	std::unique_ptr<Allocator> allocator;
	allocator = std::make_unique<Allocator>(*currentEE->ValueAddresses);

	currentEE->visitBasicBlock(state, BB, from);
}
void PartialInterpreter::visitBasicBlock(LocalState& state, llvm::BasicBlock* BB, llvm::BasicBlock* from)
{
	using namespace llvm;

	
//	LocalStateMAP stateMap(state.begin(), state.end());

	ExecutionContext& executionContext = getSingleStack();
	executionContext.CurFunction = BB->getParent();	
	executionContext.CurBB = BB;
	executionContext.CurInst = BB->begin();

	std::vector<std::pair<const llvm::Value*, const llvm::Value*> > incomings;
	while (PHINode* phi = dyn_cast<PHINode>(&*executionContext.CurInst))
	{
		if (from)
		{
			const llvm::Value* incoming = phi->getIncomingValueForBlock(from);
			incomings.push_back({phi, incoming});
		}
		executionContext.CurInst++;
	}
	executionContext.Caller = nullptr;
	for (auto& p : state)
		executionContext.Values[const_cast<llvm::Value*>(p.first)] = p.second;

	for (auto& p : incomings)
		if (executionContext.Values.count(const_cast<llvm::Value*>(p.second)))
			executionContext.Values[const_cast<llvm::Value*>(p.first)] = executionContext.Values[const_cast<llvm::Value*>(p.second)];


	while (executionContext.CurInst != BB->end())
	{
		visitOuter(*executionContext.CurInst);
		executionContext.CurInst++;
	}
}

LocalState intersection(const LocalState& lhs, const LocalState& rhs)
{
	//TODO: to implement
	return lhs;
}

class FunctionVisitingData;

class SCCVisitingData
{
	FunctionVisitingData& functionVisitingData;
	const std::vector<const llvm::BasicBlock*> BBs;
	std::unordered_set<const llvm::BasicBlock*> reacheableBBs;
	const int IDscc;
	const llvm::BasicBlock* entryPoint;
	bool hasEntryPointBeenSet{false};
	LocalState localState;
	bool hasLocalStateBeenInitialized{false};
	void setPossibleEntryPoint(const llvm::BasicBlock* BB)
	{
		if (!hasEntryPointBeenSet)
			entryPoint = BB;
		if (entryPoint != BB)
			entryPoint = nullptr;
		hasEntryPointBeenSet = true;
	}
	const llvm::BasicBlock* getSingleEntryPoint() const
	{
		return entryPoint;
	}
public:
	SCCVisitingData(FunctionVisitingData& data, const int IDscc)
		: functionVisitingData(data), IDscc(IDscc)
	{
	}
	void visit()
	{
		const llvm::BasicBlock* curr = getSingleEntryPoint();

		if (!curr)
		{
			setAsReachableWholeSCC();
			return;
		}

		//TODO: create PartialInterpreter, and fire it away! (controlling where it goes)
		//TODO: setAsReachable has to be called appropriately
	}
	void addPossibleEntryEdge(const llvm::BasicBlock* from, const llvm::BasicBlock* to, const LocalState& state)
	{
		setPossibleEntryPoint(to);

		LocalState stateWithPHI = state;
		//TODO: Add phi from 'from' to stateWithPHI (not adding them is still valid!)

		if (!hasLocalStateBeenInitialized)
			localState = stateWithPHI;
		else
			localState = intersection(localState, stateWithPHI);
		hasLocalStateBeenInitialized = true;
	}
	void addFunctionEntryPoint(const llvm::BasicBlock* entry, const llvm::CallInst& ci)
	{
		setPossibleEntryPoint(entry);

		//TODO: initialize local state with the known arguments of ci
		hasLocalStateBeenInitialized = true;
	}
	void setAsReachable(const llvm::BasicBlock* bb)
	{
		reacheableBBs.insert(bb);
	}
	bool isCurrentlyReachable(const llvm::BasicBlock* bb)
	{
		return reacheableBBs.count(bb);
	}
	void setAsReachable(const llvm::BasicBlock* from, const llvm::BasicBlock* to)
	{
		setAsReachable(from);

		//TODO: visit BBs on the path between from and to
		//(it's not needed to have another datastructure, since it's sufficient to stop adding to the stack when they are in reacheableBBs 


		//TODO: IMPROVMENT: reachability information can be attached to every EDGE (so BBs might be reachable but certain jumps might not be taken in practice)
	}
	void setAsReachableWholeSCC()
	{
		reacheableBBs.insert(BBs.begin(), BBs.end());
		//TODO: do visit and sign other reachable SCC as reachable
	}
};

class FunctionVisitingData
{
public:

};


bool PartialExecuter::runOnFunction(llvm::Function& F)
{
	using namespace llvm;

//For each SCC (process in order)
//	if single point of entry: start from there, otherwise (no point of entry -> all unreachable / multiple ones -> all reachable)
//	execute from single point of entry (+ state that's intersection of possible path to it)
//	if (unconditional or decidable) -> follow path, otherwise
//		if immediate dominator is in the SCC, jump there(a refinement would be immediate dominator in SCC while invalidating all otherwise reachable SCC) or bail out
//	bail out:
//		sign as reachable all out-going edges from the SCC (keeping only the state pre-SCC) + all BB in the SCC
		if (F.hasAddressTaken())
			return false;


		if (F.isDeclaration())
			return false;

	llvm::errs() << F.getName() << "\n";
	std::unordered_map<const BasicBlock*, int> MAP = groupBasicBlocks(F);

	std::map<int, std::vector<const BasicBlock*>> INVERSE;
	for (auto& x : MAP)
	{
		INVERSE[x.second].push_back(x.first);
	}

	for (auto& x : INVERSE)
	{
		llvm::errs() << x.first << ":\t";
//		for (auto & y : x.second)
			llvm::errs() << x.second.size() << "\t" << x.second.front()->getName() << "\n";
	}

	{

		LocalState state;
		visitBasicBlock2(state, &F.getEntryBlock());
		if (false)
		{
		using namespace llvm;
	    llvm::ExecutionEngine *currentEE;
	    llvm::Module *currentModule;
	    std::unique_ptr<Allocator> allocator;
	    

	std::unique_ptr<Module> uniqM(F.getParent()); 
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


/*		llvm::errs() << F.getName() << "\n";
		{
		for (auto& u : F.uses())
			llvm::errs() << *u.getUser() << "\n";
		llvm::errs() << "\n";
		}
*/	}
	}
	return true;
}

bool PartialExecuter::runOnModule( llvm::Module & module )
{
	using namespace llvm;

	for (Function& F : module)
	{
		if (F.getName() != "printf")
			continue;
		runOnFunction(F);
	}

	bool changed = false;
	return changed;
}

}

using namespace cheerp;

INITIALIZE_PASS_BEGIN(PartialExecuter, "PartialExecuter", "Partially execute functions",
                      false, false)
INITIALIZE_PASS_END(PartialExecuter, "PartialExecuter", "Partially execute functions",
                    false, false)
