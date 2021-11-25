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
#include "llvm/Pass.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Dominators.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Cheerp/DeterministicUnorderedSet.h"

#include <unordered_map>
#include <unordered_set>
#include <queue>


#include "llvm/ADT/SCCIterator.h"
#include "llvm/InitializePasses.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/SCCIterator.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Verifier.h"


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
#include "llvm/Cheerp/DeterministicUnorderedSet.h"


#include "llvm/Analysis/RegionInfo.h"
#include "llvm/Analysis/RegionIterator.h"
#include "llvm/Analysis/RegionPass.h"


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
//	auto &RI = AU.getResult<RegionInfoAnalysis>(F);


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
typedef std::unordered_map<const llvm::Value*, GenericValue> LocalStateMAP;

class PartialInterpreter : public llvm::Interpreter {
	const llvm::BasicBlock* fromBB{nullptr};	
	std::vector<std::pair<const llvm::Value*, GenericValue> > incomings;
public:
	std::unordered_set<const llvm::Value*> computed;
	void setIncomingBB(const llvm::BasicBlock* from)
	{
		fromBB = from;
	}
	const llvm::BasicBlock* getIncomingBB() const
	{
		return fromBB;
	}
	enum VisitingPolicy { NORMAL, REMOVE_VALUES};
	template <VisitingPolicy policy>
	llvm::BasicBlock* visitBasicBlock(llvm::BasicBlock* BB, llvm::BasicBlock* from=nullptr);
	explicit PartialInterpreter(std::unique_ptr<llvm::Module> M)
		: llvm::Interpreter(std::move(M), /*preExecute*/false)
	{
	}
	bool isValueComputed(const llvm::Value* V) const
	{
		if (isa<Constant>(V))
		{
			if (const GlobalVariable* GVar = dyn_cast<GlobalVariable>(V))
			{
				if (!GVar->hasInitializer() || GVar->isExternallyInitialized())
					return false;
			}
			return true;
		}
		if (isa<Argument>(V))
		{
			return computed.count(V);
		}
		if (isa<Instruction>(V))
		{
			return computed.count(V);
		}
		if (isa<BasicBlock>(V))
			return true;
		return false;
	}
	bool areOperandsComputed(const llvm::Instruction& I)
	{
		if (isa<SelectInst>(I))
		{
			if (isValueComputed(I.getOperand(0)))
			{
				GenericValue V = getOperandValue(I.getOperand(0), getLastStack());
				if (V.IntVal == 0u)
					if (isValueComputed(I.getOperand(2)))
						return true;
				else
					if (isValueComputed(I.getOperand(1)))
						return true;

			}	
		}
		for (auto& op : I.operands())
		{
			if (!isValueComputed(op))
				return false;
		}
		return true;
	}
	bool hasToBeSkipped(llvm::Instruction& I) //TODO:const
	{
		if (isa<LandingPadInst>(I)) //LandingPadInst are not handled by Interpreter
			return true;
		if (isa<CallBase>(I))
		{
//			if (isa<InvokeInst>(I)) //Invokes are not handled by Interpreter
//				return true;
		//	return true;
			CallBase& CB = cast<CallBase>(I);
			if (!CB.getCalledFunction())
				return true;
	//		if (!CB.getCalledFunction() || CB.getCalledFunction()->getName() != "memchr")
	//			return true;
	//		else
			{
int i=0;
bool problems = false;
				for (auto& op : CB.args())
	{
		Function* FF = dyn_cast<Function>(CB.getCalledFunction());
			if (i >= FF->getFunctionType()->getNumParams())
				break;
		if (isValueComputed(op))
		{
			//TODO: fix Vaarg
		//	llvm::errs() << *op << "\taaaaa\n";
			//TODO: no recursion!!
			computed.insert(CB.getCalledFunction()->getArg(i));
}
else
{
//	llvm::errs() << i << "-th argument not known\n";
	problems = true;
}
i++;
}

			if (CB.getCalledFunction()->getName() != "memchr")
				return true;
			}
		}
//TODO??	if (isa<StoreInst>(I))
//			return true;

		if (isa<VAArgInst>(I))
			return true;
		if (!areOperandsComputed(I))
			return true;
		if (StoreInst* load = dyn_cast<StoreInst>(&I))
		{
			return true;
  GenericValue SRC = getOperandValue(load->getPointerOperand(), getLastStack());
  GenericValue *Ptr = (GenericValue*)GVTORP(SRC);
if ((long long)Ptr < 100)
	return true;
		}
		if (LoadInst* load = dyn_cast<LoadInst>(&I))
		{
		//	return true;
  GenericValue SRC = getOperandValue(load->getPointerOperand(), getLastStack());
  GenericValue *Ptr = (GenericValue*)GVTORP(SRC);
if ((long long)Ptr < 100)
	return true;
//  if (Ptr->IntVal:w
		 
std::vector<std::pair<long long, long long> > V;
for (auto& gv : I.getFunction()->getParent()->globals())
{
	GlobalVariable* GV = dyn_cast<GlobalVariable>(&gv);
	if (!GV)
		continue;
	if (!GV->hasInitializer())
		continue;
  GenericValue SRC = getOperandValue(GV, getLastStack());
  GenericValue *Ptr = (GenericValue*)GVTORP(SRC);
//llvm ::errs() << GV->getName() << "\t" <<"\t" << (long long)Ptr<<"\t"<< GV->isConstant()<<"\n";
if (GV->isConstant())
  V.push_back({(long long)Ptr, ((long long)Ptr)+getDataLayout().getTypeAllocSize(GV->getInitializer()->getType())});
}


//std::sort(V.begin(), V.end());
for (int i=0; i<V.size(); i++)
{
	if ((long long)Ptr >= V[i].first && (long long)Ptr < V[i].second)
		return false;
}

if (false)
if (load->getPointerOperand()->getName() == "flags.i" ||
		load->getPointerOperand()->getName() == "width.i" ||
		load->getPointerOperand()->getName() == "prec.i" ||
		load->getPointerOperand()->getName() == "dprec.i" ||
		load->getPointerOperand()->getName() == "lead.i")

{
//	llvm::errs() << I << "\n";
	return false;
}


return true;



			Value* ptr = load->getPointerOperand();
			if (GetElementPtrInst* gep = dyn_cast<GetElementPtrInst>(ptr))
			{
				if (!gep->isInBounds())
					return true;
				//TODO: CHECK whether the location is also constant!!
				return false;
			}
			//ConstantExpr are also possibly good
			//PHI also
			//
			//In reality here we need a map of valid ptrs to be updated
			//TODO: 
			return false;
		}	
		return false;
	}
bool curInstModifyed;
void clearFunction(llvm::Function& F)
{
	for (Instruction& I : instructions(F))
	{
		visitOuter<VisitingPolicy::REMOVE_VALUES>(I);
	}
}

template <PartialInterpreter::VisitingPolicy policy>
void visitOuter(llvm::Instruction& I) 
	{
			if (policy == REMOVE_VALUES)
			{
				if (computed.count(&I))
					computed.erase(&I);
				return;
			}

		curInstModifyed = false;
	//	llvm::errs() << "visitOuter " << I << "\n";
		
		if (PHINode* phi = dyn_cast<PHINode>(&I))
		{
			const llvm::BasicBlock* from = getIncomingBB();
			if (from)
			{
				llvm::Value* incoming = phi->getIncomingValueForBlock(from);
				assert(incoming);
				if (isValueComputed(incoming))
					incomings.push_back({phi, getOperandValue(incoming, getLastStack())});
			}
			return;
		}
		else
		{
			for (auto& p : incomings)
			{
		//		llvm::errs() <<"PHI-->\t"<< *p.first << "\t" ;
		//		p.second.print("");
				getLastStack().Values[const_cast<llvm::Value*>(p.first)] = p.second;
				computed.insert(p.first);
			}
			incomings.clear();
		}

		const bool skip = hasToBeSkipped(I);
		const bool term = I.isTerminator();

		if (policy == VisitingPolicy::REMOVE_VALUES)

if (true)
{
		if (skip || !term)
		{
			if (!skip)
			llvm::errs() << "compute ";
			else
			llvm::errs() << "        ";
		}
		else
			llvm::errs() << "COMPUTE ";
		llvm::errs() << I << "\n";
}
		if (skip)
		{
			if (BranchInst* BR = dyn_cast<BranchInst>(&I))
			{
				//visitBranchInst(*BR);
			}
			return; 
		}
		if (term)
		{

		if (sizeStack() > 1)
		{
	BasicBlock* next = nullptr;
	if (BranchInst* BI = dyn_cast<BranchInst>(&I))
	{
		if (BI->isConditional())
		{
			if (isValueComputed(BI->getCondition()))
			{
				GenericValue V = getOperandValue(BI->getCondition(), getLastStack());
				if (V.IntVal == 0u)
					next = BI->getSuccessor(1);
				else
					next = BI->getSuccessor(0);
			}
		}
		else
			next = BI->getSuccessor(0);
	}
	else if (ReturnInst* RI = dyn_cast<ReturnInst>(&I))
	{
		visit(I);
curInstModifyed = true;
return;
	}
	assert(next);
	setIncomingBB(I.getParent());
	getLastStack().CurBB = next;
	getLastStack().CurInst = next->begin();
curInstModifyed = true;

		}
			return;
		}

		computed.insert(&I);
		visit(I);
		if (policy == PartialInterpreter::VisitingPolicy::REMOVE_VALUES)
		{
			if (getLastStack().Values.count(&I))
			       getLastStack().Values.erase(&I);	
		}
	}

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

void visitBasicBlocksResettingState(PartialInterpreter& PI, std::set<llvm::BasicBlock*>& visitedTotal, std::map<llvm::BasicBlock*, std::set<llvm::BasicBlock*>>& visitedEdgesTotal, llvm::BasicBlock* start, llvm::BasicBlock* upto = nullptr)
{
	std::vector<llvm::BasicBlock*> toVisit;
	std::set<llvm::BasicBlock*> visited;

	if (upto)
		visited.insert(upto);

	for (BasicBlock* succ : successors(start))
	{
		visitedEdgesTotal[succ].insert(start);
		if (visited.insert(succ).second)
			toVisit.push_back(succ);
	}

	while (!toVisit.empty())
	{
		llvm::BasicBlock* curr = toVisit.back();
		toVisit.pop_back();
		if (curr->getName()  == "if.else24.i.i.i")
			continue;


		visitedTotal.insert(curr);

		for (Instruction& I : *curr)
		{
			if (PI.getLastStack().Values.count(&I))
				PI.getLastStack().Values.erase(&I);
			if (PI.computed.count(&I))
				PI.computed.erase(&I);
		}
		for (BasicBlock* succ : successors(curr))
		{
			visitedEdgesTotal[succ].insert(curr);
			if (visited.insert(succ).second)
				toVisit.push_back(succ);
		}
	}
}

std::vector<llvm::BasicBlock* > TOUNREACHABLE;
void removeEdgeBetweenBlocks(llvm::BasicBlock* from, llvm::BasicBlock* to)
{
	if(false)for (PHINode& phi : to->phis())
	{
		if (phi.getBasicBlockIndex(from) == -1)
			continue;
//		llvm::errs() << phi << "\n";
//		llvm::errs() << from->getName() << "\tCONDITIONAL\n";
		phi.removeIncomingValue(from, /*DeletePHIIfEmpty*/false);
	}
	//cleanup phi
//TODO: consider using to->removePredecessor(from) (then change to unreachable can used again??)
	//change terminator
	int Z = 0;
	for (BasicBlock* bb : predecessors(to))
		if (bb == from)
			Z++;
	for (int i=0; i<Z; i++)
	{
        to->removePredecessor(from);
	}
	if(false)for (PHINode& phi : to->phis())
	{
		if (phi.getBasicBlockIndex(from) == -1)
			continue;
//		llvm::errs() << phi << "\n";
//		llvm::errs() << from->getName() << "\tCONDITIONAL\n";
		phi.removeIncomingValue(from, /*DeletePHIIfEmpty*/false);
	}
if (true)
{	llvm::Instruction* I = from->getTerminator();
	if (BranchInst* BI = dyn_cast<BranchInst>(I))
	{
		if (BI->isConditional())
		{
			//Turn to unconditional
			llvm::BasicBlock* other = nullptr;
			if (BI->getSuccessor(0) != to)
				other = BI->getSuccessor(0);
			else
				other = BI->getSuccessor(1);
			BI->eraseFromParent();
			BranchInst* newBI = BranchInst::Create(other, from);
			BI = newBI;
		}
		else
		{
	for (PHINode& phi : to->phis())
	{
	//	if (phi.getBasicBlockIndex(from) >= 0)
	//		phi.addIncomingValue(from, /*DeletePHIIfEmpty*/false);
	}
			//TOUNREACHABLE.push_back(BI->getParent());
			//changeToUnreachable(BI);
			BI->eraseFromParent();
//        to->removePredecessor(from);
			UnreachableInst* unreach = new UnreachableInst(from->getParent()->getParent()->getContext(), from);
		}
	}
	else if (SwitchInst* SI = dyn_cast<SwitchInst>(I))
	{

    for (SwitchInst::CaseIt i = SI->case_end(), e = SI->case_begin(); i != e;) {
      --i;
      auto *Successor = i->getCaseSuccessor();
      if (Successor == to) {
    //    Successor->removePredecessor(from);
        SI->removeCase(i);
      }
    }
    if (SI->getDefaultDest() == to && SI->getNumSuccessors() == 1)
    {
	    SI->eraseFromParent();
  //      to->removePredecessor(from);
		UnreachableInst* unreach = new UnreachableInst(from->getParent()->getParent()->getContext(), from);

    }

	
	}
else
{
	assert(false);
}
}
}
	
llvm::CallBase* CICCIO = nullptr;
llvm::BasicBlock* visitBasicBlock2(PartialExecuter& PE, LocalState& state, llvm::BasicBlock* BB, llvm::BasicBlock* from=nullptr)
{
TOUNREACHABLE.clear();
//	if (!CICCIO)
//		return nullptr;
	std::unique_ptr<Module> uniqM((BB)->getParent()->getParent()); 
	std::string error; 
	PartialInterpreter* currentEE = (PartialInterpreter*)(PartialInterpreter::create(std::move(uniqM), &error));
	std::unique_ptr<Allocator> allocator;
	allocator = std::make_unique<Allocator>(*currentEE->ValueAddresses);

	std::set<const llvm::Value*> SET;

	ExecutionContext& executionContext = currentEE->getSingleStack();
	executionContext.CurFunction = BB->getParent();	
//	executionContext.CurBB = BB;
//	executionContext.CurInst = BB->begin();
	int i=0;
if (CICCIO)
	for (auto& op : CICCIO->args())
	{
		if (isa<Constant>(op))
		{
				llvm::errs() << *op << "\tbbbb\n";
			//				state.push_back({BB->getParent()->getArg(i), currentEE->getConstantValue((Constant*)(&*op))});//;GenericValue((llvm::Value*)op)});
		executionContext.Values[const_cast<llvm::Argument*>(BB->getParent()->getArg(i))] = currentEE->getConstantValue((Constant*)(&*op));//;GenericValue((llvm::Value*)op)});
		currentEE->computed.insert(BB->getParent()->getArg(i));
}
i++;
//break; //??
}

std::set<BasicBlock*> toBeVisited;
std::set<BasicBlock*> visited;

//TODO : add also other state
BasicBlock* curr = BB;
std::set<BasicBlock*> visitedTotal;
std::map<BasicBlock*, int> visitedTimes;
std::map<BasicBlock*, std::set<BasicBlock*>> visitedEdgesTotal;
while (curr)
{
bool stampa = false;

	currentEE->setIncomingBB(from);
	visitedTotal.insert(curr);
	visitedTimes[curr]++;

	if (from)
		visitedEdgesTotal[curr].insert(from);
	else
	{
		//insert all of them
	}


	BasicBlock* ret = nullptr;
      // 	if (visitedTimes[curr] < 100)
			ret = currentEE->visitBasicBlock<PartialInterpreter::VisitingPolicy::NORMAL>(curr, from);

	if (ret)
	{	
	//	llvm::errs() << "DECIDED................................... " << curr->getName() << "\n";
		from = curr;
		curr = ret;
	}
	else if (llvm::BasicBlock* ZZ = PE.getImmediateDom(curr, true))
	{
if (stampa)		llvm::errs() << "JUMP TO DOMINATOR ... \t" << curr->getName() << "\t" << ZZ->getName() << "\n";

		//TODO visitBasicBlock<PartialInterpreter::VisitingPolicy::REMOVE_VALUES>(x


		for (BasicBlock* b : successors(curr))
		{
			visitedEdgesTotal[b].insert(curr);
		}
		visitBasicBlocksResettingState(*currentEE, visitedTotal, visitedEdgesTotal, curr, ZZ);



//1. DO THE VISIT BETWEEN curr and ZZ
//2. refine for SCCs
		from = nullptr;
		curr = ZZ;
	}
	else if (llvm::BasicBlock* ZZ = PE.getOnlyOne(curr))
	{
		for (BasicBlock* b : successors(curr))
		{
			visitedEdgesTotal[b].insert(curr);
		}
	if(stampa)	llvm::errs() << "GO WITH THE FLOW\n";
		from = curr;
		curr = ZZ;
//		visitBasicBlocksResettingState(*currentEE, visitedTotal, visitedEdgesTotal, curr, ZZ);

	}
	else if (llvm::BasicBlock* ZZ = PE.getImmediateDom(curr, false))
	{
if(stampa)		llvm::errs() << "JUMP TO DOMINATOR PLIPPO... \t" << curr->getName() << "\t" << ZZ->getName() << "\n";

		//TODO visitBasicBlock<PartialInterpreter::VisitingPolicy::REMOVE_VALUES>(x

		for (BasicBlock* b : successors(curr))
		{
			visitedEdgesTotal[b].insert(curr);
		}


		visitBasicBlocksResettingState(*currentEE, visitedTotal, visitedEdgesTotal, curr, ZZ);

//1. DO THE VISIT BETWEEN curr and ZZ
//2. refine for SCCs
		from = nullptr;
		curr = ZZ;
	}
	else
	{
		break;
		//return nullptr;
		llvm::errs() << "OOOOPS\n";
		curr = nullptr;
		from = nullptr;
	}
}
std::set<BasicBlock*> X;
for (BasicBlock& bb : *BB->getParent())
	X.insert(&bb);

for (BasicBlock* bb : X)
{
	if (visitedTotal.count(bb) == 0)
	{
		changeToUnreachable(bb->getTerminator());
		TOUNREACHABLE.push_back(bb);
	}
	std::set<llvm::BasicBlock*> S;
	for (auto* x : predecessors(bb))
		S.insert(x);

	if (visitedTotal.count(bb))
	if (S.size() != visitedEdgesTotal[bb].size())
		llvm::errs() << bb->getName() << "\t" << visitedEdgesTotal[bb].size() << "\t" << S.size() << "\n";
	
	for (auto* x : predecessors(bb))
		if (visitedEdgesTotal[bb].count(x) == 0)
		{
			removeEdgeBetweenBlocks(x, bb);
		}
}

for (auto* X : TOUNREACHABLE)
{
	changeToUnreachable(&*X->begin());
}
llvm::errs() << X.size() <<"\t" << visitedTotal.size() << "\n";

currentEE->removeModule((BB)->getParent()->getParent());
return curr;
}

template <PartialInterpreter::VisitingPolicy policy>
llvm::BasicBlock* PartialInterpreter::visitBasicBlock(llvm::BasicBlock* BB, llvm::BasicBlock* from)
{
	assert(BB);
	using namespace llvm;
//	llvm::errs() << BB->getName() << "------------------\n";

	//	LocalStateMAP stateMap(state.begin(), state.end());

	ExecutionContext& executionContext = getLastStack();
	//	executionContext.CurFunction = BB->getParent();	
	executionContext.CurBB = BB;
	executionContext.CurInst = BB->begin();


//	executionContext.Caller = nullptr;

	while (getLastStack().CurInst != BB->end())
	{
		ExecutionContext& exe = getLastStack();
		Instruction* curr = &*getLastStack().CurInst;
		//llvm::errs() << *executionContext.CurInst << "\n";
		visitOuter<policy>(*getLastStack().CurInst++);

		//		if (curr == &*getLastStack().CurInst)
//			 exe.CurInst++;
	}

	Instruction* Term = BB->getTerminator();
	BasicBlock* next = nullptr;
	if (BranchInst* BI = dyn_cast<BranchInst>(Term))
	{
		if (BI->isConditional())
		{
			if (isValueComputed(BI->getCondition()))
			{
				GenericValue V = getOperandValue(BI->getCondition(), executionContext);
				if (V.IntVal == 0u)
					next = BI->getSuccessor(1);
				else
					next = BI->getSuccessor(0);
			}
	/*		else if (BI->getSuccessor(0)->getName() == "error.i")
				next = BI->getSuccessor(1);
			else if (BI->getSuccessor(1)->getName() == "_vfprintf_r.exit")
				next = BI->getSuccessor(0);
			else
			{
				BasicBlock* A = BI->getSuccessor(0);
				BasicBlock* B = BI->getSuccessor(1);

				if (BranchInst* bi= dyn_cast<BranchInst>(B->getTerminator()))
					if (!bi->isConditional())
						if (bi->getSuccessor(0) == A)
							next = A;
			}*/
		}
		else
			next = BI->getSuccessor(0);
	}
	else if (SwitchInst *SI = dyn_cast<SwitchInst>(Term))
	{
		auto c = SI->findCaseValue(ConstantInt::get(SI->getFunction()->getParent()->getContext(), getOperandValue(SI->getCondition(), executionContext).IntVal ));
		next = c->getCaseSuccessor();
	}

//	llvm::errs() << *Term << "\n";
		
	//TODO: RAII to pop
	popSingleStack();
	return next;
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

void PartialExecuter::findNextVisited(llvm::Function& F, bool status)
{
	std::unordered_map<const BasicBlock*, int> MAP = groupBasicBlocks(F);

	if (!status)
		for (llvm::BasicBlock& bb : F)
		{
			MAP[&bb] = 0;
		}

	std::set<llvm::BasicBlock*> all;
	for (llvm::BasicBlock& bb : F)
		all.insert(&bb);

	all.insert(nullptr);
	
	std::map<llvm::BasicBlock*, std::set<llvm::BasicBlock*> > map;
	for (llvm::BasicBlock& bb : F)
		map[&bb] = all;

	bool changed = true;
	while (changed)
	{
		changed = false;
		for (llvm::BasicBlock& bb : F)
		{
			std::set <llvm::BasicBlock*> intersection;
			int counter = 0;
			for (llvm::BasicBlock* succ : successors(&bb))
			{
				if (MAP[succ] != MAP[&bb])
				{
					bool HAS = intersection.count(nullptr);
					intersection.clear();
					if (counter++ || HAS)
						intersection.insert(nullptr);
					continue;
				}
				if (counter++ == 0)
					intersection = map[succ];
				else
				{
					std::set < llvm::BasicBlock*> temp;
					set_intersection(intersection.begin(), intersection.end(), map[succ].begin(), map[succ].end(), std::inserter(temp, temp.begin()));
					intersection = temp;
				}
			}
			intersection.insert(&bb);
			if (intersection != map[&bb])
			{
				map[&bb] = intersection;
				changed = true;
			}
		}
	}

	if (status)
		implementationSCC = map;
	else
		implementationGLOBAL = map;

	std::map<llvm::BasicBlock*, llvm::BasicBlock*> Z;

	for (llvm::BasicBlock& bb : F)
	{
		Z[&bb] = nullptr;
		int size = map[&bb].size();

		for (auto* b : map[&bb])
		{
//			if (MAP[b] != MAP[&bb])
//				continue;
			if (map[b].size() + 1 == size)
				Z[&bb] = b;
		}
	}




if (false)	for (llvm::BasicBlock& bb : F)
	{
		llvm::errs() << bb.getName() << "\t" << map[&bb].size() << "\t" << (Z[&bb] ? Z[&bb]->getName() : "nop" )<< "\n";
	}
}

llvm::BasicBlock* PartialExecuter::getOnlyOne(llvm::BasicBlock* bb)
{
	std::unordered_map<const BasicBlock*, int> MAP = groupBasicBlocks(*bb->getParent());

	llvm::BasicBlock* ret = nullptr;
	int count = 0;

	for (BasicBlock* b : successors(bb))
	{
		if (MAP[b] != MAP[bb])
			continue;
		count++;
		if (count == 1)
			ret = b;
		if (ret != b)
			return nullptr;
	}
	if (ret == bb)
		return nullptr;
	return ret;
}

llvm::BasicBlock* PartialExecuter::getImmediateDom(llvm::BasicBlock* bb, bool status)
{
	//TODO: where to go  
	if (bb->getName() == "do.end.i")
	{
		for (auto& b : *bb->getParent())
			if (b.getName() == "if.end38.i")
				return &b;
	}
	if (bb->getName() == "sw.epilog.i.i")
	{
		for (auto& b : *bb->getParent())
			if (b.getName() == "_printf_i.exit.i")
				return &b;
	}
//if (false)
	if (bb->getName() == "if.else188.i")
	{
		for (auto& b : *bb->getParent())
			if (b.getName() == "__fpclassifyd.exit.thread.i.i")
				return &b;
	}
/*	if (bb->getName() == "do.end.i" || bb->getName() == "land.lhs.true23.i")
	{
		for (auto& b : *bb->getParent())
			if (b.getName() == "if.end38.i")
				return &b;
	}
	if (bb->getName() == "if.then47.i" || bb->getName() == "land.lhs.true23.i")
	{
		for (auto& b : *bb->getParent())
			if (b.getName() == "if.end52.i")
				return &b;
	}*/
	auto implementation = implementationGLOBAL;
	if (status)
		implementation = implementationSCC;

	{
		int size = implementation[bb].size();

		for (auto* b :implementation[bb])
		{
			if (implementation[b].size() + 1 == size)
				return  b;
		}
	}
	return nullptr;
}


class FunctionData
{
	std::string error; 
	std::unique_ptr<Allocator> allocator;
	llvm::Function& F;
	std::map<llvm::BasicBlock*, int> visitCounter;
	std::map<llvm::BasicBlock*, int> lowestOutgoing;
	std::map<llvm::BasicBlock*, std::set<llvm::BasicBlock*>> visitedEdges;
public:
	PartialInterpreter* currentEE{nullptr};
	FunctionData(llvm::Function& F)
		: F(F)
	{
	}
	llvm::Function* getFunction()
	{
		return &F;
	}
	void registerEdge(llvm::BasicBlock* from, llvm::BasicBlock* to)
	{
		visitedEdges[to].insert(from);
	}
	void visitCallBase(const llvm::CallBase& callBase)
	{
		assert(currentEE == nullptr);
	//	llvm::errs() << callBase << "\n";
		std::unique_ptr<Module> uniqM(F.getParent()); 
		currentEE = (PartialInterpreter*)(PartialInterpreter::create(std::move(uniqM), &error));
		
	//	currentEE->clearFunction(F);
		
		allocator = std::make_unique<Allocator>(*currentEE->ValueAddresses);

		std::set<const llvm::Value*> SET;

		ExecutionContext& executionContext = currentEE->getSingleStack();
		executionContext.CurFunction = &F;	
		executionContext.Caller = const_cast<llvm::CallBase*>(&callBase);

		int i=0;
	//	llvm::errs() << callBase << "\n";
		for (auto& op : callBase.args())
		{
			if (i >= F.getFunctionType()->getNumParams())
			{
				//TODO: add this check
	//			if (!currentEE->isValueComputed(op))
	//				llvm::errs() << "AAAA\n";
//					break;
	  //				executionContext.VarArgs.push_back(currentEE->getConstantValue((Constant*)(&*op)));
			}
			else if (currentEE->isValueComputed(op))
			{
//				llvm::errs() << F << "\n";
//				llvm::errs() << callBase << "\n";
//				llvm::errs() << *op << "\tcccc\n";
//				llvm::errs() << i << "\t-------\n";

				//				state.push_back({BB->getParent()->getArg(i), currentEE->getConstantValue((Constant*)(&*op))});//;GenericValue((llvm::Value*)op)});
				executionContext.Values[const_cast<llvm::Argument*>(F.getArg(i))] = currentEE->getConstantValue((Constant*)(&*op));//;GenericValue((llvm::Value*)op)});
				currentEE->computed.insert(F.getArg(i));
		
			}
			i++;
		}
  // Handle varargs arguments...

		
//llvm::errs() << "DONE\n";
	}
	void doneVisitCallBase()
	{
//		delete currentEE;
		currentEE = nullptr;
	}
	void cleanupBB()
	{
		int BBs =0;
		std::set<std::pair<llvm::BasicBlock*, llvm::BasicBlock*>> Edges;
		for (auto& x : F)
		{
			BBs++;
			for (auto* p : predecessors(&x))
				Edges.insert({p, &x});
		}

		for (auto& p : Edges)
		{
			if (visitedEdges[p.second].count(p.first) == 0)
				removeEdgeBetweenBlocks(p.first, p.second);
		}

	}
	void emitStats()
	{
		int BBs =0;
		std::set<std::pair<llvm::BasicBlock*, llvm::BasicBlock*>> Edges;
		for (auto& x : F)
		{
			BBs++;
			for (auto* p : predecessors(&x))
				Edges.insert({&x, p});
		}


		//Also start has to signed
		int X = 0;
		for (auto& x : visitedEdges)
		{
			X += x.second.size();
		}
		if ((visitedEdges.size() + 1 != BBs) || (Edges.size() != X))
		{
		llvm::errs() << "Total edges \t\t" << X << "\t" << Edges.size() <<"\n";
		llvm::errs() << "Reachable BB\t\t" << visitedEdges.size() + 1 << "\t" << BBs << "\n";
			llvm::errs() << "WORKING\t\t" << F.getName()<< "\n";
		}
	}
	bool hasModifications()
	{
		int BBs =0;
		std::set<std::pair<llvm::BasicBlock*, llvm::BasicBlock*>> Edges;
		for (auto& x : F)
		{
			BBs++;
			for (auto* p : predecessors(&x))
				Edges.insert({&x, p});
		}


		//Also start has to signed
		int X = 0;
		for (auto& x : visitedEdges)
		{
			X += x.second.size();
		}
		if ((visitedEdges.size() + 1 != BBs) || (Edges.size() != X))
		{
			return true;
		llvm::errs() << "Total edges \t\t" << X << "\t" << Edges.size() <<"\n";
		llvm::errs() << "Reachable BB\t\t" << visitedEdges.size() + 1 << "\t" << BBs << "\n";
			llvm::errs() << "WORKING\n";
		}
		return false;
	}
	void addOutgoing(llvm::BasicBlock* BB)
	{
		if (lowestOutgoing.count(BB))
			lowestOutgoing[BB] = std::min(lowestOutgoing[BB], visitCounter[BB]);
		else
			lowestOutgoing[BB] = visitCounter[BB];
	}
	bool checkOutgoing(llvm::BasicBlock* BB)
	{
		if (lowestOutgoing.count(BB) == 0)
			return true;
		llvm::errs() << lowestOutgoing[BB] << "\t" << visitCounter[BB] << "\n";
		return lowestOutgoing[BB] == visitCounter[BB];
	}
	bool incrementAndCheckVisitCounter(llvm::BasicBlock* BB)
	{
		assert(BB);
		int currentCounter = visitCounter[BB]++;

		return (currentCounter < 100);
	}
};

}//cheerp
namespace llvm{


class SubGraph;

struct GraphNode {
	BasicBlock* BB;
	SmallVector<BasicBlock*, 2> Succs;
	SubGraph& Graph;
	explicit GraphNode(BasicBlock* BB, SubGraph& Graph);
};

class SubGraph {
public:
	typedef cheerp::DeterministicUnorderedSet<BasicBlock *, cheerp::RestrictionsLifted::NoErasure> DeterministicBBSet;
	typedef DeterministicBBSet BlockSet;
	typedef std::unordered_map<BasicBlock*, GraphNode> NodeMap;
	explicit SubGraph(llvm::BasicBlock* Entry, BlockSet Blocks):
		Entry(Entry), Blocks(std::move(Blocks))
	{
	}
	BasicBlock* getEntry() const
	{
		return Entry;
	}
private:
	GraphNode* getOrCreate(BasicBlock* BB)
	{
		auto it = Nodes.find(BB);
		if (it == Nodes.end())
		{
			it = Nodes.emplace(BB, GraphNode(BB, *this)).first;
		}
		return &it->second;
	}
	friend struct GraphTraits<SubGraph*>;
	friend struct GraphNode;

	BasicBlock* Entry;
	BlockSet Blocks;
	NodeMap Nodes;
};

GraphNode::GraphNode(BasicBlock* BB, SubGraph& Graph): BB(BB), Graph(Graph)
{
        for (auto Succ: successors(BB))
        {
                // Skip edges that go outside of the SubGraph
                if (!Graph.Blocks.count(Succ))
                        continue;
		if (Succ == Graph.getEntry())
			continue;
                Succs.push_back(Succ);
        }
}

template <> struct GraphTraits<SubGraph*> {
        typedef GraphNode NodeType;
        typedef NodeType* NodeRef;
        typedef mapped_iterator<SmallVectorImpl<BasicBlock*>::iterator, std::function<GraphNode*(BasicBlock*)>> ChildIteratorType;

        static NodeType *getEntryNode(SubGraph* G) { return G->getOrCreate(G->Entry); }
        static inline ChildIteratorType child_begin(NodeType *N) {
                return ChildIteratorType(N->Succs.begin(), [N](BasicBlock* BB){ return N->Graph.getOrCreate(BB);});
        }
        static inline ChildIteratorType child_end(NodeType *N) {
                return ChildIteratorType(N->Succs.end(), [](BasicBlock* BB){ llvm_unreachable("dereferencing past-the-end iterator");return nullptr;});
        }
};
}//namespace llvm
namespace cheerp{
class BasicBlockGroupData
{
	FunctionData& data;
	std::map<llvm::BasicBlock*, std::set<llvm::BasicBlock*> > incomings;
	std::set<llvm::BasicBlock*> inner;
	bool multiHead {false};
	llvm::BasicBlock* start;
	llvm::BasicBlock* from;		//TODO: from can become a set, conserving the phi that are equals
	BasicBlockGroupData* parent;
	//TODO: keep also track of the oldest parent with a bigger inner? or somehow keep a map to jump directly to the right parent
	std::deque<BasicBlockGroupData> subGroups;
	std::map<llvm::BasicBlock*, int> subGroupsID;
	bool visitingAll{false};
	const int iteration{0};
	static std::set<llvm::BasicBlock*> getAllBasicBlocks(llvm::Function& F)
	{
		std::set<llvm::BasicBlock*> ret;
		for (llvm::BasicBlock& bb : F)
		{
			ret.insert(&bb);
		}
		return ret;
	}
	void splitIntoSCCs(std::deque<BasicBlockGroupData>& blockQueue, std::map<llvm::BasicBlock*, int>& blockToIndexMap);
public:
	BasicBlockGroupData(FunctionData& data, BasicBlockGroupData* parentBBGD, const std::set<llvm::BasicBlock*>& inner, llvm::BasicBlock* start = nullptr)
		: data(data), inner(inner), start(start), from(nullptr), parent(parentBBGD)
	{
		if (start)
			assert(start->getParent() == data.getFunction());
	}
	BasicBlockGroupData(FunctionData& data)
		: BasicBlockGroupData(data, /*parentBBGD*/nullptr, getAllBasicBlocks(*data.getFunction()), &data.getFunction()->getEntryBlock())
	{
	}
	BasicBlockGroupData(BasicBlockGroupData& BBGData)
		: data(BBGData.data), inner(BBGData.inner), start(nullptr), from(nullptr), parent(&BBGData), iteration{BBGData.iteration +1}
	{
	}
	void addIncomingEdge(llvm::BasicBlock* comingFrom, llvm::BasicBlock* target)
	{
		/*
		//if (start)
		//llvm::errs() << "FIRST\t" << start->getName() << "\n";
		//else
		//	llvm::errs() << "NONE\n";
		llvm::errs() << "PIPPO:   ";
		llvm::errs() << target->getName() << "\t" << comingFrom->getName() << "\n";
		llvm::errs() << "MEMBERS:  ";
		for (auto* g : inner)
			llvm::errs() << g->getName() << ", ";
		llvm::errs() << "\n";
*/

		assert(inner.count(target));
		if (start == nullptr)
		{
			start = target;
			from = comingFrom;
		}
		if (start != target)
		{
			multiHead = true;
			from = nullptr;
		}
		if (comingFrom != from)
		{
			from = nullptr;
		}
	}
	std::set<llvm::BasicBlock*> actualVisit(llvm::BasicBlock& BB)
	{

		data.currentEE->setIncomingBB(from);
		BasicBlock* ret = data.currentEE->visitBasicBlock<PartialInterpreter::VisitingPolicy::NORMAL>(&BB, from);

		std::set <llvm::BasicBlock*> bbs;

		if (ret)
		{
			bbs.insert(ret);
		}
		else
		{
			for (auto* bb : successors(&BB))
				bbs.insert(bb);
		}
		return bbs;
	
		//Do the visit of the BB, with comingFrom (possibly nullptr) as predecessor
		//Loop backs will be directed to another BBgroup
		//The visit will return the set of reachable BBs -> add them to the subGroups data accordingly
	}
	void notifySuccessor(llvm::BasicBlock* from, llvm::BasicBlock* succ, const int iter)
	{
		if (visitingAll)
		{
			if (inner.count(succ) == 0)
			{
				//llvm::errs() << "AAAA\t" << from->getName() << "\t" << succ->getName() << "\n";
				parent->notifySuccessor(from, succ, iter);
			}
			return;
		}
		if (subGroupsID.count(succ) == 0)
		{
			assert(parent);
			//llvm::errs() << "go to parent!\n";

			data.addOutgoing(start);
			parent->notifySuccessor(from, succ, iter);
		}
		else
		{
			int X = -subGroupsID.at(succ);
			//llvm::errs() << X << "\t" << subGroups.size() << "\n";
			assert( X < (int)subGroups.size());
			subGroups.at(X).addIncomingEdge(from, succ);
		}
	}
	void visitAll()
	{
		visitingAll = true;
		for (llvm::BasicBlock* bb : inner)
		{
			data.currentEE->setIncomingBB(nullptr);
			data.currentEE->visitBasicBlock<PartialInterpreter::VisitingPolicy::REMOVE_VALUES>(bb, nullptr);
			for (llvm::BasicBlock* succ : successors(bb))
				registerEdge(bb, succ);
		}
	}
	void registerEdge(llvm::BasicBlock* from, llvm::BasicBlock* to)
	{
		//llvm::errs() << from->getName() << "\t" << to->getName() << "\n";
		data.registerEdge(from, to);
		//llvm::errs() << "then notify\n";
		notifySuccessor(from, to, iteration);
	}
	void recursiveVisit()
	{
		//llvm::errs() << inner.size() << "\n";
		if (multiHead)
		{
	//		llvm::errs() << "FINALLY A MULTIHEAD\n remove assertion & text\n";
	//		assert(false);
			//Mark everything as reachable
			visitAll();
			return;
		}
		if (!start)
		{
			//Not reachable in any way, nothing to do
			return;
		}
		if (data.incrementAndCheckVisitCounter(start) == false)
		{
			//TODO: reset Values in certain cases!!!!!!!!!!!
		//	llvm::errs() << start->getName() << "\n";
		//	llvm::errs() << "OH NOOOOOOOOOO COUNTERED\n\n";
			visitAll();
			//Mark everything as reachable
			return;
		}
		splitIntoSCCs(subGroups, subGroupsID);	//These should be partially ordered with the last one possibly being the replica of the current one

		//Do the actual visit for start
		std::set<llvm::BasicBlock*> possibleDestinations = actualVisit(*start);
	
		for (llvm::BasicBlock* succ : possibleDestinations)
			registerEdge(start, succ);

		//Fist has been already done
		subGroups.pop_back();
		while (!subGroups.empty())
		{
			subGroups.back().recursiveVisit();
			subGroups.pop_back();
		}

//TODO		if (!data.checkOutgoing(start))
//			visitAll();
		
		/*
		llvm::errs() << start->getName() << "\t\t";
		llvm::errs() << lowerOut << "\t" << higherOut << "\n";
		if (impossibleBounds())
			visitAll();
*/	}
};

void BasicBlockGroupData::splitIntoSCCs(std::deque<BasicBlockGroupData>& blockQueue, std::map<llvm::BasicBlock*, int>& blockToIndexMap)
{
	assert(blockQueue.empty());
	assert(blockToIndexMap.empty());
	//We begin with N nodes, remove 'start', and we find the SCCs of the remaining N-1 nodes.
	//
	//For N = 1, it means 0 nodes remaining -> no SCCs
	//For N > 1, it means > 0 nodes remaining, we divide them in 1 or more SCCs
	//
	//Then iff there are any edges going back to start, we add all nodes again as a single SCC to the end
	//
	//During the actual visit we might discover that we eventually will not loop back to start (so the recursion terminate) or we stop since we reached the maximum iteration number

	SubGraph::BlockSet Group;
	for (llvm::BasicBlock* bb : inner)
	{
		Group.insert(bb);
	}
	SubGraph SG(start, std::move(Group));
	blockQueue.emplace_back(*this);

	int decreasing_index = -1;
	for (auto& SCC: make_range(scc_begin(&SG), scc_end(&SG)))
	{
		std::set<llvm::BasicBlock*> subset;
//		llvm::errs() << decreasing_index << "  :\n";
		for (auto& GN : SCC)
		{
//		llvm::errs() << "  -> " << GN->BB->getName() << "\n";
			BasicBlock* bb = GN->BB;
			subset.insert(bb);
			blockToIndexMap[bb] = decreasing_index;
		}
//		if (decreasing_index != -1)
			blockQueue.emplace_back(data, this, subset);
		decreasing_index--;
	}
	blockToIndexMap[start] = 0;	
}

bool PartialExecuter::runOnModule( llvm::Module & module )
{
	using namespace llvm;
	bool changed = false;

	for (Function& F : module)
	{
		changed |= runOnFunction(F);

		verifyFunction(F);
		for (BasicBlock& BB : F)
		{
			std::set<llvm::BasicBlock*> pred;
			for (BasicBlock* bb : predecessors(&BB))
				pred.insert(bb);

			for (PHINode& phi : BB.phis())
			{
				for (BasicBlock* bb : pred)
					assert(phi.getBasicBlockIndex(bb) != -1);
			}
		}
	}

	return changed;
}

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
//		if (F.hasAddressTaken())
//			return false;


		if (F.isDeclaration())
			return false;

//		llvm::errs() << "......\t" << F.getName() << "\n";
		FunctionData data(F);

		bool hasIndirectUse = false;

		std::vector<const CallBase*> callBases;

		int X = 0;
		for (const Use &U : F.uses())
                {
                        const User *FU = U.getUser();
                        if (!isa<CallInst>(FU) && !isa<InvokeInst>(FU))
                        {
                                hasIndirectUse = true;
                                continue;
                        }
                        const CallBase* CS = cast<CallBase>(FU);
                        if (CS->isCallee(&U))
                        {
				X++;
				callBases.push_back(CS);
//				llvm::errs() << X << "\n" << *CS << "\n";
					data.visitCallBase(*CS);	
					
					{
						BasicBlockGroupData groupData(data);
					groupData.recursiveVisit();
					}
					data.doneVisitCallBase();	
                        }
                        else
                        {
                                hasIndirectUse = true;
                        }
                }
		

		if (!hasIndirectUse && F.getLinkage() != GlobalValue::ExternalLinkage)
		{
			if (data.hasModifications())
			{
			llvm::errs() << F << "\n";
			for (const CallBase* CS : callBases)
				llvm::errs() << *CS << "\n";
			data.emitStats();
			data.cleanupBB();
		llvm::errs() << "\n";
			return true;
			}
		}

		llvm::errs() << "\n";

return false;









		llvm::errs() << "\n\n\t" << F.getName() << "\n";
	findNextVisited(F, true);
	findNextVisited(F, false);

/*
	std::map<int, std::vector<const BasicBlock*>> INVERSE;
	for (auto& x : MAP)
	{
		INVERSE[x.second].push_back(x.first);
	}

if(false)	for (auto& x : INVERSE)
	{
		llvm::errs() << x.first << ":\t";
			llvm::errs() << x.second.size() << "\t" << x.second.front()->getName() << "\n";
	}

*/	{

		llvm::BasicBlock* BB = nullptr;
		llvm::BasicBlock* from = nullptr;
		for (BasicBlock& bb : F)
		{
			if (bb.getName() == "if.end38.i")
				from = &bb;
			if (bb.getName() == "for.cond.i")
				BB = &bb;
		}
		LocalState state;
		CICCIO = nullptr;
		int COUNT=0;
		llvm::errs() << "USERS\n";
		for (auto* X : F.users())
		{
			if (CallInst* CI = dyn_cast<CallInst>(X))
			{
						COUNT++;
				llvm::errs() << *X << "\n";
				int i=0;
						CICCIO = CI;	
			}
		}
	if (COUNT > 1)
		CICCIO = nullptr;
	if (F.getName() == "__rem_pio2_large")
		return false;
	if (!CICCIO)
		return false;
//	llvm::errs() << "\n\n............\t\t";
//	llvm::errs() << F.getName() << "\n";
/*if(false)		for (auto& p : state)
		{
			llvm::errs() << *p.first << "\n\t->\t";
//			if (isa<Value*>(GVTOP(p.second)))
//				llvm::errs() << *(llvm::Value*)GVTOP(p.second);
//			else
				llvm::errs() << p.second.UIntPairVal.first << "," << p.second.UIntPairVal.second << "..." << p.second.IntVal;
			llvm::errs() << "\n\n";
		}
*/
	//	BasicBlock* next = visitBasicBlock2(state, BB, from);
		BasicBlock* next = visitBasicBlock2(*this, state, &F.getEntryBlock());
		//while (BB){
		//from = BB;
		//BB = next;
		//}
/*
 * if (true)	for (auto& p : state)
		{
			llvm::errs() << *p.first << "\n\t->\t";
//			if (isa<Value>(GVTOP(p.second)))

//				llvm::errs() << *(llvm::Value*)GVTOP(p.second);
//			else
				llvm::errs() << p.second.UIntPairVal.first << "," << p.second.UIntPairVal.second << "..." << p.second.IntVal;
			llvm::errs() << "\n\n";
		}
*/
		llvm::errs() << "\n----\n";
		for (auto &p:state)
		{
		//	llvm::errs() << *p.first << "\t\t" << *(llvm::Value*)GVTOP(p.second) << "\n";
		}
	}
	return true;
}


}

using namespace cheerp;

INITIALIZE_PASS_BEGIN(PartialExecuter, "PartialExecuter", "Partially execute functions",
                      false, false)
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(RegionInfoPass)
INITIALIZE_PASS_END(PartialExecuter, "PartialExecuter", "Partially execute functions",
                    false, false)
