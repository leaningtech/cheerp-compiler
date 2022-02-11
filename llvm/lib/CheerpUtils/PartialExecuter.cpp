//===-- PartialExecuter.cpp - Remove unused functions/globals -----------=CB//                     Cheerp: The C++ compiler for the Web
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

#include "llvm/Transforms/IPO/Attributor.h"

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

namespace cheerp {

using namespace std;

	ModuleData* MODULE_DATA;

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
}

static bool isValueComputedConstant(const llvm::Value* V)
{
	if (isa<Constant>(V))
	{
		if (const GlobalVariable* GVar = dyn_cast<GlobalVariable>(V))
		{
			if (!GVar->hasInitializer() || GVar->isExternallyInitialized())
				return false;
			if (!GVar->hasInternalLinkage())
				return false;
		//llvm::errs() << "globalVar" << "\n";
		}
		//TODO: actually in opt we got some problems since we might have to find, given a ConstantExpression, wether it's internal or external
		if (const ConstantExpr* CE = dyn_cast<ConstantExpr>(V))
		{
			for (auto& op : CE->operands())
			{
				if (!isValueComputedConstant(op))
					return false;
			}
//		llvm::errs() << *V << "\n";
//			llvm::errs() << "is CE\n";
		}
		return true;
	}
	return false;
}

typedef std::pair<const llvm::Value*, GenericValue> ValueGenericValuePair;
typedef std::vector<ValueGenericValuePair> LocalState;
typedef std::unordered_map<const llvm::Value*, GenericValue> LocalStateMAP;



class PartialInterpreter : public llvm::Interpreter {
	const llvm::BasicBlock* fromBB{nullptr};	
	enum BitMask : uint32_t
	{
		NONE = 0,
		ALIGNED2 = 0x00000001,
		ALIGNED4 = 0x00000003,
		ALIGNED8 = 0x00000007,
		ALL = 0xffffffff,
	};
	std::vector<std::pair<const llvm::Value*, std::pair<GenericValue, BitMask> > > incomings;
public:
	static void DO_STUFF(Function* pippo, CallBase* CB, CallBase* parent, PartialInterpreter& exe);
	std::unordered_map<const llvm::Value*, BitMask> stronglyKnownBits;
	BitMask computeStronglyKnownBits(const llvm::Instruction& I)
	{
		BitMask min = BitMask::ALL;

		for (auto& op : I.operands())
		{
			min = BitMask(min & getBitMask(op));
		}

		//TODO: for phis just copy them
		//TODO: also SelectInst can be specialcased
		if (min == BitMask::ALL)
		{
			//All operands fully known
			return BitMask::ALL;
		}

		switch (I.getOpcode())
		{
			case Instruction::Load:
			{
				//TODO: actually any check is needed??	YES
				return BitMask::ALL;
			}
			case Instruction::Add:
			case Instruction::Sub:
			case Instruction::Mul:
			case Instruction::PtrToInt:
			case Instruction::BitCast:
			case Instruction::GetElementPtr:
			{
				//Those all work modulo N
				return min;
			}
			case Instruction::And:
			{
				//If either is Constant && less than equal min -> ALL
				if (const ConstantInt* CI = dyn_cast<ConstantInt>(I.getOperand(0)))
				{
					if (CI->getZExtValue() <= min)
						return BitMask::ALL;
				}
				if (const ConstantInt* CI = dyn_cast<ConstantInt>(I.getOperand(1)))
				{
					if (CI->getZExtValue() <= min)
						return BitMask::ALL;
				}

				[[clang::fallthrough]];
			}
			case Instruction::Or:
			case Instruction::Xor:
			{
				//Those all work modulo 2^k
				return min;
			}
			default:
			{
				//Some operands NOT fully known, means in general no information is conserved
				return BitMask::NONE;
			}
		}
	}
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
	BitMask getBitMask(const llvm::Value* V) const
	{
		if (isa<ConstantData>(V))
			return BitMask::ALL;

		if (isa<Function>(V))
			return BitMask::NONE;

		if (GlobalVariable* GV = const_cast<GlobalVariable*>( dyn_cast<GlobalVariable>(V)))
		{
			(GV)->setAlignment(Align(8));
			if (getDataLayout().getPreferredAlign(GV) == 8)
				return BitMask::ALIGNED8;
			if (getDataLayout().getPreferredAlign(GV) == 4)
				return BitMask::ALIGNED4;
			if (getDataLayout().getPreferredAlign(GV) == 2)
				return BitMask::ALIGNED2;

		//	llvm::errs() << *GV << "\n";
		}
		if (const ConstantExpr* CE = dyn_cast<ConstantExpr>(V))
		{
			if (CE->getOpcode() == Instruction::GetElementPtr)
			{
				const GEPOperator *GEP = cast<GEPOperator>(CE);
				if (GEP->hasAllZeroIndices())
					return getBitMask(CE->getOperand(0));
			}
		}

		if (stronglyKnownBits.count(V))
			return stronglyKnownBits.at(V);

		return BitMask::NONE;
	}
	bool isValueComputed(const llvm::Value* V) const
	{
		if (isValueComputedConstant(V))
			return true;
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
				{
					if (isValueComputed(I.getOperand(2)))
						return true;
				}
				else
				{
					if (isValueComputed(I.getOperand(1)))
						return true;
				}

			}	
		}
		for (auto& op : I.operands())
		{
			if (!isValueComputed(op))
				return false;
		}
		if (const SwitchInst* SI = dyn_cast <SwitchInst>(&I))
		{
			if ((getBitMask(SI->getCondition()) != BitMask::ALL) )
				return false;
		}
		if (const BranchInst* SI = dyn_cast <BranchInst>(&I))
		{
			if (SI->isConditional())
			if ((getBitMask(SI->getCondition()) != BitMask::ALL) )
				return false;
		}
		return true;
	}
	bool hasToBeSkipped(llvm::Instruction& I) //TODO:const
	{
		if (isa<UnreachableInst>(I))
			return true;
		if (isa<LandingPadInst>(I)) //LandingPadInst are not handled by Interpreter
			return true;
//TODO??	if (isa<StoreInst>(I))
//			return true;

		if (isa<VAArgInst>(I))
			return true;
		if (!areOperandsComputed(I))
			return true;
			//ADD THAT RANDOM INSTRUCTIONS ARE NOT EXECUTABLE
		if (isa<CallBase>(I))
		{
//			if (isa<InvokeInst>(I)) //Invokes are not handled by Interpreter
//				return true;
		//	return true;
			CallBase& CB = cast<CallBase>(I);
			if (!CB.getCalledFunction())
				return true;
			{
		Function* FF = dyn_cast<Function>(CB.getCalledFunction());

	//	llvm::errs() << FF->getName() << "\n";
		if (FF->isDeclaration())
			return true;

//willReturn() will not actualy work since we should be fine to infinite loops (we are though?????)
//TODO!!!
/*
		llvm::errs() << "enters\n";
for (auto& BB : *FF)
	if (isa<UnreachableInst>(BB.getTerminator()))
		return true;
		llvm::errs() << "exites\n";

if (!FF->doesNotThrow())
	 return true;
*/
int i=0;
				for (auto& op : CB.args())
			{


					if (i >= FF->getFunctionType()->getNumParams())
						break;
				if (isValueComputed(op))
				{
					//TODO: fix Vaarg
				//	llvm::errs() << *op << "\taaaaa\n";
					//TODO: no recursion!!
					computed.insert(CB.getCalledFunction()->getArg(i));
					stronglyKnownBits[CB.getCalledFunction()->getArg(i)]= getBitMask(op);
			//		llvm::errs() << getBitMask(op) << "\t" << *op << "\n";
		//			llvm::errs() << *CB.getCalledFunction()->getArg(i) << "\t" << getBitMask(op) << "\n";

		}
		else
		{
		//	llvm::errs() << i << "-th argument not known\n";
		}
		i++;
		}

			}
		}
//		if (isa<PtrToIntInst>(I))
//			return true;	//PTR TO INT ARE not preexecutable
//TODO: also int to ptr ??
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
		 
std::vector<std::pair<long long, long long> > V;
for (auto& gv : I.getFunction()->getParent()->globals())
{
	GlobalVariable* GV = dyn_cast<GlobalVariable>(&gv);
	if (!GV)
		continue;
	if (!GV->hasInitializer())
		continue;
	if (GV->isExternallyInitialized())
		continue;
	if (!GV->hasName())
		continue;
  GenericValue SRC = getOperandValue(GV, getLastStack());
  GenericValue *Ptr = (GenericValue*)GVTORP(SRC);
//llvm ::errs() << GV->getName() << "\t" <<"\t" << (long long)Ptr<<"\t"<< GV->isConstant()<<"\n";
	//bail out if there are pointers 
//	llvm::errs() << *GV->getInitializer() << "\n";
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
		
		if (PHINode* phi = dyn_cast<PHINode>(&I))
		{
			const llvm::BasicBlock* from = getIncomingBB();
			if (from)
			{
				llvm::Value* incoming = phi->getIncomingValueForBlock(from);
				assert(incoming);
				if (isValueComputed(incoming))
					incomings.push_back({phi, {getOperandValue(incoming, getLastStack()), getBitMask(incoming)}});
			}
			return;
		}
		else
		{
			for (auto& p : incomings)
			{
		//		llvm::errs() <<"PHI-->\t"<< *p.first << "\t" ;
		//		p.second.print("");
				getLastStack().Values[const_cast<llvm::Value*>(p.first)] = p.second.first;
				computed.insert(p.first);
				stronglyKnownBits[const_cast<llvm::Value*>(p.first)] = p.second.second;
			}
			incomings.clear();
		}
///Here PHI have been properly processed

if (isa<CallBase>(&I) && sizeStack() == 1)
{
	CallBase* CB= dyn_cast<CallBase>(&I);
	llvm::Function* pippo = CB->getCalledFunction();

	if (pippo)
	{

		PartialInterpreter::DO_STUFF(pippo, CB, nullptr, *this);
	}
}


		const bool skip = hasToBeSkipped(I);
		const bool term = I.isTerminator();



	if (false)
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

			if (sizeStack() > 1)
			{
				popSingleStack();
				//CALL FAILED, delete caller!
				computed.erase(getLastStack().Caller);
			}	
			return; 
		}

		
		if (term)
		{

		if (sizeStack() > 1)
		{
	//llvm::errs() << I << "\t" << I.getParent()->getParent()->getName() << "\tciccio\n";
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
	else if (SwitchInst* BI = dyn_cast<SwitchInst>(&I))
	{
		{
			if (isValueComputed(BI->getCondition()))
			{
				GenericValue V = getOperandValue(BI->getCondition(), getLastStack());
				ConstantInt* CI = (ConstantInt*)ConstantInt::get((IntegerType*)(BI->getCondition()->getType()), V.IntVal);
				next = BI->findCaseValue(CI)->getCaseSuccessor();
			}
		}
	}
	else if (ReturnInst* RI = dyn_cast<ReturnInst>(&I))
	{
		if (RI->getReturnValue())
			stronglyKnownBits[getFirstStack().Caller] = getBitMask(RI->getReturnValue());
		visit(I);
return;
	}
	else if (UnreachableInst* UI = dyn_cast<UnreachableInst>(&I))
	{
//		visit(I);
	}
	if (next == nullptr)
		return;
	//assert(next);
	setIncomingBB(I.getParent());
	getLastStack().CurBB = next;
	getLastStack().CurInst = next->begin();

		}
			return;
		}

		computed.insert(&I);
		stronglyKnownBits[&I] = computeStronglyKnownBits(I);
		visit(I);


		//llvm::errs() << stronglyKnownBits[&I] << "\t" << I << "\n";
//	if (computed.count(&I))
//		llvm::errs() << I << "\n";
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

void removeEdgeBetweenBlocks(llvm::BasicBlock* from, llvm::BasicBlock* to)
{
	//cleanup phi
//TODO: consider using to->removePredecessor(from) (then change to unreachable can used again??)
	//change terminator
	
//	to->removePredecessor(from);
/*	for (PHINode& phi : to->phis())
	{
		while (phi.getBasicBlockIndex(from) != -1)
			to->removePredecessor(from);
		break;
	}*/

	int Z = 0;
	for (BasicBlock* bb : predecessors(to))
		if (bb == from)
			Z++;
	for (int i=0; i<Z; i++)
	{
        to->removePredecessor(from);
	}

{	llvm::Instruction* I = from->getTerminator();
	if (BranchInst* BI = dyn_cast<BranchInst>(I))
	{
		if (BI->isConditional())
		{
			assert(BI->getSuccessor(0) != BI->getSuccessor(1));
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
			BI->eraseFromParent();
			new UnreachableInst(from->getParent()->getParent()->getContext(), from);
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
		new UnreachableInst(from->getParent()->getParent()->getContext(), from);

    }
    else if (SI->getDefaultDest() == to)
    {
	    BasicBlock* other = nullptr;
    for (SwitchInst::CaseIt i = SI->case_end(), e = SI->case_begin(); i != e;) {
      --i;
      auto *Successor = i->getCaseSuccessor();
      other = Successor;
      SI->removeCase(i);
      break;
    }
    assert(other);
	    SI->setDefaultDest(other);
    }

	
	}
	else if (isa<InvokeInst>(I))
	{

		BasicBlock* BB = I->getParent();
		I->replaceAllUsesWith(UndefValue::get(I->getType()));
		I->eraseFromParent();
		new UnreachableInst(BB->getParent()->getParent()->getContext(), BB);
	}
	else if (isa<UnreachableInst>(I))
	{
	}
else
{
	llvm::errs() <<"unhandled...\t" << *I << "\n"; 
	//assert(false);
}
}
}
	
template <PartialInterpreter::VisitingPolicy policy>
llvm::BasicBlock* PartialInterpreter::visitBasicBlock(llvm::BasicBlock* BB, llvm::BasicBlock* from)
{
	assert(BB);
	using namespace llvm;
//	llvm::errs() << BB->getName() << "------------------\n";

	//	LocalStateMAP stateMap(state.begin(), state.end());

//	llvm::errs() << *BB << "\n";
	ExecutionContext& executionContext = getLastStack();
	//	executionContext.CurFunction = BB->getParent();	
	executionContext.CurBB = BB;
	executionContext.CurInst = BB->begin();
//	llvm::errs() << "ciccio\n";

//	executionContext.Caller = nullptr;

	while (getLastStack().CurInst != BB->end())
	{
		Instruction* zz = &*getLastStack().CurInst;
//		llvm::errs() << *zz << "\t" << zz->getFunction()->getName() << "\n";
if (isa<CallBase>(zz))
{
	CallBase* CB= dyn_cast<CallBase>(zz);
	llvm::Function* pippo = CB->getCalledFunction();

	if (pippo)
	{

		PartialInterpreter::DO_STUFF(pippo, CB, nullptr, *this);
	}
}
		visitOuter<policy>(*getLastStack().CurInst++);
	}

	Instruction* Term = BB->getTerminator();
	BasicBlock* next = nullptr;
	if (BranchInst* BI = dyn_cast<BranchInst>(Term))
	{
		if (BI->isConditional())
		{
			if (isValueComputed(BI->getCondition()) && (getBitMask(BI->getCondition()) == BitMask::ALL) )
			{
				GenericValue V = getOperandValue(BI->getCondition(), executionContext);
				if (V.IntVal == 0u)
					next = BI->getSuccessor(1);
				else
					next = BI->getSuccessor(0);
			}
		}
		else
			next = BI->getSuccessor(0);
	}
	else if (SwitchInst *SI = dyn_cast<SwitchInst>(Term))
	{
		if (isValueComputed(SI->getCondition()) && (getBitMask(SI->getCondition()) == BitMask::ALL) )
			{
		auto c = SI->findCaseValue(ConstantInt::get(SI->getFunction()->getParent()->getContext(), getOperandValue(SI->getCondition(), executionContext).IntVal ));
		next = c->getCaseSuccessor();
		}
	}

//	llvm::errs() << *Term << "\n";
		
	//TODO: RAII to pop
	return next;
}

LocalState intersection(const LocalState& lhs, const LocalState& rhs)
{
	//TODO: to implement
	return lhs;
}

class SCCVisitingData
{
	const std::vector<const llvm::BasicBlock*> BBs;
	std::unordered_set<const llvm::BasicBlock*> reacheableBBs;
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
	SCCVisitingData()
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

class FunctionData
{
	std::string error; 
	std::unique_ptr<Allocator> allocator;
	llvm::Function& F;
	public://TODO:remove public
	std::map<llvm::BasicBlock*, int> visitCounter;
	std::map<llvm::BasicBlock*, int> lowestOutgoing;
	std::map<llvm::BasicBlock*, std::set<llvm::BasicBlock*>> visitedEdges;
	ModuleData& moduleData;
public:
	std::deque<std::vector<const Value*> > callEquivalentQueue;
	uint32_t indexCallEquivalentQueue{0};
	PartialInterpreter* currentEE{nullptr};
	explicit FunctionData(llvm::Function& F, ModuleData& moduleData)
		: F(F), moduleData(moduleData)
	{
		MODULE_DATA = &moduleData;
	}
	llvm::Function* getFunction()
	{
		return &F;
	}
	void registerEdge(llvm::BasicBlock* from, llvm::BasicBlock* to)
	{
		visitedEdges[to].insert(from);
	}
	ExecutionContext& setUpPartialInterpreter()
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
		
		return executionContext;
	}
	std::vector<const llvm::Value*> getArguments(const llvm::CallBase* callBase, const llvm::CallBase* parent, PartialInterpreter* exe)
	{
		std::vector<const llvm::Value*> args(F.getFunctionType()->getNumParams(), nullptr);

		if (callBase)
		{
			for (uint32_t i=0; i<F.getFunctionType()->getNumParams(); i++)
				args[i] = callBase->getArgOperand(i);
			
			//Filter out not computed arguments
			for (auto& v : args)
			{
		/*		if (exe)
				{
				if (exe->isValueComputed(v) == false)
					v = nullptr;
				}
				else{
					if (isValueComputedConstant(v) == false)
						v = nullptr;
				}
*/



				if (isValueComputedConstant(v) == false)
				{
					if (isa<Argument>(v) && exe && exe->isValueComputed(v))
					{

					auto	     StoredAddr = (char*) exe->ValueAddresses->toReal(exe->getOperandValue((llvm::Value*)v, exe->getLastStack()).PointerVal);
        const GlobalValue* GV = exe->getGlobalValueAtAddress(StoredAddr);

	if (GV)
	{
	v = GV;
	}
	else
	{
		GenericValue value = exe->getOperandValue((llvm::Value*)v, exe->getLastStack());
//		value.print("uela\t");

		v=nullptr;
	}
					}
					else
						v = nullptr;
				}
			}
		}

		return args;
	}
	static bool areEquivalent(const std::vector<const llvm::Value*>& a, const std::vector<const llvm::Value*>& b)
	{
		assert(a.size() == b.size());

		const uint32_t numElements = a.size();
	
		for (uint32_t i=0; i<numElements; i++)
		{
			if (b[i] == nullptr)
				continue;

			if (a[i] != b[i])
				return false;
		}
		return true;
	}
	void enqueCallEquivalent(const std::vector<const llvm::Value*>& arguments)
	{
		//Insert the arguments in the map
		//////////////////llvm::errs() << "enque\t" << F.getName();
		if(false)
		{	for (uint32_t i=0; i<arguments.size(); i++)
			{
				const llvm::Value* x = arguments[i];
				if (x)
					llvm::errs() << "\t" << *x;
				else
					llvm::errs() << "\t" << "---";
			}
		}

		//Check wether we already visited something similar
		for (const auto& args : callEquivalentQueue)
		{
			if (areEquivalent(arguments, args))
			{
//		llvm::errs() << "\n";
				return;
			}
		}
//		llvm::errs() << "\t queueud";
//		llvm::errs() << "\n";
		callEquivalentQueue.push_back(arguments);
	}
	const std::vector<const llvm::Value*>* getSomethingToVisit()
	{
		if (indexCallEquivalentQueue >= callEquivalentQueue.size())
			return nullptr;
		else
			return &callEquivalentQueue[indexCallEquivalentQueue++];
	}	
	void visitCallEquivalent(const std::vector<const llvm::Value*>& arguments)
	{
		//Insert the arguments in the map
		if (false){llvm::errs() << "visit\t" << F.getName();
			for (uint32_t i=0; i<arguments.size(); i++)
			{
				const llvm::Value* x = arguments[i];
				if (x)
					llvm::errs() << "\t" << *x;
				else
					llvm::errs() << "\t" << "---";
			}
			llvm::errs() << "\n";
		}
		{
			ExecutionContext& executionContext = setUpPartialInterpreter();

			assert(arguments.size() == F.getFunctionType()->getNumParams());

			for (uint32_t i=0; i<arguments.size(); i++)
			{
				const llvm::Value* x = arguments[i];
				if (!x)
					continue;
				
				llvm::Argument* ith_arg = const_cast<llvm::Argument*>(F.getArg(i));
				executionContext.Values[ith_arg] = currentEE->getConstantValue((Constant*)x);
				currentEE->computed.insert(ith_arg);
				currentEE->stronglyKnownBits[ith_arg]= currentEE->getBitMask(x);
			}
		//llvm::errs() << "\n";
		//Do the visit
		actualVisit();
		
		//Cleanup
		doneVisitCallBase();
		}

	}
	void actualVisit();
	void visitCallBase(const llvm::CallBase* callBase)
	{
		const auto& equivalentArgs = getArguments(callBase, nullptr, nullptr);

		enqueCallEquivalent(equivalentArgs);
  		// Handle varargs arguments...
	}
	void visitNoInfo()
	{
		const auto& equivalentArgs = getArguments(nullptr, nullptr, nullptr);

		enqueCallEquivalent(equivalentArgs);
	}
	void doneVisitCallBase()
	{
		bool removed = currentEE->removeModule(F.getParent());
		(void)removed;
		assert(removed);
		delete currentEE;
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
		return;
			//llvm::errs() <<"\n" << F.getName() << "\n\n";
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
		//llvm::errs() << lowestOutgoing[BB] << "\t" << visitCounter[BB] << "\n";
		return lowestOutgoing[BB] == visitCounter[BB];
	}
	bool incrementAndCheckVisitCounter(llvm::BasicBlock* BB)
	{
		assert(BB);
		int currentCounter = visitCounter[BB]++;

		return (currentCounter < 100);
	}
};

class ModuleData{
public:
	std::map<const llvm::Function*, FunctionData> functionData;
        FunctionData& getFunctionData(const llvm::Function& F)
        {
                return functionData.at(&F);
        }
        void initFunctionData(llvm::Module& M)
        {
		assert(functionData.empty());
                for (Function& F : M)
                {
                        functionData.emplace(&F, FunctionData(F, *this));
                }
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
void PartialInterpreter::DO_STUFF(Function* pippo, CallBase* CB, CallBase* parent, PartialInterpreter& exe)
{
return ;
		auto& FunData = cheerp::MODULE_DATA->getFunctionData(*pippo);
		
		const auto& equivalentArgs = FunData.getArguments(CB, parent, &exe);	
		FunData.enqueCallEquivalent(equivalentArgs);
}

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
	int iteration{-1};
	std::vector<std::pair<llvm::BasicBlock*, int> > escaping;
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
		: data(BBGData.data), inner(BBGData.inner), start(nullptr), from(nullptr), parent(&BBGData)
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
	bool shouldCleanUp()
	{
		for (auto& p : escaping)
		{
			//llvm::errs() <<"escaping:\t" << p.first->getName() << "\t" << p.second <<"\n";
			if (data.visitCounter[p.first] != p.second)
				return true;
		}
		return false;
	}
	bool notifySuccessor(llvm::BasicBlock* from, llvm::BasicBlock* succ, const int iter)
	{
		if (visitingAll)//TODO: remove this special case
		{
			if (inner.count(succ) == 0)
			{
				assert(parent);
				//llvm::errs() << "AAAA\t" << from->getName() << "\t" << succ->getName() << "\n";
				parent->notifySuccessor(from, succ, iter);
			}
			return false;
		}
		if (subGroupsID.count(succ) == 0)
		{
			assert(parent);
			//llvm::errs() << "go to parent!\n";

			data.addOutgoing(start);
			const bool notified = parent->notifySuccessor(from, succ, iter);

			if (notified)
			{
				escaping.push_back({from, iter});
			}

			return false;
		}
		else
		{
			int X = -subGroupsID.at(succ);
			//llvm::errs() << X << "\t" << subGroups.size() << "\n";
			assert( X < (int)subGroups.size());
			subGroups.at(X).addIncomingEdge(from, succ);

			return true;
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
//			llvm::errs() << "no start\n";
			//Not reachable in any way, nothing to do
			return;
		}
		iteration = std::min(100, ++data.visitCounter[start]);

		if (iteration == 100)
		{

			data.visitCounter[start] = 200;
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

//TODO: fix??
		if (shouldCleanUp())
		{
			//llvm::errs() << "SHOULD CLEAN UP\n";
			//visitAll();
		}
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

void FunctionData::actualVisit()
{
	BasicBlockGroupData groupData(*this);
	groupData.recursiveVisit();
}

bool PartialExecuter::runOnModule( llvm::Module & module )
{
//	llvm::errs() << module << "\n";
	using namespace llvm;
	//TODO: classifyFunctions(module) ?? possibly to precompute where to recurse;
	
	ModuleData data;
	moduleData = &data;
	data.initFunctionData(module);

	bool changed2 = true;
	while (changed2)
	{
		changed2 = false;
		for (Function& F : module)
		{
			bool R = processFunction(F);
//			if (R)
//				llvm::errs() << "processed\t" << F.getName() << "\n";
			changed2 |= R;
		}
	}

	bool changed = false;
	for (Function& F : module)
	{
		//llvm::errs() << F.getName() << "\taaa\n";
		changed |= runOnFunction(F);

		verifyFunction(F);
		//llvm::errs() << F << "\n";
		for (BasicBlock& BB : F)
		{
			std::set<llvm::BasicBlock*> pred;
			for (BasicBlock* bb : predecessors(&BB))
				pred.insert(bb);

			for (PHINode& phi : BB.phis())
			{
//				llvm::errs() << BB << "\n";
				for (BasicBlock* bb : pred)
					assert(phi.getBasicBlockIndex(bb) != -1);
			}
		}
	}

	return changed;
}

bool PartialExecuter::processFunction(llvm::Function& F)
{
	using namespace llvm;
//For each SCC (process in order)
//	if single point of entry: start from there, otherwise (no point of entry -> all unreachable / multiple ones -> all reachable)
//	execute from single point of entry (+ state that's intersection of possible path to it)
//	if (unconditional or decidable) -> follow path, otherwise
//		if immediate dominator is in the SCC, jump there(a refinement would be immediate dominator in SCC while invalidating all otherwise reachable SCC) or bail out
//	bail out:
//		sign as reachable all out-going edges from the SCC (keeping only the state pre-SCC) + all BB in the SCC

	if (F.isDeclaration())
		return false;

	FunctionData& data = moduleData->getFunctionData(F);

	bool hasIndirectUse = false;

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
		
	//		llvm::errs() << *CS << "\n";	
			data.visitCallBase(CS);	
		}
		else
		{
			hasIndirectUse = true;
		}
	}
	
	
	if (!hasIndirectUse && F.getLinkage() != GlobalValue::ExternalLinkage)
		;
	else
	{
		data.visitNoInfo();	
	}

	bool changed = false;
	while (const auto* toBeVisited = data.getSomethingToVisit())
	{
		changed = true;
		data.visitCallEquivalent(*toBeVisited);
	}	
	return changed;
}

bool PartialExecuter::runOnFunction(llvm::Function& F)
{
	using namespace llvm;

	if (F.isDeclaration())
		return false;

	bool changed = false;

	FunctionData& data = moduleData->getFunctionData(F);
	if (data.hasModifications())
	{
			//TODO: Check no CE are in the globals we are loading from
			//TODO: collect data on used Globals -> force alignment on those
		data.emitStats();
		data.cleanupBB();
		changed = true;
	}

	if (!data.callEquivalentQueue.empty())
	{
		const uint32_t dim = data.callEquivalentQueue.front().size();

		for (int i=0; i<dim; i++)
		{
			const llvm::Value* C = nullptr;
			bool bailout = false;
			for (int j=0; j<data.callEquivalentQueue.size(); j++)
			{
				const llvm::Value* v = data.callEquivalentQueue[j][i];
				if (j==0)
					C = v;
				if (C != v)
				{
					bailout = true;
					break;
				}
			}
			if (!bailout && C)
			{
				if (C->getType() == F.getArg(i)->getType())
				{
				F.getArg(i)->replaceAllUsesWith((llvm::Value*)C);
			//llvm::errs() << "GOOD\t" << F.getName() << "\t" << i << "\t" << *C << "\n";
				changed = true;
				}
			}
		}



	}

	return changed;
}

}

using namespace cheerp;

INITIALIZE_PASS_BEGIN(PartialExecuter, "PartialExecuter", "Partially execute functions",
                      false, false)
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(RegionInfoPass)
INITIALIZE_PASS_END(PartialExecuter, "PartialExecuter", "Partially execute functions",
                    false, false)
