//===-- CheerpWriter/PartialExecuter.cpp - Analyze functions CFG to mark unreachable BBs --===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2021-2022 Leaning Technologies
//
//===----------------------------------------------------------------------===//
//
// This file implement PartialExecuter, an optimization pass that aims to do a
// guided visit of a Function CFG, propagating information from call-sites.
//
// Partial Executer uses the ExecutionEngine/Interpreter logic to compute the
// results of Instruction whose operands are known while skipping over non
// computable instructions.
//
// Main components are:
// * PartialInterpreter: used to wrap Interpreter execution and keep an updated
//   map of the strongly known bits for each Instruction during a given visit
// * SCC-visitor: used to guide execution allowing to go recover and 'skip' over
//   region of the CFG that are not predictable but can guarantee that execution
//   will restart from a given set of BBs (see BasicBlockGroupNode for explanation)
//
// Example of partial-preexecutable function:
// 	printf ('known format string', ...)
//
// Can be fully partially executed proving as unreachable the parts connected to
// non-used formats (eg. printing of double could be dropped).
//
// When a function has multiple call sites, the set of known to be alive edges will
// be basically the union of over the call sites.
// When a function is externally reachable OR has indirect uses, it will be considered
// as having an special unknown call site.
//
// PartialExecuter analyze and modify a Module IR in a fully generic mode, altough
// the main advantages will likely be found while executing it during LTO (since more
// knowledge on call sites is present).
//
//===----------------------------------------------------------------------===//

#include "../ExecutionEngine/Interpreter/Interpreter.h"
#include "llvm/ADT/SCCIterator.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Cheerp/DeterministicUnorderedSet.h"
#include "llvm/Cheerp/PartialExecuter.h"
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/IR/Operator.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/InitializePasses.h"
#include <map>
#include <unordered_map>
#include <vector>

#define DEBUG_TYPE "PartialExecuter"
STATISTIC(NumRemovedEdges, "Number of edges in the CFG that have been removed");
STATISTIC(NumModifyiedFunctions, "Number of functions modyified by PartialExecuter");
STATISTIC(NumTimesBumbedGlobals, "Number of times a GlobalVariable alignment has been increased");

using namespace llvm;

typedef cheerp::DeterministicUnorderedSet<BasicBlock *, cheerp::RestrictionsLifted::NoErasure> DeterministicBBSet;
typedef llvm::DenseSet<std::pair<GlobalVariable*, uint32_t> > NewAlignmentData;

namespace cheerp {

const uint32_t MAX_NUMBER_OF_VISITS_PER_BB = 100u;

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

static bool isGlobalVariablePartiallyExecutable(const GlobalVariable& GVar)
{
	if (!GVar.hasInitializer())
		return false;
	if (!GVar.hasInternalLinkage())
		return false;

	assert(GVar.isExternallyInitialized() == false);

	return true;
}

//Compute whether a given Value is a constant & it's value can be executed beforehand
static bool isValueComputedConstant(const llvm::Value* V)
{
	// TODO(carlo): this can easily be memoized since there is no state
	if (!isa<Constant>(V))
		return false;
	if (const GlobalVariable* GVar = dyn_cast_or_null<GlobalVariable>(V))
	{
		return isGlobalVariablePartiallyExecutable(*GVar);
	}
	if (const ConstantExpr* CE = dyn_cast_or_null<ConstantExpr>(V))
	{
		for (auto& op : CE->operands())
		{
			if (!isValueComputedConstant(op))
				return false;
		}
	}
	return true;
}

class FunctionData;
class ModuleData;

class PartialInterpreter : public llvm::Interpreter {
	friend FunctionData;
	friend ModuleData;
	enum BitMask : uint32_t
	{
		NONE = 0,
		ALIGNED2 = 0x01,
		ALIGNED4 = 0x03,
		ALIGNED8 = 0x07,
		ALL = 0xff,
		// Currently only one of those masks is ever used
		// This is since getBitMask() either returns one of those values
		// (either directly or indirectly by doing min(mask1, mask2))
	};
	std::vector<std::pair<llvm::Value*, llvm::Value*> > computedPhisValues;
	std::vector<llvm::PHINode*> notComputedPhis;
	// immutableLoadIntervals represents the ranges (when queried by the addresses computed by the Interpreter) that are known to be constant
	// currently this correspond to constant GlobalVariable, but could be possibly generalized further (eg. if you can prove they are constant,
	// and having always the same value, at every call site.
	// Example: main function can assume that any GlobalVariable (even mutable one) will still have the initial state as long as it can be proven
	// 	that no store to those GlobalVariable have been performed
	std::vector<std::pair<long long,long long>> immutableLoadIntervals;
	NewAlignmentData newAlignmentData;
public:
	// While visiting PHINodes of a BasicBlock, incomingBB will hold the incoming (if uniquely identified) or nullptr
	const llvm::BasicBlock* incomingBB{nullptr};

	// Auxiliary map to determin whether a value can be queried to the Interpreter and how many bits are actually known
	std::deque<llvm::DenseMap<const llvm::Value*, BitMask>> stronglyKnownBits;

	GenericValue getOperandValue(Value* V)
	{
		return Interpreter::getOperandValue(V, getTopCallFrame());
	}
	void assignToMaps(const Value* V, const BitMask bitMask)
	{
		stronglyKnownBits.back()[V] = bitMask;
	}
	void assignToMaps(Value* V, const BitMask bitMask, GenericValue GV)
	{
		assignToMaps(V, bitMask);
		getTopCallFrame().Values[V] = GV;
	}
	void assignToMaps(Value* V, Value* toAssign)
	{
		assignToMaps(V, getBitMask(toAssign), getOperandValue(toAssign));
	}
	void removeFromMaps(Value* V)
	{
		stronglyKnownBits.back().erase(V);
		getTopCallFrame().Values.erase(V);
	}
	void addStackFrame()
	{
		stronglyKnownBits.push_back(llvm::DenseMap<const llvm::Value*, BitMask>());
	}
	void popStackFrame()
	{
		stronglyKnownBits.pop_back();
		Interpreter::popCallFrame();
	}
	uint32_t getSizeStackFrame() const
	{
		return stronglyKnownBits.size();
	}
	BitMask computeStronglyKnownBits(const llvm::Instruction& I)
	{
		BitMask min = BitMask::ALL;

		for (auto& op : I.operands())
		{
			min = BitMask(min & getBitMask(op));
		}

		switch (I.getOpcode())
		{
			case Instruction::Load:
			{
				return BitMask::ALL;
			}
			// TODO(carlo): Select can possibly be specialised further
			case Instruction::Select:
			case Instruction::Or:
			case Instruction::Xor:
			case Instruction::Add:
			case Instruction::Sub:
			case Instruction::Mul:
			case Instruction::PtrToInt:
			case Instruction::BitCast:
			case Instruction::GetElementPtr:
			{
				//Those all work modulo 2^k
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
			default:
			{
				//Some operands NOT fully known, means in general no information is conserved
				break;
			}
		}

		if (min == BitMask::ALL)
		{
			switch (I.getOpcode())
			{
				case Instruction::ICmp:
				case Instruction::FCmp:
				case Instruction::And:
				case Instruction::SExt:
				case Instruction::ZExt:
				case Instruction::Trunc:
				case Instruction::LShr:
				case Instruction::AShr:
				case Instruction::Shl:
				case Instruction::SRem:
				case Instruction::FPToSI:
				case Instruction::FPTrunc:
				case Instruction::SIToFP:
				case Instruction::URem:
				case Instruction::FMul:
				case Instruction::FAdd:
				case Instruction::FSub:
				case Instruction::FDiv:
				case Instruction::UDiv:
				case Instruction::SDiv:
				case Instruction::FPExt:
				case Instruction::UIToFP:
				{
					return BitMask::ALL;
				}
				case Instruction::IntToPtr:
				case Instruction::Alloca:
				default:
				{
					break;
				}
			}
		}

		return BitMask::NONE;
	}
	llvm::BasicBlock* visitBasicBlock(llvm::BasicBlock& BB);
	explicit PartialInterpreter(std::unique_ptr<llvm::Module> M)
		: llvm::Interpreter(std::move(M), /*preExecute*/false)
	{
	}
	BitMask getBitMask(llvm::Value* V)
	{
		if (!V)
			return BitMask::NONE;

		if (isa<ConstantData>(V))
			return BitMask::ALL;

		if (isa<Function>(V))
			return BitMask::NONE;

		if (GlobalVariable* GV = dyn_cast<GlobalVariable>(V))
		{
			if (getDataLayout().getPreferredAlign(GV) >= 8)
				return BitMask::ALIGNED8;
			if (getDataLayout().getPreferredAlign(GV) == 4)
				return BitMask::ALIGNED4;

			// Interpreter will alrady align everything to 4, here we take note that we have to
			// align GlobalVariables (at least the one it's queried on) to 4.
			// This will allow further optimizations like executing loops like:
			// while (addr % 4) {
			//      doStuff(*addr);
			// 	addr++;
			// }
			newAlignmentData.insert({GV, 4});
			return BitMask::ALIGNED4;
		}
		if (ConstantExpr* CE = dyn_cast<ConstantExpr>(V))
		{
			if (CE->getOpcode() == Instruction::GetElementPtr)
			{
				GEPOperator *GEP = cast<GEPOperator>(CE);
				if (GEP->hasAllZeroIndices())
					return getBitMask(CE->getOperand(0));
			}
		}

		auto it = stronglyKnownBits.back().find(V);
		if (it != stronglyKnownBits.back().end())
			return it->second;

		return BitMask::NONE;
	}
	bool isValueComputed(const llvm::Value* V) const
	{
		if (isValueComputedConstant(V))
			return true;
		if (isa<BasicBlock>(V))
			return true;
		if (isa<Argument>(V) || isa<Instruction>(V))
			return stronglyKnownBits.back().count(V);
		return false;
	}
	bool areOperandsComputed(const llvm::Instruction& I)
	{
		if (isa<SelectInst>(I))
		{
			if (isValueComputed(I.getOperand(0)))
			{
				GenericValue V = getOperandValue(I.getOperand(0));
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
		if (const BranchInst* BI = dyn_cast <BranchInst>(&I))
		{
			if (BI->isConditional())
				if ((getBitMask(BI->getCondition()) == BitMask::NONE))
					return false;
		}
		return true;
	}
	void addAlignmentRequirement(NewAlignmentData& moduleAlignmentData)
	{
		for (auto& pair : newAlignmentData)
			moduleAlignmentData.insert(pair);
	}
	//We are going to interpret a CallInst, we need to add a stack frame and forward the known arguments
	void forwardArgumentsToNextFrame(CallInst& CI)
	{
		//This logic should work similarly for generic CallBase when those are allowed
		assert(!hasToBeSkipped(CI));

		const Function& calledFunc = *cast<Function>(CI.getCalledFunction());
		const uint32_t numFixedParams = calledFunc.getFunctionType()->getNumParams();

		std::vector<std::pair<const llvm::Value*, BitMask>> knownArgs;
		uint32_t argIndex=0;
		for (auto& op : CI.args())
		{
			if (argIndex >= numFixedParams)
				break;	//This is possible when VAArgs are involved, but we are not forwarding those

			if (isValueComputed(op))
				knownArgs.push_back({calledFunc.getArg(argIndex), getBitMask(op)});

			argIndex++;
		}

		addStackFrame();

		for (auto& pair : knownArgs)
			assignToMaps(pair.first, pair.second);
	}

	bool hasToBeSkipped(llvm::Instruction& I)
	{
		if (!areOperandsComputed(I))
			return true;

		if (isa<CastInst>(I) || isa<BinaryOperator>(I))
			return false;

		switch (I.getOpcode())
		{
			case Instruction::Br:
			case Instruction::ICmp:
			case Instruction::FCmp:
			case Instruction::GetElementPtr:
			case Instruction::PHI:
			case Instruction::Ret:
			case Instruction::Select:
			case Instruction::Switch:
			case Instruction::Alloca:
			{
				return false;
			}
			case Instruction::Call:
			{
				const Function* calledFunc = dyn_cast_or_null<Function>(cast<CallInst>(I).getCalledFunction());

				if (!calledFunc)
					return true;

				if (isAlreadyInCallStack(calledFunc))
					return true;

				if (calledFunc->isDeclaration())
					return true;

				if (calledFunc->hasWeakLinkage())
					return true;

				return false;
			}
			case Instruction::Load:
			{
				LoadInst& LI = cast<LoadInst>(I);

				GenericValue SRC = getOperandValue(LI.getPointerOperand());
				GenericValue *Ptr = (GenericValue*)GVTORP(SRC);

				for (const auto& interval : immutableLoadIntervals)
				{
					if ((long long)Ptr >= interval.first && (long long)Ptr < interval.second)
						return false;
				}

				// TODO(carlo): Possibly even Loads of Alloca might be proven valid
				break;
			}
			default:
			{
				//Default case is for Instruction to be skipped
				return true;
			}
		}
		return true;
	}
	void computeValidLoadIntervals(Module& _module)
	{
		// immutableLoadIntervals is populated as a first thing after PartialExecuter instantiation

		// create a 'virtual' call frame, since it might be needed by getOperandValue
		createStartingCallFrame();
		assert(immutableLoadIntervals.empty());

		for (auto& global : _module.globals())
		{
			GlobalVariable* GV = dyn_cast<GlobalVariable>(&global);
			if (!GV)
				continue;
			if (!isGlobalVariablePartiallyExecutable(*GV))
				continue;
			if (!GV->isConstant())
				continue;

			// This constraint is here only given that currently the ExecutionEngines assumes GlobalVariable to be named
			if (!GV->hasName())
				continue;

			GenericValue SRC = getOperandValue(GV);
			GenericValue *Ptr = (GenericValue*)GVTORP(SRC);

			immutableLoadIntervals.push_back({(long long)Ptr, ((long long)Ptr)+getDataLayout().getTypeAllocSize(GV->getInitializer()->getType())});
		}

		// detach the 'virtual' call frame
		popCallFrame();
	}
	// Given a Terminator, find (if there is enough information do to so) what will be the next visited BB
	// nullptr means failure to determine the next BB, and have to be handled by the caller
	// (eg. falling back to visiting every successor)
	BasicBlock* findNextBasicBlock(llvm::Instruction& I)
	{
		// Only terminators that keep the control flow inside a given Function are treated hereV
		assert(I.isTerminator());

		switch (I.getOpcode())
		{
			case Instruction::Br:
			{
				BranchInst& BI = cast<BranchInst>(I);

				if (!BI.isConditional())
					return BI.getSuccessor(0);

				Value* condition = BI.getCondition();
				if (isValueComputed(condition) && (getBitMask(condition) == BitMask::ALL) )
				{
					GenericValue V = getOperandValue(condition);
					return BI.getSuccessor((V.IntVal == 0u) ? 1 : 0);
				}
				break;
			}
			case Instruction::Switch:
			{
				SwitchInst& SI = cast<SwitchInst>(I);

				Value* condition = SI.getCondition();
				if (isValueComputed(condition) && (getBitMask(condition) == BitMask::ALL) )
				{
					GenericValue conditionValue = getOperandValue(condition);
					auto c = SI.findCaseValue(ConstantInt::get(SI.getContext(), conditionValue.IntVal ));
					return c->getCaseSuccessor();
				}
				break;
			}
			default:
			{
				// Default handling of not doing anything is conservative
				break;
			}
		}
		return nullptr;
	}
	void visitOuter(llvm::Instruction& I)
	{
		if (PHINode* phi = dyn_cast<PHINode>(&I))
		{
			// PHI have to be execute concurrently (since they may cross-reference themselves)
			// So while visiting PHI we don't modify the state of the map, but only take note
			// of what values should be in the map AFTER all phis have been processed
			if (incomingBB)
			{
				llvm::Value* incomingVal = phi->getIncomingValueForBlock(incomingBB);
				assert(incomingVal);
				if (isValueComputed(incomingVal))
				{
					computedPhisValues.push_back({phi, incomingVal});
					return;
				}
			}
			notComputedPhis.push_back(phi);
			return;
		}
		else if (&I == I.getParent()->getFirstNonPHI())
		{
			// Then, while visiting the first NonPhi instruction, we set the maps state correctly
			// computedPhisValues will hold pairs phi -> value to be assigned
			for (auto& p : computedPhisValues)
				assignToMaps(p.first, p.second);

			// notComputedPhis will hold phi to be removed from the mapping
			for (auto& p : notComputedPhis)
				   removeFromMaps(p);

			//Then we clear the vectors
			computedPhisValues.clear();
			notComputedPhis.clear();
		}

		//Here PHI have been properly processed

		bool skip = hasToBeSkipped(I);

		if (isInitialCallFrame())
		{
			// Execution in the lowest call-frame is guided externally
			if (I.isTerminator())
				return;

			//Skip Instructions we don't have enough information to execute
			if (skip)
				return;
		}
		else if (const ReturnInst* RI = dyn_cast<ReturnInst>(&I))
		{
			const Value* retVal = RI->getReturnValue();
			const BitMask BITMASK = (retVal && !skip) ? getBitMask(RI->getReturnValue()) : NONE;
			Value* caller = getCurrentCallSite();

			Interpreter::visit(I);

			stronglyKnownBits.pop_back();
			if (retVal)
				stronglyKnownBits.back()[caller] = BITMASK;

			if (skip)
				removeFromMaps(caller);
			//Note that here we return
			return;
		}
		else
		{
			//Terminators have special handling done here explicitly
			if (I.isTerminator())
			{
				BasicBlock* next = findNextBasicBlock(I);

				if (next)
				{
					// We know where execution should proceed
					incomingBB = I.getParent();
					getTopCallFrame().CurBB = next;
					getTopCallFrame().CurInst = getTopCallFrame().CurBB->begin();

					// Also here we have set the proper state for the execution so we can return
					return;
				}
				else
				{
					skip = true;
				}
			}

			//We are inside a call, here we assume all failure to execute are non-recoverable (as in no information could be gained)
			if (skip)
			{
				//Pop current stack (will happen recursively over the call-stack)
				popStackFrame();
				removeFromMaps(getTopCallFrame().Caller);

				return;
			}
		}
		//If we are here it means we have to actually perform the execution via Interpreter

		//Iff it's a call, set up the next stack frame
		if (CallInst* CI = dyn_cast<CallInst>(&I))
			forwardArgumentsToNextFrame(*CI);

		//Dispatch to the Interpreter's visitor for the given Instructon
		Interpreter::visit(I);

		if (!isa<CallInst>(I))
		{
			//Add  knownBits information
			assignToMaps(&I, computeStronglyKnownBits(I));
		}
	}

	/// Create a new interpreter object.
	///
	static ExecutionEngine* create(Module& M, std::string *ErrStr)
	{
		// Tell this Module to materialize everything and release the GVMaterializer.
		if (Error Err = M.materializeAll()) {
			std::string Msg;
			handleAllErrors(std::move(Err), [&](ErrorInfoBase &EIB) {
					Msg = EIB.message();
					});
			if (ErrStr)
				*ErrStr = Msg;
			// We got an error, just return 0
			return nullptr;
		}

		std::unique_ptr<Module> uniq(&M);
		return new PartialInterpreter(std::move(uniq));
	}
};

static void removeEdgeBetweenBlocks(llvm::BasicBlock* from, llvm::BasicBlock* to)
{
	NumRemovedEdges++;

	// Given two BasicBlocks, remove all edges between them.
	// Multiple edges are legal and might happen since SwitchInst might have more that one case going to the same BB

	{
		auto computeCardinalityPred = [](const BasicBlock* from, const BasicBlock* to) -> uint32_t
		{
			uint32_t cardinality = 0;
			for (const BasicBlock* bb : predecessors(to))
				if (bb == from)
					cardinality++;
			return cardinality;
		};

		// First count how many edges there are betwen from and to
		const uint32_t cardinalityPred = computeCardinalityPred(from, to);

		// Then call removePredecessor to fix up the phis of to
		for (uint32_t i=0; i<cardinalityPred; i++)
			to->removePredecessor(from);
	}

	{
		// Then fix up from's terminator
		llvm::Instruction* I = from->getTerminator();

		switch (I->getOpcode())
		{
			case Instruction::Br:
			{
				BranchInst* BI = cast<BranchInst>(I);
				if (BI->isConditional() && BI->getSuccessor(0) != BI->getSuccessor(1))
				{
					//Turn to unconditional
					llvm::BasicBlock* other = nullptr;
					if (BI->getSuccessor(0) != to)
						other = BI->getSuccessor(0);
					else
						other = BI->getSuccessor(1);
					BI->eraseFromParent();
					BranchInst::Create(other, from);
				}
				else
				{
					BI->eraseFromParent();
					new UnreachableInst(from->getContext(), from);
				}
				return;
			}
			case Instruction::Switch:
			{
				SwitchInst* SI = cast<SwitchInst>(I);
				for (SwitchInst::CaseIt i = SI->case_end(), e = SI->case_begin(); i != e;)
				{
					--i;
					auto *Successor = i->getCaseSuccessor();
					if (Successor == to) {
						SI->removeCase(i);
					}
				}

				if (SI->getDefaultDest() == to)
				{
					if (SI->getNumSuccessors() == 1)
					{
						SI->eraseFromParent();
						new UnreachableInst(from->getContext(), from);
						return;
					}

					BasicBlock* another = nullptr;
					for (SwitchInst::CaseIt i = SI->case_end(), e = SI->case_begin(); i != e;)
					{
						--i;
						auto *Successor = i->getCaseSuccessor();
						another = Successor;
						SI->removeCase(i);
						break;
					}
					assert(another);
					SI->setDefaultDest(another);
				}
				return;
			}
			case Instruction::Invoke:
			{
				// Given that we currently do not treat invokes the only way to get here would be via having the Invoke itself being unreachable
				BasicBlock* BB = I->getParent();
				I->replaceAllUsesWith(UndefValue::get(I->getType()));
				I->eraseFromParent();
				new UnreachableInst(BB->getContext(), BB);
				return;
			}
			case Instruction::Unreachable:
			{
				return;
			}
			default:
			{
				llvm::errs() << "Warning: " << *I << " not hanlded in removeEdgeBetweenBlocks\n";
			}
		}
	}
}

llvm::BasicBlock* PartialInterpreter::visitBasicBlock(llvm::BasicBlock& BB)
{
	ExecutionContext& executionContext = getTopCallFrame();
	executionContext.CurBB = &BB;
	executionContext.CurInst = executionContext.CurBB->begin();

	while (getTopCallFrame().CurInst != BB.end())
	{
		// Note that here we could also execute a Call, and that implies adding a CallFrame
		// executing there (possibly also in depth)
		// So getTopCallFrame() has to be called since it will possibly change
		visitOuter(*getTopCallFrame().CurInst++);
	}

	// Find (if there are enough information) the next BB to be visited
	// nullptr otherwise means failure to do so, and will fall back to visit all successors
	return findNextBasicBlock(*BB.getTerminator());
}

class ModuleData
{
	// This strings is passed to PartialInterpreter factory method (and via that to Interpreter / ExecutionEngine constructors)
	//  that might report some recoverable error via the string.
	std::string error;
	llvm::Module& module;
	PartialInterpreter* currentEE;
	std::map<const llvm::Function*, FunctionData> functionData;

	void initFunctionData();
public:
	NewAlignmentData alignmentToBeBumped;
	bool fail {false};
	llvm::Module* getModulePtr()
	{
		return &module;
	}
	ModuleData(llvm::Module& _module)
		: module(_module)
	{
		initFunctionData();

		currentEE = (PartialInterpreter*)(PartialInterpreter::create(_module, &error));
		if (currentEE == nullptr)
		{
			fail = true;
			return;
		}
		assert(currentEE);

		// Add 'virtual' frame, since it might be needed deep into computeValidLoadIntervals
		// Then compute the valid intervals
		currentEE->computeValidLoadIntervals(*getModulePtr());

		assert(currentEE->getSizeStackFrame() == 0);
	}
	FunctionData& getFunctionData(const llvm::Function& F);
	PartialInterpreter* setUpPartialInterpreter(llvm::Function& F)
	{
		assert(currentEE->getSizeStackFrame() == 0);
		currentEE->addStackFrame();
		ExecutionContext& executionContext = currentEE->createStartingCallFrame();
		executionContext.CurFunction = &F;

		return currentEE;
	}
	~ModuleData()
	{
		if (fail)
			return;
		bool removed = currentEE->removeModule(getModulePtr());
		(void)removed;
		assert(removed);
		delete currentEE;
	}
};

class FunctionData
{
	llvm::Function& F;
	std::vector<std::pair<llvm::BasicBlock*, llvm::BasicBlock*>> existingEdges;
	typedef std::vector<Value*> VectorOfArgs;

	llvm::DenseMap<const llvm::BasicBlock*, int> visitCounter;
	llvm::DenseSet<std::pair<const llvm::BasicBlock*, const llvm::BasicBlock*> > visitedEdges;
	ModuleData& moduleData;

	std::vector<VectorOfArgs> callEquivalentQueue;
	PartialInterpreter* currentEE;

	VectorOfArgs getArguments(const llvm::CallBase* callBase)
	{
		VectorOfArgs args(F.getFunctionType()->getNumParams(), nullptr);

		if (callBase)
		{
			for (uint32_t i=0; i<F.getFunctionType()->getNumParams(); i++)
				args[i] = callBase->getArgOperand(i);

			// Filter out not computed arguments
			for (auto& v : args)
			{
				if (isValueComputedConstant(v) == false)
					v = nullptr;
			}
		}

		return args;
	}
	// This function try to determine whether the set of arguments a is a subset of b
	// VectorOfArgs holds for a given call site the args[0], args[1], ... args[N]
	// with nullptr representing any possible argument (as in unknown).
	//
	// Then a is a subset of b iff:
	// for any index:
	//   either b[index] is nullptr or a[index] == b[index]
	//
	// Note that always returning 'false' here will only impact the time required to
	// analyze call sites (since work is done in cases that are already covered).
	static bool areEquivalent(const VectorOfArgs& a, const VectorOfArgs& b)
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
	void enqueCallEquivalent(const VectorOfArgs& arguments)
	{
		// Insert the arguments in the map

		// Check wether we already visited something similar
		for (const auto& args : callEquivalentQueue)
		{
			if (areEquivalent(arguments, args))
			{
				return;
			}
		}

		callEquivalentQueue.push_back(arguments);
	}
	void actualVisit();
	void doneVisitCallBase()
	{
		assert(currentEE->getSizeStackFrame() == 1);
		currentEE->popStackFrame();

		currentEE->addAlignmentRequirement(moduleData.alignmentToBeBumped);

	}
public:
	explicit FunctionData(llvm::Function& F, ModuleData& moduleData)
		: F(F), moduleData(moduleData), currentEE(nullptr)
	{
	}
	llvm::Function* getFunction()
	{
		return &F;
	}
	uint32_t getAndIncrementVisitCounter(const llvm::BasicBlock* BB)
	{
		return visitCounter[BB]++;
	}
	PartialInterpreter& getInterpreter()
	{
		// currentEE will be non-null while visiting a Call Equivalent
		assert(currentEE);
		return *currentEE;
	}
	void registerEdge(const llvm::BasicBlock* from, const llvm::BasicBlock* to)
	{
		visitedEdges.insert({from, to});
	}
	bool hasNoInfo(const VectorOfArgs& arguments) const
	{
		for (const llvm::Value* arg : arguments)
		{
			if (arg)
				// Found a non-null argument!
				return false;
		}
		return true;
	}
	void visitCallEquivalent(const VectorOfArgs& arguments)
	{
		currentEE = moduleData.setUpPartialInterpreter(F);

		// Insert the arguments in the map
		for (uint32_t i=0; i<arguments.size(); i++)
		{
			llvm::Value* x = arguments[i];
			if (!x)
				continue;
			llvm::Argument* ith_arg = F.getArg(i);
			currentEE->assignToMaps(ith_arg, x);
		}

		// Do the visit
		actualVisit();

		// Cleanup
		doneVisitCallBase();
	}
	void visitAllCallSites()
	{
		bool needsNoInfoCallSite = false;

		for (const FunctionData::VectorOfArgs& toBeVisited : callEquivalentQueue)
			if (hasNoInfo(toBeVisited))
				needsNoInfoCallSite = true;

		if (callEquivalentQueue.size() >= MAX_NUMBER_OF_VISITS_PER_BB)
			needsNoInfoCallSite = true;

		if (needsNoInfoCallSite)
		{
			// Remove all call-sites and substitute them with one with no information at all
			callEquivalentQueue.clear();
			callEquivalentQueue.push_back(VectorOfArgs());
		}

		// Visit all collected callEquivalent
		// Note that currently callEquivalentQueue is immutable during this loop (basically CallEquivalents are know beforehand)
		for (const FunctionData::VectorOfArgs& toBeVisited : callEquivalentQueue)
		{
			visitCallEquivalent(toBeVisited);
		}
	}
	void visitCallBase(const llvm::CallBase* callBase)
	{
		const auto& equivalentArgs = getArguments(callBase);

		enqueCallEquivalent(equivalentArgs);
	}
	void enqueVisitNoInfo()
	{
		// Cleaun-up whatever is already in the queue, since this is guaranteed to dominate it
		callEquivalentQueue.clear();

		const auto& equivalentArgs = getArguments(nullptr);

		enqueCallEquivalent(equivalentArgs);
	}
	void cleanupBB()
	{
		//Looping on existingEdges guarantee determinism
		for (auto& p : existingEdges)
		{
			if (visitedEdges.count(p) == 0)
				removeEdgeBetweenBlocks(p.first, p.second);
		}
	}
	void buildSetOfEdges(Function& function)
	{
		// To be deterministic, we can't actually do sorting based on pointers
		// So we have existingEdges being a vector and filled according to the visit of the function
		// existingEdgesSet is an helper data structure to filter doubles
		llvm::DenseSet<std::pair<llvm::BasicBlock*, llvm::BasicBlock*>> existingEdgesSet;
		for (BasicBlock& bb : function)
		{
			for (BasicBlock* s : successors(&bb))
			{
				auto pair = std::pair<llvm::BasicBlock*, llvm::BasicBlock*>{&bb, s};
				if (existingEdgesSet.insert(pair).second)
					existingEdges.push_back(pair);
			}
		}
	}
	bool hasModifications(const bool emitStats) const
	{
		const uint32_t numberVisitedEdges = visitedEdges.size();
		const uint32_t numberExistingEdges = existingEdges.size();

		if (numberVisitedEdges < numberExistingEdges)
		{
			if (emitStats)
			{
				llvm::errs() << "Visited edges / Existing edges \t\t" << numberVisitedEdges << " / " << numberExistingEdges <<"\n";
				llvm::errs() << "In function:\t\t" << F.getName()<< "\n";
			}
			return true;
		}
		return false;
	}
};

void ModuleData::initFunctionData()
{
	assert(functionData.empty());
	for (Function& F : module)
	{
		functionData.emplace(&F, FunctionData(F, *this));
	}
}

FunctionData& ModuleData::getFunctionData(const llvm::Function& F)
{
	return functionData.at(&F);
}

}//cheerp

namespace llvm{

// This is the logic that will allow SCCIterator to split a group of nodes into SCC components
//
// Nodes of this arbitrary graph are a set of BasicBlocks plus a copy of the entry point BB.
// Edges of this graph are:
// 	for every edge going IN the entry point, that will take copy(entry) as target
// 	for every edge going OUT the entry point, that will take original(entry) as source
// 	for every other edge that is internal at the set, it will stay the same
//
// By construction we are then guaranteed that entry will be on a single connected component with it's own
class SubGraph;

struct GraphNode {
	BasicBlock* BB;
	SmallVector<BasicBlock*, 2> Succs;
	SubGraph& Graph;
	explicit GraphNode(BasicBlock* BB, SubGraph& Graph);
};

class SubGraph {
public:
	typedef DeterministicBBSet BlockSet;
	typedef std::unordered_map<BasicBlock*, GraphNode> NodeMap;
	explicit SubGraph(llvm::BasicBlock* Entry, BlockSet Blocks):
		Entry(Entry), Blocks(std::move(Blocks))
	{
	}
	const BasicBlock* getEntry() const
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

namespace cheerp
{

// This class represent a Node part of a tree of BasicBlockGroupNodes
//	parentNode is null IFF we are in the root, otherwise points to the parent
//	childrenNodes points to the childrens of a given node
//
// Metadata associated with every node is:
//	isReachable -> whether this set is actually reachable
//	isMultiHead -> whether there are more than one entry block
//	iff isReachable && !isMultiHead -> start is the unique entry BasicBlock (entry from inside of this set, 'back-edges', are valid)
//	iff from -> from is the BasicBlock we are coming from (so PHINodes can be set)
//	blocks is a set of BasicBlocks that are part of this group
//
// For a given node:
//	union(start, children[0].blocks, children[1].blocks, ...) is always equal to the blocks of the node itself
//
// The tree is not static but will be modifyied during execution.
// Starting state is a single node containing all of a Function's BBs, start = Function's entry point, isReachable = true, from = nulltpr, isMultiHead = false
//
// Then given a reachable node with a valid start:
// 	1. We add a children with the same set of nodes, call it COPY
// 	2. We create (implicitly) a graph were all edges going back to start from the inner blocks will point to COPY's start, all other edges remain the same
// 	3. Now start will NOT have anymore any incoming edge but only outgoing
// 	4. We split this graph into SCC, start is guaranteed to be in it's own component
// 	5. We visit the start BasicBlock, taking notes of outgoing edges
// 		(they can either point to some of the blocks, possibly the copy of start, or point to sibling's SCC, in that case we will walk the tree via notifySuccessor)
// 	6. We split each SCC into its own node, and do the recursion
//
// Terminating conditions is that a given node will never be visited more than a fixed number of times.
//
// Note that llvm::Instruction, BasicBlock or Function are taken as non-const pointers (or reference).
// 	While this stage DO NOT actually modify them, we are reliant here on the Interpreter infrastructure
// 	and there values are stored as NON-const. So while logically we will not modify structure of the function
// 	we would either have to do const_casts at the boundaries or just remove constness
class BasicBlockGroupNode
{
	// Implicit tree structure
	BasicBlockGroupNode* parentNode;
	std::list<BasicBlockGroupNode> childrenNodes;

	// Other metadata
	FunctionData& data;
	const DeterministicBBSet blocks;
	bool isMultiHead;
	bool isReachable;
	llvm::BasicBlock* start;
	llvm::BasicBlock* from;
	// TODO(carlo): an optmization might be having from be a set<BasicBlock>, conserving the phi that are equals

	//Note that here also DenseMap would have worked, but for the fact that it does miss operator .at()
	typedef std::unordered_map<const llvm::BasicBlock*, BasicBlockGroupNode*> ReverseMapBBToGroup;

	// reverseMappingBBToGroup will be populated alongside childrenNodes, and for each BasicBlock reverseMappingBBToGroup[BB]
	//	will be the pointer of the SCC component BB is part of
	ReverseMapBBToGroup reverseMappingBBToGroup;
	bool visitingAll;
	static const DeterministicBBSet getAllBasicBlocks(llvm::Function& F)
	{
		DeterministicBBSet set;
		for (llvm::BasicBlock& bb : F)
		{
			set.insert(&bb);
		}
		return set;
	}
	void splitIntoSCCs(std::list<BasicBlockGroupNode>& queueToBePopulated, ReverseMapBBToGroup& blockToGroupMap);
public:
	BasicBlockGroupNode(FunctionData& data, BasicBlockGroupNode* parentBBGNode, const DeterministicBBSet& blocks, llvm::BasicBlock* start = nullptr)
		: parentNode(parentBBGNode), data(data), blocks(blocks), isMultiHead(false), isReachable(parentNode == nullptr), start(start), from(nullptr), visitingAll(false)
	{
		if (start)
			assert(start->getParent() == data.getFunction());
	}
	BasicBlockGroupNode(FunctionData& data)
		: BasicBlockGroupNode(data, /*parentBBGD*/nullptr, getAllBasicBlocks(*data.getFunction()), &data.getFunction()->getEntryBlock())
	{
	}
	BasicBlockGroupNode(BasicBlockGroupNode& BBGNode)
		: parentNode(&BBGNode), data(BBGNode.data), blocks(BBGNode.blocks), isMultiHead(false), isReachable(false), start(nullptr), from(nullptr), visitingAll(false)
	{
	}
	void addIncomingEdge(llvm::BasicBlock* comingFrom, llvm::BasicBlock* target)
	{
		isReachable = true;
		assert(blocks.count(target));
		if (start == nullptr)
		{
			start = target;
			from = comingFrom;
		}
		if (start != target)
		{
			isMultiHead = true;
			from = nullptr;
		}
		if (comingFrom != from)
		{
			from = nullptr;
		}
	}
	// Do the visit of the BB, with 'from' (possibly nullptr if unknown) as predecessor
	// Loop backs will be directed to another BBgroup
	// The visit will return the set of reachable BBs, to be added into visitNext
	void runVisitBasicBlock(llvm::BasicBlock& BB, std::vector<llvm::BasicBlock*>& visitNext)
	{
		assert(visitNext.empty());

		PartialInterpreter& interpreter = data.getInterpreter();
		interpreter.incomingBB = from;
		BasicBlock* ret = interpreter.visitBasicBlock(BB);

		if (ret)
		{
			visitNext.push_back(ret);
		}
		else
		{
			llvm::DenseSet<llvm::BasicBlock*> setSuccessors;
			for (auto* bb : successors(&BB))
			{
				if (setSuccessors.insert(bb).second)
					visitNext.push_back(bb);
			}
		}
	}
	// notifySuccessor takes care of propagating the information 'we are visiting node from, and we have a terminator that goes to succ'
	// IFF succ is not in the currently visited set of nodes, it should be in one of the siblings, so we notify the parent that
	// 	will itself (possibly recursively) propagate the information
	// otherwise, it's a matter of finding what children holds the succ node, and add the edge from->succ as incoming
	//
	// when visitingAll is set, childrens data structure is not in place since we don't have enough information to proceed
	// 	(but we need still to propagate to parent)
	void notifySuccessor(llvm::BasicBlock* from, llvm::BasicBlock* succ)
	{
		if (blocks.count(succ) == 0)
		{
			assert(parentNode);
			//It should handled by the parent SCC

			parentNode->notifySuccessor(from, succ);
		}
		else if (visitingAll)
		{
			return;
		}
		else
		{
			BasicBlockGroupNode* ptr = reverseMappingBBToGroup.at(succ);
			assert( ptr );
			ptr->addIncomingEdge(from, succ);
		}
	}
	void visitAll()
	{
		visitingAll = true;
		PartialInterpreter& interpreter = data.getInterpreter();
		interpreter.incomingBB = nullptr;
		for (llvm::BasicBlock* bb : blocks)
		{
			for (llvm::Instruction& I : *bb)
				interpreter.removeFromMaps(&I);

			for (llvm::BasicBlock* succ : successors(bb))
				registerEdge(bb, succ);
		}
	}
	void registerEdge(llvm::BasicBlock* from, llvm::BasicBlock* to)
	{
		data.registerEdge(from, to);

		notifySuccessor(from, to);
	}
	// Visit the tree of BasicBlockGroupNodes, starting from the root and visiting children depth-first
	void recursiveVisit()
	{
		if (!isReachable)
		{
			// We are in an unreachable part of the graph
			//  --> Nothing to do
			return;
		}
		if (isMultiHead)
		{
			// There are multiple BasicBlock that are reacheable from outside
			//  --> Mark everything as reachable
			visitAll();
			return;
		}

		assert(start);	//isReachable && !isMultiHead implies start being defined
		if (data.getAndIncrementVisitCounter(start) >= MAX_NUMBER_OF_VISITS_PER_BB)
		{
			// We are visiting a given BasicBlock many times
			// Since terminability is basically unprovable in general, we give up with the visit
			//  --> Mark everything as reachable
			visitAll();
			return;
		}

		splitIntoSCCs(childrenNodes, reverseMappingBBToGroup);	//These should be partially ordered with the last one possibly being the replica of the current one

		std::vector<llvm::BasicBlock*> visitNext;

		// Do the actual visit for start, while populating visitNext
		runVisitBasicBlock(*start, visitNext);

		for (llvm::BasicBlock* succ : visitNext)
			registerEdge(start, succ);

		// First has been already done
		childrenNodes.pop_back();
		while (!childrenNodes.empty())
		{
			childrenNodes.back().recursiveVisit();
			childrenNodes.pop_back();
		}
	}
};

void BasicBlockGroupNode::splitIntoSCCs(std::list<BasicBlockGroupNode>& queueToBePopulated, ReverseMapBBToGroup& blockToGroupMap)
{
	assert(queueToBePopulated.empty());
	assert(blockToGroupMap.empty());
	// We begin with N nodes, remove 'start', and we find the SCCs of the remaining N-1 nodes.
	//
	// For N = 1, it means 0 nodes remaining -> no SCCs
	// For N > 1, it means > 0 nodes remaining, we divide them in 1 or more SCCs
	//
	// Then iff there are any edges going back to start, we add all nodes again as a single SCC to the end
	//
	// During the actual visit we might discover that we eventually will not loop back to start (so the recursion terminate) or we stop since we reached the maximum iteration number

	SubGraph::BlockSet Group;
	for (llvm::BasicBlock* bb : blocks)
	{
		Group.insert(bb);
	}
	SubGraph SG(start, std::move(Group));
	queueToBePopulated.emplace_back(*this);

	for (auto& SCC: make_range(scc_begin(&SG), scc_end(&SG)))
	{
		DeterministicBBSet subset;
		for (auto& GN : SCC)
		{
			BasicBlock* bb = GN->BB;
			subset.insert(bb);
		}
		queueToBePopulated.emplace_back(data, this, subset);
		for (auto& GN : SCC)
		{
			BasicBlock* bb = GN->BB;
			blockToGroupMap[bb] = &queueToBePopulated.back();
		}
	}
	blockToGroupMap[start] = &queueToBePopulated.front();
}

void FunctionData::actualVisit()
{
	BasicBlockGroupNode groupData(*this);
	groupData.recursiveVisit();
}

static void processFunction(const llvm::Function& F, ModuleData& moduleData)
{
	// For each SCC (process in order)
	//	if single point of entry: start from there, otherwise (no point of entry -> all unreachable / multiple ones -> all reachable)
	//	execute from single point of entry (+ state that's intersection of possible path to it)
	//	if (unconditional or decidable) -> follow path, otherwise
	//		if immediate dominator is in the SCC, jump there(a refinement would be immediate dominator in SCC while invalidating all otherwise reachable SCC) or bail out
	//	bail out:
	//		sign as reachable all out-going edges from the SCC (keeping only the state pre-SCC) + all BB in the SCC

	if (F.isDeclaration())
		return;

	FunctionData& data = moduleData.getFunctionData(F);
	bool hasIndirectUseOrExternal = false;

	if (F.getLinkage() != GlobalValue::InternalLinkage)
		hasIndirectUseOrExternal = true;

	for (const Use &U : F.uses())
	{
		if (hasIndirectUseOrExternal)
			break;
		const User *FU = U.getUser();
		if (!isa<CallInst>(FU))
		{
			hasIndirectUseOrExternal = true;
			break;
		}
		//Normally this pattern should handle also InvokeInst, but here support is not yet present
		const CallBase* CS = cast<CallBase>(FU);
		if (CS->isCallee(&U))
		{
			data.visitCallBase(CS);	
		}
		else
		{
			hasIndirectUseOrExternal = true;
		}
	}

	if (hasIndirectUseOrExternal)
	{
		data.enqueVisitNoInfo();
	}

	data.visitAllCallSites();
}

static bool modifyFunction(llvm::Function& F, ModuleData& moduleData)
{
	if (F.isDeclaration())
		return false;

	FunctionData& data = moduleData.getFunctionData(F);

	data.buildSetOfEdges(F);

	bool changed = false;
	if (data.hasModifications(/*emitStats*/ false))
	{
		// Remove the edges that have never been taken
		data.cleanupBB();
		changed = true;
	}

	return changed;
}

bool PartialExecuter::runOnModule( llvm::Module & module )
{
	ModuleData data(module);
	if (data.fail)
		return false;

	// First part: analysis for determining which Edges are never taken
	for (const Function& F : module)
	{
		processFunction(F, data);
	}

	bool changed = false;

	// Second part: actually remove the missing Edges
	for (Function& F : module)
	{
		const bool modified = modifyFunction(F, data);
		if (modified)
		{
			NumModifyiedFunctions++;
			changed = true;
		}
	}

	if (changed)
	{
		// Third part (dependent on any change having already already being made):
		// 	modify alignment of GlobalVariables alignment that has been queried
		for (const auto& p : data.alignmentToBeBumped)
		{
			GlobalVariable* GV = p.first;
			const uint32_t requiredAlign = p.second;
			if (module.getDataLayout().getPreferredAlign(GV) < requiredAlign)
			{
				GV->setAlignment(Align(requiredAlign));
				NumTimesBumbedGlobals++;
				// Note that multiple globals could be bumped multiple times (eg. 2->4->8)
				// so this will not be necessary equal to the number of GlobalVariables whose
				// alignment has changed.
			}
		}
	}

	return changed;
}
}

using namespace cheerp;

INITIALIZE_PASS_BEGIN(PartialExecuter, "PartialExecuter", "Partially execute functions CFG",
                      false, false)
INITIALIZE_PASS_END(PartialExecuter, "PartialExecuter", "Partially execute functions CFG",
                    false, false)
