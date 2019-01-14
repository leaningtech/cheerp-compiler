//===-- PointerPasses.cpp - Cheerp pointer optimization passes --------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014-2018 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "CheerpPointerPasses"
#include "llvm/Analysis/InstructionSimplify.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/PointerAnalyzer.h"
#include "llvm/Cheerp/PointerPasses.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/ValueMapper.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Support/GenericDomTreeConstruction.h"
#include "llvm/Support/raw_ostream.h"
#include <set>
#include <map>

//#define DEBUG_GEP_OPT_VERBOSE 1

STATISTIC(NumIndirectFun, "Number of indirect functions processed");
STATISTIC(NumAllocasTransformedToArrays, "Number of allocas of values transformed to allocas of arrays");

namespace llvm {

bool AllocaArrays::replaceAlloca(AllocaInst* ai)
{
	const ConstantInt * ci = dyn_cast<ConstantInt>(ai->getArraySize());
	
	// Runtime alloca size, convert it to cheerp_allocate 
	if (!ci)
	{
		Module* M = ai->getParent()->getParent()->getParent();
		DataLayout targetData(M);
		Type* int32Ty = IntegerType::getInt32Ty(M->getContext());
		Function* cheerp_allocate = Intrinsic::getDeclaration(M, Intrinsic::cheerp_allocate, ai->getType());

		IRBuilder<> Builder(ai);

		Type* allocTy = ai->getAllocatedType();
		uint32_t elemSize = targetData.getTypeAllocSize(allocTy);
		Value* size = Builder.CreateMul(ai->getArraySize(), ConstantInt::get(int32Ty, elemSize, false)); 
		Instruction* alloc = CallInst::Create(cheerp_allocate, size);
		BasicBlock::iterator ii(ai);
		ReplaceInstWithInst(ai->getParent()->getInstList(), ii, alloc);
		return true;
	}

	llvm::Type * at = llvm::ArrayType::get( ai->getAllocatedType(), ci->getZExtValue() );
	AllocaInst * newAi = new AllocaInst( at );
	newAi->insertAfter( ai );
	ai->removeFromParent();
	newAi->takeName(ai);

	GetElementPtrInst * gepZero = nullptr;
	
	for ( User::use_iterator it = ai->use_begin(); it != ai->use_end(); )
	{
		Use & u = *it++;
		
		if ( BitCastInst * bi = dyn_cast<BitCastInst>(u) )
		{
			CastInst * newBi = BitCastInst::Create( bi->getOpcode(), newAi, bi->getDestTy() );
			ReplaceInstWithInst(bi, newBi);
			
			newBi->takeName( bi );
		}
		else if ( GetElementPtrInst * gep = dyn_cast<GetElementPtrInst>(u) )
		{
			SmallVector< Value *, 8 > vals;
			vals.push_back( ConstantInt::getNullValue( llvm::Type::getInt32Ty( gep->getContext() ) ) );
			
			std::copy(gep->idx_begin(), gep->idx_end(), std::back_inserter(vals) );
			
			GetElementPtrInst * newGep = GetElementPtrInst::Create(newAi, vals);
			ReplaceInstWithInst(gep, newGep);
			newGep->takeName( gep );
		}
		else
		{
			if (! gepZero )
			{
				SmallVector< Value *, 8 > vals ( 2, ConstantInt::getNullValue( llvm::Type::getInt32Ty( u->getContext() ) ) );

				gepZero = GetElementPtrInst::Create(newAi, vals, "");
				gepZero->insertAfter(newAi);
			}
			
			assert ( isa<Instruction>(u) );
			
			u.set( gepZero );
		}
	}
	
	assert( ai->use_empty() );
	delete ai;

	return true;
}

bool AllocaArrays::runOnFunction(Function& F)
{
	bool Changed = false;
	cheerp::PointerAnalyzer & PA = getAnalysis<cheerp::PointerAnalyzer>();
	cheerp::Registerize & registerize = getAnalysis<cheerp::Registerize>();

	for ( BasicBlock & BB : F )
	{
		for ( BasicBlock::iterator it = BB.begin(); it != BB.end(); )
		{
			AllocaInst * ai = dyn_cast<AllocaInst>(it++);
			if (! ai ) continue;

			if (PA.getPointerKind(ai) == cheerp::COMPLETE_OBJECT )
			{
				// No need to optimize if it is already a CO
				continue;
			}

			NumAllocasTransformedToArrays++;

			PA.invalidate(ai);
			// Careful, registerize must be invalidated before changing the function
			registerize.invalidateLiveRangeForAllocas(F);
			Changed |= replaceAlloca( ai );
		}
	}
	
	if (Changed)
		registerize.computeLiveRangeForAllocas(F);
	return Changed;
}

const char* AllocaArrays::getPassName() const
{
	return "AllocaArrays";
}

char AllocaArrays::ID = 0;

void AllocaArrays::getAnalysisUsage(AnalysisUsage & AU) const
{
	AU.addRequired<cheerp::PointerAnalyzer>();
	AU.addPreserved<cheerp::PointerAnalyzer>();
	AU.addRequired<cheerp::Registerize>();
	AU.addPreserved<cheerp::Registerize>();
	AU.addPreserved<cheerp::GlobalDepsAnalyzer>();
	llvm::Pass::getAnalysisUsage(AU);
}

FunctionPass *createAllocaArraysPass() { return new AllocaArrays(); }


const char* IndirectCallOptimizer::getPassName() const
{
	return "IndirectCallOptimizer";
}

char IndirectCallOptimizer::ID = 0;

bool IndirectCallOptimizer::runOnModule(Module & m)
{
	bool Changed = false;
	cheerp::PointerAnalyzer & PA = getAnalysis<cheerp::PointerAnalyzer>();

	for (Module::iterator it = m.begin();it != m.end(); ++it)
	{
		if ( it->hasAddressTaken() &&
		     !it->empty() &&
		     // Check that at least one argument is a regular pointer
		     std::any_of(it->arg_begin(),
				 it->arg_end(),
				 [&](const Argument & arg)
				 {
				 	return arg.getType()->isPointerTy() && (PA.getPointerKind(&arg) == cheerp::REGULAR);
				 }) &&
		     // Check that this function is called *directly* at least one time.
		     std::any_of(it->use_begin(),
				 it->use_end(),
				 [](const Use & u)
				 {
					 return ImmutableCallSite(u.getUser());
				 }) )
		{
			Function * oldFun = it;
			PA.invalidate(oldFun);
			Function * newFun = Function::Create( oldFun->getFunctionType(),
							      oldFun->getLinkage(),
							      Twine("__cheerpindirect", oldFun->getName() ) );
			
			it = m.getFunctionList().insertAfter( it, newFun);
			
			oldFun->replaceAllUsesWith(newFun);
			assert( oldFun->use_empty() );

			SmallVector< Value *, 8 > newFunArgs;
			newFunArgs.reserve ( newFun->arg_size() );
			for ( Function::arg_iterator arg = newFun->arg_begin(); arg != newFun->arg_end(); ++ arg)
				newFunArgs.push_back(arg);
			
			// Fill the new function
			BasicBlock * newBody = BasicBlock::Create( newFun->getContext(), 
								   "entry",
								   newFun );

			CallInst * fwdCall = CallInst::Create( oldFun,
							      newFunArgs,
							      "",
							      newBody);

			if ( fwdCall->getType()->isVoidTy() )
				ReturnInst::Create( newFun->getContext(), newBody );
			else
				ReturnInst::Create( newFun->getContext(), fwdCall, newBody );
			
			// Restore direct calls uses
			for ( Function::use_iterator ui = newFun->use_begin(); ui != newFun->use_end(); )
			{
				Use & u = *ui++;
				User * U = u.getUser();
				
				ImmutableCallSite cs(U);
				if ( (cs.isCall() || cs.isInvoke()) && cs.isCallee(&u) )
				{
					U->setOperand( u.getOperandNo(), oldFun );
				}
			}
			
			assert ( !oldFun->hasAddressTaken() );
			PA.invalidate(newFun);
			
			NumIndirectFun++;
			Changed = true;
		}
	}
	
	assert( m.alias_empty() );
	
	return Changed;
}

void IndirectCallOptimizer::getAnalysisUsage(AnalysisUsage& AU) const
{
	AU.addRequired<cheerp::PointerAnalyzer>();
	AU.addPreserved<cheerp::PointerAnalyzer>();
	AU.addPreserved<cheerp::GlobalDepsAnalyzer>();
	AU.addPreserved<cheerp::Registerize>();

	llvm::Pass::getAnalysisUsage(AU);
}

ModulePass* createIndirectCallOptimizerPass()
{
	return new IndirectCallOptimizer();
}

class PHIVisitor
{
public:
	typedef std::map<PHINode*, Value*> PHIMap;
	typedef std::set<Instruction*> RemoveQueue;
	PHIVisitor(PHIMap& phiMap, RemoveQueue& r):mappedPHIs(phiMap),toRemove(r)
	{
	}
	bool visitPHI(PHINode* phi);
	Value* findBase(Instruction* I);
	Value* rewrite(Instruction* I, Value* base);
private:
	std::set<Value*> visited;
	PHIMap& mappedPHIs;
	RemoveQueue& toRemove;
};

Value* PHIVisitor::findBase(Instruction* I)
{
	if (GetElementPtrInst* gep = dyn_cast<GetElementPtrInst>(I))
	{
		if (gep->getNumIndices() == 1)
		{
			Value* ptr=gep->getPointerOperand();
			if (Instruction* ptrI=dyn_cast<Instruction>(ptr))
			{
				Value* base = findBase(ptrI);
				if(base)
					return base;
				else
					return gep;
			}
			else
				return ptr;
		}
	}
	else if(PHINode* phi = dyn_cast<PHINode>(I))
	{
		if(visited.count(phi))
			return phi;
		Value* ret = NULL;
		// Avoid loops down this exploration paths
		// When the PHI is finished it will be removed from the set
		// To be eventually re-entered later on
		// NOTE: Be careful for PHIs which are not part of the loop to be transformed
		visited.insert(phi);
		for (unsigned i=0;i<phi->getNumIncomingValues();i++)
		{
			Value* incomingValue=phi->getIncomingValue(i);
			Instruction* incomingInst=dyn_cast<Instruction>(incomingValue);
			Value* baseCandidate = incomingInst ? findBase(incomingInst) : incomingValue;
			if(visited.count(baseCandidate))
				continue;
			if (baseCandidate == NULL)
			{
				ret = NULL;
				break;
			}
			if (ret == NULL)
				ret = baseCandidate;
			else if (ret != baseCandidate)
			{
				ret = NULL;
				break;
			}
		}
		visited.erase(phi);
		return ret;
	}
	return I;
}

Value* PHIVisitor::rewrite(Instruction* I, Value* base)
{
	if (I==base)
		return NULL;
	if (GetElementPtrInst* gep = dyn_cast<GetElementPtrInst>(I))
	{
		if (gep->getNumIndices() == 1)
		{
			Value* ptr=gep->getPointerOperand();
			Instruction* ptrI=dyn_cast<Instruction>(ptr);
			Value* parentOffset = ptrI ? rewrite(ptrI, base) : NULL;
			Value* thisOffset = *gep->idx_begin();
			if (parentOffset == NULL)
				return thisOffset;
			else
			{
				Value* newIndex=BinaryOperator::Create(BinaryOperator::Add, parentOffset, thisOffset, "geptoindex", gep);
				if(!gep->use_empty())
				{
					Value* newGep=GetElementPtrInst::Create(base, newIndex, "geptoindex", gep);
					gep->replaceAllUsesWith(newGep);
				}
				toRemove.insert(gep);
				return newIndex;
			}
		}
	}
	else if(PHINode* phi = dyn_cast<PHINode>(I))
	{
		auto it = mappedPHIs.find(phi);
		if (it!=mappedPHIs.end())
			return it->second;
		PHINode* newPHI = PHINode::Create(IntegerType::get(phi->getContext(), 32), phi->getNumIncomingValues(), "geptoindexphi", phi);
		mappedPHIs.insert(std::make_pair(phi, newPHI));
		for (unsigned i=0;i<phi->getNumIncomingValues();i++)
		{
			// If incomingValue is not an instruction it must be a global pointer and the base
			Value* incomingValue=phi->getIncomingValue(i);
			phi->setIncomingValue(i, UndefValue::get(phi->getType()));
			Instruction* incomingInst=dyn_cast<Instruction>(incomingValue);
			Value* index = incomingInst ? rewrite(incomingInst, base) : NULL;
			if (index == NULL)
				index = ConstantInt::get(newPHI->getType(), 0);
			newPHI->addIncoming(index, phi->getIncomingBlock(i));
		}
		Value* newOffset = newPHI;
		if(Value* n= SimplifyInstruction(newPHI, I->getModule()->getDataLayout()))
		{
			newOffset = n;
			newPHI->replaceAllUsesWith(n);
			newPHI->eraseFromParent();
		}
		Value* newGep = NULL;
		if(isa<ConstantInt>(newOffset) && cast<ConstantInt>(newOffset)->getZExtValue()==0)
			newGep=base;
		else
			newGep=GetElementPtrInst::Create(base, newOffset, "geptoindex",phi->getParent()->getFirstInsertionPt());
		phi->replaceAllUsesWith(newGep);
		return newOffset;
	}
	return NULL;
}

bool PHIVisitor::visitPHI(PHINode* phi)
{
	Value* base = findBase(phi);
	if (base == NULL)
		return false;
	// We have found a common base for all incoming values.
	// Now we want to build an integer PHI
	rewrite(phi, base);
	return true;
}

bool PointerArithmeticToArrayIndexing::runOnFunction(Function& F)
{
	bool Changed = false;
	if (F.getSection() == StringRef("asmjs"))
		return false;

	PHIVisitor::PHIMap phiMap;
	PHIVisitor::RemoveQueue toRemove;
	for ( BasicBlock & BB : F )
	{
		for ( BasicBlock::iterator it = BB.begin(); it != BB.end(); )
		{
			PHINode * phi = dyn_cast<PHINode>(it++);
			if (! phi )
				continue;
			if (! phi->getType()->isPointerTy() )
				continue;
			assert ( phi->getNumIncomingValues() != 0 );
			// LCSSA may create PHIs with just 1 value or all equal values, kill those
			Value* uniqueVal = phi->getIncomingValue(0);
			for ( unsigned i = 1; i < phi->getNumIncomingValues(); i++)
			{
				// PHIs with as single elements are confusing for the backend, remove them
				Value* newVal = phi->getIncomingValue(i);
				if(newVal == uniqueVal)
					continue;
				uniqueVal = NULL;
				break;
			}
			if(uniqueVal)
			{
				phi->replaceAllUsesWith(uniqueVal);
				phiMap.insert(std::make_pair(phi, uniqueVal));
				Changed |= true;
				continue;
			}
			Changed |= PHIVisitor(phiMap, toRemove).visitPHI(phi);
		}
	}
	for(auto& it: phiMap)
		it.first->eraseFromParent();
	for(Instruction* I: toRemove)
		I->eraseFromParent();
	return Changed;
}

const char* PointerArithmeticToArrayIndexing::getPassName() const
{
	return "PointerArithmeticToArrayIndexing";
}

char PointerArithmeticToArrayIndexing::ID = 0;

void PointerArithmeticToArrayIndexing::getAnalysisUsage(AnalysisUsage & AU) const
{
	AU.addPreserved<cheerp::GlobalDepsAnalyzer>();
	llvm::Pass::getAnalysisUsage(AU);
}

FunctionPass *createPointerArithmeticToArrayIndexingPass() { return new PointerArithmeticToArrayIndexing(); }

void PointerToImmutablePHIRemoval::hoistBlock(BasicBlock* targetBlock)
{
	std::unordered_set<BasicBlock*> predBlocks(pred_begin(targetBlock), pred_end(targetBlock));;
	for(BasicBlock* curBlock: predBlocks)
	{
		ValueToValueMapTy valueMap;
		BasicBlock* newBlock = CloneBasicBlock(targetBlock, valueMap, "phirem", targetBlock->getParent());
		// Fix the copied block
		for(Instruction& I: *targetBlock)
		{
			Instruction* mappedI = cast<Instruction>(valueMap[&I]);
			PHINode* phi = dyn_cast<PHINode>(&I);
			if(phi)
			{
				// Override the map
				valueMap[phi] = phi->getIncomingValueForBlock(curBlock);
				mappedI->eraseFromParent();
				continue;
			}
			for(uint32_t i=0;i<I.getNumOperands();i++)
			{
				Value* oldOp = mappedI->getOperand(i);
				if(valueMap[oldOp])
					mappedI->setOperand(i, valueMap[oldOp]);
			}
		}
		// Update the terminator to go to the new block
		TerminatorInst* curTerm = curBlock->getTerminator();
		for(uint32_t j = 0; j < curTerm->getNumSuccessors(); j++)
		{
			if (curTerm->getSuccessor(j) == targetBlock)
				curTerm->setSuccessor(j, newBlock);
		}
	}
	targetBlock->eraseFromParent();
}

bool PointerToImmutablePHIRemoval::runOnFunction(Function& F)
{
	bool Changed = false;

	SmallVector<BasicBlock*, 4> blocks;
	for ( BasicBlock& BB : F )
		blocks.push_back(&BB);
	for ( BasicBlock* BB : blocks )
	{
		for ( BasicBlock::iterator it = BB->begin(); it != BB->end(); )
		{
			PHINode * phi = dyn_cast<PHINode>(it++);
			if (! phi )
				continue;
			BasicBlock* parentBlock = phi->getParent();
			if ( parentBlock->getTerminator()->getNumSuccessors() != 0 )
				continue;
			if ( parentBlock->size() > 5 )
				continue;
			hoistBlock(parentBlock);
			Changed = true;
			break;
		}
	}
	return Changed;
}

const char* PointerToImmutablePHIRemoval::getPassName() const
{
	return "PointerToImmutablePHIRemoval";
}

char PointerToImmutablePHIRemoval::ID = 0;

void PointerToImmutablePHIRemoval::getAnalysisUsage(AnalysisUsage & AU) const
{
	AU.addPreserved<cheerp::GlobalDepsAnalyzer>();
	llvm::Pass::getAnalysisUsage(AU);
}

FunctionPass *createPointerToImmutablePHIRemovalPass() { return new PointerToImmutablePHIRemoval(); }

void FreeAndDeleteRemoval::deleteInstructionAndUnusedOperands(Instruction* I)
{
	SmallVector<Instruction*, 4> operandsToErase;
	for(Value* op: I->operands())
	{
		if(Instruction* opI = dyn_cast<Instruction>(op))
		{
			if(opI->hasOneUse())
				operandsToErase.push_back(opI);
		}
	}
	I->eraseFromParent();
	for(Instruction* I: operandsToErase)
		deleteInstructionAndUnusedOperands(I);
}

bool FreeAndDeleteRemoval::runOnFunction(Function& F)
{
	bool Changed = false;

	if (F.getSection()==StringRef("asmjs"))
		return false;

	bool isAllGenericJS = true;
	for (const Function& f: *F.getParent())
	{
		if (f.getSection() == StringRef("asmjs"))
		{
			isAllGenericJS = false;
			break;
		}
	}
	for ( BasicBlock& BB : F )
	{
		for ( BasicBlock::iterator it = BB.begin(); it != BB.end(); )
		{
			CallInst * call = dyn_cast<CallInst>(it++);
			if (!call)
				continue;
			Function* F = call->getCalledFunction();
			if(!F)
				continue;
			if(cheerp::isFreeFunctionName(F->getName()) && isAllGenericJS)
			{
				deleteInstructionAndUnusedOperands(call);
				Changed = true;
			}
			else if(F->getIntrinsicID()==Intrinsic::cheerp_deallocate)
			{
				Type* ty = call->getOperand(0)->getType();
				assert(isa<PointerType>(ty));
				Type* elemTy = cast<PointerType>(ty)->getElementType();
				if (isAllGenericJS || (!cheerp::TypeSupport::isAsmJSPointer(ty) && elemTy->isAggregateType())) {
					deleteInstructionAndUnusedOperands(call);
					Changed = true;
				}
			}
		}
	}
	return Changed;
}

const char* FreeAndDeleteRemoval::getPassName() const
{
	return "FreeAndDeleteRemoval";
}

char FreeAndDeleteRemoval::ID = 0;

void FreeAndDeleteRemoval::getAnalysisUsage(AnalysisUsage & AU) const
{
	llvm::Pass::getAnalysisUsage(AU);
}

FunctionPass *createFreeAndDeleteRemovalPass() { return new FreeAndDeleteRemoval(); }

uint32_t DelayInsts::countInputInstructions(Instruction* I)
{
	uint32_t ret = 0;
	for(Value* Op: I->operands())
	{
		if(isa<Instruction>(Op))
			ret++;
	}
	return ret;
}

DelayInsts::InsertPoint DelayInsts::delayInst(Instruction* I, std::vector<std::pair<Instruction*, InsertPoint>>& movedAllocaMaps,
					LoopInfo* LI, DominatorTree* DT, std::unordered_map<Instruction*, InsertPoint>& visited, bool moveAllocas)
{
	// Do not move problematic instructions
	// TODO: Call/Invoke may be moved in some conditions
	if(I->mayReadOrWriteMemory() || I->getOpcode() == Instruction::PHI ||
		I->getOpcode() == Instruction::Call || I->getOpcode() == Instruction::Invoke ||
		I->use_empty())
	{
		return InsertPoint(I);
	}
	else if(I->getOpcode() == Instruction::Alloca && !moveAllocas)
		return InsertPoint(I);
	auto it = visited.find(I);
	if(it != visited.end())
	{
		// Already delayed
		return it->second;
	}
	// Do not delay instructions that depend on more than 1 input instruction
	// Delyaing those may increase the amount of live variables
	// TODO: This is simplistic, we should use isInline to know how many actual registers
	//       are used by this instructions. We need to rework caching in PA otherwise it is too slow.
	if(countInputInstructions(I) > 1)
	{
		InsertPoint ret(I);
		visited.insert(std::make_pair(I, ret));
		return ret;
	}

	// Delay the alloca as much as possible by putting it in the dominator block of all the uses
	// Unless that block is in a loop, then put it above the loop
	// Instead of the actual user we use to insertion point after it is delayed
	InsertPoint finalInsertPoint(nullptr, nullptr, nullptr);
	bool firstUser = true;
	for(User* U: I->users())
	{
		InsertPoint insertPoint = delayInst(cast<Instruction>(U), movedAllocaMaps, LI, DT, visited, moveAllocas);
		// Deal with potential forward block terminators
		// It is safe to use them on the first user or if it is always the same
		finalInsertPoint.insertInst = cheerp::findCommonInsertionPoint(I, DT, finalInsertPoint.insertInst, insertPoint.insertInst);
		if(firstUser)
		{
			finalInsertPoint.source = insertPoint.source;
			finalInsertPoint.target = insertPoint.target;
			firstUser = false;
		}
		else if(finalInsertPoint.source != insertPoint.source ||
			finalInsertPoint.target != insertPoint.target)
		{
			finalInsertPoint.source = nullptr;
			finalInsertPoint.target = nullptr;
		}
	}
	// Never sink an instruction in an inner loop
	// Special case Allocas, we really want to put them outside of loops
	Loop* initialLoop = I->getOpcode() == Instruction::Alloca ? nullptr : LI->getLoopFor(I->getParent());
	Loop* loop = LI->getLoopFor(finalInsertPoint.insertInst->getParent());
	// If loop is now NULL we managed to move the instruction outside of any loop. Good.
	if(loop && loop != initialLoop)
	{
		// The new insert point is in a loop, but not in the previous one
		// Check if the new loop is an inner loop
		while(loop)
		{
			Loop* parentLoop = loop->getParentLoop();
			if(parentLoop == initialLoop)
				break;
			loop = parentLoop;
		}
		if(loop)
		{
			BasicBlock* loopHeader = loop->getHeader();
			// We need to put the instruction in the dominator of the loop, not in the loop header itself
			BasicBlock* loopDominator = NULL;
			// It may be convenient to put the instruction into a new loop pre-header
			// Do that if there is only 1 forward edge and it has a conditional branch
			bool createForwardBlock = true;
			for(auto it = pred_begin(loopHeader);it != pred_end(loopHeader); ++it)
			{
				// Skip all backedges
				if(loopHeader == *it || DT->dominates(loopHeader, *it))
					continue;
				if(!loopDominator)
					loopDominator = *it;
				else if(DT->dominates(loopDominator, *it))
					createForwardBlock = false;
				else if(DT->dominates(*it, loopDominator))
				{
					createForwardBlock = false;
					loopDominator = *it;
				}
				else // Find a common dominator
				{
					createForwardBlock = false;
					loopDominator = DT->findNearestCommonDominator(loopDominator, *it);
				}
			}
			if(createForwardBlock && loopDominator->getTerminator()->getNumSuccessors()>1)
			{
				finalInsertPoint.source = loopDominator;
				finalInsertPoint.target = loopHeader;
			}
			finalInsertPoint.insertInst = loopDominator->getTerminator();
		}
	}
	movedAllocaMaps.emplace_back(I, finalInsertPoint);
	visited.insert(std::make_pair(I, finalInsertPoint));
	return finalInsertPoint;
}

bool DelayInsts::runOnFunction(Function& F)
{
	// Only move allocas on genericjs
	bool moveAllocas = F.getSection()==StringRef("");
	bool Changed = false;
	bool allocaInvalidated = false;
	LoopInfo* LI = &getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
	DominatorTree* DT = &getAnalysis<DominatorTreeWrapperPass>().getDomTree();
	cheerp::Registerize& registerize = getAnalysis<cheerp::Registerize>();

	std::unordered_map<Instruction*, InsertPoint> visited;
	std::vector<std::pair<Instruction*, InsertPoint>> movedAllocaMaps;
	for ( BasicBlock& BB : F )
	{
		for ( BasicBlock::iterator it = BB.begin(); it != BB.end(); ++it)
		{
			Instruction* I = it;
			InsertPoint insertPoint = delayInst(I, movedAllocaMaps, LI, DT, visited, moveAllocas);
			if(insertPoint.insertInst == I)
				continue;
			else if(insertPoint.source == nullptr && insertPoint.insertInst == I->getNextNode())
				continue;
			if(moveAllocas && !allocaInvalidated && I->getOpcode() == Instruction::Alloca)
			{
				registerize.invalidateLiveRangeForAllocas(F);
				allocaInvalidated = true;
			}
			Changed = true;
		}
	}
	if(!Changed)
		return false;
	// Create forward blocks as required, unique them based on the source/target
	std::map<std::pair<llvm::BasicBlock*, llvm::BasicBlock*>, llvm::BasicBlock*> forwardBlocks;
	for(auto it = movedAllocaMaps.rbegin(); it != movedAllocaMaps.rend(); ++it)
	{
		if(it->second.source)
		{
			BasicBlock* source = it->second.source;
			BasicBlock* target = it->second.target;
			assert(target);
			auto fwd = forwardBlocks.find(std::make_pair(source, target));
			if(fwd == forwardBlocks.end())
			{
				BasicBlock* newB = BasicBlock::Create(F.getContext(), "delayFwd", &F);
				BranchInst::Create(target, newB);
				TerminatorInst* sourceTerm = source->getTerminator();
				for(unsigned i=0;i<sourceTerm->getNumSuccessors();i++)
				{
					if(sourceTerm->getSuccessor(i) == target)
						sourceTerm->setSuccessor(i, newB);
				}
				for(Instruction& targetI: *target)
				{
					PHINode* phi = dyn_cast<PHINode>(&targetI);
					if(!phi)
						break;
					for(unsigned i=0;i<phi->getNumIncomingValues();i++)
					{
						if(phi->getIncomingBlock(i) == source)
							phi->setIncomingBlock(i, newB);
					}
				}
				fwd = forwardBlocks.insert(std::make_pair(std::make_pair(source, target), newB)).first;
			}
			it->first->moveBefore(fwd->second->getTerminator());
		}
		else
			it->first->moveBefore(it->second.insertInst);
	}
	if(allocaInvalidated)
		registerize.computeLiveRangeForAllocas(F);
	return Changed;
}

const char* DelayInsts::getPassName() const
{
	return "DelayInsts";
}

char DelayInsts::ID = 0;

void DelayInsts::getAnalysisUsage(AnalysisUsage & AU) const
{
	AU.addPreserved<cheerp::PointerAnalyzer>();
	AU.addPreserved<cheerp::Registerize>();
	AU.addPreserved<cheerp::GlobalDepsAnalyzer>();
	AU.addRequired<DominatorTreeWrapperPass>();
	AU.addRequired<cheerp::Registerize>();
	AU.addRequired<LoopInfoWrapperPass>();
	AU.addPreserved<LoopInfoWrapperPass>();
	llvm::Pass::getAnalysisUsage(AU);
}

FunctionPass *createDelayInstsPass() { return new DelayInsts(); }

Value* GEPOptimizer::GEPRecursionData::getValueNthOperator(const OrderedGEPs::iterator it, const uint32_t index) const
{
	if (index + 1 == (*it)->getNumOperands())
		return NULL;
	else if (index < (*it)->getNumOperands())
		return (*it)->getOperand(index);
	assert(false);
	return NULL;
}

void GEPOptimizer::GEPRecursionData::optimizeGEPsRecursive(OrderedGEPs::iterator begin, const OrderedGEPs::iterator end,
		Value* base, const uint32_t endIndex)
{
	//TODO: Avoid to optimize GEPs with only 2 operands
	assert(begin != end);
	// We know that up to startIndex all indexes are equal
	// Find out how many indexes are equal in all the range and build a GEP with them from the base
	llvm::SmallVector<llvm::Value*, 4> newIndexes;
	if(endIndex == 2)
	{
		// It means that we are handling the first index, add it as it is
		// The first index is guaranteed to be the same across the range
		newIndexes.push_back((*begin)->getOperand(1));
	}
	else
	{
		// This is an optimized GEP, it will start from the passed base. Add a constant 0.
		newIndexes.push_back(ConstantInt::get(llvm::Type::getInt32Ty(base->getContext()), 0));
	}

	while (begin != end)
	{
		//Initialize range
		OrderedGEPs::iterator it = begin;
		Value* referenceOperand = getValueNthOperator(it, endIndex);
		uint32_t rangeSize = 1;
		it++;

		if (referenceOperand != NULL)
		{
			while(it!=end && referenceOperand == getValueNthOperator(it, endIndex))
			{
				rangeSize++;
				it++;
			}
		}
		// This is the first index which is different in the range.
#if DEBUG_GEP_OPT_VERBOSE
		llvm::errs() << "Index equal until " << endIndex << ", range size: " << rangeSize << "\n";
#endif
		if(rangeSize == 1)
		{
			// If the range has size 1 and we have just started we skip this GEP entirely
			if(endIndex > 2)
			{
				uint32_t oldIndexCount = newIndexes.size();
				// Create a GEP from the base to all not used yet indexes
				for(uint32_t i=endIndex;i<(*begin)->getNumOperands();i++)
					newIndexes.push_back((*begin)->getOperand(i));
				if(newIndexes.size() > 1)
				{
					Instruction* newGEP = GetElementPtrInst::Create(base, newIndexes, "", *begin);
#if DEBUG_GEP_OPT_VERBOSE
					llvm::errs() << "New GEP " << newGEP << "\n";
#endif
					erasedInst.insert(std::make_pair(*begin, newGEP));
				}
				newIndexes.resize(oldIndexCount);

			}
			// Reset the state for the next range
			begin = it;
			continue;
		}
		// Compute the insertion point to dominate all users of this GEP in the range
		Instruction* insertionPoint = findInsertionPoint(begin, it, endIndex);

		newIndexes.push_back(referenceOperand);
		// This is the first index which is different in the range. Create a GEP now.
		GetElementPtrInst* newGEP = GetElementPtrInst::Create(base, newIndexes, base->getName()+".optgep");
		newIndexes.pop_back();
		newGEP->insertBefore(insertionPoint);
		// Will be later examined, checking wheter it should be merged to its single use (or kept if there are multiple uses)
		nonTerminalGeps.push_back(newGEP);

		assert(insertionPoint);
#if DEBUG_GEP_OPT_VERBOSE
		llvm::errs() << "New GEP " << newGEP << " inserted after " << *insertionPoint << "\n";
#endif
		if(rangeSize != 1)
		{
			// Proceed with the range from 'begin' to 'it'
			// NOTE: insertPointLimit is not passed to later steps, it is only needed when dealing with the base
			optimizeGEPsRecursive(begin, it, newGEP, endIndex+1);
		}
		// Reset the state for the next range
		begin = it;
	}
}

Instruction* GEPOptimizer::GEPRecursionData::findInsertionPoint(const OrderedGEPs::iterator begin, const OrderedGEPs::iterator end, const uint32_t endIndex)
{
	Instruction* insertionPoint = NULL;

	for(OrderedGEPs::iterator it = begin; it != end;)
	{
		//We need to hold an iterator both to the current and the next item
		//to be able to delete the current item without invalidating iterators
		OrderedGEPs::iterator currIt = it;
		Instruction* curGEP = *it;
		++it;
		if(insertionPoint==NULL)
		{
			insertionPoint = curGEP;
			continue;
		}
		// Check if the current GEP dominates the old insertionPoint
		if(DT->dominates(curGEP, insertionPoint))
			insertionPoint = curGEP;
		else if(!DT->dominates(insertionPoint, curGEP))
		{
			// If the insertionPoint also does not dominate the current GEP
			// we need to find a common dominator for both
			BasicBlock* commonDominator = DT->findNearestCommonDominator(insertionPoint->getParent(), curGEP->getParent());
			assert(commonDominator);
			llvm::Instruction* insertPointCandidate = commonDominator->getTerminator();
			// Make sure that insertPointCandidate is in a valid block for this GEP
			const ValidGEPLocations& validGEPLocations = validGEPMap.at(GEPRange::createGEPRange(cast<const GetElementPtrInst>(curGEP),endIndex+1));
			if (!validGEPLocations.count(commonDominator))
			{
				// It is not safe to optimize this GEP, remove it from the set
				assert(curGEP != *begin);
				skippedGeps.insert(curGEP);
				orderedGeps.erase(currIt);
#if DEBUG_GEP_OPT_VERBOSE
				llvm::errs() << "Skipping GEP " << *curGEP << "\n";
#endif
			}
			else
			{
				insertionPoint = insertPointCandidate;
			}
		}
	}
	return insertionPoint;
}


template <> struct GraphTraits<GEPOptimizer::ValidGEPGraph::Node*> {
	typedef GEPOptimizer::ValidGEPGraph::Node NodeType;
	typedef GEPOptimizer::ValidGEPGraph::SuccIterator ChildIteratorType;

	static NodeType *getEntryNode(NodeType* N) { return N; }
	static inline ChildIteratorType child_begin(NodeType *N) {
		return ChildIteratorType(N);
	}
	static inline ChildIteratorType child_end(NodeType *N) {
		return ChildIteratorType(N, true);
	}
};
template <> struct GraphTraits<Inverse<GEPOptimizer::ValidGEPGraph::Node*>> {
	typedef GEPOptimizer::ValidGEPGraph::Node NodeType;
	typedef GEPOptimizer::ValidGEPGraph::PredIterator ChildIteratorType;

	static NodeType *getEntryNode(NodeType* N) { return N; }
	static inline ChildIteratorType child_begin(NodeType *N) {
		return ChildIteratorType(N);
	}
	static inline ChildIteratorType child_end(NodeType *N) {
		return ChildIteratorType(N, true);
	}
};
template <> struct GraphTraits<GEPOptimizer::ValidGEPGraph*> : public GraphTraits<GEPOptimizer::ValidGEPGraph::Node*> {
	static NodeType *getEntryNode(GEPOptimizer::ValidGEPGraph *G) { return G->getEntryNode(); }

	typedef mapped_iterator<
		GEPOptimizer::ValidGEPGraph::NodeMap::iterator,
		std::function<GEPOptimizer::ValidGEPGraph::Node*(GEPOptimizer::ValidGEPGraph::NodeMap::iterator::reference)>
	> mapped_iterator_type;
	struct deref_mapped_iterator: public mapped_iterator_type {
		using mapped_iterator_type::mapped_iterator;
		operator NodeType*()
		{
			return **this;
		}
	};
	static NodeType* get_second_ptr(GEPOptimizer::ValidGEPGraph::NodeMap::iterator::reference pair)
	{
		return &pair.second;
	}
	 // nodes_iterator/begin/end - Allow iteration over all nodes in the graph
	typedef deref_mapped_iterator nodes_iterator;
	static nodes_iterator nodes_begin(GEPOptimizer::ValidGEPGraph* G) { return nodes_iterator(G->Nodes.begin(), get_second_ptr); }
	static nodes_iterator nodes_end  (GEPOptimizer::ValidGEPGraph* G) { return nodes_iterator(G->Nodes.end(), get_second_ptr); }
	static size_t         size       (GEPOptimizer::ValidGEPGraph* G) { return G->Nodes.size(); }
};

void GEPOptimizer::ValidGEPGraph::getValidBlocks(ValidGEPLocations& validGEPLocations)
{
	DominatorTreeBase<Node> PDT(true);
	PDT.recalculate(*this);
	SmallVector<ValidGEPGraph::Node*, 8> ValidNodes;
	PDT.getDescendants(getOrCreate(nullptr), ValidNodes);
	for (auto V: ValidNodes)
	{
		if (V->BB)
		{
			validGEPLocations.insert(V->BB);
		}
	}
}

void GEPOptimizer::GEPRecursionData::keepOnlyDominated(ValidGEPLocations& blocks, const Value* value)
{
	if (const Instruction* I = dyn_cast<const Instruction> (value))
	{
		blocks.keepOnlyDominated(I->getParent());
	}
}



GEPOptimizer::GEPRecursionData::GEPRecursionData(Function &F, DominatorTree* DT) :
		order(),
		orderedGeps(OrderByOperands(&order)),
		skippedGeps(OrderByOperands(&order)),
		DT(DT)
{
	//order will be used to compare two Instruction. Inserting NULL here means
	//shorter subset will appear first (like in a dictionary, CAT < CATS)
	//This is required by GEPRecursionData::optimizeGEPsRecursive
	order.insert({NULL, 0});

	// Gather all the GEPs
	ValidGEPLocations AllBlocks;
	AllBlocks.insert(&F.getEntryBlock());
	AllBlocks.setDominatorTree(DT);
	for ( BasicBlock& BB : F )
	{
		for ( Instruction& I: BB )
		{
			if(!isa<GetElementPtrInst>(I))
				continue;
			// Only consider GEPs with at least  two indexes
			if(I.getNumOperands() < 3)
				continue;
			const GetElementPtrInst* GEP = cast<GetElementPtrInst>(&I);
			for (size_t i = 0; i < GEP->getNumOperands(); ++i)
			{
				order.insert({GEP->getOperand(i), order.size()});
			}

			orderedGeps.insert(&I);
			ValidGEPLocations validBlocks = AllBlocks;

			// NOTE: `i` is a size, so the end condition needs <=
			for (size_t i = 2; i <= GEP->getNumOperands(); ++i)
			{
				GEPRange Range = GEPRange::createGEPRange(GEP, i);
				auto it = validGEPMap.find(Range);
				if(it == validGEPMap.end())
				{
					ValidGEPGraph VG(&F, DT, Range, validBlocks);
					//ValidBlocks contains all the blocks that:
					//1- either leads from every possibly path to a node using a GEP
					//2- or thery are a child to such a node (in the DT)
					//They are represented as a forest (a set of (sub-)trees) in the dominator tree
					//Since property 2 guarantees that the children of a valid nodes are still valid,
					//we can keep only nodes that are roots to a sub-tree in the dominator tree
					validBlocks.clear();
					VG.getValidBlocks(validBlocks);
					validBlocks.simplify();
					//Here we introduce the additional constraint that valid blocks have to be
					//dominated by every instruction used inside the GEP
					//Since basically we are doing set intersection between the original forest and a new
					//(sub-)tree in the DT, the result will still be a forest of subtree after every
					//application of keepOnlyDominated
					//TODO: find how it is possible to avoid the O(GEP->length ^ 2) iterations (do DFS starting from the various roots instead of the whole tree)
					for (size_t j = 0; j < i; j++)
					{
						keepOnlyDominated(validBlocks, GEP->getOperand(j));
					}
					it = validGEPMap.emplace(Range, std::move(validBlocks)).first;
				}
				validBlocks = it->second;
			}
		}
	}

}


bool GEPOptimizer::runOnFunction(Function& F)
{
	DT = &getAnalysis<DominatorTreeWrapperPass>().getDomTree();

	GEPRecursionData data(F, DT);

	data.startRecursion();
	data.applyOptGEP();
	data.mergeSingleUserGEPs();

	return data.anyChange();
}

// Look for GEPs that have a common base pointer. They should have both the
// pointer and the first index equal. As the GEPs in the map are ordered we
// know that equal objects are close.

void GEPOptimizer::GEPRecursionData::startRecursion()
{
	uint32_t rangeLength = 0;
	auto it=orderedGeps.begin();
	OrderedGEPs::iterator rangeStart = it;
	while (it != orderedGeps.end())
	{
		// Check that the first two operands are the same
		while (it != orderedGeps.end() &&
			(*rangeStart)->getOperand(0) == (*it)->getOperand(0) &&
			(*rangeStart)->getOperand(1) == (*it)->getOperand(1))
		{
			rangeLength++;
			++it;
		}

		// End the range here, if the range is longer than 1 we can optimize some GEPs
		if(rangeLength > 1)
		{
#if DEBUG_GEP_OPT_VERBOSE
			llvm::errs() << "Common GEP range:";
			for(auto it2=rangeStart;it2!=it;++it2)
				llvm::errs() << **it2 << "\n";
			llvm::errs() << "\n";
#endif
			optimizeGEPsRecursive(rangeStart, it, (*rangeStart)->getOperand(0), 2);
		}
		rangeLength = 0;
		rangeStart = it;
	}

	if (!skippedGeps.empty())
	{
		swap(skippedGeps, orderedGeps);
		skippedGeps.clear();
		startRecursion();
	}
}

void GEPOptimizer::GEPRecursionData::applyOptGEP()
{
	for(auto p: erasedInst)
	{
		p.second->takeName(p.first);
		p.first->replaceAllUsesWith(p.second);
		p.first->eraseFromParent();
	}
}

void GEPOptimizer::GEPRecursionData::mergeSingleUserGEPs()
{
	//nonTerminalGeps holds inner nodes in the GEP tree
	//A child node always appear after its parent (by construction)
	//nonTerminalGeps is consumed backward, since otherwise we may modify the children of
	//the current node, and we need to avoid invalidating values still inside the vector
	while (!nonTerminalGeps.empty())
	{
		GetElementPtrInst* a = nonTerminalGeps.back();
		//Delete the current value, it may be invalidated
		nonTerminalGeps.pop_back();

		//Check whether it has only 1 user
		if (!a->hasOneUse())
			continue;

		//Check whether that user is a GEP
		GetElementPtrInst* b = cast<GetElementPtrInst>(*a->user_begin());

		llvm::SmallVector<llvm::Value*, 4> indexes;
		auto iter = a->idx_begin();
		while (iter != a->idx_end())
		{
			indexes.push_back(*iter);
			++iter;
		}
		iter = b->idx_begin();

		//The first index of a gep with depth > 0 has to be 0
		assert(cast<ConstantInt>(*iter)->equalsInt(0));

		++iter;
		while (iter != b->idx_end())
		{
			indexes.push_back(*iter);
			++iter;
		}

		GetElementPtrInst* newGEP = GetElementPtrInst::Create(a->getPointerOperand(), indexes, b->getName()+".optgepsqueezed");
		newGEP->insertBefore(b);
		b->replaceAllUsesWith(newGEP);
		b->eraseFromParent();
		a->eraseFromParent();
	}
}

const char* GEPOptimizer::getPassName() const
{
	return "GEPOptimizer";
}

char GEPOptimizer::ID = 0;

void GEPOptimizer::getAnalysisUsage(AnalysisUsage & AU) const
{
	AU.addRequired<DominatorTreeWrapperPass>();
	AU.addPreserved<cheerp::GlobalDepsAnalyzer>();
	llvm::Pass::getAnalysisUsage(AU);
}

FunctionPass *createGEPOptimizerPass() { return new GEPOptimizer(); }

}

using namespace llvm;

INITIALIZE_PASS_BEGIN(AllocaArrays, "AllocaArrays", "Transform allocas of REGULAR type to arrays of 1 element",
			false, false)
INITIALIZE_PASS_END(AllocaArrays, "AllocaArrays", "Transform allocas of REGULAR type to arrays of 1 element",
			false, false)

INITIALIZE_PASS_BEGIN(DelayInsts, "DelayInsts", "Moves instructions as close as possible to the actual users",
			false, false)
INITIALIZE_PASS_END(DelayInsts, "DelayInsts", "Moves instrucitions as close as possible to the actual users",
			false, false)

INITIALIZE_PASS_BEGIN(FreeAndDeleteRemoval, "FreeAndDeleteRemoval", "Remove free and delete calls of genericjs objects",
			false, false)
INITIALIZE_PASS_END(FreeAndDeleteRemoval, "FreeAndDeleteRemoval", "Remove free and delete calls of genericjs objects",
			false, false)

INITIALIZE_PASS_BEGIN(GEPOptimizer, "GEPOptimizer", "Rewrite GEPs in a function to remove redundant object accesses",
			false, false)
INITIALIZE_PASS_END(GEPOptimizer, "GEPOptimizer", "Rewrite GEPs in a function to remove redundant object accesses",
			false, false)
