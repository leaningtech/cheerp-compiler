//===-- PointerPasses.cpp - Cheerp pointer optimization passes --------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014-2019 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "CheerpPointerPasses"
#include "llvm/Analysis/InstructionSimplify.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Cheerp/DeterministicPtrSet.h"
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
#include "llvm/Support/raw_ostream.h"
#include <set>
#include <map>

STATISTIC(NumIndirectFun, "Number of indirect functions processed");
STATISTIC(NumAllocasTransformedToArrays, "Number of allocas of values transformed to allocas of arrays");

namespace llvm {

bool AllocaArrays::replaceAlloca(AllocaInst* ai, cheerp::GlobalDepsAnalyzer& gda)
{
	const ConstantInt * ci = dyn_cast<ConstantInt>(ai->getArraySize());
	
	// Runtime alloca size, convert it to cheerp_allocate 
	if (!ci)
	{
		Module* M = ai->getParent()->getParent()->getParent();
		DataLayout targetData(M);
		Type* int32Ty = IntegerType::getInt32Ty(M->getContext());
		Type* allocTy = ai->getAllocatedType();
		gda.visitDynSizedAlloca(allocTy);
		Function* cheerp_allocate = Intrinsic::getDeclaration(M, Intrinsic::cheerp_allocate, ai->getType());

		IRBuilder<> Builder(ai);

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
			
			GetElementPtrInst * newGep = GetElementPtrInst::Create(at, newAi, vals);
			ReplaceInstWithInst(gep, newGep);
			newGep->takeName( gep );
		}
		else
		{
			if (! gepZero )
			{
				SmallVector< Value *, 8 > vals ( 2, ConstantInt::getNullValue( llvm::Type::getInt32Ty( u->getContext() ) ) );

				gepZero = GetElementPtrInst::Create(at, newAi, vals, "");
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
	if (F.getSection()==StringRef("asmjs"))
		return false;

	bool Changed = false;
	cheerp::PointerAnalyzer & PA = getAnalysis<cheerp::PointerAnalyzer>();
	cheerp::Registerize & registerize = getAnalysis<cheerp::Registerize>();
	cheerp::GlobalDepsAnalyzer & globalDeps= getAnalysis<cheerp::GlobalDepsAnalyzer>();

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
			Changed |= replaceAlloca( ai, globalDeps );
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
	AU.addRequired<cheerp::GlobalDepsAnalyzer>();
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
					Value* newGep=GetElementPtrInst::Create(gep->getSourceElementType(), base, newIndex, "geptoindex", gep);
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
			newGep=GetElementPtrInst::Create(base->getType()->getPointerElementType(), base, newOffset, "geptoindex",phi->getParent()->getFirstInsertionPt());
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

	PHIVisitor::PHIMap phiMap;
	PHIVisitor::RemoveQueue toRemove;
	for ( BasicBlock & BB : F )
	{
		for ( BasicBlock::iterator it = BB.begin(); it != BB.end(); )
		{
			PHINode * phi = dyn_cast<PHINode>(it++);
			if (! phi )
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
			else if (! phi->getType()->isPointerTy() )
				continue;
			else if (F.getSection() == StringRef("asmjs"))
				continue;
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
	cheerp::DeterministicPtrSet<BasicBlock*> predBlocks(pred_begin(targetBlock), pred_end(targetBlock));
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
		// TODO: This should be another pass
		if(BranchInst* BI = dyn_cast<BranchInst>(BB->getTerminator()))
		{
			if(BI->isConditional())
			{
				if(FCmpInst* FC = dyn_cast<FCmpInst>(BI->getCondition()))
				{
					if(CmpInst::isUnordered(FC->getPredicate()) && FC->getNumUses() == 1)
					{
						// Invert the condition and swap the targets
						FC->setPredicate(FC->getInversePredicate());
						BI->swapSuccessors();
						Changed = true;
					}
				}
			}
		}
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

uint32_t DelayInsts::countInputRegisters(Instruction* I, const cheerp::PointerAnalyzer& PA)
{
	uint32_t count = 0;
	for(Value* Op: I->operands())
	{
		Instruction* opI = dyn_cast<Instruction>(Op);
		if(!opI)
			continue;
		if(isInlineable(*opI, PA))
			count += countInputRegisters(opI, PA);
		else
			count += 1;
		if(count >= 2)
			break;
	}
	return count;
}

DelayInsts::InsertPoint DelayInsts::delayInst(Instruction* I, std::vector<std::pair<Instruction*, InsertPoint>>& movedAllocaMaps,
					LoopInfo* LI, DominatorTree* DT, const DominatorTreeBase<BasicBlock>* PDT, const cheerp::PointerAnalyzer& PA, std::unordered_map<Instruction*, InsertPoint>& visited, bool moveAllocas)
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
	// Do not delay instructions that depend on more than 1 input register
	// Delaying those may increase the amount of live variables
	if(countInputRegisters(I, PA) >= 2)
	{
		InsertPoint ret(I);
		visited.insert(std::make_pair(I, ret));
		return ret;
	}

	// Delay the alloca as much as possible by putting it in the dominator block of all the uses
	// Unless that block is in a loop, then put it above the loop
	// Instead of the actual user we use to insertion point after it is delayed
	std::vector<InsertPoint> insertPoints;
	std::set<const User*> processedUsers;

	//Collect the insertion points for the different users
	for (User* U: I->users())
	{
		if (processedUsers.count(U))
			continue;
		processedUsers.insert(U);

		InsertPoint insertPoint = delayInst(cast<Instruction>(U), movedAllocaMaps, LI, DT, PDT, PA, visited, moveAllocas);
		if (PHINode* phi = dyn_cast<PHINode>(U))
		{
			insertPoint.target = phi->getParent();
			for(unsigned i = 0; i < phi->getNumIncomingValues(); i++)
			{
				if(phi->getIncomingValue(i) != I)
					continue;
				insertPoint.source = phi->getIncomingBlock(i);
				insertPoint.insertInst = insertPoint.source->getTerminator();
				insertPoints.push_back(insertPoint);
			}
		}
		else
			insertPoints.push_back(insertPoint);
	}

	InsertPoint finalInsertPoint = insertPoints.front();
	//Iterate over the collected insertPoints, keeping finalInsertPoint updated
	for(InsertPoint insertPoint : insertPoints)
	{
		//If they are equal, it's good for sure (this cover also the case of equal forward blocks)
		if (insertPoint == finalInsertPoint)
			continue;

		finalInsertPoint.insertInst = cheerp::findCommonInsertionPoint(I, DT, finalInsertPoint.insertInst, insertPoint.insertInst);

		// Deal with potential forward block terminators
		if (insertPoint.fwdBlockDominatesInsertionPoint(finalInsertPoint, DT, PDT))
		{
			// It is safe to use them if the inserted forward block dominates the current
			finalInsertPoint = insertPoint;
		}
		else if (finalInsertPoint.fwdBlockDominatesInsertionPoint(insertPoint, DT, PDT))
		{
			//Or the current dominates the inserted, so no need to update anything
		}
		else
		{
			//Otherwise no forward block works for this subset
			finalInsertPoint.source = nullptr;
			finalInsertPoint.target = nullptr;
		}
	}

	//Find the loop our finalInsertPoint is in
	const Loop* loop;
	if (finalInsertPoint.source)
		loop = cheerp::findCommonLoop(LI, finalInsertPoint.source, finalInsertPoint.target);
	else
		loop = LI->getLoopFor(finalInsertPoint.insertInst->getParent());
	cheerp::LoopWithDepth current(loop);

	// Never sink an instruction in an inner loop
	// Special case Allocas, we really want to put them outside of loops
	bool isAlloca = I->getOpcode() == Instruction::Alloca;
	cheerp::LoopWithDepth original(isAlloca ?
				nullptr :
				LI->getLoopFor(I->getParent()));
	while (original.depth > current.depth)
	{
		original.stepBack();
	}

	// If loop is now NULL we managed to move the instruction outside of any loop. Good.
	if(current.loop && current.loop != original.loop)
	{
		// The new insert point is in a loop, but not in the same of the initial instruction location
		// Check if the new loop is an inner loop
		while(current.loop)
		{
			if (original.depth == current.depth)
				original.stepBack();

			Loop* parentLoop = current.loop->getParentLoop();
			if(!parentLoop || parentLoop == original.loop)
				break;

			current.loop = parentLoop;
			--current.depth;
		}
		loop = current.loop;
		assert(loop && "We should still have a valid loop");

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
		if (loopDominator)
			finalInsertPoint.insertInst = loopDominator->getTerminator();

		if (loopDominator && createForwardBlock)
		{
			finalInsertPoint.source = loopDominator;
			finalInsertPoint.target = loopHeader;
		}
		else
		{
			finalInsertPoint.target = nullptr;
			finalInsertPoint.source = nullptr;
		}
	}

	if(finalInsertPoint.source && finalInsertPoint.source->getTerminator()->getNumSuccessors()==1)
	{
		//Do not create an ad-hoc block if the source has a single successor
		finalInsertPoint.insertInst = finalInsertPoint.source->getTerminator();
		finalInsertPoint.source = nullptr;
		finalInsertPoint.target = nullptr;
	}

	assert(isAlloca || I == finalInsertPoint.insertInst || DT->dominates(I, finalInsertPoint.insertInst));
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
	cheerp::PointerAnalyzer& PA = getAnalysis<cheerp::PointerAnalyzer>();

	DominatorTreeBase<BasicBlock> PDT(true);
	PDT.recalculate(F);
	assert(PDT.isPostDominator());

	std::unordered_map<Instruction*, InsertPoint> visited;
	std::vector<std::pair<Instruction*, InsertPoint>> movedAllocaMaps;
	for ( BasicBlock& BB : F )
	{
		for ( BasicBlock::iterator it = BB.begin(); it != BB.end(); ++it)
		{
			Instruction* I = &*it;
			InsertPoint insertPoint = delayInst(I, movedAllocaMaps, LI, DT, &PDT, PA, visited, moveAllocas);
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
	AU.addRequired<cheerp::PointerAnalyzer>();
	AU.addRequired<cheerp::Registerize>();
	AU.addRequired<LoopInfoWrapperPass>();
	AU.addPreserved<LoopInfoWrapperPass>();
	llvm::Pass::getAnalysisUsage(AU);
}

FunctionPass *createDelayInstsPass() { return new DelayInsts(); }

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
