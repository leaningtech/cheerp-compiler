//===-- IdenticalCodeFolding.cpp - Remove duplicate functions/globals -----===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017-2020 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "IdenticalCodeFolding"
#include "llvm/InitializePasses.h"
#include "llvm/Cheerp/IdenticalCodeFolding.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#include <unordered_map>

//#define DEBUG_VERBOSE 1

using namespace llvm;

namespace cheerp {

using namespace std;

char IdenticalCodeFolding::ID = 0;

StringRef IdenticalCodeFolding::getPassName() const
{
	return "IdenticalCodeFolding";
}

IdenticalCodeFolding::IdenticalCodeFolding() : ModulePass(ID)
{
}

void IdenticalCodeFolding::getAnalysisUsage(AnalysisUsage& AU) const
{
	AU.addPreserved<Registerize>();
	AU.addPreserved<GlobalDepsAnalyzer>();
	AU.addRequired<cheerp::GlobalDepsAnalyzer>();

	ModulePass::getAnalysisUsage(AU);
}

// This function is based on
// https://github.com/Microsoft/llvm/commit/6470102728c661adf204d0e361d30faa0a95667f
uint64_t IdenticalCodeFolding::hashFunction(llvm::Function& F)
{
	HashAccumulator64 hash;

	hash.add(F.isVarArg());
	hash.add(F.arg_size());

	SmallVector<BasicBlock*, 8> blocks;
	SmallSet<BasicBlock*, 16> visited;

	// Walk the blocks in the same order as equivalentFunction(), accumulating
	// the hash of the function "structure." (basic block and opcode sequence)
	blocks.push_back(&F.getEntryBlock());
	visited.insert(blocks[0]);
	while (!blocks.empty()) {
		const BasicBlock *BB = blocks.pop_back_val();

		// This random value acts as a block header, as otherwise the partition
		// of opcodes into blocks wouldn't affect the hash, only the order of
		// the opcodes.
		hash.add(45798);

		for (auto &Inst : *BB) {
			if (ignoreInstruction(&Inst))
				continue;
			hash.add(Inst.getOpcode());

			// In order to reduce the number of possible matches, hash the
			// GEP's constant integer indices.
			if (auto gep = dyn_cast<GetElementPtrInst>(&Inst)) {
				hash.add(gep->getNumIndices());
				if (!gep->hasAllConstantIndices())
					continue;
				for (unsigned i = 0; i < gep->getNumIndices(); i++)
					hash.add(cast<ConstantInt>(gep->idx_begin() + i)->getZExtValue());
			}
		}

		const Instruction *Term = BB->getTerminator();
		for (unsigned i = 0, e = Term->getNumSuccessors(); i != e; ++i) {
			if (!visited.insert(Term->getSuccessor(i)).second)
				continue;
			blocks.push_back(Term->getSuccessor(i));
		}
	}

	return hash.getHash();
}

bool IdenticalCodeFolding::equivalentFunction(const llvm::Function* A, const llvm::Function* B)
{
#if DEBUG_VERBOSE
	llvm::errs() << "function A: " << A->getName() << '\n';
	llvm::errs() << "function B: " << B->getName() << '\n';
#endif

	// Do not fold wasm/asmjs with generic JS functions.
	if (A->getSection() != StringRef("asmjs") || B->getSection() != StringRef("asmjs"))
		return false;

	// Mark the function pair as equivalent to deal with recursion. The
	// right equivalence value is set when |equivalentFunction()| returns.
	functionEquivalence[{A, B}] = true;

	if (!A || !B || A->isVarArg() != B->isVarArg() || A->arg_size() != B->arg_size())
		return false;

	// Do not fold functions that have inequivalent function parameter types.
	for (auto a = A->arg_begin(), b = B->arg_begin(); a != A->arg_end(); ++a, ++b) {
		if (!equivalentType(a->getType(), b->getType()))
			return false;
	}

	if (A->empty() || B->empty())
		return A->empty() == B->empty();

	SmallVector<std::pair<const BasicBlock*, const BasicBlock*>, 8> blocks;
	SmallSet<const BasicBlock*, 16> visited;

	// Walk the blocks in the same order as hashFunction().
	blocks.push_back({&A->getEntryBlock(), &B->getEntryBlock()});
	visited.insert(&A->getEntryBlock());
	while (!blocks.empty()) {
		const auto& pair = blocks.pop_back_val();
		const BasicBlock* blockA = pair.first;
		const BasicBlock* blockB = pair.second;

		if (!equivalentBlock(blockA, blockB))
			return false;

		const Instruction *termA = blockA->getTerminator();
		const Instruction *termB = blockB->getTerminator();
		if (termA->getNumSuccessors() != termB->getNumSuccessors())
			return false;

		for (unsigned i = 0, e = termA->getNumSuccessors(); i != e; ++i) {
			if (!visited.insert(termA->getSuccessor(i)).second)
				continue;
			blocks.push_back({termA->getSuccessor(i), termB->getSuccessor(i)});
		}
	}

	return true;
}

bool IdenticalCodeFolding::equivalentBlock(const llvm::BasicBlock* A, const llvm::BasicBlock* B)
{
	// Count the number of instructions in both blocks, without counting
	// ignored instructions.
	size_t countA = 0, countB = 0;
	for (auto I = A->begin(); I != A->end(); ++I) {
		if (ignoreInstruction(&*I))
			continue;
		countA++;
	}
	for (auto I = B->begin(); I != B->end(); ++I) {
		if (ignoreInstruction(&*I))
			continue;
		countB++;
	}

	if (countA != countB)
		return false;

	BasicBlock::const_iterator IA = A->begin();
	BasicBlock::const_iterator IB = B->begin();
	while (IA != A->end() && IB != B->end())
	{
		// Skip ignoreable instructions.
		if (ignoreInstruction(&*IA)) {
			++IA;
			continue;
		} else if (ignoreInstruction(&*IB)) {
			++IB;
			continue;
		}

		// TODO: Skip inlineable instructions. (requires PointerAnalyzer)

		if (IA->use_empty() != IB->use_empty())
			return false;

		if (!equivalentInstruction(&*IA, &*IB))
			return false;

		++IA;
		++IB;
	}
	// If both iterators are at the end, the block is equivalent. Otherwise,
	// one of the blocks has more non-ignored instructions.
	assert(IA == A->end() && IB == B->end());
	return true;
}

bool IdenticalCodeFolding::equivalentInstruction(const llvm::Instruction* A, const llvm::Instruction* B)
{
#if DEBUG_VERBOSE
	llvm::errs() << "IA: "; A->dump();
	llvm::errs() << "IB: "; B->dump();
#endif

	auto it = equivalenceCache.find(std::make_pair(A, B));
	if(it != equivalenceCache.end())
		return it->second;

	auto CacheAndReturn = [this,A,B](bool ret) {
		equivalenceCache.insert(std::make_pair(std::make_pair(A,B), ret));
		return ret;
	};

	if (!A || !B || A->getOpcode() != B->getOpcode())
		return CacheAndReturn(false);

	switch(A->getOpcode())
	{
		case Instruction::Alloca:
		{
			llvm::report_fatal_error("Allocas in wasm should be removed in the AllocaLowering pass. This is a bug");
		}
		case Instruction::Unreachable:
		{
			return CacheAndReturn(true);
		}
		case Instruction::PtrToInt:
		case Instruction::IntToPtr:
		case Instruction::BitCast:
		case Instruction::Trunc:
		case Instruction::FPToSI:
		case Instruction::FPToUI:
		case Instruction::FPTrunc:
		case Instruction::FPExt:
		case Instruction::FNeg:
		{
			return CacheAndReturn(equivalentOperand(A->getOperand(0), B->getOperand(0)));
		}
		case Instruction::Add:
		case Instruction::And:
		case Instruction::AShr:
		case Instruction::LShr:
		case Instruction::Mul:
		case Instruction::Or:
		case Instruction::Shl:
		case Instruction::Sub:
		case Instruction::SDiv:
		case Instruction::UDiv:
		case Instruction::SRem:
		case Instruction::URem:
		case Instruction::Xor:
		case Instruction::FAdd:
		case Instruction::FDiv:
		case Instruction::FMul:
		case Instruction::FSub:
		case Instruction::FRem:
		{
			return CacheAndReturn(equivalentOperand(A->getOperand(0), B->getOperand(0)) &&
				equivalentOperand(A->getOperand(1), B->getOperand(1)));
		}
		case Instruction::FCmp:
		case Instruction::ICmp:
		{
			const CmpInst* a = cast<CmpInst>(A);
			const CmpInst* b = cast<CmpInst>(B);
			if(a->getPredicate() != b->getPredicate())
				return CacheAndReturn(false);
			return CacheAndReturn(equivalentOperand(A->getOperand(0), B->getOperand(0)) &&
				equivalentOperand(A->getOperand(1), B->getOperand(1)));
		}
		case Instruction::Br:
		{
			const BranchInst* a = cast<BranchInst>(A);
			const BranchInst* b = cast<BranchInst>(B);
			if (a->isConditional() != b->isConditional())
				return CacheAndReturn(false);

			return CacheAndReturn(!a->isConditional() ||
				equivalentOperand(a->getCondition(), b->getCondition()));
		}
		case Instruction::Switch:
		{
			const SwitchInst* a = cast<SwitchInst>(A);
			const SwitchInst* b = cast<SwitchInst>(B);
			return CacheAndReturn(equivalentOperand(a->getCondition(), b->getCondition()));
		}
		case Instruction::VAArg:
		{
			const VAArgInst* a = cast<VAArgInst>(A);
			const VAArgInst* b = cast<VAArgInst>(B);
			return CacheAndReturn(equivalentType(a->getType(), b->getType()) &&
				equivalentOperand(a->getPointerOperand(), b->getPointerOperand()));
		}
		case Instruction::Call:
		{
			const CallInst* ci = cast<CallInst>(A);
			const Function * calledFunc = ci->getCalledFunction();
			const Value * calledValue = ci->getCalledOperand();
			const PointerType* pTy = cast<PointerType>(calledValue->getType());
			const FunctionType* fTy = cast<FunctionType>(pTy->getElementType());
			assert(!ci->isInlineAsm());

			if (calledFunc)
			{
				unsigned intrinsic = calledFunc->getIntrinsicID();

				if (!cast<CallInst>(B)->getCalledFunction() ||
					cast<CallInst>(B)->getCalledFunction()->getIntrinsicID() != intrinsic)
				{
					return CacheAndReturn(false);
				}

				switch (calledFunc->getIntrinsicID())
				{
					case Intrinsic::vaend:
					case Intrinsic::invariant_end:
					case Intrinsic::trap:
					case Intrinsic::stacksave:
					{
						return CacheAndReturn(true);
					}
					case Intrinsic::vastart:
					{
						llvm::report_fatal_error("Vastart in wasm should be removed in the AllocaLowering pass. This is a bug");
					}
					case Intrinsic::not_intrinsic:
						break;
					case Intrinsic::cheerp_downcast:
					case Intrinsic::cheerp_virtualcast:
					{
						if (!equivalentType(A->getType(), B->getType()))
							return CacheAndReturn(false);
						[[clang::fallthrough]];
					}
					default:
					{
						for (unsigned int i=0; i<A->getNumOperands(); i++)
						{
							if (!equivalentOperand(A->getOperand(i), B->getOperand(i)))
								return CacheAndReturn(false);
						}
						return CacheAndReturn(true);
					}
					break;
				}
			}

			const Value* calledValueB = cast<CallInst>(B)->getCalledOperand();
			const PointerType* pTyB = cast<PointerType>(calledValueB->getType());
			const FunctionType* fTyB = cast<FunctionType>(pTyB->getElementType());

			if (fTy->getNumParams() != fTyB->getNumParams())
				return CacheAndReturn(false);

			for (auto opA = ci->op_begin(), opB = cast<CallInst>(B)->op_begin();
					opA != ci->op_begin() + fTy->getNumParams(); ++opA, ++opB)
			{
				if (!equivalentOperand(opA->get(), opB->get()))
					return CacheAndReturn(false);
			}

			if (calledFunc)
			{
				// Traverse into functions when name does not match.
				const Function* calledFuncB = cast<CallInst>(B)->getCalledFunction();
				if (calledFunc != calledFuncB) {
					auto key = make_pair(calledFunc, calledFuncB);
					bool equivalent = false;
					auto found = functionEquivalence.find(key);
					if (found == functionEquivalence.end()) {
						equivalent = equivalentFunction(calledFunc, calledFuncB);
						functionEquivalence[key] = equivalent;
					} else {
						equivalent = found->second;
					}

					if (!equivalent)
						return CacheAndReturn(false);
				}
			}
			else if (isStaticIndirectFunction(calledValue) && isStaticIndirectFunction(calledValueB)) {
				calledFunc = cast<Function>(cast<ConstantExpr>(calledValue)->getOperand(0));
				auto calledFuncB = cast<Function>(cast<ConstantExpr>(calledValueB)->getOperand(0));
				// Traverse into functions when name does not match.
				if (calledFunc != calledFuncB) {
					auto key = make_pair(calledFunc, calledFuncB);
					bool equivalent = false;
					auto found = functionEquivalence.find(key);
					if (found == functionEquivalence.end()) {
						equivalent = equivalentFunction(calledFunc, calledFuncB);
						functionEquivalence[key] = equivalent;
					} else {
						equivalent = found->second;
					}

#if DEBUG_VERBOSE
					llvm::errs() << "function equivalent: " << equivalent << "\n";
#endif
					if (!equivalent)
						return CacheAndReturn(false);
				}
			}
			else if (!equivalentOperand(calledValue, calledValueB)) {
				return CacheAndReturn(false);
			}

			return CacheAndReturn(ci->getType()->isVoidTy() == cast<CallInst>(B)->getType()->isVoidTy());
		}
		case Instruction::GetElementPtr:
		{
			const GetElementPtrInst* a = cast<GetElementPtrInst>(A);
			const GetElementPtrInst* b = cast<GetElementPtrInst>(B);
			return CacheAndReturn(equivalentGep(a, b));
		}
		case Instruction::Load:
		{
			const Value* ptrOpA = cast<LoadInst>(A)->getPointerOperand();
			const Value* ptrOpB = cast<LoadInst>(B)->getPointerOperand();
			return CacheAndReturn(equivalentType(A->getType(), B->getType()) &&
				equivalentOperand(ptrOpA, ptrOpB));
		}
		case Instruction::Store:
		{
			const Value* ptrOpA = cast<StoreInst>(A)->getPointerOperand();
			const Value* valOpA = cast<StoreInst>(A)->getValueOperand();
			const Value* ptrOpB = cast<StoreInst>(B)->getPointerOperand();
			const Value* valOpB = cast<StoreInst>(B)->getValueOperand();

			return CacheAndReturn(equivalentOperand(ptrOpA, ptrOpB) &&
				equivalentOperand(valOpA, valOpB) &&
				// When storing values with size less than 32-bit, truncate them.
				hasSameIntegerBitWidth(valOpA->getType(), valOpB->getType()));
		}
		case Instruction::Ret:
		{
			const Value* retValA = cast<ReturnInst>(A)->getReturnValue();
			const Value* retValB = cast<ReturnInst>(B)->getReturnValue();
			if(retValA && retValB)
				return CacheAndReturn(equivalentOperand(retValA, retValB));
			return CacheAndReturn(!retValA && !retValB);
		}
		case Instruction::Select:
		{
			const SelectInst* siA = cast<SelectInst>(A);
			const SelectInst* siB = cast<SelectInst>(B);
			return CacheAndReturn(equivalentOperand(siA->getTrueValue(), siB->getTrueValue()) ||
				equivalentOperand(siA->getFalseValue(), siB->getFalseValue()) ||
				equivalentOperand(siA->getCondition(), siB->getCondition()));
		}
		case Instruction::SIToFP:
		case Instruction::UIToFP:
		case Instruction::SExt:
		case Instruction::ZExt:
		{
			uint32_t bitsA = A->getOperand(0)->getType()->getIntegerBitWidth();
			uint32_t bitsB = B->getOperand(0)->getType()->getIntegerBitWidth();
			return CacheAndReturn(bitsA == bitsB &&
				equivalentOperand(A->getOperand(0), B->getOperand(0)));
		}
		case Instruction::PHI:
		{
			const PHINode* a = cast<PHINode>(A);
			const PHINode* b = cast<PHINode>(B);

			// Avoid recursion by marking the PHIs.
			if (!visitedPhis.insert(a).second)
				return CacheAndReturn(true);
			if (visitedPhis.count(b))
				return CacheAndReturn(false);
			visitedPhis.insert(b);

			if (a->getNumIncomingValues() != b->getNumIncomingValues())
				return CacheAndReturn(false);
			for (unsigned i = 0; i < a->getNumIncomingValues(); i++) {
				if (!equivalentOperand(a->getIncomingValue(i), b->getIncomingValue(i)))
					return CacheAndReturn(false);
			}
			return CacheAndReturn(true);
		}
		default:
		{
#ifndef NDEBUG
			A->dump();
#endif
			llvm_unreachable("Unknown instruction");
		}
	}

	return CacheAndReturn(true);
}

bool IdenticalCodeFolding::equivalentOperand(const llvm::Value* A, const llvm::Value* B)
{
#if DEBUG_VERBOSE
	llvm::errs() << "OA: "; A->dump();
	llvm::errs() << "OB: "; B->dump();
#endif
	if (!A || !B)
		return false;

	if (!equivalentType(A->getType(), B->getType()))
		return false;

	if (isa<Constant>(A) || isa<Constant>(B)) {
		if (!isa<Constant>(A) || !isa<Constant>(B))
			return false;
		return equivalentConstant(cast<Constant>(A), cast<Constant>(B));
	}

	if (isa<Instruction>(A) || isa<Instruction>(B)) {
		if (!isa<Instruction>(A) || !isa<Instruction>(B))
			return false;
		return equivalentInstruction(cast<Instruction>(A), cast<Instruction>(B));
	}

	if (isa<Argument>(A) || isa<Argument>(B)) {
		const Argument* a = cast<Argument>(A);
		const Argument* b = cast<Argument>(B);
		if (!a || !b)
		    return false;
		return a->getArgNo() == b->getArgNo();
	}

#ifndef NDEBUG
	A->dump();
	B->dump();
#endif
	llvm_unreachable("unknown operand");
}

bool IdenticalCodeFolding::equivalentConstant(const llvm::Constant* A, const llvm::Constant* B)
{
	if (!equivalentType(A->getType(), B->getType()))
		return false;

	if (isa<ConstantExpr>(A) || isa<ConstantExpr>(B)) {
		const auto CA = dyn_cast<ConstantExpr>(A);
		const auto CB = dyn_cast<ConstantExpr>(B);
		if (!CA || !CB)
			return false;

		if (CA->isCast() && CB->isCast())
			return equivalentOperand(A->getOperand(0), B->getOperand(0));

		return A == B;
	}

	if (isa<ConstantInt>(A) || isa<ConstantInt>(B)) {
		const ConstantInt* a = dyn_cast<ConstantInt>(A);
		const ConstantInt* b = dyn_cast<ConstantInt>(B);
		if (!a || !b)
		    return false;
		uint32_t bitsA = a->getBitWidth();
		uint32_t bitsB = b->getBitWidth();
		if (bitsA == 64)
			return bitsB == 64 && a->getSExtValue() == b->getSExtValue();
		if (bitsA == 32)
			return bitsB == 32 && a->getSExtValue() == b->getSExtValue();
		// TODO: check for bitsA == bitsB?
		return a->getZExtValue() == b->getZExtValue();
	}

	if (isa<ConstantFP>(A) || isa<ConstantFP>(B)) {
		const ConstantFP* a = dyn_cast<ConstantFP>(A);
		const ConstantFP* b = dyn_cast<ConstantFP>(B);
		if (!a || !b)
			return false;
		return a->isExactlyValue(b->getValueAPF());
	}

	if(isa<GlobalVariable>(A) || isa<GlobalVariable>(B)) {
		if (A == B)
			return true;

		const auto GA = dyn_cast<GlobalVariable>(A);
		const auto GB = dyn_cast<GlobalVariable>(B);
		if (!GA || !GB)
			return false;

		// Check if both are constant, since non-constant global variables
		// cannot be folded.
		if (!GA->isConstant() || !GB->isConstant())
			return false;

		if (!equivalentType(GA->getType()->getElementType(), GB->getType()->getElementType()))
			return false;

		// Without an initializer, the global variable is equivalent when the
		// element type matches.
		if (!GA->hasInitializer() || !GB->hasInitializer())
			return !GA->hasInitializer() && !GB->hasInitializer();

		const auto CDSA = dyn_cast<ConstantDataSequential>(A);
		const auto CDSB = dyn_cast<ConstantDataSequential>(B);
		if (CDSA || CDSB) {
			if (!CDSA || !CDSB || CDSA->getNumElements() != CDSB->getNumElements())
				return false;

			for(uint32_t i = 0; i < CDSA->getNumElements(); i++) {
				if (!equivalentConstant(CDSA->getElementAsConstant(i), CDSB->getElementAsConstant(i)))
					return false;
			}

			return true;
		}

		return false;
	}

	if(isa<ConstantPointerNull>(A) || isa<ConstantPointerNull>(B))
		return isa<ConstantPointerNull>(A) && isa<ConstantPointerNull>(B);

	if(isa<Function>(A) || isa<Function>(B)) {
		if (!isa<Function>(A) || !isa<Function>(B))
			return false;
		return cast<Function>(A)->getName() == cast<Function>(B)->getName();
	}

	if (isa<UndefValue>(A) || isa<UndefValue>(B))
		return isa<UndefValue>(A) && isa<UndefValue>(B);

#ifndef NDEBUG
	A->dump();
	B->dump();
#endif
	llvm_unreachable("unknown constant");
}

bool IdenticalCodeFolding::equivalentType(const llvm::Type* A, const llvm::Type* B)
{
	if (!A || !B)
		return false;

#if DEBUG_VERBOSE
	llvm::errs() << "TA: "; A->dump();
	llvm::errs() << "TB: "; B->dump();
#endif

	if (A == B)
		return true;

	if (A->isArrayTy() && B->isArrayTy()) {
		const llvm::ArrayType* a = cast<ArrayType>(A);
		const llvm::ArrayType* b = cast<ArrayType>(B);
		if (a->getNumElements() != b->getNumElements())
			return false;
		return equivalentType(a->getElementType(), b->getElementType());
	}


	if (A->isStructTy() && B->isStructTy()) {
		const llvm::StructType* a = cast<StructType>(A);
		const llvm::StructType* b = cast<StructType>(B);
		if (a->getNumElements() != b->getNumElements())
			return false;

		for (unsigned i = 0; i < a->getNumElements(); i++) {
			if (!equivalentType(a->getElementType(i), b->getElementType(i)))
				return false;
		}

		return true;
	}

	if ((A->isPointerTy() || A->isIntegerTy(32)) &&
			(B->isPointerTy() || B->isIntegerTy(32)))
	{
		return true;
	}

	return false;
}

bool IdenticalCodeFolding::equivalentGep(const llvm::GetElementPtrInst* A, const llvm::GetElementPtrInst* B)
{
	struct GepListener: public LinearMemoryHelper::GepListener
	{
		GepListener() : offset(0) {}
		std::vector<std::pair<const llvm::Value*, uint32_t>> values;
		int64_t offset;
		void addValue(const llvm::Value* v, uint32_t size) override {
			values.emplace_back(v, size);
		}
		void addConst(int64_t v) override {
			offset += v;
		}
		bool isInlineable(const llvm::Value* p) override {
			return true;
		}
	};

	const llvm::Module& module = *A->getParent()->getParent()->getParent();
	GepListener gepListenerA;
	GepListener gepListenerB;
	const auto a = LinearMemoryHelper::compileGEP(&module, A, &gepListenerA, nullptr);
	const auto b = LinearMemoryHelper::compileGEP(&module, B, &gepListenerB, nullptr);

	if (gepListenerA.offset != gepListenerB.offset ||
		gepListenerA.values.size() != gepListenerB.values.size())
	{
		return false;
	}

	for (size_t i = 0; i < gepListenerA.values.size(); i++) {
		const auto pairA = gepListenerA.values[i];
		const auto pairB = gepListenerB.values[i];
		if (!equivalentOperand(pairA.first, pairB.first) ||
			pairA.second != pairB.second)
		{
			return false;
		}
	}

	return equivalentOperand(a, b);
}

bool IdenticalCodeFolding::hasSameIntegerBitWidth(const llvm::Type* A, const llvm::Type* B)
{
	if (!A || !B)
		return false;

	if (!A->isIntegerTy() && !B->isIntegerTy())
		return true;

	uint32_t bitsA = 0;
	if (A->isIntegerTy()) {
		bitsA = A->getIntegerBitWidth();
		if(bitsA == 1)
			bitsA = 8;
	} else if (A->isPointerTy()) {
		bitsA = 32;
	} else {
		return false;
	}

	uint32_t bitsB = 0;
	if (B->isIntegerTy()) {
		bitsB = B->getIntegerBitWidth();
		if(bitsB == 1)
			bitsB = 8;
	} else if (B->isPointerTy()) {
		bitsB = 32;
	} else {
		return false;
	}

	return bitsA == bitsB;
}

bool IdenticalCodeFolding::ignoreInstruction(const llvm::Instruction* I)
{
	if(const IntrinsicInst* II = dyn_cast<IntrinsicInst>(I))
	{
		// Skip some kind of intrinsics
		if(II->getIntrinsicID()==Intrinsic::dbg_declare ||
			II->getIntrinsicID()==Intrinsic::dbg_value)
		{
			return true;
		}
	}
	return false;
}

bool IdenticalCodeFolding::isStaticIndirectFunction(const llvm::Value* A)
{
	auto ce = dyn_cast<ConstantExpr>(A);
	return ce && ce->isCast() && isa<Function>(ce->getOperand(0));
}

bool IdenticalCodeFolding::runOnModule(llvm::Module& module)
{
	cheerp::GlobalDepsAnalyzer &GDA = getAnalysis<cheerp::GlobalDepsAnalyzer>();
	DL = &module.getDataLayout();

	// First, compute an hash of each function.
	std::unordered_map<uint64_t, std::vector<Function*>> functionHashes;
	for (Function& F : module.getFunctionList()) {
		if (F.isDeclaration() || F.getSection() != StringRef("asmjs"))
			continue;

		// Skip functions with special semantics, we really don't want to merge them with anything
		if (F.getName() == "malloc" || F.getName() == "calloc" ||
			F.getName() == "realloc" || F.getName() == "free" ||
			F.getName() == wasmNullptrName) {
			continue;
		}

		uint64_t hash = hashFunction(F);
		const auto& found = functionHashes.find(hash);
		if (found != functionHashes.end())
			found->second.push_back(&F);
		else
			functionHashes.insert({hash, {&F}});
	}

	// Second, compare the functions that have the same hash value.
	for (auto item : functionHashes) {
		auto& functions = item.second;

		if (functions.size() < 2)
			continue;

		LLVM_DEBUG(dbgs() << "group(" << functions.size() << "):");
		LLVM_DEBUG(
			for (auto& function: functions)
			{
				dbgs() << " " << function->getName();
			}
		);
		LLVM_DEBUG(dbgs() << "\n");

		std::unordered_map<Function*, Function*> fold;
		std::vector<std::pair<Function*, Function*>> foldOrder;

		for (unsigned i = 0; i < functions.size(); i++) {
			assert(!fold.count(functions[i]));

			//TODO: even certain external name are foldable, taking care of recreating the right functions in the Writer
			if (functions[i]->getLinkage() == llvm::GlobalValue::ExternalLinkage)
				continue;

			for (unsigned j = 0; j < functions.size(); j++) {
				if (i == j || fold.count(functions[j]))
					continue;

				visitedPhis.clear();

				auto key = make_pair(functions[i], functions[j]);
				bool equivalent = false;
				auto found = functionEquivalence.find(key);
				if (found == functionEquivalence.end()) {
					equivalent = equivalentFunction(functions[i], functions[j]);
					functionEquivalence[key] = equivalent;
				} else {
					equivalent = found->second;
				}

#if DEBUG_VERBOSE
				llvm::errs() << "function equivalent: " << equivalent << "\n";
#endif

				if (equivalent) {
					fold.insert({functions[i], functions[j]});
					foldOrder.push_back({functions[i], functions[j]});
					break;
				}
			}
		}

		LLVM_DEBUG(dbgs() << "fold " << foldOrder.size() << " of " << functions.size() << '\n');
		assert(foldOrder.size() < functions.size());

		for (auto item : foldOrder) {
			// Replace all equivalent functions with the same replacement.
			Function* replacement = item.second;
			for (;;) {
				auto it = fold.find(replacement);
				if (it == fold.end())
					break;
				replacement = it->second;
			}

			mergeTwoFunctions(item.first, replacement);

			//TODO: move external name to metadata and it becames again possible to set _icf
			if (!replacement->getName().endswith("_icf") && (replacement->getLinkage() != llvm::GlobalValue::ExternalLinkage))
			{
				LLVM_DEBUG(dbgs() << "rename " << replacement->getName() <<
						" to " << replacement->getName() + "_icf" << '\n');
				replacement->setName(replacement->getName() + "_icf");
			}

			if (GDA.asmJSExports().find(item.first) != GDA.asmJSExports().end())
				GDA.insertAsmJSExport(replacement);

			GDA.eraseFunction(item.first);
			delete item.first;
		}
	}

	return true;
}

// Merge two equivalent functions. Upon completion, function F is deleted.
void IdenticalCodeFolding::mergeTwoFunctions(Function *F, Function *G) {
	LLVM_DEBUG(dbgs() << "replace " << F->getName() << " with " << G->getName() << '\n');

	// Replace F with G is all uses, special case direct call and then do a bulk replace for the rest
	SmallVector<CallBase*, 4> directCalls;

	for (const Use &U : F->uses()) {
		User *FU = U.getUser();
		if (!isa<CallInst>(FU) && !isa<InvokeInst>(FU))
			continue;
		CallBase* CS = cast<CallBase>(FU);
		if (CS->isCallee(&U))
			directCalls.push_back(CS);
	}

	FunctionType* FType = F->getFunctionType();
	FunctionType* GType = G->getFunctionType();

	assert(FType->getNumParams() == GType->getNumParams());
	for (CallBase* CS: directCalls) {
		// BitCasts in call sites causes spurious indirect call
		// Avoid this problem by bitcasting parameters and return values as appropriate
		CallInst* callInst = cast<CallInst>(CS);
		callInst->setCalledOperand(ConstantExpr::getBitCast(G, callInst->getCalledOperand()->getType()));

		replaceCallOfBitCastWithBitCastOfCall(*callInst, /*mayFail*/ false, /*performPtrIntConversions*/ true);
	}
	if(!F->use_empty()){
		Value* replacement = G;
		if (F->getType() != G->getType()) {
			llvm::IRBuilder<> builder(F->getContext());
			replacement = builder.CreateBitCast(G, F->getType(), "icf");
		}

		F->replaceAllUsesWith(replacement);
	}
	F->dropAllReferences();

	F->removeFromParent();
}

}

using namespace cheerp;

INITIALIZE_PASS_BEGIN(IdenticalCodeFolding, "IdenticalCodeFolding", "Remove duplicate functions/globals from the module",
                      false, false)
INITIALIZE_PASS_END(IdenticalCodeFolding, "IdenticalCodeFolding", "Remove duplicate functions/globals from the module",
                    false, false)

