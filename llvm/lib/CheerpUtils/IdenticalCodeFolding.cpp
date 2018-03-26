//===-- IdenticalCodeFolding.cpp - Remove duplicate functions/globals -----===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "IdenticalCodeFolding"
#include "llvm/InitializePasses.h"
#include "llvm/Cheerp/IdenticalCodeFolding.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#include <unordered_map>

using namespace llvm;

namespace cheerp {

using namespace std;

char IdenticalCodeFolding::ID = 0;

const char* IdenticalCodeFolding::getPassName() const
{
	return "IdenticalCodeFolding";
}

IdenticalCodeFolding::IdenticalCodeFolding() : ModulePass(ID)
{
}

void IdenticalCodeFolding::getAnalysisUsage(AnalysisUsage& AU) const
{
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

		const TerminatorInst *Term = BB->getTerminator();
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

		const TerminatorInst *termA = blockA->getTerminator();
		const TerminatorInst *termB = blockB->getTerminator();
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
	if (A->size() != B->size())
		return false;

	BasicBlock::const_iterator IA = A->begin();
	BasicBlock::const_iterator IB = B->begin();
	for(; IA != A->end() && IB != B->end(); ++IA, ++IB)
	{
		// Skip ignoreable instructions.
		if (ignoreInstruction(&*IA)) {
			if (ignoreInstruction(&*IB)) {
				continue;
			}
			return false;
		} else if (ignoreInstruction(&*IB)) {
			return false;
		}

		// TODO: Skip inlineable instructions. (requires PointerAnalyzer)

		if (IA->use_empty() != IB->use_empty())
			return false;

		if (!equivalentInstruction(&*IA, &*IB))
			return false;
	}
	return true;
}

bool IdenticalCodeFolding::equivalentInstruction(const llvm::Instruction* A, const llvm::Instruction* B)
{
	if (!A || !B || A->getOpcode() != B->getOpcode())
		return false;

	switch(A->getOpcode())
	{
		case Instruction::Alloca:
		{
			llvm::report_fatal_error("Allocas in wasm should be removed in the AllocaLowering pass. This is a bug");
		}
		case Instruction::Unreachable:
		{
			return true;
		}
		case Instruction::PtrToInt:
		case Instruction::IntToPtr:
		case Instruction::BitCast:
		case Instruction::Trunc:
		case Instruction::FPToSI:
		case Instruction::FPToUI:
		case Instruction::FPTrunc:
		case Instruction::FPExt:
		{
			return equivalentOperand(A->getOperand(0), B->getOperand(0));
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
		case Instruction::FCmp:
		case Instruction::ICmp:
		{
			return equivalentOperand(A->getOperand(0), B->getOperand(0)) &&
				equivalentOperand(A->getOperand(1), B->getOperand(1));
		}
		case Instruction::Br:
		{
			const BranchInst* a = cast<BranchInst>(A);
			const BranchInst* b = cast<BranchInst>(B);
			if (a->isConditional() != b->isConditional())
				return false;

			return !a->isConditional() ||
				equivalentOperand(a->getCondition(), b->getCondition());
		}
		case Instruction::Switch:
		{
			const SwitchInst* a = cast<SwitchInst>(A);
			const SwitchInst* b = cast<SwitchInst>(B);
			return equivalentOperand(a->getCondition(), b->getCondition());
		}
		case Instruction::VAArg:
		{
			const VAArgInst* a = cast<VAArgInst>(A);
			const VAArgInst* b = cast<VAArgInst>(B);
			return equivalentType(a->getType(), b->getType()) &&
				equivalentOperand(a->getPointerOperand(), b->getPointerOperand());
		}
		case Instruction::Call:
		{
			const CallInst* ci = cast<CallInst>(A);
			const Function * calledFunc = ci->getCalledFunction();
			const Value * calledValue = ci->getCalledValue();
			const PointerType* pTy = cast<PointerType>(calledValue->getType());
			const FunctionType* fTy = cast<FunctionType>(pTy->getElementType());
			assert(!ci->isInlineAsm());

			if (calledFunc)
			{
				unsigned intrinsic = calledFunc->getIntrinsicID();

				if (!cast<CallInst>(B)->getCalledFunction() ||
					cast<CallInst>(B)->getCalledFunction()->getIntrinsicID() != intrinsic)
				{
					return false;
				}

				switch (calledFunc->getIntrinsicID())
				{
					case Intrinsic::vaend:
					case Intrinsic::invariant_end:
					case Intrinsic::trap:
					case Intrinsic::stacksave:
					{
						return true;
					}
					case Intrinsic::vastart:
					{
						llvm::report_fatal_error("Vastart in wasm should be removed in the AllocaLowering pass. This is a bug");
					}
					case Intrinsic::vacopy:
					case Intrinsic::lifetime_start:
					case Intrinsic::lifetime_end:
					{
						return equivalentOperand(A->getOperand(0), B->getOperand(0)) &&
							equivalentOperand(A->getOperand(1), B->getOperand(1));
					}
					case Intrinsic::cheerp_downcast:
					{
						return equivalentType(A->getType(), B->getType()) &&
							equivalentOperand(A->getOperand(0), B->getOperand(0)) &&
							equivalentOperand(A->getOperand(1), B->getOperand(1));
					}
					case Intrinsic::cheerp_downcast_current:
					case Intrinsic::cheerp_upcast_collapsed:
					case Intrinsic::cheerp_cast_user:
					case Intrinsic::flt_rounds:
					case Intrinsic::ctlz:
					case Intrinsic::invariant_start:
					case Intrinsic::stackrestore:
					case Intrinsic::bswap:
					{
						return equivalentOperand(A->getOperand(0), B->getOperand(0));
					}
					case Intrinsic::memmove:
					case Intrinsic::memcpy:
					case Intrinsic::memset:
					{
						return equivalentOperand(A->getOperand(0), B->getOperand(0)) &&
							equivalentOperand(A->getOperand(1), B->getOperand(1)) &&
							equivalentOperand(A->getOperand(2), B->getOperand(2));
					}
					case Intrinsic::cheerp_allocate:
					case Intrinsic::cheerp_reallocate:
					case Intrinsic::cheerp_deallocate:
					{
						break;
					}
					default:
					{
						if (intrinsic != Intrinsic::not_intrinsic) {
							A->dump();
							calledValue->dump();
						}
						assert(intrinsic == Intrinsic::not_intrinsic);
					}
					break;
				}
			}

			const Value* calledValueB = cast<CallInst>(B)->getCalledValue();
			const PointerType* pTyB = cast<PointerType>(calledValueB->getType());
			const FunctionType* fTyB = cast<FunctionType>(pTyB->getElementType());

			if (fTy->getNumParams() != fTyB->getNumParams())
				return false;

			for (auto opA = ci->op_begin(), opB = cast<CallInst>(B)->op_begin();
					opA != ci->op_begin() + fTy->getNumParams(); ++opA, ++opB)
			{
				if (!equivalentOperand(opA->get(), opB->get()))
					return false;
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
						return false;
				}
			}
			else if (!equivalentOperand(calledValue, calledValueB)) {
				return false;
			}

			return ci->getType()->isVoidTy() == cast<CallInst>(B)->getType()->isVoidTy();
		}
		case Instruction::GetElementPtr:
		{
			const GetElementPtrInst* a = cast<GetElementPtrInst>(A);
			const GetElementPtrInst* b = cast<GetElementPtrInst>(B);
			const Type* aTy = a->getPointerOperandType()->getPointerElementType();
			const Type* bTy = b->getPointerOperandType()->getPointerElementType();
			return equivalentType(aTy, bTy) &&
				equivalentIndices(a, b) &&
				equivalentOperand(a->getPointerOperand(), b->getPointerOperand());
		}
		case Instruction::Load:
		{
			const Value* ptrOpA = cast<LoadInst>(A)->getPointerOperand();
			const Value* ptrOpB = cast<LoadInst>(B)->getPointerOperand();
			return equivalentType(A->getType(), B->getType()) &&
				equivalentOperand(ptrOpA, ptrOpB);
		}
		case Instruction::Store:
		{
			const Value* ptrOpA = cast<StoreInst>(A)->getPointerOperand();
			const Value* valOpA = cast<StoreInst>(A)->getValueOperand();
			const Value* ptrOpB = cast<StoreInst>(B)->getPointerOperand();
			const Value* valOpB = cast<StoreInst>(B)->getValueOperand();

			return equivalentOperand(ptrOpA, ptrOpB) &&
				equivalentOperand(valOpA, valOpB) &&
				// When storing values with size less than 32-bit, truncate them.
				hasSameIntegerBitWidth(valOpA->getType(), valOpB->getType());
		}
		case Instruction::Ret:
		{
			const Value* retValA = cast<ReturnInst>(A)->getReturnValue();
			const Value* retValB = cast<ReturnInst>(B)->getReturnValue();
			if(retValA && retValB)
				return equivalentOperand(retValA, retValB);
			return !retValA && !retValB;
		}
		case Instruction::Select:
		{
			const SelectInst* siA = cast<SelectInst>(A);
			const SelectInst* siB = cast<SelectInst>(B);
			return equivalentOperand(siA->getTrueValue(), siB->getTrueValue()) ||
				equivalentOperand(siA->getFalseValue(), siB->getFalseValue()) ||
				equivalentOperand(siA->getCondition(), siB->getCondition());
		}
		case Instruction::SIToFP:
		case Instruction::UIToFP:
		case Instruction::SExt:
		case Instruction::ZExt:
		{
			uint32_t bitsA = A->getOperand(0)->getType()->getIntegerBitWidth();
			uint32_t bitsB = B->getOperand(0)->getType()->getIntegerBitWidth();
			return bitsA == bitsB &&
				equivalentOperand(A->getOperand(0), B->getOperand(0));
		}
		case Instruction::PHI:
		{
			const PHINode* a = cast<PHINode>(A);
			const PHINode* b = cast<PHINode>(B);

			// Avoid recursion by marking the PHIs.
			if (!visitedPhis.insert(a).second)
				return true;
			if (visitedPhis.count(b))
				return false;
			visitedPhis.insert(b);

			if (a->getNumIncomingValues() != b->getNumIncomingValues())
				return false;
			for (unsigned i = 0; i < a->getNumIncomingValues(); i++) {
				if (!equivalentOperand(a->getIncomingValue(i), b->getIncomingValue(i)))
					return false;
			}
			return true;
		}
		default:
		{
			A->dump();
			llvm_unreachable("Unknown instruction");
		}
	}

	return true;
}

bool IdenticalCodeFolding::equivalentOperand(const llvm::Value* A, const llvm::Value* B)
{
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

	A->dump();
	B->dump();
	llvm_unreachable("unknown operand");
}

bool IdenticalCodeFolding::equivalentConstant(const llvm::Constant* A, const llvm::Constant* B)
{
	if (!equivalentType(A->getType(), B->getType()))
		return false;

	if (isa<ConstantExpr>(A) || isa<ConstantExpr>(B))
		return A == B;

	if (isa<ConstantInt>(A) || isa<ConstantInt>(B)) {
		const ConstantInt* a = cast<ConstantInt>(A);
		const ConstantInt* b = cast<ConstantInt>(B);
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
		const ConstantFP* a = cast<ConstantFP>(A);
		const ConstantFP* b = cast<ConstantFP>(B);
		if (!a || !b)
			return false;
		return a->isExactlyValue(b->getValueAPF());
	}

	if(isa<GlobalVariable>(A) || isa<GlobalVariable>(B))
		return A == B;

	if(isa<ConstantPointerNull>(A) || isa<ConstantPointerNull>(B))
		return isa<ConstantPointerNull>(A) && isa<ConstantPointerNull>(B);

	if(isa<Function>(A) || isa<Function>(B)) {
		if (!isa<Function>(A) || !isa<Function>(B))
			return false;
		return cast<Function>(A)->getName() == cast<Function>(B)->getName();
	}

	if (isa<UndefValue>(A) || isa<UndefValue>(B))
		return isa<UndefValue>(A) && isa<UndefValue>(B);

	A->dump();
	B->dump();
	llvm_unreachable("unknown constant");
}

bool IdenticalCodeFolding::equivalentType(const llvm::Type* A, const llvm::Type* B)
{
	if (!A || !B)
		return false;

	if (A->isFloatTy() && B->isFloatTy())
		return true;
	if (A->isDoubleTy() && B->isDoubleTy())
		return true;
	if (A->isIntegerTy() && B->isIntegerTy())
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

bool IdenticalCodeFolding::equivalentIndices(const llvm::GetElementPtrInst* A, const llvm::GetElementPtrInst* B)
{
	if (!A || !B || A->getNumIndices() != B->getNumIndices())
		return false;

	for (unsigned i = 0; i < A->getNumIndices(); i++) {
		if (!equivalentOperand((A->idx_begin() + i)->get(), (B->idx_begin() + i)->get()))
			return false;
	}

	return true;
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

bool IdenticalCodeFolding::runOnModule(llvm::Module& module)
{
	cheerp::GlobalDepsAnalyzer &GDA = getAnalysis<cheerp::GlobalDepsAnalyzer>();

	// First, compute an hash of each function.
	std::unordered_map<uint64_t, std::vector<Function*>> functionHashes;
	for (Function& F : module.getFunctionList()) {
		if (F.isDeclaration() || F.getSection() != StringRef("asmjs"))
			continue;

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

		DEBUG(dbgs() << "group(" << functions.size() << "):");
		for (auto& function: functions)
			DEBUG(dbgs() << " " << function->getName());
		DEBUG(dbgs() << "\n");

		std::unordered_map<Function*, Function*> fold;
		std::vector<std::pair<Function*, Function*>> foldOrder;

		for (unsigned i = 0; i < functions.size(); i++) {
			assert(!fold.count(functions[i]));

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

				if (equivalent) {
					fold.insert({functions[i], functions[j]});
					foldOrder.push_back({functions[i], functions[j]});
					break;
				}
			}
		}

		DEBUG(dbgs() << "fold " << foldOrder.size() << " of " << functions.size() << '\n');
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

			if (!mergeTwoFunctions(item.first, replacement))
				continue;

			if (GDA.asmJSExports().find(item.first) != GDA.asmJSExports().end())
				GDA.insertAsmJSExport(replacement);

			GDA.eraseFunction(item.first);
			delete item.first;
		}
	}

	return true;
}

// Merge two equivalent functions. Upon completion, function F is deleted.
bool IdenticalCodeFolding::mergeTwoFunctions(Function *F, Function *G) {
	DEBUG(dbgs() << "replace " << F->getName() << " with " << G->getName() << '\n');

	// TODO is this necessary?
	unsigned MaxAlignment = std::max(F->getAlignment(), G->getAlignment());
	G->setAlignment(MaxAlignment);
	
	Value* replacement = G;
	if (F->getType() != G->getType()) {
		llvm::IRBuilder<> builder(F->getContext());
		replacement = builder.CreateBitCast(G, F->getType(), "icf");
	}

	F->replaceAllUsesWith(replacement);
	F->dropAllReferences();

	F->removeFromParent();
	return true;
}

}

using namespace cheerp;

INITIALIZE_PASS_BEGIN(IdenticalCodeFolding, "IdenticalCodeFolding", "Remove duplicate functions/globals from the module",
                      false, false)
INITIALIZE_PASS_END(IdenticalCodeFolding, "IdenticalCodeFolding", "Remove duplicate functions/globals from the module",
                    false, false)

