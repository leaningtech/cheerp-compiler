//===-- GlobalDepsAnalyzer.cpp - Remove unused functions/globals -----------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2011-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include <algorithm>
#include <llvm/Analysis/OptimizationRemarkEmitter.h>
#include "llvm/Cheerp/JsExport.h"
#include "llvm/InitializePasses.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Cheerp/CommandLine.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/Cheerp/LinearMemoryHelper.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/ValueSymbolTable.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Transforms/Utils/BuildLibCalls.h"
#include "llvm/Transforms/Utils/Local.h"
#include "llvm/Transforms/Utils/SimplifyLibCalls.h"
#include "llvm/ADT/Triple.h"

#define DEBUG_TYPE "GlobalDepsAnalyzer"

using namespace llvm;

STATISTIC(NumRemovedGlobals, "Number of unused globals which have been removed");

namespace cheerp {

using namespace std;

const char* wasmNullptrName = "__wasm_nullptr";

GlobalDepsAnalyzer::GlobalDepsAnalyzer(MATH_MODE mathMode_, bool llcPass)
	: hasBuiltin{{false}}, mathMode(mathMode_), DL(NULL),
	  entryPoint(NULL), hasCreateClosureUsers(false), hasVAArgs(false),
	  hasPointerArrays(false), hasCheerpException(false), llcPass(llcPass),
	  hasUndefinedSymbolErrors(false), forceTypedArrays(false)
{
}
static void createNullptrFunction(llvm::Module& module)
{
	llvm::Function* wasmNullptr = module.getFunction(StringRef(wasmNullptrName));
	if (wasmNullptr)
		return;

	// Create a dummy function that prevents nullptr conflicts, since the first
	// function address is zero.
	IRBuilder<> builder(module.getContext());
	auto fTy = FunctionType::get(builder.getVoidTy(), false);
	auto stub = Function::Create(fTy, Function::InternalLinkage, wasmNullptrName, &module);
	stub->setSection("asmjs");

	auto block = BasicBlock::Create(module.getContext(), "entry", stub);
	builder.SetInsertPoint(block);
	builder.CreateUnreachable();
}

void GlobalDepsAnalyzer::simplifyCalls(llvm::Module & module) const
{
	std::vector<llvm::CallInst*> deleteList;
	auto LibCallReplacer = [](Instruction *I, Value *With)
	{
		I->replaceAllUsesWith(With);
		I->eraseFromParent();
	};
	OptimizationRemarkEmitter ORE;
	for (Function& F : module.getFunctionList()) {
		FunctionAnalysisManager& FAM = MAM->getResult<FunctionAnalysisManagerModuleProxy>(module).getManager();
		const llvm::TargetLibraryInfo* TLI = &FAM.getResult<TargetLibraryAnalysis>(F);
		assert(TLI);
		LibCallSimplifier callSimplifier(*DL, TLI, ORE, nullptr, nullptr, LibCallReplacer);

		for (BasicBlock& bb : F)
		{
			for (Instruction& I : bb)
			{
				if (isa<CallBase>(I)) {
					CallBase& cb = cast<CallBase>(I);

					bool isAsmJS = F.getSection() == StringRef("asmjs");
					//Replace call(bitcast) with bitcast(call)
					//Might fail and leave the CI calling to a bitcast if the prerequisite are not met (eg. the number of paramethers differ)
					replaceCallOfBitCastWithBitCastOfCall(cb, /*mayFail*/ true, isAsmJS);

					Function* calledFunc = cb.getCalledFunction();

					// Skip indirect calls
					if (calledFunc == nullptr)
						continue;

					if (isa<CallInst>(I)) {
						CallInst& ci = cast<CallInst>(I);
						IRBuilder<> Builder(&ci);
						if (Value* with = callSimplifier.optimizeCall(&ci, Builder)) {
							ci.replaceAllUsesWith(with);
							deleteList.push_back(&ci);
							continue;
						}
					}
				}
			}
		}
	}
	for (CallInst* ci : deleteList) {
		ci->eraseFromParent();
	}
}

void GlobalDepsAnalyzer::extendLifetime(GlobalValue* G)
{
	assert(G);

	externals.push_back(G);
	VisitedSet visited;
	SubExprVec vec;
	visitGlobal( G, visited, vec );
	assert( visited.empty() );
}

void GlobalDepsAnalyzer::extendLifetimeIfPresent(GlobalValue* G)
{
	if (!G)
		return;
	extendLifetime(G);
}

void GlobalDepsAnalyzer::replaceFunctionAliasWithAliasee(llvm::Module &module, StringRef name)
{
	GlobalAlias *funcAlias = module.getNamedAlias(name);
	if (funcAlias)
	{
		Function *actualFunc = dyn_cast<Function>(funcAlias->getAliaseeObject());

		assert(actualFunc);

		funcAlias->replaceAllUsesWith(actualFunc);
		funcAlias->eraseFromParent();

		actualFunc->setName(name);
	}
}

bool GlobalDepsAnalyzer::isAtomicInstruction(const llvm::Instruction& I)
{
	if (isa<AtomicRMWInst>(I) || isa<AtomicCmpXchgInst>(I) || isa<FenceInst>(I))
		return true;
	else if (const llvm::LoadInst* li = dyn_cast<llvm::LoadInst>(&I))
		return li->isAtomic();
	else if (const llvm::StoreInst* si = dyn_cast<llvm::StoreInst>(&I))
		return si->isAtomic();
	return false;
}

bool GlobalDepsAnalyzer::runOnModule( llvm::Module & module )
{
	DL = &module.getDataLayout();
	assert(DL);
	VisitedSet visited;
	hasAtomics = false;

	replaceFunctionAliasWithAliasee(module, "malloc");
	replaceFunctionAliasWithAliasee(module, "calloc");
	replaceFunctionAliasWithAliasee(module, "realloc");
	replaceFunctionAliasWithAliasee(module, "free");
	replaceFunctionAliasWithAliasee(module, "memcpy");
	replaceFunctionAliasWithAliasee(module, "memset");
	replaceFunctionAliasWithAliasee(module, "memmove");

	// Replace the aliases with the actual values
	for (auto& a: make_early_inc_range(module.aliases()))
	{
		a.replaceAllUsesWith( a.getAliasee() );
		// When generating shared modules the aliases might provide additional exported names for functions
		if(!WasmSharedModule)
			a.eraseFromParent();
	}

	simplifyCalls(module);

	if (!llcPass)
	{
		for (Function& F : module.getFunctionList())
		{
			//Those intrinsics may come back as result of other optimizations
			//And we may need the actual functions to lower the intrinsics
			const auto builtinID = BuiltinInstr::getMathBuiltin(F);
			const auto typedBuiltinID = TypedBuiltinInstr::getMathTypedBuiltin(F);

			if (cheerp::BuiltinInstr::isValidJSMathBuiltin(builtinID)
				|| isValidWasmMathBuiltin(typedBuiltinID)
				|| cheerp::TypedBuiltinInstr::mayBeLoweredInto(F)
				|| F.getName() == "memcpy"
				|| F.getName() == "memset"
				|| F.getName() == "memmove")
			{
				if (TypedBuiltinInstr::isAlwaysExactNatively(typedBuiltinID))
					continue;
				extendLifetime(&F);
			}
		}
	}

	bool anyWasmFuncAddrTaken = false;
	// Replace calls like 'printf("Hello!")' with 'puts("Hello!")'.
	for (Function& F : module.getFunctionList()) {
		bool asmjs = F.getSection() == StringRef("asmjs");
		if (asmjs) {
			anyWasmFuncAddrTaken |= F.hasAddressTaken();
		}
		for (BasicBlock& bb : F)
		{
			bool advance = false;	//Do not advance at the start
			for (BasicBlock::iterator instructionIterator = bb.begin(); ;)
			{
				//Might be useful NOT to advance mid-iteration in case of deletions
				//The base case is doing the advancement at the start of the cycle
				if (advance)
				{
					++instructionIterator;
				}
				advance = true;
				if (instructionIterator == bb.end())
					break;
				Instruction& I = *instructionIterator;

				if (isAtomicInstruction(I))
					hasAtomics = true;

				if (isa<CallInst>(I)) {
					CallInst* ci = cast<CallInst>(&I);
					Function* calledFunc = ci->getCalledFunction();

					// Skip indirect calls
					if (calledFunc == nullptr)
						continue;

					if (Function* F = TypedBuiltinInstr::functionToLowerInto(*calledFunc, module))
					{
						ci->setCalledFunction(F);
						continue;
					}

					unsigned II = calledFunc->getIntrinsicID();

					if (II == Intrinsic::prefetch)
					{
						--instructionIterator;
						advance = false;
						ci->eraseFromParent();
						continue;
					}

					if((II == Intrinsic::cheerp_allocate         ||
						II == Intrinsic::cheerp_allocate_array   ||
						II == Intrinsic::cheerp_reallocate       ||
						II == Intrinsic::cheerp_deallocate       ) &&
						!isa<ConstantPointerNull>(ci->getArgOperand(0)))
					{
						Function* OrigFunc = dyn_cast<Function>(ci->getOperand(0)->stripPointerCastsSafe());
						assert(OrigFunc);
						if (llcPass)
						{
							// cheerp_*allocate functions have the form
							// cheerp_func(orig_func, args...).
							// If orig_func is not null, convert to orig_func(args...)
							// and add appropriate bitcasts
							IRBuilder<> Builder(ci);
							SmallVector<Value*, 2> newOps;
							for (uint32_t i = 0; i < OrigFunc->getFunctionType()->getNumParams(); i++) {
								Type* expectedTy = OrigFunc->getFunctionType()->getParamType(i);
								// HACK: the nothrow version of new has std::nothrow_t as last parameter
								// we pass a constant null to it
								Value* op = i+1 >= ci->arg_size()? ConstantPointerNull::get(cast<PointerType>(expectedTy)) : ci->getArgOperand(i+1);
								newOps.push_back(Builder.CreateBitCast(op, expectedTy));
							}
							CallBase* NewCall = Builder.CreateCall(OrigFunc, newOps);
							Value* replacement = Builder.CreateBitCast(NewCall, ci->getType());
							ci->replaceAllUsesWith(replacement);
							ci->eraseFromParent();
							instructionIterator = NewCall->getIterator();
							advance = false;
							continue;
						}
						else
						{
							extendLifetime(OrigFunc);
						}
					}

					if(II == Intrinsic::exp2)
					{
						// Expand this to pow, we can't simply forward to the libc since exp2 is optimized away to the intrinsic itself
						if (!llcPass)
							continue;

						Type* t = ci->getType();
						Function* F = module.getFunction(t->isFloatTy() ? "powf" : "pow");
						CallInst* newCall = CallInst::Create(F, { ConstantFP::get(t, 2.0), ci->getOperand(0) }, "", ci);
						ci->replaceAllUsesWith(newCall);

						//Set up loop variable, so the next loop will check and possibly expand newCall
						--instructionIterator;
						advance = false;
						assert(&*instructionIterator == newCall);

						ci->eraseFromParent();
						continue;
					}
					if(II == Intrinsic::cttz)
					{
						if (!llcPass)
							continue;

						if (mathMode == WASM_BUILTINS)
							continue;

						Type* t = ci->getType();
						Value* X = ci->getOperand(0);

						Value* bitWidth = ConstantInt::get(t, t->isIntegerTy(32) ? 32 : 64);
						Value* bitWidthMin1 = ConstantInt::get(t, t->isIntegerTy(32) ? 31 : 63);

						Instruction* negateX = BinaryOperator::CreateSub(ConstantInt::get(t, 0), X, "cttz_neg", ci);
						Instruction* andNegX = BinaryOperator::CreateAnd(X, negateX, "cttz_and", ci);
						Instruction* ctlz = CallInst::Create(Intrinsic::getDeclaration(&module, Intrinsic::ctlz, {t}), {andNegX, ci->getOperand(1)}, "cttz_call", ci);
						Instruction* sub = BinaryOperator::CreateSub(bitWidthMin1, ctlz, "cttz_sub", ci);
                                                Instruction* cmp = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_NE, ConstantInt::get(t, 0), ci->getOperand(0), "cttz_cmp", ci);
						Instruction* res = SelectInst::Create(cmp, sub, bitWidth, "cttz_select", ci);

						ci->replaceAllUsesWith(res);

						//Set up loop variable, so the next loop will check and possibly expand newCall
						--instructionIterator;
						advance = false;
						assert(&*instructionIterator == res);

						ci->eraseFromParent();
						continue;
					}
					if(II == Intrinsic::ctpop)
					{
						if (!llcPass)
							continue;

						if (mathMode == WASM_BUILTINS)
							continue;

						Type* t = ci->getType();
						Value* X = ci->getOperand(0);
						// https://graphics.stanford.edu/%7Eseander/bithacks.html#CountBitsSetParallel
						// Somewhat surprising the code takes the same number of operations independently of the width,
						// thanks to a final summation step via a single multiplication that scales with the width
						APInt allOnes = APInt::getAllOnes(t->getIntegerBitWidth());
						X = BinaryOperator::CreateSub(X,
							BinaryOperator::CreateAnd(
								BinaryOperator::CreateLShr(X, ConstantInt::get(t, 1), "", ci),
								ConstantInt::get(t, allOnes.udiv(3)), "", ci), "", ci);
						X = BinaryOperator::CreateAdd(
							BinaryOperator::CreateAnd(X, ConstantInt::get(t, allOnes.udiv(15) * 3), "", ci),
							BinaryOperator::CreateAnd(
								BinaryOperator::CreateLShr(X, ConstantInt::get(t, 2), "", ci),
								ConstantInt::get(t, allOnes.udiv(15) * 3), "", ci), "", ci);
						X = BinaryOperator::CreateAnd(ConstantInt::get(t, allOnes.udiv(255) * 15),
							BinaryOperator::CreateAdd(X,
								BinaryOperator::CreateLShr(X, ConstantInt::get(t, 4), "", ci), "", ci), "", ci);
						X = BinaryOperator::CreateMul(X, ConstantInt::get(t, allOnes.udiv(255)), "", ci);
						Instruction* res = BinaryOperator::CreateLShr(X, ConstantInt::get(t, (t->getIntegerBitWidth() - 8)), "", ci);

						ci->replaceAllUsesWith(res);

						//Set up loop variable, so the next loop will check and possibly expand newCall
						--instructionIterator;
						advance = false;
						assert(&*instructionIterator == res);

						ci->eraseFromParent();
						continue;
					}
					if(II == Intrinsic::usub_sat)
					{
						if (!llcPass)
							continue;

						Value* A = ci->getOperand(0);
						Value* B = ci->getOperand(1);

						Instruction* op = BinaryOperator::CreateSub(A, B, "usub_sat_sub", ci);
						Instruction* cmp = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_ULT, A, B, "usub_sat_cmp", ci);
						Instruction* res = SelectInst::Create(cmp, ConstantInt::get(A->getType(), 0), op, "usub_sat", ci);

						ci->replaceAllUsesWith(res);

						//Set up loop variable, so the next loop will check and possibly expand newCall
						--instructionIterator;
						advance = false;
						assert(&*instructionIterator == res);

						ci->eraseFromParent();
						continue;
					}
					if(II == Intrinsic::uadd_sat)
					{
						if (!llcPass)
							continue;

						Value* A = ci->getOperand(0);
						Value* B = ci->getOperand(1);

						Instruction* op = BinaryOperator::CreateAdd(A, B, "uadd_sat_add", ci);
						Instruction* cmp = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_ULT, op, B, "uadd_sat_cmp", ci);
						Instruction* res = SelectInst::Create(cmp, ConstantInt::get(A->getType(), -1), op, "uadd_sat", ci);

						ci->replaceAllUsesWith(res);

						//Set up loop variable, so the next loop will check and possibly expand newCall
						--instructionIterator;
						advance = false;
						assert(&*instructionIterator == res);

						ci->eraseFromParent();
						continue;
					}
					if(II == Intrinsic::smin)
					{
						if (!llcPass || ci->getOperand(0)->getType()->isVectorTy())
							continue;

						Value* A = ci->getOperand(0);
						Value* B = ci->getOperand(1);

						Instruction* cmp = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, A, B, "smin_cmp", ci);
						Instruction* res = SelectInst::Create(cmp, A, B, "smin_select", ci);

						ci->replaceAllUsesWith(res);

						//Set up loop variable, so the next loop will check and possibly expand newCall
						--instructionIterator;
						advance = false;
						assert(&*instructionIterator == res);

						ci->eraseFromParent();
						continue;
					}
					if(II == Intrinsic::smax)
					{
						if (!llcPass || ci->getOperand(0)->getType()->isVectorTy())
							continue;

						Value* A = ci->getOperand(0);
						Value* B = ci->getOperand(1);

						Instruction* cmp = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SGT, A, B, "smax_cmp", ci);
						Instruction* res = SelectInst::Create(cmp, A, B, "smax_select", ci);

						ci->replaceAllUsesWith(res);

						//Set up loop variable, so the next loop will check and possibly expand newCall
						--instructionIterator;
						advance = false;
						assert(&*instructionIterator == res);

						ci->eraseFromParent();
						continue;
					}
					if(II == Intrinsic::umin)
					{
						if (!llcPass || ci->getOperand(0)->getType()->isVectorTy())
							continue;

						Value* A = ci->getOperand(0);
						Value* B = ci->getOperand(1);

						Instruction* cmp = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_ULT, A, B, "smin_cmp", ci);
						Instruction* res = SelectInst::Create(cmp, A, B, "smin_select", ci);

						ci->replaceAllUsesWith(res);

						//Set up loop variable, so the next loop will check and possibly expand newCall
						--instructionIterator;
						advance = false;
						assert(&*instructionIterator == res);

						ci->eraseFromParent();
						continue;
					}
					if(II == Intrinsic::umax)
					{
						if (!llcPass || ci->getOperand(0)->getType()->isVectorTy())
							continue;

						Value* A = ci->getOperand(0);
						Value* B = ci->getOperand(1);

						Instruction* cmp = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_UGT, A, B, "smax_cmp", ci);
						Instruction* res = SelectInst::Create(cmp, A, B, "smax_select", ci);

						ci->replaceAllUsesWith(res);

						//Set up loop variable, so the next loop will check and possibly expand newCall
						--instructionIterator;
						advance = false;
						assert(&*instructionIterator == res);

						ci->eraseFromParent();
						continue;
					}
					if(II == Intrinsic::umul_with_overflow)
					{
						if (!llcPass)
							continue;

						Value* A = ci->getOperand(0);
						Value* B = ci->getOperand(1);

						assert(A->getType()->isIntegerTy());
						Value* mul = BinaryOperator::CreateMul(A, B, "umul_with_overflow_mul", ci);
						// To check for overflow:
						// If A is zero there can be no overflow.
						// If c divided by a is not equal to b, there was overflow.
						Value* firstCondition = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_NE, A, Constant::getNullValue(A->getType()), "umul_with_overflow_check1", ci);
						Value* CoverA = BinaryOperator::CreateUDiv(mul, A, "", ci);
						Value* secondCondition = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_NE, CoverA, B, "umul_with_overflow_check2", ci);
						Value* overflow = BinaryOperator::CreateAnd(firstCondition, secondCondition, "umul_with_overflow_overflow", ci);

						SmallVector<Instruction*, 16> deleteList;
						for (User* U: ci->users())
						{
							assert(isa<ExtractValueInst>(U) || isa<InsertValueInst>(U));
							if (ExtractValueInst* EVI = dyn_cast<ExtractValueInst>(U))
							{
								ArrayRef<unsigned> indices = EVI->getIndices();
								if (indices[0] == 0)
									U->replaceAllUsesWith(mul);
								else
									U->replaceAllUsesWith(overflow);
								deleteList.push_back(cast<Instruction>(EVI));
							}
							else if (InsertValueInst* IVI = dyn_cast<InsertValueInst>(U))
							{
								// Create an aggregate with the original value and the one being inserted.
								ArrayRef<unsigned> indices = IVI->getIndices();
								Value *newAggregate = UndefValue::get(IVI->getType());
								if (indices[0] == 0)
								{
									newAggregate = InsertValueInst::Create(newAggregate, IVI->getInsertedValueOperand(), { 0 }, "", ci);
									newAggregate = InsertValueInst::Create(newAggregate, overflow, { 1 }, "", ci);
								}
								else
								{
									newAggregate = InsertValueInst::Create(newAggregate, mul, { 0 }, "", ci);
									newAggregate = InsertValueInst::Create(newAggregate, IVI->getInsertedValueOperand(), { 1 }, "", ci);
								}
								U->replaceAllUsesWith(newAggregate);
								deleteList.push_back(cast<Instruction>(EVI));
							}
						}

						for (Instruction* I: deleteList)
							I->eraseFromParent();

						//Set up loop variable, so the next loop will check and possibly expand newCall
						--instructionIterator;
						advance = false;

						ci->eraseFromParent();
					}

					// Replace math intrinsics with C library calls if necessary
					if(llcPass)
					{
						const auto& builtin = TypedBuiltinInstr::getMathTypedBuiltin(*calledFunc);

						if (builtin == TypedBuiltinInstr::UNSUPPORTED)
						{
							llvm::errs() << calledFunc->getName() << " is not supported\n";
							llvm_unreachable("Unsupported builtin can not be lowered\n");
						}

						if (mathMode != NO_BUILTINS)
							continue;

						if (builtin == TypedBuiltinInstr::NONE)
							continue;

						if (TypedBuiltinInstr::isAlwaysExactNatively(builtin))
							continue;

						Function* F = module.getFunction(functionName(builtin));
						assert(F);
						ci->setCalledFunction(F);
					}
				}
			}
		}
	}

	DenseSet<const Function*> droppedMathBuiltins;

	// Drop the code for math functions that will be replaced by builtins
	if (mathMode != NO_BUILTINS && llcPass)
	{
		for (Function& F : module.getFunctionList())
		{
			//Builtins could be inserted in the function table, for now just avoid to drop them
			if (F.hasAddressTaken())
				continue;

			const auto builtinID = BuiltinInstr::getMathBuiltin(F);
			if (cheerp::BuiltinInstr::isValidJSMathBuiltin(builtinID))
			{
				//Preserve sinf and cosf if in WASM modality
				if (mathMode == WASM_BUILTINS &&
					(BuiltinInstr::COS_F == builtinID ||
					BuiltinInstr::SIN_F == builtinID))
					continue;

				F.deleteBody();
				droppedMathBuiltins.insert(&F);
			}
		}
	}

	//Compile the list of JS methods
	for (auto record : getJsExportRecords(module))
		visitStruct(record.getType());
	for (auto function : getJsExportFunctions(module))
	{
		extendLifetime(function.getFunction());
		if (function.getFunction()->getSection() == "asmjs")
			asmJSExportedFunctions.insert(function.getFunction());
	}
	if (WasmSharedModule)
	{
		auto ShouldBeExported = [](const GlobalValue& GV)
		{
			return GV.hasDefaultVisibility() && GV.hasExternalLinkage();
		};
		for(Function& f: module)
		{
			if(!ShouldBeExported(f))
				continue;
			if(f.empty())
				continue;
			assert(f.getSection() == "asmjs");
			extendLifetime(&f);
			asmJSExportedFunctions.insert(&f);
		}
		for(GlobalAlias& a: make_early_inc_range(module.aliases()))
		{
			// Cleanup aliases that are not exported along the way, or we
			// might leave around uses of functions which are otherwise deleted.
			if(!ShouldBeExported(a))
			{
				a.eraseFromParent();
				continue;
			}
			GlobalValue* aliasee = cast<GlobalValue>(a.getAliasee()->stripPointerCastsSafe());
			if(Function* f = dyn_cast<Function>(aliasee))
			{
				if(f->empty())
				{
					a.eraseFromParent();
					continue;
				}
				assert(f->getSection() == "asmjs");
				extendLifetime(f);
				asmJSExportedAliases.insert(&a);
			}
		}
	}
	for (NamedMDNode & namedNode : module.named_metadata() )
	{
		StringRef name = namedNode.getName();
		if(name.endswith("_bases"))
		{
			MDNode* basesMeta = namedNode.getOperand(0);
			assert(basesMeta->getNumOperands()>=1);
			uint32_t firstBase = cast<ConstantInt>(cast<ConstantAsMetadata>(basesMeta->getOperand(0))->getValue())->getZExtValue();
			StructType * t = StructType::getTypeByName(module.getContext(), name.drop_back(6));
			if (t)
				basesInfo.emplace(t, firstBase);
		}
	}

	llvm::Function* startFunc = module.getFunction("_start");
	if (startFunc)
	{
		// Webmain entry point
		extendLifetime(startFunc);
		if (startFunc->getSection() == "asmjs")
			asmJSExportedFunctions.insert(startFunc);
		else if (LinearOutput == LinearOutputTy::Wasm && LowerAtomics)
		{
			// Because memory_init is empty, it will not be automatically tagged as asmjs
			// when the _start function is not, but it should. So we do it manually.
			llvm::Function* initFunc = module.getFunction("__memory_init");
			assert(initFunc);
			asmJSExportedFunctions.insert(initFunc);
		}
	}
	else
	{
		llvm::errs() << "warning: _start function point not found\n";
	}
	entryPoint = startFunc;

	bool isWasi = Triple(module.getTargetTriple()).getOS() == Triple::WASI;

	// If -pthread is linked in, and this is not WASI mode, keep the _startPreThread function alive.
	if (!LowerAtomics && !isWasi)
	{
		llvm::Function* startPreThread = module.getFunction("_startPreThread");
		if (startPreThread)
		{
			extendLifetime(startPreThread);
			if (startPreThread->getSection() == "asmjs")
				asmJSExportedFunctions.insert(startPreThread);
			else if (LinearOutput == LinearOutputTy::Wasm)
			{
				llvm::Function* initFunc = module.getFunction("__memory_init");
				assert(initFunc);
				asmJSExportedFunctions.insert(initFunc);
			}
		}
		else
			llvm::errs() << "warning: _startPreThread function point not found, and -pthread is linked\n";
	}

	processEnqueuedFunctions();

	// Flush out all functions
	processEnqueuedFunctions();

	bool isWasmTarget = Triple(module.getTargetTriple()).isCheerpWasm();
	// Create a dummy function that prevents nullptr conflicts.
	if(isWasmTarget && anyWasmFuncAddrTaken)
		createNullptrFunction(module);

	// Set the sret slot in the asmjs section if there is asmjs code
	GlobalVariable* Sret = module.getGlobalVariable("cheerpSretSlot");
	if (Sret)
	{
		if (isWasmTarget)
			Sret->setSection(StringRef("asmjs"));
		if (llcPass && !Sret->hasInitializer())
			Sret->setInitializer(ConstantInt::get(Type::getInt32Ty(module.getContext()), 0));
	}

	// Create a global slot to bitcast float <-> int.
	// It will only be needed for asmjs code.
	if (!llcPass)
	{
		Type* SlotType = ArrayType::get(IntegerType::get(module.getContext(), 8), 8);
		GlobalVariable* BitCastSlot = cast<GlobalVariable>(module.getOrInsertGlobal("cheerpBitCastSlot", SlotType));
		assert(!BitCastSlot->hasInitializer() && "cheerpBitCastSlot already defined");
		BitCastSlot->setSection("asmjs");
		BitCastSlot->setInitializer(ConstantAggregateZero::get(BitCastSlot->getValueType()));
		BitCastSlot->setAlignment(Align(8));
		extendLifetime(BitCastSlot);
		SubExprVec vec;
		visitGlobal(BitCastSlot, visited, vec );
		// Ensure external linkage to prevent GlobalOpts to remove it
		externals.push_back(BitCastSlot);
	}

	auto markAsReachableIfPresent = [this, &visited](Function* F)
	{
		if (F) {
			visitFunction(F, visited);
			assert(visited.empty());
			reachableGlobals.insert(F);
		}
	};

	// Mark the __wasm_nullptr as reachable.
	llvm::Function* wasmNullptr = module.getFunction(StringRef(wasmNullptrName));
	markAsReachableIfPresent(wasmNullptr);

	// Mark the 64-bit division functions as reachable if we are in opt.
	if (!llcPass)
	{
		markAsReachableIfPresent(module.getFunction("__modti3"));
		markAsReachableIfPresent(module.getFunction("__umodti3"));
		markAsReachableIfPresent(module.getFunction("__divti3"));
		markAsReachableIfPresent(module.getFunction("__udivti3"));
	}

	// If libc exit isn't called, remove global destructors.
	Function* exitFunction = module.getFunction("exit");
	if (!llcPass && (exitFunction == nullptr || !isReachable(module.getFunction("exit"))))
		removeGlobalDestructors(module);

	NumRemovedGlobals = filterModule(droppedMathBuiltins, module);

	if(hasUndefinedSymbolErrors)
		llvm::report_fatal_error("Strict linking enabled and undefined symbols found");

	// Detect all used math builtins
	if (mathMode != NO_BUILTINS && llcPass)
	{
		// We have already dropped all unused functions, so we can simply check if these exists
		for(const Function& F: module)
		{
			const auto builtinID = BuiltinInstr::getMathBuiltin(F);

			if (cheerp::BuiltinInstr::isValidJSMathBuiltin(builtinID))
				if (mathMode != WASM_BUILTINS || !TypedBuiltinInstr::isWasmIntrinsic(&F))
					hasBuiltin[builtinID] = true;
		}
		if (mathMode == WASM_BUILTINS)
		{
			//In Wasm, these are better implemented as function calls than JS builtins
			hasBuiltin[BuiltinInstr::COS_F] = false;
			hasBuiltin[BuiltinInstr::SIN_F] = false;
		}
	}
	// Detect all used non-math builtins
	for(const Function& F: module)
	{
		if(F.getIntrinsicID() == Intrinsic::cheerp_grow_memory && !isWasi)
		{
			hasBuiltin[BuiltinInstr::GROW_MEM] = true;
		}
		else if(F.getIntrinsicID() == Intrinsic::copysign && mathMode == JS_BUILTINS)
		{
			hasBuiltin[BuiltinInstr::ABS_F] = true;
		}
	}

	//Build the map of existing functions types that are called indirectly to their representative (or nullptr if multiple representative exist)
	struct FunctionData
	{
		Function* F;
		bool directlyUsed;
		FunctionData(Function* F, bool directlyUsed):F(F),directlyUsed(directlyUsed)
		{
		}
	};
	struct IndirectFunctionsData
	{
		std::vector<FunctionData> funcs;
		std::vector<CallBase*> indirectCallSites;
		bool signatureUsed;
		IndirectFunctionsData():signatureUsed(false)
		{
		}
	};

	auto isSingleUnreachable = [](const llvm::Function& F) -> bool
	{
		if (F.getInstructionCount() != 1)
			return false;

		for (const llvm::BasicBlock& BB : F)
			for (const llvm::Instruction& I : BB)
				if (!isa<UnreachableInst>(I))
					return false;

		return true;
	};

	std::vector<Function*> toUnreachable;
	std::unordered_map<FunctionType*, IndirectFunctionsData, LinearMemoryHelper::FunctionSignatureHash, LinearMemoryHelper::FunctionSignatureCmp>
		validIndirectCallTypesMap(10, LinearMemoryHelper::FunctionSignatureHash(/*isStrict*/!llcPass), LinearMemoryHelper::FunctionSignatureCmp(/*isStrict*/!llcPass));
	std::unordered_set<FunctionType*, LinearMemoryHelper::FunctionSignatureHash, LinearMemoryHelper::FunctionSignatureCmp>
		validTargetOfIndirectCall(10, LinearMemoryHelper::FunctionSignatureHash(/*isStrict*/!llcPass), LinearMemoryHelper::FunctionSignatureCmp(/*isStrict*/!llcPass));
	for (Function& F : module.getFunctionList())
	{
		if (F.getSection() != StringRef("asmjs"))
			continue;

		//If a given function is composed only of unreachable instructions, it can be excluded from consideration as possible call_indirect target
		if (isSingleUnreachable(F))
			continue;

		// Similar logic to hasAddressTaken, but we also need to find out if there is any direct use
		bool hasIndirectUse = false;
		bool hasDirectUse = F.hasExternalLinkage();
		for (const Use &U : F.uses())
		{
			if(hasDirectUse && hasIndirectUse)
				break;
			const User *FU = U.getUser();
			if (!isa<CallInst>(FU) && !isa<InvokeInst>(FU))
			{
				hasIndirectUse = true;
				continue;
			}
			const CallBase* CS = cast<CallBase>(FU);
			if (CS->isCallee(&U))
			{
				hasDirectUse = true;
			}
			else
			{
				hasIndirectUse = true;
			}
		}

		if(!hasIndirectUse)
			continue;
		validIndirectCallTypesMap[F.getFunctionType()].funcs.emplace_back(&F, hasDirectUse);
	}

	//Check agains the previous set what CallInstruction are actually impossible (and remove them)
	std::vector<llvm::CallBase*> unreachList;
	std::vector<std::pair<llvm::CallBase*, llvm::Function*> > devirtualizedCalls;

	//Fixing function casts implies that new functions types will be created
	//Exporting the table implies that functions can be added outside of our control
	if (!FixWrongFuncCasts && !WasmExportedTable)
	{
		for (Function& F : module.getFunctionList())
		{
			if (F.getSection() != StringRef("asmjs"))
				continue;
			for (BasicBlock& bb : F)
			{
				for (Instruction& I : bb)
				{
					CallBase* ci = dyn_cast<CallBase>(&I);
					if (!ci || ci->isInlineAsm())
						continue;
					Value* calledValue = ci->getCalledOperand();
					if (isa<Function>(calledValue))
						continue;
					//This is an indirect call, and we can check whether the called function type exist at all
					auto it = validIndirectCallTypesMap.find(ci->getFunctionType());
					if (it == validIndirectCallTypesMap.end())
					{
						// There is no indirectly used function with the signature, the code must be unreachable
						unreachList.push_back(ci);
						break;
					}
					else if(it->second.funcs.size() == 1)
					{
						// For this signature there is only one indirectly use function, we can devirtualize it
						assert(ci->getCalledFunction() == nullptr);
						assert(!isa<Function>(ci->getCalledOperand()));
						llvm::Function* toBeCalledFunc = it->second.funcs[0].F;
						llvm::Constant* devirtualizedCall = toBeCalledFunc;
						if(devirtualizedCall->getType() != calledValue->getType())
							devirtualizedCall = ConstantExpr::getBitCast(devirtualizedCall, calledValue->getType());
						ci->setCalledOperand(devirtualizedCall);

						// Always an asmjs functions, thus it is ok to perform ptr to int conversions
						replaceCallOfBitCastWithBitCastOfCall(*ci, false, true);

						devirtualizedCalls.push_back({ci, toBeCalledFunc});
					}
					else
					{
						it->second.indirectCallSites.push_back(ci);
						validTargetOfIndirectCall.insert(it->second.funcs[0].F->getFunctionType());
					}
					it->second.signatureUsed = true;
				}
			}
		}
		// Apply the argument in reverse, if there is no call with a given signature we can drop the functions
		for(auto& it: validIndirectCallTypesMap)
		{
			if(it.second.signatureUsed)
				continue;
			// This signature was never used, we can drop corresponding methods if they have no direct call either
			for(auto& fIt: it.second.funcs)
			{
				if(fIt.directlyUsed)
					continue;
				toUnreachable.push_back(fIt.F);
			}
		}
	}

	//Loop over every possible call site (either direct or indirect that matches the signature)
	//and check whether it happens to be that a given argument is always the same global (while skipping over UndefValues)
	for (auto pair : validIndirectCallTypesMap)
	{
		if (pair.second.indirectCallSites.empty())
			continue;

		//The function can be variadic and so take a variable number of arguments in the call sites
		//but up to numArgs we can find commonality and substitute them directly in the function
		const uint32_t numArgs = pair.first->getNumParams();

		//Check (only once for the whole group of indirect functions) all possible indirect call sites
		std::vector<Value*> constantArgs(numArgs, nullptr);
		std::vector<bool> toReplaceInIndirectCalls(numArgs, true);
		for (uint32_t numArg=0; numArg<numArgs; numArg++)
		{
			Value* curr = pair.second.indirectCallSites[0]->getArgOperand(numArg);

			for (auto& ci : pair.second.indirectCallSites)
			{
				Value* V = ci->getArgOperand(numArg);
				if (curr && isa<UndefValue>(curr))
					curr = V;
				if (V != curr)
					curr = nullptr;
			}

			if (curr && isa<Constant>(curr))
				constantArgs[numArg] = curr;
		}

		//Now constantArg[0...numARgs] is either nullptr or the Constant to be substituted

		for (auto& x : pair.second.funcs)
		{
			std::vector<CallBase*> directCalls;

			//Collect all relevant direct call sites
			for (Use &U : x.F->uses())
			{
				User *FU = U.getUser();
				if (!isa<CallInst>(FU) && !isa<InvokeInst>(FU))
					continue;
				CallBase* CS = cast<CallBase>(FU);
				if (CS->isCallee(&U))
				{
					directCalls.push_back(CS);
				}
			}

			for (uint32_t numArg=0; numArg<numArgs; numArg++)
			{
				Value* toSubstitute = constantArgs[numArg];

				//Check if all direct call sites have always the same constant
				for (auto& ci : directCalls)
				{
					Value * V = ci->getArgOperand(numArg);

					if (toSubstitute && isa<UndefValue>(toSubstitute))
						toSubstitute = V;
					if (V != toSubstitute)
						toSubstitute = nullptr;
				}

				auto Arg = x.F->arg_begin();
				Arg += numArg;

				if (toSubstitute && isa<UndefValue>(toSubstitute))
					toSubstitute = UndefValue::get(Arg->getType());

				if (toSubstitute && isa<Constant>(toSubstitute) && Arg->getType() == toSubstitute->getType())
				{
					//Change every use of the Argument to the relevant Constant
					Arg->replaceAllUsesWith(toSubstitute);

					//Change in every direct call site the Constant with UndefValue
					for (auto& ci : directCalls)
					{
						Value * V = ci->getArgOperand(numArg);
						ci->setArgOperand(numArg, UndefValue::get(V->getType()));
					}
				}

				if (!Arg->user_empty())
					toReplaceInIndirectCalls[numArg] = false;
			}
		}

		for (uint32_t numArg=0; numArg<numArgs; numArg++)
		{
			if (!toReplaceInIndirectCalls[numArg])
				continue;

			//If the common constant has been substituted in all functions of the group, we can as well change the operand to UndefValue
			for (auto& ci : pair.second.indirectCallSites)
			{
				Value* V = ci->getArgOperand(numArg);
				ci->setArgOperand(numArg, UndefValue::get(V->getType()));
			}
		}
	}

	std::unordered_set<llvm::Function*> modifiedFunctions;

	//Processing has to be done in reverse, so that multiple unreachable callInst in the same BasicBlock are processed from the last to the first
	//This avoid erasing the latter ones while processing the first
	for (CallBase* ci : reverse(unreachList))
	{
		modifiedFunctions.insert(ci->getParent()->getParent());
		llvm::changeToUnreachable(ci, /*UseTrap*/false);
	}

	for (auto f : toUnreachable)
	{
		// Replace the body with a single unreachable instruction
		// We need this placeholder to properly satisfy code that wants a non-zero address for this function
		f->deleteBody();
		llvm::BasicBlock* unreachableBlock = llvm::BasicBlock::Create(f->getContext(), "", f);
		new llvm::UnreachableInst(unreachableBlock->getContext(), unreachableBlock);
	}

	//Clean up unreachable blocks
	for (Function* F : modifiedFunctions)
	{
		removeUnreachableBlocks(*F);
	}

	std::vector<Function*> toBeSubstitutedIndirectUses;

	if (!llcPass)
	{
		for (Function& F : module.getFunctionList())
		{
			if (F.getSection() != StringRef("asmjs"))
				continue;

			if (reachableGlobals.count(&F))
				continue;

			if (F.getLinkage() != GlobalValue::InternalLinkage)
				continue;

			if (validTargetOfIndirectCall.count(F.getFunctionType()))
				continue;

			//If we are here it's an amsjs function, not reachable from genericjs, with internal linking and no valid indirect calls
			toBeSubstitutedIndirectUses.push_back(&F);
		}
	}

	for (Function* F : toBeSubstitutedIndirectUses)
	{
		std::vector<Use*> indirectUses;
		for (Use &U : F->uses())
		{
			const User *FU = U.getUser();
			if (!isa<CallInst>(FU) && !isa<InvokeInst>(FU))
			{
				indirectUses.push_back(&U);
				continue;
			}
			const CallBase* CS = cast<CallBase>(FU);
			if (CS->isCallee(&U))
			{
				//Nothing to do
			}
			else
			{
				indirectUses.push_back(&U);
			}
		}

		if (indirectUses.empty())
		{
			//Nothing to substitute, avoid creating an empty function
			continue;
		}

		//Create an function (with the right type) composed by a single unreachable statement
		Function* placeholderFunc = Function::Create(F->getFunctionType(), F->getLinkage(), F->getName() + "_unreachable", module);
		llvm::BasicBlock* unreachableBlock = llvm::BasicBlock::Create(placeholderFunc->getContext(), "", placeholderFunc);
		new llvm::UnreachableInst(unreachableBlock->getContext(), unreachableBlock);
		placeholderFunc->setSection("asmjs");

		//Visit the function, so it's in the relevant GlobalDepsAnalyzer data structures
		VisitedSet visited;
		visitFunction(placeholderFunc, visited);

		//Visit the indirect uses of the old functions, and change them to use the new (almost empty) function
		replaceSomeUsesWith(indirectUses, placeholderFunc);
	}

	return true;
}

void GlobalDepsAnalyzer::visitGlobal( const GlobalValue * C, VisitedSet & visited, const SubExprVec & subexpr )
{
	// Cycle detector
	if ( !visited.insert(C).second )
	{
		assert( reachableGlobals.count(C) );
		if ( const GlobalVariable * GV = dyn_cast< GlobalVariable >(C) )
		{
			assert( !subexpr.empty() );

			varsFixups.emplace( GV, subexpr );
		}
		return;
	}

	if ( reachableGlobals.insert(C).second )
	{

		if(const GlobalAlias * GA = dyn_cast<GlobalAlias>(C) )
		{
			SubExprVec vec;
			visitGlobal(cast<GlobalValue>(GA->getAliasee()), visited, vec );
		}
		else if (const Function * F = dyn_cast<Function>(C) )
		{
			enqueueFunction(F);
		}
		else if (const GlobalVariable * GV = dyn_cast<GlobalVariable>(C) )
		{
			if (GV->hasInitializer() )
			{
				// Add the "GlobalVariable - initializer" use to the subexpr,
				// in order to being able to get the global variable from the fixup map
				SubExprVec Newsubexpr (1, &GV->getOperandUse(0));
				visitConstant( GV->getInitializer(), visited, Newsubexpr);
				Type* globalType = GV->getInitializer()->getType();
				if(GV->getSection() != StringRef("asmjs"))
					visitType(globalType, /*forceTypedArray*/ false);
			}
			varsOrder.push_back(GV);
		}
	}

	visited.erase(C);
}

void GlobalDepsAnalyzer::visitConstant( const Constant * C, VisitedSet & visited, SubExprVec & subexpr )
{
	if ( const GlobalValue * GV = dyn_cast<GlobalValue>(C) )
		visitGlobal(GV, visited, subexpr);
	else if(const ConstantExpr * CE = dyn_cast<const ConstantExpr>(C))
	{
		for(const Value* V: CE->operands())
		{
			const Constant* C=cast<Constant>(V);
			visitConstant(C, visited, subexpr);
		}
	}
	else if(const ConstantArray* d = dyn_cast<const ConstantArray>(C) )
	{
		assert(d->getType()->getNumElements() == d->getNumOperands());
		
		for (ConstantArray::const_op_iterator it = d->op_begin();it != d->op_end(); ++it)
		{
			assert( isa<Constant>(it) );
			subexpr.push_back( it );
			visitConstant( cast<Constant>(it), visited, subexpr);
			subexpr.pop_back();
		}
	}
	else if(const ConstantStruct* d = dyn_cast<const ConstantStruct>(C) )
	{
		assert(d->getType()->getNumElements() == d->getNumOperands());
		
		for (ConstantArray::const_op_iterator it = d->op_begin();it != d->op_end(); ++it)
		{
			assert( isa<Constant>(it) );
			subexpr.push_back( it );
			visitConstant(cast<Constant>(it), visited, subexpr);
			subexpr.pop_back();
		}
	}
	else if (const ConstantVector* CV = dyn_cast<const ConstantVector>(C))
	{
		assert(CV->getType()->getNumElements() == CV->getNumOperands());

		for (auto it = CV->op_begin(); it != CV->op_end(); it++)
		{
			assert(isa<Constant>(it));
			subexpr.push_back(it);
			visitConstant(cast<Constant>(it), visited, subexpr);
			subexpr.pop_back();
		}
	}
}

void GlobalDepsAnalyzer::visitDynSizedAlloca( llvm::Type* pointedType )
{
	if(TypeSupport::isTypedArrayType(pointedType, forceTypedArrays))
		return;
	else if(pointedType->isPointerTy())
		hasPointerArrays = true;
	else
		arraysNeeded.insert( pointedType );
}

void GlobalDepsAnalyzer::visitFunction(const Function* F, VisitedSet& visited)
{
	VisitedSet NewvisitPath;


	if (F->getIntrinsicID() == Intrinsic::cheerp_throw)
		hasCheerpException = true;

	if(F->hasPersonalityFn())
	{
		hasCheerpException = true;
		SubExprVec Newsubexpr;
		visitConstant(F->getPersonalityFn(), visited, Newsubexpr);
	}
	const Module* module = F->getParent();
	if (F->getName() == "malloc")
	{
		// If we are in opt, there is a chance that a following
		// pass will convert malloc into a calloc, so keep that if we keep malloc
		Function* fcalloc = module->getFunction("calloc");
		if (fcalloc && !llcPass)
		{
			extendLifetime(fcalloc);
		}
	}
	bool isAsmJS = F->getSection() == StringRef("asmjs");
	for ( const BasicBlock & bb : *F )
		for (const Instruction & I : bb)
		{
			for (const Value * v : I.operands() )
			{
				if (const Constant * c = dyn_cast<Constant>(v) )
				{
					SubExprVec Newsubexpr;
					visitConstant(c, NewvisitPath, Newsubexpr);
					assert( NewvisitPath.empty() );
				}
			}

			if ( const AllocaInst* AI = dyn_cast<AllocaInst>(&I) )
			{
				Type* allocaType = AI->getAllocatedType();
				if(!isAsmJS)
					visitType(allocaType, forceTypedArrays);
			}
			else if ( const CallBase* CB = dyn_cast<CallBase>(&I) )
			{
				DynamicAllocInfo ai (CB, DL, forceTypedArrays);
				if ( !isAsmJS && ai.isValidAlloc() && !TypeSupport::isAsmJSPointed(ai.getCastedPointedType()))
				{
					if ( ai.useCreatePointerArrayFunc() )
						hasPointerArrays = true;
					else if ( ai.useCreateArrayFunc() )
					{
						if ( ai.getAllocType() == DynamicAllocInfo::cheerp_reallocate )
							arrayResizesNeeded.insert( ai.getCastedPointedType() );
						else
							arraysNeeded.insert( ai.getCastedPointedType() );
					}
					if ( StructType* ST = dyn_cast<StructType>(ai.getCastedPointedType()) )
						visitStruct(ST);
				}
			}
			else if (isa<ResumeInst>(&I))
			{
				extendLifetimeIfPresent(module->getFunction("__cxa_resume"));
			}
			else if (!isAsmJS && I.getOpcode() == Instruction::VAArg)
				hasVAArgs = true;
			// Handle calls from asmjs module to outside and vice-versa
			// and fill the info for the function tables
			if (isa<CallBase>(I))
			{
				const CallBase& ci = cast<CallBase>(I);
				const Function * calledFunc = ci.getCalledFunction();
				// calledFunc can be null, but if the calledValue is a bitcast,
				// this can still be a direct call
				if (calledFunc == nullptr && isBitCast(ci.getCalledOperand()))
				{
					const llvm::User* bc = cast<llvm::User>(ci.getCalledOperand());
					calledFunc = dyn_cast<Function>(bc->getOperand(0));
				}
				// TODO: Handle import/export of indirect calls if possible
				if (!calledFunc)
					continue;
				// Direct call
				if (!calledFunc->isIntrinsic())
				{
					bool calleeIsAsmJS = calledFunc->getSection() == StringRef("asmjs");
					// asm.js function called from outside
					if (calleeIsAsmJS && !isAsmJS && !calledFunc->empty())
						asmJSExportedFunctions.insert(calledFunc);
					// normal function called from asm.js
					else if (!calleeIsAsmJS && isAsmJS)
						asmJSImportedFunctions.insert(calledFunc);
				}
				else if (calledFunc->getIntrinsicID() == Intrinsic::memset)
					extendLifetime(module->getFunction("memset"));
				else if (calledFunc->getIntrinsicID() == Intrinsic::memcpy)
					extendLifetime(module->getFunction("memcpy"));
				else if (calledFunc->getIntrinsicID() == Intrinsic::memmove)
					extendLifetime(module->getFunction("memmove"));
			}
		}
	
	// Gather informations about all the classes which may be downcast targets
	if (F->getIntrinsicID() == Intrinsic::cheerp_downcast)
	{
		Type* elementType = nullptr;
		Type* retType = nullptr;
		for (auto* u : F->users())
		{
			if (const CallBase* CB = dyn_cast<CallBase>(u))
			{
				Type* currElementType = CB->getParamElementType(0);
				if (!elementType)
					elementType = currElementType;
				else
					assert(elementType == currElementType);
				Type* currRetType = CB->getRetElementType();
				if (!retType)
					retType = currRetType;
				else
					assert(retType == currRetType);
			}
		}
		if (!elementType || !retType)
			return;

		// A downcast from a type to i8* is conventially used to support pointers to
		// member functions and does not imply that the type needs the downcast array
		Type* ty = retType->isIntegerTy(8) ? elementType : retType;
		// If both the origin and the target types are i8* (void*), give up on collecting info.
		// This is used in exception handling, and we will collect the info from
		// another syntetic downcast from the thrown type to itself.
		if(ty->isIntegerTy(8))
			return;
		assert(ty->isStructTy());
		
		StructType * st = cast<StructType>(ty);
		
		// We only need metadata for non client objects and if there are bases
		if (TypeSupport::isClientType(retType))
			return;
		do
		{
			if (TypeSupport::hasBasesInfoMetadata(st, *F->getParent()))
			{
				classesWithBaseInfoNeeded.insert(st);
				visitStruct(st);
				break;
			}
		}
		while((st=st->getDirectBase()));
	}
	if (F->getIntrinsicID() == Intrinsic::cheerp_virtualcast)
	{
		Type* elementType = nullptr;
		for (auto* u : F->users())
		{
			if (const CallBase* CB = dyn_cast<CallBase>(u))
			{
				Type* currElementType = CB->getParamElementType(0);
				if (!elementType)
					elementType = currElementType;
				else
					assert(elementType == currElementType);
			}
		}
		if (!elementType)
			return;

		StructType* base = cast<StructType>(elementType);
		if (!base->hasAsmJS())
		{
			std::unordered_map<StructType*, bool> visitedClasses;
			for (const auto& i: module->getIdentifiedStructTypes())
			{
				visitVirtualcastBases(i, base, visitedClasses);
			}
		}
	}
	else if (F->getIntrinsicID() == Intrinsic::cheerp_create_closure)
		hasCreateClosureUsers = true;
}

void GlobalDepsAnalyzer::visitVirtualcastBases(StructType* derived, StructType* base, std::unordered_map<StructType*, bool>& visitedClasses)
{
	if (visitedClasses.count(derived))
		return;
	for (llvm::StructType *direct = derived; direct != nullptr; direct = direct->getDirectBase())
	{
		if (direct == base)
		{
			visitedClasses.emplace(derived, true);
			classesWithBaseInfoNeeded.insert(derived);
			return;
		}
	}
	auto i = basesInfo.find(derived);
	if (i != basesInfo.end())
	{
		for (auto b = derived->element_begin() + i->second; b != derived->element_end(); b++)
		{
			if(!isa<StructType>(*b))
			{
				// Base has collapse to an element which is not a struct, so it cannot contain "base" in it's hierarchy
				continue;
			}
			StructType* st = cast<StructType>(*b);
			visitVirtualcastBases(st, base, visitedClasses);
			if (visitedClasses[st])
			{
				visitedClasses.emplace(derived, true);
				classesWithBaseInfoNeeded.insert(derived);
				return;
			}
		}
	}
	visitedClasses.emplace(derived, false);
}

void GlobalDepsAnalyzer::visitType( Type* t, bool forceTypedArray )
{
	if( ArrayType* AT=dyn_cast<ArrayType>(t) )
	{
		Type* elementType = AT->getElementType();
		if(elementType->isStructTy() && cast<StructType>(elementType)->hasAsmJS())
			return;
		else if(elementType->isPointerTy())
			hasPointerArrays = true;
		else if(!TypeSupport::isTypedArrayType(elementType, forceTypedArray) && AT->getNumElements() > 8)
			arraysNeeded.insert(elementType);
		visitType(elementType, forceTypedArray);
	}
	else if( StructType* ST=dyn_cast<StructType>(t) )
		visitStruct(ST);
}

void GlobalDepsAnalyzer::visitStruct( StructType* ST )
{
	if(ST->hasByteLayout() || ST->hasAsmJS())
		return;
	if (ST->hasName() && ST->getName() == "class._ZN6client15CheerpExceptionE")
		hasCheerpException = true;
	classesNeeded.insert(ST);
	for(uint32_t i=0;i<ST->getNumElements();i++)
		visitType(ST->getElementType(i), /*forceTypedArray*/ false);
}

llvm::StructType* GlobalDepsAnalyzer::needsDowncastArray(llvm::StructType* t) const
{
	// True if the struct or any of its direct bases is used in a downcast
	while(t)
	{
		if(classesWithBaseInfoNeeded.count(t))
			return t;
		t=t->getDirectBase();
	}
	return NULL;
}

void GlobalDepsAnalyzer::logUndefinedSymbol(const GlobalValue* GV)
{
	// Only emit errors during LTO, the idea being that LTO might
	// exploit undefined symbol as a source of information, so
	// it's better to warn the user earlier later than later
	if(llcPass)
		return;
	if(StrictLinking == "warning")
		llvm::errs() << "warning: symbol not defined " << GV->getName() << "\n";
	else if(StrictLinking == "error")
	{
		llvm::errs() << "error: symbol not defined " << GV->getName() << "\n";
		hasUndefinedSymbolErrors = true;
	}
}

bool GlobalDepsAnalyzer::isMathIntrinsic(const llvm::Function* F)
{
	//modf is not properly a builtin, but we can call operator% on floating point in JavaScript
	const auto builtinID = cheerp::BuiltinInstr::getMathBuiltin(*F);
	return cheerp::BuiltinInstr::isValidJSMathBuiltin(builtinID) ||
		(builtinID == BuiltinInstr::MOD_F);
}

void GlobalDepsAnalyzer::removeGlobalDestructors(llvm::Module& M)
{
	// The goal is to empty the function cxa_atexit, and let optimization do the rest.
	Function* cxaAtexit = M.getFunction("__cxa_atexit");
	if (cxaAtexit == nullptr)
		return;

	GlobalValue::LinkageTypes linkage = cxaAtexit->getLinkage();
	cxaAtexit->deleteBody();
	BasicBlock* block = BasicBlock::Create(M.getContext(), "entry", cxaAtexit);
	ConstantInt* ret = ConstantInt::get(Type::getInt32Ty(M.getContext()), 0);
	IRBuilder<> Builder(M.getContext());
	Builder.SetInsertPoint(block);
	Builder.CreateRet(ret);
	cxaAtexit->setLinkage(linkage);
}

int GlobalDepsAnalyzer::filterModule( const DenseSet<const Function*>& droppedMathBuiltins, Module & module )
{
	std::vector< llvm::GlobalValue * > eraseQueue;
	
	// Detach all the global variables, and put the unused ones in the eraseQueue
	for ( Module::global_iterator it = module.global_begin(); it != module.global_end(); )
	{
		GlobalVariable * var = &*it++;
		var->removeFromParent();
		if ( ! isReachable(var) )
			eraseQueue.push_back(var);
		else
		{
			bool isClient = TypeSupport::isClientGlobal(var);
			if( var->hasInitializer() )
			{
				if( !WasmSharedModule && !isClient)
					var->setLinkage(GlobalValue::InternalLinkage);
			}
			else if( !isClient  && var->getName() != "__cxa_cheerp_clause_table")
				logUndefinedSymbol(var);
		}
	}
	
	// Detach all the functions, and put the unused ones in the eraseQueue
	for (Module::iterator it = module.begin(); it != module.end(); )
	{
		Function * f = &*it++;
		if ( !isReachable(f) )
		{
			eraseQueue.push_back(f);
			f->removeFromParent();
		}
		else if( !f->empty() )
		{
			// We need to modify code to enforce correctness
			f->removeFnAttr(Attribute::OptimizeNone);
			// Never internalize functions that may have a better native implementation
			if(TypedBuiltinInstr::isWasmIntrinsic(f) || isMathIntrinsic(f))
			{
				f->setLinkage(GlobalValue::WeakAnyLinkage);
				f->setVisibility(GlobalValue::HiddenVisibility);
			}
			else
				f->setLinkage(GlobalValue::InternalLinkage);
		}
		else if(!droppedMathBuiltins.count(f) && f->getIntrinsicID()==0 &&
			!TypeSupport::isClientFunc(f) && !TypeSupport::isClientConstructorName(f->getName()) &&
			!TypeSupport::isWasiFuncName(f->getName()) &&
			// Special case "free" here, it might be used in genericjs code and lowered by the backend
			f->getName() != "free" &&
			// Special case "__memory_init", it will be populated just before the writer.
			f->getName() != "__memory_init")
		{
			logUndefinedSymbol(f);
		}
	}

	// Put back all the global variables, in the right order
	for ( const GlobalVariable * var : varsOrder )
		module.getGlobalList().push_back( const_cast<GlobalVariable*>(var) );
	
	// Drop all the references from the eraseQueue
	for ( GlobalValue * var : eraseQueue )
	{
		//NOTE yeah.. dropAllReferences is not virtual.
		if ( Function * f = dyn_cast<Function>(var) )
			f->dropAllReferences();
		else
			var->dropAllReferences();
	}
	
	// Remove dead constant users
	for ( GlobalValue * var : eraseQueue )
		var->removeDeadConstantUsers();

	// Now we can safely invoke operator delete
	for ( GlobalValue * var : eraseQueue )
	{
		if ( Function * f = dyn_cast<Function>(var) )
			delete f;
		else if ( GlobalAlias * a = dyn_cast<GlobalAlias>(var) )
			delete a;
		else
			delete cast<GlobalVariable>(var);
	}

	for ( GlobalValue * var: externals)
		var->setLinkage(GlobalValue::ExternalLinkage);

	return eraseQueue.size();
}

//Insert Function on execution queue
void GlobalDepsAnalyzer::enqueueFunction(const llvm::Function* F) {
	functionsQueue.push_back(F);
}

//Process Functions in the execution queue
void GlobalDepsAnalyzer::processEnqueuedFunctions() {
	while (!functionsQueue.empty())
	{
		const Function* F = functionsQueue.back();
		functionsQueue.pop_back();
		VisitedSet visited;
		visitFunction(F, visited);
		assert( visited.empty() );
	}
}

void GlobalDepsAnalyzer::insertAsmJSExport(const llvm::Function* F) {
	asmJSExportedFunctions.insert(F);
}

void GlobalDepsAnalyzer::insertAsmJSImport(const llvm::Function* F) {
	asmJSImportedFunctions.insert(F);
}

void GlobalDepsAnalyzer::removeAsmJSImport(const llvm::Function* F) {
	asmJSImportedFunctions.erase(F);
}

void GlobalDepsAnalyzer::insertDynAllocArray(Type* t) {
	arraysNeeded.insert(t);
}

void GlobalDepsAnalyzer::eraseFunction(llvm::Function* F) {
	assert(F && F != entryPoint && "Cound not erase entry point!");

	asmJSExportedFunctions.erase(F);
	asmJSImportedFunctions.erase(F);
	reachableGlobals.erase(F);
}

}

using namespace cheerp;

llvm::PreservedAnalyses GlobalDepsAnalyzerPass::run(Module& M, ModuleAnalysisManager& MAM)
{
	GlobalDepsAnalyzer& GDA = MAM.getResult<GlobalDepsAnalysis>(M).getInner(MAM, data);
	GDA.runOnModule(M);

	PreservedAnalyses PA;
	PA.preserve<GlobalDepsAnalysis>();

	return PA;
}

AnalysisKey GlobalDepsAnalysis::Key;
GlobalDepsAnalyzer* GlobalDepsAnalyzerWrapper::innerPtr{nullptr};

GlobalDepsAnalyzerWrapper GlobalDepsAnalysis::run(Module& M, ModuleAnalysisManager& MAM)
{
	static llvm::Module* modulePtr = nullptr;
	assert(modulePtr != &M);
	modulePtr = &M;
	return GlobalDepsAnalyzerWrapper();
}
