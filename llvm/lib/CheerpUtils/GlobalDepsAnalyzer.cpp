//===-- GlobalDepsAnalyzer.cpp - Remove unused functions/globals -----------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2019 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "GlobalDepsAnalyzer"
#include <algorithm>
#include <llvm/Analysis/OptimizationDiagnosticInfo.h>
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
#include "llvm/Transforms/Utils/Local.h"
#include "llvm/Transforms/Utils/SimplifyLibCalls.h"

using namespace llvm;

STATISTIC(NumRemovedGlobals, "Number of unused globals which have been removed");

namespace cheerp {

using namespace std;

char GlobalDepsAnalyzer::ID = 0;
const char* wasmNullptrName = "__wasm_nullptr";

StringRef GlobalDepsAnalyzer::getPassName() const
{
	return "GlobalDepsAnalyzer";
}

GlobalDepsAnalyzer::GlobalDepsAnalyzer(MATH_MODE mathMode_, bool llcPass, bool wasmStart)
	: ModulePass(ID), hasBuiltin{{false}}, mathMode(mathMode_), DL(NULL),
	  TLI(NULL), entryPoint(NULL), hasCreateClosureUsers(false), hasVAArgs(false),
	  hasPointerArrays(false), hasAsmJS(false), hasAsmJSMalloc(false),
	  mayNeedAsmJSFree(false), llcPass(llcPass), wasmStart(wasmStart), delayPrintf(true),
	  hasUndefinedSymbolErrors(false), forceTypedArrays(false)
{
}

void GlobalDepsAnalyzer::getAnalysisUsage(AnalysisUsage& AU) const
{
	AU.addPreserved<cheerp::PointerAnalyzer>();
	AU.addPreserved<cheerp::Registerize>();

	llvm::ModulePass::getAnalysisUsage(AU);
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

static void callGlobalConstructorsOnStart(llvm::Module& M, GlobalDepsAnalyzer& GDA)
{
	// Determine if a function should be constructed that calls the global
	// constructors on start. The function will not be constructed when there
	// are no global constructors.
	auto constructors = cheerp::ModuleGlobalConstructors(M);
	if (!constructors || constructors->op_begin() == constructors->op_end())
		return;

	// Create the function with the call instructions.
	IRBuilder<> builder(M.getContext());
	auto fTy = FunctionType::get(builder.getVoidTy(), false);
	auto stub = Function::Create(fTy, Function::InternalLinkage, "_start", &M);
	stub->setSection("asmjs");

	auto block = BasicBlock::Create(M.getContext(), "entry", stub);
	builder.SetInsertPoint(block);

	for (auto it = constructors->op_begin(); it != constructors->op_end(); ++it)
	{
		assert(isa<ConstantStruct>(it));
		ConstantStruct* cs = cast<ConstantStruct>(it);
		assert(isa<Function>(cs->getAggregateElement(1)));
		Function* F = cast<Function>(cs->getAggregateElement(1));

		if (F->getSection() != StringRef("asmjs"))
			continue;

		builder.CreateCall(F, {});
	}

	builder.CreateRet(nullptr);
	return;
}

bool GlobalDepsAnalyzer::runOnModule( llvm::Module & module )
{
	DL = &module.getDataLayout();
	assert(DL);
	auto *TLIP = getAnalysisIfAvailable<TargetLibraryInfoWrapperPass>();
	TLI = TLIP ? &TLIP->getTLI() : nullptr;
	assert(TLI);
	VisitedSet visited;

	// Replace calls like 'printf("Hello!")' with 'puts("Hello!")'.
	bool foundMemset = false, foundMemcpy = false, foundMemmove = false;
	std::vector<llvm::CallInst*> deleteList;
	auto LibCallReplacer = [](Instruction *I, Value *With)
	{
		I->replaceAllUsesWith(With);
		I->eraseFromParent();
	};
	OptimizationRemarkEmitter ORE;
	LibCallSimplifier callSimplifier(*DL, TLI, ORE, LibCallReplacer);
	for (Function& F : module.getFunctionList()) {
		F.setPersonalityFn(nullptr);
		bool asmjs = F.getSection() == StringRef("asmjs");
		for (BasicBlock& bb : F)
		{
			for (Instruction& I : bb)
			{
				if (isa<CallInst>(I)) {
					CallInst& ci = cast<CallInst>(I);
					Function* calledFunc = ci.getCalledFunction();

					// Skip indirect calls
					if (calledFunc == nullptr)
						continue;

					if (Value* with = callSimplifier.optimizeCall(&ci)) {
						// Do not introduce further intrinsics now
						if(!isa<IntrinsicInst>(with)) {
							ci.replaceAllUsesWith(with);
							deleteList.push_back(&ci);
							continue;
						}
					}
					unsigned II = calledFunc->getIntrinsicID();

					if(asmjs)
					{
						if(II == Intrinsic::cheerp_allocate)
						{
							Function* F = module.getFunction("malloc");
							assert(F);
							ci.setCalledFunction(F);
							Type* oldType = ci.getType();
							if(oldType != F->getReturnType())
							{
								Instruction* newCast = new BitCastInst(UndefValue::get(F->getReturnType()), oldType, "", ci.getNextNode());
								ci.replaceAllUsesWith(newCast);
								ci.mutateType(F->getReturnType());
								newCast->setOperand(0, &ci);
							}
						}
						else if(II == Intrinsic::cheerp_reallocate)
						{
							Function* F = module.getFunction("realloc");
							assert(F);
							ci.setCalledFunction(F);
							Type* oldType = ci.getType();
							if(oldType != F->getReturnType())
							{
								Instruction* newParamCast = new BitCastInst(ci.getOperand(0), F->getReturnType(), "", &ci);
								ci.setOperand(0, newParamCast);
								Instruction* newCast = new BitCastInst(UndefValue::get(F->getReturnType()), oldType, "", ci.getNextNode());
								ci.replaceAllUsesWith(newCast);
								ci.mutateType(F->getReturnType());
								newCast->setOperand(0, &ci);
							}
						}
						else if(II == Intrinsic::cheerp_deallocate)
						{
							Function* F = module.getFunction("free");
							assert(F);
							ci.setCalledFunction(F);
							Type* oldType = ci.getOperand(0)->getType();
							Type* newType = F->arg_begin()->getType();
							if(oldType != newType)
							{
								Instruction* newCast = new BitCastInst(ci.getOperand(0), newType, "", &ci);
								ci.setOperand(0, newCast);
							}
						}

						foundMemset |= II == Intrinsic::memset;
						foundMemcpy |= II == Intrinsic::memcpy;
						foundMemmove |= II == Intrinsic::memmove;
					}

					if(II == Intrinsic::exp2)
					{
						// Expand this to pow, we can't simply forward to the libc since exp2 is optimized away to the intrinsic itself
						Type* t = ci.getType();
						Function* F = module.getFunction(t->isFloatTy() ? "powf" : "pow");
						Value* newCall = CallInst::Create(F, { ConstantFP::get(t, 2.0), ci.getOperand(0) }, "", &ci);
						ci.replaceAllUsesWith(newCall);
						deleteList.push_back(&ci);
					}

					// Replace math intrinsics with C library calls if necessary
					if(mathMode == NO_BUILTINS)
					{
#define REPLACE_MATH_FUNC(ii, f, d) if(II == ii) { Function* F = module.getFunction(calledFunc->getReturnType()->isFloatTy() ? f : d); assert(F); ci.setCalledFunction(F); }
						REPLACE_MATH_FUNC(Intrinsic::fabs, "fabsf", "fabs");
						REPLACE_MATH_FUNC(Intrinsic::ceil, "ceilf", "ceil");
						REPLACE_MATH_FUNC(Intrinsic::cos, "cosf", "cos");
						REPLACE_MATH_FUNC(Intrinsic::exp, "expf", "exp");
						REPLACE_MATH_FUNC(Intrinsic::floor, "floorf", "floor");
						REPLACE_MATH_FUNC(Intrinsic::log, "logf", "log");
						REPLACE_MATH_FUNC(Intrinsic::pow, "powf", "pow");
						REPLACE_MATH_FUNC(Intrinsic::sin, "sinf", "sin");
						REPLACE_MATH_FUNC(Intrinsic::sqrt, "sqrtf", "sqrt");
#undef REPLACE_MATH_FUNC
					}
				}
			}
		}
	}
	for (CallInst* ci : deleteList) {
		ci->eraseFromParent();
	}

	DenseSet<const Function*> droppedMathBuiltins;

	// Drop the code for math functions that will be replaced by builtins
	if (mathMode == USE_BUILTINS)
	{
#define DROP_MATH_FUNC(x) if(Function* F = module.getFunction(x)) { F->deleteBody(); droppedMathBuiltins.insert(F); }
		DROP_MATH_FUNC("fabs"); DROP_MATH_FUNC("fabsf");
		DROP_MATH_FUNC("acos"); DROP_MATH_FUNC("acosf");
		DROP_MATH_FUNC("asin"); DROP_MATH_FUNC("asinf");
		DROP_MATH_FUNC("atan"); DROP_MATH_FUNC("atanf");
		DROP_MATH_FUNC("atan2"); DROP_MATH_FUNC("atan2f");
		DROP_MATH_FUNC("ceil"); DROP_MATH_FUNC("ceilf");
		DROP_MATH_FUNC("cos"); DROP_MATH_FUNC("cosf");
		DROP_MATH_FUNC("exp"); DROP_MATH_FUNC("expf");
		DROP_MATH_FUNC("floor"); DROP_MATH_FUNC("floorf");
		DROP_MATH_FUNC("log"); DROP_MATH_FUNC("logf");
		DROP_MATH_FUNC("pow"); DROP_MATH_FUNC("powf");
		DROP_MATH_FUNC("sin"); DROP_MATH_FUNC("sinf");
		DROP_MATH_FUNC("sqrt"); DROP_MATH_FUNC("sqrtf");
		DROP_MATH_FUNC("tan"); DROP_MATH_FUNC("tanf");
		DROP_MATH_FUNC("fmod"); DROP_MATH_FUNC("fmodf");
#undef DROP_MATH_FUNC
	}

	//Compile the list of JS methods
	//Look for metadata which ends in _methods. They are the have the list
	//of exported methods for JS layout classes
	for (NamedMDNode & namedNode : module.named_metadata() )
	{
		StringRef name = namedNode.getName();

		if(name.endswith("_methods") && (name.startswith("class._Z") || name.startswith("struct._Z")))
		{
			StructType * t = TypeSupport::getJSExportedTypeFromMetadata(name, module).first;
			visitStruct(t);
		}
		else if(name!="jsexported_methods")
			continue;
		for (const MDNode * node : namedNode.operands() )
		{
			assert( isa<Function>(cast<ConstantAsMetadata>(node->getOperand(0))->getValue()) );
			Function* f = cast<Function>(cast<ConstantAsMetadata>(node->getOperand(0))->getValue());
			
			SubExprVec vec;
			visitGlobal( f, visited, vec );
			assert( visited.empty() );
			externals.push_back(f);
			if (f->getSection() == StringRef("asmjs"))
			{
				asmJSExportedFuncions.insert(f);
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
			StructType * t = module.getTypeByName(name.drop_back(6));
			if (t)
				basesInfo.emplace(t, firstBase);
		}
	}

#define USE_MEMORY_FUNC(var, name) \
	if (var) { \
		llvm::Function* f = module.getFunction(#name); \
		assert(f); \
		SubExprVec vec; \
		visitGlobal(f, visited, vec); \
		assert(visited.empty()); \
		/* Make sure the method is not internalized, otherwise it will be dropped */ \
		externals.push_back(f); \
	}
	USE_MEMORY_FUNC(foundMemset, memset)
	USE_MEMORY_FUNC(foundMemcpy, memcpy)
	USE_MEMORY_FUNC(foundMemmove, memmove)
#undef USE_MEMORY_FUNC

	llvm::Function* webMainOrMain = module.getFunction("_Z7webMainv");
	if (webMainOrMain || (webMainOrMain = module.getFunction("webMain")) ||
		(webMainOrMain = module.getFunction("main")))
	{
		// Webmain entry point
		SubExprVec vec;
		visitGlobal( webMainOrMain, visited, vec );
		assert( visited.empty() );
		externals.push_back(webMainOrMain);
	}
	else
	{
		llvm::errs() << "warning: webMain or main entry point not found\n";
	}
	entryPoint = webMainOrMain;
	
	//Process constructors
	const ConstantArray* constructors = ModuleGlobalConstructors(module);
	// Random things which may go boom
	if (constructors) {
		auto getConstructorPriority = []( const Constant * p ) -> uint32_t 
		{
			assert( isa< ConstantStruct >(p) );
			assert( isa< ConstantInt >(p->getAggregateElement(0u) ) );
			
			return cast<ConstantInt>(p->getAggregateElement(0u) )->getSExtValue();
		};
		
		auto getConstructorFunction = []( const Constant * p ) -> const llvm::Function *
		{
			assert( isa< ConstantStruct >(p) );
			assert( isa< Function >(p->getAggregateElement(1) ) );
			
			return cast<Function>( p->getAggregateElement(1) );
		};
		
		auto getConstructorData = [](const Constant* p) -> const llvm::Constant*
		{
			assert(isa<ConstantStruct>(p) );
			if (!p->getAggregateElement(2))
				return nullptr;
			return p->getAggregateElement(2);
		};
		
		auto constComparator = [&]( const Constant * lhs, const Constant * rhs ) -> bool
		{
			return std::make_pair( getConstructorPriority(lhs), lhs) < 
				std::make_pair( getConstructorPriority(rhs), rhs);
		};
		
		std::set< const Constant *, decltype(constComparator) > requiredConstructors( constComparator );
	
		for (ConstantArray::const_op_iterator it = constructors->op_begin();
		     it != constructors->op_end(); ++it)
		{
			assert( isa<Constant>(it) );
			const Constant * p = cast<Constant>(it);

			requiredConstructors.insert(p);
			SubExprVec vec;
			visitGlobal( getConstructorFunction(p), visited, vec );
			const llvm::Constant* data = getConstructorData(p);
			if (data)
				visitConstant(data, visited, vec);
			assert( visited.empty() );
		}
		
		constructorsNeeded.reserve( requiredConstructors.size() );
		std::transform( requiredConstructors.begin(),
				requiredConstructors.end(),
				std::back_inserter(constructorsNeeded),
				getConstructorFunction );

		auto constructorVar = module.getGlobalVariable("llvm.global_ctors");
		reachableGlobals.insert(constructorVar);
		varsOrder.push_back(constructorVar);
	}
	while (!functionsQueue.empty())
	{
		const Function* F = functionsQueue.back();
		functionsQueue.pop_back();
		visitFunction( F, visited);
		assert( visited.empty() );
	}
	// Detect if the code actually uses printf_float
	delayPrintf = false;
	bool usesFloatPrintf = false;
	for (const GlobalValue* v: printfLikeQueue)
	{
		StringRef n = v->getName();
		if(!usesFloatPrintf && (n == "asnprintf" || n == "asprintf" || n == "dprintf" || n == "fprintf" ||
			n == "vfprintf" || n == "printf" || n == "snprintf" || n == "sprintf" ||
			n == "vasnprintf" || n == "vasprintf" || n == "vdprintf" || n == "vprintf" ||
			n == "vsnprintf" || n == "vsprintf"))
		{
			// In this list only keep non-wide and non-i-prefixed printf functions
			// NOTE: Wide char versions do not have a separate printf_float method
			usesFloatPrintf = true;
		}
		SubExprVec vec;
		visitGlobal(v, visited, vec);
		assert(visited.empty());
	}
	// Erase printf_float body if it is not used
	if(!usesFloatPrintf)
	{
		llvm::Function* printfFloat = module.getFunction("_printf_float");
		if(printfFloat)
		{
			printfFloat->deleteBody();
			printfFloat->replaceAllUsesWith(UndefValue::get(printfFloat->getType()));
		}
	}
	// Flush out all functions
	while (!functionsQueue.empty())
	{
		const Function* F = functionsQueue.back();
		functionsQueue.pop_back();
		visitFunction( F, visited);
		assert( visited.empty() );
	}

	if(mayNeedAsmJSFree)
	{
		Function* ffree = module.getFunction("free");
		if (ffree)
		{
			if(!hasAsmJSMalloc)
			{
				// The symbol is still used around, so keep it but make it empty
				ffree->setSection("");
				ffree->deleteBody();
			}
			else
			{
				hasAsmJS = true;
				asmJSExportedFuncions.insert(ffree);
				externals.push_back(ffree);
				// Visit free and friends
				functionsQueue.push_back(ffree);
				while (!functionsQueue.empty())
				{
					const Function* F = functionsQueue.back();
					functionsQueue.pop_back();
					visitFunction( F, visited);
					assert( visited.empty() );
				}
			}
			reachableGlobals.insert(ffree);
		}
	}

	// Create a dummy function that prevents nullptr conflicts.
	if(hasAsmJS)
		createNullptrFunction(module);

	// Mark the __wasm_nullptr as reachable.
	llvm::Function* wasmNullptr = module.getFunction(StringRef(wasmNullptrName));
	if (wasmNullptr) {
		SubExprVec vec;
		visitGlobal(wasmNullptr, visited, vec);
		assert(visited.empty());
	}

	NumRemovedGlobals = filterModule(droppedMathBuiltins, module);

	if(hasUndefinedSymbolErrors)
		llvm::report_fatal_error("String linking enabled and undefined symbols found");

	// Detect all used math builtins
	if (mathMode == USE_BUILTINS)
	{
		// We have already dropped all unused functions, so we can simply check if these exists
#define CHECK_MATH_FUNC(x, d, f) { hasBuiltin[x ## _F64] = module.getFunction(f) || module.getFunction(d); }
		CHECK_MATH_FUNC(ABS, "fabs", "fabsf");
		CHECK_MATH_FUNC(ACOS, "acos", "acosf");
		CHECK_MATH_FUNC(ASIN, "asin", "asinf");
		CHECK_MATH_FUNC(ATAN, "atan", "atanf");
		CHECK_MATH_FUNC(ATAN2, "atan2", "atan2f");
		CHECK_MATH_FUNC(CEIL, "ceil", "ceilf");
		CHECK_MATH_FUNC(COS, "cos", "cosf");
		CHECK_MATH_FUNC(EXP, "exp", "expf");
		CHECK_MATH_FUNC(FLOOR, "floor", "floorf");
		CHECK_MATH_FUNC(LOG, "log", "logf");
		CHECK_MATH_FUNC(POW, "pow", "powf");
		CHECK_MATH_FUNC(SIN, "sin", "sinf");
		CHECK_MATH_FUNC(SQRT, "sqrt", "sqrtf");
		CHECK_MATH_FUNC(TAN, "tan", "tanf");
#undef CHECK_MATH_FUNC

		// Also look for intrinsics
		for(const Function& F: module)
		{
			uint32_t II = F.getIntrinsicID();
			if(!II)
				continue;
			if(II == Intrinsic::fabs)
				hasBuiltin[ABS_F64] = true;
			else if(II == Intrinsic::ceil)
				hasBuiltin[CEIL_F64] = true;
			else if(II == Intrinsic::cos)
				hasBuiltin[COS_F64] = true;
			else if(II == Intrinsic::exp)
				hasBuiltin[EXP_F64] = true;
			else if(II == Intrinsic::floor)
				hasBuiltin[FLOOR_F64] = true;
			else if(II == Intrinsic::log)
				hasBuiltin[LOG_F64] = true;
			else if(II == Intrinsic::pow)
				hasBuiltin[POW_F64] = true;
			else if(II == Intrinsic::sin)
				hasBuiltin[SIN_F64] = true;
			else if(II == Intrinsic::sqrt)
				hasBuiltin[SQRT_F64] = true;
			else if(II == Intrinsic::ctlz)
				hasBuiltin[CLZ32] = true;
		}
	}
	// Detect all used non-math builtins
	for(const Function& F: module)
	{
		if(F.getIntrinsicID() == Intrinsic::cheerp_grow_memory)
		{
			hasBuiltin[GROW_MEM] = true;
		}
	}

	//Build the map of existing functions types that are called indirectly to their representative (or nullptr if multiple representative exist)
	std::unordered_map<FunctionType*, Function*, LinearMemoryHelper::FunctionSignatureHash<true>, LinearMemoryHelper::FunctionSignatureCmp<true>> validIndirectCallTypesMap;
	for (Function& F : module.getFunctionList())
	{
		if (F.getSection() != StringRef("asmjs"))
			continue;
		if(!F.hasAddressTaken())
			continue;
		auto it = validIndirectCallTypesMap.insert(std::make_pair(F.getFunctionType(), &F));
		if(!it.second)
		{
			// An entry was already there, reset the unique function
			it.first->second = nullptr;
		}
	}

	//Check agains the previous set what CallInstruction are actually impossible (and remove them)
	std::vector<llvm::CallInst*> unreachList;

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
					CallInst* ci = dyn_cast<CallInst>(&I);
					if (!ci)
						continue;
					Value* calledValue = ci->getCalledValue();
					if (!isa<Instruction>(calledValue))
						continue;
					//This is an indirect call, and we can check whether the called function type exist at all
					auto it = validIndirectCallTypesMap.find(ci->getFunctionType());
					if (it == validIndirectCallTypesMap.end())
					{
						// There is no indirectly used function with the signature, the code must be unreachable
						unreachList.push_back(ci);
						break;
					}
					else if(it->second)
					{
						// For this signature there is only one indirectly use function, we can devirtualize it
						assert(ci->getCalledFunction() == nullptr);
						assert(!isa<Function>(ci->getCalledValue()));
						llvm::Constant* devirtualizedCall = it->second;
						if(devirtualizedCall->getType() != calledValue->getType())
							devirtualizedCall = ConstantExpr::getBitCast(devirtualizedCall, calledValue->getType());
						ci->setCalledFunction(devirtualizedCall);
					}
				}
			}
		}
	}

	std::set<llvm::Function*> modifiedFunctions;
	for (CallInst* ci : unreachList)
	{
		modifiedFunctions.insert(ci->getParent()->getParent());
		llvm::changeToUnreachable(ci, /*UseTrap*/false);
	}
	for (Function* F : modifiedFunctions)
	{
		removeUnreachableBlocks(*F);
	}

	// Create the start function only if we have a wasm module without js loader
	if (wasmStart)
		callGlobalConstructorsOnStart(module, *this);

	return true;
}

void GlobalDepsAnalyzer::visitGlobal( const GlobalValue * C, VisitedSet & visited, const SubExprVec & subexpr )
{
	// Delay visiting all printf-like globals, we need to dectect if printf_float is actually used
	if ( delayPrintf && C->hasName() && C->getName().endswith("printf") )
	{
		printfLikeQueue.insert(C);
		return;
	}

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
			if ( C->getName() == StringRef("free") )
			{
				// Don't visit free right now. We do it only if
				// actyally needed at the end
				mayNeedAsmJSFree = true;
			}
			else
			{
				if (C->getName() == StringRef("malloc"))
					hasAsmJSMalloc = true;

				if (C->getSection() == StringRef("asmjs"))
				{
					hasAsmJS = true;
				}
				functionsQueue.push_back(F);
			}
		}
		else if (const GlobalVariable * GV = dyn_cast<GlobalVariable>(C) )
		{
			if (C->getSection() == StringRef("asmjs"))
			{
				hasAsmJS = true;
			}
			if (GV->hasInitializer() )
			{
				// Add the "GlobalVariable - initializer" use to the subexpr,
				// in order to being able to get the global variable from the fixup map
				SubExprVec Newsubexpr (1, &GV->getOperandUse(0));
				visitConstant( GV->getInitializer(), visited, Newsubexpr);
				Type* globalType = GV->getInitializer()->getType();
				if(GV->getSection() != StringRef("asmjs"))
					visitType(globalType, /*forceTypedArray*/ true);
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

	const Module* module = F->getParent();
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
			else if ( ImmutableCallSite(&I).isCall() || ImmutableCallSite(&I).isInvoke() )
			{
				DynamicAllocInfo ai (ImmutableCallSite(&I), DL, forceTypedArrays);
				if ( !isAsmJS && ai.isValidAlloc() )
				{
					assert(!TypeSupport::isAsmJSPointer(ai.getCastedType()));
					if ( ai.useCreatePointerArrayFunc() )
						hasPointerArrays = true;
					else if ( ai.useCreateArrayFunc() )
					{
						if ( ai.getAllocType() == DynamicAllocInfo::cheerp_reallocate )
							arrayResizesNeeded.insert( ai.getCastedType()->getElementType() );
						else
							arraysNeeded.insert( ai.getCastedType()->getElementType() );
					}
					if ( StructType* ST = dyn_cast<StructType>(ai.getCastedType()->getElementType()) )
						visitStruct(ST);
				}
			}
			if (!isAsmJS && I.getOpcode() == Instruction::VAArg)
				hasVAArgs = true;
			// Handle calls from asmjs module to outside and vice-versa
			// and fill the info for the function tables
			if (isa<CallInst>(I))
			{
				const CallInst& ci = cast<CallInst>(I);
				const Function * calledFunc = ci.getCalledFunction();
				// calledFunc can be null, but if the calledValue is a bitcast,
				// this can still be a direct call
				if (calledFunc == nullptr && isBitCast(ci.getCalledValue()))
				{
					const llvm::User* bc = cast<llvm::User>(ci.getCalledValue());
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
						asmJSExportedFuncions.insert(calledFunc);
					// normal function called from asm.js
					else if (!calleeIsAsmJS && isAsmJS)
						asmJSImportedFuncions.insert(calledFunc);
				}
				// if this is an allocation intrinsic and we are in asmjs,
				// visit the corresponding libc function. The same applies if the allocated type is asmjs.
				else if (calledFunc->getIntrinsicID() == Intrinsic::cheerp_allocate ||
				    calledFunc->getIntrinsicID() == Intrinsic::cheerp_allocate_array)
				{
					if (isAsmJS || TypeSupport::isAsmJSPointer(calledFunc->getReturnType()))
					{
						Function* fmalloc = module->getFunction("malloc");
						if (fmalloc)
						{
							SubExprVec vec;
							visitGlobal(fmalloc, visited, vec );
							if(!isAsmJS)
								asmJSExportedFuncions.insert(fmalloc);
							externals.push_back(fmalloc);
							hasAsmJSMalloc = true;
						}
					}
				}
				else if (calledFunc->getIntrinsicID() == Intrinsic::cheerp_reallocate)
				{
					if (isAsmJS || TypeSupport::isAsmJSPointer(calledFunc->getReturnType()))
					{
						Function* frealloc = module->getFunction("realloc");
						if (frealloc)
						{
							SubExprVec vec;
							visitGlobal(frealloc, visited, vec );
							if(!isAsmJS)
								asmJSExportedFuncions.insert(frealloc);
							externals.push_back(frealloc);
							hasAsmJSMalloc = true;
						}
					}
				}
				else if (calledFunc->getIntrinsicID() == Intrinsic::cheerp_deallocate)
				{
					Type* ty = ci.getOperand(0)->getType();
					bool basicType = !ty->isAggregateType();
					bool asmjsPtr = TypeSupport::isAsmJSPointer(ty);
					if (isAsmJS || basicType || asmjsPtr)
					{
						// Delay adding free, it will be done only if asm.js malloc is actually there
						mayNeedAsmJSFree = true;
					}
				}
			}
		}
	
	// Gather informations about all the classes which may be downcast targets
	if (F->getIntrinsicID() == Intrinsic::cheerp_downcast)
	{
		Type* retType = F->getReturnType()->getPointerElementType();
		// A downcast from a type to i8* is conventially used to support pointers to
		// member functions and does not imply that the type needs the downcast array
		Type* ty = retType->isIntegerTy(8) ? F->arg_begin()->getType()->getPointerElementType() : retType;
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
		StructType* base = cast<StructType>(F->getFunctionType()->getParamType(0)->getPointerElementType());
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
		visitType(elementType, /*forceTypedArray*/ false);
	}
	else if( StructType* ST=dyn_cast<StructType>(t) )
		visitStruct(ST);
}

void GlobalDepsAnalyzer::visitStruct( StructType* ST )
{
	if(ST->hasByteLayout() || ST->hasAsmJS())
		return;
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
	// Only emit errors during the final compilation step
	if(!llcPass)
		return;
	if(StrictLinking == "warning")
		llvm::errs() << "warning: symbol not defined " << GV->getName() << "\n";
	else if(StrictLinking == "error")
	{
		llvm::errs() << "error: symbol not defined " << GV->getName() << "\n";
		hasUndefinedSymbolErrors = true;
	}
}

bool GlobalDepsAnalyzer::isMathIntrinsic(StringRef funcName)
{
#define CHECK_MATH_FUNC(x) if(funcName == x) return true;
	CHECK_MATH_FUNC("fabs"); CHECK_MATH_FUNC("fabsf");
	CHECK_MATH_FUNC("acos"); CHECK_MATH_FUNC("acosf");
	CHECK_MATH_FUNC("asin"); CHECK_MATH_FUNC("asinf");
	CHECK_MATH_FUNC("atan"); CHECK_MATH_FUNC("atanf");
	CHECK_MATH_FUNC("ceil"); CHECK_MATH_FUNC("ceilf");
	CHECK_MATH_FUNC("cos"); CHECK_MATH_FUNC("cosf");
	CHECK_MATH_FUNC("exp"); CHECK_MATH_FUNC("expf");
	CHECK_MATH_FUNC("floor"); CHECK_MATH_FUNC("floorf");
	CHECK_MATH_FUNC("log"); CHECK_MATH_FUNC("logf");
	CHECK_MATH_FUNC("pow"); CHECK_MATH_FUNC("powf");
	CHECK_MATH_FUNC("sin"); CHECK_MATH_FUNC("sinf");
	CHECK_MATH_FUNC("sqrt"); CHECK_MATH_FUNC("sqrtf");
	CHECK_MATH_FUNC("tan"); CHECK_MATH_FUNC("tanf");
	CHECK_MATH_FUNC("fmod"); CHECK_MATH_FUNC("fmodf");
#undef CHECK_MATH_FUNC
	return false;
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
		else if( var->hasInitializer() )
		{
			if( var->getName()!="llvm.global_ctors" )
				var->setLinkage(GlobalValue::InternalLinkage);
		}
		else if( !TypeSupport::isClientGlobal(var) )
			logUndefinedSymbol(var);
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
			// Never internalize functions that may have a better native implementation
			if(isWasmIntrinsic(f) || isMathIntrinsic(f->getName()))
				f->setLinkage(GlobalValue::WeakAnyLinkage);
			else
				f->setLinkage(GlobalValue::InternalLinkage);
		}
		else if(!droppedMathBuiltins.count(f) && f->getIntrinsicID()==0 &&
			!TypeSupport::isClientFunc(f) && !TypeSupport::isClientConstructorName(f->getName()) &&
			// Special case "free" here, it might be used in genericjs code and lowered by the backend
			f->getName() != "free")
		{
			logUndefinedSymbol(f);
		}
	}

	// Detach only the unreachable aliases
	for (Module::alias_iterator it = module.alias_begin(); it != module.alias_end(); )
	{
		GlobalAlias * GA = &*it++;
		
		if ( isReachable(GA) )
		{
			if (!llcPass)
				continue;
			// Replace the alias with the actual value
			GA->replaceAllUsesWith( GA->getAliasee() );
		}
		GA->removeFromParent();
		eraseQueue.push_back(GA);
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
		var->deleteValue();

	for ( GlobalValue * var: externals)
		var->setLinkage(GlobalValue::ExternalLinkage);

	return eraseQueue.size();
}

void GlobalDepsAnalyzer::insertAsmJSExport(llvm::Function* F) {
	asmJSExportedFuncions.insert(F);
}

void GlobalDepsAnalyzer::eraseFunction(llvm::Function* F) {
	// TODO getEntryPoint should also be checked.

	auto it = std::find(constructorsNeeded.begin(), constructorsNeeded.end(), F);
	if (it != constructorsNeeded.end())
		constructorsNeeded.erase(it);

	asmJSExportedFuncions.erase(F);
	asmJSImportedFuncions.erase(F);
	reachableGlobals.erase(F);
}

}

using namespace cheerp;

INITIALIZE_PASS_BEGIN(GlobalDepsAnalyzer, "GlobalDepsAnalyzer", "Remove unused globals from the module",
                      false, false)
INITIALIZE_PASS_END(GlobalDepsAnalyzer, "GlobalDepsAnalyzer", "Remove unused globals from the module",
                    false, false)
