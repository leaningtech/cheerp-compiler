//===-- GlobalDepsAnalyzer.cpp - Remove unused functions/globals -----------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2015 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "GlobalDepsAnalyzer"
#include <algorithm>
#include "llvm/ADT/Statistic.h"
#include "llvm/Cheerp/CommandLine.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/Cheerp/LinearMemoryHelper.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/ValueSymbolTable.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Transforms/Utils/SimplifyLibCalls.h"

using namespace llvm;

STATISTIC(NumRemovedGlobals, "Number of unused globals which have been removed");

namespace cheerp {

using namespace std;

char GlobalDepsAnalyzer::ID = 0;
const char* wasmNullptrName = "__wasm_nullptr";

const char* GlobalDepsAnalyzer::getPassName() const
{
	return "GlobalDepsAnalyzer";
}

GlobalDepsAnalyzer::GlobalDepsAnalyzer() : ModulePass(ID), DL(NULL),
	TLI(NULL), entryPoint(NULL), hasCreateClosureUsers(false), hasVAArgs(false),
	hasPointerArrays(false), hasAsmJS(false), forceTypedArrays(false)
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

		builder.CreateCall(F);
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
	LibCallSimplifier callSimplifier(*DL, TLI, LibCallReplacer);
	for (Function& F : module.getFunctionList()) {
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
						ci.replaceAllUsesWith(with);
						deleteList.push_back(&ci);
					}

					foundMemset |= calledFunc->getIntrinsicID() == Intrinsic::memset;
					foundMemcpy |= calledFunc->getIntrinsicID() == Intrinsic::memcpy;
					foundMemmove |= calledFunc->getIntrinsicID() == Intrinsic::memmove;
				}
			}
		}
		if (F.getSection() == StringRef("asmjs"))
			hasAsmJS = true;
	}
	for (CallInst* ci : deleteList) {
		ci->eraseFromParent();
	}

	//Compile the list of JS methods
	//Look for metadata which ends in _methods. They are the have the list
	//of exported methods for JS layout classes
	for (NamedMDNode & namedNode : module.named_metadata() )
	{
		StringRef name = namedNode.getName();

		if(name.endswith("_methods") && name.startswith("class._Z"))
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

	NumRemovedGlobals = filterModule(module);

	callGlobalConstructorsOnStart(module, *this);

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
			functionsQueue.push_back(F);
		else if (const GlobalVariable * GV = dyn_cast<GlobalVariable>(C) )
		{
			if (GV->hasInitializer() )
			{
				// Add the "GlobalVariable - initializer" use to the subexpr,
				// in order to being able to get the global variable from the fixup map
				SubExprVec Newsubexpr (1, &GV->getOperandUse(0));
				visitConstant( GV->getInitializer(), visited, Newsubexpr);
				Type* globalType = GV->getInitializer()->getType();
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
				visitType(allocaType, forceTypedArrays);
			}
			else if ( ImmutableCallSite(&I).isCall() || ImmutableCallSite(&I).isInvoke() )
			{
				DynamicAllocInfo ai (&I, DL, forceTypedArrays);
				if ( ai.isValidAlloc() )
				{
					if ( ai.useCreateArrayFunc() )
						arraysNeeded.insert( ai.getCastedType()->getElementType() );
					if ( ai.useCreatePointerArrayFunc() )
						hasPointerArrays = true;
					if ( StructType* ST = dyn_cast<StructType>(ai.getCastedType()->getElementType()) )
						visitStruct(ST);
				}
			}
			if (I.getOpcode() == Instruction::VAArg)
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
					if (calleeIsAsmJS && !isAsmJS)
						asmJSExportedFuncions.insert(calledFunc);
					// normal function called from asm.js (exclude client globals for now)
					else if (!calleeIsAsmJS && isAsmJS && !TypeSupport::isClientGlobal(calledFunc))
						asmJSImportedFuncions.insert(calledFunc);
				}
				// if this is an allocation intrinsic and we are in asmjs,
				// visit the corresponding libc function. The same applies if the allocated type is asmjs.
				else if (calledFunc->getIntrinsicID() == Intrinsic::cheerp_allocate ||
				    calledFunc->getIntrinsicID() == Intrinsic::cheerp_allocate_array)
				{
					Type* ty = calledFunc->getReturnType();
					bool basicType = !ty->isAggregateType();
					if (hasAsmJS && (isAsmJS || basicType))
					{
						Function* fmalloc = module->getFunction("malloc");
						if (fmalloc)
						{
							SubExprVec vec;
							visitGlobal(fmalloc, visited, vec );
							if(!isAsmJS)
								asmJSExportedFuncions.insert(fmalloc);
						}
					}
				}
				else if (calledFunc->getIntrinsicID() == Intrinsic::cheerp_reallocate)
				{
					Type* ty = calledFunc->getReturnType();
					bool basicType = !ty->isAggregateType();
					if (hasAsmJS && (isAsmJS || basicType))
					{
						Function* frealloc = module->getFunction("realloc");
						if (frealloc)
						{
							SubExprVec vec;
							visitGlobal(frealloc, visited, vec );
							if(!isAsmJS)
								asmJSExportedFuncions.insert(frealloc);
						}
					}
				}
				else if (calledFunc->getIntrinsicID() == Intrinsic::cheerp_deallocate)
				{
					Type* ty = ci.getOperand(0)->getType();
					bool basicType = !ty->isAggregateType();
					if (hasAsmJS && (isAsmJS || basicType))
					{
						Function* ffree = module->getFunction("free");
						if (ffree)
						{
							SubExprVec vec;
							visitGlobal(ffree, visited, vec );
							if(!isAsmJS)
								asmJSExportedFuncions.insert(ffree);
						}
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
		if(retType->isIntegerTy(8))
			return;
		assert(retType->isStructTy());
		
		StructType * st = cast<StructType>(retType);
		
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
			assert(isa<StructType>(*b));
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
		if(elementType->isPointerTy())
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

int GlobalDepsAnalyzer::filterModule( llvm::Module & module )
{
	std::vector< llvm::GlobalValue * > eraseQueue;
	
	// Detach all the global variables, and put the unused ones in the eraseQueue
	for ( Module::global_iterator it = module.global_begin(); it != module.global_end(); )
	{
		GlobalVariable * var = it++;
		var->removeFromParent();
		if( var->hasInitializer() && var->getName()!="llvm.global_ctors")
			var->setLinkage(GlobalValue::InternalLinkage);
		
		if ( ! isReachable(var) )
			eraseQueue.push_back(var);
	}
	
	// Detach all the functions, and put the unused ones in the eraseQueue
	for (Module::iterator it = module.begin(); it != module.end(); )
	{
		Function * f = it++;
		if( !f->empty() )
		{
			// Never internalize functions that may have a better native implementation
			LibFunc::Func Func;
			if (!TLI->getLibFunc(f->getName(), Func))
				f->setLinkage(GlobalValue::InternalLinkage);
		}
		
		if ( !isReachable(f) )
		{
			eraseQueue.push_back(f);
			f->removeFromParent();
		}
	}

	// Detach only the unreachable aliases
	for (Module::alias_iterator it = module.alias_begin(); it != module.alias_end(); )
	{
		GlobalAlias * GA = it++;
		
		if ( !isReachable(GA) )
		{
			eraseQueue.push_back(GA);
			GA->removeFromParent();
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
		delete var;

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
