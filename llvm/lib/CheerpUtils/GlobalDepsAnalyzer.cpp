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
#include "llvm/ADT/Statistic.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/Support/FormattedStream.h"

using namespace llvm;

STATISTIC(NumRemovedGlobals, "Number of unused globals which have been removed");

namespace cheerp {

using namespace std;

char GlobalDepsAnalyzer::ID = 0;

const char* GlobalDepsAnalyzer::getPassName() const
{
	return "GlobalDepsAnalyzer";
}

GlobalDepsAnalyzer::GlobalDepsAnalyzer() : ModulePass(ID),
	hasCreateClosureUsers(false), hasVAArgs(false), hasPointerArrays(false)
{
}

void GlobalDepsAnalyzer::getAnalysisUsage(AnalysisUsage& AU) const
{
	AU.addPreserved<cheerp::PointerAnalyzer>();
	AU.addPreserved<cheerp::Registerize>();

	llvm::ModulePass::getAnalysisUsage(AU);
}

bool GlobalDepsAnalyzer::runOnModule( llvm::Module & module )
{
	VisitedSet visited;
	
	//Compile the list of JS methods
	//Look for metadata which ends in _methods. They are the have the list
	//of exported methods for JS layout classes
	for (NamedMDNode & namedNode : module.named_metadata() )
	{
		StringRef name = namedNode.getName();

		if(!name.endswith("_methods") ||
		   !name.startswith("class._Z") )
			continue;

		StructType * t = TypeSupport::getJSExportedTypeFromMetadata(name, module).first;
		visitStruct(t);

		for (const MDNode * node : namedNode.operands() )
		{
			assert( isa<Function>(cast<ConstantAsMetadata>(node->getOperand(0))->getValue()) );
			Function* f = cast<Function>(cast<ConstantAsMetadata>(node->getOperand(0))->getValue());
			
			SubExprVec vec;
			visitGlobal( f, visited, vec );
			assert( visited.empty() );
			externals.push_back(f);
		}
	}
	
	Function * webMain = module.getFunction("_Z7webMainv");
	if(!webMain)
	{
		llvm::report_fatal_error("No webMain entry point found", false);
	}
	else
	{
		// Webmain entry point
		SubExprVec vec;
		visitGlobal( webMain, visited, vec );
		assert( visited.empty() );
		externals.push_back(webMain);
	}
	
	//Process constructors
	if (GlobalVariable * constructorVar = module.getGlobalVariable("llvm.global_ctors") )
	{
		// Random things which may go boom
		if ( !constructorVar->hasInitializer() ||
			!isa<ConstantArray>( constructorVar->getInitializer() ) )
			return false;
		
		const ConstantArray * constructors = cast<ConstantArray>( constructorVar->getInitializer() );

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
		
		auto getConstructorVariable = []( const Constant * p ) -> const llvm::GlobalValue *
		{
			assert( isa< ConstantStruct >(p) );
			
			if ( p->getAggregateElement(2) == nullptr || isa<ConstantPointerNull>(p->getAggregateElement(2)) )
				return nullptr;
			assert( isa< GlobalValue >(p->getAggregateElement(2) ) );
			return cast<GlobalValue>(p->getAggregateElement(2) );
		};
		
		auto constComparator = [&]( const Constant * lhs, const Constant * rhs ) -> bool
		{
			return std::make_pair( getConstructorPriority(lhs), lhs) < 
				std::make_pair( getConstructorPriority(rhs), rhs);
		};
		
		std::set< const Constant *, decltype(constComparator) > requiredConstructors( constComparator );
	
		bool Modified = true;
		
		// Do this many times, since each time we add a global we might 
		// have added a globalvariable required by another constructor
		while (Modified)
		{
			Modified = false;
	
			for (ConstantArray::const_op_iterator it = constructors->op_begin();
			     it != constructors->op_end(); ++it)
			{
				assert( isa<Constant>(it) );
				const Constant * p = cast<Constant>(it);
				
				if (requiredConstructors.count(p) )
					continue;
				
				const GlobalValue * var = getConstructorVariable(p);

				if ( nullptr == var  || reachableGlobals.count(var) )
				{
					requiredConstructors.insert(p);
					SubExprVec vec;
					visitGlobal( getConstructorFunction(p), visited, vec );
					assert( visited.empty() );
					
					Modified = true;
				}
			}
		}
		
		constructorsNeeded.reserve( requiredConstructors.size() );
		std::transform( requiredConstructors.begin(),
				requiredConstructors.end(),
				std::back_inserter(constructorsNeeded),
				getConstructorFunction );

		reachableGlobals.insert(constructorVar);
		varsOrder.push_back(constructorVar);
	}
	NumRemovedGlobals = filterModule(module);
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
			visitFunction(F, visited);
		else if (const GlobalVariable * GV = dyn_cast<GlobalVariable>(C) )
		{
			if (GV->hasInitializer() )
			{
				// Add the "GlobalVariable - initializer" use to the subexpr,
				// in order to being able to get the global variable from the fixup map
				SubExprVec Newsubexpr (1, &GV->getOperandUse(0));
				visitConstant( GV->getInitializer(), visited, Newsubexpr);
				Type* globalType = GV->getInitializer()->getType();
				if( ArrayType* AT=dyn_cast<ArrayType>(globalType) )
					globalType = AT->getElementType();
				if( StructType* ST= dyn_cast<StructType>(globalType) )
					visitStruct(ST);
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
				if( ArrayType* AT=dyn_cast<ArrayType>(allocaType) )
					allocaType = AT->getElementType();
				if( StructType* ST= dyn_cast<StructType>(allocaType) )
					visitStruct(ST);
			}
			else if ( ImmutableCallSite(&I).isCall() || ImmutableCallSite(&I).isInvoke() )
			{
				DynamicAllocInfo ai (&I);
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
		}
	
	// Gather informations about all the classes which may be downcast targets
	if (F->getIntrinsicID() == Intrinsic::cheerp_downcast)
	{
		Type* retType = F->getReturnType()->getPointerElementType();
		assert(retType->isStructTy());
		
		StructType * st = cast<StructType>(retType);
		
		// We only need metadata for non client objects and if there are bases
		if (!TypeSupport::isClientType(retType) && TypeSupport::hasBasesInfoMetadata(st, *F->getParent()) )
		{
			classesWithBaseInfoNeeded.insert(st);
			classesNeeded.insert(st);
		}
	}
	else if (F->getIntrinsicID() == Intrinsic::cheerp_create_closure)
		hasCreateClosureUsers = true;
}

void GlobalDepsAnalyzer::visitStruct( StructType* ST )
{
	if(ST->hasByteLayout())
		return;
	classesNeeded.insert(ST);
	for(uint32_t i=0;i<ST->getNumElements();i++)
	{
		Type* elementType = ST->getElementType(i);
		if( ArrayType* AT=dyn_cast<ArrayType>(elementType) )
			elementType = AT->getElementType();
		if( StructType* ST= dyn_cast<StructType>(elementType) )
			visitStruct(ST);
	}
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
			f->setLinkage(GlobalValue::InternalLinkage);
		
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

}

using namespace cheerp;

INITIALIZE_PASS_BEGIN(GlobalDepsAnalyzer, "GlobalDepsAnalyzer", "Remove unused globals from the module",
                      false, false)
INITIALIZE_PASS_END(GlobalDepsAnalyzer, "GlobalDepsAnalyzer", "Remove unused globals from the module",
                    false, false)
