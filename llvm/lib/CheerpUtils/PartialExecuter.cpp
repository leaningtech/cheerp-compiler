//===-- PartialExecuter.cpp - Remove unused functions/globals -----------===//
//
//                     Cheerp: The C++ compiler for the Web
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


using namespace llvm;

//STATISTIC(NumRemovedGlobals, "Number of unused globals which have been removed");

namespace cheerp {

using namespace std;

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
	AU.addPreserved<cheerp::PointerAnalyzer>();
	AU.addPreserved<cheerp::Registerize>();

	llvm::ModulePass::getAnalysisUsage(AU);
}

class PartialInterpreter : public llvm::Interpreter {
public:
	explicit PartialInterpreter(std::unique_ptr<llvm::Module> M)
		: llvm::Interpreter(std::move(M), /*preExecute*/false)
	{
	}
	
//	void visit(llvm::Instruction& I) override
//	{
//		llvm::errs() << I << "\n";
//		if (/*check whether has to be skipped*/false)
//			return;
//		Interpreter::visit(I);
//	}	
};


bool PartialExecuter::runOnModule( llvm::Module & module )
{
	using namespace llvm;

	for (Function& F : module)
	{
		if (F.getName() == "main")
		{
using namespace llvm;
    llvm::ExecutionEngine *currentEE;
    llvm::Module *currentModule;
    std::unique_ptr<Allocator> allocator;
    

std::unique_ptr<Module> uniqM(&module); 
    std::string error; 
        std::string triple = sys::getProcessTriple();
    const Target *target = TargetRegistry::lookupTarget(triple, error);

    TargetMachine* machine = target->createTargetMachine(triple, "", "", TargetOptions(), None); 
 
    EngineBuilder builder(std::move(uniqM)); 
    builder.setEngineKind(llvm::EngineKind::PreExecuteInterpreter); 
    builder.setOptLevel(CodeGenOpt::Default); 
    builder.setErrorStr(&error); 
    builder.setVerifyModules(true); 
 
    currentEE = builder.create(machine); 
    assert(currentEE && "failed to create execution engine!"); 
//    currentEE->InstallStoreListener(StoreListener); 
  //  currentEE->InstallAllocaListener(AllocaListener); 
    //currentEE->InstallRetListener(RetListener); 
 //   currentEE->InstallLazyFunctionCreator(LazyFunctionCreator); 

    allocator = std::make_unique<Allocator>(*currentEE->ValueAddresses);

    currentEE->runFunction(&F, std::vector< GenericValue >(100));
/*
			llvm::errs() << F.getName() << "\n";
    std::unique_ptr<Module> uniqM(&module); 
			llvm::errs() << F.getName() << "\n";
			PartialInterpreter p(std::move(uniqM));
			llvm::errs() << F.getName() << "\n";

			p.runFunction(&F, ArrayRef<GenericValue>())	
*/
		}


		if (F.hasAddressTaken())
			continue;

		if (F.isDeclaration())
			continue;

		llvm::errs() << F.getName() << "\n";
		{
		for (auto& u : F.uses())
			llvm::errs() << *u.getUser() << "\n";
		llvm::errs() << "\n";
		}
	}




	bool changed = false;
	return changed;
}

}

using namespace cheerp;

INITIALIZE_PASS_BEGIN(PartialExecuter, "PartialExecuter", "Partially execute functions",
                      false, false)
INITIALIZE_PASS_END(PartialExecuter, "PartialExecuter", "Partially execute functions",
                    false, false)
