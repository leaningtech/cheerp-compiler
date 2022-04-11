//===-- Cheerp/PassRegistry.h - Register Cheerp passes ---==//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2022 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef CHEERP_PASS_REGISTRY_H
#define CHEERP_PASS_REGISTRY_H

#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/CheerpLowerInvoke.h"
#include "llvm/Cheerp/CFGPasses.h"
#include "llvm/Cheerp/TypeOptimizer.h"
#include "llvm/Cheerp/GEPOptimizer.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/ReplaceNopCastsAndByteSwaps.h"
#include "llvm/Cheerp/LinearMemoryHelper.h"
#include "llvm/Cheerp/I64Lowering.h"
#include "llvm/Cheerp/AllocaMerging.h"
#include "llvm/Cheerp/AllocaLowering.h"
#include "llvm/Cheerp/FixIrreducibleControlFlow.h"
#include "llvm/Cheerp/IdenticalCodeFolding.h"
#include "llvm/Cheerp/ByValLowering.h"
#include "llvm/Cheerp/PartialExecuter.h"
#include "llvm/Cheerp/PreExecute.h"
#include "llvm/Cheerp/PointerPasses.h"
#include "llvm/Cheerp/GEPOptimizer.h"
#include "llvm/Cheerp/CFGPasses.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/SourceMaps.h"
#include "llvm/Cheerp/StructMemFuncLowering.h"
#include "llvm/Cheerp/ConstantExprLowering.h"
#include "llvm/Cheerp/InvokeWrapping.h"
#include "llvm/Cheerp/FFIWrapping.h"
#include "llvm/Cheerp/StoreMerging.h"
#include "llvm/Cheerp/CommandLine.h"

namespace cheerp {

class CheerpWritePassImpl : public PassInfoMixin<CheerpWritePassImpl> {
private:
	raw_ostream &Out;
	TargetMachine *TM;
public:
	PreservedAnalyses run(Module& M, ModuleAnalysisManager&);
	CheerpWritePassImpl(raw_ostream &o, TargetMachine* _TM) :
		Out(o), TM(_TM)
	{
		(void)(TM);
	}
	static bool isRequired() {return true;}
};

} //cheerp

#endif

