//===-- Cheerp/WastWriter.h - The Cheerp JavaScript generator -----------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_WAST_WRITER_H
#define _CHEERP_WAST_WRITER_H

#if 0
#include "llvm/Analysis/AliasAnalysis.h"
#endif
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/PointerAnalyzer.h"
#include "llvm/Cheerp/Registerize.h"
#if 0
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DebugInfo.h"
#endif
#include "llvm/Support/FormattedStream.h"
#if 0
#include <set>
#include <map>
#include <array>
#endif

namespace cheerp
{

const uint32_t WasmPage = 64*1024;

class CheerpWastWriter
{
private:
	llvm::Module& module;
	llvm::DataLayout targetData;
	const llvm::Function* currentFun;
	const PointerAnalyzer & PA;
	const Registerize & registerize;

	GlobalDepsAnalyzer & globalDeps;

	// Codegen custom globals
	uint32_t usedGlobals;
	uint32_t stackTopGlobal;
	static const char* getTypeString(llvm::Type* t);
	void compileMethodLocals(const llvm::Function& F);
	void compileMethod(const llvm::Function& F);
	void compileGlobal(const llvm::GlobalVariable& G);
	// Returns true if it has handled local assignent internally
	bool compileInstruction(const llvm::Instruction& I);
public:
	llvm::formatted_raw_ostream& stream;
	CheerpWastWriter(llvm::Module& m, llvm::formatted_raw_ostream& s, cheerp::PointerAnalyzer & PA,
			cheerp::Registerize & registerize,
			cheerp::GlobalDepsAnalyzer & gda):
		module(m),
		targetData(&m),
		currentFun(NULL),
		PA(PA),
		registerize(registerize),
		globalDeps(gda),
		usedGlobals(0),
		stackTopGlobal(0),
		stream(s)
	{
	}
	void makeWast();
	void compileBB(const llvm::BasicBlock& BB);
	void compileConstant(const llvm::Constant* c);
	void compileOperand(const llvm::Value* v);
	bool needsPointerKindConversionForBlocks(const llvm::BasicBlock* to, const llvm::BasicBlock* from);
	void compilePHIOfBlockFromOtherBlock(const llvm::BasicBlock* to, const llvm::BasicBlock* from);
};

}
#endif
