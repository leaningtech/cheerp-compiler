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
#include "llvm/Cheerp/LinearMemoryHelper.h"
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
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/DebugInfo.h"
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
	Registerize & registerize;

	llvm::LLVMContext& Ctx;

	GlobalDepsAnalyzer & globalDeps;
	const LinearMemoryHelper& linearHelper;
	std::unordered_map<const llvm::Function*, uint32_t> functionIds;

	// Codegen custom globals
	uint32_t usedGlobals;
	uint32_t stackTopGlobal;

	// The wasm module heap size
	uint32_t heapSize;

	// If true, the Wast file is loaded using a JavaScript loader. This allows
	// FFI calls to methods outside of the Wast file. When false, write
	// opcode 'unreachable' for calls to unknown functions.
	bool useWastLoader;

	static const char* getTypeString(llvm::Type* t);
	void compileMethodLocals(const llvm::Function& F, bool needsLabel);
	void compileMethodParams(const llvm::Function& F);
	void compileMethodResult(const llvm::Function& F);
	void compileMethod(const llvm::Function& F);
	void compileImport(const llvm::Function& F);
	void compileGlobal(const llvm::GlobalVariable& G);
	void compileDataSection();
	// Returns true if it has handled local assignent internally
	bool compileInstruction(const llvm::Instruction& I);
	void compileGEP(const llvm::User* gepInst);
	static const char* getIntegerPredicate(llvm::CmpInst::Predicate p);

	struct WastBytesWriter: public LinearMemoryHelper::ByteListener
	{
		llvm::formatted_raw_ostream& stream;
		WastBytesWriter(llvm::formatted_raw_ostream& stream)
			: stream(stream)
		{
		}
		void addByte(uint8_t b) override;
	};

	struct WastGepWriter: public LinearMemoryHelper::GepListener
	{
		CheerpWastWriter& writer;
		bool first;
		WastGepWriter(CheerpWastWriter& writer):writer(writer),first(true)
		{
		}
		void addValue(const llvm::Value* v, uint32_t size) override;
		void addConst(uint32_t v) override;
	};
public:
	llvm::formatted_raw_ostream& stream;
	CheerpWastWriter(llvm::Module& m, llvm::formatted_raw_ostream& s, cheerp::PointerAnalyzer & PA,
			cheerp::Registerize & registerize,
			cheerp::GlobalDepsAnalyzer & gda,
			const LinearMemoryHelper& linearHelper,
			llvm::LLVMContext& C,
			unsigned heapSize,
			bool useWastLoader):
		module(m),
		targetData(&m),
		currentFun(NULL),
		PA(PA),
		registerize(registerize),
		Ctx(C),
		globalDeps(gda),
		linearHelper(linearHelper),
		usedGlobals(0),
		stackTopGlobal(0),
		heapSize(heapSize),
		useWastLoader(useWastLoader),
		stream(s)
	{
	}
	void makeWast();
	void compileBB(const llvm::BasicBlock& BB);
	void compileDowncast(llvm::ImmutableCallSite callV);
	void compileConstantExpr(const llvm::ConstantExpr* ce);
	void compileConstant(const llvm::Constant* c);
	void compileOperand(const llvm::Value* v);
	void compileSignedInteger(const llvm::Value* v, bool forComparison);
	void compileUnsignedInteger(const llvm::Value* v);
	bool needsPointerKindConversion(const llvm::Instruction* phi, const llvm::Value* incoming);
	void compilePHIOfBlockFromOtherBlock(const llvm::BasicBlock* to, const llvm::BasicBlock* from);
};

}
#endif
