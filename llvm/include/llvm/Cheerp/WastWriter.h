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

#include <sstream>

#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/LinearMemoryHelper.h"
#include "llvm/Cheerp/PointerAnalyzer.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/Support/FormattedStream.h"

namespace cheerp
{

const uint32_t WasmPage = 64*1024;

enum CheerpMode {
	CHEERP_MODE_WASM = 0,
	CHEERP_MODE_WAST = 1,
};

class CheerpWastWriter;

class Section : public std::stringstream {
private:
	uint32_t sectionId;
	const char* sectionName;
	CheerpWastWriter* writer;

public:
	Section(uint32_t sectionId, const char* sectionName, CheerpWastWriter* writer);
	~Section();
};

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
	// Helper class to manage linear memory state
	const LinearMemoryHelper& linearHelper;

	// Codegen custom globals
	uint32_t usedGlobals;
	uint32_t stackTopGlobal;

	// The wasm module heap size
	uint32_t heapSize;

	// If true, the Wast file is loaded using a JavaScript loader. This allows
	// FFI calls to methods outside of the Wast file. When false, write
	// opcode 'unreachable' for calls to unknown functions.
	bool useWastLoader;

public:
	CheerpMode cheerpMode;

private:
	void compileModule();
	void compileTypeSection();
	void compileFunctionSection();
	void compileImportSection();
	void compileTableSection();
	void compileMemoryAndGlobalSection();
	void compileExportSection();
	void compileStartSection();
	void compileElementSection();
	void compileCodeSection();
	void compileDataSection();

	static const char* getTypeString(const llvm::Type* t);
	void compileMethodLocals(std::ostream& code, const llvm::Function& F, bool needsLabel);
	void compileMethodParams(std::ostream& code, const llvm::FunctionType* F);
	void compileMethodResult(std::ostream& code, const llvm::Type* F);
	void compileMethod(std::ostream& code, const llvm::Function& F);
	void compileImport(std::ostream& code, const llvm::Function& F);
	void compileGlobal(const llvm::GlobalVariable& G);
	// Returns true if it has handled local assignent internally
	bool compileInstruction(std::ostream& code, const llvm::Instruction& I);
	void compileGEP(std::ostream& code, const llvm::User* gepInst);
	static const char* getIntegerPredicate(llvm::CmpInst::Predicate p);

	struct WastBytesWriter: public LinearMemoryHelper::ByteListener
	{
		std::ostream& code;
		const CheerpWastWriter& writer;
		WastBytesWriter(std::ostream& code, const CheerpWastWriter& writer)
			: code(code), writer(writer)
		{
		}
		void addByte(uint8_t b) override;
	};

	struct WastGepWriter: public LinearMemoryHelper::GepListener
	{
		CheerpWastWriter& writer;
		std::ostream& code;
		bool first;
		WastGepWriter(CheerpWastWriter& writer, std::ostream& code)
			: writer(writer), code(code), first(true)
		{
		}
		void addValue(const llvm::Value* v, uint32_t size) override;
		void addConst(int64_t v) override;
	};
public:
	llvm::formatted_raw_ostream& stream;
	CheerpWastWriter(llvm::Module& m, llvm::formatted_raw_ostream& s, cheerp::PointerAnalyzer & PA,
			cheerp::Registerize & registerize,
			cheerp::GlobalDepsAnalyzer & gda,
			const LinearMemoryHelper& linearHelper,
			llvm::LLVMContext& C,
			unsigned heapSize,
			bool useWastLoader,
			CheerpMode cheerpMode):
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
		cheerpMode(cheerpMode),
		stream(s)
	{
	}
	void makeWast();
	void compileBB(std::ostream& code, const llvm::BasicBlock& BB);
	void compileDowncast(std::ostream& code, llvm::ImmutableCallSite callV);
	void compileConstantExpr(std::ostream& code, const llvm::ConstantExpr* ce);
	void compileConstant(std::ostream& code, const llvm::Constant* c);
	void compileOperand(std::ostream& code, const llvm::Value* v);
	void compileSignedInteger(std::ostream& code, const llvm::Value* v, bool forComparison);
	void compileUnsignedInteger(std::ostream& code, const llvm::Value* v);
	void encodeInst(uint32_t opcode, const char* name, std::ostream& code);
	void encodeBinOp(const llvm::Instruction& I, std::ostream& code);
	void encodeS32Inst(uint32_t opcode, const char* name, int32_t immediate, std::ostream& code);
	void encodeU32Inst(uint32_t opcode, const char* name, uint32_t immediate, std::ostream& code);
	void encodeU32U32Inst(uint32_t opcode, const char* name, uint32_t i1, uint32_t i2, std::ostream& code);
	void encodePredicate(const llvm::Type* ty, const llvm::CmpInst::Predicate predicate, std::ostream& code);
	void encodeLoad(const llvm::Type* ty, std::ostream& code);
	bool needsPointerKindConversion(const llvm::Instruction* phi, const llvm::Value* incoming);
	void compilePHIOfBlockFromOtherBlock(std::ostream& code, const llvm::BasicBlock* to, const llvm::BasicBlock* from);
};

}
#endif
