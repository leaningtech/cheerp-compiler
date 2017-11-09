//===-- Cheerp/WasmWriter.h - The Cheerp JavaScript generator -----------------===//
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
#include "llvm/Cheerp/NameGenerator.h"
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

class CheerpWasmWriter;

typedef std::iostream WasmBuffer;

class Section : public std::stringstream {
private:
	uint32_t sectionId;
	const char* sectionName;
	CheerpWasmWriter* writer;

public:
	Section(uint32_t sectionId, const char* sectionName, CheerpWasmWriter* writer);
	~Section();
};

class CheerpWasmWriter
{
private:
	llvm::Module& module;
	llvm::DataLayout targetData;
	const llvm::Function* currentFun;
	Registerize & registerize;

	llvm::LLVMContext& Ctx;

	GlobalDepsAnalyzer & globalDeps;
	// Helper class to manage linear memory state
	const LinearMemoryHelper& linearHelper;

	const NameGenerator& namegen;

	// Codegen custom globals
	uint32_t usedGlobals;
	uint32_t stackTopGlobal;

	// The wasm module heap size
	uint32_t heapSize;

	// If true, the Wasm file is loaded using a JavaScript loader. This allows
	// FFI calls to methods outside of the Wasm file. When false, write
	// opcode 'unreachable' for calls to unknown functions.
	bool useWasmLoader;

	// If true, embed a custom section called 'name' in binary wasm that maps
	// the function ids to C++ mangled function names. If available in LLVM IR,
	// it will also add names to local variables inside functions.
	bool prettyCode;

public:
	const PointerAnalyzer & PA;
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
	void compileNameSection();

	static const char* getTypeString(const llvm::Type* t);
	void compileMethodLocals(WasmBuffer& code, const llvm::Function& F, bool needsLabel);
	void compileMethodParams(WasmBuffer& code, const llvm::FunctionType* F);
	void compileMethodResult(WasmBuffer& code, const llvm::Type* F);
	void compileMethod(WasmBuffer& code, const llvm::Function& F);
	void compileImport(WasmBuffer& code, const llvm::Function& F);
	void compileGlobal(const llvm::GlobalVariable& G);
	// Returns true if it has handled local assignent internally
	bool compileInstruction(WasmBuffer& code, const llvm::Instruction& I);
	void compileGEP(WasmBuffer& code, const llvm::User* gepInst);
	static const char* getIntegerPredicate(llvm::CmpInst::Predicate p);

	struct WasmBytesWriter: public LinearMemoryHelper::ByteListener
	{
		std::ostream& code;
		const CheerpWasmWriter& writer;
		WasmBytesWriter(WasmBuffer& code, const CheerpWasmWriter& writer)
			: code(code), writer(writer)
		{
		}
		void addByte(uint8_t b) override;
	};

	struct WasmGepWriter: public LinearMemoryHelper::GepListener
	{
		CheerpWasmWriter& writer;
		WasmBuffer& code;
		bool first;
		int32_t constPart;
		WasmGepWriter(CheerpWasmWriter& writer, WasmBuffer& code)
			: writer(writer), code(code), first(true), constPart(0)
		{
		}
		void addValue(const llvm::Value* v, uint32_t size) override;
		void addConst(int64_t v) override;
	};
public:
	llvm::raw_ostream& stream;
	CheerpWasmWriter(llvm::Module& m, llvm::raw_ostream& s, cheerp::PointerAnalyzer & PA,
			cheerp::Registerize & registerize,
			cheerp::GlobalDepsAnalyzer & gda,
			const LinearMemoryHelper& linearHelper,
			const NameGenerator& namegen,
			llvm::LLVMContext& C,
			unsigned heapSize,
			bool useWasmLoader,
			bool prettyCode,
			CheerpMode cheerpMode):
		module(m),
		targetData(&m),
		currentFun(NULL),
		registerize(registerize),
		Ctx(C),
		globalDeps(gda),
		linearHelper(linearHelper),
		namegen(namegen),
		usedGlobals(0),
		stackTopGlobal(0),
		heapSize(heapSize),
		useWasmLoader(useWasmLoader),
		prettyCode(prettyCode),
		PA(PA),
		cheerpMode(cheerpMode),
		stream(s)
	{
	}
	void makeWasm();
	void compileBB(WasmBuffer& code, const llvm::BasicBlock& BB);
	void compileDowncast(WasmBuffer& code, llvm::ImmutableCallSite callV);
	void compileConstantExpr(WasmBuffer& code, const llvm::ConstantExpr* ce);
	void compileConstant(WasmBuffer& code, const llvm::Constant* c);
	void compileOperand(WasmBuffer& code, const llvm::Value* v);
	void compileSignedInteger(WasmBuffer& code, const llvm::Value* v, bool forComparison);
	void compileUnsignedInteger(WasmBuffer& code, const llvm::Value* v);
	void encodeInst(uint32_t opcode, const char* name, WasmBuffer& code);
	void encodeBinOp(const llvm::Instruction& I, WasmBuffer& code);
	void encodeS32Inst(uint32_t opcode, const char* name, int32_t immediate, WasmBuffer& code);
	void encodeU32Inst(uint32_t opcode, const char* name, uint32_t immediate, WasmBuffer& code);
	void encodeU32U32Inst(uint32_t opcode, const char* name, uint32_t i1, uint32_t i2, WasmBuffer& code);
	void encodePredicate(const llvm::Type* ty, const llvm::CmpInst::Predicate predicate, WasmBuffer& code);
	void compileICmp(const llvm::ICmpInst& ci, const llvm::CmpInst::Predicate p, WasmBuffer& code);
	void encodeLoad(const llvm::Type* ty, uint32_t offset, WasmBuffer& code);
	void encodeWasmIntrinsic(WasmBuffer& code, const llvm::Function* F);
	bool tryEncodeFloatAsInt(WasmBuffer& code, const llvm::ConstantFP* f);
	bool tryEncodeFloat64AsFloat32(WasmBuffer& code, const llvm::ConstantFP* f);
	bool needsPointerKindConversion(const llvm::Instruction* phi, const llvm::Value* incoming);
	void compilePHIOfBlockFromOtherBlock(WasmBuffer& code, const llvm::BasicBlock* to, const llvm::BasicBlock* from);
};

}
#endif
