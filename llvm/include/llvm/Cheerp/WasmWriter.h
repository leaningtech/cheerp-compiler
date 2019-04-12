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
#include "llvm/Cheerp/TokenList.h"
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
	CheerpWasmWriter* writer;

public:
	Section(uint32_t sectionId, const char* sectionName, CheerpWasmWriter* writer);
	~Section();
};

class CheerpWasmWriter
{
private:
	llvm::Module& module;
	llvm::Pass& pass;
	llvm::DataLayout targetData;
	const llvm::Function* currentFun;
	uint32_t lastWrittenReg;
	Registerize & registerize;

	llvm::LLVMContext& Ctx;

	EdgeContext edgeContext;
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

	// If true, use relooper instead of stackifier
	bool useCfgLegacy;

	// If true, a set_local instruction is buffered. This mechanism is used to
	// combine set_local followed by a get_local into a tee_local. The
	// setLocalId field tracks the instruction's immediate.
	bool hasSetLocal;

	// The immediate of the buffered set_local instruction. If hasSetLocal is
	// false, this value is set to `(uint32_t)-1`.
	uint32_t setLocalId;

	// Lookup map that translates register IDs to local indices. The local
	// index includes the number of arguments that are defined before the first
	// local variable.
	std::vector<int> localMap;

public:
	enum GLOBAL_CONSTANT_ENCODING { NONE = 0, FULL, INT, FLOAT32 };
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
	void compileMethodLocals(WasmBuffer& code, const std::vector<int>& locals);
	void compileMethodParams(WasmBuffer& code, const llvm::FunctionType* F);
	void compileMethodResult(WasmBuffer& code, const llvm::Type* F);

	void compileBranchTable(WasmBuffer& code, const llvm::SwitchInst* si,
		const std::vector<std::pair<int, int>>& cases);
	void compileCondition(WasmBuffer& code, const llvm::BasicBlock* BB, bool booleanInvert);
	const llvm::BasicBlock* compileTokens(WasmBuffer& code, const TokenList& Tokens);
	void compileMethod(WasmBuffer& code, llvm::Function& F);
	void compileImport(WasmBuffer& code, llvm::StringRef funcName, llvm::FunctionType* FTy);
	void compileGlobal(const llvm::GlobalVariable& G);
	// Returns true if it has handled local assignent internally
	bool compileInstruction(WasmBuffer& code, const llvm::Instruction& I);
	bool compileInlineInstruction(WasmBuffer& code, const llvm::Instruction& I);
	void compileGEP(WasmBuffer& code, const llvm::User* gepInst, bool standalone = false);
	void compileLoad(WasmBuffer& code, const llvm::LoadInst& I, bool signExtend);
	// Returns true if all the uses have signed semantics
	// NOTE: Careful, this is not in sync with needsUnsignedTruncation!
	//       All the users listed here must _not_ call needsUnsignedTruncation!
	bool isSignedLoad(const llvm::Value* V) const;
	// Returns true if, by potentially inverting commutative instructions, there is a way
	// to use the last written register as the first operand
	bool mayHaveLastWrittenRegAsFirstOperand(const llvm::Value* v) const;
	// Returns the constant unsigned offset to use in the load/store
	uint32_t compileLoadStorePointer(WasmBuffer& code, const llvm::Value* ptrOp);
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
		bool isInlineable(const llvm::Value* p) override;
	};
	std::unordered_map<const llvm::Constant*, std::pair<uint32_t, GLOBAL_CONSTANT_ENCODING>> globalizedConstants;
public:
	llvm::raw_ostream& stream;
	CheerpWasmWriter(llvm::Module& m, llvm::Pass& p, llvm::raw_ostream& s, cheerp::PointerAnalyzer & PA,
			cheerp::Registerize & registerize,
			cheerp::GlobalDepsAnalyzer & gda,
			const LinearMemoryHelper& linearHelper,
			const NameGenerator& namegen,
			llvm::LLVMContext& C,
			unsigned heapSize,
			bool useWasmLoader,
			bool prettyCode,
			bool useCfgLegacy,
			CheerpMode cheerpMode):
		module(m),
		pass(p),
		targetData(&m),
		currentFun(NULL),
		lastWrittenReg(0xffffffff),
		registerize(registerize),
		Ctx(C),
		edgeContext(),
		globalDeps(gda),
		linearHelper(linearHelper),
		namegen(namegen),
		usedGlobals(0),
		stackTopGlobal(0),
		heapSize(heapSize),
		useWasmLoader(useWasmLoader),
		prettyCode(prettyCode),
		useCfgLegacy(useCfgLegacy),
		hasSetLocal(false),
		setLocalId((uint32_t)-1),
		PA(PA),
		cheerpMode(cheerpMode),
		stream(s)
	{
	}
	void makeWasm();
	void compileBB(WasmBuffer& code, const llvm::BasicBlock& BB);
	void compileDowncast(WasmBuffer& code, llvm::ImmutableCallSite callV);
	void compileConstantExpr(WasmBuffer& code, const llvm::ConstantExpr* ce);
	void compileConstant(WasmBuffer& code, const llvm::Constant* c, bool forGlobalInit);
	void compileOperand(WasmBuffer& code, const llvm::Value* v);
	void compileSignedInteger(WasmBuffer& code, const llvm::Value* v, bool forComparison);
	void compileUnsignedInteger(WasmBuffer& code, const llvm::Value* v);
	void encodeInst(uint32_t opcode, const char* name, WasmBuffer& code);
	void encodeBufferedSetLocal(WasmBuffer& code);
	void encodeBinOp(const llvm::Instruction& I, WasmBuffer& code);
	void encodeS32Inst(uint32_t opcode, const char* name, int32_t immediate, WasmBuffer& code);
	void encodeU32Inst(uint32_t opcode, const char* name, uint32_t immediate, WasmBuffer& code);
	void encodeU32U32Inst(uint32_t opcode, const char* name, uint32_t i1, uint32_t i2, WasmBuffer& code);
	void encodePredicate(const llvm::Type* ty, const llvm::CmpInst::Predicate predicate, WasmBuffer& code);
	void compileICmp(const llvm::ICmpInst& ci, const llvm::CmpInst::Predicate p, WasmBuffer& code);
	void compileFCmp(const llvm::Value* lhs, const llvm::Value* rhs, const llvm::CmpInst::Predicate p, WasmBuffer& code);
	void encodeLoad(const llvm::Type* ty, uint32_t offset, WasmBuffer& code, bool signExtend);
	void encodeWasmIntrinsic(WasmBuffer& code, const llvm::Function* F);
	void encodeBranchTable(WasmBuffer& code, std::vector<uint32_t> table, int32_t defaultBlock);
	void encodeDataSectionChunk(WasmBuffer& data, uint32_t address, const std::string& buf);
	uint32_t encodeDataSectionChunks(WasmBuffer& data, uint32_t address, const std::string& buf);
	bool tryEncodeFloatAsInt(const llvm::ConstantFP* f, int32_t& value);
	bool tryEncodeFloat64AsFloat32(const llvm::ConstantFP* f, float& value);
	uint32_t getIntEncodingLength(int32_t val) const;
	void compileFloatToText(WasmBuffer& code, const llvm::APFloat& f, uint32_t precision);
	GLOBAL_CONSTANT_ENCODING shouldEncodeConstantAsGlobal(const llvm::Constant* C, uint32_t useCount, uint32_t getGlobalCost);
	GLOBAL_CONSTANT_ENCODING shouldEncodeConstantIntAsGlobal(int32_t val, uint32_t useCount, uint32_t getGlobalCost);
	bool needsPointerKindConversion(const llvm::Instruction* phi, const llvm::Value* incoming);
	void compilePHIOfBlockFromOtherBlock(WasmBuffer& code, const llvm::BasicBlock* to, const llvm::BasicBlock* from);
};

}
#endif
