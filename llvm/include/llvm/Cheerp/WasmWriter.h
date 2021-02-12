//===-- Cheerp/WasmWriter.h - The Cheerp JavaScript generator -----------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017-2020 Leaning Technologies
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
#include "llvm/Cheerp/DeterministicUnorderedSet.h"
#include "llvm/Cheerp/WasmOpcodes.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/Support/FormattedStream.h"

namespace cheerp
{

const uint32_t WasmPage = 64*1024;

class CheerpWasmWriter;

typedef llvm::raw_pwrite_stream WasmBuffer;


template<unsigned N>
class Chunk: public llvm::raw_svector_ostream {
private:
	llvm::SmallString<N> buffer;
public:
	Chunk(): llvm::raw_svector_ostream(buffer)
	{}
	llvm::SmallVectorImpl<char>& buf()
	{
		return buffer;
	}
};

class Section : public Chunk<256> {
private:
	CheerpWasmWriter* writer;

public:
	Section(uint32_t sectionId, const char* sectionName, CheerpWasmWriter* writer);
	~Section();
};

class CheerpWasmWriter
{
public:
	bool shouldDefer(const llvm::Instruction* I) const;

private:
	llvm::Module& module;
	llvm::Pass& pass;
	llvm::DataLayout targetData;
	const llvm::Function* currentFun;
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

	// Lookup map that translates register IDs to local indices. The local
	// index includes the number of arguments that are defined before the first
	// local variable.
	std::vector<int> localMap;

	class TeeLocals
	{
		struct TeeLocalCandidate
		{
			const llvm::Value* v;
			bool isInstructionAssigment;
			uint32_t localId;
			uint32_t bufferOffset;
			bool used;
			TeeLocalCandidate(const llvm::Value* v, bool isInstructionAssigment, uint32_t l, uint32_t o) :
				v(v),isInstructionAssigment(isInstructionAssigment),localId(l),bufferOffset(o),used(false)
			{
			}
		};

		typedef std::deque<TeeLocalCandidate> TeeLocalCandidatesVector;
		struct TeeLocalStack
		{
			TeeLocalCandidatesVector stack;
			// Last point on the buffer where there was nothing on top of the stack
			uint32_t instStartPos;
			TeeLocalStack(TeeLocalCandidatesVector&& vector, uint32_t instStartPos) : stack(std::move(vector)), instStartPos(instStartPos)
			{
			}
			TeeLocalStack() : stack(), instStartPos(0)
			{
			}
		};
		std::deque<TeeLocalStack> teeLocalCandidatesStack;

	public:
		struct LocalInserted
		{
			const llvm::Instruction* I;
			uint32_t localId;
			uint32_t bufferOffset;
		};

	private:
		std::vector<LocalInserted> localInserted;
		llvm::DenseSet<const llvm::Instruction*> valueUsed;
	public:
		TeeLocals()
		{
		}
		uint32_t findDepth(const llvm::Value* v) const
		{
			uint32_t c = 0;
			const TeeLocalCandidatesVector& teeLocalCandidates = teeLocalCandidatesStack.back().stack;
			for(auto it = teeLocalCandidates.rbegin(); it != teeLocalCandidates.rend(); ++it)
			{
				++c;
				if(it->used)
					break;
				if(it->v == v)
					return c;
			}
			return -1;

		}
		bool couldPutTeeLocalOnStack(const llvm::Value* v, const uint32_t currOffset, uint32_t& bufferOffset, uint32_t& localId)
		{
			if(teeLocalCandidatesStack.empty() || currOffset != teeLocalCandidatesStack.back().instStartPos)
				return false;

			// Search for candidates
			TeeLocalCandidatesVector& teeLocalCandidates = teeLocalCandidatesStack.back().stack;
			for(auto it = teeLocalCandidates.rbegin(); it != teeLocalCandidates.rend(); ++it)
			{
				if(it->used)
					break;
				if(it->v == v)
				{
					it->used = true;
					bufferOffset = it->bufferOffset;
					localId = it->localId;
					if (it->isInstructionAssigment)
					{
						//We could safely remove a tee_local only if it comes from an Instruction assigment
						//(the other case being setting incoming into a phi, and that could not be removed)
						const llvm::Instruction* I = llvm::cast<llvm::Instruction>(v);
						localInserted.push_back({I, localId, bufferOffset});
						valueUsed.insert(I);
					}
					return true;
				}
			}
			return false;
		}
		void addCandidate(const llvm::Value* v, bool isInstructionAssigment, const uint32_t local, const uint32_t offset)
		{
			teeLocalCandidatesStack.back().stack.emplace_back(v, isInstructionAssigment, local, offset);
			if (isInstructionAssigment)
			{
				const llvm::Instruction* I = llvm::cast<llvm::Instruction>(v);
				localInserted.push_back({I, local, offset});
			}
		}
		void removeConsumed(const TeeLocalCandidate* candidate = NULL)
		{
			// Remove consumed tee local candidates
			TeeLocalCandidatesVector& teeLocalCandidates = teeLocalCandidatesStack.back().stack;

			uint32_t index = 0;
			for (auto& c : teeLocalCandidates)
			{
				if (candidate == NULL && c.used)
				{
					teeLocalCandidates.erase(teeLocalCandidates.begin() + index, teeLocalCandidates.end());
					break;
				}

				if (&c == candidate)
					candidate = NULL;
				index++;
			}
		}
		const TeeLocalCandidate* lastUsed() const
		{
			const TeeLocalCandidatesVector& teeLocalCandidates = teeLocalCandidatesStack.back().stack;
			auto lastUsedIt = std::find_if(teeLocalCandidates.rbegin(), teeLocalCandidates.rend(), [](const TeeLocalCandidate& c) { return c.used; });
			if (lastUsedIt == teeLocalCandidates.rend())
				return NULL;
			return &*lastUsedIt;
		}
		void addIndentation(WasmBuffer& code)
		{
			teeLocalCandidatesStack.emplace_back();
			instructionStart(code);
		}
		void decreaseIndentation(WasmBuffer& code, bool performCheck = true)
		{
			if (performCheck)
				assert(teeLocalCandidatesStack.back().instStartPos == code.tell());
			teeLocalCandidatesStack.pop_back();
		}
		void clearTopmostCandidates(WasmBuffer& code, const uint32_t depth)
		{
			for (uint32_t d=0; d<depth; d++)
				decreaseIndentation(code, /*performCheck*/false);
			for (uint32_t d=0; d<depth; d++)
				addIndentation(code);
		}
		void performInitialization(WasmBuffer& code)
		{
			//There should be no layers when in the initial state
			assert(teeLocalCandidatesStack.empty());
			addIndentation(code);
		}
		const std::vector<LocalInserted>& getLocalInserted()
		{
			return localInserted;
		}
		bool isValueUsed(const llvm::Instruction* I)
		{
			return valueUsed.count(I);
		}
		void clear(WasmBuffer& code)
		{
			//There should be only the last layer when we call clear
			decreaseIndentation(code, /*performCheck*/false);
			assert(teeLocalCandidatesStack.empty());
			localInserted.clear();
			valueUsed.clear();
		}
		void instructionStart(WasmBuffer& code)
		{
			teeLocalCandidatesStack.back().instStartPos = code.tell();
		}
		bool needsSubStack(WasmBuffer& code) const
		{
			const uint32_t pos = teeLocalCandidatesStack.back().instStartPos;
			return pos != 0 && pos != code.tell();
		}
	};

	llvm::DenseSet<const llvm::Instruction*> getLocalDone;

	// Whether to enable shared memory
	bool sharedMemory;

	// Whether to disable memory growth
	bool noGrowMemory;

	// Whether to export the function table from the module
	bool exportedTable;

	mutable std::vector<uint32_t> nopLocations;

	void filterNop(llvm::SmallVectorImpl<char>& buffer) const;
public:
	TeeLocals teeLocals;
	std::vector<const llvm::Instruction*> deferred;
	using DeterministicInstructionSet = cheerp::DeterministicUnorderedSet<const llvm::Instruction*, RestrictionsLifted::NoPointerStability>;
	using InstructionToDependenciesMap = llvm::DenseMap<const llvm::Instruction*, DeterministicInstructionSet>;
	InstructionToDependenciesMap memoryDependencies;
	InstructionToDependenciesMap localsDependencies;
	llvm::DenseSet<const llvm::Instruction*> compiled;

	void renderDeferred(WasmBuffer& code, const std::vector<const llvm::Instruction*>& deferred);

	enum GLOBAL_CONSTANT_ENCODING { NONE = 0, FULL, GLOBAL };
	const PointerAnalyzer & PA;

	void checkImplicitedAssignedPhi(const llvm::Function& F);
	void generateNOP(WasmBuffer& code);
	void putNOP(WasmBuffer& code, uint32_t localId, uint32_t bufferOffset, bool isValueUsed)
	{
		llvm::SmallString<8> buf;
		llvm::raw_svector_ostream wbuf(buf);
		encodeInst(WasmU32Opcode::TEE_LOCAL, localId, wbuf);
		uint32_t teeSize = wbuf.tell();
		if (!isValueUsed)
		{
			char drop = 0x1A; // DROP the value on the stack
			code.pwrite(&drop, 1, bufferOffset++);
			teeSize -= 1;
		}
		nopLocations.push_back(bufferOffset);
		buf.clear();
		buf.assign(teeSize, 0x1 /*NOP*/);
		code.pwrite(buf.begin(), teeSize, bufferOffset);
	}
	//IFF returns true, it has modified the buffer so to obtain an extra value on the stack
	bool hasPutTeeLocalOnStack(WasmBuffer& code, const llvm::Value* v)
	{
		const uint32_t currOffset = code.tell();
		uint32_t bufferOffset;
		uint32_t localId;
		if (teeLocals.couldPutTeeLocalOnStack(v, currOffset, bufferOffset, localId))
		{
			llvm::SmallString<8> buf;
			llvm::raw_svector_ostream wbuf(buf);
			encodeInst(WasmU32Opcode::TEE_LOCAL, localId, wbuf);
			code.pwrite(buf.begin(), wbuf.tell(), bufferOffset);
			return true;
		}
		return false;
	}
	uint32_t findDepth(const llvm::Value* v) const;

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
	void compileCondition(WasmBuffer& code, const llvm::Value* cond, bool booleanInvert);
	const llvm::BasicBlock* compileTokens(WasmBuffer& code, const TokenList& Tokens);
	void compileMethod(WasmBuffer& code, const llvm::Function& F);
	void compileImport(WasmBuffer& code, llvm::StringRef funcName, llvm::FunctionType* FTy);
	void compileGlobal(const llvm::GlobalVariable& G);
	// Returns true if it has handled local assignent internally
	bool compileInstruction(WasmBuffer& code, const llvm::Instruction& I);
	bool compileInlineInstruction(WasmBuffer& code, const llvm::Instruction& I);
	void compileGEP(WasmBuffer& code, const llvm::User* gepInst, bool standalone = false);
	void compileLoad(WasmBuffer& code, const llvm::LoadInst& I, bool signExtend);
	void compileGetLocal(WasmBuffer& code, const llvm::Instruction* v);
	// Returns true if all the uses have signed semantics
	// NOTE: Careful, this is not in sync with needsUnsignedTruncation!
	//       All the users listed here must _not_ call needsUnsignedTruncation!
	bool isSignedLoad(const llvm::Value* V) const;
	bool isReturnPartOfTailCall(const llvm::Instruction& ti) const;
	bool isTailCall(const llvm::CallInst& ci) const;
	// Returns true if, by potentially inverting commutative instructions, there is a way
	// to use the last written register as the first operand
	bool mayHaveLastWrittenRegAsFirstOperand(const llvm::Value* v) const;
	// Returns the constant unsigned offset to use in the load/store
	uint32_t compileLoadStorePointer(WasmBuffer& code, const llvm::Value* ptrOp);
	static const char* getIntegerPredicate(llvm::CmpInst::Predicate p);

	std::map<const llvm::BasicBlock*, const llvm::PHINode*> selectPHINodesHandledAsResult(const std::vector<const llvm::BasicBlock*>& possibleBB) const;
	int gainOfHandlingPhiOnTheEdge(const llvm::PHINode* phi, const llvm::Value* incoming) const;
	int gainOfHandlingPhiOnTheEdge(const llvm::PHINode* phi) const;


	const llvm::BasicBlock* currentBB{NULL};
	void checkAndSanitizeDependencies(InstructionToDependenciesMap& dependencies) const;
	void flushGeneric(WasmBuffer& code, const llvm::Instruction& I, const InstructionToDependenciesMap& dependencies);
	void flushMemoryDependencies(WasmBuffer& code, const llvm::Instruction& I);
	void flushSetLocalDependencies(WasmBuffer& code, const llvm::Instruction& I);

	struct WasmBytesWriter: public LinearMemoryHelper::ByteListener
	{
		WasmBuffer& code;
		const CheerpWasmWriter& writer;
		WasmBytesWriter(WasmBuffer& code, const CheerpWasmWriter& writer)
			: code(code), writer(writer)
		{
		}
		void addByte(uint8_t b) override;
	};

	struct WasmGepWriter: public LinearMemoryHelper::LinearGepListener
	{
		CheerpWasmWriter& writer;
		WasmBuffer& code;
		std::vector<std::pair<const llvm::Value*, uint32_t>> addedValues;
		std::vector<std::pair<const llvm::Value*, uint32_t>> subbedValues;
		int32_t constPart;
		WasmGepWriter(CheerpWasmWriter& writer, WasmBuffer& code)
			: LinearMemoryHelper::LinearGepListener(writer.PA), writer(writer), code(code), constPart(0)
		{
		}
		void addValue(const llvm::Value* v, uint32_t size) override;
		void subValue(const llvm::Value* v, uint32_t size) override;
		void addConst(int64_t v) override;
		bool hasSubValue() const override
		{
			return true;
		}
		void compileValue(const llvm::Value* v, uint32_t size) const;

		//compileValues will always put something on the stack
		//the result will be the offset to be encoded in the load/store
		//if positiveOffsetAllowed == false, the result will be always 0
		uint32_t compileValues(bool positiveOffsetAllowed) const;
	};
	std::unordered_map<const llvm::Constant*, std::pair<uint32_t, GLOBAL_CONSTANT_ENCODING>> globalizedConstants;
	LinearMemoryHelper::GlobalUsageMap globalizedGlobalsIDs;
	mutable InlineableCache inlineableCache;
public:
	llvm::raw_ostream& stream;
	CheerpWasmWriter(llvm::Module& m, llvm::Pass& p, llvm::raw_ostream& s, const cheerp::PointerAnalyzer & PA,
			cheerp::Registerize & registerize,
			cheerp::GlobalDepsAnalyzer & gda,
			const LinearMemoryHelper& linearHelper,
			const NameGenerator& namegen,
			llvm::LLVMContext& C,
			unsigned heapSize,
			bool useWasmLoader,
			bool prettyCode,
			bool useCfgLegacy,
			bool sharedMemory,
			bool exportedTable):
		module(m),
		pass(p),
		targetData(&m),
		currentFun(NULL),
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
		sharedMemory(sharedMemory),
		noGrowMemory(!linearHelper.canGrowMemory()),
		exportedTable(exportedTable),
		PA(PA),
		inlineableCache(PA),
		stream(s)
	{
	}
	void makeWasm();
	void compileBB(WasmBuffer& code, const llvm::BasicBlock& BB, const llvm::PHINode* phiHandledAsResult = nullptr);
	void compileDowncast(WasmBuffer& code, const llvm::CallBase* callV);
	void compileConstantExpr(WasmBuffer& code, const llvm::ConstantExpr* ce);
	void compileConstant(WasmBuffer& code, const llvm::Constant* c, bool forGlobalInit);
	void compileInstructionAndSet(WasmBuffer& code, const llvm::Instruction& I);
	void compileOperand(WasmBuffer& code, const llvm::Value* v);
	void compileSignedInteger(WasmBuffer& code, const llvm::Value* v, bool forComparison);
	void compileUnsignedInteger(WasmBuffer& code, const llvm::Value* v);
	void compileTypedZero(WasmBuffer& code, llvm::Type* t);
	static void encodeInst(WasmOpcode opcode, WasmBuffer& code);
	static void encodeInst(WasmS32Opcode opcode, int32_t immediate, WasmBuffer& code);
	static void encodeInst(WasmS64Opcode opcode, int64_t immediate, WasmBuffer& code);
	static void encodeInst(WasmU32Opcode opcode, uint32_t immediate, WasmBuffer& code);
	static void encodeInst(WasmU32U32Opcode opcode, uint32_t i1, uint32_t i2, WasmBuffer& code);
	void encodeBinOp(const llvm::Instruction& I, WasmBuffer& code);
	void encodePredicate(const llvm::Type* ty, const llvm::CmpInst::Predicate predicate, WasmBuffer& code);
	void compileICmp(const llvm::ICmpInst& ci, const llvm::CmpInst::Predicate p, WasmBuffer& code);
	void compileICmp(const llvm::Value* op0, const llvm::Value* op1, const llvm::CmpInst::Predicate p, WasmBuffer& code);
	void compileFCmp(const llvm::Value* lhs, const llvm::Value* rhs, const llvm::CmpInst::Predicate p, WasmBuffer& code);
	void encodeLoad(const llvm::Type* ty, uint32_t offset, WasmBuffer& code, bool signExtend);
	void encodeWasmIntrinsic(WasmBuffer& code, const llvm::Function* F);
	void encodeBranchTable(WasmBuffer& code, std::vector<uint32_t> table, int32_t defaultBlock);
	void encodeDataSectionChunk(WasmBuffer& data, uint32_t address, llvm::StringRef buf);
	uint32_t encodeDataSectionChunks(WasmBuffer& data, uint32_t address, llvm::StringRef buf);
	void compileFloatToText(WasmBuffer& code, const llvm::APFloat& f, uint32_t precision);
	GLOBAL_CONSTANT_ENCODING shouldEncodeConstantAsGlobal(const llvm::Constant* C, uint32_t useCount, uint32_t getGlobalCost);
	bool requiresExplicitAssigment(const llvm::Instruction* phi, const llvm::Value* incoming);
	void compilePHIOfBlockFromOtherBlock(WasmBuffer& code, const llvm::BasicBlock* to, const llvm::BasicBlock* from, const llvm::PHINode* phiHandledAsResult = nullptr);
	bool isInlineable(const llvm::Instruction& I) const
	{
		return inlineableCache.isInlineable(I);
	}
};

}
#endif
