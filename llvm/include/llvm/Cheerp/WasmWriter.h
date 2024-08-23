//===-- Cheerp/WasmWriter.h - The Cheerp JavaScript generator -----------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2017-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_WAST_WRITER_H
#define _CHEERP_WAST_WRITER_H

#include <sstream>

#include "llvm/Cheerp/BaseWriter.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/LinearMemoryHelper.h"
#include "llvm/Cheerp/NameGenerator.h"
#include "llvm/Cheerp/AllocaMerging.h"
#include "llvm/Cheerp/PointerAnalyzer.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/InvokeWrapping.h"
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
	Chunk(Chunk& copy): llvm::raw_svector_ostream(buffer)
	{
		buffer = copy.buffer;
	}
	llvm::SmallVectorImpl<char>& buf()
	{
		return buffer;
	}
};

class Section : public Chunk<256> {
private:
	enum class State {
		GENERATING,
		ENCODED,
		DISCARDED,
	};
	State state{State::GENERATING};
	const bool hasName;
	const std::string name;
	const uint32_t sectionId;
	CheerpWasmWriter* writer;
public:
	Section(uint32_t sectionId, const char* sectionName, CheerpWasmWriter* writer);
	void encode();
	void discard();
	~Section();
};

class CheerpWasmWriter final : public CheerpBaseWriter
{
public:
	bool shouldDefer(const llvm::Instruction* I) const;

	bool isGlobalized(const llvm::Value* v) const final
	{
		if (const llvm::GlobalVariable* GV = llvm::dyn_cast<llvm::GlobalVariable>(v))
			return globalizedGlobalsIDs.count(GV);
		return false;
	}
private:
	llvm::Module& module;
	llvm::ModuleAnalysisManager& MAM;
	llvm::FunctionAnalysisManager& FAM;
	llvm::DataLayout targetData;
	const llvm::Function* currentFun;
	Registerize & registerize;

	llvm::LLVMContext& Ctx;

	EdgeContext edgeContext;
	GlobalDepsAnalyzer & globalDeps;
	// Helper class to manage linear memory state
	const LinearMemoryHelper& linearHelper;

	const LandingPadTable& landingPadTable;

	const NameGenerator& namegen;

	const AllocaStoresExtractor& allocaStoresExtractor;

	TypeSupport types;

	// Codegen custom globals
	uint32_t usedGlobals;
	uint32_t stackTopGlobal;
	uint32_t oSlotGlobal;
	uint32_t nullArrayGlobal;
	uint32_t nullObjGlobal;

	// The wasm module heap size
	uint32_t heapSize;

	// If true, the Wasm file is loaded using a JavaScript loader. This allows
	// FFI calls to methods outside of the Wasm file. When false, treat functions
	// without a definition as imports.
	bool useWasmLoader;

	// If true, embed a custom section called 'name' in binary wasm that maps
	// the function ids to C++ mangled function names. If available in LLVM IR,
	// it will also add names to local variables inside functions.
	bool prettyCode;

	// Lookup map that translates register IDs to local indices. The local
	// index includes the number of arguments that are defined before the first
	// local variable.
	std::vector<int> localMap;

	// TODO: move to a utils file
	// Used to keep track of the index of elements inside structs,
	// the vector contains the index of split regulars without a constant offset.
	std::unordered_map<const llvm::StructType*, std::unordered_map<uint32_t, uint32_t>> structElemIdxCache;

	// Returns the index inside the struct where the downcast array is located
	std::unordered_map<const llvm::StructType*, uint32_t> downcastArrayIndices;

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
	const bool exportedTable;

	mutable std::vector<uint32_t> nopLocations;

	void filterNop(llvm::SmallVectorImpl<char>& buffer, std::function<void(uint32_t, char)> filterCallback) const;
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
		llvm::errs() << "[putNOP] encoding TEE_LOCAL id: " << localId << "\n";
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
	
		// TODO: get the right localId since SPLIT_REGULARS add extra indices into the arguments

		const uint32_t currOffset = code.tell();
		uint32_t bufferOffset;
		uint32_t localId;
		if (teeLocals.couldPutTeeLocalOnStack(v, currOffset, bufferOffset, localId))
		{
			llvm::SmallString<8> buf;
			llvm::raw_svector_ostream wbuf(buf);
			encodeInst(WasmU32Opcode::TEE_LOCAL, localId, wbuf);

			code.pwrite(buf.begin(), wbuf.tell(), bufferOffset);
			// If we call local_tee on a GC type it needs to be casted back to that type again
			// since the local it will be stored into is of anyref type 
			POINTER_KIND kind = COMPLETE_OBJECT;
			kind = getLocalPointerKind(v);
			if (v->getType()->isPointerTy())
			{
				kind = PA.getPointerKind(v);
				if (kind == REGULAR && PA.getConstantOffsetForPointer(v)) // TODO: needsDowncastArray
				{
					kind = SPLIT_REGULAR;
				}
			}
			compileRefCast(code, v->getType(), kind);
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
	void compileMemorySection();
	void compileGlobalSection();
	void compileExportSection();
	void compileElementSection();
	void compileDataCountSection();
	void compileCodeSection();
	void compileDataSection();
	void compileNameSection();

	static const char* getTypeString(const llvm::Type* t);
	void compileMethodArgs(WasmBuffer& code, llvm::User::const_op_iterator it, llvm::User::const_op_iterator itE, const llvm::CallBase& callV);
	void compileMethodLocals(WasmBuffer& code, const std::vector<int>& locals);
	void compileMethodParams(WasmBuffer& code, const llvm::FunctionType* F);
	void compileMethodResult(WasmBuffer& code, const llvm::Type* F);

	void compileRefCast(WasmBuffer& code, const llvm::Type* Ty, POINTER_KIND kind);
	void compileCompleteObject(WasmBuffer& code, const llvm::Value* p, const llvm::Value* offset = (const llvm::Value *)nullptr, const bool compileFullAccess = true);
	void compileAccessToElement(WasmBuffer& code, llvm::Type* tp, llvm::ArrayRef< const llvm::Value* > indices, bool compileLastWrapperArray, const bool compileFullAccess);
	void compileOffsetForGEP(WasmBuffer& code, llvm::Type* pointedOperandType, llvm::ArrayRef< const llvm::Value* > indices);
	void compileGEPOffset(WasmBuffer& code, const llvm::User* gepInst);
	void compileGEPBase(WasmBuffer& code, const llvm::User* gepInst);
	void compileGEPGC(WasmBuffer& code, const llvm::User* gep_inst, POINTER_KIND kind, bool compileFullAccess);
	void compilePointerBase(WasmBuffer& code, const llvm::Value* ptr, const uint32_t elemIdx = 0);
	void compilePointerBaseTyped(WasmBuffer& code, const llvm::Value* ptr, const llvm::Type* elemType, const uint32_t elemIdx = 0);
	void compilePointerOffset(WasmBuffer& code, const llvm::Value* ptr);
	void compilePointerAs(WasmBuffer& code, const llvm::Value* p, POINTER_KIND kind);
	void compileRootStructWithDowncastArray(Section& section, const llvm::StructType* sTy);
	void compileSubType(Section& section, const llvm::StructType* sTy);
	void compileStructType(Section& section, const llvm::StructType* sTy);
	void compileArrayType(Section& section, const llvm::ArrayType* aTy);
	void compilePackedType(Section& section, const llvm::Type* Ty);
	void compileStorageType(Section& section, const llvm::Type* Ty);

	void compileBranchTable(WasmBuffer& code, const llvm::SwitchInst* si,
		const std::vector<std::pair<int, int>>& cases);
	void compileCondition(WasmBuffer& code, const llvm::Value* cond, bool booleanInvert);
	const llvm::BasicBlock* compileTokens(WasmBuffer& code, const TokenList& Tokens);
	uint32_t getDowncastArraySize(llvm::StructType* sTy, uint32_t size) const;
	uint32_t compileDowncastInitializerRecursive(WasmBuffer& code, Chunk<128> currClassAccess, llvm::StructType* sTy, uint32_t baseCount);
	void compileDowncastInitializer(WasmBuffer& code, llvm::StructType* sTy);
	void compileCreatePointerArrayFunc(WasmBuffer& code);
	void compileCreateDynamicAllocArrayFunc(WasmBuffer& code, llvm::Type* Ty);
	void compileResizeArrayFunc(WasmBuffer& code, llvm::Type* Ty);
	void compileMethod(WasmBuffer& code, const llvm::Function& F);
	void compileImport(WasmBuffer& code, llvm::StringRef funcName, llvm::FunctionType* FTy);
	void compileImportMemory(WasmBuffer& code);
	void compileGlobal(const llvm::GlobalVariable& G);
	// Returns true if it has handled local assignent internally
	bool compileInstruction(WasmBuffer& code, const llvm::Instruction& I);
	bool compileInlineInstruction(WasmBuffer& code, const llvm::Instruction& I);
	void compileGEP(WasmBuffer& code, const llvm::User* gepInst, bool standalone = false);
	void compileLoad(WasmBuffer& code, const llvm::LoadInst& I, bool signExtend);
	void compileStore(WasmBuffer& code, const llvm::StoreInst& I);
	void compileGetLocal(WasmBuffer& code, const llvm::Instruction* v, uint32_t elemIdx);
	POINTER_KIND getLocalPointerKind(const llvm::Value* v);
	uint32_t cacheDowncastOffset(const llvm::StructType* sTy);
	uint32_t calculateAndCacheElemInfo(const llvm::StructType* sTy);
	uint32_t getExpandedStructElemIdx(const llvm::StructType* sTy, uint32_t elemIdx);
	bool needsExpandedStruct(const llvm::StructType* sTy);
	bool needsOffsetAsElement(const llvm::StructType* sTy, uint32_t elemIdx);
	void compileLoadGC(WasmBuffer& code, const llvm::LoadInst& li, llvm::Type* loadedType, const llvm::Value* ptrOp, llvm::StructType* sTy, uint32_t structElemIdx, bool isOffset);
	void compileStoreGC(WasmBuffer& code, const llvm::StoreInst& si, llvm::Type* storedType, llvm::StructType* sTy, uint32_t structElemIdx, bool isOffset, POINTER_KIND ptrKind, POINTER_KIND storeKind);
	void allocateSimpleType(WasmBuffer& code, llvm::Type* Ty, const llvm::Value* init);
	void allocateComplexType(WasmBuffer& code, llvm::Type* Ty, bool hasDowncastArray, uint32_t& usedValuesFromMap, const AllocaStoresExtractor::OffsetToValueMap* offsetToValueMap, uint32_t offset);
	void allocateTypeGC(WasmBuffer& code, llvm::Type* Ty, bool hasDowncastArray, const AllocaStoresExtractor::OffsetToValueMap* offsetToValueMap = (const cheerp::AllocaStoresExtractor::OffsetToValueMap *)nullptr);
	void compileAllocationGC(WasmBuffer& code, const DynamicAllocInfo& info);
	void callDowncastArrayInit(WasmBuffer& code, const llvm::Type* Ty);
	void compileDowncastGC(WasmBuffer& code, const llvm::CallBase* callV);
	void compileVirtualcastGC(WasmBuffer& code, const llvm::CallBase* callV);
	uint32_t compileArraySizeGC(WasmBuffer& code, const DynamicAllocInfo & info, bool encode);
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
	uint32_t numberOfImportedFunctions{0};
public:
	llvm::raw_ostream& stream;
	CheerpWasmWriter(llvm::Module& m, llvm::ModuleAnalysisManager& MAM, llvm::raw_ostream& s, const cheerp::PointerAnalyzer & PA,
			cheerp::Registerize & registerize,
			cheerp::GlobalDepsAnalyzer & gda,
			const LinearMemoryHelper& linearHelper,
			const LandingPadTable& landingPadTable,
			const NameGenerator& namegen,
			AllocaStoresExtractor& allocaStoresExtractor,
			llvm::LLVMContext& C,
			unsigned heapSize,
			bool useWasmLoader,
			bool prettyCode,
			bool sharedMemory,
			bool exportedTable):
		module(m),
		MAM(MAM),
		FAM(MAM.getResult<llvm::FunctionAnalysisManagerModuleProxy>(m).getManager()),
		targetData(&m),
		currentFun(NULL),
		registerize(registerize),
		Ctx(C),
		edgeContext(),
		globalDeps(gda),
		linearHelper(linearHelper),
		landingPadTable(landingPadTable),
		namegen(namegen),
		allocaStoresExtractor(allocaStoresExtractor),
		usedGlobals(0),
		stackTopGlobal(0),
		oSlotGlobal(0),
		nullArrayGlobal(0),
		nullObjGlobal(0),
		heapSize(heapSize),
		useWasmLoader(useWasmLoader),
		prettyCode(prettyCode),
		sharedMemory(sharedMemory),
		noGrowMemory(!linearHelper.canGrowMemory()),
		exportedTable(exportedTable),
		PA(PA),
		inlineableCache(PA),
		stream(s),
		types(m)
	{
	}
	void makeWasm();
	void compileBB(WasmBuffer& code, const llvm::BasicBlock& BB, const llvm::PHINode* phiHandledAsResult = nullptr);
	void compileDowncast(WasmBuffer& code, const llvm::CallBase* callV);
	bool doesConstantDependOnUndefined(const llvm::Constant* C) const;
	void compileConstantAggregate(WasmBuffer& code, const llvm::ConstantAggregate* ca);
	void compileConstantExpr(WasmBuffer& code, const llvm::ConstantExpr* ce);
	void compileConstant(WasmBuffer& code, const llvm::Constant* c, bool forGlobalInit);
	void compileInstructionAndSet(WasmBuffer& code, const llvm::Instruction& I);
	void compileOperand(WasmBuffer& code, const llvm::Value* v);
	void compileAggregateElem(WasmBuffer& code, const llvm::Value* v, uint32_t elemIdx);
	void compileSignedInteger(WasmBuffer& code, const llvm::Value* v, bool forComparison);
	void compileUnsignedInteger(WasmBuffer& code, const llvm::Value* v);
	void compileTypedZero(WasmBuffer& code, const llvm::Type* t);
	static void encodeInst(WasmOpcode opcode, WasmBuffer& code);
	static void encodeInst(WasmS32Opcode opcode, int32_t immediate, WasmBuffer& code);
	static void encodeInst(WasmS64Opcode opcode, int64_t immediate, WasmBuffer& code);
	static void encodeInst(WasmU32Opcode opcode, uint32_t immediate, WasmBuffer& code);
	static void encodeInst(WasmGCOpcode opcode, WasmBuffer& code);
	static void encodeInst(WasmGCOpcode opcode, int32_t immediate, WasmBuffer& code);
	static void encodeInst(WasmGCOpcode opcode, int32_t i1, int32_t i2, WasmBuffer& code);
	static void encodeInst(WasmFCU32Opcode opcode, uint32_t immediate, WasmBuffer& code);
	static void encodeInst(WasmFCU32U32Opcode opcode, uint32_t i1, uint32_t i2, WasmBuffer& code);
	static void encodeInst(WasmSIMDOpcode opcode, WasmBuffer& code);
	static void encodeInst(WasmSIMDU32Opcode opcode, uint32_t immediate, WasmBuffer& code);
	static void encodeInst(WasmSIMDU32U32Opcode opcode, uint32_t i1, uint32_t i2, WasmBuffer& code);
	static void encodeInst(WasmSIMDU32U32U32Opcode opcode, uint32_t i1, uint32_t i2, uint32_t i3, WasmBuffer& code);
	static void encodeInst(WasmU32U32Opcode opcode, uint32_t i1, uint32_t i2, WasmBuffer& code);
	static void encodeInst(WasmThreadsU32Opcode opcode, uint32_t immediate, WasmBuffer& code);
	static void encodeInst(WasmThreadsU32U32Opcode opcode, uint32_t i1, uint32_t i2, WasmBuffer& code);
	void encodeInst(WasmInvalidOpcode opcode, WasmBuffer& code);
	void encodeVectorConstantZero(WasmBuffer& code);
	void encodeConstantDataVector(WasmBuffer& code, const llvm::ConstantDataVector* cdv);
	void encodeConstantVector(WasmBuffer& code, const llvm::ConstantVector* cv);
	void encodeExtractLane(WasmBuffer& code, const llvm::ExtractElementInst& eei);
	void encodeReplaceLane(WasmBuffer& code, const llvm::InsertElementInst& iei);
	void encodeVectorTruncation(WasmBuffer& code, const llvm::Instruction& I);
	void encodeLoadingShuffle(WasmBuffer& code, const llvm::FixedVectorType* vecType);
	void encodeStoringShuffle(WasmBuffer& code, const llvm::FixedVectorType* vecType);
	void encodeBranchHint(const llvm::BranchInst* BI, const bool IfNot, WasmBuffer& code);
	void encodeBinOp(const llvm::Instruction& I, WasmBuffer& code);
	void encodePredicate(const llvm::Type* ty, const llvm::CmpInst::Predicate predicate, WasmBuffer& code);
	void compileICmp(const llvm::ICmpInst& ci, const llvm::CmpInst::Predicate p, WasmBuffer& code);
	void compileICmp(const llvm::Value* op0, const llvm::Value* op1, const llvm::CmpInst::Predicate p, WasmBuffer& code);
	void compileFCmp(const llvm::Value* lhs, const llvm::Value* rhs, const llvm::CmpInst::Predicate p, WasmBuffer& code);
	void encodeLoad(llvm::Type* ty, uint32_t offset, WasmBuffer& code, bool signExtend, bool atomic);
	void encodeWasmIntrinsic(WasmBuffer& code, const llvm::Function* F);
	void encodeBranchTable(WasmBuffer& code, std::vector<uint32_t> table, int32_t defaultBlock);
	void encodeDataSectionChunk(WasmBuffer& data, uint32_t address, llvm::StringRef buf);
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
