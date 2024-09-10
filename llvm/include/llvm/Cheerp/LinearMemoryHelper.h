//===-- Cheerp/LinearMemoryHelper.h - The Cheerp JavaScript generator -----===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2017-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_LINEAR_MEMORY_HELPER_H
#define _CHEERP_LINEAR_MEMORY_HELPER_H

#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/BuiltinInstructions.h"
#include "llvm/Cheerp/PointerAnalyzer.h"
#include <map>
#include <unordered_map>
#include <stack>

namespace cheerp
{

	class LinearMemoryAnalysis;

struct LinearMemoryHelperInitializer
{
	enum FunctionAddressMode {
		AsmJS = 0,
		Wasm
	};
	FunctionAddressMode mode;
	uint32_t memorySize;
	uint32_t stackSize;
	uint32_t stackOffset;
	bool growMem;
	bool hasAsmjsMem;
};

class LinearMemoryHelperWrapper;

class LinearMemoryHelper
{
public:
	/**
	 * NOTE: function addresses are different in asmjs and wasm: in asmjs
	 * they are offsets in the corresponding table, while in wasm they are
	 * offsets in a big table that is built concatenating all the tables
	 * together. In order to have a linear address space for functions also
	 * in asmjs, we will reserve the tpo 16 bits of the address to indicate
	 * the table number. This effectively limits the maximum number of tables
	 * and functions per table to 2^16 in asmjs.
	 */
	/**
	 * Used to compile asm.js indirect function calls
	 */
	struct FunctionTableInfo {
		llvm::SmallString<4> name;
		std::vector<const llvm::Function*> functions;
		size_t offset;
		size_t typeIndex;
	};
	/**
	 * Custom hash and compare functions for the FunctionTableInfoMap
	 * Two types are considered equal if they are both void, floating point,
	 * or (integer or pointer). Two function types are considered equal if
	 * they return equivalent types and they have parameters with equivalent
	 * types in the same order
	 */
	struct FunctionSignatureHash
	{
		bool isStrict;
		FunctionSignatureHash(bool isStrict):isStrict(isStrict)
		{
		}
		std::size_t operator()(const llvm::FunctionType* const& fTy) const 
		{
			const llvm::Type* retTy = fTy->getReturnType();
			size_t hash = 31;
			hash = hash*31 + std::hash<size_t>()(typeKindOf(retTy, isStrict));
			for (const auto& pTy: fTy->params())
			{
				hash = hash*31 + std::hash<size_t>()(typeKindOf(pTy, isStrict));
			}
			return hash;
		}
	};
	struct FunctionSignatureCmp
	{
	private:
		bool isStrict;
	public:
		FunctionSignatureCmp(bool isStrict):isStrict(isStrict)
		{
		}
		bool operator()(const llvm::FunctionType* const& lhs, const llvm::FunctionType* const& rhs) const
		{
			if (isStrict && lhs->isVarArg() != rhs->isVarArg())
				return false;

			size_t r1 = typeKindOf(lhs->getReturnType(), isStrict);
			size_t r2 = typeKindOf(rhs->getReturnType(), isStrict);
			if (r1 != r2)
				return false;
			// For WasmGC return types are encoded using direct type indices so the types must match 
			if (r1 == TypeKind::RefPointer && lhs->getReturnType() != rhs->getReturnType())
				return false;
			if (lhs->getNumParams() != rhs->getNumParams())
				return false;
			auto lit = lhs->param_begin();
			auto rit = rhs->param_begin();
			for (;lit != lhs->param_end(); lit++,rit++)
			{
				r1 = typeKindOf(*lit, isStrict);
				r2 = typeKindOf(*rit, isStrict);
				if (r1 != r2)
					return false;
				// TODO: add a check to make sure that pointers with and without constant offsets dont match
			}
			return true;
		}
	};

	struct GlobalDataChunk
	{
	public:
		GlobalDataChunk(uint32_t address, std::vector<uint8_t> &rawData, uint32_t start, uint32_t length) : address(address), view(&rawData[start], length)
		{
		}
		uint32_t address;
		llvm::ArrayRef<uint8_t> view;
	};
	const GlobalDataChunk &getGlobalDataChunk(uint32_t number) const;

	static std::string getFunctionTableName(const llvm::FunctionType* ft)
	{
		auto getTypeKindChar = [](const llvm::Type* ty)
		{
			switch (typeKindOf(ty, /*isStrict*/true))
			{
				case TypeKind::Void:
					return 'v';
				case TypeKind::Integer:
				case TypeKind::RawPointer:
					return 'i';
				case TypeKind::Integer64:
					return 'j';
				case TypeKind::RefPointer:
					return 'r';
				case TypeKind::Double:
					return 'd';
				case TypeKind::Float:
					return 'f';
				case TypeKind::Vector:
					break;
			}
			llvm_unreachable("");
		};
		std::string table_name;
		llvm::Type* ret = ft->getReturnType();
		table_name += getTypeKindChar(ret);
		for (const auto& param : ft->params())
		{
			table_name += getTypeKindChar(param);
		}
		return table_name;
	}

	/**
	 * Used to store the information needed to compile and use the asm.js
	 * function tables for indirect calls
	 */
	typedef std::unordered_map<const llvm::FunctionType*,FunctionTableInfo,
		  FunctionSignatureHash,FunctionSignatureCmp> FunctionTableInfoMap;
	typedef std::vector<const llvm::FunctionType*> FunctionTableOrder;
	/**
	 * Used to assign asm.js function pointers
	 */
	typedef std::unordered_map<const llvm::Function*, int32_t> FunctionAddressesMap;
	/**
	 * Used to assign asm.js globals
	 */
	typedef std::unordered_map<const llvm::GlobalVariable*, int32_t> GlobalAddressesMap;
	typedef std::unordered_map<int32_t, const llvm::GlobalVariable*> InverseGlobalAddressesMap;

	typedef std::unordered_map<const llvm::FunctionType*, size_t,
		FunctionSignatureHash,FunctionSignatureCmp> FunctionTypeIndicesMap;

	typedef std::unordered_map<const llvm::Type*, int32_t> GCTypesIndicesMap;

	typedef std::unordered_map<const llvm::StructType*, std::vector<const llvm::StructType*>> DirectBaseToSubTypesMap;

	typedef std::unordered_map<const llvm::Type*, uint32_t> TypeToIndexMap;

	// Used to get the right localId for an argument, where the argument's number
	// can be used as an index in the vector to ge the right id 
	typedef std::unordered_map<const llvm::Function*, std::vector<uint32_t>> ArgumentLocalIdMap;

	LinearMemoryHelper(const LinearMemoryHelperInitializer& data) :
			module(nullptr), globalDeps(nullptr),
		mode(data.mode), functionTables(3, FunctionSignatureHash(/*isStrict*/false), FunctionSignatureCmp(/*isStrict*/false)),
		functionTypeIndices(3, FunctionSignatureHash(/*isStrict*/false), FunctionSignatureCmp(/*isStrict*/false)),
		GCTypeCount(0), maxTypeIdx(0), maxFunctionId(0), memorySize(data.memorySize*1024*1024),
		stackSize(data.stackSize*1024*1024), stackOffset((data.stackOffset+7) & ~7), growMem(data.growMem),
		hasAsmjsMem(data.hasAsmjsMem)
	{
	}
	bool runOnModule(llvm::Module& module, GlobalDepsAnalyzer* GDA)
	{
		this->module = &module;
		globalDeps = GDA;
		builtinIds.fill(std::numeric_limits<uint32_t>::max());

		addGCTypes();
		addStack();
		addGlobals();
		checkMemorySize();
		addMemoryInfo();

		return false;
	}

	void populateGlobalData();
	void addFunctions(const PointerAnalyzer* PA);

	uint32_t getGlobalVariableAddress(const llvm::GlobalVariable* G) const;
	const llvm::GlobalVariable* getGlobalVariableFromAddress(llvm::Value* C) const;
	uint32_t getFunctionAddress(const llvm::Function* F) const;
	bool functionHasAddress(const llvm::Function* F) const;
	uint32_t getFunctionAddressMask(const llvm::FunctionType* Fty) const;
	llvm::Type* getSplitRegularType(void) const;
	llvm::Type* getRegularType(void) const;
	FunctionTableInfoMap& getFunctionTables()
	{
		return functionTables;
	}
	const FunctionTableInfoMap& getFunctionTables() const
	{
		return functionTables;
	}
	const FunctionTableOrder& getFunctionTableOrder() const
	{
		return functionTableOrder;
	}

	/**
	 * Get a list of the asm.js functions. This list excludes functions without
	 * an "asmjs" section. This list does also exclude native WebAssembly
	 * intrinsics (like sqrt, abs, copysign, etc.), when cheerp-mode is set to
	 * WebAssembly.
	 */
	const std::vector<const llvm::Function*> & functions() const {
		return asmjsFunctions_;
	}

	const std::unordered_set<const llvm::StructType *>& getDowncastArrayClasses() const {
		return downcastArrayClasses;
	}

	bool hasDowncastArray(const llvm::StructType* sTy) const {
		return (downcastArrayClasses.count(sTy) > 0);
	}

	bool isSuperType(const llvm::StructType* sTy) const {
		return (superTypes.count(sTy) > 0);
	}

	int32_t getGCTypeIndex(const llvm::Type* Ty, POINTER_KIND kind, bool needsDowncastArray = false) const;

	const GCTypesIndicesMap& getGCTypeIndices() const {
		return GCTypeIndices;
	}

	int32_t getGCTypeCount() const {
		return GCTypeCount;
	}

	uint32_t getCreatePointerArrayFuncTypeIndex() const {
		return createPointerArrayFuncTypeIndex;
	}

	uint32_t getCreatePointerArrayFuncId() const {
		return createPointerArrayFuncId;
	}

	const std::vector<const llvm::Type*> getTypesUsedForDynResizeFuncTypes() const {
		return typesUsedForDynResizeFuncTypes;
	}
	
	const TypeToIndexMap& getDynResizeFuncTypeIndices() const {
		return resizeArrayFuncTypeIndices;
	}

	const TypeToIndexMap& getResizeArrayFuncIds() const {
		return resizeArrayFuncIds;
	}

	const std::vector<llvm::Type*>& getTypesThatRequireCreateArrayFunc() const {
		return typesThatRequireCreateArrayFunc;
	}
	
	int32_t getCreateArrayFuncTypeIndex() const {
		return createArrayFuncTypeIndex;
	}


	const TypeToIndexMap& getCreateArrayFuncIds() const {
		return createArrayFuncIds;
	}

	// the init functions for downcasts will be encoded with direct type ID's
	const TypeToIndexMap& getDowncastFuncTypeIndices() const {
		return downcastFuncTypeIndices;
	}

	const TypeToIndexMap& getDowncastFuncIds() const {
		return downcastFuncIds;
	}

	const FunctionTypeIndicesMap& getFunctionTypeIndices() const {
		return functionTypeIndices;
	}

	const std::unordered_map<const llvm::Function*, uint32_t>& getFunctionIds() const {
		return functionIds;
	}

	uint32_t getStackStart() const {
		return stackStart;
	}
	uint32_t getHeapStart() const {
		return heapStart;
	}
	uint32_t getAmountChunks() const {
		return globalDataChunks.size();
	}

	/**
	 * Vector of distinct function types that corresponds to the function list,
	 * and are ordered by the appearence in that list.
	 */
	const std::vector<const llvm::FunctionType*> getFunctionTypes() const {
		return functionTypes;
	}

	uint32_t getArgumentLocalId(const llvm::Argument* arg, uint32_t elemIdx) const;

	const std::vector<const llvm::Type*> getGCTypes() const {
		return GCTypes;
	}

	int32_t getRegularObjectIdx() const {
		return regularObjectIdx;
	}

	int32_t getSplitRegularObjectIdx() const {
		return splitRegularObjectIdx;
	}

	/**
	 * Get a list of the asm.js global variables. This list excludes global
	 * variables without an "asmjs" section.
	 */
	const std::vector<const llvm::GlobalVariable*> & globals() const {
		return asmjsGlobals;
	}

	const std::vector<const llvm::GlobalVariable*> & addressableGlobals() const {
		return asmjsAddressableGlobals;
	}

	typedef llvm::DenseMap<const llvm::GlobalVariable*, uint32_t> GlobalUsageMap;
	const GlobalUsageMap& getGlobalizedGlobalUsage() const
	{
		return globalizedGlobalsUsage;
	}

	struct ByteListener
	{
		virtual void addByte(uint8_t b) = 0;
		virtual ~ByteListener()
		{
		}
	};
	void compileConstantAsBytes(const llvm::Constant* c, bool asmjs, ByteListener* listener, int32_t offset=0) const;
	bool isZeroInitializer(const llvm::Constant* c) const;
	bool hasNonZeroInitialiser(const llvm::GlobalVariable* G) const;
	struct GepListener
	{
		virtual void addValue(const llvm::Value* v, uint32_t size) = 0;
		virtual void subValue(const llvm::Value* v, uint32_t size)
		{
			assert(false);
		}
		virtual void addConst(int64_t v) = 0;
		virtual bool isInlineable(const llvm::Value* p) = 0;
		virtual bool hasSubValue() const
		{
			return false;
		}
		virtual ~GepListener()
		{
		}
	};
	class LinearGepListener: public GepListener
	{
	private:
		const PointerAnalyzer& PA;
	public:
		LinearGepListener(const PointerAnalyzer& PA):PA(PA)
		{
		}
		bool isInlineable(const llvm::Value* p) override;
	};
	static int64_t compileGEPOperand(const llvm::Value* idxVal, uint32_t size, GepListener* listener, bool invert);
	// Returns the base of the compiled expression
	const llvm::Value* compileGEP(const llvm::Value* p, GepListener* listener, const PointerAnalyzer* PA) const;
	static const llvm::Value* compileGEP(const llvm::Module* module, const llvm::Value* p, GepListener* listener, const PointerAnalyzer* PA);

	uint32_t getBuiltinId(BuiltinInstr::BUILTIN b) const
	{
		assert(builtinIds[b] != std::numeric_limits<uint32_t>::max());
		return builtinIds[b];
	}

	bool canGrowMemory() const {
		return growMem;
	}
private:
	// Different kind of types for the purpose of comparing function signatures
	enum TypeKind {
		Void = 0,
		Integer,
		Integer64,
		Double,
		Float,
		RawPointer,
		RefPointer,
		Vector,
	};
	static TypeKind typeKindOf(const llvm::Type* type, bool isStrict)
	{
		// TODO: Should be removable after we stop hacking GC types in from the JS side
		// Is needed to have function type comparisons working properly
		if (TypeSupport::isTypeGC(type))
			return TypeKind::RefPointer;

		if (type->isIntegerTy(64))
			return TypeKind::Integer64;
		if (type->isIntegerTy())
			return TypeKind::Integer;
		if (type->isDoubleTy())
			return TypeKind::Double;
		if (type->isFloatTy())
			return TypeKind::Float;
		if (type->isPointerTy() && TypeSupport::isRawPointer(type, true))
			return (isStrict ? TypeKind::RawPointer : TypeKind::Integer);
		if (type->isPointerTy() || type->isAggregateType())
			return TypeKind::RefPointer;
		if (type->isVoidTy())
			return TypeKind::Void;
		if (type->isVectorTy())
			return TypeKind::Vector;
		llvm_unreachable("unrecognized type kind");
	}

	// The VectorWriter struct is used while populating the global data.
	struct VectorWriter : public ByteListener
	{
		uint32_t address;
		uint32_t currentZeroStreak;
		uint32_t splitThreshold;
		uint32_t maxChunks;
		uint32_t startOfChunk;
		uint32_t lastNonZero;
		uint32_t startAddress;
		bool isDataAvailable;
		std::vector<uint8_t> &rawData;
		std::vector<GlobalDataChunk> &chunks;
		VectorWriter(std::vector<uint8_t> &rawGlobalData, std::vector<GlobalDataChunk> &chunks, uint32_t splitThreshold, uint32_t maxChunks, uint32_t startAddress) : address(0), currentZeroStreak(0), splitThreshold(splitThreshold), maxChunks(maxChunks), startOfChunk(0), lastNonZero(0), startAddress(startAddress), isDataAvailable(false), rawData(rawGlobalData), chunks(chunks)
		{
		}
		void addByte(uint8_t b) override;
		void setAddress(uint32_t newAddress)
		{
			uint32_t offsetAddress = newAddress - startAddress;
			currentZeroStreak += (offsetAddress - address);
			address = offsetAddress;
		}
		bool splitChunk(bool force = false, bool hasAsmjsMem = false);
	};

	void setGlobalPtrIfPresent(llvm::StringRef name, uint32_t ptr);
	void addGlobals();
	void addGCTypes();
	void addStack();
	void addMemoryInfo();
	void checkMemorySize();

	void dependencyDFS(const llvm::Type* Ty, std::unordered_set<const llvm::Type*>& assignedTypes, std::vector<const llvm::Type*>& sorted);
	void dependencySort(std::vector<const llvm::Type*>& allTypes, std::vector<const llvm::Type*>& sorted);

	void markSubtypesForDowncast(const llvm::StructType* directBase, DirectBaseToSubTypesMap& subTypeMap);
	void cacheDowncastArrayClassesRecursive(llvm::StructType* sTy, const TypeSupport& types, DirectBaseToSubTypesMap& subTypeMap);
	void cacheDowncastArrayClasses(DirectBaseToSubTypesMap& subTypeMap);

	llvm::Module* module;
	GlobalDepsAnalyzer* globalDeps;

	LinearMemoryHelperInitializer::FunctionAddressMode mode;

	FunctionTableInfoMap functionTables;
	FunctionTableOrder functionTableOrder;
	std::vector<const llvm::Function*> asmjsFunctions_;

	std::unordered_map<const llvm::Function*, uint32_t> functionIds;
	std::array<uint32_t, BuiltinInstr::numGenericBuiltins()> builtinIds;
	std::unordered_set<const llvm::StructType*> superTypes;
	void cacheFunctionArgumentLocalIds(const PointerAnalyzer* PA, const llvm::Function* F);
	ArgumentLocalIdMap localIdsForArguments;
	TypeToIndexMap downcastFuncIds;
	TypeToIndexMap downcastFuncTypeIndices;
	TypeToIndexMap createArrayFuncIds;
	TypeToIndexMap resizeArrayFuncIds;
	TypeToIndexMap resizeArrayFuncTypeIndices;
	// All struct, array and pointer types can be collapsed down to a split regular type
	// these variables hold all the types that are required for the function types
	std::vector<const llvm::Type*> typesUsedForDynResizeFuncTypes;
	std::vector<const llvm::Type*> typesUsedForCreateArrayFuncTypes;
	// non-aggregate types do not have to be allocated though a create array function
	// this vector hold all the types that do require a separate function
	std::vector<llvm::Type*> typesThatRequireCreateArrayFunc;
	uint32_t createArrayFuncTypeIndex;
	uint32_t createPointerArrayFuncId;
	uint32_t createPointerArrayFuncTypeIndex;
	std::vector<const llvm::FunctionType*> functionTypes;
	std::vector<const llvm::Type*> GCTypes;
	std::unordered_set<const llvm::StructType*> downcastArrayClasses;
	// This map does not include function type indices for extra added functions
	// like the downcast or array initializers, they have their own maps
	FunctionTypeIndicesMap functionTypeIndices;
	GCTypesIndicesMap GCTypeIndices;
	int32_t GCTypeCount;
	uint32_t maxTypeIdx;
	uint32_t maxFunctionId;
	int32_t regularObjectIdx;
	int32_t splitRegularObjectIdx;

	std::vector<const llvm::GlobalVariable*> asmjsGlobals;
	std::vector<const llvm::GlobalVariable*> asmjsAddressableGlobals;
	GlobalUsageMap globalizedGlobalsUsage;
	void generateGlobalizedGlobalsUsage();
	std::vector<uint8_t> rawGlobalData;
	std::vector<GlobalDataChunk> globalDataChunks;

	FunctionAddressesMap functionAddresses;
	GlobalAddressesMap globalAddresses;
	InverseGlobalAddressesMap inverseGlobalAddresses;
	// The next address available to allocate global variables.
	// The heap space will start after the last global variable allocation
	uint32_t heapStart{8};
	// Total memory size
	uint32_t memorySize;
	// Stack size
	uint32_t stackSize;
	// Stack start (it grows downwards)
	uint32_t stackStart;
	// Offset from 0x0 to the stack top. Primarily used with Asan to reserve
	// the lower addresses for null pointer checks
	uint32_t stackOffset;
	// Whether memory can grow at runtime or not
	bool growMem;
	// Whether there is an extra asmjs memory file.
	bool hasAsmjsMem;
	llvm::ModuleAnalysisManager* MAM;
	friend LinearMemoryHelperWrapper;
};


class LinearMemoryHelperWrapper {
	static LinearMemoryHelper* innerPtr;
public:
	static LinearMemoryHelper& getInner(llvm::ModuleAnalysisManager& MAM, LinearMemoryHelperInitializer& data)
	{
		if (innerPtr)
			delete innerPtr;
		innerPtr = new LinearMemoryHelper(data);
		innerPtr->MAM = &MAM;
		return *innerPtr;
	}
	operator LinearMemoryHelper&()
	{
		assert(innerPtr);
		return *innerPtr;
	}
  bool invalidate(llvm::Module& M, const llvm::PreservedAnalyses& PA, llvm::ModuleAnalysisManager::Invalidator &)
  {
		auto PAC = PA.getChecker<LinearMemoryAnalysis>();
		return !(PAC.preserved() || PAC.preservedSet<llvm::AllAnalysesOn<llvm::Module>>());
  }
};

/// Analysis pass which computes a \c DominatorTree.
class LinearMemoryAnalysis : public llvm::AnalysisInfoMixin<LinearMemoryAnalysis> {
  friend llvm::AnalysisInfoMixin<LinearMemoryAnalysis>;
  static llvm::AnalysisKey Key;
  
public:
  /// Provide the result typedef for this analysis pass.
  using Result = LinearMemoryHelperWrapper;

  /// Run the analysis pass over a function and produce a dominator tree.
  Result run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM)
  {
	static llvm::Module* modulePtr = nullptr;
	assert(modulePtr != &M);
	modulePtr = &M;
	return LinearMemoryHelperWrapper();
  }
};

class LinearMemoryHelperPass : public llvm::PassInfoMixin<LinearMemoryHelperPass> {
	LinearMemoryHelperInitializer data;
public:
	llvm::PreservedAnalyses run(llvm::Module &M, llvm::ModuleAnalysisManager& MAM)
	{
		LinearMemoryHelper& LMH = MAM.getResult<LinearMemoryAnalysis>(M).getInner(MAM, data);
		GlobalDepsAnalyzer& GDA = MAM.getResult<GlobalDepsAnalysis>(M);
		LMH.runOnModule(M, &GDA);

		return llvm::PreservedAnalyses::all();
	}
	LinearMemoryHelperPass(const LinearMemoryHelperInitializer& data) : data(data) {}
	static bool isRequired() { return true; }
};

}

#endif
