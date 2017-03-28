//===-- Cheerp/TypeOptimizer.h - Cheerp utility code --------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2015 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef CHEERP_TYPE_OPTIMIZER_H
#define CHEERP_TYPE_OPTIMIZER_H

#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include <map>
#include <set>

namespace cheerp
{

class TypeOptimizer: public llvm::ModulePass
{
private:
	struct TypeMappingInfo
	{
		enum MAPPING_KIND { IDENTICAL, COLLAPSED, COLLAPSING, COLLAPSING_BUT_USED, BYTE_LAYOUT_TO_ARRAY, POINTER_FROM_ARRAY, FLATTENED_ARRAY,
						MERGED_MEMBER_ARRAYS, MERGED_MEMBER_ARRAYS_AND_COLLAPSED };
		llvm::Type* mappedType;
		MAPPING_KIND elementMappingKind;
		TypeMappingInfo():mappedType(NULL),elementMappingKind(IDENTICAL)
		{
		}
		TypeMappingInfo(llvm::Type* t, MAPPING_KIND k):mappedType(t),elementMappingKind(k)
		{
		}
		operator llvm::Type*() const
		{
			return mappedType;
		}
		static bool isCollapsedStruct(MAPPING_KIND k)
		{
			return k == COLLAPSED || k == MERGED_MEMBER_ARRAYS_AND_COLLAPSED;
		}
	};
	llvm::Module* module;
	const llvm::DataLayout* DL;
	std::unordered_map<llvm::StructType*,std::set<llvm::StructType*>> downcastSourceToDestinationsMapping;
	std::unordered_map<llvm::StructType*, std::vector<std::pair<uint32_t, uint32_t>>> membersMappingData;
	std::unordered_map<llvm::GlobalVariable*, llvm::Constant*> globalsMapping;
	std::unordered_map<llvm::GlobalValue*, llvm::Type*> globalTypeMapping;
	std::unordered_map<llvm::StructType*, llvm::Type*> baseTypesForByteLayout;
	std::unordered_map<llvm::Type*, TypeMappingInfo> typesMapping;
	std::unordered_set<llvm::Function*> pendingFunctions;
	// In this context a field "escapes" if it has any use which is not just a load/store
	std::unordered_set<std::pair<llvm::StructType*, uint32_t>, PairHash<llvm::StructType*, uint32_t>> escapingFields;
#ifndef NDEBUG
	std::unordered_set<llvm::Type*> newStructTypes;
#endif
	llvm::Constant* rewriteGlobal(llvm::GlobalVariable* GV);
	void rewriteGlobalInit(llvm::GlobalVariable* GV);
	TypeMappingInfo rewriteType(llvm::Type* t);
	void rewriteUses(llvm::Value* V, llvm::Value* NewV);
	std::pair<llvm::Constant*, uint8_t> rewriteConstant(llvm::Constant* C);
	void rewriteFunction(llvm::Function* F);
	void rewriteIntrinsic(llvm::Function* F, llvm::FunctionType* FT);
	void gatherAllTypesInfo(const llvm::Module& M);
	uint8_t rewriteGEPIndexes(llvm::SmallVector<llvm::Value*, 4>& newIndexes, llvm::Type* ptrType, llvm::ArrayRef<llvm::Use> idxs,
				llvm::Type* targetType, llvm::Instruction* insertionPoint);
	bool isUnsafeDowncastSource(llvm::StructType* st);
	void addAllBaseTypesForByteLayout(llvm::StructType* st, llvm::Type* base);
	static void pushAllBaseConstantElements(llvm::SmallVector<llvm::Constant*, 4>& newElements, llvm::Constant* C, llvm::Type* baseType);
	// Helper function to handle the various kind of arrays in constants
	static void pushAllArrayConstantElements(llvm::SmallVector<llvm::Constant*, 4>& newElements, llvm::Constant* array);
	static llvm::StructType* isEscapingStructGEP(const llvm::User* GEP);
public:
	static char ID;
	explicit TypeOptimizer() : ModulePass(ID) { }
	bool runOnModule(llvm::Module &M);
	const char *getPassName() const;
};

//===----------------------------------------------------------------------===//
//
// TypeOptimizer - Change the layout of structs and arrays to reduce the number of allocated objects at runtime
//
inline llvm::ModulePass *createTypeOptimizerPass()
{
	return new TypeOptimizer();
}

}

#endif
