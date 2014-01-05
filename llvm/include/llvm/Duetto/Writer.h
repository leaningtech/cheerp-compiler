//===-- Duetto/Writer.h - The Duetto JavaScript generator -------------===//
//
//                     Duetto: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2013 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _DUETTO_WRITER_H
#define _DUETTO_WRITER_H

#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Support/FormattedStream.h"
#include <set>
#include <map>

//De-comment this to debug why a global is included in the JS
//#define DEBUG_GLOBAL_DEPS

namespace duetto
{

class DuettoWriter
{
private:
	struct Fixup
	{
		const llvm::GlobalVariable* base;
		std::string baseName;
		const llvm::Constant* value;
		Fixup(const llvm::GlobalVariable* b, const std::string& bn, const llvm::Constant* v):
			base(b),baseName(bn),value(v)
		{
		}
	};

	llvm::Module& module;
	llvm::DataLayout targetData;
	const llvm::Function* currentFun;
	std::set<llvm::StructType*> classesNeeded;
	std::set<llvm::StructType*> arraysNeeded;
	std::set<const llvm::GlobalValue*> globalsDone;
#ifdef DEBUG_GLOBAL_DEPS
	std::map<const llvm::GlobalValue*, const llvm::GlobalValue*> globalsQueue;
#else
	std::set<const llvm::GlobalValue*> globalsQueue;
#endif
	typedef std::multimap<const llvm::GlobalVariable*, Fixup> FixupMapType;
	FixupMapType globalsFixupMap;
	bool printMethodNames;
	bool printLambdaBridge;
	bool printHandleVAArg;
	bool printCreateArrayPointer;
	uint32_t getIntFromValue(const llvm::Value* v) const;
	bool isValidTypeCast(const llvm::Value* cast, const llvm::Value* castOp, llvm::Type* src, llvm::Type* dst) const;
	bool isClientType(const llvm::Type* t) const;
	bool isClientGlobal(const char* mangledName) const;
	bool isI32Type(llvm::Type* t) const;
	bool isTypedArrayType(llvm::Type* t) const;
	void compileTypedArrayType(llvm::Type* t);
	bool isComingFromAllocation(const llvm::Value* val) const;
	bool isNopCast(const llvm::Value* val) const;
	bool isValidVoidPtrSource(const llvm::Value* val) const;
	bool isValidVoidPtrSource(const llvm::Value* val, std::set<const llvm::PHINode*>& visitedPhis) const;
	bool isInlineable(const llvm::Instruction& I) const;
	bool isBitCast(const llvm::Value* v) const;
	bool isGEP(const llvm::Value* v) const;
	bool isImmutableType(const llvm::Type* t) const;
	bool isUnion(const llvm::Type* t) const;
	void compileTerminatorInstruction(const llvm::TerminatorInst& I);
	void compileTerminatorInstruction(const llvm::TerminatorInst& I,
			const std::map<const llvm::BasicBlock*, uint32_t>& blocksMap);
	bool compileInlineableInstruction(const llvm::Instruction& I);
	void addSelfPointer(const llvm::Value* obj);
	// COMPILE_ADD_SELF is returned by AllocaInst when a self pointer must be added to the returned value
	enum COMPILE_INSTRUCTION_FEEDBACK { COMPILE_OK = 0, COMPILE_UNSUPPORTED, COMPILE_ADD_SELF };
	COMPILE_INSTRUCTION_FEEDBACK compileNotInlineableInstruction(const llvm::Instruction& I);
	enum COMPILE_FLAG { NORMAL = 0, DRY_RUN = 1 };
	const llvm::Type* compileRecursiveAccessToGEP(llvm::Type* curType, const llvm::Use* it,
			const llvm::Use* const itE, COMPILE_FLAG flag);
	void compilePredicate(llvm::CmpInst::Predicate p);
	void compileOperandForIntegerPredicate(const llvm::Value* v, llvm::CmpInst::Predicate p);
	void compileEqualPointersComparison(const llvm::Value* lhs, const llvm::Value* rhs, llvm::CmpInst::Predicate p);
	void compileType(llvm::Type* t);
	void compileTypeImpl(llvm::Type* t);
	enum POINTER_KIND { UNDECIDED = 0, COMPLETE_OBJECT, COMPLETE_ARRAY, REGULAR };
	POINTER_KIND getPointerKind(const llvm::Value* v, std::map<const llvm::PHINode*, POINTER_KIND>& visitedPhis);
	POINTER_KIND getPointerKind(const llvm::Value* v);
	/*
	 * \param v The pointer to dereference, it may be a regular pointer, a complete obj or a complete array
	 * \param offset An offset coming from code, which may be also NULL
	 * \param namedOffset An offset that will be added verbatim to the code
	 */
	void compileDereferencePointer(const llvm::Value* v, const llvm::Value* offset, const char* namedOffset = NULL);
	void compileFastGEPDereference(const llvm::Value* operand, const llvm::Use* idx_begin, const llvm::Use* idx_end);
	void compileGEP(const llvm::Value* val, const llvm::Use* it, const llvm::Use* const itE);
	const llvm::Type* compileObjectForPointerGEP(const llvm::Value* val, const llvm::Use* it,
			const llvm::Use* const itE, COMPILE_FLAG flag);
	bool compileOffsetForPointerGEP(const llvm::Value* val, const llvm::Use* it, const llvm::Use* const itE,
			const llvm::Type* lastType);
	const llvm::Type* compileObjectForPointer(const llvm::Value* val, COMPILE_FLAG flag);
	/*
	 * Returns true if anything is printed
	 */
	bool compileOffsetForPointer(const llvm::Value* val, const llvm::Type* lastType);
	llvm::Type* findRealType(const llvm::Value* v, std::set<const llvm::PHINode*>& visitedPhis) const;
	void compileMove(const llvm::Value* dest, const llvm::Value* src, const llvm::Value* size);
	enum COPY_DIRECTION { FORWARD=0, BACKWARD, RESET };
	void compileMemFunc(const llvm::Value* dest, const llvm::Value* src, const llvm::Value* size,
			COPY_DIRECTION copyDirection);
	void compileCopyRecursive(const std::string& baseName, const llvm::Value* baseDest,
		const llvm::Value* baseSrc, const llvm::Type* currentType, const char* namedOffset);
	void compileResetRecursive(const std::string& baseName, const llvm::Value* baseDest,
		const llvm::Value* resetValue, const llvm::Type* currentType, const char* namedOffset);
	void compileDowncast(const llvm::Value* src, uint32_t baseOffset);
	void compileAllocation(const llvm::Value* callV, const llvm::Value* size);
	void compileFree(const llvm::Value* obj);
	void compilePointer(const llvm::Value* v, POINTER_KIND acceptedKind);
	void compileOperandImpl(const llvm::Value* v);
	enum NAME_KIND { LOCAL=0, GLOBAL=1 };
	void printLLVMName(const llvm::StringRef& s, NAME_KIND nameKind) const;
	void printVarName(const llvm::Value* v);
	void printArgName(const llvm::Argument* v) const;
	void compileMethodArgs(const llvm::User::const_op_iterator it, const llvm::User::const_op_iterator itE);
	void handleBuiltinNamespace(const char* ident, llvm::User::const_op_iterator it,
			llvm::User::const_op_iterator itE);
	bool handleBuiltinCall(const char* ident, const llvm::Value* callV,
			llvm::User::const_op_iterator it, llvm::User::const_op_iterator itE, bool userImplemented);
	bool safeUsagesForNewedMemory(const llvm::Value* v) const;
	bool safeCallForNewedMemory(const llvm::CallInst* ci) const;
	uint32_t getUniqueIndexForValue(const llvm::Value* v);
	uint32_t getUniqueIndex();
	std::map<const llvm::Value*, uint32_t> unnamedValueMap;
	uint32_t currentUniqueIndex;
	void compileMethod(const llvm::Function& F);
	void compileGlobal(const llvm::GlobalVariable& G);
	void gatherDependencies(const llvm::Constant* C, const llvm::GlobalVariable* base,
			const llvm::Twine& baseName, const llvm::Constant* value);
	uint32_t compileClassTypeRecursive(const std::string& baseName, llvm::StructType* currentType, uint32_t baseCount);
	void compileClassType(llvm::StructType* T);
	void compileArrayClassType(llvm::StructType* T);
	void compileArrayPointerType();
	void compileLambdaBridge();
	void compileHandleVAArg();
	enum OperandFix{ OPERAND_NO_FIX = 0, OPERAND_EXPAND_COMPLETE_OBJECTS };
	void compileConstantExpr(const llvm::ConstantExpr* ce);
	enum CONSTRUCTOR_ACTION { ADD_TO_QUEUE=0, COMPILE=1 };
	void handleConstructors(llvm::GlobalVariable* GV, CONSTRUCTOR_ACTION action);
	void compileNullPtrs();
	static uint32_t getMaskForBitWidth(int width);
	void compileSignedInteger(const llvm::Value* v);
	void compileUnsignedInteger(const llvm::Value* v);
public:
	llvm::raw_ostream& stream;
	DuettoWriter(llvm::Module& m, llvm::raw_ostream& s):
		module(m),targetData(&m),currentFun(NULL),printMethodNames(false),printLambdaBridge(false),printHandleVAArg(false),
		printCreateArrayPointer(false),currentUniqueIndex(0),stream(s)
	{
	}
	void makeJS();
	void compileBB(const llvm::BasicBlock& BB, const std::map<const llvm::BasicBlock*, uint32_t>& blocksMap);
	void compileOperand(const llvm::Value* v, OperandFix fix = OPERAND_NO_FIX);
	void compileConstant(const llvm::Constant* c);
	void compilePHIOfBlockFromOtherBlock(const llvm::BasicBlock* to, const llvm::BasicBlock* from);
};

}
#endif
