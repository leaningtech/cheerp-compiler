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
	bool isClientArrayType(const llvm::Type* t) const;
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
	// COMPILE_ADD_SELF is returned by AllocaInst when a self pointer must be added to the returned value
	// COMPILE_EMPTY is returned if there is no need to add a ;\n to end the line
	enum COMPILE_INSTRUCTION_FEEDBACK { COMPILE_OK = 0, COMPILE_UNSUPPORTED, COMPILE_ADD_SELF, COMPILE_EMPTY };
	COMPILE_INSTRUCTION_FEEDBACK compileTerminatorInstruction(const llvm::TerminatorInst& I);
	COMPILE_INSTRUCTION_FEEDBACK compileTerminatorInstruction(const llvm::TerminatorInst& I,
			const std::map<const llvm::BasicBlock*, uint32_t>& blocksMap);
	bool compileInlineableInstruction(const llvm::Instruction& I);
	void addSelfPointer(const llvm::Value* obj);
	COMPILE_INSTRUCTION_FEEDBACK compileNotInlineableInstruction(const llvm::Instruction& I);
	enum COMPILE_FLAG { NORMAL = 0, DRY_RUN = 1, GEP_DIRECT = 2 };
	const llvm::Type* compileRecursiveAccessToGEP(llvm::Type* curType, const llvm::Use* it,
			const llvm::Use* const itE, COMPILE_FLAG flag);
	void compilePredicate(llvm::CmpInst::Predicate p);
	void compileOperandForIntegerPredicate(const llvm::Value* v, llvm::CmpInst::Predicate p);
	void compileEqualPointersComparison(const llvm::Value* lhs, const llvm::Value* rhs, llvm::CmpInst::Predicate p);
	enum COMPILE_TYPE_STYLE { LITERAL_OBJ=0, THIS_OBJ };
	void compileType(llvm::Type* t, COMPILE_TYPE_STYLE style);
	void compileTypeImpl(llvm::Type* t, COMPILE_TYPE_STYLE style);
	
	/**
	 * \addtogroup pointers Pointer implementation
	 * 
	 * Three type of pointers are used:
	 *   - COMPLETE_OBJECT This pointer can point only to a C++ struct/class type. It is implemented 
	 *                     by directly binding a JS var to the pointed object. This type of pointer
	 *                     does not support arithmetic nor ordering. In order to allow conversion to REGULAR pointer,
	 *                     the object bound to a COMPLETE_OBJECT pointer shall contain the member "s", which is a JS ref to the object itself.
	 *   - COMPLETE_ARRAY  This pointer points to the first element of a C++ array of struct/class or primitive type.
	 *                     It is implemented in JS by binding a var to the pointed array. It does not support
	 *                     global ordering.
	 *   - REGULAR         Regular pointers are implemented with a JS object of the form: "{ d : OBJ, o : OFFSET }", where
	 *                     OBJ is a JS reference to a *container* object of the pointer, and OFFSET is the offset of the pointed
	 *                     object inside the container object (it can be a string or a integral value).
	 *                     Pointers of type COMPLETE_OBJECT and COMPLETE_ARRAY are always convertible to REGULAR pointers,
	 *                     but not viceversa. REGULAR pointers support pointer arithmetic; two REGULAR pointers are ordinable 
	 *                     if they have the same container object. 
	 * 
	 * General rules (which apply both to JS immutable types and to struct/class objects):
	 *    -# In order for an object to be bound to a regular pointer, a "container" object must exists. If the 
	 *       object being bound to a pointer is a member object of some class/struct type or a member of an array the 
	 *       container object naturally exists. Otherwise such object must be created inside an array of size one.
	 * 
	 *    -# An array of objects can always be bound to a COMPLETE_ARRAY pointer, just by taking its name.
	 * 
	 *    -# A single object can always be bound to a COMPLETE_OBJECT pointer, just by taking its name.
	 * 
	 *    -# A COMPLETE_ARRAY pointer can be converted to a REGULAR pointer by setting the "d" member to the COMPLETE_ARRAY pointer
	 *       name, and the "o" member to zero.
	 * 
	 *    -# A COMPLETE_OBJECT pointer can be converted to a REGULAR pointer using the self member. Notice that
	 *       arithmetic operations on such pointer are still available, provided the user does not dereference the result, and generate
	 *       a on offset value like "s1, s2" etc.
	 *       
	 * 
	 * Optimization:
	 *    -  Remove self-pointer. Avoid the creation of the member ".s" if the conversion to REGULAR pointer is not required.
	 * 
	 * 
	 * @{
	 */
	
	enum POINTER_KIND {
		UNDECIDED = 0,
		COMPLETE_OBJECT,
		COMPLETE_ARRAY,
		REGULAR
	};
	
	typedef std::map<const llvm::Value *, POINTER_KIND> pointer_kind_map_t;
	mutable pointer_kind_map_t pointerKindMap;

	POINTER_KIND dfsPointerKind(const llvm::Value* v, std::map<const llvm::PHINode*, POINTER_KIND>& visitedPhis) const;
	POINTER_KIND getPointerKind(const llvm::Value* v) const;
	
	// Functionalities provided by a pointer
	enum POINTER_USAGE_FLAG {
		POINTER_NONCONST_DEREF = 0x1, // The pointer is used to modify the pointed object
		POINTER_ARITHMETIC = 0x2, // The pointer can be incremented/decremented etc, and/or it is used to access an array (i.e. p[i])
		POINTER_ORDINABLE = 0x4, // The pointer is used for a comparison with another pointer
		POINTER_CASTABLE_TO_INT = 0x8,  // The pointer is explicitly casted to an integer (usually used to implement pointers hash table)
		
		POINTER_UNKNOWN = (1LL << 32LL) - 1
	};

	typedef std::map<const llvm::Value *, uint32_t> pointer_usage_map_t;
	
	// Returns a bitmask of POINTER_USAGE_FLAG
	/** 
	 * Compute the usage of a single pointer, regardless of the phi nodes
	 */
	//TODO at the moment if it is used in a CallInst it returns POINTER_UNKNOWN.
	// CallInst should be handled inside getPointerUsageFlagsComplete, in order to provide information on how that pointer is used inside the function call.
	// This is especially important at the moment for memset/memcpy/memmove.
	uint32_t getPointerUsageFlags(const llvm::Value* v) const;
	mutable pointer_usage_map_t pointerUsageMap;
	
	/**
	 * Compute the sum of the usages of all the "child" pointers, where "child pointer" means any pointer which can be initialized to this one's value.
	 * \param v The pointer to inspect.
	 * \param openset Set of the visited pointers in order to stop cyclic dependencies in the phi node.
	 */
	uint32_t dfsPointerUsageFlagsComplete(const llvm::Value * v,std::set<const llvm::Value *> & openset) const;
	
	/**
	 * Memoization wrapper around dfsPointerUsageFlagsComplete
	 */
	uint32_t getPointerUsageFlagsComplete(const llvm::Value * v) const;
	mutable pointer_usage_map_t pointerCompleteUsageMap;
	
#ifdef DUETTO_DEBUG_POINTERS
	typedef std::set<const llvm::Value *> known_pointers_t;
	mutable known_pointers_t debugAllPointersSet;
#endif //DUETTO_DEBUG_POINTERS
	
	/** @} */

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
	void compileAllocation(const llvm::Value* callV, const llvm::Value* size, const llvm::Value* numElements = NULL);
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
	COMPILE_INSTRUCTION_FEEDBACK handleBuiltinCall(const char* ident, const llvm::Value* callV,
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
	bool getBasesInfo(const llvm::StructType* t, uint32_t& firstBase, uint32_t& baseCount);
	uint32_t compileClassTypeRecursive(const std::string& baseName, llvm::StructType* currentType, uint32_t baseCount);
	void compileClassType(llvm::StructType* T);
	void compileArrayClassType(llvm::StructType* T);
	void compileArrayPointerType();
	void compileLambdaBridge();
	void compileHandleVAArg();
	void compileConstantExpr(const llvm::ConstantExpr* ce);
	enum CONSTRUCTOR_ACTION { ADD_TO_QUEUE=0, COMPILE=1 };
	void handleConstructors(llvm::GlobalVariable* GV, CONSTRUCTOR_ACTION action);
	void compileNullPtrs();
	static uint32_t getMaskForBitWidth(int width);
	void compileSignedInteger(const llvm::Value* v);
	void compileUnsignedInteger(const llvm::Value* v);
	//JS interoperability support
	void compileClassesExportedToJs();
public:
	llvm::raw_ostream& stream;
	DuettoWriter(llvm::Module& m, llvm::raw_ostream& s):
		module(m),targetData(&m),currentFun(NULL),printMethodNames(false),printLambdaBridge(false),printHandleVAArg(false),
		printCreateArrayPointer(false),currentUniqueIndex(0),stream(s)
	{
	}
	void makeJS();
	void compileBB(const llvm::BasicBlock& BB, const std::map<const llvm::BasicBlock*, uint32_t>& blocksMap);
	void compileOperand(const llvm::Value* v, POINTER_KIND requestedPointerKind = UNDECIDED);
	void compileConstant(const llvm::Constant* c);
	void compilePHIOfBlockFromOtherBlock(const llvm::BasicBlock* to, const llvm::BasicBlock* from);
};

}
#endif
