//===-- Cheerp/Writer.h - The Cheerp JavaScript generator -----------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2015 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_WRITER_H
#define _CHEERP_WRITER_H

#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/NameGenerator.h"
#include "llvm/Cheerp/PointerAnalyzer.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/SourceMaps.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/Support/FormattedStream.h"
#include <set>
#include <map>
#include <array>

namespace cheerp
{

class NewLineHandler
{
};

/**
 * Black magic to conditionally enable indented output
 */
class ostream_proxy
{
public:
	ostream_proxy( llvm::raw_ostream & s, SourceMapGenerator* g, bool readableOutput = false ) :
		stream(s),
		sourceMapGenerator(g),
		readableOutput(readableOutput),
		newLine(true),
		indentLevel(0)
	{}

	friend ostream_proxy& operator<<( ostream_proxy & os, char c )
	{
		uint64_t begin = os.stream.tell();
		os.write_indent(c);
		uint64_t end = os.stream.tell();
		if(os.sourceMapGenerator)
			os.sourceMapGenerator->addLineOffset(end-begin);
		return os;
	}

	friend ostream_proxy& operator<<( ostream_proxy & os, llvm::StringRef s )
	{
		uint64_t begin = os.stream.tell();
		os.write_indent(s);
		uint64_t end = os.stream.tell();
		if(os.sourceMapGenerator)
			os.sourceMapGenerator->addLineOffset(end-begin);
		return os;
	}

	friend ostream_proxy& operator<<( ostream_proxy & os, const NewLineHandler& handler)
	{
		if(!os.readableOutput)
			return os;
		if(os.sourceMapGenerator)
			os.sourceMapGenerator->finishLine();
		os.stream << '\n';
		os.newLine = true;
		return os;
	}

	template<class T>
	friend typename std::enable_if<
		!std::is_convertible<T&&, llvm::StringRef>::value, // Use this only if T is not convertible to StringRef
		ostream_proxy&>::type operator<<( ostream_proxy & os, T && t )
	{
		uint64_t begin = os.stream.tell();
		if ( os.newLine && os.readableOutput )
			for ( int i = 0; i < os.indentLevel; i++ )
				os.stream << '\t';

		os.stream << std::forward<T>(t);
		os.newLine = false;
		uint64_t end = os.stream.tell();
		if(os.sourceMapGenerator)
			os.sourceMapGenerator->addLineOffset(end-begin);
		return os;
	}

private:

	// Return true if we are closing a curly bracket, need to unindent by 1.
	bool updateIndent( char c ) {
		if ( c == '{') indentLevel++;
		else if ( c == '}') {indentLevel--; return true; }
		return false;
	}

	// Return true if we are closing a curly, need to unindent by 1.
	bool updateIndent( llvm::StringRef s) {
		if (s.empty() )
			return false;
		bool ans = updateIndent(s.front());
		for (auto it = s.begin()+1; it != s.end(); ++it)
			updateIndent(*it);
		return ans;
	}

	template<class T>
	void write_indent(T && t)
	{
		int oldIndent = indentLevel;
		if (updateIndent( std::forward<T>(t) ) )
			oldIndent--;

		if ( newLine && readableOutput )
			for ( int i = 0; i < oldIndent; i++ )
				stream << '\t';

		stream << std::forward<T>(t);
		newLine = false;
	}

	llvm::raw_ostream & stream;
	SourceMapGenerator* sourceMapGenerator;
	bool readableOutput;
	bool newLine;
	int indentLevel;
};

const static int V8MaxLiteralDepth = 3;
const static int V8MaxLiteralProperties = 8;

class CheerpWriter
{
public:
	enum PARENT_PRIORITY { LOWEST = 0, TERNARY, LOGICAL_OR, LOGICAL_AND, BIT_OR, BIT_XOR, BIT_AND, COMPARISON, SHIFT, ADD_SUB, MUL_DIV, HIGHEST, COERCION };
private:
	enum HEAP_TYPE {HEAP8=0, HEAP16, HEAP32, HEAPF32, HEAPF64};

	llvm::Module& module;
	llvm::DataLayout targetData;
	const llvm::Function* currentFun;
	const PointerAnalyzer & PA;
	const Registerize & registerize;

	GlobalDepsAnalyzer & globalDeps;
	NameGenerator namegen;
	TypeSupport types;
	std::set<const llvm::GlobalVariable*> compiledGVars;
	const std::array<const char*,5> typedArrayNames = {{"Uint8Array","Uint16Array","Int32Array","Float32Array","Float64Array"}};
	const std::array<const char*,5> heapNames = {{"HEAP8","HEAP16","HEAP32","HEAPF32","HEAPF64"}};

	// map asmjs global variables to their address
	std::map<const llvm::GlobalVariable*,uint32_t> gVarsAddr;
	// the keys are unique for every function signature
	// the values are a list of the function names with that signature
	std::map<std::string, std::vector<std::string>> functionTables;
	// The next address available to allocate global variables.
	// The heap space will start after the last global variable allocation
	uint32_t heapStartAsmJS{8};
	// Support for source maps
	SourceMapGenerator* sourceMapGenerator;
	std::map<llvm::StringRef, llvm::DISubprogram> functionToDebugInfoMap;
	const NewLineHandler NewLine;

	// Flag to signal if we should take advantage of native JavaScript math functions
	bool useNativeJavaScriptMath;
	// Flag to signal if we should take advantage of native 32-bit integer multiplication
	bool useMathImul;
	// Flag to signal if we should create a closure to avoid global namespace pollution
	bool makeModule;
	// Flag to signal if we should add a credit comment line
	bool addCredits;
	// Flag to signal if we should add code that measures time until main is reached
	bool measureTimeToMain;
	// The asm.js module heap size
	uint32_t heapSize;
	// Flag to signal if we should add bounds-checking code for arrays
	bool checkBounds;
	// Flag to signal if we should add defined member checking code for objects
	bool checkDefined;
	// Flag to signal if we should generate typed arrays when element type is
	// double. Without this flag, normal arrays are used since they are
	// currently faster on v8.
	bool forceTypedArrays;

	/**
	 * \addtogroup MemFunction methods to handle memcpy, memmove, mallocs and free (and alike)
	 *
	 * @{
	 */

	/**
	 * Compile memcpy and memmove
	 */
	void compileMemFunc(const llvm::Value* dest,
	                    const llvm::Value* srcOrResetVal,
	                    const llvm::Value* size);

	/**
	 * Copy baseSrc into baseDest
	 */
	void compileCopyElement(const llvm::Value* baseDest,
	                        const llvm::Value* baseSrc,
	                        llvm::Type* currentType);

	uint32_t compileArraySize(const DynamicAllocInfo& info, bool shouldPrint, bool isBytes = false);
	void compileAllocation(const DynamicAllocInfo& info);
	void compileFree(const llvm::Value* obj);

	/** @} */

	// COMPILE_EMPTY is returned if there is no need to add a ;\n to end the line
	enum COMPILE_INSTRUCTION_FEEDBACK { COMPILE_OK = 0, COMPILE_UNSUPPORTED, COMPILE_EMPTY };

	void handleBuiltinNamespace(const char* identifier, llvm::ImmutableCallSite callV);
	COMPILE_INSTRUCTION_FEEDBACK handleBuiltinCall(llvm::ImmutableCallSite callV, const llvm::Function* f);

	void compilePredicate(llvm::CmpInst::Predicate p);

	/**
	 * \addtogroup Pointers Methods to compile pointers
	 * @{
	 */

	/**
	 * Compile an == or != pointer comparison
	 */
	void compileEqualPointersComparison(const llvm::Value* lhs, const llvm::Value* rhs, llvm::CmpInst::Predicate p);

	/**
	 * Writes an access expression (i.e. something like .a3.a1[n].a4, etc.. ) using the given indices
	 */
	void compileAccessToElement(llvm::Type* tp, llvm::ArrayRef< const llvm::Value* > indices, bool compileLastWrapperArray);

	/**
	 * Write the offset part of a GEP as a literal or numerical offset
	 */
	void compileOffsetForGEP(llvm::Type* pointerOperandType, llvm::ArrayRef< const llvm::Value* > indices);

	/**
	 * Compile a COMPLETE_OBJECT.
	 * If the given value is a COMPLETE_OBJECT, just invoke compileOperand, otherwise do a promotion
	 */
	void compileCompleteObject(const llvm::Value*, const llvm::Value* offset = nullptr);

	/**
	 * Compile the pointer base.
	 */
	void compilePointerBase(const llvm::Value*, bool forEscapingPointer=false);

	/**
	 * Compile the pointer offset.
	 */
	void compilePointerOffset(const llvm::Value*, PARENT_PRIORITY parentPrio, bool forEscapingPointer=false);

	/**
	 * BYTE_LAYOUT_OFFSET_FULL: Compile the full offset in bytes till the element
	 * BYTE_LAYOUT_OFFSET_STOP_AT_ARRAY: Compile the offset in bytes till the array, if any, containing the element.
	 *                                   The offset into the array will be returned.
         * BYTE_LAYOUT_OFFSET_NO_PRINT: Like BYTE_LAYOUT_OFFSET_STOP_AT_ARRAY, but does not print any code.
	 */
	enum BYTE_LAYOUT_OFFSET_MODE { BYTE_LAYOUT_OFFSET_FULL = 0, BYTE_LAYOUT_OFFSET_STOP_AT_ARRAY, BYTE_LAYOUT_OFFSET_NO_PRINT };
	/**
	 * Compile the offset in bytes from the byte layout base found by recursively traversing BitCasts and GEPs.
	 * If a GEP from a byte layout pointer to an immutable type is contained in an ArrayType we want to construct the typed array
	 * starting from the array itself instead of from the value. This will make it possible to loop backward over the array.
	 */
	const llvm::Value* compileByteLayoutOffset(const llvm::Value* p, BYTE_LAYOUT_OFFSET_MODE offsetMode);

	void compileRawPointer(const llvm::Value* p);

	/**
	 * Compile a pointer from a GEP expression, with the given pointer kind
	 */
	void compileGEP(const llvm::User* gepInst, POINTER_KIND kind);
	void compileGEPBase(const llvm::User* gep_inst, bool forEscapingPointer);
	void compileGEPOffset(const llvm::User* gep_inst);

	/**
	 * Compile a pointer with the specified kind
	 */
	void compilePointerAs(const llvm::Value* p, POINTER_KIND kind)
	{
		assert(p->getType()->isPointerTy());
		assert(kind != SPLIT_REGULAR);
		POINTER_KIND valueKind = PA.getPointerKind(p);

		if(kind == COMPLETE_OBJECT)
		{
			compileCompleteObject(p);
		}
		else if (kind == RAW)
		{
			compileRawPointer(p);
		}
		else if (llvm::isa<llvm::ConstantPointerNull>(p))
		{
			stream << "nullObj";
		}
		else if (PA.getConstantOffsetForPointer(p) || valueKind == SPLIT_REGULAR)
		{
			stream << "{d:";
			compilePointerBase(p, true);
			stream << ",o:";
			compilePointerOffset(p, LOWEST);
			stream << "}";
		}
		else
		{
			assert(valueKind == REGULAR || valueKind == BYTE_LAYOUT);
			compileOperand(p);
		}
	}

	/**
	 * Compile a (possibly dynamic) downcast
	 */
	void compileDowncast(llvm::ImmutableCallSite callV);

	/** @} */

	void compileConstantExpr(const llvm::ConstantExpr* ce);
	bool doesConstantDependOnUndefined(const llvm::Constant* C) const;
	void compileMethodArgs(llvm::User::const_op_iterator it, llvm::User::const_op_iterator itE, llvm::ImmutableCallSite, bool forceBoolean);
	COMPILE_INSTRUCTION_FEEDBACK compileTerminatorInstruction(const llvm::TerminatorInst& I);
	COMPILE_INSTRUCTION_FEEDBACK compileNotInlineableInstruction(const llvm::Instruction& I, PARENT_PRIORITY parentPrio);
	COMPILE_INSTRUCTION_FEEDBACK compileInlineableInstruction(const llvm::Instruction& I, PARENT_PRIORITY parentPrio);

	void compileSignedInteger(const llvm::Value* v, bool forComparison, PARENT_PRIORITY parentPrio);
	void compileUnsignedInteger(const llvm::Value* v, PARENT_PRIORITY parentPrio);

	void compileMethodLocal(llvm::StringRef name, Registerize::REGISTER_KIND kind);
	void compileMethodLocals(const llvm::Function& F, bool needsLabel);
	void compileMethod(const llvm::Function& F);
	/**
	 * Helper structure for compiling globals
	 */
	struct GlobalSubExprInfo
	{
		POINTER_KIND kind;
		bool hasConstantOffset;
		GlobalSubExprInfo(POINTER_KIND k, bool h):kind(k),hasConstantOffset(h)
		{
		}
	};
	/**
	 * Helper method for compiling globals
	 */
	GlobalSubExprInfo compileGlobalSubExpr(const GlobalDepsAnalyzer::SubExprVec& subExpr);
	void compileGlobal(const llvm::GlobalVariable& G);
	void compileParamTypeAnnotationsAsmJS(const llvm::Function* F);
	void compileGlobalAsmJS(const llvm::GlobalVariable& G);
	void compileGlobalsInitAsmJS();
	void compileNullPtrs();
	void compileCreateClosure();
	void compileHandleVAArg();
	/**
	 * This method supports both ConstantArray and ConstantDataSequential
	 */
	void compileConstantArrayMembers(const llvm::Constant* C);

	/**
	 * Methods implemented in Types.cpp
	 */
	enum COMPILE_TYPE_STYLE { LITERAL_OBJ=0, THIS_OBJ };
	void compileTypedArrayType(llvm::Type* t);
	void compileSimpleType(llvm::Type* t);
	// varName is used for a fake assignment to break literals into smaller units.
	// This is useful to avoid a huge penalty on V8 when creating large literals
	uint32_t compileComplexType(llvm::Type* t, COMPILE_TYPE_STYLE style, llvm::StringRef varName, uint32_t maxDepth, uint32_t totalLiteralProperties);
	void compileType(llvm::Type* t, COMPILE_TYPE_STYLE style, llvm::StringRef varName = llvm::StringRef());
	uint32_t compileClassTypeRecursive(const std::string& baseName, llvm::StructType* currentType, uint32_t baseCount);
	void compileClassType(llvm::StructType* T);
	void compileClassConstructor(llvm::StructType* T);
	void compileArrayClassType(llvm::Type* T);
	void compileArrayPointerType();

	/**
	 * Methods implemented in Opcodes.cpp
	 */
	void compileIntegerComparison(const llvm::Value* lhs, const llvm::Value* rhs, llvm::CmpInst::Predicate p, PARENT_PRIORITY parentPrio);
	void compilePtrToInt(const llvm::Value* v);
	void compileSubtraction(const llvm::Value* lhs, const llvm::Value* rhs, PARENT_PRIORITY parentPrio);
	void compileBitCast(const llvm::User* bc_inst, POINTER_KIND kind);
	void compileBitCastBase(const llvm::User* bi, bool forEscapingPointer);
	void compileBitCastOffset(const llvm::User* bi);
	void compileSelect(const llvm::User* select, const llvm::Value* cond, const llvm::Value* lhs, const llvm::Value* rhs, PARENT_PRIORITY parentPrio);

	static uint32_t getMaskForBitWidth(int width)
	{
		return (1 << width) - 1;
	}

	//JS interoperability support
	std::vector<llvm::StringRef> compileClassesExportedToJs();
	void addExportedFreeFunctions(std::vector<llvm::StringRef>& namesList, const llvm::NamedMDNode* namedNode);
public:
	ostream_proxy stream;
	CheerpWriter(llvm::Module& m, llvm::raw_ostream& s, cheerp::PointerAnalyzer & PA, cheerp::Registerize & registerize,
	             cheerp::GlobalDepsAnalyzer & gda, SourceMapGenerator* sourceMapGenerator, const std::vector<std::string>& reservedNames, bool ReadableOutput,
	             bool MakeModule, bool NoRegisterize, bool UseNativeJavaScriptMath, bool useMathImul, bool addCredits, bool measureTimeToMain,
	             unsigned HeapSize, bool CheckBounds, bool CheckDefined, bool forceTypedArrays):
		module(m),targetData(&m),currentFun(NULL),PA(PA),registerize(registerize),globalDeps(gda),
		namegen(m, globalDeps, registerize, PA, reservedNames, ReadableOutput),types(m),
		sourceMapGenerator(sourceMapGenerator),NewLine(),useNativeJavaScriptMath(UseNativeJavaScriptMath),
		useMathImul(useMathImul),makeModule(MakeModule),addCredits(addCredits),measureTimeToMain(measureTimeToMain),
	        heapSize(HeapSize),checkBounds(CheckBounds),checkDefined(CheckDefined),forceTypedArrays(forceTypedArrays),stream(s, sourceMapGenerator, ReadableOutput)
	{
	}
	void makeJS();
	void compileBB(const llvm::BasicBlock& BB);
	void compileConstant(const llvm::Constant* c);
	void compileConstantAsBytes(const llvm::Constant* c, bool first = false, bool asmjs = false);
	void compileOperand(const llvm::Value* v, PARENT_PRIORITY parentPrio = HIGHEST, bool allowBooleanObjects = false);
	bool needsPointerKindConversion(const llvm::Instruction* phi, const llvm::Value* incoming);
	bool needsPointerKindConversionForBlocks(const llvm::BasicBlock* to, const llvm::BasicBlock* from);
	void compilePHIOfBlockFromOtherBlock(const llvm::BasicBlock* to, const llvm::BasicBlock* from);
	void compileOperandForIntegerPredicate(const llvm::Value* v, llvm::CmpInst::Predicate p, PARENT_PRIORITY parentPrio);

	void compileStackFrame();
	void compileStackRet();
	void compileAllocaAsmJS(uint32_t size, uint32_t alignment);
	// returns the amount fo shift required for the selected heap
	int compileHeapForType(llvm::Type* et);
	void compileHeapAccess(const llvm::Value* p, llvm::Type* t = nullptr);
	std::string getFunctionTableNameAsmJS(const llvm::FunctionType* ft);
	// return -1 if function name not found in table
	int getFunctionOffsetInTableAsmJS(const std::string& fname, const llvm::FunctionType* ftype);
	/**
	 * Compile the function tables for the asm.js module
	 */
	void compileFunctionTablesAsmJS();
	/**
	 * Fills the functionTables data structure use to handle indirect calls in asm.js code
	 */
	void fillFunctionTablesAsmJS();
	/**
	 * Compile the declaration of the mathematical functions for the asm.js module
	 */
	void compileMathDeclAsmJS();
	/**
	 * Compile the memmove helper function for asm.js code
	 */
	void compileMemmoveHelperAsmJS();
	/**
	 * Compile the printString helper function for asm.js code
	 */
	void compilePrintStringHelperAsmJS();
	/**
	 * Compile a bound-checking statement on REGULAR or SPLIT_REGULAR pointer
	 */
	void compileCheckBounds(const llvm::Value* p);
	/**
	 * Compile a bound-checking function definition
	 */
	void compileCheckBoundsHelper();
	/**
	 * Compile a bound-checking statement for heap accesses in asm.js
	 */
	void compileCheckBoundsAsmJS(const llvm::Value* p);
	/**
	 * Compile a bound-checking function definition for asm.js heap
	 */
	void compileCheckBoundsAsmJSHelper();
	/**
	 * Compile a statement for checking function pointer validity in asm.js
	 */
	void compileCheckFunctionPtrAsmJS(const llvm::Value* p, uint32_t size);
	/**
	 * Compile a function definition for checking function pointer validity in asm.js
	 */
	void compileCheckFunctionPtrAsmJSHelper();
	/**
	 * Compile a function to assure a GEP property access is defined
	 */
	void compileCheckDefined(const llvm::Value* p);
	/**
	 * Compile a function for checking if a reference is defined
	 */
	void compileCheckDefinedHelper();
};

}
#endif
