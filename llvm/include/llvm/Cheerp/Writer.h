//===-- Cheerp/Writer.h - The Cheerp JavaScript generator -----------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2020 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_WRITER_H
#define _CHEERP_WRITER_H

#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Cheerp/AllocaMerging.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/LinearMemoryHelper.h"
#include "llvm/Cheerp/NameGenerator.h"
#include "llvm/Cheerp/PointerAnalyzer.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/SourceMaps.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/Cheerp/TokenList.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/Support/FormattedStream.h"
#include <set>
#include <map>
#include <array>

struct Relooper;

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

	// Get the underlying stream, keep in mind that if you write to it then you need to manually synchronize the state with the
	// the 'syncRawStream' method to avoid breaking sourcemaps.
	llvm::raw_ostream & getRawStream() const
	{
		return stream;
	}

	void syncRawStream(uint64_t beginVal)
	{
		uint64_t end = stream.tell();
		if(sourceMapGenerator)
			sourceMapGenerator->addLineOffset(end-beginVal);
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

class CheerpWriter
{
public:
	enum PARENT_PRIORITY { LOWEST = 0, INTN, UINTN, FROUND, TERNARY, LOGICAL_OR, LOGICAL_AND, BIT_OR, BIT_XOR, BIT_AND, COMPARISON, SHIFT, ADD_SUB, MUL_DIV, HIGHEST };
private:
	enum HEAP_TYPE {
		HEAP8=0,
		HEAP16,
		HEAP32,
		HEAPF32,
		HEAPF64,
		HEAP64,
		LAST_WASM = HEAP64,
		LAST_ASMJS = HEAPF64
	};
	enum MODULE_TYPE { NONE = 0, CLOSURE, COMMONJS };
	// COMPILE_EMPTY is returned if there is no need to add a ;\n to end the line
	enum COMPILE_INSTRUCTION_FEEDBACK { COMPILE_OK = 0, COMPILE_UNSUPPORTED, COMPILE_EMPTY };

	llvm::Module& module;
	llvm::Pass& pass;
	llvm::DataLayout targetData;
	const llvm::Function* currentFun;
	const PointerAnalyzer & PA;
	Registerize & registerize;

	EdgeContext edgeContext;
	GlobalDepsAnalyzer & globalDeps;
	const LinearMemoryHelper& linearHelper;
	const NameGenerator& namegen;
	const AllocaStoresExtractor& allocaStoresExtractor;
	TypeSupport types;
	std::set<const llvm::GlobalVariable*> compiledGVars;
	const std::array<const char*,6> typedArrayNames = {{"Uint8Array","Uint16Array","Int32Array","Float32Array","Float64Array","BigInt64Array"}};

	class HeapNames
	{
	public:
		HeapNames(NameGenerator& namegen) :
			heapNames{{namegen.getBuiltinName(NameGenerator::Builtin::HEAP8), namegen.getBuiltinName(NameGenerator::Builtin::HEAP16),
				namegen.getBuiltinName(NameGenerator::Builtin::HEAP32), namegen.getBuiltinName(NameGenerator::Builtin::HEAPF32),
				namegen.getBuiltinName(NameGenerator::Builtin::HEAPF64), namegen.getBuiltinName(NameGenerator::Builtin::HEAP64)}}
		{
			used.fill(false);
		}
		const llvm::StringRef& getHeapName(const int id)
		{
			markHeapNameAsUsed(id);
			return heapNames[id];
		}
		const llvm::StringRef& getHeapNameWithoutMarking(const int id) const
		{
			return heapNames[id];
		}
		bool isHeapNameUsed(const int id) const
		{
			return used[id];
		}
		void markHeapNameAsUsed(const int id)
		{
			used[id] = true;
		}
	private:
		const std::array<llvm::StringRef,6> heapNames;
		std::array<bool, 6> used;

	} heapNames;

	const llvm::StringRef& getHeapName(const int id)
	{
		return heapNames.getHeapName(id);
	}
	const llvm::StringRef& getHeapNameWithoutMarking(const int id) const
	{
		return heapNames.getHeapNameWithoutMarking(id);
	}
	bool isHeapNameUsed(const int id) const
	{
		return heapNames.isHeapNameUsed(id);
	}
	void markHeapNameAsUsed(const int id)
	{
		heapNames.markHeapNameAsUsed(id);
	}

	// Stream to put the initialized asmjs memory into.
	llvm::raw_ostream* asmJSMem;
	// Name of the initialized asmjs memory file.
	const std::string& asmJSMemFile;

	// Support for source maps
	SourceMapGenerator* sourceMapGenerator;
	std::map<llvm::StringRef, const llvm::DISubprogram*> functionToDebugInfoMap;
	const NewLineHandler NewLine;

	// Flag to signal if we should take advantage of native JavaScript math functions
	bool useNativeJavaScriptMath;
	// Flag to signal if we should take advantage of native 32-bit integer multiplication
	bool useMathImul;
	// Flag to signal if we should take advantage of native 32-bit float numbers
	bool useMathFround;
	// Enum to signal if we should create a module, and which kind
	MODULE_TYPE makeModule;
	// Flag to signal if we should add a credit comment line
	bool addCredits;
	// Flag to signal if we should add code that measures time until main is reached
	bool measureTimeToMain;
	// The asm.js module heap size
	uint32_t heapSize;
	// Flag to signal if we should add bounds-checking code
	bool checkBounds;
	// Flag to signal if we should use relooper instead of stackifier
	bool useCfgLegacy;
	// The name of the external wasm file, or empty if not present
	const std::string& wasmFile;
	// Flag to signal if we should generate typed arrays when element type is
	// double. Without this flag, normal arrays are used since they are
	// currently faster on v8.
	bool forceTypedArrays;
	// Flag to signal if we should use js variables instead of literals for globals addresses
	bool symbolicGlobalsAsmJS;
	// Flag to signal if we should emit readable or compressed output
	bool readableOutput;
	// Flag to keep track of state of already declared stuff
	bool areDummiesDeclared{false};
	bool areAsmJSExportsDeclared{false};
	bool areJsExportedExportsDeclared{false};
	// Flag to signal whether the root object has been deemed necessary
	bool isRootNeeded{false};

	/**
	 * \addtogroup MemFunction methods to handle memcpy, memmove, mallocs and free (and alike)
	 *
	 * @{
	 */

	/**
	  * Forward to the appropriate function of the NameGenerator
	  */
	llvm::StringRef getName(const llvm::Value* v, const bool doNotConsiderEdgeContext = false) const
	{
		if (doNotConsiderEdgeContext || edgeContext.isNull())
			return namegen.getName(v);
		return namegen.getNameForEdge(v, edgeContext);
	}
	llvm::StringRef getSecondaryName(const llvm::Value* v, const bool doNotConsiderEdgeContext = false) const
	{
		if (doNotConsiderEdgeContext || edgeContext.isNull())
			return namegen.getSecondaryName(v);
		return namegen.getSecondaryNameForEdge(v, edgeContext);
	}

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
	COMPILE_INSTRUCTION_FEEDBACK compileFree(const llvm::Value* obj);

	/** @} */

	std::pair<std::string, std::string> getBuiltinClassAndFunc(const char* identifier);
	COMPILE_INSTRUCTION_FEEDBACK handleBuiltinNamespace(const char* identifier, const llvm::CallBase& callV);
	COMPILE_INSTRUCTION_FEEDBACK handleBuiltinCall(const llvm::CallBase& callV, const llvm::Function* f);

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

	void compileRawPointer(const llvm::Value* p, PARENT_PRIORITY prio = PARENT_PRIORITY::HIGHEST, bool forceGEP = false);

	/**
	 * Compile a pointer from a GEP expression, with the given pointer kind
	 */
	void compileGEP(const llvm::User* gepInst, POINTER_KIND kind, PARENT_PRIORITY parentPrio);
	void compileGEPBase(const llvm::User* gep_inst, bool forEscapingPointer);
	void compileGEPOffset(const llvm::User* gep_inst, PARENT_PRIORITY parentPrio);

	/**
	 * Compile a pointer with the specified kind
	 */
	void compilePointerAs(const llvm::Value* p, POINTER_KIND kind, PARENT_PRIORITY prio = HIGHEST);

	/**
	 * Decide if `v` needs to be coerced to its integer type, based on the value of
	 * `coercionPrio`: If `coercionPrio` is `BIT_OR`,`BIT_AND`, or `SHIFT`,
	 * it means that the coercion has already been done, and can be skipped by
	 * the caller of this function.
	 */
	bool needsIntCoercion(PARENT_PRIORITY coercionPrio)
	{
		return (coercionPrio != BIT_OR && coercionPrio != BIT_AND && coercionPrio != SHIFT);
	}
	/**
	 * Decide if 'v' needs to be coerced to float.
	 */
	bool needsFloatCoercion(PARENT_PRIORITY coercionPrio)
	{
		return useMathFround && coercionPrio != FROUND;
	}
	/**
	 * Return the next priority higher than `prio`.
	 * For binary operators in general the rhs must increment the priority
	 * if there are more operators with the same priority (e.g. FMul,FDiv,FRem)
	 */
	PARENT_PRIORITY nextPrio(PARENT_PRIORITY prio)
	{
		int new_prio = prio;
		return static_cast<PARENT_PRIORITY>(++new_prio);
	}

	/**
	 * Compile a (possibly dynamic) downcast
	 */
	void compileDowncast(const llvm::CallBase& callV);
	/**
	 * Compile a cast to a virtual base
	 */
	void compileVirtualcast(const llvm::CallBase& callV);

	/** @} */

	void compileConstantExpr(const llvm::ConstantExpr* ce, PARENT_PRIORITY parentPrio, bool asmjs);
	bool doesConstantDependOnUndefined(const llvm::Constant* C) const;
	void compileMethodArgs(llvm::User::const_op_iterator it, llvm::User::const_op_iterator itE, const llvm::CallBase&, bool forceBoolean);
	COMPILE_INSTRUCTION_FEEDBACK compileTerminatorInstruction(const llvm::Instruction& I);
	bool compileCompoundStatement(const llvm::Instruction* I, uint32_t regId);
	COMPILE_INSTRUCTION_FEEDBACK compileNotInlineableInstruction(const llvm::Instruction& I, PARENT_PRIORITY parentPrio);
	COMPILE_INSTRUCTION_FEEDBACK compileInlineableInstruction(const llvm::Instruction& I, PARENT_PRIORITY parentPrio);

	void compileSignedInteger(const llvm::Value* v, bool forComparison, PARENT_PRIORITY parentPrio);
	void compileUnsignedInteger(const llvm::Value* v, bool forAsmJSComparison, PARENT_PRIORITY parentPrio, bool forceTruncation = false);

	void compileMethodLocal(llvm::StringRef name, Registerize::REGISTER_KIND kind);
	void compileMethodLocals(const llvm::Function& F, bool needsLabel);
	void compileCondition(const llvm::BasicBlock* BB, bool booleanInvert);
	static llvm::DenseSet<const Token*> getLabeledTokens(const TokenList& Tokens);
	static bool omitBraces(const Token& T, const PointerAnalyzer& PA, const Registerize& registerize);
	void compileTokens(const TokenList& Tokens);
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
	void compileBuiltins(bool asmjs);
	/**
	 * This method compiles an helper function for getting an ArrayBuffer from
	 * a file, usable from the browser and node
	 */
	void compileFetchBuffer();
	/**
	 * This method supports both ConstantArray and ConstantDataSequential
	 */
	void compileConstantArrayMembers(const llvm::Constant* C);

	/**
	 * Methods implemented in Types.cpp
	 */
	enum COMPILE_TYPE_STYLE { LITERAL_OBJ=0, THIS_OBJ };
	void compileTypedArrayType(llvm::Type* t);
	void compileSimpleType(llvm::Type* t, llvm::Value* init);
	// varName is used for a fake assignment to break literals into smaller units.
	// This is useful to avoid a huge penalty on V8 when creating large literals
	uint32_t compileComplexType(llvm::Type* t, COMPILE_TYPE_STYLE style, llvm::StringRef varName, uint32_t maxDepth, uint32_t totalLiteralProperties,
					const AllocaStoresExtractor::OffsetToValueMap* offsetToValueMap, uint32_t offset, uint32_t& usedValuesFromMap);
	void compileType(llvm::Type* t, COMPILE_TYPE_STYLE style, llvm::StringRef varName = llvm::StringRef(), const AllocaStoresExtractor::OffsetToValueMap* offsetToValueMap = nullptr);
	uint32_t compileClassTypeRecursive(const std::string& baseName, llvm::StructType* currentType, uint32_t baseCount);
	void compileClassType(llvm::StructType* T);
	void compileClassConstructor(llvm::StructType* T);
	void compileArrayClassType(llvm::Type* T);
	void compileResizeArrayClassType(llvm::Type* T);
	void compileArrayPointerType();
	static bool needsUnsignedTruncation(std::unordered_set<const llvm::Value*> visited, const llvm::Value* v, bool asmjs);

	/**
	 * Methods implemented in Opcodes.cpp
	 */
	void compilePtrToInt(const llvm::Value* v, bool isInt64);
	void compileSubtraction(const llvm::Value* lhs, const llvm::Value* rhs, PARENT_PRIORITY parentPrio, bool asmjs);
	void compileDivRem(const llvm::Value* lhs, const llvm::Value* rhs, PARENT_PRIORITY parentPrio, char op, bool isSigned);
	void compileBitCast(const llvm::User* bc_inst, POINTER_KIND kind, PARENT_PRIORITY parentPrio);
	void compileBitCastBase(const llvm::User* bi, bool forEscapingPointer);
	void compileBitCastOffset(const llvm::User* bi, PARENT_PRIORITY parentPrio);
	void compileSelect(const llvm::User* select, const llvm::Value* cond, const llvm::Value* lhs, const llvm::Value* rhs, PARENT_PRIORITY parentPrio);

	//JS interoperability support
	uint32_t countJsParameters(const llvm::Function* F, bool isStatic) const;
	void compileDeclExportedToJs(const bool alsoDeclare);

public:
	struct JSExportedNamedDecl
	{
		JSExportedNamedDecl(const llvm::Function* F, llvm::StringRef name) : F(F), t(nullptr), node(nullptr), name(name)
		{
		}
		JSExportedNamedDecl(const llvm::StructType* t, const llvm::NamedMDNode& node, llvm::StringRef name) : F(nullptr), t(t), node(&node), name(name)
		{
		}
		const llvm::Function* F;
		const llvm::StructType* t;
		const llvm::NamedMDNode* node;
		std::string name;
		bool isClass() const
		{
			return t;
		}
	};
	static std::deque<JSExportedNamedDecl> buildJsExportedNamedDecl(const llvm::Module& M);
	static void prependRootToNames(std::deque<CheerpWriter::JSExportedNamedDecl> & exportedDecls);
	static void normalizeDeclList(std::deque<CheerpWriter::JSExportedNamedDecl> & exportedDecls);

	static bool isNamespaced(const llvm::StringRef str)
	{
		return (str.find('.') < str.size());
	}

private:
	// Names of js-exported items
	std::deque<CheerpWriter::JSExportedNamedDecl> jsExportedDecls;

	bool hasJSExports();
	void compileInlineAsm(const llvm::CallInst& ci);

	struct JSBytesWriter: public LinearMemoryHelper::ByteListener
	{
		ostream_proxy& stream;
		bool first;
		JSBytesWriter(ostream_proxy& stream):stream(stream),first(true)
		{
		}
		void addByte(uint8_t b) override;
	};
	struct BinaryBytesWriter: public LinearMemoryHelper::ByteListener
	{
		ostream_proxy& stream;
		BinaryBytesWriter(ostream_proxy& stream):stream(stream)
		{
		}
		void addByte(uint8_t b) override {stream <<(char)b;};
	};

	struct AsmJSGepWriter: public LinearMemoryHelper::LinearGepListener
	{
		CheerpWriter& writer;
		bool offset;
		bool use_imul;
		AsmJSGepWriter(CheerpWriter& writer, bool use_imul=true): LinearMemoryHelper::LinearGepListener(writer.PA), writer(writer), offset(false), use_imul(use_imul)
		{
		}
		void addValue(const llvm::Value* v, uint32_t size) override;
		void addConst(int64_t v) override;
	};
public:
	// Data to optimize asm.js rendering of return statements
	uint32_t blockDepth;
	const llvm::BasicBlock* lastDepth0Block;
	ostream_proxy stream;
	CheerpWriter(llvm::Module& m, llvm::Pass& p, llvm::raw_ostream& s, cheerp::PointerAnalyzer & PA,
			cheerp::Registerize & registerize,
			cheerp::GlobalDepsAnalyzer & gda,
			const cheerp::LinearMemoryHelper & linearHelper,
			cheerp::NameGenerator& namegen,
			cheerp::AllocaStoresExtractor& allocaStoresExtractor,
			llvm::raw_ostream* asmJSMem,
			const std::string& asmJSMemFile,
			SourceMapGenerator* sourceMapGenerator,
			bool readableOutput,
			llvm::StringRef makeModule,
			bool useNativeJavaScriptMath,
			bool useMathImul,
			bool useMathFround,
			bool addCredits,
			bool measureTimeToMain,
			unsigned heapSize,
			bool checkBounds,
			bool useCfgLegacy,
			bool compileGlobalsAddrAsmJS,
			const std::string& wasmFile,
			bool forceTypedArrays):
		module(m),
		pass(p),
		targetData(&m),
		currentFun(NULL),
		PA(PA),
		registerize(registerize),
		edgeContext(),
		globalDeps(gda),
		linearHelper(linearHelper),
		namegen(namegen),
		allocaStoresExtractor(allocaStoresExtractor),
		types(m),
		heapNames(namegen),
		asmJSMem(asmJSMem),
		asmJSMemFile(asmJSMemFile),
		sourceMapGenerator(sourceMapGenerator),
		NewLine(),
		useNativeJavaScriptMath(useNativeJavaScriptMath),
		useMathImul(useMathImul),
		useMathFround(useMathFround),
		makeModule(makeModule==llvm::StringRef("closure")?
			MODULE_TYPE::CLOSURE : (makeModule==llvm::StringRef("commonjs")?
			MODULE_TYPE::COMMONJS : MODULE_TYPE::NONE)),
		addCredits(addCredits),
		measureTimeToMain(measureTimeToMain),
		heapSize(heapSize),
		checkBounds(checkBounds),
		useCfgLegacy(useCfgLegacy),
		wasmFile(wasmFile),
		forceTypedArrays(forceTypedArrays),
		symbolicGlobalsAsmJS(compileGlobalsAddrAsmJS),
		readableOutput(readableOutput),
		blockDepth(0),
		lastDepth0Block(nullptr),
		stream(s, sourceMapGenerator, readableOutput)
	{
	}
	void makeJS();
	void compileBB(const llvm::BasicBlock& BB);
	void compileConstant(const llvm::Constant* c, PARENT_PRIORITY parentPrio = HIGHEST);
	void compileOperand(const llvm::Value* v, PARENT_PRIORITY parentPrio = HIGHEST, bool allowBooleanObjects = false);
	void compilePHIOfBlockFromOtherBlock(const llvm::BasicBlock* to, const llvm::BasicBlock* from);
	void compileOperandForIntegerPredicate(const llvm::Value* v, llvm::CmpInst::Predicate p, PARENT_PRIORITY parentPrio);
	void compileIntegerComparison(const llvm::Value* lhs, const llvm::Value* rhs, llvm::CmpInst::Predicate p, PARENT_PRIORITY parentPrio);
	void compileFloatComparison(const llvm::Value* lhs, const llvm::Value* rhs, llvm::CmpInst::Predicate p, PARENT_PRIORITY parentPrio, bool asmjs);
	bool isInlineableInstruction(const llvm::Value* v) const;

	// returns the amount fo shift required for accessing the corresponding heap
	int getHeapShiftForType(llvm::Type* et);
	int compileHeapForType(llvm::Type* et);
	void compileHeapAccess(const llvm::Value* p, llvm::Type* t = nullptr);
	/**
	 * Compile the function tables for the asm.js module
	 */
	void compileFunctionTablesAsmJS();
	/**
	 * Compile the declaration of the mathematical functions for the asm.js module
	 */
	void compileMathDeclAsmJS();
	/**
	 * Compile the function for growing the wasm linear memory
	 */
	void compileGrowMem();
	/**
	 * Compile an helper function to assign all global heap symbols
	 */
	void compileAssignHeaps(bool wasm);
	/**
	 * Compile the helper functions for exposing the asm.js stack pointer
	 */
	void compileStackPtrHelpersAsmJS();
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
	void compileCheckBoundsAsmJS(const llvm::Value* p, int alignMask);
	/**
	 * Compile a bound-checking function definition for asm.js heap
	 */
	void compileCheckBoundsAsmJSHelper();
	/**
	 * Compile a function to assure a GEP property access is defined
	 */
	void compileCheckDefined(const llvm::Value* p, bool needsOffset);
	/**
	 * Compile a function for checking if a reference is defined
	 */
	void compileCheckDefinedHelper();
	/**
	 * Compile a JS string while escaping special characters
	 */
	static void compileEscapedString(llvm::raw_ostream& stream, llvm::StringRef str, bool forJSON);
	/**
	 * Run relooper on a function, this code is here since it is also used by CheerpWastWriter
	 */
	static Relooper* runRelooperOnFunction(const llvm::Function& F, const PointerAnalyzer& PA,
	                                       const Registerize& registerize);
	/**
	 * Returns if a switch/br_table is appropriate to render this terminator
	 */
	static bool useSwitch(const llvm::Instruction* term);
	static bool needsPointerKindConversion(const llvm::PHINode* phi, const llvm::Value* incoming,
	                                       const PointerAnalyzer& PA, const Registerize& registerize, const EdgeContext& edgeContext);
	static bool needsPointerKindConversionForBlocks(const llvm::BasicBlock* to, const llvm::BasicBlock* from,
	                                                const PointerAnalyzer& PA, const Registerize& registerize);
	static bool needsUnsignedTruncation(const llvm::Value* v, bool asmjs);
private:

	enum Options{NEED_SOURCE_MAPS, MEASURE_TIME_TO_MAIN, NEED_MODULE_CLOSURE, MAX_OPTION};
	typedef std::array<bool,Options::MAX_OPTION> OptionsSet;

	/*
	 * Helper functions to make makeJS more modular
	 */
	void compileFileBegin(const OptionsSet& options);
	void compileFileEnd(const OptionsSet& options);
	void compileSourceMapsBegin();
	void compileSourceMapsEnd();
	void compileTimeToMainBegin();
	void compileTimeToMainEnd();
	void compileModuleClosureBegin();
	void compileModuleClosureEnd();
	void compileHelpers();
	void compileImports();
	void compileAsmJSClosure();
	void compileAsmJSTopLevel();
	void compileGenericJS();
	void compileWasmLoader();
	void compileAsmJSLoader();
	void compileCommonJSModule();
	void compileLoaderOrModuleEnd();
	void compileDeclareExports();
	void compileDefineExports();
	void compileCommonJSExports();
	void compileConstructors();
	void compileDummies();
	void compileNamespaces();
	void compileRootIfNeeded();
};

}
#endif
