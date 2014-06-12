//===-- Cheerp/Writer.h - The Cheerp JavaScript generator -----------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_WRITER_H
#define _CHEERP_WRITER_H

#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/NameGenerator.h"
#include "llvm/Cheerp/PointerAnalyzer.h"
#include "llvm/Cheerp/SourceMaps.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Support/FormattedStream.h"
#include <set>
#include <map>

namespace cheerp
{

class NewLineHandler
{
private:
	SourceMapGenerator& sourceMapGenerator;
public:
	NewLineHandler(SourceMapGenerator& s):sourceMapGenerator(s)
	{
	}

	friend llvm::raw_ostream& operator<<(llvm::raw_ostream& s, const NewLineHandler& handler)
	{
		s << '\n';
		handler.sourceMapGenerator.finishLine();
		return s;
	}

};

/**
 * Black magic to conditionally enable indented output
 */
class ostream_proxy
{
public:
	ostream_proxy( llvm::raw_ostream & s, bool readableOutput = false ) :
		stream(s),
		readableOutput(readableOutput)
	{}

	friend ostream_proxy& operator<<( ostream_proxy & os, char c )
	{
		os.write_indent(c);
		return os;
	}

	friend ostream_proxy& operator<<( ostream_proxy & os, llvm::StringRef s )
	{
		os.write_indent(s);
		return os;
	}

	friend ostream_proxy& operator<<( ostream_proxy & os, const NewLineHandler& handler)
	{
		os.stream << handler;
		os.newLine = true;
		return os;
	}

	template<class T>
	friend typename std::enable_if<
		!std::is_convertible<T&&, llvm::StringRef>::value, // Use this only if T is not convertible to StringRef
		ostream_proxy&>::type operator<<( ostream_proxy & os, T && t )
	{
		if ( os.newLine && os.readableOutput )
			for ( int i = 0; i < os.indentLevel; i++ )
				os.stream << '\t';

		os.stream << std::forward<T>(t);
		os.newLine = false;
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
	bool readableOutput;
	bool newLine = true;
	int indentLevel = 0;
};

class CheerpWriter
{
private:

	llvm::Module& module;
	llvm::DataLayout targetData;
	llvm::AliasAnalysis& AA;
	const llvm::Function* currentFun;
	
	GlobalDepsAnalyzer globalDeps;
	NameGenerator namegen;
	TypeSupport types;
	PointerAnalyzer analyzer;
	std::set<const llvm::GlobalVariable *> compiledGVars;
	
	// Support for source maps
	SourceMapGenerator sourceMapGenerator;
	std::string sourceMapName;
	const NewLineHandler NewLine;

	void compileTypedArrayType(llvm::Type* t);
	
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
	llvm::Type* compileRecursiveAccessToGEP(llvm::Type* curType, const llvm::Use* it,
			const llvm::Use* const itE, COMPILE_FLAG flag);
	void compilePredicate(llvm::CmpInst::Predicate p);
	void compileOperandForIntegerPredicate(const llvm::Value* v, llvm::CmpInst::Predicate p);
	void compileEqualPointersComparison(const llvm::Value* lhs, const llvm::Value* rhs, llvm::CmpInst::Predicate p);
	void compileIntegerComparison(const llvm::Value* lhs, const llvm::Value* rhs, llvm::CmpInst::Predicate p);
	void compilePtrToInt(const llvm::Value* v);
	void compileSubtraction(const llvm::Value* lhs, const llvm::Value* rhs);
	enum COMPILE_TYPE_STYLE { LITERAL_OBJ=0, THIS_OBJ };
	void compileType(llvm::Type* t, COMPILE_TYPE_STYLE style);
	void compileTypeImpl(llvm::Type* t, COMPILE_TYPE_STYLE style);
	
	/*
	 * \param v The pointer to dereference, it may be a regular pointer, a complete obj or a complete array
	 * \param offset An offset coming from code, which may be also NULL
	 * \param namedOffset An offset that will be added verbatim to the code
	 */
	void compileDereferencePointer(const llvm::Value* v, const llvm::Value* offset, const char* namedOffset = NULL);
	void compileGEP(const llvm::Value* val, const llvm::Use* it, const llvm::Use* const itE);
	llvm::Type* compileObjectForPointerGEP(const llvm::Value* val, const llvm::Use* it,
			const llvm::Use* const itE, COMPILE_FLAG flag);
	bool compileOffsetForPointerGEP(const llvm::Value* val, const llvm::Use* it, const llvm::Use* const itE,
			llvm::Type* lastType);
	llvm::Type* compileObjectForPointer(const llvm::Value* val, COMPILE_FLAG flag);
	/*
	 * Returns true if anything is printed
	 */
	bool compileOffsetForPointer(const llvm::Value* val, llvm::Type* lastType);
	void compileMove(const llvm::Value* dest, const llvm::Value* src, const llvm::Value* size);
	enum COPY_DIRECTION { FORWARD=0, BACKWARD, RESET };
	void compileMemFunc(const llvm::Value* dest, const llvm::Value* src, const llvm::Value* size,
			COPY_DIRECTION copyDirection);
	void compileCopyRecursive(const std::string& baseName, const llvm::Value* baseDest,
		const llvm::Value* baseSrc, llvm::Type* currentType, const char* namedOffset);
	void compileResetRecursive(const std::string& baseName, const llvm::Value* baseDest,
		const llvm::Value* resetValue, llvm::Type* currentType, const char* namedOffset);
	void compileDowncast( llvm::ImmutableCallSite callV );
	void compileAllocation(const DynamicAllocInfo & info);
	void compileFree(const llvm::Value* obj);
	void compilePointer(const llvm::Value* v, POINTER_KIND acceptedKind);
	void compileOperandImpl(const llvm::Value* v);
	void compileMethodArgs(const llvm::User::const_op_iterator it, const llvm::User::const_op_iterator itE);
	void compileMethodArgsForDirectCall(const llvm::User::const_op_iterator it, const llvm::User::const_op_iterator itE, llvm::Function::const_arg_iterator arg_it);
	void handleBuiltinNamespace(const char* ident, const llvm::Function* calledFunction,
			llvm::User::const_op_iterator it, llvm::User::const_op_iterator itE);
	COMPILE_INSTRUCTION_FEEDBACK handleBuiltinCall(llvm::ImmutableCallSite callV, const llvm::Function * f);
	void compileMethod(const llvm::Function& F);
	void compileGlobal(const llvm::GlobalVariable& G);
	uint32_t compileClassTypeRecursive(const std::string& baseName, llvm::StructType* currentType, uint32_t baseCount);
	void compileClassType(llvm::StructType* T);
	void compileArrayClassType(llvm::StructType* T);
	void compileArrayPointerType();
	void compileCreateClosure();
	void compileHandleVAArg();
	void compileConstantExpr(const llvm::ConstantExpr* ce);
	void compileNullPtrs();
	static uint32_t getMaskForBitWidth(int width);
	void compileSignedInteger(const llvm::Value* v);
	void compileUnsignedInteger(const llvm::Value* v);
	//JS interoperability support
	void compileClassesExportedToJs();
public:
	ostream_proxy stream;
	CheerpWriter(llvm::Module& m, llvm::raw_ostream& s, llvm::AliasAnalysis& AA,
		const std::string& sourceMapName, llvm::raw_ostream* sourceMap, bool ReadableOutput):
		module(m),targetData(&m),AA(AA),currentFun(NULL),globalDeps(m), namegen( globalDeps, ReadableOutput ),types(m, globalDeps.classesWithBaseInfo() ), analyzer( namegen, types, AA ),
		sourceMapGenerator(sourceMap,m.getContext()),sourceMapName(sourceMapName),NewLine(sourceMapGenerator),
		stream(s, ReadableOutput)
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
