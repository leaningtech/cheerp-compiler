//===-- Cheerp/PointerAnalyzer.h - Cheerp pointer analyzer code --------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2013 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_POINTER_ANALYZER_H
#define _CHEERP_POINTER_ANALYZER_H

#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/Timer.h"

namespace cheerp {

enum POINTER_KIND {
	COMPLETE_OBJECT,
	REGULAR
};

class PointerAnalyzer {
public:
	PointerAnalyzer() : 
#ifndef NDEBUG
		timerGroup("Pointer Analyzer"),
		gpkTimer("getPointerKind",timerGroup),
		gpkfrTimer("getPointerKindForReturn",timerGroup)
#endif //NDEBUG
	{}

	void prefetch( const llvm::Module & ) const;

	POINTER_KIND getPointerKind(const llvm::Value* v) const;
	POINTER_KIND getPointerKindForReturn(const llvm::Function* F) const;
	POINTER_KIND getPointerKindForType( llvm::Type * tp) const;
#ifndef NDEBUG
	// Dump a pointer value info
	void dumpPointer(const llvm::Value * v, bool dumpOwnerFuncion = true) const;
#endif //NDEBUG

	typedef llvm::DenseMap<const llvm::Value*, POINTER_KIND> ValueKindMap;

private:
	mutable ValueKindMap cache;

#ifndef NDEBUG
	mutable llvm::TimerGroup timerGroup;
	mutable llvm::Timer gpkTimer, gpkfrTimer;
#endif //NDEBUG
};

#ifndef NDEBUG

void dumpAllPointers(const llvm::Function &, const PointerAnalyzer & );
void writePointerDumpHeader();

#endif //NDEBUG

}

#endif //_CHEERP_POINTER_ANALYZER_H
