//===-- Cheerp/JsExport.h - Cheerp JsExport utilities routines -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2019-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_JSEXPORT_H
#define _CHEERP_JSEXPORT_H

#include "llvm/IR/Module.h"

namespace cheerp
{

template<class T>
class JsExportIterator {
	const llvm::Module& module;
	llvm::NamedMDNode::const_op_iterator it;

public:
	JsExportIterator(const llvm::Module& module, llvm::NamedMDNode::const_op_iterator it) : module(module), it(it) {}

	bool operator==(const JsExportIterator& other) const {
		return it == other.it;
	}

	bool operator!=(const JsExportIterator& other) const {
		return it != other.it;
	}

	JsExportIterator& operator++() {
		++it;
		return *this;
	}

	JsExportIterator operator++(int) {
		JsExportIterator tmp(it);
		operator++();
		return tmp;
	}

	JsExportIterator& operator--() {
		--it;
		return *this;
	}

	JsExportIterator operator--(int) {
		JsExportIterator tmp(it);
		operator--();
		return tmp;
	}

	T operator*() const {
		return T(module, *it);
	}
};

class JsExportRecord {
	llvm::StringRef name;
	llvm::StructType* type;

public:
	JsExportRecord(const llvm::Module& module, const llvm::MDNode* node);

	llvm::StringRef getName() const;
	llvm::StructType* getType() const;
	std::string getJsName() const;
};

class JsExportFunction {
	llvm::Function* function;
	uint32_t flags;

public:
	JsExportFunction(const llvm::Module& module, const llvm::MDNode* node);

	llvm::StringRef getBaseName() const;
	llvm::Function* getFunction() const;
	std::string getJsName() const;
	bool isStatic() const;
	bool isConstructor() const;
};

using JsExportRecordIterator = JsExportIterator<JsExportRecord>;
using JsExportFunctionIterator = JsExportIterator<JsExportFunction>;

llvm::iterator_range<JsExportRecordIterator> getJsExportRecords(const llvm::Module& module);
llvm::iterator_range<JsExportFunctionIterator> getJsExportFunctions(const llvm::Module& module);

}//namespace cheerp

#endif //_CHEERP_JSEXPORT_H
