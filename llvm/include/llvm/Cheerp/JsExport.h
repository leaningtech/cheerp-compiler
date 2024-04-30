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
#include "llvm/Cheerp/Utility.h"
#include <map>

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

class JsExportName {
	llvm::StringRef name;

public:
	JsExportName(llvm::StringRef name);

	llvm::StringRef string() const;
	llvm::StringRef base() const;
	llvm::StringRef root() const;
	std::vector<llvm::StringRef> split() const;
	std::string js() const;
};

class JsExportRecord {
	llvm::StructType* type;
	llvm::StructType* base = nullptr;
	uint32_t flags;

public:
	JsExportRecord(const llvm::Module& module, const llvm::MDNode* node);

	llvm::StringRef getStructName() const;
	JsExportName getName() const;
	llvm::StructType* getType() const;
	llvm::StructType* getBase() const;
	bool isAbstract() const;
};

class JsExportFunction {
	llvm::Function* function;
	uint32_t flags;

public:
	JsExportFunction(const llvm::Module& module, const llvm::MDNode* node);

	JsExportName getName() const;
	llvm::StringRef getPropertyName() const;
	llvm::Function* getFunction() const;
	bool isStatic() const;
	bool isConstructor() const;
	bool isGetter() const;
	bool isSetter() const;
	bool isProperty() const;
};

class JsExportProperty {
	std::optional<JsExportFunction> getter;
	std::optional<JsExportFunction> setter;

public:
	llvm::StringRef getName() const;
	llvm::Type* getType() const;
	bool hasGetter() const;
	bool hasSetter() const;
	const JsExportFunction& getGetter() const;
	const JsExportFunction& getSetter() const;
	bool isStatic() const;
	void insert(JsExportFunction&& func);
};

template<class T>
using JsExportMap = std::map<llvm::StringRef, T>;

class JsExportClass : public JsExportRecord {
	JsExportMap<JsExportFunction> methods;
	JsExportMap<JsExportProperty> properties;

public:
	JsExportClass(const JsExportRecord& record);

	const JsExportMap<JsExportFunction>& getMethods() const;
	const JsExportMap<JsExportProperty>& getProperties() const;
	JsExportFunction& insert(llvm::StringRef name, JsExportFunction&& method);
	JsExportProperty& insert(llvm::StringRef name, JsExportProperty&& property);
};

class JsExportModule;

using JsExport = std::variant<JsExportModule, JsExportFunction, JsExportProperty, JsExportClass>;
using JsExportRef = std::variant<JsExportModule*, JsExportFunction*, JsExportProperty*, JsExportClass*>;

class JsExportModule {
	JsExportMap<JsExport> exports;
	bool types = false;

public:
	const JsExportMap<JsExport>& getExports() const;
	bool hasTypes() const;
	JsExportRef insert(llvm::ArrayRef<llvm::StringRef> name, JsExport&& ex);

	template<class F>
	void getExportsFlat(F func) const {
		for (const auto& [name, value] : exports)
			getExportsHelper(name, value, func);
	}

	template<class... T, class F>
	void getExportsFilter(F func) const {
		getExportsFlat([&func](llvm::StringRef name, const JsExport& value) {
			if ((std::holds_alternative<T>(value) || ...))
				func(name, value);
		});
	}

private:
	template<class F>
	static void getExportsHelper(llvm::StringRef name, const JsExport& value, F& func) {
		func(name, value);

		if (auto* ex = std::get_if<JsExportModule>(&value)) {
			for (const auto& [exportName, exportValue] : ex->exports) {
				std::string nameString = (name + "." + exportName).str();
				getExportsHelper(nameString, exportValue, func);
			}
		}
	}
};

using JsExportRecordIterator = JsExportIterator<JsExportRecord>;
using JsExportFunctionIterator = JsExportIterator<JsExportFunction>;

llvm::iterator_range<JsExportRecordIterator> getJsExportRecords(const llvm::Module& module);
llvm::iterator_range<JsExportFunctionIterator> getJsExportFunctions(const llvm::Module& module);
JsExportModule getJsExportModule(const llvm::Module& module);

}//namespace cheerp

#endif //_CHEERP_JSEXPORT_H
