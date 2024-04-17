//===-- JSInterop.cpp - The Cheerp JavaScript generator -------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2015-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/Writer.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/Cheerp/JsExport.h"
#include <numeric>
#include <sstream>

using namespace llvm;
using namespace cheerp;

uint32_t CheerpWriter::countJsParameters(const llvm::Function* F, bool isStatic) const
{
	uint32_t ret = 0;
	auto it = F->arg_begin();
	auto itE = F->arg_end();
	if(!isStatic)
	{
		// The this parameter is implicit
		++it;
	}
	while(it != itE)
	{
		if(it->getType()->isPointerTy() && PA.getPointerKind(&(*it)) == SPLIT_REGULAR)
			ret+=2;
		else
			ret+=1;
		++it;
	}
	return ret;
}

static std::pair<std::string, std::string> buildArgumentsString(const llvm::Function* F, bool isStatic, const PointerAnalyzer& PA, const DenseMap<const Type*, std::string>& toBeWrapped, StringRef nameGenerated)
{
	// While code-generating JSExported functions we need to write something like:
	// function someName(a0,a1,a2,a3,a4,a5) {
	// 	return equivalentInnerFunctionName(a0,a1,a2,[a3],0,a4,a5);	//very much an example, where a3 needs to be passes as SPLIT_REGULAR
	// }
	//
	// This helper function will return (via a pair) two strings that will be used like:
	// function someName(PAIR_FIRST_MEMBER)
	// {
	// 	return equivalentInnerFunctionName(PAIR_SECOND_MEMBER);
	// }


	std::ostringstream args_outer;		// args_outer will contain the comma-separated list of arguments to the outer function
	std::ostringstream args_inner;		// args_inner will contain the comma-separated list of arguments to the inner function

	uint32_t ret = 0;
	auto it = F->arg_begin();
	auto itE = F->arg_end();
	if(!isStatic)
	{
		// The this parameter is implicit, skip it
		++it;
	}
	while(it != itE)
	{
		std::ostringstream param;
		param << "a" << ret;
		++ret;
		const std::string aX = param.str();
		if (aX == nameGenerated)
			continue;

		args_inner << aX << ",";
		Type* ty = it->getType();
		if (!ty->isPointerTy() || !toBeWrapped.count(ty->getPointerElementType()))
		{
			args_outer << aX << ",";
			it++;
			continue;
		}

		POINTER_KIND innerKind = PA.getPointerKind(&(*it));
		POINTER_KIND outerKind = innerKind;
		Type* tp = it->getType()->getPointerElementType();
		if (toBeWrapped.count(tp))
			outerKind = PA.getPointerKindForJSExportedType(const_cast<Type*>(tp));

		if (outerKind == REGULAR && innerKind == SPLIT_REGULAR)
		{
			args_outer << aX << ".this.d," << aX << ".this.o";
		}
		else if (outerKind == REGULAR && innerKind == COMPLETE_OBJECT)
		{
			args_outer << aX << ".this.d[" << aX << ".this.o]";
		}
		else
		{
			assert(outerKind == innerKind && (outerKind == RAW || outerKind == COMPLETE_OBJECT));
			args_outer << aX << ".this";
		}
		args_outer << ",";
		++it;
	}

	// Extract from stringstream
	std::string inner = args_inner.str();
	std::string outer = args_outer.str();

	// Normalize removing (evenutal) last comma
	if (inner.size())
		inner.pop_back();
	if (outer.size())
		outer.pop_back();

	// Package in pair
	return {inner, outer};
}

bool CheerpWriter::hasJSExports()
{
	auto* recordsMetadata = module.getNamedMetadata("jsexport_records");
	auto* functionsMetadata = module.getNamedMetadata("jsexport_functions");

	return recordsMetadata || functionsMetadata;
}

void CheerpWriter::compileJsExportFunctionBody(const Function* f, bool isStatic, const StructType* implicitThis)
{
	auto argumentsStrings = buildArgumentsString(f, isStatic, PA, jsExportedTypes, namegen.getName(f, 0));
	const llvm::StructType* retType = nullptr;
	if (f->getReturnType() && f->getReturnType()->isPointerTy())
		retType = dyn_cast<StructType>(f->getReturnType()->getPointerElementType());
	auto internalName = namegen.getName(f, 0);

	if(argumentsStrings.first == argumentsStrings.second)
	{
		// The arguments used internally and externally are identical, no mapping is required
		// check if we can use the function directly without a wrapper
		if(isStatic && !jsExportedTypes.count(retType))
		{
			stream << internalName;
			return;
		}
	}
	stream << "function(" << argumentsStrings.first << "){" << NewLine;

	stream << "return ";
	bool isRegular = false;
	if (jsExportedTypes.count(retType))
	{
		stream << "Object.create(" << jsExportedTypes.find(retType)->getSecond() << ".prototype,{this:{value:";
		if (PA.getPointerKindForJSExportedType(const_cast<StructType*>(retType)) == REGULAR)
		{
			assert(PA.getPointerKindForReturn(f) == SPLIT_REGULAR);
			isRegular = true;
			stream << "{d:";
		}
	}

	stream << internalName << "(";
	if(!isStatic && implicitThis)
	{
		POINTER_KIND argKind = PA.getPointerKind(&*f->arg_begin());
		POINTER_KIND thisKind = PA.getPointerKindForJSExportedType(const_cast<StructType*>(implicitThis));
		if (thisKind == REGULAR && argKind == SPLIT_REGULAR)
		{
			stream << "this.this.d,this.this.o";
		}
		else if (thisKind == REGULAR && argKind == COMPLETE_OBJECT)
		{
			stream << "this.this.d[this.this.o]";
		}
		else
		{
			assert(thisKind == argKind && (thisKind == RAW || thisKind == COMPLETE_OBJECT));
			stream << "this.this";
		}

		if(argumentsStrings.second.size() > 0)
			stream << ",";
	}
	stream << argumentsStrings.second << ")";
	if (jsExportedTypes.count(retType))
	{
		if (isRegular)
		{
			stream << ",o:oSlot}";
		}
		stream << "}})";
	}
	stream << ";" << NewLine;
	stream << "}";
}

void CheerpWriter::compileJsExportProperty(const JsExportProperty& property, bool isStatic, const llvm::StructType* implicitThis)
{
	stream << "{" << NewLine;

	if (property.hasGetter())
	{
		stream << "get:";
		compileJsExportFunctionBody(property.getGetter().getFunction(), isStatic, implicitThis);
		stream << "," << NewLine;
	}

	if (property.hasSetter())
	{
		stream << "set:";
		compileJsExportFunctionBody(property.getSetter().getFunction(), isStatic, implicitThis);
		stream << "," << NewLine;
	}

	stream << "}";
}

void CheerpWriter::compileDeclExportedToJs(const bool alsoDeclare)
{
	auto processFunction = [&](llvm::StringRef name, const JsExportFunction& func) -> void
	{
		if (alsoDeclare && !isNamespaced(name))
			stream << "var ";

		stream << name << '=';
		compileJsExportFunctionBody(func.getFunction(), /*isStatic*/ true, nullptr);
		stream << ";" << NewLine;
	};

	auto processRecord = [&](llvm::StringRef jsClassName, const JsExportClass& record) -> void
	{
		const llvm::Function* newFunc = nullptr;
		const llvm::StructType* t = record.getType();

		for (const auto& [_, method] : record.getMethods())
			if (method.isConstructor())
				newFunc = method.getFunction();

		if (alsoDeclare && !isNamespaced(jsClassName))
			stream << "function " << jsClassName << '(';
		else
			stream << jsClassName << "=function (";

		//First compile the constructor
		if (newFunc)
		{
			assert( globalDeps.isReachable(newFunc) );

			POINTER_KIND thisKind = PA.getPointerKindForJSExportedType(const_cast<StructType*>(t));
			const auto argumentsStrings = buildArgumentsString(newFunc, /*isStatic*/true, PA, jsExportedTypes, namegen.getName(newFunc, 0));

			stream << argumentsStrings.first << "){" << NewLine;

			stream << "this.this=";
			if (thisKind == REGULAR)
				stream << "{d:";

			stream << namegen.getName(newFunc, 0);
			stream << "(" << argumentsStrings.second<< ")";

			//We need to manually add the self pointer
			if (thisKind == REGULAR)
				stream << ","<< NewLine <<"o:oSlot}";
			stream << ";";
		}
		else
		{
			stream << "){" << NewLine;
			stream << "throw new Error(\"Class/Struct " << jsClassName << " do not have a [[cheerp::jsexport]]-ed constructor\");";
		}
		stream << NewLine << "};" << NewLine;

		//Then compile other methods and add them to the prototype
		for (const auto& [_, method] : record.getMethods())
		{
			if (!method.isConstructor())
			{
				llvm::StringRef methodName = method.getName().base();
				const bool isStatic = method.isStatic();
				const Function * f = method.getFunction();
				assert( globalDeps.isReachable(f) );

				stream << jsClassName;
				if (!isStatic)
					stream << ".prototype";
				stream << '.' << methodName << "=";
				compileJsExportFunctionBody(f, isStatic, t);
				stream << ";" << NewLine;
			}
		}

		for (const auto& [_, property] : record.getProperties())
		{
			stream << "Object.defineProperty(" << jsClassName;
			if (!property.isStatic())
				stream << ".prototype";
			stream << ",'" << property.getName() << "',";
			compileJsExportProperty(property, property.isStatic(), t);
			stream << ");" << NewLine;
		}
	};

	auto processProperty = [&](llvm::StringRef name, const JsExportProperty& prop) -> void
	{
		auto sep = name.rfind('.');

		if (sep != llvm::StringRef::npos)
		{
			stream << "Object.defineProperty(" << name.substr(0, sep) << ",'" << name.substr(sep + 1) << "',";
			compileJsExportProperty(prop, true, nullptr);
			stream << ");" << NewLine;
		}
	};

	jsExportedDecls.getExportsFlat([&](llvm::StringRef name, const JsExport& jsex)
	{
		if (auto* func = std::get_if<JsExportFunction>(&jsex))
			processFunction(name, *func);
		else if (auto* record = std::get_if<JsExportClass>(&jsex))
			processRecord(name, *record);
		else if (auto* prop = std::get_if<JsExportProperty>(&jsex))
			processProperty(name, *prop);
	});
}

void CheerpWriter::prependRootToNames(JsExportModule& exportedDecls)
{
	JsExportModule tmp;
	llvm::StringRef root = "__root";
	tmp.insert(root, std::move(exportedDecls));
	exportedDecls = std::move(tmp);
}

void CheerpWriter::compileInlineAsm(const CallInst& ci)
{
	const InlineAsm* a=cast<InlineAsm>(ci.getCalledOperand());
	// NOTE: We ignore the constraint string here, since the frontend ensures that only "r" is allowed
	StringRef str = a->getAsmString();
	bool needsParamIndex = false;
	int paramIndex = 0;
	auto constraints = a->ParseConstraints();
	// TODO: support output parameters in the template string
	int numOutputs = std::accumulate(constraints.begin(), constraints.end(),
		0, [](int acc, const llvm::InlineAsm::ConstraintInfo& c) {
			return acc + (c.Type == InlineAsm::isOutput);
		});
	auto getInputParamIndex = [&](int p) {
		if (p - numOutputs < 0) {
			llvm::errs() << str << "\n";
			llvm::report_fatal_error("Cheerp: Output operands in the asm template are not supported");
		}
		return p - numOutputs;
	};
	for(unsigned i=0;i<str.size();i++)
	{
		if(needsParamIndex)
		{
			if(str[i] >= '0' && str[i] <= '9')
			{
				if(paramIndex == -1)
					paramIndex = 0;
				paramIndex *= 10;
				paramIndex += str[i] - '0';
			}
			else if(paramIndex == -1)
			{
				// Not even 1 digit after the $, it's an escape
				stream << str[i];
				needsParamIndex = false;
			}
			else
			{
				// Parameter parsed, do not forget to output this char as well
				compileOperand(ci.getOperand(getInputParamIndex(paramIndex)));
				stream << str[i];
				needsParamIndex = false;
			}
		}
		else if(str[i] == '$')
		{
			needsParamIndex = true;
			paramIndex = -1;
		}
		else
			stream << str[i];
	}
	if(needsParamIndex)
	{
		// The string ends with a parameter index, do not forget it
		compileOperand(ci.getOperand(getInputParamIndex(paramIndex)));
	}
}
