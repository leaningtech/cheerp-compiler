//===-- JSInterop.cpp - The Cheerp JavaScript generator -------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2015-2020 Leaning Technologies
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

static std::pair<std::string, std::string> buildArgumentsString(const llvm::Function* F, bool isStatic, const PointerAnalyzer& PA, const std::map<const StructType*, StringRef>& toBeWrapped)
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
		const std::string aX = param.str();

		bool isWrapped = false;
		if (it->getType()->isPointerTy() && toBeWrapped.count(dyn_cast<StructType>(it->getType()->getPointerElementType())))
			isWrapped = true;

		args_inner << aX << ",";
		if(it->getType()->isPointerTy() && PA.getPointerKind(&(*it)) == SPLIT_REGULAR)
		{
			assert(isWrapped);
			{
				// Split regular representation
				args_outer << aX << ".this.d," << aX << ".this.o,";
			}
		}
		else
		{
			if (isWrapped)
			{
				// Complete object
				args_outer << aX << ".this.d[" << aX << ".this.o],";
			}
			else
			{
				// forward as input
				args_outer << aX << ",";
			}
		}
		++ret;
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

	for( const NamedMDNode& namedNode: module.named_metadata())
	{
		StringRef name = namedNode.getName();

		if(name == "jsexported_free_functions")
			return true;

		if (name.endswith("_methods") && (name.startswith("class.") || name.startswith("struct.")) )
			return true;
	}
	return false;
}

static std::vector<const llvm::MDNode*> uniqueMDNodes(const llvm::NamedMDNode& namedNode)
{
	std::vector<const llvm::MDNode*> vectorMDNode;
	{
		//There might be repeated nodes, here we unique them while keeping sorted
		llvm::DenseSet<const llvm::MDNode*> set;
		for ( NamedMDNode::const_op_iterator it = namedNode.op_begin(); it != namedNode.op_end(); ++ it )
		{
			if (set.insert(*it).second)
				vectorMDNode.push_back(*it);
		}
	}
	return vectorMDNode;
}

void CheerpWriter::compileDeclExportedToJs(const bool alsoDeclare)
{
	std::map<const StructType*, StringRef> jsexportedTypes;

	auto compileFunctionBody = [&](const Function * f, bool isStatic, const StructType* implicitThis) -> void
	{
		auto argumentsStrings = buildArgumentsString(f, isStatic, PA, jsexportedTypes);
		stream << "function(" << argumentsStrings.first << "){" << NewLine;

		const llvm::StructType* retType = nullptr;
		if (f->getReturnType() && f->getReturnType()->isPointerTy())
			retType = dyn_cast<StructType>(f->getReturnType()->getPointerElementType());

		if (jsexportedTypes.count(retType))
		{
			stream << "var _=Object.create(" << jsexportedTypes.at(retType) << ".prototype);" << NewLine;
			stream << "_.this={d:";
		}
		else
			stream << "return ";

		auto internalName = namegen.getName(f);
		stream << internalName << "(";
		if(!isStatic && implicitThis)
		{
			if(PA.getPointerKind(&*f->arg_begin())==SPLIT_REGULAR)
				stream << "this.this.d,this.this.o";
			else
				stream << "this.this.d[this.this.o]";
			if(argumentsStrings.second.size() > 0)
				stream << ",";
		}
		stream << argumentsStrings.second << ")";
		if (jsexportedTypes.count(retType))
		{
			stream << "," << NewLine << "o:oSlot};" << NewLine;
			stream << "return _;" << NewLine;
		}
		else
		{
			stream << ";" << NewLine;
		}
		stream << "};" << NewLine;
	};

	auto processFunction = [&](const Function * f, const StringRef& name) -> void
	{
		if (alsoDeclare && !isNamespaced(name))
			stream << "var ";

		stream << name << '=';

		compileFunctionBody(f, /*isStatic*/ true, nullptr);
	};

	auto processRecord = [&](const StructType* t, const llvm::NamedMDNode& namedNode, const llvm::StringRef& jsClassName) -> void
	{
		auto vectorMDNode = uniqueMDNodes(namedNode);

		auto getMethodName = [&](const MDNode * node) -> StringRef
		{
			assert( isa<Function>(cast<ConstantAsMetadata>(node->getOperand(0))->getValue()) );

			StringRef mangledName = cast<Function>(cast<ConstantAsMetadata>(node->getOperand(0))->getValue())->getName();

			demangler_iterator dmg(mangledName);

			for (unsigned int i=0;i<jsClassName.size(); i++)
			{
				if (jsClassName[i] == '.')
					dmg++;
			}
			if (!isRootNeeded)
				dmg++;

			StringRef functionName = *dmg++;

			assert( dmg == demangler_iterator() );
			return functionName;
		};
		auto isStaticMethod = [&](const MDNode * node) -> bool {
			assert(node->getNumOperands() >= 2);
			assert( isa<ConstantInt>(cast<ConstantAsMetadata>(node->getOperand(1))->getValue()) );
			const uint32_t value = cast<ConstantInt>(cast<ConstantAsMetadata>(node->getOperand(1))->getValue())->getZExtValue();
			return cheerp::isStatic(value);
		};

		auto isConstructor = [&](const MDNode * node ) -> bool
		{
			assert(node->getNumOperands() >= 2);
			assert( isa<ConstantInt>(cast<ConstantAsMetadata>(node->getOperand(1))->getValue()) );
			const uint32_t value = cast<ConstantInt>(cast<ConstantAsMetadata>(node->getOperand(1))->getValue())->getZExtValue();
			return cheerp::isConstructor(value);
		};

		auto isDestructor = [&](const MDNode * node ) -> bool
		{
			assert(node->getNumOperands() >= 2);
			assert( isa<ConstantInt>(cast<ConstantAsMetadata>(node->getOperand(1))->getValue()) );
			const uint32_t value = cast<ConstantInt>(cast<ConstantAsMetadata>(node->getOperand(1))->getValue())->getZExtValue();
			return cheerp::isDestructor(value);
		};


		auto constructor = std::find_if(vectorMDNode.begin(), vectorMDNode.end(), isConstructor );

		const bool hasConstructor = (constructor != vectorMDNode.end());

		if (hasConstructor && std::find_if( std::next(constructor), vectorMDNode.end(), isConstructor ) != vectorMDNode.end() )
		{
			llvm::report_fatal_error( Twine("More than one constructor defined for class: ", jsClassName) );
			return;
		}

		if (alsoDeclare && !isNamespaced(jsClassName))
			stream << "function " << jsClassName << '(';
		else
			stream << jsClassName << "=function (";
		const Function * f = NULL;

		//First compile the constructor
		if (hasConstructor)
		{
			const MDNode* node = *constructor;
			f = cast<Function>(cast<ConstantAsMetadata>(node->getOperand(0))->getValue());

			const auto argumentsStrings = buildArgumentsString(f, /*isStatic*/false, PA, jsexportedTypes);

			stream << argumentsStrings.first << "){" << NewLine;

			stream << "this.this={d:[";
			// Regular constructor
			compileType(const_cast<StructType*>(t), LITERAL_OBJ);
			//We need to manually add the self pointer
			stream << "],"<< NewLine <<"o:0};" << NewLine;
			compileOperand(f);
			stream << '(';
				stream << "this.this.d[0]";

			if (argumentsStrings.second.size() > 0)
				stream << "," << argumentsStrings.second;
			stream << ");";

			assert( globalDeps.isReachable(f) );
		}
		else
		{
			stream << "){" << NewLine;
			stream << "throw new Error(\"Class/Struct " << jsClassName << " do not have a [[cheerp::jsexport]]-ed constructor\");";
		}
		stream << NewLine << "};" << NewLine;

		bool compiledDestructor = false;
		//Then compile other methods and add them to the prototype
		for ( const MDNode* node : vectorMDNode)
		{
			if ( isConstructor(node) )
				continue;

			StringRef methodName = getMethodName(node);
			if (isDestructor(node))
			{
				compiledDestructor = true;
				methodName = StringRef("delete");
			}
			bool isStatic = isStaticMethod(node);

			const Function * f = cast<Function>(cast<ConstantAsMetadata>(node->getOperand(0))->getValue());

			stream << jsClassName;
			if (!isStatic)
				stream << ".prototype";
			stream << '.' << methodName << "=";

			compileFunctionBody(f, isStatic, t);

			assert( globalDeps.isReachable(f) );
		}
		if (!compiledDestructor)
		{
			stream << jsClassName << ".prototype.delete=function(){" << NewLine << "};" << NewLine;
		}
	};

	for (const auto& jsex : jsExportedDecls)
	{
		if (jsex.isClass())
			jsexportedTypes.insert({jsex.t, jsex.name});
	}

	for (const auto& jsex : jsExportedDecls)
	{
		if (jsex.isClass())
			processRecord(jsex.t, *jsex.node, jsex.name);
		else
			processFunction(jsex.F, jsex.name);
	}
}

std::deque<CheerpWriter::JSExportedNamedDecl> CheerpWriter::buildJsExportedNamedDecl(const llvm::Module& M)
{
	//Here a vector is used since repetitions are not allowed
	std::deque<CheerpWriter::JSExportedNamedDecl> jsExported;

	auto processFunction = [&jsExported](const Function * f) -> void
	{
		const std::string name = TypeSupport::getNamespacedFunctionName(f->getName());
		jsExported.emplace_back(f, name);
	};

	auto processRecord = [&M, &jsExported](const llvm::NamedMDNode& namedNode, const llvm::StringRef& name) -> void
	{
		auto structAndName = TypeSupport::getJSExportedTypeFromMetadata(name, M);
		StringRef jsClassName = structAndName.second;
		jsExported.emplace_back(structAndName.first, namedNode, jsClassName);
	};

	//This functions take cares of iterating over all metadata, executing processFunction on each jsexport-ed function
	//and processRecord on each jsexport-ed class/struct
	iterateOverJsExportedMetadata(M, processFunction, processRecord);

	return jsExported;
}

void CheerpWriter::prependRootToNames(std::deque<CheerpWriter::JSExportedNamedDecl> & exportedDecls)
{
	//We will then consider "__root" as a namespace
	for (auto& jsex : exportedDecls)
	{
		jsex.name = "__root." + jsex.name;
	}
}

void CheerpWriter::normalizeDeclList(std::deque<CheerpWriter::JSExportedNamedDecl> & exportedDecls)
{
	auto comparator = [](const JSExportedNamedDecl& a, const JSExportedNamedDecl& b) -> bool {return a.name < b.name;};
	auto equality = [](const JSExportedNamedDecl& a, const JSExportedNamedDecl& b) -> bool {return a.name == b.name;};

	std::sort(exportedDecls.begin(), exportedDecls.end(), comparator);
	auto it = adjacent_find(exportedDecls.begin(), exportedDecls.end(), equality);
	if (it != exportedDecls.end())
	{
		llvm::report_fatal_error( Twine("Name clash on [[cheerp::jsexport]]-ed items on the name: ", it->name));
	}
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
