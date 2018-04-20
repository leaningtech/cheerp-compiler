//===-- JSInterop.cpp - The Cheerp JavaScript generator -------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2015 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/Writer.h"

using namespace llvm;
using namespace cheerp;

void CheerpWriter::addExportedFreeFunctions(std::vector<StringRef>& namesList, const NamedMDNode* namedNode)
{
	for ( NamedMDNode::const_op_iterator it = namedNode->op_begin(); it != namedNode->op_end(); ++ it )
	{
		const MDNode * node = *it;
		const Function * f = cast<Function>(cast<ConstantAsMetadata>(node->getOperand(0))->getValue());
		// Currently we assign the function to the mangled name, it works better with extern "C" functions
		stream << "var " << f->getName() << '=' << namegen.getName(f) << ';' << NewLine;
		namesList.push_back(f->getName());
	}
}

std::vector<StringRef> CheerpWriter::compileClassesExportedToJs()
{
	std::vector<StringRef> exportedNames;
	//Look for metadata which ends in _methods. They are lists
	//of exported methods for JS layout classes
	for( Module::const_named_metadata_iterator it = module.named_metadata_begin(),
		itE = module.named_metadata_end(); it!=itE; ++it)
	{
		const NamedMDNode* namedNode = it;
		StringRef name = namedNode->getName();

		if(name == "jsexported_methods")
		{
			addExportedFreeFunctions(exportedNames, namedNode);
			continue;
		}

		if (!name.endswith("_methods") || !name.startswith("class.") )
			continue;

		auto structAndName = TypeSupport::getJSExportedTypeFromMetadata(name, module);
		StructType* t = structAndName.first;
		StringRef jsClassName = structAndName.second;

		auto getMethodName = [&](const MDNode * node) -> StringRef
		{
			assert( isa<Function>(cast<ConstantAsMetadata>(node->getOperand(0))->getValue()) );

			StringRef mangledName = cast<Function>(cast<ConstantAsMetadata>(node->getOperand(0))->getValue())->getName();

			demangler_iterator dmg(mangledName);
			if ( *dmg++ != jsClassName )
				assert( false && "[[jsexport]]: method should be in class" );

			StringRef functionName = *dmg++;

			assert( dmg == demangler_iterator() );
			return functionName;
		};

		//TODO many things to check.. For. ex C1/C2/C3, names collisions, template classes!
		auto isConstructor = [&](const MDNode * node ) -> bool
		{
			return getMethodName(node).startswith("C1");
		};

		auto constructor = std::find_if(namedNode->op_begin(), namedNode->op_end(), isConstructor );

		//First compile the constructor
		if (constructor == namedNode->op_end() )
		{
			llvm::report_fatal_error( Twine("Class: ", jsClassName).concat(" does not define a constructor!") );
			return exportedNames;
		}

		if ( std::find_if( std::next(constructor), namedNode->op_end(), isConstructor ) != namedNode->op_end() )
		{
			llvm::report_fatal_error( Twine("More than one constructor defined for class: ", jsClassName) );
			return exportedNames;
		}

		const MDNode* node = *constructor;
		const Function * f = cast<Function>(cast<ConstantAsMetadata>(node->getOperand(0))->getValue());

		exportedNames.push_back(jsClassName);

		stream << "function " << jsClassName << '(';
		for(uint32_t i=0;i<f->arg_size()-1;i++)
		{
			if(i!=0)
				stream << ",";
			stream << 'a' << i;
		}
		stream << "){" << NewLine;
		compileType(t, THIS_OBJ);
		//We need to manually add the self pointer
		stream << ';' << NewLine << "this.d=[this];" << NewLine;
		compileOperand(f);
		stream << '(';
		if(PA.getPointerKind(f->arg_begin())==COMPLETE_OBJECT)
			stream << "this";
		else
			stream << "{d:this.d,o:0}";

		for(uint32_t i=0;i<f->arg_size()-1;i++)
			stream << ",a" << i;
		stream << ");" << NewLine << "}" << NewLine;

		assert( globalDeps.isReachable(f) );

		//Then compile other methods and add them to the prototype
		for ( NamedMDNode::const_op_iterator it = namedNode->op_begin(); it != namedNode->op_end(); ++ it )
		{
			if ( isConstructor(*it) )
				continue;

			StringRef methodName = getMethodName(*it);

			const MDNode * node = *it;
			const Function * f = cast<Function>(cast<ConstantAsMetadata>(node->getOperand(0))->getValue());

			stream << jsClassName << ".prototype." << methodName << "=function (";
			for(uint32_t i=0;i<f->arg_size()-1;i++)
			{
				if(i!=0)
					stream << ",";
				stream << 'a' << i;
			}
			stream << "){" << NewLine << "return ";
			compileOperand(f);
			stream << '(';
			if(PA.getPointerKind(f->arg_begin())==COMPLETE_OBJECT)
				stream << "this";
			else
				stream << "{d:this.d,o:0}";
			for(uint32_t i=0;i<f->arg_size()-1;i++)
				stream << ",a" << i;
			stream << ");" << NewLine << "};" << NewLine;

			assert( globalDeps.isReachable(f) );
		}
	}
	return exportedNames;
}
