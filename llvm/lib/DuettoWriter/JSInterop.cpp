//===-- JSInterop.cpp - The Duetto JavaScript generator -------------===//
//
//                     Duetto: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Duetto/Writer.h"

using namespace llvm;
using namespace duetto;

const char* getMethodFromMangledName(StringRef className, const char* name)
{
	if(strncmp(name,"_ZN",3)!=0)
		return NULL;
	name+=3;
	char* localClassName;
	int classNameLen = strtol(name, &localClassName, 10);
	if(strncmp(localClassName,className.data(),className.size())!=0)
		return NULL;
	name+=className.size();
	return localClassName+classNameLen;
}

void DuettoWriter::compileClassesExportedToJs()
{
	Module::const_named_metadata_iterator it=module.named_metadata_begin();
	Module::const_named_metadata_iterator itE=module.named_metadata_end();
	//Look for metadata which ends in _methods. They are the have the list
	//of exported methods for JS layout classes
	for(;it!=itE;++it)
	{
		const NamedMDNode* namedNode=&(*it);
		StringRef name = namedNode->getName();
		if(!name.endswith("_methods"))
			continue;
		//Extract the class name
		const char* className = name.data();
		if(strncmp(className,"class._Z",8)!=0)
			continue;
		className+=8;
		char* endOfClassLen;
		int classNameLen = strtol(className, &endOfClassLen, 10);
		int classPartLen = classNameLen+(endOfClassLen-className);
		//The full name of the class including the length starts at className
		//and is classPartLen bytes long
		StringRef jsClassName(endOfClassLen,classNameLen);
		//Retrieve the corresponding StructType
		StructType* t = module.getTypeByName(name.substr(0, 8+classPartLen));
		//First compile the constructor
		for(uint32_t i=0;i<namedNode->getNumOperands();i++)
		{
			MDNode* node=namedNode->getOperand(i);
			Function* f=cast<Function>(cast<ConstantAsMetadata>(node->getOperand(0))->getValue());
			//Extract the method name
			const char* methodName = getMethodFromMangledName(jsClassName, f->getName().data());
			if(strncmp(methodName,"C1",2)!=0)
				continue;
			stream << "function " << jsClassName << '(';
			for(uint32_t i=0;i<f->arg_size()-1;i++)
			{
				if(i!=0)
					stream << ",";
				stream << 'a' << i;
			}
			stream << "){\n";
			compileType(t, THIS_OBJ);
			//We need to manually add the self pointer
			stream << ";\nthis.s=this;\n";
			compileOperand(f);
			stream << "({d:this,o:'s'}";
			for(uint32_t i=0;i<f->arg_size()-1;i++)
				stream << ",a" << i;
			stream << ");\n}\n";
			
			assert( globalDeps.isReachable(f) );
			break;
		}
		//Then compile other methods and add them to the prototype
		for(uint32_t i=0;i<namedNode->getNumOperands();i++)
		{
			MDNode* node=namedNode->getOperand(i);
			Function* f=cast<Function>(cast<ConstantAsMetadata>(node->getOperand(0))->getValue());
			//Extract the method name
			const char* methodPart = getMethodFromMangledName(jsClassName, f->getName().data());
			if(strncmp(methodPart,"C1",2)==0)
				continue;
			char* methodName;
			int methodNameLen = strtol(methodPart, &methodName, 10);
			StringRef jsMethodName(methodName, methodNameLen);
			stream << jsClassName << ".prototype." << jsMethodName << "=function (";
			for(uint32_t i=0;i<f->arg_size()-1;i++)
			{
				if(i!=0)
					stream << ",";
				stream << 'a' << i;
			}
			stream << "){\nreturn ";
			compileOperand(f);
			stream << "({d:this,o:'s'}";
			for(uint32_t i=0;i<f->arg_size()-1;i++)
				stream << ",a" << i;
			stream << ");\n}\n";
			
			assert( globalDeps.isReachable(f) );
		}
	}
}
