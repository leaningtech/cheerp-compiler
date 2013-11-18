//===-- NativeRewriter.cpp.cpp - Duetto helper ----------------------------===//
//
//                     Duetto: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2013 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Duetto/Utils.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Intrinsics.h"

using namespace llvm;
using namespace std;

bool DuettoUtils::findMangledClassName(const char* const s, const char* &className, int& classLen)
{
	if(strncmp(s,"_ZN",3)!=0)
		return false;
	const char* tmp=s+3;
	char* endPtr;
	int nsLen=strtol(tmp, &endPtr, 10);
	tmp=endPtr;
	if(nsLen==0 || (strncmp(tmp,"client",nsLen)!=0))
		return false;

	tmp+=nsLen;
	classLen=strtol(tmp, &endPtr, 10);
	className=endPtr;

	if(classLen==0)
		return false;
	return true;
}

bool DuettoUtils::isBuiltinConstructor(const char* s, const std::string& typeName)
{
	const char* mangledName;
	int mangledNameLen;
	//Extract the class name from the mangled one
	if(findMangledClassName(s, mangledName, mangledNameLen)==false)
		return false;

	if(typeName.compare(0, std::string::npos, mangledName, mangledNameLen)!=0)
		return false;

	if(strncmp(mangledName+mangledNameLen, "C1", 2)==0 ||
	   strncmp(mangledName+mangledNameLen, "C2", 2)==0)
		return true;

	return false;
}

void DuettoUtils::baseSubstitutionForBuiltin(User* i, Instruction* old, AllocaInst* source)
{
	Instruction* userInst=dyn_cast<Instruction>(i);
	assert(userInst);
	LoadInst* loadI=new LoadInst(source, "duettoPtrLoad", userInst);
	userInst->replaceUsesOfWith(old, loadI);
}

/*
 * Check if a type is builtin and return the type name
 */
bool DuettoUtils::isBuiltinType(const char* typeName, std::string& builtinName)
{
	if(strncmp(typeName, "class.", 6)!=0)
		return false;
	typeName+=6;
	const char* mangledName;
	int mangledNameLen;
	if(findMangledClassName(typeName, mangledName, mangledNameLen)==false)
		return false;

	builtinName.assign(mangledName, mangledNameLen);
	return true;
}

bool DuettoUtils::rewriteIfNativeConstructorCall(Module& M, Instruction* i, AllocaInst* newI, Instruction* callInst,
						  Function* called,const std::string& builtinTypeName,
						  SmallVector<Value*, 4>& initialArgs)
{
	//A constructor call does have a name, it's non virtual!
	if(called==NULL)
		return false;
	//To be a candidate for substitution it must have an empty body
	if(!called->empty())
		return false;
	const char* funcName=called->getName().data();
	if(!isBuiltinConstructor(funcName, builtinTypeName))
		return false;
	//Verify that this contructor is for the current alloca
	if(callInst->getOperand(0)!=i)
		return false;

	FunctionType* initialType=called->getFunctionType();
	SmallVector<Type*, 4> initialArgsTypes(initialType->param_begin()+1,
			initialType->param_end());
	FunctionType* newFunctionType=FunctionType::get(*initialType->param_begin(),
			initialArgsTypes, false);
	//Morph into a different call
	//For some builtins we have special support. For the rest we use a default implementation
	std::string duettoBuiltinCreateName;
	if(builtinTypeName=="String" ||
		builtinTypeName=="Callback" || builtinTypeName=="Float32Array")
	{
		duettoBuiltinCreateName=std::string("_duettoCreateBuiltin")+funcName;
	}
	else
		duettoBuiltinCreateName="default_duettoCreateBuiltin_"+builtinTypeName;
	Function* duettoBuiltinCreate=cast<Function>(M.getOrInsertFunction(duettoBuiltinCreateName,
			newFunctionType));
	CallInst* newCall=CallInst::Create(duettoBuiltinCreate,
			initialArgs, "duettoCreateCall", callInst);
	new StoreInst(newCall, newI, callInst);
	return true;
}

void DuettoUtils::rewriteNativeAllocationUsers(Module& M, SmallVector<Instruction*,4>& toRemove,
						Instruction* i, Type* t,
						const std::string& builtinTypeName)
{
	//Instead of allocating the type, allocate a pointer to the type
	AllocaInst* newI=new AllocaInst(PointerType::getUnqual(t),"duettoPtrAlloca",i);
	toRemove.push_back(i);

	Instruction::use_iterator it=i->use_begin();
	Instruction::use_iterator itE=i->use_end();
	SmallVector<User*, 4> users;
	for(;it!=itE;++it)
		users.push_back(it->getUser());
	//Loop over the uses and look for constructors call
	for(unsigned j=0;j<users.size();j++)
	{
		Instruction* userInst = dyn_cast<Instruction>(users[j]);
		if(userInst==NULL)
		{
			baseSubstitutionForBuiltin(users[j], i, newI);
			continue;
		}
		switch(userInst->getOpcode())
		{
			case Instruction::Call:
			{
				CallInst* callInst=static_cast<CallInst*>(userInst);
				//Ignore the last argument, since it's not part of the real ones
				SmallVector<Value*, 4> initialArgs(callInst->op_begin()+1,callInst->op_end()-1);
				bool ret=rewriteIfNativeConstructorCall(M, i, newI, callInst,
									callInst->getCalledFunction(),builtinTypeName,
									initialArgs);
				if(ret)
					toRemove.push_back(callInst);
				else
					baseSubstitutionForBuiltin(callInst, i, newI);
				break;
			}
			case Instruction::Invoke:
			{
				InvokeInst* invokeInst=static_cast<InvokeInst*>(userInst);
				SmallVector<Value*, 4> initialArgs(invokeInst->op_begin()+1,invokeInst->op_end()-3);
				bool ret=rewriteIfNativeConstructorCall(M, i, newI, invokeInst,
									invokeInst->getCalledFunction(),builtinTypeName,
									initialArgs);
				if(ret)
				{
					toRemove.push_back(invokeInst);
					//We need to add a branch to the success label of the invoke call
					BranchInst::Create(invokeInst->getNormalDest(),invokeInst);
				}
				else
					baseSubstitutionForBuiltin(invokeInst, i, newI);
				break;
			}
			default:
			{
				baseSubstitutionForBuiltin(users[j], i, newI);
				break;
			}
		}
	}
}

void DuettoUtils::rewriteNativeObjectsConstructors(Module& M, Function& F)
{
	//Vector of the instructions to be removed in the second pass
	SmallVector<Instruction*, 4> toRemove;

	Function::iterator B=F.begin();
	Function::iterator BE=F.end();
	for(;B!=BE;++B)
	{
		BasicBlock::iterator I=B->begin();
		BasicBlock::iterator IE=B->end();
		for(;I!=IE;++I)
		{
			if(I->getOpcode()==Instruction::Alloca)
			{
				AllocaInst* i=cast<AllocaInst>(&(*I));
				Type* t=i->getAllocatedType();

				std::string builtinTypeName;
				if(!t->isStructTy() || !cast<StructType>(t)->hasName() ||
						!isBuiltinType(t->getStructName().data(), builtinTypeName))
				{
					continue;
				}
				rewriteNativeAllocationUsers(M,toRemove,i,t,builtinTypeName);
			}
			else if(I->getOpcode()==Instruction::Call)
			{
				CallInst* i=cast<CallInst>(&(*I));
				//Check if the function is the C++ new
				Function* called=i->getCalledFunction();
				if(called==NULL)
					continue;
				if(called->getIntrinsicID() != Intrinsic::duetto_allocate)
					continue;
				//This should be a typed new
				Type* t=i->getType()->getPointerElementType();
				std::string builtinTypeName;
				if(!t->isStructTy() || !isBuiltinType(t->getStructName().data(), builtinTypeName))
					continue;
				rewriteNativeAllocationUsers(M,toRemove,i,t,builtinTypeName);
			}
		}
	}

	//Remove the instructions in backward order to avoid dependency issues
	for(int i=toRemove.size();i>0;i--)
		toRemove[i-1]->eraseFromParent();
}
