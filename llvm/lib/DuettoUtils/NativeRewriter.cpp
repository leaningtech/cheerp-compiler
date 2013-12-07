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

#include "llvm/ADT/SmallSet.h"
#include "llvm/Duetto/Utils.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/PassManager.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Utils/Cloning.h"

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

bool DuettoUtils::isBuiltinConstructor(const char* s, const char*& startOfType, const char*& endOfType)
{
	const char* mangledName;
	int mangledNameLen;
	//Extract the class name from the mangled one
	if(findMangledClassName(s, mangledName, mangledNameLen)==false)
		return false;

	startOfType = mangledName;
	endOfType = mangledName+mangledNameLen;

	if(strncmp(mangledName+mangledNameLen, "C1", 2)==0 ||
	   strncmp(mangledName+mangledNameLen, "C2", 2)==0)
		return true;

	return false;
}

bool DuettoUtils::isBuiltinConstructorForType(const char* s, const std::string& typeName)
{
	const char* startOfType;
	const char* endOfType;
	if(!isBuiltinConstructor(s, startOfType, endOfType))
		return false;

	if(typeName.compare(0, std::string::npos, startOfType, endOfType-startOfType)!=0)
		return false;

	return true;
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

Function* DuettoUtils::getReturningConstructor(Module& M, Function* called)
{
	FunctionType* initialType=called->getFunctionType();
	SmallVector<Type*, 4> initialArgsTypes(initialType->param_begin()+1, initialType->param_end());
	FunctionType* newFunctionType=FunctionType::get(*initialType->param_begin(), initialArgsTypes, false);
	return cast<Function>(M.getOrInsertFunction(Twine("duettoCreate",called->getName()).str(),newFunctionType));
}

bool DuettoUtils::rewriteIfNativeConstructorCall(Module& M, Instruction* i, AllocaInst* newI, Instruction* callInst,
						  Function* called, const std::string& builtinTypeName,
						  SmallVector<Value*, 4>& initialArgs)
{
	//Indirect call, ignore it
	if(called==NULL)
		return false;

	//Check if this is a builtin constructor for the type
	const char* funcName=called->getName().data();
	if(!isBuiltinConstructorForType(funcName, builtinTypeName))
		return false;

	//Verify that this contructor is for the current alloca
	if(callInst->getOperand(0)!=i)
		return false;

	//Verify that this one is not already a returning construtor
	if(!callInst->getType()->isVoidTy())
		return false;

	Function* newFunc = getReturningConstructor(M, called);
	CallInst* newCall=CallInst::Create(newFunc, initialArgs, "retConstructor", callInst);
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

void DuettoUtils::rewriteConstructorImplementation(Module& M, Function& F)
{
	//Visit each instruction and take note of the ones that needs to be replaced
	Function::iterator B=F.begin();
	Function::iterator BE=F.end();
	ValueToValueMapTy valueMap;
	CallInst* lowerConstructor = NULL;
	CallInst* oldLowerConstructor = NULL;
	for(;B!=BE;++B)
	{
		BasicBlock::iterator I=B->begin();
		BasicBlock::iterator IE=B->end();
		for(;I!=IE;++I)
		{
			switch(I->getOpcode())
			{
				case Instruction::Call:
				{
					CallInst* callInst=static_cast<CallInst*>(&(*I));
					if(Function* f=callInst->getCalledFunction())
					{
						const char* startOfType;
						const char* endOfType;
						if(!DuettoUtils::isBuiltinConstructor(f->getName().data(),
									startOfType, endOfType))
						{
							continue;
						}
						//Check that the constructor is for 'this'
						if(callInst->getOperand(0)!=F.arg_begin())
							continue;
						//Only one lower constructor can be allowed
						if(lowerConstructor)
						{
							llvm::report_fatal_error("Only one base constructor is supported",
									false);
							return;
						}
						//If this is another constructor for the same type, change it to a
						//returning constructor and use it as the 'this' argument
						Function* newFunc = getReturningConstructor(M, f);
						llvm::SmallVector<Value*, 4> newArgs;
						for(unsigned i=1;i<callInst->getNumArgOperands();i++)
						{
							newArgs.push_back(callInst->getArgOperand(i));
						}
						lowerConstructor = CallInst::Create(newFunc, newArgs);
						//Save which call is the constructror, we need to remove it after this loop
						oldLowerConstructor = callInst;
					}
					break;
				}
			}
		}
	}
	//Kill the old base construtor now
	oldLowerConstructor->eraseFromParent();
	valueMap.insert(make_pair(F.arg_begin(), lowerConstructor));

	//Copy the simplified code in a function with the right signature
	Function* newFunc=getReturningConstructor(M, &F);
	Function::arg_iterator origArg=++F.arg_begin();
	Function::arg_iterator newArg=newFunc->arg_begin();
	for(unsigned i=1;i<F.arg_size();i++)
	{
		valueMap.insert(make_pair(&(*origArg), &(*newArg)));
		++origArg;
		++newArg;
	}
	SmallVector<ReturnInst*, 4> returns;
	CloneFunctionInto(newFunc, &F, valueMap, false, returns);

	//Find the right place to add the base construtor call
	assert(lowerConstructor->getNumArgOperands()<=1 && "Native constructors with multiple args are not supported");
	Instruction* callPred = NULL;
	if (lowerConstructor->getNumArgOperands()==1 && Instruction::classof(lowerConstructor->getArgOperand(0)))
	{
		//Switch the argument to the one in the new func
		lowerConstructor->setArgOperand(0, valueMap[lowerConstructor->getArgOperand(0)]);
		callPred = cast<Instruction>(lowerConstructor->getArgOperand(0));
	}
	else
		callPred = &newFunc->getEntryBlock().front();

	//Add add it
	lowerConstructor->insertAfter(callPred);

	//Override the returs values
	for(unsigned i=0;i<returns.size();i++)
	{
		Instruction* newInst = ReturnInst::Create(M.getContext(),lowerConstructor);
		newInst->insertBefore(returns[i]);
		returns[i]->removeFromParent();
	}
	//Recursively move all the users of the lower constructor after the call itself
	Instruction* insertPoint = lowerConstructor->getNextNode();
	SmallVector<Value*, 4> usersQueue(lowerConstructor->getNumUses());
	unsigned int i;
	Value::use_iterator it;
	for(i=usersQueue.size()-1,it=lowerConstructor->use_begin();it!=lowerConstructor->use_end();++it,i--)
		usersQueue[i]=it->getUser();

	SmallSet<Instruction*, 4> movedInstructions;
	while(!usersQueue.empty())
	{
		Instruction* cur=dyn_cast<Instruction>(usersQueue.pop_back_val());
		if(!cur)
			continue;
		if(movedInstructions.count(cur))
			continue;
		movedInstructions.insert(cur);
		cur->moveBefore(insertPoint);
		//Add users of this instrucution as well
		usersQueue.resize(usersQueue.size()+cur->getNumUses());
		for(i=usersQueue.size()-1,it=cur->use_begin();it!=cur->use_end();++it,i--)
			usersQueue[i]=it->getUser();
	}
}

void DuettoUtils::rewriteNativeObjectsConstructors(Module& M, Function& F)
{
	const char* startOfType;
	const char* endOfType;
	if(isBuiltinConstructor(F.getName().data(), startOfType, endOfType) &&
		F.getReturnType()->isVoidTy())
	{
		assert(!F.empty());
		rewriteConstructorImplementation(M, F);
		return;
	}
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
