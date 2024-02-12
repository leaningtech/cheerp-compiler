//===-- NativeRewriter.cpp - Cheerp helper --------------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2011-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/SmallSet.h"
#include "llvm/Cheerp/Demangler.h"
#include "llvm/Cheerp/NativeRewriter.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/PromoteMemToReg.h"

using namespace llvm;
using namespace std;

static bool isNamespaceClient(const char* s)
{
	if(strncmp(s,"_ZN",3)!=0)
		return false;
	const char* tmp=s+3;
	char* endPtr;
	int nsLen=strtol(tmp, &endPtr, 10);
	tmp=endPtr;
	if(nsLen==0 || (strncmp(tmp,"client",nsLen)!=0))
		return false;

	return true;
}

bool CheerpNativeRewriterPass::findMangledClassName(const char* s, std::string& name)
{
	if (!isNamespaceClient(s))
		return false;

	name = getClassName(s);

	return true;
}

bool CheerpNativeRewriterPass::isBuiltinConstructor(const char* s)
{
	if (!isNamespaceClient(s))
		return false;

	cheerp::Demangler demangler(s);

	assert(demangler.isNamespaceClient());

	return (demangler.isMangled() &&
			demangler.isFunction() &&
			demangler.isConstructor());
}

std::string CheerpNativeRewriterPass::getClassName(const char* s)
{
	cheerp::Demangler demangler(s);

	assert(isNamespaceClient(s));

	return demangler.getJSMangling(/*doCleanup*/false);
}

bool CheerpNativeRewriterPass::isBuiltinConstructorForType(const char* s, const std::string& typeName)
{
	if(!isBuiltinConstructor(s))
		return false;

	if(typeName != getClassName(s))
		return false;

	return true;
}

void CheerpNativeRewriterPass::baseSubstitutionForBuiltin(User* i, Instruction* old, AllocaInst* source)
{
	Instruction* userInst=dyn_cast<Instruction>(i);
	assert(userInst);
	Instruction* insertPoint=userInst;
	// We need to insert the new instruction in the right block for PHIs
	if(PHINode* phi=dyn_cast<PHINode>(userInst))
	{
		for(uint32_t i=0;i<phi->getNumIncomingValues();i++)
		{
			if(phi->getIncomingValue(i)==old)
			{
				insertPoint=phi->getIncomingBlock(i)->getTerminator();
				LoadInst* loadI=new LoadInst(source->getAllocatedType(), source, "cheerpPtrLoad", insertPoint);
				phi->setIncomingValue(i, loadI);
				return;
			}
		}
		assert(false);
		return;
	}
	// if the previous instruction is an invoke ,then it must mean that this user
	// is an old constructor that we are replacing, since invokes are terminators.
	// Don't add a load here because it would break the IR, and it is useless anyway.
	// Just put an undef, then later we are going to remove the user entirely.
	if(userInst->getPrevNode() && isa<InvokeInst>(userInst->getPrevNode()))
	{
		userInst->replaceUsesOfWith(old, UndefValue::get(old->getType()));
		return;
	}
	LoadInst* loadI=new LoadInst(source->getAllocatedType(), source, "cheerpPtrLoad", insertPoint);
	userInst->replaceUsesOfWith(old, loadI);
}

/*
 * Check if a type is builtin and return the type name
 */
bool CheerpNativeRewriterPass::isBuiltinType(const char* typeName, std::string& builtinName)
{
	if(strncmp(typeName, "class.", 6)==0)
	{
		typeName+=6;
	}
	else if(strncmp(typeName, "struct.", 7)==0)
	{
		typeName+=7;
	}
	else
		return false;

	std::string name;
	if(findMangledClassName(typeName, name)==false)
		return false;

	builtinName = name;
	return true;
}

Function* CheerpNativeRewriterPass::getReturningConstructor(Module& M, Function* called)
{
	FunctionType* initialType=called->getFunctionType();
	SmallVector<Type*, 4> initialArgsTypes(initialType->param_begin()+1, initialType->param_end());
	FunctionType* newFunctionType=FunctionType::get(*initialType->param_begin(), initialArgsTypes, false);
	return cast<Function>(M.getOrInsertFunction(Twine("cheerpCreate",called->getName()).str(),newFunctionType).getCallee());
}

static bool redundantStringConstructor(Instruction* ci, SmallVectorImpl<Value*>& initialArgs)
{
	if (initialArgs.size() != 1)
		return false;
	Type* ty = ci->getOperand(0)->getType();
	Type* argTy = initialArgs[0]->getType();
	if (!argTy->isPointerTy() || !argTy->getPointerElementType()->isStructTy())
		return false;
	if (ty == argTy && ty->getPointerElementType()->getStructName() == StringRef("class._ZN6client6StringE"))
		return true;
	return false;
}

bool CheerpNativeRewriterPass::rewriteIfNativeConstructorCall(Module& M, Instruction* i, AllocaInst* newI, Instruction* callInst,
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

	//We optimize the special case of String(String), removing the constructor
	//call entirely
	if (redundantStringConstructor(callInst, initialArgs))
	{
		new StoreInst(initialArgs[0], newI, callInst);
		return true;
	}
	Function* newFunc = getReturningConstructor(M, called);
	CallBase* newCall = nullptr;
	Instruction* InsertPt = nullptr;
	if (InvokeInst* inv = dyn_cast<InvokeInst>(callInst))
	{
		BasicBlock* nextBB = BasicBlock::Create(M.getContext(), "invokeCont", inv->getParent()->getParent(), inv->getNormalDest());
		newCall=InvokeInst::Create(newFunc, nextBB, inv->getUnwindDest(), initialArgs, "retConstructor", callInst);
		InsertPt = BranchInst::Create(inv->getNormalDest(), nextBB);
	}
	else
	{
		newCall=CallInst::Create(newFunc, initialArgs, "retConstructor", callInst);
		InsertPt = callInst;
	}
	new StoreInst(newCall, newI, InsertPt);
	newCall->setDebugLoc(callInst->getDebugLoc());
	return true;
}

void CheerpNativeRewriterPass::rewriteNativeAllocationUsers(Module& M, SmallVector<Instruction*,4>& toRemove,
						Instruction* i, Type* t,
						const std::string& builtinTypeName)
{
	//Instead of allocating the type, allocate a pointer to the type
	AllocaInst* newI=new AllocaInst(PointerType::getUnqual(t),0,"cheerpPtrAlloca",
		&i->getParent()->getParent()->front().front());
	bool foundConstructor = false;

	Instruction::use_iterator it=i->use_begin();
	Instruction::use_iterator itE=i->use_end();
	SmallVector<User*, 4> users;
	for(;it!=itE;++it)
		users.push_back(it->getUser());
	//Loop over the uses and look for constructors call
	for(unsigned j=0;j<users.size();j++)
	{
		if(CallBase* cb = dyn_cast<CallBase>(users[j]))
		{
			SmallVector<Value*, 4> initialArgs(cb->arg_begin()+1,cb->arg_end());
			bool ret=rewriteIfNativeConstructorCall(M, i, newI, cb,
								cb->getCalledFunction(),builtinTypeName,
								initialArgs);
			if(ret)
			{
				if(!foundConstructor)
				{
					toRemove.push_back(i);
					foundConstructor = true;
				}
				toRemove.push_back(cb);
				continue;
			}
		}
		baseSubstitutionForBuiltin(users[j], i, newI);
	}
	if(!foundConstructor)
		new StoreInst(i, newI, i->getNextNode());
}

void CheerpNativeRewriterPass::rewriteConstructorImplementation(Module& M, Function& F, DominatorTree& DT)
{
	//Copy the code in a function with the right signature
	Function* newFunc=getReturningConstructor(M, &F);
	if(!newFunc->empty())
		return;

	//Visit each instruction and take note of the ones that needs to be replaced
	ValueToValueMapTy valueMap;
	CallInst* lowerConstructor = NULL;
	BitCastInst* lowerCast = NULL;
	const CallInst* oldLowerConstructor = NULL;
	for(auto& BB: F)
	{
		SmallVector<AllocaInst*, 8> allocasToPromote;
		for(auto& I: BB)
		{
			if(I.getOpcode() != Instruction::Alloca)
				continue;
			// Remove allocas, to be able to track how values are used
			AllocaInst* AI = cast<AllocaInst>(&I);
			if(!isAllocaPromotable(AI))
				continue;
			allocasToPromote.push_back(AI);
		}
		if(!allocasToPromote.empty())
		{
			PromoteMemToReg(allocasToPromote, DT);
		}
		for(const auto& I: BB)
		{
			if(I.getOpcode()!=Instruction::Call)
				continue;
			const CallInst& callInst=cast<CallInst>(I);
			Function* f=callInst.getCalledFunction();
			if(!f)
				continue;
			if(!CheerpNativeRewriterPass::isBuiltinConstructor(f->getName().data()))
				continue;
			//Check that the constructor is for 'this'
			Value* firstArg = callInst.getOperand(0);
			while(cheerp::isBitCast(firstArg))
			{
				valueMap.insert(make_pair(firstArg, UndefValue::get(firstArg->getType())));
				firstArg = cast<User>(firstArg)->getOperand(0);
			}
			if(firstArg!=&*F.arg_begin())
				continue;
			//If this is another constructor for the same type, change it to a
			//returning constructor and use it as the 'this' argument
			Function* newFunc = getReturningConstructor(M, f);
			llvm::SmallVector<Value*, 4> newArgs;
			for(auto& arg: make_range(callInst.arg_begin()+1, callInst.arg_end()))
				newArgs.push_back(arg.get());
			lowerConstructor = CallInst::Create(newFunc, newArgs);
			oldLowerConstructor = &callInst;
			break;
		}
		if(lowerConstructor)
			break;
	}

	//Clone the linkage first
	newFunc->setLinkage(F.getLinkage());
	Function::arg_iterator origArg=F.arg_begin() + 1;
	Function::arg_iterator newArg=newFunc->arg_begin();
	if (!lowerConstructor)
	{
		std::string diag = "No native constructor found for class ";
		diag.append(getClassName(F.getName().data()));
		llvm::report_fatal_error(StringRef(diag), false);
	}
	if(lowerConstructor->getType() != F.arg_begin()->getType())
	{
		lowerCast = new BitCastInst( lowerConstructor, F.arg_begin()->getType());
		valueMap.insert(make_pair(&*F.arg_begin(), lowerCast));
	}
	else
		valueMap.insert(make_pair(&*F.arg_begin(), lowerConstructor));

	for(unsigned i=1;i<F.arg_size();i++)
	{
		valueMap.insert(make_pair(&(*origArg), &(*newArg)));
		++origArg;
		++newArg;
	}
	SmallVector<ReturnInst*, 4> returns;
	CloneFunctionInto(newFunc, &F, valueMap, CloneFunctionChangeType::LocalChangesOnly, returns);

	//Find the right place to add the base construtor call
	if (lowerConstructor->arg_size()>1)
		llvm::report_fatal_error("Native constructors with multiple args are not supported", false);
	Instruction* callPred = &newFunc->getEntryBlock().front();
	if (lowerConstructor->arg_size()==1)
	{
		//Switch the argument to the one in the new func
		lowerConstructor->setArgOperand(0, valueMap[lowerConstructor->getArgOperand(0)]);
		if (Instruction::classof(lowerConstructor->getArgOperand(0)))
			callPred = cast<Instruction>(lowerConstructor->getArgOperand(0));
	}

	//And add it
	lowerConstructor->insertAfter(callPred);
	if(lowerCast)
		lowerCast->insertAfter(lowerConstructor);

	//Override the return values
	for(unsigned i=0;i<returns.size();i++)
	{
		Instruction* newInst = ReturnInst::Create(M.getContext(), lowerCast ? (Instruction*)lowerCast : (Instruction*)lowerConstructor);
		newInst->insertBefore(returns[i]);
		returns[i]->eraseFromParent();
	}
	returns.clear();
	//Recursively move all the users of the lower constructor after the call itself
	Instruction* reorderStart = lowerCast ? (Instruction*)lowerCast : (Instruction*)lowerConstructor;
	Instruction* insertPoint = reorderStart->getNextNode();
	SmallVector<Value*, 4> usersQueue(reorderStart->getNumUses());
	unsigned int i;
	Value::use_iterator it;
	for(i=usersQueue.size()-1,it=reorderStart->use_begin();it!=reorderStart->use_end();++it,i--)
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
		// Terminators should not move from their position
		if(cur->isTerminator())
			continue;
		cur->moveBefore(insertPoint);
		//Add users of this instrucution as well
		usersQueue.resize(usersQueue.size()+cur->getNumUses());
		for(i=usersQueue.size()-1,it=cur->use_begin();it!=cur->use_end();++it,i--)
			usersQueue[i]=it->getUser();
	}
	cast<Instruction>(valueMap[oldLowerConstructor])->eraseFromParent();
}

bool CheerpNativeRewriterPass::rewriteNativeObjectsConstructors(Module& M, Function& F, DominatorTree& DT)
{
	//Vector of the instructions to be removed in the second pass
	SmallVector<Instruction*, 4> toRemove;

	bool Changed = false;
	for(auto& BB: F)
	{
		for(auto& I: BB)
		{
			if(isa<AllocaInst>(I))
			{
				AllocaInst& i=cast<AllocaInst>(I);
				Type* t=i.getAllocatedType();

				std::string builtinTypeName;
				if(!t->isStructTy() || !cast<StructType>(t)->hasName() ||
						!isBuiltinType(t->getStructName().data(), builtinTypeName))
				{
					continue;
				}
				rewriteNativeAllocationUsers(M,toRemove,&i,t,builtinTypeName);
				Changed = true;
			}
			else if(isa<CallBase>(I))
			{
				CallBase& i=cast<CallBase>(I);
				//Check if the function is the C++ new
				Function* called=i.getCalledFunction();
				if(called==NULL)
					continue;
				if(called->getIntrinsicID() != Intrinsic::cheerp_allocate &&
				   called->getIntrinsicID() != Intrinsic::cheerp_allocate_array)
					continue;
				//This should be a typed new
				Type* t=i.getParamElementType(0);
				assert(t);
				std::string builtinTypeName;
				if(!t->isStructTy() || !isBuiltinType(t->getStructName().data(), builtinTypeName))
					continue;
				rewriteNativeAllocationUsers(M,toRemove,&i,t,builtinTypeName);
				Changed = true;
			}
		}
	}

	//Remove the instructions in backward order to avoid dependency issues
	for(int i=toRemove.size();i>0;i--)
		toRemove[i-1]->eraseFromParent();

	if(isBuiltinConstructor(F.getName().data()) &&
		F.getReturnType()->isVoidTy())
	{
		assert(!F.empty());
		rewriteConstructorImplementation(M, F, DT);
		return true;
	}

	return Changed;
}

bool CheerpNativeRewriterPass::runOnFunction(Function& F, DominatorTree& DT)
{
	rewriteNativeObjectsConstructors(*F.getParent(), F, DT);
	return true;
}

PreservedAnalyses CheerpNativeRewriterPass::run(Function& F, FunctionAnalysisManager& AM)
{
  auto &DT = AM.getResult<DominatorTreeAnalysis>(F);
  if (!runOnFunction(F, DT))
    return PreservedAnalyses::all();
  return PreservedAnalyses::none();
}
