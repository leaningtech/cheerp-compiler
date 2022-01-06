//===-- InvokeWrapping.cpp - Cheerp backend pass --------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2021 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/InvokeWrapping.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/CodeGen/Analysis.h"

namespace cheerp {

using namespace llvm;

static CallInst* replaceInvokeWithWrapper(InvokeInst* IV, Function* Wrapper, ArrayRef<Value*> extraArgs)
{
	SmallVector<Value *, 16> CallArgs(extraArgs.begin(), extraArgs.end());
	CallArgs.append(IV->arg_begin(), IV->arg_end());
	SmallVector<OperandBundleDef, 1> OpBundles;
	IV->getOperandBundlesAsDefs(OpBundles);
	// Insert a normal call instruction...
	CallInst *NewCall = CallInst::Create(Wrapper->getFunctionType(), Wrapper, CallArgs, OpBundles, "", IV);
	NewCall->takeName(IV);
	NewCall->setCallingConv(IV->getCallingConv());
	NewCall->setAttributes(IV->getAttributes());
	NewCall->setDebugLoc(IV->getDebugLoc());
	IV->replaceAllUsesWith(NewCall);
	return NewCall;
}

static GlobalVariable* getOrInsertHelperGlobal(Module& M)
{
    auto* Ty = llvm::StructType::getTypeByName(M.getContext(), "struct._ZN10__cxxabiv119__cheerp_landingpadE");
	assert(Ty);
	GlobalVariable* G = cast<GlobalVariable>(M.getOrInsertGlobal("__cheerpExceptionHelperGlobal", Ty->getPointerTo(), [&M, Ty]()
	{
		auto* g = new GlobalVariable(M, Ty->getPointerTo(), false, GlobalVariable::ExternalLinkage, ConstantPointerNull::get(Ty->getPointerTo()));
		g->setName("__cheerpExceptionHelperGlobal");
		g->setLinkage(GlobalVariable::ExternalLinkage);
		if (Ty->hasAsmJS())
			g->setSection("asmjs");
		return g;
	}));
	return G;
}

int LandingPadTable::LocalTypeIdMap::getTypeIdFor(Value* V)
{
	GlobalValue* GV = llvm::ExtractTypeInfo(V);
	if(GV == nullptr)
	{
		return 0;
	}
	auto it = typeIdMap.find(GV);
	if (it != typeIdMap.end())
	{
		return it->second;
	}
	int id = typeIdMap.size()+1;
	typeIdMap.insert(std::make_pair(GV, id));
	return id;
}
int LandingPadTable::LocalTypeIdMap::getTypeIdFor(Value* V) const
{
	GlobalValue* GV = llvm::ExtractTypeInfo(V);
	if(GV == nullptr)
	{
		return 0;
	}
	auto it = typeIdMap.find(GV);
	assert(it != typeIdMap.end());
	return it->second;
}

void LandingPadTable::populate(Module& M)
{
	Type* Int32Ty = IntegerType::get(M.getContext(), 32);
	StructType* elemTy = nullptr;
	table = nullptr;
	// Initialize these variables lazily only if we encounter landing pads
	auto initData = [this, &elemTy, &M]()
	{
		if (elemTy)
			return;
		elemTy = StructType::getTypeByName(M.getContext(), "struct._ZN10__cxxabiv115__cheerp_clauseE");
		assert(elemTy && "missing __cheerp_clause type");
		table = M.getGlobalVariable("__cxa_cheerp_clause_table");
		assert(table && "missing __cxa_cheerp_clause_table global");
		assert(!table->hasInitializer() && "__cxa_cheerp_clause_table alread initialized");
	};

	std::vector<Constant*> v;
	for (Function& F: M.functions())
	{
		auto& typeIdMap = getLocalTypeIdMap(&F);
		for (Instruction& I: instructions(F))
		{
			if (!isa<LandingPadInst>(I))
				continue;
			initData();
			auto& LP = cast<LandingPadInst>(I);
			Constant* start = ConstantInt::get(Int32Ty, v.size());
			Constant* n = ConstantInt::get(Int32Ty, LP.getNumClauses());
			entries.insert(std::make_pair(&LP, Entry{start, n}));
			for(unsigned i = 0; i < LP.getNumClauses(); i++)
			{
				Constant* Clause = LP.getClause(i);
				PointerType* InfoTy = cast<PointerType>(elemTy->getElementType(0));
				int id = typeIdMap.getTypeIdFor(Clause);
				Clause = isa<ConstantPointerNull>(Clause)
					? ConstantPointerNull::get(InfoTy)
					: Clause;
				SmallVector<Constant*, 2> fields {
					Clause,
					ConstantInt::get(Int32Ty, id)
				};
				Constant* el = ConstantStruct::get(elemTy, fields);
				v.push_back(el);
			}
		}
	}
	// If we found no landing pads, there is nothing to do
	if(elemTy == nullptr)
		return;
	Constant* init = ConstantArray::get(ArrayType::get(elemTy, v.size()), v);
	table->setValueType(init->getType());
	table->mutateType(init->getType()->getPointerTo());
	table->setInitializer(init);
	if (elemTy->hasAsmJS())
		table->setSection("asmjs");
	// Move the table to the end of the globals. Since GDA already ran we can't deal
	// with the fact that the RTTI globals referenced here may be not yet defined.
	// Also, it is a waste do do the fixups when we can just render this last.
	table->removeFromParent();
	M.getGlobalList().push_back(table);
}


static Function* getInvokeWrapper(Module& M, Function* F, Constant* PersonalityFn, Type* LPadTy, LandingPadTable& table)
{
	Type* Int32Ty = IntegerType::get(M.getContext(), 32);
	FunctionType* OldTy = F->getFunctionType();
	SmallVector<Type*, 4> ParamTypes;
	// Start
	ParamTypes.push_back(Int32Ty);
	// N
	ParamTypes.push_back(Int32Ty);
	for (auto* paramTy: OldTy->params())
	{
		ParamTypes.push_back(paramTy);
	}
	FunctionType* Ty = FunctionType::get(OldTy->getReturnType(), ParamTypes, OldTy->isVarArg());
	std::string fname = "__invoke_wrapper__";
	fname += F->getName();
	Function* Wrapper = cast<Function>(M.getOrInsertFunction(fname, Ty).getCallee());
	if (!Wrapper->empty())
		return Wrapper;
	setForceRawAttribute(M, Wrapper);

	Wrapper->setPersonalityFn(PersonalityFn);
	BasicBlock* Entry = BasicBlock::Create(M.getContext(),"entry", Wrapper);
	BasicBlock* Cont = BasicBlock::Create(M.getContext(),"cont", Wrapper);
	BasicBlock* Catch = BasicBlock::Create(M.getContext(),"catch", Wrapper);
	IRBuilder<> Builder(Entry);

	SmallVector<Value*, 4> params;
	for(auto& arg: make_range(Wrapper->arg_begin()+2, Wrapper->arg_end()))
		params.push_back(&arg);
	InvokeInst* ForwardInvoke = Builder.CreateInvoke(F, Cont, Catch, params);

	GlobalVariable* Helper = getOrInsertHelperGlobal(M);

	Builder.SetInsertPoint(Cont);
	Value* Ret = ForwardInvoke->getType()->isVoidTy() ? nullptr : ForwardInvoke;
	Builder.CreateStore(ConstantPointerNull::get(cast<PointerType>(Helper->getType()->getPointerElementType())), Helper);
	Builder.CreateRet(Ret);

	Builder.SetInsertPoint(Catch);
	Value* Start = Wrapper->getArg(0);
	Value* N = Wrapper->getArg(1);
	LandingPadInst* LP = Builder.CreateLandingPad(LPadTy, 0);
	LP->setCleanup(true);
	table.addEntry(LP, LandingPadTable::Entry { Start, N });
	Builder.CreateStore(LP, Helper);
	Ret = ForwardInvoke->getType()->isVoidTy() ? nullptr : UndefValue::get(ForwardInvoke->getType());
	Builder.CreateRet(Ret);

	return Wrapper;
}

using IndirectStubMap = DenseMap<FunctionType*, Function*>;
static InvokeInst* replaceIndirectInvokeWithStub(Module& M, InvokeInst* IV, IndirectStubMap& stubs)
{
	Type* Int32Ty = IntegerType::get(M.getContext(), 32);
	FunctionType* OldTy = IV->getFunctionType();
	auto it = stubs.find(OldTy);
	if (it == stubs.end())
	{
		SmallVector<Type*, 4> ParamTypes;
		// TableIdx
		ParamTypes.push_back(Int32Ty);
		for (auto* paramTy: OldTy->params())
		{
			ParamTypes.push_back(paramTy);
		}
		std::string fname = "__indirect_invoke_stub_";
		fname += std::to_string(stubs.size()) + "__";
		FunctionType* StubTy = FunctionType::get(OldTy->getReturnType(), ParamTypes, OldTy->isVarArg());
		Function* Stub = cast<Function>(M.getOrInsertFunction(fname, StubTy).getCallee());
		assert(Stub->empty());
		Stub->setSection("asmjs");

		BasicBlock* Entry = BasicBlock::Create(M.getContext(),"entry", Stub);
		IRBuilder<> Builder(Entry);

		SmallVector<Value*, 4> params;
		for(auto& arg: make_range(Stub->arg_begin()+1, Stub->arg_end()))
			params.push_back(&arg);
		Value* TableIdx = Stub->getArg(0);
		Value* Called = Builder.CreateIntToPtr(TableIdx, OldTy->getPointerTo());
		Value* Call = Builder.CreateCall(OldTy, Called, params);
		Value* Ret = Call->getType()->isVoidTy() ? nullptr : Call;
		Builder.CreateRet(Ret);

		it = stubs.insert(std::make_pair(OldTy, Stub)).first;
	}
	Function* Stub = it->getSecond();
	IRBuilder<> Builder(IV);
	Value* TableIdx = Builder.CreatePtrToInt(IV->getCalledOperand(), Int32Ty);
	SmallVector<Value*, 4> Args;
	Args.push_back(TableIdx);
	Args.append(IV->arg_begin(), IV->arg_end());
	InvokeInst* StubIV = Builder.CreateInvoke(Stub->getFunctionType(), Stub, IV->getNormalDest(), IV->getUnwindDest(), Args);
	IV->replaceAllUsesWith(StubIV);
	IV->eraseFromParent();
	return StubIV;
}

static Function* wrapInvoke(Module& M, InvokeInst& IV, DenseSet<Instruction*>& ToRemove, LandingPadTable& table)
{
	Constant* PersonalityFn = IV.getParent()->getParent()->getPersonalityFn();
	LandingPadInst* OldLP = IV.getUnwindDest()->getLandingPadInst();
	ToRemove.insert(OldLP);
	Type* LPadTy = OldLP->getType();
	Function* F = IV.getCalledFunction();
	assert(F);
	Function* Wrapper = getInvokeWrapper(M, F, PersonalityFn, LPadTy, table);

	IRBuilder<> Builder(&IV);
	LandingPadTable::Entry e = table.getEntry(OldLP);
	SmallVector<Value*, 2> extraArgs = {
		e.start,
		e.n,
	};
	replaceInvokeWithWrapper(&IV, Wrapper, extraArgs);

	GlobalVariable* Helper = getOrInsertHelperGlobal(M);
	Value* Ex = Builder.CreateLoad(Helper->getType()->getPointerElementType(), Helper);
	Value* Cond = Builder.CreateICmpEQ(Ex, ConstantPointerNull::get(cast<PointerType>(Ex->getType())));
	Builder.CreateCondBr(Cond, IV.getNormalDest(), IV.getUnwindDest());

	IV.eraseFromParent();

	return Wrapper;
}

static Function* wrapResume(Module& M, ResumeInst* RS)
{
	Function* CxaResume = M.getFunction("__cxa_resume");
	assert(CxaResume);
	IRBuilder<> Builder(RS);
	Value* LP = RS->getOperand(0);
	if (LP->getType() != CxaResume->getFunctionType()->getParamType(0))
	{
		LP = Builder.CreateBitCast(LP, CxaResume->getFunctionType()->getParamType(0));
	}
	Value* Call = Builder.CreateCall(CxaResume->getFunctionType(), CxaResume, LP);
	RS->replaceAllUsesWith(Call);
	Builder.CreateUnreachable();
	RS->eraseFromParent();
	return CxaResume;
}


bool InvokeWrapping::runOnModule(Module& M)
{
	auto& GDA = getAnalysis<cheerp::GlobalDepsAnalyzer>();
	bool Changed = false;

	table.populate(M);

	IndirectStubMap stubs;
	DenseSet<Instruction*> OldLPs;
	for (Function& F: make_early_inc_range(M.functions()))
	{
		bool asmjs = F.getSection() == "asmjs";
		for (auto& I: make_early_inc_range(instructions(F)))
		{
			if (auto* IV = dyn_cast<InvokeInst>(&I))
			{
				if (!asmjs)
					continue;
				Changed = true;
				if (IV->isIndirectCall())
					IV = replaceIndirectInvokeWithStub(M, IV, stubs);
				bool asmjsCallee = IV->getCalledFunction()->getSection() == "asmjs";
				Function* W = wrapInvoke(M, *IV, OldLPs, table);
				if (asmjsCallee)
				{
					GDA.insertAsmJSExport(IV->getCalledFunction());
				}
				GDA.insertAsmJSImport(W);
			} else if(auto* RS = dyn_cast<ResumeInst>(&I)) {
				Changed = true;
				Function* W = wrapResume(M, RS);
				if (asmjs)
					GDA.insertAsmJSImport(W);
			}
		}
	}
	for (auto* OldLP: OldLPs)
	{
		IRBuilder<> Builder(OldLP);
		GlobalVariable* Helper = getOrInsertHelperGlobal(M);
		Value* Ex = Builder.CreateLoad(Helper->getType()->getPointerElementType(), Helper);
		OldLP->replaceAllUsesWith(Ex);
		OldLP->eraseFromParent();
	}
	return Changed;
}

void InvokeWrapping::getAnalysisUsage(llvm::AnalysisUsage & AU) const
{
	AU.addRequired<cheerp::GlobalDepsAnalyzer>();
	AU.addPreserved<cheerp::GlobalDepsAnalyzer>();
	llvm::Pass::getAnalysisUsage(AU);
}

char InvokeWrapping::ID = 0;

ModulePass *createInvokeWrappingPass() { return new InvokeWrapping(); }
}
