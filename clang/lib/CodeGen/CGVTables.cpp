//===--- CGVTables.cpp - Emit LLVM Code for C++ vtables -------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This contains code dealing with C++ code generation of virtual tables.
//
//===----------------------------------------------------------------------===//

#include "CGCXXABI.h"
#include "CodeGenFunction.h"
#include "CodeGenModule.h"
#include "clang/AST/Attr.h"
#include "clang/AST/CXXInheritance.h"
#include "clang/AST/RecordLayout.h"
#include "clang/Basic/CodeGenOptions.h"
#include "clang/CodeGen/CGFunctionInfo.h"
#include "clang/CodeGen/ConstantInitBuilder.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/Support/Format.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include <algorithm>
#include <cstdio>

using namespace clang;
using namespace CodeGen;

CodeGenVTables::CodeGenVTables(CodeGenModule &CGM)
    : CGM(CGM), VTContext(CGM.getContext().getVTableContext()) {}

llvm::Constant *CodeGenModule::GetAddrOfThunk(StringRef Name, llvm::Type *FnTy,
                                              GlobalDecl GD) {
  return GetOrCreateLLVMFunction(Name, FnTy, GD, /*ForVTable=*/true,
                                 /*DontDefer=*/true, /*IsThunk=*/true);
}

static unsigned ComputeTopologicalBaseOffset(CodeGenModule &CGM,
                                          const CXXRecordDecl* AdjustmentTarget,
                                          const CXXBasePath& AdjustmentPath) {
  const CXXBasePath& p=AdjustmentPath;
  llvm::SmallVector<const CXXBaseSpecifier*, 4> path;
  for(unsigned i=0;i<p.size();i++)
	path.push_back(p[i].Base);

  return CGM.ComputeBaseIdOffset(AdjustmentTarget, path);
}

static unsigned ComputeTopologicalBaseOffset(CodeGenModule &CGM,
                                          const CXXRecordDecl* AdjustmentTarget,
					  const CXXRecordDecl* AdjustmentSource) {
  CXXBasePaths Paths(/*FindAmbiguities=*/true, /*RecordPaths=*/true,
                     /*DetectVirtual=*/false);
#ifndef NDEBUG
  bool isDerived=
#endif
  AdjustmentTarget->isDerivedFrom(AdjustmentSource, Paths);
  assert(isDerived);

  CXXBasePaths::const_paths_iterator it=Paths.begin();
  const CXXBasePath& p=*it;
  assert((++it)==Paths.end());
  return ComputeTopologicalBaseOffset(CGM, AdjustmentTarget, p);
}

static void setThunkProperties(CodeGenModule &CGM, const ThunkInfo &Thunk,
                               llvm::Function *ThunkFn, bool ForVTable,
                               GlobalDecl GD) {
  CGM.setFunctionLinkage(GD, ThunkFn);
  CGM.getCXXABI().setThunkLinkage(ThunkFn, ForVTable, GD,
                                  !Thunk.Return.isEmpty());

  // Set the right visibility.
  CGM.setGVProperties(ThunkFn, GD);

  if (!CGM.getCXXABI().exportThunk()) {
    ThunkFn->setDLLStorageClass(llvm::GlobalValue::DefaultStorageClass);
    ThunkFn->setDSOLocal(true);
  }

  if (CGM.supportsCOMDAT() && ThunkFn->isWeakForLinker())
    ThunkFn->setComdat(CGM.getModule().getOrInsertComdat(ThunkFn->getName()));
}

#ifndef NDEBUG
static bool similar(const ABIArgInfo &infoL, CanQualType typeL,
                    const ABIArgInfo &infoR, CanQualType typeR) {
  return (infoL.getKind() == infoR.getKind() &&
          (typeL == typeR ||
           (isa<PointerType>(typeL) && isa<PointerType>(typeR)) ||
           (isa<ReferenceType>(typeL) && isa<ReferenceType>(typeR))));
}
#endif

static RValue PerformReturnAdjustment(CodeGenFunction &CGF,
                                      QualType ResultType, RValue RV,
                                      const ThunkInfo &Thunk) {
  // Emit the return adjustment.
  bool NullCheckValue = !ResultType->isReferenceType();

  llvm::BasicBlock *AdjustNull = nullptr;
  llvm::BasicBlock *AdjustNotNull = nullptr;
  llvm::BasicBlock *AdjustEnd = nullptr;

  llvm::Value *ReturnValue = RV.getScalarVal();

  if (NullCheckValue) {
    AdjustNull = CGF.createBasicBlock("adjust.null");
    AdjustNotNull = CGF.createBasicBlock("adjust.notnull");
    AdjustEnd = CGF.createBasicBlock("adjust.end");

    llvm::Value *IsNull = CGF.Builder.CreateIsNull(ReturnValue);
    CGF.Builder.CreateCondBr(IsNull, AdjustNull, AdjustNotNull);
    CGF.EmitBlock(AdjustNotNull);
  }

  auto ClassDecl = ResultType->getPointeeType()->getAsCXXRecordDecl();
  auto ClassAlign = CGF.CGM.getClassPointerAlignment(ClassDecl);
  ReturnValue = CGF.CGM.getCXXABI().performReturnAdjustment(
      CGF,
      Address(ReturnValue, CGF.ConvertTypeForMem(ResultType->getPointeeType()),
              ClassAlign),
      Thunk.Return);

  if (NullCheckValue) {
    CGF.Builder.CreateBr(AdjustEnd);
    CGF.EmitBlock(AdjustNull);
    CGF.Builder.CreateBr(AdjustEnd);
    CGF.EmitBlock(AdjustEnd);

    llvm::PHINode *PHI = CGF.Builder.CreatePHI(ReturnValue->getType(), 2);
    PHI->addIncoming(ReturnValue, AdjustNotNull);
    PHI->addIncoming(llvm::Constant::getNullValue(ReturnValue->getType()),
                     AdjustNull);
    ReturnValue = PHI;
  }

  return RValue::get(ReturnValue);
}

/// This function clones a function's DISubprogram node and enters it into
/// a value map with the intent that the map can be utilized by the cloner
/// to short-circuit Metadata node mapping.
/// Furthermore, the function resolves any DILocalVariable nodes referenced
/// by dbg.value intrinsics so they can be properly mapped during cloning.
static void resolveTopLevelMetadata(llvm::Function *Fn,
                                    llvm::ValueToValueMapTy &VMap) {
  // Clone the DISubprogram node and put it into the Value map.
  auto *DIS = Fn->getSubprogram();
  if (!DIS)
    return;
  auto *NewDIS = DIS->replaceWithDistinct(DIS->clone());
  VMap.MD()[DIS].reset(NewDIS);

  // Find all llvm.dbg.declare intrinsics and resolve the DILocalVariable nodes
  // they are referencing.
  for (auto &BB : Fn->getBasicBlockList()) {
    for (auto &I : BB) {
      if (auto *DII = dyn_cast<llvm::DbgVariableIntrinsic>(&I)) {
        auto *DILocal = DII->getVariable();
        if (!DILocal->isResolved())
          DILocal->resolve();
      }
    }
  }
}

// This function does roughly the same thing as GenerateThunk, but in a
// very different way, so that va_start and va_end work correctly.
// FIXME: This function assumes "this" is the first non-sret LLVM argument of
//        a function, and that there is an alloca built in the entry block
//        for all accesses to "this".
// FIXME: This function assumes there is only one "ret" statement per function.
// FIXME: Cloning isn't correct in the presence of indirect goto!
// FIXME: This implementation of thunks bloats codesize by duplicating the
//        function definition.  There are alternatives:
//        1. Add some sort of stub support to LLVM for cases where we can
//           do a this adjustment, then a sibcall.
//        2. We could transform the definition to take a va_list instead of an
//           actual variable argument list, then have the thunks (including a
//           no-op thunk for the regular definition) call va_start/va_end.
//           There's a bit of per-call overhead for this solution, but it's
//           better for codesize if the definition is long.
llvm::Function *
CodeGenFunction::GenerateVarArgsThunk(llvm::Function *Fn,
                                      const CGFunctionInfo &FnInfo,
                                      GlobalDecl GD, const ThunkInfo &Thunk) {
  const CXXMethodDecl *MD = cast<CXXMethodDecl>(GD.getDecl());
  const FunctionProtoType *FPT = MD->getType()->castAs<FunctionProtoType>();
  QualType ResultType = FPT->getReturnType();

  // Get the original function
  assert(FnInfo.isVariadic());
  llvm::Type *Ty = CGM.getTypes().GetFunctionType(FnInfo);
  llvm::Value *Callee = CGM.GetAddrOfFunction(GD, Ty, /*ForVTable=*/true);
  llvm::Function *BaseFn = cast<llvm::Function>(Callee);

  // Cloning can't work if we don't have a definition. The Microsoft ABI may
  // require thunks when a definition is not available. Emit an error in these
  // cases.
  if (!MD->isDefined()) {
    CGM.ErrorUnsupported(MD, "return-adjusting thunk with variadic arguments");
    return Fn;
  }
  assert(!BaseFn->isDeclaration() && "cannot clone undefined variadic method");

  // Clone to thunk.
  llvm::ValueToValueMapTy VMap;

  // We are cloning a function while some Metadata nodes are still unresolved.
  // Ensure that the value mapper does not encounter any of them.
  resolveTopLevelMetadata(BaseFn, VMap);
  llvm::Function *NewFn = llvm::CloneFunction(BaseFn, VMap);
  Fn->replaceAllUsesWith(NewFn);
  NewFn->takeName(Fn);
  Fn->eraseFromParent();
  Fn = NewFn;

  // "Initialize" CGF (minimally).
  CurFn = Fn;

  // Get the "this" value
  llvm::Function::arg_iterator AI = Fn->arg_begin();
  if (CGM.ReturnTypeUsesSRet(FnInfo))
    ++AI;

  // Find the first store of "this", which will be to the alloca associated
  // with "this".
  Address ThisPtr =
      Address(&*AI, ConvertTypeForMem(MD->getThisType()->getPointeeType()),
              CGM.getClassPointerAlignment(MD->getParent()));
  llvm::BasicBlock *EntryBB = &Fn->front();
  llvm::BasicBlock::iterator ThisStore =
      llvm::find_if(*EntryBB, [&](llvm::Instruction &I) {
        return isa<llvm::StoreInst>(I) &&
               I.getOperand(0) == ThisPtr.getPointer();
      });
  assert(ThisStore != EntryBB->end() &&
         "Store of this should be in entry block?");
  // Adjust "this", if necessary.
  Builder.SetInsertPoint(&*ThisStore);
  llvm::Value *AdjustedThisPtr =
      CGM.getCXXABI().performThisAdjustment(*this, ThisPtr, Thunk.This);
  AdjustedThisPtr = Builder.CreateBitCast(AdjustedThisPtr,
                                          ThisStore->getOperand(0)->getType());
  ThisStore->setOperand(0, AdjustedThisPtr);

  if (!Thunk.Return.isEmpty()) {
    // Fix up the returned value, if necessary.
    for (llvm::BasicBlock &BB : *Fn) {
      llvm::Instruction *T = BB.getTerminator();
      if (isa<llvm::ReturnInst>(T)) {
        RValue RV = RValue::get(T->getOperand(0));
        T->eraseFromParent();
        Builder.SetInsertPoint(&BB);
        RV = PerformReturnAdjustment(*this, ResultType, RV, Thunk);
        Builder.CreateRet(RV.getScalarVal());
        break;
      }
    }
  }

  return Fn;
}

void CodeGenFunction::StartThunk(llvm::Function *Fn, GlobalDecl GD,
                                 const CGFunctionInfo &FnInfo,
                                 bool IsUnprototyped, const CXXMethodDecl* OriginalMethod) {
  assert(!CurGD.getDecl() && "CurGD was already set!");
  CurGD = GD;
  CurFuncIsThunk = true;

  // Build FunctionArgs.
  const CXXMethodDecl *MD = cast<CXXMethodDecl>(GD.getDecl());
  QualType ThisType = MD->getThisType();
  QualType ResultType;
  if (IsUnprototyped)
    ResultType = CGM.getContext().VoidTy;
  else if (CGM.getCXXABI().HasThisReturn(GD))
    ResultType = ThisType;
  else if (CGM.getCXXABI().hasMostDerivedReturn(GD))
    ResultType = CGM.getContext().VoidPtrTy;
  else
    ResultType = OriginalMethod->getType()->castAs<FunctionProtoType>()->getReturnType();
  FunctionArgList FunctionArgs;

  // Create the implicit 'this' parameter declaration.
  CGM.getCXXABI().buildThisParam(*this, FunctionArgs, OriginalMethod);

  // Add the rest of the parameters, if we have a prototype to work with.
  if (!IsUnprototyped) {
    FunctionArgs.append(MD->param_begin(), MD->param_end());

    if (isa<CXXDestructorDecl>(MD))
      CGM.getCXXABI().addImplicitStructorParams(*this, ResultType,
                                                FunctionArgs);
  }

  // Start defining the function.
  auto NL = ApplyDebugLocation::CreateEmpty(*this);
  StartFunction(GlobalDecl(), ResultType, Fn, FnInfo, FunctionArgs,
                MD->getLocation());
  // Create a scope with an artificial location for the body of this function.
  auto AL = ApplyDebugLocation::CreateArtificial(*this);

  // Since we didn't pass a GlobalDecl to StartFunction, do this ourselves.
  CGM.getCXXABI().EmitInstanceFunctionProlog(*this);
  CXXThisValue = CXXABIThisValue;
  CurCodeDecl = MD;
  CurFuncDecl = OriginalMethod;
}

void CodeGenFunction::FinishThunk() {
  // Clear these to restore the invariants expected by
  // StartFunction/FinishFunction.
  CurCodeDecl = nullptr;
  CurFuncDecl = nullptr;

  FinishFunction();
}

void CodeGenFunction::EmitCallAndReturnForThunk(llvm::FunctionCallee Callee,
                                                const ThunkInfo *Thunk,
                                                bool IsUnprototyped) {
  assert(isa<CXXMethodDecl>(CurGD.getDecl()) &&
         "Please use a new CGF for this thunk");
  const CXXMethodDecl *MD = cast<CXXMethodDecl>(CurGD.getDecl());

  // Adjust the 'this' pointer if necessary
  llvm::Value *AdjustedThisPtr =
    Thunk ? CGM.getCXXABI().performThisAdjustment(
                          *this, LoadCXXThisAddress(), Thunk->This)
          : LoadCXXThis();

  // If perfect forwarding is required a variadic method, a method using
  // inalloca, or an unprototyped thunk, use musttail. Emit an error if this
  // thunk requires a return adjustment, since that is impossible with musttail.
  if (CurFnInfo->usesInAlloca() || CurFnInfo->isVariadic() || IsUnprototyped) {
    if (Thunk && !Thunk->Return.isEmpty()) {
      if (IsUnprototyped)
        CGM.ErrorUnsupported(
            MD, "return-adjusting thunk with incomplete parameter type");
      else if (CurFnInfo->isVariadic())
        llvm_unreachable("shouldn't try to emit musttail return-adjusting "
                         "thunks for variadic functions");
      else
        CGM.ErrorUnsupported(
            MD, "non-trivial argument copy for return-adjusting thunk");
    }
    EmitMustTailThunk(CurGD, AdjustedThisPtr, Callee);
    return;
  }

  // Start building CallArgs.
  CallArgList CallArgs;
  QualType ThisType = MD->getThisType();
  CallArgs.add(RValue::get(AdjustedThisPtr), ThisType);

  if (isa<CXXDestructorDecl>(MD))
    CGM.getCXXABI().adjustCallArgsForDestructorThunk(*this, CurGD, CallArgs);

#ifndef NDEBUG
  unsigned PrefixArgs = CallArgs.size() - 1;
#endif
  // Add the rest of the arguments.
  for (const ParmVarDecl *PD : MD->parameters())
    EmitDelegateCallArg(CallArgs, PD, SourceLocation());

  const FunctionProtoType *FPT = MD->getType()->castAs<FunctionProtoType>();

#ifndef NDEBUG
  const CGFunctionInfo &CallFnInfo = CGM.getTypes().arrangeCXXMethodCall(
      CallArgs, FPT, RequiredArgs::forPrototypePlus(FPT, 1), PrefixArgs);
  assert(CallFnInfo.getRegParm() == CurFnInfo->getRegParm() &&
         CallFnInfo.isNoReturn() == CurFnInfo->isNoReturn() &&
         CallFnInfo.getCallingConvention() == CurFnInfo->getCallingConvention());
  assert(isa<CXXDestructorDecl>(MD) || // ignore dtor return types
         similar(CallFnInfo.getReturnInfo(), CallFnInfo.getReturnType(),
                 CurFnInfo->getReturnInfo(), CurFnInfo->getReturnType()));
  assert(CallFnInfo.arg_size() == CurFnInfo->arg_size());
  for (unsigned i = 0, e = CurFnInfo->arg_size(); i != e; ++i)
    assert(similar(CallFnInfo.arg_begin()[i].info,
                   CallFnInfo.arg_begin()[i].type,
                   CurFnInfo->arg_begin()[i].info,
                   CurFnInfo->arg_begin()[i].type));
#endif

  // Determine whether we have a return value slot to use.
  QualType ResultType = CGM.getCXXABI().HasThisReturn(CurGD)
                            ? ThisType
                            : CGM.getCXXABI().hasMostDerivedReturn(CurGD)
                                  ? CGM.getContext().VoidPtrTy
                                  : FPT->getReturnType();
  ReturnValueSlot Slot;
  if (!ResultType->isVoidType() &&
      (CurFnInfo->getReturnInfo().getKind() == ABIArgInfo::Indirect ||
       hasAggregateEvaluationKind(ResultType)))
    Slot = ReturnValueSlot(ReturnValue, ResultType.isVolatileQualified(),
                           /*IsUnused=*/false, /*IsExternallyDestructed=*/true);

  // Now emit our call.
  llvm::CallBase *CallOrInvoke;
  RValue RV = EmitCall(*CurFnInfo, CGCallee::forDirect(Callee, CurGD), Slot, CallArgs, &CallOrInvoke);

  // Consider return adjustment if we have ThunkInfo.
  if (Thunk && !Thunk->Return.isEmpty())
    RV = PerformReturnAdjustment(*this, ResultType, RV, *Thunk);
  else if (llvm::CallInst* Call = dyn_cast<llvm::CallInst>(CallOrInvoke))
    Call->setTailCallKind(llvm::CallInst::TCK_Tail);

  // Emit return.
  if (!ResultType->isVoidType() && Slot.isNull())
    CGM.getCXXABI().EmitReturnFromThunk(*this, RV, ResultType);

  // Disable the final ARC autorelease.
  AutoreleaseResult = false;

  FinishThunk();
}

void CodeGenFunction::EmitMustTailThunk(GlobalDecl GD,
                                        llvm::Value *AdjustedThisPtr,
                                        llvm::FunctionCallee Callee) {
  // Emitting a musttail call thunk doesn't use any of the CGCall.cpp machinery
  // to translate AST arguments into LLVM IR arguments.  For thunks, we know
  // that the caller prototype more or less matches the callee prototype with
  // the exception of 'this'.
  SmallVector<llvm::Value *, 8> Args(llvm::make_pointer_range(CurFn->args()));

  // Set the adjusted 'this' pointer.
  const ABIArgInfo &ThisAI = CurFnInfo->arg_begin()->info;
  if (ThisAI.isDirect()) {
    const ABIArgInfo &RetAI = CurFnInfo->getReturnInfo();
    int ThisArgNo = RetAI.isIndirect() && !RetAI.isSRetAfterThis() ? 1 : 0;
    llvm::Type *ThisType = Args[ThisArgNo]->getType();
    if (ThisType != AdjustedThisPtr->getType())
      AdjustedThisPtr = Builder.CreateBitCast(AdjustedThisPtr, ThisType);
    Args[ThisArgNo] = AdjustedThisPtr;
  } else {
    assert(ThisAI.isInAlloca() && "this is passed directly or inalloca");
    Address ThisAddr = GetAddrOfLocalVar(CXXABIThisDecl);
    llvm::Type *ThisType = ThisAddr.getElementType();
    if (ThisType != AdjustedThisPtr->getType())
      AdjustedThisPtr = Builder.CreateBitCast(AdjustedThisPtr, ThisType);
    Builder.CreateStore(AdjustedThisPtr, ThisAddr);
  }

  // Emit the musttail call manually.  Even if the prologue pushed cleanups, we
  // don't actually want to run them.
  llvm::CallInst *Call = Builder.CreateCall(Callee, Args);
  Call->setTailCallKind(llvm::CallInst::TCK_MustTail);

  // Apply the standard set of call attributes.
  unsigned CallingConv;
  llvm::AttributeList Attrs;
  CGM.ConstructAttributeList(Callee.getCallee()->getName(), *CurFnInfo, GD,
                             Attrs, CallingConv, /*AttrOnCallSite=*/true,
                             /*IsThunk=*/false);
  Call->setAttributes(Attrs);
  Call->setCallingConv(static_cast<llvm::CallingConv::ID>(CallingConv));

  if (Call->getType()->isVoidTy())
    Builder.CreateRetVoid();
  else
    Builder.CreateRet(Call);

  // Finish the function to maintain CodeGenFunction invariants.
  // FIXME: Don't emit unreachable code.
  EmitBlock(createBasicBlock());

  FinishThunk();
}

void CodeGenFunction::generateThunk(llvm::Function *Fn,
                                    const CGFunctionInfo &FnInfo, GlobalDecl GD,
                                    const ThunkInfo &Thunk,
                                    bool IsUnprototyped) {
  const CXXMethodDecl* OriginalMethod = getTarget().isByteAddressable() ? cast<CXXMethodDecl>(GD.getDecl()) :
                                                                          Thunk.Method;
  const CGFunctionInfo &ThunkFnInfo = CGM.getTypes().arrangeGlobalDeclaration(GD);
  StartThunk(Fn, GD, ThunkFnInfo, IsUnprototyped, OriginalMethod);
  // Create a scope with an artificial location for the body of this function.
  auto AL = ApplyDebugLocation::CreateArtificial(*this);

  // Get our callee. Use a placeholder type if this method is unprototyped so
  // that CodeGenModule doesn't try to set attributes.
  llvm::Type *Ty;
  if (IsUnprototyped)
    Ty = llvm::StructType::get(getLLVMContext());
  else {
    Ty = CGM.getTypes().GetFunctionType(ThunkFnInfo);
  }

  llvm::Value *Callee = nullptr;
  if(Thunk.isMemberPointerThunk && OriginalMethod->isVirtual())
    Callee = CGM.getCXXABI().getVirtualFunctionPointer(*this, OriginalMethod, LoadCXXThisAddress(), Ty, SourceLocation()).getFunctionPointer();
  else
  {
    llvm::Constant* CalleeConst = CGM.GetAddrOfFunction(GD, Ty, /*ForVTable=*/true);
    // Fix up the function type for an unprototyped musttail call.
    if (IsUnprototyped)
      CalleeConst = llvm::ConstantExpr::getBitCast(CalleeConst, Fn->getType());
    Callee = CalleeConst;
  }

  // Make the call and return the result.
  EmitCallAndReturnForThunk(llvm::FunctionCallee(cast<llvm::FunctionType>(Ty), Callee),
                            &Thunk, IsUnprototyped);
}

static bool shouldEmitVTableThunk(CodeGenModule &CGM, const CXXMethodDecl *MD,
                                  bool IsUnprototyped, bool ForVTable) {
  // Always emit thunks in the MS C++ ABI. We cannot rely on other TUs to
  // provide thunks for us.
  if (CGM.getTarget().getCXXABI().isMicrosoft())
    return true;

  // In the Itanium C++ ABI, vtable thunks are provided by TUs that provide
  // definitions of the main method. Therefore, emitting thunks with the vtable
  // is purely an optimization. Emit the thunk if optimizations are enabled and
  // all of the parameter types are complete.
  if (ForVTable)
    return CGM.getCodeGenOpts().OptimizationLevel && !IsUnprototyped;

  // Always emit thunks along with the method definition.
  return true;
}

llvm::Constant *CodeGenVTables::maybeEmitThunk(GlobalDecl GD,
                                               ThunkInfo TI,
                                               bool ForVTable) {
  bool byteAddressable = CGM.getTarget().isByteAddressable();
  const CXXMethodDecl* OriginalMethod = TI.Method;
  const CXXMethodDecl *MD = cast<CXXMethodDecl>(GD.getDecl());

  // Override the non virtual offset in bytes with the topological offset on NBA targets
  bool asmjs = OriginalMethod? OriginalMethod->getParent()->hasAttr<AsmJSAttr>() : false;
  if(!byteAddressable && !asmjs) {
    if(!TI.This.isEmpty())
      TI.This.NonVirtual = ComputeTopologicalBaseOffset(CGM, TI.This.AdjustmentTarget, TI.This.AdjustmentPath);
    // Return adjustment will be handled with a "reverse" downcast with a negative offset
    if(!TI.Return.isEmpty())
      TI.Return.NonVirtual = ComputeTopologicalBaseOffset(CGM, TI.Return.AdjustmentSource, TI.Return.AdjustmentTarget);
  }
 
  // First, get a declaration. Compute the mangled name. Don't worry about
  // getting the function prototype right, since we may only need this
  // declaration to fill in a vtable slot.
  SmallString<256> Name;
  MangleContext &MCtx = CGM.getCXXABI().getMangleContext();
  llvm::raw_svector_ostream Out(Name);
  if (const CXXDestructorDecl *DD = dyn_cast<CXXDestructorDecl>(MD))
    MCtx.mangleCXXDtorThunk(DD, GD.getDtorType(), TI.This, Out);
  else
    MCtx.mangleThunk(MD, TI, Out);
  llvm::Type *ThunkVTableTy = CGM.getTypes().GetFunctionTypeForVTable(
                                  byteAddressable?GD:GD.getWithDecl(OriginalMethod));
  llvm::Constant *Thunk = CGM.GetAddrOfThunk(Name, ThunkVTableTy, GD);

  // If we don't need to emit a definition, return this declaration as is.
  bool IsUnprototyped = !CGM.getTypes().isFuncTypeConvertible(
      MD->getType()->castAs<FunctionType>());
  if (!shouldEmitVTableThunk(CGM, MD, IsUnprototyped, ForVTable))
    return Thunk;

  // Arrange a function prototype appropriate for a function definition. In some
  // cases in the MS ABI, we may need to build an unprototyped musttail thunk.
  const CGFunctionInfo &FnInfo =
      IsUnprototyped ? CGM.getTypes().arrangeUnprototypedMustTailThunk(MD)
                     : CGM.getTypes().arrangeGlobalDeclaration(
                     +                byteAddressable?GD:GD.getWithDecl(OriginalMethod));
  llvm::FunctionType *ThunkFnTy = CGM.getTypes().GetFunctionType(FnInfo);

  llvm::Function *ThunkFn = cast<llvm::Function>(Thunk->stripPointerCastsSafe());

  int thisIndex = ThunkFn->hasStructRetAttr() && !FnInfo.getReturnInfo().isSRetAfterThis() ? 1 : 0;
  llvm::Type* CurrentThisTy = CGM.getTypes().ConvertTypeForMem(cast<PointerType>(FnInfo.arg_begin()->type)->getPointeeType());

  if (!ThunkFn->hasParamAttribute(thisIndex, llvm::Attribute::ElementType))
    ThunkFn->addParamAttr(thisIndex, llvm::Attribute::get(CGM.getLLVMContext(), llvm::Attribute::ElementType, CurrentThisTy));

  // If the type of the underlying GlobalValue is wrong, we'll have to replace
  // it. It should be a declaration.
  // CHEERP: If there are virtual bases it is possible that the type of `this` mismatches
  // between the existing and new think, but one is directbase of the other.
  // We keep the thunk with the most basic `this` type
  bool NewIsDirectBase = false;
  bool CurrentIsDirectBase = false;
  if (!CGM.getTarget().isByteAddressable()) {
    // Geth the `this` type. It can be the first or second parameter (if there is a
    // struct return pointer)
    llvm::StructType* CurrentThisStructTy = cast<llvm::StructType>(CurrentThisTy);
    llvm::StructType* NewThisTy = cast<llvm::StructType>(ThunkFn->getParamAttribute(thisIndex, llvm::Attribute::ElementType).getValueAsType());

    // Check if one of the types is directbase of the other (check all the directbase hierarchy)
    for (llvm::StructType* I = NewThisTy; I != nullptr; I = I->getDirectBase()) {
      if (I == CurrentThisTy) {
        NewIsDirectBase = true;
        break;
      }
    }
    for (llvm::StructType* I = CurrentThisStructTy; I != nullptr; I = I->getDirectBase()) {
      if (I == NewThisTy) {
        CurrentIsDirectBase = true;
        break;
      }
    }
  }
  if (ThunkFn->getFunctionType() != ThunkFnTy && !NewIsDirectBase) {
    llvm::GlobalValue *OldThunkFn = ThunkFn;

    // If the types mismatch then we have to rewrite the definition.
    assert((OldThunkFn->isDeclaration() || CurrentIsDirectBase) &&
           "Shouldn't replace non-declaration");

    // Remove the name from the old thunk function and get a new thunk.
    OldThunkFn->setName(StringRef());
    ThunkFn = llvm::Function::Create(ThunkFnTy, llvm::Function::ExternalLinkage,
                                     Name.str(), &CGM.getModule());
    CGM.SetLLVMFunctionAttributes(GD, FnInfo, ThunkFn, /*IsThunk=*/false);

    ThunkFn->addParamAttr(thisIndex, llvm::Attribute::get(CGM.getLLVMContext(), llvm::Attribute::ElementType, CurrentThisTy));

    // If needed, replace the old thunk with a bitcast.
    if (!OldThunkFn->use_empty()) {
      llvm::Constant *NewPtrForOldDecl =
          llvm::ConstantExpr::getBitCast(ThunkFn, OldThunkFn->getType());
      OldThunkFn->replaceAllUsesWith(NewPtrForOldDecl);
    }

    // Remove the old thunk.
    OldThunkFn->eraseFromParent();
  }

  bool ABIHasKeyFunctions = CGM.getTarget().getCXXABI().hasKeyFunctions();
  bool UseAvailableExternallyLinkage = ForVTable && ABIHasKeyFunctions;

  if (!ThunkFn->isDeclaration()) {
    if (!ABIHasKeyFunctions || UseAvailableExternallyLinkage) {
      // There is already a thunk emitted for this function, do nothing.
      return ThunkFn;
    }

    setThunkProperties(CGM, TI, ThunkFn, ForVTable, GD);
    return ThunkFn;
  }

  // If this will be unprototyped, add the "thunk" attribute so that LLVM knows
  // that the return type is meaningless. These thunks can be used to call
  // functions with differing return types, and the caller is required to cast
  // the prototype appropriately to extract the correct value.
  if (IsUnprototyped)
    ThunkFn->addFnAttr("thunk");

  CGM.SetLLVMFunctionAttributesForDefinition(GD.getDecl(), ThunkFn);
  // CHEERP: if the class is declared with the asmjs attribute, put the thunk
  // in the asmjs section
  if (GD.getDecl()->hasAttr<AsmJSAttr>()) {
    ThunkFn->setSection("asmjs");
  }

  // Thunks for variadic methods are special because in general variadic
  // arguments cannot be perfectly forwarded. In the general case, clang
  // implements such thunks by cloning the original function body. However, for
  // thunks with no return adjustment on targets that support musttail, we can
  // use musttail to perfectly forward the variadic arguments.
  bool ShouldCloneVarArgs = false;
  if (!IsUnprototyped && ThunkFn->isVarArg()) {
    ShouldCloneVarArgs = true;
    if (TI.Return.isEmpty()) {
      switch (CGM.getTriple().getArch()) {
      case llvm::Triple::x86_64:
      case llvm::Triple::x86:
      case llvm::Triple::aarch64:
        ShouldCloneVarArgs = false;
        break;
      default:
        break;
      }
    }
  }

  if (ShouldCloneVarArgs) {
    if (UseAvailableExternallyLinkage)
      return ThunkFn;
    ThunkFn =
        CodeGenFunction(CGM).GenerateVarArgsThunk(ThunkFn, FnInfo, GD, TI);
  } else {
    // Normal thunk body generation.
    CodeGenFunction(CGM).generateThunk(ThunkFn, FnInfo, GD, TI, IsUnprototyped);
  }

  setThunkProperties(CGM, TI, ThunkFn, ForVTable, GD);
  return ThunkFn;
}

void CodeGenVTables::EmitThunks(GlobalDecl GD) {
  const CXXMethodDecl *MD =
    cast<CXXMethodDecl>(GD.getDecl())->getCanonicalDecl();

  // We don't need to generate thunks for the base destructor.
  if (isa<CXXDestructorDecl>(MD) && GD.getDtorType() == Dtor_Base)
    return;

  const VTableContextBase::ThunkInfoVectorTy *ThunkInfoVector =
      VTContext->getThunkInfo(GD);

  if (!ThunkInfoVector)
    return;

  for (const ThunkInfo& Thunk : *ThunkInfoVector)
    maybeEmitThunk(GD, Thunk, /*ForVTable=*/false);
}

void CodeGenVTables::addRelativeComponent(ConstantAggregateBuilderBase &builder,
                                          llvm::Constant *component,
                                          unsigned vtableAddressPoint,
                                          bool vtableHasLocalLinkage,
                                          bool isCompleteDtor) const {
  // No need to get the offset of a nullptr.
  if (component->isNullValue())
    return builder.add(llvm::ConstantInt::get(CGM.Int32Ty, 0));

  auto *globalVal =
      cast<llvm::GlobalValue>(component->stripPointerCastsAndAliases());
  llvm::Module &module = CGM.getModule();

  // We don't want to copy the linkage of the vtable exactly because we still
  // want the stub/proxy to be emitted for properly calculating the offset.
  // Examples where there would be no symbol emitted are available_externally
  // and private linkages.
  auto stubLinkage = vtableHasLocalLinkage ? llvm::GlobalValue::InternalLinkage
                                           : llvm::GlobalValue::ExternalLinkage;

  llvm::Constant *target;
  if (auto *func = dyn_cast<llvm::Function>(globalVal)) {
    target = llvm::DSOLocalEquivalent::get(func);
  } else {
    llvm::SmallString<16> rttiProxyName(globalVal->getName());
    rttiProxyName.append(".rtti_proxy");

    // The RTTI component may not always be emitted in the same linkage unit as
    // the vtable. As a general case, we can make a dso_local proxy to the RTTI
    // that points to the actual RTTI struct somewhere. This will result in a
    // GOTPCREL relocation when taking the relative offset to the proxy.
    llvm::GlobalVariable *proxy = module.getNamedGlobal(rttiProxyName);
    if (!proxy) {
      proxy = new llvm::GlobalVariable(module, globalVal->getType(),
                                       /*isConstant=*/true, stubLinkage,
                                       globalVal, rttiProxyName);
      proxy->setDSOLocal(true);
      proxy->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
      if (!proxy->hasLocalLinkage()) {
        proxy->setVisibility(llvm::GlobalValue::HiddenVisibility);
        proxy->setComdat(module.getOrInsertComdat(rttiProxyName));
      }
      // Do not instrument the rtti proxies with hwasan to avoid a duplicate
      // symbol error. Aliases generated by hwasan will retain the same namebut
      // the addresses they are set to may have different tags from different
      // compilation units. We don't run into this without hwasan because the
      // proxies are in comdat groups, but those aren't propagated to the alias.
      RemoveHwasanMetadata(proxy);
    }
    target = proxy;
  }

  builder.addRelativeOffsetToPosition(CGM.Int32Ty, target,
                                      /*position=*/vtableAddressPoint);
}

bool CodeGenVTables::useRelativeLayout() const {
  return CGM.getTarget().getCXXABI().isItaniumFamily() &&
         CGM.getItaniumVTableContext().isRelativeLayout();
}

llvm::Type *CodeGenVTables::getVTableComponentType() const {
  if (useRelativeLayout())
    return CGM.Int32Ty;
  return CGM.Int8PtrTy;
}

static void AddPointerLayoutOffset(const CodeGenModule &CGM,
                                   ConstantAggregateBuilderBase &builder,
                                   CharUnits offset) {
  llvm::Constant* c = llvm::ConstantInt::get(CGM.PtrDiffTy, offset.getQuantity());
  c = llvm::ConstantExpr::getIntToPtr(c, CGM.Int8PtrTy);
  builder.add(c);
}

static void AddRelativeLayoutOffset(const CodeGenModule &CGM,
                                    ConstantAggregateBuilderBase &builder,
                                    CharUnits offset) {
  builder.add(llvm::ConstantInt::get(CGM.Int32Ty, offset.getQuantity()));
}

void CodeGenVTables::addVTableComponent(ConstantAggregateBuilderBase &builder,
                                        const CXXRecordDecl *LayoutClass,
                                        const VTableLayout &layout,
                                        unsigned componentIndex,
                                        llvm::Constant *rtti,
                                        unsigned &nextVTableThunkIndex,
                                        unsigned vtableAddressPoint,
                                        bool vtableHasLocalLinkage) {
  auto &component = layout.vtable_components()[componentIndex];

  auto addOffsetConstant =
      useRelativeLayout() ? AddRelativeLayoutOffset : AddPointerLayoutOffset;

  bool asmjs = LayoutClass->hasAttr<AsmJSAttr>();

  llvm::PointerType* elemType = CGM.getTarget().isByteAddressable() ? CGM.Int8PtrTy : llvm::FunctionType::get( CGM.Int32Ty, true )->getPointerTo();

  switch (component.getKind()) {
  case VTableComponent::CK_VCallOffset:
    if(!CGM.getTarget().isByteAddressable()) {
      int32_t Offset = 0;
      if(asmjs)
        Offset = component.getVCallOffset().getQuantity();
      else if (LayoutClass != component.getVCall() && component.getVCall() != nullptr)
        Offset =  CGM.ComputeVirtualBaseIdOffset(LayoutClass, component.getVCall());
      return builder.addInt(CGM.PtrDiffTy, Offset);
    }
    return addOffsetConstant(CGM, builder, component.getVCallOffset());

  case VTableComponent::CK_VBaseOffset:
    if(!CGM.getTarget().isByteAddressable()) {
      int32_t Offset;
      if(asmjs)
        Offset = component.getVBaseOffset().getQuantity();
      else
        Offset = CGM.ComputeVirtualBaseIdOffset(LayoutClass, component.getVBase());
      return builder.addInt(CGM.PtrDiffTy, Offset);
    }
    return addOffsetConstant(CGM, builder, component.getVBaseOffset());

  case VTableComponent::CK_OffsetToTop:
    if(!CGM.getTarget().isByteAddressable())
       return builder.addInt(CGM.PtrDiffTy, component.getOffsetToTop().getQuantity());
    return addOffsetConstant(CGM, builder, component.getOffsetToTop());

  case VTableComponent::CK_RTTI:
    if (useRelativeLayout())
      return addRelativeComponent(builder, rtti, vtableAddressPoint,
                                  vtableHasLocalLinkage,
                                  /*isCompleteDtor=*/false);
    else
      return builder.add(llvm::ConstantExpr::getBitCast(rtti, CGM.getTarget().isByteAddressable() ? CGM.Int8PtrTy : CGM.getTypes().GetClassTypeInfoType()->getPointerTo()));

  case VTableComponent::CK_FunctionPointer:
  case VTableComponent::CK_CompleteDtorPointer:
  case VTableComponent::CK_DeletingDtorPointer: {
    GlobalDecl GD = component.getGlobalDecl();

    if (CGM.getLangOpts().CUDA) {
      // Emit NULL for methods we can't codegen on this
      // side. Otherwise we'd end up with vtable with unresolved
      // references.
      const CXXMethodDecl *MD = cast<CXXMethodDecl>(GD.getDecl());
      // OK on device side: functions w/ __device__ attribute
      // OK on host side: anything except __device__-only functions.
      bool CanEmitMethod =
          CGM.getLangOpts().CUDAIsDevice
              ? MD->hasAttr<CUDADeviceAttr>()
              : (MD->hasAttr<CUDAHostAttr>() || !MD->hasAttr<CUDADeviceAttr>());
      if (!CanEmitMethod)
        return builder.add(llvm::ConstantExpr::getNullValue(CGM.Int8PtrTy));
      // Method is acceptable, continue processing as usual.
    }

    auto getSpecialVirtualFn = [&](StringRef name) -> llvm::Constant * {
      // FIXME(PR43094): When merging comdat groups, lld can select a local
      // symbol as the signature symbol even though it cannot be accessed
      // outside that symbol's TU. The relative vtables ABI would make
      // __cxa_pure_virtual and __cxa_deleted_virtual local symbols, and
      // depending on link order, the comdat groups could resolve to the one
      // with the local symbol. As a temporary solution, fill these components
      // with zero. We shouldn't be calling these in the first place anyway.
      if (useRelativeLayout())
        return llvm::ConstantPointerNull::get(CGM.Int8PtrTy);

      // For NVPTX devices in OpenMP emit special functon as null pointers,
      // otherwise linking ends up with unresolved references.
      if (CGM.getLangOpts().OpenMP && CGM.getLangOpts().OpenMPIsDevice &&
          CGM.getTriple().isNVPTX())
        return llvm::ConstantPointerNull::get(CGM.Int8PtrTy);
      llvm::FunctionType *fnTy =
          llvm::FunctionType::get(CGM.VoidTy, /*isVarArg=*/false);
      llvm::Constant *fn = cast<llvm::Constant>(
          CGM.CreateRuntimeFunction(fnTy, name).getCallee());
      if (auto f = dyn_cast<llvm::Function>(fn))
        f->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
      return fn;
    };

    llvm::Constant *fnPtr;

    // Pure virtual member functions.
    if (cast<CXXMethodDecl>(GD.getDecl())->isPure()) {
      if (!PureVirtualFn)
        PureVirtualFn =
            getSpecialVirtualFn(CGM.getCXXABI().GetPureVirtualCallName());
      fnPtr = PureVirtualFn;

    // Deleted virtual member functions.
    } else if (cast<CXXMethodDecl>(GD.getDecl())->isDeleted()) {
      if (!DeletedVirtualFn)
        DeletedVirtualFn =
            getSpecialVirtualFn(CGM.getCXXABI().GetDeletedVirtualCallName());
      fnPtr = DeletedVirtualFn;

    // Thunks.
    } else if (nextVTableThunkIndex < layout.vtable_thunks().size() &&
               layout.vtable_thunks()[nextVTableThunkIndex].first == componentIndex) {
      auto thunkInfo = layout.vtable_thunks()[nextVTableThunkIndex].second;

      bool asmjs = cast<CXXMethodDecl>(GD.getDecl())->getParent()->hasAttr<AsmJSAttr>();
      if (!CGM.getTarget().isByteAddressable() && !asmjs) {
        // Override the non virtual offset in bytes with the topological offset
        // TODO: Really move topological offset logic in AST
        if(!thunkInfo.This.isEmpty())
          thunkInfo.This.NonVirtual = ComputeTopologicalBaseOffset(CGM, thunkInfo.This.AdjustmentTarget, thunkInfo.This.AdjustmentPath);
        if(!thunkInfo.Return.isEmpty())
          thunkInfo.Return.NonVirtual = ComputeTopologicalBaseOffset(CGM, thunkInfo.Return.AdjustmentSource, thunkInfo.Return.AdjustmentTarget);
      }
 
      nextVTableThunkIndex++;
      fnPtr = maybeEmitThunk(GD, thunkInfo, /*ForVTable=*/true);

    // Otherwise we can use the method definition directly.
    } else {
      llvm::Type *fnTy = CGM.getTypes().GetFunctionTypeForVTable(GD);
      fnPtr = CGM.GetAddrOfFunction(GD, fnTy, /*ForVTable=*/true);
    }

    if (useRelativeLayout()) {
      return addRelativeComponent(
          builder, fnPtr, vtableAddressPoint, vtableHasLocalLinkage,
          component.getKind() == VTableComponent::CK_CompleteDtorPointer);
    } else
      return builder.add(llvm::ConstantExpr::getBitCast(fnPtr, elemType));
  }

  case VTableComponent::CK_UnusedFunctionPointer:
    if (useRelativeLayout())
      return builder.add(llvm::ConstantExpr::getNullValue(CGM.Int32Ty));
    else
      return builder.addNullPointer(elemType);
  }

  llvm_unreachable("Unexpected vtable component kind");
}

llvm::Type *CodeGenVTables::getVTableType(const VTableLayout &layout, const CXXRecordDecl* LayoutClass) {
  bool asmjs = LayoutClass->hasAttr<AsmJSAttr>();
  SmallVector<llvm::Type *, 4> tys;
  llvm::Type *componentType = getVTableComponentType();
  for (unsigned i = 0, e = layout.getNumVTables(); i != e; ++i) {
    if(CGM.getTarget().isByteAddressable()) {
      tys.push_back(llvm::ArrayType::get(componentType, layout.getVTableSize(i)));
    } else {
      auto firstComp = layout.vtable_components().begin() + layout.getVTableOffset(i);
      auto lastComp = firstComp + layout.getVTableSize(i);
      tys.push_back(CGM.getTypes().GetVTableSubObjectType(CGM, firstComp, lastComp, 0, asmjs));
    }
  }

  return llvm::StructType::get(CGM.getLLVMContext(), tys, false, NULL, /*isByteLayout*/false, asmjs);
}

void CodeGenVTables::createVTableInitializer(ConstantStructBuilder &builder,
                                             const CXXRecordDecl *LayoutClass,
                                             const VTableLayout &layout,
                                             llvm::Constant *rtti,
                                             bool vtableHasLocalLinkage) {
  llvm::Type *componentType = getVTableComponentType();

  const auto &addressPoints = layout.getAddressPointIndices();
  unsigned nextVTableThunkIndex = 0;
  if (CGM.getTarget().isByteAddressable()) {
  for (unsigned vtableIndex = 0, endIndex = layout.getNumVTables();
       vtableIndex != endIndex; ++vtableIndex) {
    auto vtableElem = builder.beginArray(componentType);

    size_t vtableStart = layout.getVTableOffset(vtableIndex);
    size_t vtableEnd = vtableStart + layout.getVTableSize(vtableIndex);
    for (size_t componentIndex = vtableStart; componentIndex < vtableEnd;
         ++componentIndex) {
      addVTableComponent(vtableElem, LayoutClass, layout, componentIndex, rtti,
                         nextVTableThunkIndex, addressPoints[vtableIndex],
                         vtableHasLocalLinkage);
    }
    vtableElem.finishAndAddTo(builder);
  }
  } else {
    // This is very ugly, we need to duplicate the whole code since we need to pass a StructBuilder and not an ArrayBuilder
    bool asmjs = LayoutClass->hasAttr<AsmJSAttr>();
    for (unsigned i = 0, e = layout.getNumVTables(); i != e; ++i) {
      auto vtableElem = builder.beginStruct();
      size_t thisIndex = layout.getVTableOffset(i);
      size_t nextIndex = thisIndex + layout.getVTableSize(i);
      for (unsigned i = thisIndex; i != nextIndex; ++i) {
        addVTableComponent(vtableElem, LayoutClass, layout, i, rtti, nextVTableThunkIndex, -1, vtableHasLocalLinkage);
      }
      vtableElem.finishAndAddTo(builder, cast<llvm::StructType>(CGM.getTypes().GetVTableBaseType(asmjs)), asmjs);
    }
  }
}

llvm::GlobalVariable *CodeGenVTables::GenerateConstructionVTable(
    const CXXRecordDecl *RD, const BaseSubobject &Base, bool BaseIsVirtual,
    llvm::GlobalVariable::LinkageTypes Linkage,
    VTableAddressPointsMapTy &AddressPoints) {
  if (CGDebugInfo *DI = CGM.getModuleDebugInfo())
    DI->completeClassData(Base.getBase());

  bool asmjs = RD->hasAttr<AsmJSAttr>();
  std::unique_ptr<VTableLayout> VTLayout(
      getItaniumVTableContext().createConstructionVTableLayout(
          Base.getBase(), Base.getBaseOffset(), BaseIsVirtual, RD));

  // Add the address points.
  AddressPoints = VTLayout->getAddressPoints();

  // Get the mangled construction vtable name.
  SmallString<256> OutName;
  llvm::raw_svector_ostream Out(OutName);
  cast<ItaniumMangleContext>(CGM.getCXXABI().getMangleContext())
      .mangleCXXCtorVTable(RD, Base.getBaseOffset().getQuantity(),
                           Base.getBase(), Out);
  SmallString<256> Name(OutName);

  bool UsingRelativeLayout = getItaniumVTableContext().isRelativeLayout();
  bool VTableAliasExists =
      UsingRelativeLayout && CGM.getModule().getNamedAlias(Name);
  if (VTableAliasExists) {
    // We previously made the vtable hidden and changed its name.
    Name.append(".local");
  }

  llvm::Type *VTType = getVTableType(*VTLayout, RD);

  // Construction vtable symbols are not part of the Itanium ABI, so we cannot
  // guarantee that they actually will be available externally. Instead, when
  // emitting an available_externally VTT, we provide references to an internal
  // linkage construction vtable. The ABI only requires complete-object vtables
  // to be the same for all instances of a type, not construction vtables.
  if (Linkage == llvm::GlobalVariable::AvailableExternallyLinkage)
    Linkage = llvm::GlobalVariable::InternalLinkage;

  unsigned Align = CGM.getDataLayout().getABITypeAlignment(VTType);

  // Create the variable that will hold the construction vtable.
  llvm::GlobalVariable *VTable =
      CGM.CreateOrReplaceCXXRuntimeVariable(Name, VTType, Linkage, Align);

  // V-tables are always unnamed_addr.
  VTable->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

  llvm::Constant *RTTI = CGM.GetAddrOfRTTIDescriptor(
      CGM.getContext().getTagDeclType(Base.getBase()));

  // Create and set the initializer.
  ConstantInitBuilder builder(CGM);
  auto components = builder.beginStruct();
  createVTableInitializer(components, RD, *VTLayout, RTTI,
                          VTable->hasLocalLinkage());
  components.finishAndSetAsInitializer(VTable, nullptr, asmjs);
  if (asmjs)
    VTable->setSection("asmjs");

  // Set properties only after the initializer has been set to ensure that the
  // GV is treated as definition and not declaration.
  assert(!VTable->isDeclaration() && "Shouldn't set properties on declaration");
  CGM.setGVProperties(VTable, RD);

  CGM.EmitVTableTypeMetadata(RD, VTable, *VTLayout.get());

  if (UsingRelativeLayout) {
    RemoveHwasanMetadata(VTable);
    if (!VTable->isDSOLocal())
      GenerateRelativeVTableAlias(VTable, OutName);
  }

  return VTable;
}

// Ensure this vtable is not instrumented by hwasan. That is, a global alias is
// not generated for it. This is mainly used by the relative-vtables ABI where
// vtables instead contain 32-bit offsets between the vtable and function
// pointers. Hwasan is disabled for these vtables for now because the tag in a
// vtable pointer may fail the overflow check when resolving 32-bit PLT
// relocations. A future alternative for this would be finding which usages of
// the vtable can continue to use the untagged hwasan value without any loss of
// value in hwasan.
void CodeGenVTables::RemoveHwasanMetadata(llvm::GlobalValue *GV) const {
  if (CGM.getLangOpts().Sanitize.has(SanitizerKind::HWAddress)) {
    llvm::GlobalValue::SanitizerMetadata Meta;
    if (GV->hasSanitizerMetadata())
      Meta = GV->getSanitizerMetadata();
    Meta.NoHWAddress = true;
    GV->setSanitizerMetadata(Meta);
  }
}

// If the VTable is not dso_local, then we will not be able to indicate that
// the VTable does not need a relocation and move into rodata. A frequent
// time this can occur is for classes that should be made public from a DSO
// (like in libc++). For cases like these, we can make the vtable hidden or
// private and create a public alias with the same visibility and linkage as
// the original vtable type.
void CodeGenVTables::GenerateRelativeVTableAlias(llvm::GlobalVariable *VTable,
                                                 llvm::StringRef AliasNameRef) {
  assert(getItaniumVTableContext().isRelativeLayout() &&
         "Can only use this if the relative vtable ABI is used");
  assert(!VTable->isDSOLocal() && "This should be called only if the vtable is "
                                  "not guaranteed to be dso_local");

  // If the vtable is available_externally, we shouldn't (or need to) generate
  // an alias for it in the first place since the vtable won't actually by
  // emitted in this compilation unit.
  if (VTable->hasAvailableExternallyLinkage())
    return;

  // Create a new string in the event the alias is already the name of the
  // vtable. Using the reference directly could lead to use of an inititialized
  // value in the module's StringMap.
  llvm::SmallString<256> AliasName(AliasNameRef);
  VTable->setName(AliasName + ".local");

  auto Linkage = VTable->getLinkage();
  assert(llvm::GlobalAlias::isValidLinkage(Linkage) &&
         "Invalid vtable alias linkage");

  llvm::GlobalAlias *VTableAlias = CGM.getModule().getNamedAlias(AliasName);
  if (!VTableAlias) {
    VTableAlias = llvm::GlobalAlias::create(VTable->getValueType(),
                                            VTable->getAddressSpace(), Linkage,
                                            AliasName, &CGM.getModule());
  } else {
    assert(VTableAlias->getValueType() == VTable->getValueType());
    assert(VTableAlias->getLinkage() == Linkage);
  }
  VTableAlias->setVisibility(VTable->getVisibility());
  VTableAlias->setUnnamedAddr(VTable->getUnnamedAddr());

  // Both of these imply dso_local for the vtable.
  if (!VTable->hasComdat()) {
    // If this is in a comdat, then we shouldn't make the linkage private due to
    // an issue in lld where private symbols can be used as the key symbol when
    // choosing the prevelant group. This leads to "relocation refers to a
    // symbol in a discarded section".
    VTable->setLinkage(llvm::GlobalValue::PrivateLinkage);
  } else {
    // We should at least make this hidden since we don't want to expose it.
    VTable->setVisibility(llvm::GlobalValue::HiddenVisibility);
  }

  VTableAlias->setAliasee(VTable);
}

static bool shouldEmitAvailableExternallyVTable(const CodeGenModule &CGM,
                                                const CXXRecordDecl *RD) {
  return CGM.getCodeGenOpts().OptimizationLevel > 0 &&
         CGM.getCXXABI().canSpeculativelyEmitVTable(RD);
}

/// Compute the required linkage of the vtable for the given class.
///
/// Note that we only call this at the end of the translation unit.
llvm::GlobalVariable::LinkageTypes
CodeGenModule::getVTableLinkage(const CXXRecordDecl *RD) {
  if (!RD->isExternallyVisible())
    return llvm::GlobalVariable::InternalLinkage;

  // We're at the end of the translation unit, so the current key
  // function is fully correct.
  const CXXMethodDecl *keyFunction = Context.getCurrentKeyFunction(RD);
  if (keyFunction && !RD->hasAttr<DLLImportAttr>()) {
    // If this class has a key function, use that to determine the
    // linkage of the vtable.
    const FunctionDecl *def = nullptr;
    if (keyFunction->hasBody(def))
      keyFunction = cast<CXXMethodDecl>(def);

    switch (keyFunction->getTemplateSpecializationKind()) {
      case TSK_Undeclared:
      case TSK_ExplicitSpecialization:
        assert((def || CodeGenOpts.OptimizationLevel > 0 ||
                CodeGenOpts.getDebugInfo() != codegenoptions::NoDebugInfo) &&
               "Shouldn't query vtable linkage without key function, "
               "optimizations, or debug info");
        if (!def && CodeGenOpts.OptimizationLevel > 0)
          return llvm::GlobalVariable::AvailableExternallyLinkage;

        if (keyFunction->isInlined())
          return !Context.getLangOpts().AppleKext ?
                   llvm::GlobalVariable::LinkOnceODRLinkage :
                   llvm::Function::InternalLinkage;

        return llvm::GlobalVariable::ExternalLinkage;

      case TSK_ImplicitInstantiation:
        return !Context.getLangOpts().AppleKext ?
                 llvm::GlobalVariable::LinkOnceODRLinkage :
                 llvm::Function::InternalLinkage;

      case TSK_ExplicitInstantiationDefinition:
        return !Context.getLangOpts().AppleKext ?
                 llvm::GlobalVariable::WeakODRLinkage :
                 llvm::Function::InternalLinkage;

      case TSK_ExplicitInstantiationDeclaration:
        llvm_unreachable("Should not have been asked to emit this");
    }
  }

  // -fapple-kext mode does not support weak linkage, so we must use
  // internal linkage.
  if (Context.getLangOpts().AppleKext)
    return llvm::Function::InternalLinkage;

  llvm::GlobalVariable::LinkageTypes DiscardableODRLinkage =
      llvm::GlobalValue::LinkOnceODRLinkage;
  llvm::GlobalVariable::LinkageTypes NonDiscardableODRLinkage =
      llvm::GlobalValue::WeakODRLinkage;
  if (RD->hasAttr<DLLExportAttr>()) {
    // Cannot discard exported vtables.
    DiscardableODRLinkage = NonDiscardableODRLinkage;
  } else if (RD->hasAttr<DLLImportAttr>()) {
    // Imported vtables are available externally.
    DiscardableODRLinkage = llvm::GlobalVariable::AvailableExternallyLinkage;
    NonDiscardableODRLinkage = llvm::GlobalVariable::AvailableExternallyLinkage;
  }

  switch (RD->getTemplateSpecializationKind()) {
    case TSK_Undeclared:
    case TSK_ExplicitSpecialization:
    case TSK_ImplicitInstantiation:
      return DiscardableODRLinkage;

    case TSK_ExplicitInstantiationDeclaration:
      // Explicit instantiations in MSVC do not provide vtables, so we must emit
      // our own.
      if (getTarget().getCXXABI().isMicrosoft())
        return DiscardableODRLinkage;
      return shouldEmitAvailableExternallyVTable(*this, RD)
                 ? llvm::GlobalVariable::AvailableExternallyLinkage
                 : llvm::GlobalVariable::ExternalLinkage;

    case TSK_ExplicitInstantiationDefinition:
      return NonDiscardableODRLinkage;
  }

  llvm_unreachable("Invalid TemplateSpecializationKind!");
}

/// This is a callback from Sema to tell us that a particular vtable is
/// required to be emitted in this translation unit.
///
/// This is only called for vtables that _must_ be emitted (mainly due to key
/// functions).  For weak vtables, CodeGen tracks when they are needed and
/// emits them as-needed.
void CodeGenModule::EmitVTable(CXXRecordDecl *theClass) {
  VTables.GenerateClassData(theClass);
}

void
CodeGenVTables::GenerateClassData(const CXXRecordDecl *RD) {
  if (CGDebugInfo *DI = CGM.getModuleDebugInfo())
    DI->completeClassData(RD);

  if (RD->getNumVBases())
    CGM.getCXXABI().emitVirtualInheritanceTables(RD);

  CGM.getCXXABI().emitVTableDefinitions(*this, RD);
}

/// At this point in the translation unit, does it appear that can we
/// rely on the vtable being defined elsewhere in the program?
///
/// The response is really only definitive when called at the end of
/// the translation unit.
///
/// The only semantic restriction here is that the object file should
/// not contain a vtable definition when that vtable is defined
/// strongly elsewhere.  Otherwise, we'd just like to avoid emitting
/// vtables when unnecessary.
bool CodeGenVTables::isVTableExternal(const CXXRecordDecl *RD) {
  assert(RD->isDynamicClass() && "Non-dynamic classes have no VTable.");

  // We always synthesize vtables if they are needed in the MS ABI. MSVC doesn't
  // emit them even if there is an explicit template instantiation.
  if (CGM.getTarget().getCXXABI().isMicrosoft())
    return false;

  // If we have an explicit instantiation declaration (and not a
  // definition), the vtable is defined elsewhere.
  TemplateSpecializationKind TSK = RD->getTemplateSpecializationKind();
  if (TSK == TSK_ExplicitInstantiationDeclaration)
    return true;

  // Otherwise, if the class is an instantiated template, the
  // vtable must be defined here.
  if (TSK == TSK_ImplicitInstantiation ||
      TSK == TSK_ExplicitInstantiationDefinition)
    return false;

  // Otherwise, if the class doesn't have a key function (possibly
  // anymore), the vtable must be defined here.
  const CXXMethodDecl *keyFunction = CGM.getContext().getCurrentKeyFunction(RD);
  if (!keyFunction)
    return false;

  // Otherwise, if we don't have a definition of the key function, the
  // vtable must be defined somewhere else.
  return !keyFunction->hasBody();
}

/// Given that we're currently at the end of the translation unit, and
/// we've emitted a reference to the vtable for this class, should
/// we define that vtable?
static bool shouldEmitVTableAtEndOfTranslationUnit(CodeGenModule &CGM,
                                                   const CXXRecordDecl *RD) {
  // If vtable is internal then it has to be done.
  if (!CGM.getVTables().isVTableExternal(RD))
    return true;

  // If it's external then maybe we will need it as available_externally.
  return shouldEmitAvailableExternallyVTable(CGM, RD);
}

/// Given that at some point we emitted a reference to one or more
/// vtables, and that we are now at the end of the translation unit,
/// decide whether we should emit them.
void CodeGenModule::EmitDeferredVTables() {
#ifndef NDEBUG
  // Remember the size of DeferredVTables, because we're going to assume
  // that this entire operation doesn't modify it.
  size_t savedSize = DeferredVTables.size();
#endif

  for (const CXXRecordDecl *RD : DeferredVTables)
    if (shouldEmitVTableAtEndOfTranslationUnit(*this, RD))
      VTables.GenerateClassData(RD);
    else if (shouldOpportunisticallyEmitVTables())
      OpportunisticVTables.push_back(RD);

  assert(savedSize == DeferredVTables.size() &&
         "deferred extra vtables during vtable emission?");
  DeferredVTables.clear();
}

bool CodeGenModule::AlwaysHasLTOVisibilityPublic(const CXXRecordDecl *RD) {
  if (RD->hasAttr<LTOVisibilityPublicAttr>() || RD->hasAttr<UuidAttr>())
    return true;

  if (!getCodeGenOpts().LTOVisibilityPublicStd)
    return false;

  const DeclContext *DC = RD;
  while (true) {
    auto *D = cast<Decl>(DC);
    DC = DC->getParent();
    if (isa<TranslationUnitDecl>(DC->getRedeclContext())) {
      if (auto *ND = dyn_cast<NamespaceDecl>(D))
        if (const IdentifierInfo *II = ND->getIdentifier())
          if (II->isStr("std") || II->isStr("stdext"))
            return true;
      break;
    }
  }

  return false;
}

bool CodeGenModule::HasHiddenLTOVisibility(const CXXRecordDecl *RD) {
  LinkageInfo LV = RD->getLinkageAndVisibility();
  if (!isExternallyVisible(LV.getLinkage()))
    return true;

  if (getTriple().isOSBinFormatCOFF()) {
    if (RD->hasAttr<DLLExportAttr>() || RD->hasAttr<DLLImportAttr>())
      return false;
  } else {
    if (LV.getVisibility() != HiddenVisibility)
      return false;
  }

  return !AlwaysHasLTOVisibilityPublic(RD);
}

llvm::GlobalObject::VCallVisibility CodeGenModule::GetVCallVisibilityLevel(
    const CXXRecordDecl *RD, llvm::DenseSet<const CXXRecordDecl *> &Visited) {
  // If we have already visited this RD (which means this is a recursive call
  // since the initial call should have an empty Visited set), return the max
  // visibility. The recursive calls below compute the min between the result
  // of the recursive call and the current TypeVis, so returning the max here
  // ensures that it will have no effect on the current TypeVis.
  if (!Visited.insert(RD).second)
    return llvm::GlobalObject::VCallVisibilityTranslationUnit;

  LinkageInfo LV = RD->getLinkageAndVisibility();
  llvm::GlobalObject::VCallVisibility TypeVis;
  if (!isExternallyVisible(LV.getLinkage()))
    TypeVis = llvm::GlobalObject::VCallVisibilityTranslationUnit;
  else if (HasHiddenLTOVisibility(RD))
    TypeVis = llvm::GlobalObject::VCallVisibilityLinkageUnit;
  else
    TypeVis = llvm::GlobalObject::VCallVisibilityPublic;

  for (auto B : RD->bases())
    if (B.getType()->getAsCXXRecordDecl()->isDynamicClass())
      TypeVis = std::min(
          TypeVis,
          GetVCallVisibilityLevel(B.getType()->getAsCXXRecordDecl(), Visited));

  for (auto B : RD->vbases())
    if (B.getType()->getAsCXXRecordDecl()->isDynamicClass())
      TypeVis = std::min(
          TypeVis,
          GetVCallVisibilityLevel(B.getType()->getAsCXXRecordDecl(), Visited));

  return TypeVis;
}

void CodeGenModule::EmitVTableTypeMetadata(const CXXRecordDecl *RD,
                                           llvm::GlobalVariable *VTable,
                                           const VTableLayout &VTLayout) {
  if (!getCodeGenOpts().LTOUnit)
    return;

  CharUnits PointerWidth = Context.toCharUnitsFromBits(
      Context.getTargetInfo().getPointerWidth(LangAS::Default));

  typedef std::pair<const CXXRecordDecl *, unsigned> AddressPoint;
  std::vector<AddressPoint> AddressPoints;
  for (auto &&AP : VTLayout.getAddressPoints())
    AddressPoints.push_back(std::make_pair(
        AP.first.getBase(), VTLayout.getVTableOffset(AP.second.VTableIndex) +
                                AP.second.AddressPointIndex));

  // Sort the address points for determinism.
  llvm::sort(AddressPoints, [this](const AddressPoint &AP1,
                                   const AddressPoint &AP2) {
    if (&AP1 == &AP2)
      return false;

    std::string S1;
    llvm::raw_string_ostream O1(S1);
    getCXXABI().getMangleContext().mangleTypeName(
        QualType(AP1.first->getTypeForDecl(), 0), O1);
    O1.flush();

    std::string S2;
    llvm::raw_string_ostream O2(S2);
    getCXXABI().getMangleContext().mangleTypeName(
        QualType(AP2.first->getTypeForDecl(), 0), O2);
    O2.flush();

    if (S1 < S2)
      return true;
    if (S1 != S2)
      return false;

    return AP1.second < AP2.second;
  });

  ArrayRef<VTableComponent> Comps = VTLayout.vtable_components();
  for (auto AP : AddressPoints) {
    // Create type metadata for the address point.
    AddVTableTypeMetadata(VTable, PointerWidth * AP.second, AP.first);

    // The class associated with each address point could also potentially be
    // used for indirect calls via a member function pointer, so we need to
    // annotate the address of each function pointer with the appropriate member
    // function pointer type.
    for (unsigned I = 0; I != Comps.size(); ++I) {
      if (Comps[I].getKind() != VTableComponent::CK_FunctionPointer)
        continue;
      llvm::Metadata *MD = CreateMetadataIdentifierForVirtualMemPtrType(
          Context.getMemberPointerType(
              Comps[I].getFunctionDecl()->getType(),
              Context.getRecordType(AP.first).getTypePtr()));
      VTable->addTypeMetadata((PointerWidth * I).getQuantity(), MD);
    }
  }

  if (getCodeGenOpts().VirtualFunctionElimination ||
      getCodeGenOpts().WholeProgramVTables) {
    llvm::DenseSet<const CXXRecordDecl *> Visited;
    llvm::GlobalObject::VCallVisibility TypeVis =
        GetVCallVisibilityLevel(RD, Visited);
    if (TypeVis != llvm::GlobalObject::VCallVisibilityPublic)
      VTable->setVCallVisibilityMetadata(TypeVis);
  }
}

llvm::Type* CodeGenTypes::GetVTableBaseType(bool asmjs)
{
  StringRef typeName = asmjs ? "struct._ZN10__cxxabiv119__vtable_base_asmjsE" : "struct._ZN10__cxxabiv113__vtable_baseE";
  llvm::Type* ResultType = llvm::StructType::getTypeByName(CGM.getLLVMContext(), typeName);
  if(!ResultType)
  {
    llvm::StructType* ty = llvm::StructType::create(CGM.getLLVMContext(), typeName);
    ResultType = ty;
  }
  return ResultType;
}

llvm::Type* CodeGenTypes::GetVTableSubObjectType(CodeGenModule& CGM,
                                          const VTableComponent* begin,
                                          const VTableComponent* end,
                                          uint32_t extraOffsets,
                                          bool asmjs)
{
  llvm::Type* OffsetTy = CGM.getTypes().ConvertType(CGM.getContext().getPointerDiffType());
  llvm::Type* FuncPtrTy = llvm::FunctionType::get( CGM.Int32Ty, true )->getPointerTo();
  llvm::SmallVector<llvm::Type*, 16> VTableTypes;

  for (auto C = begin; C!= end; C++) {
    switch (C->getKind()) {
    case VTableComponent::CK_VCallOffset:
    case VTableComponent::CK_VBaseOffset:
    case VTableComponent::CK_OffsetToTop:
      VTableTypes.push_back(OffsetTy);
      break;
    case VTableComponent::CK_RTTI:
      VTableTypes.push_back(CGM.getTypes().GetClassTypeInfoType()->getPointerTo());
      break;
    case VTableComponent::CK_FunctionPointer:
    case VTableComponent::CK_CompleteDtorPointer:
    case VTableComponent::CK_DeletingDtorPointer:
    case VTableComponent::CK_UnusedFunctionPointer:
      VTableTypes.push_back(FuncPtrTy);
      break;
    };
  }
  for (uint32_t i = 0; i < extraOffsets; i++) {
    VTableTypes.push_back(OffsetTy);
  }
  llvm::StructType* ret = llvm::StructType::get(CGM.getLLVMContext(), VTableTypes,
                            false, cast<llvm::StructType>(CGM.getTypes().GetVTableBaseType(asmjs)), /*isByteLayout*/false, asmjs);
  return ret;
}

llvm::Type* CodeGenTypes::GetPrimaryVTableType(const CXXRecordDecl* RD) {
  ItaniumVTableContext &VTContext = CGM.getItaniumVTableContext();
  const VTableLayout& VTLayout = VTContext.getVTableLayout(RD);
  bool asmjs = RD->hasAttr<AsmJSAttr>();

  auto firstComp = VTLayout.vtable_components().begin() + VTLayout.getVTableOffset(0);
  auto lastComp = firstComp + VTLayout.getVTableSize(0);
  return GetVTableSubObjectType(CGM, firstComp, lastComp, 0, asmjs);
}

llvm::Type* CodeGenTypes::GetSecondaryVTableType(const CXXRecordDecl* RD) {
  ItaniumVTableContext &VTContext = CGM.getItaniumVTableContext();
  const VTableLayout& VTLayout = VTContext.getVTableLayout(RD);
  bool asmjs = RD->hasAttr<AsmJSAttr>();

  auto firstComp = VTLayout.vtable_components().begin() + VTLayout.getVTableOffset(0);
  auto lastComp = firstComp + VTLayout.getVTableSize(0);
  return GetVTableSubObjectType(CGM, firstComp, lastComp, VTLayout.getPrimaryVirtualMethodsCount(), asmjs);
}

llvm::Type* CodeGenTypes::GetBasicVTableType(uint32_t virtualMethodsCount, bool asmjs)
{
  llvm::SmallVector<llvm::Type*, 16> VTableTypes;
  llvm::Type* OffsetTy = CGM.getTypes().ConvertType(CGM.getContext().getPointerDiffType());
  llvm::Type* FuncPtrTy = llvm::FunctionType::get( CGM.Int32Ty, true )->getPointerTo();

  if (asmjs)
  {
    // Offset to top
    VTableTypes.push_back(OffsetTy);
  }

  // RTTI
  VTableTypes.push_back(GetClassTypeInfoType()->getPointerTo());

  // Virtual functions
  for(uint32_t j=0;j<virtualMethodsCount;j++)
    VTableTypes.push_back(FuncPtrTy);

  return llvm::StructType::get(getLLVMContext(), VTableTypes, false, cast<llvm::StructType>(GetVTableBaseType(asmjs)), /*isByteLayout*/false, asmjs);
}

llvm::Type* CodeGenTypes::GetClassTypeInfoType()
{
  llvm::Type* ResultType = llvm::StructType::getTypeByName(CGM.getLLVMContext(), "class._ZSt9type_info");
  if(!ResultType)
    ResultType = llvm::StructType::create(CGM.getLLVMContext(),"class._ZSt9type_info");
  return ResultType;
}
