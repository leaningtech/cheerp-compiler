//===- ExpandStructRegs.cpp - Expand out variables with struct type--------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass expands out some uses of LLVM variables
// (a.k.a. registers) of struct type.  It replaces loads and stores of
// structs with separate loads and stores of the structs' fields.  The
// motivation is to omit struct types from PNaCl's stable ABI.
//
// ExpandStructRegs does not yet handle all possible uses of struct
// values.  It is intended to handle the uses that Clang and the SROA
// pass generate.  Clang generates struct loads and stores, along with
// extractvalue instructions, in its implementation of C++ method
// pointers, and the SROA pass sometimes converts this code to using
// insertvalue instructions too.
//
// ExpandStructRegs does not handle:
//
//  * Array types.
//  * Function types containing arguments or return values of struct
//    type without the "byval" or "sret" attributes.  Since by-value
//    struct-passing generally uses "byval"/"sret", this does not
//    matter.
//
// Other limitations:
//
//  * ExpandStructRegs does not attempt to use memcpy() where that
//    might be more appropriate than copying fields individually.
//  * ExpandStructRegs does not preserve the contents of padding
//    between fields when copying structs.  However, the contents of
//    padding fields are not defined anyway.
//
//===----------------------------------------------------------------------===//

#include "llvm/InitializePasses.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Cheerp/ExpandStructRegs.h"

#define DEBUG_TYPE "expand-struct-regs"

using namespace llvm;

// Copy debug information from Original to New, and return New.
template <typename T> T *CopyDebug(T *New, Instruction *Original) {
  New->setDebugLoc(Original->getDebugLoc());
  return New;
}

StringRef ExpandStructRegs::getPassName() const {
	return "ExpandStructRegs";
}

static bool DoAnotherPass(Type *Ty) { return isa<StructType>(Ty); }
static bool DoAnotherPass(Value *V) { return DoAnotherPass(V->getType()); }

static bool SplitUpPHINode(PHINode *Phi) {
  StructType *STy = cast<StructType>(Phi->getType());

  Value *NewStruct = UndefValue::get(STy);
  Instruction *NewStructInsertPt = &*Phi->getParent()->getFirstInsertionPt();

  bool NeedsAnotherPass = false;

  // Create a separate PHINode for each struct field.
  for (unsigned Index = 0; Index < STy->getNumElements(); ++Index) {
    SmallVector<unsigned, 1> EVIndexes;
    EVIndexes.push_back(Index);

    Type *ElemTy = STy->getElementType(Index);
    NeedsAnotherPass = NeedsAnotherPass || DoAnotherPass(ElemTy);

    PHINode *NewPhi = PHINode::Create(ElemTy, Phi->getNumIncomingValues(),
                                      Phi->getName() + ".index", Phi);
    CopyDebug(NewPhi, Phi);
    for (unsigned PhiIndex = 0; PhiIndex < Phi->getNumIncomingValues();
         ++PhiIndex) {
      BasicBlock *IncomingBB = Phi->getIncomingBlock(PhiIndex);
      Value *EV = CopyDebug(
          ExtractValueInst::Create(Phi->getIncomingValue(PhiIndex), EVIndexes,
                                   Phi->getName() + ".extract",
                                   IncomingBB->getTerminator()),
          Phi);
      NewPhi->addIncoming(EV, IncomingBB);
    }

    // Reconstruct the original struct value.
    NewStruct = CopyDebug(InsertValueInst::Create(NewStruct, NewPhi, EVIndexes,
                                                  Phi->getName() + ".insert",
                                                  NewStructInsertPt),
                          Phi);
  }
  Phi->replaceAllUsesWith(NewStruct);
  Phi->eraseFromParent();

  return NeedsAnotherPass;
}

static bool SplitUpSelect(SelectInst *Select) {
  StructType *STy = cast<StructType>(Select->getType());
  Value *NewStruct = UndefValue::get(STy);

  bool NeedsAnotherPass = false;
  // Create a separate SelectInst for each struct field.
  for (unsigned Index = 0; Index < STy->getNumElements(); ++Index) {
    SmallVector<unsigned, 1> EVIndexes;
    EVIndexes.push_back(Index);

    Value *TrueVal = CopyDebug(
        ExtractValueInst::Create(Select->getTrueValue(), EVIndexes,
                                 Select->getName() + ".extract", Select),
        Select);
    Value *FalseVal = CopyDebug(
        ExtractValueInst::Create(Select->getFalseValue(), EVIndexes,
                                 Select->getName() + ".extract", Select),
        Select);
    Value *NewSelect =
        CopyDebug(SelectInst::Create(Select->getCondition(), TrueVal, FalseVal,
                                     Select->getName() + ".index", Select),
                  Select);

    NeedsAnotherPass = NeedsAnotherPass || DoAnotherPass(NewSelect);

    // Reconstruct the original struct value.
    NewStruct = CopyDebug(
        InsertValueInst::Create(NewStruct, NewSelect, EVIndexes,
                                Select->getName() + ".insert", Select),
        Select);
  }
  Select->replaceAllUsesWith(NewStruct);
  Select->eraseFromParent();

  return NeedsAnotherPass;
}

template <class InstType>
static void ProcessLoadOrStoreAttrs(InstType *Dest, InstType *Src,
                                    StructType* STy, const unsigned Index,
                                    const DataLayout *DL) {
  CopyDebug(Dest, Src);
  Dest->setVolatile(Src->isVolatile());
  if (Src->isAtomic()) {
    errs() << "Use: " << *Src << "\n";
    report_fatal_error("Atomic struct loads/stores not supported");
  }

  if (!Src->getAlignment()) {
    return;
  }

  const StructLayout *SL = DL->getStructLayout(STy);
  const unsigned Alignment = Src->getAlignment();
  Dest->setAlignment(Align(MinAlign(Alignment, SL->getElementOffset(Index))));
}

template <class InstType>
static void ProcessArrayLoadOrStoreAttrs(InstType *Dest, InstType *Src,
                                         ArrayType* ATy, const unsigned Index,
                                         const DataLayout *DL) {
  CopyDebug(Dest, Src);
  Dest->setVolatile(Src->isVolatile());
  if (Src->isAtomic()) {
    errs() << "Use: " << *Src << "\n";
    report_fatal_error("Atomic struct loads/stores not supported");
  }

  if (!Src->getAlignment()) {
    return;
  }

  const unsigned Alignment = Src->getAlignment();
  Dest->setAlignment(Align(MinAlign(Alignment, Index * DL->getTypeSizeInBits(ATy->getElementType()))));
}

static bool SplitUpStore(StoreInst *Store, const DataLayout *DL) {
  StructType *STy = cast<StructType>(Store->getValueOperand()->getType());

  bool NeedsAnotherPass = false;
  // Create a separate store instruction for each struct field.
  for (unsigned Index = 0; Index < STy->getNumElements(); ++Index) {
    SmallVector<Value *, 2> Indexes;
    Indexes.push_back(ConstantInt::get(Store->getContext(), APInt(32, 0)));
    Indexes.push_back(ConstantInt::get(Store->getContext(), APInt(32, Index)));
    Value *GEP =
        CopyDebug(GetElementPtrInst::Create(STy,
                      Store->getPointerOperand(), Indexes,
                      Store->getPointerOperand()->getName() + ".index", Store),
                  Store);
    NeedsAnotherPass =
        NeedsAnotherPass || DoAnotherPass(GEP->getType()->getContainedType(0));

    SmallVector<unsigned, 1> EVIndexes;
    EVIndexes.push_back(Index);
    Value *Field = ExtractValueInst::Create(Store->getValueOperand(), EVIndexes,
                                            "", Store);
    StoreInst *NewStore = new StoreInst(Field, GEP, Store);
    ProcessLoadOrStoreAttrs(NewStore, Store, STy, Index, DL);
  }
  Store->eraseFromParent();

  return NeedsAnotherPass;
}

static bool SplitUpLoad(LoadInst *Load, const DataLayout *DL) {
  StructType *STy = cast<StructType>(Load->getType());
  Value *NewStruct = UndefValue::get(STy);

  bool NeedsAnotherPass = false;
  // Create a separate load instruction for each struct field.
  for (unsigned Index = 0; Index < STy->getNumElements(); ++Index) {
    SmallVector<Value *, 2> Indexes;
    Indexes.push_back(ConstantInt::get(Load->getContext(), APInt(32, 0)));
    Indexes.push_back(ConstantInt::get(Load->getContext(), APInt(32, Index)));
    Value *GEP =
        CopyDebug(GetElementPtrInst::Create(STy, Load->getPointerOperand(), Indexes,
                                            Load->getName() + ".index", Load),
                  Load);
    LoadInst *NewLoad = new LoadInst(STy->getElementType(Index), GEP, Load->getName() + ".field", Load);

    NeedsAnotherPass = NeedsAnotherPass || DoAnotherPass(NewLoad);
    ProcessLoadOrStoreAttrs(NewLoad, Load, STy, Index, DL);

    // Reconstruct the struct value.
    SmallVector<unsigned, 1> EVIndexes;
    EVIndexes.push_back(Index);
    NewStruct =
        CopyDebug(InsertValueInst::Create(NewStruct, NewLoad, EVIndexes,
                                          Load->getName() + ".insert", Load),
                  Load);
  }
  Load->replaceAllUsesWith(NewStruct);
  Load->eraseFromParent();

  return NeedsAnotherPass;
}

static bool SplitUpArrayStore(StoreInst *Store, const DataLayout *DL) {
  ArrayType *ATy = cast<ArrayType>(Store->getValueOperand()->getType());

  bool NeedsAnotherPass = false;
  // Create a separate store instruction for each struct field.
  for (unsigned Index = 0; Index < ATy->getNumElements(); ++Index) {
    SmallVector<Value *, 2> Indexes;
    Indexes.push_back(ConstantInt::get(Store->getContext(), APInt(32, 0)));
    Indexes.push_back(ConstantInt::get(Store->getContext(), APInt(32, Index)));
    Value *GEP =
        CopyDebug(GetElementPtrInst::Create(ATy,
                      Store->getPointerOperand(), Indexes,
                      Store->getPointerOperand()->getName() + ".index", Store),
                  Store);
    NeedsAnotherPass =
        NeedsAnotherPass || DoAnotherPass(GEP->getType()->getContainedType(0));

    SmallVector<unsigned, 1> EVIndexes;
    EVIndexes.push_back(Index);
    Value *Field = ExtractValueInst::Create(Store->getValueOperand(), EVIndexes,
                                            "", Store);
    StoreInst *NewStore = new StoreInst(Field, GEP, Store);
    ProcessArrayLoadOrStoreAttrs(NewStore, Store, ATy, Index, DL);
  }
  Store->eraseFromParent();

  return NeedsAnotherPass;
}

static bool SplitUpArrayLoad(LoadInst *Load, const DataLayout *DL) {
  ArrayType *ATy = cast<ArrayType>(Load->getType());
  Value *NewStruct = UndefValue::get(ATy);

  bool NeedsAnotherPass = false;
  // Create a separate load instruction for each struct field.
  for (unsigned Index = 0; Index < ATy->getNumElements(); ++Index) {
    SmallVector<Value *, 2> Indexes;
    Indexes.push_back(ConstantInt::get(Load->getContext(), APInt(32, 0)));
    Indexes.push_back(ConstantInt::get(Load->getContext(), APInt(32, Index)));
    Value *GEP =
        CopyDebug(GetElementPtrInst::Create(ATy, Load->getPointerOperand(), Indexes,
                                            Load->getName() + ".index", Load),
                  Load);
    LoadInst *NewLoad = new LoadInst(ATy->getElementType(), GEP, Load->getName() + ".field", Load);

    NeedsAnotherPass = NeedsAnotherPass || DoAnotherPass(NewLoad);
    ProcessArrayLoadOrStoreAttrs(NewLoad, Load, ATy, Index, DL);

    // Reconstruct the struct value.
    SmallVector<unsigned, 1> EVIndexes;
    EVIndexes.push_back(Index);
    NewStruct =
        CopyDebug(InsertValueInst::Create(NewStruct, NewLoad, EVIndexes,
                                          Load->getName() + ".insert", Load),
                  Load);
  }
  Load->replaceAllUsesWith(NewStruct);
  Load->eraseFromParent();

  return NeedsAnotherPass;
}

static bool ExpandExtractValue(ExtractValueInst *EV,
                               SmallVectorImpl<Instruction *> *ToErase) {
  // Search for the insertvalue instruction that inserts the struct field
  // referenced by this extractvalue instruction, excluding CmpXchg which
  // returns a struct and is handled by RewriteAtomics.
  Value *StructVal = EV->getAggregateOperand();
  Value *ResultField = nullptr;

  // The current depth of the search. It's impossible to backtrack in our search
  // tree (all prior (not in the CFG sense) extractvalues will already be
  // expanded), so this variable is never reset to zero.
  size_t EVIndex = 0;

  // Some intrinsics and cmpxchg returns struct vals and this pass can't do
  // anything but ignore them.
  if (isa<IntrinsicInst>(StructVal) || isa<AtomicCmpXchgInst>(StructVal))
    return false;

  for (;;) {
    LLVM_DEBUG(dbgs() << "Expanding struct value: " << *StructVal << "\n");

    if (InsertValueInst *IV = dyn_cast<InsertValueInst>(StructVal)) {

      size_t IVIndex = 0;
      for (; EVIndex < EV->getIndices().size() &&
                 IVIndex < IV->getIndices().size();
           ++IVIndex, ++EVIndex) {

        const bool Equal =
            (EV->getIndices()[EVIndex] == IV->getIndices()[IVIndex]);

        if (IVIndex + 1 == IV->getIndices().size() && Equal) {
          if (EVIndex + 1 == EV->getIndices().size()) {
            // Exact match. We break out of all loops and ResultField will
            // replace EV.
            ResultField = IV->getInsertedValueOperand();
          } else {
            // We've found a match, but haven't reached the end of EV's indexes.
            // We continue looping through the outermost loop, and search for
            // indices on the next level down (ie we increment EVIndex).
            // This branch is common when encountering nested insertvalues; for
            // example:
            // ```llvm
            // %1 = insertvalue { i32 } undef, i32 1, 0
            // %2 = insertvalue { { i32 } } %1, { i32 } %1, 0
            // %3 = extractvalue { { i32 } } %2, 0, 0
            // ```
            StructVal = IV->getInsertedValueOperand();
            ++EVIndex;
          }
          break;
        } else if (!Equal) {
          // No match.  Try the next struct value in the chain.
          // For example:
          // ```llvm
          // %1 = insertvalue { i32, i32, i32 } undef, i32 5, 0
          // %2 = insertvalue { i32, i32, i32 } %1, i32 10, 1
          // %3 = insertvalue { i32, i32, i32 } %2, i32 15, 2
          // %4 = extractvalue { i32, i32, i32 } %3, 0
          // ```
          // In this case, to expand %4, this branch will hit insertvalues %3
          // and %2 before
          // it finds the solution, %1.
          StructVal = IV->getAggregateOperand();
          break;
        }

        // One last case worth mentioning:
        // ```llvm
        // %aa = alloca { i32 }
        // %a = insertvalue { i32 } undef, i32 1, 0
        // %b = insertvalue { { i32 } } undef, { i32 } %a, 0
        // %c = extractvalue { { i32 } } %b, 0
        // store { i32 } %c, { i32 }* %aa
        // ```
        // In the case of %c, the condition of our inner loop will be false, and
        // we will fall into (EVIndex == EV->getIndices().size())
        // Note that in this case, SplitStore will have inserted an extra
        // extractvalue and GEP:
        // ```llvm
        // %aa = alloca { i32 }
        // %a = insertvalue { i32 } undef, i32 1, 0
        // %b = insertvalue { { i32 } } undef, { i32 } %a, 0
        // %c.extractval = extractvalue { i32 } %a, 0
        // %aa.index = getelementptr { i32 }* %aa, i32 0, i32 0
        // store i32 %c, i32* %aa.index
        // ```
      }
      if (ResultField) {
        // \O/ We're done with this ExtractValueInst!
        break;
      } else if (EVIndex == EV->getIndices().size()) {
        // We've found an insertvalue that inserts at one or more levels deeper
        // than this extractvalue. For example (borrowed from the tests), where
        // %h is EV && %e is IV:
        // ```llvm
        // %e = insertvalue { { { i32, i64 } }, i64 } undef, { i32, i64 } %b, 0, 0
        // %h = extractvalue { { { i32, i64 } }, i64 } %e, 0
        // ; later on..
        // %1 = extractvalue { { i32, i64 } } %h, 0
        // ```
        // This expands to:
        // ```llvm
        // %e = insertvalue { { { i32, i64 } }, i64 } undef, { i32, i64 } %b, 0, 0
        // %1 = insertvalue { { i32, i64 } } undef, { i32, i64 } %b, 0
        // %h = extractvalue { { { i32, i64 } }, i64 } %e, 0
        // %2 = extractvalue { { i32, i64 } } %h, 0
        // ```
        // Then, outside the outer loop, %h is deleted:
        // ```llvm
        // %e = insertvalue { { { i32, i64 } }, i64 } undef, { i32, i64 } %b, 0, 0
        // %1 = insertvalue { { i32, i64 } } undef, { i32, i64 } %b, 0
        // %2 = extractvalue { { i32, i64 } } %1, 0
        // ```
        // %2 will be expanded at a later point.
        // This branch used the second index in %e to create %1 (because %2 &&
        // %e's first indices where equal).
        //
        // Additionally, it's impossible to not change StructVal && not hit this
        // branch (but the reverse is not true!).

        SmallVector<unsigned, 4> Indices(IV->getIndices().begin() + IVIndex,
                                         IV->getIndices().end());

        InsertValueInst *Insert = InsertValueInst::Create(
            UndefValue::get(EV->getType()), IV->getInsertedValueOperand(),
            Indices, "", EV);
        ToErase->push_back(Insert);
        ResultField = CopyDebug(Insert, EV);
        break;
      }

      // At this point, StructVal must be changed.
    } else if (Constant *C = dyn_cast<Constant>(StructVal)) {
      SmallVector<unsigned, 4> Indices(EV->getIndices().begin() + EVIndex,
                                       EV->getIndices().end());
      ResultField = ConstantExpr::getExtractValue(C, Indices);
      break;
    } else if (isa<LoadInst>(StructVal)) {
      ResultField = StructVal;
      break;
    } else {
      errs() << "Value: " << *StructVal << "\n";
      report_fatal_error("Unrecognized struct value");
    }
  }

  assert(ResultField); // Failsafe.
  EV->replaceAllUsesWith(ResultField);
  EV->eraseFromParent();
  return true;
}

static bool ExpandExtractValues(Function &Func, bool Finalize) {
  bool Changed = false;

  SmallVector<Instruction *, 10> ToErase;
  // Expand out all the extractvalue instructions.  Also collect up
  // the insertvalue instructions for later deletion so that we do not
  // need to make extra passes across the whole function.

  for (auto &BB : Func) {
    for (BasicBlock::iterator Iter = BB.begin(), E = BB.end(); Iter != E;) {
      Instruction *Inst = &*Iter++;
      if (ExtractValueInst *EV = dyn_cast<ExtractValueInst>(Inst)) {
        Changed |= ExpandExtractValue(EV, &ToErase);
      } else if (isa<InsertValueInst>(Inst)) {
        ToErase.push_back(Inst);
        Changed = true;
      }
    }
  }

  if (Finalize) {
    // Delete the insertvalue instructions. These can reference each
    // other, so we must do dropAllReferences() before doing
    // eraseFromParent(), otherwise we will try to erase instructions
    // that are still referenced.
    for (Instruction *I : ToErase) {
      I->dropAllReferences();
    }

    for (Instruction *I : ToErase) {
      I->eraseFromParent();
    }
  }

  return Changed;
}

bool ExpandStructRegs::runOnFunction(Function &Func) {
  bool Changed = false;
  DL = &Func.getParent()->getDataLayout();

  auto SplitUpInstructions = [&]() {
    bool NeedsAnotherPass;
    do {
      NeedsAnotherPass = false;
      // Split up aggregate loads, stores and phi nodes into operations on
      // scalar types.  This inserts extractvalue and insertvalue
      // instructions which we will expand out later.
      for (Function::iterator BB = Func.begin(), E = Func.end(); BB != E; ++BB) {
        for (BasicBlock::iterator Iter = BB->begin(), E = BB->end(); Iter != E;) {
          Instruction *Inst = &*Iter++;
          if (StoreInst *Store = dyn_cast<StoreInst>(Inst)) {
            if (Store->getValueOperand()->getType()->isStructTy()) {
              NeedsAnotherPass |= SplitUpStore(Store, DL);
              Changed = true;
            } else if (Store->getValueOperand()->getType()->isArrayTy()) {
              NeedsAnotherPass |= SplitUpArrayStore(Store, DL);
              Changed = true;
            }
          } else if (LoadInst *Load = dyn_cast<LoadInst>(Inst)) {
            if (Load->getType()->isStructTy()) {
              NeedsAnotherPass |= SplitUpLoad(Load, DL);
              Changed = true;
            } else if (Load->getType()->isArrayTy()) {
              NeedsAnotherPass |= SplitUpArrayLoad(Load, DL);
              Changed = true;
            }
          } else if (PHINode *Phi = dyn_cast<PHINode>(Inst)) {
            if (Phi->getType()->isStructTy()) {
              NeedsAnotherPass |= SplitUpPHINode(Phi);
              Changed = true;
            }
          } else if (SelectInst *Select = dyn_cast<SelectInst>(Inst)) {
            if (Select->getType()->isStructTy()) {
              NeedsAnotherPass |= SplitUpSelect(Select);
              Changed = true;
            }
          }
        }
      }
    } while (NeedsAnotherPass);
  };

  SplitUpInstructions();
  Changed |= ExpandExtractValues(Func, false);

  if (Changed) {
    // insertvalues that receive insertvalues may require additional splitting
    // and expansion.
    // TODO: do we need an arbitrary amount of such passes?
    SplitUpInstructions();
    ExpandExtractValues(Func, true);
  }

  return Changed;
}

char ExpandStructRegs::ID = 0;

FunctionPass *llvm::createExpandStructRegs() {
  return new ExpandStructRegs();
}

INITIALIZE_PASS(ExpandStructRegs, "ExpandStructRegs",
              "Expand out variables with struct types", false, false)
