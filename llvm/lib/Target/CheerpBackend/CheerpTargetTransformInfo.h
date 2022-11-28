//===-- CheerpTargetTransformInfo.h - TargetTransformInfo for the CheerpBackend -------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2022 Leaning Technlogies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_TARGETTRANSFORMINFO_H
#define _CHEERP_TARGETTRANSFORMINFO_H

#include "CheerpTargetMachine.h"
#include "llvm/CodeGen/BasicTTIImpl.h"
#include "llvm/CodeGen/TargetLowering.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include <algorithm>

namespace llvm {

class CheerpTTIImpl final : public BasicTTIImplBase<CheerpTTIImpl> {
  typedef BasicTTIImplBase<CheerpTTIImpl> BaseT;
  typedef TargetTransformInfo TTI;
  friend BaseT;

  const CheerpSubtarget *ST;
  const CheerpTargetLowering *TLI;

  const CheerpSubtarget *getST() const { return ST; }
  const CheerpTargetLowering *getTLI() const { return TLI; }

public:
  CheerpTTIImpl(const CheerpTargetMachine *TM, const Function &F)
      : BaseT(TM, F.getParent()->getDataLayout()), ST(TM->getSubtargetImpl(F)),
        TLI(ST->getTargetLowering()) {}

  unsigned getNumberOfRegisters(unsigned ClassID) const;
  TypeSize getRegisterBitWidth(TargetTransformInfo::RegisterKind K) const;
  InstructionCost getArithmeticInstrCost(
      unsigned Opcode, Type *Ty, TTI::TargetCostKind CostKind,
      TTI::OperandValueInfo Opd1Info = {TTI::OK_AnyValue, TTI::OP_None},
      TTI::OperandValueInfo Opd2Info = {TTI::OK_AnyValue, TTI::OP_None},
      ArrayRef<const Value *> Args = ArrayRef<const Value *>(),
      const Instruction *CxtI = nullptr);
  InstructionCost getVectorInstrCost(unsigned Opcode, Type *Val,
                                     unsigned Index);
  InstructionCost getVectorInstrCost(const Instruction& I, Type *Val,
                                     unsigned Index);

  bool areInlineCompatible(const Function *Caller,
                           const Function *Callee) const;
};

} // end namespace llvm

#endif
