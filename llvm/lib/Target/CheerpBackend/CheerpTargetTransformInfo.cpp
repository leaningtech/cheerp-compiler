#include "CheerpTargetTransformInfo.h"
#include "llvm/CodeGen/CostTable.h"
#include "llvm/Support/Debug.h"
using namespace llvm;

#define DEBUG_TYPE "wasmtti"

TargetTransformInfo::PopcntSupportKind
CheerpTTIImpl::getPopcntSupport(unsigned TyWidth) const {
  assert(isPowerOf2_32(TyWidth) && "Ty width must be power of 2");
  return TargetTransformInfo::PSK_FastHardware;
}

unsigned CheerpTTIImpl::getNumberOfRegisters(unsigned ClassID) const {
  unsigned Result = BaseT::getNumberOfRegisters(ClassID);

  // For SIMD, use at least 16 registers, as a rough guess.
  bool Vector = (ClassID == 1);
  if (Vector)
    Result = std::max(Result, 16u);

  return Result;
}

TypeSize CheerpTTIImpl::getRegisterBitWidth(
    TargetTransformInfo::RegisterKind K) const {
  switch (K) {
  case TargetTransformInfo::RGK_Scalar:
    return TypeSize::getFixed(64);
  case TargetTransformInfo::RGK_FixedWidthVector:
    return TypeSize::getFixed(128);
  case TargetTransformInfo::RGK_ScalableVector:
    return TypeSize::getScalable(0);
  }

  llvm_unreachable("Unsupported register kind");
}

InstructionCost CheerpTTIImpl::getArithmeticInstrCost(
    unsigned Opcode, Type *Ty, TTI::TargetCostKind CostKind,
    TTI::OperandValueKind Opd1Info, TTI::OperandValueKind Opd2Info,
    TTI::OperandValueProperties Opd1PropInfo,
    TTI::OperandValueProperties Opd2PropInfo, ArrayRef<const Value *> Args,
    const Instruction *CxtI) {

  InstructionCost Cost =
      BasicTTIImplBase<CheerpTTIImpl>::getArithmeticInstrCost(
          Opcode, Ty, CostKind, Opd1Info, Opd2Info, Opd1PropInfo, Opd2PropInfo);

  if (auto *VTy = dyn_cast<VectorType>(Ty)) {
    switch (Opcode) {
    case Instruction::LShr:
    case Instruction::AShr:
    case Instruction::Shl:
      // SIMD128's shifts currently only accept a scalar shift count. For each
      // element, we'll need to extract, op, insert. The following is a rough
      // approxmation.
      if (Opd2Info != TTI::OK_UniformValue &&
          Opd2Info != TTI::OK_UniformConstantValue)
        Cost =
            cast<FixedVectorType>(VTy)->getNumElements() *
            (TargetTransformInfo::TCC_Basic +
             getArithmeticInstrCost(Opcode, VTy->getElementType(), CostKind) +
             TargetTransformInfo::TCC_Basic);
      break;
    }
  }
  return Cost;
}

InstructionCost CheerpTTIImpl::getVectorInstrCost(unsigned Opcode,
                                                       Type *Val,
                                                       unsigned Index) {
  InstructionCost Cost =
      BasicTTIImplBase::getVectorInstrCost(Opcode, Val, Index);

  // SIMD128's insert/extract currently only take constant indices.
  if (Index == -1u)
    return Cost + 25 * TargetTransformInfo::TCC_Expensive;

  return Cost;
}

bool CheerpTTIImpl::areInlineCompatible(const Function *Caller,
                                             const Function *Callee) const {
  // Allow inlining only when the Callee has a subset of the Caller's
  // features. In principle, we should be able to inline regardless of any
  // features because WebAssembly supports features at module granularity, not
  // function granularity, but without this restriction it would be possible for
  // a module to "forget" about features if all the functions that used them
  // were inlined.
  const TargetMachine &TM = getTLI()->getTargetMachine();

  const FeatureBitset &CallerBits =
      TM.getSubtargetImpl(*Caller)->getFeatureBits();
  const FeatureBitset &CalleeBits =
      TM.getSubtargetImpl(*Callee)->getFeatureBits();

  return (CallerBits & CalleeBits) == CalleeBits;
}

void CheerpTTIImpl::getUnrollingPreferences(
    Loop *L, ScalarEvolution &SE, TTI::UnrollingPreferences &UP,
    OptimizationRemarkEmitter *ORE) const {
  // Scan the loop: don't unroll loops with calls. This is a standard approach
  // for most (all?) targets.
  for (BasicBlock *BB : L->blocks())
    for (Instruction &I : *BB)
      if (isa<CallInst>(I) || isa<InvokeInst>(I))
        if (const Function *F = cast<CallBase>(I).getCalledFunction())
          if (isLoweredToCall(F))
            return;

  // The chosen threshold is within the range of 'LoopMicroOpBufferSize' of
  // the various microarchitectures that use the BasicTTI implementation and
  // has been selected through heuristics across multiple cores and runtimes.
  UP.Partial = UP.Runtime = UP.UpperBound = true;
  UP.PartialThreshold = 30;

  // Avoid unrolling when optimizing for size.
  UP.OptSizeThreshold = 0;
  UP.PartialOptSizeThreshold = 0;

  // Set number of instructions optimized when "back edge"
  // becomes "fall through" to default value of 2.
  UP.BEInsns = 2;
}
