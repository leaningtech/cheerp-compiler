/*===- TableGen'erated file -------------------------------------*- C++ -*-===*\
|*                                                                            *|
|* Subtarget Enumeration Source Fragment                                      *|
|*                                                                            *|
|* Automatically generated file, do not edit!                                 *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*/


#ifdef GET_SUBTARGETINFO_ENUM
#undef GET_SUBTARGETINFO_ENUM

namespace llvm {
namespace WebAssembly {
enum {
  FeatureAtomics = 0,
  FeatureBulkMemory = 1,
  FeatureExceptionHandling = 2,
  FeatureExtendedConst = 3,
  FeatureMultivalue = 4,
  FeatureMutableGlobals = 5,
  FeatureNontrappingFPToInt = 6,
  FeatureReferenceTypes = 7,
  FeatureRelaxedSIMD = 8,
  FeatureSIMD128 = 9,
  FeatureSignExt = 10,
  FeatureTailCall = 11,
  NumSubtargetFeatures = 12
};
} // end namespace WebAssembly
} // end namespace llvm

#endif // GET_SUBTARGETINFO_ENUM


#ifdef GET_SUBTARGETINFO_MACRO
GET_SUBTARGETINFO_MACRO(HasAtomics, false, hasAtomics)
GET_SUBTARGETINFO_MACRO(HasBulkMemory, false, hasBulkMemory)
GET_SUBTARGETINFO_MACRO(HasExceptionHandling, false, hasExceptionHandling)
GET_SUBTARGETINFO_MACRO(HasExtendedConst, false, hasExtendedConst)
GET_SUBTARGETINFO_MACRO(HasMultivalue, false, hasMultivalue)
GET_SUBTARGETINFO_MACRO(HasMutableGlobals, false, hasMutableGlobals)
GET_SUBTARGETINFO_MACRO(HasNontrappingFPToInt, false, hasNontrappingFPToInt)
GET_SUBTARGETINFO_MACRO(HasReferenceTypes, false, hasReferenceTypes)
GET_SUBTARGETINFO_MACRO(HasSignExt, false, hasSignExt)
GET_SUBTARGETINFO_MACRO(HasTailCall, false, hasTailCall)
#undef GET_SUBTARGETINFO_MACRO
#endif // GET_SUBTARGETINFO_MACRO


#ifdef GET_SUBTARGETINFO_MC_DESC
#undef GET_SUBTARGETINFO_MC_DESC

namespace llvm {
// Sorted (by key) array of values for CPU features.
extern const llvm::SubtargetFeatureKV WebAssemblyFeatureKV[] = {
  { "atomics", "Enable Atomics", WebAssembly::FeatureAtomics, { { { 0x0ULL, 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "bulk-memory", "Enable bulk memory operations", WebAssembly::FeatureBulkMemory, { { { 0x0ULL, 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "exception-handling", "Enable Wasm exception handling", WebAssembly::FeatureExceptionHandling, { { { 0x0ULL, 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "extended-const", "Enable extended const expressions", WebAssembly::FeatureExtendedConst, { { { 0x0ULL, 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "multivalue", "Enable multivalue blocks, instructions, and functions", WebAssembly::FeatureMultivalue, { { { 0x0ULL, 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "mutable-globals", "Enable mutable globals", WebAssembly::FeatureMutableGlobals, { { { 0x0ULL, 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "nontrapping-fptoint", "Enable non-trapping float-to-int conversion operators", WebAssembly::FeatureNontrappingFPToInt, { { { 0x0ULL, 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "reference-types", "Enable reference types", WebAssembly::FeatureReferenceTypes, { { { 0x0ULL, 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "relaxed-simd", "Enable relaxed-simd instructions", WebAssembly::FeatureRelaxedSIMD, { { { 0x0ULL, 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "sign-ext", "Enable sign extension operators", WebAssembly::FeatureSignExt, { { { 0x0ULL, 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "simd128", "Enable 128-bit SIMD", WebAssembly::FeatureSIMD128, { { { 0x0ULL, 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "tail-call", "Enable tail call instructions", WebAssembly::FeatureTailCall, { { { 0x0ULL, 0x0ULL, 0x0ULL, 0x0ULL, } } } },
};

#ifdef DBGFIELD
#error "<target>GenSubtargetInfo.inc requires a DBGFIELD macro"
#endif
#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
#define DBGFIELD(x) x,
#else
#define DBGFIELD(x)
#endif

// ===============================================================
// Data tables for the new per-operand machine model.

// {ProcResourceIdx, Cycles}
extern const llvm::MCWriteProcResEntry WebAssemblyWriteProcResTable[] = {
  { 0,  0}, // Invalid
}; // WebAssemblyWriteProcResTable

// {Cycles, WriteResourceID}
extern const llvm::MCWriteLatencyEntry WebAssemblyWriteLatencyTable[] = {
  { 0,  0}, // Invalid
}; // WebAssemblyWriteLatencyTable

// {UseIdx, WriteResourceID, Cycles}
extern const llvm::MCReadAdvanceEntry WebAssemblyReadAdvanceTable[] = {
  {0,  0,  0}, // Invalid
}; // WebAssemblyReadAdvanceTable

#undef DBGFIELD

static const llvm::MCSchedModel NoSchedModel = {
  MCSchedModel::DefaultIssueWidth,
  MCSchedModel::DefaultMicroOpBufferSize,
  MCSchedModel::DefaultLoopMicroOpBufferSize,
  MCSchedModel::DefaultLoadLatency,
  MCSchedModel::DefaultHighLatency,
  MCSchedModel::DefaultMispredictPenalty,
  false, // PostRAScheduler
  false, // CompleteModel
  0, // Processor ID
  nullptr, nullptr, 0, 0, // No instruction-level machine model.
  nullptr, // No Itinerary
  nullptr // No extra processor descriptor
};

// Sorted (by key) array of values for CPU subtype.
extern const llvm::SubtargetSubTypeKV WebAssemblySubTypeKV[] = {
 { "bleeding-edge", { { { 0xe63ULL, 0x0ULL, 0x0ULL, 0x0ULL, } } }, { { { 0x0ULL, 0x0ULL, 0x0ULL, 0x0ULL, } } }, &NoSchedModel },
 { "generic", { { { 0x420ULL, 0x0ULL, 0x0ULL, 0x0ULL, } } }, { { { 0x0ULL, 0x0ULL, 0x0ULL, 0x0ULL, } } }, &NoSchedModel },
 { "mvp", { { { 0x0ULL, 0x0ULL, 0x0ULL, 0x0ULL, } } }, { { { 0x0ULL, 0x0ULL, 0x0ULL, 0x0ULL, } } }, &NoSchedModel },
};

namespace WebAssembly_MC {
unsigned resolveVariantSchedClassImpl(unsigned SchedClass,
    const MCInst *MI, const MCInstrInfo *MCII, unsigned CPUID) {
  // Don't know how to resolve this scheduling class.
  return 0;
}
} // end namespace WebAssembly_MC

struct WebAssemblyGenMCSubtargetInfo : public MCSubtargetInfo {
  WebAssemblyGenMCSubtargetInfo(const Triple &TT,
    StringRef CPU, StringRef TuneCPU, StringRef FS,
    ArrayRef<SubtargetFeatureKV> PF,
    ArrayRef<SubtargetSubTypeKV> PD,
    const MCWriteProcResEntry *WPR,
    const MCWriteLatencyEntry *WL,
    const MCReadAdvanceEntry *RA, const InstrStage *IS,
    const unsigned *OC, const unsigned *FP) :
      MCSubtargetInfo(TT, CPU, TuneCPU, FS, PF, PD,
                      WPR, WL, RA, IS, OC, FP) { }

  unsigned resolveVariantSchedClass(unsigned SchedClass,
      const MCInst *MI, const MCInstrInfo *MCII,
      unsigned CPUID) const override {
    return WebAssembly_MC::resolveVariantSchedClassImpl(SchedClass, MI, MCII, CPUID);
  }
};

static inline MCSubtargetInfo *createWebAssemblyMCSubtargetInfoImpl(const Triple &TT, StringRef CPU, StringRef TuneCPU, StringRef FS) {
  return new WebAssemblyGenMCSubtargetInfo(TT, CPU, TuneCPU, FS, WebAssemblyFeatureKV, WebAssemblySubTypeKV, 
                      WebAssemblyWriteProcResTable, WebAssemblyWriteLatencyTable, WebAssemblyReadAdvanceTable, 
                      nullptr, nullptr, nullptr);
}

} // end namespace llvm

#endif // GET_SUBTARGETINFO_MC_DESC


#ifdef GET_SUBTARGETINFO_TARGET_DESC
#undef GET_SUBTARGETINFO_TARGET_DESC

#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

// ParseSubtargetFeatures - Parses features string setting specified
// subtarget options.
void llvm::WebAssemblySubtarget::ParseSubtargetFeatures(StringRef CPU, StringRef TuneCPU, StringRef FS) {
  LLVM_DEBUG(dbgs() << "\nFeatures:" << FS);
  LLVM_DEBUG(dbgs() << "\nCPU:" << CPU);
  LLVM_DEBUG(dbgs() << "\nTuneCPU:" << TuneCPU << "\n\n");
  InitMCProcessorInfo(CPU, TuneCPU, FS);
  const FeatureBitset &Bits = getFeatureBits();
  if (Bits[WebAssembly::FeatureAtomics]) HasAtomics = true;
  if (Bits[WebAssembly::FeatureBulkMemory]) HasBulkMemory = true;
  if (Bits[WebAssembly::FeatureExceptionHandling]) HasExceptionHandling = true;
  if (Bits[WebAssembly::FeatureExtendedConst]) HasExtendedConst = true;
  if (Bits[WebAssembly::FeatureMultivalue]) HasMultivalue = true;
  if (Bits[WebAssembly::FeatureMutableGlobals]) HasMutableGlobals = true;
  if (Bits[WebAssembly::FeatureNontrappingFPToInt]) HasNontrappingFPToInt = true;
  if (Bits[WebAssembly::FeatureReferenceTypes]) HasReferenceTypes = true;
  if (Bits[WebAssembly::FeatureRelaxedSIMD] && SIMDLevel < RelaxedSIMD) SIMDLevel = RelaxedSIMD;
  if (Bits[WebAssembly::FeatureSIMD128] && SIMDLevel < SIMD128) SIMDLevel = SIMD128;
  if (Bits[WebAssembly::FeatureSignExt]) HasSignExt = true;
  if (Bits[WebAssembly::FeatureTailCall]) HasTailCall = true;
}
#endif // GET_SUBTARGETINFO_TARGET_DESC


#ifdef GET_SUBTARGETINFO_HEADER
#undef GET_SUBTARGETINFO_HEADER

namespace llvm {
class DFAPacketizer;
namespace WebAssembly_MC {
unsigned resolveVariantSchedClassImpl(unsigned SchedClass, const MCInst *MI, const MCInstrInfo *MCII, unsigned CPUID);
} // end namespace WebAssembly_MC

struct WebAssemblyGenSubtargetInfo : public TargetSubtargetInfo {
  explicit WebAssemblyGenSubtargetInfo(const Triple &TT, StringRef CPU, StringRef TuneCPU, StringRef FS);
public:
  unsigned resolveSchedClass(unsigned SchedClass,  const MachineInstr *DefMI, const TargetSchedModel *SchedModel) const override;
  unsigned resolveVariantSchedClass(unsigned SchedClass, const MCInst *MI, const MCInstrInfo *MCII, unsigned CPUID) const override;
  DFAPacketizer *createDFAPacketizer(const InstrItineraryData *IID) const;
};
} // end namespace llvm

#endif // GET_SUBTARGETINFO_HEADER


#ifdef GET_SUBTARGETINFO_CTOR
#undef GET_SUBTARGETINFO_CTOR

#include "llvm/CodeGen/TargetSchedule.h"

namespace llvm {
extern const llvm::SubtargetFeatureKV WebAssemblyFeatureKV[];
extern const llvm::SubtargetSubTypeKV WebAssemblySubTypeKV[];
extern const llvm::MCWriteProcResEntry WebAssemblyWriteProcResTable[];
extern const llvm::MCWriteLatencyEntry WebAssemblyWriteLatencyTable[];
extern const llvm::MCReadAdvanceEntry WebAssemblyReadAdvanceTable[];
WebAssemblyGenSubtargetInfo::WebAssemblyGenSubtargetInfo(const Triple &TT, StringRef CPU, StringRef TuneCPU, StringRef FS)
  : TargetSubtargetInfo(TT, CPU, TuneCPU, FS, makeArrayRef(WebAssemblyFeatureKV, 12), makeArrayRef(WebAssemblySubTypeKV, 3), 
                        WebAssemblyWriteProcResTable, WebAssemblyWriteLatencyTable, WebAssemblyReadAdvanceTable, 
                        nullptr, nullptr, nullptr) {}

unsigned WebAssemblyGenSubtargetInfo
::resolveSchedClass(unsigned SchedClass, const MachineInstr *MI, const TargetSchedModel *SchedModel) const {
  report_fatal_error("Expected a variant SchedClass");
} // WebAssemblyGenSubtargetInfo::resolveSchedClass

unsigned WebAssemblyGenSubtargetInfo
::resolveVariantSchedClass(unsigned SchedClass, const MCInst *MI, const MCInstrInfo *MCII, unsigned CPUID) const {
  return WebAssembly_MC::resolveVariantSchedClassImpl(SchedClass, MI, MCII, CPUID);
} // WebAssemblyGenSubtargetInfo::resolveVariantSchedClass

} // end namespace llvm

#endif // GET_SUBTARGETINFO_CTOR


#ifdef GET_STIPREDICATE_DECLS_FOR_MC_ANALYSIS
#undef GET_STIPREDICATE_DECLS_FOR_MC_ANALYSIS

#endif // GET_STIPREDICATE_DECLS_FOR_MC_ANALYSIS


#ifdef GET_STIPREDICATE_DEFS_FOR_MC_ANALYSIS
#undef GET_STIPREDICATE_DEFS_FOR_MC_ANALYSIS

#endif // GET_STIPREDICATE_DEFS_FOR_MC_ANALYSIS

