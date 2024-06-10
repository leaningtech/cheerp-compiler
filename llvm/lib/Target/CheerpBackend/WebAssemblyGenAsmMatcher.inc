/*===- TableGen'erated file -------------------------------------*- C++ -*-===*\
|*                                                                            *|
|* Assembly Matcher Source Fragment                                           *|
|*                                                                            *|
|* Automatically generated file, do not edit!                                 *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*/


#ifdef GET_ASSEMBLER_HEADER
#undef GET_ASSEMBLER_HEADER
  // This should be included into the middle of the declaration of
  // your subclasses implementation of MCTargetAsmParser.
  FeatureBitset ComputeAvailableFeatures(const FeatureBitset &FB) const;
  void convertToMCInst(unsigned Kind, MCInst &Inst, unsigned Opcode,
                       const OperandVector &Operands);
  void convertToMapAndConstraints(unsigned Kind,
                           const OperandVector &Operands) override;
  unsigned MatchInstructionImpl(const OperandVector &Operands,
                                MCInst &Inst,
                                uint64_t &ErrorInfo,
                                FeatureBitset &MissingFeatures,
                                bool matchingInlineAsm,
                                unsigned VariantID = 0);
  unsigned MatchInstructionImpl(const OperandVector &Operands,
                                MCInst &Inst,
                                uint64_t &ErrorInfo,
                                bool matchingInlineAsm,
                                unsigned VariantID = 0) {
    FeatureBitset MissingFeatures;
    return MatchInstructionImpl(Operands, Inst, ErrorInfo, MissingFeatures,
                                matchingInlineAsm, VariantID);
  }

#endif // GET_ASSEMBLER_HEADER_INFO


#ifdef GET_OPERAND_DIAGNOSTIC_TYPES
#undef GET_OPERAND_DIAGNOSTIC_TYPES

#endif // GET_OPERAND_DIAGNOSTIC_TYPES


#ifdef GET_REGISTER_MATCHER
#undef GET_REGISTER_MATCHER

// Bits for subtarget features that participate in instruction matching.
enum SubtargetFeatureBits : uint8_t {
  Feature_HasSIMD128Bit = 8,
  Feature_HasRelaxedSIMDBit = 7,
  Feature_HasAtomicsBit = 0,
  Feature_HasMultivalueBit = 4,
  Feature_HasNontrappingFPToIntBit = 5,
  Feature_NotHasNontrappingFPToIntBit = 11,
  Feature_HasSignExtBit = 9,
  Feature_HasTailCallBit = 10,
  Feature_HasExceptionHandlingBit = 2,
  Feature_HasBulkMemoryBit = 1,
  Feature_HasReferenceTypesBit = 6,
  Feature_HasExtendedConstBit = 3,
};

#endif // GET_REGISTER_MATCHER


#ifdef GET_SUBTARGET_FEATURE_NAME
#undef GET_SUBTARGET_FEATURE_NAME

// User-level names for subtarget features that participate in
// instruction matching.
static const char *getSubtargetFeatureName(uint64_t Val) {
  switch(Val) {
  case Feature_HasSIMD128Bit: return "simd128";
  case Feature_HasRelaxedSIMDBit: return "relaxed-simd";
  case Feature_HasAtomicsBit: return "atomics";
  case Feature_HasMultivalueBit: return "multivalue";
  case Feature_HasNontrappingFPToIntBit: return "nontrapping-fptoint";
  case Feature_NotHasNontrappingFPToIntBit: return "nontrapping-fptoint";
  case Feature_HasSignExtBit: return "sign-ext";
  case Feature_HasTailCallBit: return "tail-call";
  case Feature_HasExceptionHandlingBit: return "exception-handling";
  case Feature_HasBulkMemoryBit: return "bulk-memory";
  case Feature_HasReferenceTypesBit: return "reference-types";
  case Feature_HasExtendedConstBit: return "extended-const";
  default: return "(unknown)";
  }
}

#endif // GET_SUBTARGET_FEATURE_NAME


#ifdef GET_MATCHER_IMPLEMENTATION
#undef GET_MATCHER_IMPLEMENTATION

static const uint8_t TiedAsmOperandTable[][3] = { /* empty  */ {0, 0, 0} };

namespace {
enum OperatorConversionKind {
  CVT_Done,
  CVT_Reg,
  CVT_Tied,
  CVT_imm_95_0,
  CVT_95_addImmOperands,
  CVT_95_addBrListOperands,
  CVT_95_addFPImmf32Operands,
  CVT_95_addFPImmf64Operands,
  CVT_NUM_CONVERTERS
};

enum InstructionConversionKind {
  Convert__imm_95_0,
  Convert__Imm1_0,
  Convert__BrList1_0,
  Convert__Imm1_0__Imm1_1,
  Convert_NoOperands,
  Convert__imm_95_0__imm_95_0,
  Convert__FPImmf321_0,
  Convert__Imm1_1__Imm1_0,
  Convert__FPImmf641_0,
  Convert__Imm1_0__Imm1_1__Imm1_2__Imm1_3__Imm1_4__Imm1_5__Imm1_6__Imm1_7__Imm1_8__Imm1_9__Imm1_10__Imm1_11__Imm1_12__Imm1_13__Imm1_14__Imm1_15,
  Convert__FPImmf641_0__FPImmf641_1,
  Convert__Imm1_0__Imm1_1__Imm1_2__Imm1_3,
  Convert__FPImmf321_0__FPImmf321_1__FPImmf321_2__FPImmf321_3,
  Convert__Imm1_0__Imm1_1__Imm1_2__Imm1_3__Imm1_4__Imm1_5__Imm1_6__Imm1_7,
  Convert__Imm1_1__Imm1_0__Imm1_2,
  CVT_NUM_SIGNATURES
};

} // end anonymous namespace

static const uint8_t ConversionTable[CVT_NUM_SIGNATURES][33] = {
  // Convert__imm_95_0
  { CVT_imm_95_0, 0, CVT_Done },
  // Convert__Imm1_0
  { CVT_95_addImmOperands, 1, CVT_Done },
  // Convert__BrList1_0
  { CVT_95_addBrListOperands, 1, CVT_Done },
  // Convert__Imm1_0__Imm1_1
  { CVT_95_addImmOperands, 1, CVT_95_addImmOperands, 2, CVT_Done },
  // Convert_NoOperands
  { CVT_Done },
  // Convert__imm_95_0__imm_95_0
  { CVT_imm_95_0, 0, CVT_imm_95_0, 0, CVT_Done },
  // Convert__FPImmf321_0
  { CVT_95_addFPImmf32Operands, 1, CVT_Done },
  // Convert__Imm1_1__Imm1_0
  { CVT_95_addImmOperands, 2, CVT_95_addImmOperands, 1, CVT_Done },
  // Convert__FPImmf641_0
  { CVT_95_addFPImmf64Operands, 1, CVT_Done },
  // Convert__Imm1_0__Imm1_1__Imm1_2__Imm1_3__Imm1_4__Imm1_5__Imm1_6__Imm1_7__Imm1_8__Imm1_9__Imm1_10__Imm1_11__Imm1_12__Imm1_13__Imm1_14__Imm1_15
  { CVT_95_addImmOperands, 1, CVT_95_addImmOperands, 2, CVT_95_addImmOperands, 3, CVT_95_addImmOperands, 4, CVT_95_addImmOperands, 5, CVT_95_addImmOperands, 6, CVT_95_addImmOperands, 7, CVT_95_addImmOperands, 8, CVT_95_addImmOperands, 9, CVT_95_addImmOperands, 10, CVT_95_addImmOperands, 11, CVT_95_addImmOperands, 12, CVT_95_addImmOperands, 13, CVT_95_addImmOperands, 14, CVT_95_addImmOperands, 15, CVT_95_addImmOperands, 16, CVT_Done },
  // Convert__FPImmf641_0__FPImmf641_1
  { CVT_95_addFPImmf64Operands, 1, CVT_95_addFPImmf64Operands, 2, CVT_Done },
  // Convert__Imm1_0__Imm1_1__Imm1_2__Imm1_3
  { CVT_95_addImmOperands, 1, CVT_95_addImmOperands, 2, CVT_95_addImmOperands, 3, CVT_95_addImmOperands, 4, CVT_Done },
  // Convert__FPImmf321_0__FPImmf321_1__FPImmf321_2__FPImmf321_3
  { CVT_95_addFPImmf32Operands, 1, CVT_95_addFPImmf32Operands, 2, CVT_95_addFPImmf32Operands, 3, CVT_95_addFPImmf32Operands, 4, CVT_Done },
  // Convert__Imm1_0__Imm1_1__Imm1_2__Imm1_3__Imm1_4__Imm1_5__Imm1_6__Imm1_7
  { CVT_95_addImmOperands, 1, CVT_95_addImmOperands, 2, CVT_95_addImmOperands, 3, CVT_95_addImmOperands, 4, CVT_95_addImmOperands, 5, CVT_95_addImmOperands, 6, CVT_95_addImmOperands, 7, CVT_95_addImmOperands, 8, CVT_Done },
  // Convert__Imm1_1__Imm1_0__Imm1_2
  { CVT_95_addImmOperands, 2, CVT_95_addImmOperands, 1, CVT_95_addImmOperands, 3, CVT_Done },
};

void WebAssemblyAsmParser::
convertToMCInst(unsigned Kind, MCInst &Inst, unsigned Opcode,
                const OperandVector &Operands) {
  assert(Kind < CVT_NUM_SIGNATURES && "Invalid signature!");
  const uint8_t *Converter = ConversionTable[Kind];
  unsigned OpIdx;
  Inst.setOpcode(Opcode);
  for (const uint8_t *p = Converter; *p; p += 2) {
    OpIdx = *(p + 1);
    switch (*p) {
    default: llvm_unreachable("invalid conversion entry!");
    case CVT_Reg:
      static_cast<WebAssemblyOperand &>(*Operands[OpIdx]).addRegOperands(Inst, 1);
      break;
    case CVT_Tied: {
      assert(OpIdx < (size_t)(std::end(TiedAsmOperandTable) -
                              std::begin(TiedAsmOperandTable)) &&
             "Tied operand not found");
      unsigned TiedResOpnd = TiedAsmOperandTable[OpIdx][0];
      if (TiedResOpnd != (uint8_t)-1)
        Inst.addOperand(Inst.getOperand(TiedResOpnd));
      break;
    }
    case CVT_imm_95_0:
      Inst.addOperand(MCOperand::createImm(0));
      break;
    case CVT_95_addImmOperands:
      static_cast<WebAssemblyOperand &>(*Operands[OpIdx]).addImmOperands(Inst, 1);
      break;
    case CVT_95_addBrListOperands:
      static_cast<WebAssemblyOperand &>(*Operands[OpIdx]).addBrListOperands(Inst, 1);
      break;
    case CVT_95_addFPImmf32Operands:
      static_cast<WebAssemblyOperand &>(*Operands[OpIdx]).addFPImmf32Operands(Inst, 1);
      break;
    case CVT_95_addFPImmf64Operands:
      static_cast<WebAssemblyOperand &>(*Operands[OpIdx]).addFPImmf64Operands(Inst, 1);
      break;
    }
  }
}

void WebAssemblyAsmParser::
convertToMapAndConstraints(unsigned Kind,
                           const OperandVector &Operands) {
  assert(Kind < CVT_NUM_SIGNATURES && "Invalid signature!");
  unsigned NumMCOperands = 0;
  const uint8_t *Converter = ConversionTable[Kind];
  for (const uint8_t *p = Converter; *p; p += 2) {
    switch (*p) {
    default: llvm_unreachable("invalid conversion entry!");
    case CVT_Reg:
      Operands[*(p + 1)]->setMCOperandNum(NumMCOperands);
      Operands[*(p + 1)]->setConstraint("r");
      ++NumMCOperands;
      break;
    case CVT_Tied:
      ++NumMCOperands;
      break;
    case CVT_imm_95_0:
      Operands[*(p + 1)]->setMCOperandNum(NumMCOperands);
      Operands[*(p + 1)]->setConstraint("");
      ++NumMCOperands;
      break;
    case CVT_95_addImmOperands:
      Operands[*(p + 1)]->setMCOperandNum(NumMCOperands);
      Operands[*(p + 1)]->setConstraint("m");
      NumMCOperands += 1;
      break;
    case CVT_95_addBrListOperands:
      Operands[*(p + 1)]->setMCOperandNum(NumMCOperands);
      Operands[*(p + 1)]->setConstraint("m");
      NumMCOperands += 1;
      break;
    case CVT_95_addFPImmf32Operands:
      Operands[*(p + 1)]->setMCOperandNum(NumMCOperands);
      Operands[*(p + 1)]->setConstraint("m");
      NumMCOperands += 1;
      break;
    case CVT_95_addFPImmf64Operands:
      Operands[*(p + 1)]->setMCOperandNum(NumMCOperands);
      Operands[*(p + 1)]->setConstraint("m");
      NumMCOperands += 1;
      break;
    }
  }
}

namespace {

/// MatchClassKind - The kinds of classes which participate in
/// instruction matching.
enum MatchClassKind {
  InvalidMatchClass = 0,
  OptionalMatchClass = 1,
  MCK_LAST_TOKEN = OptionalMatchClass,
  MCK_EXTERNREF, // register class 'EXTERNREF'
  MCK_F32, // register class 'F32'
  MCK_F64, // register class 'F64'
  MCK_FUNCREF, // register class 'FUNCREF'
  MCK_V128, // register class 'V128'
  MCK_I32, // register class 'I32'
  MCK_I64, // register class 'I64'
  MCK_LAST_REGISTER = MCK_I64,
  MCK_BrList, // user defined class 'BrListAsmOperand'
  MCK_Imm, // user defined class 'ImmAsmOperand'
  MCK_FPImmf32, // user defined class 'anonymous_3910'
  MCK_FPImmf64, // user defined class 'anonymous_3911'
  NumMatchClassKinds
};

} // end anonymous namespace

static unsigned getDiagKindFromRegisterClass(MatchClassKind RegisterClass) {
  return MCTargetAsmParser::Match_InvalidOperand;
}

static MatchClassKind matchTokenString(StringRef Name) {
  return InvalidMatchClass;
}

/// isSubclass - Compute whether \p A is a subclass of \p B.
static bool isSubclass(MatchClassKind A, MatchClassKind B) {
  if (A == B)
    return true;

  return false;
}

static unsigned validateOperandClass(MCParsedAsmOperand &GOp, MatchClassKind Kind) {
  WebAssemblyOperand &Operand = (WebAssemblyOperand &)GOp;
  if (Kind == InvalidMatchClass)
    return MCTargetAsmParser::Match_InvalidOperand;

  if (Operand.isToken() && Kind <= MCK_LAST_TOKEN)
    return isSubclass(matchTokenString(Operand.getToken()), Kind) ?
             MCTargetAsmParser::Match_Success :
             MCTargetAsmParser::Match_InvalidOperand;

  switch (Kind) {
  default: break;
  // 'BrList' class
  case MCK_BrList: {
    DiagnosticPredicate DP(Operand.isBrList());
    if (DP.isMatch())
      return MCTargetAsmParser::Match_Success;
    break;
    }
  // 'Imm' class
  case MCK_Imm: {
    DiagnosticPredicate DP(Operand.isImm());
    if (DP.isMatch())
      return MCTargetAsmParser::Match_Success;
    break;
    }
  // 'FPImmf32' class
  case MCK_FPImmf32: {
    DiagnosticPredicate DP(Operand.isFPImm());
    if (DP.isMatch())
      return MCTargetAsmParser::Match_Success;
    break;
    }
  // 'FPImmf64' class
  case MCK_FPImmf64: {
    DiagnosticPredicate DP(Operand.isFPImm());
    if (DP.isMatch())
      return MCTargetAsmParser::Match_Success;
    break;
    }
  } // end switch (Kind)

  if (Operand.isReg()) {
    MatchClassKind OpKind;
    switch (Operand.getReg()) {
    default: OpKind = InvalidMatchClass; break;
    case WebAssembly::FP32: OpKind = MCK_I32; break;
    case WebAssembly::FP64: OpKind = MCK_I64; break;
    case WebAssembly::SP32: OpKind = MCK_I32; break;
    case WebAssembly::SP64: OpKind = MCK_I64; break;
    case WebAssembly::I32_0: OpKind = MCK_I32; break;
    case WebAssembly::I64_0: OpKind = MCK_I64; break;
    case WebAssembly::F32_0: OpKind = MCK_F32; break;
    case WebAssembly::F64_0: OpKind = MCK_F64; break;
    case WebAssembly::V128_0: OpKind = MCK_V128; break;
    case WebAssembly::FUNCREF_0: OpKind = MCK_FUNCREF; break;
    case WebAssembly::EXTERNREF_0: OpKind = MCK_EXTERNREF; break;
    }
    return isSubclass(OpKind, Kind) ? (unsigned)MCTargetAsmParser::Match_Success :
                                      getDiagKindFromRegisterClass(Kind);
  }

  if (Kind > MCK_LAST_TOKEN && Kind <= MCK_LAST_REGISTER)
    return getDiagKindFromRegisterClass(Kind);

  return MCTargetAsmParser::Match_InvalidOperand;
}

#ifndef NDEBUG
const char *getMatchClassName(MatchClassKind Kind) {
  switch (Kind) {
  case InvalidMatchClass: return "InvalidMatchClass";
  case OptionalMatchClass: return "OptionalMatchClass";
  case MCK_EXTERNREF: return "MCK_EXTERNREF";
  case MCK_F32: return "MCK_F32";
  case MCK_F64: return "MCK_F64";
  case MCK_FUNCREF: return "MCK_FUNCREF";
  case MCK_V128: return "MCK_V128";
  case MCK_I32: return "MCK_I32";
  case MCK_I64: return "MCK_I64";
  case MCK_BrList: return "MCK_BrList";
  case MCK_Imm: return "MCK_Imm";
  case MCK_FPImmf32: return "MCK_FPImmf32";
  case MCK_FPImmf64: return "MCK_FPImmf64";
  case NumMatchClassKinds: return "NumMatchClassKinds";
  }
  llvm_unreachable("unhandled MatchClassKind!");
}

#endif // NDEBUG
FeatureBitset WebAssemblyAsmParser::
ComputeAvailableFeatures(const FeatureBitset &FB) const {
  FeatureBitset Features;
  if (FB[WebAssembly::FeatureSIMD128])
    Features.set(Feature_HasSIMD128Bit);
  if (FB[WebAssembly::FeatureRelaxedSIMD])
    Features.set(Feature_HasRelaxedSIMDBit);
  if (FB[WebAssembly::FeatureAtomics])
    Features.set(Feature_HasAtomicsBit);
  if (FB[WebAssembly::FeatureMultivalue])
    Features.set(Feature_HasMultivalueBit);
  if (FB[WebAssembly::FeatureNontrappingFPToInt])
    Features.set(Feature_HasNontrappingFPToIntBit);
  if (!FB[WebAssembly::FeatureNontrappingFPToInt])
    Features.set(Feature_NotHasNontrappingFPToIntBit);
  if (FB[WebAssembly::FeatureSignExt])
    Features.set(Feature_HasSignExtBit);
  if (FB[WebAssembly::FeatureTailCall])
    Features.set(Feature_HasTailCallBit);
  if (FB[WebAssembly::FeatureExceptionHandling])
    Features.set(Feature_HasExceptionHandlingBit);
  if (FB[WebAssembly::FeatureBulkMemory])
    Features.set(Feature_HasBulkMemoryBit);
  if (FB[WebAssembly::FeatureReferenceTypes])
    Features.set(Feature_HasReferenceTypesBit);
  if (FB[WebAssembly::FeatureExtendedConst])
    Features.set(Feature_HasExtendedConstBit);
  return Features;
}

static bool checkAsmTiedOperandConstraints(const WebAssemblyAsmParser&AsmParser,
                               unsigned Kind,
                               const OperandVector &Operands,
                               uint64_t &ErrorInfo) {
  assert(Kind < CVT_NUM_SIGNATURES && "Invalid signature!");
  const uint8_t *Converter = ConversionTable[Kind];
  for (const uint8_t *p = Converter; *p; p += 2) {
    switch (*p) {
    case CVT_Tied: {
      unsigned OpIdx = *(p + 1);
      assert(OpIdx < (size_t)(std::end(TiedAsmOperandTable) -
                              std::begin(TiedAsmOperandTable)) &&
             "Tied operand not found");
      unsigned OpndNum1 = TiedAsmOperandTable[OpIdx][1];
      unsigned OpndNum2 = TiedAsmOperandTable[OpIdx][2];
      if (OpndNum1 != OpndNum2) {
        auto &SrcOp1 = Operands[OpndNum1];
        auto &SrcOp2 = Operands[OpndNum2];
        if (!AsmParser.areEqualRegs(*SrcOp1, *SrcOp2)) {
          ErrorInfo = OpndNum2;
          return false;
        }
      }
      break;
    }
    default:
      break;
    }
  }
  return true;
}

static const char MnemonicTable[] =
    "\014atomic.fence\005block\002br\005br_if\010br_table\004call\015call_in"
    "direct\013call_params\014call_results\005catch\tcatch_all\010catchret\n"
    "cleanupret\016compiler_fence\tdata.drop\010delegate\004drop\004else\003"
    "end\tend_block\014end_function\006end_if\010end_loop\007end_try\020exte"
    "rnref.select\007f32.abs\007f32.add\010f32.ceil\tf32.const\021f32.conver"
    "t_i32_s\021f32.convert_i32_u\021f32.convert_i64_s\021f32.convert_i64_u\014"
    "f32.copysign\016f32.demote_f64\007f32.div\006f32.eq\tf32.floor\006f32.g"
    "e\006f32.gt\006f32.le\010f32.load\006f32.lt\007f32.max\007f32.min\007f3"
    "2.mul\006f32.ne\013f32.nearest\007f32.neg\023f32.reinterpret_i32\nf32.s"
    "elect\010f32.sqrt\tf32.store\007f32.sub\tf32.trunc\tf32x4.abs\tf32x4.ad"
    "d\nf32x4.ceil\025f32x4.convert_i32x4_s\025f32x4.convert_i32x4_u\027f32x"
    "4.demote_f64x2_zero\tf32x4.div\010f32x4.eq\022f32x4.extract_lane\013f32"
    "x4.floor\010f32x4.ge\010f32x4.gt\010f32x4.le\010f32x4.lt\tf32x4.max\tf3"
    "2x4.min\tf32x4.mul\010f32x4.ne\015f32x4.nearest\tf32x4.neg\nf32x4.pmax\n"
    "f32x4.pmin f32x4.relaxed_dot_bf16x8_add_f32\022f32x4.relaxed_madd\021f3"
    "2x4.relaxed_max\021f32x4.relaxed_min\023f32x4.relaxed_nmadd\022f32x4.re"
    "place_lane\013f32x4.splat\nf32x4.sqrt\tf32x4.sub\013f32x4.trunc\007f64."
    "abs\007f64.add\010f64.ceil\tf64.const\021f64.convert_i32_s\021f64.conve"
    "rt_i32_u\021f64.convert_i64_s\021f64.convert_i64_u\014f64.copysign\007f"
    "64.div\006f64.eq\tf64.floor\006f64.ge\006f64.gt\006f64.le\010f64.load\006"
    "f64.lt\007f64.max\007f64.min\007f64.mul\006f64.ne\013f64.nearest\007f64"
    ".neg\017f64.promote_f32\023f64.reinterpret_i64\nf64.select\010f64.sqrt\t"
    "f64.store\007f64.sub\tf64.trunc\tf64x2.abs\tf64x2.add\nf64x2.ceil\031f6"
    "4x2.convert_low_i32x4_s\031f64x2.convert_low_i32x4_u\tf64x2.div\010f64x"
    "2.eq\022f64x2.extract_lane\013f64x2.floor\010f64x2.ge\010f64x2.gt\010f6"
    "4x2.le\010f64x2.lt\tf64x2.max\tf64x2.min\tf64x2.mul\010f64x2.ne\015f64x"
    "2.nearest\tf64x2.neg\nf64x2.pmax\nf64x2.pmin\027f64x2.promote_low_f32x4"
    "\022f64x2.relaxed_madd\021f64x2.relaxed_max\021f64x2.relaxed_min\023f64"
    "x2.relaxed_nmadd\022f64x2.replace_lane\013f64x2.splat\nf64x2.sqrt\tf64x"
    "2.sub\013f64x2.trunc\016funcref.select\nglobal.get\nglobal.set\ti16x8.a"
    "bs\ti16x8.add\017i16x8.add_sat_s\017i16x8.add_sat_u\016i16x8.all_true\014"
    "i16x8.avgr_u\015i16x8.bitmask\010i16x8.eq\035i16x8.extadd_pairwise_i8x1"
    "6_s\035i16x8.extadd_pairwise_i8x16_u\031i16x8.extend_high_i8x16_s\031i1"
    "6x8.extend_high_i8x16_u\030i16x8.extend_low_i8x16_s\030i16x8.extend_low"
    "_i8x16_u\031i16x8.extmul_high_i8x16_s\031i16x8.extmul_high_i8x16_u\030i"
    "16x8.extmul_low_i8x16_s\030i16x8.extmul_low_i8x16_u\024i16x8.extract_la"
    "ne_s\024i16x8.extract_lane_u\ni16x8.ge_s\ni16x8.ge_u\ni16x8.gt_s\ni16x8"
    ".gt_u\ni16x8.le_s\ni16x8.le_u\017i16x8.load8x8_s\017i16x8.load8x8_u\ni1"
    "6x8.lt_s\ni16x8.lt_u\013i16x8.max_s\013i16x8.max_u\013i16x8.min_s\013i1"
    "6x8.min_u\ti16x8.mul\024i16x8.narrow_i32x4_s\024i16x8.narrow_i32x4_u\010"
    "i16x8.ne\ti16x8.neg\023i16x8.q15mulr_sat_s\037i16x8.relaxed_dot_i8x16_i"
    "7x16_s\030i16x8.relaxed_laneselect\027i16x8.relaxed_q15mulr_s\022i16x8."
    "replace_lane\ti16x8.shl\013i16x8.shr_s\013i16x8.shr_u\013i16x8.splat\ti"
    "16x8.sub\017i16x8.sub_sat_s\017i16x8.sub_sat_u\007i32.add\007i32.and\017"
    "i32.atomic.load\023i32.atomic.load16_u\022i32.atomic.load8_u\022i32.ato"
    "mic.rmw.add\022i32.atomic.rmw.and\026i32.atomic.rmw.cmpxchg\021i32.atom"
    "ic.rmw.or\022i32.atomic.rmw.sub\023i32.atomic.rmw.xchg\022i32.atomic.rm"
    "w.xor\026i32.atomic.rmw16.add_u\026i32.atomic.rmw16.and_u\032i32.atomic"
    ".rmw16.cmpxchg_u\025i32.atomic.rmw16.or_u\026i32.atomic.rmw16.sub_u\027"
    "i32.atomic.rmw16.xchg_u\026i32.atomic.rmw16.xor_u\025i32.atomic.rmw8.ad"
    "d_u\025i32.atomic.rmw8.and_u\031i32.atomic.rmw8.cmpxchg_u\024i32.atomic"
    ".rmw8.or_u\025i32.atomic.rmw8.sub_u\026i32.atomic.rmw8.xchg_u\025i32.at"
    "omic.rmw8.xor_u\020i32.atomic.store\022i32.atomic.store16\021i32.atomic"
    ".store8\007i32.clz\ti32.const\007i32.ctz\ti32.div_s\ti32.div_u\006i32.e"
    "q\007i32.eqz\016i32.extend16_s\015i32.extend8_s\010i32.ge_s\010i32.ge_u"
    "\010i32.gt_s\010i32.gt_u\010i32.le_s\010i32.le_u\010i32.load\014i32.loa"
    "d16_s\014i32.load16_u\013i32.load8_s\013i32.load8_u\010i32.lt_s\010i32."
    "lt_u\007i32.mul\006i32.ne\006i32.or\ni32.popcnt\023i32.reinterpret_f32\t"
    "i32.rem_s\ti32.rem_u\010i32.rotl\010i32.rotr\ni32.select\007i32.shl\ti3"
    "2.shr_s\ti32.shr_u\ti32.store\013i32.store16\ni32.store8\007i32.sub\017"
    "i32.trunc_f32_s\017i32.trunc_f32_u\017i32.trunc_f64_s\017i32.trunc_f64_"
    "u\023i32.trunc_sat_f32_s\023i32.trunc_sat_f32_u\023i32.trunc_sat_f64_s\023"
    "i32.trunc_sat_f64_u\014i32.wrap_i64\007i32.xor\ti32x4.abs\ti32x4.add\016"
    "i32x4.all_true\015i32x4.bitmask\021i32x4.dot_i16x8_s\010i32x4.eq\035i32"
    "x4.extadd_pairwise_i16x8_s\035i32x4.extadd_pairwise_i16x8_u\031i32x4.ex"
    "tend_high_i16x8_s\031i32x4.extend_high_i16x8_u\030i32x4.extend_low_i16x"
    "8_s\030i32x4.extend_low_i16x8_u\031i32x4.extmul_high_i16x8_s\031i32x4.e"
    "xtmul_high_i16x8_u\030i32x4.extmul_low_i16x8_s\030i32x4.extmul_low_i16x"
    "8_u\022i32x4.extract_lane\ni32x4.ge_s\ni32x4.ge_u\ni32x4.gt_s\ni32x4.gt"
    "_u\ni32x4.le_s\ni32x4.le_u\020i32x4.load16x4_s\020i32x4.load16x4_u\ni32"
    "x4.lt_s\ni32x4.lt_u\013i32x4.max_s\013i32x4.max_u\013i32x4.min_s\013i32"
    "x4.min_u\ti32x4.mul\010i32x4.ne\ti32x4.neg#i32x4.relaxed_dot_i8x16_i7x1"
    "6_add_s\030i32x4.relaxed_laneselect\033i32x4.relaxed_trunc_f32x4_s\033i"
    "32x4.relaxed_trunc_f32x4_u i32x4.relaxed_trunc_f64x2_s_zero i32x4.relax"
    "ed_trunc_f64x2_u_zero\022i32x4.replace_lane\ti32x4.shl\013i32x4.shr_s\013"
    "i32x4.shr_u\013i32x4.splat\ti32x4.sub\027i32x4.trunc_sat_f32x4_s\027i32"
    "x4.trunc_sat_f32x4_u\034i32x4.trunc_sat_f64x2_s_zero\034i32x4.trunc_sat"
    "_f64x2_u_zero\007i64.add\007i64.and\017i64.atomic.load\023i64.atomic.lo"
    "ad16_u\023i64.atomic.load32_u\022i64.atomic.load8_u\022i64.atomic.rmw.a"
    "dd\022i64.atomic.rmw.and\026i64.atomic.rmw.cmpxchg\021i64.atomic.rmw.or"
    "\022i64.atomic.rmw.sub\023i64.atomic.rmw.xchg\022i64.atomic.rmw.xor\026"
    "i64.atomic.rmw16.add_u\026i64.atomic.rmw16.and_u\032i64.atomic.rmw16.cm"
    "pxchg_u\025i64.atomic.rmw16.or_u\026i64.atomic.rmw16.sub_u\027i64.atomi"
    "c.rmw16.xchg_u\026i64.atomic.rmw16.xor_u\026i64.atomic.rmw32.add_u\026i"
    "64.atomic.rmw32.and_u\032i64.atomic.rmw32.cmpxchg_u\025i64.atomic.rmw32"
    ".or_u\026i64.atomic.rmw32.sub_u\027i64.atomic.rmw32.xchg_u\026i64.atomi"
    "c.rmw32.xor_u\025i64.atomic.rmw8.add_u\025i64.atomic.rmw8.and_u\031i64."
    "atomic.rmw8.cmpxchg_u\024i64.atomic.rmw8.or_u\025i64.atomic.rmw8.sub_u\026"
    "i64.atomic.rmw8.xchg_u\025i64.atomic.rmw8.xor_u\020i64.atomic.store\022"
    "i64.atomic.store16\022i64.atomic.store32\021i64.atomic.store8\007i64.cl"
    "z\ti64.const\007i64.ctz\ti64.div_s\ti64.div_u\006i64.eq\007i64.eqz\016i"
    "64.extend16_s\016i64.extend32_s\015i64.extend8_s\020i64.extend_i32_s\020"
    "i64.extend_i32_u\010i64.ge_s\010i64.ge_u\010i64.gt_s\010i64.gt_u\010i64"
    ".le_s\010i64.le_u\010i64.load\014i64.load16_s\014i64.load16_u\014i64.lo"
    "ad32_s\014i64.load32_u\013i64.load8_s\013i64.load8_u\010i64.lt_s\010i64"
    ".lt_u\007i64.mul\006i64.ne\006i64.or\ni64.popcnt\023i64.reinterpret_f64"
    "\ti64.rem_s\ti64.rem_u\010i64.rotl\010i64.rotr\ni64.select\007i64.shl\t"
    "i64.shr_s\ti64.shr_u\ti64.store\013i64.store16\013i64.store32\ni64.stor"
    "e8\007i64.sub\017i64.trunc_f32_s\017i64.trunc_f32_u\017i64.trunc_f64_s\017"
    "i64.trunc_f64_u\023i64.trunc_sat_f32_s\023i64.trunc_sat_f32_u\023i64.tr"
    "unc_sat_f64_s\023i64.trunc_sat_f64_u\007i64.xor\ti64x2.abs\ti64x2.add\016"
    "i64x2.all_true\015i64x2.bitmask\010i64x2.eq\031i64x2.extend_high_i32x4_"
    "s\031i64x2.extend_high_i32x4_u\030i64x2.extend_low_i32x4_s\030i64x2.ext"
    "end_low_i32x4_u\031i64x2.extmul_high_i32x4_s\031i64x2.extmul_high_i32x4"
    "_u\030i64x2.extmul_low_i32x4_s\030i64x2.extmul_low_i32x4_u\022i64x2.ext"
    "ract_lane\ni64x2.ge_s\ni64x2.gt_s\ni64x2.le_s\020i64x2.load32x2_s\020i6"
    "4x2.load32x2_u\ni64x2.lt_s\ti64x2.mul\010i64x2.ne\ti64x2.neg\030i64x2.r"
    "elaxed_laneselect\022i64x2.replace_lane\ti64x2.shl\013i64x2.shr_s\013i6"
    "4x2.shr_u\013i64x2.splat\ti64x2.sub\ti8x16.abs\ti8x16.add\017i8x16.add_"
    "sat_s\017i8x16.add_sat_u\016i8x16.all_true\014i8x16.avgr_u\015i8x16.bit"
    "mask\010i8x16.eq\024i8x16.extract_lane_s\024i8x16.extract_lane_u\ni8x16"
    ".ge_s\ni8x16.ge_u\ni8x16.gt_s\ni8x16.gt_u\ni8x16.le_s\ni8x16.le_u\ni8x1"
    "6.lt_s\ni8x16.lt_u\013i8x16.max_s\013i8x16.max_u\013i8x16.min_s\013i8x1"
    "6.min_u\024i8x16.narrow_i16x8_s\024i8x16.narrow_i16x8_u\010i8x16.ne\ti8"
    "x16.neg\014i8x16.popcnt\030i8x16.relaxed_laneselect\025i8x16.relaxed_sw"
    "izzle\022i8x16.replace_lane\ti8x16.shl\013i8x16.shr_s\013i8x16.shr_u\015"
    "i8x16.shuffle\013i8x16.splat\ti8x16.sub\017i8x16.sub_sat_s\017i8x16.sub"
    "_sat_u\015i8x16.swizzle\002if\tlocal.get\tlocal.set\tlocal.tee\004loop\024"
    "memory.atomic.notify\024memory.atomic.wait32\024memory.atomic.wait64\013"
    "memory.copy\013memory.fill\013memory.grow\013memory.init\013memory.size"
    "\003nop\013ref.is_null\017ref.null_extern\015ref.null_func\007rethrow\006"
    "return\013return_call\024return_call_indirect\023return_call_results\nt"
    "able.copy\ntable.fill\ttable.get\ntable.grow\ttable.set\ntable.size\005"
    "throw\003try\013unreachable\010v128.and\013v128.andnot\015v128.any_true"
    "\016v128.bitselect\nv128.const\tv128.load\020v128.load16_lane\021v128.l"
    "oad16_splat\020v128.load32_lane\021v128.load32_splat\020v128.load32_zer"
    "o\020v128.load64_lane\021v128.load64_splat\020v128.load64_zero\017v128."
    "load8_lane\020v128.load8_splat\010v128.not\007v128.or\013v128.select\nv"
    "128.store\021v128.store16_lane\021v128.store32_lane\021v128.store64_lan"
    "e\020v128.store8_lane\010v128.xor";

// Feature bitsets.
enum : uint8_t {
  AMFBS_None,
  AMFBS_HasAtomics,
  AMFBS_HasBulkMemory,
  AMFBS_HasExceptionHandling,
  AMFBS_HasNontrappingFPToInt,
  AMFBS_HasReferenceTypes,
  AMFBS_HasRelaxedSIMD,
  AMFBS_HasSIMD128,
  AMFBS_HasSignExt,
  AMFBS_HasTailCall,
};

static constexpr FeatureBitset FeatureBitsets[] = {
  {}, // AMFBS_None
  {Feature_HasAtomicsBit, },
  {Feature_HasBulkMemoryBit, },
  {Feature_HasExceptionHandlingBit, },
  {Feature_HasNontrappingFPToIntBit, },
  {Feature_HasReferenceTypesBit, },
  {Feature_HasRelaxedSIMDBit, },
  {Feature_HasSIMD128Bit, },
  {Feature_HasSignExtBit, },
  {Feature_HasTailCallBit, },
};

namespace {
  struct MatchEntry {
    uint16_t Mnemonic;
    uint16_t Opcode;
    uint8_t ConvertFn;
    uint8_t RequiredFeaturesIdx;
    uint8_t Classes[16];
    StringRef getMnemonic() const {
      return StringRef(MnemonicTable + Mnemonic + 1,
                       MnemonicTable[Mnemonic]);
    }
  };

  // Predicate for searching for an opcode.
  struct LessOpcode {
    bool operator()(const MatchEntry &LHS, StringRef RHS) {
      return LHS.getMnemonic() < RHS;
    }
    bool operator()(StringRef LHS, const MatchEntry &RHS) {
      return LHS < RHS.getMnemonic();
    }
    bool operator()(const MatchEntry &LHS, const MatchEntry &RHS) {
      return LHS.getMnemonic() < RHS.getMnemonic();
    }
  };
} // end anonymous namespace

static const MatchEntry MatchTable0[] = {
  { 0 /* atomic.fence */, WebAssembly::ATOMIC_FENCE_S, Convert__imm_95_0, AMFBS_HasAtomics, {  }, },
  { 13 /* block */, WebAssembly::BLOCK_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 19 /* br */, WebAssembly::BR_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 22 /* br_if */, WebAssembly::BR_IF_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 28 /* br_table */, WebAssembly::BR_TABLE_I32_S, Convert__BrList1_0, AMFBS_None, { MCK_BrList }, },
  { 28 /* br_table */, WebAssembly::BR_TABLE_I64_S, Convert__BrList1_0, AMFBS_None, { MCK_BrList }, },
  { 37 /* call */, WebAssembly::CALL_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 42 /* call_indirect */, WebAssembly::CALL_INDIRECT_S, Convert__Imm1_0__Imm1_1, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 56 /* call_params */, WebAssembly::CALL_PARAMS_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 68 /* call_results */, WebAssembly::CALL_RESULTS_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 81 /* catch */, WebAssembly::CATCH_S, Convert__Imm1_0, AMFBS_HasExceptionHandling, { MCK_Imm }, },
  { 87 /* catch_all */, WebAssembly::CATCH_ALL_S, Convert_NoOperands, AMFBS_HasExceptionHandling, {  }, },
  { 97 /* catchret */, WebAssembly::CATCHRET_S, Convert__imm_95_0__imm_95_0, AMFBS_HasExceptionHandling, {  }, },
  { 106 /* cleanupret */, WebAssembly::CLEANUPRET_S, Convert_NoOperands, AMFBS_HasExceptionHandling, {  }, },
  { 117 /* compiler_fence */, WebAssembly::COMPILER_FENCE_S, Convert_NoOperands, AMFBS_HasAtomics, {  }, },
  { 132 /* data.drop */, WebAssembly::anonymous_6249DATA_DROP_S, Convert__Imm1_0, AMFBS_HasBulkMemory, { MCK_Imm }, },
  { 132 /* data.drop */, WebAssembly::anonymous_6250DATA_DROP_S, Convert__Imm1_0, AMFBS_HasBulkMemory, { MCK_Imm }, },
  { 142 /* delegate */, WebAssembly::DELEGATE_S, Convert__Imm1_0, AMFBS_HasExceptionHandling, { MCK_Imm }, },
  { 151 /* drop */, WebAssembly::DROP_EXTERNREF_S, Convert_NoOperands, AMFBS_HasReferenceTypes, {  }, },
  { 151 /* drop */, WebAssembly::DROP_FUNCREF_S, Convert_NoOperands, AMFBS_HasReferenceTypes, {  }, },
  { 151 /* drop */, WebAssembly::DROP_V128_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 151 /* drop */, WebAssembly::DROP_F32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 151 /* drop */, WebAssembly::DROP_F64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 151 /* drop */, WebAssembly::DROP_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 151 /* drop */, WebAssembly::DROP_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 156 /* else */, WebAssembly::ELSE_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 161 /* end */, WebAssembly::END_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 165 /* end_block */, WebAssembly::END_BLOCK_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 175 /* end_function */, WebAssembly::END_FUNCTION_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 188 /* end_if */, WebAssembly::END_IF_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 195 /* end_loop */, WebAssembly::END_LOOP_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 204 /* end_try */, WebAssembly::END_TRY_S, Convert_NoOperands, AMFBS_HasExceptionHandling, {  }, },
  { 212 /* externref.select */, WebAssembly::SELECT_EXTERNREF_S, Convert_NoOperands, AMFBS_HasReferenceTypes, {  }, },
  { 229 /* f32.abs */, WebAssembly::ABS_F32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 237 /* f32.add */, WebAssembly::ADD_F32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 245 /* f32.ceil */, WebAssembly::CEIL_F32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 254 /* f32.const */, WebAssembly::CONST_F32_S, Convert__FPImmf321_0, AMFBS_None, { MCK_FPImmf32 }, },
  { 264 /* f32.convert_i32_s */, WebAssembly::F32_CONVERT_S_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 282 /* f32.convert_i32_u */, WebAssembly::F32_CONVERT_U_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 300 /* f32.convert_i64_s */, WebAssembly::F32_CONVERT_S_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 318 /* f32.convert_i64_u */, WebAssembly::F32_CONVERT_U_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 336 /* f32.copysign */, WebAssembly::COPYSIGN_F32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 349 /* f32.demote_f64 */, WebAssembly::F32_DEMOTE_F64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 364 /* f32.div */, WebAssembly::DIV_F32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 372 /* f32.eq */, WebAssembly::EQ_F32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 379 /* f32.floor */, WebAssembly::FLOOR_F32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 389 /* f32.ge */, WebAssembly::GE_F32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 396 /* f32.gt */, WebAssembly::GT_F32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 403 /* f32.le */, WebAssembly::LE_F32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 410 /* f32.load */, WebAssembly::LOAD_F32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 410 /* f32.load */, WebAssembly::LOAD_F32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 419 /* f32.lt */, WebAssembly::LT_F32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 426 /* f32.max */, WebAssembly::MAX_F32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 434 /* f32.min */, WebAssembly::MIN_F32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 442 /* f32.mul */, WebAssembly::MUL_F32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 450 /* f32.ne */, WebAssembly::NE_F32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 457 /* f32.nearest */, WebAssembly::NEAREST_F32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 469 /* f32.neg */, WebAssembly::NEG_F32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 477 /* f32.reinterpret_i32 */, WebAssembly::F32_REINTERPRET_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 497 /* f32.select */, WebAssembly::SELECT_F32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 508 /* f32.sqrt */, WebAssembly::SQRT_F32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 517 /* f32.store */, WebAssembly::STORE_F32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 517 /* f32.store */, WebAssembly::STORE_F32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 527 /* f32.sub */, WebAssembly::SUB_F32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 535 /* f32.trunc */, WebAssembly::TRUNC_F32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 545 /* f32x4.abs */, WebAssembly::ABS_F32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 555 /* f32x4.add */, WebAssembly::ADD_F32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 565 /* f32x4.ceil */, WebAssembly::CEIL_F32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 576 /* f32x4.convert_i32x4_s */, WebAssembly::sint_to_fp_F32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 598 /* f32x4.convert_i32x4_u */, WebAssembly::uint_to_fp_F32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 620 /* f32x4.demote_f64x2_zero */, WebAssembly::demote_zero_F32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 644 /* f32x4.div */, WebAssembly::DIV_F32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 654 /* f32x4.eq */, WebAssembly::EQ_F32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 663 /* f32x4.extract_lane */, WebAssembly::EXTRACT_LANE_F32x4_S, Convert__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm }, },
  { 682 /* f32x4.floor */, WebAssembly::FLOOR_F32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 694 /* f32x4.ge */, WebAssembly::GE_F32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 703 /* f32x4.gt */, WebAssembly::GT_F32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 712 /* f32x4.le */, WebAssembly::LE_F32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 721 /* f32x4.lt */, WebAssembly::LT_F32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 730 /* f32x4.max */, WebAssembly::MAX_F32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 740 /* f32x4.min */, WebAssembly::MIN_F32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 750 /* f32x4.mul */, WebAssembly::MUL_F32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 760 /* f32x4.ne */, WebAssembly::NE_F32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 769 /* f32x4.nearest */, WebAssembly::NEAREST_F32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 783 /* f32x4.neg */, WebAssembly::NEG_F32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 793 /* f32x4.pmax */, WebAssembly::PMAX_F32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 804 /* f32x4.pmin */, WebAssembly::PMIN_F32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 815 /* f32x4.relaxed_dot_bf16x8_add_f32 */, WebAssembly::RELAXED_DOT_BFLOAT_S, Convert_NoOperands, AMFBS_HasRelaxedSIMD, {  }, },
  { 848 /* f32x4.relaxed_madd */, WebAssembly::MADD_F32x4_S, Convert_NoOperands, AMFBS_HasRelaxedSIMD, {  }, },
  { 867 /* f32x4.relaxed_max */, WebAssembly::SIMD_RELAXED_FMAX_F32x4_S, Convert_NoOperands, AMFBS_HasRelaxedSIMD, {  }, },
  { 885 /* f32x4.relaxed_min */, WebAssembly::SIMD_RELAXED_FMIN_F32x4_S, Convert_NoOperands, AMFBS_HasRelaxedSIMD, {  }, },
  { 903 /* f32x4.relaxed_nmadd */, WebAssembly::NMADD_F32x4_S, Convert_NoOperands, AMFBS_HasRelaxedSIMD, {  }, },
  { 923 /* f32x4.replace_lane */, WebAssembly::REPLACE_LANE_F32x4_S, Convert__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm }, },
  { 942 /* f32x4.splat */, WebAssembly::SPLAT_F32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 954 /* f32x4.sqrt */, WebAssembly::SQRT_F32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 965 /* f32x4.sub */, WebAssembly::SUB_F32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 975 /* f32x4.trunc */, WebAssembly::TRUNC_F32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 987 /* f64.abs */, WebAssembly::ABS_F64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 995 /* f64.add */, WebAssembly::ADD_F64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 1003 /* f64.ceil */, WebAssembly::CEIL_F64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 1012 /* f64.const */, WebAssembly::CONST_F64_S, Convert__FPImmf641_0, AMFBS_None, { MCK_FPImmf64 }, },
  { 1022 /* f64.convert_i32_s */, WebAssembly::F64_CONVERT_S_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 1040 /* f64.convert_i32_u */, WebAssembly::F64_CONVERT_U_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 1058 /* f64.convert_i64_s */, WebAssembly::F64_CONVERT_S_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 1076 /* f64.convert_i64_u */, WebAssembly::F64_CONVERT_U_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 1094 /* f64.copysign */, WebAssembly::COPYSIGN_F64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 1107 /* f64.div */, WebAssembly::DIV_F64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 1115 /* f64.eq */, WebAssembly::EQ_F64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 1122 /* f64.floor */, WebAssembly::FLOOR_F64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 1132 /* f64.ge */, WebAssembly::GE_F64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 1139 /* f64.gt */, WebAssembly::GT_F64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 1146 /* f64.le */, WebAssembly::LE_F64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 1153 /* f64.load */, WebAssembly::LOAD_F64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 1153 /* f64.load */, WebAssembly::LOAD_F64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 1162 /* f64.lt */, WebAssembly::LT_F64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 1169 /* f64.max */, WebAssembly::MAX_F64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 1177 /* f64.min */, WebAssembly::MIN_F64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 1185 /* f64.mul */, WebAssembly::MUL_F64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 1193 /* f64.ne */, WebAssembly::NE_F64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 1200 /* f64.nearest */, WebAssembly::NEAREST_F64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 1212 /* f64.neg */, WebAssembly::NEG_F64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 1220 /* f64.promote_f32 */, WebAssembly::F64_PROMOTE_F32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 1236 /* f64.reinterpret_i64 */, WebAssembly::F64_REINTERPRET_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 1256 /* f64.select */, WebAssembly::SELECT_F64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 1267 /* f64.sqrt */, WebAssembly::SQRT_F64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 1276 /* f64.store */, WebAssembly::STORE_F64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 1276 /* f64.store */, WebAssembly::STORE_F64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 1286 /* f64.sub */, WebAssembly::SUB_F64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 1294 /* f64.trunc */, WebAssembly::TRUNC_F64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 1304 /* f64x2.abs */, WebAssembly::ABS_F64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1314 /* f64x2.add */, WebAssembly::ADD_F64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1324 /* f64x2.ceil */, WebAssembly::CEIL_F64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1335 /* f64x2.convert_low_i32x4_s */, WebAssembly::convert_low_s_F64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1361 /* f64x2.convert_low_i32x4_u */, WebAssembly::convert_low_u_F64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1387 /* f64x2.div */, WebAssembly::DIV_F64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1397 /* f64x2.eq */, WebAssembly::EQ_F64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1406 /* f64x2.extract_lane */, WebAssembly::EXTRACT_LANE_F64x2_S, Convert__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm }, },
  { 1425 /* f64x2.floor */, WebAssembly::FLOOR_F64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1437 /* f64x2.ge */, WebAssembly::GE_F64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1446 /* f64x2.gt */, WebAssembly::GT_F64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1455 /* f64x2.le */, WebAssembly::LE_F64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1464 /* f64x2.lt */, WebAssembly::LT_F64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1473 /* f64x2.max */, WebAssembly::MAX_F64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1483 /* f64x2.min */, WebAssembly::MIN_F64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1493 /* f64x2.mul */, WebAssembly::MUL_F64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1503 /* f64x2.ne */, WebAssembly::NE_F64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1512 /* f64x2.nearest */, WebAssembly::NEAREST_F64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1526 /* f64x2.neg */, WebAssembly::NEG_F64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1536 /* f64x2.pmax */, WebAssembly::PMAX_F64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1547 /* f64x2.pmin */, WebAssembly::PMIN_F64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1558 /* f64x2.promote_low_f32x4 */, WebAssembly::promote_low_F64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1582 /* f64x2.relaxed_madd */, WebAssembly::MADD_F64x2_S, Convert_NoOperands, AMFBS_HasRelaxedSIMD, {  }, },
  { 1601 /* f64x2.relaxed_max */, WebAssembly::SIMD_RELAXED_FMAX_F64x2_S, Convert_NoOperands, AMFBS_HasRelaxedSIMD, {  }, },
  { 1619 /* f64x2.relaxed_min */, WebAssembly::SIMD_RELAXED_FMIN_F64x2_S, Convert_NoOperands, AMFBS_HasRelaxedSIMD, {  }, },
  { 1637 /* f64x2.relaxed_nmadd */, WebAssembly::NMADD_F64x2_S, Convert_NoOperands, AMFBS_HasRelaxedSIMD, {  }, },
  { 1657 /* f64x2.replace_lane */, WebAssembly::REPLACE_LANE_F64x2_S, Convert__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm }, },
  { 1676 /* f64x2.splat */, WebAssembly::SPLAT_F64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1688 /* f64x2.sqrt */, WebAssembly::SQRT_F64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1699 /* f64x2.sub */, WebAssembly::SUB_F64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1709 /* f64x2.trunc */, WebAssembly::TRUNC_F64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1721 /* funcref.select */, WebAssembly::SELECT_FUNCREF_S, Convert_NoOperands, AMFBS_HasReferenceTypes, {  }, },
  { 1736 /* global.get */, WebAssembly::GLOBAL_GET_EXTERNREF_S, Convert__Imm1_0, AMFBS_HasReferenceTypes, { MCK_Imm }, },
  { 1736 /* global.get */, WebAssembly::GLOBAL_GET_FUNCREF_S, Convert__Imm1_0, AMFBS_HasReferenceTypes, { MCK_Imm }, },
  { 1736 /* global.get */, WebAssembly::GLOBAL_GET_V128_S, Convert__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm }, },
  { 1736 /* global.get */, WebAssembly::GLOBAL_GET_F32_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 1736 /* global.get */, WebAssembly::GLOBAL_GET_F64_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 1736 /* global.get */, WebAssembly::GLOBAL_GET_I32_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 1736 /* global.get */, WebAssembly::GLOBAL_GET_I64_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 1747 /* global.set */, WebAssembly::GLOBAL_SET_EXTERNREF_S, Convert__Imm1_0, AMFBS_HasReferenceTypes, { MCK_Imm }, },
  { 1747 /* global.set */, WebAssembly::GLOBAL_SET_FUNCREF_S, Convert__Imm1_0, AMFBS_HasReferenceTypes, { MCK_Imm }, },
  { 1747 /* global.set */, WebAssembly::GLOBAL_SET_V128_S, Convert__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm }, },
  { 1747 /* global.set */, WebAssembly::GLOBAL_SET_F32_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 1747 /* global.set */, WebAssembly::GLOBAL_SET_F64_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 1747 /* global.set */, WebAssembly::GLOBAL_SET_I32_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 1747 /* global.set */, WebAssembly::GLOBAL_SET_I64_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 1758 /* i16x8.abs */, WebAssembly::ABS_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1768 /* i16x8.add */, WebAssembly::ADD_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1778 /* i16x8.add_sat_s */, WebAssembly::ADD_SAT_S_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1794 /* i16x8.add_sat_u */, WebAssembly::ADD_SAT_U_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1810 /* i16x8.all_true */, WebAssembly::ALLTRUE_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1825 /* i16x8.avgr_u */, WebAssembly::AVGR_U_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1838 /* i16x8.bitmask */, WebAssembly::BITMASK_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1852 /* i16x8.eq */, WebAssembly::EQ_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1861 /* i16x8.extadd_pairwise_i8x16_s */, WebAssembly::int_wasm_extadd_pairwise_signed_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1891 /* i16x8.extadd_pairwise_i8x16_u */, WebAssembly::int_wasm_extadd_pairwise_unsigned_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1921 /* i16x8.extend_high_i8x16_s */, WebAssembly::extend_high_s_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1947 /* i16x8.extend_high_i8x16_u */, WebAssembly::extend_high_u_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1973 /* i16x8.extend_low_i8x16_s */, WebAssembly::extend_low_s_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 1998 /* i16x8.extend_low_i8x16_u */, WebAssembly::extend_low_u_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2023 /* i16x8.extmul_high_i8x16_s */, WebAssembly::EXTMUL_HIGH_S_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2049 /* i16x8.extmul_high_i8x16_u */, WebAssembly::EXTMUL_HIGH_U_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2075 /* i16x8.extmul_low_i8x16_s */, WebAssembly::EXTMUL_LOW_S_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2100 /* i16x8.extmul_low_i8x16_u */, WebAssembly::EXTMUL_LOW_U_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2125 /* i16x8.extract_lane_s */, WebAssembly::EXTRACT_LANE_I16x8_s_S, Convert__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm }, },
  { 2146 /* i16x8.extract_lane_u */, WebAssembly::EXTRACT_LANE_I16x8_u_S, Convert__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm }, },
  { 2167 /* i16x8.ge_s */, WebAssembly::GE_S_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2178 /* i16x8.ge_u */, WebAssembly::GE_U_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2189 /* i16x8.gt_s */, WebAssembly::GT_S_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2200 /* i16x8.gt_u */, WebAssembly::GT_U_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2211 /* i16x8.le_s */, WebAssembly::LE_S_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2222 /* i16x8.le_u */, WebAssembly::LE_U_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2233 /* i16x8.load8x8_s */, WebAssembly::LOAD_EXTEND_S_I16x8_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm }, },
  { 2233 /* i16x8.load8x8_s */, WebAssembly::LOAD_EXTEND_S_I16x8_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm }, },
  { 2249 /* i16x8.load8x8_u */, WebAssembly::LOAD_EXTEND_U_I16x8_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm }, },
  { 2249 /* i16x8.load8x8_u */, WebAssembly::LOAD_EXTEND_U_I16x8_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm }, },
  { 2265 /* i16x8.lt_s */, WebAssembly::LT_S_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2276 /* i16x8.lt_u */, WebAssembly::LT_U_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2287 /* i16x8.max_s */, WebAssembly::MAX_S_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2299 /* i16x8.max_u */, WebAssembly::MAX_U_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2311 /* i16x8.min_s */, WebAssembly::MIN_S_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2323 /* i16x8.min_u */, WebAssembly::MIN_U_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2335 /* i16x8.mul */, WebAssembly::MUL_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2345 /* i16x8.narrow_i32x4_s */, WebAssembly::NARROW_S_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2366 /* i16x8.narrow_i32x4_u */, WebAssembly::NARROW_U_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2387 /* i16x8.ne */, WebAssembly::NE_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2396 /* i16x8.neg */, WebAssembly::NEG_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2406 /* i16x8.q15mulr_sat_s */, WebAssembly::Q15MULR_SAT_S_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2426 /* i16x8.relaxed_dot_i8x16_i7x16_s */, WebAssembly::RELAXED_DOT_S, Convert_NoOperands, AMFBS_HasRelaxedSIMD, {  }, },
  { 2458 /* i16x8.relaxed_laneselect */, WebAssembly::LANESELECT_I16x8_S, Convert_NoOperands, AMFBS_HasRelaxedSIMD, {  }, },
  { 2483 /* i16x8.relaxed_q15mulr_s */, WebAssembly::RELAXED_Q15MULR_S_I16x8_S, Convert_NoOperands, AMFBS_HasRelaxedSIMD, {  }, },
  { 2507 /* i16x8.replace_lane */, WebAssembly::REPLACE_LANE_I16x8_S, Convert__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm }, },
  { 2526 /* i16x8.shl */, WebAssembly::SHL_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2536 /* i16x8.shr_s */, WebAssembly::SHR_S_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2548 /* i16x8.shr_u */, WebAssembly::SHR_U_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2560 /* i16x8.splat */, WebAssembly::SPLAT_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2572 /* i16x8.sub */, WebAssembly::SUB_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2582 /* i16x8.sub_sat_s */, WebAssembly::SUB_SAT_S_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2598 /* i16x8.sub_sat_u */, WebAssembly::SUB_SAT_U_I16x8_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 2614 /* i32.add */, WebAssembly::ADD_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 2622 /* i32.and */, WebAssembly::AND_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 2630 /* i32.atomic.load */, WebAssembly::ATOMIC_LOAD_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2630 /* i32.atomic.load */, WebAssembly::ATOMIC_LOAD_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2646 /* i32.atomic.load16_u */, WebAssembly::ATOMIC_LOAD16_U_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2646 /* i32.atomic.load16_u */, WebAssembly::ATOMIC_LOAD16_U_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2666 /* i32.atomic.load8_u */, WebAssembly::ATOMIC_LOAD8_U_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2666 /* i32.atomic.load8_u */, WebAssembly::ATOMIC_LOAD8_U_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2685 /* i32.atomic.rmw.add */, WebAssembly::ATOMIC_RMW_ADD_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2685 /* i32.atomic.rmw.add */, WebAssembly::ATOMIC_RMW_ADD_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2704 /* i32.atomic.rmw.and */, WebAssembly::ATOMIC_RMW_AND_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2704 /* i32.atomic.rmw.and */, WebAssembly::ATOMIC_RMW_AND_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2723 /* i32.atomic.rmw.cmpxchg */, WebAssembly::ATOMIC_RMW_CMPXCHG_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2723 /* i32.atomic.rmw.cmpxchg */, WebAssembly::ATOMIC_RMW_CMPXCHG_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2746 /* i32.atomic.rmw.or */, WebAssembly::ATOMIC_RMW_OR_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2746 /* i32.atomic.rmw.or */, WebAssembly::ATOMIC_RMW_OR_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2764 /* i32.atomic.rmw.sub */, WebAssembly::ATOMIC_RMW_SUB_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2764 /* i32.atomic.rmw.sub */, WebAssembly::ATOMIC_RMW_SUB_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2783 /* i32.atomic.rmw.xchg */, WebAssembly::ATOMIC_RMW_XCHG_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2783 /* i32.atomic.rmw.xchg */, WebAssembly::ATOMIC_RMW_XCHG_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2803 /* i32.atomic.rmw.xor */, WebAssembly::ATOMIC_RMW_XOR_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2803 /* i32.atomic.rmw.xor */, WebAssembly::ATOMIC_RMW_XOR_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2822 /* i32.atomic.rmw16.add_u */, WebAssembly::ATOMIC_RMW16_U_ADD_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2822 /* i32.atomic.rmw16.add_u */, WebAssembly::ATOMIC_RMW16_U_ADD_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2845 /* i32.atomic.rmw16.and_u */, WebAssembly::ATOMIC_RMW16_U_AND_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2845 /* i32.atomic.rmw16.and_u */, WebAssembly::ATOMIC_RMW16_U_AND_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2868 /* i32.atomic.rmw16.cmpxchg_u */, WebAssembly::ATOMIC_RMW16_U_CMPXCHG_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2868 /* i32.atomic.rmw16.cmpxchg_u */, WebAssembly::ATOMIC_RMW16_U_CMPXCHG_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2895 /* i32.atomic.rmw16.or_u */, WebAssembly::ATOMIC_RMW16_U_OR_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2895 /* i32.atomic.rmw16.or_u */, WebAssembly::ATOMIC_RMW16_U_OR_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2917 /* i32.atomic.rmw16.sub_u */, WebAssembly::ATOMIC_RMW16_U_SUB_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2917 /* i32.atomic.rmw16.sub_u */, WebAssembly::ATOMIC_RMW16_U_SUB_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2940 /* i32.atomic.rmw16.xchg_u */, WebAssembly::ATOMIC_RMW16_U_XCHG_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2940 /* i32.atomic.rmw16.xchg_u */, WebAssembly::ATOMIC_RMW16_U_XCHG_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2964 /* i32.atomic.rmw16.xor_u */, WebAssembly::ATOMIC_RMW16_U_XOR_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2964 /* i32.atomic.rmw16.xor_u */, WebAssembly::ATOMIC_RMW16_U_XOR_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2987 /* i32.atomic.rmw8.add_u */, WebAssembly::ATOMIC_RMW8_U_ADD_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 2987 /* i32.atomic.rmw8.add_u */, WebAssembly::ATOMIC_RMW8_U_ADD_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 3009 /* i32.atomic.rmw8.and_u */, WebAssembly::ATOMIC_RMW8_U_AND_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 3009 /* i32.atomic.rmw8.and_u */, WebAssembly::ATOMIC_RMW8_U_AND_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 3031 /* i32.atomic.rmw8.cmpxchg_u */, WebAssembly::ATOMIC_RMW8_U_CMPXCHG_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 3031 /* i32.atomic.rmw8.cmpxchg_u */, WebAssembly::ATOMIC_RMW8_U_CMPXCHG_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 3057 /* i32.atomic.rmw8.or_u */, WebAssembly::ATOMIC_RMW8_U_OR_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 3057 /* i32.atomic.rmw8.or_u */, WebAssembly::ATOMIC_RMW8_U_OR_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 3078 /* i32.atomic.rmw8.sub_u */, WebAssembly::ATOMIC_RMW8_U_SUB_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 3078 /* i32.atomic.rmw8.sub_u */, WebAssembly::ATOMIC_RMW8_U_SUB_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 3100 /* i32.atomic.rmw8.xchg_u */, WebAssembly::ATOMIC_RMW8_U_XCHG_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 3100 /* i32.atomic.rmw8.xchg_u */, WebAssembly::ATOMIC_RMW8_U_XCHG_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 3123 /* i32.atomic.rmw8.xor_u */, WebAssembly::ATOMIC_RMW8_U_XOR_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 3123 /* i32.atomic.rmw8.xor_u */, WebAssembly::ATOMIC_RMW8_U_XOR_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 3145 /* i32.atomic.store */, WebAssembly::ATOMIC_STORE_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 3145 /* i32.atomic.store */, WebAssembly::ATOMIC_STORE_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 3162 /* i32.atomic.store16 */, WebAssembly::ATOMIC_STORE16_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 3162 /* i32.atomic.store16 */, WebAssembly::ATOMIC_STORE16_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 3181 /* i32.atomic.store8 */, WebAssembly::ATOMIC_STORE8_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 3181 /* i32.atomic.store8 */, WebAssembly::ATOMIC_STORE8_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 3199 /* i32.clz */, WebAssembly::CLZ_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3207 /* i32.const */, WebAssembly::CONST_I32_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 3217 /* i32.ctz */, WebAssembly::CTZ_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3225 /* i32.div_s */, WebAssembly::DIV_S_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3235 /* i32.div_u */, WebAssembly::DIV_U_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3245 /* i32.eq */, WebAssembly::EQ_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3252 /* i32.eqz */, WebAssembly::EQZ_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3260 /* i32.extend16_s */, WebAssembly::I32_EXTEND16_S_I32_S, Convert_NoOperands, AMFBS_HasSignExt, {  }, },
  { 3275 /* i32.extend8_s */, WebAssembly::I32_EXTEND8_S_I32_S, Convert_NoOperands, AMFBS_HasSignExt, {  }, },
  { 3289 /* i32.ge_s */, WebAssembly::GE_S_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3298 /* i32.ge_u */, WebAssembly::GE_U_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3307 /* i32.gt_s */, WebAssembly::GT_S_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3316 /* i32.gt_u */, WebAssembly::GT_U_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3325 /* i32.le_s */, WebAssembly::LE_S_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3334 /* i32.le_u */, WebAssembly::LE_U_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3343 /* i32.load */, WebAssembly::LOAD_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 3343 /* i32.load */, WebAssembly::LOAD_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 3352 /* i32.load16_s */, WebAssembly::LOAD16_S_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 3352 /* i32.load16_s */, WebAssembly::LOAD16_S_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 3365 /* i32.load16_u */, WebAssembly::LOAD16_U_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 3365 /* i32.load16_u */, WebAssembly::LOAD16_U_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 3378 /* i32.load8_s */, WebAssembly::LOAD8_S_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 3378 /* i32.load8_s */, WebAssembly::LOAD8_S_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 3390 /* i32.load8_u */, WebAssembly::LOAD8_U_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 3390 /* i32.load8_u */, WebAssembly::LOAD8_U_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 3402 /* i32.lt_s */, WebAssembly::LT_S_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3411 /* i32.lt_u */, WebAssembly::LT_U_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3420 /* i32.mul */, WebAssembly::MUL_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3428 /* i32.ne */, WebAssembly::NE_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3435 /* i32.or */, WebAssembly::OR_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3442 /* i32.popcnt */, WebAssembly::POPCNT_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3453 /* i32.reinterpret_f32 */, WebAssembly::I32_REINTERPRET_F32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3473 /* i32.rem_s */, WebAssembly::REM_S_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3483 /* i32.rem_u */, WebAssembly::REM_U_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3493 /* i32.rotl */, WebAssembly::ROTL_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3502 /* i32.rotr */, WebAssembly::ROTR_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3511 /* i32.select */, WebAssembly::SELECT_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3522 /* i32.shl */, WebAssembly::SHL_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3530 /* i32.shr_s */, WebAssembly::SHR_S_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3540 /* i32.shr_u */, WebAssembly::SHR_U_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3550 /* i32.store */, WebAssembly::STORE_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 3550 /* i32.store */, WebAssembly::STORE_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 3560 /* i32.store16 */, WebAssembly::STORE16_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 3560 /* i32.store16 */, WebAssembly::STORE16_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 3572 /* i32.store8 */, WebAssembly::STORE8_I32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 3572 /* i32.store8 */, WebAssembly::STORE8_I32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 3583 /* i32.sub */, WebAssembly::SUB_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3591 /* i32.trunc_f32_s */, WebAssembly::I32_TRUNC_S_F32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3607 /* i32.trunc_f32_u */, WebAssembly::I32_TRUNC_U_F32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3623 /* i32.trunc_f64_s */, WebAssembly::I32_TRUNC_S_F64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3639 /* i32.trunc_f64_u */, WebAssembly::I32_TRUNC_U_F64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3655 /* i32.trunc_sat_f32_s */, WebAssembly::I32_TRUNC_S_SAT_F32_S, Convert_NoOperands, AMFBS_HasNontrappingFPToInt, {  }, },
  { 3675 /* i32.trunc_sat_f32_u */, WebAssembly::I32_TRUNC_U_SAT_F32_S, Convert_NoOperands, AMFBS_HasNontrappingFPToInt, {  }, },
  { 3695 /* i32.trunc_sat_f64_s */, WebAssembly::I32_TRUNC_S_SAT_F64_S, Convert_NoOperands, AMFBS_HasNontrappingFPToInt, {  }, },
  { 3715 /* i32.trunc_sat_f64_u */, WebAssembly::I32_TRUNC_U_SAT_F64_S, Convert_NoOperands, AMFBS_HasNontrappingFPToInt, {  }, },
  { 3735 /* i32.wrap_i64 */, WebAssembly::I32_WRAP_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3748 /* i32.xor */, WebAssembly::XOR_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 3756 /* i32x4.abs */, WebAssembly::ABS_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 3766 /* i32x4.add */, WebAssembly::ADD_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 3776 /* i32x4.all_true */, WebAssembly::ALLTRUE_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 3791 /* i32x4.bitmask */, WebAssembly::BITMASK_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 3805 /* i32x4.dot_i16x8_s */, WebAssembly::DOT_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 3823 /* i32x4.eq */, WebAssembly::EQ_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 3832 /* i32x4.extadd_pairwise_i16x8_s */, WebAssembly::int_wasm_extadd_pairwise_signed_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 3862 /* i32x4.extadd_pairwise_i16x8_u */, WebAssembly::int_wasm_extadd_pairwise_unsigned_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 3892 /* i32x4.extend_high_i16x8_s */, WebAssembly::extend_high_s_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 3918 /* i32x4.extend_high_i16x8_u */, WebAssembly::extend_high_u_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 3944 /* i32x4.extend_low_i16x8_s */, WebAssembly::extend_low_s_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 3969 /* i32x4.extend_low_i16x8_u */, WebAssembly::extend_low_u_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 3994 /* i32x4.extmul_high_i16x8_s */, WebAssembly::EXTMUL_HIGH_S_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 4020 /* i32x4.extmul_high_i16x8_u */, WebAssembly::EXTMUL_HIGH_U_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 4046 /* i32x4.extmul_low_i16x8_s */, WebAssembly::EXTMUL_LOW_S_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 4071 /* i32x4.extmul_low_i16x8_u */, WebAssembly::EXTMUL_LOW_U_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 4096 /* i32x4.extract_lane */, WebAssembly::EXTRACT_LANE_I32x4_S, Convert__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm }, },
  { 4115 /* i32x4.ge_s */, WebAssembly::GE_S_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 4126 /* i32x4.ge_u */, WebAssembly::GE_U_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 4137 /* i32x4.gt_s */, WebAssembly::GT_S_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 4148 /* i32x4.gt_u */, WebAssembly::GT_U_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 4159 /* i32x4.le_s */, WebAssembly::LE_S_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 4170 /* i32x4.le_u */, WebAssembly::LE_U_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 4181 /* i32x4.load16x4_s */, WebAssembly::LOAD_EXTEND_S_I32x4_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm }, },
  { 4181 /* i32x4.load16x4_s */, WebAssembly::LOAD_EXTEND_S_I32x4_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm }, },
  { 4198 /* i32x4.load16x4_u */, WebAssembly::LOAD_EXTEND_U_I32x4_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm }, },
  { 4198 /* i32x4.load16x4_u */, WebAssembly::LOAD_EXTEND_U_I32x4_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm }, },
  { 4215 /* i32x4.lt_s */, WebAssembly::LT_S_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 4226 /* i32x4.lt_u */, WebAssembly::LT_U_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 4237 /* i32x4.max_s */, WebAssembly::MAX_S_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 4249 /* i32x4.max_u */, WebAssembly::MAX_U_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 4261 /* i32x4.min_s */, WebAssembly::MIN_S_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 4273 /* i32x4.min_u */, WebAssembly::MIN_U_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 4285 /* i32x4.mul */, WebAssembly::MUL_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 4295 /* i32x4.ne */, WebAssembly::NE_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 4304 /* i32x4.neg */, WebAssembly::NEG_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 4314 /* i32x4.relaxed_dot_i8x16_i7x16_add_s */, WebAssembly::RELAXED_DOT_ADD_S, Convert_NoOperands, AMFBS_HasRelaxedSIMD, {  }, },
  { 4350 /* i32x4.relaxed_laneselect */, WebAssembly::LANESELECT_I32x4_S, Convert_NoOperands, AMFBS_HasRelaxedSIMD, {  }, },
  { 4375 /* i32x4.relaxed_trunc_f32x4_s */, WebAssembly::int_wasm_relaxed_trunc_signed_I32x4_S, Convert_NoOperands, AMFBS_HasRelaxedSIMD, {  }, },
  { 4403 /* i32x4.relaxed_trunc_f32x4_u */, WebAssembly::int_wasm_relaxed_trunc_unsigned_I32x4_S, Convert_NoOperands, AMFBS_HasRelaxedSIMD, {  }, },
  { 4431 /* i32x4.relaxed_trunc_f64x2_s_zero */, WebAssembly::int_wasm_relaxed_trunc_signed_zero_I32x4_S, Convert_NoOperands, AMFBS_HasRelaxedSIMD, {  }, },
  { 4464 /* i32x4.relaxed_trunc_f64x2_u_zero */, WebAssembly::int_wasm_relaxed_trunc_unsigned_zero_I32x4_S, Convert_NoOperands, AMFBS_HasRelaxedSIMD, {  }, },
  { 4497 /* i32x4.replace_lane */, WebAssembly::REPLACE_LANE_I32x4_S, Convert__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm }, },
  { 4516 /* i32x4.shl */, WebAssembly::SHL_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 4526 /* i32x4.shr_s */, WebAssembly::SHR_S_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 4538 /* i32x4.shr_u */, WebAssembly::SHR_U_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 4550 /* i32x4.splat */, WebAssembly::SPLAT_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 4562 /* i32x4.sub */, WebAssembly::SUB_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 4572 /* i32x4.trunc_sat_f32x4_s */, WebAssembly::fp_to_sint_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 4596 /* i32x4.trunc_sat_f32x4_u */, WebAssembly::fp_to_uint_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 4620 /* i32x4.trunc_sat_f64x2_s_zero */, WebAssembly::trunc_sat_zero_s_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 4649 /* i32x4.trunc_sat_f64x2_u_zero */, WebAssembly::trunc_sat_zero_u_I32x4_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 4678 /* i64.add */, WebAssembly::ADD_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 4686 /* i64.and */, WebAssembly::AND_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 4694 /* i64.atomic.load */, WebAssembly::ATOMIC_LOAD_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 4694 /* i64.atomic.load */, WebAssembly::ATOMIC_LOAD_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 4710 /* i64.atomic.load16_u */, WebAssembly::ATOMIC_LOAD16_U_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 4710 /* i64.atomic.load16_u */, WebAssembly::ATOMIC_LOAD16_U_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 4730 /* i64.atomic.load32_u */, WebAssembly::ATOMIC_LOAD32_U_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 4730 /* i64.atomic.load32_u */, WebAssembly::ATOMIC_LOAD32_U_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 4750 /* i64.atomic.load8_u */, WebAssembly::ATOMIC_LOAD8_U_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 4750 /* i64.atomic.load8_u */, WebAssembly::ATOMIC_LOAD8_U_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 4769 /* i64.atomic.rmw.add */, WebAssembly::ATOMIC_RMW_ADD_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 4769 /* i64.atomic.rmw.add */, WebAssembly::ATOMIC_RMW_ADD_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 4788 /* i64.atomic.rmw.and */, WebAssembly::ATOMIC_RMW_AND_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 4788 /* i64.atomic.rmw.and */, WebAssembly::ATOMIC_RMW_AND_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 4807 /* i64.atomic.rmw.cmpxchg */, WebAssembly::ATOMIC_RMW_CMPXCHG_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 4807 /* i64.atomic.rmw.cmpxchg */, WebAssembly::ATOMIC_RMW_CMPXCHG_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 4830 /* i64.atomic.rmw.or */, WebAssembly::ATOMIC_RMW_OR_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 4830 /* i64.atomic.rmw.or */, WebAssembly::ATOMIC_RMW_OR_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 4848 /* i64.atomic.rmw.sub */, WebAssembly::ATOMIC_RMW_SUB_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 4848 /* i64.atomic.rmw.sub */, WebAssembly::ATOMIC_RMW_SUB_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 4867 /* i64.atomic.rmw.xchg */, WebAssembly::ATOMIC_RMW_XCHG_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 4867 /* i64.atomic.rmw.xchg */, WebAssembly::ATOMIC_RMW_XCHG_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 4887 /* i64.atomic.rmw.xor */, WebAssembly::ATOMIC_RMW_XOR_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 4887 /* i64.atomic.rmw.xor */, WebAssembly::ATOMIC_RMW_XOR_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 4906 /* i64.atomic.rmw16.add_u */, WebAssembly::ATOMIC_RMW16_U_ADD_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 4906 /* i64.atomic.rmw16.add_u */, WebAssembly::ATOMIC_RMW16_U_ADD_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 4929 /* i64.atomic.rmw16.and_u */, WebAssembly::ATOMIC_RMW16_U_AND_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 4929 /* i64.atomic.rmw16.and_u */, WebAssembly::ATOMIC_RMW16_U_AND_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 4952 /* i64.atomic.rmw16.cmpxchg_u */, WebAssembly::ATOMIC_RMW16_U_CMPXCHG_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 4952 /* i64.atomic.rmw16.cmpxchg_u */, WebAssembly::ATOMIC_RMW16_U_CMPXCHG_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 4979 /* i64.atomic.rmw16.or_u */, WebAssembly::ATOMIC_RMW16_U_OR_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 4979 /* i64.atomic.rmw16.or_u */, WebAssembly::ATOMIC_RMW16_U_OR_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5001 /* i64.atomic.rmw16.sub_u */, WebAssembly::ATOMIC_RMW16_U_SUB_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5001 /* i64.atomic.rmw16.sub_u */, WebAssembly::ATOMIC_RMW16_U_SUB_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5024 /* i64.atomic.rmw16.xchg_u */, WebAssembly::ATOMIC_RMW16_U_XCHG_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5024 /* i64.atomic.rmw16.xchg_u */, WebAssembly::ATOMIC_RMW16_U_XCHG_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5048 /* i64.atomic.rmw16.xor_u */, WebAssembly::ATOMIC_RMW16_U_XOR_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5048 /* i64.atomic.rmw16.xor_u */, WebAssembly::ATOMIC_RMW16_U_XOR_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5071 /* i64.atomic.rmw32.add_u */, WebAssembly::ATOMIC_RMW32_U_ADD_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5071 /* i64.atomic.rmw32.add_u */, WebAssembly::ATOMIC_RMW32_U_ADD_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5094 /* i64.atomic.rmw32.and_u */, WebAssembly::ATOMIC_RMW32_U_AND_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5094 /* i64.atomic.rmw32.and_u */, WebAssembly::ATOMIC_RMW32_U_AND_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5117 /* i64.atomic.rmw32.cmpxchg_u */, WebAssembly::ATOMIC_RMW32_U_CMPXCHG_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5117 /* i64.atomic.rmw32.cmpxchg_u */, WebAssembly::ATOMIC_RMW32_U_CMPXCHG_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5144 /* i64.atomic.rmw32.or_u */, WebAssembly::ATOMIC_RMW32_U_OR_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5144 /* i64.atomic.rmw32.or_u */, WebAssembly::ATOMIC_RMW32_U_OR_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5166 /* i64.atomic.rmw32.sub_u */, WebAssembly::ATOMIC_RMW32_U_SUB_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5166 /* i64.atomic.rmw32.sub_u */, WebAssembly::ATOMIC_RMW32_U_SUB_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5189 /* i64.atomic.rmw32.xchg_u */, WebAssembly::ATOMIC_RMW32_U_XCHG_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5189 /* i64.atomic.rmw32.xchg_u */, WebAssembly::ATOMIC_RMW32_U_XCHG_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5213 /* i64.atomic.rmw32.xor_u */, WebAssembly::ATOMIC_RMW32_U_XOR_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5213 /* i64.atomic.rmw32.xor_u */, WebAssembly::ATOMIC_RMW32_U_XOR_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5236 /* i64.atomic.rmw8.add_u */, WebAssembly::ATOMIC_RMW8_U_ADD_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5236 /* i64.atomic.rmw8.add_u */, WebAssembly::ATOMIC_RMW8_U_ADD_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5258 /* i64.atomic.rmw8.and_u */, WebAssembly::ATOMIC_RMW8_U_AND_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5258 /* i64.atomic.rmw8.and_u */, WebAssembly::ATOMIC_RMW8_U_AND_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5280 /* i64.atomic.rmw8.cmpxchg_u */, WebAssembly::ATOMIC_RMW8_U_CMPXCHG_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5280 /* i64.atomic.rmw8.cmpxchg_u */, WebAssembly::ATOMIC_RMW8_U_CMPXCHG_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5306 /* i64.atomic.rmw8.or_u */, WebAssembly::ATOMIC_RMW8_U_OR_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5306 /* i64.atomic.rmw8.or_u */, WebAssembly::ATOMIC_RMW8_U_OR_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5327 /* i64.atomic.rmw8.sub_u */, WebAssembly::ATOMIC_RMW8_U_SUB_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5327 /* i64.atomic.rmw8.sub_u */, WebAssembly::ATOMIC_RMW8_U_SUB_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5349 /* i64.atomic.rmw8.xchg_u */, WebAssembly::ATOMIC_RMW8_U_XCHG_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5349 /* i64.atomic.rmw8.xchg_u */, WebAssembly::ATOMIC_RMW8_U_XCHG_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5372 /* i64.atomic.rmw8.xor_u */, WebAssembly::ATOMIC_RMW8_U_XOR_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5372 /* i64.atomic.rmw8.xor_u */, WebAssembly::ATOMIC_RMW8_U_XOR_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5394 /* i64.atomic.store */, WebAssembly::ATOMIC_STORE_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5394 /* i64.atomic.store */, WebAssembly::ATOMIC_STORE_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5411 /* i64.atomic.store16 */, WebAssembly::ATOMIC_STORE16_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5411 /* i64.atomic.store16 */, WebAssembly::ATOMIC_STORE16_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5430 /* i64.atomic.store32 */, WebAssembly::ATOMIC_STORE32_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5430 /* i64.atomic.store32 */, WebAssembly::ATOMIC_STORE32_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5449 /* i64.atomic.store8 */, WebAssembly::ATOMIC_STORE8_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5449 /* i64.atomic.store8 */, WebAssembly::ATOMIC_STORE8_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 5467 /* i64.clz */, WebAssembly::CLZ_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5475 /* i64.const */, WebAssembly::CONST_I64_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 5485 /* i64.ctz */, WebAssembly::CTZ_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5493 /* i64.div_s */, WebAssembly::DIV_S_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5503 /* i64.div_u */, WebAssembly::DIV_U_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5513 /* i64.eq */, WebAssembly::EQ_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5520 /* i64.eqz */, WebAssembly::EQZ_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5528 /* i64.extend16_s */, WebAssembly::I64_EXTEND16_S_I64_S, Convert_NoOperands, AMFBS_HasSignExt, {  }, },
  { 5543 /* i64.extend32_s */, WebAssembly::I64_EXTEND32_S_I64_S, Convert_NoOperands, AMFBS_HasSignExt, {  }, },
  { 5558 /* i64.extend8_s */, WebAssembly::I64_EXTEND8_S_I64_S, Convert_NoOperands, AMFBS_HasSignExt, {  }, },
  { 5572 /* i64.extend_i32_s */, WebAssembly::I64_EXTEND_S_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5589 /* i64.extend_i32_u */, WebAssembly::I64_EXTEND_U_I32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5606 /* i64.ge_s */, WebAssembly::GE_S_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5615 /* i64.ge_u */, WebAssembly::GE_U_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5624 /* i64.gt_s */, WebAssembly::GT_S_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5633 /* i64.gt_u */, WebAssembly::GT_U_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5642 /* i64.le_s */, WebAssembly::LE_S_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5651 /* i64.le_u */, WebAssembly::LE_U_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5660 /* i64.load */, WebAssembly::LOAD_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 5660 /* i64.load */, WebAssembly::LOAD_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 5669 /* i64.load16_s */, WebAssembly::LOAD16_S_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 5669 /* i64.load16_s */, WebAssembly::LOAD16_S_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 5682 /* i64.load16_u */, WebAssembly::LOAD16_U_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 5682 /* i64.load16_u */, WebAssembly::LOAD16_U_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 5695 /* i64.load32_s */, WebAssembly::LOAD32_S_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 5695 /* i64.load32_s */, WebAssembly::LOAD32_S_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 5708 /* i64.load32_u */, WebAssembly::LOAD32_U_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 5708 /* i64.load32_u */, WebAssembly::LOAD32_U_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 5721 /* i64.load8_s */, WebAssembly::LOAD8_S_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 5721 /* i64.load8_s */, WebAssembly::LOAD8_S_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 5733 /* i64.load8_u */, WebAssembly::LOAD8_U_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 5733 /* i64.load8_u */, WebAssembly::LOAD8_U_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 5745 /* i64.lt_s */, WebAssembly::LT_S_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5754 /* i64.lt_u */, WebAssembly::LT_U_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5763 /* i64.mul */, WebAssembly::MUL_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5771 /* i64.ne */, WebAssembly::NE_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5778 /* i64.or */, WebAssembly::OR_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5785 /* i64.popcnt */, WebAssembly::POPCNT_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5796 /* i64.reinterpret_f64 */, WebAssembly::I64_REINTERPRET_F64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5816 /* i64.rem_s */, WebAssembly::REM_S_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5826 /* i64.rem_u */, WebAssembly::REM_U_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5836 /* i64.rotl */, WebAssembly::ROTL_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5845 /* i64.rotr */, WebAssembly::ROTR_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5854 /* i64.select */, WebAssembly::SELECT_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5865 /* i64.shl */, WebAssembly::SHL_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5873 /* i64.shr_s */, WebAssembly::SHR_S_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5883 /* i64.shr_u */, WebAssembly::SHR_U_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5893 /* i64.store */, WebAssembly::STORE_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 5893 /* i64.store */, WebAssembly::STORE_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 5903 /* i64.store16 */, WebAssembly::STORE16_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 5903 /* i64.store16 */, WebAssembly::STORE16_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 5915 /* i64.store32 */, WebAssembly::STORE32_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 5915 /* i64.store32 */, WebAssembly::STORE32_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 5927 /* i64.store8 */, WebAssembly::STORE8_I64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 5927 /* i64.store8 */, WebAssembly::STORE8_I64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_None, { MCK_Imm, MCK_Imm }, },
  { 5938 /* i64.sub */, WebAssembly::SUB_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5946 /* i64.trunc_f32_s */, WebAssembly::I64_TRUNC_S_F32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5962 /* i64.trunc_f32_u */, WebAssembly::I64_TRUNC_U_F32_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5978 /* i64.trunc_f64_s */, WebAssembly::I64_TRUNC_S_F64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 5994 /* i64.trunc_f64_u */, WebAssembly::I64_TRUNC_U_F64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 6010 /* i64.trunc_sat_f32_s */, WebAssembly::I64_TRUNC_S_SAT_F32_S, Convert_NoOperands, AMFBS_HasNontrappingFPToInt, {  }, },
  { 6030 /* i64.trunc_sat_f32_u */, WebAssembly::I64_TRUNC_U_SAT_F32_S, Convert_NoOperands, AMFBS_HasNontrappingFPToInt, {  }, },
  { 6050 /* i64.trunc_sat_f64_s */, WebAssembly::I64_TRUNC_S_SAT_F64_S, Convert_NoOperands, AMFBS_HasNontrappingFPToInt, {  }, },
  { 6070 /* i64.trunc_sat_f64_u */, WebAssembly::I64_TRUNC_U_SAT_F64_S, Convert_NoOperands, AMFBS_HasNontrappingFPToInt, {  }, },
  { 6090 /* i64.xor */, WebAssembly::XOR_I64_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 6098 /* i64x2.abs */, WebAssembly::ABS_I64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6108 /* i64x2.add */, WebAssembly::ADD_I64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6118 /* i64x2.all_true */, WebAssembly::ALLTRUE_I64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6133 /* i64x2.bitmask */, WebAssembly::BITMASK_I64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6147 /* i64x2.eq */, WebAssembly::EQ_I64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6156 /* i64x2.extend_high_i32x4_s */, WebAssembly::extend_high_s_I64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6182 /* i64x2.extend_high_i32x4_u */, WebAssembly::extend_high_u_I64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6208 /* i64x2.extend_low_i32x4_s */, WebAssembly::extend_low_s_I64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6233 /* i64x2.extend_low_i32x4_u */, WebAssembly::extend_low_u_I64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6258 /* i64x2.extmul_high_i32x4_s */, WebAssembly::EXTMUL_HIGH_S_I64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6284 /* i64x2.extmul_high_i32x4_u */, WebAssembly::EXTMUL_HIGH_U_I64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6310 /* i64x2.extmul_low_i32x4_s */, WebAssembly::EXTMUL_LOW_S_I64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6335 /* i64x2.extmul_low_i32x4_u */, WebAssembly::EXTMUL_LOW_U_I64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6360 /* i64x2.extract_lane */, WebAssembly::EXTRACT_LANE_I64x2_S, Convert__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm }, },
  { 6379 /* i64x2.ge_s */, WebAssembly::GE_S_I64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6390 /* i64x2.gt_s */, WebAssembly::GT_S_I64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6401 /* i64x2.le_s */, WebAssembly::LE_S_I64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6412 /* i64x2.load32x2_s */, WebAssembly::LOAD_EXTEND_S_I64x2_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm }, },
  { 6412 /* i64x2.load32x2_s */, WebAssembly::LOAD_EXTEND_S_I64x2_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm }, },
  { 6429 /* i64x2.load32x2_u */, WebAssembly::LOAD_EXTEND_U_I64x2_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm }, },
  { 6429 /* i64x2.load32x2_u */, WebAssembly::LOAD_EXTEND_U_I64x2_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm }, },
  { 6446 /* i64x2.lt_s */, WebAssembly::LT_S_I64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6457 /* i64x2.mul */, WebAssembly::MUL_I64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6467 /* i64x2.ne */, WebAssembly::NE_I64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6476 /* i64x2.neg */, WebAssembly::NEG_I64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6486 /* i64x2.relaxed_laneselect */, WebAssembly::LANESELECT_I64x2_S, Convert_NoOperands, AMFBS_HasRelaxedSIMD, {  }, },
  { 6511 /* i64x2.replace_lane */, WebAssembly::REPLACE_LANE_I64x2_S, Convert__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm }, },
  { 6530 /* i64x2.shl */, WebAssembly::SHL_I64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6540 /* i64x2.shr_s */, WebAssembly::SHR_S_I64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6552 /* i64x2.shr_u */, WebAssembly::SHR_U_I64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6564 /* i64x2.splat */, WebAssembly::SPLAT_I64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6576 /* i64x2.sub */, WebAssembly::SUB_I64x2_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6586 /* i8x16.abs */, WebAssembly::ABS_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6596 /* i8x16.add */, WebAssembly::ADD_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6606 /* i8x16.add_sat_s */, WebAssembly::ADD_SAT_S_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6622 /* i8x16.add_sat_u */, WebAssembly::ADD_SAT_U_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6638 /* i8x16.all_true */, WebAssembly::ALLTRUE_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6653 /* i8x16.avgr_u */, WebAssembly::AVGR_U_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6666 /* i8x16.bitmask */, WebAssembly::BITMASK_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6680 /* i8x16.eq */, WebAssembly::EQ_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6689 /* i8x16.extract_lane_s */, WebAssembly::EXTRACT_LANE_I8x16_s_S, Convert__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm }, },
  { 6710 /* i8x16.extract_lane_u */, WebAssembly::EXTRACT_LANE_I8x16_u_S, Convert__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm }, },
  { 6731 /* i8x16.ge_s */, WebAssembly::GE_S_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6742 /* i8x16.ge_u */, WebAssembly::GE_U_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6753 /* i8x16.gt_s */, WebAssembly::GT_S_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6764 /* i8x16.gt_u */, WebAssembly::GT_U_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6775 /* i8x16.le_s */, WebAssembly::LE_S_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6786 /* i8x16.le_u */, WebAssembly::LE_U_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6797 /* i8x16.lt_s */, WebAssembly::LT_S_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6808 /* i8x16.lt_u */, WebAssembly::LT_U_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6819 /* i8x16.max_s */, WebAssembly::MAX_S_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6831 /* i8x16.max_u */, WebAssembly::MAX_U_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6843 /* i8x16.min_s */, WebAssembly::MIN_S_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6855 /* i8x16.min_u */, WebAssembly::MIN_U_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6867 /* i8x16.narrow_i16x8_s */, WebAssembly::NARROW_S_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6888 /* i8x16.narrow_i16x8_u */, WebAssembly::NARROW_U_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6909 /* i8x16.ne */, WebAssembly::NE_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6918 /* i8x16.neg */, WebAssembly::NEG_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6928 /* i8x16.popcnt */, WebAssembly::POPCNT_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 6941 /* i8x16.relaxed_laneselect */, WebAssembly::LANESELECT_I8x16_S, Convert_NoOperands, AMFBS_HasRelaxedSIMD, {  }, },
  { 6966 /* i8x16.relaxed_swizzle */, WebAssembly::RELAXED_SWIZZLE_S, Convert_NoOperands, AMFBS_HasRelaxedSIMD, {  }, },
  { 6988 /* i8x16.replace_lane */, WebAssembly::REPLACE_LANE_I8x16_S, Convert__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm }, },
  { 7007 /* i8x16.shl */, WebAssembly::SHL_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 7017 /* i8x16.shr_s */, WebAssembly::SHR_S_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 7029 /* i8x16.shr_u */, WebAssembly::SHR_U_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 7041 /* i8x16.shuffle */, WebAssembly::SHUFFLE_S, Convert__Imm1_0__Imm1_1__Imm1_2__Imm1_3__Imm1_4__Imm1_5__Imm1_6__Imm1_7__Imm1_8__Imm1_9__Imm1_10__Imm1_11__Imm1_12__Imm1_13__Imm1_14__Imm1_15, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm }, },
  { 7055 /* i8x16.splat */, WebAssembly::SPLAT_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 7067 /* i8x16.sub */, WebAssembly::SUB_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 7077 /* i8x16.sub_sat_s */, WebAssembly::SUB_SAT_S_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 7093 /* i8x16.sub_sat_u */, WebAssembly::SUB_SAT_U_I8x16_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 7109 /* i8x16.swizzle */, WebAssembly::SWIZZLE_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 7123 /* if */, WebAssembly::IF_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 7126 /* local.get */, WebAssembly::LOCAL_GET_EXTERNREF_S, Convert__Imm1_0, AMFBS_HasReferenceTypes, { MCK_Imm }, },
  { 7126 /* local.get */, WebAssembly::LOCAL_GET_FUNCREF_S, Convert__Imm1_0, AMFBS_HasReferenceTypes, { MCK_Imm }, },
  { 7126 /* local.get */, WebAssembly::LOCAL_GET_V128_S, Convert__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm }, },
  { 7126 /* local.get */, WebAssembly::LOCAL_GET_F32_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 7126 /* local.get */, WebAssembly::LOCAL_GET_F64_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 7126 /* local.get */, WebAssembly::LOCAL_GET_I32_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 7126 /* local.get */, WebAssembly::LOCAL_GET_I64_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 7136 /* local.set */, WebAssembly::LOCAL_SET_EXTERNREF_S, Convert__Imm1_0, AMFBS_HasReferenceTypes, { MCK_Imm }, },
  { 7136 /* local.set */, WebAssembly::LOCAL_SET_FUNCREF_S, Convert__Imm1_0, AMFBS_HasReferenceTypes, { MCK_Imm }, },
  { 7136 /* local.set */, WebAssembly::LOCAL_SET_V128_S, Convert__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm }, },
  { 7136 /* local.set */, WebAssembly::LOCAL_SET_F32_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 7136 /* local.set */, WebAssembly::LOCAL_SET_F64_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 7136 /* local.set */, WebAssembly::LOCAL_SET_I32_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 7136 /* local.set */, WebAssembly::LOCAL_SET_I64_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 7146 /* local.tee */, WebAssembly::LOCAL_TEE_EXTERNREF_S, Convert__Imm1_0, AMFBS_HasReferenceTypes, { MCK_Imm }, },
  { 7146 /* local.tee */, WebAssembly::LOCAL_TEE_FUNCREF_S, Convert__Imm1_0, AMFBS_HasReferenceTypes, { MCK_Imm }, },
  { 7146 /* local.tee */, WebAssembly::LOCAL_TEE_V128_S, Convert__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm }, },
  { 7146 /* local.tee */, WebAssembly::LOCAL_TEE_F32_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 7146 /* local.tee */, WebAssembly::LOCAL_TEE_F64_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 7146 /* local.tee */, WebAssembly::LOCAL_TEE_I32_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 7146 /* local.tee */, WebAssembly::LOCAL_TEE_I64_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 7156 /* loop */, WebAssembly::LOOP_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 7161 /* memory.atomic.notify */, WebAssembly::MEMORY_ATOMIC_NOTIFY_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 7161 /* memory.atomic.notify */, WebAssembly::MEMORY_ATOMIC_NOTIFY_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 7182 /* memory.atomic.wait32 */, WebAssembly::MEMORY_ATOMIC_WAIT32_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 7182 /* memory.atomic.wait32 */, WebAssembly::MEMORY_ATOMIC_WAIT32_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 7203 /* memory.atomic.wait64 */, WebAssembly::MEMORY_ATOMIC_WAIT64_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 7203 /* memory.atomic.wait64 */, WebAssembly::MEMORY_ATOMIC_WAIT64_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasAtomics, { MCK_Imm, MCK_Imm }, },
  { 7224 /* memory.copy */, WebAssembly::anonymous_6249MEMORY_COPY_A32_S, Convert__Imm1_0__Imm1_1, AMFBS_HasBulkMemory, { MCK_Imm, MCK_Imm }, },
  { 7224 /* memory.copy */, WebAssembly::anonymous_6250MEMORY_COPY_A64_S, Convert__Imm1_0__Imm1_1, AMFBS_HasBulkMemory, { MCK_Imm, MCK_Imm }, },
  { 7236 /* memory.fill */, WebAssembly::anonymous_6249MEMORY_FILL_A32_S, Convert__Imm1_0, AMFBS_HasBulkMemory, { MCK_Imm }, },
  { 7236 /* memory.fill */, WebAssembly::anonymous_6250MEMORY_FILL_A64_S, Convert__Imm1_0, AMFBS_HasBulkMemory, { MCK_Imm }, },
  { 7248 /* memory.grow */, WebAssembly::anonymous_4396MEMORY_GROW_A32_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 7248 /* memory.grow */, WebAssembly::anonymous_4397MEMORY_GROW_A64_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 7260 /* memory.init */, WebAssembly::anonymous_6249MEMORY_INIT_A32_S, Convert__Imm1_0__Imm1_1, AMFBS_HasBulkMemory, { MCK_Imm, MCK_Imm }, },
  { 7260 /* memory.init */, WebAssembly::anonymous_6250MEMORY_INIT_A64_S, Convert__Imm1_0__Imm1_1, AMFBS_HasBulkMemory, { MCK_Imm, MCK_Imm }, },
  { 7272 /* memory.size */, WebAssembly::anonymous_4396MEMORY_SIZE_A32_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 7272 /* memory.size */, WebAssembly::anonymous_4397MEMORY_SIZE_A64_S, Convert__Imm1_0, AMFBS_None, { MCK_Imm }, },
  { 7284 /* nop */, WebAssembly::NOP_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 7288 /* ref.is_null */, WebAssembly::REF_IS_NULL_EXTERNREF_S, Convert_NoOperands, AMFBS_HasReferenceTypes, {  }, },
  { 7288 /* ref.is_null */, WebAssembly::REF_IS_NULL_FUNCREF_S, Convert_NoOperands, AMFBS_HasReferenceTypes, {  }, },
  { 7300 /* ref.null_extern */, WebAssembly::REF_NULL_EXTERNREF_S, Convert_NoOperands, AMFBS_HasReferenceTypes, {  }, },
  { 7316 /* ref.null_func */, WebAssembly::REF_NULL_FUNCREF_S, Convert_NoOperands, AMFBS_HasReferenceTypes, {  }, },
  { 7330 /* rethrow */, WebAssembly::RETHROW_S, Convert__Imm1_0, AMFBS_HasExceptionHandling, { MCK_Imm }, },
  { 7338 /* return */, WebAssembly::RETURN_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 7345 /* return_call */, WebAssembly::RET_CALL_S, Convert__Imm1_0, AMFBS_HasTailCall, { MCK_Imm }, },
  { 7357 /* return_call_indirect */, WebAssembly::RET_CALL_INDIRECT_S, Convert__Imm1_0__Imm1_1, AMFBS_HasTailCall, { MCK_Imm, MCK_Imm }, },
  { 7378 /* return_call_results */, WebAssembly::RET_CALL_RESULTS_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 7398 /* table.copy */, WebAssembly::TABLE_COPY_S, Convert__Imm1_0__Imm1_1, AMFBS_HasReferenceTypes, { MCK_Imm, MCK_Imm }, },
  { 7409 /* table.fill */, WebAssembly::TABLE_FILL_EXTERNREF_S, Convert__Imm1_0, AMFBS_HasReferenceTypes, { MCK_Imm }, },
  { 7409 /* table.fill */, WebAssembly::TABLE_FILL_FUNCREF_S, Convert__Imm1_0, AMFBS_HasReferenceTypes, { MCK_Imm }, },
  { 7420 /* table.get */, WebAssembly::TABLE_GET_EXTERNREF_S, Convert__Imm1_0, AMFBS_HasReferenceTypes, { MCK_Imm }, },
  { 7420 /* table.get */, WebAssembly::TABLE_GET_FUNCREF_S, Convert__Imm1_0, AMFBS_HasReferenceTypes, { MCK_Imm }, },
  { 7430 /* table.grow */, WebAssembly::TABLE_GROW_EXTERNREF_S, Convert__Imm1_0, AMFBS_HasReferenceTypes, { MCK_Imm }, },
  { 7430 /* table.grow */, WebAssembly::TABLE_GROW_FUNCREF_S, Convert__Imm1_0, AMFBS_HasReferenceTypes, { MCK_Imm }, },
  { 7441 /* table.set */, WebAssembly::TABLE_SET_EXTERNREF_S, Convert__Imm1_0, AMFBS_HasReferenceTypes, { MCK_Imm }, },
  { 7441 /* table.set */, WebAssembly::TABLE_SET_FUNCREF_S, Convert__Imm1_0, AMFBS_HasReferenceTypes, { MCK_Imm }, },
  { 7451 /* table.size */, WebAssembly::TABLE_SIZE_S, Convert__Imm1_0, AMFBS_HasReferenceTypes, { MCK_Imm }, },
  { 7462 /* throw */, WebAssembly::THROW_S, Convert__Imm1_0, AMFBS_HasExceptionHandling, { MCK_Imm }, },
  { 7468 /* try */, WebAssembly::TRY_S, Convert__Imm1_0, AMFBS_HasExceptionHandling, { MCK_Imm }, },
  { 7472 /* unreachable */, WebAssembly::DEBUG_UNREACHABLE_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 7472 /* unreachable */, WebAssembly::UNREACHABLE_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 7484 /* v128.and */, WebAssembly::AND_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 7493 /* v128.andnot */, WebAssembly::ANDNOT_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 7505 /* v128.any_true */, WebAssembly::ANYTRUE_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 7519 /* v128.bitselect */, WebAssembly::BITSELECT_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 7534 /* v128.const */, WebAssembly::CONST_V128_I64x2_S, Convert__Imm1_0__Imm1_1, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm }, },
  { 7534 /* v128.const */, WebAssembly::CONST_V128_F64x2_S, Convert__FPImmf641_0__FPImmf641_1, AMFBS_HasSIMD128, { MCK_FPImmf64, MCK_FPImmf64 }, },
  { 7534 /* v128.const */, WebAssembly::CONST_V128_I32x4_S, Convert__Imm1_0__Imm1_1__Imm1_2__Imm1_3, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm }, },
  { 7534 /* v128.const */, WebAssembly::CONST_V128_F32x4_S, Convert__FPImmf321_0__FPImmf321_1__FPImmf321_2__FPImmf321_3, AMFBS_HasSIMD128, { MCK_FPImmf32, MCK_FPImmf32, MCK_FPImmf32, MCK_FPImmf32 }, },
  { 7534 /* v128.const */, WebAssembly::CONST_V128_I16x8_S, Convert__Imm1_0__Imm1_1__Imm1_2__Imm1_3__Imm1_4__Imm1_5__Imm1_6__Imm1_7, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm }, },
  { 7534 /* v128.const */, WebAssembly::CONST_V128_I8x16_S, Convert__Imm1_0__Imm1_1__Imm1_2__Imm1_3__Imm1_4__Imm1_5__Imm1_6__Imm1_7__Imm1_8__Imm1_9__Imm1_10__Imm1_11__Imm1_12__Imm1_13__Imm1_14__Imm1_15, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm, MCK_Imm }, },
  { 7545 /* v128.load */, WebAssembly::LOAD_V128_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm }, },
  { 7545 /* v128.load */, WebAssembly::LOAD_V128_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm }, },
  { 7555 /* v128.load16_lane */, WebAssembly::LOAD_LANE_I16x8_A32_S, Convert__Imm1_1__Imm1_0__Imm1_2, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm, MCK_Imm }, },
  { 7555 /* v128.load16_lane */, WebAssembly::LOAD_LANE_I16x8_A64_S, Convert__Imm1_1__Imm1_0__Imm1_2, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm, MCK_Imm }, },
  { 7572 /* v128.load16_splat */, WebAssembly::LOAD16_SPLAT_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm }, },
  { 7572 /* v128.load16_splat */, WebAssembly::LOAD16_SPLAT_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm }, },
  { 7590 /* v128.load32_lane */, WebAssembly::LOAD_LANE_I32x4_A32_S, Convert__Imm1_1__Imm1_0__Imm1_2, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm, MCK_Imm }, },
  { 7590 /* v128.load32_lane */, WebAssembly::LOAD_LANE_I32x4_A64_S, Convert__Imm1_1__Imm1_0__Imm1_2, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm, MCK_Imm }, },
  { 7607 /* v128.load32_splat */, WebAssembly::LOAD32_SPLAT_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm }, },
  { 7607 /* v128.load32_splat */, WebAssembly::LOAD32_SPLAT_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm }, },
  { 7625 /* v128.load32_zero */, WebAssembly::LOAD_ZERO_I32x4_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm }, },
  { 7625 /* v128.load32_zero */, WebAssembly::LOAD_ZERO_I32x4_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm }, },
  { 7642 /* v128.load64_lane */, WebAssembly::LOAD_LANE_I64x2_A32_S, Convert__Imm1_1__Imm1_0__Imm1_2, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm, MCK_Imm }, },
  { 7642 /* v128.load64_lane */, WebAssembly::LOAD_LANE_I64x2_A64_S, Convert__Imm1_1__Imm1_0__Imm1_2, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm, MCK_Imm }, },
  { 7659 /* v128.load64_splat */, WebAssembly::LOAD64_SPLAT_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm }, },
  { 7659 /* v128.load64_splat */, WebAssembly::LOAD64_SPLAT_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm }, },
  { 7677 /* v128.load64_zero */, WebAssembly::LOAD_ZERO_I64x2_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm }, },
  { 7677 /* v128.load64_zero */, WebAssembly::LOAD_ZERO_I64x2_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm }, },
  { 7694 /* v128.load8_lane */, WebAssembly::LOAD_LANE_I8x16_A32_S, Convert__Imm1_1__Imm1_0__Imm1_2, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm, MCK_Imm }, },
  { 7694 /* v128.load8_lane */, WebAssembly::LOAD_LANE_I8x16_A64_S, Convert__Imm1_1__Imm1_0__Imm1_2, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm, MCK_Imm }, },
  { 7710 /* v128.load8_splat */, WebAssembly::LOAD8_SPLAT_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm }, },
  { 7710 /* v128.load8_splat */, WebAssembly::LOAD8_SPLAT_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm }, },
  { 7727 /* v128.not */, WebAssembly::NOT_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 7736 /* v128.or */, WebAssembly::OR_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
  { 7744 /* v128.select */, WebAssembly::SELECT_V128_S, Convert_NoOperands, AMFBS_None, {  }, },
  { 7756 /* v128.store */, WebAssembly::STORE_V128_A32_S, Convert__Imm1_1__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm }, },
  { 7756 /* v128.store */, WebAssembly::STORE_V128_A64_S, Convert__Imm1_1__Imm1_0, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm }, },
  { 7767 /* v128.store16_lane */, WebAssembly::STORE_LANE_I16x8_A32_S, Convert__Imm1_1__Imm1_0__Imm1_2, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm, MCK_Imm }, },
  { 7767 /* v128.store16_lane */, WebAssembly::STORE_LANE_I16x8_A64_S, Convert__Imm1_1__Imm1_0__Imm1_2, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm, MCK_Imm }, },
  { 7785 /* v128.store32_lane */, WebAssembly::STORE_LANE_I32x4_A32_S, Convert__Imm1_1__Imm1_0__Imm1_2, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm, MCK_Imm }, },
  { 7785 /* v128.store32_lane */, WebAssembly::STORE_LANE_I32x4_A64_S, Convert__Imm1_1__Imm1_0__Imm1_2, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm, MCK_Imm }, },
  { 7803 /* v128.store64_lane */, WebAssembly::STORE_LANE_I64x2_A32_S, Convert__Imm1_1__Imm1_0__Imm1_2, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm, MCK_Imm }, },
  { 7803 /* v128.store64_lane */, WebAssembly::STORE_LANE_I64x2_A64_S, Convert__Imm1_1__Imm1_0__Imm1_2, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm, MCK_Imm }, },
  { 7821 /* v128.store8_lane */, WebAssembly::STORE_LANE_I8x16_A32_S, Convert__Imm1_1__Imm1_0__Imm1_2, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm, MCK_Imm }, },
  { 7821 /* v128.store8_lane */, WebAssembly::STORE_LANE_I8x16_A64_S, Convert__Imm1_1__Imm1_0__Imm1_2, AMFBS_HasSIMD128, { MCK_Imm, MCK_Imm, MCK_Imm }, },
  { 7838 /* v128.xor */, WebAssembly::XOR_S, Convert_NoOperands, AMFBS_HasSIMD128, {  }, },
};

#include "llvm/Support/Debug.h"
#include "llvm/Support/Format.h"

unsigned WebAssemblyAsmParser::
MatchInstructionImpl(const OperandVector &Operands,
                     MCInst &Inst,
                     uint64_t &ErrorInfo,
                     FeatureBitset &MissingFeatures,
                     bool matchingInlineAsm, unsigned VariantID) {
  // Eliminate obvious mismatches.
  if (Operands.size() > 17) {
    ErrorInfo = 17;
    return Match_InvalidOperand;
  }

  // Get the current feature set.
  const FeatureBitset &AvailableFeatures = getAvailableFeatures();

  // Get the instruction mnemonic, which is the first token.
  StringRef Mnemonic = ((WebAssemblyOperand &)*Operands[0]).getToken();

  // Some state to try to produce better error messages.
  bool HadMatchOtherThanFeatures = false;
  bool HadMatchOtherThanPredicate = false;
  unsigned RetCode = Match_InvalidOperand;
  MissingFeatures.set();
  // Set ErrorInfo to the operand that mismatches if it is
  // wrong for all instances of the instruction.
  ErrorInfo = ~0ULL;
  // Find the appropriate table for this asm variant.
  const MatchEntry *Start, *End;
  switch (VariantID) {
  default: llvm_unreachable("invalid variant!");
  case 0: Start = std::begin(MatchTable0); End = std::end(MatchTable0); break;
  }
  // Search the table.
  auto MnemonicRange = std::equal_range(Start, End, Mnemonic, LessOpcode());

  DEBUG_WITH_TYPE("asm-matcher", dbgs() << "AsmMatcher: found " <<
  std::distance(MnemonicRange.first, MnemonicRange.second) <<
  " encodings with mnemonic '" << Mnemonic << "'\n");

  // Return a more specific error code if no mnemonics match.
  if (MnemonicRange.first == MnemonicRange.second)
    return Match_MnemonicFail;

  for (const MatchEntry *it = MnemonicRange.first, *ie = MnemonicRange.second;
       it != ie; ++it) {
    const FeatureBitset &RequiredFeatures = FeatureBitsets[it->RequiredFeaturesIdx];
    bool HasRequiredFeatures =
      (AvailableFeatures & RequiredFeatures) == RequiredFeatures;
    DEBUG_WITH_TYPE("asm-matcher", dbgs() << "Trying to match opcode "
                                          << MII.getName(it->Opcode) << "\n");
    // equal_range guarantees that instruction mnemonic matches.
    assert(Mnemonic == it->getMnemonic());
    bool OperandsValid = true;
    for (unsigned FormalIdx = 0, ActualIdx = 1; FormalIdx != 16; ++FormalIdx) {
      auto Formal = static_cast<MatchClassKind>(it->Classes[FormalIdx]);
      DEBUG_WITH_TYPE("asm-matcher",
                      dbgs() << "  Matching formal operand class " << getMatchClassName(Formal)
                             << " against actual operand at index " << ActualIdx);
      if (ActualIdx < Operands.size())
        DEBUG_WITH_TYPE("asm-matcher", dbgs() << " (";
                        Operands[ActualIdx]->print(dbgs()); dbgs() << "): ");
      else
        DEBUG_WITH_TYPE("asm-matcher", dbgs() << ": ");
      if (ActualIdx >= Operands.size()) {
        DEBUG_WITH_TYPE("asm-matcher", dbgs() << "actual operand index out of range\n");
        if (Formal == InvalidMatchClass) {
          break;
        }
        if (isSubclass(Formal, OptionalMatchClass)) {
          continue;
        }
        OperandsValid = false;
        ErrorInfo = ActualIdx;
        break;
      }
      MCParsedAsmOperand &Actual = *Operands[ActualIdx];
      unsigned Diag = validateOperandClass(Actual, Formal);
      if (Diag == Match_Success) {
        DEBUG_WITH_TYPE("asm-matcher",
                        dbgs() << "match success using generic matcher\n");
        ++ActualIdx;
        continue;
      }
      // If the generic handler indicates an invalid operand
      // failure, check for a special case.
      if (Diag != Match_Success) {
        unsigned TargetDiag = validateTargetOperandClass(Actual, Formal);
        if (TargetDiag == Match_Success) {
          DEBUG_WITH_TYPE("asm-matcher",
                          dbgs() << "match success using target matcher\n");
          ++ActualIdx;
          continue;
        }
        // If the target matcher returned a specific error code use
        // that, else use the one from the generic matcher.
        if (TargetDiag != Match_InvalidOperand && HasRequiredFeatures)
          Diag = TargetDiag;
      }
      // If current formal operand wasn't matched and it is optional
      // then try to match next formal operand
      if (Diag == Match_InvalidOperand && isSubclass(Formal, OptionalMatchClass)) {
        DEBUG_WITH_TYPE("asm-matcher", dbgs() << "ignoring optional operand\n");
        continue;
      }
      // If this operand is broken for all of the instances of this
      // mnemonic, keep track of it so we can report loc info.
      // If we already had a match that only failed due to a
      // target predicate, that diagnostic is preferred.
      if (!HadMatchOtherThanPredicate &&
          (it == MnemonicRange.first || ErrorInfo <= ActualIdx)) {
        if (HasRequiredFeatures && (ErrorInfo != ActualIdx || Diag != Match_InvalidOperand))
          RetCode = Diag;
        ErrorInfo = ActualIdx;
      }
      // Otherwise, just reject this instance of the mnemonic.
      OperandsValid = false;
      break;
    }

    if (!OperandsValid) {
      DEBUG_WITH_TYPE("asm-matcher", dbgs() << "Opcode result: multiple "
                                               "operand mismatches, ignoring "
                                               "this opcode\n");
      continue;
    }
    if (!HasRequiredFeatures) {
      HadMatchOtherThanFeatures = true;
      FeatureBitset NewMissingFeatures = RequiredFeatures & ~AvailableFeatures;
      DEBUG_WITH_TYPE("asm-matcher", dbgs() << "Missing target features:";
                      for (unsigned I = 0, E = NewMissingFeatures.size(); I != E; ++I)
                        if (NewMissingFeatures[I])
                          dbgs() << ' ' << I;
                      dbgs() << "\n");
      if (NewMissingFeatures.count() <=
          MissingFeatures.count())
        MissingFeatures = NewMissingFeatures;
      continue;
    }

    Inst.clear();

    Inst.setOpcode(it->Opcode);
    // We have a potential match but have not rendered the operands.
    // Check the target predicate to handle any context sensitive
    // constraints.
    // For example, Ties that are referenced multiple times must be
    // checked here to ensure the input is the same for each match
    // constraints. If we leave it any later the ties will have been
    // canonicalized
    unsigned MatchResult;
    if ((MatchResult = checkEarlyTargetMatchPredicate(Inst, Operands)) != Match_Success) {
      Inst.clear();
      DEBUG_WITH_TYPE(
          "asm-matcher",
          dbgs() << "Early target match predicate failed with diag code "
                 << MatchResult << "\n");
      RetCode = MatchResult;
      HadMatchOtherThanPredicate = true;
      continue;
    }

    if (matchingInlineAsm) {
      convertToMapAndConstraints(it->ConvertFn, Operands);
      if (!checkAsmTiedOperandConstraints(*this, it->ConvertFn, Operands, ErrorInfo))
        return Match_InvalidTiedOperand;

      return Match_Success;
    }

    // We have selected a definite instruction, convert the parsed
    // operands into the appropriate MCInst.
    convertToMCInst(it->ConvertFn, Inst, it->Opcode, Operands);

    // We have a potential match. Check the target predicate to
    // handle any context sensitive constraints.
    if ((MatchResult = checkTargetMatchPredicate(Inst)) != Match_Success) {
      DEBUG_WITH_TYPE("asm-matcher",
                      dbgs() << "Target match predicate failed with diag code "
                             << MatchResult << "\n");
      Inst.clear();
      RetCode = MatchResult;
      HadMatchOtherThanPredicate = true;
      continue;
    }

    if (!checkAsmTiedOperandConstraints(*this, it->ConvertFn, Operands, ErrorInfo))
      return Match_InvalidTiedOperand;

    DEBUG_WITH_TYPE(
        "asm-matcher",
        dbgs() << "Opcode result: complete match, selecting this opcode\n");
    return Match_Success;
  }

  // Okay, we had no match.  Try to return a useful error code.
  if (HadMatchOtherThanPredicate || !HadMatchOtherThanFeatures)
    return RetCode;

  ErrorInfo = 0;
  return Match_MissingFeature;
}

#endif // GET_MATCHER_IMPLEMENTATION


#ifdef GET_MNEMONIC_SPELL_CHECKER
#undef GET_MNEMONIC_SPELL_CHECKER

static std::string WebAssemblyMnemonicSpellCheck(StringRef S, const FeatureBitset &FBS, unsigned VariantID) {
  const unsigned MaxEditDist = 2;
  std::vector<StringRef> Candidates;
  StringRef Prev = "";

  // Find the appropriate table for this asm variant.
  const MatchEntry *Start, *End;
  switch (VariantID) {
  default: llvm_unreachable("invalid variant!");
  case 0: Start = std::begin(MatchTable0); End = std::end(MatchTable0); break;
  }

  for (auto I = Start; I < End; I++) {
    // Ignore unsupported instructions.
    const FeatureBitset &RequiredFeatures = FeatureBitsets[I->RequiredFeaturesIdx];
    if ((FBS & RequiredFeatures) != RequiredFeatures)
      continue;

    StringRef T = I->getMnemonic();
    // Avoid recomputing the edit distance for the same string.
    if (T.equals(Prev))
      continue;

    Prev = T;
    unsigned Dist = S.edit_distance(T, false, MaxEditDist);
    if (Dist <= MaxEditDist)
      Candidates.push_back(T);
  }

  if (Candidates.empty())
    return "";

  std::string Res = ", did you mean: ";
  unsigned i = 0;
  for (; i < Candidates.size() - 1; i++)
    Res += Candidates[i].str() + ", ";
  return Res + Candidates[i].str() + "?";
}

#endif // GET_MNEMONIC_SPELL_CHECKER


#ifdef GET_MNEMONIC_CHECKER
#undef GET_MNEMONIC_CHECKER

static bool WebAssemblyCheckMnemonic(StringRef Mnemonic,
                                const FeatureBitset &AvailableFeatures,
                                unsigned VariantID) {
  // Find the appropriate table for this asm variant.
  const MatchEntry *Start, *End;
  switch (VariantID) {
  default: llvm_unreachable("invalid variant!");
  case 0: Start = std::begin(MatchTable0); End = std::end(MatchTable0); break;
  }

  // Search the table.
  auto MnemonicRange = std::equal_range(Start, End, Mnemonic, LessOpcode());

  if (MnemonicRange.first == MnemonicRange.second)
    return false;

  for (const MatchEntry *it = MnemonicRange.first, *ie = MnemonicRange.second;
       it != ie; ++it) {
    const FeatureBitset &RequiredFeatures =
      FeatureBitsets[it->RequiredFeaturesIdx];
    if ((AvailableFeatures & RequiredFeatures) == RequiredFeatures)
      return true;
  }
  return false;
}

#endif // GET_MNEMONIC_CHECKER

