/*===- TableGen'erated file -------------------------------------*- C++ -*-===*\
|*                                                                            *|
|* "Fast" Instruction Selector for the WebAssembly target                     *|
|*                                                                            *|
|* Automatically generated file, do not edit!                                 *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*/


// FastEmit Immediate Predicate functions.
static bool Predicate_ImmI8(int64_t Imm) {
return -(1 << (8 - 1)) <= Imm && Imm < (1 << (8 - 1));
}
static bool Predicate_ImmI16(int64_t Imm) {
return -(1 << (16 - 1)) <= Imm && Imm < (1 << (16 - 1));
}
static bool Predicate_LaneIdx32(int64_t Imm) {
return 0 <= Imm && Imm < 32;
}
static bool Predicate_LaneIdx16(int64_t Imm) {
return 0 <= Imm && Imm < 16;
}
static bool Predicate_LaneIdx8(int64_t Imm) {
return 0 <= Imm && Imm < 8;
}
static bool Predicate_LaneIdx4(int64_t Imm) {
return 0 <= Imm && Imm < 4;
}
static bool Predicate_LaneIdx2(int64_t Imm) {
return 0 <= Imm && Imm < 2;
}


// FastEmit functions for ISD::ABS.

unsigned fastEmit_ISD_ABS_MVT_v16i8_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v16i8)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::ABS_I8x16, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_ABS_MVT_v8i16_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::ABS_I16x8, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_ABS_MVT_v4i32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::ABS_I32x4, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_ABS_MVT_v2i64_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v2i64)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::ABS_I64x2, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_ABS_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::v16i8: return fastEmit_ISD_ABS_MVT_v16i8_r(RetVT, Op0);
  case MVT::v8i16: return fastEmit_ISD_ABS_MVT_v8i16_r(RetVT, Op0);
  case MVT::v4i32: return fastEmit_ISD_ABS_MVT_v4i32_r(RetVT, Op0);
  case MVT::v2i64: return fastEmit_ISD_ABS_MVT_v2i64_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for ISD::ANY_EXTEND.

unsigned fastEmit_ISD_ANY_EXTEND_MVT_i32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::i64)
    return 0;
  return fastEmitInst_r(WebAssembly::I64_EXTEND_U_I32, &WebAssembly::I64RegClass, Op0);
}

unsigned fastEmit_ISD_ANY_EXTEND_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_ANY_EXTEND_MVT_i32_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for ISD::BITCAST.

unsigned fastEmit_ISD_BITCAST_MVT_i32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  return fastEmitInst_r(WebAssembly::F32_REINTERPRET_I32, &WebAssembly::F32RegClass, Op0);
}

unsigned fastEmit_ISD_BITCAST_MVT_i64_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::f64)
    return 0;
  return fastEmitInst_r(WebAssembly::F64_REINTERPRET_I64, &WebAssembly::F64RegClass, Op0);
}

unsigned fastEmit_ISD_BITCAST_MVT_f32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  return fastEmitInst_r(WebAssembly::I32_REINTERPRET_F32, &WebAssembly::I32RegClass, Op0);
}

unsigned fastEmit_ISD_BITCAST_MVT_f64_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::i64)
    return 0;
  return fastEmitInst_r(WebAssembly::I64_REINTERPRET_F64, &WebAssembly::I64RegClass, Op0);
}

unsigned fastEmit_ISD_BITCAST_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_BITCAST_MVT_i32_r(RetVT, Op0);
  case MVT::i64: return fastEmit_ISD_BITCAST_MVT_i64_r(RetVT, Op0);
  case MVT::f32: return fastEmit_ISD_BITCAST_MVT_f32_r(RetVT, Op0);
  case MVT::f64: return fastEmit_ISD_BITCAST_MVT_f64_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for ISD::CTLZ.

unsigned fastEmit_ISD_CTLZ_MVT_i32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  return fastEmitInst_r(WebAssembly::CLZ_I32, &WebAssembly::I32RegClass, Op0);
}

unsigned fastEmit_ISD_CTLZ_MVT_i64_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::i64)
    return 0;
  return fastEmitInst_r(WebAssembly::CLZ_I64, &WebAssembly::I64RegClass, Op0);
}

unsigned fastEmit_ISD_CTLZ_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_CTLZ_MVT_i32_r(RetVT, Op0);
  case MVT::i64: return fastEmit_ISD_CTLZ_MVT_i64_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for ISD::CTPOP.

unsigned fastEmit_ISD_CTPOP_MVT_i32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  return fastEmitInst_r(WebAssembly::POPCNT_I32, &WebAssembly::I32RegClass, Op0);
}

unsigned fastEmit_ISD_CTPOP_MVT_i64_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::i64)
    return 0;
  return fastEmitInst_r(WebAssembly::POPCNT_I64, &WebAssembly::I64RegClass, Op0);
}

unsigned fastEmit_ISD_CTPOP_MVT_v16i8_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v16i8)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::POPCNT_I8x16, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_CTPOP_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_CTPOP_MVT_i32_r(RetVT, Op0);
  case MVT::i64: return fastEmit_ISD_CTPOP_MVT_i64_r(RetVT, Op0);
  case MVT::v16i8: return fastEmit_ISD_CTPOP_MVT_v16i8_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for ISD::CTTZ.

unsigned fastEmit_ISD_CTTZ_MVT_i32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  return fastEmitInst_r(WebAssembly::CTZ_I32, &WebAssembly::I32RegClass, Op0);
}

unsigned fastEmit_ISD_CTTZ_MVT_i64_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::i64)
    return 0;
  return fastEmitInst_r(WebAssembly::CTZ_I64, &WebAssembly::I64RegClass, Op0);
}

unsigned fastEmit_ISD_CTTZ_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_CTTZ_MVT_i32_r(RetVT, Op0);
  case MVT::i64: return fastEmit_ISD_CTTZ_MVT_i64_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for ISD::FABS.

unsigned fastEmit_ISD_FABS_MVT_f32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  return fastEmitInst_r(WebAssembly::ABS_F32, &WebAssembly::F32RegClass, Op0);
}

unsigned fastEmit_ISD_FABS_MVT_f64_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::f64)
    return 0;
  return fastEmitInst_r(WebAssembly::ABS_F64, &WebAssembly::F64RegClass, Op0);
}

unsigned fastEmit_ISD_FABS_MVT_v4f32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v4f32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::ABS_F32x4, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_FABS_MVT_v2f64_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v2f64)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::ABS_F64x2, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_FABS_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::f32: return fastEmit_ISD_FABS_MVT_f32_r(RetVT, Op0);
  case MVT::f64: return fastEmit_ISD_FABS_MVT_f64_r(RetVT, Op0);
  case MVT::v4f32: return fastEmit_ISD_FABS_MVT_v4f32_r(RetVT, Op0);
  case MVT::v2f64: return fastEmit_ISD_FABS_MVT_v2f64_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for ISD::FCEIL.

unsigned fastEmit_ISD_FCEIL_MVT_f32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  return fastEmitInst_r(WebAssembly::CEIL_F32, &WebAssembly::F32RegClass, Op0);
}

unsigned fastEmit_ISD_FCEIL_MVT_f64_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::f64)
    return 0;
  return fastEmitInst_r(WebAssembly::CEIL_F64, &WebAssembly::F64RegClass, Op0);
}

unsigned fastEmit_ISD_FCEIL_MVT_v4f32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v4f32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::CEIL_F32x4, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_FCEIL_MVT_v2f64_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v2f64)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::CEIL_F64x2, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_FCEIL_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::f32: return fastEmit_ISD_FCEIL_MVT_f32_r(RetVT, Op0);
  case MVT::f64: return fastEmit_ISD_FCEIL_MVT_f64_r(RetVT, Op0);
  case MVT::v4f32: return fastEmit_ISD_FCEIL_MVT_v4f32_r(RetVT, Op0);
  case MVT::v2f64: return fastEmit_ISD_FCEIL_MVT_v2f64_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for ISD::FFLOOR.

unsigned fastEmit_ISD_FFLOOR_MVT_f32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  return fastEmitInst_r(WebAssembly::FLOOR_F32, &WebAssembly::F32RegClass, Op0);
}

unsigned fastEmit_ISD_FFLOOR_MVT_f64_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::f64)
    return 0;
  return fastEmitInst_r(WebAssembly::FLOOR_F64, &WebAssembly::F64RegClass, Op0);
}

unsigned fastEmit_ISD_FFLOOR_MVT_v4f32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v4f32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::FLOOR_F32x4, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_FFLOOR_MVT_v2f64_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v2f64)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::FLOOR_F64x2, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_FFLOOR_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::f32: return fastEmit_ISD_FFLOOR_MVT_f32_r(RetVT, Op0);
  case MVT::f64: return fastEmit_ISD_FFLOOR_MVT_f64_r(RetVT, Op0);
  case MVT::v4f32: return fastEmit_ISD_FFLOOR_MVT_v4f32_r(RetVT, Op0);
  case MVT::v2f64: return fastEmit_ISD_FFLOOR_MVT_v2f64_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for ISD::FNEARBYINT.

unsigned fastEmit_ISD_FNEARBYINT_MVT_f32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  return fastEmitInst_r(WebAssembly::NEAREST_F32, &WebAssembly::F32RegClass, Op0);
}

unsigned fastEmit_ISD_FNEARBYINT_MVT_f64_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::f64)
    return 0;
  return fastEmitInst_r(WebAssembly::NEAREST_F64, &WebAssembly::F64RegClass, Op0);
}

unsigned fastEmit_ISD_FNEARBYINT_MVT_v4f32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v4f32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::NEAREST_F32x4, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_FNEARBYINT_MVT_v2f64_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v2f64)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::NEAREST_F64x2, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_FNEARBYINT_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::f32: return fastEmit_ISD_FNEARBYINT_MVT_f32_r(RetVT, Op0);
  case MVT::f64: return fastEmit_ISD_FNEARBYINT_MVT_f64_r(RetVT, Op0);
  case MVT::v4f32: return fastEmit_ISD_FNEARBYINT_MVT_v4f32_r(RetVT, Op0);
  case MVT::v2f64: return fastEmit_ISD_FNEARBYINT_MVT_v2f64_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for ISD::FNEG.

unsigned fastEmit_ISD_FNEG_MVT_f32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  return fastEmitInst_r(WebAssembly::NEG_F32, &WebAssembly::F32RegClass, Op0);
}

unsigned fastEmit_ISD_FNEG_MVT_f64_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::f64)
    return 0;
  return fastEmitInst_r(WebAssembly::NEG_F64, &WebAssembly::F64RegClass, Op0);
}

unsigned fastEmit_ISD_FNEG_MVT_v4f32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v4f32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::NEG_F32x4, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_FNEG_MVT_v2f64_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v2f64)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::NEG_F64x2, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_FNEG_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::f32: return fastEmit_ISD_FNEG_MVT_f32_r(RetVT, Op0);
  case MVT::f64: return fastEmit_ISD_FNEG_MVT_f64_r(RetVT, Op0);
  case MVT::v4f32: return fastEmit_ISD_FNEG_MVT_v4f32_r(RetVT, Op0);
  case MVT::v2f64: return fastEmit_ISD_FNEG_MVT_v2f64_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for ISD::FP_EXTEND.

unsigned fastEmit_ISD_FP_EXTEND_MVT_f32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::f64)
    return 0;
  return fastEmitInst_r(WebAssembly::F64_PROMOTE_F32, &WebAssembly::F64RegClass, Op0);
}

unsigned fastEmit_ISD_FP_EXTEND_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::f32: return fastEmit_ISD_FP_EXTEND_MVT_f32_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for ISD::FP_ROUND.

unsigned fastEmit_ISD_FP_ROUND_MVT_f64_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  return fastEmitInst_r(WebAssembly::F32_DEMOTE_F64, &WebAssembly::F32RegClass, Op0);
}

unsigned fastEmit_ISD_FP_ROUND_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::f64: return fastEmit_ISD_FP_ROUND_MVT_f64_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for ISD::FP_TO_SINT.

unsigned fastEmit_ISD_FP_TO_SINT_MVT_f32_MVT_i32_r(unsigned Op0) {
  if ((!Subtarget->hasNontrappingFPToInt())) {
    return fastEmitInst_r(WebAssembly::FP_TO_SINT_I32_F32, &WebAssembly::I32RegClass, Op0);
  }
  if ((Subtarget->hasNontrappingFPToInt())) {
    return fastEmitInst_r(WebAssembly::I32_TRUNC_S_SAT_F32, &WebAssembly::I32RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_FP_TO_SINT_MVT_f32_MVT_i64_r(unsigned Op0) {
  if ((!Subtarget->hasNontrappingFPToInt())) {
    return fastEmitInst_r(WebAssembly::FP_TO_SINT_I64_F32, &WebAssembly::I64RegClass, Op0);
  }
  if ((Subtarget->hasNontrappingFPToInt())) {
    return fastEmitInst_r(WebAssembly::I64_TRUNC_S_SAT_F32, &WebAssembly::I64RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_FP_TO_SINT_MVT_f32_r(MVT RetVT, unsigned Op0) {
switch (RetVT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_FP_TO_SINT_MVT_f32_MVT_i32_r(Op0);
  case MVT::i64: return fastEmit_ISD_FP_TO_SINT_MVT_f32_MVT_i64_r(Op0);
  default: return 0;
}
}

unsigned fastEmit_ISD_FP_TO_SINT_MVT_f64_MVT_i32_r(unsigned Op0) {
  if ((!Subtarget->hasNontrappingFPToInt())) {
    return fastEmitInst_r(WebAssembly::FP_TO_SINT_I32_F64, &WebAssembly::I32RegClass, Op0);
  }
  if ((Subtarget->hasNontrappingFPToInt())) {
    return fastEmitInst_r(WebAssembly::I32_TRUNC_S_SAT_F64, &WebAssembly::I32RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_FP_TO_SINT_MVT_f64_MVT_i64_r(unsigned Op0) {
  if ((!Subtarget->hasNontrappingFPToInt())) {
    return fastEmitInst_r(WebAssembly::FP_TO_SINT_I64_F64, &WebAssembly::I64RegClass, Op0);
  }
  if ((Subtarget->hasNontrappingFPToInt())) {
    return fastEmitInst_r(WebAssembly::I64_TRUNC_S_SAT_F64, &WebAssembly::I64RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_FP_TO_SINT_MVT_f64_r(MVT RetVT, unsigned Op0) {
switch (RetVT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_FP_TO_SINT_MVT_f64_MVT_i32_r(Op0);
  case MVT::i64: return fastEmit_ISD_FP_TO_SINT_MVT_f64_MVT_i64_r(Op0);
  default: return 0;
}
}

unsigned fastEmit_ISD_FP_TO_SINT_MVT_v4f32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::fp_to_sint_I32x4, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_FP_TO_SINT_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::f32: return fastEmit_ISD_FP_TO_SINT_MVT_f32_r(RetVT, Op0);
  case MVT::f64: return fastEmit_ISD_FP_TO_SINT_MVT_f64_r(RetVT, Op0);
  case MVT::v4f32: return fastEmit_ISD_FP_TO_SINT_MVT_v4f32_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for ISD::FP_TO_UINT.

unsigned fastEmit_ISD_FP_TO_UINT_MVT_f32_MVT_i32_r(unsigned Op0) {
  if ((!Subtarget->hasNontrappingFPToInt())) {
    return fastEmitInst_r(WebAssembly::FP_TO_UINT_I32_F32, &WebAssembly::I32RegClass, Op0);
  }
  if ((Subtarget->hasNontrappingFPToInt())) {
    return fastEmitInst_r(WebAssembly::I32_TRUNC_U_SAT_F32, &WebAssembly::I32RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_FP_TO_UINT_MVT_f32_MVT_i64_r(unsigned Op0) {
  if ((!Subtarget->hasNontrappingFPToInt())) {
    return fastEmitInst_r(WebAssembly::FP_TO_UINT_I64_F32, &WebAssembly::I64RegClass, Op0);
  }
  if ((Subtarget->hasNontrappingFPToInt())) {
    return fastEmitInst_r(WebAssembly::I64_TRUNC_U_SAT_F32, &WebAssembly::I64RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_FP_TO_UINT_MVT_f32_r(MVT RetVT, unsigned Op0) {
switch (RetVT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_FP_TO_UINT_MVT_f32_MVT_i32_r(Op0);
  case MVT::i64: return fastEmit_ISD_FP_TO_UINT_MVT_f32_MVT_i64_r(Op0);
  default: return 0;
}
}

unsigned fastEmit_ISD_FP_TO_UINT_MVT_f64_MVT_i32_r(unsigned Op0) {
  if ((!Subtarget->hasNontrappingFPToInt())) {
    return fastEmitInst_r(WebAssembly::FP_TO_UINT_I32_F64, &WebAssembly::I32RegClass, Op0);
  }
  if ((Subtarget->hasNontrappingFPToInt())) {
    return fastEmitInst_r(WebAssembly::I32_TRUNC_U_SAT_F64, &WebAssembly::I32RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_FP_TO_UINT_MVT_f64_MVT_i64_r(unsigned Op0) {
  if ((!Subtarget->hasNontrappingFPToInt())) {
    return fastEmitInst_r(WebAssembly::FP_TO_UINT_I64_F64, &WebAssembly::I64RegClass, Op0);
  }
  if ((Subtarget->hasNontrappingFPToInt())) {
    return fastEmitInst_r(WebAssembly::I64_TRUNC_U_SAT_F64, &WebAssembly::I64RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_FP_TO_UINT_MVT_f64_r(MVT RetVT, unsigned Op0) {
switch (RetVT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_FP_TO_UINT_MVT_f64_MVT_i32_r(Op0);
  case MVT::i64: return fastEmit_ISD_FP_TO_UINT_MVT_f64_MVT_i64_r(Op0);
  default: return 0;
}
}

unsigned fastEmit_ISD_FP_TO_UINT_MVT_v4f32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::fp_to_uint_I32x4, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_FP_TO_UINT_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::f32: return fastEmit_ISD_FP_TO_UINT_MVT_f32_r(RetVT, Op0);
  case MVT::f64: return fastEmit_ISD_FP_TO_UINT_MVT_f64_r(RetVT, Op0);
  case MVT::v4f32: return fastEmit_ISD_FP_TO_UINT_MVT_v4f32_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for ISD::FRINT.

unsigned fastEmit_ISD_FRINT_MVT_f32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  return fastEmitInst_r(WebAssembly::NEAREST_F32, &WebAssembly::F32RegClass, Op0);
}

unsigned fastEmit_ISD_FRINT_MVT_f64_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::f64)
    return 0;
  return fastEmitInst_r(WebAssembly::NEAREST_F64, &WebAssembly::F64RegClass, Op0);
}

unsigned fastEmit_ISD_FRINT_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::f32: return fastEmit_ISD_FRINT_MVT_f32_r(RetVT, Op0);
  case MVT::f64: return fastEmit_ISD_FRINT_MVT_f64_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for ISD::FSQRT.

unsigned fastEmit_ISD_FSQRT_MVT_f32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  return fastEmitInst_r(WebAssembly::SQRT_F32, &WebAssembly::F32RegClass, Op0);
}

unsigned fastEmit_ISD_FSQRT_MVT_f64_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::f64)
    return 0;
  return fastEmitInst_r(WebAssembly::SQRT_F64, &WebAssembly::F64RegClass, Op0);
}

unsigned fastEmit_ISD_FSQRT_MVT_v4f32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v4f32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::SQRT_F32x4, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_FSQRT_MVT_v2f64_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v2f64)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::SQRT_F64x2, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_FSQRT_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::f32: return fastEmit_ISD_FSQRT_MVT_f32_r(RetVT, Op0);
  case MVT::f64: return fastEmit_ISD_FSQRT_MVT_f64_r(RetVT, Op0);
  case MVT::v4f32: return fastEmit_ISD_FSQRT_MVT_v4f32_r(RetVT, Op0);
  case MVT::v2f64: return fastEmit_ISD_FSQRT_MVT_v2f64_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for ISD::FTRUNC.

unsigned fastEmit_ISD_FTRUNC_MVT_f32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  return fastEmitInst_r(WebAssembly::TRUNC_F32, &WebAssembly::F32RegClass, Op0);
}

unsigned fastEmit_ISD_FTRUNC_MVT_f64_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::f64)
    return 0;
  return fastEmitInst_r(WebAssembly::TRUNC_F64, &WebAssembly::F64RegClass, Op0);
}

unsigned fastEmit_ISD_FTRUNC_MVT_v4f32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v4f32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::TRUNC_F32x4, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_FTRUNC_MVT_v2f64_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v2f64)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::TRUNC_F64x2, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_FTRUNC_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::f32: return fastEmit_ISD_FTRUNC_MVT_f32_r(RetVT, Op0);
  case MVT::f64: return fastEmit_ISD_FTRUNC_MVT_f64_r(RetVT, Op0);
  case MVT::v4f32: return fastEmit_ISD_FTRUNC_MVT_v4f32_r(RetVT, Op0);
  case MVT::v2f64: return fastEmit_ISD_FTRUNC_MVT_v2f64_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for ISD::SCALAR_TO_VECTOR.

unsigned fastEmit_ISD_SCALAR_TO_VECTOR_MVT_i32_MVT_v16i8_r(unsigned Op0) {
  return fastEmitInst_r(WebAssembly::SPLAT_I8x16, &WebAssembly::V128RegClass, Op0);
}

unsigned fastEmit_ISD_SCALAR_TO_VECTOR_MVT_i32_MVT_v8i16_r(unsigned Op0) {
  return fastEmitInst_r(WebAssembly::SPLAT_I16x8, &WebAssembly::V128RegClass, Op0);
}

unsigned fastEmit_ISD_SCALAR_TO_VECTOR_MVT_i32_MVT_v4i32_r(unsigned Op0) {
  return fastEmitInst_r(WebAssembly::SPLAT_I32x4, &WebAssembly::V128RegClass, Op0);
}

unsigned fastEmit_ISD_SCALAR_TO_VECTOR_MVT_i32_r(MVT RetVT, unsigned Op0) {
switch (RetVT.SimpleTy) {
  case MVT::v16i8: return fastEmit_ISD_SCALAR_TO_VECTOR_MVT_i32_MVT_v16i8_r(Op0);
  case MVT::v8i16: return fastEmit_ISD_SCALAR_TO_VECTOR_MVT_i32_MVT_v8i16_r(Op0);
  case MVT::v4i32: return fastEmit_ISD_SCALAR_TO_VECTOR_MVT_i32_MVT_v4i32_r(Op0);
  default: return 0;
}
}

unsigned fastEmit_ISD_SCALAR_TO_VECTOR_MVT_i64_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v2i64)
    return 0;
  return fastEmitInst_r(WebAssembly::SPLAT_I64x2, &WebAssembly::V128RegClass, Op0);
}

unsigned fastEmit_ISD_SCALAR_TO_VECTOR_MVT_f32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v4f32)
    return 0;
  return fastEmitInst_r(WebAssembly::SPLAT_F32x4, &WebAssembly::V128RegClass, Op0);
}

unsigned fastEmit_ISD_SCALAR_TO_VECTOR_MVT_f64_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v2f64)
    return 0;
  return fastEmitInst_r(WebAssembly::SPLAT_F64x2, &WebAssembly::V128RegClass, Op0);
}

unsigned fastEmit_ISD_SCALAR_TO_VECTOR_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_SCALAR_TO_VECTOR_MVT_i32_r(RetVT, Op0);
  case MVT::i64: return fastEmit_ISD_SCALAR_TO_VECTOR_MVT_i64_r(RetVT, Op0);
  case MVT::f32: return fastEmit_ISD_SCALAR_TO_VECTOR_MVT_f32_r(RetVT, Op0);
  case MVT::f64: return fastEmit_ISD_SCALAR_TO_VECTOR_MVT_f64_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for ISD::SIGN_EXTEND.

unsigned fastEmit_ISD_SIGN_EXTEND_MVT_i32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::i64)
    return 0;
  return fastEmitInst_r(WebAssembly::I64_EXTEND_S_I32, &WebAssembly::I64RegClass, Op0);
}

unsigned fastEmit_ISD_SIGN_EXTEND_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_SIGN_EXTEND_MVT_i32_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for ISD::SINT_TO_FP.

unsigned fastEmit_ISD_SINT_TO_FP_MVT_i32_MVT_f32_r(unsigned Op0) {
  return fastEmitInst_r(WebAssembly::F32_CONVERT_S_I32, &WebAssembly::F32RegClass, Op0);
}

unsigned fastEmit_ISD_SINT_TO_FP_MVT_i32_MVT_f64_r(unsigned Op0) {
  return fastEmitInst_r(WebAssembly::F64_CONVERT_S_I32, &WebAssembly::F64RegClass, Op0);
}

unsigned fastEmit_ISD_SINT_TO_FP_MVT_i32_r(MVT RetVT, unsigned Op0) {
switch (RetVT.SimpleTy) {
  case MVT::f32: return fastEmit_ISD_SINT_TO_FP_MVT_i32_MVT_f32_r(Op0);
  case MVT::f64: return fastEmit_ISD_SINT_TO_FP_MVT_i32_MVT_f64_r(Op0);
  default: return 0;
}
}

unsigned fastEmit_ISD_SINT_TO_FP_MVT_i64_MVT_f32_r(unsigned Op0) {
  return fastEmitInst_r(WebAssembly::F32_CONVERT_S_I64, &WebAssembly::F32RegClass, Op0);
}

unsigned fastEmit_ISD_SINT_TO_FP_MVT_i64_MVT_f64_r(unsigned Op0) {
  return fastEmitInst_r(WebAssembly::F64_CONVERT_S_I64, &WebAssembly::F64RegClass, Op0);
}

unsigned fastEmit_ISD_SINT_TO_FP_MVT_i64_r(MVT RetVT, unsigned Op0) {
switch (RetVT.SimpleTy) {
  case MVT::f32: return fastEmit_ISD_SINT_TO_FP_MVT_i64_MVT_f32_r(Op0);
  case MVT::f64: return fastEmit_ISD_SINT_TO_FP_MVT_i64_MVT_f64_r(Op0);
  default: return 0;
}
}

unsigned fastEmit_ISD_SINT_TO_FP_MVT_v4i32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v4f32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::sint_to_fp_F32x4, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_SINT_TO_FP_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_SINT_TO_FP_MVT_i32_r(RetVT, Op0);
  case MVT::i64: return fastEmit_ISD_SINT_TO_FP_MVT_i64_r(RetVT, Op0);
  case MVT::v4i32: return fastEmit_ISD_SINT_TO_FP_MVT_v4i32_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for ISD::TRUNCATE.

unsigned fastEmit_ISD_TRUNCATE_MVT_i64_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  return fastEmitInst_r(WebAssembly::I32_WRAP_I64, &WebAssembly::I32RegClass, Op0);
}

unsigned fastEmit_ISD_TRUNCATE_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::i64: return fastEmit_ISD_TRUNCATE_MVT_i64_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for ISD::UINT_TO_FP.

unsigned fastEmit_ISD_UINT_TO_FP_MVT_i32_MVT_f32_r(unsigned Op0) {
  return fastEmitInst_r(WebAssembly::F32_CONVERT_U_I32, &WebAssembly::F32RegClass, Op0);
}

unsigned fastEmit_ISD_UINT_TO_FP_MVT_i32_MVT_f64_r(unsigned Op0) {
  return fastEmitInst_r(WebAssembly::F64_CONVERT_U_I32, &WebAssembly::F64RegClass, Op0);
}

unsigned fastEmit_ISD_UINT_TO_FP_MVT_i32_r(MVT RetVT, unsigned Op0) {
switch (RetVT.SimpleTy) {
  case MVT::f32: return fastEmit_ISD_UINT_TO_FP_MVT_i32_MVT_f32_r(Op0);
  case MVT::f64: return fastEmit_ISD_UINT_TO_FP_MVT_i32_MVT_f64_r(Op0);
  default: return 0;
}
}

unsigned fastEmit_ISD_UINT_TO_FP_MVT_i64_MVT_f32_r(unsigned Op0) {
  return fastEmitInst_r(WebAssembly::F32_CONVERT_U_I64, &WebAssembly::F32RegClass, Op0);
}

unsigned fastEmit_ISD_UINT_TO_FP_MVT_i64_MVT_f64_r(unsigned Op0) {
  return fastEmitInst_r(WebAssembly::F64_CONVERT_U_I64, &WebAssembly::F64RegClass, Op0);
}

unsigned fastEmit_ISD_UINT_TO_FP_MVT_i64_r(MVT RetVT, unsigned Op0) {
switch (RetVT.SimpleTy) {
  case MVT::f32: return fastEmit_ISD_UINT_TO_FP_MVT_i64_MVT_f32_r(Op0);
  case MVT::f64: return fastEmit_ISD_UINT_TO_FP_MVT_i64_MVT_f64_r(Op0);
  default: return 0;
}
}

unsigned fastEmit_ISD_UINT_TO_FP_MVT_v4i32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v4f32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::uint_to_fp_F32x4, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_ISD_UINT_TO_FP_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_UINT_TO_FP_MVT_i32_r(RetVT, Op0);
  case MVT::i64: return fastEmit_ISD_UINT_TO_FP_MVT_i64_r(RetVT, Op0);
  case MVT::v4i32: return fastEmit_ISD_UINT_TO_FP_MVT_v4i32_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for ISD::ZERO_EXTEND.

unsigned fastEmit_ISD_ZERO_EXTEND_MVT_i32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::i64)
    return 0;
  return fastEmitInst_r(WebAssembly::I64_EXTEND_U_I32, &WebAssembly::I64RegClass, Op0);
}

unsigned fastEmit_ISD_ZERO_EXTEND_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_ZERO_EXTEND_MVT_i32_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for WebAssemblyISD::BR_TABLE.

unsigned fastEmit_WebAssemblyISD_BR_TABLE_MVT_i32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::isVoid)
    return 0;
  return fastEmitInst_r(WebAssembly::BR_TABLE_I32, &WebAssembly::I32RegClass, Op0);
}

unsigned fastEmit_WebAssemblyISD_BR_TABLE_MVT_i64_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::isVoid)
    return 0;
  return fastEmitInst_r(WebAssembly::BR_TABLE_I64, &WebAssembly::I64RegClass, Op0);
}

unsigned fastEmit_WebAssemblyISD_BR_TABLE_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::i32: return fastEmit_WebAssemblyISD_BR_TABLE_MVT_i32_r(RetVT, Op0);
  case MVT::i64: return fastEmit_WebAssemblyISD_BR_TABLE_MVT_i64_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for WebAssemblyISD::CONVERT_LOW_S.

unsigned fastEmit_WebAssemblyISD_CONVERT_LOW_S_MVT_v4i32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v2f64)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::convert_low_s_F64x2, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_WebAssemblyISD_CONVERT_LOW_S_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::v4i32: return fastEmit_WebAssemblyISD_CONVERT_LOW_S_MVT_v4i32_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for WebAssemblyISD::CONVERT_LOW_U.

unsigned fastEmit_WebAssemblyISD_CONVERT_LOW_U_MVT_v4i32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v2f64)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::convert_low_u_F64x2, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_WebAssemblyISD_CONVERT_LOW_U_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::v4i32: return fastEmit_WebAssemblyISD_CONVERT_LOW_U_MVT_v4i32_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for WebAssemblyISD::DEMOTE_ZERO.

unsigned fastEmit_WebAssemblyISD_DEMOTE_ZERO_MVT_v2f64_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v4f32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::demote_zero_F32x4, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_WebAssemblyISD_DEMOTE_ZERO_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::v2f64: return fastEmit_WebAssemblyISD_DEMOTE_ZERO_MVT_v2f64_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for WebAssemblyISD::EXTEND_HIGH_S.

unsigned fastEmit_WebAssemblyISD_EXTEND_HIGH_S_MVT_v16i8_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::extend_high_s_I16x8, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_WebAssemblyISD_EXTEND_HIGH_S_MVT_v8i16_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::extend_high_s_I32x4, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_WebAssemblyISD_EXTEND_HIGH_S_MVT_v4i32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v2i64)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::extend_high_s_I64x2, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_WebAssemblyISD_EXTEND_HIGH_S_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::v16i8: return fastEmit_WebAssemblyISD_EXTEND_HIGH_S_MVT_v16i8_r(RetVT, Op0);
  case MVT::v8i16: return fastEmit_WebAssemblyISD_EXTEND_HIGH_S_MVT_v8i16_r(RetVT, Op0);
  case MVT::v4i32: return fastEmit_WebAssemblyISD_EXTEND_HIGH_S_MVT_v4i32_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for WebAssemblyISD::EXTEND_HIGH_U.

unsigned fastEmit_WebAssemblyISD_EXTEND_HIGH_U_MVT_v16i8_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::extend_high_u_I16x8, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_WebAssemblyISD_EXTEND_HIGH_U_MVT_v8i16_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::extend_high_u_I32x4, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_WebAssemblyISD_EXTEND_HIGH_U_MVT_v4i32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v2i64)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::extend_high_u_I64x2, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_WebAssemblyISD_EXTEND_HIGH_U_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::v16i8: return fastEmit_WebAssemblyISD_EXTEND_HIGH_U_MVT_v16i8_r(RetVT, Op0);
  case MVT::v8i16: return fastEmit_WebAssemblyISD_EXTEND_HIGH_U_MVT_v8i16_r(RetVT, Op0);
  case MVT::v4i32: return fastEmit_WebAssemblyISD_EXTEND_HIGH_U_MVT_v4i32_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for WebAssemblyISD::EXTEND_LOW_S.

unsigned fastEmit_WebAssemblyISD_EXTEND_LOW_S_MVT_v16i8_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::extend_low_s_I16x8, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_WebAssemblyISD_EXTEND_LOW_S_MVT_v8i16_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::extend_low_s_I32x4, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_WebAssemblyISD_EXTEND_LOW_S_MVT_v4i32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v2i64)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::extend_low_s_I64x2, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_WebAssemblyISD_EXTEND_LOW_S_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::v16i8: return fastEmit_WebAssemblyISD_EXTEND_LOW_S_MVT_v16i8_r(RetVT, Op0);
  case MVT::v8i16: return fastEmit_WebAssemblyISD_EXTEND_LOW_S_MVT_v8i16_r(RetVT, Op0);
  case MVT::v4i32: return fastEmit_WebAssemblyISD_EXTEND_LOW_S_MVT_v4i32_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for WebAssemblyISD::EXTEND_LOW_U.

unsigned fastEmit_WebAssemblyISD_EXTEND_LOW_U_MVT_v16i8_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::extend_low_u_I16x8, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_WebAssemblyISD_EXTEND_LOW_U_MVT_v8i16_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::extend_low_u_I32x4, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_WebAssemblyISD_EXTEND_LOW_U_MVT_v4i32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v2i64)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::extend_low_u_I64x2, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_WebAssemblyISD_EXTEND_LOW_U_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::v16i8: return fastEmit_WebAssemblyISD_EXTEND_LOW_U_MVT_v16i8_r(RetVT, Op0);
  case MVT::v8i16: return fastEmit_WebAssemblyISD_EXTEND_LOW_U_MVT_v8i16_r(RetVT, Op0);
  case MVT::v4i32: return fastEmit_WebAssemblyISD_EXTEND_LOW_U_MVT_v4i32_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for WebAssemblyISD::PROMOTE_LOW.

unsigned fastEmit_WebAssemblyISD_PROMOTE_LOW_MVT_v4f32_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v2f64)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::promote_low_F64x2, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_WebAssemblyISD_PROMOTE_LOW_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::v4f32: return fastEmit_WebAssemblyISD_PROMOTE_LOW_MVT_v4f32_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for WebAssemblyISD::TRUNC_SAT_ZERO_S.

unsigned fastEmit_WebAssemblyISD_TRUNC_SAT_ZERO_S_MVT_v2f64_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::trunc_sat_zero_s_I32x4, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_WebAssemblyISD_TRUNC_SAT_ZERO_S_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::v2f64: return fastEmit_WebAssemblyISD_TRUNC_SAT_ZERO_S_MVT_v2f64_r(RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for WebAssemblyISD::TRUNC_SAT_ZERO_U.

unsigned fastEmit_WebAssemblyISD_TRUNC_SAT_ZERO_U_MVT_v2f64_r(MVT RetVT, unsigned Op0) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_r(WebAssembly::trunc_sat_zero_u_I32x4, &WebAssembly::V128RegClass, Op0);
  }
  return 0;
}

unsigned fastEmit_WebAssemblyISD_TRUNC_SAT_ZERO_U_r(MVT VT, MVT RetVT, unsigned Op0) {
  switch (VT.SimpleTy) {
  case MVT::v2f64: return fastEmit_WebAssemblyISD_TRUNC_SAT_ZERO_U_MVT_v2f64_r(RetVT, Op0);
  default: return 0;
  }
}

// Top-level FastEmit function.

unsigned fastEmit_r(MVT VT, MVT RetVT, unsigned Opcode, unsigned Op0) override {
  switch (Opcode) {
  case ISD::ABS: return fastEmit_ISD_ABS_r(VT, RetVT, Op0);
  case ISD::ANY_EXTEND: return fastEmit_ISD_ANY_EXTEND_r(VT, RetVT, Op0);
  case ISD::BITCAST: return fastEmit_ISD_BITCAST_r(VT, RetVT, Op0);
  case ISD::CTLZ: return fastEmit_ISD_CTLZ_r(VT, RetVT, Op0);
  case ISD::CTPOP: return fastEmit_ISD_CTPOP_r(VT, RetVT, Op0);
  case ISD::CTTZ: return fastEmit_ISD_CTTZ_r(VT, RetVT, Op0);
  case ISD::FABS: return fastEmit_ISD_FABS_r(VT, RetVT, Op0);
  case ISD::FCEIL: return fastEmit_ISD_FCEIL_r(VT, RetVT, Op0);
  case ISD::FFLOOR: return fastEmit_ISD_FFLOOR_r(VT, RetVT, Op0);
  case ISD::FNEARBYINT: return fastEmit_ISD_FNEARBYINT_r(VT, RetVT, Op0);
  case ISD::FNEG: return fastEmit_ISD_FNEG_r(VT, RetVT, Op0);
  case ISD::FP_EXTEND: return fastEmit_ISD_FP_EXTEND_r(VT, RetVT, Op0);
  case ISD::FP_ROUND: return fastEmit_ISD_FP_ROUND_r(VT, RetVT, Op0);
  case ISD::FP_TO_SINT: return fastEmit_ISD_FP_TO_SINT_r(VT, RetVT, Op0);
  case ISD::FP_TO_UINT: return fastEmit_ISD_FP_TO_UINT_r(VT, RetVT, Op0);
  case ISD::FRINT: return fastEmit_ISD_FRINT_r(VT, RetVT, Op0);
  case ISD::FSQRT: return fastEmit_ISD_FSQRT_r(VT, RetVT, Op0);
  case ISD::FTRUNC: return fastEmit_ISD_FTRUNC_r(VT, RetVT, Op0);
  case ISD::SCALAR_TO_VECTOR: return fastEmit_ISD_SCALAR_TO_VECTOR_r(VT, RetVT, Op0);
  case ISD::SIGN_EXTEND: return fastEmit_ISD_SIGN_EXTEND_r(VT, RetVT, Op0);
  case ISD::SINT_TO_FP: return fastEmit_ISD_SINT_TO_FP_r(VT, RetVT, Op0);
  case ISD::TRUNCATE: return fastEmit_ISD_TRUNCATE_r(VT, RetVT, Op0);
  case ISD::UINT_TO_FP: return fastEmit_ISD_UINT_TO_FP_r(VT, RetVT, Op0);
  case ISD::ZERO_EXTEND: return fastEmit_ISD_ZERO_EXTEND_r(VT, RetVT, Op0);
  case WebAssemblyISD::BR_TABLE: return fastEmit_WebAssemblyISD_BR_TABLE_r(VT, RetVT, Op0);
  case WebAssemblyISD::CONVERT_LOW_S: return fastEmit_WebAssemblyISD_CONVERT_LOW_S_r(VT, RetVT, Op0);
  case WebAssemblyISD::CONVERT_LOW_U: return fastEmit_WebAssemblyISD_CONVERT_LOW_U_r(VT, RetVT, Op0);
  case WebAssemblyISD::DEMOTE_ZERO: return fastEmit_WebAssemblyISD_DEMOTE_ZERO_r(VT, RetVT, Op0);
  case WebAssemblyISD::EXTEND_HIGH_S: return fastEmit_WebAssemblyISD_EXTEND_HIGH_S_r(VT, RetVT, Op0);
  case WebAssemblyISD::EXTEND_HIGH_U: return fastEmit_WebAssemblyISD_EXTEND_HIGH_U_r(VT, RetVT, Op0);
  case WebAssemblyISD::EXTEND_LOW_S: return fastEmit_WebAssemblyISD_EXTEND_LOW_S_r(VT, RetVT, Op0);
  case WebAssemblyISD::EXTEND_LOW_U: return fastEmit_WebAssemblyISD_EXTEND_LOW_U_r(VT, RetVT, Op0);
  case WebAssemblyISD::PROMOTE_LOW: return fastEmit_WebAssemblyISD_PROMOTE_LOW_r(VT, RetVT, Op0);
  case WebAssemblyISD::TRUNC_SAT_ZERO_S: return fastEmit_WebAssemblyISD_TRUNC_SAT_ZERO_S_r(VT, RetVT, Op0);
  case WebAssemblyISD::TRUNC_SAT_ZERO_U: return fastEmit_WebAssemblyISD_TRUNC_SAT_ZERO_U_r(VT, RetVT, Op0);
  default: return 0;
  }
}

// FastEmit functions for ISD::ADD.

unsigned fastEmit_ISD_ADD_MVT_i32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  return fastEmitInst_rr(WebAssembly::ADD_I32, &WebAssembly::I32RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_ADD_MVT_i64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i64)
    return 0;
  return fastEmitInst_rr(WebAssembly::ADD_I64, &WebAssembly::I64RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_ADD_MVT_v16i8_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v16i8)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::ADD_I8x16, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_ADD_MVT_v8i16_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::ADD_I16x8, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_ADD_MVT_v4i32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::ADD_I32x4, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_ADD_MVT_v2i64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v2i64)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::ADD_I64x2, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_ADD_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_ADD_MVT_i32_rr(RetVT, Op0, Op1);
  case MVT::i64: return fastEmit_ISD_ADD_MVT_i64_rr(RetVT, Op0, Op1);
  case MVT::v16i8: return fastEmit_ISD_ADD_MVT_v16i8_rr(RetVT, Op0, Op1);
  case MVT::v8i16: return fastEmit_ISD_ADD_MVT_v8i16_rr(RetVT, Op0, Op1);
  case MVT::v4i32: return fastEmit_ISD_ADD_MVT_v4i32_rr(RetVT, Op0, Op1);
  case MVT::v2i64: return fastEmit_ISD_ADD_MVT_v2i64_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for ISD::AND.

unsigned fastEmit_ISD_AND_MVT_i32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  return fastEmitInst_rr(WebAssembly::AND_I32, &WebAssembly::I32RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_AND_MVT_i64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i64)
    return 0;
  return fastEmitInst_rr(WebAssembly::AND_I64, &WebAssembly::I64RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_AND_MVT_v16i8_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v16i8)
    return 0;
  return fastEmitInst_rr(WebAssembly::AND, &WebAssembly::V128RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_AND_MVT_v8i16_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  return fastEmitInst_rr(WebAssembly::AND, &WebAssembly::V128RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_AND_MVT_v4i32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  return fastEmitInst_rr(WebAssembly::AND, &WebAssembly::V128RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_AND_MVT_v2i64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v2i64)
    return 0;
  return fastEmitInst_rr(WebAssembly::AND, &WebAssembly::V128RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_AND_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_AND_MVT_i32_rr(RetVT, Op0, Op1);
  case MVT::i64: return fastEmit_ISD_AND_MVT_i64_rr(RetVT, Op0, Op1);
  case MVT::v16i8: return fastEmit_ISD_AND_MVT_v16i8_rr(RetVT, Op0, Op1);
  case MVT::v8i16: return fastEmit_ISD_AND_MVT_v8i16_rr(RetVT, Op0, Op1);
  case MVT::v4i32: return fastEmit_ISD_AND_MVT_v4i32_rr(RetVT, Op0, Op1);
  case MVT::v2i64: return fastEmit_ISD_AND_MVT_v2i64_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for ISD::FADD.

unsigned fastEmit_ISD_FADD_MVT_f32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  return fastEmitInst_rr(WebAssembly::ADD_F32, &WebAssembly::F32RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_FADD_MVT_f64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::f64)
    return 0;
  return fastEmitInst_rr(WebAssembly::ADD_F64, &WebAssembly::F64RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_FADD_MVT_v4f32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v4f32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::ADD_F32x4, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_FADD_MVT_v2f64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v2f64)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::ADD_F64x2, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_FADD_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::f32: return fastEmit_ISD_FADD_MVT_f32_rr(RetVT, Op0, Op1);
  case MVT::f64: return fastEmit_ISD_FADD_MVT_f64_rr(RetVT, Op0, Op1);
  case MVT::v4f32: return fastEmit_ISD_FADD_MVT_v4f32_rr(RetVT, Op0, Op1);
  case MVT::v2f64: return fastEmit_ISD_FADD_MVT_v2f64_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for ISD::FCOPYSIGN.

unsigned fastEmit_ISD_FCOPYSIGN_MVT_f32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  return fastEmitInst_rr(WebAssembly::COPYSIGN_F32, &WebAssembly::F32RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_FCOPYSIGN_MVT_f64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::f64)
    return 0;
  return fastEmitInst_rr(WebAssembly::COPYSIGN_F64, &WebAssembly::F64RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_FCOPYSIGN_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::f32: return fastEmit_ISD_FCOPYSIGN_MVT_f32_rr(RetVT, Op0, Op1);
  case MVT::f64: return fastEmit_ISD_FCOPYSIGN_MVT_f64_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for ISD::FDIV.

unsigned fastEmit_ISD_FDIV_MVT_f32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  return fastEmitInst_rr(WebAssembly::DIV_F32, &WebAssembly::F32RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_FDIV_MVT_f64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::f64)
    return 0;
  return fastEmitInst_rr(WebAssembly::DIV_F64, &WebAssembly::F64RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_FDIV_MVT_v4f32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v4f32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::DIV_F32x4, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_FDIV_MVT_v2f64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v2f64)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::DIV_F64x2, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_FDIV_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::f32: return fastEmit_ISD_FDIV_MVT_f32_rr(RetVT, Op0, Op1);
  case MVT::f64: return fastEmit_ISD_FDIV_MVT_f64_rr(RetVT, Op0, Op1);
  case MVT::v4f32: return fastEmit_ISD_FDIV_MVT_v4f32_rr(RetVT, Op0, Op1);
  case MVT::v2f64: return fastEmit_ISD_FDIV_MVT_v2f64_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for ISD::FMAXIMUM.

unsigned fastEmit_ISD_FMAXIMUM_MVT_f32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  return fastEmitInst_rr(WebAssembly::MAX_F32, &WebAssembly::F32RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_FMAXIMUM_MVT_f64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::f64)
    return 0;
  return fastEmitInst_rr(WebAssembly::MAX_F64, &WebAssembly::F64RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_FMAXIMUM_MVT_v4f32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v4f32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::MAX_F32x4, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_FMAXIMUM_MVT_v2f64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v2f64)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::MAX_F64x2, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_FMAXIMUM_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::f32: return fastEmit_ISD_FMAXIMUM_MVT_f32_rr(RetVT, Op0, Op1);
  case MVT::f64: return fastEmit_ISD_FMAXIMUM_MVT_f64_rr(RetVT, Op0, Op1);
  case MVT::v4f32: return fastEmit_ISD_FMAXIMUM_MVT_v4f32_rr(RetVT, Op0, Op1);
  case MVT::v2f64: return fastEmit_ISD_FMAXIMUM_MVT_v2f64_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for ISD::FMINIMUM.

unsigned fastEmit_ISD_FMINIMUM_MVT_f32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  return fastEmitInst_rr(WebAssembly::MIN_F32, &WebAssembly::F32RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_FMINIMUM_MVT_f64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::f64)
    return 0;
  return fastEmitInst_rr(WebAssembly::MIN_F64, &WebAssembly::F64RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_FMINIMUM_MVT_v4f32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v4f32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::MIN_F32x4, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_FMINIMUM_MVT_v2f64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v2f64)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::MIN_F64x2, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_FMINIMUM_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::f32: return fastEmit_ISD_FMINIMUM_MVT_f32_rr(RetVT, Op0, Op1);
  case MVT::f64: return fastEmit_ISD_FMINIMUM_MVT_f64_rr(RetVT, Op0, Op1);
  case MVT::v4f32: return fastEmit_ISD_FMINIMUM_MVT_v4f32_rr(RetVT, Op0, Op1);
  case MVT::v2f64: return fastEmit_ISD_FMINIMUM_MVT_v2f64_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for ISD::FMUL.

unsigned fastEmit_ISD_FMUL_MVT_f32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  return fastEmitInst_rr(WebAssembly::MUL_F32, &WebAssembly::F32RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_FMUL_MVT_f64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::f64)
    return 0;
  return fastEmitInst_rr(WebAssembly::MUL_F64, &WebAssembly::F64RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_FMUL_MVT_v4f32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v4f32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::MUL_F32x4, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_FMUL_MVT_v2f64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v2f64)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::MUL_F64x2, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_FMUL_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::f32: return fastEmit_ISD_FMUL_MVT_f32_rr(RetVT, Op0, Op1);
  case MVT::f64: return fastEmit_ISD_FMUL_MVT_f64_rr(RetVT, Op0, Op1);
  case MVT::v4f32: return fastEmit_ISD_FMUL_MVT_v4f32_rr(RetVT, Op0, Op1);
  case MVT::v2f64: return fastEmit_ISD_FMUL_MVT_v2f64_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for ISD::FSUB.

unsigned fastEmit_ISD_FSUB_MVT_f32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  return fastEmitInst_rr(WebAssembly::SUB_F32, &WebAssembly::F32RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_FSUB_MVT_f64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::f64)
    return 0;
  return fastEmitInst_rr(WebAssembly::SUB_F64, &WebAssembly::F64RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_FSUB_MVT_v4f32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v4f32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::SUB_F32x4, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_FSUB_MVT_v2f64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v2f64)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::SUB_F64x2, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_FSUB_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::f32: return fastEmit_ISD_FSUB_MVT_f32_rr(RetVT, Op0, Op1);
  case MVT::f64: return fastEmit_ISD_FSUB_MVT_f64_rr(RetVT, Op0, Op1);
  case MVT::v4f32: return fastEmit_ISD_FSUB_MVT_v4f32_rr(RetVT, Op0, Op1);
  case MVT::v2f64: return fastEmit_ISD_FSUB_MVT_v2f64_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for ISD::MUL.

unsigned fastEmit_ISD_MUL_MVT_i32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  return fastEmitInst_rr(WebAssembly::MUL_I32, &WebAssembly::I32RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_MUL_MVT_i64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i64)
    return 0;
  return fastEmitInst_rr(WebAssembly::MUL_I64, &WebAssembly::I64RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_MUL_MVT_v8i16_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::MUL_I16x8, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_MUL_MVT_v4i32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::MUL_I32x4, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_MUL_MVT_v2i64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v2i64)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::MUL_I64x2, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_MUL_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_MUL_MVT_i32_rr(RetVT, Op0, Op1);
  case MVT::i64: return fastEmit_ISD_MUL_MVT_i64_rr(RetVT, Op0, Op1);
  case MVT::v8i16: return fastEmit_ISD_MUL_MVT_v8i16_rr(RetVT, Op0, Op1);
  case MVT::v4i32: return fastEmit_ISD_MUL_MVT_v4i32_rr(RetVT, Op0, Op1);
  case MVT::v2i64: return fastEmit_ISD_MUL_MVT_v2i64_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for ISD::OR.

unsigned fastEmit_ISD_OR_MVT_i32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  return fastEmitInst_rr(WebAssembly::OR_I32, &WebAssembly::I32RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_OR_MVT_i64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i64)
    return 0;
  return fastEmitInst_rr(WebAssembly::OR_I64, &WebAssembly::I64RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_OR_MVT_v16i8_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v16i8)
    return 0;
  return fastEmitInst_rr(WebAssembly::OR, &WebAssembly::V128RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_OR_MVT_v8i16_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  return fastEmitInst_rr(WebAssembly::OR, &WebAssembly::V128RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_OR_MVT_v4i32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  return fastEmitInst_rr(WebAssembly::OR, &WebAssembly::V128RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_OR_MVT_v2i64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v2i64)
    return 0;
  return fastEmitInst_rr(WebAssembly::OR, &WebAssembly::V128RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_OR_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_OR_MVT_i32_rr(RetVT, Op0, Op1);
  case MVT::i64: return fastEmit_ISD_OR_MVT_i64_rr(RetVT, Op0, Op1);
  case MVT::v16i8: return fastEmit_ISD_OR_MVT_v16i8_rr(RetVT, Op0, Op1);
  case MVT::v8i16: return fastEmit_ISD_OR_MVT_v8i16_rr(RetVT, Op0, Op1);
  case MVT::v4i32: return fastEmit_ISD_OR_MVT_v4i32_rr(RetVT, Op0, Op1);
  case MVT::v2i64: return fastEmit_ISD_OR_MVT_v2i64_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for ISD::ROTL.

unsigned fastEmit_ISD_ROTL_MVT_i32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  return fastEmitInst_rr(WebAssembly::ROTL_I32, &WebAssembly::I32RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_ROTL_MVT_i64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i64)
    return 0;
  return fastEmitInst_rr(WebAssembly::ROTL_I64, &WebAssembly::I64RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_ROTL_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_ROTL_MVT_i32_rr(RetVT, Op0, Op1);
  case MVT::i64: return fastEmit_ISD_ROTL_MVT_i64_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for ISD::ROTR.

unsigned fastEmit_ISD_ROTR_MVT_i32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  return fastEmitInst_rr(WebAssembly::ROTR_I32, &WebAssembly::I32RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_ROTR_MVT_i64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i64)
    return 0;
  return fastEmitInst_rr(WebAssembly::ROTR_I64, &WebAssembly::I64RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_ROTR_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_ROTR_MVT_i32_rr(RetVT, Op0, Op1);
  case MVT::i64: return fastEmit_ISD_ROTR_MVT_i64_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for ISD::SADDSAT.

unsigned fastEmit_ISD_SADDSAT_MVT_v16i8_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v16i8)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::ADD_SAT_S_I8x16, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_SADDSAT_MVT_v8i16_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::ADD_SAT_S_I16x8, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_SADDSAT_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::v16i8: return fastEmit_ISD_SADDSAT_MVT_v16i8_rr(RetVT, Op0, Op1);
  case MVT::v8i16: return fastEmit_ISD_SADDSAT_MVT_v8i16_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for ISD::SDIV.

unsigned fastEmit_ISD_SDIV_MVT_i32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  return fastEmitInst_rr(WebAssembly::DIV_S_I32, &WebAssembly::I32RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_SDIV_MVT_i64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i64)
    return 0;
  return fastEmitInst_rr(WebAssembly::DIV_S_I64, &WebAssembly::I64RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_SDIV_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_SDIV_MVT_i32_rr(RetVT, Op0, Op1);
  case MVT::i64: return fastEmit_ISD_SDIV_MVT_i64_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for ISD::SHL.

unsigned fastEmit_ISD_SHL_MVT_i32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  return fastEmitInst_rr(WebAssembly::SHL_I32, &WebAssembly::I32RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_SHL_MVT_i64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i64)
    return 0;
  return fastEmitInst_rr(WebAssembly::SHL_I64, &WebAssembly::I64RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_SHL_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_SHL_MVT_i32_rr(RetVT, Op0, Op1);
  case MVT::i64: return fastEmit_ISD_SHL_MVT_i64_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for ISD::SMAX.

unsigned fastEmit_ISD_SMAX_MVT_v16i8_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v16i8)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::MAX_S_I8x16, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_SMAX_MVT_v8i16_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::MAX_S_I16x8, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_SMAX_MVT_v4i32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::MAX_S_I32x4, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_SMAX_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::v16i8: return fastEmit_ISD_SMAX_MVT_v16i8_rr(RetVT, Op0, Op1);
  case MVT::v8i16: return fastEmit_ISD_SMAX_MVT_v8i16_rr(RetVT, Op0, Op1);
  case MVT::v4i32: return fastEmit_ISD_SMAX_MVT_v4i32_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for ISD::SMIN.

unsigned fastEmit_ISD_SMIN_MVT_v16i8_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v16i8)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::MIN_S_I8x16, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_SMIN_MVT_v8i16_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::MIN_S_I16x8, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_SMIN_MVT_v4i32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::MIN_S_I32x4, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_SMIN_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::v16i8: return fastEmit_ISD_SMIN_MVT_v16i8_rr(RetVT, Op0, Op1);
  case MVT::v8i16: return fastEmit_ISD_SMIN_MVT_v8i16_rr(RetVT, Op0, Op1);
  case MVT::v4i32: return fastEmit_ISD_SMIN_MVT_v4i32_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for ISD::SRA.

unsigned fastEmit_ISD_SRA_MVT_i32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  return fastEmitInst_rr(WebAssembly::SHR_S_I32, &WebAssembly::I32RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_SRA_MVT_i64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i64)
    return 0;
  return fastEmitInst_rr(WebAssembly::SHR_S_I64, &WebAssembly::I64RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_SRA_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_SRA_MVT_i32_rr(RetVT, Op0, Op1);
  case MVT::i64: return fastEmit_ISD_SRA_MVT_i64_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for ISD::SREM.

unsigned fastEmit_ISD_SREM_MVT_i32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  return fastEmitInst_rr(WebAssembly::REM_S_I32, &WebAssembly::I32RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_SREM_MVT_i64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i64)
    return 0;
  return fastEmitInst_rr(WebAssembly::REM_S_I64, &WebAssembly::I64RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_SREM_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_SREM_MVT_i32_rr(RetVT, Op0, Op1);
  case MVT::i64: return fastEmit_ISD_SREM_MVT_i64_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for ISD::SRL.

unsigned fastEmit_ISD_SRL_MVT_i32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  return fastEmitInst_rr(WebAssembly::SHR_U_I32, &WebAssembly::I32RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_SRL_MVT_i64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i64)
    return 0;
  return fastEmitInst_rr(WebAssembly::SHR_U_I64, &WebAssembly::I64RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_SRL_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_SRL_MVT_i32_rr(RetVT, Op0, Op1);
  case MVT::i64: return fastEmit_ISD_SRL_MVT_i64_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for ISD::SUB.

unsigned fastEmit_ISD_SUB_MVT_i32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  return fastEmitInst_rr(WebAssembly::SUB_I32, &WebAssembly::I32RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_SUB_MVT_i64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i64)
    return 0;
  return fastEmitInst_rr(WebAssembly::SUB_I64, &WebAssembly::I64RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_SUB_MVT_v16i8_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v16i8)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::SUB_I8x16, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_SUB_MVT_v8i16_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::SUB_I16x8, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_SUB_MVT_v4i32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::SUB_I32x4, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_SUB_MVT_v2i64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v2i64)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::SUB_I64x2, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_SUB_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_SUB_MVT_i32_rr(RetVT, Op0, Op1);
  case MVT::i64: return fastEmit_ISD_SUB_MVT_i64_rr(RetVT, Op0, Op1);
  case MVT::v16i8: return fastEmit_ISD_SUB_MVT_v16i8_rr(RetVT, Op0, Op1);
  case MVT::v8i16: return fastEmit_ISD_SUB_MVT_v8i16_rr(RetVT, Op0, Op1);
  case MVT::v4i32: return fastEmit_ISD_SUB_MVT_v4i32_rr(RetVT, Op0, Op1);
  case MVT::v2i64: return fastEmit_ISD_SUB_MVT_v2i64_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for ISD::UADDSAT.

unsigned fastEmit_ISD_UADDSAT_MVT_v16i8_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v16i8)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::ADD_SAT_U_I8x16, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_UADDSAT_MVT_v8i16_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::ADD_SAT_U_I16x8, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_UADDSAT_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::v16i8: return fastEmit_ISD_UADDSAT_MVT_v16i8_rr(RetVT, Op0, Op1);
  case MVT::v8i16: return fastEmit_ISD_UADDSAT_MVT_v8i16_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for ISD::UDIV.

unsigned fastEmit_ISD_UDIV_MVT_i32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  return fastEmitInst_rr(WebAssembly::DIV_U_I32, &WebAssembly::I32RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_UDIV_MVT_i64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i64)
    return 0;
  return fastEmitInst_rr(WebAssembly::DIV_U_I64, &WebAssembly::I64RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_UDIV_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_UDIV_MVT_i32_rr(RetVT, Op0, Op1);
  case MVT::i64: return fastEmit_ISD_UDIV_MVT_i64_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for ISD::UMAX.

unsigned fastEmit_ISD_UMAX_MVT_v16i8_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v16i8)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::MAX_U_I8x16, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_UMAX_MVT_v8i16_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::MAX_U_I16x8, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_UMAX_MVT_v4i32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::MAX_U_I32x4, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_UMAX_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::v16i8: return fastEmit_ISD_UMAX_MVT_v16i8_rr(RetVT, Op0, Op1);
  case MVT::v8i16: return fastEmit_ISD_UMAX_MVT_v8i16_rr(RetVT, Op0, Op1);
  case MVT::v4i32: return fastEmit_ISD_UMAX_MVT_v4i32_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for ISD::UMIN.

unsigned fastEmit_ISD_UMIN_MVT_v16i8_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v16i8)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::MIN_U_I8x16, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_UMIN_MVT_v8i16_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::MIN_U_I16x8, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_UMIN_MVT_v4i32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::MIN_U_I32x4, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_ISD_UMIN_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::v16i8: return fastEmit_ISD_UMIN_MVT_v16i8_rr(RetVT, Op0, Op1);
  case MVT::v8i16: return fastEmit_ISD_UMIN_MVT_v8i16_rr(RetVT, Op0, Op1);
  case MVT::v4i32: return fastEmit_ISD_UMIN_MVT_v4i32_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for ISD::UREM.

unsigned fastEmit_ISD_UREM_MVT_i32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  return fastEmitInst_rr(WebAssembly::REM_U_I32, &WebAssembly::I32RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_UREM_MVT_i64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i64)
    return 0;
  return fastEmitInst_rr(WebAssembly::REM_U_I64, &WebAssembly::I64RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_UREM_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_UREM_MVT_i32_rr(RetVT, Op0, Op1);
  case MVT::i64: return fastEmit_ISD_UREM_MVT_i64_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for ISD::XOR.

unsigned fastEmit_ISD_XOR_MVT_i32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  return fastEmitInst_rr(WebAssembly::XOR_I32, &WebAssembly::I32RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_XOR_MVT_i64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::i64)
    return 0;
  return fastEmitInst_rr(WebAssembly::XOR_I64, &WebAssembly::I64RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_XOR_MVT_v16i8_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v16i8)
    return 0;
  return fastEmitInst_rr(WebAssembly::XOR, &WebAssembly::V128RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_XOR_MVT_v8i16_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  return fastEmitInst_rr(WebAssembly::XOR, &WebAssembly::V128RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_XOR_MVT_v4i32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  return fastEmitInst_rr(WebAssembly::XOR, &WebAssembly::V128RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_XOR_MVT_v2i64_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v2i64)
    return 0;
  return fastEmitInst_rr(WebAssembly::XOR, &WebAssembly::V128RegClass, Op0, Op1);
}

unsigned fastEmit_ISD_XOR_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_XOR_MVT_i32_rr(RetVT, Op0, Op1);
  case MVT::i64: return fastEmit_ISD_XOR_MVT_i64_rr(RetVT, Op0, Op1);
  case MVT::v16i8: return fastEmit_ISD_XOR_MVT_v16i8_rr(RetVT, Op0, Op1);
  case MVT::v8i16: return fastEmit_ISD_XOR_MVT_v8i16_rr(RetVT, Op0, Op1);
  case MVT::v4i32: return fastEmit_ISD_XOR_MVT_v4i32_rr(RetVT, Op0, Op1);
  case MVT::v2i64: return fastEmit_ISD_XOR_MVT_v2i64_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for WebAssemblyISD::NARROW_U.

unsigned fastEmit_WebAssemblyISD_NARROW_U_MVT_v8i16_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v16i8)
    return 0;
  return fastEmitInst_rr(WebAssembly::NARROW_U_I8x16, &WebAssembly::V128RegClass, Op0, Op1);
}

unsigned fastEmit_WebAssemblyISD_NARROW_U_MVT_v4i32_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  return fastEmitInst_rr(WebAssembly::NARROW_U_I16x8, &WebAssembly::V128RegClass, Op0, Op1);
}

unsigned fastEmit_WebAssemblyISD_NARROW_U_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::v8i16: return fastEmit_WebAssemblyISD_NARROW_U_MVT_v8i16_rr(RetVT, Op0, Op1);
  case MVT::v4i32: return fastEmit_WebAssemblyISD_NARROW_U_MVT_v4i32_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for WebAssemblyISD::SWIZZLE.

unsigned fastEmit_WebAssemblyISD_SWIZZLE_MVT_v16i8_rr(MVT RetVT, unsigned Op0, unsigned Op1) {
  if (RetVT.SimpleTy != MVT::v16i8)
    return 0;
  if ((Subtarget->hasSIMD128())) {
    return fastEmitInst_rr(WebAssembly::SWIZZLE, &WebAssembly::V128RegClass, Op0, Op1);
  }
  return 0;
}

unsigned fastEmit_WebAssemblyISD_SWIZZLE_rr(MVT VT, MVT RetVT, unsigned Op0, unsigned Op1) {
  switch (VT.SimpleTy) {
  case MVT::v16i8: return fastEmit_WebAssemblyISD_SWIZZLE_MVT_v16i8_rr(RetVT, Op0, Op1);
  default: return 0;
  }
}

// Top-level FastEmit function.

unsigned fastEmit_rr(MVT VT, MVT RetVT, unsigned Opcode, unsigned Op0, unsigned Op1) override {
  switch (Opcode) {
  case ISD::ADD: return fastEmit_ISD_ADD_rr(VT, RetVT, Op0, Op1);
  case ISD::AND: return fastEmit_ISD_AND_rr(VT, RetVT, Op0, Op1);
  case ISD::FADD: return fastEmit_ISD_FADD_rr(VT, RetVT, Op0, Op1);
  case ISD::FCOPYSIGN: return fastEmit_ISD_FCOPYSIGN_rr(VT, RetVT, Op0, Op1);
  case ISD::FDIV: return fastEmit_ISD_FDIV_rr(VT, RetVT, Op0, Op1);
  case ISD::FMAXIMUM: return fastEmit_ISD_FMAXIMUM_rr(VT, RetVT, Op0, Op1);
  case ISD::FMINIMUM: return fastEmit_ISD_FMINIMUM_rr(VT, RetVT, Op0, Op1);
  case ISD::FMUL: return fastEmit_ISD_FMUL_rr(VT, RetVT, Op0, Op1);
  case ISD::FSUB: return fastEmit_ISD_FSUB_rr(VT, RetVT, Op0, Op1);
  case ISD::MUL: return fastEmit_ISD_MUL_rr(VT, RetVT, Op0, Op1);
  case ISD::OR: return fastEmit_ISD_OR_rr(VT, RetVT, Op0, Op1);
  case ISD::ROTL: return fastEmit_ISD_ROTL_rr(VT, RetVT, Op0, Op1);
  case ISD::ROTR: return fastEmit_ISD_ROTR_rr(VT, RetVT, Op0, Op1);
  case ISD::SADDSAT: return fastEmit_ISD_SADDSAT_rr(VT, RetVT, Op0, Op1);
  case ISD::SDIV: return fastEmit_ISD_SDIV_rr(VT, RetVT, Op0, Op1);
  case ISD::SHL: return fastEmit_ISD_SHL_rr(VT, RetVT, Op0, Op1);
  case ISD::SMAX: return fastEmit_ISD_SMAX_rr(VT, RetVT, Op0, Op1);
  case ISD::SMIN: return fastEmit_ISD_SMIN_rr(VT, RetVT, Op0, Op1);
  case ISD::SRA: return fastEmit_ISD_SRA_rr(VT, RetVT, Op0, Op1);
  case ISD::SREM: return fastEmit_ISD_SREM_rr(VT, RetVT, Op0, Op1);
  case ISD::SRL: return fastEmit_ISD_SRL_rr(VT, RetVT, Op0, Op1);
  case ISD::SUB: return fastEmit_ISD_SUB_rr(VT, RetVT, Op0, Op1);
  case ISD::UADDSAT: return fastEmit_ISD_UADDSAT_rr(VT, RetVT, Op0, Op1);
  case ISD::UDIV: return fastEmit_ISD_UDIV_rr(VT, RetVT, Op0, Op1);
  case ISD::UMAX: return fastEmit_ISD_UMAX_rr(VT, RetVT, Op0, Op1);
  case ISD::UMIN: return fastEmit_ISD_UMIN_rr(VT, RetVT, Op0, Op1);
  case ISD::UREM: return fastEmit_ISD_UREM_rr(VT, RetVT, Op0, Op1);
  case ISD::XOR: return fastEmit_ISD_XOR_rr(VT, RetVT, Op0, Op1);
  case WebAssemblyISD::NARROW_U: return fastEmit_WebAssemblyISD_NARROW_U_rr(VT, RetVT, Op0, Op1);
  case WebAssemblyISD::SWIZZLE: return fastEmit_WebAssemblyISD_SWIZZLE_rr(VT, RetVT, Op0, Op1);
  default: return 0;
  }
}

// FastEmit functions for ISD::EXTRACT_VECTOR_ELT.

unsigned fastEmit_ISD_EXTRACT_VECTOR_ELT_MVT_v16i8_ri_Predicate_LaneIdx16(MVT RetVT, unsigned Op0, uint64_t imm1) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  return fastEmitInst_ri(WebAssembly::EXTRACT_LANE_I8x16_u, &WebAssembly::I32RegClass, Op0, imm1);
}

unsigned fastEmit_ISD_EXTRACT_VECTOR_ELT_ri_Predicate_LaneIdx16(MVT VT, MVT RetVT, unsigned Op0, uint64_t imm1) {
  switch (VT.SimpleTy) {
  case MVT::v16i8: return fastEmit_ISD_EXTRACT_VECTOR_ELT_MVT_v16i8_ri_Predicate_LaneIdx16(RetVT, Op0, imm1);
  default: return 0;
  }
}

// Top-level FastEmit function.

unsigned fastEmit_ri_Predicate_LaneIdx16(MVT VT, MVT RetVT, unsigned Opcode, unsigned Op0, uint64_t imm1) {
  switch (Opcode) {
  case ISD::EXTRACT_VECTOR_ELT: return fastEmit_ISD_EXTRACT_VECTOR_ELT_ri_Predicate_LaneIdx16(VT, RetVT, Op0, imm1);
  default: return 0;
  }
}

// FastEmit functions for ISD::EXTRACT_VECTOR_ELT.

unsigned fastEmit_ISD_EXTRACT_VECTOR_ELT_MVT_v8i16_ri_Predicate_LaneIdx8(MVT RetVT, unsigned Op0, uint64_t imm1) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  return fastEmitInst_ri(WebAssembly::EXTRACT_LANE_I16x8_u, &WebAssembly::I32RegClass, Op0, imm1);
}

unsigned fastEmit_ISD_EXTRACT_VECTOR_ELT_ri_Predicate_LaneIdx8(MVT VT, MVT RetVT, unsigned Op0, uint64_t imm1) {
  switch (VT.SimpleTy) {
  case MVT::v8i16: return fastEmit_ISD_EXTRACT_VECTOR_ELT_MVT_v8i16_ri_Predicate_LaneIdx8(RetVT, Op0, imm1);
  default: return 0;
  }
}

// Top-level FastEmit function.

unsigned fastEmit_ri_Predicate_LaneIdx8(MVT VT, MVT RetVT, unsigned Opcode, unsigned Op0, uint64_t imm1) {
  switch (Opcode) {
  case ISD::EXTRACT_VECTOR_ELT: return fastEmit_ISD_EXTRACT_VECTOR_ELT_ri_Predicate_LaneIdx8(VT, RetVT, Op0, imm1);
  default: return 0;
  }
}

// FastEmit functions for ISD::EXTRACT_VECTOR_ELT.

unsigned fastEmit_ISD_EXTRACT_VECTOR_ELT_MVT_v4i32_ri_Predicate_LaneIdx4(MVT RetVT, unsigned Op0, uint64_t imm1) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  return fastEmitInst_ri(WebAssembly::EXTRACT_LANE_I32x4, &WebAssembly::I32RegClass, Op0, imm1);
}

unsigned fastEmit_ISD_EXTRACT_VECTOR_ELT_MVT_v4f32_ri_Predicate_LaneIdx4(MVT RetVT, unsigned Op0, uint64_t imm1) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  return fastEmitInst_ri(WebAssembly::EXTRACT_LANE_F32x4, &WebAssembly::F32RegClass, Op0, imm1);
}

unsigned fastEmit_ISD_EXTRACT_VECTOR_ELT_ri_Predicate_LaneIdx4(MVT VT, MVT RetVT, unsigned Op0, uint64_t imm1) {
  switch (VT.SimpleTy) {
  case MVT::v4i32: return fastEmit_ISD_EXTRACT_VECTOR_ELT_MVT_v4i32_ri_Predicate_LaneIdx4(RetVT, Op0, imm1);
  case MVT::v4f32: return fastEmit_ISD_EXTRACT_VECTOR_ELT_MVT_v4f32_ri_Predicate_LaneIdx4(RetVT, Op0, imm1);
  default: return 0;
  }
}

// Top-level FastEmit function.

unsigned fastEmit_ri_Predicate_LaneIdx4(MVT VT, MVT RetVT, unsigned Opcode, unsigned Op0, uint64_t imm1) {
  switch (Opcode) {
  case ISD::EXTRACT_VECTOR_ELT: return fastEmit_ISD_EXTRACT_VECTOR_ELT_ri_Predicate_LaneIdx4(VT, RetVT, Op0, imm1);
  default: return 0;
  }
}

// FastEmit functions for ISD::EXTRACT_VECTOR_ELT.

unsigned fastEmit_ISD_EXTRACT_VECTOR_ELT_MVT_v2i64_ri_Predicate_LaneIdx2(MVT RetVT, unsigned Op0, uint64_t imm1) {
  if (RetVT.SimpleTy != MVT::i64)
    return 0;
  return fastEmitInst_ri(WebAssembly::EXTRACT_LANE_I64x2, &WebAssembly::I64RegClass, Op0, imm1);
}

unsigned fastEmit_ISD_EXTRACT_VECTOR_ELT_MVT_v2f64_ri_Predicate_LaneIdx2(MVT RetVT, unsigned Op0, uint64_t imm1) {
  if (RetVT.SimpleTy != MVT::f64)
    return 0;
  return fastEmitInst_ri(WebAssembly::EXTRACT_LANE_F64x2, &WebAssembly::F64RegClass, Op0, imm1);
}

unsigned fastEmit_ISD_EXTRACT_VECTOR_ELT_ri_Predicate_LaneIdx2(MVT VT, MVT RetVT, unsigned Op0, uint64_t imm1) {
  switch (VT.SimpleTy) {
  case MVT::v2i64: return fastEmit_ISD_EXTRACT_VECTOR_ELT_MVT_v2i64_ri_Predicate_LaneIdx2(RetVT, Op0, imm1);
  case MVT::v2f64: return fastEmit_ISD_EXTRACT_VECTOR_ELT_MVT_v2f64_ri_Predicate_LaneIdx2(RetVT, Op0, imm1);
  default: return 0;
  }
}

// Top-level FastEmit function.

unsigned fastEmit_ri_Predicate_LaneIdx2(MVT VT, MVT RetVT, unsigned Opcode, unsigned Op0, uint64_t imm1) {
  switch (Opcode) {
  case ISD::EXTRACT_VECTOR_ELT: return fastEmit_ISD_EXTRACT_VECTOR_ELT_ri_Predicate_LaneIdx2(VT, RetVT, Op0, imm1);
  default: return 0;
  }
}

// FastEmit functions for ISD::ConstantFP.

unsigned fastEmit_ISD_ConstantFP_MVT_f32_f(MVT RetVT, const ConstantFP *f0) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  return fastEmitInst_f(WebAssembly::CONST_F32, &WebAssembly::F32RegClass, f0);
}

unsigned fastEmit_ISD_ConstantFP_MVT_f64_f(MVT RetVT, const ConstantFP *f0) {
  if (RetVT.SimpleTy != MVT::f64)
    return 0;
  return fastEmitInst_f(WebAssembly::CONST_F64, &WebAssembly::F64RegClass, f0);
}

unsigned fastEmit_ISD_ConstantFP_f(MVT VT, MVT RetVT, const ConstantFP *f0) {
  switch (VT.SimpleTy) {
  case MVT::f32: return fastEmit_ISD_ConstantFP_MVT_f32_f(RetVT, f0);
  case MVT::f64: return fastEmit_ISD_ConstantFP_MVT_f64_f(RetVT, f0);
  default: return 0;
  }
}

// Top-level FastEmit function.

unsigned fastEmit_f(MVT VT, MVT RetVT, unsigned Opcode, const ConstantFP *f0) override {
  switch (Opcode) {
  case ISD::ConstantFP: return fastEmit_ISD_ConstantFP_f(VT, RetVT, f0);
  default: return 0;
  }
}

// FastEmit functions for ISD::Constant.

unsigned fastEmit_ISD_Constant_MVT_i32_i(MVT RetVT, uint64_t imm0) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  return fastEmitInst_i(WebAssembly::CONST_I32, &WebAssembly::I32RegClass, imm0);
}

unsigned fastEmit_ISD_Constant_MVT_i64_i(MVT RetVT, uint64_t imm0) {
  if (RetVT.SimpleTy != MVT::i64)
    return 0;
  return fastEmitInst_i(WebAssembly::CONST_I64, &WebAssembly::I64RegClass, imm0);
}

unsigned fastEmit_ISD_Constant_i(MVT VT, MVT RetVT, uint64_t imm0) {
  switch (VT.SimpleTy) {
  case MVT::i32: return fastEmit_ISD_Constant_MVT_i32_i(RetVT, imm0);
  case MVT::i64: return fastEmit_ISD_Constant_MVT_i64_i(RetVT, imm0);
  default: return 0;
  }
}

// Top-level FastEmit function.

unsigned fastEmit_i(MVT VT, MVT RetVT, unsigned Opcode, uint64_t imm0) override {
  switch (Opcode) {
  case ISD::Constant: return fastEmit_ISD_Constant_i(VT, RetVT, imm0);
  default: return 0;
  }
}

