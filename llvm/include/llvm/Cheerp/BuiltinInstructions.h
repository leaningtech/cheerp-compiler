//===-- Cheerp/BuiltinInstructions.h - Cheerp utility code ---------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2020 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_BUILTIN_INSTRUCTIONS_H
#define _CHEERP_BUILTIN_INSTRUCTIONS_H

#include "llvm/IR/Function.h"
#include "llvm/IR/Intrinsics.h"

namespace cheerp
{
namespace BuiltinInstr
{

enum BUILTIN {NONE, ABS_F, ACOS_F, ASIN_F, ATAN_F, ATAN2_F, CEIL_F, COS_F, EXP_F, FLOOR_F, LOG_F, POW_F, SIN_F, SQRT_F, TAN_F,
				MOD_F, CLZ, GROW_MEM, MAX_BUILTIN};
};	//close BuiltinInstr

namespace TypedBuiltinInstr
{
enum TYPED_BUILTIN { NONE, ABS_F32, ABS_F64, ACOS_F32, ACOS_F64, ASIN_F32, ASIN_F64, ATAN_F32, ATAN_F64, ATAN2_F32, ATAN2_F64,
				CEIL_F32, CEIL_F64, COS_F32, COS_F64, EXP_F32, EXP_F64, FLOOR_F32, FLOOR_F64, LOG_F32, LOG_F64,
				POW_F32, POW_F64, SIN_F32, SIN_F64, SQRT_F32, SQRT_F64, TAN_F32, TAN_F64, MOD_F32, MOD_F64,
				TRUNC_F32, TRUNC_F64, ROUND_F32, ROUND_F64, MIN_F32, MIN_F64, MAX_F32, MAX_F64, COPYSIGN_F32, COPYSIGN_F64,
				CLZ_32, GROW_MEM, MAX_BUILTIN};
};	//close TypedBuiltinInstr

namespace BuiltinInstr
{

inline constexpr uint32_t numGenericBuiltins()
{
	return MAX_BUILTIN;
}

inline bool isValidJSMathBuiltin(const BUILTIN& b)
{
	return b > NONE && b <= TAN_F;
}

inline bool isFloatMathRenderedInJS(const BUILTIN& b)
{
	return b > NONE && b <= MOD_F;
}

inline BUILTIN getMathBuiltin(const llvm::Function& F)
{
#define IS_MATH_FUNC(x, d, f) { if (F.getName() == d || F.getName() == f) return x;}
	IS_MATH_FUNC(ABS_F, "fabs", "fabsf");
	IS_MATH_FUNC(ACOS_F, "acos", "acosf");
	IS_MATH_FUNC(ASIN_F, "asin", "asinf");
	IS_MATH_FUNC(ATAN_F, "atan", "atanf");
	IS_MATH_FUNC(ATAN2_F, "atan2", "atan2f");
	IS_MATH_FUNC(CEIL_F, "ceil", "ceilf");
	IS_MATH_FUNC(COS_F, "cos", "cosf");
	IS_MATH_FUNC(EXP_F, "exp", "expf");
	IS_MATH_FUNC(FLOOR_F, "floor", "floorf");
	IS_MATH_FUNC(LOG_F, "log", "logf");
	IS_MATH_FUNC(POW_F, "pow", "powf");
	IS_MATH_FUNC(SIN_F, "sin", "sinf");
	IS_MATH_FUNC(SQRT_F, "sqrt", "sqrtf");
	IS_MATH_FUNC(TAN_F, "tan", "tanf");
	IS_MATH_FUNC(MOD_F, "fmod", "fmodf");
#undef IS_MATH_FUNC

	switch (F.getIntrinsicID())
	{
	case llvm::Intrinsic::fabs:
		return ABS_F;
	case llvm::Intrinsic::ceil:
		return CEIL_F;
	case llvm::Intrinsic::cos:
		return COS_F;
	case llvm::Intrinsic::exp:
		return EXP_F;
	case llvm::Intrinsic::floor:
		return FLOOR_F;
	case llvm::Intrinsic::log:
		return LOG_F;
	case llvm::Intrinsic::pow:
		return POW_F;
	case llvm::Intrinsic::sin:
		return SIN_F;
	case llvm::Intrinsic::sqrt:
		return SQRT_F;
	case llvm::Intrinsic::ctlz:
		return CLZ;
	default:
		break;
	}

	return NONE;
}

};	//close BuiltinsInstr

namespace TypedBuiltinInstr
{
#define WASM_INTRINSIC_LIST_BUILTIN(x) \
	x("f32.abs", 0x8b, "fabsf", ABS_F32) \
	x("f32.ceil", 0x8d, "ceilf", CEIL_F32) \
	x("f32.floor", 0x8e, "floorf", FLOOR_F32) \
	x("f32.trunc", 0x8f, "truncf", TRUNC_F32) \
	x("f32.nearest", 0x90, "roundf", ROUND_F32) \
	x("f32.sqrt", 0x91, "sqrtf", SQRT_F32) \
	x("f32.min", 0x96, "fminf", MIN_F32) \
	x("f32.max", 0x97, "fmaxf", MAX_F32) \
	x("f32.copysign", 0x98, "copysignf", COPYSIGN_F32) \
	\
	x("f64.abs", 0x99, "fabs", ABS_F64) \
	x("f64.ceil", 0x9b, "ceil", CEIL_F64) \
	x("f64.floor", 0x9c, "floor", FLOOR_F64) \
	x("f64.trunc", 0x9d, "trunc", TRUNC_F64) \
	x("f64.nearest", 0x9e, "round", ROUND_F64) \
	x("f64.sqrt", 0x9f, "sqrt", SQRT_F64) \
	x("f64.min", 0xa4, "fmin", MIN_F64) \
	x("f64.max", 0xa5, "fmax", MAX_F64) \
	x("f64.copysign", 0xa6, "copysign", COPYSIGN_F64) \
	\
	x("i32.clz", 0x67, "", CLZ_32) \

inline bool isValidWasmMathBuiltin(const TYPED_BUILTIN& b)
{
	switch (b)
	{
#define WASM_INTRINSIC(name, opcode, Fname, builtin) \
	case builtin: \
		return true;
WASM_INTRINSIC_LIST_BUILTIN(WASM_INTRINSIC)
#undef WASM_INTRINSIC
	default:
		break;
	}
	return false;
}

inline uint32_t opcodeWasmBuiltin(const TYPED_BUILTIN& b)
{
	switch (b)
	{
#define WASM_INTRINSIC(name, opcode, Fname, builtin) \
	case builtin: \
		return opcode;
WASM_INTRINSIC_LIST_BUILTIN(WASM_INTRINSIC)
#undef WASM_INTRINSIC
	default:
		break;
	}
	llvm_unreachable("A builtin should have been found");
	return 0;
}

inline std::string nameWasmBuiltin(const TYPED_BUILTIN& b)
{
	switch (b)
	{
#define WASM_INTRINSIC(name, opcode, Fname, builtin) \
	case builtin: \
		return name;
WASM_INTRINSIC_LIST_BUILTIN(WASM_INTRINSIC)
#undef WASM_INTRINSIC
	default:
		break;
	}
	llvm_unreachable("A builtin should have been found");
	return "";
}

inline TYPED_BUILTIN getMathTypedBuiltin(const llvm::Function& F)
{
#define WASM_INTRINSIC(name, opcode, Fname, builtin) \
	if (F.getName() == Fname) \
		return builtin;
WASM_INTRINSIC_LIST_BUILTIN(WASM_INTRINSIC)
#undef WASM_INTRINSIC

	if (F.getName() == "__ieee754_sqrt")
		return SQRT_F64;

	return NONE;
}

};	//close TypedBuiltinInstr

};	//close cheerp

#endif
