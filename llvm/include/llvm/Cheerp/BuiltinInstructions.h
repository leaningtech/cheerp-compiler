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

enum BUILTIN {NONE, ABS_F, ACOS_F, ASIN_F, ATAN_F, ATAN2_F, CEIL_F, COS_F, EXP_F, FLOOR_F, LOG_F, POW_F, SIN_F, SQRT_F, TAN_F, CLZ,
				MOD_F, GROW_MEM, MAX_BUILTIN};
};	//close BuiltinInstr

namespace TypedBuiltinInstr
{
enum TYPED_BUILTIN { NONE, ABS_F32, ABS_F64, ACOS_F32, ACOS_F64, ASIN_F32, ASIN_F64, ATAN_F32, ATAN_F64, ATAN2_F32, ATAN2_F64,
				CEIL_F32, CEIL_F64, COS_F32, COS_F64, EXP_F32, EXP_F64, FLOOR_F32, FLOOR_F64, LOG_F32, LOG_F64,
				POW_F32, POW_F64, SIN_F32, SIN_F64, SQRT_F32, SQRT_F64, TAN_F32, TAN_F64, MOD_F32, MOD_F64,
				TRUNC_F32, TRUNC_F64, ROUND_F32, ROUND_F64, MIN_F32, MIN_F64, MAX_F32, MAX_F64, COPYSIGN_F32, COPYSIGN_F64,
				CLZ_32, GROW_MEM, MAX_BUILTIN, UNSUPPORTED};
};	//close TypedBuiltinInstr

namespace BuiltinInstr
{

inline constexpr uint32_t numGenericBuiltins()
{
	return MAX_BUILTIN;
}

inline bool isValidJSMathBuiltin(const BUILTIN& b)
{
	return b > NONE && b <= CLZ;
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

#define TYPED_FUNCTIONS_LIST(x) \
	x(ACOS_F32, ACOS_F64, "acos", "acosf") \
	x(ASIN_F32, ASIN_F64, "asin", "asinf") \
	x(ATAN_F32, ATAN_F64, "atan", "atanf") \
	x(ATAN2_F32, ATAN2_F64, "atan2", "atan2f") \
	x(COS_F32, COS_F64, "cos", "cosf") \
	x(EXP_F32, EXP_F64, "exp", "expf") \
	x(LOG_F32, LOG_F64, "log", "logf") \
	x(POW_F32, POW_F64, "pow", "powf") \
	x(SIN_F32, SIN_F64, "sin", "sinf") \
	x(TAN_F32, TAN_F64, "tan", "tanf") \

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

inline uint32_t numExtraParameters(const llvm::Function* F)
{
	if (F)
	{
		switch (F->getIntrinsicID())
		{
		case llvm::Intrinsic::ctlz:
		case llvm::Intrinsic::cttz:
			return 1;
		default:
			break;
		};
	}
	return 0;
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

inline const char* nameWasmBuiltin(const TYPED_BUILTIN& b)
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

inline std::string functionName(const TYPED_BUILTIN& b)
{
	switch (b)
	{
#define WASM_INTRINSIC(name, opcode, Fname, builtin) \
	case TYPED_BUILTIN::builtin: \
		return Fname;
WASM_INTRINSIC_LIST_BUILTIN(WASM_INTRINSIC)
#undef WASM_INTRINSIC
	default:
		break;
	}
	switch (b)
	{
#define TYPED_FUNCTIONS(opcode32, opcode64, Fname32, Fname64) \
	case TYPED_BUILTIN::opcode32: \
		return Fname32;	\
	case TYPED_BUILTIN::opcode64: \
		return Fname64;
TYPED_FUNCTIONS_LIST(TYPED_FUNCTIONS)
#undef TYPED_FUNCTIONS
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

	const bool floatType = F.getReturnType()->isFloatTy();

	if (F.getName() == "__ieee754_sqrt")
	{
		assert(!floatType);
		return SQRT_F64;
	}

	switch (F.getIntrinsicID())
	{
	case llvm::Intrinsic::fabs:
		return floatType ? ABS_F32 : ABS_F64;
	case llvm::Intrinsic::ceil:
		return floatType ? CEIL_F32 : CEIL_F64;
	case llvm::Intrinsic::floor:
		return floatType ? FLOOR_F32 : FLOOR_F64;
	case llvm::Intrinsic::sqrt:
		return floatType ? SQRT_F32 : SQRT_F64;
	case llvm::Intrinsic::trunc:
		return floatType ? TRUNC_F32 : TRUNC_F64;
	case llvm::Intrinsic::round:
		return floatType ? ROUND_F32 : ROUND_F64;
	case llvm::Intrinsic::minnum:
//	case llvm::Intrinsic::minimum:	<-- to be introduced later
		return floatType ? MIN_F32 : MIN_F64;
	case llvm::Intrinsic::maxnum:
//	case llvm::Intrinsic::maximum:	<-- to be introduced later
		return floatType ? MAX_F32 : MAX_F64;
	case llvm::Intrinsic::copysign:
		return floatType ? COPYSIGN_F32 : COPYSIGN_F64;
	case llvm::Intrinsic::cos:
		return floatType ? COS_F32 : COS_F64;
	case llvm::Intrinsic::exp:
		return floatType ? EXP_F32 : EXP_F64;
	case llvm::Intrinsic::log:
		return floatType ? LOG_F32 : LOG_F64;
	case llvm::Intrinsic::pow:
		return floatType ? POW_F32 : POW_F64;
	case llvm::Intrinsic::sin:
		return floatType ? SIN_F32 : SIN_F64;
	case llvm::Intrinsic::ctlz:
		//TODO: add 64 bit
		return CLZ_32;
	case llvm::Intrinsic::powi:
	case llvm::Intrinsic::exp2:
	case llvm::Intrinsic::log10:
	case llvm::Intrinsic::log2:
	case llvm::Intrinsic::fma:
	case llvm::Intrinsic::rint:
	case llvm::Intrinsic::nearbyint:
//	case llvm::Intrinsic::lround:	<-- to be introduced later
//	case llvm::Intrinsic::llround:	<-- to be introduced later
//	case llvm::Intrinsic::lrint:	<-- to be introduced later
//	case llvm::Intrinsic::llrint:	<-- to be introduced later
		return UNSUPPORTED;
	default:
		break;
	}

	return NONE;
}

inline bool isAlwaysExactNatively(const TYPED_BUILTIN& b)
{
	if (b == CLZ_32)
		return true;

	//Other builtins that returns the same bit-to-bit result as the libc implementation could return true

	return false;
}

inline bool isWasmIntrinsic(const llvm::Function* F)
{
	const auto& builtin = getMathTypedBuiltin(*F);
	return isValidWasmMathBuiltin(builtin);
}

};	//close TypedBuiltinInstr

};	//close cheerp

#endif
