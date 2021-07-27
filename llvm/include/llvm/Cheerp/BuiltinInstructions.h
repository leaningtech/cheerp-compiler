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
#include "llvm/Cheerp/WasmOpcodes.h"

namespace cheerp
{
namespace BuiltinInstr
{

enum BUILTIN {NONE, ABS_F, ACOS_F, ASIN_F, ATAN_F, ATAN2_F, CEIL_F, COS_F, EXP_F, FLOOR_F, LOG_F, POW_F, SIN_F, SQRT_F, TAN_F, CLZ,
				MOD_F, GROW_MEM, MAX_BUILTIN};
}	//close BuiltinInstr

namespace TypedBuiltinInstr
{
enum TYPED_BUILTIN { NONE, ABS_F32, ABS_F64, ACOS_F32, ACOS_F64, ASIN_F32, ASIN_F64, ATAN_F32, ATAN_F64, ATAN2_F32, ATAN2_F64,
				CEIL_F32, CEIL_F64, COS_F32, COS_F64, EXP_F32, EXP_F64, FLOOR_F32, FLOOR_F64, LOG_F32, LOG_F64,
				POW_F32, POW_F64, SIN_F32, SIN_F64, SQRT_F32, SQRT_F64, TAN_F32, TAN_F64, MOD_F32, MOD_F64,
				TRUNC_F32, TRUNC_F64, ROUND_F32, ROUND_F64, MIN_F32, MIN_F64, MAX_F32, MAX_F64, COPYSIGN_F32, COPYSIGN_F64,
				CLZ_32, CLZ_64, CTZ_32, CTZ_64, GROW_MEM, MAX_BUILTIN, UNSUPPORTED};
}	//close TypedBuiltinInstr

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

}	//close BuiltinsInstr

namespace TypedBuiltinInstr
{
#define WASM_INTRINSIC_LIST_BUILTIN(x) \
	x(F32_ABS, "fabsf", ABS_F32) \
	x(F32_CEIL, "ceilf", CEIL_F32) \
	x(F32_FLOOR, "floorf", FLOOR_F32) \
	x(F32_TRUNC, "truncf", TRUNC_F32) \
	x(F32_NEAREST, "roundf", ROUND_F32) \
	x(F32_SQRT, "sqrtf", SQRT_F32) \
	x(F32_MIN, "fminf", MIN_F32) \
	x(F32_MAX, "fmaxf", MAX_F32) \
	x(F32_COPYSIGN, "copysignf", COPYSIGN_F32) \
	\
	x(F64_ABS, "fabs", ABS_F64) \
	x(F64_CEIL, "ceil", CEIL_F64) \
	x(F64_FLOOR, "floor", FLOOR_F64) \
	x(F64_TRUNC, "trunc", TRUNC_F64) \
	x(F64_NEAREST, "round", ROUND_F64) \
	x(F64_SQRT, "sqrt", SQRT_F64) \
	x(F64_MIN, "fmin", MIN_F64) \
	x(F64_MAX, "fmax", MAX_F64) \
	x(F64_COPYSIGN, "copysign", COPYSIGN_F64) \
	\
	x(I32_CLZ, "", CLZ_32) \
	x(I32_CTZ, "", CTZ_32) \
	x(I64_CLZ, "", CLZ_64) \
	x(I64_CTZ, "", CTZ_64) \

#define TYPED_FUNCTIONS_LIST(x) \
	x(ACOS_F32, ACOS_F64, "acosf", "acos") \
	x(ASIN_F32, ASIN_F64, "asinf", "asin") \
	x(ATAN_F32, ATAN_F64, "atanf", "atan") \
	x(ATAN2_F32, ATAN2_F64, "atan2f", "atan2") \
	x(COS_F32, COS_F64, "cosf", "cos") \
	x(EXP_F32, EXP_F64, "expf", "exp") \
	x(LOG_F32, LOG_F64, "logf", "log") \
	x(POW_F32, POW_F64, "powf", "pow") \
	x(SIN_F32, SIN_F64, "sinf", "sin") \
	x(TAN_F32, TAN_F64, "tanf", "tan") \


#define LLVM_BUILTINS_TO_LOWER_LIST(x) \
	x(llvm::Intrinsic::log2, "log2f", "log2") \
	x(llvm::Intrinsic::fma, "fmaf", "fma") \
	x(llvm::Intrinsic::maxnum, "fmaxf", "fmax") \
	x(llvm::Intrinsic::minnum, "fminf", "fmin") \
	x(llvm::Intrinsic::rint, "rintf", "rint") \
	x(llvm::Intrinsic::round, "roundf", "round") \
	x(llvm::Intrinsic::sin, "sinf", "sin") \
	x(llvm::Intrinsic::cos, "cosf", "cos") \

inline bool mayBeLoweredInto(const llvm::Function& F)
{
#define TO_BE_LOWERED_INTO(builtin, FNameFloat, FNameDouble) \
	if (F.getName() == FNameDouble || F.getName() == FNameFloat) \
		return true;
LLVM_BUILTINS_TO_LOWER_LIST(TO_BE_LOWERED_INTO)
#undef TO_BE_LOWERED_INTO
	return false;
}

inline llvm::Function* functionToLowerInto(const llvm::Function& F, llvm::Module& module)
{
	const bool floatType = F.getReturnType()->isFloatTy();
	const uint32_t builtinId = F.getIntrinsicID();

	(void)floatType;

	switch (builtinId)
	{
#define TO_BE_LOWERED_INTO(builtin, FNameFloat, FNameDouble) \
	case builtin: \
		return module.getFunction(floatType ? FNameFloat : FNameDouble);
LLVM_BUILTINS_TO_LOWER_LIST(TO_BE_LOWERED_INTO)
#undef TO_BE_LOWERED_INTO
	default:
		return nullptr;
	}
}

inline bool isValidWasmMathBuiltin(const TYPED_BUILTIN& b)
{
	switch (b)
	{
#define WASM_INTRINSIC(opcode, Fname, builtin) \
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

inline WasmOpcode opcodeWasmBuiltin(const TYPED_BUILTIN& b)
{
	switch (b)
	{
#define WASM_INTRINSIC(opcode, Fname, builtin) \
	case builtin: \
		return WasmOpcode::opcode;
WASM_INTRINSIC_LIST_BUILTIN(WASM_INTRINSIC)
#undef WASM_INTRINSIC
	default:
		break;
	}
	llvm_unreachable("A builtin should have been found");
	return WasmOpcode::UNREACHABLE;
}

inline std::string functionName(const TYPED_BUILTIN& b)
{
	switch (b)
	{
#define WASM_INTRINSIC(opcode, Fname, builtin) \
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
#define WASM_INTRINSIC(opcode, Fname, builtin) \
	if (F.getName() == Fname) \
		return builtin;
WASM_INTRINSIC_LIST_BUILTIN(WASM_INTRINSIC)
#undef WASM_INTRINSIC

	const bool floatType = F.getReturnType()->isFloatTy();
	const bool i64Type = F.getReturnType()->isIntegerTy(64);

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
		return i64Type ? CLZ_64 : CLZ_32;
	case llvm::Intrinsic::cttz:
		return i64Type ? CTZ_64 : CTZ_32;
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
	if (b == CLZ_32 || b == CTZ_32)
		return true;

	//Other builtins that returns the same bit-to-bit result as the libc implementation could return true

	return false;
}

inline bool isWasmIntrinsic(const llvm::Function* F)
{
	const auto& builtin = getMathTypedBuiltin(*F);
	return isValidWasmMathBuiltin(builtin);
}

}	//close TypedBuiltinInstr

}	//close cheerp

#endif
