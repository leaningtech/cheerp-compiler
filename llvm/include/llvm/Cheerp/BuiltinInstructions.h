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
};	//close cheerp

#endif
