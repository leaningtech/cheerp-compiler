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

namespace cheerp
{
namespace BuiltinInstr
{

enum BUILTIN { ABS_F64, ACOS_F64, ASIN_F64, ATAN_F64, ATAN2_F64, CEIL_F64, COS_F64, EXP_F64, FLOOR_F64, LOG_F64, POW_F64, SIN_F64, SQRT_F64, TAN_F64,
				CLZ32, GROW_MEM, MAX_BUILTIN };

};	//close BuiltinsInstr
};	//close cheerp

#endif
