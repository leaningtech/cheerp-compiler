//===- CheerpRegisterInfo.td - Describe the Cheerp Registers -----*- tablegen -*-===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2022 Leaning Technologies
//
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file describes the Cheerp register classes
///
//===----------------------------------------------------------------------===//

class CheerpBackendReg<string n> : Register<n> {
  let Namespace = "CheerpBackend";
}

class CheerpBackendRegClass<list<ValueType> regTypes, int alignment, dag regList>
    : RegisterClass<"CheerpBackend", regTypes, alignment, regList>;

//===----------------------------------------------------------------------===//
// Registers
//===----------------------------------------------------------------------===//

// Special registers used as the frame and stack pointer.
//
// WebAssembly may someday supports mixed 32-bit and 64-bit heaps in the same
// application, which requires separate width FP and SP.
def FP32 : CheerpBackendReg<"%FP32">;
def FP64 : CheerpBackendReg<"%FP64">;
def SP32 : CheerpBackendReg<"%SP32">;
def SP64 : CheerpBackendReg<"%SP64">;

// The register allocation framework requires register classes have at least
// one register, so we define a few for the integer / floating point register
// classes since we otherwise don't need a physical register in those classes.
// These are also used a "types" in the generated assembly matcher.
def I32_0 : CheerpBackendReg<"%i32.0">;
def I64_0 : CheerpBackendReg<"%i64.0">;
def F32_0 : CheerpBackendReg<"%f32.0">;
def F64_0 : CheerpBackendReg<"%f64.0">;

def V128_0: CheerpBackendReg<"%v128">;

def FUNCREF_0 : CheerpBackendReg<"%funcref.0">;
def EXTERNREF_0 : CheerpBackendReg<"%externref.0">;

// The value stack "register". This is an opaque entity which serves to order
// uses and defs that must remain in LIFO order.
def VALUE_STACK : CheerpBackendReg<"STACK">;

// The incoming arguments "register". This is an opaque entity which serves to
// order the ARGUMENT instructions that are emulating live-in registers and
// must not be scheduled below other instructions.
def ARGUMENTS : CheerpBackendReg<"ARGUMENTS">;

//===----------------------------------------------------------------------===//
//  Register classes
//===----------------------------------------------------------------------===//

def I32 : CheerpBackendRegClass<[i32], 32, (add FP32, SP32, I32_0)>;
def I64 : CheerpBackendRegClass<[i64], 64, (add FP64, SP64, I64_0)>;
def F32 : CheerpBackendRegClass<[f32], 32, (add F32_0)>;
def F64 : CheerpBackendRegClass<[f64], 64, (add F64_0)>;
def V128 : CheerpBackendRegClass<[v4f32, v2f64, v2i64, v4i32, v16i8, v8i16], 128,
                               (add V128_0)>;
def FUNCREF : CheerpBackendRegClass<[funcref], 0, (add FUNCREF_0)>;
def EXTERNREF : CheerpBackendRegClass<[externref], 0, (add EXTERNREF_0)>;

