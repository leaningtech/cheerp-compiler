//- Cheerp.td - Describe the Cheerp Target Machine --*- tablegen -*-//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2022 Leaning Technologies
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This is a target description file for the Cheerp architecture.
///
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Target-independent interfaces which we are implementing
//===----------------------------------------------------------------------===//

include "llvm/Target/Target.td"

//===----------------------------------------------------------------------===//
// Register File Description
//===----------------------------------------------------------------------===//

include "CheerpRegisterInfo.td"


//===----------------------------------------------------------------------===//
// Target Declaration
//===----------------------------------------------------------------------===//

def CheerpBackend : Target {

}
