// RUN: %clang_cc1 -triple %itanium_abi_triple -emit-llvm -o %t %s

@interface Int1 @end

typedef Int1 Int1Typedef;
@compatibility_alias Int1Alias Int1Typedef;

@implementation Int1Alias @end
