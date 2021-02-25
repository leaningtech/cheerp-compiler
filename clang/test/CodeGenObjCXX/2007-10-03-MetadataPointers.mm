// RUN: %clang_cc1 -triple %itanium_abi_triple -emit-llvm %s -o /dev/null

@class NSImage;
void bork() {
  NSImage *nsimage;
  [nsimage release];
}
