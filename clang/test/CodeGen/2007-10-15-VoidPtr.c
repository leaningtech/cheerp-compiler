// RUN: %clang_cc1 -triple %itanium_abi_triple -emit-llvm %s -o /dev/null
void bork(void **data) {
  (*(unsigned short *) (&(data[37])[927]) = 0);
}
