// Test haystack overflow in strcasestr function
// RUN: %clang_asan %s -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clang_asan -cheerp-linear-output=asmjs %s -o %t && not %run %t 2>&1 | FileCheck %s
// Test intercept_strstr asan option
// CHEERPASAN: TODO Disable other interceptors because strlen may be called inside strcasestr

// There's no interceptor for strcasestr on Windows
// XFAIL: windows-msvc

#define _GNU_SOURCE
#include <assert.h>
#include <string.h>
#include <sanitizer/asan_interface.h>

int main(int argc, char **argv) {
  char *r = 0;
  char s2[] = "c";
  char s1[4] = "abC";
  __asan_poison_memory_region ((char *)&s1[2], 2);
  r = strcasestr(s1, s2);
  // CHECK:'s1'{{.*}} <== Memory access at offset {{[0-9]+}} partially overflows this variable
  assert(r == s1 + 2);
  return 0;
}
