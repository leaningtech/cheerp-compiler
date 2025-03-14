// RUN: %clang_cc1 -triple riscv32 -emit-llvm %s -o - | FileCheck %s
// RUN: %clang_cc1 -triple riscv32 -emit-llvm -fforce-enable-int128 %s -o - \
// RUN:   | FileCheck %s -check-prefixes=CHECK,CHECK-FORCEINT128
// RUN: %clang_cc1 -triple riscv32 -target-feature +f -target-abi ilp32f -emit-llvm %s -o - \
// RUN:     | FileCheck %s
// RUN: %clang_cc1 -triple riscv32 -target-feature +d -target-feature +f -target-abi ilp32d -emit-llvm %s -o - \
// RUN:     | FileCheck %s

// This file contains test cases that will have the same output for the ilp32,
// ilp32f, and ilp32d ABIs.

#include <stddef.h>
#include <stdint.h>

// CHECK-LABEL: define{{.*}} void @f_void()
void f_void(void) {}

// Scalar arguments and return values smaller than the word size are extended
// according to the sign of their type, up to 32 bits

// CHECK-LABEL: define{{.*}} zeroext i1 @f_scalar_0(i1 noundef zeroext %x)
_Bool f_scalar_0(_Bool x) { return x; }

// CHECK-LABEL: define{{.*}} signext i8 @f_scalar_1(i8 noundef signext %x)
int8_t f_scalar_1(int8_t x) { return x; }

// CHECK-LABEL: define{{.*}} zeroext i8 @f_scalar_2(i8 noundef zeroext %x)
uint8_t f_scalar_2(uint8_t x) { return x; }

// CHECK-LABEL: define{{.*}} i32 @f_scalar_3(i32 noundef %x)
int32_t f_scalar_3(int32_t x) { return x; }

// CHECK-LABEL: define{{.*}} i64 @f_scalar_4(i64 noundef %x)
int64_t f_scalar_4(int64_t x) { return x; }

#ifdef __SIZEOF_INT128__
// CHECK-FORCEINT128-LABEL: define{{.*}} i128 @f_scalar_5(i128 noundef %x)
__int128_t f_scalar_5(__int128_t x) { return x; }
#endif

// CHECK-LABEL: define{{.*}} float @f_fp_scalar_1(float noundef %x)
float f_fp_scalar_1(float x) { return x; }

// CHECK-LABEL: define{{.*}} double @f_fp_scalar_2(double noundef %x)
double f_fp_scalar_2(double x) { return x; }

// Scalars larger than 2*xlen are passed/returned indirect. However, the
// RISC-V LLVM backend can handle this fine, so the function doesn't need to
// be modified.

// CHECK-LABEL: define{{.*}} fp128 @f_fp_scalar_3(fp128 noundef %x)
long double f_fp_scalar_3(long double x) { return x; }

// Empty structs or unions are ignored.

struct empty_s {};

// CHECK-LABEL: define{{.*}} void @f_agg_empty_struct()
struct empty_s f_agg_empty_struct(struct empty_s x) {
  return x;
}

union empty_u {};

// CHECK-LABEL: define{{.*}} void @f_agg_empty_union()
union empty_u f_agg_empty_union(union empty_u x) {
  return x;
}

// Aggregates <= 2*xlen may be passed in registers, so will be coerced to
// integer arguments. The rules for return are the same.

struct tiny {
  uint8_t a, b, c, d;
};

// CHECK-LABEL: define{{.*}} void @f_agg_tiny(i32 %x.coerce)
void f_agg_tiny(struct tiny x) {
  x.a += x.b;
  x.c += x.d;
}

// CHECK-LABEL: define{{.*}} i32 @f_agg_tiny_ret()
struct tiny f_agg_tiny_ret(void) {
  return (struct tiny){1, 2, 3, 4};
}

typedef uint8_t v4i8 __attribute__((vector_size(4)));
typedef int32_t v1i32 __attribute__((vector_size(4)));

// CHECK-LABEL: define{{.*}} void @f_vec_tiny_v4i8(i32 noundef %x.coerce)
void f_vec_tiny_v4i8(v4i8 x) {
  x[0] = x[1];
  x[2] = x[3];
}

// CHECK-LABEL: define{{.*}} i32 @f_vec_tiny_v4i8_ret()
v4i8 f_vec_tiny_v4i8_ret(void) {
  return (v4i8){1, 2, 3, 4};
}

// CHECK-LABEL: define{{.*}} void @f_vec_tiny_v1i32(i32 noundef %x.coerce)
void f_vec_tiny_v1i32(v1i32 x) {
  x[0] = 114;
}

// CHECK-LABEL: define{{.*}} i32 @f_vec_tiny_v1i32_ret()
v1i32 f_vec_tiny_v1i32_ret(void) {
  return (v1i32){1};
}

struct small {
  int32_t a, *b;
};

// CHECK-LABEL: define{{.*}} void @f_agg_small([2 x i32] %x.coerce)
void f_agg_small(struct small x) {
  x.a += *x.b;
  x.b = &x.a;
}

// CHECK-LABEL: define{{.*}} [2 x i32] @f_agg_small_ret()
struct small f_agg_small_ret(void) {
  return (struct small){1, 0};
}

typedef uint8_t v8i8 __attribute__((vector_size(8)));
typedef int64_t v1i64 __attribute__((vector_size(8)));

// CHECK-LABEL: define{{.*}} void @f_vec_small_v8i8(i64 noundef %x.coerce)
void f_vec_small_v8i8(v8i8 x) {
  x[0] = x[7];
}

// CHECK-LABEL: define{{.*}} i64 @f_vec_small_v8i8_ret()
v8i8 f_vec_small_v8i8_ret(void) {
  return (v8i8){1, 2, 3, 4, 5, 6, 7, 8};
}

// CHECK-LABEL: define{{.*}} void @f_vec_small_v1i64(i64 noundef %x.coerce)
void f_vec_small_v1i64(v1i64 x) {
  x[0] = 114;
}

// CHECK-LABEL: define{{.*}} i64 @f_vec_small_v1i64_ret()
v1i64 f_vec_small_v1i64_ret(void) {
  return (v1i64){1};
}

// Aggregates of 2*xlen size and 2*xlen alignment should be coerced to a
// single 2*xlen-sized argument, to ensure that alignment can be maintained if
// passed on the stack.

struct small_aligned {
  int64_t a;
};

// CHECK-LABEL: define{{.*}} void @f_agg_small_aligned(i64 %x.coerce)
void f_agg_small_aligned(struct small_aligned x) {
  x.a += x.a;
}

// CHECK-LABEL: define{{.*}} i64 @f_agg_small_aligned_ret(i64 %x.coerce)
struct small_aligned f_agg_small_aligned_ret(struct small_aligned x) {
  return (struct small_aligned){10};
}

// Aggregates greater > 2*xlen will be passed and returned indirectly
struct large {
  int32_t a, b, c, d;
};

// CHECK-LABEL: define{{.*}} void @f_agg_large(ptr noundef %x)
void f_agg_large(struct large x) {
  x.a = x.b + x.c + x.d;
}

// The address where the struct should be written to will be the first
// argument
// CHECK-LABEL: define{{.*}} void @f_agg_large_ret(ptr noalias sret(%struct.large) align 4 %agg.result, i32 noundef %i, i8 noundef signext %j)
struct large f_agg_large_ret(int32_t i, int8_t j) {
  return (struct large){1, 2, 3, 4};
}

typedef unsigned char v16i8 __attribute__((vector_size(16)));

// CHECK-LABEL: define{{.*}} void @f_vec_large_v16i8(ptr noundef %0)
void f_vec_large_v16i8(v16i8 x) {
  x[0] = x[7];
}

// CHECK-LABEL: define{{.*}} void @f_vec_large_v16i8_ret(ptr noalias sret(<16 x i8>) align 16 %agg.result)
v16i8 f_vec_large_v16i8_ret(void) {
  return (v16i8){1, 2, 3, 4, 5, 6, 7, 8};
}

// Scalars passed on the stack should not have signext/zeroext attributes
// (they are anyext).

// CHECK-LABEL: define{{.*}} i32 @f_scalar_stack_1(i32 %a.coerce, [2 x i32] %b.coerce, i64 %c.coerce, ptr noundef %d, i8 noundef zeroext %e, i8 noundef signext %f, i8 noundef %g, i8 noundef %h)
int f_scalar_stack_1(struct tiny a, struct small b, struct small_aligned c,
                     struct large d, uint8_t e, int8_t f, uint8_t g, int8_t h) {
  return g + h;
}

// Ensure that scalars passed on the stack are still determined correctly in
// the presence of large return values that consume a register due to the need
// to pass a pointer.

// CHECK-LABEL: define{{.*}} void @f_scalar_stack_2(ptr noalias sret(%struct.large) align 4 %agg.result, i32 noundef %a, i64 noundef %b, i64 noundef %c, fp128 noundef %d, i8 noundef zeroext %e, i8 noundef %f, i8 noundef %g)
struct large f_scalar_stack_2(int32_t a, int64_t b, int64_t c, long double d,
                              uint8_t e, int8_t f, uint8_t g) {
  return (struct large){a, e, f, g};
}

// CHECK-LABEL: define{{.*}} fp128 @f_scalar_stack_4(i32 noundef %a, i64 noundef %b, i64 noundef %c, fp128 noundef %d, i8 noundef zeroext %e, i8 noundef %f, i8 noundef %g)
long double f_scalar_stack_4(int32_t a, int64_t b, int64_t c, long double d,
                             uint8_t e, int8_t f, uint8_t g) {
  return d;
}

// Aggregates and >=XLen scalars passed on the stack should be lowered just as
// they would be if passed via registers.

// CHECK-LABEL: define{{.*}} void @f_scalar_stack_5(double noundef %a, i64 noundef %b, double noundef %c, i64 noundef %d, i32 noundef %e, i64 noundef %f, float noundef %g, double noundef %h, fp128 noundef %i)
void f_scalar_stack_5(double a, int64_t b, double c, int64_t d, int e,
                      int64_t f, float g, double h, long double i) {}

// CHECK-LABEL: define{{.*}} void @f_agg_stack(double noundef %a, i64 noundef %b, double noundef %c, i64 noundef %d, i32 %e.coerce, [2 x i32] %f.coerce, i64 %g.coerce, ptr noundef %h)
void f_agg_stack(double a, int64_t b, double c, int64_t d, struct tiny e,
                 struct small f, struct small_aligned g, struct large h) {}

// Ensure that ABI lowering happens as expected for vararg calls. For RV32
// with the base integer calling convention there will be no observable
// differences in the lowered IR for a call with varargs vs without.

int f_va_callee(int, ...);

// CHECK-LABEL: define{{.*}} void @f_va_caller()
// CHECK: call i32 (i32, ...) @f_va_callee(i32 noundef 1, i32 noundef 2, i64 noundef 3, double noundef 4.000000e+00, double noundef 5.000000e+00, i32 {{%.*}}, [2 x i32] {{%.*}}, i64 {{%.*}}, ptr noundef {{%.*}})
void f_va_caller(void) {
  f_va_callee(1, 2, 3LL, 4.0f, 5.0, (struct tiny){6, 7, 8, 9},
              (struct small){10, NULL}, (struct small_aligned){11},
              (struct large){12, 13, 14, 15});
}

// CHECK-LABEL: define{{.*}} i32 @f_va_1(ptr noundef %fmt, ...) {{.*}} {
// CHECK:   [[FMT_ADDR:%.*]] = alloca ptr, align 4
// CHECK:   [[VA:%.*]] = alloca ptr, align 4
// CHECK:   [[V:%.*]] = alloca i32, align 4
// CHECK:   store ptr %fmt, ptr [[FMT_ADDR]], align 4
// CHECK:   call void @llvm.va_start.p0(ptr [[VA]])
// CHECK:   [[ARGP_CUR:%.*]] = load ptr, ptr [[VA]], align 4
// CHECK:   [[ARGP_NEXT:%.*]] = getelementptr inbounds i8, ptr [[ARGP_CUR]], i32 4
// CHECK:   store ptr [[ARGP_NEXT]], ptr [[VA]], align 4
// CHECK:   [[TMP1:%.*]] = load i32, ptr [[ARGP_CUR]], align 4
// CHECK:   store i32 [[TMP1]], ptr [[V]], align 4
// CHECK:   call void @llvm.va_end.p0(ptr [[VA]])
// CHECK:   [[TMP2:%.*]] = load i32, ptr [[V]], align 4
// CHECK:   ret i32 [[TMP2]]
// CHECK: }
int f_va_1(char *fmt, ...) {
  __builtin_va_list va;

  __builtin_va_start(va, fmt);
  int v = __builtin_va_arg(va, int);
  __builtin_va_end(va);

  return v;
}

// An "aligned" register pair (where the first register is even-numbered) is
// used to pass varargs with 2x xlen alignment and 2x xlen size. Ensure the
// correct offsets are used.

// CHECK-LABEL: @f_va_2(
// CHECK:         [[FMT_ADDR:%.*]] = alloca ptr, align 4
// CHECK-NEXT:    [[VA:%.*]] = alloca ptr, align 4
// CHECK-NEXT:    [[V:%.*]] = alloca double, align 8
// CHECK-NEXT:    store ptr [[FMT:%.*]], ptr [[FMT_ADDR]], align 4
// CHECK-NEXT:    call void @llvm.va_start.p0(ptr [[VA]])
// CHECK-NEXT:    [[ARGP_CUR:%.*]] = load ptr, ptr [[VA]], align 4
// CHECK-NEXT:    [[TMP0:%.*]] = ptrtoint ptr [[ARGP_CUR]] to i32
// CHECK-NEXT:    [[TMP1:%.*]] = add i32 [[TMP0]], 7
// CHECK-NEXT:    [[TMP2:%.*]] = and i32 [[TMP1]], -8
// CHECK-NEXT:    [[ARGP_CUR_ALIGNED:%.*]] = inttoptr i32 [[TMP2]] to ptr
// CHECK-NEXT:    [[ARGP_NEXT:%.*]] = getelementptr inbounds i8, ptr [[ARGP_CUR_ALIGNED]], i32 8
// CHECK-NEXT:    store ptr [[ARGP_NEXT]], ptr [[VA]], align 4
// CHECK-NEXT:    [[TMP4:%.*]] = load double, ptr [[ARGP_CUR_ALIGNED]], align 8
// CHECK-NEXT:    store double [[TMP4]], ptr [[V]], align 8
// CHECK-NEXT:    call void @llvm.va_end.p0(ptr [[VA]])
// CHECK-NEXT:    [[TMP5:%.*]] = load double, ptr [[V]], align 8
// CHECK-NEXT:    ret double [[TMP5]]
double f_va_2(char *fmt, ...) {
  __builtin_va_list va;

  __builtin_va_start(va, fmt);
  double v = __builtin_va_arg(va, double);
  __builtin_va_end(va);

  return v;
}

// Two "aligned" register pairs.

// CHECK-LABEL: @f_va_3(
// CHECK:         [[FMT_ADDR:%.*]] = alloca ptr, align 4
// CHECK-NEXT:    [[VA:%.*]] = alloca ptr, align 4
// CHECK-NEXT:    [[V:%.*]] = alloca double, align 8
// CHECK-NEXT:    [[W:%.*]] = alloca i32, align 4
// CHECK-NEXT:    [[X:%.*]] = alloca double, align 8
// CHECK-NEXT:    store ptr [[FMT:%.*]], ptr [[FMT_ADDR]], align 4
// CHECK-NEXT:    call void @llvm.va_start.p0(ptr [[VA]])
// CHECK-NEXT:    [[ARGP_CUR:%.*]] = load ptr, ptr [[VA]], align 4
// CHECK-NEXT:    [[TMP0:%.*]] = ptrtoint ptr [[ARGP_CUR]] to i32
// CHECK-NEXT:    [[TMP1:%.*]] = add i32 [[TMP0]], 7
// CHECK-NEXT:    [[TMP2:%.*]] = and i32 [[TMP1]], -8
// CHECK-NEXT:    [[ARGP_CUR_ALIGNED:%.*]] = inttoptr i32 [[TMP2]] to ptr
// CHECK-NEXT:    [[ARGP_NEXT:%.*]] = getelementptr inbounds i8, ptr [[ARGP_CUR_ALIGNED]], i32 8
// CHECK-NEXT:    store ptr [[ARGP_NEXT]], ptr [[VA]], align 4
// CHECK-NEXT:    [[TMP4:%.*]] = load double, ptr [[ARGP_CUR_ALIGNED]], align 8
// CHECK-NEXT:    store double [[TMP4]], ptr [[V]], align 8
// CHECK-NEXT:    [[ARGP_CUR2:%.*]] = load ptr, ptr [[VA]], align 4
// CHECK-NEXT:    [[ARGP_NEXT3:%.*]] = getelementptr inbounds i8, ptr [[ARGP_CUR2]], i32 4
// CHECK-NEXT:    store ptr [[ARGP_NEXT3]], ptr [[VA]], align 4
// CHECK-NEXT:    [[TMP6:%.*]] = load i32, ptr [[ARGP_CUR2]], align 4
// CHECK-NEXT:    store i32 [[TMP6]], ptr [[W]], align 4
// CHECK-NEXT:    [[ARGP_CUR4:%.*]] = load ptr, ptr [[VA]], align 4
// CHECK-NEXT:    [[TMP7:%.*]] = ptrtoint ptr [[ARGP_CUR4]] to i32
// CHECK-NEXT:    [[TMP8:%.*]] = add i32 [[TMP7]], 7
// CHECK-NEXT:    [[TMP9:%.*]] = and i32 [[TMP8]], -8
// CHECK-NEXT:    [[ARGP_CUR4_ALIGNED:%.*]] = inttoptr i32 [[TMP9]] to ptr
// CHECK-NEXT:    [[ARGP_NEXT5:%.*]] = getelementptr inbounds i8, ptr [[ARGP_CUR4_ALIGNED]], i32 8
// CHECK-NEXT:    store ptr [[ARGP_NEXT5]], ptr [[VA]], align 4
// CHECK-NEXT:    [[TMP11:%.*]] = load double, ptr [[ARGP_CUR4_ALIGNED]], align 8
// CHECK-NEXT:    store double [[TMP11]], ptr [[X]], align 8
// CHECK-NEXT:    call void @llvm.va_end.p0(ptr [[VA]])
// CHECK-NEXT:    [[TMP12:%.*]] = load double, ptr [[V]], align 8
// CHECK-NEXT:    [[TMP13:%.*]] = load double, ptr [[X]], align 8
// CHECK-NEXT:    [[ADD:%.*]] = fadd double [[TMP12]], [[TMP13]]
// CHECK-NEXT:    ret double [[ADD]]
double f_va_3(char *fmt, ...) {
  __builtin_va_list va;

  __builtin_va_start(va, fmt);
  double v = __builtin_va_arg(va, double);
  int w = __builtin_va_arg(va, int);
  double x = __builtin_va_arg(va, double);
  __builtin_va_end(va);

  return v + x;
}

// CHECK-LABEL: define{{.*}} i32 @f_va_4(ptr noundef %fmt, ...) {{.*}} {
// CHECK:         [[FMT_ADDR:%.*]] = alloca ptr, align 4
// CHECK-NEXT:    [[VA:%.*]] = alloca ptr, align 4
// CHECK-NEXT:    [[V:%.*]] = alloca i32, align 4
// CHECK-NEXT:    [[LD:%.*]] = alloca fp128, align 16
// CHECK-NEXT:    [[TS:%.*]] = alloca [[STRUCT_TINY:%.*]], align 1
// CHECK-NEXT:    [[SS:%.*]] = alloca [[STRUCT_SMALL:%.*]], align 4
// CHECK-NEXT:    [[LS:%.*]] = alloca [[STRUCT_LARGE:%.*]], align 4
// CHECK-NEXT:    [[RET:%.*]] = alloca i32, align 4
// CHECK-NEXT:    store ptr [[FMT:%.*]], ptr [[FMT_ADDR]], align 4
// CHECK-NEXT:    call void @llvm.va_start.p0(ptr [[VA]])
// CHECK-NEXT:    [[ARGP_CUR:%.*]] = load ptr, ptr [[VA]], align 4
// CHECK-NEXT:    [[ARGP_NEXT:%.*]] = getelementptr inbounds i8, ptr [[ARGP_CUR]], i32 4
// CHECK-NEXT:    store ptr [[ARGP_NEXT]], ptr [[VA]], align 4
// CHECK-NEXT:    [[TMP1:%.*]] = load i32, ptr [[ARGP_CUR]], align 4
// CHECK-NEXT:    store i32 [[TMP1]], ptr [[V]], align 4
// CHECK-NEXT:    [[ARGP_CUR2:%.*]] = load ptr, ptr [[VA]], align 4
// CHECK-NEXT:    [[ARGP_NEXT3:%.*]] = getelementptr inbounds i8, ptr [[ARGP_CUR2]], i32 4
// CHECK-NEXT:    store ptr [[ARGP_NEXT3]], ptr [[VA]], align 4
// CHECK-NEXT:    [[TMP3:%.*]] = load ptr, ptr [[ARGP_CUR2]], align 4
// CHECK-NEXT:    [[TMP4:%.*]] = load fp128, ptr [[TMP3]], align 16
// CHECK-NEXT:    store fp128 [[TMP4]], ptr [[LD]], align 16
// CHECK-NEXT:    [[ARGP_CUR4:%.*]] = load ptr, ptr [[VA]], align 4
// CHECK-NEXT:    [[ARGP_NEXT5:%.*]] = getelementptr inbounds i8, ptr [[ARGP_CUR4]], i32 4
// CHECK-NEXT:    store ptr [[ARGP_NEXT5]], ptr [[VA]], align 4
// CHECK-NEXT:    call void @llvm.memcpy.p0.p0.i32(ptr align 1 [[TS]], ptr align 4 [[ARGP_CUR4]], i32 4, i1 false)
// CHECK-NEXT:    [[ARGP_CUR6:%.*]] = load ptr, ptr [[VA]], align 4
// CHECK-NEXT:    [[ARGP_NEXT7:%.*]] = getelementptr inbounds i8, ptr [[ARGP_CUR6]], i32 8
// CHECK-NEXT:    store ptr [[ARGP_NEXT7]], ptr [[VA]], align 4
// CHECK-NEXT:    call void @llvm.memcpy.p0.p0.i32(ptr align 4 [[SS]], ptr align 4 [[ARGP_CUR6]], i32 8, i1 false)
// CHECK-NEXT:    [[ARGP_CUR8:%.*]] = load ptr, ptr [[VA]], align 4
// CHECK-NEXT:    [[ARGP_NEXT9:%.*]] = getelementptr inbounds i8, ptr [[ARGP_CUR8]], i32 4
// CHECK-NEXT:    store ptr [[ARGP_NEXT9]], ptr [[VA]], align 4
// CHECK-NEXT:    [[TMP12:%.*]] = load ptr, ptr [[ARGP_CUR8]], align 4
// CHECK-NEXT:    call void @llvm.memcpy.p0.p0.i32(ptr align 4 [[LS]], ptr align 4 [[TMP12]], i32 16, i1 false)
// CHECK-NEXT:    call void @llvm.va_end.p0(ptr [[VA]])
int f_va_4(char *fmt, ...) {
  __builtin_va_list va;

  __builtin_va_start(va, fmt);
  int v = __builtin_va_arg(va, int);
  long double ld = __builtin_va_arg(va, long double);
  struct tiny ts = __builtin_va_arg(va, struct tiny);
  struct small ss = __builtin_va_arg(va, struct small);
  struct large ls = __builtin_va_arg(va, struct large);
  __builtin_va_end(va);

  int ret = (int)((long double)v + ld);
  ret = ret + ts.a + ts.b + ts.c + ts.d;
  ret = ret + ss.a + (int)ss.b;
  ret = ret + ls.a + ls.b + ls.c + ls.d;

  return ret;
}
