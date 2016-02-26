/* RUN: %clang_cc1 %s -emit-llvm -triple x86_64-apple-darwin -o - | FileCheck %s

The FE must generate padding here both at the end of each PyG_Head and
between array elements.  Reduced from Python. */

typedef union _gc_head {
  struct {
    union _gc_head *gc_next;
    union _gc_head *gc_prev;
    long gc_refs;
  } gc;
  int dummy __attribute__((aligned(16)));
} PyGC_Head;

struct gc_generation {
  PyGC_Head head;
  int threshold;
  int count;
};

#define GEN_HEAD(n) (&generations[n].head)

// The idea is that there are 6 undefs in this structure initializer to cover
// the padding between elements.
// CHECK: @generations = global [3 x %struct._Z13gc_generation] [%struct._Z13gc_generation { %union._Z8_gc_head { %struct._ZN8_gc_headUt7_3_E { %union._Z8_gc_head* getelementptr inbounds ([3 x %struct._Z13gc_generation], [3 x %struct._Z13gc_generation]* @generations, i32 0, i32 0, i32 0), %union._Z8_gc_head* getelementptr inbounds ([3 x %struct._Z13gc_generation], [3 x %struct._Z13gc_generation]* @generations, i32 0, i32 0, i32 0), i64 0 }, [8 x i8] undef }, i32 700, i32 0, [8 x i8] undef }, %struct._Z13gc_generation { %union._Z8_gc_head { %struct._ZN8_gc_headUt7_3_E { %union._Z8_gc_head* getelementptr inbounds ([3 x %struct._Z13gc_generation], [3 x %struct._Z13gc_generation]* @generations, i32 0, i32 1, i32 0), %union._Z8_gc_head* getelementptr inbounds ([3 x %struct._Z13gc_generation], [3 x %struct._Z13gc_generation]* @generations, i32 0, i32 1, i32 0), i64 0 }, [8 x i8] undef }, i32 10, i32 0, [8 x i8] undef }, %struct._Z13gc_generation { %union._Z8_gc_head { %struct._ZN8_gc_headUt7_3_E { %union._Z8_gc_head* getelementptr inbounds ([3 x %struct._Z13gc_generation], [3 x %struct._Z13gc_generation]* @generations, i32 0, i32 2, i32 0), %union._Z8_gc_head* getelementptr inbounds ([3 x %struct._Z13gc_generation], [3 x %struct._Z13gc_generation]* @generations, i32 0, i32 2, i32 0), i64 0 }, [8 x i8] undef }, i32 10, i32 0, [8 x i8] undef }]
/* linked lists of container objects */
struct gc_generation generations[3] = {
        /* PyGC_Head,                           threshold,      count */
        {{{GEN_HEAD(0), GEN_HEAD(0), 0}},       700,            0},
        {{{GEN_HEAD(1), GEN_HEAD(1), 0}},       10,             0},
        {{{GEN_HEAD(2), GEN_HEAD(2), 0}},       10,             0},
};
