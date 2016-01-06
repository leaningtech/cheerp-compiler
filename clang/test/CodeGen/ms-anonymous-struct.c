// RUN: %clang_cc1 -fms-extensions -emit-llvm -o - %s | FileCheck %s

// CHECK: %struct._Z4test = type { i32, %struct._Z7nested2, i32 }
// CHECK: %struct._Z7nested2 = type { i32, %struct._Z7nested1, i32 }
// CHECK: %struct._Z7nested1 = type { i32, i32 }
typedef struct nested1 {
    int a1;
    int b1;
} NESTED1;

struct nested2 {
    int a;
    NESTED1; 
    int b;
};

struct test {
    int    x;
    struct nested2; 
    int    y;
};


void foo()
{
  // CHECK: %var = alloca %struct._Z4test, align 4
  struct test var;

  // CHECK: getelementptr inbounds %struct._Z4test, %struct._Z4test* %var, i32 0, i32 1
  // CHECK-NEXT: getelementptr inbounds %struct._Z7nested2, %struct._Z7nested2* %{{.*}}, i32 0, i32 0
  // CHECK-NEXT: load i32, i32* %{{.*}}, align 4
  var.a;

  // CHECK-NEXT: getelementptr inbounds %struct._Z4test, %struct._Z4test* %var, i32 0, i32 1
  // CHECK-NEXT: getelementptr inbounds %struct._Z7nested2, %struct._Z7nested2* %{{.*}}, i32 0, i32 2
  // CHECK-NEXT: load i32, i32* %{{.*}}, align 4
  var.b;

  // CHECK-NEXT: getelementptr inbounds %struct._Z4test, %struct._Z4test* %var, i32 0, i32 1
  // CHECK-NEXT: getelementptr inbounds %struct._Z7nested2, %struct._Z7nested2* %{{.*}}, i32 0, i32 1
  // CHECK-NEXT: getelementptr inbounds %struct._Z7nested1, %struct._Z7nested1* %{{.*}}, i32 0, i32 0
  // CHECK-NEXT: load i32, i32* %{{.*}}, align 4
  var.a1;

  // CHECK-NEXT: getelementptr inbounds %struct._Z4test, %struct._Z4test* %{{.*}}var, i32 0, i32 1
  // CHECK-NEXT: getelementptr inbounds %struct._Z7nested2, %struct._Z7nested2* %{{.*}}, i32 0, i32 1
  // CHECK-NEXT: getelementptr inbounds %struct._Z7nested1, %struct._Z7nested1* %{{.*}}, i32 0, i32 1
  // CHECK-NEXT: load i32, i32* %{{.*}}, align 4
  var.b1;

  // CHECK-NEXT: getelementptr inbounds %struct._Z4test, %struct._Z4test* %var, i32 0, i32 0
  // CHECK-NEXT: load i32, i32* %{{.*}}, align 4
  var.x;

  // CHECK-NEXT: getelementptr inbounds %struct._Z4test, %struct._Z4test* %var, i32 0, i32 2
  // CHECK-NEXT: load i32, i32* %{{.*}}, align 4
  var.y;
}

void foo2(struct test* var)
{
  // CHECK: alloca %struct._Z4test*, align
  // CHECK-NEXT: store %struct._Z4test* %var, %struct._Z4test** %{{.*}}, align
  // CHECK-NEXT: load %struct._Z4test*, %struct._Z4test** %{{.*}}, align
  // CHECK-NEXT: getelementptr inbounds %struct._Z4test, %struct._Z4test* %{{.*}}, i32 0, i32 1
  // CHECK-NEXT: getelementptr inbounds %struct._Z7nested2, %struct._Z7nested2* %{{.*}}, i32 0, i32 0
  // CHECK-NEXT: load i32, i32* %{{.*}}, align 4
  var->a;

  // CHECK-NEXT: load %struct._Z4test*, %struct._Z4test** %{{.*}}, align
  // CHECK-NEXT: getelementptr inbounds %struct._Z4test, %struct._Z4test* %{{.*}}, i32 0, i32 1
  // CHECK-NEXT: getelementptr inbounds %struct._Z7nested2, %struct._Z7nested2* %{{.*}}, i32 0, i32 2
  // CHECK-NEXT: load i32, i32* %{{.*}}, align 4
  var->b;

  // CHECK-NEXT: load %struct._Z4test*, %struct._Z4test** %{{.*}}, align
  // CHECK-NEXT: getelementptr inbounds %struct._Z4test, %struct._Z4test* %{{.*}}, i32 0, i32 1
  // CHECK-NEXT: getelementptr inbounds %struct._Z7nested2, %struct._Z7nested2* %{{.*}}, i32 0, i32 1
  // CHECK-NEXT: getelementptr inbounds %struct._Z7nested1, %struct._Z7nested1* %{{.*}}, i32 0, i32 0
  // CHECK-NEXT: load i32, i32* %{{.*}}, align 4
  var->a1;

  // CHECK-NEXT: load %struct._Z4test*, %struct._Z4test** %{{.*}}, align
  // CHECK-NEXT: getelementptr inbounds %struct._Z4test, %struct._Z4test* %{{.*}}, i32 0, i32 1
  // CHECK-NEXT: getelementptr inbounds %struct._Z7nested2, %struct._Z7nested2* %{{.*}}, i32 0, i32 1
  // CHECK-NEXT: getelementptr inbounds %struct._Z7nested1, %struct._Z7nested1* %{{.*}}, i32 0, i32 1
  // CHECK-NEXT: load i32, i32* %{{.*}}, align 4
  var->b1;

  // CHECK-NEXT: load %struct._Z4test*, %struct._Z4test** %{{.*}}, align
  // CHECK-NEXT: getelementptr inbounds %struct._Z4test, %struct._Z4test* %{{.*}}, i32 0, i32 0
  // CHECK-NEXT: load i32, i32* %{{.*}}, align 4
  var->x;

  // CHECK-NEXT: load %struct._Z4test*, %struct._Z4test** %{{.*}}, align
  // CHECK-NEXT: getelementptr inbounds %struct._Z4test, %struct._Z4test* %{{.*}}, i32 0, i32 2
  // CHECK-NEXT: load i32, i32* %{{.*}}, align 4
  var->y;
}
