// RUN: %clang_cc1 -triple %itanium_abi_triple %s -emit-llvm -o - | FileCheck %s
// PR 4349

union reg
{
    unsigned char b[2][2];
    unsigned short w[2];
    unsigned int d;
};
struct cpu
{
    union reg pc;
};
extern struct cpu cpu;
struct svar
{
    void *ptr;
};
// CHECK: @svars1 = {{(dso_local )?}}global [1 x %struct.svar] [%struct.svar { i8* bitcast (%union.reg* getelementptr inbounds (%struct.cpu, %struct.cpu* @cpu, i32 0, i32 0) to i8*) }]
struct svar svars1[] =
{
    { &((cpu.pc).w[0]) }
};
// CHECK: @svars2 = {{(dso_local )?}}global [1 x %struct.svar] [%struct.svar { i8* bitcast{{.*}}%struct.cpu* @cpu{{.*}} to i8*) }]
struct svar svars2[] =
{
    { &((cpu.pc).b[0][1]) }
};
// CHECK: @svars3 = {{(dso_local )?}}global [1 x %struct.svar] [%struct.svar { i8* bitcast{{.*}}%struct.cpu* @cpu{{.*}} to i8*) }]
struct svar svars3[] =
{
    { &((cpu.pc).w[1]) }
};
// CHECK: @svars4 = {{(dso_local )?}}global [1 x %struct.svar] [%struct.svar { i8* bitcast{{.*}}%struct.cpu* @cpu{{.*}} to i8*) }]
struct svar svars4[] =
{
    { &((cpu.pc).b[1][1]) }
};
