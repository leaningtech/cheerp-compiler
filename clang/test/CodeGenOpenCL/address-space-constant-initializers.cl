// RUN: %clang_cc1 -triple %itanium_abi_triple %s -ffake-address-space-map -emit-llvm -o - | FileCheck -check-prefix=FAKE %s
// RUN: %clang_cc1 %s -triple amdgcn-amd-amdhsa -emit-llvm -o - | FileCheck -check-prefix=AMDGCN %s

typedef struct {
    int i;
    float f; // At non-zero offset.
} ArrayStruct;

__constant ArrayStruct constant_array_struct = { 0, 0.0f };

typedef struct {
    __constant float* constant_float_ptr;
} ConstantArrayPointerStruct;

// FAKE: %struct.ConstantArrayPointerStruct = type { float addrspace(2)* }
// FAKE: addrspace(2) constant %struct.ConstantArrayPointerStruct { float addrspace(2)* getelementptr inbounds (%struct.ArrayStruct, %struct.ArrayStruct addrspace(2)* @constant_array_struct, i32 0, i32 1) }
// AMDGCN: %struct.ConstantArrayPointerStruct = type { float addrspace(4)* }
// AMDGCN: addrspace(4) constant %struct.ConstantArrayPointerStruct { float addrspace(4)* getelementptr inbounds (%struct.ArrayStruct, %struct.ArrayStruct addrspace(4)* @constant_array_struct, i32 0, i32 1) }
// Bug  18567
__constant ConstantArrayPointerStruct constant_array_pointer_struct = {
    &constant_array_struct.f
};

__kernel void initializer_cast_is_valid_crash()
{
  unsigned char v512[64] = {
      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x02,0x00
  };

}
