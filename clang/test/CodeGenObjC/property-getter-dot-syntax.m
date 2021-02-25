// RUN: %clang_cc1 -triple %itanium_abi_triple -emit-llvm -o %t %s

@protocol NSObject
- (void *)description;
@end

int main()
{
        id<NSObject> eggs;
        void *eggsText= eggs.description;
}
