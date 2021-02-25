// RUN: %clang_cc1 -triple %itanium_abi_triple -emit-llvm -o %t %s

@interface Test { }
+ (Test *)crash;
+ (void)setCrash: (int)value;
@end

@implementation Test
static int _value;
- (void)cachesPath
{
 static Test *cachesPath;

 if (!cachesPath) {
  Test *crash = Test.crash;
 }
}
+ (Test *)crash{ return 0; }
+ (void)setCrash: (int)value{ _value = value; }
@end

