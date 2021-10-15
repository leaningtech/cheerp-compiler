; RUN: llc < %s

target triple = "x86_64-unknown-linux"

define float @test1()
{
	ret float extractelement (<2 x float> bitcast (<1 x double> <double 0x3f800000> to <2 x float>), i32 1);
}
