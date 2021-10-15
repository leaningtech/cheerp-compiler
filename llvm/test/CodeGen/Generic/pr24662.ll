; RUN: llc < %s -fast-isel
; RUN: llc < %s

target triple = "x86_64-unknown-linux"

define i60 @PR24662a() {
  ret i60 trunc (i670010 fptoui(float 0x400D9999A0000000 to i670010) to i60)
}

define i60 @PR24662b() {
  %1 = fptoui float 0x400D9999A0000000 to i670010
  %2 = trunc i670010 %1 to i60
  ret i60 %2
}
