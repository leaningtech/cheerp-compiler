; Bitcode compatibility test for llvm
;
; Please update this file when making any IR changes. Information on the
; release process for this file is available here:
;
;     http://llvm.org/docs/DeveloperPolicy.html#ir-backwards-compatibility

; RUN: llvm-as < %s | llvm-dis | llvm-as | llvm-dis | FileCheck %s
; RUN: verify-uselistorder < %s

target datalayout = "E"
; CHECK: target datalayout = "E"

target triple = "x86_64-apple-macosx10.10.0"
; CHECK: target triple = "x86_64-apple-macosx10.10.0"

;; Module-level assembly
module asm "beep boop"
; CHECK: module asm "beep boop"

;; Comdats
$comdat.any = comdat any
; CHECK: $comdat.any = comdat any
$comdat.exactmatch = comdat exactmatch
; CHECK: $comdat.exactmatch = comdat exactmatch
$comdat.largest = comdat largest
; CHECK: $comdat.largest = comdat largest
$comdat.noduplicates = comdat nodeduplicate
; CHECK: $comdat.noduplicates = comdat nodeduplicate
$comdat.samesize = comdat samesize
; CHECK: $comdat.samesize = comdat samesize

;; Constants
@const.true = constant i1 true
; CHECK: @const.true = constant i1 true
@const.false = constant i1 false
; CHECK: @const.false = constant i1 false
@const.int = constant i32 zeroinitializer
; CHECK: @const.int = constant i32 0
@const.float = constant double 0.0
; CHECK: @const.float = constant double 0.0
@const.null = constant i8* null
; CHECK: @const.null = constant i8* null
%const.struct.type = type { i32, i8, i64 }
%const.struct.type.packed = type <{ i32, i8 }>
@const.struct = constant %const.struct.type { i32 -1, i8 undef, i64 poison }
; CHECK: @const.struct = constant %const.struct.type { i32 -1, i8 undef, i64 poison }
@const.struct.packed = constant %const.struct.type.packed <{ i32 -1, i8 1 }>
; CHECK: @const.struct.packed = constant %const.struct.type.packed <{ i32 -1, i8 1 }>

; CHECK: @constant.array.i8  = constant [3 x i8] c"\00\01\00"
@constant.array.i8  = constant [3 x i8] [i8 -0, i8 1, i8 0]
; CHECK: @constant.array.i16 = constant [3 x i16] [i16 0, i16 1, i16 0]
@constant.array.i16 = constant [3 x i16] [i16 -0, i16 1, i16 0]
; CHECK: @constant.array.i32 = constant [3 x i32] [i32 0, i32 1, i32 0]
@constant.array.i32 = constant [3 x i32] [i32 -0, i32 1, i32 0]
; CHECK: @constant.array.i64 = constant [3 x i64] [i64 0, i64 1, i64 0]
@constant.array.i64 = constant [3 x i64] [i64 -0, i64 1, i64 0]
; CHECK: @constant.array.f16 = constant [3 x half] [half 0xH8000, half 0xH3C00, half 0xH0000]
@constant.array.f16 = constant [3 x half] [half -0.0, half 1.0, half 0.0]
; CHECK: @constant.array.f32 = constant [3 x float] [float -0.000000e+00, float 1.000000e+00, float 0.000000e+00]
@constant.array.f32 = constant [3 x float] [float -0.0, float 1.0, float 0.0]
; CHECK: @constant.array.f64 = constant [3 x double] [double -0.000000e+00, double 1.000000e+00, double 0.000000e+00]
@constant.array.f64 = constant [3 x double] [double -0.0, double 1.0, double 0.0]

; CHECK: @constant.vector.i8  = constant <3 x i8>  <i8 0, i8 1, i8 0>
@constant.vector.i8  = constant <3 x i8>  <i8 -0, i8 1, i8 0>
; CHECK: @constant.vector.i16 = constant <3 x i16> <i16 0, i16 1, i16 0>
@constant.vector.i16 = constant <3 x i16> <i16 -0, i16 1, i16 0>
; CHECK: @constant.vector.i32 = constant <3 x i32> <i32 0, i32 1, i32 0>
@constant.vector.i32 = constant <3 x i32> <i32 -0, i32 1, i32 0>
; CHECK: @constant.vector.i64 = constant <3 x i64> <i64 0, i64 1, i64 0>
@constant.vector.i64 = constant <3 x i64> <i64 -0, i64 1, i64 0>
; CHECK: @constant.vector.f16 = constant <3 x half> <half 0xH8000, half 0xH3C00, half 0xH0000>
@constant.vector.f16 = constant <3 x half> <half -0.0, half 1.0, half 0.0>
; CHECK: @constant.vector.f32 = constant <3 x float> <float -0.000000e+00, float 1.000000e+00, float 0.000000e+00>
@constant.vector.f32 = constant <3 x float> <float -0.0, float 1.0, float 0.0>
; CHECK: @constant.vector.f64 = constant <3 x double> <double -0.000000e+00, double 1.000000e+00, double 0.000000e+00>
@constant.vector.f64 = constant <3 x double> <double -0.0, double 1.0, double 0.0>

;; Global Variables
; Format: [@<GlobalVarName> =] [Linkage] [Visibility] [DLLStorageClass]
;         [ThreadLocal] [(unnamed_addr|local_unnamed_addr)] [AddrSpace] [ExternallyInitialized]
;         <global | constant> <Type> [<InitializerConstant>]
;         [, section "name"] [, comdat [($name)]] [, align <Alignment>]

; Global Variables -- Simple
@g1 = global i32 0
; CHECK: @g1 = global i32 0
@g2 = constant i32 0
; CHECK: @g2 = constant i32 0

; Global Variables -- Linkage
@g.private = private global i32 0
; CHECK: @g.private = private global i32 0
@g.internal = internal global i32 0
; CHECK: @g.internal = internal global i32 0
@g.available_externally = available_externally global i32 0
; CHECK: @g.available_externally = available_externally global i32 0
@g.linkonce = linkonce global i32 0
; CHECK: @g.linkonce = linkonce global i32 0
@g.weak = weak global i32 0
; CHECK: @g.weak = weak global i32 0
@g.common = common global i32 0
; CHECK: @g.common = common global i32 0
@g.appending = appending global [4 x i8] c"test"
; CHECK: @g.appending = appending global [4 x i8] c"test"
@g.extern_weak = extern_weak global i32
; CHECK: @g.extern_weak = extern_weak global i32
@g.linkonce_odr = linkonce_odr global i32 0
; CHECK: @g.linkonce_odr = linkonce_odr global i32 0
@g.weak_odr = weak_odr global i32 0
; CHECK: @g.weak_odr = weak_odr global i32 0
@g.external = external global i32
; CHECK: @g.external = external global i32

; Global Variables -- Visibility
@g.default = default global i32 0
; CHECK: @g.default = global i32 0
@g.hidden = hidden global i32 0
; CHECK: @g.hidden = hidden global i32 0
@g.protected = protected global i32 0
; CHECK: @g.protected = protected global i32 0

; Global Variables -- DLLStorageClass
@g.dlldefault = default global i32 0
; CHECK: @g.dlldefault = global i32 0
@g.dllimport = external dllimport global i32
; CHECK: @g.dllimport = external dllimport global i32
@g.dllexport = dllexport global i32 0
; CHECK: @g.dllexport = dllexport global i32 0

; Global Variables -- ThreadLocal
@g.notthreadlocal = global i32 0
; CHECK: @g.notthreadlocal = global i32 0
@g.generaldynamic = thread_local global i32 0
; CHECK: @g.generaldynamic = thread_local global i32 0
@g.localdynamic = thread_local(localdynamic) global i32 0
; CHECK: @g.localdynamic = thread_local(localdynamic) global i32 0
@g.initialexec = thread_local(initialexec) global i32 0
; CHECK: @g.initialexec = thread_local(initialexec) global i32 0
@g.localexec = thread_local(localexec) global i32 0
; CHECK: @g.localexec = thread_local(localexec) global i32 0

; Global Variables -- unnamed_addr and local_unnamed_addr
@g.unnamed_addr = unnamed_addr global i32 0
; CHECK: @g.unnamed_addr = unnamed_addr global i32 0
@g.local_unnamed_addr = local_unnamed_addr global i32 0
; CHECK: @g.local_unnamed_addr = local_unnamed_addr global i32 0

; Global Variables -- AddrSpace
@g.addrspace = addrspace(1) global i32 0
; CHECK: @g.addrspace = addrspace(1) global i32 0

; Global Variables -- ExternallyInitialized
@g.externally_initialized = external externally_initialized global i32
; CHECK: @g.externally_initialized = external externally_initialized global i32

; Global Variables -- section
@g.section = global i32 0, section "_DATA"
; CHECK: @g.section = global i32 0, section "_DATA"

; Global Variables -- partition
@g.partition = global i32 0, partition "part"
; CHECK: @g.partition = global i32 0, partition "part"

; Global Variables -- comdat
@comdat.any = global i32 0, comdat
; CHECK: @comdat.any = global i32 0, comdat
@comdat.exactmatch = global i32 0, comdat
; CHECK: @comdat.exactmatch = global i32 0, comdat
@comdat.largest = global i32 0, comdat
; CHECK: @comdat.largest = global i32 0, comdat
@comdat.noduplicates = global i32 0, comdat
; CHECK: @comdat.noduplicates = global i32 0, comdat
@comdat.samesize = global i32 0, comdat
; CHECK: @comdat.samesize = global i32 0, comdat

; Force two globals from different comdats into sections with the same name.
$comdat1 = comdat any
$comdat2 = comdat any
@g.comdat1 = global i32 0, section "SharedSection", comdat($comdat1)
; CHECK: @g.comdat1 = global i32 0, section "SharedSection", comdat($comdat1)
@g.comdat2 = global i32 0, section "SharedSection", comdat($comdat2)
; CHECK: @g.comdat2 = global i32 0, section "SharedSection", comdat($comdat2)

; Global Variables -- align
@g.align = global i32 0, align 4
; CHECK: @g.align = global i32 0, align 4

; Global Variables -- Intrinsics
%pri.func.data = type { i32, void ()*, i8* }
@g.used1 = global i32 0
@g.used2 = global i32 0
@g.used3 = global i8 0
declare void @g.f1()
@llvm.used = appending global [1 x i32*] [i32* @g.used1], section "llvm.metadata"
; CHECK: @llvm.used = appending global [1 x i32*] [i32* @g.used1], section "llvm.metadata"
@llvm.compiler.used = appending global [1 x i32*] [i32* @g.used2], section "llvm.metadata"
; CHECK: @llvm.compiler.used = appending global [1 x i32*] [i32* @g.used2], section "llvm.metadata"
@llvm.global_ctors = appending global [1 x %pri.func.data] [%pri.func.data { i32 0, void ()* @g.f1, i8* @g.used3 }], section "llvm.metadata"
; CHECK: @llvm.global_ctors = appending global [1 x %pri.func.data] [%pri.func.data { i32 0, void ()* @g.f1, i8* @g.used3 }], section "llvm.metadata"
@llvm.global_dtors = appending global [1 x %pri.func.data] [%pri.func.data { i32 0, void ()* @g.f1, i8* @g.used3 }], section "llvm.metadata"
; CHECK: @llvm.global_dtors = appending global [1 x %pri.func.data] [%pri.func.data { i32 0, void ()* @g.f1, i8* @g.used3 }], section "llvm.metadata"

; Global Variables -- sanitizers
@g.no_sanitize_address = global i32 0, no_sanitize_address
@g.no_sanitize_hwaddress = global i32 0, no_sanitize_hwaddress
@g.sanitize_memtag = global i32 0, sanitize_memtag
@g.no_sanitize_multiple = global i32 0, no_sanitize_address, no_sanitize_hwaddress
@g.sanitize_address_dyninit = global i32 0, sanitize_address_dyninit
@g.sanitize_multiple = global i32 0, sanitize_memtag, sanitize_address_dyninit
; CHECK: @g.no_sanitize_address = global i32 0, no_sanitize_address
; CHECK: @g.no_sanitize_hwaddress = global i32 0, no_sanitize_hwaddress
; CHECK: @g.sanitize_memtag = global i32 0, sanitize_memtag
; CHECK: @g.no_sanitize_multiple = global i32 0, no_sanitize_address, no_sanitize_hwaddress
; CHECK: @g.sanitize_address_dyninit = global i32 0, sanitize_address_dyninit
; CHECK: @g.sanitize_multiple = global i32 0, sanitize_memtag, sanitize_address_dyninit

;; Aliases
; Format: @<Name> = [Linkage] [Visibility] [DLLStorageClass] [ThreadLocal]
;                   [unnamed_addr] alias <AliaseeTy> @<Aliasee>

; Aliases -- Linkage
@a.private = private alias i32, i32* @g.private
; CHECK: @a.private = private alias i32, i32* @g.private
@a.internal = internal alias i32, i32* @g.internal
; CHECK: @a.internal = internal alias i32, i32* @g.internal
@a.linkonce = linkonce alias i32, i32* @g.linkonce
; CHECK: @a.linkonce = linkonce alias i32, i32* @g.linkonce
@a.weak = weak alias i32, i32* @g.weak
; CHECK: @a.weak = weak alias i32, i32* @g.weak
@a.linkonce_odr = linkonce_odr alias i32, i32* @g.linkonce_odr
; CHECK: @a.linkonce_odr = linkonce_odr alias i32, i32* @g.linkonce_odr
@a.weak_odr = weak_odr alias i32, i32* @g.weak_odr
; CHECK: @a.weak_odr = weak_odr alias i32, i32* @g.weak_odr
@a.external = external alias i32, i32* @g1
; CHECK: @a.external = alias i32, i32* @g1

; Aliases -- Visibility
@a.default = default alias i32, i32* @g.default
; CHECK: @a.default = alias i32, i32* @g.default
@a.hidden = hidden alias i32, i32* @g.hidden
; CHECK: @a.hidden = hidden alias i32, i32* @g.hidden
@a.protected = protected alias i32, i32* @g.protected
; CHECK: @a.protected = protected alias i32, i32* @g.protected

; Aliases -- DLLStorageClass
@a.dlldefault = default alias i32, i32* @g.dlldefault
; CHECK: @a.dlldefault = alias i32, i32* @g.dlldefault
@a.dllexport = dllexport alias i32, i32* @g.dllexport
; CHECK: @a.dllexport = dllexport alias i32, i32* @g.dllexport

; Aliases -- ThreadLocal
@a.notthreadlocal = alias i32, i32* @g.notthreadlocal
; CHECK: @a.notthreadlocal = alias i32, i32* @g.notthreadlocal
@a.generaldynamic = thread_local alias i32, i32* @g.generaldynamic
; CHECK: @a.generaldynamic = thread_local alias i32, i32* @g.generaldynamic
@a.localdynamic = thread_local(localdynamic) alias i32, i32* @g.localdynamic
; CHECK: @a.localdynamic = thread_local(localdynamic) alias i32, i32* @g.localdynamic
@a.initialexec = thread_local(initialexec) alias i32, i32* @g.initialexec
; CHECK: @a.initialexec = thread_local(initialexec) alias i32, i32* @g.initialexec
@a.localexec = thread_local(localexec) alias i32, i32* @g.localexec
; CHECK: @a.localexec = thread_local(localexec) alias i32, i32* @g.localexec

; Aliases -- unnamed_addr and local_unnamed_addr
@a.unnamed_addr = unnamed_addr alias i32, i32* @g.unnamed_addr
; CHECK: @a.unnamed_addr = unnamed_addr alias i32, i32* @g.unnamed_addr
@a.local_unnamed_addr = local_unnamed_addr alias i32, i32* @g.local_unnamed_addr
; CHECK: @a.local_unnamed_addr = local_unnamed_addr alias i32, i32* @g.local_unnamed_addr

; Aliases -- partition
; CHECK: @alias.partition = alias i32, i32* @g.partition, partition "part"
@alias.partition = alias i32, i32* @g.partition, partition "part"

;; IFunc
; Format @<Name> = [Linkage] [Visibility] ifunc <IFuncTy>,
;                  <ResolverTy>* @<Resolver>

; IFunc -- Linkage
@ifunc.external = external ifunc void (), void ()* ()* @ifunc_resolver
; CHECK: @ifunc.external = ifunc void (), void ()* ()* @ifunc_resolver
@ifunc.private = private ifunc void (), void ()* ()* @ifunc_resolver
; CHECK: @ifunc.private = private ifunc void (), void ()* ()* @ifunc_resolver
@ifunc.internal = internal ifunc void (), void ()* ()* @ifunc_resolver
; CHECK: @ifunc.internal = internal ifunc void (), void ()* ()* @ifunc_resolver

; IFunc -- Visibility
@ifunc.default = default ifunc void (), void ()* ()* @ifunc_resolver
; CHECK: @ifunc.default = ifunc void (), void ()* ()* @ifunc_resolver
@ifunc.hidden = hidden ifunc void (), void ()* ()* @ifunc_resolver
; CHECK: @ifunc.hidden = hidden ifunc void (), void ()* ()* @ifunc_resolver
@ifunc.protected = protected ifunc void (), void ()* ()* @ifunc_resolver
; CHECK: @ifunc.protected = protected ifunc void (), void ()* ()* @ifunc_resolver

; IFunc -- partition
; CHECK: @ifunc.partition = ifunc void (), void ()* ()* @ifunc_resolver, partition "part"
@ifunc.partition = ifunc void (), void ()* ()* @ifunc_resolver, partition "part"

define void ()* @ifunc_resolver() {
entry:
  ret void ()* null
}

;; Functions
; Format: define [linkage] [visibility] [DLLStorageClass]
;         [cconv] [ret attrs]
;         <ResultType> @<FunctionName> ([argument list])
;         [(unnamed_addr|local_unnamed_addr)] [fn Attrs] [section "name"] [comdat [($name)]]
;         [align N] [gc] [prefix Constant] [prologue Constant]
;         [personality Constant] { ... }

; Functions -- Simple
declare void @f1 ()
; CHECK: declare void @f1()

define void @f2 () {
; CHECK: define void @f2()
entry:
  ret void
}

; Functions -- linkage
define private void @f.private() {
; CHECK: define private void @f.private()
entry:
  ret void
}
define internal void @f.internal() {
; CHECK: define internal void @f.internal()
entry:
  ret void
}
define available_externally void @f.available_externally() {
; CHECK: define available_externally void @f.available_externally()
entry:
  ret void
}
define linkonce void @f.linkonce() {
; CHECK: define linkonce void @f.linkonce()
entry:
  ret void
}
define weak void @f.weak() {
; CHECK: define weak void @f.weak()
entry:
  ret void
}
define linkonce_odr void @f.linkonce_odr() {
; CHECK: define linkonce_odr void @f.linkonce_odr()
entry:
  ret void
}
define weak_odr void @f.weak_odr() {
; CHECK: define weak_odr void @f.weak_odr()
entry:
  ret void
}
declare external void @f.external()
; CHECK: declare void @f.external()
declare extern_weak void @f.extern_weak()
; CHECK: declare extern_weak void @f.extern_weak()

; Functions -- visibility
declare default void @f.default()
; CHECK: declare void @f.default()
declare hidden void @f.hidden()
; CHECK: declare hidden void @f.hidden()
declare protected void @f.protected()
; CHECK: declare protected void @f.protected()

; Functions -- DLLStorageClass
declare dllimport void @f.dllimport()
; CHECK: declare dllimport void @f.dllimport()
declare dllexport void @f.dllexport()
; CHECK: declare dllexport void @f.dllexport()

; Functions -- cconv (Calling conventions)
declare ccc void @f.ccc()
; CHECK: declare void @f.ccc()
declare fastcc void @f.fastcc()
; CHECK: declare fastcc void @f.fastcc()
declare coldcc void @f.coldcc()
; CHECK: declare coldcc void @f.coldcc()
declare cc10 void @f.cc10()
; CHECK: declare ghccc void @f.cc10()
declare ghccc void @f.ghccc()
; CHECK: declare ghccc void @f.ghccc()
declare cc11 void @f.cc11()
; CHECK: declare cc11 void @f.cc11()
declare webkit_jscc void @f.webkit_jscc()
; CHECK: declare webkit_jscc void @f.webkit_jscc()
declare anyregcc void @f.anyregcc()
; CHECK: declare anyregcc void @f.anyregcc()
declare preserve_mostcc void @f.preserve_mostcc()
; CHECK: declare preserve_mostcc void @f.preserve_mostcc()
declare preserve_allcc void @f.preserve_allcc()
; CHECK: declare preserve_allcc void @f.preserve_allcc()
declare swifttailcc void @f.swifttailcc()
; CHECK: declare swifttailcc void @f.swifttailcc()
declare cc64 void @f.cc64()
; CHECK: declare x86_stdcallcc void @f.cc64()
declare x86_stdcallcc void @f.x86_stdcallcc()
; CHECK: declare x86_stdcallcc void @f.x86_stdcallcc()
declare cc65 void @f.cc65()
; CHECK: declare x86_fastcallcc void @f.cc65()
declare x86_fastcallcc void @f.x86_fastcallcc()
; CHECK: declare x86_fastcallcc void @f.x86_fastcallcc()
declare cc66 void @f.cc66()
; CHECK: declare arm_apcscc void @f.cc66()
declare arm_apcscc void @f.arm_apcscc()
; CHECK: declare arm_apcscc void @f.arm_apcscc()
declare cc67 void @f.cc67()
; CHECK: declare arm_aapcscc void @f.cc67()
declare arm_aapcscc void @f.arm_aapcscc()
; CHECK: declare arm_aapcscc void @f.arm_aapcscc()
declare cc68 void @f.cc68()
; CHECK: declare arm_aapcs_vfpcc void @f.cc68()
declare arm_aapcs_vfpcc void @f.arm_aapcs_vfpcc()
; CHECK: declare arm_aapcs_vfpcc void @f.arm_aapcs_vfpcc()
declare cc69 void @f.cc69()
; CHECK: declare msp430_intrcc void @f.cc69()
declare msp430_intrcc void @f.msp430_intrcc()
; CHECK: declare msp430_intrcc void @f.msp430_intrcc()
declare cc70 void @f.cc70()
; CHECK: declare x86_thiscallcc void @f.cc70()
declare x86_thiscallcc void @f.x86_thiscallcc()
; CHECK: declare x86_thiscallcc void @f.x86_thiscallcc()
declare cc71 void @f.cc71()
; CHECK: declare ptx_kernel void @f.cc71()
declare ptx_kernel void @f.ptx_kernel()
; CHECK: declare ptx_kernel void @f.ptx_kernel()
declare cc72 void @f.cc72()
; CHECK: declare ptx_device void @f.cc72()
declare ptx_device void @f.ptx_device()
; CHECK: declare ptx_device void @f.ptx_device()
declare cc75 void @f.cc75()
; CHECK: declare spir_func void @f.cc75()
declare spir_func void @f.spir_func()
; CHECK: declare spir_func void @f.spir_func()
declare cc76 void @f.cc76()
; CHECK: declare spir_kernel void @f.cc76()
declare spir_kernel void @f.spir_kernel()
; CHECK: declare spir_kernel void @f.spir_kernel()
declare cc77 void @f.cc77()
; CHECK: declare intel_ocl_bicc void @f.cc77()
declare intel_ocl_bicc void @f.intel_ocl_bicc()
; CHECK: declare intel_ocl_bicc void @f.intel_ocl_bicc()
declare cc78 void @f.cc78()
; CHECK: declare x86_64_sysvcc void @f.cc78()
declare x86_64_sysvcc void @f.x86_64_sysvcc()
; CHECK: declare x86_64_sysvcc void @f.x86_64_sysvcc()
declare cc79 void @f.cc79()
; CHECK: declare win64cc void @f.cc79()
declare win64cc void @f.win64cc()
; CHECK: declare win64cc void @f.win64cc()
declare cc80 void @f.cc80()
; CHECK: declare x86_vectorcallcc void @f.cc80()
declare x86_vectorcallcc void @f.x86_vectorcallcc()
; CHECK: declare x86_vectorcallcc void @f.x86_vectorcallcc()
declare cc81 void @f.cc81()
; CHECK: declare hhvmcc void @f.cc81()
declare hhvmcc void @f.hhvmcc()
; CHECK: declare hhvmcc void @f.hhvmcc()
declare cc82 void @f.cc82()
; CHECK: declare hhvm_ccc void @f.cc82()
declare hhvm_ccc void @f.hhvm_ccc()
; CHECK: declare hhvm_ccc void @f.hhvm_ccc()
declare cc83 void @f.cc83(i8* byval(i8))
; CHECK: declare x86_intrcc void @f.cc83(i8* byval(i8))
declare x86_intrcc void @f.x86_intrcc(i8* byval(i8))
; CHECK: declare x86_intrcc void @f.x86_intrcc(i8* byval(i8))
declare cc84 void @f.cc84()
; CHECK: declare avr_intrcc void @f.cc84()
declare avr_intrcc void @f.avr_intrcc()
; CHECK: declare avr_intrcc void @f.avr_intrcc()
declare cc85 void @f.cc85()
; CHECK: declare avr_signalcc void @f.cc85()
declare avr_signalcc void @f.avr_signalcc()
; CHECK: declare avr_signalcc void @f.avr_signalcc()
declare cc87 void @f.cc87()
; CHECK: declare amdgpu_vs void @f.cc87()
declare amdgpu_vs void @f.amdgpu_vs()
; CHECK: declare amdgpu_vs void @f.amdgpu_vs()
declare cc88 void @f.cc88()
; CHECK: declare amdgpu_gs void @f.cc88()
declare amdgpu_gs void @f.amdgpu_gs()
; CHECK: declare amdgpu_gs void @f.amdgpu_gs()
declare cc89 void @f.cc89()
; CHECK: declare amdgpu_ps void @f.cc89()
declare amdgpu_ps void @f.amdgpu_ps()
; CHECK: declare amdgpu_ps void @f.amdgpu_ps()
declare cc90 void @f.cc90()
; CHECK: declare amdgpu_cs void @f.cc90()
declare amdgpu_cs void @f.amdgpu_cs()
; CHECK: declare amdgpu_cs void @f.amdgpu_cs()
declare amdgpu_gfx void @f.amdgpu_gfx()
; CHECK: declare amdgpu_gfx void @f.amdgpu_gfx()
declare cc91 void @f.cc91()
; CHECK: declare amdgpu_kernel void @f.cc91()
declare amdgpu_kernel void @f.amdgpu_kernel()
; CHECK: declare amdgpu_kernel void @f.amdgpu_kernel()
declare cc93 void @f.cc93()
; CHECK: declare amdgpu_hs void @f.cc93()
declare amdgpu_hs void @f.amdgpu_hs()
; CHECK: declare amdgpu_hs void @f.amdgpu_hs()
declare cc95 void @f.cc95()
; CHECK: declare amdgpu_ls void @f.cc95()
declare amdgpu_ls void @f.amdgpu_ls()
; CHECK: declare amdgpu_ls void @f.amdgpu_ls()
declare cc96 void @f.cc96()
; CHECK: declare amdgpu_es void @f.cc96()
declare amdgpu_es void @f.amdgpu_es()
; CHECK: declare amdgpu_es void @f.amdgpu_es()
declare cc1023 void @f.cc1023()
; CHECK: declare cc1023 void @f.cc1023()

; Functions -- ret attrs (Return attributes)
declare zeroext i64 @f.zeroext()
; CHECK: declare zeroext i64 @f.zeroext()
declare signext i64 @f.signext()
; CHECK: declare signext i64 @f.signext()
declare inreg i32* @f.inreg()
; CHECK: declare inreg i32* @f.inreg()
declare noalias i32* @f.noalias()
; CHECK: declare noalias i32* @f.noalias()
declare nonnull i32* @f.nonnull()
; CHECK: declare nonnull i32* @f.nonnull()
declare dereferenceable(4) i32* @f.dereferenceable4()
; CHECK: declare dereferenceable(4) i32* @f.dereferenceable4()
declare dereferenceable(8) i32* @f.dereferenceable8()
; CHECK: declare dereferenceable(8) i32* @f.dereferenceable8()
declare dereferenceable(16) i32* @f.dereferenceable16()
; CHECK: declare dereferenceable(16) i32* @f.dereferenceable16()
declare dereferenceable_or_null(4) i32* @f.dereferenceable4_or_null()
; CHECK: declare dereferenceable_or_null(4) i32* @f.dereferenceable4_or_null()
declare dereferenceable_or_null(8) i32* @f.dereferenceable8_or_null()
; CHECK: declare dereferenceable_or_null(8) i32* @f.dereferenceable8_or_null()
declare dereferenceable_or_null(16) i32* @f.dereferenceable16_or_null()
; CHECK: declare dereferenceable_or_null(16) i32* @f.dereferenceable16_or_null()

; Functions -- Parameter attributes
declare void @f.param.zeroext(i8 zeroext)
; CHECK: declare void @f.param.zeroext(i8 zeroext)
declare void @f.param.signext(i8 signext)
; CHECK: declare void @f.param.signext(i8 signext)
declare void @f.param.inreg(i8 inreg)
; CHECK: declare void @f.param.inreg(i8 inreg)
declare void @f.param.byval({ i8, i8 }* byval({ i8, i8 }))
; CHECK: declare void @f.param.byval({ i8, i8 }* byval({ i8, i8 }))
declare void @f.param.inalloca(i8* inalloca(i8))
; CHECK: declare void @f.param.inalloca(i8* inalloca(i8))
declare void @f.param.sret(i8* sret(i8))
; CHECK: declare void @f.param.sret(i8* sret(i8))
declare void @f.param.noalias(i8* noalias)
; CHECK: declare void @f.param.noalias(i8* noalias)
declare void @f.param.nocapture(i8* nocapture)
; CHECK: declare void @f.param.nocapture(i8* nocapture)
declare void @f.param.nest(i8* nest)
; CHECK: declare void @f.param.nest(i8* nest)
declare i8* @f.param.returned(i8* returned)
; CHECK: declare i8* @f.param.returned(i8* returned)
declare void @f.param.nonnull(i8* nonnull)
; CHECK: declare void @f.param.nonnull(i8* nonnull)
declare void @f.param.dereferenceable(i8* dereferenceable(4))
; CHECK: declare void @f.param.dereferenceable(i8* dereferenceable(4))
declare void @f.param.dereferenceable_or_null(i8* dereferenceable_or_null(4))
; CHECK: declare void @f.param.dereferenceable_or_null(i8* dereferenceable_or_null(4))
declare void @f.param.stack_align([2 x double] alignstack(16))
; CHECK: declare void @f.param.stack_align([2 x double] alignstack(16))
declare void @f.param.swiftself(i8* swiftself)
; CHECK: declare void @f.param.swiftself(i8* swiftself)
declare void @f.param.swiftasync(i8* swiftasync)
; CHECK: declare void @f.param.swiftasync(i8* swiftasync)
declare void @f.param.swifterror(i8** swifterror)
; CHECK: declare void @f.param.swifterror(i8** swifterror)
declare void @f.param.allocalign(i32 allocalign)
; CHECK: declare void @f.param.allocalign(i32 allocalign)
declare void @f.param.allocptr(i32* allocptr)
; CHECK: declare void @f.param.allocptr(i32* allocptr)

; Functions -- unnamed_addr and local_unnamed_addr
declare void @f.unnamed_addr() unnamed_addr
; CHECK: declare void @f.unnamed_addr() unnamed_addr
declare void @f.local_unnamed_addr() local_unnamed_addr
; CHECK: declare void @f.local_unnamed_addr() local_unnamed_addr

; Functions -- fn Attrs (Function attributes)
declare void @f.alignstack4() alignstack(4)
; CHECK: declare void @f.alignstack4() #0
declare void @f.alignstack8() alignstack(8)
; CHECK: declare void @f.alignstack8() #1
declare void @f.alwaysinline() alwaysinline
; CHECK: declare void @f.alwaysinline() #2
declare void @f.cold() cold
; CHECK: declare void @f.cold() #3
declare void @f.convergent() convergent
; CHECK: declare void @f.convergent() #4
declare void @f.inlinehint() inlinehint
; CHECK: declare void @f.inlinehint() #5
declare void @f.jumptable() unnamed_addr jumptable
; CHECK: declare void @f.jumptable() unnamed_addr #6
declare void @f.minsize() minsize
; CHECK: declare void @f.minsize() #7
declare void @f.naked() naked
; CHECK: declare void @f.naked() #8
declare void @f.nobuiltin() nobuiltin
; CHECK: declare void @f.nobuiltin() #9
declare void @f.noduplicate() noduplicate
; CHECK: declare void @f.noduplicate() #10
declare void @f.noimplicitfloat() noimplicitfloat
; CHECK: declare void @f.noimplicitfloat() #11
declare void @f.noinline() noinline
; CHECK: declare void @f.noinline() #12
declare void @f.nonlazybind() nonlazybind
; CHECK: declare void @f.nonlazybind() #13
declare void @f.noredzone() noredzone
; CHECK: declare void @f.noredzone() #14
declare void @f.noreturn() noreturn
; CHECK: declare void @f.noreturn() #15
declare void @f.nounwind() nounwind
; CHECK: declare void @f.nounwind() #16
declare void @f.optnone() noinline optnone
; CHECK: declare void @f.optnone() #17
declare void @f.optsize() optsize
; CHECK: declare void @f.optsize() #18
declare void @f.readnone() readnone
; CHECK: declare void @f.readnone() #19
declare void @f.readonly() readonly
; CHECK: declare void @f.readonly() #20
declare void @f.returns_twice() returns_twice
; CHECK: declare void @f.returns_twice() #21
declare void @f.safestack() safestack
; CHECK: declare void @f.safestack() #22
declare void @f.sanitize_address() sanitize_address
; CHECK: declare void @f.sanitize_address() #23
declare void @f.sanitize_memory() sanitize_memory
; CHECK: declare void @f.sanitize_memory() #24
declare void @f.sanitize_thread() sanitize_thread
; CHECK: declare void @f.sanitize_thread() #25
declare void @f.ssp() ssp
; CHECK: declare void @f.ssp() #26
declare void @f.sspreq() sspreq
; CHECK: declare void @f.sspreq() #27
declare void @f.sspstrong() sspstrong
; CHECK: declare void @f.sspstrong() #28
declare void @f.thunk() "thunk"
; CHECK: declare void @f.thunk() #29
declare void @f.uwtable() uwtable
; CHECK: declare void @f.uwtable() #30
declare void @f.kvpair() "cpu"="cortex-a8"
; CHECK:declare void @f.kvpair() #31
declare void @f.norecurse() norecurse
; CHECK: declare void @f.norecurse() #32
declare void @f.inaccessiblememonly() inaccessiblememonly
; CHECK: declare void @f.inaccessiblememonly() #33
declare void @f.inaccessiblemem_or_argmemonly() inaccessiblemem_or_argmemonly
; CHECK: declare void @f.inaccessiblemem_or_argmemonly() #34
declare void @f.strictfp() #35

; Functions -- section
declare void @f.section() section "80"
; CHECK: declare void @f.section() section "80"

; Functions -- partition
define void @f.partition() partition "part" {
; CHECK: define void @f.partition() partition "part"
  ret void
}

; Functions -- comdat
define void @f.comdat_any() comdat($comdat.any) {
; CHECK: define void @f.comdat_any() comdat($comdat.any)
entry:
  ret void
}
define void @f.comdat_exactmatch() comdat($comdat.exactmatch) {
; CHECK: define void @f.comdat_exactmatch() comdat($comdat.exactmatch)
entry:
  ret void
}
define void @f.comdat_largest() comdat($comdat.largest) {
; CHECK: define void @f.comdat_largest() comdat($comdat.largest)
entry:
  ret void
}
define void @f.comdat_noduplicates() comdat($comdat.noduplicates) {
; CHECK: define void @f.comdat_noduplicates() comdat($comdat.noduplicates)
entry:
  ret void
}
define void @f.comdat_samesize() comdat($comdat.samesize) {
; CHECK: define void @f.comdat_samesize() comdat($comdat.samesize)
entry:
  ret void
}

; Functions -- align
declare void @f.align2() align 2
; CHECK: declare void @f.align2() align 2
declare void @f.align4() align 4
; CHECK: declare void @f.align4() align 4
declare void @f.align8() align 8
; CHECK: declare void @f.align8() align 8

; Functions -- GC
declare void @f.gcshadow() gc "shadow-stack"
; CHECK: declare void @f.gcshadow() gc "shadow-stack"

; Functions -- Prefix data
declare void @f.prefixi32() prefix i32 1684365668
; CHECK: declare void @f.prefixi32() prefix i32 1684365668
declare void @f.prefixarray() prefix [4 x i32] [i32 0, i32 1, i32 2, i32 3]
; CHECK: declare void @f.prefixarray() prefix [4 x i32] [i32 0, i32 1, i32 2, i32 3]

; Functions -- Prologue data
declare void @f.prologuei32() prologue i32 1684365669
; CHECK: declare void @f.prologuei32() prologue i32 1684365669
declare void @f.prologuearray() prologue [4 x i32] [i32 0, i32 1, i32 2, i32 3]
; CHECK: declare void @f.prologuearray() prologue [4 x i32] [i32 0, i32 1, i32 2, i32 3]

; Functions -- Personality constant
declare void @llvm.donothing() nounwind readnone
; CHECK: declare void @llvm.donothing() #35
define void @f.no_personality() personality i8 3 {
; CHECK: define void @f.no_personality() personality i8 3
  invoke void @llvm.donothing() to label %normal unwind label %exception
exception:
  %cleanup = landingpad i8 cleanup
  br label %normal
normal:
  ret void
}

declare i32 @f.personality_handler()
; CHECK: declare i32 @f.personality_handler()
define void @f.personality() personality i32 ()* @f.personality_handler {
; CHECK: define void @f.personality() personality i32 ()* @f.personality_handler
  invoke void @llvm.donothing() to label %normal unwind label %exception
exception:
  %cleanup = landingpad i32 cleanup
  br label %normal
normal:
  ret void
}

;; Atomic Memory Ordering Constraints
define void @atomics(i32* %word) {
  ;; Atomic Compare And Exchange w/o alignment
  %cmpxchg_no_align.0 = cmpxchg i32* %word, i32 0, i32 4 monotonic monotonic
  ; CHECK: %cmpxchg_no_align.0 = cmpxchg i32* %word, i32 0, i32 4 monotonic monotonic
  %cmpxchg_no_align.1 = cmpxchg i32* %word, i32 0, i32 5 acq_rel monotonic
  ; CHECK: %cmpxchg_no_align.1 = cmpxchg i32* %word, i32 0, i32 5 acq_rel monotonic
  %cmpxchg_no_align.2 = cmpxchg i32* %word, i32 0, i32 6 acquire monotonic
  ; CHECK: %cmpxchg_no_align.2 = cmpxchg i32* %word, i32 0, i32 6 acquire monotonic
  %cmpxchg_no_align.3 = cmpxchg i32* %word, i32 0, i32 7 release monotonic
  ; CHECK: %cmpxchg_no_align.3 = cmpxchg i32* %word, i32 0, i32 7 release monotonic
  %cmpxchg_no_align.4 = cmpxchg i32* %word, i32 0, i32 8 seq_cst monotonic
  ; CHECK: %cmpxchg_no_align.4 = cmpxchg i32* %word, i32 0, i32 8 seq_cst monotonic
  %cmpxchg_no_align.5 = cmpxchg weak i32* %word, i32 0, i32 9 seq_cst monotonic
  ; CHECK: %cmpxchg_no_align.5 = cmpxchg weak i32* %word, i32 0, i32 9 seq_cst monotonic
  %cmpxchg_no_align.6 = cmpxchg volatile i32* %word, i32 0, i32 10 seq_cst monotonic
  ; CHECK: %cmpxchg_no_align.6 = cmpxchg volatile i32* %word, i32 0, i32 10 seq_cst monotonic
  %cmpxchg_no_align.7 = cmpxchg weak volatile i32* %word, i32 0, i32 11 syncscope("singlethread") seq_cst monotonic
  ; CHECK: %cmpxchg_no_align.7 = cmpxchg weak volatile i32* %word, i32 0, i32 11 syncscope("singlethread") seq_cst monotonic

  ;; Atomic Compare And Exchange w/ alignment
  %cmpxchg.0 = cmpxchg i32* %word, i32 0, i32 4 monotonic monotonic, align 16
  ; CHECK: %cmpxchg.0 = cmpxchg i32* %word, i32 0, i32 4 monotonic monotonic, align 16
  %cmpxchg.1 = cmpxchg i32* %word, i32 0, i32 5 acq_rel monotonic, align 16
  ; CHECK: %cmpxchg.1 = cmpxchg i32* %word, i32 0, i32 5 acq_rel monotonic, align 16
  %cmpxchg.2 = cmpxchg i32* %word, i32 0, i32 6 acquire monotonic, align 16
  ; CHECK: %cmpxchg.2 = cmpxchg i32* %word, i32 0, i32 6 acquire monotonic, align 16
  %cmpxchg.3 = cmpxchg i32* %word, i32 0, i32 7 release monotonic, align 16
  ; CHECK: %cmpxchg.3 = cmpxchg i32* %word, i32 0, i32 7 release monotonic, align 16
  %cmpxchg.4 = cmpxchg i32* %word, i32 0, i32 8 seq_cst monotonic, align 16
  ; CHECK: %cmpxchg.4 = cmpxchg i32* %word, i32 0, i32 8 seq_cst monotonic, align 16
  %cmpxchg.5 = cmpxchg weak i32* %word, i32 0, i32 9 seq_cst monotonic, align 16
  ; CHECK: %cmpxchg.5 = cmpxchg weak i32* %word, i32 0, i32 9 seq_cst monotonic, align 16
  %cmpxchg.6 = cmpxchg volatile i32* %word, i32 0, i32 10 seq_cst monotonic, align 16
  ; CHECK: %cmpxchg.6 = cmpxchg volatile i32* %word, i32 0, i32 10 seq_cst monotonic, align 16
  %cmpxchg.7 = cmpxchg weak volatile i32* %word, i32 0, i32 11 syncscope("singlethread") seq_cst monotonic, align 16
  ; CHECK: %cmpxchg.7 = cmpxchg weak volatile i32* %word, i32 0, i32 11 syncscope("singlethread") seq_cst monotonic, align 16

  ;; Atomic w/o alignment
  %atomicrmw_no_align.xchg = atomicrmw xchg i32* %word, i32 12 monotonic
  ; CHECK: %atomicrmw_no_align.xchg = atomicrmw xchg i32* %word, i32 12 monotonic
  %atomicrmw_no_align.add = atomicrmw add i32* %word, i32 13 monotonic
  ; CHECK: %atomicrmw_no_align.add = atomicrmw add i32* %word, i32 13 monotonic
  %atomicrmw_no_align.sub = atomicrmw sub i32* %word, i32 14 monotonic
  ; CHECK: %atomicrmw_no_align.sub = atomicrmw sub i32* %word, i32 14 monotonic
  %atomicrmw_no_align.and = atomicrmw and i32* %word, i32 15 monotonic
  ; CHECK: %atomicrmw_no_align.and = atomicrmw and i32* %word, i32 15 monotonic
  %atomicrmw_no_align.nand = atomicrmw nand i32* %word, i32 16 monotonic
  ; CHECK: %atomicrmw_no_align.nand = atomicrmw nand i32* %word, i32 16 monotonic
  %atomicrmw_no_align.or = atomicrmw or i32* %word, i32 17 monotonic
  ; CHECK: %atomicrmw_no_align.or = atomicrmw or i32* %word, i32 17 monotonic
  %atomicrmw_no_align.xor = atomicrmw xor i32* %word, i32 18 monotonic
  ; CHECK: %atomicrmw_no_align.xor = atomicrmw xor i32* %word, i32 18 monotonic
  %atomicrmw_no_align.max = atomicrmw max i32* %word, i32 19 monotonic
  ; CHECK: %atomicrmw_no_align.max = atomicrmw max i32* %word, i32 19 monotonic
  %atomicrmw_no_align.min = atomicrmw volatile min i32* %word, i32 20 monotonic
  ; CHECK: %atomicrmw_no_align.min = atomicrmw volatile min i32* %word, i32 20 monotonic
  %atomicrmw_no_align.umax = atomicrmw umax i32* %word, i32 21 syncscope("singlethread") monotonic
  ; CHECK: %atomicrmw_no_align.umax = atomicrmw umax i32* %word, i32 21 syncscope("singlethread") monotonic
  %atomicrmw_no_align.umin = atomicrmw volatile umin i32* %word, i32 22 syncscope("singlethread") monotonic
  ; CHECK: %atomicrmw_no_align.umin = atomicrmw volatile umin i32* %word, i32 22 syncscope("singlethread") monotonic

  ;; Atomic w/ alignment
  %atomicrmw.xchg = atomicrmw xchg i32* %word, i32 12 monotonic, align 16
  ; CHECK: %atomicrmw.xchg = atomicrmw xchg i32* %word, i32 12 monotonic, align 16
  %atomicrmw.add = atomicrmw add i32* %word, i32 13 monotonic, align 16
  ; CHECK: %atomicrmw.add = atomicrmw add i32* %word, i32 13 monotonic, align 16
  %atomicrmw.sub = atomicrmw sub i32* %word, i32 14 monotonic, align 16
  ; CHECK: %atomicrmw.sub = atomicrmw sub i32* %word, i32 14 monotonic, align 16
  %atomicrmw.and = atomicrmw and i32* %word, i32 15 monotonic, align 16
  ; CHECK: %atomicrmw.and = atomicrmw and i32* %word, i32 15 monotonic, align 16
  %atomicrmw.nand = atomicrmw nand i32* %word, i32 16 monotonic, align 16
  ; CHECK: %atomicrmw.nand = atomicrmw nand i32* %word, i32 16 monotonic, align 16
  %atomicrmw.or = atomicrmw or i32* %word, i32 17 monotonic, align 16
  ; CHECK: %atomicrmw.or = atomicrmw or i32* %word, i32 17 monotonic, align 16
  %atomicrmw.xor = atomicrmw xor i32* %word, i32 18 monotonic, align 16
  ; CHECK: %atomicrmw.xor = atomicrmw xor i32* %word, i32 18 monotonic, align 16
  %atomicrmw.max = atomicrmw max i32* %word, i32 19 monotonic, align 16
  ; CHECK: %atomicrmw.max = atomicrmw max i32* %word, i32 19 monotonic, align 16
  %atomicrmw.min = atomicrmw volatile min i32* %word, i32 20 monotonic, align 16
  ; CHECK: %atomicrmw.min = atomicrmw volatile min i32* %word, i32 20 monotonic, align 16
  %atomicrmw.umax = atomicrmw umax i32* %word, i32 21 syncscope("singlethread") monotonic, align 16
  ; CHECK: %atomicrmw.umax = atomicrmw umax i32* %word, i32 21 syncscope("singlethread") monotonic, align 16
  %atomicrmw.umin = atomicrmw volatile umin i32* %word, i32 22 syncscope("singlethread") monotonic, align 16
  ; CHECK: %atomicrmw.umin = atomicrmw volatile umin i32* %word, i32 22 syncscope("singlethread") monotonic, align 16

  fence acquire
  ; CHECK: fence acquire
  fence release
  ; CHECK: fence release
  fence acq_rel
  ; CHECK: fence acq_rel
  fence syncscope("singlethread") seq_cst
  ; CHECK: fence syncscope("singlethread") seq_cst

  %ld.1 = load atomic i32, i32* %word monotonic, align 4
  ; CHECK: %ld.1 = load atomic i32, i32* %word monotonic, align 4
  %ld.2 = load atomic volatile i32, i32* %word acquire, align 8
  ; CHECK: %ld.2 = load atomic volatile i32, i32* %word acquire, align 8
  %ld.3 = load atomic volatile i32, i32* %word syncscope("singlethread") seq_cst, align 16
  ; CHECK: %ld.3 = load atomic volatile i32, i32* %word syncscope("singlethread") seq_cst, align 16

  store atomic i32 23, i32* %word monotonic, align 4
  ; CHECK: store atomic i32 23, i32* %word monotonic, align 4
  store atomic volatile i32 24, i32* %word monotonic, align 4
  ; CHECK: store atomic volatile i32 24, i32* %word monotonic, align 4
  store atomic volatile i32 25, i32* %word syncscope("singlethread") monotonic, align 4
  ; CHECK: store atomic volatile i32 25, i32* %word syncscope("singlethread") monotonic, align 4
  ret void
}

define void @fp_atomics(float* %word) {
; CHECK: %atomicrmw.xchg = atomicrmw xchg float* %word, float 1.000000e+00 monotonic
  %atomicrmw.xchg = atomicrmw xchg float* %word, float 1.0 monotonic

; CHECK: %atomicrmw.fadd = atomicrmw fadd float* %word, float 1.000000e+00 monotonic
  %atomicrmw.fadd = atomicrmw fadd float* %word, float 1.0 monotonic

; CHECK: %atomicrmw.fsub = atomicrmw fsub float* %word, float 1.000000e+00 monotonic
  %atomicrmw.fsub = atomicrmw fsub float* %word, float 1.0 monotonic

; CHECK: %atomicrmw.fmax = atomicrmw fmax float* %word, float 1.000000e+00 monotonic
  %atomicrmw.fmax = atomicrmw fmax float* %word, float 1.0 monotonic

; CHECK: %atomicrmw.fmin = atomicrmw fmin float* %word, float 1.000000e+00 monotonic
  %atomicrmw.fmin = atomicrmw fmin float* %word, float 1.0 monotonic

  ret void
}

define void @pointer_atomics(i8** %word) {
; CHECK: %atomicrmw.xchg = atomicrmw xchg i8** %word, i8* null monotonic
  %atomicrmw.xchg = atomicrmw xchg i8** %word, i8* null monotonic
  ret void
}

;; Fast Math Flags
define void @fastmathflags_unop(float %op1) {
  %f.nnan = fneg nnan float %op1
  ; CHECK: %f.nnan = fneg nnan float %op1
  %f.ninf = fneg ninf float %op1
  ; CHECK: %f.ninf = fneg ninf float %op1
  %f.nsz = fneg nsz float %op1
  ; CHECK: %f.nsz = fneg nsz float %op1
  %f.arcp = fneg arcp float %op1
  ; CHECK: %f.arcp = fneg arcp float %op1
  %f.contract = fneg contract float %op1
  ; CHECK: %f.contract = fneg contract float %op1
  %f.afn = fneg afn float %op1
  ; CHECK: %f.afn = fneg afn float %op1
  %f.reassoc = fneg reassoc float %op1
  ; CHECK: %f.reassoc = fneg reassoc float %op1
  %f.fast = fneg fast float %op1
  ; CHECK: %f.fast = fneg fast float %op1
  ret void
}

define void @fastmathflags_binops(float %op1, float %op2) {
  %f.nnan = fadd nnan float %op1, %op2
  ; CHECK: %f.nnan = fadd nnan float %op1, %op2
  %f.ninf = fadd ninf float %op1, %op2
  ; CHECK: %f.ninf = fadd ninf float %op1, %op2
  %f.nsz = fadd nsz float %op1, %op2
  ; CHECK: %f.nsz = fadd nsz float %op1, %op2
  %f.arcp = fadd arcp float %op1, %op2
  ; CHECK: %f.arcp = fadd arcp float %op1, %op2
  %f.contract = fadd contract float %op1, %op2
  ; CHECK: %f.contract = fadd contract float %op1, %op2
  %f.afn = fadd afn float %op1, %op2
  ; CHECK: %f.afn = fadd afn float %op1, %op2
  %f.reassoc = fadd reassoc float %op1, %op2
  ; CHECK: %f.reassoc = fadd reassoc float %op1, %op2
  %f.fast = fadd fast float %op1, %op2
  ; CHECK: %f.fast = fadd fast float %op1, %op2
  ret void
}

define void @fastmathflags_select(i1 %cond, float %op1, float %op2) {
  %f.nnan = select nnan i1 %cond, float %op1, float %op2
  ; CHECK: %f.nnan = select nnan i1 %cond, float %op1, float %op2
  %f.ninf = select ninf i1 %cond, float %op1, float %op2
  ; CHECK: %f.ninf = select ninf i1 %cond, float %op1, float %op2
  %f.nsz = select nsz i1 %cond, float %op1, float %op2
  ; CHECK: %f.nsz = select nsz i1 %cond, float %op1, float %op2
  %f.arcp = select arcp i1 %cond, float %op1, float %op2
  ; CHECK: %f.arcp = select arcp i1 %cond, float %op1, float %op2
  %f.contract = select contract i1 %cond, float %op1, float %op2
  ; CHECK: %f.contract = select contract i1 %cond, float %op1, float %op2
  %f.afn = select afn i1 %cond, float %op1, float %op2
  ; CHECK: %f.afn = select afn i1 %cond, float %op1, float %op2
  %f.reassoc = select reassoc i1 %cond, float %op1, float %op2
  ; CHECK: %f.reassoc = select reassoc i1 %cond, float %op1, float %op2
  %f.fast = select fast i1 %cond, float %op1, float %op2
  ; CHECK: %f.fast = select fast i1 %cond, float %op1, float %op2
  ret void
}

define void @fastmathflags_vector_select(<2 x i1> %cond, <2 x double> %op1, <2 x double> %op2) {
  %f.nnan.nsz = select nnan nsz <2 x i1> %cond, <2 x double> %op1, <2 x double> %op2
  ; CHECK: %f.nnan.nsz = select nnan nsz <2 x i1> %cond, <2 x double> %op1, <2 x double> %op2
  %f.fast = select fast <2 x i1> %cond, <2 x double> %op1, <2 x double> %op2
  ; CHECK: %f.fast = select fast <2 x i1> %cond, <2 x double> %op1, <2 x double> %op2
  ret void
}

define void @fastmathflags_array_select(i1 %cond, [2 x double] %op1, [2 x double] %op2) {
  %f.nnan.nsz = select nnan nsz i1 %cond, [2 x double] %op1, [2 x double] %op2
  ; CHECK: %f.nnan.nsz = select nnan nsz i1 %cond, [2 x double] %op1, [2 x double] %op2
  %f.fast = select fast i1 %cond, [2 x double] %op1, [2 x double] %op2
  ; CHECK: %f.fast = select fast i1 %cond, [2 x double] %op1, [2 x double] %op2
  ret void
}

define void @fastmathflags_phi(i1 %cond, float %f1, float %f2, double %d1, double %d2, half %h1, half %h2) {
entry:
  br i1 %cond, label %L1, label %L2
L1:
  br label %exit
L2:
  br label %exit
exit:
  %p.nnan = phi nnan float [ %f1, %L1 ], [ %f2, %L2 ]
  ; CHECK: %p.nnan = phi nnan float [ %f1, %L1 ], [ %f2, %L2 ]
  %p.ninf = phi ninf double [ %d1, %L1 ], [ %d2, %L2 ]
  ; CHECK: %p.ninf = phi ninf double [ %d1, %L1 ], [ %d2, %L2 ]
  %p.contract = phi contract half [ %h1, %L1 ], [ %h2, %L2 ]
  ; CHECK: %p.contract = phi contract half [ %h1, %L1 ], [ %h2, %L2 ]
  %p.nsz.reassoc = phi reassoc nsz float [ %f1, %L1 ], [ %f2, %L2 ]
  ; CHECK: %p.nsz.reassoc = phi reassoc nsz float [ %f1, %L1 ], [ %f2, %L2 ]
  %p.fast = phi fast half [ %h2, %L1 ], [ %h1, %L2 ]
  ; CHECK: %p.fast = phi fast half [ %h2, %L1 ], [ %h1, %L2 ]
  ret void
}

define void @fastmathflags_vector_phi(i1 %cond, <4 x float> %f1, <4 x float> %f2, <2 x double> %d1, <2 x double> %d2, <8 x half> %h1, <8 x half> %h2) {
entry:
  br i1 %cond, label %L1, label %L2
L1:
  br label %exit
L2:
  br label %exit
exit:
  %p.nnan = phi nnan <4 x float> [ %f1, %L1 ], [ %f2, %L2 ]
  ; CHECK: %p.nnan = phi nnan <4 x float> [ %f1, %L1 ], [ %f2, %L2 ]
  %p.ninf = phi ninf <2 x double> [ %d1, %L1 ], [ %d2, %L2 ]
  ; CHECK: %p.ninf = phi ninf <2 x double> [ %d1, %L1 ], [ %d2, %L2 ]
  %p.contract = phi contract <8 x half> [ %h1, %L1 ], [ %h2, %L2 ]
  ; CHECK: %p.contract = phi contract <8 x half> [ %h1, %L1 ], [ %h2, %L2 ]
  %p.nsz.reassoc = phi reassoc nsz <4 x float> [ %f1, %L1 ], [ %f2, %L2 ]
  ; CHECK: %p.nsz.reassoc = phi reassoc nsz <4 x float> [ %f1, %L1 ], [ %f2, %L2 ]
  %p.fast = phi fast <8 x half> [ %h2, %L1 ], [ %h1, %L2 ]
  ; CHECK: %p.fast = phi fast <8 x half> [ %h2, %L1 ], [ %h1, %L2 ]
  ret void
}

define void @fastmathflags_array_phi(i1 %cond, [4 x float] %f1, [4 x float] %f2, [2 x double] %d1, [2 x double] %d2, [8 x half] %h1, [8 x half] %h2) {
entry:
  br i1 %cond, label %L1, label %L2
L1:
  br label %exit
L2:
  br label %exit
exit:
  %p.nnan = phi nnan [4 x float] [ %f1, %L1 ], [ %f2, %L2 ]
  ; CHECK: %p.nnan = phi nnan [4 x float] [ %f1, %L1 ], [ %f2, %L2 ]
  %p.ninf = phi ninf [2 x double] [ %d1, %L1 ], [ %d2, %L2 ]
  ; CHECK: %p.ninf = phi ninf [2 x double] [ %d1, %L1 ], [ %d2, %L2 ]
  %p.contract = phi contract [8 x half] [ %h1, %L1 ], [ %h2, %L2 ]
  ; CHECK: %p.contract = phi contract [8 x half] [ %h1, %L1 ], [ %h2, %L2 ]
  %p.nsz.reassoc = phi reassoc nsz [4 x float] [ %f1, %L1 ], [ %f2, %L2 ]
  ; CHECK: %p.nsz.reassoc = phi reassoc nsz [4 x float] [ %f1, %L1 ], [ %f2, %L2 ]
  %p.fast = phi fast [8 x half] [ %h2, %L1 ], [ %h1, %L2 ]
  ; CHECK: %p.fast = phi fast [8 x half] [ %h2, %L1 ], [ %h1, %L2 ]
  ret void
}

; Check various fast math flags and floating-point types on calls.

declare float @fmf_f32()
declare double @fmf_f64()
declare <4 x double> @fmf_v4f64()

; CHECK-LABEL: fastMathFlagsForCalls(
define void @fastMathFlagsForCalls(float %f, double %d1, <4 x double> %d2) {
  %call.fast = call fast float @fmf_f32()
  ; CHECK: %call.fast = call fast float @fmf_f32()

  ; Throw in some other attributes to make sure those stay in the right places.

  %call.nsz.arcp = notail call nsz arcp double @fmf_f64()
  ; CHECK: %call.nsz.arcp = notail call nsz arcp double @fmf_f64()

  %call.nnan.ninf = tail call nnan ninf fastcc <4 x double> @fmf_v4f64()
  ; CHECK: %call.nnan.ninf = tail call nnan ninf fastcc <4 x double> @fmf_v4f64()

  ret void
}

declare [2 x float] @fmf_a2f32()
declare [2 x double] @fmf_a2f64()
declare [2 x <4 x double>] @fmf_a2v4f64()

; CHECK-LABEL: fastMathFlagsForArrayCalls(
define void @fastMathFlagsForArrayCalls([2 x float] %f, [2 x double] %d1, [2 x <4 x double>] %d2) {
  %call.fast = call fast [2 x float] @fmf_a2f32()
  ; CHECK: %call.fast = call fast [2 x float] @fmf_a2f32()

  ; Throw in some other attributes to make sure those stay in the right places.

  %call.nsz.arcp = notail call nsz arcp [2 x double] @fmf_a2f64()
  ; CHECK: %call.nsz.arcp = notail call nsz arcp [2 x double] @fmf_a2f64()

  %call.nnan.ninf = tail call nnan ninf fastcc [2 x <4 x double>] @fmf_a2v4f64()
  ; CHECK: %call.nnan.ninf = tail call nnan ninf fastcc [2 x <4 x double>] @fmf_a2v4f64()

  ret void
}

;; Type System
%opaquety = type opaque
define void @typesystem() {
  %p0 = bitcast i8* null to i32 (i32)*
  ; CHECK: %p0 = bitcast i8* null to i32 (i32)*
  %p1 = bitcast i8* null to void (i8*)*
  ; CHECK: %p1 = bitcast i8* null to void (i8*)*
  %p2 = bitcast i8* null to i32 (i8*, ...)*
  ; CHECK: %p2 = bitcast i8* null to i32 (i8*, ...)*
  %p3 = bitcast i8* null to { i32, i8 } (i8*, ...)*
  ; CHECK: %p3 = bitcast i8* null to { i32, i8 } (i8*, ...)*
  %p4 = bitcast i8* null to <{ i32, i8 }> (i8*, ...)*
  ; CHECK: %p4 = bitcast i8* null to <{ i32, i8 }> (i8*, ...)*
  %p5 = bitcast i8* null to <{ i32, i8 }> (<{ i8*, i64 }>*, ...)*
  ; CHECK: %p5 = bitcast i8* null to <{ i32, i8 }> (<{ i8*, i64 }>*, ...)*

  %t0 = alloca i1942652
  ; CHECK: %t0 = alloca i1942652
  %t1 = alloca half
  ; CHECK: %t1 = alloca half
  %t2 = alloca float
  ; CHECK: %t2 = alloca float
  %t3 = alloca double
  ; CHECK: %t3 = alloca double
  %t4 = alloca fp128
  ; CHECK: %t4 = alloca fp128
  %t5 = alloca x86_fp80
  ; CHECK: %t5 = alloca x86_fp80
  %t6 = alloca ppc_fp128
  ; CHECK: %t6 = alloca ppc_fp128
  %t7 = alloca x86_mmx
  ; CHECK: %t7 = alloca x86_mmx
  %t8 = alloca %opaquety*
  ; CHECK: %t8 = alloca %opaquety*
  %t9 = alloca <4 x i32>
  ; CHECK: %t9 = alloca <4 x i32>
  %t10 = alloca <vscale x 4 x i32>
  ; CHECK: %t10 = alloca <vscale x 4 x i32>

  ret void
}

declare void @llvm.token(token)
; CHECK: declare void @llvm.token(token)

;; Inline Assembler Expressions
define void @inlineasm(i32 %arg) {
  call i32 asm "bswap $0", "=r,r"(i32 %arg)
  ; CHECK: call i32 asm "bswap $0", "=r,r"(i32 %arg)
  call i32 asm sideeffect "blt $1, $2, $3", "=r,r,rm"(i32 %arg, i32 %arg)
  ; CHECK: call i32 asm sideeffect "blt $1, $2, $3", "=r,r,rm"(i32 %arg, i32 %arg)
  ret void
}

;; Instructions

; Instructions -- Terminators
define void @instructions.terminators(i8 %val) personality i32 -10 {
  br i1 false, label %iftrue, label %iffalse
  ; CHECK: br i1 false, label %iftrue, label %iffalse
  br label %iftrue
  ; CHECK: br label %iftrue
iftrue:
  ret void
  ; CHECK: ret void
iffalse:

  switch i8 %val, label %defaultdest [
  ; CHECK: switch i8 %val, label %defaultdest [
         i8 0, label %defaultdest.0
         ; CHECK: i8 0, label %defaultdest.0
         i8 1, label %defaultdest.1
         ; CHECK: i8 1, label %defaultdest.1
         i8 2, label %defaultdest.2
         ; CHECK: i8 2, label %defaultdest.2
  ]
  ; CHECK: ]
defaultdest:
  ret void
defaultdest.0:
  ret void
defaultdest.1:
  ret void
defaultdest.2:

  indirectbr i8* blockaddress(@instructions.terminators, %defaultdest.2), [label %defaultdest.2]
  ; CHECK: indirectbr i8* blockaddress(@instructions.terminators, %defaultdest.2), [label %defaultdest.2]
  indirectbr i8* blockaddress(@instructions.terminators, %defaultdest.2), [label %defaultdest.2, label %defaultdest.2]
  ; CHECK: indirectbr i8* blockaddress(@instructions.terminators, %defaultdest.2), [label %defaultdest.2, label %defaultdest.2]

  invoke fastcc void @f.fastcc()
  ; CHECK: invoke fastcc void @f.fastcc()
         to label %defaultdest unwind label %exc
         ; CHECK: to label %defaultdest unwind label %exc
exc:
  %cleanup = landingpad i32 cleanup

  resume i32 undef
  ; CHECK: resume i32 undef
  resume i32 poison
  ; CHECK: resume i32 poison
  unreachable
  ; CHECK: unreachable

  ret void
}

define i32 @instructions.win_eh.1() personality i32 -3 {
entry:
  %arg1 = alloca i32
  %arg2 = alloca i32
  invoke void @f.ccc() to label %normal unwind label %catchswitch1
  invoke void @f.ccc() to label %normal unwind label %catchswitch2
  invoke void @f.ccc() to label %normal unwind label %catchswitch3

catchswitch1:
  %cs1 = catchswitch within none [label %catchpad1] unwind to caller

catchpad1:
  catchpad within %cs1 []
  br label %normal
  ; CHECK: catchpad within %cs1 []
  ; CHECK-NEXT: br label %normal

catchswitch2:
  %cs2 = catchswitch within none [label %catchpad2] unwind to caller

catchpad2:
  catchpad within %cs2 [i32* %arg1]
  br label %normal
  ; CHECK: catchpad within %cs2 [i32* %arg1]
  ; CHECK-NEXT: br label %normal

catchswitch3:
  %cs3 = catchswitch within none [label %catchpad3] unwind label %cleanuppad1

catchpad3:
  catchpad within %cs3 [i32* %arg1, i32* %arg2]
  br label %normal
  ; CHECK: catchpad within %cs3 [i32* %arg1, i32* %arg2]
  ; CHECK-NEXT: br label %normal

cleanuppad1:
  %clean.1 = cleanuppad within none []
  unreachable
  ; CHECK: %clean.1 = cleanuppad within none []
  ; CHECK-NEXT: unreachable

normal:
  ret i32 0
}
;
define i32 @instructions.win_eh.2() personality i32 -4 {
entry:
  invoke void @f.ccc() to label %invoke.cont unwind label %catchswitch

invoke.cont:
  invoke void @f.ccc() to label %continue unwind label %cleanup

cleanup:
  %clean = cleanuppad within none []
  ; CHECK: %clean = cleanuppad within none []
  cleanupret from %clean unwind to caller
  ; CHECK: cleanupret from %clean unwind to caller

catchswitch:
  %cs = catchswitch within none [label %catchpad] unwind label %terminate

catchpad:
  %catch = catchpad within %cs []
  br label %body
  ; CHECK: %catch = catchpad within %cs []
  ; CHECK-NEXT: br label %body

body:
  invoke void @f.ccc() [ "funclet"(token %catch) ]
    to label %continue unwind label %terminate.inner
  catchret from %catch to label %return
  ; CHECK: catchret from %catch to label %return

return:
  ret i32 0

terminate.inner:
  cleanuppad within %catch []
  unreachable
  ; CHECK: cleanuppad within %catch []
  ; CHECK-NEXT: unreachable

terminate:
  cleanuppad within none []
  unreachable
  ; CHECK: cleanuppad within none []
  ; CHECK-NEXT: unreachable

continue:
  ret i32 0
}

; Instructions -- Unary Operations
define void @instructions.unops(double %op1) {
  fneg double %op1
  ; CHECK: fneg double %op1
  ret void
}

; Instructions -- Binary Operations
define void @instructions.binops(i8 %op1, i8 %op2) {
  ; nuw x nsw
  add i8 %op1, %op2
  ; CHECK: add i8 %op1, %op2
  add nuw i8 %op1, %op2
  ; CHECK: add nuw i8 %op1, %op2
  add nsw i8 %op1, %op2
  ; CHECK: add nsw i8 %op1, %op2
  add nuw nsw i8 %op1, %op2
  ; CHECK: add nuw nsw i8 %op1, %op2
  sub i8 %op1, %op2
  ; CHECK: sub i8 %op1, %op2
  sub nuw i8 %op1, %op2
  ; CHECK: sub nuw i8 %op1, %op2
  sub nsw i8 %op1, %op2
  ; CHECK: sub nsw i8 %op1, %op2
  sub nuw nsw i8 %op1, %op2
  ; CHECK: sub nuw nsw i8 %op1, %op2
  mul i8 %op1, %op2
  ; CHECK: mul i8 %op1, %op2
  mul nuw i8 %op1, %op2
  ; CHECK: mul nuw i8 %op1, %op2
  mul nsw i8 %op1, %op2
  ; CHECK: mul nsw i8 %op1, %op2
  mul nuw nsw i8 %op1, %op2
  ; CHECK: mul nuw nsw i8 %op1, %op2

  ; exact
  udiv i8 %op1, %op2
  ; CHECK: udiv i8 %op1, %op2
  udiv exact i8 %op1, %op2
  ; CHECK: udiv exact i8 %op1, %op2
  sdiv i8 %op1, %op2
  ; CHECK: sdiv i8 %op1, %op2
  sdiv exact i8 %op1, %op2
  ; CHECK: sdiv exact i8 %op1, %op2

  ; none
  urem i8 %op1, %op2
  ; CHECK: urem i8 %op1, %op2
  srem i8 %op1, %op2
  ; CHECK: srem i8 %op1, %op2

  ret void
}

; Instructions -- Bitwise Binary Operations
define void @instructions.bitwise_binops(i8 %op1, i8 %op2) {
  ; nuw x nsw
  shl i8 %op1, %op2
  ; CHECK: shl i8 %op1, %op2
  shl nuw i8 %op1, %op2
  ; CHECK: shl nuw i8 %op1, %op2
  shl nsw i8 %op1, %op2
  ; CHECK: shl nsw i8 %op1, %op2
  shl nuw nsw i8 %op1, %op2
  ; CHECK: shl nuw nsw i8 %op1, %op2

  ; exact
  lshr i8 %op1, %op2
  ; CHECK: lshr i8 %op1, %op2
  lshr exact i8 %op1, %op2
  ; CHECK: lshr exact i8 %op1, %op2
  ashr i8 %op1, %op2
  ; CHECK: ashr i8 %op1, %op2
  ashr exact i8 %op1, %op2
  ; CHECK: ashr exact i8 %op1, %op2

  ; none
  and i8 %op1, %op2
  ; CHECK: and i8 %op1, %op2
  or i8 %op1, %op2
  ; CHECK: or i8 %op1, %op2
  xor i8 %op1, %op2
  ; CHECK: xor i8 %op1, %op2

  ret void
}

; Instructions -- Vector Operations
define void @instructions.vectorops(<4 x float> %vec, <4 x float> %vec2) {
  extractelement <4 x float> %vec, i8 0
  ; CHECK: extractelement <4 x float> %vec, i8 0
  insertelement <4 x float> %vec, float 3.500000e+00, i8 0
  ; CHECK: insertelement <4 x float> %vec, float 3.500000e+00, i8 0
  shufflevector <4 x float> %vec, <4 x float> %vec2, <2 x i32> zeroinitializer
  ; CHECK: shufflevector <4 x float> %vec, <4 x float> %vec2, <2 x i32> zeroinitializer

  ret void
}

; Instructions -- Aggregate Operations
define void @instructions.aggregateops({ i8, i32 } %up, <{ i8, i32 }> %p,
                                       [3 x i8] %arr, { i8, { i32 }} %n,
                                       <2 x i8*> %pvec, <2 x i64> %offsets) {
  extractvalue { i8, i32 } %up, 0
  ; CHECK: extractvalue { i8, i32 } %up, 0
  extractvalue <{ i8, i32 }> %p, 1
  ; CHECK: extractvalue <{ i8, i32 }> %p, 1
  extractvalue [3 x i8] %arr, 2
  ; CHECK: extractvalue [3 x i8] %arr, 2
  extractvalue { i8, { i32 } } %n, 1, 0
  ; CHECK: extractvalue { i8, { i32 } } %n, 1, 0

  insertvalue { i8, i32 } %up, i8 1, 0
  ; CHECK: insertvalue { i8, i32 } %up, i8 1, 0
  insertvalue <{ i8, i32 }> %p, i32 2, 1
  ; CHECK: insertvalue <{ i8, i32 }> %p, i32 2, 1
  insertvalue [3 x i8] %arr, i8 0, 0
  ; CHECK: insertvalue [3 x i8] %arr, i8 0, 0
  insertvalue { i8, { i32 } } %n, i32 0, 1, 0
  ; CHECK: insertvalue { i8, { i32 } } %n, i32 0, 1, 0

  %up.ptr = alloca { i8, i32 }
  %p.ptr = alloca <{ i8, i32 }>
  %arr.ptr = alloca [3 x i8]
  %n.ptr = alloca { i8, { i32 } }

  getelementptr { i8, i32 }, { i8, i32 }* %up.ptr, i8 0
  ; CHECK: getelementptr { i8, i32 }, { i8, i32 }* %up.ptr, i8 0
  getelementptr <{ i8, i32 }>, <{ i8, i32 }>* %p.ptr, i8 1
  ; CHECK: getelementptr <{ i8, i32 }>, <{ i8, i32 }>* %p.ptr, i8 1
  getelementptr [3 x i8], [3 x i8]* %arr.ptr, i8 2
  ; CHECK: getelementptr [3 x i8], [3 x i8]* %arr.ptr, i8 2
  getelementptr { i8, { i32 } }, { i8, { i32 } }* %n.ptr, i32 0, i32 1
  ; CHECK: getelementptr { i8, { i32 } }, { i8, { i32 } }* %n.ptr, i32 0, i32 1
  getelementptr inbounds { i8, { i32 } }, { i8, { i32 } }* %n.ptr, i32 1, i32 0
  ; CHECK: getelementptr inbounds { i8, { i32 } }, { i8, { i32 } }* %n.ptr, i32 1, i32 0
  getelementptr i8, <2 x i8*> %pvec, <2 x i64> %offsets
  ; CHECK: getelementptr i8, <2 x i8*> %pvec, <2 x i64> %offsets

  ret void
}

; Instructions -- Memory Access and Addressing Operations
!7 = !{i32 1}
!8 = !{}
!9 = !{i64 4}
define void @instructions.memops(i32** %base) {
  alloca i32, i8 4, align 4
  ; CHECK: alloca i32, i8 4, align 4
  alloca inalloca i32, i8 4, align 4
  ; CHECK: alloca inalloca i32, i8 4, align 4

  load i32*, i32** %base, align 8, !invariant.load !7, !nontemporal !8, !nonnull !7, !dereferenceable !9, !dereferenceable_or_null !9
  ; CHECK: load i32*, i32** %base, align 8, !invariant.load !7, !nontemporal !8, !nonnull !7, !dereferenceable !9, !dereferenceable_or_null !9
  load volatile i32*, i32** %base, align 8, !invariant.load !7, !nontemporal !8, !nonnull !7, !dereferenceable !9, !dereferenceable_or_null !9
  ; CHECK: load volatile i32*, i32** %base, align 8, !invariant.load !7, !nontemporal !8, !nonnull !7, !dereferenceable !9, !dereferenceable_or_null !9

  store i32* null, i32** %base, align 4, !nontemporal !8
  ; CHECK: store i32* null, i32** %base, align 4, !nontemporal !8
  store volatile i32* null, i32** %base, align 4, !nontemporal !8
  ; CHECK: store volatile i32* null, i32** %base, align 4, !nontemporal !8

  ret void
}

; Instructions -- Conversion Operations
define void @instructions.conversions() {
  trunc i32 -1 to i1
  ; CHECK: trunc i32 -1 to i1
  zext i32 -1 to i64
  ; CHECK: zext i32 -1 to i64
  sext i32 -1 to i64
  ; CHECK: sext i32 -1 to i64
  fptrunc float undef to half
  ; CHECK: fptrunc float undef to half
  fpext half undef to float
  ; CHECK: fpext half undef to float
  fptoui float undef to i32
  ; CHECK: fptoui float undef to i32
  fptosi float undef to i32
  ; CHECK: fptosi float undef to i32
  fptrunc float poison to half
  ; CHECK: fptrunc float poison to half
  fpext half poison to float
  ; CHECK: fpext half poison to float
  fptoui float poison to i32
  ; CHECK: fptoui float poison to i32
  fptosi float poison to i32
  ; CHECK: fptosi float poison to i32
  uitofp i32 1 to float
  ; CHECK: uitofp i32 1 to float
  sitofp i32 -1 to float
  ; CHECK: sitofp i32 -1 to float
  ptrtoint i8* null to i64
  ; CHECK: ptrtoint i8* null to i64
  inttoptr i64 0 to i8*
  ; CHECK: inttoptr i64 0 to i8*
  bitcast i32 0 to i32
  ; CHECK: bitcast i32 0 to i32
  addrspacecast i32* null to i32 addrspace(1)*
  ; CHECK: addrspacecast i32* null to i32 addrspace(1)*

  ret void
}

; Instructions -- Other Operations
define void @instructions.other(i32 %op1, i32 %op2, half %fop1, half %fop2, <2 x i32> %vop, i8* %pop) {
entry:
  icmp eq  i32 %op1, %op2
  ; CHECK: icmp eq  i32 %op1, %op2
  icmp ne  i32 %op1, %op2
  ; CHECK: icmp ne  i32 %op1, %op2
  icmp ugt i32 %op1, %op2
  ; CHECK: icmp ugt i32 %op1, %op2
  icmp uge i32 %op1, %op2
  ; CHECK: icmp uge i32 %op1, %op2
  icmp ult i32 %op1, %op2
  ; CHECK: icmp ult i32 %op1, %op2
  icmp ule i32 %op1, %op2
  ; CHECK: icmp ule i32 %op1, %op2
  icmp sgt i32 %op1, %op2
  ; CHECK: icmp sgt i32 %op1, %op2
  icmp sge i32 %op1, %op2
  ; CHECK: icmp sge i32 %op1, %op2
  icmp slt i32 %op1, %op2
  ; CHECK: icmp slt i32 %op1, %op2
  icmp sle i32 %op1, %op2
  ; CHECK: icmp sle i32 %op1, %op2

  fcmp false half %fop1, %fop2
  ; CHECK: fcmp false half %fop1, %fop2
  fcmp oeq   half %fop1, %fop2
  ; CHECK: fcmp oeq   half %fop1, %fop2
  fcmp ogt   half %fop1, %fop2
  ; CHECK: fcmp ogt   half %fop1, %fop2
  fcmp oge   half %fop1, %fop2
  ; CHECK: fcmp oge   half %fop1, %fop2
  fcmp olt   half %fop1, %fop2
  ; CHECK: fcmp olt   half %fop1, %fop2
  fcmp ole   half %fop1, %fop2
  ; CHECK: fcmp ole   half %fop1, %fop2
  fcmp one   half %fop1, %fop2
  ; CHECK: fcmp one   half %fop1, %fop2
  fcmp ord   half %fop1, %fop2
  ; CHECK: fcmp ord   half %fop1, %fop2
  fcmp ueq   half %fop1, %fop2
  ; CHECK: fcmp ueq   half %fop1, %fop2
  fcmp ugt   half %fop1, %fop2
  ; CHECK: fcmp ugt   half %fop1, %fop2
  fcmp uge   half %fop1, %fop2
  ; CHECK: fcmp uge   half %fop1, %fop2
  fcmp ult   half %fop1, %fop2
  ; CHECK: fcmp ult   half %fop1, %fop2
  fcmp ule   half %fop1, %fop2
  ; CHECK: fcmp ule   half %fop1, %fop2
  fcmp une   half %fop1, %fop2
  ; CHECK: fcmp une   half %fop1, %fop2
  fcmp uno   half %fop1, %fop2
  ; CHECK: fcmp uno   half %fop1, %fop2
  fcmp true  half %fop1, %fop2
  ; CHECK: fcmp true  half %fop1, %fop2

  br label %exit
L1:
  %v1 = add i32 %op1, %op2
  br label %exit
L2:
  %v2 = add i32 %op1, %op2
  br label %exit
exit:
  phi i32 [ %v1, %L1 ], [ %v2, %L2 ], [ %op1, %entry ]
  ; CHECK: phi i32 [ %v1, %L1 ], [ %v2, %L2 ], [ %op1, %entry ]

  select i1 true, i32 0, i32 1
  ; CHECK: select i1 true, i32 0, i32 1
  select <2 x i1> <i1 true, i1 false>, <2 x i8> <i8 2, i8 3>, <2 x i8> <i8 3, i8 2>
  ; CHECK: select <2 x i1> <i1 true, i1 false>, <2 x i8> <i8 2, i8 3>, <2 x i8> <i8 3, i8 2>

  call void @f.nobuiltin() builtin
  ; CHECK: call void @f.nobuiltin() #50

  call fastcc noalias i32* @f.noalias() noinline
  ; CHECK: call fastcc noalias i32* @f.noalias() #12
  tail call ghccc nonnull i32* @f.nonnull() minsize
  ; CHECK: tail call ghccc nonnull i32* @f.nonnull() #7

  freeze i32 %op1
  ; CHECK: freeze i32 %op1
  freeze i32 10
  ; CHECK: freeze i32 10
  freeze half %fop1
  ; CHECK: freeze half %fop1
  freeze <2 x i32> %vop
  ; CHECK: freeze <2 x i32> %vop
  freeze i8* %pop
  ; CHECK: freeze i8* %pop
  ret void
}

define void @instructions.call_musttail(i8* inalloca(i8) %val) {
  musttail call void @f.param.inalloca(i8* inalloca(i8) %val)
  ; CHECK: musttail call void @f.param.inalloca(i8* inalloca(i8) %val)

  ret void
}

define void @instructions.call_notail() {
  notail call void @f1()
  ; CHECK: notail call void @f1()

  ret void
}

define void @instructions.landingpad() personality i32 -2 {
  invoke void @llvm.donothing() to label %proceed unwind label %catch1
  invoke void @llvm.donothing() to label %proceed unwind label %catch2
  invoke void @llvm.donothing() to label %proceed unwind label %catch3
  invoke void @llvm.donothing() to label %proceed unwind label %catch4

catch1:
  landingpad i32
  ; CHECK: landingpad i32
             cleanup
             ; CHECK: cleanup
  br label %proceed

catch2:
  landingpad i32
  ; CHECK: landingpad i32
             cleanup
             ; CHECK: cleanup
             catch i32* null
             ; CHECK: catch i32* null
  br label %proceed

catch3:
  landingpad i32
  ; CHECK: landingpad i32
             cleanup
             ; CHECK: cleanup
             catch i32* null
             ; CHECK: catch i32* null
             catch i32* null
             ; CHECK: catch i32* null
  br label %proceed

catch4:
  landingpad i32
  ; CHECK: landingpad i32
             filter [2 x i32] zeroinitializer
             ; CHECK: filter [2 x i32] zeroinitializer
  br label %proceed

proceed:
  ret void
}

;; Intrinsic Functions

; Intrinsic Functions -- Variable Argument Handling
declare void @llvm.va_start.p0i8(i8*)
declare void @llvm.va_copy.p0i8.p0i8(i8*, i8*)
declare void @llvm.va_end.p0i8(i8*)
define void @instructions.va_arg(i8* %v, ...) {
  %ap = alloca i8*
  %ap2 = bitcast i8** %ap to i8*

  call void @llvm.va_start.p0i8(i8* %ap2)
  ; CHECK: call void @llvm.va_start.p0i8(i8* %ap2)

  va_arg i8* %ap2, i32
  ; CHECK: va_arg i8* %ap2, i32

  call void @llvm.va_copy.p0i8.p0i8(i8* %v, i8* %ap2)
  ; CHECK: call void @llvm.va_copy.p0i8.p0i8(i8* %v, i8* %ap2)

  call void @llvm.va_end.p0i8(i8* %ap2)
  ; CHECK: call void @llvm.va_end.p0i8(i8* %ap2)

  ret void
}

; Intrinsic Functions -- Accurate Garbage Collection
declare void @llvm.gcroot(i8**, i8*)
declare i8* @llvm.gcread(i8*, i8**)
declare void @llvm.gcwrite(i8*, i8*, i8**)
define void @intrinsics.gc() gc "shadow-stack" {
  %ptrloc = alloca i8*
  call void @llvm.gcroot(i8** %ptrloc, i8* null)
  ; CHECK: call void @llvm.gcroot(i8** %ptrloc, i8* null)

  call i8* @llvm.gcread(i8* null, i8** %ptrloc)
  ; CHECK: call i8* @llvm.gcread(i8* null, i8** %ptrloc)

  %ref = alloca i8
  call void @llvm.gcwrite(i8* %ref, i8* null, i8** %ptrloc)
  ; CHECK: call void @llvm.gcwrite(i8* %ref, i8* null, i8** %ptrloc)

  ret void
}

; Intrinsic Functions -- Code Generation
declare i8* @llvm.returnaddress(i32)
declare i8* @llvm.frameaddress(i32)
declare i32 @llvm.read_register.i32(metadata)
declare i64 @llvm.read_register.i64(metadata)
declare void @llvm.write_register.i32(metadata, i32)
declare void @llvm.write_register.i64(metadata, i64)
declare i8* @llvm.stacksave()
declare void @llvm.stackrestore(i8*)
declare void @llvm.prefetch.p0i8(i8*, i32, i32, i32)
declare void @llvm.pcmarker(i32)
declare i64 @llvm.readcyclecounter()
declare void @llvm.clear_cache(i8*, i8*)
declare void @llvm.instrprof_increment(i8*, i64, i32, i32)

!10 = !{!"rax"}
define void @intrinsics.codegen() {
  call i8* @llvm.returnaddress(i32 1)
  ; CHECK: call i8* @llvm.returnaddress(i32 1)
  call i8* @llvm.frameaddress(i32 1)
  ; CHECK: call i8* @llvm.frameaddress.p0i8(i32 1)

  call i32 @llvm.read_register.i32(metadata !10)
  ; CHECK: call i32 @llvm.read_register.i32(metadata !10)
  call i64 @llvm.read_register.i64(metadata !10)
  ; CHECK: call i64 @llvm.read_register.i64(metadata !10)
  call void @llvm.write_register.i32(metadata !10, i32 0)
  ; CHECK: call void @llvm.write_register.i32(metadata !10, i32 0)
  call void @llvm.write_register.i64(metadata !10, i64 0)
  ; CHECK: call void @llvm.write_register.i64(metadata !10, i64 0)

  %stack = call i8* @llvm.stacksave()
  ; CHECK: %stack = call i8* @llvm.stacksave()
  call void @llvm.stackrestore(i8* %stack)
  ; CHECK: call void @llvm.stackrestore(i8* %stack)

  call void @llvm.prefetch.p0i8(i8* %stack, i32 0, i32 3, i32 0)
  ; CHECK: call void @llvm.prefetch.p0i8(i8* %stack, i32 0, i32 3, i32 0)

  call void @llvm.pcmarker(i32 1)
  ; CHECK: call void @llvm.pcmarker(i32 1)

  call i64 @llvm.readcyclecounter()
  ; CHECK: call i64 @llvm.readcyclecounter()

  call void @llvm.clear_cache(i8* null, i8* null)
  ; CHECK: call void @llvm.clear_cache(i8* null, i8* null)

  call void @llvm.instrprof_increment(i8* null, i64 0, i32 0, i32 0)
  ; CHECK: call void @llvm.instrprof_increment(i8* null, i64 0, i32 0, i32 0)

  ret void
}

declare void @llvm.localescape(...)
declare i8* @llvm.localrecover(i8* %func, i8* %fp, i32 %idx)
define void @intrinsics.localescape() {
  %static.alloca = alloca i32
  call void (...) @llvm.localescape(i32* %static.alloca)
  ; CHECK: call void (...) @llvm.localescape(i32* %static.alloca)

  call void @intrinsics.localrecover()

  ret void
}
define void @intrinsics.localrecover() {
  %func = bitcast void ()* @intrinsics.localescape to i8*
  %fp = call i8* @llvm.frameaddress(i32 1)
  call i8* @llvm.localrecover(i8* %func, i8* %fp, i32 0)
  ; CHECK: call i8* @llvm.localrecover(i8* %func, i8* %fp, i32 0)

  ret void
}

; We need this function to provide `uses' for some metadata tests.
define void @misc.metadata() {
  call void @f1(), !srcloc !11
  call void @f1(), !srcloc !12
  call void @f1(), !srcloc !13
  call void @f1(), !srcloc !14
  ret void
}

declare void @op_bundle_callee_0()
declare void @op_bundle_callee_1(i32,i32)

define void @call_with_operand_bundle0(i32* %ptr) {
; CHECK-LABEL: call_with_operand_bundle0(
 entry:
  %l = load i32, i32* %ptr
  %x = add i32 42, 1
  call void @op_bundle_callee_0() [ "foo"(i32 42, i64 100, i32 %x), "bar"(float  0.000000e+00, i64 100, i32 %l) ]
; CHECK: call void @op_bundle_callee_0() [ "foo"(i32 42, i64 100, i32 %x), "bar"(float  0.000000e+00, i64 100, i32 %l) ]
  ret void
}

define void @call_with_operand_bundle1(i32* %ptr) {
; CHECK-LABEL: call_with_operand_bundle1(
 entry:
  %l = load i32, i32* %ptr
  %x = add i32 42, 1

  call void @op_bundle_callee_0()
  call void @op_bundle_callee_0() [ "foo"() ]
  call void @op_bundle_callee_0() [ "foo"(i32 42, i64 100, i32 %x), "bar"(float  0.000000e+00, i64 100, i32 %l) ]
; CHECK: @op_bundle_callee_0(){{$}}
; CHECK-NEXT: call void @op_bundle_callee_0() [ "foo"() ]
; CHECK-NEXT: call void @op_bundle_callee_0() [ "foo"(i32 42, i64 100, i32 %x), "bar"(float  0.000000e+00, i64 100, i32 %l) ]
  ret void
}

define void @call_with_operand_bundle2(i32* %ptr) {
; CHECK-LABEL: call_with_operand_bundle2(
 entry:
  call void @op_bundle_callee_0() [ "foo"() ]
; CHECK: call void @op_bundle_callee_0() [ "foo"() ]
  ret void
}

define void @call_with_operand_bundle3(i32* %ptr) {
; CHECK-LABEL: call_with_operand_bundle3(
 entry:
  %l = load i32, i32* %ptr
  %x = add i32 42, 1
  call void @op_bundle_callee_0() [ "foo"(i32 42, i64 100, i32 %x), "foo"(i32 42, float  0.000000e+00, i32 %l) ]
; CHECK: call void @op_bundle_callee_0() [ "foo"(i32 42, i64 100, i32 %x), "foo"(i32 42, float  0.000000e+00, i32 %l) ]
  ret void
}

define void @call_with_operand_bundle4(i32* %ptr) {
; CHECK-LABEL: call_with_operand_bundle4(
 entry:
  %l = load i32, i32* %ptr
  %x = add i32 42, 1
  call void @op_bundle_callee_1(i32 10, i32 %x) [ "foo"(i32 42, i64 100, i32 %x), "foo"(i32 42, float  0.000000e+00, i32 %l) ]
; CHECK: call void @op_bundle_callee_1(i32 10, i32 %x) [ "foo"(i32 42, i64 100, i32 %x), "foo"(i32 42, float  0.000000e+00, i32 %l) ]
  ret void
}

; Invoke versions of the above tests:


define void @invoke_with_operand_bundle0(i32* %ptr) personality i8 3 {
; CHECK-LABEL: @invoke_with_operand_bundle0(
 entry:
  %l = load i32, i32* %ptr
  %x = add i32 42, 1
  invoke void @op_bundle_callee_0() [ "foo"(i32 42, i64 100, i32 %x), "bar"(float  0.000000e+00, i64 100, i32 %l) ] to label %normal unwind label %exception
; CHECK: invoke void @op_bundle_callee_0() [ "foo"(i32 42, i64 100, i32 %x), "bar"(float  0.000000e+00, i64 100, i32 %l) ]

exception:
  %cleanup = landingpad i8 cleanup
  br label %normal
normal:
  ret void
}

define void @invoke_with_operand_bundle1(i32* %ptr) personality i8 3 {
; CHECK-LABEL: @invoke_with_operand_bundle1(
 entry:
  %l = load i32, i32* %ptr
  %x = add i32 42, 1

  invoke void @op_bundle_callee_0() to label %normal unwind label %exception
; CHECK: invoke void @op_bundle_callee_0(){{$}}

exception:
  %cleanup = landingpad i8 cleanup
  br label %normal

normal:
  invoke void @op_bundle_callee_0() [ "foo"() ] to label %normal1 unwind label %exception1
; CHECK: invoke void @op_bundle_callee_0() [ "foo"() ]

exception1:
  %cleanup1 = landingpad i8 cleanup
  br label %normal1

normal1:
  invoke void @op_bundle_callee_0() [ "foo"(i32 42, i64 100, i32 %x), "foo"(i32 42, float  0.000000e+00, i32 %l) ] to label %normal2 unwind label %exception2
; CHECK: invoke void @op_bundle_callee_0() [ "foo"(i32 42, i64 100, i32 %x), "foo"(i32 42, float  0.000000e+00, i32 %l) ]

exception2:
  %cleanup2 = landingpad i8 cleanup
  br label %normal2

normal2:
  ret void
}

define void @invoke_with_operand_bundle2(i32* %ptr) personality i8 3 {
; CHECK-LABEL: @invoke_with_operand_bundle2(
 entry:
  invoke void @op_bundle_callee_0() [ "foo"() ] to label %normal unwind label %exception
; CHECK: invoke void @op_bundle_callee_0() [ "foo"() ]

exception:
  %cleanup = landingpad i8 cleanup
  br label %normal
normal:
  ret void
}

define void @invoke_with_operand_bundle3(i32* %ptr) personality i8 3 {
; CHECK-LABEL: @invoke_with_operand_bundle3(
 entry:
  %l = load i32, i32* %ptr
  %x = add i32 42, 1
  invoke void @op_bundle_callee_0() [ "foo"(i32 42, i64 100, i32 %x), "foo"(i32 42, float  0.000000e+00, i32 %l) ] to label %normal unwind label %exception
; CHECK: invoke void @op_bundle_callee_0() [ "foo"(i32 42, i64 100, i32 %x), "foo"(i32 42, float  0.000000e+00, i32 %l) ]

exception:
  %cleanup = landingpad i8 cleanup
  br label %normal
normal:
  ret void
}

define void @invoke_with_operand_bundle4(i32* %ptr) personality i8 3 {
; CHECK-LABEL: @invoke_with_operand_bundle4(
 entry:
  %l = load i32, i32* %ptr
  %x = add i32 42, 1
  invoke void @op_bundle_callee_1(i32 10, i32 %x) [ "foo"(i32 42, i64 100, i32 %x), "foo"(i32 42, float  0.000000e+00, i32 %l) ]
        to label %normal unwind label %exception
; CHECK: invoke void @op_bundle_callee_1(i32 10, i32 %x) [ "foo"(i32 42, i64 100, i32 %x), "foo"(i32 42, float  0.000000e+00, i32 %l) ]

exception:
  %cleanup = landingpad i8 cleanup
  br label %normal
normal:
  ret void
}

declare void @vaargs_func(...)
define void @invoke_with_operand_bundle_vaarg(i32* %ptr) personality i8 3 {
; CHECK-LABEL: @invoke_with_operand_bundle_vaarg(
 entry:
  %l = load i32, i32* %ptr
  %x = add i32 42, 1
  invoke void (...) @vaargs_func(i32 10, i32 %x) [ "foo"(i32 42, i64 100, i32 %x), "foo"(i32 42, float  0.000000e+00, i32 %l) ]
        to label %normal unwind label %exception
; CHECK: invoke void (...) @vaargs_func(i32 10, i32 %x) [ "foo"(i32 42, i64 100, i32 %x), "foo"(i32 42, float  0.000000e+00, i32 %l) ]

exception:
  %cleanup = landingpad i8 cleanup
  br label %normal
normal:
  ret void
}


declare void @f.writeonly() writeonly
; CHECK: declare void @f.writeonly() #41

declare void @f.speculatable() speculatable
; CHECK: declare void @f.speculatable() #42

;; Constant Expressions

define i8** @constexpr() {
  ; CHECK: ret i8** getelementptr inbounds ({ [4 x i8*], [4 x i8*] }, { [4 x i8*], [4 x i8*] }* null, i32 0, inrange i32 1, i32 2)
  ret i8** getelementptr inbounds ({ [4 x i8*], [4 x i8*] }, { [4 x i8*], [4 x i8*] }* null, i32 0, inrange i32 1, i32 2)
}

define void @instructions.strictfp() strictfp {
  call void @f.strictfp() strictfp
  ; CHECK: call void @f.strictfp() #43

  ret void
}

declare void @f.nosanitize_coverage() nosanitize_coverage
; CHECK: declare void @f.nosanitize_coverage() #44

declare void @f.disable_sanitizer_instrumentation() disable_sanitizer_instrumentation
; CHECK: declare void @f.disable_sanitizer_instrumentation() #45

; immarg attribute
declare void @llvm.test.immarg.intrinsic(i32 immarg)
; CHECK: declare void @llvm.test.immarg.intrinsic(i32 immarg)

; byval attribute with type
%named_type = type [8 x i8]
declare void @byval_type(i32* byval(i32) align 2)
declare void @byval_type2({ i8, i8* }* byval({ i8, i8* }))
declare void @byval_named_type(%named_type* byval(%named_type))
; CHECK: declare void @byval_type(i32* byval(i32) align 2)
; CHECK: declare void @byval_type2({ i8, i8* }* byval({ i8, i8* }))
; CHECK: declare void @byval_named_type([8 x i8]* byval([8 x i8]))

declare void @f.allocsize_one(i32) allocsize(0)
declare void @f.allocsize_two(i32, i32) allocsize(1, 0)
; CHECK: Function Attrs: allocsize(0)
; CHECK: declare void @f.allocsize_one(i32)
; CHECK: Function Attrs: allocsize(1,0)
; CHECK: declare void @f.allocsize_two(i32, i32)

declare void @f.nosanitize_bounds() nosanitize_bounds
; CHECK: declare void @f.nosanitize_bounds() #48

declare void @f.allockind() allockind("alloc,uninitialized")
; CHECK: declare void @f.allockind() #49

; CHECK: attributes #0 = { alignstack=4 }
; CHECK: attributes #1 = { alignstack=8 }
; CHECK: attributes #2 = { alwaysinline }
; CHECK: attributes #3 = { cold }
; CHECK: attributes #4 = { convergent }
; CHECK: attributes #5 = { inlinehint }
; CHECK: attributes #6 = { jumptable }
; CHECK: attributes #7 = { minsize }
; CHECK: attributes #8 = { naked }
; CHECK: attributes #9 = { nobuiltin }
; CHECK: attributes #10 = { noduplicate }
; CHECK: attributes #11 = { noimplicitfloat }
; CHECK: attributes #12 = { noinline }
; CHECK: attributes #13 = { nonlazybind }
; CHECK: attributes #14 = { noredzone }
; CHECK: attributes #15 = { noreturn }
; CHECK: attributes #16 = { nounwind }
; CHECK: attributes #17 = { noinline optnone }
; CHECK: attributes #18 = { optsize }
; CHECK: attributes #19 = { memory(none) }
; CHECK: attributes #20 = { memory(read) }
; CHECK: attributes #21 = { returns_twice }
; CHECK: attributes #22 = { safestack }
; CHECK: attributes #23 = { sanitize_address }
; CHECK: attributes #24 = { sanitize_memory }
; CHECK: attributes #25 = { sanitize_thread }
; CHECK: attributes #26 = { ssp }
; CHECK: attributes #27 = { sspreq }
; CHECK: attributes #28 = { sspstrong }
; CHECK: attributes #29 = { "thunk" }
; CHECK: attributes #30 = { uwtable }
; CHECK: attributes #31 = { "cpu"="cortex-a8" }
; CHECK: attributes #32 = { norecurse }
; CHECK: attributes #33 = { memory(inaccessiblemem: readwrite) }
; CHECK: attributes #34 = { memory(argmem: readwrite, inaccessiblemem: readwrite) }
; CHECK: attributes #35 = { nocallback nofree nosync nounwind willreturn memory(none) }
; CHECK: attributes #36 = { nocallback nofree nosync nounwind willreturn }
; CHECK: attributes #37 = { nounwind memory(argmem: read) }
; CHECK: attributes #38 = { nounwind memory(argmem: readwrite) }
; CHECK: attributes #39 = { nounwind memory(read) }
; CHECK: attributes #40 = { nocallback nofree nosync nounwind willreturn memory(argmem: readwrite, inaccessiblemem: readwrite) }
; CHECK: attributes #41 = { memory(write) }
; CHECK: attributes #42 = { speculatable }
; CHECK: attributes #43 = { strictfp }
; CHECK: attributes #44 = { nosanitize_coverage }
; CHECK: attributes #45 = { disable_sanitizer_instrumentation }
; CHECK: attributes #46 = { allocsize(0) }
; CHECK: attributes #47 = { allocsize(1,0) }
; CHECK: attributes #48 = { nosanitize_bounds }
; CHECK: attributes #49 = { allockind("alloc,uninitialized") }
; CHECK: attributes #50 = { builtin }

;; Metadata

; Metadata -- Module flags
!llvm.module.flags = !{!0, !1, !2, !4, !5, !6}
; CHECK: !llvm.module.flags = !{!0, !1, !2, !4, !5, !6}

!0 = !{i32 1, !"mod1", i32 0}
; CHECK: !0 = !{i32 1, !"mod1", i32 0}
!1 = !{i32 2, !"mod2", i32 0}
; CHECK: !1 = !{i32 2, !"mod2", i32 0}
!2 = !{i32 3, !"mod3", !3}
; CHECK: !2 = !{i32 3, !"mod3", !3}
!3 = !{!"mod6", !0}
; CHECK: !3 = !{!"mod6", !0}
!4 = !{i32 4, !"mod4", i32 0}
; CHECK: !4 = !{i32 4, !"mod4", i32 0}
!5 = !{i32 5, !"mod5", !0}
; CHECK: !5 = !{i32 5, !"mod5", !0}
!6 = !{i32 6, !"mod6", !0}
; CHECK: !6 = !{i32 6, !"mod6", !0}

; Metadata -- Check `distinct'
!11 = distinct !{}
; CHECK: !11 = distinct !{}
!12 = distinct !{}
; CHECK: !12 = distinct !{}
!13 = !{!11}
; CHECK: !13 = !{!11}
!14 = !{!12}
; CHECK: !14 = !{!12}
