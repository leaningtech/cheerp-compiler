; Tests that CoroEarly pass correctly lowers coro.resume, coro.destroy
; RUN: opt < %s -S -passes=coro-early | FileCheck %s

; CHECK-LABEL: @callResume(
define void @callResume(i8* %hdl) {
; CHECK-NEXT: entry
entry:
; CHECK-NEXT: %0 = call i8* @llvm.coro.subfn.addr.p0i8.p0i8(i8* %hdl, i8 0)
; CHECK-NEXT: %1 = bitcast i8* %0 to void (%coroFrameBase*)*
; CHECK-NEXT: %cast = bitcast i8* %hdl to %coroFrameBase*
; CHECK-NEXT: call fastcc void %1(%coroFrameBase* %cast)
  call void @llvm.coro.resume.p0i8(i8* %hdl)

; CHECK-NEXT: %2 = call i8* @llvm.coro.subfn.addr.p0i8.p0i8(i8* %hdl, i8 1)
; CHECK-NEXT: %3 = bitcast i8* %2 to void (%coroFrameBase*)*
; CHECK-NEXT: %cast1 = bitcast i8* %hdl to %coroFrameBase*
; CHECK-NEXT: call fastcc void %3(%coroFrameBase* %cast1)
  call void @llvm.coro.destroy.p0i8(i8* %hdl)

  ret void
; CHECK-NEXT: ret void
}

; CHECK-LABEL: @eh(
define void @eh(i8* %hdl) personality i8* null {
; CHECK-NEXT: entry
entry:
;  CHECK-NEXT: %0 = call i8* @llvm.coro.subfn.addr.p0i8.p0i8(i8* %hdl, i8 0)
;  CHECK-NEXT: %1 = bitcast i8* %0 to void (%coroFrameBase*)*
;  CHECK-NEXT: %cast = bitcast i8* %hdl to %coroFrameBase*
;  CHECK-NEXT: invoke fastcc void %1(%coroFrameBase* %cast)
  invoke void @llvm.coro.resume.p0i8(i8* %hdl)
          to label %cont unwind label %ehcleanup
cont:
  ret void

ehcleanup:
  %0 = cleanuppad within none []
  cleanupret from %0 unwind to caller
}


declare void @llvm.coro.resume.p0i8(i8*)
declare void @llvm.coro.destroy.p0i8(i8*)
