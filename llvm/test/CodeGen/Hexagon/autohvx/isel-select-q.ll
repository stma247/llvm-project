; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -mtriple=hexagon < %s | FileCheck %s

; Check that selection (based on i1) between vector predicates works.
define <128 x i8> @f0(<128 x i8> %a0, <128 x i8> %a1, <128 x i8> %a2, <128 x i8> %a3, i32 %a4) #0 {
; CHECK-LABEL: f0:
; CHECK:       // %bb.0:
; CHECK-NEXT:    {
; CHECK-NEXT:     q0 = vcmp.gt(v0.b,v1.b)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     q1 = vcmp.gt(v1.b,v2.b)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     r2 = #-1
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     p0 = cmp.gt(r0,#0)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     v0 = vand(q1,r2)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     v2 = vand(q0,r2)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     if (p0) v0 = v2
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     q3 = vand(v0,r2)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     v0 = vmux(q3,v1,v3)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     jumpr r31
; CHECK-NEXT:    }
  %v0 = icmp sgt <128 x i8> %a0, %a1
  %v1 = icmp sgt <128 x i8> %a1, %a2
  %v2 = icmp sgt i32 %a4, 0
  %v3 = select i1 %v2, <128 x i1> %v0, <128 x i1> %v1
  %v4 = select <128 x i1> %v3, <128 x i8> %a1, <128 x i8> %a3
  ret <128 x i8> %v4
}

define <64 x i16> @f1(<64 x i16> %a0, <64 x i16> %a1, <64 x i16> %a2, <64 x i16> %a3, i32 %a4) #0 {
; CHECK-LABEL: f1:
; CHECK:       // %bb.0:
; CHECK-NEXT:    {
; CHECK-NEXT:     q0 = vcmp.gt(v0.h,v1.h)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     q1 = vcmp.gt(v1.h,v2.h)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     r2 = #-1
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     p0 = cmp.gt(r0,#0)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     v0 = vand(q1,r2)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     v2 = vand(q0,r2)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     if (p0) v0 = v2
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     q3 = vand(v0,r2)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     v0 = vmux(q3,v1,v3)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     jumpr r31
; CHECK-NEXT:    }
  %v0 = icmp sgt <64 x i16> %a0, %a1
  %v1 = icmp sgt <64 x i16> %a1, %a2
  %v2 = icmp sgt i32 %a4, 0
  %v3 = select i1 %v2, <64 x i1> %v0, <64 x i1> %v1
  %v4 = select <64 x i1> %v3, <64 x i16> %a1, <64 x i16> %a3
  ret <64 x i16> %v4
}

define <32 x i32> @f2(<32 x i32> %a0, <32 x i32> %a1, <32 x i32> %a2, <32 x i32> %a3, i32 %a4) #0 {
; CHECK-LABEL: f2:
; CHECK:       // %bb.0:
; CHECK-NEXT:    {
; CHECK-NEXT:     q0 = vcmp.gt(v0.w,v1.w)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     q1 = vcmp.gt(v1.w,v2.w)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     r2 = #-1
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     p0 = cmp.gt(r0,#0)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     v0 = vand(q1,r2)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     v2 = vand(q0,r2)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     if (p0) v0 = v2
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     q3 = vand(v0,r2)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     v0 = vmux(q3,v1,v3)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     jumpr r31
; CHECK-NEXT:    }
  %v0 = icmp sgt <32 x i32> %a0, %a1
  %v1 = icmp sgt <32 x i32> %a1, %a2
  %v2 = icmp sgt i32 %a4, 0
  %v3 = select i1 %v2, <32 x i1> %v0, <32 x i1> %v1
  %v4 = select <32 x i1> %v3, <32 x i32> %a1, <32 x i32> %a3
  ret <32 x i32> %v4
}

; Selection of vector predicates first converts them into regular vectors.
; Check that all-true and all-false bool vectors are optimized into splat(-1)
; and vxor(v,v).
define <128 x i8> @f3(<128 x i8> %a0, <128 x i8> %a1, i32 %a2) #0 {
; CHECK-LABEL: f3:
; CHECK:       // %bb.0:
; CHECK-NEXT:    {
; CHECK-NEXT:     r2 = #-1
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     p0 = cmp.gt(r0,#0)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     v2 = vxor(v2,v2)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     v3 = vsplat(r2)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     if (p0) v2 = v3
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     q0 = vand(v2,r2)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     v0 = vmux(q0,v0,v1)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     jumpr r31
; CHECK-NEXT:    }
  %v0 = insertelement <128 x i1> undef, i1 true, i32 0
  %v1 = shufflevector <128 x i1> %v0, <128 x i1> undef, <128 x i32> zeroinitializer
  %v2 = icmp sgt i32 %a2, 0
  %v3 = select i1 %v2, <128 x i1> %v1, <128 x i1> zeroinitializer
  %v4 = select <128 x i1> %v3, <128 x i8> %a0, <128 x i8> %a1
  ret <128 x i8> %v4
}

define <64 x i16> @f4(<64 x i16> %a0, <64 x i16> %a1, i32 %a2) #0 {
; CHECK-LABEL: f4:
; CHECK:       // %bb.0:
; CHECK-NEXT:    {
; CHECK-NEXT:     r2 = #-1
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     p0 = cmp.gt(r0,#0)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     v2 = vxor(v2,v2)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     v3 = vsplat(r2)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     if (p0) v2 = v3
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     q0 = vand(v2,r2)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     v0 = vmux(q0,v0,v1)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     jumpr r31
; CHECK-NEXT:    }
  %v0 = insertelement <64 x i1> undef, i1 true, i32 0
  %v1 = shufflevector <64 x i1> %v0, <64 x i1> undef, <64 x i32> zeroinitializer
  %v2 = icmp sgt i32 %a2, 0
  %v3 = select i1 %v2, <64 x i1> %v1, <64 x i1> zeroinitializer
  %v4 = select <64 x i1> %v3, <64 x i16> %a0, <64 x i16> %a1
  ret <64 x i16> %v4
}

define <32 x i32> @f5(<32 x i32> %a0, <32 x i32> %a1, i32 %a2) #0 {
; CHECK-LABEL: f5:
; CHECK:       // %bb.0:
; CHECK-NEXT:    {
; CHECK-NEXT:     r2 = #-1
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     p0 = cmp.gt(r0,#0)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     v2 = vxor(v2,v2)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     v3 = vsplat(r2)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     if (p0) v2 = v3
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     q0 = vand(v2,r2)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     v0 = vmux(q0,v0,v1)
; CHECK-NEXT:    }
; CHECK-NEXT:    {
; CHECK-NEXT:     jumpr r31
; CHECK-NEXT:    }
  %v0 = insertelement <32 x i1> undef, i1 true, i32 0
  %v1 = shufflevector <32 x i1> %v0, <32 x i1> undef, <32 x i32> zeroinitializer
  %v2 = icmp sgt i32 %a2, 0
  %v3 = select i1 %v2, <32 x i1> %v1, <32 x i1> zeroinitializer
  %v4 = select <32 x i1> %v3, <32 x i32> %a0, <32 x i32> %a1
  ret <32 x i32> %v4
}

attributes #0 = { nounwind "target-cpu"="hexagonv66" "target-features"="+hvx,+hvx-length128b,-packets" }

