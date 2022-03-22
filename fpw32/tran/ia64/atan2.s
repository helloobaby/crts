.file "atan2.s"

// Copyright (c) 2000, Intel Corporation
// All rights reserved.
// 
// Contributed 2/2/2000 by John Harrison, Ted Kubaska, Bob Norin, Shane Story,
// and Ping Tak Peter Tang of the Computational Software Lab, Intel Corporation.
// 
// WARRANTY DISCLAIMER
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL INTEL OR ITS 
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
// EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
// PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY 
// OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
// 
// Intel Corporation is the author of this code, and requests that all
// problem reports or change requests be submitted to it directly at 
// http://developer.intel.com/opensource.
//
// History
//==============================================================
// 2/02/00  Initial version
// 4/04/00  Unwind support added
// 8/15/00  Bundle added after call to __libm_error_support to properly
//          set [the previously overwritten] GR_Parameter_RESULT.
// 8/17/00  Changed predicate register macro-usage to direct predicate
//          names due to an assembler bug.
// 9/28/00  Updated to set invalid on SNaN inputs
//
// API
//==============================================================
// double atan2(double Y, double X)
//
// Overview of operation
//==============================================================
//
// There are two basic paths: swap true and swap false.
// atan2(Y,X) ==> atan2(V/U) where U >= V. If Y > X, we must swap.
//
// p6  swap True
// p7  swap False
// p8  X+   (If swap=True p8=p9=0)
// p9  X-
//
// all the other predicates p10 thru p15 and false for the main path
//
// Special values
//==============================================================
//              Y                 x          Result
//             +number           +inf        +0
//             -number           +inf        -0
//             +number           -inf        +pi
//             -number           -inf        -pi
//
//             +inf              +number     +pi/2
//             -inf              +number     -pi/2
//             +inf              -number     +pi/2
//             -inf              -number     -pi/2
//
//             +inf              +inf        +pi/4
//             -inf              +inf        -pi/4
//             +inf              -inf        +3pi/4
//             -inf              -inf        -3pi/4
//
//             +1                +1          +pi/4
//             -1                +1          -pi/4
//             +1                -1          +3pi/4
//             -1                -1          -3pi/4
//
//             +number           +0          +pi/2
//             -number           +0          -pi/2
//             +number           -0          +pi/2
//             -number           -0          -pi/2
//
//             +0                +number     +0 
//             -0                +number     -0 
//             +0                -number     +pi
//             -0                -number     -pi
//
//             +0                +0          +0 
//             -0                +0          -0 
//             +0                -0          +pi
//             -0                -0          -pi
//
//            Nan             anything      quiet Y
//            anything        NaN           quiet X

// atan(+-0/+-0) sets double error tag to 37
// atan(+-0/+-0) sets single error tag to 38


// Assembly macros
//==============================================================

EXP_AD_P1                    = r33
EXP_AD_P2                    = r34


GR_SAVE_B0                   = r35
GR_SAVE_GP                   = r36
GR_SAVE_PFS                  = r37

GR_Parameter_X               = r38
GR_Parameter_Y               = r39
GR_Parameter_RESULT          = r40
atan2_GR_tag                 = r41


atan2_X                      = f9
atan2_Y                      = f8

atan2_u1_X                   = f32
atan2_u1_Y                   = f33
atan2_Umax                   = f34
atan2_Vmin                   = f35
atan2_two                    = f36
atan2_absX                   = f37
atan2_z1_X                   = f38
atan2_z1_Y                   = f39
atan2_B1X                    = f40
atan2_B1Y                    = f41
atan2_wp                     = f42
atan2_B1sq                   = f43
atan2_z                      = f44
atan2_w                      = f45

atan2_P0                     = f46
atan2_P1                     = f47
atan2_P2                     = f48
atan2_P3                     = f49
atan2_P4                     = f50
atan2_P5                     = f51
atan2_P6                     = f52
atan2_P7                     = f53
atan2_P8                     = f54
atan2_P9                     = f55
atan2_P10                    = f56
atan2_P11                    = f57
atan2_P12                    = f58
atan2_P13                    = f59
atan2_P14                    = f60
atan2_P15                    = f61
atan2_P16                    = f62
atan2_P17                    = f63
atan2_P18                    = f64
atan2_P19                    = f65
atan2_P20                    = f66
atan2_P21                    = f67
atan2_P22                    = f68
atan2_Pi_by_2                = f69

atan2_V13                    = f70
atan2_W11                    = f71
atan2_E                      = f72
atan2_gamma                  = f73
atan2_V11                    = f74
atan2_V12                    = f75
atan2_V7                     = f76
atan2_V8                     = f77
atan2_W7                     = f78
atan2_W8                     = f79
atan2_W3                     = f80
atan2_W4                     = f81
atan2_V3                     = f82
atan2_V4                     = f83
atan2_F                      = f84
atan2_gV                     = f85
atan2_V10                    = f86
atan2_zcub                   = f87
atan2_V6                     = f88
atan2_V9                     = f89
atan2_W10                    = f90
atan2_W6                     = f91
atan2_W2                     = f92
atan2_V2                     = f93

atan2_alpha                  = f94
atan2_alpha_1                = f95
atan2_gVF                    = f96
atan2_V5                     = f97
atan2_W12                    = f98
atan2_W5                     = f99
atan2_alpha_sq               = f100
atan2_Cp                     = f101
atan2_V1                     = f102

atan2_NORM_X                 = f103

atan2_W1                     = f104
atan2_alpha_cub              = f105
atan2_C                      = f106
atan2_P                      = f107
atan2_d                      = f108
atan2_A_hi                   = f109
atan2_dsq                    = f110
atan2_pd                     = f111
atan2_A_lo                   = f112
atan2_A                      = f113

atan2_Pp                     = f114
atan2_NORM_X                 = f115

atan2_sgnY                   = f116
atan2_pi                     = f117
atan2_sgnX                   = f118
atan2_sgnXY                  = f119

atan2_3pi_by_4               = f120
atan2_pi_by_4                = f121

atan2_NORM_Y                 = f122

//atan2_sF                     = p7
//atan2_sT                     = p6

// These coefficients are for atan2. 
// You can also use this set to substitute those used in the |X| <= 1 case for atan; 
// BUT NOT vice versa.

/////////////////////////////////////////////////////////////


.data

.align 16

atan2_tb1:
data8 0xB199DD6D2675C40F ,  0x0000BFFA // P10
data8 0xA21922DC45605EA1 ,  0x00003FFA // P11
data8 0xD78F28FC2A592781 ,  0x0000BFFA // P8
data8 0xC2F01E5DDD100DBE ,  0x00003FFA // P9
data8 0x9D89D7D55C3287A5 ,  0x00003FFB // P5
data8 0xF0F03ADB3FC930D3 ,  0x00003FFA // P7
data8 0xF396268151CFB11C ,  0x00003FF7 // P17 
data8 0x9D3436AABE218776 ,  0x00003FF5 // P19
data8 0x80D601879218B53A ,  0x00003FFA // P13
data8 0xA2270D30A90AA220 ,  0x00003FF9 // P15
data8 0xCCCCCCCCCCC906CD ,  0x00003FFC // P1
data8 0xE38E38E320A8A098 ,  0x00003FFB // P3
data8 0xFE7E52D2A89995B3 ,  0x0000BFEC // P22
data8 0xC90FDAA22168C235 ,  0x00003FFE // pi/4

atan2_tb2:
data8 0x9F90FB984D8E39D0 ,  0x0000BFF3 // P20
data8 0xCE585A259BD8374C ,  0x00003FF0 // P21
data8 0xBA2E8B9793955C77 ,  0x0000BFFB // P4
data8 0x88887EBB209E3543 ,  0x0000BFFB // P6
data8 0xD818B4BB43D84BF2 ,  0x0000BFF8 // P16
data8 0xDEC343E068A6D2A8 ,  0x0000BFF6 // P18
data8 0x9297B23CCFFB291F ,  0x0000BFFA // P12
data8 0xD5F4F2182E7A8725 ,  0x0000BFF9 // P14
data8 0xAAAAAAAAAAAAA8A9 ,  0x0000BFFD // P0
data8 0x9249249247E37913 ,  0x0000BFFC // P2
data8 0xC90FDAA22168C235 ,  0x00003FFF // pi/2
data8 0xC90FDAA22168C235 ,  0x00004000 // pi
data8 0x96cbe3f9990e91a8 ,  0x00004000 // 3pi/4




.align 32
.global atan2#

////////////////////////////////////////////////////////

.section .text
.proc  atan2#
.align 32
atan2:
// qnan snan inf norm     unorm 0 -+
// 0    0    1   0        0     0 11


//         Y NAN?     p10 p11
// p10 ==> quiet Y and return
// p11     X NAN?     p12, p13 
// p12 ==> quiet X and return

{ .mfi
           alloc        r32           = ar.pfs,1,5,4,0
           frcpa.s1     atan2_u1_X,p6 = f1,atan2_X
           addl         EXP_AD_P2   = @ltoff(atan2_tb2), gp
}
{ .mfi
           addl         EXP_AD_P1   = @ltoff(atan2_tb1), gp
           fclass.m.unc p10,p11 = f8, 0xc3
           nop.i 999
;;
}

{ .mfi
           ld8  EXP_AD_P1 = [EXP_AD_P1]
           frcpa.s1     atan2_u1_Y,p7 = f1,atan2_Y
           nop.i 999
}
{ .mfi
           nop.m 999
           fma.s1       atan2_two  = f1,f1,f1 
           nop.i 999
;;
}


{ .mfi
           ld8 EXP_AD_P2 = [ EXP_AD_P2]
           famax.s1     atan2_Umax =  f8,f9
           nop.i 999
}
;;

{ .mfi
           nop.m 999
           fmerge.s     atan2_absX = f0,atan2_X
           nop.i 999
}
;;

// p10 Y NAN, quiet and return
{ .mfi
           ldfe         atan2_P10  = [EXP_AD_P1],16
           fmerge.s     atan2_sgnY = atan2_Y,f1
           nop.i 999
}
{ .mfb
           nop.m 999
(p10)      fma.d f8 = f8,f9,f0 
(p10)      br.ret.spnt b0
;;
}


{ .mmf
           ldfe         atan2_P11  = [EXP_AD_P1],16
           ldfe         atan2_P20  = [EXP_AD_P2],16
           fmerge.s     atan2_sgnX = atan2_X,f1
;;
}


{ .mfi 
           ldfe         atan2_P8   = [EXP_AD_P1],16
           fma.s1       atan2_z1_X = atan2_u1_X, atan2_Y, f0
           nop.i 999
}
{ .mfi 

           ldfe         atan2_P21  = [EXP_AD_P2],16
           fma.s1       atan2_z1_Y = atan2_u1_Y, atan2_X, f0
           nop.i 999
;;
}

{ .mfi 
           ldfe         atan2_P9   = [EXP_AD_P1],16
           fnma.s1      atan2_B1X  = atan2_u1_X, atan2_X, atan2_two
           nop.i 999
}
{ .mfi 

           ldfe         atan2_P4   = [EXP_AD_P2],16
           fnma.s1      atan2_B1Y  = atan2_u1_Y, atan2_Y, atan2_two
           nop.i 999
;;
}

// p6 (atan2_sT) true if swap
// p7 (atan2_sF) true if no swap
// p11 ==> Y !NAN;  X NAN?

{ .mfi
           ldfe         atan2_P5   = [EXP_AD_P1],16
//           fcmp.eq.unc.s1 atan2_sF,atan2_sT    = atan2_Umax, atan2_X
           fcmp.eq.unc.s1 p7,p6    = atan2_Umax, atan2_X
           nop.i 999
}
{ .mfi
           ldfe         atan2_P6   = [EXP_AD_P2],16
(p11)      fclass.m.unc p12,p13    = f9, 0xc3
           nop.i 999
;;
}

{ .mmf
           ldfe         atan2_P7   = [EXP_AD_P1],16
           ldfe         atan2_P16  = [EXP_AD_P2],16
           famin.s1     atan2_Vmin =  f8,f9
;;
}

// p8 true if X positive
// p9 true if X negative
// both are false is swap is true
{ .mfi
           ldfe         atan2_P17  = [EXP_AD_P1],16
//(atan2_sF) fcmp.eq.unc.s1 p8,p9    = atan2_sgnX,f1
(p7) fcmp.eq.unc.s1 p8,p9    = atan2_sgnX,f1
           nop.i 999
}
{ .mfi
           ldfe         atan2_P18  = [EXP_AD_P2],16
           fma.s1       atan2_sgnXY     = atan2_sgnX, atan2_sgnY, f0 
           nop.i 999
;;
}


{ .mfi
           ldfe         atan2_P19  = [EXP_AD_P1],16
//(atan2_sF) fma.s1       atan2_wp   = atan2_z1_X, atan2_z1_X, f0
(p7) fma.s1       atan2_wp   = atan2_z1_X, atan2_z1_X, f0
           nop.i 999
}
{ .mfi
           ldfe         atan2_P12  = [EXP_AD_P2],16
//(atan2_sT) fma.s1       atan2_wp   = atan2_z1_Y, atan2_z1_Y, f0
(p6) fma.s1       atan2_wp   = atan2_z1_Y, atan2_z1_Y, f0
           nop.i 999
;;
}


{ .mfi
           ldfe         atan2_P13  = [EXP_AD_P1],16
//(atan2_sF) fma.s1       atan2_z         = atan2_z1_X, atan2_B1X, f0
(p7) fma.s1       atan2_z         = atan2_z1_X, atan2_B1X, f0
           nop.i 999
}
{ .mfi
           ldfe         atan2_P14  = [EXP_AD_P2],16
//(atan2_sT) fma.s1       atan2_z         = atan2_z1_Y, atan2_B1Y, f0
(p6) fma.s1       atan2_z         = atan2_z1_Y, atan2_B1Y, f0
           nop.i 999
;;
}


{ .mfi
           ldfe         atan2_P15       = [EXP_AD_P1],16
//(atan2_sF) fma.s1       atan2_B1sq = atan2_B1X, atan2_B1X, f0
(p7) fma.s1       atan2_B1sq = atan2_B1X, atan2_B1X, f0
           nop.i 999
}
{ .mfi
           ldfe         atan2_P0        = [EXP_AD_P2],16
//(atan2_sT) fma.s1       atan2_B1sq = atan2_B1Y, atan2_B1Y, f0
(p6) fma.s1       atan2_B1sq = atan2_B1Y, atan2_B1Y, f0
           nop.i 999
;;
}


// p12 ==> X NAN, quiet and return
{ .mfi
           ldfe         atan2_P1        = [EXP_AD_P1],16
           fmerge.s     atan2_Umax      = f0,atan2_Umax
           nop.i 999
}
{ .mfb
           ldfe         atan2_P2        = [EXP_AD_P2],16
(p12)      fma.d        f8 = f9,f8,f0
(p12)      br.ret.spnt b0
;;
}


// p10 ==> x  inf     y ?
// p11 ==> x !inf     y ?
{ .mfi
           ldfe         atan2_P3        = [EXP_AD_P1],16
           fmerge.s     atan2_Vmin      = f0,atan2_Vmin
           nop.i 999
}
{ .mfi
           ldfe         atan2_Pi_by_2   = [EXP_AD_P2],16
           fclass.m.unc p10,p11 = f9, 0x23
           nop.i 999
;;
}


{ .mmf
           ldfe         atan2_P22       = [EXP_AD_P1],16
           ldfe         atan2_pi        = [EXP_AD_P2],16
           nop.f 999
;;
}

{ .mfi
           nop.m 999 
           fma.s0       atan2_NORM_X    = f9,f1,f0
           nop.i 999
}
{ .mfi
           nop.m 999 
           fma.s0       atan2_NORM_Y    = f8,f1,f0
           nop.i 999
;;
}



{ .mfi
           ldfe         atan2_pi_by_4       = [EXP_AD_P1],16
           fma.s1       atan2_w         = atan2_wp, atan2_B1sq,f0
           nop.i 999
}
{ .mfi
           ldfe         atan2_3pi_by_4       = [EXP_AD_P2],16
//(atan2_sT) fmerge.ns    atan2_sgnXY     = atan2_sgnXY, atan2_sgnXY
(p6) fmerge.ns    atan2_sgnXY     = atan2_sgnXY, atan2_sgnXY
           nop.i 999
;;
}

// p12 ==> x  inf     y inf
// p13 ==> x  inf     y !inf
{ .mfi
           nop.m 999
           fmerge.s     atan2_z         = f0, atan2_z
           nop.i 999
}

{ .mfi
           nop.m 99
(p10)      fclass.m.unc p12,p13 = f8, 0x23
           nop.i 999
}
{ .mfi
           nop.m 99
(p11)      fclass.m.unc p14,p15 = f8, 0x23
           nop.i 999
;;
}

{ .mfi
           nop.m 999
(p12)      fcmp.eq.unc.s1 p10,p11       = atan2_sgnX,f1
           nop.i 99
;;
}


{ .mfb
           nop.m 999
(p14)      fma.d       f8 = atan2_sgnY, atan2_Pi_by_2, f0
(p14)      br.ret.spnt b0
;;
}

{ .mfi
           nop.m 999
           fma.s1       atan2_V13       = atan2_w, atan2_P11, atan2_P10
           nop.i 999
}
{ .mfi
           nop.m 999
           fma.s1       atan2_W11       = atan2_w, atan2_P21, atan2_P20
           nop.i 999
;;
}


{ .mfi
           nop.m 999
           fma.s1       atan2_E         = atan2_Vmin, atan2_z, atan2_Umax
           nop.i 999
}
{ .mfi
           nop.m 999
           fnma.s1      atan2_gamma     = atan2_Umax, atan2_z, f1
           nop.i 999
;;
}

{ .mfi
           nop.m 999
           fma.s1       atan2_V11       = atan2_w, atan2_P9, atan2_P8
           nop.i 999
}
{ .mfi
           nop.m 999
           fma.s1       atan2_V12       = atan2_w, atan2_w, f0
           nop.i 999
;;
}

{ .mfi
           nop.m 999
           fma.s1       atan2_V7        = atan2_w, atan2_P5 , atan2_P4 
           nop.i 999
}
{ .mfi
           nop.m 999
           fma.s1       atan2_V8        = atan2_w, atan2_P7 , atan2_P6 
           nop.i 999
;;
}

{ .mfi
           nop.m 999
           fma.s1       atan2_W7        = atan2_w, atan2_P17, atan2_P16 
           nop.i 999
}
{ .mfi
           nop.m 999
           fma.s1       atan2_W8        = atan2_w, atan2_P19, atan2_P18
           nop.i 999
;;
}

{ .mfi
           nop.m 999
           fma.s1       atan2_W3        = atan2_w, atan2_P13, atan2_P12 
           nop.i 999
}
{ .mfi
           nop.m 999
           fma.s1       atan2_W4        = atan2_w, atan2_P15, atan2_P14
           nop.i 999
;;
}

{ .mfi
           nop.m 999
           fma.s1       atan2_V3        = atan2_w, atan2_P1 , atan2_P0 
           nop.i 999
}
{ .mfi
           nop.m 999
           fma.s1       atan2_V4        = atan2_w, atan2_P3 , atan2_P2
           nop.i 999
;;
}

{ .mfi
           nop.m 999
           fma.s1       atan2_zcub      = atan2_z, atan2_w, f0
           nop.i 999
}
{ .mfi
           nop.m 999
           fnma.s1       atan2_gV        = atan2_Umax, atan2_z, atan2_Vmin 
           nop.i 999
;;
}

{ .mfi
           nop.m 999
           frcpa.s1     atan2_F,p15     = f1, atan2_E
           nop.i 999
}
{ .mfi
           nop.m 999
           fma.s1       atan2_V10       = atan2_V12, atan2_V13, atan2_V11
           nop.i 999
;;
}

{ .mfi
           nop.m 999
           fma.s1       atan2_V6        = atan2_V12, atan2_V8 , atan2_V7 
           nop.i 999
}
{ .mfi
           nop.m 999
           fma.s1       atan2_V9        = atan2_V12, atan2_V12, f0
           nop.i 999
;;
}

{ .mfi
           nop.m 999
           fma.s1       atan2_W10       = atan2_V12, atan2_P22 , atan2_W11
           nop.i 999
}
{ .mfi
           nop.m 999
           fma.s1       atan2_W6        = atan2_V12, atan2_W8 , atan2_W7
           nop.i 999
;;
}

{ .mfi
           nop.m 999
           fma.s1       atan2_W2        = atan2_V12, atan2_W4  , atan2_W3
           nop.i 999
}
{ .mfi
           nop.m 999
           fma.s1       atan2_V2        = atan2_V12, atan2_V4 , atan2_V3
           nop.i 999
;;
}


// Both X and Y are INF
// p10 ==> X +
// p11 ==> X -
.pred.rel "mutex",p10,p11
{ .mfb
           nop.m 999
(p10)      fma.d       f8              = atan2_sgnY, atan2_pi_by_4, f0
(p10)      br.ret.spnt b0
}
{ .mfb
           nop.m 999
(p11)      fma.d       f8              = atan2_sgnY, atan2_3pi_by_4, f0
(p11)      br.ret.spnt b0
;;
}


.pred.rel "mutex",p8,p9,p6
{ .mfi
           nop.m 999
           fnma.s1      atan2_alpha     = atan2_E, atan2_F, f1
           nop.i 999
}
{ .mfi
           nop.m 999
           fnma.s1      atan2_alpha_1   = atan2_E, atan2_F, atan2_two
           nop.i 999
;;
}


{ .mfi
           nop.m 999
//(atan2_sT) fmerge.s     atan2_P         = atan2_Y, atan2_Pi_by_2
(p6) fmerge.s     atan2_P         = atan2_Y, atan2_Pi_by_2
           nop.i 999
}
{ .mfi
           nop.m 999
           fma.s1       atan2_gVF       = atan2_gV, atan2_F, f0
           nop.i 999
;;
}


{ .mfi
           nop.m 999
           fma.s1       atan2_V5        = atan2_V9, atan2_V10, atan2_V6
           nop.i 999
}
{ .mfi
           nop.m 999
           fma.s1       atan2_W12       = atan2_V9, atan2_V9, f0
           nop.i 999
;;
}



{ .mfi
           nop.m 999
(p8)       fmerge.s     atan2_P         = atan2_sgnY, f0
           nop.i 999
}
{ .mfi
           nop.m 999
           fma.s1       atan2_W5        = atan2_V9, atan2_W10, atan2_W6
           nop.i 999
;;
}




{ .mfi
           nop.m 999
(p9)       fmerge.s     atan2_P         = atan2_sgnY, atan2_pi
           nop.i 999
;;
}


{ .mfi
           nop.m 999
           fma.s1       atan2_alpha_sq  = atan2_alpha, atan2_alpha, f0  
           nop.i 999
}
{ .mfi
           nop.m 999
           fma.s1       atan2_Cp        = atan2_alpha, atan2_alpha_1, f1  
           nop.i 999
;;
}


{ .mfi
           nop.m 999
           fma.s1       atan2_V1        = atan2_V9, atan2_V5, atan2_V2
           nop.i 999
}
{ .mfi
           nop.m 999
           fma.s1       atan2_W12       = atan2_V9, atan2_W12, f0
           nop.i 999
;;
}


// p13 ==> x  inf     y !inf
{ .mfi
           nop.m 999
           fma.s1       atan2_W1        = atan2_V9, atan2_W5, atan2_W2
           nop.i 999
}
{ .mfi
           nop.m 999
(p13)      fcmp.eq.unc.s1 p10,p11       = atan2_sgnX,f1
           nop.i 999
;;
}


{ .mfi
           nop.m 999
           fma.s1       atan2_alpha_cub = atan2_alpha, atan2_alpha_sq, f0
           nop.i 999
}
{ .mfi
           nop.m 999
           fma.s1       atan2_C         = atan2_gVF, atan2_Cp, f0
           nop.i 999
;;
}

.pred.rel "mutex",p10,p11 
// x inf y !inf
{ .mfb
           nop.m 999
(p10)      fmerge.s     f8              = atan2_sgnY, f0
(p10)      br.ret.spnt b0
}
{ .mfb
           nop.m 999
(p11)      fma.d        f8              = atan2_sgnY, atan2_pi, f0
(p11)      br.ret.spnt b0
;;
}



// p10 ==> y   0     x?
// p11 ==> y  !0     x?
{ .mfi
           nop.m 999
           fclass.m.unc p10,p11 = f8, 0x07
           nop.i 999
;;
}

{ .mfi
           nop.m 999
           fma.s1       atan2_Pp        = atan2_W12, atan2_W1, atan2_V1
           nop.i 999
}
{ .mfi
           nop.m 999
           fma.s1       atan2_d         = atan2_alpha_cub, atan2_C, atan2_C
           nop.i 999
;;
}

// p12 ==>  y0     x0
// p13 ==>  y0     x!0
// p14 ==>  y!0    x0
// p15 ==>  y!0    x!0
{ .mfi
           nop.m 999
(p10)      fclass.m.unc p12,p13 = f9, 0x07 
           nop.i 999
}
{ .mfi
           nop.m 999
(p11)      fclass.m.unc p14,p15 = f9, 0x07 
           nop.i 999
;;
}




{ .mfb
           nop.m 999
(p13)      fcmp.eq.unc.s1 p10,p11       = atan2_sgnX,f1
(p12)      br.spnt ATAN2_ERROR
;;
}



{ .mfi
           nop.m 999
           fma.s1       atan2_pd        = atan2_P0, atan2_d, f0
           nop.i 999
}
{ .mfi
           nop.m 999
           fma.s1       atan2_dsq       = atan2_d, atan2_d, f0
           nop.i 999
;;
}

{ .mfi
           nop.m 999
           fma.s1       atan2_A_hi      = atan2_zcub, atan2_Pp, atan2_z
           nop.i 999
}
{ .mfb
           nop.m 999
(p14)      fma.d       f8 = atan2_sgnY, atan2_Pi_by_2, f0
(p14)      br.ret.spnt b0                
;;
}



{ .mfb
           nop.m 999
(p10)      fmerge.s     f8              = atan2_sgnY, f0
(p10)      br.ret.spnt b0
}
{ .mfb
           nop.m 999
(p11)      fma.d        f8              = atan2_sgnY, atan2_pi, f0
(p11)      br.ret.spnt b0
;;
}



{ .mfi
           nop.m 999
           fma.s1       atan2_A_lo      = atan2_pd, atan2_dsq, atan2_d
           nop.i 999
;;
}


{ .mfi
           nop.m 999
           fma.s1       atan2_A         = atan2_A_hi, f1, atan2_A_lo
           nop.i 999
;;
}

{ .mfb
           nop.m 999
           fma.d        f8              = atan2_sgnXY, atan2_A, atan2_P
           br.ret.sptk  b0     
}

ATAN2_ERROR:

{ .mfi
          nop.m 999
          fcmp.eq.unc.s1 p10,p11       = atan2_sgnX,f1
          nop.i 999
}
;;

{ .mfi
          mov        atan2_GR_tag     = 37 
(p10)     fmerge.s     f10             = atan2_sgnY, f0
          nop.i 999 
}
{ .mfi
          nop.m 999
(p11)     fma.d        f10            = atan2_sgnY, atan2_pi, f0
          nop.i 999
;;
}
.endp atan2#


// Stack operations when calling error support.
//       (1)               (2)                          (3) (call)              (4)
//   sp   -> +          psp -> +                     psp -> +                   sp -> +
//           |                 |                            |                         |
//           |                 | <- GR_Y               R3 ->| <- GR_RESULT            | -> f8
//           |                 |                            |                         |
//           | <-GR_Y      Y2->|                       Y2 ->| <- GR_Y                 |
//           |                 |                            |                         |
//           |                 | <- GR_X               X1 ->|                         |
//           |                 |                            |                         |
//  sp-64 -> +          sp ->  +                     sp ->  +                         +
//    save ar.pfs          save b0                                               restore gp
//    save gp                                                                    restore ar.pfs


.proc __libm_error_region
__libm_error_region:
.prologue
// (1)
{ .mfi
        add   GR_Parameter_Y=-32,sp             // Parameter 2 value
        nop.f 999
.save   ar.pfs,GR_SAVE_PFS
        mov  GR_SAVE_PFS=ar.pfs                 // Save ar.pfs
}
{ .mfi
.fframe 64
        add sp=-64,sp                          // Create new stack
        nop.f 0
        mov GR_SAVE_GP=gp                      // Save gp
};;


// (2)
{ .mmi
        stfd [GR_Parameter_Y] = f8,16         // STORE Parameter 2 on stack
        add GR_Parameter_X = 16,sp            // Parameter 1 address
.save   b0, GR_SAVE_B0
        mov GR_SAVE_B0=b0                     // Save b0
};;

.body
// (3)
{ .mib
        stfd [GR_Parameter_X] = f9                   // STORE Parameter 1 on stack
        add   GR_Parameter_RESULT = 0,GR_Parameter_Y // Parameter 3 address
        nop.b 0                                 
}
{ .mib
        stfd [GR_Parameter_Y] = f10                  // STORE Parameter 3 on stack
        add   GR_Parameter_Y = -16,GR_Parameter_Y
        br.call.sptk b0=__libm_error_support#        // Call error handling function
};;
{ .mmi
        nop.m 0
        nop.m 0
        add   GR_Parameter_RESULT = 48,sp
};;

// (4)
{ .mmi
        ldfd  f8 = [GR_Parameter_RESULT]       // Get return result off stack
.restore
        add   sp = 64,sp                       // Restore stack pointer
        mov   b0 = GR_SAVE_B0                  // Restore return address
};;
{ .mib
        mov   gp = GR_SAVE_GP                  // Restore gp
        mov   ar.pfs = GR_SAVE_PFS             // Restore ar.pfs
        br.ret.sptk     b0                     // Return
};;

.endp __libm_error_region

.type   __libm_error_support#,@function
.global __libm_error_support#
