!Subroutine
! Subroutine VDIF_WQM()
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
!
!==============================================================================|
Subroutine VDIF_WQM (F)
  !
      Use MOD_PREC, Only: SP, DP !LB added DP 3jun2016
      Use MOD_LIMS, Only: MTLOC, KBM1, MLOC, KB, KBM2, KBM1
  !
      Use MOD_TGE, Only: ISONB !

  !
      Use MOD_HYDROVARS, Only: DZ, DZ2D, DZZ, DZZ2D,D, KH
      Use MOD_SIZES, Only: NCP
      Use MOD_WQM, Only: NAC, AC, DLT, ZDFBCK, VDIFFLUX !LB added VDIFFLUX 26jan2016
      Use MOD_CONTROL, Only: MSR
      Implicit None
      Real (SP), Dimension (0:MTLOC, KBM1, NCP) :: F
      Real (SP), Dimension (0:MTLOC, KBM1) :: FF
      Real (SP), Dimension (MLOC, KB) :: VHF, VHPF
  !  KURT GLAESEMANN 8 SEP 2009 - Fix dimensions on AF, CF, RAD KB!=NCP
      Real (SP), Dimension (MLOC, KB) :: AF, CF, RAD
  !    !!LB changed to DP the following, to be as in fvcom 3jun2016
  !    Real (DP), Dimension (0:MTLOC, KBM1) :: FF
  !    Real (DP), Dimension (MLOC, KB) :: VHF, VHPF
  !!  KURT GLAESEMANN 8 SEP 2009 - Fix dimensions on AF, CF, RAD KB!=NCP
  !    Real (DP), Dimension (MLOC, KB) :: AF, CF, RAD
!
      Real (SP), Dimension (MLOC) :: BENFLUX, WFSURF
      Real (SP), Dimension (MLOC) :: SOURCE1, SOURCE2, SOURCE3
      Real (SP), Dimension (MLOC) :: TBOT
      Real (SP) :: FKH, UMOLPR
      Real (SP) :: TEMPWUVBOT, TMP
      Real (SP) :: auxLBdiff !LB
      Integer :: I, K, J, KI, JCON, II
  !
      UMOLPR = ZDFBCK * 1.E0
  !  KURT GLAESEMANN - this was not initalized
      BENFLUX = 0
!
  !!VDIFFLUX = 0.0  !LB initialize
  !
  !----------------------------------------------------------------
  !
  !  the following section solves the equation
  !  dti*(kh*f')' -f=-fb
  !
  !----------------------------------------------------------------
  !
      Do K = 2, KBM1
         Do I = 1, MLOC
			IF(D(I) > 0.0_SP)THEN
               FKH = KH (I, K)
               AF (I, K-1) = - DLT * (FKH+UMOLPR) / &
              & (DZ2D(I,K-1)*DZZ2D(I,K-1)*D(I)*D(I))
               CF (I, K) = - DLT * (FKH+UMOLPR) / &
              & (DZ2D(I,K)*DZZ2D(I,K-1)*D(I)*D(I))
            End If
         End Do
      End Do
  !
      WFSURF = 0.0
  !
  !------------------------------------------------
  !  Surface BCs; WFSURF
  !-----------------------------------------------
  !
  !  KURT GLAESEMANN 8 SEP 2009 -  MERGE ALL JCON LOOPS INTO ONE BIG ONE - FASTER
  !  A BUNCH OF VARIABLES LOST A NCP DIMENSION
      Do JCON = 1, NAC
         II = AC (JCON)
         Do I = 1, MLOC
			IF(D(I) > 0.0_SP)THEN
               VHF (I, 1) = AF (I, 1) / (AF(I, 1)-1.)
               VHPF (I, 1) = - DLT * WFSURF (I) / (-DZ2D(I,1)*D(I)) - F (I, &
              & 1, II)
               VHPF (I, 1) = VHPF (I, 1) / (AF(I, 1)-1.)
            End If
         End Do
     !
         Do K = 2, KBM2
            Do I = 1, MLOC
				IF(D(I) > 0.0_SP)THEN
                  VHPF (I, K) = 1. / (AF(I, K)+CF(I, K)*(1.-VHF(I, &
                 & K-1))-1.)
                  VHF (I, K) = AF (I, K) * VHPF (I, K)
                  VHPF (I, K) = (CF(I, K)*VHPF(I, K-1)-F(I, K, II)) * &
                 & VHPF (I, K)
               End If
            End Do
         End Do
     !
         Do K = 1, KBM1
            Do I = 1, MLOC
				IF(D(I) > 0.0_SP)THEN
                  FF (I, K) = F (I, K, II)
               End If
            End Do
         End Do
     !
         Do I = 1, MLOC
			IF(D(I) > 0.0_SP .And. ISONB(I) /= 2)THEN
               FF (I, KBM1) = (CF(I, KBM1)*VHPF(I, KBM2)-FF(I, &
              & KBM1)-DLT*BENFLUX(I)/(D(I)*DZ2D(I,KBM1))) / (CF(I, &
              & KBM1)*(1.-VHF(I, KBM2))-1.)
               !
               auxLBdiff = (FF(I, KBM1)-F(I, KBM1, II)) / DLT * 86400.0 !LB
			   !VDIFFLUX(I, KBM1, II) = auxLBdiff !LB: snapshot
               !VDIFFLUX (I, KBM1, II) = VDIFFLUX (I, KBM1, II) + &
              !& auxLBdiff !LB: accumulated
            End If
         End Do

         Do K = 2, KBM1
            KI = KB - K
            Do I = 1, MLOC
			IF(D(I) > 0.0_SP .And. ISONB(I) /= 2)THEN
                  FF (I, KI) = (VHF(I, KI)*FF(I, KI+1)+VHPF(I, KI))
              !
                  auxLBdiff = (FF(I, KI)-F(I, KI, II)) / DLT * 86400.0 !LB
				!VDIFFLUX(I, KI, II) = auxLBdiff !LB: snapshot
                  !VDIFFLUX (I, KI, II) = VDIFFLUX (I, KI, II) + &
                 !& auxLBdiff !LB: accumulated
               End If
            End Do
         End Do

         Do I = 1, MLOC
			Do K = 1, KBM1
                  F (I, K, II) = FF (I, K)
             End Do
         End Do
      End Do
  !
      Return
End Subroutine VDIF_WQM
!==============================================================================!
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
!
