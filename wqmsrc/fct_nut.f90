!       subroutine FCT_NUT()
!
!==============================================================================|
!       FLUX CONTROL FOR NUTRIENTS                                             |
!==============================================================================|
!
Subroutine FCT_NUT
  !       #  if defined (WET_DRY)
  !
  !==============================================================================|
      Use MOD_LIMS, Only: MLOC, KBM1, NUMQBC, NUMPNT, NBTX_PNT, TCR_PNT
  !
      Use MOD_TGE, Only: NTSN, NBSN
  !
      Use MOD_HYDROVARS, Only: DZ,DZ2D, THOUR
  !Wen Long took MOD_CONTROL out of MOD_HYDROVARS and put the used variables here
      Use MOD_CONTROL, Only: INFLOW_TYPE
      Use MOD_BCS, Only: INODEQ, INOPNT,TXPNT_ID, TCRPNT_ID !!LOCAL NON-POINT SOURCE NODES
  !
      Use MOD_WQM, Only: C2, C2F, NAC, AC, TxcKntOn, TcrKntOn
      Use MOD_PREC, Only: SP
  !
      Use MOD_BCMAP, Only: IOBCN, I_OBC_N
  !
      Implicit None
      Real (SP) :: NUTMAX, NUTMIN
      Integer :: I, J, K, JCON, II
  !==============================================================================|
      Do JCON = 1, NAC
         II = AC (JCON)
         Do I = 1, MLOC
            If (IOBCN > 0) Then
               Do J = 1, IOBCN
                  If (I == I_OBC_N(J)) Go To 200
               End Do
            End If
        !
            If (NUMQBC > 0) Then
               Do J = 1, NUMQBC
                  If (INFLOW_TYPE == 'node') Then
                     If (I == INODEQ(J)) Go To 200
                  End If
               End Do
            End If
        !
            If (NUMPNT > 0) Then
               Do J = 1, NUMPNT
                  If (I == INOPNT(J)) Go To 200
               End Do
            End If

        IF (TxcKntOn) THEN
              IF (NBTX_PNT > 0) THEN
                    DO J = 1, NBTX_PNT
                      IF (I == TXPNT_ID(J)) GO TO 200
                    END DO
              END IF
        END IF

        IF (TcrKntOn) THEN
              IF (TCR_PNT > 0) THEN
                    DO J = 1, TCR_PNT
                      IF (I == TCRPNT_ID(J)) GO TO 200
                    END DO
              END IF
        END IF

        !
            Do K = 1, KBM1
               NUTMAX = MAXVAL (C2(NBSN(I, 1:NTSN(I)), K, II))
               NUTMIN = MINVAL (C2(NBSN(I, 1:NTSN(I)), K, II))
           !
               If (K == 1) Then
                  NUTMAX = Max (NUTMAX, (C2(I, K, II)*DZ2D(I,K+1)+C2(I, &
                 & K+1, II)*DZ2D(I,K))/(DZ2D(I,K)+DZ2D(I,K+1)))
                  NUTMIN = Min (NUTMIN, (C2(I, K, II)*DZ2D(I,K+1)+C2(I, &
                 & K+1, II)*DZ2D(I,K))/(DZ2D(I,K)+DZ2D(I,K+1)))
               Else If (K == KBM1) Then
                  NUTMAX = Max (NUTMAX, (C2(I, K, II)*DZ2D(I,K-1)+C2(I, &
                 & K-1, II)*DZ2D(I,K))/(DZ2D(I,K)+DZ2D(I,K-1)))
                  NUTMIN = Min (NUTMIN, (C2(I, K, II)*DZ2D(I,K-1)+C2(I, &
                 & K-1, II)*DZ2D(I,K))/(DZ2D(I,K)+DZ2D(I,K-1)))
               Else
                  NUTMAX = Max (NUTMAX, (C2(I, K, II)*DZ2D(I,K-1)+C2(I, &
                 & K-1, II)*DZ2D(I,K))/(DZ2D(I,K)+DZ2D(I,K-1)), (C2(I, K, &
                 & II)*DZ2D(I,K+1)+C2(I, K+1, II)*DZ2D(I,K))/(DZ2D(I,K)+DZ2D(I,K+1)))
                  NUTMIN = Min (NUTMIN, (C2(I, K, II)*DZ2D(I,K-1)+C2(I, &
                 & K-1, II)*DZ2D(I,K))/(DZ2D(I,K)+DZ2D(I,K-1)), (C2(I, K, &
                 & II)*DZ2D(I,K+1)+C2(I, K+1, II)*DZ2D(I,K))/(DZ2D(I,K)+DZ2D(I,K+1)))
               End If
           !
           !       tykim commented out two lines below
               If (NUTMIN-C2F(I, K, II) > 0.0_SP) C2F (I, K, II) = &
              & NUTMIN
               If (C2F(I, K, II)-NUTMAX > 0.0_SP) C2F (I, K, II) = &
              & NUTMAX
           !
           !       IF(NUTMIN-C2F(I,K,II) >  C2F(I,K,II)*0.001 ) C2F(I,K,II) = NUTMIN
           !       IF(C2F(I,K,II)-NUTMAX >  C2F(I,K,II)*0.001 ) C2F(I,K,II) = NUTMAX
            End Do
        !
200         Continue
        !
         End Do
      End Do
      Return
  !       #  endif
End Subroutine FCT_NUT
!==============================================================================|
