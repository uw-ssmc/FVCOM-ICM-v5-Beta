!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
!
!=============================================================================!
!
!Subroutine ADV_WQM()
!
!
Subroutine ADV_WQM

      Use MOD_LIMS, Only: KB, MYID, NPROCS, NCV, NUMPNT, MLOC, MTLOC, &
     & NTLOC, KBM1, NCV_I, NBTX_PNT, TCR_PNT
      Use MOD_PREC, Only: SP
      Use MOD_TGE, Only: ISONB, NTSN, NBSN, NIEC, NTRG, XIJE, YIJE, &
     & DLTXE, DLTYE
!
      Use MOD_HYDROVARS, Only: ZERO, VX, VY, ART1, ART2, DZ,DZ2D, DT, DT1, &
     & DTFA, UU, VV, WTS, VISCOFH, THOUR, XFLUX_OBC_WQM !LB added XFLUX_OBC_WQM may17,2016
  !Wen Long took MOD_CONTROL out of MOD_HYDROVARS and put the used variables here
      Use MOD_CONTROL, Only: PAR, MSR !	&           !!TRUE IF 1 RUN
!
      Use MOD_WQM, Only: NAC, AC, C2, C1, C2F, DLT, DTM, NCP, XYDF, &
     & ELTMS, ADVFLUX, TxcKntOn, TcrKntOn
      Use MOD_WQMINIT, Only: XYDFU
      Use MOD_BCS, Only: INOPNT, PNT_TM, WVQDIST, PWQDIS, PDWQDIS, &
     & PQDIS, PDQDIS, TXPNT_ID, TX_TM, TXQDIST, TXQDIS, TXCQDIS, &
	  & TXFQDIS, TXCFQDIS, TCRPNT_ID, TCR_TM, TCRQDIST, TCRQDIS, &  !!DISCHARGE at non-point source node
     & BCTFQDIS, TCRCQDIS, BCTCFQDIS  
      Use MOD_BCMAP, Only: IOBCN, I_OBC_N !Added by LB 17may2016
		USE MOD_SIZES, ONLY : NTXVB, NBTCR

      Use MOD_PAR, Only: NBN, BN_LOC, BN_MLT, BNC, NODE_MATCH_ALL !, & !
!
      Implicit None
      Real (SP), Allocatable, Dimension (:, :, :) :: XFLUX, XFLUX_ADV
      Real (SP), Dimension (0:MTLOC) :: PUPX, PUPY, PVPX, PVPY !LB 6may2016 turn MLOC into 0:MTLOC
      Real (SP), Dimension (0:MTLOC) :: PFPX, PFPY, PFPXD, PFPYD, &
     & VISCOFF !LB 6may2016 turn MLOC into 0:MTLOC
      Real (SP), Dimension (3*(NTLOC)) :: DTIJ !LB commented
  !REAL(SP), DIMENSION(3*(NTLOC),KBM1) :: DTIJ  !LB added to make as in HYD code -04/29/2016
      Real (SP), Dimension (3*(NTLOC), KBM1) :: UVN
      Real (SP) :: FFD, FF1, X11, Y11, X22, Y22, X33, Y33, XI, YI
      Real (SP) :: DXA, DYA, DXB_A (NCV_I), DYB_A (NCV_I), DXB_B &
     & (NCV_I), DYB_B (NCV_I), FIJ1, FIJ2, UN, DYB, DXB
      Real (SP) :: TXX, TYY, FXX, FYY, VISCOF, EXFLUX, TEMP
      Real (SP) :: FACT, FM1
      Real (SP) :: TT, TTIME, STPOINT, STPOINT1, STPOINT2
      Integer :: I, I1, I2, IA, IB, IC, J, J1, J2, JTMP, K, JJ, JCON, &
     & II
      Real (SP) :: C21MIN, C21MAX, C22MIN, C22MAX

      Logical XYDFUISON
      Real (SP) :: TMP1 (MLOC), TMP2 (MLOC)
  !
      Integer :: NJ1, NJ2, NJ3, EXTRA,WQVB,TXSRT,BCSRT
  !TYKIM for nonpoint sources
      Integer :: L1, L2, IERR
      Real (SP) :: UFACT, DFACT
      Real (SP) :: auxLBadv
!
  !!ADVFLUX = 0.0  !LB initialize
  !------------------------------------------------------------------------------
  !
  !  KURT GLAESEMANN 17 SEPT 2009, make XFLUX allocated
      Allocate (XFLUX(0:MTLOC, KB, NCP))
      Allocate (XFLUX_ADV(0:MTLOC, KB, NCP))
  !  KURT GLAESEMANN 10 SEPT 2009 - convert this to logic below
  !   FACT = 0.0
  !   FM1  = 1.0
  !   IF(XYDFU == ' ON')THEN
  !     FACT = 1.0
  !     FM1  = 0.0
  !   ENDIF
      XYDFUISON = .False.
      If (XYDFU == ' ON') XYDFUISON = .True.
  !
  ! KURT GLAESEMANN 10 SEPT 2009 - precalculate stuff
      Do I = 1, MLOC
         TMP1 (I) = DLT / DT (I) / ART1 (I)
         TMP2 (I) = DT (I) / DTFA (I)
     !TMP2 (I) = 1.0  !test LB 6/6/2016
      End Do
  !
  ! KURT GLAESEMANN 17 SEPT 2009 precalculate more stuff
      Do I = 1, NCV_I
         IA = NIEC (I, 1)
         IB = NIEC (I, 2)
         XI = 0.5_SP * (XIJE(I, 1)+XIJE(I, 2))
         YI = 0.5_SP * (YIJE(I, 1)+YIJE(I, 2))
         DXB_A (I) = XI - VX (IA)
         DYB_A (I) = YI - VY (IA)
         DXB_B (I) = XI - VX (IB)
         DYB_B (I) = YI - VY (IB)
      End Do
		WQVB = NCP - NTXVB     ! No. of water quality related variables
      TXSRT = NCP - NTXVB + 1   ! starting index for toxics
      BCSRT = NCP - NTXVB - NBTCR + 1 ! starting index for bacteria/pollutants
  !
  !--Initialize Fluxes-----------------------------------------------------------
  !
      XFLUX = 0.0
      XFLUX_ADV = 0.0
  !
  !--Loop Over Control Volume Sub-Edges And Calculate Normal Velocity------------
  !
      Do I = 1, NCV
         I1 = NTRG (I)
         DTIJ (I) = DT1 (I1)!!!!commented by LB, as it is in FVCOM-HYD code -Arp29,2016
         Do K = 1, KBM1
            UVN (I, K) = VV (I1, K) * DLTXE (I) - UU (I1, K) * DLTYE &
           & (I)
         End Do
      End Do
  !
  !
  !--Calculate the Advection and Horizontal Diffusion Terms----------------------
  !
      Do JCON = 1, NAC
         II = AC (JCON)
         Do K = 1, KBM1
            PFPX = 0.0
            PFPY = 0.0

            PFPXD = 0.0 !LBtest
            PFPYD = 0.0
        !
            Do I = 1, MLOC
               Do J = 1, NTSN (I) - 1
                  I1 = NBSN (I, J)
                  I2 = NBSN (I, J+1)


				FF1 = 0.5_SP * (C2(I1, K, II)+C2(I2, K, II))

                  PFPX (I) = PFPX (I) + FF1 * (VY(I1)-VY(I2))
                  PFPY (I) = PFPY (I) + FF1 * (VX(I2)-VX(I1))

               End Do
               PFPX (I) = PFPX (I) / ART2 (I)
               PFPY (I) = PFPY (I) / ART2 (I)

            End Do
        !
            Do I = 1, MLOC
               VISCOFF (I) = VISCOFH (I, K)
            End Do
        !
            Do I = 1, NCV_I
               IA = NIEC (I, 1)
               IB = NIEC (I, 2)

!
               UN = UVN (I, K)
           !
           !  KURT GLAESEMANN 10 SEPT 2009 - convert FACT and FM1 to logic
               If (XYDFUISON) Then
                  VISCOF = XYDF * ((VISCOFF(IA)+VISCOFF(IB))*0.5)
               Else
                  VISCOF = XYDF
               End If
           !   VISCOF=XYDF*(FACT*(VISCOFF(IA)+VISCOFF(IB))*0.5 + FM1)

               TXX = 0.5 * (PFPX(IA)+PFPX(IB)) * VISCOF
               TYY = 0.5 * (PFPY(IA)+PFPY(IB)) * VISCOF
           !
               FXX = - DTIJ (I) * TXX * DLTYE (I)
               FYY = DTIJ (I) * TYY * DLTXE (I)

           !!LB added commented above and added below to have DTIJ as in HYD code -04/29/2016
           !FXX = - DTIJ (I,K) * TXX * DLTYE (I)
           !FYY = DTIJ (I,K) * TYY * DLTXE (I)
           !
               If (UN .Ge. 0) Then
                  IC = IB
                  DYB = DYB_B (I)
                  DXB = DXB_B (I)
               Else
                  IC = IA
                  DYB = DYB_A (I)
                  DXB = DXB_A (I)
               End If
               C22MIN = C2 (IC, K, II)
               C22MAX = C2 (IC, K, II)
               Do J = 1, NTSN (IC) - 1
                  TEMP = C2 (NBSN(IC, J), K, II)
                  C22MIN = Min (C22MIN, TEMP)
                  C22MAX = Max (C22MAX, TEMP)
               End Do
               FIJ2 = C2 (IC, K, II) + DXB * PFPX (IC) + DYB * PFPY &
              & (IC)
               If (FIJ2 < C22MIN) FIJ2 = C22MIN
               If (FIJ2 > C22MAX) FIJ2 = C22MAX
           !caps above for minmax criteria
               EXFLUX = - UN * DTIJ (I) * FIJ2 + FXX + FYY
           !!LB added commented above and added below to have DTIJ as in HYD code -04/29/2016
           !EXFLUX = - UN * DTIJ (I,K) * FIJ2 + FXX + FYY
!
           ! END KURT
               XFLUX (IA, K, II) = XFLUX (IA, K, II) + EXFLUX
               XFLUX (IB, K, II) = XFLUX (IB, K, II) - EXFLUX
           !
               XFLUX_ADV (IA, K, II) = XFLUX_ADV (IA, K, II) + &
              & (EXFLUX-FXX-FYY)
               XFLUX_ADV (IB, K, II) = XFLUX_ADV (IB, K, II) - &
              & (EXFLUX-FXX-FYY)
            End Do
         End Do
      End Do
  !
  !
  !-Accumulate Fluxes at Boundary Nodes
  !
      If (PAR) Then
     ! KURT GLAESEMANN 17 SEPT 2009 DO MULTIPLE VARIABLES AT ONCE
     !    DO JCON=1,NAC
     !      II=AC(JCON)
     !      CALL NODE_MATCH(0,NBN,BN_MLT,BN_LOC,BNC,MTLOC,KB,MYID,NPROCS,XFLUX(:,:,II))
     !    ENDDO
     !  KURT GLAESEMANN 22 SEPT 2009 - DO ALL AT ONCE
         Call NODE_MATCH_ALL (0, NBN, BN_MLT, BN_LOC, BNC, MTLOC, KB, &
        & NCP, MYID, NPROCS, XFLUX)
      End If

  !
  !!Added by LB in May 17, 2016, to use this xflux_obc_wqm in bcond_wqm.F (as done in HYD)
      Do JCON = 1, NAC
         II = AC (JCON)
         Do K = 1, KBM1
            If (IOBCN > 0) Then
               Do I = 1, IOBCN
                  I1 = I_OBC_N (I)
                  XFLUX_OBC_WQM (I, K, II) = XFLUX_ADV (I1, K, II)
               End Do
            End If
         End Do
      End Do
  !!end addition LB
!

   !NON-POINT/point SOURCE INTERPOLATION for water quality parameters
      If (NUMPNT > 0) Then
         Allocate (PWQDIS(NUMPNT, NCP))
         PWQDIS = ZERO
         Allocate (PQDIS(NUMPNT))
         PQDIS = ZERO
         Call BRACKET (PNT_TM, THOUR, L1, L2, DFACT, UFACT, IERR)
         PWQDIS (:, :) = UFACT * PDWQDIS (:, :, L1) + DFACT * PDWQDIS &
        & (:, :, L2)
         PQDIS (:) = UFACT * PDQDIS (:, L1) + DFACT * PDQDIS (:, L2)

         PQDIS (:) = PQDIS (:) * Tanh (ELTMS/FLOAT(86400))

      End If
!-------------- point source interpolation for toxics --------------------------
			IF (TxcKntOn) THEN
						IF (NBTX_PNT > 0) THEN
								ALLOCATE (TXCFQDIS(NBTX_PNT, TXSRT:NCP-NBTCR))
								TXCFQDIS = ZERO
								ALLOCATE (TXFQDIS(NBTX_PNT))
								TXFQDIS = ZERO
								CALL BRACKET (TX_TM, THOUR, L1, L2, DFACT, UFACT, IERR)
								TXCFQDIS (:, :) = UFACT * TXCQDIS (:, :, L1) + DFACT * TXCQDIS (:, :, L2)
								TXFQDIS (:) = UFACT * TXQDIS (:, L1) + DFACT * TXQDIS (:, L2)
								TXFQDIS (:) = TXFQDIS (:) * Tanh (ELTMS/FLOAT(86400))
								!WRITE(*,*)ELTMS,TXSRT,UFACT,DFACT,TXFQDIS (:)
								!WRITE(*,*)'---------------------------------'
								!WRITE(*,*)TXSRT,UFACT,DFACT,TXCQDIS (:, :, L1)
								!WRITE(*,*)'*********************************'
						END IF
			END IF
!-------------------------------------------------------------------------------- 

!-------------- point source interpolation for toxics --------------------------
			IF (TcrKntOn) THEN
						IF (TCR_PNT > 0) THEN
								ALLOCATE (BCTCFQDIS(TCR_PNT, BCSRT:NCP))
								BCTCFQDIS = ZERO
								ALLOCATE (BCTFQDIS(TCR_PNT))
								BCTFQDIS = ZERO
								CALL BRACKET (TCR_TM, THOUR, L1, L2, DFACT, UFACT, IERR)
								BCTCFQDIS (:, :) = UFACT * TCRCQDIS (:, :, L1) + DFACT * TCRCQDIS (:, :, L2)
								BCTFQDIS (:) = UFACT * TCRQDIS (:, L1) + DFACT * TCRQDIS (:, L2)
								BCTFQDIS (:) = BCTFQDIS (:) * Tanh (ELTMS/FLOAT(86400))
								!WRITE(*,*)ELTMS,TXSRT,UFACT,DFACT,TXFQDIS (:)
								!WRITE(*,*)'---------------------------------'
								!WRITE(*,*)TXSRT,UFACT,DFACT,TXCQDIS (:, :, L1)
								!WRITE(*,*)'*********************************'
						END IF
			END IF
!-------------------------------------------------------------------------------- 


      Do JCON = 1, NAC
         II = AC (JCON)
     !--Calculate the Vertical Terms------------------------------------------------
     !
         Do I=1,MLOC
            Do K = 1, KBM1
               If (K == 1) Then
                  TEMP = - WTS (I, K+1) * (C2(I, K, II)*DZ2D(I,K+1)+C2(I, &
                 & K+1, II)*DZ2D(I,K)) / (DZ2D(I,K)+DZ2D(I,K+1))
               Else If (K == KBM1) Then
                  TEMP = WTS (I, K) * (C2(I, K, II)*DZ2D(I,K-1)+C2(I, K-1, &
                 & II)*DZ2D(I,K)) / (DZ2D(I,K)+DZ2D(I,K-1))
               Else
                  TEMP = WTS (I, K) * (C2(I, K, II)*DZ2D(I,K-1)+C2(I, K-1, &
                 & II)*DZ2D(I,K)) / (DZ2D(I,K)+DZ2D(I,K-1)) - WTS (I, K+1) * (C2(I, &
                 & K, II)*DZ2D(I,K+1)+C2(I, K+1, II)*DZ2D(I,K)) / &
                 & (DZ2D(I,K)+DZ2D(I,K+1))
               End If
           !
           !
           !--Total Fluxes ---------------------------------------------------------------
           !
               If (ISONB(I) == 2) Then
                  XFLUX (I, K, II) = TEMP * ART1 (I) / DZ2D (I,K)
               Else
                  XFLUX (I, K, II) = XFLUX (I, K, II) + TEMP * ART1 (I) &
                 & / DZ2D (I,K)
               End If
           ! KURT GLAESEMANN 10 SEPT 2009 MERGE TWO LOOPS- rgl uncommented
           ! to allow nonpoint
            End Do
         End Do
     !
     !TYKIM
		 !===============================================================================
     !POINT SOURCE FLUX for water quality parameters
     !===============================================================================
         If ((NUMPNT > 0) .AND. (II < TXSRT)) Then
            Do J = 1, NUMPNT
               JJ = INOPNT (J)
               Do K = 1, KBM1
                  STPOINT = PWQDIS (J, II)

                  XFLUX (JJ, K, II) = XFLUX (JJ, K, II) - PQDIS (J) * &
                 & WVQDIST (J, K) * STPOINT / DZ2D (JJ,K)

               End Do
            End Do
         End If
    !------------------------------------------------------------------------------
	 !===============================================================================
	 !POINT SOURCE FLUX for toxics
	 !===============================================================================
    IF (TxcKntOn) THEN
			 IF ((NBTX_PNT > 0) .AND. (II >= TXSRT)) THEN
					DO J = 1, NBTX_PNT
						 JJ = TXPNT_ID (J)
						 DO K = 1, KBM1
								STPOINT = TXCFQDIS (J, II)

								XFLUX (JJ, K, II) = XFLUX (JJ, K, II) - TXFQDIS (J) * &
							 & TXQDIST (J, K) * STPOINT / DZ2D (JJ,K)
							 !WRITE(*,*)J,XFLUX (JJ, K, II),TXFQDIS (J),TXQDIST (J, K),STPOINT
							 !WRITE(*,*)'---------------------------------'

						 END DO
					END DO
			 END IF
   END IF
	!------------------------------------------------------------------------------ 
    !===============================================================================
	 !POINT SOURCE FLUX for bacteria/pollutants/tracer
	 !===============================================================================
    IF (TcrKntOn) THEN
			 IF ((TCR_PNT > 0) .AND. (II >= BCSRT)) THEN
					DO J = 1, TCR_PNT
						 JJ = TCRPNT_ID (J)
						 DO K = 1, KBM1
								STPOINT = BCTCFQDIS (J, II)

								XFLUX (JJ, K, II) = XFLUX (JJ, K, II) - BCTFQDIS (J) * &
							 & TCRQDIST (J, K) * STPOINT / DZ2D (JJ,K)
							 !WRITE(*,*)J,XFLUX (JJ, K, II),TXFQDIS (J),TXQDIST (J, K),STPOINT
							 !WRITE(*,*)'---------------------------------'

						 END DO
					END DO
			 END IF
   END IF
	!------------------------------------------------------------------------------



     !--Update Water Quality Variables--------------------------------
     !
         Do I = 1, MLOC
				Do K = 1, KBM1
				! KURT GLAESEMANN 10 SEPT 2009 PRECALCULATE A BUNCH OF STUFF
				!           C1(I,K,II)=(C2(I,K,II)-XFLUX(I,K,II)/ART1(I)*(DLT/DT(I)))*   &
				!                         (DT(I)/DTFA(I))+DTM(I,K,II)*DLT
				!Here was where wq variables updated! Hooray - Now in wqm_main
!
				!      IF(AMOD(ELTMS,DLT).eq.0) THEN
!
					C1 (I, K, II) = (C2(I, K, II)-XFLUX(I, K, II)*TMP1(I)) * &
              & TMP2 (I)!+ DTM (I, K, II) * DLT
!

			     !Wen Long: here TMP1 (I) = DLT / DT (I) / ART1 (I)
			 	 !Wen Long: here TMP2 (I) = DT (I) / DTFA (I)

				!wen long:
				!DTM should have unit of concentration/sec
				!DLT should have unit of sec, which is the time step
				!cocnentration for each constituent is different,in general g/m^3
				!
				!
				!      ELSE
				!          	C1(I,K,II)=(C2(I,K,II)-XFLUX(I,K,II)*TMP1(I))*TMP2(I)
				!      ENDIF
!
				!ORI        C1(I,K,II)=(C2(I,K,II)-XFLUX(I,K,II)/ART1(I)*(DLT/DT(I)))*   &
				!                         (DT(I)/DTFA(I))+DTM(I,K,II)*DLT
				C2F (I, K, II) = Max (C1(I, K, II), 0.0)
!
				If (II .Eq. 4 .Or. II .Eq. 5 .Or. II .Eq. 6) C2F (I, K, &
              & II) = Max (C1(I, K, II), 0.003)!make sure there are seeds for ALG1,2 and 3
!
				!LB: save flux of horiz,vert adv + horiz diffusion in term ADVFLUX in units mass/m3/d
				auxLBadv = - XFLUX (I, K, II) * TMP1 (I) * TMP2 (I)
				auxLBadv = auxLBadv / DLT * 86400.0
				!ADVFLUX(I, K, II) =  auxLBadv !LB: snapshot
				!ADVFLUX (I, K, II) = ADVFLUX (I, K, II) + auxLBadv !LB:accumulated
				End Do
         End Do !End of I loop
      End Do !End of JCON  loop (i.e. II)

  !!Test for snapshot fluxes
  !IF(MSR)WRITE(*,*)'LBnote: DICADV(1,1)=',ADVFLUX(1, 1, 33)
  !IF(MSR)WRITE(*,*)'LBnote: should be=',-XFLUX(1,1,33)*TMP1(1)*TMP2(1)/DLT*86400.0
  !
!
!
  !   !LB: the following DO loop is analogous to Salinity adjustment in HYD code (adjust_ts.F)
  !   !    to keep S at mouth of rivers from being too low. Done for TALK to test in case where TALK resembles S
  !If (NUMPNT > 0) THEN
  !  II = 34
  !   DO I=1,NUMPNT
  !      J = INOPNT(I)
  !      DO K=1,KBM1
  !             C2F(J,K,II) = MAX(C2F(J,K,II),PWQDIS(I, II))
  !              !!S1(J,K) = MAX(S1(J,K),SDIS(I))
  !      END DO
  !   END DO
  !End If
!
!
    If (NUMPNT > 0) DEALLOCATE (PQDIS, PWQDIS)
    IF(NBTX_PNT > 0) DEALLOCATE (TXCFQDIS, TXFQDIS)
    IF(TCR_PNT > 0) DEALLOCATE (BCTCFQDIS, BCTFQDIS)
      Deallocate (XFLUX)
      Deallocate (XFLUX_ADV)
      Return
End Subroutine ADV_WQM
!==============================================================================!
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
!
