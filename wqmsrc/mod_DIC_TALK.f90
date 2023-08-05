Module MOD_DIC_TALK
!
      Use MOD_PREC, Only: SP
      Use MOD_CONTROL, Only: MSR
!
      Use MOD_LIMS, Only: MLOC, KBM1, MTLOC
      Implicit None
!
Contains
  !==============================================================================|
  !    This file has the following subroutines:                                  !
  !                                                                              !
  !   PCO2AIR_READ_INITIAL: Read atmospheric pCO2 data (initial record)          !
  !                and the choice of parameterization to compute air-sea         !
  !                exchange of O2 and CO2                                        !
  !   TOTINC      : Calculation of changes in TDIC (DTTIC)                        !
  !   ALKALINE    : Calculation of changes in TALK (DTALK)                       !
  !                                                                              !
  !   Laura Bianucci, Feb 2015 - laura.bianucci@pnnl.gov                         !
  !   Wen Long added wet_dry, Oct, 2016. wen.long@pnnl.gov						 !
  !==============================================================================|
!
  !************************************************************************
  !**          S U B R O U T I N E   P C O 2 A I R _ R E A D             **
  !************************************************************************
!
      Subroutine PCO2AIR_READ_INITIAL
!
         Use MOD_SIZES, Only: MGL
         Use MOD_CONTROL, Only: PAR, SERIAL
		 USE MOD_FILEINFO, ONLY: AIRC
!
         Use MOD_CO2SYS, Only: K0, FugFac, pCO2atm, AIRSEA_OPTION, &
        & MAPCO2ATM, UNITCO2ATM, NXPCO2, pco2atmNX, &       !CO2star_sat,       &!
		& JDstart1
!f90pprUSE MOD_CO2SYS, ONLY:   K0,                &
!f90ppr  FugFac,            &
!f90ppr  !CO2star_sat,       &!
!f90pprpCO2atm,           &
!f90ppr  AIRSEA_OPTION,     &
!f90ppr  MAPCO2ATM,         &
!f90ppr  UNITCO2ATM,        &
!f90ppr  NXPCO2,            &
!f90ppr  pco2atmNX
!
!
!
!
         Use MOD_PAR
!
         Implicit None

         Integer :: I
         Real (SP), Allocatable :: RTMP21 (:)
    ! Read file

         Open (AIRC, File='inputs/input_pCO2atm.dat', Status='OLD')

    ! What parameterization do we use for air-sea exchange of O2 and CO2?     !1=reareation,  2=Wanninkhof (1992), 3=Nightingale (20
         Read (AIRC, 1009) AIRSEA_OPTION
         If (MSR) write (*,*) 'Parameterization air-sea exchange: (1=re&
        &areation, 2=W92, 3=N00, 4=Ho06, 5=W12, 6=PNNL)'
         If (MSR) write (*,*) '  --> AIRSEA_OPTION=', AIRSEA_OPTION

    ! Is atm CO2 uniform in space or varies?  or does it vary with time? NOTE: it can only vary with time OR space, not both
         Read (AIRC, 1010) MAPCO2ATM
!
    ! Are we providing pCO2atm or fCO2atm?
         Read (AIRC, 1011) UNITCO2ATM
!
         If (MAPCO2ATM == 'UNIFORM') Then
            Read (AIRC, 1030) pCO2atm (1)
            NXPCO2 = - 999.9
!
            Do I = 0, MTLOC
               pCO2atm (I) = pCO2atm (1)
            End Do
!
         Else If (MAPCO2ATM == 'TIMEVAR') Then
            Read (AIRC, 1031) NXPCO2, pco2atmNX
!
            Do I = 0, MTLOC
               pCO2atm (I) = pco2atmNX
            End Do
!
         Else !spatial varying -LB: this part has not been debugged yet
            Allocate (RTMP21(MGL))
            RTMP21 = 0.0
!
            Do I = 1, MGL
               Read (AIRC, 1030) RTMP21 (I)
            End Do
!
            If (SERIAL) Then
               pCO2atm = RTMP21
            End If
            If (PAR) Then
               Do I = 1, MLOC
                  pCO2atm (I) = RTMP21 (NGID(I))
               End Do
               Do I = 1, NHN !LB: I dont think pCO2atm is used in halo nodes,
			                 !but I leave it for the sake of completeness
                  pCO2atm (I+MLOC) = RTMP21 (HN_LST(I))
               End Do
            End If
            Deallocate (RTMP21)
         End If

         Read (AIRC, 1032) JDstart1(1), JDstart1(2)
         If (MSR) write (*,*) '  JDstart1=', JDstart1(1), JDstart1(2)


!
    ! LB: time dependence of PCO2atm --> not implemented yet
!
!!!NOW DONE IN TOTINC
    !! Transform pCO2atm to CO2star_sat:  CO2star_sat = K0 * fCO2atm = K0 * pCO2atm * FugFac    [K0 units: mol/kg-SW/atm]
    !
    !     IF (UNITCO2ATM == 'pCO2') THEN  !if we provided pCO2atm in microatm or ppmv
    !       DO I=0,MLOC
    !         CO2star_sat(I) = K0(I,1) * pCO2atm(I) * FugFac(I,1)   !micromol/kg-SW --> assume rho=1000kg/m3 --> mmol/m3
    !       END DO
    !     ELSE                            !if we provided fCO2atm in microatm or ppmv
    !       DO I=0,MLOC
    !         CO2star_sat(I) = K0(I,1) * pCO2atm(I)   !micromol/kg-SW --> assume rho=1000kg/m3 --> mmol/m3
    !       END DO
    !     END IF
    !     !IF(NLID(759) /= 0) WRITE(*,*)'LBnote mod: K0(759,1)=',K0(NLID(759),1),' pCO2atm(759)=',pCO2atm(NLID(759))
    !     WRITE(*,*)'LBnote mod: K0(1,1)=',K0(1,1),' pCO2atm(1)=',pCO2atm(1)
!
    !***** Input FORMAT statements
!
1009     Format (/ // (I1))
1010     Format (A7)
1011     Format (A4)
1030     Format (F8.0)
1031     Format (F8.1, F8.3)
1032     Format (F8.0, F8.0) !AN
!
         Return
      End Subroutine PCO2AIR_READ_INITIAL
!
  !************************************************************************
  !**                 S U B R O U T I N E   T O T I N C                  **
  !************************************************************************
!
      Subroutine TOTINC (DTTIC)
!
         Use MOD_HYDROVARS, Only: DZ,DZ2D, D, RHO !LB added RHO Jan 8 2016
!
    USE MOD_WQM, ONLY :        &!
         B1,             &!
         B2,             &!
         BM1,            &!
         BM2,            &!
         DOXG,           &!
         FCLD1,          &!
         FCLD2,          &!
         FCLDP,          &!
         FCLP1,          &!
         FCLP2,          &!
         FCLPP,          &!
         FCRD1,          &!
         FCRD2,          &!
         FCRDP,          &!
         FCRP1,          &!
         FCRP2,          &!
         FDOP,           &!
         KHR1,           &!
         KHR2,           &!
         P1,             &!
         P2,             &!
         PR1,            &!
         PR2,            &!
         PRSP1,          &!
         PRSP2,          &!
         DENIT,          &!
         KHODOC,         &!
         MNLLDOC,        &!
         MNLRDOC,        &!
         SALT,           &!
         T,              &!
         AREAR,           &!
         BREAR,           &!
         CREAR,           &!
         WMS,             &
         NT,              &
         FTCOD,           &!
         KHOCOD,          &
         BENTHIC_FLUXES,  &
		 JDAY,            &! AN
     & SAV_CALC,      & ! Laki for SAV-DIC interaction
		 CSODflux,        &
         COD
!
!
         USE MOD_ZOOP, ONLY: AOCRSZ, AOCRLZ, DOSZ, DOLZ, B1SZ,B2SZ, &
                              & B1LZ,B2LZ ! AN activates it for zoopl, Laki added zooplankton predation
!
         Use MOD_CO2SYS, Only: CO2star_sat, pCO2atm, CO2star_surf, &
        & AIRSEA_OPTION, UNITCO2ATM, K0, FugFac, DICUPT, DICBMP, &
        & DICPRD, DICMNL, DICDEN, DICGAS, DICSED, &         !DICNIT,         &!
        & JDstart1
!f90pprUSE MOD_CO2SYS, ONLY:          &
!f90ppr  CO2star_sat,    &
!f90ppr  pCO2atm,        &
!f90ppr  CO2star_surf,   &
!f90ppr  AIRSEA_OPTION,  &
!f90ppr  UNITCO2ATM,     &
!f90ppr  K0,             &
!f90ppr  FugFac,         &
!f90ppr  DICUPT,         &
!f90ppr  DICBMP,         &
!f90ppr  DICPRD,         &
!f90ppr  DICMNL,         &
!f90ppr  DICDEN,         &
!f90ppr  !DICNIT,         &!
!f90ppr  DICGAS,         &
!f90ppr  DICSED
!
         Use MOD_WQMINIT, Only: COD_CALC, ZOO_CALC !AN add ZOO_CALC
         USE MOD_SAV, ONLY : BM_LEAF, LEAF, ACDWSAV,PLEAF,SAVCELL,NSAVCELL     ! Laki added for SAV

         Implicit None
!
         Integer :: I, K, SAVID,II
!
         Real (SP) :: CP1, CP2, FRDO1, FRDO2, DOR1, DOR2, DOP1, DOP2, &
        & CPCOD, CPSAV
		!REAL(SP) :: CP3, FRDO3, DOR3, DOP3
         Real (SP) :: FLUXCO2, TDOS, RNU, KRDO, KRCO2, FACTOR, Sc !parameters in air-sea CO2 flux calculation
         Real (SP), Dimension (0:MTLOC, KBM1) :: DTTIC
         Real (SP) :: auxLB
!
         Real (SP) :: RCCO
		 Real (SP) :: BENDIC !mmoleC/m^3/day
!
         DTTIC = 0.0
!
    !**LB note: for the sake of completeness, I coded (but commented out)
    !           all equations related to Algae 3
         CPSAV = 0.0
         Do K = 1, KBM1
            Do I = 1, MLOC
!
          !********* Uptake
               CP1 = P1 (I, K) * B1 (I, K)   !  1/d * gC/m3 =gC/m3/d
               CP2 = P2 (I, K) * B2 (I, K)
          !    CP3    = P3(I,K)*B3(I,K)
          !--------------------- uptake by the SAV ----------------------------------
          IF((SAV_CALC) .AND. (K<=6)) THEN
               CPSAV = 0.0
               DO II=1,NSAVCELL
                  IF(I == SAVCELL(II)) THEN
                     CPSAV = 0.14*PLEAF(I,1)*LEAF(I,1)/(D(I)*DZ2D(I,K))   ! gC/m3   ! Added by Laki
                     EXIT              ! 0.14 was added as temporary solution to account DIC production due to respiration by SAV
                  END IF
               END DO
          ELSE
               CPSAV = 0.0
          END IF
          !--------------------------------------------------------------------------
          !********* Respiration
          !-- Fracion of losses by basal metabolism and photoresp that go to TDIC
               FRDO1 = (1.-FCLD1-FCRD1-FCLP1-FCRP1) * DOXG (I, K) / &
              & (KHR1+DOXG(I, K))
               FRDO2 = (1.-FCLD2-FCRD2-FCLP2-FCRP2) * DOXG (I, K) / &
              & (KHR2+DOXG(I, K))
          !    FRDO3  = (1.-FCLD3-FCRD3-FCLP3-FCRP3)*DOXG(I,K)/(KHR3+DOXG(I,K))
               DOR1 = (P1(I, K)*PRSP1+BM1(I, K)) * FRDO1 * B1 (I, K)
               DOR2 = (P2(I, K)*PRSP2+BM2(I, K)) * FRDO2 * B2 (I, K)
          !    DOR3   = (P3(I,K)*PRSP3+BM3(I,K))*FRDO3*B3(I,K)
!
          !-- Fracion of losses by predation that go to TDIC
               DOP1 = FDOP * (PR1 (I, K)+B1SZ(I, K)+B1LZ(I, K)) * DOXG (I, K) / (KHR1+DOXG(I, &
              & K))
               DOP2 = FDOP * (PR2 (I, K)+B2SZ(I, K)+B2LZ(I, K)) * DOXG (I, K) / (KHR2+DOXG(I, &
              & K))
          !    DOP3   = FDOP*PR3(I,K)*DOXG(I,K)/(KHR3+DOXG(I,K))
!
!
               If (COD_CALC) Then
             !-- Production of TDIC by the oxydation of sulfide to sulfate (?) (COD)
                  CPCOD = DOXG (I, K) / (KHOCOD+DOXG(I, K)) * FTCOD (I, &
                 & K) * COD (I, K) * 12. / 64. ! CHANGE O2 EQUIVALENTS TO C
               End If !LB: I don't really understand this conversion, but we don't really use COD

				IF(D(I) > 0.0_SP)THEN

          !!LB: snapshot of fluxes at given time
          !DICUPT(I,K) = (-CP1-CP2)/12.*1000.0  !TDIC uptake mmolC/m3/day
          !DICBMP(I,K) = (DOR1+DOR2)/12.*1000.0  !TDIC source by basal metabolism and photorespiration
          !DICPRD(I,K) = (DOP1+DOP2)/12.*1000.0  !TDIC source by predation
          !DICMNL(I,K) = (MNLLDOC(I,K) +MNLRDOC(I,K))/12.*1000.0  !TDIC source by mineralization of LDOC and RDOC
          !DICDEN(I,K) = DENIT(I,K)/12.*1000.0  !TDIC source by water column denitrification
          !LB: accumulate fluxes per DThistoryFile(t_his_dlt) period (integration will be finalized in wqm_main.F, wqm_output subrou
          IF(SAV_CALC) THEN
               DICUPT (I, K) = DICUPT (I, K) + (-CP1-CP2-CPSAV) / 12. * &
              & 1000.0 !TDIC uptake mmolC/m3/d
          ELSE
               DICUPT (I, K) = DICUPT (I, K) + (-CP1-CP2) / 12. * &
              & 1000.0 !TDIC uptake mmolC/m3/d
          END IF
               DICBMP (I, K) = DICBMP (I, K) + (DOR1+DOR2) / 12. * &
              & 1000.0 !TDIC source by basal metabolism and photorespiration
               DICPRD (I, K) = DICPRD (I, K) + (DOP1+DOP2) / 12. * &
              & 1000.0 !TDIC source by predation
               DICMNL (I, K) = DICMNL (I, K) + (MNLLDOC(I, &
              & K)+MNLRDOC(I, K)) / 12. * 1000.0 !TDIC source by mineralization of LDOC and RDOC
               DICDEN (I, K) = DICDEN (I, K) + DENIT (I, K) / 12. * &
              & 1000.0 !TDIC source by water column denitrification
	      !
          !********* Change in TIC
          !
	           If (ZOO_CALC) Then
          !DTTIC(I,K) = (-CP1-CP2 +DOR1+DOR2 +DOP1+DOP2 +CPCOD        &   !-CP3+DOR3+DOP3 removed by LB
          !               + MNLLDOC(I,K) +MNLRDOC(I,K)                &
          !               + DENIT(I,K)                                &   !LB note: DENIT was missing in C.Cerco's code
          !               + DOSZ(I,K)/AOCRSZ+DOLZ(I,K)/AOCRLZ)        &
          !            /12./86400.
			       DTTIC(I,K) = (-CP1-CP2-CPSAV +DOR1+DOR2 +DOP1+DOP2    &   !-CP3+DOR3+DOP3 removed by LB
                  & + MNLLDOC(I,K) +MNLRDOC(I,K)                &
                  & + DENIT(I,K)                                &   !LB note: DENIT was missing in C.Cerco's code
                  & + DOSZ(I,K)/AOCRSZ+DOLZ(I,K)/AOCRLZ)        &   !AN Add for Zoopl
                  & /12./86400. * 1000.0
			   Else
                   DTTIC (I, K) = (-CP1-CP2-CPSAV+DOR1+DOR2+DOP1+DOP2+MNLLDOC(I, &
                  & K)+MNLRDOC(I, K)+DENIT(I, K)) / 12. / 86400. * 1000.0
			       !LB note: also added *1000 to get units of mmol/m3/s (instead of mol/m3/s)
			   End If
!
               If (COD_CALC) Then
                  DTTIC (I, K) = DTTIC (I, K) + CPCOD / 12. / 86400. * &
                 & 1000.0
               End If


          !
          !LB Jan 25 2016: Do not include nitrification as a sink of TDIC. Rationale is that we are implicitly assuming that
          !                bacterial biomass is constant, eg nitrification = -kn2 * Bn * NH4 = -(kn2 * Bn) * NH4 = -kn1 * NH4.
          !                where kn2 = a second-order rate and Bn = the biomass of the nitrifiers. This has been an implicit
          !                assumption since Streeter and Phelps. (From Steve Chapra's email to Greg Pelletier)
          !
          !!********* Nitrification  14 g N/mol N, 11 mol N/mol C  !LB: where does 11 molN/molC come from?
          !     !LB: the ratio comes from a table in Carpenter & Capone's "N in the marine environment book,
          !     !    which cites Wezernak and Gannon (1967) and Billen (1976) (info from Carl Cerco).
          !          DTTIC(I,K) = DTTIC(I,K) - NT(I,K)/14./11./86400.*1000.0  !NT is in gN/m3/d
          !
          !          !DICNIT(I,K) = - NT(I,K)/14./11.*1000.0  !TDIC uptake by nitrification  mmolC/m3/day
          !          DICNIT(I,K) = DICNIT(I,K)- NT(I,K)/14./11.*1000.0  !TDIC uptake by nitrification  mmolC/m3/day

				ENDIF
            End Do
         End Do
!
!
    !********  Atmospheric Exchange
!
    ! Transform pCO2atm OR fCO2atm to CO2star_sat:  CO2star_sat = K0 * fCO2atm = K0 * pCO2atm * FugFac    [K0 units: mol/kg-SW/atm]
         If (UNITCO2ATM == 'pCO2') Then !if we provided pCO2atm in microatm or ppmv
            Do I = 1, MLOC
			!CO2star_sat(I) = K0(I,1) * pCO2atm(I) * FugFac(I,1)   !micromol/kg-SW --> assume rho=1000kg/m3 --> mmol/m3!LB commented o
               CO2star_sat (I) = K0 (I, 1) * pCO2atm (I) * FugFac (I, &
              & 1) * RHO (I, 1) * 1.E-3_SP !mmol/m3
            End Do
         Else !if we provided fCO2atm in microatm or ppmv
            Do I = 1, MLOC
			   !CO2star_sat(I) = K0(I,1) * pCO2atm(I)   !micromol/kg-SW --> assume rho=1000kg/m3 --> mmol/m3!LB commented on 8jan2016			   !CO2star_sat(I) = K0(I,1) * pCO2atm(I) * RHO(I,1) * 1.E-3_SP  !mmol/m3 !!LB: for some reason, this line doesn't give a cor
               auxLB = K0 (I, 1) * pCO2atm (I) * RHO (I, 1) * 1.E-3_SP
               CO2star_sat (I) = auxLB

            End Do
         End If

         If (AIRSEA_OPTION == 1) Then !using AREAR,BREAR,and CREAR
!
            FACTOR = AREAR * (BREAR*WMS) ** CREAR
            Do I = 1, MLOC
               TDOS = T (I, 1)
               RNU = 0.54 + 0.7 * TDOS / 30.0 - 0.07 * SALT (I, 1) / &
              & 35.0
			 ! KRDO      = 0.157*RNU*(1.5*WMS)**1.5
               KRDO = FACTOR * RNU !up to here, same as in calculation of air-sea O2 fluxes (m/day)
               KRCO2 = (32.0/44.0) ** 0.25 * KRDO !conversion of oxygen reareation into CO2 reareagion rate by Chapra (1997)

				IF(D(I) > 0.0_SP)THEN
					FLUXCO2 = KRCO2 / (D(I)*DZ2D(I,1)) * &
              & (CO2star_sat(I)-CO2star_surf(I))

				ELSE
					FLUXCO2=0.0_SP
				ENDIF
!
               DTTIC (I, 1) = DTTIC (I, 1) + FLUXCO2 / 86400.0
			   !DICGAS(I,1) = FLUXCO2  !TDIC flux from air-sea gas exchange, snapshot  mmolC/m3/d
               DICGAS (I, 1) = DICGAS (I, 1) + FLUXCO2 !TDIC flux from air-sea gas exchange, accumulated
            End Do
!
         !Else
         Else If (AIRSEA_OPTION>1 .AND. AIRSEA_OPTION<=5) Then
!
            If (AIRSEA_OPTION == 2) Then !Wanninkhof (1992)
               FACTOR = 0.31 * WMS ** 2.0 !0.31 for winds in steady staate;  0.39 for long-term averaged winds
            Else If (AIRSEA_OPTION == 3) Then !Nightingale (2000)
               FACTOR = (0.33*WMS+0.222*WMS**2.0)
            Else If (AIRSEA_OPTION == 4) Then !Ho (2006)
               FACTOR = 0.266 * WMS ** 2.0
            Else If (AIRSEA_OPTION == 5) Then !Wanninkhof et al (2013): wind in equation is <U^2>,meaning the time mean of U^2
               FACTOR = 0.251 * WMS ** 2.0 !over the time interval of flux determination (they compute monthly fluxes with 6hourly wind data)

			   !FACTOR = AREAR * (BREAR*WMS) ** CREAR
			   !equivalent to AREAR = 0.251, BREAR=1.0, CREAR=2.0

            End If
!
            Do I = 1, MLOC
               Sc = 2073.1 - 125.62 * T (I, 1) + 3.6276 * T (I, 1) ** &
              & 2.0 - 0.043219 * T (I, 1) ** 3.0 !Schmidt number for CO2 (W92)
				!Sc = 2116.8 - 136.25 * T(I,1) + 4.7353 * T(I,1)**2.0          &
                !     - 0.092307 * T(I,1)**3.0 + 0.0007555 * T(I,1)**4.0     !Schmidt number for CO2 (W14)
!
               KRCO2 = FACTOR * Sqrt (660.0/Sc)!gas transfer velocity in cm/hr
               KRCO2 = KRCO2 * 24.0 / 100.0 !m/day

				IF(D(I) > 0.0_SP)THEN
!
					FLUXCO2 = KRCO2 / (D(I)*DZ2D(I,1)) * &
              & (CO2star_sat(I)-CO2star_surf(I))!mmolC/m3/d
				ELSE
					FLUXCO2=0.0_SP
				ENDIF
!
               DTTIC (I, 1) = DTTIC (I, 1) + FLUXCO2 / 86400.0 !mmolC/m3/sec
			   !DICGAS(I,1) = FLUXCO2  !TDIC flux from air-sea gas exchange, snapshot   !mmolC/m3/day
               DICGAS (I, 1) = DICGAS (I, 1) + FLUXCO2 !TDIC flux from air-sea gas exchange, accumulated
            End Do
		 Else If (AIRSEA_OPTION == 6) Then
		    FACTOR = AREAR * (BREAR*WMS) ** CREAR
!
			Do I = 1, MLOC

			If (JDAY <= JDstart1(1) .OR. JDAY >= JDstart1(2)) Then
               TDOS = T (I, 1)
               RNU = 0.54 + 0.7 * TDOS / 30.0 - 0.07 * SALT (I, 1) / &
              & 35.0
			 ! KRDO      = 0.157*RNU*(1.5*WMS)**1.5
               KRDO = FACTOR * RNU !up to here, same as in calculation of air-sea O2 fluxes (m/day)
               KRCO2 = (32.0/44.0) ** 0.25 * KRDO !conversion of oxygen reareation into CO2 reareagion rate by Chapra (1997)

			Else If (JDAY > JDstart1(1) .AND. JDAY < JDstart1(2)) Then
               Sc = 2073.1 - 125.62 * T (I, 1) + 3.6276 * T (I, 1) ** &
              & 2.0 - 0.043219 * T (I, 1) ** 3.0 !Schmidt number for CO2 (W92)
				!Sc = 2116.8 - 136.25 * T(I,1) + 4.7353 * T(I,1)**2.0          &
                !     - 0.092307 * T(I,1)**3.0 + 0.0007555 * T(I,1)**4.0     !Schmidt number for CO2 (W14)
!
               KRCO2 = FACTOR * Sqrt (660.0/Sc)!gas transfer velocity in cm/hr
               KRCO2 = KRCO2 * 24.0 / 100.0 !m/day
		    End If
!
				IF(D(I) > 0.0_SP)THEN
!
					FLUXCO2 = KRCO2 / (D(I)*DZ(1)) * &
              & (CO2star_sat(I)-CO2star_surf(I))!mmolC/m3/d
				ELSE
					FLUXCO2=0.0_SP
				ENDIF
!
               DTTIC (I, 1) = DTTIC (I, 1) + FLUXCO2 / 86400.0 !mmolC/m3/sec
			   !DICGAS(I,1) = FLUXCO2  !DIC flux from air-sea gas exchange, snapshot   !mmolC/m3/day
               DICGAS (I, 1) = DICGAS (I, 1) + FLUXCO2 !DIC flux from air-sea gas exchange, accumulated
            End Do
!
!
         End If

	  !******** Sediment flux
         If ( .Not. BENTHIC_FLUXES) Then
       !---Use QUAL2K approach:    DICflux    =    CSOD    *    RCCO    *  DZbottomLayerWaterColumn^(-1)
       !                         mmolC/m^3/d     gO2/m^2/d    mmolC/gO2        1/m

            RCCO = 1.0_SP / 2.667_SP !gC/gO2
            RCCO = RCCO / 12.0_SP    !molC/gO2  (1 molC = 12.0107 gC) LB: USE 12 instead of 12.0107 because I also round up gN:molN to 14.0
            RCCO = RCCO * 1000.0_SP !mmolC/gO2
            Do I = 1, MLOC

			IF(D(I) > 0.0_SP)THEN
               BENDIC = CSODflux (I) * RCCO / (D(I)*DZ2D(I,KBM1))	!CSODflux = CSOD + XJCNO3 in mod_sed.F

			ELSE
			   BENDIC=0.0_SP
			ENDIF
               DTTIC (I, KBM1) = DTTIC (I, KBM1) + BENDIC / 86400.0_SP
			   !DICSED(I,KBM1) = BENDIC  !TDIC flux from sediments, snapshot
               DICSED (I, KBM1) = DICSED (I, KBM1) + BENDIC 		!TDIC flux from sediments, accumulated
            End Do

!
!
         End If
!
    !!NOTE: so far, if BENTHIC_FLUXES=true, we yet don't have BENDIC addition (no TDIC from sediments)
!

		 DO I=1, MLOC  !Set the DTTIC DICSED, DICGAS etc to zero for dry areas
			IF(D(I) <= 0.0_SP)THEN
				DO K=1,KBM1

					DTTIC(I,K)=0.0_SP 					!Wen Long: killed change to DTTIC
					DICSED(I,K)=DICSED(I,K)+0.0_SP
					DICGAS(I,K)=DICGAS(I,K)+0.0_SP
					DICUPT (I, K) =DICUPT(I,K)+ 0.0_SP
					DICBMP (I, K) =DICBMP(I,K)+ 0.0_SP
					DICPRD (I, K) =DICPRD(I,K)+ 0.0_SP
					DICMNL (I, K) =DICMNL(I,K)+ 0.0_SP
					DICDEN (I, K) =DICDEN(I,K)+ 0.0_SP

				ENDDO
			ENDIF
		 ENDDO
         Return
      End Subroutine TOTINC
!
!
  !************************************************************************
  !**                 S U B R O U T I N E   A L K A L I N                **
  !************************************************************************
!
      Subroutine ALKALIN (DTTALK)
!
         Use MOD_HYDROVARS, Only: DZ,DZ2D, D !CURRENT DEPTH
!
         Use MOD_WQM, Only: B1, B2, ANC1, ANC2, P1NNF, P2, PN1, PN2, &
        & NT, DENNO3, MNLRDON, MNLLDON, BENNH4, BENNO3, PRECIP, &
        & ATMOS_LOADS, ATMNH4, ATMNO3
!
         Use MOD_CO2SYS, Only: ALKNH4, ALKNO3, ALKNIT, ALKDEN, ALKREM, &
        & ALKNH4SED, ALKNO3SED
!
!
         Implicit None
!
    !     REAL(SP) NH4A1, NH4A2, NH4A3, NO3A1, NO3A2, NO3A3
    !     REAL(SP) NH4SAV, NO3SAV
    !REAL(SP):: ALKNH4,ALKNO3,ALKNIT,ALKDEN,ALKREM
         Integer :: I, K
!
         Real (SP), Dimension (0:MTLOC, KBM1) :: DTTALK
         Real (SP) :: alk_nh4, alk_no3, alk_nit, alk_den, alk_rem
         Real (SP) :: alks_nh4sed, alks_no3sed
         Real (SP) :: NH4A1, NH4A2, NO3A1, NO3A2
!
         DTTALK = 0.0
!
         Do K = 1, KBM1
            Do I = 1, MLOC

			IF(D(I) > 0.0_SP)THEN
		  !
          !********* Algal nitrogen utilization
!
               NH4A1 = PN1 (I, K) * P1NNF (I, K) * ANC1 * B1 (I, K)
               NH4A2 = PN2 (I, K) * P2 (I, K) * ANC2 * B2 (I, K)
               NO3A1 = (1.-PN1(I, K)) * P1NNF (I, K) * ANC1 * B1 (I, K)
               NO3A2 = (1.-PN2(I, K)) * P2 (I, K) * ANC2 * B2 (I, K)
!
          !!Code as in Carl Cerco's:       (
          !!          ALKNH4 = -15.*(NH4A1+NH4A2)/16./14.    !+NH4A3
          !!           ALKNO3 =  17.*(NO3A1+NO3A2)/16./14.    !+NO3A3
!!!Fractions 15/16 and 17/16  come from Brewer and Goldman (1976) (see Cerco et al 2013).
!
          ! Wolf-Gladrow et al (200), using explicit conservative definition of TA (analogous to Dickson's TA definition)
          !    considers that there is 1 mol chance in TA per mole consumed of NH4 or NO3,
          !     i.e,         ALKNH4 =-(NH4A1+NH4A2)/14.  ,  ALKNO3 =  (NO3A1+NO3A2)/14.
          ! Then, there is no need to re-compute NH4A*,NO3A* -->computed in wqm_kin.F and stored as ALGNH4, ALGNO3
               alk_nh4 = - (NH4A1+NH4A2) / 14. * 1000.0 !ALGNH4 is the balance between NH4 uptake and sources by Predation and photorespiration
               alk_no3 = (NO3A1+NO3A2) / 14. * 1000.0
!
          !********* Nitrification  14 g N/mol N
!
               alk_nit = - 2. * NT (I, K) / 14. * 1000.0
!
          !********* Denitrification   !this one was missing in Cerco's model
          ! LB note: in wqm_in.F: DENNO3(I,K)  = -ANDC*DENIT(I,K) is the loss of NO3 due to denit in the water column
               alk_den = - DENNO3 (I, K) / 14. * 1000.0 !OR ANDC*DENIT(I,K)/14.
!
          !********* Remineralization of organic DON   !this one was missing in Cerco's model
               alk_rem = (MNLRDON(I, K)+MNLLDON(I, K)) / 14. * 1000.0

          !********* Change in TALK
!
				!!MAKE changes in TALK due to bio = 0
				!alk_nh4 = 0.0
				!alk_no3 = 0.0
				!alk_nit = 0.0
				!alk_rem = 0.0
				!alk_den = 0.0
!
		ELSE
				alk_nh4 = 0.0_SP
				alk_no3 = 0.0_SP
				alk_nit = 0.0_SP
				alk_rem = 0.0_SP
				alk_den = 0.0_SP
		ENDIF

               DTTALK (I, K) = &
              & (alk_nh4+alk_no3+alk_nit+alk_den+alk_rem) / 86400.
!
          !!LB: snapshot of fluxes
          !          ALKNH4(I,K) = alk_nh4
          !          ALKNO3(I,K) = alk_no3
          !          ALKNIT(I,K) = alk_nit
          !          ALKDEN(I,K) = alk_den
          !          ALKREM(I,K) = alk_rem
          !LB: accumulate fluxes per DThistoryFile(t_his_dlt) period (integration will be finalized in wqm_main.F, wqm_output subrou
               ALKNH4 (I, K) = ALKNH4 (I, K) + alk_nh4
               ALKNO3 (I, K) = ALKNO3 (I, K) + alk_no3
               ALKNIT (I, K) = ALKNIT (I, K) + alk_nit
               ALKDEN (I, K) = ALKDEN (I, K) + alk_den
               ALKREM (I, K) = ALKREM (I, K) + alk_rem

            End Do
         End Do
!
    !******** Sediment flux    of NO3- and NH4+
!
         Do I = 1, MLOC


			IF(D(I) > 0.0_SP)THEN

				alks_nh4sed = BENNH4 (I) / (D(I)*DZ2D(I,KBM1)) / 14. * 1000.0 !mmolN/m^3/day     !BENNH4 is in gN/m^2/day
				alks_no3sed = - BENNO3 (I) / (D(I)*DZ2D(I,KBM1)) / 14. * 1000.0 !mmolN/m^3/day     !BENNO3 is in gN/m^2/day

			ELSE
				alks_nh4sed=0.0_SP
				alks_no3sed=0.0_SP
			ENDIF

            DTTALK (I, KBM1) = DTTALK (I, KBM1) + alks_nh4sed / 86400. !mmolN/m^3/sec
            DTTALK (I, KBM1) = DTTALK (I, KBM1) + alks_no3sed / 86400. !mmolN/m^3/sec
!
			!!if(MSR.and.I.eq.3)write(*,*)'LBnoted: TAnh4sed=',alks_nh4sed,' TAno3sed=',alks_no3sed
			!ALKNH4SED(I,KBM1) = alks_nh4sed  !snapshot
			!ALKNO3SED(I,KBM1) = alks_no3sed  !snapshot
            ALKNH4SED (I, KBM1) = ALKNH4SED (I, KBM1) + alks_nh4sed !accumulated
            ALKNO3SED (I, KBM1) = ALKNO3SED (I, KBM1) + alks_no3sed !accumulated
!
			!ALKNH4SED(I,KBM1) = 0.0    !to have benfluxesTA=0 comment lines above (inside DO loop and uncomment these 2 lines)
			!ALKNO3SED(I,KBM1) =  0.0


         End Do
!
    !******** Atm deposition of NO3- and NH4+ : not included yet
         If (ATMOS_LOADS) Then
            Do I = 1, MLOC
				IF(D(I) > 0.0_SP)THEN
					DTTALK (I, 1) = DTTALK (I, 1) + PRECIP * ATMNH4 / &
              & (D(I)*DZ2D(I,1)) / 14. * 1000.0
					DTTALK (I, 1) = DTTALK (I, 1) - PRECIP * ATMNO3 / &
              & (D(I)*DZ2D(I,1)) / 14. * 1000.0
				ELSE
					!Wen Long: If dry, what would atm deposition do? settle into sediments directly?
					!Wen Long: NOT implemented
				ENDIF
            End Do
         End If

         Return
      End Subroutine ALKALIN
!
End Module MOD_DIC_TALK
