!mod_fc.F
!************************************************************************
!**                                                                    **
!**                           FVCOM-ICM_4.0                            **
!**                                                                    **
!**               A Finite Volume Based Integrated Compartment         **
!**                         Water Quality Model                        **      
!**        The original unstructured-grid ICM code was developed by    ** 
!**    the FVCOM development team at the University of Massachusetts   ** 
!**         through a contract with U.S. Army Corps of Engineers       ** 
!**         [Dr. Changsheng Chen (PI), Dr. Jianhua Qi and              ** 
!**                      Dr. Geoffrey W. Cowles]                       **
!**                                                                    **
!**                Subsequent Development and Maintenance by           ** 
!**                   PNNL/UW Salish Sea Modeling Center               **
!**                                                                    **
!**                 Tarang Khangaonkar    :  PNNL (2008 - Present)     **
!**                 Lakshitha Premathilake:  PNNL (2019 - Present)     **
!**                 Adi Nugraha           :  PNNL/UW (2018 - Present)  **
!**                 Kurt Glaesmann        :  PNNL (2008 - Present)     **
!**                 Laura Bianucci        :  PNNL/DFO(2015 - Present)  **
!**                 Wen Long              :  PNNL (2012-2016)          **
!**                 Taeyum Kim            :  PNNL (2008-2011)          **
!**                 Rochelle G Labiosa    :  PNNL (2009-2010)          **
!**                                                                    **
!**                                                                    **
!**                     Adopted from CE-QUAL-ICM  Model                **
!**                           Developed by:                            **
!**                                                                    **
!**             Carl F. Cerco      : Water quality scheme              **
!**             Raymond S. Chapman : Numerical solution scheme         **
!**             Thomas M. Cole     : Computer algorithms & coding      **
!**             Hydroqual          : Sediment compartment              **
!**                                                                    **
!**                    Water Quality Modeling Group                    **
!**                    U.S. Army Corps of Engineers                    **
!**                    Waterways Experiment Station                    **
!**                    Vicksburg, Mississippi 39180                    **
!**                                                                    **
!************************************************************************
!
MODULE SC_TCR
  USE MOD_LIMS, Only: MTLOC, KBM1, KB, MLOC, TCR_PNT, MYID
  USE MOD_PREC, Only: SP
  USE MOD_SIZES, Only: MGL, NGL,NCP,NTXVB,NBTCR 
  USE MOD_WQM, ONLY: SSI, C1, C2, POC_CONCENS, DOC_CONCENS, TcrtOutput, NumTcrOutVar, TcrOutVar,TcrtConW, &
                    & TcrtConW_GL, TcrtConSdm, TcrtConSdm_GL, I0, SALT, T, WMS, &
                    & RdcnCon_W, RdcnCon_AlgW, RdcnCon_ZplW, RdcnCon_SldW, RdcnCon_POCW, RdcnConTW, RdcnConTW_GL, &
                    & RdcnConW_GL, RdcnConAlgW_GL, RdcnConZplW_GL, RdcnConSldW_GL, RdcnConPOCW_GL, &
                    & RdcnConW_S1, RdcnCon_POCS1, RdcnCon_DOCS1, RdcnCon_SldS1, &
                    & RdcnConWS1_GL, RdcnConPOCS1_GL, RdcnConDOCS1_GL, RdcnConSldS1_GL, &
                    & RdcnConW_S2, RdcnCon_POCS2, RdcnCon_DOCS2, RdcnCon_SldS2, &
                    & RdcnConWS2_GL, RdcnConPOCS2_GL, RdcnConDOCS2_GL, RdcnConSldS2_GL, &
                    & RdcnConTS1, RdcnConTS1_GL, RdcnConTS2, RdcnConTS2_GL,SdmHT,JDAY, &
                    & TCRF_INC,TCRF_PNT,TCRF_OBC,TCRF_INC_SD,TcrSdmInitType,TcrSdmUnfm, &
                    & BACTERIAON,RADIONUCON,NOTCRKNTS,NumTcrOutVar,KnBact,C0SalBact, AlphaBact, &
                    & ThetaBact,RnHalfTime,RnKd,VonKrmn,Rgfh,PtcDensity,ErsnCoeff,CrtShear, &
                    & SdmPrst,SPM,WSPM,SedFlux,SusFlux,ULND,VLND,WWLND,SdmBRL,DTSPM, BtmShear

  USE MOD_CONTROL, ONLY: SERIAL, MSR, PAR
  USE MOD_PAR, Only: NGID, NHN, HN_LST, NLID
  USE MOD_TYPES, Only: BC
  USE MOD_BCMAP, ONLY : NUT_TM, IOBCN, I_OBC_N, WQOBC, I_OBC_GL, IOBCN_GL
  USE MOD_BCS, ONLY : TCRPNT_ID, TCR_TM, TCRQDIST, TCRQDIS, TCRCQDIS
  USE MOD_HYDROVARS, ONLY : D
  USE MOD_SED, ONLY : HSED, HSED1

IMPLICIT NONE 
REAL(SP), ALLOCATABLE, DIMENSION (:, :, :) :: RTMPX
INTEGER :: TCCST, I, K, JCON, JJ, JT, JK, J, CNT, NJ
INTEGER,  ALLOCATABLE, DIMENSION (:) :: TEMP, TEMP2, TEMP3
REAL(SP), ALLOCATABLE, DIMENSION (:, :, :) :: ConTcr, ConTcr_GL, DWTCR
REAL(SP), ALLOCATABLE, DIMENSION (:, :) :: ConTcrSdm1, ConTcrSdm2, ConTcrSdm1_GL, ConTcrSdm2_GL, WtcSPM
REAL(SP), ALLOCATABLE, DIMENSION (:) :: SSDL, SSDL_GL, SSIWC_GL, SSD_MSS, SLDMS1, SLDMS2
REAL(SP), ALLOCATABLE, DIMENSION (:) :: TCRHT1, TCRHT2, POCCNS1, POCCNS2, DOCCNS1, DOCCNS2
REAL(SP), ALLOCATABLE, DIMENSION (:,:) :: FDrn_WS1, FPrnSSI_S1, FPrnPOC_S1, FPrnDOC_S1
REAL(SP), ALLOCATABLE, DIMENSION (:,:) :: FDrn_WS2, FPrnSSI_S2, FPrnPOC_S2, FPrnDOC_S2
REAL(SP), ALLOCATABLE, DIMENSION (:,:) :: KPrnPOC_S1, KPrnDOC_S1, KPrnSSI_S1, KPrnPOC_S2, KPrnSSI_S2, KPrnDOC_S2
REAL(SP), ALLOCATABLE, DIMENSION (:) ::  PSRT1, PSRT1_GL, PSRT2, PSRT2_GL
REAL(SP), ALLOCATABLE, DIMENSION (:) :: WSSI12, WPOC12, DFFVS12W, MolWghtCng
REAL(SP), ALLOCATABLE, DIMENSION (:, :, :) :: FPrnSSI_W, FDrn_W
REAL(SP), ALLOCATABLE, DIMENSION (:, :, :) :: FPrnPOC_W, FPrnBOC1_W, FPrnDOC_W, FPrnZOO_W, FPrnBOC2_W
REAL(SP), ALLOCATABLE, DIMENSION (:, :, :) :: KPrnSSI_W, DSTCR
REAL(SP), ALLOCATABLE, DIMENSION (:, :, :) :: KPrnPOC_W, KPrnBOC1_W, KPrnDOC_W, KPrnZOO_W, KPrnBOC2_W

REAL(SP) :: SED_DEN, DFFCOEFS12, BRLVEL, SED_DEN1, SED_DEN2,SdmDryDen
CHARACTER(LEN = 1024) :: FLAG_SD_INI, FLAG_WIND
CHARACTER(LEN = 1024) :: KINETICSWT
REAL(SP) ::TCR_INIT_SD, KD, KBact
INTEGER :: NoRnds
REAL(SP), DIMENSION(12) :: SpAct
REAL(SP), DIMENSION(12) :: DecayConst
LOGICAL :: DstrSdmHT,InitDstrSPM


CONTAINS 


!--------------------------------------------------------------------------------------------
SUBROUTINE BactAlloc

  USE MOD_WQM, ONLY: SPM
IMPLICIT NONE
REAL(SP), ALLOCATABLE, DIMENSION(:,:) :: TEMPBTC
REAL(SP), ALLOCATABLE, DIMENSION(:) :: TEMPBTC1
INTEGER :: end_file,I

!------------------ Allocation of tracer/pollutant related variables ------------------------ 
          ALLOCATE(ConTcr(0:MTLOC,KBM1,NBTCR))
          ConTcr = 0.0
          ALLOCATE(ConTcrSdm1(0:MTLOC,NBTCR))
          ConTcrSdm1 = 0.0
          ALLOCATE(ConTcrSdm2(0:MTLOC,NBTCR))
          ConTcrSdm2 = 0.0

          ALLOCATE(DWTCR(0:MTLOC,KBM1,NBTCR))
          DWTCR = 0.0
          ALLOCATE(DSTCR(0:MTLOC,NBTCR,2))
          DSTCR = 0.0
          ALLOCATE(TcrtConW(0:MTLOC,KBM1,NBTCR))
          TcrtConW = 0.0 
          ALLOCATE(TcrtConSdm(0:MTLOC,NBTCR))
          TcrtConSdm = 0.0

!------------------ Allocation for radionuclides concentrations in each constituents -------------- 
IF(RADIONUCON) THEN
          ALLOCATE(RdcnCon_W(MTLOC,KBM1,NBTCR))
          RdcnCon_W = 0.0 
          ALLOCATE(RdcnCon_AlgW(MTLOC,KBM1,NBTCR))
          RdcnCon_AlgW = 0.0 
          ALLOCATE(RdcnCon_ZplW(MTLOC,KBM1,NBTCR))
          RdcnCon_ZplW = 0.0 
          ALLOCATE(RdcnCon_SldW(MTLOC,KBM1,NBTCR))
          RdcnCon_SldW = 0.0 
          ALLOCATE(RdcnCon_POCW(MTLOC,KBM1,NBTCR))
          RdcnCon_POCW = 0.0

          ALLOCATE(RdcnConW_S1(MTLOC,NBTCR))
          RdcnConW_S1 = 0.0 
          ALLOCATE(RdcnConW_S2(MTLOC,NBTCR))
          RdcnConW_S2 = 0.0 
          ALLOCATE(RdcnCon_POCS1(MTLOC,NBTCR))
          RdcnCon_POCS1 = 0.0 
          ALLOCATE(RdcnCon_POCS2(MTLOC,NBTCR))
          RdcnCon_POCS2 = 0.0 
          ALLOCATE(RdcnCon_DOCS1(MTLOC,NBTCR))
          RdcnCon_DOCS1 = 0.0 
          ALLOCATE(RdcnCon_DOCS2(MTLOC,NBTCR))
          RdcnCon_DOCS2 = 0.0 
          ALLOCATE(RdcnCon_SldS1(MTLOC,NBTCR))
          RdcnCon_SldS1 = 0.0 
          ALLOCATE(RdcnCon_SldS2(MTLOC,NBTCR))
          RdcnCon_SldS2 = 0.0 

          ALLOCATE(RdcnConTW(MTLOC,KBM1))
          RdcnConTW = 0.0 
          ALLOCATE(RdcnConTS1(MTLOC))
          RdcnConTS1 = 0.0 
          ALLOCATE(RdcnConTS2(MTLOC))
          RdcnConTS2 = 0.0
END IF
!-------------------------- Allocation for radionuclide kinetics water column----------------------- 
          ALLOCATE(SSDL(MTLOC))
          SSDL = 0.0
          ALLOCATE(SSDL_GL(MGL))
          SSDL_GL = 0.0
          ALLOCATE(PSRT1(MTLOC))
          PSRT1 = 0.0
          ALLOCATE(PSRT2(MTLOC))
          PSRT2 = 0.0
          ALLOCATE(PSRT1_GL(MGL))
          PSRT1_GL = 0.0
          ALLOCATE(PSRT2_GL(MGL))
          PSRT2_GL = 0.0
          ALLOCATE(SSD_MSS(MTLOC))
          SSD_MSS = 0.0
          ALLOCATE(TCRHT1(MTLOC))
          TCRHT1 = 0.0
          ALLOCATE(TCRHT2(MTLOC))
          TCRHT2 = 0.0

IF(RADIONUCON) THEN
          ALLOCATE(FPrnSSI_W(0:MTLOC,KBM1,NBTCR))
          FPrnSSI_W = 0.0
          ALLOCATE(FPrnPOC_W(MTLOC,KBM1,NBTCR))
          FPrnPOC_W = 0.0
          ALLOCATE(FPrnDOC_W(MTLOC,KBM1,NBTCR))
          FPrnDOC_W = 0.0
          ALLOCATE(FPrnBOC1_W(MTLOC,KBM1,NBTCR))
          FPrnBOC1_W = 0.0
          ALLOCATE(FPrnBOC2_W(MTLOC,KBM1,NBTCR))
          FPrnBOC2_W = 0.0
          ALLOCATE(FPrnZOO_W(MTLOC,KBM1,NBTCR))
          FPrnZOO_W = 0.0
          ALLOCATE(FDrn_W(0:MTLOC,KBM1,NBTCR))
          FDrn_W = 0.0
          ALLOCATE(KPrnSSI_W(MTLOC,KBM1,NBTCR))
          KPrnSSI_W = 0.0
          ALLOCATE(KPrnPOC_W(MTLOC,KBM1,NBTCR))
          KPrnPOC_W = 0.0
          ALLOCATE(KPrnDOC_W(MTLOC,KBM1,NBTCR))
          KPrnDOC_W = 0.0
          ALLOCATE(KPrnBOC1_W(MTLOC,KBM1,NBTCR))
          KPrnBOC1_W = 0.0
          ALLOCATE(KPrnBOC2_W(MTLOC,KBM1,NBTCR))
          KPrnBOC2_W = 0.0
          ALLOCATE(KPrnZOO_W(MTLOC,KBM1,NBTCR))
          KPrnZOO_W = 0.0
!--------------------------- Allocation for radionuclide kinetics for sediment layer ------------------- 
          
          ALLOCATE(FPrnSSI_S1(MTLOC,NBTCR))
          FPrnSSI_S1 = 0.0
          ALLOCATE(FDrn_WS1(0:MTLOC,NBTCR))
          FDrn_WS1 = 0.0
          ALLOCATE(KPrnSSI_S1(MTLOC,NBTCR))
          KPrnSSI_S1 = 0.0
          ALLOCATE(KPrnPOC_S1(MTLOC,NBTCR))
          KPrnPOC_S1 = 0.0
          ALLOCATE(KPrnDOC_S1(MTLOC,NBTCR))
          KPrnDOC_S1 = 0.0
          ALLOCATE(FPrnDOC_S1(MTLOC,NBTCR))
          FPrnDOC_S1 = 0.0
          ALLOCATE(FPrnPOC_S1(MTLOC,NBTCR))
          FPrnPOC_S1 = 0.0

          ALLOCATE(FPrnSSI_S2(MTLOC,NBTCR))
          FPrnSSI_S2 = 0.0
          ALLOCATE(FDrn_WS2(MTLOC,NBTCR))
          FDrn_WS2 = 0.0
          ALLOCATE(KPrnSSI_S2(MTLOC,NBTCR))
          KPrnSSI_S2 = 0.0
          ALLOCATE(KPrnPOC_S2(MTLOC,NBTCR))
          KPrnPOC_S2 = 0.0
          ALLOCATE(KPrnDOC_S2(MTLOC,NBTCR))
          KPrnDOC_S2 = 0.0
          ALLOCATE(FPrnDOC_S2(MTLOC,NBTCR))
          FPrnDOC_S2 = 0.0
          ALLOCATE(FPrnPOC_S2(MTLOC,NBTCR))
          FPrnPOC_S2 = 0.0
!------------------------------------------------------------------------------------------- 
!------------------ concentration of POC and DOC in sediment layers ------------------------

          ALLOCATE(WSSI12(MTLOC))
          WSSI12 = 0.0
          ALLOCATE(WPOC12(MTLOC))
          WPOC12 = 0.0
          ALLOCATE(DFFVS12W(MTLOC))
          DFFVS12W = 0.0
 END IF
          !ALLOCATE(POC_CONCENS(MTLOC,2))
          !POC_CONCENS = 0.0
          !ALLOCATE(DOC_CONCENS(MTLOC,2))
          !DOC_CONCENS = 0.0
          ALLOCATE(POCCNS1(MTLOC))
          POCCNS1 = 0.0
          ALLOCATE(POCCNS2(MTLOC))
          POCCNS2 = 0.0
          ALLOCATE(DOCCNS1(MTLOC))
          DOCCNS1 = 0.0
          ALLOCATE(DOCCNS2(MTLOC))
          DOCCNS2 = 0.0
!-------------------------------------------------------------------------------------------
         IF(RADIONUCON) THEN 
               ALLOCATE (SusFlux(0:MTLOC))
               SusFlux = 0.0
               ALLOCATE (SedFlux(0:MTLOC))
               SedFlux = 0.0
               ALLOCATE (BtmShear(0:MTLOC))
               BtmShear = 0.0
               ALLOCATE (ULND(0:MTLOC))
               ULND = 0.0
               ALLOCATE (VLND(0:MTLOC))
               VLND = 0.0 
               ALLOCATE (WWLND(0:MTLOC))
               WWLND = 0.0
               ALLOCATE (SdmHT(0:MTLOC))
               SdmHT = 0.0
               ALLOCATE (SdmBRL(0:MTLOC))
               SdmBRL = 0.0
               ALLOCATE (DTSPM(0:MTLOC,KBM1))
               DTSPM = 0.0
         END IF

!------------------------------- initialization of tracer in sediment layer ----------------------------
InitDstrSPM = .FALSE.
IF(InitDstrSPM .AND. RADIONUCON) THEN
   OPEN(UNIT = 210, FILE = 'inputs/InitSPM.dat', ACTION = 'READ')
   READ(210,*, IOSTAT = end_file)
   IF(end_file < 0) THEN
   	    WRITE (*,*) "Error occured - No file for initial conditions for toxics in sediments"
   	    STOP
   END IF
   ALLOCATE(TEMPBTC(MGL,KBM1))
   DO I=1, MGL
      DO j=1, KBM1
        READ(210,*) TEMPBTC(I,j)
      END DO
   END DO

   CLOSE(210)
   If (SERIAL) THEN
     SPM = TEMPBTC
   END IF
# if defined(MULTIPROCESSOR)
    IF (PAR) THEN
          DO I = 1, MLOC
            SPM(I,:) = TEMPBTC(NGID(I),:)
          END DO
          DO I = 1, NHN
            SPM(I+MLOC,:) = TEMPBTC(HN_LST(I),:)
          END DO
     END IF
# endif
    DEALLOCATE(TEMPBTC)
END IF
!----------------------------------------------------------------------------------------------------------
DstrSdmHT = .FALSE.
IF(DstrSdmHT .AND. RADIONUCON) THEN 
  OPEN(UNIT = 212, FILE = 'inputs/SdmThickness.dat', ACTION = 'READ')
  READ(212,*, IOSTAT = end_file)
   IF(end_file < 0) THEN
   	    WRITE (*,*) "Error occured - No file for initial conditions for sediment thickness"
   	    STOP
   END IF
   ALLOCATE(TEMPBTC1(MGL))
   DO I=1, MGL
     READ(212,*) TEMPBTC1(I)
   END DO
  CLOSE(212)
# if defined(MULTIPROCESSOR)
    IF (PAR) THEN
          DO I = 1, MLOC
            SdmHT(I) = TEMPBTC1(NGID(I))
          END DO
          DO I = 1, NHN
            SdmHT(I+MLOC) = TEMPBTC1(HN_LST(I))
          END DO
     END IF
# endif  
 DEALLOCATE(TEMPBTC1)
END IF
!-------------------------------------------------------------------------------------------------------- 
      
END SUBROUTINE
!------------------------------------------------------------------------------------------



!--------------------------------------------------------------------------------------------
SUBROUTINE ReadBactData
 IMPLICIT NONE 
 INTEGER :: end_file, I1, NUM_NDS_TC, NTCR_PNT_GL, TCRQTIME, I
 INTEGER, ALLOCATABLE, DIMENSION(:) :: OBCNDS_TCR, TCGL_2_LOC_PNT
 REAL(SP), ALLOCATABLE, DIMENSION(:,:) :: RTEMP, RTEMP2
 REAL(SP), ALLOCATABLE, DIMENSION(:,:,:) :: RTEMP1
 REAL(SP), ALLOCATABLE, DIMENSION(:,:) :: TEMPBTC
 REAL(SP) :: TTIME, FBUnitScale
 CHARACTER(LEN = 64) :: FLGTCR_OUTPUT
 INTEGER :: NumLine

FBUnitScale = 1E+04       ! FB units - cfu/100 ml ---> cfu/m3


HSED1 = 0.1         ! initialization of sediment layer thickness
HSED = 0.9          ! initialization of sediment layer thickness
IF(RADIONUCON) THEN
    SpAct(1) = 3.560E14
    SpAct(2) = 1.657E11
    SpAct(3) = 4.187E13
    SpAct(4) = 2.100E12
    SpAct(5) = 5.111E12
    SpAct(6) = 6.336E08
    SpAct(7) = 1.221E14
    SpAct(8) = 3.839E13
    SpAct(9) = 3.839E13
    SpAct(10) = 6.537E06
    SpAct(11) = 3.203E12

    DO I=1,NoRnds
      DecayConst(I) = LOG(2.0)*(1.0/(RnHalfTime(I)*3.154E7))
    END DO
    SdmHT = 0.1
END IF
!------------------------------- initialization of tracer in sediment layer ----------------------------
 IF(TRIM(TcrSdmInitType) == 'UNIFORM') THEN
   ConTcrSdm1 = TcrSdmUnfm
   ConTcrSdm2 = TcrSdmUnfm
 ELSE
   OPEN(UNIT = 210, FILE = TRIM(TCRF_INC_SD), ACTION = 'READ')
   READ(210,*, IOSTAT = end_file)
   IF(end_file < 0) THEN
   	    WRITE (*,*) "Error occured - No file for initial conditions for toxics in sediments"
   	    STOP
   END IF
   ALLOCATE(TEMPBTC(MGL,NBTCR))
   DO I=1, MGL
     READ(210,*) (TEMPBTC(I,j), j=1,NBTCR)
   END DO

   CLOSE(210)
   If (SERIAL) THEN
     ConTcrSdm1 = TEMPBTC
     ConTcrSdm2 = TEMPBTC
   END IF
# if defined(MULTIPROCESSOR)
    IF (PAR) THEN
          DO I = 1, MLOC
            ConTcrSdm1(I,:) = TEMPBTC(NGID(I),:)
            ConTcrSdm2(I,:) = TEMPBTC(NGID(I),:)
          END DO
          DO I = 1, NHN
            ConTcrSdm1(I+MLOC,:) = TEMPBTC(HN_LST(I),:)
            ConTcrSdm2(I+MLOC,:) = TEMPBTC(HN_LST(I),:)
          END DO
     END IF
# endif
    DEALLOCATE(TEMPBTC)
 END IF

!-------------------------------- initial conditions for tracer in the water column -------------------
OPEN(UNIT = 211, FILE = TRIM(TCRF_INC), ACTION = 'READ')
READ(211,*, IOSTAT = end_file)

IF(end_file < 0) THEN
	    WRITE (*,*) "Error occured - No file for initial conditions for toxics.dat"
	    STOP
END IF
TCCST = NCP - NBTCR + 1
ALLOCATE (RTMPX(0:MGL, KBM1, TCCST:NCP))         
RTMPX = 0.0
DO I = 1, MGL
        DO K = 1, KBM1
            READ (211,*) (RTMPX(I, K, JCON), JCON=TCCST, NCP)
        END DO
END DO
If (SERIAL) C1(:,:,TCCST:NCP) = RTMPX

#  if defined (MULTIPROCESSOR)
         IF (PAR) THEN
            DO JCON = TCCST, NCP
               DO K = 1, KBM1
                  DO I = 1, MLOC
                     C1 (I, K, JCON) = RTMPX (NGID(I), K, JCON)
                     C2 (I, K, JCON) = RTMPX (NGID(I), K, JCON)
                  END DO
                  DO I = 1, NHN
                     C1 (I+MLOC, K, JCON) = RTMPX (HN_LST(I), K, JCON)
                     C2 (I+MLOC, K, JCON) = RTMPX (HN_LST(I), K, JCON)
                  END DO
               END DO
            END DO
         END IF
#  endif

 DEALLOCATE(RTMPX)
 CLOSE(211)
!------------------------------------- OBC conditions for tracer kinetics ----------------------
!OPEN(UNIT = 212, FILE = TRIM(TCRF_OBC), ACTION = 'READ')
!READ(212,*)NUM_NDS_TC
!IF(NUM_NDS_TC /= IOBCN_GL) THEN
!  WRITE(*,*)'Number of nodes for OBC does not match with water quality related OBC nodes'
!  STOP
!END IF

!ALLOCATE(OBCNDS_TCR(IOBCN_GL))
!DO J =1,IOBCN_GL
!  READ(212,*)OBCNDS_TCR(j)
!END DO

!#  if defined (MULTIPROCESSOR)
!    IF(PAR) THEN
!        DO JT = 1, NUT_TM%NTIMES
!          READ(212,*)
!         DO JJ = TCCST, NCP
!           DO I = 1, IOBCN_GL
!              I1 = NLID(I_OBC_GL(I))
!               IF(I1 == 0) THEN
!                 READ(212,*)
!                 CYCLE
!               END IF

!                DO JK = 1, IOBCN
!                 IF (I1 == I_OBC_N(JK)) THEN
!                   READ(212,*) (WQOBC(JK,K,JT,JJ), K=1, KBM1)
!                   EXIT
!                 ELSE
!                    !WRITE(*,*)'Reading of OBC data for Toxics encountered a problem', I,I1,IOBCN, I_OBC_N
!                   CYCLE
!                 END IF
!               END DO

!              END DO
!            END DO
!        END DO
!   END IF
!# endif
! CLOSE(212)
!DEALLOCATE(OBCNDS_TCR)

 !--------------------------------- tracer or pollutant inflows from point sources -------------------------
OPEN(UNIT = 213, FILE = TRIM(TCRF_PNT), ACTION = 'READ')
READ(213,*)
READ(213,*)NTCR_PNT_GL   ! no. of global nodes for tracer point sources
TCR_PNT = 0

IF (NTCR_PNT_GL > 0) THEN
    ALLOCATE (TEMP(NTCR_PNT_GL), TEMP2(NTCR_PNT_GL), TEMP3(NTCR_PNT_GL))
    DO I = 1, NTCR_PNT_GL  ! reading the global node ids for point sources
      READ (213,*) TEMP (I)
    END DO
    !------------ Mapping ids for serial computing ------------------------
    IF (SERIAL) THEN
       TCR_PNT = NTCR_PNT_GL
       ALLOCATE (TCRPNT_ID(TCR_PNT))
       TCRPNT_ID (:) = TEMP (:)
    END IF
    !------------ Mapping ids for parallel computing -----------------------
# if defined (MULTIPROCESSOR)
             IF (PAR) THEN
                CNT = 0
                DO I = 1, NTCR_PNT_GL
                   IF (NLID(TEMP(I)) /= 0) THEN
                      CNT = CNT + 1
                      TEMP2 (CNT) = NLID (TEMP(I))
                      TEMP3 (CNT) = I
                   END IF
                END DO
                TCR_PNT = CNT
                ALLOCATE (TCGL_2_LOC_PNT(TCR_PNT), TCRPNT_ID(TCR_PNT))
                TCRPNT_ID (1:CNT) = TEMP2 (1:CNT)! The point locally
                TCGL_2_LOC_PNT (1:CNT) = TEMP3 (1:CNT)! The point globally
             END IF
# endif

 DEALLOCATE (TEMP, TEMP2, TEMP3)

!---------------- Read the flow distribution for each node --------------------------
 ALLOCATE (RTEMP(NTCR_PNT_GL, KBM1))
DO I = 1, NTCR_PNT_GL
   READ (213,*) J, (RTEMP(I, K), K=1, KBM1)
END DO
ALLOCATE (TCRQDIST(TCR_PNT, KBM1))
If (SERIAL) TCRQDIST(1:TCR_PNT, :) = RTEMP (1:NTCR_PNT_GL, :)
!-------------------------------------------------------------------------------------
#  if defined (MULTIPROCESSOR)
         IF (PAR) THEN
            TCRQDIST (1:TCR_PNT, :) = RTEMP (TCGL_2_LOC_PNT(1:TCR_PNT),:)
         END IF
#  endif
!-------------------------------------------------------------------------------------
!---------------- Reading the tracer concentration data ----------------------------------
READ(213,*) TCRQTIME
TCR_TM%NTIMES = TCRQTIME
TCR_TM%LABEL = "TCR-point source"
ALLOCATE (TCR_TM%TIMES(TCRQTIME))
ALLOCATE (RTEMP1(NTCR_PNT_GL, TCCST:NCP, TCRQTIME))
ALLOCATE (RTEMP2(NTCR_PNT_GL, TCRQTIME))
!------------------------------------------------------------------------------------
DO I = 1, TCRQTIME
   READ (213,*) TTIME
   TCR_TM%TIMES (I) = TTIME
   READ (213,*) (RTEMP2(J, I), J=1, NTCR_PNT_GL)
   DO NJ = TCCST,NCP
      READ (213,*) (RTEMP1(J, NJ, I), J=1, NTCR_PNT_GL)
   END DO
END DO

!---------------------------------------- FB Unit Conversion -------------------------------
! Fecal bacteria concentration usually is ususally expressed in cfu/100 ml
! unit connversion is needed for loading, advection and diffusion process cfu/m3
IF(BACTERIAON) THEN 
    RTEMP1 = RTEMP1*FBUnitScale
END IF

!-------------- Transforming data into local arrays ---------------------------------------- 

ALLOCATE (TCRQDIS(TCR_PNT,TCRQTIME))
ALLOCATE (TCRCQDIS(TCR_PNT, TCCST:NCP, TCRQTIME))

IF(SERIAL) THEN
  TCRQDIS(1:TCR_PNT,:) = RTEMP2 (1:NTCR_PNT_GL,:)
  TCRCQDIS(1:TCR_PNT,:,:) = RTEMP1 (1:NTCR_PNT_GL,:,:)
END IF

#  if defined (MULTIPROCESSOR)
         IF (PAR) THEN
            TCRQDIS (1:TCR_PNT, :) = RTEMP2 (TCGL_2_LOC_PNT(1:TCR_PNT), :)
            TCRCQDIS (1:TCR_PNT, :, :) = RTEMP1 (TCGL_2_LOC_PNT(1:TCR_PNT), :, :)
            DEALLOCATE (TCGL_2_LOC_PNT)
         END IF
#  endif

DEALLOCATE (RTEMP, RTEMP1, RTEMP2)
CLOSE(213)
END IF
!----------------------- information for outputing tracer concentration data ----------------  
IF(NumTcrOutVar > 0) THEN 
  TcrtOutput= .TRUE. 
ELSE 
  TcrtOutput= .FALSE.
END IF 

END SUBROUTINE
!-------------------------------------------------------------------------------------------------



!------------------------------------------------------------------------------------------------
SUBROUTINE TcrSldKnt(TMSTEP)

  USE MOD_HYDROVARS, ONLY : D,DZ,ART1,DZ2D,RHO,UU,VV
  USE MOD_TGE, ONLY: NBVE,NTVE
  IMPLICIT NONE

 REAL(SP), INTENT(IN) :: TMSTEP
 INTEGER :: tcrID,I,K,JJ
 REAL(SP) :: DelZb, ErsnShear, Wsdm, DelHT, TmCount
 REAL(SP), DIMENSION(12) :: MonthArray,SusPTM


 !--------------------- Specific Sedimentation process for RN fate and transport -----------------------
 IF(RADIONUCON) THEN
     MonthArray = (/31,28,31,30,31,30,31,31,30,31,30,31/)
     SusPTM = (/1.20344,1.0072,0.8111,1.02414,0.93935,0.6287,0.55816,0.53322,0.73771,1.12175,1.38323,1.39962/)
     TmCount = 0.0
      DO I=1,12
          TmCount = TmCount + MonthArray(I)
          IF(INT(JDAY) <= INT(TmCount)) THEN
              SPM(:,1) = SusPTM(I) 
              EXIT
          END IF
      END DO
    !---------------------------- Sedimentation of SPM through Water column ---------------------------- 
    !----------------------------- Settling from the First Layer ---------------------------------------
    !SPM(:,1) = 5.0     !(g/m3)
    WSPM(:,:) = 0.0000579   !(m/s)
    PSRT1(:) = 0.62     !(%)

    DO I=1,MLOC 
        DTSPM (I, 1) = -WSPM(I, 1)*SPM(I, 1) / (D(I)*DZ2D(I,1))
    END DO

    !----------------------------- Settling from the second Layer onwards -----------------------------
    DO K = 2, KBM1-1
      DO I = 1, MLOC
        DTSPM (I, K) = (WSPM(I, K-1)*SPM(I, K-1)-WSPM(I,K)*SPM(I, K)) / (D(I)*DZ2D(I,K))
      END DO 
    END DO

    !-------------------------------------------------------------------------------------------------- 
     SedFlux(:) = WSPM(:,KBM1)*SPM(:,KBM1)    ! sedimentation flux (gm-2s-1)

    !----------------------------- Estimation the erosion of sediment layer --------------------------
    !-------- Calculating nodal velocities---------------------------
    ULND = 0.0
    VLND = 0.0
    WWLND = 0.0
    SdmDryDen = (1.0 - SdmPrst)*PtcDensity    ! sediment dry density
    SdmBRL = 0.0

    DO I=1,MLOC
      DO J=1, NTVE(I)
          ULND(I) = ULND(I) + UU(NBVE(I,J),KBM1)
          VLND(I) = VLND(I) + VV(NBVE(I,J),KBM1)
      END DO
      ULND(I) = ULND(I)/NTVE(I)
      VLND(I) = VLND(I)/NTVE(I)
      WWLND(I) = (ULND(I)*ULND(I) + VLND(I)*VLND(I))
      DelZb = D(I)*DZ2D(I,KBM1)

      BtmShear(I) = RHO(I, KBM1)*VonKrmn*DelZb*WWLND(I) / &
                    & ((DelZb + Rgfh)*LOG((DelZb + Rgfh)/Rgfh) - DelZb)
      ErsnShear = ErsnCoeff*((BtmShear(I)/CrtShear) - 1.0)*1E-5
      SusFlux(I) = MAX(0.0, ErsnShear)
      SusFlux(I) = SusFlux(I)*1E3
      
      DTSPM (I, KBM1) = (-SedFlux(I) + SusFlux(I))/DelZb   ! change of sediment concentration in the bottom layer

      Wsdm = (-SedFlux(I) + SusFlux(I))/(SdmDryDen*1E3)
      SdmHT(I) = SdmHT(I) - Wsdm*TMSTEP

      IF(SdmHT(I) < 0.001) THEN 
          SdmHT(I) = 0.001
      END IF

      IF(SdmHT(I) > 0.1) THEN 
        DelHT = SdmHT(I) - 0.1 
        SdmBRL(I) = (DelHT/TMSTEP)*(SdmDryDen*1E3)
        SdmHT(I) = 0.100
      END IF

      SPM(I,2:KBM1) = SPM(I,2:KBM1) + DTSPM(I,2:KBM1)*TMSTEP 
      DO j=1,KBM1
        IF(SPM(I,j) < 0.0) THEN
            SPM(I,j) = 0.0 
        END IF 
      END DO

    END DO
 !WRITE(*,*)'Sediment calculations are processed'
 END IF
END SUBROUTINE
!---------------------------------------------------------------------------------------------


!--------------------------------- Tracer kinetics -------------------------------------------
SUBROUTINE TcrKnt(TMSTEP)

USE MOD_WQM, ONLY : WSSHI,WSSNET,WS1,WS2,WS1NET,WS2NET,WSL,WSR, &
                  & WSLNET,WSRNET,SPM,WSPM,SedFlux,SusFlux,SdmBRL
USE MOD_HYDROVARS, ONLY : D,DZ,DZ2D 

IMPLICIT NONE
REAL(SP), INTENT(IN) :: TMSTEP
INTEGER :: strBctID 
REAL(SP) :: UnitCnv, fracDenominatorWC, fracDenominatorSD1, fracDenominatorSD2
REAL (SP) :: DCrnSSI_W, DCrnPOC_W, DCrnALG1_W, DCrnALG2_W, DCrnZOO_W, DCrnDOC_W
REAL (SP) :: DCrnSSI_INS1, DCrnPOC_INS1, DCrnAG1_INS1, DCrnAG2_INS1
REAL (SP) :: DCrnSSI_INS2, DCrnPOC_INS2, DCrnAG1_INS2, DCrnAG2_INS2,KLO2
REAL (SP) :: SldConLay1, SldConLay2, SSIConR1, SSIConR2, TotSldCon1, TotSldCon2
REAL (SP) :: DCrnSSI_OUTS1, DCrnPOC_OUTS1, DCrnDOC_OUTS1, GRADC12, DCrnDOC_DFFS1, DCrnDOC_DFFS2
REAL (SP) :: TOTDCrn_INS1, TOTDCrn_INS2, Rg, T_abs, VOLVEL,KRATIO, KLCFF,KGCFF
REAL (SP) :: DCrnDecay,VelSus, VelBRL

  strBctID = NCP - NBTCR + 1
  ConTcr(:,:,:) = C2(:,:,strBctID:NCP)
  DWTCR(:,:,:) = 0.0
  DSTCR(:,:,:) = 0.0

!------------------------------ Bacteria kinetics --------------------------------------------------
! I0 - units are in E/m2/day - E = Einstein - The energy of 1 mole of photons
! 1 E = 1 mol × NA h f = 1 mol × 6.02214076×10e23 mol−1 × 6.62607015×10e-34 J s × f = 3.9903127128934321×10e-10 J s × f
! f is the frequency  = c/lambda, lambda =  wave length (m), c = speed of light (m/s)
! So, E to J conversion depends on the frequency, for short light waves - (400 - 700 nm), average frequency = 550 nm
! 1 E = 217502.84661581277 J
! 1 J = 0.239006 cal
! 1 E/m2/day  =>  factor cal/cm2/day => (51984.48535825895/1E6) cal/cm2/day
IF(BACTERIAON) THEN
UnitCnv = 0.05198448535825895
  DO K=1, KBM1
    DO I=1, MLOC 
      KBact = (KnBact + C0SalBact*SALT(I,K) + AlphaBact*UnitCnv*I0)*((ThetaBact**(T(I,K) - 20)))
      DO J=1, NBTCR 
        DWTCR(I,K,J) = -KBact*ConTcr(I,K,J)/86400.0     ! converting daliy decay into decay/per second
      END DO
    END DO
  END DO
END IF
!----------------------------------------------------------------------------------------------------

!*********************************************************************************************************
!------------------------------ Radionuclide kinetics -----------------------------------------------
! This kinetics are similar to toxics, but created as a separate module to keep the flexibility for future expansion
IF(RADIONUCON) THEN
!------------------------- data assignment ------------------------------------------ 
DFFCOEFS12 = 3.51E-10

DO k =1, MLOC
  DO j =1, KBM1
    DO I=1,NBTCR 
      KPrnSSI_W(k,j,I) = RnKd(I)*1E-6 
      KPrnSSI_S1(k,I) = RnKd(I)*1E-6 
      IF(ConTcr(k,j,I) < 1E-15) THEN 
        ConTcr(k,j,I) = 1E-30
      ELSE
        ConTcr(k,j,I) = ConTcr(k,j,I)*(1.0/SpAct(I))
      END IF

      IF(ConTcrSdm1(k,I) < 1E-15) THEN 
        ConTcrSdm1(k,I) = 1E-30
      ELSE
        ConTcrSdm1(k,I) = ConTcrSdm1(k,I)*(1.0/SpAct(I))
      END IF
    END DO
  END DO 
END DO

!------------------------- Computing fractions for RNs --------------------------------
 DO I=1, MLOC
   DO K=1, KBM1
     DO j=1, NBTCR
     !------------------ For the water column ------------------------------------------
      fracDenominatorWC = 1.0 + KPrnSSI_W(I,K,j)*SPM(I,K) 

      FDrn_W(I,K,j) = 1.0/fracDenominatorWC    ! dissolved RNs fraction
      FPrnSSI_W(I,K,j) = (KPrnSSI_W(I,K,j)*SPM(I,K)) / fracDenominatorWC    ! RNs fraction in SPM
    END DO

   END DO
  !----------------------- For the sediment layer 1----------------------------------------
     DO j=1, NBTCR
      fracDenominatorSD1 = PSRT1(I) + KPrnSSI_S1(I,j)*(SdmDryDen*1E3)

      FDrn_WS1(I,j) = PSRT1(I)/fracDenominatorSD1      ! dissolved RNs fraction in sediment layer -1
      FPrnSSI_S1(I,j) = KPrnSSI_S1(I,j)*(SdmDryDen*1E3) / fracDenominatorSD1   ! RNs fraction in SSI sdlyr -1

    END DO
 END DO

!---------------------------------------- RNs partitioning and other kinetics ----------------------- 
!---------------------------------------- For top layer of the water column -------------------------- 

  DO I=1, MLOC 
    DO J=1, NBTCR

      DCrnSSI_W = (WSPM(I,1)*ConTcr(I,1,j)*FPrnSSI_W(I,1,j)) / (D(I)*DZ2D(I,1))
      DCrnDecay = (ConTcr(I,1,j) *(1.0 - EXP(-DecayConst(J)*TMSTEP)))/TMSTEP

      DWTCR(I,1,J) = (-DCrnSSI_W - DCrnDecay)*SpAct(J)
      IF(DWTCR(I,1,J) < 1E-30) THEN 
        DWTCR(I,1,J) = 1E-30
      END IF
      !DWTCR(I,1,J) = (-DCrnSSI_W)

      IF(ISNAN(DWTCR(I,1,J))) THEN 
          WRITE(*,*)'concen change goes nuts (top)', DWTCR(I,1,J),ConTcr(I,1,j),FPrnSSI_W(I,1,j),KPrnSSI_W(I,1,j),SPM(I,1) 
          STOP
      END IF
    END DO
  END DO
!----------------------------------------------------------------------------------------------------- 


!---------------------------------------- For middle layers of WC ------------------------------------ 
 DO K=2,KBM1
   DO I=1,MLOC
     DO j=1,NBTCR

        DCrnSSI_W = (WSPM(I,K-1)*ConTcr(I,K-1,j)*FPrnSSI_W(I,K-1,j) - WSPM(I,K)*ConTcr(I,K,j)*FPrnSSI_W(I,K,j)) &
                & / (D(I)*DZ2D(I,K))
        DCrnDecay = (ConTcr(I,K,j) *(1.0 - EXP(-DecayConst(j)*TMSTEP)))/TMSTEP

        DWTCR(I,K,j) = (DCrnSSI_W - DCrnDecay)*SpAct(j) 
        !DWTCR(I,K,j) = (DCrnSSI_W)

        !IF((JDAY > 0.125000000000000) .AND. (JDAY < 0.125092592592593)) THEN 
        !    WRITE(*,*)'Debugging concen change.....', DCrnSSI_W,DWTCR(I,K,j),FPrnSSI_W(I,K-1,j)
        !    STOP
        !END IF

        IF(DWTCR(I,K,j) < 1E-30) THEN 
            DWTCR(I,K,j) = 1E-30
        END IF

        IF(ISNAN(DWTCR(I,K,J))) THEN 
           WRITE(*,*)'concen change goes nuts (middle)', DWTCR(I,K,J),ConTcr(I,K,j),FPrnSSI_W(I,K,j),NBTCR,K
           STOP
        END IF
    END DO
   END DO
 END DO
!------------------------------------------------------------------------------------------------------ 
!--------------------------- Bottom layer resuspension  -----------------------------------------------
! No Resuspension velocity has been used. Instead Net settling velocity has been used to
! comply with existing solid kinetics Model
!------------------------------------------------------------------------------------------------------

 DO I=1,MLOC
   DO j=1,NBTCR
      !DFFVS12W(I) = (69.35*PSRT1(I)*(MolWghtCng(j)**(-2.0/3.0)))/365.0   ! Diffusive mixing velocity (Chapra, 1997) (m/d)

      VelSus = SusFlux(I)/(SdmDryDen*1E3)
      !WRITE()
      DCrnSSI_W = (VelSus*ConTcrSdm1(I,j)*FDrn_WS1(I,j)) &
              & / (D(I)*DZ2D(I,KBM1))   ! similar to resuspension
      GRADC12 = (ConTcrSdm1(I,j)*FDrn_WS1(I,j) - ConTcr(I,KBM1,j)*FDrn_W(I,KBM1,j))/(0.5*(SdmHT(I)+D(I)*DZ2D(I,KBM1)))
      DCrnDOC_DFFS1 = DFFCOEFS12*GRADC12/SdmHT(I)    ! Molecular diffusion into sdlyr 2
      !DCrnDOC_DFFS1 = 0.0
      !DCrnSSI_W = 0.0
      DCrnDecay = (ConTcr(I,KBM1,j) *(1.0 - EXP(-DecayConst(j)*TMSTEP)))/TMSTEP

      DWTCR(I,KBM1,j) = (DWTCR(I,KBM1,j)) + (DCrnSSI_W + DCrnDOC_DFFS1 - DCrnDecay)*SpAct(j)

      IF(DWTCR(I,KBM1,j) < 1E-30) THEN 
          DWTCR(I,KBM1,j) = 1E-30
      END IF

        IF(ISNAN(DWTCR(I,KBM1,J))) THEN 
           WRITE(*,*)'concen change goes nuts (bottom)', DWTCR(I,KBM1,j)
           STOP
        END IF

   END DO
 END DO
!--------------------------------------------------------------------------------------- 


!-------------------- RNs kinetics in sediment layer ----------------------------------
!-------------------- sediment layer -1 ------------------------------------------------
DO I=1, MLOC
  DO j=1, NBTCR
    BRLVEL = SdmBRL(I)/(SdmDryDen*1E3)
    DCrnSSI_INS1 = (WSPM(I,KBM1)*ConTcr(I,KBM1,j)*FPrnSSI_W(I,KBM1,j)) / D(I)*DZ2D(I,KBM1)
    DCrnSSI_OUTS1 = (BRLVEL*ConTcrSdm1(I,j)*FPrnSSI_S1(I,j))/SdmHT(I)
    !DCrnDOC_OUTS1 = DFFVS12W(I)*(ConTcrSdm1(I,j)*FPrnDOC_S1(I,j) - ConTcr(I,KBM1,j)*FPrnDOC_W(I,KBM1,j)) / &
    !                         & TCRHT1(I) / 86400.0   ! Diffusive mixing with water column
    GRADC12 = (ConTcrSdm1(I,j)*FDrn_WS1(I,j) - ConTcr(I,KBM1,j)*FDrn_W(I,KBM1,j))/(0.5*(SdmHT(I)+D(I)*DZ2D(I,KBM1)))
    DCrnDOC_DFFS1 = DFFCOEFS12*GRADC12/SdmHT(I)    ! Molecular diffusion into sdlyr 2

    DCrnDecay = (ConTcrSdm1(I,j)*(1.0 - EXP(-DecayConst(j)*TMSTEP)))/TMSTEP

    DSTCR(I,j,1) = (DCrnSSI_INS1 - DCrnSSI_OUTS1 - DCrnDOC_DFFS1 - DCrnDecay)*SpAct(j)

      IF(DSTCR(I,j,1) < 1E-30) THEN 
          DSTCR(I,j,1) = 1E-30
      END IF

  END DO
END DO
!----------------------------------------------------------------------------------------

DO I=1,NBTCR 
!    ConTcr(:,:,I) = ConTcr(:,:,I)*SpAct(I)
    ConTcrSdm1(:,I) = ConTcrSdm1(:,I)*SpAct(I)
END DO

END IF

END SUBROUTINE
!------------------------------------------------------------------------------------------------ 



!------------------------------- concentration data extraction ----------------------------------
SUBROUTINE ExtractTcrData 

IMPLICIT NONE 

INTEGER :: I,K,JJ

    IF (BACTERIAON) THEN
        TcrtConW = ConTcr*1E-04     ! FB units back to cfu/100 ml in outputs     
    ELSE 
        TcrtConW(:,:,:) = ConTcr(:,:,:)
    END IF
    SdmDryDen = 1.0
    TcrtConSdm(:,:) = ConTcrSdm1(:,:)/SdmDryDen

END SUBROUTINE
!----------------------------------------------------------------------------------------------

END MODULE 
