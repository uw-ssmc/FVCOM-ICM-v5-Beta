MODULE SC_TCR
  USE MOD_LIMS, Only: MTLOC, KBM1, KB, MLOC, TCR_PNT
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
                    & RdcnConTS1, RdcnConTS1_GL, RdcnConTS2, RdcnConTS2_GL

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
REAL(SP), ALLOCATABLE, DIMENSION (:, :) :: ConTcrSdm1, ConTcrSdm2, ConTcrSdm1_GL, ConTcrSdm2_GL
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

REAL(SP) :: SED_DEN, DFFCOEFS12, BRLVEL, SED_DEN1, SED_DEN2

CHARACTER(LEN = 1024) :: TCRF_INC,TCRF_PNT,TCRF_OBC,TCRF_INC_SD,FLAG_SD_INI, FLAG_WIND
CHARACTER(LEN = 1024) :: KINETICSWT
REAL(SP) ::TCR_INIT_SD, KD, KBact, KnBact, C0SalBact, AlphaBact, ThetaBact


CONTAINS 


!--------------------------------------------------------------------------------------------
SUBROUTINE BactAlloc
    IMPLICIT NONE

!------------------ Allocation of tracer/pollutant related variables ------------------------ 
          ALLOCATE(ConTcr(MTLOC,KBM1,NBTCR))
          ConTcr = 0.0
          ALLOCATE(ConTcrSdm1(MTLOC,NBTCR))
          ConTcrSdm1 = 0.0
          ALLOCATE(ConTcrSdm2(MTLOC,NBTCR))
          ConTcrSdm2 = 0.0
          ALLOCATE(ConTcr_GL(MGL,KBM1,NBTCR))
          ConTcr_GL = 0.0
          ALLOCATE(ConTcrSdm1_GL(MGL,NBTCR))
          ConTcrSdm1_GL = 0.0
          ALLOCATE(ConTcrSdm2_GL(MGL,NBTCR))
          ConTcrSdm2_GL = 0.0

          ALLOCATE(DWTCR(MTLOC,KBM1,NBTCR))
          DWTCR = 0.0
          ALLOCATE(DSTCR(MTLOC,NBTCR,2))
          DSTCR = 0.0
          ALLOCATE(TcrtConW(MTLOC,KBM1,NBTCR))
          TcrtConW = 0.0 
          ALLOCATE(TcrtConW_GL(MGL,KBM1,NBTCR))
          TcrtConW_GL = 0.0 
          ALLOCATE(TcrtConSdm(MTLOC,NBTCR))
          TcrtConSdm = 0.0
          ALLOCATE(TcrtConSdm_GL(MGL,NBTCR))
          TcrtConSdm_GL = 0.0
!------------------ Allocation for radionuclides concentrations in each constituents -------------- 
IF(TRIM(KINETICSWT) == 'RADIONUCON') THEN
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
          ALLOCATE(RdcnConW_GL(MGL,KBM1,NBTCR))
          RdcnConW_GL = 0.0
          ALLOCATE(RdcnConAlgW_GL(MGL,KBM1,NBTCR))
          RdcnConAlgW_GL = 0.0 
          ALLOCATE(RdcnConZplW_GL(MGL,KBM1,NBTCR))
          RdcnConZplW_GL = 0.0 
          ALLOCATE(RdcnConSldW_GL(MGL,KBM1,NBTCR))
          RdcnConSldW_GL = 0.0 
          ALLOCATE(RdcnConPOCW_GL(MGL,KBM1,NBTCR))
          RdcnConPOCW_GL = 0.0
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
          ALLOCATE(RdcnConWS1_GL(MGL,NBTCR))
          RdcnConWS1_GL = 0.0 
          ALLOCATE(RdcnConWS2_GL(MGL,NBTCR))
          RdcnConWS2_GL = 0.0 
          ALLOCATE(RdcnConPOCS1_GL(MGL,NBTCR))
          RdcnConPOCS1_GL = 0.0 
          ALLOCATE(RdcnConPOCS2_GL(MGL,NBTCR))
          RdcnConPOCS2_GL = 0.0
          ALLOCATE(RdcnConDOCS1_GL(MGL,NBTCR))
          RdcnConDOCS1_GL = 0.0 
          ALLOCATE(RdcnConDOCS2_GL(MGL,NBTCR))
          RdcnConDOCS2_GL = 0.0
          ALLOCATE(RdcnConSldS1_GL(MGL,NBTCR))
          RdcnConSldS1_GL = 0.0
          ALLOCATE(RdcnConSldS2_GL(MGL,NBTCR))
          RdcnConSldS2_GL = 0.0
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

IF(TRIM(KINETICSWT) == 'RADIONUCON') THEN
          ALLOCATE(FPrnSSI_W(MTLOC,KBM1,NBTCR))
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
          ALLOCATE(FDrn_W(MTLOC,KBM1,NBTCR))
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
          ALLOCATE(FDrn_WS1(MTLOC,NBTCR))
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
          ALLOCATE(POC_CONCENS(MTLOC,2))
          POC_CONCENS = 0.0
          ALLOCATE(DOC_CONCENS(MTLOC,2))
          DOC_CONCENS = 0.0
          ALLOCATE(POCCNS1(MTLOC))
          POCCNS1 = 0.0
          ALLOCATE(POCCNS2(MTLOC))
          POCCNS2 = 0.0
          ALLOCATE(DOCCNS1(MTLOC))
          DOCCNS1 = 0.0
          ALLOCATE(DOCCNS2(MTLOC))
          DOCCNS2 = 0.0
!-------------------------------------------------------------------------------------------

      
END SUBROUTINE
!------------------------------------------------------------------------------------------



!--------------------------------------------------------------------------------------------
SUBROUTINE ReadBactData
 IMPLICIT NONE 
 INTEGER :: end_file, I1, NUM_NDS_TC, NTCR_PNT_GL, TCRQTIME
 INTEGER, ALLOCATABLE, DIMENSION(:) :: OBCNDS_TCR, TCGL_2_LOC_PNT
 REAL(SP), ALLOCATABLE, DIMENSION(:,:) :: RTEMP, RTEMP2
 REAL(SP), ALLOCATABLE, DIMENSION(:,:,:) :: RTEMP1
 REAL(SP), ALLOCATABLE, DIMENSION(:,:) :: TEMPBTC
 REAL(SP) :: TTIME, FBUnitScale
 CHARACTER(LEN = 64) :: FLGTCR_OUTPUT
 INTEGER :: NumLine


FBUnitScale = 1E+04       ! FB units - cfu/100 ml ---> cfu/m3

OPEN(UNIT = 379, FILE = 'inputs/bact_cnt.dat', ACTION = 'READ')
READ(379,*, IOSTAT = end_file)
IF(end_file < 0) THEN
	    WRITE (*,*) "Error occured - Problem with bact_cnt.dat"
	    STOP
END IF

READ(379,*) TCRF_INC    ! input file of initial conditions for tracer
READ(379,*) TCRF_PNT    ! input file of loading from point sources
READ(379,*) TCRF_OBC    ! input file for open boundary conditions
READ(379,*) TCRF_INC_SD ! file for tracer initial conditions for sediment layer
READ(379,*)
READ(379,*) FLAG_SD_INI  ! flag for initial tracer distribution in sediment (DISTRIBUTED/UNIFORM)
READ(379,*) TCR_INIT_SD     ! uniform tracer concen if tracer distribution in sediment is uniform
READ(379,*)
READ(379,*) FLGTCR_OUTPUT   ! Flag for outputting tracer concentration data 
READ(379,*)
READ(379,*) KINETICSWT    ! type of kinetics to be used for the tracers
READ(379,*)
READ(379,*) KnBact 
READ(379,*) C0SalBact
READ(379,*) AlphaBact
READ(379,*) ThetaBact

CLOSE(379)

HSED1 = 0.1         ! initialization of sediment layer thickness
HSED = 0.9          ! initialization of sediment layer thickness

!------------------------------- initialization of tracer in sediment layer ----------------------------
 IF(TRIM(FLAG_SD_INI) == 'UNIFORM') THEN
   ConTcrSdm1 = TCR_INIT_SD
   ConTcrSdm2 = TCR_INIT_SD
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
OPEN(UNIT = 212, FILE = TRIM(TCRF_OBC), ACTION = 'READ')
READ(212,*)NUM_NDS_TC
IF(NUM_NDS_TC /= IOBCN_GL) THEN
  WRITE(*,*)'Number of nodes for OBC does not match with water quality related OBC nodes'
  STOP
END IF

ALLOCATE(OBCNDS_TCR(IOBCN_GL))
DO J =1,IOBCN_GL
  READ(212,*)OBCNDS_TCR(j)
END DO

#  if defined (MULTIPROCESSOR)
    IF(PAR) THEN
        DO JT = 1, NUT_TM%NTIMES
          READ(212,*)
          DO JJ = TCCST, NCP
            DO I = 1, IOBCN_GL
               I1 = NLID(I_OBC_GL(I))
                IF(I1 == 0) THEN
                  READ(212,*)
                  CYCLE
                END IF

                DO JK = 1, IOBCN
                  IF (I1 == I_OBC_N(JK)) THEN
                    READ(212,*) (WQOBC(JK,K,JT,JJ), K=1, KBM1)
                    EXIT
                  ELSE
                    !WRITE(*,*)'Reading of OBC data for Toxics encountered a problem', I,I1,IOBCN, I_OBC_N
                    CYCLE
                  END IF
                END DO

              END DO
            END DO
        END DO
    END IF
# endif
 CLOSE(212)
 DEALLOCATE(OBCNDS_TCR)

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
IF(TRIM(KINETICSWT) == 'BACTERIAON') THEN 
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
IF(TRIM(FLGTCR_OUTPUT) == 'TRUE') THEN 
  TcrtOutput= .TRUE. 
  OPEN(UNIT = 215, FILE = 'inputs/bact_output_infor.dat', ACTION = 'READ') 
  NumLine = 0
  DO
    READ(215, *,IOSTAT=end_file)
    IF(end_file /= 0) THEN
      REWIND(215)
      EXIT
    ELSE
      NumLine = NumLine + 1
    END IF
  END DO
  NumTcrOutVar  = NumLine
  ALLOCATE(TcrOutVar(NumLine))
  DO i=1,NumLine
    READ(215,*) TcrOutVar(i)
  END DO
  CLOSE(215)
ELSE 
  TcrtOutput= .FALSE.
END IF 

END SUBROUTINE
!-------------------------------------------------------------------------------------------------



!------------------------------------------------------------------------------------------------
SUBROUTINE TcrSldKnt(TMSTEP)
  USE MOD_WQM, ONLY: TTL_MASS, C2, TTL_MASS2, TTL_MASS3, TTL_MASS3_GL
  USE MOD_HYDROVARS, ONLY : D,DZ,ART1,DZ2D
  IMPLICIT NONE

 REAL(SP), INTENT(IN) :: TMSTEP
 INTEGER :: tcrID

!----------------------- Accumulation of total solid mass (SSI) -------------------------
 TCRHT1 = HSED1       ! obtain thickness of sediment layer - 1
 TCRHT2 = HSED        ! obtain thickness of sediment layer - 2

! TXCHT1 = 0.1
! TXCHT2 = 0.9

 TTL_MASS = 0.0
 TTL_MASS2 = 0.0
 TTL_MASS3 = 0.0
 tcrID = NCP - NBTCR + 1
 DO I=1, MLOC
   DO J=1, KBM1
     !TTL_MASS(I) = TTL_MASS(I) + C2(I,J,3)*ART1(I)*D(I)*DZ2D(I,KBM1)*0.001
      DO JJ=tcrID, NCP
        TTL_MASS(I) = TTL_MASS(I) + C2(I,J,JJ)*ART1(I)*D(I)*DZ(J)*0.001
      END DO

   END DO
      !TTL_MASS(I) = C2(I,10,3)
      !TTL_MASS(I) = TTL_MASS(I) / KBM1     ! Layer averaged concentration
      !CONPCB_W(I) = CONPCB_W(I) / KBM1     ! layer averaged PCB concentration
      !! SSD_MSS needs to be modified according to actual sediment layer thickness
      SSD_MSS(I) =  SSD_MSS(I) + SSDL(I)*TMSTEP   ! accumulated settled sediment concen over time
      !TTL_MASS2(I) = TTL_MASS2(I) + CONPCB_S(I)*ART1(I)*D(I)*DZ(KBM1)*0.001
      POCCNS1(I) = POC_CONCENS(I,1)
      POCCNS2(I) = POC_CONCENS(I,2)
      DOCCNS1(I) = DOC_CONCENS(I,1)
      DOCCNS2(I) = DOC_CONCENS(I,2)

      DO JJ=1,NBTCR
        TTL_MASS2(I) = TTL_MASS2(I) + ConTcrSdm1(I,JJ)*ART1(I)*TCRHT1(I)*0.001
        TTL_MASS3(I) = TTL_MASS3(I) + ConTcrSdm2(I,JJ)*ART1(I)*TCRHT2(I)*0.001
      END DO
      !TTL_MASS2(I) = TTL_MASS2(I) + SSD_MSS(I)*ART1(I)*TXCHT1(I)*0.001

 END DO

END SUBROUTINE
!---------------------------------------------------------------------------------------------




!--------------------------------- Tracer kinetics -------------------------------------------
SUBROUTINE TcrKnt()

USE MOD_WQM, ONLY : WSSHI,WSSNET,WS1,WS2,WS1NET,WS2NET,WSL,WSR, &
                  & WSLNET,WSRNET 
USE MOD_HYDROVARS, ONLY : D,DZ,DZ2D 

IMPLICIT NONE
INTEGER :: strBctID 
REAL(SP) :: UnitCnv, fracDenominatorWC, fracDenominatorSD1, fracDenominatorSD2
REAL (SP) :: DCrnSSI_W, DCrnPOC_W, DCrnALG1_W, DCrnALG2_W, DCrnZOO_W, DCrnDOC_W
REAL (SP) :: DCrnSSI_INS1, DCrnPOC_INS1, DCrnAG1_INS1, DCrnAG2_INS1
REAL (SP) :: DCrnSSI_INS2, DCrnPOC_INS2, DCrnAG1_INS2, DCrnAG2_INS2,KLO2
REAL (SP) :: SldConLay1, SldConLay2, SSIConR1, SSIConR2, TotSldCon1, TotSldCon2
REAL (SP) :: DCrnSSI_OUTS1, DCrnPOC_OUTS1, DCrnDOC_OUTS1, GRADC12, DCrnDOC_DFFS1, DCrnDOC_DFFS2
REAL (SP) :: TOTDCrn_INS1, TOTDCrn_INS2, Rg, T_abs, VOLVEL,KRATIO, KLCFF,KGCFF

  strBctID = NCP - NBTCR + 1
  ConTcr(:,:,:) = C2(:,:,strBctID:NCP)

!------------------------------ Bacteria kinetics --------------------------------------------------
! I0 - units are in E/m2/day - E = Einstein - The energy of 1 mole of photons
! 1 E = 1 mol × NA h f = 1 mol × 6.02214076×10e23 mol−1 × 6.62607015×10e-34 J s × f = 3.9903127128934321×10e-10 J s × f
! f is the frequency  = c/lambda, lambda =  wave length (m), c = speed of light (m/s)
! So, E to J conversion depends on the frequency, for short light waves - (400 - 700 nm), average frequency = 550 nm
! 1 E = 217502.84661581277 J
! 1 J = 0.239006 cal
! 1 E/m2/day  =>  factor cal/cm2/day => (51984.48535825895/1E6) cal/cm2/day
IF(TRIM(KINETICSWT) == 'BACTERIAON') THEN
DWTCR = 0.0
UnitCnv = 0.05198448535825895
  DO K=1, KBM1
    DO I=1, MLOC 
      KBact = (KnBact + C0SalBact*SALT(I,K) + AlphaBact*UnitCnv*I0)*((ThetaBact**(T(I,K) - 20)))
      !KBact = 1.0 ! wfni
      !KBact = 0.5*(0.1 + 0.05*SALT(I,K))
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
IF(TRIM(KINETICSWT) == 'RADIONUCON') THEN

  SldConLay1 = 1.1E06   ! g/m3
  SldConLay2 = 1.6E06   ! g/m3

!------------------------ Updating the porosities ----------------------------------
    DO I=1, MLOC

      SSIConR1 = TCRHT2(I)/(TCRHT1(I) + TCRHT2(I))
      SSIConR2 = TCRHT1(I)/(TCRHT1(I) + TCRHT2(I))

      TotSldCon1 = SSD_MSS(I)*SSIConR1 + POCCNS1(I) + SldConLay1
      TotSldCon2 = SSD_MSS(I)*SSIConR2 + POCCNS2(I) + SldConLay2

      PSRT1(I) = 1.0 - TotSldCon1/SED_DEN
      PSRT2(I) = 1.0 - TotSldCon2/SED_DEN
  END DO
!-------------------------------------------------------------------------------------


!------------------------- Computing fractions for RNs --------------------------------
 DO I=1, MLOC
   DO K=1, KBM1
     DO j=1, NBTCR
     !------------------ For the water column ------------------------------------------
      fracDenominatorWC = 1.0 + KPrnSSI_W(I,K,j)*C2(I,K,3) + KPrnPOC_W(I,K,j)*(C2(I,K,11) + &
                          & C2(I,K,12)) + KPrnBOC1_W(I,K,j)*C2(I,K,4) + KPrnBOC2_W(I,K,j)*C2(I,K,5) + &
                          & KPrnZOO_W(I,K,j)*(C2(I,K,7) + C2(I,K,8)) + KPrnDOC_W(I,K,j)*(C2(I,K,9) + C2(I,K,10))

      FDrn_W(I,K,j) = 1.0/fracDenominatorWC    ! dissolved RNs fraction
      FPrnSSI_W(I,K,j) = (KPrnSSI_W(I,K,j)*C2(I,K,3)) / fracDenominatorWC    ! RNs fraction in SSI
      FPrnBOC1_W(I,K,j) = (KPrnBOC1_W(I,K,j)*C2(I,K,4)) / fracDenominatorWC  ! RNs fraction in algae 1
      FPrnBOC2_W(I,K,j) = (KPrnBOC2_W(I,K,j)*C2(I,K,5)) / fracDenominatorWC  ! RNs fraction in algae 1
      FPrnZOO_W(I,K,j) = (KPrnZOO_W(I,K,j)*(C2(I,K,7) + C2(I,K,8))) / fracDenominatorWC  ! RNs fraction in zooplankton

      FPrnPOC_W(I,K,j) = (KPrnPOC_W(I,K,j)*(C2(I,K,11) + C2(I,K,12))) / fracDenominatorWC ! RNs fraction in POC
      FPrnDOC_W(I,K,j) = (KPrnDOC_W(I,K,j)*(C2(I,K,9) + C2(I,K,10))) / fracDenominatorWC  ! RNs fraction in DOC

    END DO

   END DO
  !----------------------- For the sediment layer 1----------------------------------------
     DO j=1, NBTCR
       fracDenominatorSD1 = PSRT1(I) + KPrnSSI_S1(I,j)*SSD_MSS(I) + KPrnDOC_S1(I,j)*DOCCNS1(I) + &
                            & + KPrnPOC_S1(I,j)*POCCNS1(I)

      FDrn_WS1(I,j) = PSRT1(I)/fracDenominatorSD1      ! dissolved RNs fraction in sediment layer -1
      FPrnDOC_S1(I,j) = KPrnDOC_S1(I,j)*DOCCNS1(I) / fracDenominatorSD1 ! RNs fraction in DOC sdlyr-1
      FPrnSSI_S1(I,j) = KPrnSSI_S1(I,j)*SSD_MSS(I) / fracDenominatorSD1   ! RNs fraction in SSI sdlyr -1
      FPrnPOC_S1(I,j) = KPrnPOC_S1(I,j)*POCCNS1(I) / fracDenominatorSD1  ! RNs fraction in POC sdlyr -1

 !----------------------- For the sediment layer 2 ----------------------------------------

       fracDenominatorSD2 = PSRT2(I) + KPrnSSI_S2(I,j)*SSD_MSS(I) + KPrnDOC_S2(I,j)*DOCCNS2(I) + &
                            & + KPrnPOC_S2(I,j)*POCCNS2(I)
      FDrn_WS2(I,j) = PSRT2(I) / fracDenominatorSD2
      FPrnDOC_S2(I,j) = KPrnDOC_S2(I,j)*DOCCNS2(I) / fracDenominatorSD2
      FPrnSSI_S2(I,j) = KPrnSSI_S2(I,j)*SSD_MSS(I) / fracDenominatorSD2
      FPrnPOC_S2(I,j) = KPrnPOC_S2(I,j)*POCCNS2(I) / fracDenominatorSD2
    END DO
 END DO

!---------------------------------------- RNs partitioning and other kinetics ----------------------- 
!---------------------------------------- For top layer of the water column -------------------------- 
DWTCR = 0.0
  DO I=1, MLOC 
    DO J=1, NBTCR

      DCrnSSI_W = (WSSHI (I,1)*ConTcr(I,1,j)*FPrnSSI_W(I,1,j)) / (D(I)*DZ2D(I,1)) / 86400.0
      DCrnPOC_W = (WSL (I,1)*ConTcr(I,1,j)*FPrnPOC_W(I,1,j)) / (D(I)*DZ2D(I,1)) / 86400.0
      DCrnALG1_W = (WS1 (I,1)*ConTcr(I,1,j)*FPrnBOC1_W(I,1,j)) / (D(I)*DZ2D(I,1)) / 86400.0
      DCrnALG2_W = (WS2 (I,1)*ConTcr(I,1,j)*FPrnBOC2_W(I,1,j)) / (D(I)*DZ2D(I,1)) / 86400.0

      DWTCR(I,1,J) = -DCrnSSI_W - DCrnPOC_W - DCrnALG1_W - DCrnALG2_W
    END DO
  END DO
!----------------------------------------------------------------------------------------------------- 

!---------------------------------------- For middle layers of WC ------------------------------------ 
 DO K=2,KBM1
   DO I=1,MLOC
     DO j=1,NBTCR

       DCrnSSI_W = (WSSHI (I,K-1)*ConTcr(I,K-1,j)*FPrnSSI_W(I,K-1,j) - WSSHI (I,K)*ConTcr(I,K,j)*FPrnSSI_W(I,K,j)) &
                & / (D(I)*DZ2D(I,K)) /86400.0
       DCrnPOC_W = (WSL (I,K-1)*ConTcr(I,K-1,j)*FPrnPOC_W(I,K-1,j) - WSL (I,K)*ConTcr(I,K,j)*FPrnPOC_W(I,K,j)) &
                & / (D(I)*DZ2D(I,K)) /86400.0
       DCrnALG1_W = (WS1 (I,K-1)*ConTcr(I,K-1,j)*FPrnBOC1_W(I,K-1,j) - WS1 (I,K)*ConTcr(I,K,j)*FPrnBOC1_W(I,K,j)) &
                & / (D(I)*DZ2D(I,K)) /86400.0
       DCrnALG2_W = (WS2 (I,K-1)*ConTcr(I,K-1,j)*FPrnBOC2_W(I,K-1,j) - WS2 (I,K)*ConTcr(I,K,j)*FPrnBOC2_W(I,K,j)) &
                & / (D(I)*DZ2D(I,K)) /86400.0

        DWTCR(I,K,j) = DCrnSSI_W + DCrnPOC_W + DCrnALG1_W + DCrnALG2_W

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
      DFFVS12W(I) = (69.35*PSRT1(I)*(MolWghtCng(j)**(-2.0/3.0)))/365.0   ! Diffusive mixing velocity (Chapra, 1997) (m/d)

      DCrnSSI_W = ((WSSHI (I,KBM1) - WSSNET(I))*ConTcr(I,KBM1,j)*FPrnSSI_W(I,KBM1,j)) &
              & / (D(I)*DZ2D(I,KBM1)) / 86400.0   ! similar to resuspension
      DCrnPOC_W = ((WSL (I,KBM1) - WSLNET(I))*ConTcr(I,KBM1,j)*FPrnPOC_W(I,KBM1,j)) &
              & / (D(I)*DZ2D(I,KBM1)) / 86400.0
      DCrnALG1_W = ((WS1 (I,KBM1) - WS1NET(I))*ConTcr(I,KBM1,j)*FPrnBOC1_W(I,KBM1,j)) &
              & / (D(I)*DZ2D(I,KBM1)) / 86400.0
      DCrnALG2_W = ((WS2 (I,KBM1) - WS2NET(I))*ConTcr(I,KBM1,j)*FPrnBOC2_W(I,KBM1,j)) &
              & / (D(I)*DZ2D(I,KBM1)) / 86400.0
      DCrnDOC_W = DFFVS12W(I)*(ConTcrSdm1(I,j)*FPrnDOC_S1(I,j) - ConTcr(I,KBM1,j)*FPrnDOC_W(I,KBM1,j)) / &
                  & TCRHT1(I) / 86400.0      ! Diffusive mixing with sediment layer
      !DCrnDOC_W = 0.0

       DWTCR(I,KBM1,j) = DWTCR(I,KBM1,j) + DCrnSSI_W + DCrnPOC_W + DCrnALG1_W + DCrnALG2_W &
                          & + DCrnDOC_W

   END DO
 END DO
!--------------------------------------------------------------------------------------- 
!-------------------- RNs kinetics in sediment layer -----------------------------------
!-------------------- sediment layer -1 ------------------------------------------------
DO I=1, MLOC
  DO j=1, NBTCR
    DCrnSSI_INS1 = (WSSNET(I)*ConTcr(I,KBM1,j)*FPrnSSI_W(I,KBM1,j)) / TCRHT1(I) / 86400.0
    DCrnPOC_INS1 = (WSLNET(I)*ConTcr(I,KBM1,j)*FPrnPOC_W(I,KBM1,j)) / TCRHT1(I) / 86400.0
    DCrnAG1_INS1 = (WS1NET(I)*ConTcr(I,KBM1,j)*FPrnBOC1_W(I,KBM1,j)) / TCRHT1(I) / 86400.0
    DCrnAG2_INS1 = (WS2NET(I)*ConTcr(I,KBM1,j)*FPrnBOC2_W(I,KBM1,j)) / TCRHT1(I) / 86400.0
    TOTDCrn_INS1 = (DCrnSSI_INS1 + DCrnPOC_INS1 + DCrnAG1_INS1 + DCrnAG2_INS1)*(TCRHT2(I)/(TCRHT1(I)+TCRHT2(I)))

    !DCSSI_OUTS1 = WSSI12(I)*CONPCB_S1(I,j)*FP_SSI_S1(I,j) / TXCHT1(I) / 86400.0
    !DCPOC_OUTS1 = WPOC12(I)*CONPCB_S1(I,j)*FP_POC_S1(I,J) / TXCHT1(I) / 86400.0
    DCrnDOC_OUTS1 = DFFVS12W(I)*(ConTcrSdm1(I,j)*FPrnDOC_S1(I,j) - ConTcr(I,KBM1,j)*FPrnDOC_W(I,KBM1,j)) / &
                              & TCRHT1(I) / 86400.0   ! Diffusive mixing with water column
    GRADC12 = (ConTcrSdm1(I,j)*FPrnDOC_S1(I,j) - ConTcrSdm2(I,j)*FPrnDOC_S2(I,j))/(0.5*(TCRHT1(I)+TCRHT2(I)))
    DCrnDOC_DFFS1 = DFFCOEFS12*GRADC12/TCRHT1(I)    ! Molecular diffusion into sdlyr 2

    !DCDOC_OUTS1 = 0.0
    !DCDOC_DFFS1 = 0.0

    DSTCR(I,j,1) = TOTDCrn_INS1 - DCrnDOC_OUTS1 - DCrnDOC_DFFS1


  END DO
END DO
!----------------------------------------------------------------------------------------
!----------------------------------sediment layer 2 -------------------------------------

DO I=1, MLOC
  DO j=1, NTXVB

    DCrnSSI_INS2 = (WSSNET(I)*ConTcr(I,KBM1,j)*FPrnSSI_W(I,KBM1,j)) / TCRHT2(I) / 86400.0
    DCrnPOC_INS2 = (WSLNET(I)*ConTcr(I,KBM1,j)*FPrnPOC_W(I,KBM1,j)) / TCRHT2(I) / 86400.0
    DCrnAG1_INS2 = (WS1NET(I)*ConTcr(I,KBM1,j)*FPrnBOC1_W(I,KBM1,j)) / TCRHT2(I) / 86400.0
    DCrnAG2_INS2 = (WS2NET(I)*ConTcr(I,KBM1,j)*FPrnBOC2_W(I,KBM1,j)) / TCRHT2(I) / 86400.0
    TOTDCrn_INS2 = (DCrnSSI_INS2 + DCrnPOC_INS2 + DCrnAG2_INS1 + DCrnAG2_INS2)*(TCRHT1(I)/(TCRHT1(I)+TCRHT2(I)))

    !DCSSI_INS2 = WSSI12(I)*CONPCB_S2(I,j)*FP_SSI_S2(I,j) / TXCHT2(I) / 86400.0
    !DCPOC_INS2 = WPOC12(I)*CONPCB_S2(I,j)*FP_POC_S2(I,J) / TXCHT2(I) / 86400.0

    GRADC12 = (ConTcrSdm1(I,j)*FPrnDOC_S1(I,j) - ConTcrSdm2(I,j)*FPrnDOC_S2(I,j))/(0.5*(TCRHT1(I)+TCRHT2(I)))
    DCrnDOC_DFFS2 = DFFCOEFS12*GRADC12/TCRHT2(I)
    !DCDOC_DFFS2 = 0.0

     DSTCR(I,j,2) = TOTDCrn_INS2 + DCrnDOC_DFFS2

  END DO
END DO

END IF


END SUBROUTINE
!------------------------------------------------------------------------------------------------ 






!------------------------------- concentration data extraction ----------------------------------
SUBROUTINE ExtractTcrData 

IMPLICIT NONE 

IF(TRIM(KINETICSWT) == 'BACTERIAON') THEN
    TcrtConW = ConTcr*1E-04     ! FB units back to cfu/100 ml in outputs     
ELSE 
    TcrtConW = ConTcr
END IF

TcrtConSdm = ConTcrSdm1 + ConTcrSdm2

END SUBROUTINE
!----------------------------------------------------------------------------------------------

END MODULE 
