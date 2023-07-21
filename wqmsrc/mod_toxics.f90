MODULE TXC_KNT

  USE MOD_LIMS, Only: MTLOC, KBM1, KB, MLOC, NBTX_PNT
  USE MOD_PREC, Only: SP
  USE MOD_SIZES, Only: MGL, NGL,NCP,NTXVB,NBTCR  ! NTXVB - no. of variables for toxics
  USE MOD_WQM, ONLY: SSI, C1, C2, POC_CONCENS, DOC_CONCENS, WMS, TxcOutVar, NumTxcOutVar, TxcOutput, &
                    & PcbCon_W, PcbCon_AlgW, PcbCon_ZplW, PcbCon_SldW, PcbCon_POCW,PcbConTW, &
                    & PcbConWSZ, PcbConWLZ, PcbCon_DOCW, BioAcc, &
                    & PcbConW_S1, PcbCon_POCS1, PcbCon_DOCS1, PcbCon_SldS1, &
                    & PcbConW_S2, PcbCon_POCS2, PcbCon_DOCS2, PcbCon_SldS2, &
                    & PcbConTS1, PcbConTS2,  &
                    & TXCF_INC,TXCF_PNT,TXCF_OBC, TXCF_INC_SD,PTC_SD, PTC_WC,SED_DEN, & 
                    & DFFCOEFS12, BRLVEL,MolWghtCng,HNRCFF,TxcSdmInitType,TxcSdmUnfm, &
                    & PCBTxcOn,MTLTxcOn,NumTxcOutVar,TxcHotWC,TxcHotstart,TxcHotSD1,TxcHotSD2, PCBOutVrb

  USE MOD_CONTROL, ONLY: SERIAL, MSR, PAR
  USE MOD_PAR, Only: NGID, NHN, HN_LST, NLID
  USE MOD_TYPES, Only: BC
  USE MOD_BCMAP, ONLY : NUT_TM, IOBCN, I_OBC_N, WQOBC, I_OBC_GL, IOBCN_GL
  USE MOD_BCS, ONLY : TXPNT_ID, TX_TM, TXQDIST, TXQDIS, TXCQDIS
  USE MOD_HYDROVARS, ONLY : D
  USE MOD_SED, ONLY : HSED, HSED1
  ! HSED = Thickness of the sediment layer 2 (m)
  ! HSED1 = Thickness of the sediment layer 1 (m)
 
IMPLICIT NONE

 REAL(SP), ALLOCATABLE, DIMENSION (:, :, :) :: RTMPX
!----------------- Auxiliary varibales need for Toxics kinetics ---------------------
 REAL(SP), ALLOCATABLE, DIMENSION (:, :, :) :: KP_SSI_W, CONPCB_W, CONPCB_WGL
 REAL(SP), ALLOCATABLE, DIMENSION (:, :, :) :: KP_POC_W, KP_BOC1_W, KP_DOC_W, KP_ZOO_W, KP_BOC2_W
 REAL(SP), ALLOCATABLE, DIMENSION (:, :) :: CONPCB_S1, CONPCB_S1GL, CONPCB_S2, CONPCB_S2GL
! KP_SSI_W - Partition coefficient for the SSI in water column
! KP_BOC_W - Partition coefficient for BOC in water column
! KP_POC_W - Partition coefficient for POC in water column
! KP_DOC_W - Partition coefficient for DOC in water column
 REAL(SP), ALLOCATABLE, DIMENSION (:, :, :) :: FP_SSI_W, FD_W, DWPCB
 REAL(SP), ALLOCATABLE, DIMENSION (:, :, :) :: FP_POC_W, FP_BOC1_W, FP_DOC_W, FP_ZOO_W, FP_BOC2_W
 REAL(SP), ALLOCATABLE, DIMENSION (:, :, :) :: DSPCB
! FP_SSI_W - particulate fraction in the water column
! FD_W - dissolved fraction in the water column
! FP_BOC_W - particulate fraction of BOC in water column
! FP_POC_W - particulate fraction of POC in water column
! FP_DOC_W - particulate fraction of DOC in water column
 REAL(SP), ALLOCATABLE, DIMENSION (:) :: SSDL, SSDL_GL, SSIWC_GL, SSD_MSS, SLDMS1, SLDMS2
 REAL(SP), ALLOCATABLE, DIMENSION (:) :: TXCHT1, TXCHT2, POCCNS1, POCCNS2, DOCCNS1, DOCCNS2
 REAL(SP), ALLOCATABLE, DIMENSION (:,:) :: FD_WS1, FP_SSI_S1,FP_POC_S1,FP_DOC_S1
 REAL(SP), ALLOCATABLE, DIMENSION (:,:) :: FD_WS2, FP_SSI_S2,FP_POC_S2,FP_DOC_S2
 REAL(SP), ALLOCATABLE, DIMENSION (:,:) :: KP_POC_S1, KP_DOC_S1,KP_SSI_S1,KP_POC_S2, KP_SSI_S2, KP_DOC_S2
 REAL(SP), ALLOCATABLE, DIMENSION (:) ::  PSRT1, PSRT1_GL,PSRT2, PSRT2_GL
 REAL(SP), ALLOCATABLE, DIMENSION (:) :: WSSI12, WPOC12, DFFVS12W
 REAL(SP) ::  SED_DEN1, SED_DEN2
! FD_S - dissolved fraction of toxics in sediment layer
! FP_SSI_S - particulate fraction of toxics in sediment layer
! KP_SSI_S - Partition coefficient of SSI for sediment
! KP_POC_S - Partition coefficient of POC for sediment
! KP_DOC_S - Partition coefficient of DOC for sediment
! PRST1 - porosity of layer 1
! PRST2 - Porosity of layer 2
 INTEGER :: TXCST, I, K, JCON, JJ, JT, JK, J, CNT, NJ, NOCHN_IDS, NumCng
 CHARACTER(LEN = 1024) :: FLAG_SD_INI, FLAG_WIND
 INTEGER, ALLOCATABLE, DIMENSION(:) :: TEMP, TEMP2, TEMP3, CHN_IDS
 REAL(SP) :: WNSSI, WNPOC, WINDVEL

 CONTAINS

!-------------------------------------------------------------------------------------
SUBROUTINE TXC_ALLOC

!----------------- Toxics concentrations in each constituents--------------------------------------------------
          ALLOCATE(PcbCon_W(0:MTLOC,KBM1,NTXVB))
          PcbCon_W = 0.0 
          ALLOCATE(PcbCon_AlgW(0:MTLOC,KBM1,NTXVB))
          PcbCon_AlgW = 0.0 
          ALLOCATE(PcbCon_ZplW(0:MTLOC,KBM1,NTXVB))
          PcbCon_ZplW = 0.0 
          ALLOCATE(PcbCon_SldW(0:MTLOC,KBM1,NTXVB))
          PcbCon_SldW = 0.0 
          ALLOCATE(PcbCon_POCW(0:MTLOC,KBM1,NTXVB))
          PcbCon_POCW = 0.0
          ALLOCATE(PcbCon_DOCW(0:MTLOC,KBM1,NTXVB))
          PcbCon_DOCW = 0.0
          !ALLOCATE(PcbConW_GL(MGL,KBM1,NTXVB))
          !PcbConW_GL = 0.0
          !ALLOCATE(PcbConAlgW_GL(MGL,KBM1,NTXVB))
          !PcbConAlgW_GL = 0.0 
          !ALLOCATE(PcbConZplW_GL(MGL,KBM1,NTXVB))
          !PcbConZplW_GL = 0.0 
          !ALLOCATE(PcbConSldW_GL(MGL,KBM1,NTXVB))
          !PcbConSldW_GL = 0.0 
          !ALLOCATE(PcbConPOCW_GL(MGL,KBM1,NTXVB))
          !PcbConPOCW_GL = 0.0
          ALLOCATE(PcbConW_S1(0:MTLOC,NTXVB))
          PcbConW_S1 = 0.0 
          ALLOCATE(PcbConW_S2(0:MTLOC,NTXVB))
          PcbConW_S2 = 0.0 
          ALLOCATE(PcbCon_POCS1(0:MTLOC,NTXVB))
          PcbCon_POCS1 = 0.0 
          ALLOCATE(PcbCon_POCS2(0:MTLOC,NTXVB))
          PcbCon_POCS2 = 0.0 
          ALLOCATE(PcbCon_DOCS1(0:MTLOC,NTXVB))
          PcbCon_DOCS1 = 0.0 
          ALLOCATE(PcbCon_DOCS2(0:MTLOC,NTXVB))
          PcbCon_DOCS2 = 0.0 
          ALLOCATE(PcbCon_SldS1(0:MTLOC,NTXVB))
          PcbCon_SldS1 = 0.0 
          ALLOCATE(PcbCon_SldS2(0:MTLOC,NTXVB))
          PcbCon_SldS2 = 0.0 
          !ALLOCATE(PcbConWS1_GL(MGL,NTXVB))
          !PcbConWS1_GL = 0.0 
          !ALLOCATE(PcbConWS2_GL(MGL,NTXVB))
          !PcbConWS2_GL = 0.0 
          !ALLOCATE(PcbConPOCS1_GL(MGL,NTXVB))
          !PcbConPOCS1_GL = 0.0 
          !ALLOCATE(PcbConPOCS2_GL(MGL,NTXVB))
          !PcbConPOCS2_GL = 0.0
          !ALLOCATE(PcbConDOCS1_GL(MGL,NTXVB))
          !PcbConDOCS1_GL = 0.0 
          !ALLOCATE(PcbConDOCS2_GL(MGL,NTXVB))
          !PcbConDOCS2_GL = 0.0
          !ALLOCATE(PcbConSldS1_GL(MGL,NTXVB))
          !PcbConSldS1_GL = 0.0
          !ALLOCATE(PcbConSldS2_GL(MGL,NTXVB))
          !PcbConSldS2_GL = 0.0
          ALLOCATE(PcbConTW(0:MTLOC,KBM1))
          PcbConTW = 0.0 
          ALLOCATE(PcbConTS1(0:MTLOC))
          PcbConTS1 = 0.0 
          ALLOCATE(PcbConTS2(0:MTLOC))
          PcbConTS2 = 0.0
!----------------------------------------------------------------------------------
          ALLOCATE(SSDL(0:MTLOC))
          SSDL = 0.0
          ALLOCATE(FP_SSI_W(0:MTLOC,KBM1,NTXVB))
          FP_SSI_W = 0.0
          ALLOCATE(FP_POC_W(0:MTLOC,KBM1,NTXVB))
          FP_POC_W = 0.0
          ALLOCATE(FP_DOC_W(0:MTLOC,KBM1,NTXVB))
          FP_DOC_W = 0.0
          ALLOCATE(FP_BOC1_W(0:MTLOC,KBM1,NTXVB))
          FP_BOC1_W = 0.0
          ALLOCATE(FP_BOC2_W(0:MTLOC,KBM1,NTXVB))
          FP_BOC2_W = 0.0
          ALLOCATE(FP_ZOO_W(0:MTLOC,KBM1,NTXVB))
          FP_ZOO_W = 0.0
          ALLOCATE(FD_W(0:MTLOC,KBM1,NTXVB))
          FD_W = 0.0
          ALLOCATE(KP_SSI_W(0:MTLOC,KBM1,NTXVB))
          KP_SSI_W = 0.0
          ALLOCATE(KP_POC_W(0:MTLOC,KBM1,NTXVB))
          KP_POC_W = 0.0
          ALLOCATE(KP_DOC_W(0:MTLOC,KBM1,NTXVB))
          KP_DOC_W = 0.0
          ALLOCATE(KP_BOC1_W(0:MTLOC,KBM1,NTXVB))
          KP_BOC1_W = 0.0
          ALLOCATE(KP_BOC2_W(0:MTLOC,KBM1,NTXVB))
          KP_BOC2_W = 0.0
          ALLOCATE(KP_ZOO_W(0:MTLOC,KBM1,NTXVB))
          KP_ZOO_W = 0.0
!----------------------------------------------------------------------------------

          ALLOCATE(FP_SSI_S1(0:MTLOC,NTXVB))
          FP_SSI_S1 = 0.0
          ALLOCATE(FD_WS1(0:MTLOC,NTXVB))
          FD_WS1 = 0.0
          ALLOCATE(KP_SSI_S1(0:MTLOC,NTXVB))
          KP_SSI_S1 = 0.0
          ALLOCATE(KP_POC_S1(0:MTLOC,NTXVB))
          KP_POC_S1 = 0.0
          ALLOCATE(KP_DOC_S1(0:MTLOC,NTXVB))
          KP_DOC_S1 = 0.0
          ALLOCATE(FP_DOC_S1(0:MTLOC,NTXVB))
          FP_DOC_S1 = 0.0
          ALLOCATE(FP_POC_S1(0:MTLOC,NTXVB))
          FP_POC_S1 = 0.0

          ALLOCATE(FP_SSI_S2(0:MTLOC,NTXVB))
          FP_SSI_S2 = 0.0
          ALLOCATE(FD_WS2(0:MTLOC,NTXVB))
          FD_WS2 = 0.0
          ALLOCATE(KP_SSI_S2(0:MTLOC,NTXVB))
          KP_SSI_S2 = 0.0
          ALLOCATE(KP_POC_S2(0:MTLOC,NTXVB))
          KP_POC_S2 = 0.0
          ALLOCATE(KP_DOC_S2(0:MTLOC,NTXVB))
          KP_DOC_S2 = 0.0
          ALLOCATE(FP_DOC_S2(0:MTLOC,NTXVB))
          FP_DOC_S2 = 0.0
          ALLOCATE(FP_POC_S2(0:MTLOC,NTXVB))
          FP_POC_S2 = 0.0
  !------------------ concentration of POC and DOC in sediment layers ------------------------
          ALLOCATE(POC_CONCENS(0:MTLOC,2))
          POC_CONCENS = 0.0
          ALLOCATE(DOC_CONCENS(0:MTLOC,2))
          DOC_CONCENS = 0.0
          ALLOCATE(WSSI12(0:MTLOC))
          WSSI12 = 0.0
          ALLOCATE(WPOC12(0:MTLOC))
          WPOC12 = 0.0
          ALLOCATE(DFFVS12W(0:MTLOC))
          DFFVS12W = 0.0
          ALLOCATE(POCCNS1(0:MTLOC))
          POCCNS1 = 0.0
          ALLOCATE(POCCNS2(0:MTLOC))
          POCCNS2 = 0.0
          ALLOCATE(DOCCNS1(0:MTLOC))
          DOCCNS1 = 0.0
          ALLOCATE(DOCCNS2(0:MTLOC))
          DOCCNS2 = 0.0
          !ALLOCATE(SLDMS1(MTLOC))
          !SLDMS1 = 0.0
          !ALLOCATE(SLDMS2(MTLOC))
          !SLDMS2 = 0.0
!---------------------------------------------------------------------------------------------

          ALLOCATE(PSRT1(0:MTLOC))
          PSRT1 = 0.0
          ALLOCATE(PSRT2(0:MTLOC))
          PSRT2 = 0.0
          ALLOCATE(DSPCB(0:MTLOC,NTXVB,2))
          DSPCB = 0.0
          ALLOCATE(DWPCB(0:MTLOC,KBM1,NTXVB))
          DWPCB = 0.0
          !ALLOCATE(SSDL_GL(MGL))
          !SSDL_GL = 0.0
          !ALLOCATE(PSRT1_GL(MGL))
          !PSRT1_GL = 0.0
          !ALLOCATE(PSRT2_GL(MGL))
          !PSRT2_GL = 0.0
          !ALLOCATE(SSIWC_GL(MGL))
          !SSIWC_GL = 0.0
          ALLOCATE(CONPCB_W(0:MTLOC,KBM1,NTXVB))
          CONPCB_W = 0.0
          ALLOCATE(CONPCB_S1(0:MTLOC,NTXVB))
          CONPCB_S1 = 0.0
          ALLOCATE(CONPCB_S2(0:MTLOC,NTXVB))
          CONPCB_S2 = 0.0
          !ALLOCATE(CONPCB_WGL(MGL,KBM1,NTXVB))
          !CONPCB_WGL = 0.0
          !ALLOCATE(CONPCB_S1GL(MGL,NTXVB))
          !CONPCB_S1GL = 0.0
          !ALLOCATE(CONPCB_S2GL(MGL,NTXVB))
          !CONPCB_S2GL = 0.0

          !ALLOCATE(SSI_GL(0:MGL,KBM1))
          !SSI_GL = 0.0
          ALLOCATE(SSD_MSS(0:MTLOC))
          SSD_MSS = 0.0
          ALLOCATE(TXCHT1(0:MTLOC))
          TXCHT1 = 0.0
          ALLOCATE(TXCHT2(0:MTLOC))
          TXCHT2 = 0.0

          IF(BioAcc == .TRUE.) THEN 
            ALLOCATE(PcbConWSZ(0:MTLOC,KBM1,NTXVB))
            PcbConWSZ = 0.0
            ALLOCATE(PcbConWLZ(0:MTLOC,KBM1,NTXVB))
            PcbConWLZ = 0.0
          END IF




END SUBROUTINE
!--------------------------------------------------------------------------------------



!-------------------------------------------------------------------------------------
SUBROUTINE READ_TXC_DATA()
 IMPLICIT NONE

 INTEGER :: end_file, I1, NUM_NDS, NBTX_PNT_GL, TXQTIME
 INTEGER, ALLOCATABLE, DIMENSION(:) :: OBC_NDS, TXGL_2_LOC_PNT
 REAL(SP), ALLOCATABLE, DIMENSION(:,:) :: RTEMP, RTEMP2
 REAL(SP), ALLOCATABLE, DIMENSION(:,:,:) :: RTEMP1
 REAL(SP), ALLOCATABLE, DIMENSION(:,:) :: TEMPTXC1,TEMPTXC2,TEMPTXC 
 CHARACTER(LEN = 64) :: FLGTXC_OUTPUT
 REAL (SP) :: TTIME, TXC_INIT_SD, UNIT_SCALE, UNIT_SCALE_SD, UNIT_SCL_LDN
 INTEGER :: NumLine


  INCLUDE "mpif.h"


UNIT_SCALE = 1E-06      ! ng/L to g/m^3 unit conversion , data is available in ng/L
UNIT_SCALE_SD = 1E-09   ! ng/g to g/m^3 unit conversion, g/m^3 = ng/g *rho(g/m^3)*UNIT_SCALE_SD
UNIT_SCL_LDN = 1E-09    ! ng/m3   to g/m3 for loading data

 KP_SSI_W = 0.0
 KP_SSI_S1 = PTC_SD
 KP_SSI_S2 = PTC_SD
 KP_SSI_W = 0.0 
 KP_SSI_S1 = 0.0 
 KP_SSI_S2 = 0.0
 KP_BOC1_W = PTC_WC
 KP_BOC2_W = PTC_WC
 KP_DOC_W = PTC_WC*0.01
 KP_POC_W = PTC_WC
 KP_ZOO_W = PTC_WC
 KP_POC_S1 = PTC_SD
 KP_DOC_S1 = PTC_SD
 KP_POC_S2 = PTC_SD
 KP_DOC_S2 = PTC_SD
!------------------------------- initialization of toxics in sediment layer ---------------------------- 

IF(TxcHotstart == .FALSE.) THEN
  IF(TRIM(TxcSdmInitType) == 'UNIFORM') THEN
    CONPCB_S1 = TxcSdmUnfm
    CONPCB_S2 = TxcSdmUnfm
  ELSE
    OPEN(UNIT = 211, FILE = TRIM(TXCF_INC_SD), ACTION = 'READ')
    READ(211,*, IOSTAT = end_file)
    IF(end_file < 0) THEN
          WRITE (*,*) "Error occured - No file for initial conditions for toxics in sediments"
          STOP
    END IF
    ALLOCATE(TEMPTXC1(MGL,NTXVB))
    ALLOCATE(TEMPTXC2(MGL,NTXVB))

    DO I=1, MGL
      READ(211,*) (TEMPTXC1(I,j), j=1,NTXVB)
    END DO
    !TEMPTXC = TEMPTXC*UNIT_SCALE
    TEMPTXC1 = TEMPTXC1*UNIT_SCALE_SD*SED_DEN
    TEMPTXC2 = TEMPTXC1
    CLOSE(211)
  END IF

ELSE
    ALLOCATE(TEMPTXC1(MGL,NTXVB))
    ALLOCATE(TEMPTXC2(MGL,NTXVB))

    OPEN(UNIT = 208, FILE = TRIM(TxcHotSD1), ACTION = 'READ')
      READ(208,*, IOSTAT = end_file)
      IF(end_file < 0) THEN
            WRITE (*,*) "Error occured - No file named inputs/Txc_HotstrSD1.dat"
            STOP
      END IF
      DO I=1, MGL
        READ(208,*) (TEMPTXC1(I,j), j=1,NTXVB)
      END DO
    CLOSE(208)

    OPEN(UNIT = 207, FILE = TRIM(TxcHotSD2), ACTION = 'READ')
      READ(207,*, IOSTAT = end_file)
      IF(end_file < 0) THEN
            WRITE (*,*) "Error occured - No file named inputs/Txc_HotstrSD1.dat"
            STOP
      END IF
      DO I=1, MGL
        READ(207,*) (TEMPTXC2(I,j), j=1,NTXVB)
      END DO
    CLOSE(207)

END IF

IF(TRIM(TxcSdmInitType) /= 'UNIFORM') THEN
    If (SERIAL) THEN
      CONPCB_S1 = TEMPTXC1
      CONPCB_S2 = TEMPTXC2
    END IF


      IF (PAR) THEN
            DO I = 1, MLOC
              CONPCB_S1 (I,:) = TEMPTXC1 (NGID(I),:)
              CONPCB_S2 (I,:) = TEMPTXC2 (NGID(I),:)
            END DO
            DO I = 1, NHN
              CONPCB_S1 (I+MLOC,:) = TEMPTXC1 (HN_LST(I),:)
              CONPCB_S2 (I+MLOC,:) = TEMPTXC2 (HN_LST(I),:)
            END DO
      END IF

    DEALLOCATE(TEMPTXC1)
    DEALLOCATE(TEMPTXC2)
END IF

!----------------------------------------------initializtion of toxic concen in water column -------------------
TXCST = NCP - NTXVB - NBTCR + 1
ALLOCATE (RTMPX(0:MGL, KBM1, TXCST:NCP-NBTCR))         ! NTXVB - toxics related variables for water column

IF(TxcHotstart == .FALSE.) THEN
        OPEN(UNIT = 212, FILE = TRIM(TXCF_INC), ACTION = 'READ')
        READ(212,*, IOSTAT = end_file)
        !WRITE(*,*)TRIM(TXCF_INC)
        !STOP
        IF(end_file < 0) THEN
              WRITE (*,*) "Error occured - No file for initial conditions for toxics.dat"
              STOP
        END IF

        RTMPX = 0.0
        DO I = 1, MGL
                DO K = 1, KBM1
                    READ (212,*) (RTMPX(I, K, JCON), JCON=TXCST, NCP-NBTCR)
                END DO
        END DO
        CLOSE(212)
        RTMPX = RTMPX*UNIT_SCALE
ELSE 
        OPEN(UNIT = 212, FILE = TRIM(TxcHotWC), ACTION = 'READ')
        READ(212,*, IOSTAT = end_file)
        IF(end_file < 0) THEN
            WRITE (*,*) "Error occured - No file named inputs/Txc_HotstrWC.dat"
            STOP
        END IF
        RTMPX = 0.0
        DO I = 1, MGL
          DO K = 1, KBM1
            READ (212,*) (RTMPX(I, K, JCON), JCON=TXCST, NCP)
          END DO
        END DO
        CLOSE(212)
END IF

If (SERIAL) C1(:,:,TXCST:NCP-NBTCR) = RTMPX


         IF (PAR) THEN
            DO JCON = TXCST, NCP-NBTCR
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

DEALLOCATE(RTMPX)

!---------------------------------------------------------------------------------------------
!OPEN(UNIT = 213, FILE = TRIM(TXCF_OBC), ACTION = 'READ')
!READ(213,*)NUM_NDS
!IF(NUM_NDS /= IOBCN_GL) THEN
!  WRITE(*,*)'Number of nodes for OBC does not match with water quality related OBC nodes'
!  STOP
!END IF

!ALLOCATE(OBC_NDS(IOBCN_GL))
!DO J =1,IOBCN_GL
!  READ(213,*)OBC_NDS(j)
!END DO

!#  if defined (1)
!    IF(PAR) THEN
!        DO JT = 1, NUT_TM%NTIMES
!          READ(213,*)
!          DO JJ = TXCST, NCP
!            DO I = 1, IOBCN_GL
!               I1 = NLID(I_OBC_GL(I))
!                IF(I1 == 0) THEN
!                  READ(213,*)
!                  CYCLE
!                END IF

!                DO JK = 1, IOBCN
!                  IF (I1 == I_OBC_N(JK)) THEN
!                    READ(213,*) (WQOBC(JK,K,JT,JJ), K=1, KBM1)
!                    EXIT
!                  ELSE
!                    !WRITE(*,*)'Reading of OBC data for Toxics encountered a problem', I,I1,IOBCN, I_OBC_N
!                    CYCLE
!                 END IF
!                END DO

!              END DO
!            END DO
!        END DO
!    END IF
!# endif
! CLOSE(213)

!-------------------------------------------------------------------------------------------
OPEN(UNIT = 215, FILE = TRIM(TXCF_PNT), ACTION = 'READ')
READ(215,*)
READ(215,*)NBTX_PNT_GL   ! no. of global nodes for toxics point sources
NBTX_PNT = 0

IF (NBTX_PNT_GL > 0) THEN
    ALLOCATE (TEMP(NBTX_PNT_GL), TEMP2(NBTX_PNT_GL), TEMP3(NBTX_PNT_GL))
    DO I = 1, NBTX_PNT_GL  ! reading the global node ids for point sources
      READ (215,*) TEMP (I)
    END DO
    !------------ Mapping ids for serial computing ------------------------
    IF (SERIAL) THEN
       NBTX_PNT = NBTX_PNT_GL
       ALLOCATE (TXPNT_ID(NBTX_PNT))
       TXPNT_ID (:) = TEMP (:)
    END IF
    !------------ Mapping ids for parallel computing -----------------------

             IF (PAR) THEN
                CNT = 0
                DO I = 1, NBTX_PNT_GL
                   IF (NLID(TEMP(I)) /= 0) THEN
                      CNT = CNT + 1
                      TEMP2 (CNT) = NLID (TEMP(I))
                      TEMP3 (CNT) = I
                   END IF
                END DO
                NBTX_PNT = CNT
                ALLOCATE (TXGL_2_LOC_PNT(NBTX_PNT), TXPNT_ID(NBTX_PNT))
                TXPNT_ID (1:CNT) = TEMP2 (1:CNT)! The point locally
                TXGL_2_LOC_PNT (1:CNT) = TEMP3 (1:CNT)! The point globally
             END IF


 DEALLOCATE (TEMP, TEMP2, TEMP3)

!---------------- Read the flow distribution for each node --------------------------
 ALLOCATE (RTEMP(NBTX_PNT_GL, KBM1))
DO I = 1, NBTX_PNT_GL
   READ (215,*) J, (RTEMP(I, K), K=1, KBM1)
END DO
ALLOCATE (TXQDIST(NBTX_PNT, KBM1))
If (SERIAL) TXQDIST (1:NBTX_PNT, :) = RTEMP (1:NBTX_PNT_GL, :)
!-------------------------------------------------------------------------------------

         IF (PAR) THEN
            TXQDIST (1:NBTX_PNT, :) = RTEMP (TXGL_2_LOC_PNT(1:NBTX_PNT),:)
         END IF

!-------------------------------------------------------------------------------------
!---------------- Reading the Tx concentration data ----------------------------------
READ(215,*) TXQTIME
TX_TM%NTIMES = TXQTIME
TX_TM%LABEL = "TX-point source"
ALLOCATE (TX_TM%TIMES(TXQTIME))
ALLOCATE (RTEMP1(NBTX_PNT_GL, TXCST:NCP-NBTCR, TXQTIME))
ALLOCATE (RTEMP2(NBTX_PNT_GL, TXQTIME))
!------------------------------------------------------------------------------------
DO I = 1, TXQTIME
   READ (215,*) TTIME
   TX_TM%TIMES (I) = TTIME
   READ (215,*) (RTEMP2(J, I), J=1, NBTX_PNT_GL)
   DO NJ = TXCST,NCP-NBTCR
      READ (215,*) (RTEMP1(J, NJ, I), J=1, NBTX_PNT_GL)
   END DO
END DO

RTEMP1 = RTEMP1*UNIT_SCL_LDN

!-------------- Transforming data into local arrays --------------------------------
ALLOCATE (TXQDIS(NBTX_PNT,TXQTIME))
ALLOCATE (TXCQDIS(NBTX_PNT, TXCST:NCP-NBTCR, TXQTIME))

IF(SERIAL) THEN
  TXQDIS(1:NBTX_PNT,:) = RTEMP2 (1:NBTX_PNT_GL,:)
  TXCQDIS(1:NBTX_PNT,:,:) = RTEMP1 (1:NBTX_PNT_GL,:,:)
END IF


         IF (PAR) THEN
            TXQDIS (1:NBTX_PNT, :) = RTEMP2 (TXGL_2_LOC_PNT(1:NBTX_PNT), :)
            TXCQDIS (1:NBTX_PNT, :, :) = RTEMP1 (TXGL_2_LOC_PNT(1:NBTX_PNT), :, :)
            DEALLOCATE (TXGL_2_LOC_PNT)
         END IF


DEALLOCATE (RTEMP, RTEMP1, RTEMP2)
CLOSE(215)
END IF
HSED1 = 0.1
HSED = 0.1

!----------------------- information for outputing toxicant concentration data ----------------  
IF(NumTxcOutVar > 0) THEN 
  TxcOutput = .TRUE.
  ALLOCATE(TxcOutVar(NumTxcOutVar))
  DO I=1,NumTxcOutVar
    TxcOutVar(I) = PCBOutVrb(I)
  END DO 
ELSE 
  TxcOutput = .FALSE.
END IF 

END SUBROUTINE
!-------------------------------------------------------------------------------------


!------------------------------------------------------------------------------------
SUBROUTINE SLD_KNT(TMSTEP)
  USE MOD_WQM, ONLY: TTL_MASS, C2, TTL_MASS2, TTL_MASS3, TTL_MASS3_GL
  USE MOD_HYDROVARS, ONLY : D,DZ,ART1,DZ2D
  IMPLICIT NONE

 REAL(SP) :: SUM
 REAL(SP), INTENT(IN) :: TMSTEP
 INTEGER :: txcID

!----------------------- Accumulation of total solid mass (SSI) -------------------------
 TXCHT1 = HSED1       ! obtain thickness of sediment layer - 1
 TXCHT2 = HSED        ! obtain thickness of sediment layer - 2

! TXCHT1 = 0.1
! TXCHT2 = 0.9

 txcID = NCP - NTXVB - NBTCR + 1
 DO I=1, MLOC
   DO J=1, KBM1
     !TTL_MASS(I) = TTL_MASS(I) + C2(I,J,3)*ART1(I)*D(I)*DZ2D(I,KBM1)*0.001
      CONPCB_W(I,J,:) = C2(I,J,txcID:NCP-NBTCR)

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

 END DO

END SUBROUTINE
!---------------------------------------------------------------------------------------


!!---------------------------------------------------------------------------------------
!!--------- auxilliary subroutine to compute 1D mass accumulation for analytical solutions--------
!SUBROUTINE OneDimMassAcc()

!  USE MOD_WQM, ONLY : TTL_MASS, TTL_MASS_GL,M1DMass, ART1_GL, D_GL, M1SDMass, &
!                      & PCB_W, PCB_S, PORST1, PORST2
!  USE MOD_PREC, ONLY: SP
!  IMPLICIT NONE

!  INTEGER :: j,k,I
!  REAL(SP) :: TOT_VOL, SED_VOL

!  k = MGL
!  j = 1

!  DO WHILE (k > 0)
!    M1DMass(j) = 0.0
!    M1SDMass(j) = 0.0
!    PCB_W(j,:) = 0.0
!    PCB_S(j,:) = 0.0
!    PORST1(j) = 0.0
!    PORST2(j) = 0.0
!    TOT_VOL = 0.0
!    SED_VOL = 0.0
!    DO I = k-4, k
!      !M1DMass(j) = M1DMass(j) + TTL_MASS_GL(I)   ! accumulated mass along sections (1D)
!      M1SDMass(j) = M1SDMass(j) + SSDL_GL(I)     ! accumulated sediment mass along sections (1D)
!      !M1SDMass(j) = M1SDMass(j) + PSRT_GL(I)
!      !TOT_VOL = TOT_VOL + ART1_GL(I)*D_GL(I)
!      !SED_VOL = SED_VOL + ART1_GL(I)*1.0
!      PCB_W(j,:) = PCB_W(j,:) + CONPCB_WGL(I,KBM1,:)
!      PCB_S(j,:) = PCB_S(j,:) + CONPCB_S1GL(I,:)
!      PORST1(j) = PORST1(j) + PSRT1_GL(I)
!      PORST2(j) = PORST2(j) + PSRT2_GL(I)

!    END DO
!      M1DMass(j) = M1DMass(j)/5.0         ! average concentration across a section
!      !M1DMass(j) =   M1DMass(j)/TOT_VOL  ! Mass is converted into concentration
!      !M1SDMass(j) = M1SDMass(j)/SED_VOL  ! sediment mass converted into concentration
!      M1SDMass(j) = M1SDMass(j)/5.0
!      PCB_W(j,:) = PCB_W(j,:)/5.0
!      PCB_S(j,:) = PCB_S(j,:)/5.0
!      PORST1(j) = PORST1(j)/5.0
!      PORST2(j) = PORST2(j)/5.0
!    k = k - 5
!    j = j + 1
!  END DO

!END SUBROUTINE
!!--------------------------------------------------------------------------------------


!--------------------------------------------------------------------------------------
!--------------- Computing the fractions for toxicants in different phases ------------
SUBROUTINE UpdtTX_FRCNS
USE MOD_WQM, ONLY : TTL_MASS
 IMPLICIT NONE

 REAL(SP) :: fracDenominatorWC, fracDenominatorSD1, fracDenominatorSD2


 DO I=1, MLOC
   DO K=1, KBM1
     DO j=1,NTXVB
     !------------------ For the water column ------------------------------------------
      ! fracDenominatorWC = 1.0 + KP_SSI_W(I,K,j)*C2(I,K,3) + KP_POC_W(I,K,j)*(C2(I,K,11) + &
      !                     & C2(I,K,12)) + KP_BOC1_W(I,K,j)*C2(I,K,4) + KP_BOC2_W(I,K,j)*C2(I,K,5) + &
      !                     & KP_ZOO_W(I,K,j)*(C2(I,K,7) + C2(I,K,8)) + KP_DOC_W(I,K,j)*(C2(I,K,9) + C2(I,K,10))

      fracDenominatorWC = 1.0 +  KP_POC_W(I,K,j)*(C2(I,K,11) + &
                          & C2(I,K,12)) + KP_BOC1_W(I,K,j)*C2(I,K,4) + KP_BOC2_W(I,K,j)*C2(I,K,5) + &
                          & KP_DOC_W(I,K,j)*(C2(I,K,9) + C2(I,K,10))   ! No zooplankton

      !fracDenominatorWC = 1.0 +  KP_POC_W(I,K,j)*(C2(I,K,11) + &
      !                    & C2(I,K,12)) + KP_BOC1_W(I,K,j)*C2(I,K,4) + KP_BOC2_W(I,K,j)*C2(I,K,5)   ! No DOC

      
      
      FD_W(I,K,j) = 1.0/fracDenominatorWC    ! dissolved toxicant fraction
      !FP_SSI_W(I,K,j) = (KP_SSI_W(I,K,j)*C2(I,K,3)) / fracDenominatorWC    ! toxicant fraction in SSI
      FP_BOC1_W(I,K,j) = (KP_BOC1_W(I,K,j)*C2(I,K,4)) / fracDenominatorWC  ! toxicant fraction in algae 1
      FP_BOC2_W(I,K,j) = (KP_BOC2_W(I,K,j)*C2(I,K,5)) / fracDenominatorWC  ! toxicant fraction in algae 1
      !FP_ZOO_W(I,K,j) = (KP_ZOO_W(I,K,j)*(C2(I,K,7) + C2(I,K,8))) / fracDenominatorWC  ! toxicant fraction in zooplankton
      FP_ZOO_W(I,K,j) = 0.0
      FP_SSI_W(I,K,j) = 0.0

      FP_POC_W(I,K,j) = (KP_POC_W(I,K,j)*(C2(I,K,11) + C2(I,K,12))) / fracDenominatorWC ! toxicant fraction in POC
      FP_DOC_W(I,K,j) = (KP_DOC_W(I,K,j)*(C2(I,K,9) + C2(I,K,10))) / fracDenominatorWC  ! toxicant fraction in DOC
      !FP_DOC_W(I,K,j) = 0.0



    END DO

   END DO
  !----------------------- For the sediment layer 1----------------------------------------
     DO j=1,NTXVB
       fracDenominatorSD1 = PSRT1(I) + KP_SSI_S1(I,j)*SSD_MSS(I) + KP_DOC_S1(I,j)*DOCCNS1(I) + &
                            & + KP_POC_S1(I,j)*POCCNS1(I)

      FD_WS1(I,j) = PSRT1(I)/fracDenominatorSD1      ! dissolved toxicant fraction in sediment layer -1
      FP_DOC_S1(I,j) = KP_DOC_S1(I,j)*DOCCNS1(I) / fracDenominatorSD1 ! toxicant fraction in DOC sdlyr-1
      FP_SSI_S1(I,j) = KP_SSI_S1(I,j)*SSD_MSS(I) / fracDenominatorSD1   ! toxicant fraction in SSI sdlyr -1
      FP_POC_S1(I,j) = KP_POC_S1(I,j)*POCCNS1(I) / fracDenominatorSD1  ! toxicant fraction in POC sdlyr -1

 !----------------------- For the sediment layer 2 ----------------------------------------

       fracDenominatorSD2 = PSRT2(I) + KP_SSI_S2(I,j)*SSD_MSS(I) + KP_DOC_S2(I,j)*DOCCNS2(I) + &
                            & + KP_POC_S2(I,j)*POCCNS2(I)
      FD_WS2(I,j) = PSRT2(I) / fracDenominatorSD2
      FP_DOC_S2(I,j) = KP_DOC_S2(I,j)*DOCCNS2(I) / fracDenominatorSD2
      FP_SSI_S2(I,j) = KP_SSI_S2(I,j)*SSD_MSS(I) / fracDenominatorSD2
      FP_POC_S2(I,j) = KP_POC_S2(I,j)*POCCNS2(I) / fracDenominatorSD2
    END DO
 END DO

END SUBROUTINE
!--------------------------------------------------------------------------------------


!--------------------------------------------------------------------------------------
SUBROUTINE CalcPorst

  USE MOD_HYDROVARS, ONLY : ART1
  IMPLICIT NONE

  REAL(SP) :: SSI_CON_RT1, SSI_CON_RT2, TOT_SLD_CONCEN1, TOT_SLD_CONCEN2
  REAL(SP) :: SLD_CONCEN1, SLD_CONCEN2
  !SED_DEN = 1.7E06  ! g/m3
  SLD_CONCEN1 = 1.1E06   ! g/m3
  SLD_CONCEN2 = 1.6E06   ! g/m3


  DO I=1, MLOC
      !SSI_CON_RT1 = HSED1(I)/(HSED(I) + HSED1(I))
      !SSI_CON_RT2 = HSED(I)/(HSED(I) + HSED1(I))

      SSI_CON_RT1 = TXCHT2(I)/(TXCHT1(I) + TXCHT2(I))
      SSI_CON_RT2 = TXCHT1(I)/(TXCHT1(I) + TXCHT2(I))

      !POC_CONCENS = mg/m3

      TOT_SLD_CONCEN1 = SSD_MSS(I)*SSI_CON_RT1 + POCCNS1(I)
      TOT_SLD_CONCEN2 = SSD_MSS(I)*SSI_CON_RT2 + POCCNS2(I)

      !TOT_SLD_CONCEN1 = SSD_MSS(I)*SSI_CON_RT1
      !TOT_SLD_CONCEN2 = SSD_MSS(I)*SSI_CON_RT2
      TOT_SLD_CONCEN1 = TOT_SLD_CONCEN1 + SLD_CONCEN1
      TOT_SLD_CONCEN2 = TOT_SLD_CONCEN2 + SLD_CONCEN2

      PSRT1(I) = 1.0 - TOT_SLD_CONCEN1/SED_DEN
      PSRT2(I) = 1.0 - TOT_SLD_CONCEN2/SED_DEN
  END DO

END SUBROUTINE
!--------------------------------------------------------------------------------------


!--------------------------------------------------------------------------------------
SUBROUTINE PCB_KNT()
  USE MOD_HYDROVARS, ONLY : D,DZ,DZ2D
  USE MOD_WQM, ONLY : WSSHI,WSSNET,WS1,WS2,WS1NET,WS2NET,WSL,WSR, &
                  & WSLNET,WSRNET
IMPLICIT NONE

  REAL (SP) :: DC_SSI_W, DC_POC_W, DC_ALG1_W, DC_ALG2_W, DC_ZOO_W, DC_DOC_W, DC_VOL_W
  REAL (SP) :: DCSSI_INS1, DCPOC_INS1, DCAG1_INS1, DCAG2_INS1
  REAL (SP) :: DCSSI_INS2, DCPOC_INS2, DCAG1_INS2, DCAG2_INS2,KLO2
  REAL (SP) :: DCSSI_OUTS1, DCPOC_OUTS1, DCDOC_OUTS1, GRADC12, DCDOC_DFFS1, DCDOC_DFFS2
  REAL (SP) :: TOTDC_INS1, TOTDC_INS2, Rg, T_abs, VOLVEL,KRATIO, KLCFF,KGCFF
  REAL (SP), DIMENSION(0:MTLOC,KBM1,NTXVB) :: DMPCB_W
  REAL (SP), DIMENSION(0:MTLOC,NTXVB, 2) :: DMPCB_S

  DMPCB_S = 0.0
  DMPCB_W = 0.0

!--------------------- Toxic kinetics in top layer of the WC ----------------------------
KLO2 = 0.728*(WMS**0.5) - 0.317*WMS + 0.0372*(WMS**2.0)          ! Banks-Herrera equation (m/d)
T_abs = 283.0
Rg = 8.206E-05
 DO I=1,MLOC
   DO j=1,NTXVB
     KLCFF = KLO2*((32.0/MolWghtCng(j))**0.25)*365.0      ! m/yr
     KGCFF = 61320.0*((18/MolWghtCng(j))**0.25)*WMS   ! m/yr
     KRATIO = KLCFF/KGCFF
     VOLVEL = KLCFF*(HNRCFF/(HNRCFF + Rg*T_abs*KRATIO))   ! m/yr
     DC_VOL_W = VOLVEL*CONPCB_W(I,1,j) / (D(I)*DZ2D(I,1)) / (86400.0*365.0)
     IF(DC_VOL_W < 0.0) THEN
       DC_VOL_W = 0.0          ! Volatilization model assumes negligible PCB concen in atmosphere
     END IF

     IF(WMS < 0.00001) DC_VOL_W = 0.0

     DC_SSI_W = (WSSHI (I,1)*CONPCB_W(I,1,j)*FP_SSI_W(I,1,j)) / (D(I)*DZ2D(I,1)) / 86400.0
     DC_POC_W = (WSL (I,1)*CONPCB_W(I,1,j)*FP_POC_W(I,1,j)) / (D(I)*DZ2D(I,1)) / 86400.0
     DC_ALG1_W = (WS1 (I,1)*CONPCB_W(I,1,j)*FP_BOC1_W(I,1,j)) / (D(I)*DZ2D(I,1)) / 86400.0
     DC_ALG2_W = (WS2 (I,1)*CONPCB_W(I,1,j)*FP_BOC2_W(I,1,j)) / (D(I)*DZ2D(I,1)) / 86400.0

     DMPCB_W(I,1,j) = -DC_SSI_W - DC_POC_W - DC_ALG1_W - DC_ALG2_W - DC_VOL_W

     IF(ISNAN(DMPCB_W(I,1,j))) THEN 
        WRITE(*,*)'Toxic Concen difference Values goes to NAN at top layer',DC_VOL_W,HNRCFF
        STOP
     END IF

  END DO
 END DO
!---------------------------------------------------------------------------------------

!---------------------- Toxic kinetics in middle layers of WC ----------------------------
 DO K=2,KBM1
   DO I=1,MLOC
     DO j=1,NTXVB
       !DELC_W3 = (Vvt*CONPCB_W(I)*FD_W(I)) / (D(I)*DZ2D(I,KBM1)) /86400.0
       DC_SSI_W = (WSSHI (I,K-1)*CONPCB_W(I,K-1,j)*FP_SSI_W(I,K-1,j) - WSSHI (I,K)*CONPCB_W(I,K,j)*FP_SSI_W(I,K,j)) &
                & / (D(I)*DZ2D(I,K)) /86400.0
       DC_POC_W = (WSL (I,K-1)*CONPCB_W(I,K-1,j)*FP_POC_W(I,K-1,j) - WSL (I,K)*CONPCB_W(I,K,j)*FP_POC_W(I,K,j)) &
                & / (D(I)*DZ2D(I,K)) /86400.0
       DC_ALG1_W = (WS1 (I,K-1)*CONPCB_W(I,K-1,j)*FP_BOC1_W(I,K-1,j) - WS1 (I,K)*CONPCB_W(I,K,j)*FP_BOC1_W(I,K,j)) &
                & / (D(I)*DZ2D(I,K)) /86400.0
       DC_ALG2_W = (WS2 (I,K-1)*CONPCB_W(I,K-1,j)*FP_BOC2_W(I,K-1,j) - WS2 (I,K)*CONPCB_W(I,K,j)*FP_BOC2_W(I,K,j)) &
                & / (D(I)*DZ2D(I,K)) /86400.0

       DMPCB_W(I,K,j) = DC_SSI_W + DC_POC_W + DC_ALG1_W + DC_ALG2_W 

      IF(ISNAN(DMPCB_W(I,K,j))) THEN 
        WRITE(*,*)'Toxic Concen difference Values goes to NAN at middle layers'
        STOP
      END IF

    END DO
   END DO
 END DO
 !-------------------------------------------------------------------------------------

!-------------------- Bottom layer contaminant kinetics ---------------------
! No Resuspension velocity has been used. Instead Net settling velocity has been used to
! comply with existing solid kinetics Model
!----------------------------------------------------------------------------------------
 DO I=1,MLOC
   DO j=1,NTXVB
      DFFVS12W(I) = (69.35*PSRT1(I)*(MolWghtCng(j)**(-2.0/3.0)))/365.0   ! Diffusive mixing velocity (Chapra, 1997) (m/d)
      DC_SSI_W = ((WSSHI (I,KBM1) - WSSNET(I))*CONPCB_W(I,KBM1,j)*FP_SSI_W(I,KBM1,j)) &
              & / (D(I)*DZ2D(I,KBM1)) / 86400.0   ! similar to resuspension
      DC_POC_W = ((WSL (I,KBM1) - WSLNET(I))*CONPCB_W(I,KBM1,j)*FP_POC_W(I,KBM1,j)) &
              & / (D(I)*DZ2D(I,KBM1)) / 86400.0
      DC_ALG1_W = ((WS1 (I,KBM1) - WS1NET(I))*CONPCB_W(I,KBM1,j)*FP_BOC1_W(I,KBM1,j)) &
              & / (D(I)*DZ2D(I,KBM1)) / 86400.0
      DC_ALG2_W = ((WS2 (I,KBM1) - WS2NET(I))*CONPCB_W(I,KBM1,j)*FP_BOC2_W(I,KBM1,j)) &
              & / (D(I)*DZ2D(I,KBM1)) / 86400.0
      DC_DOC_W = DFFVS12W(I)*(CONPCB_S1(I,j)*FP_DOC_S1(I,j) - CONPCB_W(I,KBM1,j)*FP_DOC_W(I,KBM1,j)) / &
                  & TXCHT1(I) / 86400.0      ! Diffusive mixing with sediment layer
      DC_DOC_W = 0.0

      DMPCB_W(I,KBM1,j) = DMPCB_W(I,KBM1,j) + DC_SSI_W + DC_POC_W + DC_ALG1_W + DC_ALG2_W &
                          & + DC_DOC_W 

      IF(ISNAN(DMPCB_W(I,KBM1,j))) THEN 
        WRITE(*,*)'Toxic Concen difference Values goes to NAN at the bottom layer'
        STOP
      END IF

   END DO
 END DO
!---------------------------------------------------------------------------------------
!-------------------- contaminant kinetics in sediment layer ---------------------------
!-------------------- sediment layer -1 ------------------------------------------------
DO I=1, MLOC
  DO j=1, NTXVB
    DCSSI_INS1 = (WSSNET(I)*CONPCB_W(I,KBM1,j)*FP_SSI_W(I,KBM1,j)) / TXCHT1(I) / 86400.0
    DCPOC_INS1 = (WSLNET(I)*CONPCB_W(I,KBM1,j)*FP_POC_W(I,KBM1,j)) / TXCHT1(I) / 86400.0
    DCAG1_INS1 = (WS1NET(I)*CONPCB_W(I,KBM1,j)*FP_BOC1_W(I,KBM1,j)) / TXCHT1(I) / 86400.0
    DCAG2_INS1 = (WS2NET(I)*CONPCB_W(I,KBM1,j)*FP_BOC2_W(I,KBM1,j)) / TXCHT1(I) / 86400.0
    TOTDC_INS1 = (DCSSI_INS1 + DCPOC_INS1 + DCAG1_INS1 + DCAG2_INS1)*(TXCHT2(I)/(TXCHT1(I)+TXCHT2(I)))

    !DCSSI_OUTS1 = WSSI12(I)*CONPCB_S1(I,j)*FP_SSI_S1(I,j) / TXCHT1(I) / 86400.0
    !DCPOC_OUTS1 = WPOC12(I)*CONPCB_S1(I,j)*FP_POC_S1(I,J) / TXCHT1(I) / 86400.0
    DCDOC_OUTS1 = DFFVS12W(I)*(CONPCB_S1(I,j)*FP_DOC_S1(I,j) - CONPCB_W(I,KBM1,j)*FP_DOC_W(I,KBM1,j)) / &
                              & TXCHT1(I) / 86400.0   ! Diffusive mixing with water column
    GRADC12 = (CONPCB_S1(I,j)*FP_DOC_S1(I,j) - CONPCB_S2(I,j)*FP_DOC_S2(I,j))/(0.5*(TXCHT1(I)+TXCHT2(I)))
    DCDOC_DFFS1 = DFFCOEFS12*GRADC12/TXCHT1(I)    ! Molecular diffusion into sdlyr 2

    !DCDOC_OUTS1 = 0.0
    !DCDOC_DFFS1 = 0.0

    DMPCB_S(I,j,1) = TOTDC_INS1 - DCDOC_OUTS1 - DCDOC_DFFS1


  END DO
END DO
!----------------------------------------------------------------------------------------
!----------------------------------sediment layer 2 -------------------------------------

DO I=1, MLOC
  DO j=1, NTXVB

    DCSSI_INS2 = (WSSNET(I)*CONPCB_W(I,KBM1,j)*FP_SSI_W(I,KBM1,j)) / TXCHT2(I) / 86400.0
    DCPOC_INS2 = (WSLNET(I)*CONPCB_W(I,KBM1,j)*FP_POC_W(I,KBM1,j)) / TXCHT2(I) / 86400.0
    DCAG1_INS2 = (WS1NET(I)*CONPCB_W(I,KBM1,j)*FP_BOC1_W(I,KBM1,j)) / TXCHT2(I) / 86400.0
    DCAG2_INS2 = (WS2NET(I)*CONPCB_W(I,KBM1,j)*FP_BOC2_W(I,KBM1,j)) / TXCHT2(I) / 86400.0
    TOTDC_INS2 = (DCSSI_INS2 + DCPOC_INS2 + DCAG2_INS1 + DCAG2_INS2)*(TXCHT1(I)/(TXCHT1(I)+TXCHT2(I)))

    !DCSSI_INS2 = WSSI12(I)*CONPCB_S2(I,j)*FP_SSI_S2(I,j) / TXCHT2(I) / 86400.0
    !DCPOC_INS2 = WPOC12(I)*CONPCB_S2(I,j)*FP_POC_S2(I,J) / TXCHT2(I) / 86400.0

    GRADC12 = (CONPCB_S1(I,j)*FP_DOC_S1(I,j) - CONPCB_S2(I,j)*FP_DOC_S2(I,j))/(0.5*(TXCHT1(I)+TXCHT2(I)))
    DCDOC_DFFS2 = DFFCOEFS12*GRADC12/TXCHT2(I)
    !DCDOC_DFFS2 = 0.0

    DMPCB_S(I,j,2) = TOTDC_INS2 + DCDOC_DFFS2

  END DO
END DO

 DSPCB = DMPCB_S
 DWPCB = DMPCB_W

END SUBROUTINE
!-------------------------------------------------------------------------------------


!------------------------------------------------------------------------------------- 
SUBROUTINE ExtractTxcData

 IMPLICIT NONE 

 PcbCon_W = FD_W * CONPCB_W
 PcbCon_ZplW = FP_ZOO_W * CONPCB_W 
 PcbCon_AlgW = FP_BOC1_W * CONPCB_W  +  FP_BOC2_W * CONPCB_W 
 PcbCon_SldW = FP_SSI_W * CONPCB_W 
 PcbCon_POCW = FP_POC_W * CONPCB_W
 PcbCon_DOCW = FP_DOC_W * CONPCB_W
 
 PcbConW_S1 = FD_WS1 * CONPCB_S1 
 PcbCon_POCS1 = FP_POC_S1 * CONPCB_S1 
 PcbCon_DOCS1 = FP_DOC_S1 * CONPCB_S1
 PcbConW_S2 = FD_WS2 * CONPCB_S2 
 PcbCon_POCS2 = FP_POC_S2 * CONPCB_S2 
 PcbCon_DOCS2 = FP_DOC_S2 * CONPCB_S2 
 
 DO I=1,NTXVB  
  DO J=1, KBM1
    PcbConTW(:,J) = PcbConTW(:,J) + PcbCon_W(:,J,I)
  END DO
    PcbConTS1(:) = PcbConTS1(:) + PcbConW_S1(:,I)
    PcbConTS2(:) = PcbConTS2(:) + PcbConW_S2(:,I)
 END DO

END SUBROUTINE
!--------------------------------------------------------------------------------------- 



!---------------------------------------------------------------------------------------
!SUBROUTINE BioAccum
!USE MOD_ZOOP, ONLY: RSZ, RLZ

!  IMPLICIT NONE 

  !----------------- 
!  DO K=1,KBM1
!    DO I=1,MLOC 
!      DO J=1,NTXVB
!        AlgWtWg(I,K,J) = (FP_BOC1_W(I,K,J) * CONPCB_W(I,K,J)  +  FP_BOC2_W(I,K,J) * CONPCB_W(I,K,J)) / &
!                            & (C2(I,K,4) + C2(I,K,5))  ! Need to be factored for wetbiomass weight and lipid fraction 
!        DTxSZDT(I,K,J) = Kuptk*FD_W(I,K,J)*CONPCB_W(I,J,K) + AlphAlgToSZ*
        
!      END DO
!    END DO
!  END DO




!END SUBROUTINE
!---------------------------------------------------------------------------------------

END MODULE
