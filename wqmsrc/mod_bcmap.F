Module MOD_BCMAP
  !
      Use MOD_PREC, Only: SP
      Use MOD_TYPES, Only: BC
      USE MOD_WQM, ONLY: TcrKntOn
  !
  !WLong moved the following here from mod_obcs.F
      Integer :: IOBCN_GL !!GLOBAL NUMBER OF OPEN BOUNDARY NODES
      Integer :: IOBCN !!LOCAL NUMBER OF OPEN BOUNDARY NODES
      Integer, Allocatable :: I_OBC_GL (:)!!GLOBAL ID OF OPEN BOUNDARY NODES
      Integer, Allocatable :: I_OBC_N (:)!!OPEN BOUNDARY NODE LIST
  !
  !WLong moved here from mod_bcs.F
      Real (SP), Allocatable :: WQOBC (:, :, :, :)!!WATER QUALITY DATA AT BOUNDARY
      Type (BC) :: NUT_TM !!TIME MAPPING FOR NUTRIENT OBC  !should be
  !
Contains
  !
  !Subroutine BCMAP()
  !Subroutine BCMAP_DEALLOC
  !
  !--------------------------------------------
      Subroutine BCMAP
    !--------------------------------------------
         Use MOD_SIZES, Only: NCP, MGL, NTXVB, NBTCR
    !
         Use MOD_LIMS, Only: KBM1, NPROCS
!
         Use MOD_CONTROL, Only: SERIAL, MSR, PAR, CASENAME
    !
         Use MOD_PREC, Only: SP
    	 USE MOD_FILEINFO, ONLY: INOBC
         Use MOD_UTILS, Only: FOPEN
# if defined (MULTIPROCESSOR)
         Use MOD_PAR, Only: NLID
# endif
         Implicit None
    !
    !
#if defined (MULTIPROCESSOR)
         Include "mpif.h"
#endif
         Integer :: I, I1, NCNT, IERR, J, JJ, JT, K, TXCSRT !, I2, JCON  !LBcleanup
         Integer, Allocatable :: TEMP1 (:)!, TEMP2 (:), TEMP3 (:), TEMP4 & !!LBcleanup
    !& (:), TEMP5 (:), TEMP6 (:), TEMP7 (:), ITEMP (:) !!LBcleanup
         Real (SP), Allocatable :: TEMP9 (:, :, :, :)! KURT GLAESEMANN 3-MAR-2010
         Real (SP) :: FTEMP1
         Real (SP), Allocatable :: WQOBC_GL (:, :, :, :)!!NUT Series at OB sigma layer
    !
         Integer ::  NTMP, numTcrtVrb
         Character (Len=100) :: ISTR
    !

    !------------------------------------------------------------------------------!
    !
    !==============================================================================|
    !   OPEN BOUNDARY CONDITION NODES                                              |
    !==============================================================================|
    !
    !----------------------------REPORT--------------------------------------------!
    !
         If (MSR) WRITE (*,*) '!'
         If (MSR) WRITE (*,*) '!           SETTING UP OPEN BOUNDARY NOD&
        &ES  '
         If (MSR) WRITE (*,*) '!'
    !
         IOBCN = 0
    !   IBCN  = 0
         TXCSRT = NCP - NTXVB - NBTCR
    !
         ISTR = "./inputs/" // trim (CASENAME)
         Call FOPEN (INOBC, trim(ISTR)//'_obc_wq.dat', "cfr")
         Rewind (INOBC)
    !
         Read (INOBC,*) IOBCN_GL
    !
         If (IOBCN_GL > 0) Then
       !
       !------------Read in Open Boundary Nodes and Temperature/Salinity Conditions---!
       !
       !
       !----Input Number of Data Times for T/S at Open Boundary Every Sigma Layers---!
       !
            Allocate (I_OBC_GL(IOBCN_GL))
       !
            Do I = 1, IOBCN_GL
               Read (INOBC,*) I1, I_OBC_GL (I)
            End Do
            NCNT = 0
            Do WHILE (.True.)
               Read (INOBC,*, END=10) FTEMP1
               Do JJ = 1, NCP - NTXVB - NBTCR
                  Do J = 1, IOBCN_GL
                     Read (INOBC,*)
                  End Do
               End Do
               NCNT = NCNT + 1
            End Do
10          Continue
            Rewind (INOBC)
       !
            If (NCNT == 0) WRITE (*,*) 'NO WQ DATA at BND'
       !
            NUT_TM%NTIMES = NCNT
       !
       !    READ nutrient open boundary values
       !
            Allocate (NUT_TM%TIMES(NUT_TM%NTIMES))
            NUT_TM%TIMES = 0.
            Allocate (WQOBC_GL(IOBCN_GL, KBM1, NUT_TM%NTIMES, NCP))
            WQOBC_GL = 0.
       !
            Read (INOBC,*)
            Do I = 1, IOBCN_GL
               Read (INOBC,*)
            End Do
       !
            Do JT = 1, NUT_TM%NTIMES
               Read (INOBC,*) NUT_TM%TIMES(JT)!time(hours)
               Do JJ = 1, NCP - NTXVB - NBTCR
                  Do I = 1, IOBCN_GL
                     Read (INOBC,*) NTMP, (WQOBC_GL(I, K, JT, JJ), K=1, &
                    & KBM1)!temp in OB sigma layer
                  End Do
               End Do
          !          NUT_TM%TIMES = 24.0*(NUT_TM%TIMES) !shift to model hours
            End Do
            Close (INOBC)

          IF(TcrKntOn == .TRUE.) THEN 
               numTcrtVrb = NCP - NBTCR + 1
               OPEN(UNIT = 212, FILE = 'inputs/tepco_tcr_obc.dat', ACTION = 'READ')
               READ(212,*)
               DO I = 1, IOBCN_GL
                    READ (212,*)
               END DO
               DO JT = 1, NUT_TM%NTIMES
               READ (212,*)
                    DO JJ = numTcrtVrb, NCP
                         DO I = 1, IOBCN_GL
                              READ (212,*) (WQOBC_GL(I, K, JT, JJ), K=1, &
                              & KBM1)!temp in OB sigma layer
                         END DO
                    END DO
          !          NUT_TM%TIMES = 24.0*(NUT_TM%TIMES) !shift to model hours
               END DO
               CLOSE(212)

          END IF
       !
       !---------Note in log file if TALK is a function of Salinity at open boundary---------!
       !            (done in bcond_wqm.F, not here) - LB Jan 7 2016                          !
#ifdef TA_OBC_INI_FROM_S
            Write (*,*) '!  TALK at OBC and INI      :    function of s&
           &alinity'
#endif
#ifdef TA_INI_FROM_S
            Write (*,*) '!  TALK at INI      :    function of salinity'
#endif
       !---------LB Jan 7 2016---------------------------------------------------------------!
       !
       !----------------------Make Sure It Is In Global Domain------------------------!
       !
            Do I = 1, IOBCN_GL
               If ((I_OBC_GL(I) > MGL)) Then
                  Write (*,*) '==================ERROR=================&
                 &================='
                  Write (*,*) 'OPEN BOUNDARY NODE NUMBER', I, 'IS NOT I&
                 &N THE'
                  Write (*,*) 'GLOBAL DOMAIN'
                  Write (*,*) 'CHECK INPUT FILE AND ENSURE OPEN BOUNDAR&
                 &Y NODES <= ', MGL
                  Write (*,*) '========================================&
                 &================='
                  Call PSTOP
               End If
            End Do
       !
       !----------Shift Open Boundary Node List,Type,Salt,and Temp to Local-----------!
       !
            If (SERIAL) Then
          !
               IOBCN = IOBCN_GL
               Allocate (I_OBC_N(IOBCN))
               I_OBC_N = I_OBC_GL
          !
               Allocate (WQOBC(IOBCN, KBM1, NUT_TM%NTIMES, NCP))
               WQOBC = 0.0
               WQOBC (:, :, :, :) = WQOBC_GL (:, :, :, :)! KURT GLAESEMANN 19-MAR-2010
          !
            End If
       !
#    if defined (MULTIPROCESSOR)
            If (PAR) Then
               Allocate (TEMP1(IOBCN_GL))
               Allocate (TEMP9(IOBCN_GL, KBM1, NUT_TM%NTIMES, NCP))
               TEMP9 = 0 ! KURT GLAESEMANN 3-MAR-2010
               NCNT = 0
          !!SET UP LOCAL OPEN BOUNDARY NODES
               Do I = 1, IOBCN_GL
                  I1 = NLID (I_OBC_GL(I))
                  If (I1 /= 0) Then
                     NCNT = NCNT + 1
                     TEMP1 (NCNT) = I1
                     TEMP9 (NCNT, :, :, :) = WQOBC_GL (I, :, :, :)! KURT GLAESEMANN 3-MAR-2010
                  End If
               End Do
               IOBCN = NCNT
          !
               If (NCNT > 0) Then
                  Allocate (I_OBC_N(NCNT))
                  I_OBC_N = TEMP1 (1:NCNT)
                  Allocate (WQOBC(NCNT, KBM1, NUT_TM%NTIMES, NCP))
                  WQOBC = 0.0 ! KURT GLAESEMANN 3-MAR-2010
                  WQOBC (1:NCNT, :, :, :) = TEMP9 (1:NCNT, :, :, :)! KURT GLAESEMANN 3-MAR-2010
               End If
          !
               Deallocate (TEMP9)! KURT GLAESEMANN 3-MAR-2010
               Deallocate (TEMP1)
            End If
#    endif
       !
            Deallocate (WQOBC_GL)
            Write (*,*) '!  TEMP/SALT OBC READ      :    COMPLETE'
         End If !!IOBCN_GL > 0
    !
    !==============================================================================|
    !   REPORT AND CHECK RESULTS                                                   |
    !==============================================================================|
         Allocate (TEMP1(NPROCS))
         TEMP1 (1) = IOBCN
    !
# if defined (MULTIPROCESSOR)
         Call MPI_GATHER (IOBCN, 1, MPI_INTEGER, TEMP1, 1, MPI_INTEGER, &
        & 0, MPI_COMM_WORLD, IERR)
# endif
    !
    !
         If (MSR) WRITE (*, 100) '!  IOBCN                 :', &
        & IOBCN_GL, (TEMP1(I), I=1, NPROCS)
         Deallocate (TEMP1)
    !
         Return
100      Format (1 X, A26, I6, " =>", 2 X, 4(I5, 1 H,))
      End Subroutine BCMAP
  !==============================================================================|
  !
      Subroutine BCMAP_DEALLOC
    !
    !WLong moved here from wqm_main.F
         If (ALLOCATED(I_OBC_N)) DEALLOCATE (I_OBC_N)
         If (ALLOCATED(I_OBC_GL)) DEALLOCATE (I_OBC_GL)
    !
    !WLong added deallocation WQOBC
    !
         If (ALLOCATED(WQOBC)) DEALLOCATE (WQOBC)
    !
    !LB: these deallocations were missing
         If (ALLOCATED(NUT_TM%TIMES)) DEALLOCATE (NUT_TM%TIMES)
    !
    !
    !
      End Subroutine BCMAP_DEALLOC
  !
End Module MOD_BCMAP
