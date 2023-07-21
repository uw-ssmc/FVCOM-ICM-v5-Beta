!subroutine DOMDEC()
!
!==============================================================================|
!  DECOMPOSE THE DOMAIN BY ELEMENTS USING METIS GRAPH PARTITIONING TOOL        |
!    RETURNS[EL_PID(1:NGL)]                                                    |
!    EL_PID(I) = Processor ID Of Owner of Element I                            |
!==============================================================================|
!
Subroutine DOMDEC (NGL, NPROCS, EL_PID, MSR)

  !
      Use MOD_HYDROVARS, Only: NVG
  !
  !==============================================================================|
      Implicit None
      Include "mpif.h"
      Integer, Intent (In) :: NGL, NPROCS
      Integer, Intent (Out) :: EL_PID (NGL)
      Logical, Intent (In) :: MSR
      Integer, Allocatable :: NVT (:)
      Integer :: I, IERR, ii !NTEMP,!LBcleanup
  !==============================================================================|
  !
  !
  !----------------READ IN NODE LIST FROM ***_grd.dat FILE-----------------------!
  !
  !
      If (MSR) Then
         Allocate (NVT(3*NGL))
     !
     ! KURT GLAESEMANN - fix NVT creation
     !  II = 0
     !  DO I=1,NGL
     !    II = II+1
     !    NVT(II)   = NVG(I,1)    !TEMP(I,1)
     !    NVT(II+1) = NVG(I,1)    !TEMP(I,3)
     !    NVT(II+2) = NVG(I,1)    !TEMP(I,2)
     !  ENDDO
         ii = - 2
         Do I = 1, NGL
            ii = ii + 3
            NVT (ii) = NVG (I, 1)!TEMP(I,1)
            NVT (ii+1) = NVG (I, 2)!TEMP(I,3)
            NVT (ii+2) = NVG (I, 3)!TEMP(I,2)
         End Do
     !
     !
     !-------------DECOMPOSE ELEMENTS USING METIS GRAPH PARTITIONING ---------------!
     !
         Call PARTITION (NPROCS, NGL, maxval(NVT), loc(NVT), &
        & loc(EL_PID))
     !
         EL_PID = EL_PID + 1
         Deallocate (NVT)
     !
      End If
  !
      Call MPI_BCAST (EL_PID, NGL, MPI_INTEGER, 0, MPI_COMM_WORLD, &
     & IERR)
  !
  !

End Subroutine DOMDEC
!==============================================================================|
