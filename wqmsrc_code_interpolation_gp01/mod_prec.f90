!===============================================================================!
! DEFINE FLOATING POINT PRECISION USING KIND                                    !
!===============================================================================!
Module MOD_PREC
      Implicit None
      Include "mpif.h"
  !
  !--Single Precision Coding------------------------------------------------------!
  !--Double Precision Coding------------------------------------------------------!
      Integer, Parameter :: SP = Selected_Real_Kind (12, 300)
  !   INTEGER, PARAMETER :: SP = KIND(1.d0)   !LB: guarantee precision up to that of the machine-compiler-specific double precision 
!
      Integer, Parameter :: MPI_F = MPI_DOUBLE_PRECISION
  !
  !
  !
      Integer, Parameter :: DP = Selected_Real_Kind (12, 300)
  !   INTEGER, PARAMETER :: DP     = KIND(1.d0)   !LB: guarantee precision up to that of the machine-compiler-specific double precis
!
      Integer, Parameter :: MPI_DP = MPI_DOUBLE_PRECISION
  !
  ! KURT GLAESEMANN SEPT 22 2009 - USE CORRECT PRECISION WHEN READ NETCDF
      Integer, Parameter :: CDF_PREC = 4
  !
  ! KURT GLAESEMANN 14 April 2015 - USE CORRECT PRECISION WHEN USING MPI ON CDF DATA
      Integer, Parameter :: MPI_CDF = MPI_REAL
  !
End Module MOD_PREC
!
