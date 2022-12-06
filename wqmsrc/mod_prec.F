!===============================================================================!
! DEFINE FLOATING POINT PRECISION USING KIND                                    !
!===============================================================================!
Module MOD_PREC
      Implicit None
#  if defined (MULTIPROCESSOR)
      Include "mpif.h"
#  endif
  !
  !--Single Precision Coding------------------------------------------------------!
#  if !defined (DOUBLE_PRECISION)
      Integer, Parameter :: SP = Selected_Real_Kind (6, 30)
#  if defined (MULTIPROCESSOR)
      Integer, Parameter :: MPI_F = MPI_REAL
#  endif
  !
#  else
  !--Double Precision Coding------------------------------------------------------!
      Integer, Parameter :: SP = Selected_Real_Kind (12, 300)
  !   INTEGER, PARAMETER :: SP = KIND(1.d0)   !LB: guarantee precision up to that of the machine-compiler-specific double precision 
!
#  if defined (MULTIPROCESSOR)
      Integer, Parameter :: MPI_F = MPI_DOUBLE_PRECISION
#  endif
#  endif
  !
  !
  !
      Integer, Parameter :: DP = Selected_Real_Kind (12, 300)
  !   INTEGER, PARAMETER :: DP     = KIND(1.d0)   !LB: guarantee precision up to that of the machine-compiler-specific double precis
!
#  if defined (MULTIPROCESSOR)
      Integer, Parameter :: MPI_DP = MPI_DOUBLE_PRECISION
#  endif
  !
  ! KURT GLAESEMANN SEPT 22 2009 - USE CORRECT PRECISION WHEN READ NETCDF
      Integer, Parameter :: CDF_PREC = 4
  !
  ! KURT GLAESEMANN 14 April 2015 - USE CORRECT PRECISION WHEN USING MPI ON CDF DATA
#  if defined (MULTIPROCESSOR)
      Integer, Parameter :: MPI_CDF = MPI_REAL
#  endif
  !
End Module MOD_PREC
!
