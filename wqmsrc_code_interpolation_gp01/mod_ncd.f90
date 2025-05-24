!==============================================================================|
!   NCD UTILITIES                                                              !
!==============================================================================|
!
Module MOD_NCD
  !
      Use NETCDF
      Use MOD_PREC, Only: SP, CDF_PREC !
      Implicit None
      Save
  !
      Integer :: NC_FID
  !
  !
Contains
  !
  !subroutines:
  !
  ! subroutine	GETSVAR()
  ! subroutine	GETDVAR()
  ! subroutine	PUTDVAR()
  ! subroutine	PUTSVAR()
  !
  !functions:
  ! function 	GETDIM()
  !
  !
  !==============================================================================!
      Integer Function GETDIM (FID, SSIZE, DIMNAME)
    !==============================================================================!
    !  Read dimensions
    !==============================================================================!
    !
         Implicit None
         Integer, Intent (In) :: FID
         Integer, Intent (In) :: SSIZE
         Character (Len=SSIZE), Intent (In) :: DIMNAME
         Integer :: LENGTH
         Integer :: IERR
         Integer :: DIMID
         Character (Len=NF90_MAX_NAME) :: TEMPNAME
    !
         IERR = NF90_INQ_DIMID (FID, TRIM(DIMNAME), DIMID)
         If (IERR /= NF90_NOERR) Then
            Write (*,*) 'Error getting dimension id: ', TRIM (DIMNAME)
            Write (*,*) TRIM (NF90_STRERROR(IERR))
            Stop
         End If
    !
         IERR = NF90_INQUIRE_DIMENSION (FID, DIMID, TEMPNAME, LENGTH)
         If (IERR /= NF90_NOERR) Then
            Write (*,*) 'Error getting dimension: ', TRIM (DIMNAME)
            Write (*,*) TRIM (NF90_STRERROR(IERR))
            Stop
         End If
    !
         GETDIM = LENGTH
    !
      End Function GETDIM
  !==============================================================================!
  !
  !==============================================================================!
  !
      Subroutine GETSVAR (FID, NLEN, VARNAME, I1, I2, TEMP)
    !============================================================================!
    !  Read Static variables
    !==============================================================================!
    !
         Implicit None
         Integer, Intent (In) :: FID
         Integer, Intent (In) :: NLEN
         Character (Len=NLEN), Intent (In) :: VARNAME
         Integer, Intent (In) :: I1, I2
         Real (SP), Intent (Out) :: TEMP (I1, I2)
         Integer :: IERR
         Integer :: VARID
         Integer, Allocatable :: DIMS (:)
    !
         If (I2 == 1) Then
            Allocate (DIMS(1))
            DIMS (1) = 1
         Else
            Allocate (DIMS(2))
            DIMS (1) = 1
            DIMS (2) = 1
         End If
    !
         IERR = NF90_INQ_VARID (FID, TRIM(VARNAME), VARID)
         If (IERR /= NF90_NOERR) Then
            Write (*,*) 'error getting variable id: ', TRIM (VARNAME)
            Write (*,*) TRIM (NF90_STRERROR(IERR))
            Stop
         End If
    !
         IERR = NF90_GET_VAR (FID, VARID, TEMP, DIMS)
         If (IERR /= NF90_NOERR) Then
            Write (*,*) 'error getting variable: ', TRIM (VARNAME)
            Write (*,*) TRIM (NF90_STRERROR(IERR))
            Stop
         End If
    ! KURT GLAESEMANN add explicit deallocate
         Deallocate (DIMS)
    !
         Return
      End Subroutine GETSVAR
  !==============================================================================!
  !
  !==============================================================================!
      Subroutine GETDVAR (FID, NLEN, VARNAME, I1, I2, TEMP, NT)
    !============================================================================!
    !  Read time dynamic variables
    !==============================================================================!
    !
         Implicit None
         Integer, Intent (In) :: FID
         Integer, Intent (In) :: NLEN
         Character (Len=NLEN), Intent (In) :: VARNAME
         Integer, Intent (In) :: I1, I2
         Real (CDF_PREC), Intent (Out) :: TEMP (I1, I2)
         Integer :: IERR
         Integer :: VARID
         Integer :: NT
         Integer, Allocatable :: DIMS (:)
    !
         If (I2 == 1) Then
            Allocate (DIMS(2))
            DIMS (1) = 1
            DIMS (2) = NT
         Else
            Allocate (DIMS(3))
            DIMS (1) = 1
            DIMS (2) = 1
            DIMS (3) = NT
         End If
    !
         IERR = NF90_INQ_VARID (FID, TRIM(VARNAME), VARID)
         If (IERR /= NF90_NOERR) Then
            Write (*,*) 'error getting variable id: ', TRIM (VARNAME)
            Write (*,*) TRIM (NF90_STRERROR(IERR))
            Stop
         End If
    !
         IERR = NF90_GET_VAR (FID, VARID, TEMP, DIMS)
         If (IERR /= NF90_NOERR) Then
            Write (*,*) 'error getting variable: ', TRIM (VARNAME)
            Write (*,*) TRIM (NF90_STRERROR(IERR))
            Stop
         End If
    ! KURT GLAESEMANN add explicit deallocate
         Deallocate (DIMS)
    !
         Return
      End Subroutine GETDVAR
  !==============================================================================!
  !
  !==============================================================================!
      Subroutine PUTDVAR (FID, NLEN, VARNAME, I1, TEMP, NT)
    !============================================================================!
    !  Write dynamic time variables
    !==============================================================================!
    !
         Implicit None
         Integer, Intent (In) :: FID
         Integer, Intent (In) :: NLEN
         Character (Len=NLEN), Intent (In) :: VARNAME
         Integer, Intent (In) :: I1
         Real (SP), Intent (In) :: TEMP (I1)
         Integer :: IERR
         Integer :: VARID
         Integer, Allocatable :: DIMS (:)
         Integer :: NT
    !
         If (I1 == 1) Then
            Allocate (DIMS(1))
            DIMS (1) = NT
         Else
            Allocate (DIMS(2))
            DIMS (1) = 1
            DIMS (2) = NT
         End If
    !
         IERR = NF90_INQ_VARID (FID, TRIM(VARNAME), VARID)
         If (IERR /= NF90_NOERR) Then
            Write (*,*) 'error getting variable id: ', TRIM (VARNAME)
            Write (*,*) TRIM (NF90_STRERROR(IERR))
            Stop
         End If
    !
         IERR = NF90_PUT_VAR (FID, VARID, TEMP, DIMS)
         If (IERR /= NF90_NOERR) Then
            Write (*,*) 'error getting variable: ', TRIM (VARNAME)
            Write (*,*) TRIM (NF90_STRERROR(IERR))
            Stop
         End If
    ! KURT GLAESEMANN add explicit deallocate
         Deallocate (DIMS)
    !
         Return
      End Subroutine PUTDVAR
  !==============================================================================!
  !
  !==============================================================================!
      Subroutine PUTSVAR (FID, NLEN, VARNAME, I1, TEMP)
    !============================================================================!
    !  Write static variables
    !==============================================================================!
    !
         Implicit None
         Integer, Intent (In) :: FID
         Integer, Intent (In) :: NLEN
         Character (Len=NLEN), Intent (In) :: VARNAME
         Integer, Intent (In) :: I1
         Real (SP), Intent (In) :: TEMP (I1)
         Integer :: IERR
         Integer :: VARID
         Integer, Dimension (1) :: DIMS
    !
         DIMS (1) = 1
    !
         IERR = NF90_INQ_VARID (FID, TRIM(VARNAME), VARID)
         If (IERR /= NF90_NOERR) Then
            Write (*,*) 'error getting variable id: ', TRIM (VARNAME)
            Write (*,*) TRIM (NF90_STRERROR(IERR))
            Stop
         End If
    !
         IERR = NF90_PUT_VAR (FID, VARID, TEMP, DIMS)
         If (IERR /= NF90_NOERR) Then
            Write (*,*) 'error getting variable: ', TRIM (VARNAME)
            Write (*,*) TRIM (NF90_STRERROR(IERR))
            Stop
         End If
    !
         Return
      End Subroutine PUTSVAR
  !==============================================================================!
  !
End Module MOD_NCD
