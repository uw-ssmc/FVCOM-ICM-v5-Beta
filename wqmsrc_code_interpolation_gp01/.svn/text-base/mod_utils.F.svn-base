Module MOD_UTILS

	USE MOD_FILEINFO, ONLY: IREPORT
	
Contains
  !subroutines:
  !subroutine PERROR()
  !subroutine	GET_TIMESAMP()
  !subroutine	FOPEN()
  !subroutine	CRAY_SYSTEM_CALL()

      Subroutine PERROR (IOUT, ER1, ER2, ER3, ER4)
    !
         Implicit None
         Integer :: IOUT
         Character (Len=*) :: ER1
         Character (Len=*), Optional :: ER2
         Character (Len=*), Optional :: ER3
         Character (Len=*), Optional :: ER4
    !
         Write (IOUT,*) '==================ERROR=======================&
        &==========='
         Write (IOUT,*) ER1
         If (PRESENT(ER2)) WRITE (IOUT,*) ER2
         If (PRESENT(ER3)) WRITE (IOUT,*) ER3
         If (PRESENT(ER4)) WRITE (IOUT,*) ER4
         Write (IOUT,*) '==============================================&
        &==========='
         Stop
         Return
    !
      End Subroutine PERROR
    !
      Subroutine GET_TIMESTAMP (TS)
         Character (Len=*) TS
         Character (Len=8) D
         Character (Len=10) T
    !
         Call DATE_AND_TIME (DATE=D, TIME=T)
         TS = D (7:8) // '/' // D (5:6) // '/' // D (1:4) // '   ' // T &
        & (1:2) // ':' // T (3:4)
      End Subroutine GET_TIMESTAMP
  
      Subroutine FOPEN (IUNIT, INSTR, IOPT)
         Implicit None
         Integer, Intent (In) :: IUNIT
         Character (Len=*) :: INSTR
         Character (Len=3), Intent (In) :: IOPT
         Character (Len=11) :: FORMSTR
         Character (Len=7) :: STATSTR
         Logical CHECK, FEXIST
	!
         If (IOPT(1:1) == "c") Then
            STATSTR = "old"
            CHECK = .True.
         Else If (IOPT(1:1) == "o") Then
            STATSTR = "unknown"
            CHECK = .False.
         Else
            Call PERROR (IREPORT, "FIRST LETTER IN FOPEN OPTION STRING MUST B&
           &E 'c' OR 'o'")
         End If
         If (IOPT(2:2) == "f") Then
            FORMSTR = "formatted"
         Else If (IOPT(2:2) == "u") Then
            FORMSTR = "unformatted"
         Else
            Call PERROR (IREPORT, "ERROR PROCESSING FOPEN ON FILE", INSTR, "2&
           &ND LETTER IN FOPEN OPTION STRING MUST BE 'f' OR 'u'")
         End If
    !
         If (CHECK) Then
            Inquire (File=INSTR, Exist=FEXIST)
            If ( .Not. FEXIST) Call PERROR (IREPORT, "FILE "//INSTR//" NOT FO&
           &UND")
         End If
    !
         Open (IUNIT, File=INSTR, Status=TRIM(STATSTR), &
        & Form=TRIM(FORMSTR))
    !
         If (IOPT(3:3) == "r") Rewind (IUNIT)
    !
      End Subroutine FOPEN
    !
      Subroutine CRAY_SYSTEM_CALL (INSTRING)
    !
         Character (Len=*) :: INSTRING
         Integer :: ISTAT
    !
         ISTAT = 0
# if defined (CRAY)
         ISTAT = ISHELL (TRIM(INSTRING))
# endif
         If (ISTAT /= 0) Then
            Write (*,*) 'UNABLE TO COMPLETE COMMAND: ' // TRIM &
           & (INSTRING)
            Stop
         End If
    !
      End Subroutine CRAY_SYSTEM_CALL
  !
End Module MOD_UTILS
!
