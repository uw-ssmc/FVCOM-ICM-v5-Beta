!
!Subroutines :
!	Subroutine PSTOP
!
!
!==============================================================================|
!     UTILITIES FILE                                                           |
!      PSTOP:   HALTS PROGRAM CORRECTLY                                        |
!      WRITE_BANNER: WRITE FVCOM BANNER TO OUTPUT                              |
!==============================================================================|
!
!
!==============================================================================|
!Subroutine PSTOP
!  !==============================================================================|
!#if defined (1)
!  !
!      Include "mpif.h"
!      Integer IERR
!  !  CALL MPI_ABORT(MPI_COMM_WORLD,IERR)
!      Call MPI_FINALIZE (IERR)
!#endif
!  !
!      Stop
!End Subroutine PSTOP

 SUBROUTINE PSTOP
!==============================================================================|

  Include "mpif.h"
 !USE MPI
  INTEGER IERR, ecode

  ecode = -1
  CALL MPI_ABORT(MPI_COMM_WORLD,ecode,IERR)

  CALL MPI_FINALIZE(IERR)


  STOP
  END SUBROUTINE PSTOP

SUBROUTINE WRITE_BANNER(IUNIT)

  INTEGER, INTENT(IN) :: IUNIT

   WRITE(IUNIT,*)'!==============================================================================!'
   WRITE(IUNIT,*)'!######  #       #    ####   ####    #       #       #####  #### #      #      !'
   WRITE(IUNIT,*)'!#        #     #    #      #    #   # #   # #         #   #     # #  # #      !'
   WRITE(IUNIT,*)'!####      #   #     #      #    #   #   #   #  #####  #   #     #   #  #      !'
   WRITE(IUNIT,*)'!#          # #      #      #    #   #       #         #   #     #      #      !'
   WRITE(IUNIT,*)'!#           #        ####   ####    #       #       ###### #### #      #      !'
   WRITE(IUNIT,*)'!==============================================================================!'
   WRITE(IUNIT,*)'!                                                                              !'
   WRITE(IUNIT,*)'!=================DOMAIN DECOMPOSITION USING: METIS 4.0.1 =====================!'
   WRITE(IUNIT,*)'!============Copyright 1998, Regents of University of Minnesota================!'
   WRITE(IUNIT,*)'!                                                                              !'

   RETURN
END SUBROUTINE WRITE_BANNER
!==============================================================================|
!
