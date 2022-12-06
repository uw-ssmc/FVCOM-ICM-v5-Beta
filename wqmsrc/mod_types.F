!  KURT GLAESEMANN 22 SEPT 2009- Converted all POINTER to ALLOCATABLE, since its faster
Module MOD_TYPES
      Use MOD_PREC, Only: SP
      Implicit None
  !
      Type GMAP
         Integer NSIZE
         Integer, Allocatable, Dimension (:) :: LOC_2_GL
      End Type GMAP
  !
      Type COMM
     !----------------------------------------------------------
     ! SND: TRUE IF YOU ARE TO SEND TO PROCESSOR               |
     ! RCV: TRUE IF YOU ARE TO RECEIVE FROM PROCESSOR          |
     ! NSND: NUMBER OF DATA TO SEND TO PROCESSOR               |
     ! NRCV: NUMBER OF DATA TO RECEIVE FROM PROCESSOR          |
     ! SNDP: ARRAY POINTING TO LOCATIONS TO SEND TO PROCESSOR  |
     ! RCVP: ARRAY POINTING TO LOCATIONS RECEIVED FROM PROCESS |
     ! RCPT: PONTER TO LOCATION IN RECEIVE BUFFER             |
     !----------------------------------------------------------
     !
     !  LOGICAL :: SND,RCV
         Integer NSND, NRCV, RCPT
         Integer, Allocatable, Dimension (:) :: SNDP, RCVP
         Real (SP), Allocatable, Dimension (:) :: MLTP
      End Type COMM
  !
      Type BC
         Integer NTIMES
         Real (SP), Allocatable, Dimension (:) :: TIMES
         Character (Len=80) :: LABEL
      End Type BC
  !
End Module MOD_TYPES
!
