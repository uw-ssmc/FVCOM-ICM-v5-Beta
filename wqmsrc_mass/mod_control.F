!
!==============================================================================|
!   CONTROL VARIABLES                                                          |
!==============================================================================|
!
Module MOD_CONTROL
  !
      Use MOD_PREC, Only: SP !
      Implicit None
      Save
  !
      Logical SERIAL !!TRUE IF SINGLE PROCESSOR
      Logical MSR !!TRUE IF MASTER PROCESSOR (MYID==1)
      Logical PAR !!TRUE IF MULTIPROCESSOR RUN
  !
      Character (Len=80) CASENAME !!LETTER ACRONYM SPECIFYING CASE IDENTITY (MAX 80 CHARS)
      Character (Len=120) CASETITLE !!CASE TITLE
  !
      Real (SP) :: HMAX !!GLOBAL MAXIMUM DEPTH
      Real (SP) :: HMIN !!GLOBAL MINIMUM DEPTH
      Real (SP) :: UMOL !!VERTICAL DIFFUSION COEFFICIENT
      Real (SP) :: HORCON !!HORIZONTAL DIFFUSION COEFFICIENT
      Real (SP) :: DTI !!internal time step
  !
      Character (Len=80) HORZMIX !!CONTROLS HORIZONTAL DIFFUSION COEF CALC (constant/closure)
      Character (Len=100) :: FILENUMBER, PREFF, INPDIR, GEOAREA, &
     & RIV_FILENUMBER
      Character (Len=80) INFLOW_TYPE !!SPECIFIED RIVER INFLOW TYPE (edge/node)
      Character (Len=80) POINT_ST_TYPE !!(calculated/specified)
      Character (Len=80) PNT_SOURCE !!point_source
      Integer :: DAY, in_jday
  !
End Module MOD_CONTROL
