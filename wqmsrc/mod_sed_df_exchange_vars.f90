!
Module MOD_SED_DF_EXCHANGE_VARS
  !
      Use MOD_PREC, Only: SP
      Use MOD_LIMS, Only: MTLOC
      Implicit None
      Save
  !
      Real (SP) :: M1_SED_DF
      Real (SP) :: M2_SED_DF
      Real (SP), Allocatable :: POC1TM1S_SED_DF (:), POC2TM1S_SED_DF &
     & (:)!POC1TM1S and POC2TM1S shared between DF and sediment module
  !
  !
  !
Contains
  !
      Subroutine SED_DF_EXCHANGE_ALLOC
    !
         Allocate (POC1TM1S_SED_DF(MTLOC))
         POC1TM1S_SED_DF = 0.0
         Allocate (POC2TM1S_SED_DF(MTLOC))
         POC2TM1S_SED_DF = 0.0
    !
      End Subroutine SED_DF_EXCHANGE_ALLOC
  !
      Subroutine SED_DF_EXCHANGE_DEALLOC
    !
         If (ALLOCATED(POC1TM1S_SED_DF)) DEALLOCATE (POC1TM1S_SED_DF)
         If (ALLOCATED(POC2TM1S_SED_DF)) DEALLOCATE (POC2TM1S_SED_DF)
    !
      End Subroutine SED_DF_EXCHANGE_DEALLOC
End Module MOD_SED_DF_EXCHANGE_VARS
!
