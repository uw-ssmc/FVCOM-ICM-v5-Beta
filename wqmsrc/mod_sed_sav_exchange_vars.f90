!
Module MOD_SED_SAV_EXCHANGE_VARS
  !
      Use MOD_PREC, Only: SP
      Use MOD_LIMS, Only: MTLOC
      Implicit None
      Save
  !
      Real (SP), Allocatable :: NH4T2TM1S_SHARE (:)!NH4T2TM1S to be shared between SAV and sediment module
      Real (SP), Allocatable :: PO4T2TM1S_SHARE (:)!PO4T2TM1S to be shared between SAV and sediment mnodule
      Real (SP), Allocatable :: HST2TM1S_SHARE (:)!
      Real (SP) :: M2_SHARE !kg/L
      Real (SP) :: PIE2HS_SHARE !partitioning coef. of H2S in sediment layer 2 (L/kg)
  !
Contains
  !
      Subroutine SED_SAV_EXCHANGE_ALLOC
    !
         Allocate (NH4T2TM1S_SHARE(MTLOC))
         NH4T2TM1S_SHARE = 0.0
         Allocate (PO4T2TM1S_SHARE(MTLOC))
         PO4T2TM1S_SHARE = 0.0
         Allocate (HST2TM1S_SHARE(MTLOC))
         HST2TM1S_SHARE = 0.0
    !
      End Subroutine SED_SAV_EXCHANGE_ALLOC
  !
      Subroutine SED_SAV_EXCHANGE_DEALLOC
    !
         If (ALLOCATED(NH4T2TM1S_SHARE)) DEALLOCATE (NH4T2TM1S_SHARE)
         If (ALLOCATED(PO4T2TM1S_SHARE)) DEALLOCATE (PO4T2TM1S_SHARE)
         If (ALLOCATED(HST2TM1S_SHARE)) DEALLOCATE (HST2TM1S_SHARE)
    !
      End Subroutine SED_SAV_EXCHANGE_DEALLOC
End Module MOD_SED_SAV_EXCHANGE_VARS
!
