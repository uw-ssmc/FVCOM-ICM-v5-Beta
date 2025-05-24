!
Module MOD_BCS
  !
      Use MOD_TYPES, Only: BC
      Use MOD_PREC, Only: SP
  !
      Use MOD_CONTROL, Only: PAR
  !
      Implicit None
      Save
  !----------------boundary conditions: fresh water discharge-------------------------!
      Integer, Allocatable :: INODEQ (:)!!LOCAL FRESH WATER INFLOW NODES
      Integer, Allocatable :: ICELLQ (:)!!LOCAL FRESH WATER INFLOW ELEMENTS
      Integer, Allocatable :: RIV_GL2LOC (:)!!GLOBAL TO LOCAL MAP OF FW NODES
      Integer, Allocatable :: INOPNT (:)!!LOCAL NON-POINT SOURCE NODES
  !
      Type (BC) :: QBC_TM !!TIME MAP FOR RIVER DISCHARGE DATA
      Type (BC) :: PNT_TM !!TIME MAP FOR NON POINT SOURCE DATA
  !
  !WLong moved this to mod_bcmap.F
  !TYPE(BC)              :: NUT_TM           !!TIME MAPPING FOR NUTRIENT OBC  !should be
  !
  !WLong these are never used for we are not soving hydrodynamics
  !REAL(SP), ALLOCATABLE     ::  VQDIST(:,:)     !!DISCHARGE VERTICAL DISTRIBUTION
  !REAL(SP), ALLOCATABLE     ::   DQDIS(:,:)     !!WATER FLUX DISCHARGE DATA
  !
      Real (SP), Allocatable :: DWDIS (:, :, :)!!WATER QUALITY DISCHARGE DATA
      Real (SP), Allocatable :: WDIS (:, :)!!CURRENT TIME FRESH WATER QUALITY
      Real (SP), Allocatable :: WVQDIST (:, :)!!DISCHARGE VERTICAL DISTRIBUTION for point source
  !
  !
      Real (SP), Allocatable :: PQDIS (:)!!Current DISCHARGE at non-point source node
      Real (SP), Allocatable :: PDQDIS (:, :)!!DISCHARGE at non-point source node
      Real (SP), Allocatable :: PWQDIS (:, :)!!Current WATER QUALITY at non-point source node
  !
      Real (SP), Allocatable :: PDWQDIS (:, :, :)!!WATER QUALITY DATA at non-point source node		
  !
  !Wlong moved this to mod_bcmap.F
  !REAL(SP), ALLOCATABLE     :: WQOBC(:,:,:,:)   !!WATER QUALITY DATA AT BOUNDARY	!WLong should not be here
  !===================================================================================!
  !
      Integer, Allocatable :: N_ICELLQ (:, :)!!node number of the edge for element edge source
  !
Contains
  !
  !Subroutine BCS_ALLOC()
  !Subroutine BCS_DEALLOC()
  !
      Subroutine BCS_ALLOC
    !
    !INODEQ(:)        !!LOCAL FRESH WATER INFLOW NODES		  !allocated in bcs_force
    !ICELLQ(:)        !!LOCAL FRESH WATER INFLOW ELEMENTS 				!in bcs_force	
    !RIV_GL2LOC(:)    !!GLOBAL TO LOCAL MAP OF FW NODES					!in bcs_force	
    !INOPNT(:)        !!LOCAL NON-POINT SOURCE NODES					!in bcs_force	
    !
    !DWDIS(:,:,:)     !!WATER QUALITY DISCHARGE DATA					!in bcs_force	
    !
    !WDIS(:,:)        !!CURRENT TIME FRESH WATER QUALITY				!in bcond_wqm.F	
    !WVQDIST(:,:)     !!DISCHARGE VERTICAL DISTRIBUTION for point source!in bcs_force	
    !
    !PQDIS(:)         !!Current DISCHARGE at non-point source node		!in adv_wqm.F	
    !PDQDIS(:,:)      !!DISCHARGE at non-point source node				!in bcs_force.F
    !PWQDIS(:,:)      !!Current WATER QUALITY at non-point source node  !in adv_wqm.F	
    !
    !PDWQDIS(:,:,:)   !!WATER QUALITY DATA at non-point source node		!in bcs_force.F
    !
    !QBC_TM%TIMES(:)  !in bcs_force.F
    !PNT_TM%TIMES(:)  !in bcs_force.F
    !
    !!VQDIST(:,:)      !!DISCHARGE VERTICAL DISTRIBUTION				!never used		
    !!DQDIS(:,:)       !!WATER FLUX DISCHARGE DATA						!never used		
    !
    !===================================================================================!
    !
      End Subroutine BCS_ALLOC
  !
      Subroutine BCS_DEALLOC
    !
         If (ALLOCATED(INODEQ)) DEALLOCATE (INODEQ)
         If (ALLOCATED(ICELLQ)) DEALLOCATE (ICELLQ)
         If (PAR) Then
            If (ALLOCATED(RIV_GL2LOC)) DEALLOCATE (RIV_GL2LOC)
         End If
         If (ALLOCATED(INOPNT)) DEALLOCATE (INOPNT)
         If (ALLOCATED(DWDIS)) DEALLOCATE (DWDIS)
         If (ALLOCATED(WVQDIST)) DEALLOCATE (WVQDIST)
         If (ALLOCATED(PDQDIS)) DEALLOCATE (PDQDIS)
         If (ALLOCATED(PDWQDIS)) DEALLOCATE (PDWQDIS)
    !
    !IF(ALLOCATED(WDIS))DEALLOCATE(WDIS)					!locally deallocated in bcond_wqm.F
    !IF(ALLOCATED(PQDIS))DEALLOCATE(PQDIS)					!locally deallocated in adv_wqm.F
    !IF(ALLOCATED(PWQDIS))DEALLOCATE(PWQDIS)				!locally deallocated in adv_wqm.F
    !
    !!IF(ALLOCATED(VQDIST))DEALLOCATE(VQDIST)				!never used for not solving flow equation
    !!IF(ALLOCATED(DQDIS))DEALLOCATE(DQDIS)				!never used for not solving flow equation
    !
         If (ALLOCATED(QBC_TM%TIMES)) DEALLOCATE (QBC_TM%TIMES)
         If (ALLOCATED(PNT_TM%TIMES)) DEALLOCATE (PNT_TM%TIMES)
    !
    !
    !LB: these were missing:
         If (ALLOCATED(N_ICELLQ)) DEALLOCATE (N_ICELLQ)
    !
      End Subroutine BCS_DEALLOC
  !
End Module MOD_BCS
