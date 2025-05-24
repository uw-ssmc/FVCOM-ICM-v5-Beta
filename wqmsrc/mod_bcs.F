!mod_bcs.F
!!************************************************************************
!**                                                                    **
!**                           FVCOM-ICM_4.0                            **
!**                                                                    **
!**               A Finite Volume Based Integrated Compartment         **
!**                         Water Quality Model                        **      
!**        The original unstructured-grid ICM code was developed by    ** 
!**    the FVCOM development team at the University of Massachusetts   ** 
!**         through a contract with U.S. Army Corps of Engineers       ** 
!**         [Dr. Changsheng Chen (PI), Dr. Jianhua Qi and              ** 
!**                      Dr. Geoffrey W. Cowles]                       **
!**                                                                    **
!**                Subsequent Development and Maintenance by           ** 
!**                   PNNL/UW Salish Sea Modeling Center               **
!**                                                                    **
!**                 Tarang Khangaonkar    :  PNNL (2008 - Present)     **
!**                 Lakshitha Premathilake:  PNNL (2019 - Present)     **
!**                 Adi Nugraha           :  PNNL/UW (2018 - Present)  **
!**                 Kurt Glaesmann        :  PNNL (2008 - Present)     **
!**                 Laura Bianucci        :  PNNL/DFO(2015 - Present)  **
!**                 Wen Long              :  PNNL (2012-2016)          **
!**                 Taeyum Kim            :  PNNL (2008-2011)          **
!**                 Rochelle G Labiosa    :  PNNL (2009-2010)          **
!**                                                                    **
!**                                                                    **
!**                     Adopted from CE-QUAL-ICM  Model                **
!**                           Developed by:                            **
!**                                                                    **
!**             Carl F. Cerco      : Water quality scheme              **
!**             Raymond S. Chapman : Numerical solution scheme         **
!**             Thomas M. Cole     : Computer algorithms & coding      **
!**             Hydroqual          : Sediment compartment              **
!**                                                                    **
!**                    Water Quality Modeling Group                    **
!**                    U.S. Army Corps of Engineers                    **
!**                    Waterways Experiment Station                    **
!**                    Vicksburg, Mississippi 39180                    **
!**                                                                    **
!************************************************************************
!
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
      INTEGER, ALLOCATABLE :: TXPNT_ID (:) ! local point source idss for toxics
      INTEGER, ALLOCATABLE :: TCRPNT_ID (:) ! local point source idss for bacteria/pollutant

      Type (BC) :: QBC_TM !!TIME MAP FOR RIVER DISCHARGE DATA
      Type (BC) :: PNT_TM !!TIME MAP FOR non-POINT/point SOURCE DATA
      Type (BC) :: TX_TM   ! Time map for point sources of toxics loading
      Type (BC) :: TCR_TM   ! Time map for point sources of bacteria/pollutant loading
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
      REAL (SP), ALLOCATABLE :: TXQDIST(:,:) !! vertical flow distribution for toxics sources
      REAL (SP), ALLOCATABLE :: TCRQDIST(:,:) !! vertical flow distribution for bacteria/pollutant sources
  !
      Real (SP), Allocatable :: PQDIS (:)!!Current DISCHARGE at non-point source node
      Real (SP), Allocatable :: PDQDIS (:, :)!!DISCHARGE at non-point/point source node
      REAL (SP), ALLOCATABLE :: TXQDIS (:, :) !! discharge at point source node for toxics
      REAL (SP), ALLOCATABLE :: TXFQDIS (:) ! for flux control routines
      REAL (SP), ALLOCATABLE :: TCRQDIS (:, :) !! discharge at point source node for Bacteria
      REAL (SP), ALLOCATABLE :: BCTFQDIS (:) ! for flux control routines for Bacteria

      Real (SP), Allocatable :: PWQDIS (:, :)!!Current WATER QUALITY at non-point/point source node
      REAL (SP), ALLOCATABLE :: TXCQDIS (:, :, :) ! toxics concentration at point source nodes
      REAL (SP), ALLOCATABLE :: TXCFQDIS (:, :) ! for flux control routines
      REAL (SP), ALLOCATABLE :: TCRCQDIS (:, :, :) ! bacteria concentration at point source nodes
      REAL (SP), ALLOCATABLE :: BCTCFQDIS (:, :) ! for flux control routines bacteria

  !
      Real (SP), Allocatable :: PDWQDIS (:, :, :)!!WATER QUALITY DATA at non-point source node
  !
  !Wlong moved this to mod_bcmap.F
  !REAL(SP), ALLOCATABLE     :: WQOBC(:,:,:,:)   !!WATER QUALITY DATA AT BOUNDARY	
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
#if defined (MULTIPROCESSOR)
         If (PAR) Then
            If (ALLOCATED(RIV_GL2LOC)) DEALLOCATE (RIV_GL2LOC)
         End If
#endif
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
