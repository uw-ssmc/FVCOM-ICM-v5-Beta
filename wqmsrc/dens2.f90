!==============================================================================|
!     COMPUTE DENSITY USING SALINITY AND POTENTIAL TEMP                        |
!		 Code taken from FVCOM               							       |
!  CALCULATES: RHO(M) DENSITY AT NODES				                           |
!==============================================================================|
!
Subroutine DENS2
!
  !==============================================================================|
      Use MOD_PREC, Only: SP
      Use MOD_WQM, Only: T, SALT
      Use MOD_HYDROVARS, Only: D, RHO
      Use MOD_LIMS, Only: MTLOC, KBM1
      Use MOD_CONTROL, Only: MSR
      Implicit None
      Real (SP), Dimension (0:MTLOC, KBM1) :: RHOF, TF, SF
      Integer :: I, K
  !==============================================================================|
  !
!!!LB: Initialize RHO as zero
  !!   RHO = 0.0_SP
  !
  !
  !  CALCULATE DENSITY FROM EQUATION OF STATE
  !
      Do I = 1, MTLOC
         Do K = 1, KBM1
            TF (I, K) = T (I, K)!LB uncommented this on Jan 7 2016
            SF (I, K) = SALT (I, K)
!
        !LB 7jan16 commented:       TF(I,K) = max(T1(I,K),1.0) !T.K 1/31/2013 added this for stability in intertidal regions
!
            RHOF (I, K) = SF (I, K) * SF (I, K) * SF (I, K) * &
           & 6.76786136E-6_SP - SF (I, K) * SF (I, K) * 4.8249614E-4_SP &
           & + SF (I, K) * 8.14876577E-1_SP - 0.22584586E0_SP
!
            RHOF (I, K) = RHOF (I, K) * (TF(I, K)*TF(I, K)*TF(I, &
           & K)*1.667E-8_SP-TF(I, K)*TF(I, K)*8.164E-7_SP+TF(I, &
           & K)*1.803E-5_SP)
!
            RHOF (I, K) = RHOF (I, K) + 1. - TF (I, K) * TF (I, K) * TF &
           & (I, K) * 1.0843E-6_SP + TF (I, K) * TF (I, K) * &
           & 9.8185E-5_SP - TF (I, K) * 4.786E-3_SP
!
            RHOF (I, K) = RHOF (I, K) * (SF(I, K)*SF(I, K)*SF(I, &
           & K)*6.76786136E-6_SP-SF(I, K)*SF(I, &
           & K)*4.8249614E-4_SP+SF(I, &
           & K)*8.14876577E-1_SP+3.895414E-2_SP)
!
            RHOF (I, K) = RHOF (I, K) - (TF(I, K)-3.98_SP) ** 2 * &
           & (TF(I, K)+283.0_SP) / (503.57_SP*(TF(I, K)+67.26_SP))
         End Do
      End Do
!
  !
  !  CALCULATE RHO at nodes (internal + halo nodes)
  !
      Do I = 1, MTLOC
     !IF (D(I) > 0.0_SP)THEN  !LB: didn't include this here- I need RHO to be calculated everywhere! (this IF exists in FVCOM)
         Do K = 1, KBM1
            RHO (I, K) = RHOF (I, K) * 1.e-3_SP
        !
        !--Up to here, RHO is actually sigma-t and in cgs units. Transform to density and MKS (kg/m^3):
            RHO (I, K) = (1.0_SP+RHO(I, K)) * 1000.0_SP
         End Do
     !END IF  !else, RHO stays zero at total depth D<0 (dry?)
      End Do
  !
  !If(MSR) Write (*,*) 'LBnote D1. RHOF,RHO,D(at some surf place)= ',RHOF(1,1),RHO(1,1),D(1)
  !
!
  !!RHO = 1000.0_SP
!
      Return
End Subroutine DENS2
!==============================================================================|
