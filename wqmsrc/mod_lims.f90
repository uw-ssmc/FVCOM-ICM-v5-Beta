!==============================================================================|
!   GLOBAL LIMITS AND ARRAY SIZING PARAMETERS                                  !
!==============================================================================|
!
Module MOD_LIMS
  !
      Implicit None
      Save
  !
      Integer NLOC !!NUMBER OF ELEMENTS
      Integer MLOC !!NUMBER OF NODES
      Integer NISBCE_1 !!LOCAL NUMBER OF ELEMENTS WITH ISBCE = 1
      Integer NISBCE_2 !!LOCAL NUMBER OF ELEMENTS WITH ISBCE = 2
      Integer NISBCE_3 !!LOCAL NUMBER OF ELEMENTS WITH ISBCE = 3
  !
      Integer KB !!NUMBER OF SIGMA LEVELS
      Integer KBM1 !!NUMBER OF SIGMA LEVELS-1
      Integer KBM2 !!NUMBER OF SIGMA LEVELS-2
      Integer MYID !!UNIQUE PROCESSOR ID (1 => NPROCS)
      Integer NPROCS !!NUMBER OF PROCESSORS
      Integer NE !!NUMBER OF UNIQUE EDGES
      Integer NCV !!NUMBER OF INTERNAL CONTROL VOLUMES (EXTENDED LOCAL ONLY)
  !
      Integer IINT !!TYKIM added for nudging
      Integer NCV_I !!NUMBER OF INTERNAL CONTROL VOLUMES (LOCAL ONLY)
      Integer NTLOC !!TOTAL OF LOCAL INTERNAL + HALO ELEMENTS
      Integer MTLOC !!TOTAL OF LOCAL INTERNAL + HALO NODES
      Integer NCT !!(NTLOC) *3
      Integer MX_NBR_ELEM !!MAX NUMBER OF ELEMENTS SURROUNDING A NODE
  !
      Integer NUMQBC_GL, NUMPNT_GL
      Integer NUMQBC, NUMPNT, NBTX_PNT, TCR_PNT
      Integer NstationMax
      Parameter (NstationMax=200)
  !
  !Maximum number of stations
  !note this is predifined here because this is going into
  !defintion of NstationNum_GL for NAMELIST /wqm_stations/
  !fortran90 does not support dynamic arrays in namelist yet
  !
End Module MOD_LIMS
!
