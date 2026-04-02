!wqm_main.F
!************************************************************************
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
!!
Program FVCOMICM
  !
      Use MOD_PREC, Only: 				&
#if defined (MULTIPROCESSOR)
		MPI_F,							&
#endif
		SP

      Use MOD_SIZES, Only: NCP, NSSFP, NGL, NSAVM, NTXVB, NBTCR ! AN
      Use MOD_LIMS, Only: MYID, NPROCS, IINT, NTLOC, NLOC, MLOC, KB
  !
      Use MOD_TGE, Only: NV, TGE_DEALLOC
  !
      Use MOD_HYDROVARS, Only: PI, ART1, Z, ZZ,ZZ2D, DZ,DZ2D, DZZ, H, D, DT, DT1, &
     & EL, ET, DTFA, UU, VV, UUT, VVT, WTST, UARD_OBCNT, XFLUX_OBCT, &
     & DTFAT, WTS, UARD_OBCN, XFLUX_OBC, KH, VISCOFH, UNC1, VNC1, WNC1, &
     & WTSNC1, UARD_OBCNNC1, XFLUX_OBCNC1, DTFANC1, KHNC1, TNC1, SNC1, &
     & ELNC1, UNC2, VNC2, WNC2, WTSNC2, UARD_OBCNNC2, XFLUX_OBCNC2, &
     & DTFANC2, KHNC2, TNC2, SNC2, ELNC2, THOUR, t_his_start, &
     & t_his_end, t_his_dlt, hydro_dlt, Nstation, NstationNum_GL, &
     & t_stn_start, t_stn_end, t_stn_dlt, STNFN, HISFN, HISFN_PREFIX, &
     & HISFN_EXT, HISFN_FINAL, HISFN_SPLIT_BYLEVEL, HYDRO_GEOM_DEALLOC, &
     & HYDRO_DEALLOC, NTHYDRO,IFNC,hydro_filenumstart,NTRECNC

	   USE MOD_HYDRO, ONLY:			&!
#if defined (MULTIPROCESSOR)
        BROADCAST_HYDRO         &    !BROADCAST_HYDRO()
       ,BROADCAST_HYDRO_REDUCED &    !BROADCAST_HYDRO_REDUCED()
       ,SCATTER,                &    !SCATTER()
#endif
	    HYDRO		   			     !HYDRO()
!
  !
      Use MOD_BCMAP, Only: BCMAP_DEALLOC

      Use MOD_CONTROL, Only: SERIAL, MSR, PAR, CASENAME
  !
      Use MOD_FILEINFO, Only: DIA, BFO, KFL, RSO, SNP, PLT, APL, TFL, &
     & OPL, SFO, ICO, MBL, UNIT_STN, UNIT_HIS, CNAME, INIT_FILE_INFO,IREPORT
  !
      Use MOD_SF, Only: SFEEDER, NSPECIES, SFEED, SFEED_GL, SUSPFEED, &
     & SF_DEALLOC
!
      Use MOD_WQM, Only: JDAY, DLT, AHMDLT, DLTAV, ELTMS, ELTMS1, &
     & ELTMJD, SEDIMENT_CALC, T, C2, SALT, SSI, B1, B2, B3, LDOC, RDOC, &
     & LPOC, RPOC, NH4, NO3, UREA, LDON, RDON, LPON, RPON, PO4, LDOP, &
     & RDOP, LPOP, RPOP, PIP, COD, DOXG, SIUPB, SIAT, PIB1, PIB2, PIB3, &
     & Q1, Q2, Q3, BENTHIC_FLUXES, DSSR, DTC, FD, FLOW, I0, IT, &
     & LIGHT_EXTINCTION, RELTMS, SAV_CALC, SAVENT, SEDENT, TODS, TTSS, &
     & Z_DIFFUSION, AC, ANC1, ANC2, ANC3, ASC1, ASC2, ASC3, C1, C2F, &
     & CMASS, DTM, FLUXS, FNDLT, KBM1, MASS_BALANCE, NAC, NIT, SFENT, &
     & SOLIDS_CALC, TMEND, TMSTRT, WCMC, WCMN, WCMP, WCMS, XYDF, &
     & XY_DIFFUSION, ATMFLXN, ATMFLXNB, ATMFLXP, ATMFLXPB, ATMLDON, &
     & ATMLDOP, ATMNH4, ATMNO3, ATMPO4, ATMRDON, ATMRDOP, BENFLXDN, &
     & BENFLXPN, BENFLXPNB, DLWCMC, DLWCMN, DLWCMP, IWCMC, IWCMN, &
     & IWCMP, PRECIP, SFATOT, ANDC, BENFLXDNB, BENFLXDP, BENFLXDPB, &
     & BENFLXPC, BENFLXPCB, BENFLXPP, BENFLXPPB, BENNH4, BENNO3, BENSA, &
     & BENPO4, BNDN, DENIT, DLALGC, DLWCKMC, DLWCKMCB, DLWCKMN, &
     & DLWCKMNB, DTMP1, FLXTLDON, FLXTLPON, FLXTNH4, FLXTNO3, FLXTRDON, &
     & INFLOW, JCB, MNLLDOC, MNLRDOC, PCFWS, PPFWS, RESPC, BNDC, &
     & BNDFLXC, BNDFLXN, BNDFLXP, BNDP, DLWCSSMN, DLWCSSMP, FLXT1, &
     & FLXT2, FLXT3, FLXTLDOC, FLXTLDOP, FLXTLPOC, FLXTLPOP, FLXTLZ, &
     & FLXTPIB1, FLXTPIB2, FLXTPIB3, FLXTPO4, FLXTRDOC, FLXTRDOP, &
     & FLXTRPOC, FLXTRPON, FLXTRPOP, FLXTSZ, S1FLXN, S1FLXP, S2FLXN, &
     & S3FLXN, BENDEN, BURIALFLXC, BURIALFLXCB, BURIALFLXN, &
     & BURIALFLXNB, BURIALFLXP, BURIALFLXPB, DFTMP, DLSEDKC, DLSEDKCB, &
     & DLSEDKN, DLSEDKNB, DLSEDMC, DLSEDMN, DLSEDMP, DLSEDSSMC, &
     & DLSEDSSMN, DLSEDSSMP, DLWCSSMC, DTMP2, ERRSEDN, ERRWCC, ERRWCN, &
     & ERRWCP, S1FLXC, S2FLXC, S2FLXP, S3FLXC, S3FLXP, C2_GL, CCHL1, &
     & CCHL1_GL, D_GL, EL_GL, ERRSEDC, ERRSEDP, H_GL, MGL, MTLOC, P1, &
     & S_GL, T_GL, B1SZ_GL, BM1, BM1_GL, BM2, BM2_GL, FI1, FI1_GL, FI2, &
     & FI2_GL, NL1, NL1_GL, NL2, NL2_GL, P1_GL, P2, P2_GL, PL1, PL1_GL, &
     & PL2, PL2_GL, B1LZ_GL, B2LZ_GL, B2SZ_GL, DCOD, DCOD_GL, DDOC, &
     & DDOC_GL, IAVG_GL, IK1, IK1_GL, IK2, IK2_GL, NITRIF, NITRIF_GL, &
     & PR1, PR1_GL, PR2, PR2_GL, GPP_GL, NPP_GL, GPP, NPP, total_netPP, &
     & total_netPP_GL, BFLUX, BFLUX_GL, COD_GL, COURMX, DIAGNOSTICS, &
     & DOLZ_GL, DOPR, DOPR_GL, DORALG, DORALG_GL, DOSZ_GL, FTCOD, &
     & FTCOD_GL, REAERDO, REAERDO_GL, RESP, RESP_GL, PNFWS, CTEMP, &
     & DIFFMX, B, BB, IJDAY, LENGTH, SAV_LOADS, WQM_DEALLOC, UL_GL, &
     & VL_GL, SL_GL, TL_GL, WTSL_GL, KHL_GL, TDIC, TALK, pH, pCO2, &
     & pH_GL, pCO2_GL, ADVFLUX, ADVFLUX_GL, VDIFFLUX, VDIFFLUX_GL, &
	 & TSS_GL, SSI_GL, TURB_GL, KESS_GL, PARAD_GL, LEAF_GL, STEM_GL, ROOT_GL, &
	 & TUBER_GL, EP_GL, TxcKntOn, TxcOutput, NumTxVrb, TcrKntOn,WqCalcOn, &           !LB added ADV,VDIF FLUXES 26jan2016
     & BioAcc,PcbConWSZ,PcbConWLZ,PcbConWSZ_GL,PcbConWLZ_GL

	 ! Adi-2/13/18 added TSS_GL, SSI_GL, TURB_GL, KESS_GL
  !
      Use MOD_OWQ, Only: INTKE, NXKEI, IAVG, KESS, OWQ_DEALLOC, &
     & LGHT_ATTN, TSS, TURB, PARAD !Adi-2/13/18, Added TSS, TURB

	!  Use MOD_OWQ, Only: INTKE, NXKEI, IAVG, KESS, OWQ_DEALLOC, &
    ! & LGHT_ATTN
  !
      Use MOD_CO2SYS, Only: CO2SYSCONST_DEALLOC
  !
      Use MOD_WQMINIT, Only: RSOD, RSODP, RSOFN, DLTD, DLTDP, DIAD, &
     & DIADP, DIAF, COURFS, COURBS, F, SB, DIFFFS, DIFFBS, NXTVD, &
     & NXDIA, MXDLT, TM1, TM2, EXT1, EXT2, EXT3, EXT4, TITLE, FILENAME, &
     & RESTART_OUT, END_RUN, VOLUME_BALANCE, ICOND_OUT, AUTO_STEPPING, &
     & NEW_VOLUMES, ALGAE_CALC, ZOO_CALC, CARBON_CALC, NITROGEN_CALC, &
     & PHOSPHORUS_CALC, COD_CALC, OXYGEN_CALC, SILICA_CALC, LEFT_FLOWB, &
     & IFLOWP, C1MIN, C1MAX, DOVDAYS, ACC, DLTVAL, DLTMAX, DLTFTN, &
     & WQMINIT_DEALLOC, CARBONATE_CALC, CBN_IMPACT_BIO
!
      Use MOD_ALGAL, Only: ALGAE
!
      Use MOD_ZOOP, Only: ANCSZ, ANCLZ, APCSZ, APCLZ, CLSZ, CLLZ, B1SZ, &
     & B2SZ, B1LZ, B2LZ, DOSZ, DOLZ, ACLSZ, ACLLZ, ARSZ, ARLZ, ABMSZ, &
     & ABMLZ, AMSZ, AMLZ, APRSZLZ, AGSZ, AGLZ, ADOCSZ, APOCSZ, ADOCLZ, &
     & APOCLZ, ANH4SZ, ADONSZ, APONSZ, ANH4LZ, ADONLZ, APONLZ, APO4SZ, &
     & ADOPSZ, APOPSZ, APO4LZ, ADOPLZ, APOPLZ, APRSZ, APRLZ, APISZ, &
     & APILZ, AB1SZ, AB2SZ, AB3SZ, AB1LZ, AB2LZ, AB3LZ, ADOSZ, ADOLZ, &
     & ASASZ, ASUSZ, ASALZ, ASULZ, SZ, LZ, ZOOP_DEALLOC
!
      Use MOD_KIN, Only: TEMPER, SOLIDS, ZOOPL, CARBON, NITROG, PHOSPH, &
     & CODMND, OXYGEN, SILICA, BEN_FLUX
  !
      Use MOD_DIC_TALK, Only: TOTINC, ALKALIN
  !
      Use MOD_PH_PCO2
      USE TXC_KNT, ONLY: TXC_ALLOC, READ_TXC_DATA, PCB_KNT, &
                    & NOCHN_IDS, CHN_IDS, CalcPorst, UpdtTX_FRCNS,SLD_KNT,DWPCB,DSPCB, &
                    & CONPCB_S1,CONPCB_W, CONPCB_S2, ExtractTxcData, CONPCB_WGL, BioAccum, &
                    & CONPCB_S1GL, CONPCB_S2GL, DTxcSZDT, DTxcLZDT
      USE SC_TCR, ONLY : BactAlloc, ReadBactData, DWTCR, DSTCR, &
                    & ConTcrSdm1, ConTcrSdm2, TcrtOutput, TcrKnt, ExtractTcrData,ConTcr, &
                    & ConTcrSdm1_GL, ConTcrSdm2_GL,TcrSldKnt
  !
      Use MOD_SED, Only: STEADY_STATE_SED_G3, STEADY_STATE_SED_IC, &
     & STEADY_STATE_SED_TS, DLTS, NDTSED, IERR, ISEDMN, ISEDMP, ISEDMC, &
     & SEDMN, SEDMP, SEDMC, BENSTRTM1S, SODTM1S, JNH4TM1S, JNO3TM1S, &
     & JPO4TM1S, JCH4TM1S, JCH4GTM1S, JHSTM1S, JSITM1S, NH41TM1S, &
     & NH42TM1S, NH4T2TM1S, NO31TM1S, NO32TM1S, NO3T2TM1S, HS1TM1S, &
     & HS2TM1S, HST2TM1S, SI1TM1S, SI2TM1S, SIT2TM1S, PO41TM1S, &
     & PO42TM1S, PO4T2TM1S, CH41TM1S, CH42TM1S, CH4T2TM1S, SO4T2TM1S, &
     & BURIALC, BURIALN, BURIALP, DIAGENC, CTEMP_GL, CPOP_GL, CPON_GL, &
     & CPOC_GL, CPOS_GL, PO4T2TM1S_GL, NH4T2TM1S_GL, NO3T2TM1S_GL, &
     & HST2TM1S_GL, CH4T2TM1S_GL, CH41TM1S_GL, SO4T2TM1S_GL, &
     & SIT2TM1S_GL, BENSTRTM1S_GL, SODTM1S_GL, PO41TM1S_GL, &
     & NH41TM1S_GL, NO31TM1S_GL, HS1TM1S_GL, SI1TM1S_GL, BENDEN_GL, &
     & JPOC_GL, JPON_GL, JPOP_GL, JPOS_GL, CH40_GL, JNH4_GL, JNO3_GL, &
     & JCH4_GL, JCH4G_GL, JHS_GL, JPO4_GL, JSI_GL, NH41_GL, NH42_GL, &
     & NO31_GL, NO32_GL, PO41_GL, PO42_GL, SI1_GL, SI2_GL, CH42_GL, &
     & HS1_GL, HS2_GL, HSED1_GL, HSED1, JPOC, JPON, JPOP, JPOS, &
     & JPOCaccum, JPONaccum, JPOPaccum, JPOSaccum, CPOC, CPON, CPOP, &
     & CPOS, CPO4, CNH4, CNO3, CHS, CSI, DIAGN, SED_INIT2, &
     & SED_INIT_ICI, SED_DEALLOC, POM_ACCUMUL, SED_CALC, &
     & SED_DIAGENESIS_G3, AGGREGATE_POM_FLUX, FRACL1, FRACL2, FRACL3
  !
      Use MOD_SED_DF_EXCHANGE_VARS, Only: SED_DF_EXCHANGE_ALLOC, &
     & SED_DF_EXCHANGE_DEALLOC
  !
      Use MOD_SED_SF_EXCHANGE_VARS, Only: SED_SF_EXCHANGE_ALLOC, &
     & SED_SF_EXCHANGE_DEALLOC
  !
      Use MOD_SED_SAV_EXCHANGE_VARS, Only: SED_SAV_EXCHANGE_ALLOC, &
     & SED_SAV_EXCHANGE_DEALLOC
  !
      Use MOD_BA, Only: BALGAE_CALC, BBM, BBM_GL, BA_DEALLOC
  !
      Use MOD_OBCS, Only: SETUP_OBC, OBCS_DEALLOC
  !
      Use MOD_BCS, Only: BCS_DEALLOC
  !
      Use MOD_SAV, Only: SAV_DEALLOC, SAV_LOADS_DEALLOC, SAV_COMP, &
	 & LEAF, STEM, ROOT, TUBER, EP
  !
#  if defined (MULTIPROCESSOR)
      Use MOD_PAR, Only: ELID, NLID, ELID_X, NLID_X, NGID, EC, NC, BNC, &
     & EMAP, NMAP, NODE_MATCH_ALL, EXCHANGE_ALL, GATHER, INIT_MPI_ENV, &
     & PAR_DEALLOC, NBN, BN_LOC, BN_MLT, GATHER_TXC !added to use node_match_all instead of exchange in line ~645
!
#  endif
  !
      Use MOD_FILENAMES, Only: FNAMEPREFIX, FNAMEEXT !, ifindext

# if defined (WET_DRY)
	USE  MOD_WD, ONLY: ISWETN, WD_WET_JUDGE, WET_DRY_ON, WD_UPDATE, WD_DEALLOC
# endif

#  if defined (MULTIPROCESSOR)
      Use MOD_BUFFERS, Only: RCVBUF, SNDBUF
#  endif
  !
      Use MOD_DF, Only: DFEEDER, DFEEDM1S_GL, DFEEDM1S, DF_DEALLOC
  !
#if defined (ONLY_AIRSEA)
      Use MOD_CO2SYS, Only: CO2star_sat, pCO2atm, CO2star_surf, &
     & AIRSEA_OPTION, UNITCO2ATM, K0, FugFac, DICGAS, &
	 & JDstart1,INSTCDFLX
      Use MOD_WQM, Only: AREAR, BREAR, CREAR, WMS
#endif

#if defined (OUTPUT_NETCDF)
	 USE MOD_NCD, only: out_netcdf_data, set_ncd_io, CDF_OUT, CDF_INT
#endif

      Implicit None
# if defined (MULTIPROCESSOR)
      Include "mpif.h"
# endif
      Integer :: J, JF, JG, JCON, K, L, N
      Integer :: I, II, III, ISF, JP, JNP
      Integer :: JS1, JS2, JS3, PRO2, VTK_FLNUM, numTcrtVrb
  !
      Integer :: MASTER_IINT !Wen Long test synchronization of while loop
      Real (SP) :: MASTER_ELTMS !Wen Long test synchronization of while loop
!
      Character (Len=80) :: TEMPSTR, FileNum
  !
      Integer :: I1, I2, numTxvb,jj,kk
      Real (SP) :: ITT, TTMP1, TTMP2, DELCN, numTxcVrb
!
  !WLong Added logicals for writing station and history outputs
      Logical :: OPEN_HIS, PRINT_HIS
      Logical :: OPEN_STN, PRINT_STN
      Integer (4) :: CTR_HIS, CTR_STN !record counter for station and history outputs
  !
      Integer (4) :: NTSTN_OLD, NTSTN
      Integer (4) :: NTHIS_OLD, NTHIS
  !
  !   CHARACTER(1024):: fnameprefix, fnameext
      Character (1024) :: HISFN_FILENUMBER
  !
  !Wen Long time the simulation here
  !
      Real (8) :: WTIME_MOD = 0.d0
      Real (8) :: WTIME_SYS = 0.d0
      Real (8) :: TOTAL_TIME = 0.d0
      Real (4) :: ETIME, DTIME !ETIME and DTIME are intrinsic functions to calculate time
      Real (4) :: ELAPSED (2)
  !
      Character (Len=1024) :: TITLE_STN, TcrWCRestartFile, TcrSDRestartFile, TxcWCRestartFile, TxcSD1RestartFile, TxcSD2RestartFile
      CHARACTER (LEN=1024) :: TxcAccmSZRstrFile, TxcAccmLZRstrFile
  !
      Character (Len=1024) :: COMMAND_STR, SLEEPTIME_STR
      Real (SP) :: SLEEPTIME
  !
      Logical :: END_RUN_CHILD = .False.
      Integer :: ISNDTAG = 0, IRCVTAG = 0
      LOGICAL :: PTNTX_SLD = .FALSE.
!
#if defined (ONLY_AIRSEA)
      Real (SP) :: FLUXCO2, TDOS, RNU, KRDO, KRCO2, FACTOR, Sc
      Real (SP) :: FACTORaux, CHLORN, AA, TSaux, DOS
      Real (SP) :: auxLB
#endif

	  REAL (SP) :: ELTMS_Hydro1=0.0,ELTMS_hydro2=0.0

#if defined (INTERP_HYDRO)
	  REAL (SP) :: UFACT, DFACT
#endif
!
  !
  !************************************************************************
  !**                       Program Initialization                       **
  !************************************************************************
  !
  !==============================================================================!
  !   SETUP PARALLEL ENVIRONMENT                                                 !
  !==============================================================================!
  !
      SERIAL = .True.
      PAR = .False.
      MSR = .True.
      MYID = 1
      NPROCS = 1
      VTK_FLNUM = 300
      CBN_IMPACT_BIO = .FALSE.
      
#  if defined (MULTIPROCESSOR)
      Call INIT_MPI_ENV (MYID, NPROCS, SERIAL, PAR, MSR)
#  endif
  !
  !--IMPORT CASENAME FROM COMMAND LINE
      If (MSR) Then
         Call GETARG (1, TEMPSTR)
     !
         If (LEN_TRIM(TEMPSTR) == 0) Then
            Write (*,*) 'PLEASE PROVIDE CASENAME ON COMMAND LINE'
            Write (*,*) 'STOPPING...'
            Call PSTOP
         End If
         CASENAME = ADJUSTL (TEMPSTR)
      End If
  !
      If (MSR) Then
         TOTAL_TIME = TOTAL_TIME + DTIME (ELAPSED)!CALL DTIME() and return user time in ELAPSED(1), system time in ELAPSED(2)
         WTIME_MOD = WTIME_MOD + ELAPSED (1)
         WTIME_SYS = WTIME_SYS + ELAPSED (2)
      End If
  !
  !
#  if defined (MULTIPROCESSOR)
      Call MPI_BCAST (CASENAME, 80, MPI_CHARACTER, 0, MPI_COMM_WORLD, &
     & IERR)
#  endif
  !
      Call INIT_FILE_INFO ()	!  Setup file handles for all modules
!-------------------------------------------------------------------------------
      CALL MainSetup         ! newly added function for model re-structuring-Laki
      CALL Init_ALLOC        ! newly added functions for model re-structuring-Laki
!-------------------------------------------------------------------------------
  !
      Call INPUTS ()			!  Initialze

#if defined (OUTPUT_NETCDF)
	  CALL SET_NCD_IO(IREPORT)
#endif
      If (SEDIMENT_CALC) Then
     !
     !CALL SED_INIT()          !  Setup sediment diagenesis module (including SED_ALLOC)
     !CALL SED_INIT2()         !  initialize the values from input read by INPUTS() above
     !
         If (DFEEDER) Then
            Call SED_DF_EXCHANGE_ALLOC
         End If
     !
         Call SED_SF_EXCHANGE_ALLOC
     !
         If (SAV_CALC) Then
            Call SED_SAV_EXCHANGE_ALLOC
         End If
      End If
  !
      Call SETUP_OBC

    !----------------------------------- Computations for toxics -----------------------------
      IF (TxcKntOn) THEN
            IF (MSR) WRITE(*,*)'Toxic module is activated ..........'
            CALL TXC_ALLOC
            CALL READ_TXC_DATA
      END IF
    !------------------------------------------------------------------------------------------

    !---------------------------------- computations for bacteria/pollutants ---------------------- 
      IF (TcrKntOn) THEN 
            IF (MSR) WRITE(*,*)'Tracer module is activated ..........'
            CALL BactAlloc
            CALL ReadBactData
      END IF
    !---------------------------------------------------------------------------------------------- 

    !--------------------------------------------------------------------------------------
  !CALL SET_BNDRY           !WLong: Was in FVCOM, but now This removed,

  !
      CTR_STN = 0 !counter for station output records
      CTR_HIS = 0 !counter for history output records
      PRINT_HIS = .False.
      PRINT_STN = .False.
      OPEN_HIS = .False.
      OPEN_STN = .False.
  !
  !counting numbe of steps past t_his_start and t_stn_start
  
  !restart file there
!
      NTHIS_OLD = FLOOR ((JDAY-t_his_start)/t_his_dlt) - 1 !number of steps for history output
      NTSTN_OLD = FLOOR ((JDAY-t_stn_start)/t_stn_dlt) - 1 !number of steps for station output
      NTHIS = 0
      NTSTN = 0
  !
      T (:, :) = C2 (:, :, 1)
      SALT (:, :) = C2 (:, :, 2)
      SSI (:, :) = C2 (:, :, 3)
      B1 (:, :) = C2 (:, :, 4)
      B2 (:, :) = C2 (:, :, 5)
      B3 (:, :) = C2 (:, :, 6)
      SZ (:, :) = C2 (:, :, 7)
      LZ (:, :) = C2 (:, :, 8)
      LDOC (:, :) = C2 (:, :, 9)
      RDOC (:, :) = C2 (:, :, 10)
      LPOC (:, :) = C2 (:, :, 11)
      RPOC (:, :) = C2 (:, :, 12)
      NH4 (:, :) = C2 (:, :, 13)
      NO3 (:, :) = C2 (:, :, 14)
      UREA (:, :) = C2 (:, :, 15)
      LDON (:, :) = C2 (:, :, 16)
      RDON (:, :) = C2 (:, :, 17)
      LPON (:, :) = C2 (:, :, 18)
      RPON (:, :) = C2 (:, :, 19)
      PO4 (:, :) = C2 (:, :, 20)
      LDOP (:, :) = C2 (:, :, 21)
      RDOP (:, :) = C2 (:, :, 22)
      LPOP (:, :) = C2 (:, :, 23)
      RPOP (:, :) = C2 (:, :, 24)
      PIP (:, :) = C2 (:, :, 25)
      COD (:, :) = C2 (:, :, 26)
      DOXG (:, :) = C2 (:, :, 27)
      SIUPB (:, :) = C2 (:, :, 28)
      SIAT (:, :) = C2 (:, :, 29)
      PIB1 (:, :) = C2 (:, :, 30)
      PIB2 (:, :) = C2 (:, :, 31)
      PIB3 (:, :) = C2 (:, :, 32)
      TDIC (:, :) = C2 (:, :, 33)!LB
      TALK (:, :) = C2 (:, :, 34)!LB
      
    !-------------------------------------- For tracer/pollutant module----------------------------------
    IF (TcrKntOn) THEN 
         numTcrtVrb = NCP - NBTCR + 1
         JJ=1
         DO I=numTcrtVrb,NCP
            ConTcr(:,:,JJ) =  C2(:,:,I)
            JJ = JJ + 1
         END DO
    END IF
    !----------------------------------------------------------------------------------------------------

      IINT = 0
      

  !************************************************************************
  !**                          Begin Simulation                          **
  !************************************************************************
  !
    Do WHILE ( .Not. END_RUN)        ! starting the main computational loop
     !
         IINT = IINT + 1
         THOUR = ELTMS / 3600.0
     !
         If (MSR) WRITE (*,*) 'SIMULATION TIME: ', THOUR, ' HOURS', &
        & JDAY, 'JDAYS'

     ! time-varying forcing data

         If (JDAY >= NXTVD) Call TVDS (NXTVD)

# if defined (MULTIPROCESSOR)
         If (PAR) Then
!
        !Wen Long
        !Here we should make sure ELTMS is the same for all processors
        !as we are going to have a broadcasting from master process
        !and only master process is reading the hydrodynamics.

!
            If (MSR) Then !Master calculates its own IINT
               MASTER_IINT = IINT !Then broadcast it to all processes
               MASTER_ELTMS = ELTMS
            End If
            Call MPI_BCAST (MASTER_IINT, 1, MPI_INTEGER, 0, &
           & MPI_COMM_WORLD, IERR)
            Call MPI_BCAST (MASTER_ELTMS, 1, MPI_REAL8, 0, &
           & MPI_COMM_WORLD, IERR)!Notice MPI_REAL8
        !
            If (MASTER_IINT /= IINT) Then !child checks for consistency
               Write (*,*) 'MYID=', MYID, '. Oops, inconsistent time st&
              &ep here with master process'
            End If
        !
            If (MASTER_ELTMS /= ELTMS) Then !child checks for consistency
               Write (*,*) 'MYID=', MYID, '. Oops, inconsistent time, E&
              &LTMS=', ELTMS
            End If
         End If
# endif
        
		 !ELTMS_hydro1 =<ELTMS <=ELTMS_hydro2


!         If ((Mod(ELTMS, hydro_dlt).EQ.0.0) .And. (ELTMS > 0.0)) Then

		 !ELTMS_hydro2	= TMSTRT*86400.0_SP+(NTHYDRO-1)*hydro_dlt   !second record
		 !ELTMS_hydro1	= ELTMS_hydro2-hydro_dlt					!first record

		  If ( ELTMS>= ELTMS_hydro2 .AND. ELTMS > 0.0_SP) Then
		  !If ( ELTMS>= ELTMS_hydro2 .AND. ELTMS > TMSTRT*86400.0_SP) Then  !Wen Long : this allow hydro_dlt to not be
		  !    															   !integral multiple of DLT

	        UNC1 = UNC2
            VNC1 = VNC2
            WNC1 = WNC2
            WTSNC1 = WTSNC2
            UARD_OBCNNC1 = UARD_OBCNNC2
            XFLUX_OBCNC1 = XFLUX_OBCNC2
            DTFANC1 = DTFANC2
            KHNC1 = KHNC2
            ELNC1 = ELNC2
            TNC1 = TNC2
            SNC1 = SNC2

	       !update the master process hydrodynamics in to UNC2, etc
           
            If (MSR) Call HYDRO ()

        !update the child process hydrodynamics by broadcasting from master
# if defined (MULTIPROCESSOR)

        !Method A): scatter the hydrodynamics from master proc to other procs
        !           This is supposed to be faster as the message size is smaller. 
!
        !Method B) broadcast hydrodynamics from master proc to other procs
            Call BROADCAST_HYDRO (0, UNC2, VNC2, WTSNC2, UARD_OBCNNC2, &
           & XFLUX_OBCNC2, DTFANC2, KHNC2, ELNC2, TNC2, SNC2)

		!Wen Long: Since only the master is reading hydro, we need to make sure
		!all the records are correct for all the processors
		!
			Call MPI_BCAST (NTHYDRO, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERR)
			Call MPI_BCAST (NTRECNC, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERR)
			Call MPI_BCAST (IFNC, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERR)

# endif
         End If
     !
     !--Time interpolation of physical fields
     !
#if defined (INTERP_HYDRO)
		 IF(MSR)THEN
			ELTMS_hydro2	= TMSTRT*86400.0_SP+(NTHYDRO-1)*hydro_dlt   !second record
			ELTMS_hydro1	= ELTMS_hydro2-hydro_dlt					!first record
		 ENDIF
#	if defined (MULTIPROCESSOR)
		 Call MPI_BCAST (ELTMS_hydro2, 1, MPI_F, 0, MPI_COMM_WORLD, IERR)
		 Call MPI_BCAST (ELTMS_hydro1, 1, MPI_F, 0, MPI_COMM_WORLD, IERR)
#	endif
		 UFACT			= (ELTMS_hydro2-ELTMS)/hydro_dlt
		 DFACT			= (ELTMS-ELTMS_hydro1)/hydro_dlt

		!IF(ELTMS<ELTMS_hydro1)THEN
		!							!for starting period, where the model time ELTMS
		!							!is less than the first time step in the hydro netcdf
		!							!use the first record only
		!   UFACT=1.0
		!   DFACT=0.0
		! ENDIF

         UU 			= UNC1*UFACT 		+ UNC2*DFACT
         VV 			= VNC1*UFACT 		+ VNC2*DFACT
         WTS 			= WTSNC1*UFACT 		+ WTSNC2*DFACT
         UARD_OBCN 		= UARD_OBCNNC1*UFACT+ UARD_OBCNNC2*DFACT
         XFLUX_OBC 		= XFLUX_OBCNC1*UFACT+ XFLUX_OBCNC2*DFACT
         DTFA 			= DTFANC1*UFACT		+ DTFANC2*DFACT
         KH 			= KHNC1*UFACT 		+ KHNC2*DFACT
         T 				= TNC1*UFACT 		+ TNC2*DFACT
         SALT 			= SNC1*UFACT 		+ SNC2*DFACT
         EL 			= ELNC1*UFACT 		+ ELNC2*DFACT

		 IF(ELTMS > ELTMS_hydro2)THEN
			IF(MSR)WRITE(*,*)'Oops, running out of hydroydnamics model results at NTHYDRO= ',NTHYDRO
			IF(MSR)WRITE (*,*) 'NC File Number: ', IFNC+hydro_filenumstart-1
			IF(MSR)WRITE(*,*) 'time1, time, time2, NHYD',ELTMS_hydro1,ELTMS, ELTMS_hydro2, NTHYDRO
			CALL PSTOP
		 ENDIF
		!WRITE(*,*) 'MYID, time1, time, time2, NHYD',MYID,ELTMS_hydro1,ELTMS, ELTMS_hydro2, NTHYDRO
#else
		 ELTMS_hydro2	= TMSTRT*86400.0_SP+(NTHYDRO-1)*hydro_dlt   !second record
		 ELTMS_hydro1	= ELTMS_hydro2-hydro_dlt					!first record
		 UU = UNC1
         VV = VNC1
         WTS = WTSNC1
         UARD_OBCN = UARD_OBCNNC1
         XFLUX_OBC = XFLUX_OBCNC1
         DTFA = DTFANC1
         KH = KHNC1
         T = TNC1
         SALT = SNC1
         EL = ELNC1
#endif

         D = H + EL

         C2 (:, :, 1) = T (:, :)
         C2 (:, :, 2) = SALT (:, :)


     !IF (NLID(503) /= 0) THEN
     !   write(*,*),'LBnote Ssurf(node503river)= ',SALT(NLID(503),1)
     !   write(*,*),'LBnote Usurf(node503river)= ',UU(ELID(796),1)
     !   write(*,*),'LBnote Vsurf(node503river)= ',VV(ELID(796),1)
     !   write(*,*),'LBnote TAsur(node503river)= ',C2(NLID(503),1,34)
     !END IF
     !
     !--Calculate density !LB 8 Jan 2015
         Call DENS2
!
     !
     !******* Dead sea case
     !
         If ( .Not. FLOW) Then
            UU = 0.0
            VV = 0.0
            WTS = 0.0

			!WLong need to check here to make sure D is positive
         End If
     !

#if defined (WET_DRY)
		 IF(WET_DRY_ON)THEN
			CALL WD_WET_JUDGE
		 ENDIF
#endif

         If ( .Not. Z_DIFFUSION) KH = 0.0
         ELTMS1 = REAL (ELTMS, 8)
     !******* Time-variable solar radiation
     !--- TIME OF DAY (SECONDS)
         RELTMS = ELTMS
         TODS = Mod (RELTMS, 86400.)
 !

 !
         If (TODS < TTSS) Then
            I0 = 0.
		 Else If (TODS > 86400.-TTSS) Then ! AN: Avoid spike at the end of the day
            I0 = 0.
		 Else
        !--- DAYS SINCE SUNRISE (DAYS)
            DSSR = (TODS-TTSS) / 86400.
 !
            I0 = PI * IT * Sin (PI*DSSR/FD) / 2. / FD
 !
            !If(MSR) Write (*,*) 'FD, JDAY - in MAIN= ', FD, JDAY !AN
            I0 = Max (I0, 0.)
 !
         End If


     !
     !--- HARDWIRE LIGHT HERE
     !        I0 = 1.0
     !
     !******* Light Attenuation
     !
         If (LIGHT_EXTINCTION) Then !Wen Long added this flag of LIGHT_EXTINCTION
            If (SAV_CALC .Or. ALGAE_CALC) Then
               If (JDAY >= NXKEI) Then
                  NXKEI = NXKEI + INTKE / 24.
                  If (I0 > 0. .Or. IAVG(1, 1) > 0.) Call LGHT_ATTN !calculate light attenuation
               End If
            End If
         End If
     !
     !******* SAV Submodel
     !
         SAVENT = - 9999
         If (SAV_CALC) Then
            Call SAV_COMP
         End If
     !
     !******* Determine cell quota for this time step
     !WLong and LB reactivated calculation of Q1,Q2,Q3
         Do K = 1, KBM1
            Do I = 1, MLOC
               Q1 (I, K) = PIB1 (I, K) / (B1(I, K)+1.0E-9)!Phosphorus to Carbon Ration in Alg 1 (gP/gC)
               Q2 (I, K) = PIB2 (I, K) / (B2(I, K)+1.0E-9)!Phosphorus to Carbon Ration in Alg 2 (gP/gC)
               Q3 (I, K) = PIB3 (I, K) / (B3(I, K)+1.0E-9)!Phosphorus to Carbon Ration in Alg 1 (gP/gC)
           !
           !WLong and LB: for testing sediment diageesis
           !WLong and LB: Im going to give a constant value
           !
               If (SEDIMENT_CALC .And. ACC(30) == 'OFF') Then
                  Q1 (I, K) = 0.0243
               End If
               If (SEDIMENT_CALC .And. ACC(31) == 'OFF') Then
                  Q2 (I, K) = 0.0243
               End If
               If (SEDIMENT_CALC .And. ACC(32) == 'OFF') Then
                  Q3 (I, K) = 0.0243
               End If
           !
            End Do
         End Do
     !If(MSR) Write (*,*) 'LBnote 1. pH,pCO2,RHO(at some surf place)= ',pH(1,1),pCO2(1,1),RHO(1,1)

!---------------------------------- Toxic kinetic update -------------------------------------
IF (TxcKntOn) THEN
     CALL SLD_KNT(DLT)     ! Evaluate toxic related variables

     CALL CalcPorst
     CALL UpdtTX_FRCNS
     CALL ExtractTxcData
     CALL PCB_KNT

     IF(BioAcc == .TRUE.) THEN 
        CALL BioAccum
     END IF


     numTxvb = NCP - NTXVB - NBTCR + 1
     !C2(:,:,numTxvb:NCP) = C2(:,:,numTxvb:NCP) + DWPCB(:,:,:)*DLT

     DO I=1,MLOC
        DO J=1,KBM1 
            kk = 0
            DO K=numTxvb,NCP
                kk = kk + 1
                C2(I,J,K) = C2(I,J,K) + DWPCB(I,J,kk)*DLT
                IF(C2(I,J,K) < 1E-32) C2(I,J,K) = 1E-28

                IF(BioAcc) THEN 
                    PcbConWSZ(I,J,kk) = PcbConWSZ(I,J,kk) + DTxcSZDT(I,J,kk)*DLT
                    PcbConWLZ(I,J,Kk) = PcbConWLZ(I,J,kk) + DTxcLZDT(I,J,kk)*DLT 
                    IF(PcbConWSZ(I,J,kk) < 1E-32) PcbConWSZ(I,J,kk) = 1E-28
                    IF(PcbConWLZ(I,J,kk) < 1E-32) PcbConWLZ(I,J,kk) = 1E-28
                END IF
            END DO 
        END DO 
     END DO

     !------------- For sediment Layer ----------------------------------
     DO I=1,MLOC
       DO K=1,NTXVB
        CONPCB_S1(I,K) = CONPCB_S1(I,K) + DSPCB(I,K,1)*DLT
        CONPCB_S2(I,K) = CONPCB_S2(I,K) + DSPCB(I,K,2)*DLT
        IF(CONPCB_S1(I,K) < 1E-32) CONPCB_S1(I,K) = 1E-28
        IF(CONPCB_S2(I,K) < 1E-32) CONPCB_S2(I,K) = 1E-28
       END DO
     END DO

END IF
!--------------------------------------------------------------------------------- 

!------------------------ Tracer kinetic update -------------------------------- 
IF(TcrKntOn) THEN
    CALL TcrSldKnt(DLT)
    CALL TcrKnt(DLT)
    numTcrtVrb = NCP - NBTCR + 1
    DO I=1,MLOC
        DO J=1,KBM1 
            kk = 0
            DO K=numTcrtVrb,NCP
                kk = kk + 1
                !DWTCR(I,J,kk) = 0.0
                C2(I,J,K) = C2(I,J,K) + DWTCR(I,J,kk)*DLT
            END DO 
        END DO 
    END DO

    DO I=1,MLOC 
        kk = 0
        DO K=numTcrtVrb,NCP
            kk = kk + 1 
            ConTcrSdm1(I,kk) = ConTcrSdm1(I,kk) + DSTCR(I,kk,1)*DLT
        END DO
    END DO
    
    !ConTcrSdm1(:,:) = ConTcrSdm1(:,:) + DSTCR(:,:,1)*DLT
    !ConTcrSdm2(:,:) = ConTcrSdm2(:,:) + DSTCR(:,:,2)*DLT
    !CALL ExtractTcrData
END IF
!------------------------------------------------------------------------------

#if !defined (NO_BIOLOGY) && !defined (ONLY_AIRSEA)
    IF(WqCalcOn == .TRUE.) THEN  ! for wqm calculations
     !     IF (TEMPERATURE_CALC) CALL TEMPER(DTC(0,1,1))

         DTC (:, :, 1) = 0.0
     !In next section first subscript changed to 0 per KURT
         If (SOLIDS_CALC) Call SOLIDS (DTC(0, 1, 3), FLUXS(0, 1, 1))!WLong and LB, we should move this into the IF block below as well??
    END IF  ! for wqm calculations 

         If (Mod(ELTMS, DLT) .Eq. 0) Then

#     if(1)
    IF(WqCalcOn == .TRUE.) THEN     ! for wqm calculations
            If (ZOO_CALC) Call ZOOPL (DTC(0, 1, 7), DTC(0, 1, 8))
            If (ALGAE_CALC) Call ALGAE (DTC(0, 1, 4), DTC(0, 1, 5), &
           & DTC(0, 1, 6), FLUXS(0, 1, 2), FLUXS(0, 1, 3), FLUXS(0, 1, &
           & 4))
            If (CARBON_CALC) Call CARBON (DTC(0, 1, 9), DTC(0, 1, 10), &
           & DTC(0, 1, 11), DTC(0, 1, 12), FLUXS(0, 1, 5))
            If (NITROGEN_CALC) Call NITROG (DTC(0, 1, 13), DTC(0, 1, &
           & 14), DTC(0, 1, 16), DTC(0, 1, 17), DTC(0, 1, 18), DTC(0, &
           & 1, 19), FLUXS(0, 1, 6))
            If (PHOSPHORUS_CALC) Call PHOSPH (DTC(0, 1, 20), DTC(0, 1, &
           & 21), DTC(0, 1, 22), DTC(0, 1, 23), DTC(0, 1, 24), DTC(0, &
           & 1, 25), DTC(0, 1, 30), DTC(0, 1, 31), DTC(0, 1, 32), &
           & FLUXS(0, 1, 7), FLUXS(0, 1, 8), FLUXS(0, 1, 9))
            If (COD_CALC) Call CODMND (DTC(0, 1, 26))
            If (OXYGEN_CALC) Call OXYGEN (DTC(0, 1, 27))
            If (SILICA_CALC) Call SILICA (DTC(0, 1, 28), DTC(0, 1, 29), &
           & FLUXS(0, 1, 10))
        !

            If (CARBONATE_CALC) Then
               Call CALC_PH_PCO2
               Call TOTINC (DTC(0, 1, 33))!DTC for TDIC and TALK is in mmol/m3/sec
               Call ALKALIN (DTC(0, 1, 34))!(rather than g/m3/sec like other variables)
            End If
        !
            SFENT = - 9999
            If (SFEEDER) Then
               Do I = 1, NSPECIES
                  Call SUSPFEED (I, DTC(0, 1, 4), DTC(0, 1, 5), DTC(0, &
                 & 1, 6), DTC(0, 1, 30), DTC(0, 1, 31), DTC(0, 1, 32), &
                 & DTC(0, 1, 11), DTC(0, 1, 23), DTC(0, 1, 18), DTC(0, &
                 & 1, 12), DTC(0, 1, 24), DTC(0, 1, 19), DTC(0, 1, 3), &
                 & DTC(0, 1, 28), DTC(0, 1, 29), DTC(0, 1, 20))
               End Do
            End If
    END IF  ! for wqm calculations
#     endif


        !     *** Convert water column change in concentration to change in mass
            Do JCON = 1, NAC
               II = AC (JCON)
               Do K = 1, KBM1
                  Do I = 1, MLOC
#  if !defined (WET_DRY)
						IF(D(I) > 0.0_SP)THEN
#  else
						IF(ISWETN(I) == 1)THEN
#  endif
							DTM (I, K, II) = DTC (I, K, II)
						ELSE
							DTC (I, K, II) = 0.0_SP
							DTM (I, K, II) = DTC (I, K, II)
						ENDIF
                  End Do
               End Do
            End Do
        !
         End If


     
     !***** Horizontal diffusion
     !
     !If (XY_DIFFUSION .And. XYDF < 0.0) Call VISCOF_H  !commented by WL and LB 12may16
     
         Call VISCOF_H !LB and WL added this call 12may2016


!      CALL VERTVL  
!
!#if defined (WET_DRY)
!		IF(WET_DRY_ON) CALL WD_UPDATE(2) !update wet_dry if DTFA is changed by VERTVL above
!#endif

         Call EXCHANGE_WQM (C2)		!LB commented to emulate us_fvcom.F order 5may2016
         Call ADV_WQM 				!calculates ADVFLUX (horiz adv + vert adv + horiz diff)
         Call FCT_NUT
         Do JCON = 1, NAC 			!Update biology here instead of adv_wqm.F !LB
            II = AC (JCON)
            Do I = 1, MLOC
               Do K = 1, KBM1
#  if !defined (WET_DRY)
					IF(D(I) > 0.0_SP)THEN
#  else
					IF(ISWETN(I) == 1)THEN
#  endif

                        C2F (I, K, II) = C2F (I, K, II) + DTM (I, K, II) * &
                 & DLT
					ENDIF


                  C2F (I, K, II) = Max (C2F(I, K, II), 0.0)
                  If (II .Eq. 4 .Or. II .Eq. 5 .Or. II .Eq. 6) Then
                     C2F (I, K, II) = Max (C2F(I, K, II), 0.003)
									!make sure there are seeds for ALG1,2 and 3
				  End If
				  If (II .Eq. 7 .Or. II .Eq. 8) Then
                     C2F (I, K, II) = Max (C2F(I, K, II), 0.001)
									!make sure there are seeds for SZ and LZ !Adi
                  End If
               End Do
            End Do
         End Do

         Call VDIF_WQM (C2F)!calculates VDIFFLUX (vert diff)

         Call BCOND_WQM !Boundary Conditions            !!LB commented to emulate us_fvcom.F order 5may2016


         Call EXCHANGE_WQM (C2F)!Interprocessor Exchange  !LB commented to emulate us_fvcom.F order 5may2016
         C2 = C2F
         ET = EL
         DT = D

     !
     !LB-------------------------------------------------------------------------------
     !******* LB: pH and pCO2 were calculated with TDIC and TALK before advection and diffusion.
     !        Therefore, to keep pH and pCO2 up to the current time, we need to call
     !        calc_pH_pCO2 again - but only if we're saving pH and pCO2 in the output (otherwise, we don't really care!)
     
     IF(WqCalcOn == .TRUE.) THEN    ! for wqm calculations
            If (CARBONATE_CALC) Then
            Call CALC_PH_PCO2 !(pH,pCO2)
         End If
     END IF     ! for wqm calculations
     !LB-------------------------------------------------------------------------------
     !

!
# if(1) /* Laura and Wen moved sediment calculation after WQ update */
     !
     !******* Sediment kinetics

     IF(WqCalcOn == .TRUE.) THEN           ! for wqm calculations
         SEDENT = - 9999
         If (SEDIMENT_CALC) Then !---------------------------------------------------------
			!set initial condition using steady state calculation
            If ((STEADY_STATE_SED_IC) .And. (JDAY < &
           & TMSTRT+DLT/86400.0)) Then
               Call SED_INIT_ICI !WL: make sure the sediment CTEMP get the initial condition of temperature from
           !just updated water column (end of first time step)

           !
           !OR SIMPLY USE the following line
           ! CTEMP=T(:,KBM1)
           !
           !
               Call POM_ACCUMUL
           !
               If (AGGREGATE_POM_FLUX) Then !LB: Option to accumulate all labilities from water column and then distribute them to labilities in sediments
                  Do I = 1, MLOC
                 !Accumulate all labilities into JPOM(I,1)
                     JPOCaccum (I, 1) = JPOCaccum (I, 1) + JPOCaccum &
                    & (I, 2) + JPOCaccum (I, 3)
                     JPONaccum (I, 1) = JPONaccum (I, 1) + JPONaccum &
                    & (I, 2) + JPONaccum (I, 3)
                     JPOPaccum (I, 1) = JPOPaccum (I, 1) + JPOPaccum &
                    & (I, 2) + JPOPaccum (I, 3)
                 !
                 !Divide JPOM(I,1) into labilities according to FRACL1,FRACL2,FRACL3 (given in BFI input)
                     JPOC (I, 1) = JPOCaccum (I, 1) * FRACL1
                     JPON (I, 1) = JPONaccum (I, 1) * FRACL1
                     JPOP (I, 1) = JPOPaccum (I, 1) * FRACL1
                 !
                     JPOC (I, 2) = JPOCaccum (I, 1) * FRACL2
                     JPON (I, 2) = JPONaccum (I, 1) * FRACL2
                     JPOP (I, 2) = JPOPaccum (I, 1) * FRACL2
                 !
                     JPOC (I, 3) = JPOCaccum (I, 1) * FRACL3
                     JPON (I, 3) = JPONaccum (I, 1) * FRACL3
                     JPOP (I, 3) = JPOPaccum (I, 1) * FRACL3
                 !
                     JPOS (I) = JPOSaccum (I)
                  End Do
              !
               Else
                  Do I = 1, MLOC
                     Do K = 1, 3
                        JPOC (I, K) = JPOCaccum (I, K)
                        JPON (I, K) = JPONaccum (I, K)
                        JPOP (I, K) = JPOPaccum (I, K)
                     End Do
                     JPOS (I) = JPOSaccum (I)
                  End Do
               End If


           !
               Call SED_CALC (.True.)!True means use steady state solution as initial condition in sediment module
               Call SED_INIT2 !set the initial conditions using the calcualated value
           !
           !set accumulative fluxes back to zero
               JPOCaccum = 0.0
               JPONaccum = 0.0
               JPOPaccum = 0.0
               JPOSaccum = 0.0
            End If !**********************
        !
        !LB: Here we call POM_ACCUMUL to accumulate organic matter in JPOM variables during NDTSED time steps.
        !    Then, we divide JPOM/NDTSED to calculate average JPOM in that period.
        !    The resulting JPOM is used in SED_CALC, which is only called every NDTSED timesteps
        !
            Call POM_ACCUMUL
!
            If (Mod(NIT, NDTSED) .Eq. 0 .And. NIT .Ne. 0) Then !LB: if NIT is multiple of NDTSED, then average JPOM and call sed_calc
           !
               If (AGGREGATE_POM_FLUX) Then !LB: Option to accumulate all labilities from water column and then distribute them to labilities in sediments
                  Do I = 1, MLOC
                 !Accumulate all labilities into JPOM(I,1)
                     JPOCaccum (I, 1) = JPOCaccum (I, 1) + JPOCaccum &
                    & (I, 2) + JPOCaccum (I, 3)
                     JPONaccum (I, 1) = JPONaccum (I, 1) + JPONaccum &
                    & (I, 2) + JPONaccum (I, 3)
                     JPOPaccum (I, 1) = JPOPaccum (I, 1) + JPOPaccum &
                    & (I, 2) + JPOPaccum (I, 3)
                 !
                 !Divide JPOM(I,1) into labilities according to FRACL1,FRACL2,FRACL3 (given in BFI input)
                     JPOC (I, 1) = JPOCaccum (I, 1) * FRACL1 / REAL &
                    & (NDTSED)
                     JPON (I, 1) = JPONaccum (I, 1) * FRACL1 / REAL &
                    & (NDTSED)
                     JPOP (I, 1) = JPOPaccum (I, 1) * FRACL1 / REAL &
                    & (NDTSED)
                 !
                     JPOC (I, 2) = JPOCaccum (I, 1) * FRACL2 / REAL &
                    & (NDTSED)
                     JPON (I, 2) = JPONaccum (I, 1) * FRACL2 / REAL &
                    & (NDTSED)
                     JPOP (I, 2) = JPOPaccum (I, 1) * FRACL2 / REAL &
                    & (NDTSED)
                 !
                     JPOC (I, 3) = JPOCaccum (I, 1) * FRACL3 / REAL &
                    & (NDTSED)
                     JPON (I, 3) = JPONaccum (I, 1) * FRACL3 / REAL &
                    & (NDTSED)
                     JPOP (I, 3) = JPOPaccum (I, 1) * FRACL3 / REAL &
                    & (NDTSED)
                 !
                     JPOS (I) = JPOSaccum (I) / REAL (NDTSED)
                  End Do
              !
               Else
                  Do I = 1, MLOC
                     Do K = 1, 3
                        JPOC (I, K) = JPOCaccum (I, K) / REAL (NDTSED)
                        JPON (I, K) = JPONaccum (I, K) / REAL (NDTSED)
                        JPOP (I, K) = JPOPaccum (I, K) / REAL (NDTSED)
                     End Do
                     JPOS (I) = JPOSaccum (I) / REAL (NDTSED)
                  End Do
               End If
           !
               If (STEADY_STATE_SED_TS) Then
                  Call SED_CALC (.True.)!quasi-steady state calculation
               Else
                  Call SED_CALC (.False.)!FALSE means use time-varying calculation
               End If
           !
           !set accumulative fluxes back to zero
               JPOCaccum = 0.0
               JPONaccum = 0.0
               JPOPaccum = 0.0
               JPOSaccum = 0.0
           !
            End If
        !
         Else If (BENTHIC_FLUXES) Then !---------------------------------------------------------
            Call BEN_FLUX
         End If !---------------------------------------------------------
    END IF  ! for wqm calculations 
     !
! SEDIMENT CALC
# endif
! !defined (NO_BIOLOGY)
#endif


#if defined (ONLY_AIRSEA)
     !LB: case with no biology but open to atmospheric exchange (DO and TDIC) - Feb 26, 2016
!
    IF (WqCalcOn == .TRUE.) THEN      ! for wqm calculations
         DTM = 0.0
         If (UNITCO2ATM == 'pCO2') Then !if we provided pCO2atm in microatm or ppmv
            Do I = 0, MLOC
           !CO2star_sat(I) = K0(I,1) * pCO2atm(I) * FugFac(I,1)   !micromol/kg-SW --> assume rho=1000kg/m3 --> mmol/m3!LB commented
               CO2star_sat (I) = K0 (I, 1) * pCO2atm (I) * FugFac (I, &
              & 1) * RHO (I, 1) * 1.E-3_SP !mmol/m3
            End Do
         Else !if we provided fCO2atm in microatm or ppmv
            Do I = 0, MLOC
           !CO2star_sat(I) = K0(I,1) * pCO2atm(I)   !micromol/kg-SW --> assume rho=1000kg/m3 --> mmol/m3!LB commented on 8jan2016\
           !CO2star_sat(I) = K0(I,1) * pCO2atm(I) * RHO(I,1) * 1.E-3_SP  !mmol/m3 !!LB: 
               auxLB = K0 (I, 1) * pCO2atm (I) * RHO (I, 1) * 1.E-3_SP
               CO2star_sat (I) = auxLB
            End Do
         End If
!
         If (AIRSEA_OPTION == 1) Then !using AREAR,BREAR,and CREAR
!
            FACTORaux = AREAR * (BREAR*WMS) ** CREAR
            Do I = 1, MLOC
               TDOS = T (I, 1)
               RNU = 0.54 + 0.7 * TDOS / 30.0 - 0.07 * SALT (I, 1) / &
              & 35.0
           !            KRDO      = 0.157*RNU*(1.5*WMS)**1.5
               KRDO = FACTORaux * RNU !up to here, same as in calculation of air-sea O2 fluxes (m/day)
               KRCO2 = (32.0/44.0) ** 0.25 * KRDO !conversion of oxygen reareation into CO2 reareagion rate by Chapra (1997)
!
           !CO2 exchange
               FLUXCO2 = KRCO2 / (D(I)*DZ2D(I,1)) * &
              & (CO2star_sat(I)-CO2star_surf(I))
!
               DTM (I, 1, 33) = FLUXCO2 / 86400.0
           !DICGAS(I,1) = FLUXCO2  !TDIC flux from air-sea gas exchange, snapshot  mmolC/m3/d
               DICGAS (I, 1) = DICGAS (I, 1) + ((FLUXCO2/86400.0)*DLT*ART1(I)*D(I)*DZ2D(I,1))/1E6 !TDIC flux from air-sea gas exchange, accumulated
               INSTCDFLX(I,1) = FLUXCO2
           !oxygen exchange
               CHLORN = SALT (I, 1) / 1.80655
               DOS = 14.5532 + TDOS * (0.0054258*TDOS-0.38217) - CHLORN &
              & * (0.1665+TDOS*(9.796E-5*TDOS-5.866E-3))
               DTM (I, 1, 27) = KRDO / (D(I)*DZ2D(I,1)) * & (DOS-DOXG(I, &
              & 1)) / 86400.
            End Do
!
         !Else
         Else If(AIRSEA_OPTION>1 .AND. AIRSEA_OPTION<=5) Then
!
            If (AIRSEA_OPTION == 2) Then !Wanninkhof (1992)
               FACTORaux = 0.31 * WMS ** 2.0 !0.31 for winds in steady staate;  0.39 for long-term averaged winds
            Else If (AIRSEA_OPTION == 3) Then !Nightingale (2000)
               FACTORaux = (0.33*WMS+0.222*WMS**2.0)
            Else If (AIRSEA_OPTION == 4) Then !Ho (2006)
               FACTORaux = 0.266 * WMS ** 2.0
            Else If (AIRSEA_OPTION == 5) Then !Wanninkhof et al (2013): wind in equation is <U^2>,meaning the time mean of U^2
               FACTORaux = 0.251 * WMS ** 2.0 !over the time interval of flux determination (they compute monthly fluxes with 6hourly wind data)
            End If
!
            Do I = 1, MLOC
           !CO2 exchange
               Sc = 2073.1 - 125.62 * T (I, 1) + 3.6276 * T (I, 1) ** &
              & 2.0 - 0.043219 * T (I, 1) ** 3.0 !Schmidt number for CO2 (W92)
           !Sc = 2116.8 - 136.25 * T(I,1) + 4.7353 * T(I,1)**2.0          &
                !     - 0.092307 * T(I,1)**3.0 + 0.0007555 * T(I,1)**4.0     !Schmidt number for CO2 (W14)
!
               KRCO2 = FACTORaux * Sqrt (660.0/Sc)!gas transfer velocity in cm/hr
               KRCO2 = KRCO2 * 24.0 / 100.0 !m/day
!
               FLUXCO2 = KRCO2 / (D(I)*DZ2D(I,1)) * &
              & (CO2star_sat(I)-CO2star_surf(I))!mmolC/m3/d
!
               DTM (I, 1, 33) = FLUXCO2 / 86400.0 !mmolC/m3/sec
           !DICGAS(I,1) = FLUXCO2  !TDIC flux from air-sea gas exchange, snapshot   !mmolC/m3/day
              DICGAS (I, 1) = DICGAS (I, 1) + ((FLUXCO2/86400.0)*DLT*ART1(I)*D(I)*DZ2D(I,1))/1E6 !TDIC flux from air-sea gas exchange, accumulated
              FLUXCO2(I,1) = FLUXCO2
           !oxygen exchange
               Sc = 1953.4 - 128.0 * T (I, 1) + 3.9918 * T (I, 1) ** &
              & 2.0 - 0.050091 * T (I, 1) ** 3.0 !Schmidt number for O2 (W92)
           !  Sc = 1920.4 - 135.6 * T(I,1) + 5.2122 * T(I,1)**2.0          &
           !       - 0.10939 * T(I,1)**3.0 + 0.00093777 * T(I,1)**4.0    !Schmidt number for O2 (W14)
!
               KRDO = FACTORaux * Sqrt (660.0/Sc)!gas transfer velocity (W12) in cm/hr
               KRDO = KRDO * 24.0 / 100.0 !m/day
!
           !LB:
           !  Calculate O2 saturation concentration using Garcia and Gordon
           !  L&O (1992) formula, (EXP(AA) is in ml/l).
           !
               TSaux = Log ((298.15-T(I, 1))/(273.15+T(I, 1)))
               AA = 2.00907 + TSaux * (3.22014+TSaux*(4.05010+&
              & TSaux*(4.94457+TSaux*(-0.256847+TSaux*3.88767)))) + &
              & SALT (I, 1) * (-0.00624523+TSaux*(-0.00737614+TSaux*(-&
              & 0.0103410+TSaux*-0.00817083))) + - 0.000000488682 * &
              & SALT (I, 1) * SALT (I, 1)
!
               DOS = Exp (AA) * 1.429 !  Converting from ml/l to mg/l (1mg/l=22.391/31.998 ml/l ~ 0.7ml/l)
!
               DTM (I, 1, 27) = KRDO / (D(I)*DZ2D(I,1)) * (DOS-DOXG(I, 1)) &
              & / 86400.
            End Do
		 Else If(AIRSEA_OPTION == 6) Then ! Combination of Option 1 method and Wanninkhof et al (2013)
		    FACTORaux = AREAR * (BREAR*WMS) ** CREAR
!
			If(MSR) Write (*,*) 'JDAY= ',JDAY
			Do I = 1, MLOC
			 If (JDAY <= JDstart1(1) .OR. JDAY >= JDstart1(2)) Then
               TDOS = T (I, 1)
               RNU = 0.54 + 0.7 * TDOS / 30.0 - 0.07 * SALT (I, 1) / &
              & 35.0
           !            KRDO      = 0.157*RNU*(1.5*WMS)**1.5
               KRDO = FACTORaux * RNU !up to here, same as in calculation of air-sea O2 fluxes (m/day)
               KRCO2 = (32.0/44.0) ** 0.25 * KRDO !conversion of oxygen reareation into CO2 reareagion rate by Chapra (1997)
!
!
           !oxygen exchange
               CHLORN = SALT (I, 1) / 1.80655
               DOS = 14.5532 + TDOS * (0.0054258*TDOS-0.38217) - CHLORN &
              & * (0.1665+TDOS*(9.796E-5*TDOS-5.866E-3))
!
			 Else If (JDAY > JDstart1(1) .AND. JDAY < JDstart1(2)) Then
           !CO2 exchange
               Sc = 2073.1 - 125.62 * T (I, 1) + 3.6276 * T (I, 1) ** &
              & 2.0 - 0.043219 * T (I, 1) ** 3.0 !Schmidt number for CO2 (W92)
           !Sc = 2116.8 - 136.25 * T(I,1) + 4.7353 * T(I,1)**2.0          &
                !     - 0.092307 * T(I,1)**3.0 + 0.0007555 * T(I,1)**4.0     !Schmidt number for CO2 (W14)
!
               KRCO2 = FACTORaux * Sqrt (660.0/Sc)!gas transfer velocity in cm/hr
               KRCO2 = KRCO2 * 24.0 / 100.0 !m/day
!
           !oxygen exchange
               Sc = 1953.4 - 128.0 * T (I, 1) + 3.9918 * T (I, 1) ** &
              & 2.0 - 0.050091 * T (I, 1) ** 3.0 !Schmidt number for O2 (W92)
           !  Sc = 1920.4 - 135.6 * T(I,1) + 5.2122 * T(I,1)**2.0          &
           !       - 0.10939 * T(I,1)**3.0 + 0.00093777 * T(I,1)**4.0    !Schmidt number for O2 (W14)
!
               KRDO = FACTORaux * Sqrt (660.0/Sc)!gas transfer velocity (W12) in cm/hr
               KRDO = KRDO * 24.0 / 100.0 !m/day
!
           !LB:
           !  Calculate O2 saturation concentration using Garcia and Gordon
           !  L&O (1992) formula, (EXP(AA) is in ml/l).
           !
               TSaux = Log ((298.15-T(I, 1))/(273.15+T(I, 1)))
               AA = 2.00907 + TSaux * (3.22014+TSaux*(4.05010+&
              & TSaux*(4.94457+TSaux*(-0.256847+TSaux*3.88767)))) + &
              & SALT (I, 1) * (-0.00624523+TSaux*(-0.00737614+TSaux*(-&
              & 0.0103410+TSaux*-0.00817083))) + - 0.000000488682 * &
              & SALT (I, 1) * SALT (I, 1)
!
               DOS = Exp (AA) * 1.429 !  Converting from ml/l to mg/l (1mg/l=22.391/31.998 ml/l ~ 0.7ml/l)
!
			 END IF
!
			  FLUXCO2 = KRCO2 / (D(I)*DZ(1)) * &
             & (CO2star_sat(I)-CO2star_surf(I))!mmolC/m3/d
!
              DTM (I, 1, 33) = FLUXCO2 / 86400.0 !mmolC/m3/sec
           !DICGAS(I,1) = FLUXCO2  !DIC flux from air-sea gas exchange, snapshot   !mmolC/m3/day
              DICGAS (I, 1) = DICGAS (I, 1) + ((FLUXCO2/86400.0)*DLT*ART1(I)*D(I)*DZ2D(I,1))/1E6 !DIC flux from air-sea gas exchange, accumulated
              INSTCDFLX(I,1) = FLUXCO2
              DTM (I, 1, 27) = KRDO / (D(I)*DZ(1)) * (DOS-DOXG(I, 1)) &
             & / 86400.
            End Do
!
         End If
!
!!!UPDATE TDIC AND DO FIELDS:
     !
     !***** Horizontal diffusion
     !
         If (XY_DIFFUSION .And. XYDF < 0.0) Call VISCOF_H

!

!      CALL VERTVL  !
!
!#if defined (WET_DRY)
!		IF(WET_DRY_ON) CALL WD_UPDATE(2) !update wet_dry if DTFA is changed by VERTVL above
!#endif
END IF  ! for wqm calculations

         Call EXCHANGE_WQM (C2)
         Call ADV_WQM !calculates ADVFLUX (horiz adv + vert adv + horiz diff)
         Call FCT_NUT
         Do II = 27, 33, 6 !Update biology here instead of adv_wqm.F !LB ONLY DO and TDIC
            Do I = 1, MLOC
               Do K = 1, KBM1
#  if !defined (WET_DRY)
				  IF(D(I) > 0.0_SP)THEN
#  else
				  IF(ISWETN(I) == 1)THEN
#  endif
					C2F (I, K, II) = C2F (I, K, II) + DTM (I, K, II) * &
                 & DLT
				  ENDIF
                  C2F (I, K, II) = Max (C2F(I, K, II), 0.0)
                  If (II .Eq. 4 .Or. II .Eq. 5 .Or. II .Eq. 6) Then
                     C2F (I, K, II) = Max (C2F(I, K, II), 0.003)!make sure there are seeds for ALG1,2 and 3
                  End If
               End Do
            End Do
         End Do
         Call VDIF_WQM (C2F)!calculates VDIFFLUX (vert diff)
         Call BCOND_WQM !Boundary Conditions            !
     ! KURT GLAESEMANN - have to exchange again, after boundary conditions
     ! because boundary condition code does NOT know about the buffer regions
         Call EXCHANGE_WQM (C2F)!Interprocessor Exchange
         C2 = C2F
         ET = EL
         DT = D
     !
     !LB-------------------------------------------------------------------------------
     !******* LB: pH and pCO2 were calculated with TDIC and TALK before advection and diffusion.
     !        Therefore, to keep pH and pCO2 up to the current time, we need to call
     !        calc_pH_pCO2 again - but only if we're saving pH and pCO2 in the output (otherwise, we don't really care!)
    IF (WqCalcOn == .TRUE.) THEN       ! for wqm calculations
         If (CARBONATE_CALC) Then
            Call CALC_PH_PCO2 !(pH,pCO2)
         End If
    END IF    ! for wqm calculations
     !LB-------------------------------------------------------------------------------
!
!ONLY_AIRSEA
#endif

     !
     !******* Update time related variables
     !
         NIT = NIT + 1
         ELTMS = ELTMS + DLT
         DLTAV = ELTMS / NIT
         JDAY = ELTMS / 86400.
         ELTMJD = JDAY - TMSTRT


#if defined (WET_DRY)
		 IF(WET_DRY_ON)CALL WD_UPDATE(3) !recycle the wet dry information to previous time step
#endif

         If (JDAY .Ge. TMEND) END_RUN = .True.

#if defined(MULTIPROCESSOR)
         If (PAR) Then
			!Check and make sure all procs end the run at the same time
            If (MSR) Then !Master receive END_RUN from each child
               Do I = 2, NPROCS
                  IRCVTAG = I + 1000
                  Call MPI_RECV (END_RUN_CHILD, 1, MPI_LOGICAL, I-1, &
                 & IRCVTAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE, IERR)
                  If (END_RUN) WRITE (*,*) 'Child ID=', I, 'CHILD END_R&
                 &UN=', END_RUN_CHILD
               End Do
            Else !Child procs send to master
               ISNDTAG = MYID + 1000
               Call MPI_SEND (END_RUN, 1, MPI_LOGICAL, 0, ISNDTAG, &
              & MPI_COMM_WORLD, IERR)
            End If
         End If
     !
#endif
     !
         If (MSR) Then
            If (END_RUN) WRITE (*,*) 'MYID =', MYID, 'END_RUN=', &
           & END_RUN !master print its own END_RUN
         End If
     !
         If (JDAY >= DLTD(DLTDP+1)) Then
            DLTDP = DLTDP + 1
            If ( .Not. AUTO_STEPPING) DLT = DLTVAL (DLTDP)
            MXDLT = DLTMAX (DLTDP)
            FNDLT = DLTFTN (DLTDP)
         End If

     !******* Mass balance
     !
         If (MASS_BALANCE) Then
        !
        !       *** Water column constituent mass
        !
            Do JCON = 1, NAC
               II = AC (JCON)
               CMASS (II) = 0.0
               Do K = 1, KBM1
                  Do I = 1, MLOC
                     CMASS (II) = CMASS (II) + C1 (I, K, II) * ART1 (I) &
                    & * D (I) * DZ2D (I,K) * 0.001 !the 0.001 here is to convert to kg
                  End Do
               End Do
            End Do
        !
        !********* Water column nutrient mass
        !
            WCMN = ANC1 * CMASS (4) + ANC2 * CMASS (5) + ANC3 * CMASS &
           & (6) + ANCSZ * CMASS (7) + ANCLZ * CMASS (8) + CMASS (13) + &
           & CMASS (14) + CMASS (15) + CMASS (16) + CMASS (17) + CMASS &
           & (18) + CMASS (19)
            WCMP = APCSZ * CMASS (7) + APCLZ * CMASS (8) + CMASS (20) + &
           & CMASS (21) + CMASS (22) + CMASS (23) + CMASS (24) + CMASS &
           & (25) + CMASS (30) + CMASS (31) + CMASS (32)
            WCMC = CMASS (4) + CMASS (5) + CMASS (6) + CMASS (7) + &
           & CMASS (8) + CMASS (9) + CMASS (10) + CMASS (11) + CMASS &
           & (12)
            WCMS = ASC1 * CMASS (4) + ASC2 * CMASS (5) + ASC3 * CMASS &
           & (6) + CMASS (28) + CMASS (29)
        !
        !********* Change in water column nutrient mass
        !
            DLWCMN = WCMN - IWCMN
            DLWCMP = WCMP - IWCMP
            DLWCMC = WCMC - IWCMC
        !
        !********* Atmospheric loadings during DLT time
        !
            ATMFLXN = ATMFLXN + (ATMNH4+ATMNO3+ATMLDON+ATMRDON) * &
           & PRECIP * SFATOT / 1000. * DLT
        !unit is (gN/m^3)* (m/sec) * (m^2)/1000 * (sec)
        !==> gN/sec*sec/1000 ==kgN
            ATMFLXP = ATMFLXP + (ATMPO4+ATMLDOP+ATMRDOP) * PRECIP * &
           & SFATOT / 1000. * DLT
            TM1 = .001 * DLT * PRECIP
        !
            Do SB = 1, MLOC
               ATMFLXNB (SB) = ATMFLXNB (SB) + &
              & (ATMNH4+ATMNO3+ATMLDON+ATMRDON) * ART1 (SB) * TM1
           !
           !unit of ATMFLXNB here is:
           !   gN/m^3 * (m^2) * (sec) * (m/sec) /1000
           !== gN/1000 = kgN
           !
               ATMFLXPB (SB) = ATMFLXPB (SB) + (ATMPO4+ATMLDOP+ATMRDOP) &
              & * ART1 (SB) * TM1
            End Do
        !
        !       *** Benthic transfers
        !
            TM1 = DLT / 86400000.
            Do BB = 1, MLOC
           !
               TM2 = PNFWS (BB) * ART1 (BB) * TM1 ! unit
           !unit of TM2 is (gN/m^2/day)* (m^2) * (sec)/86400 /1000
           ! ==>  gN /day * day /1000 = kgN in DLT (sec) time)
           !
               BENFLXPN = BENFLXPN + TM2
               BENFLXPNB (BB) = BENFLXPNB (BB) + TM2
           !
               TM2 = (BENNH4(BB)+BENNO3(BB)) * ART1 (BB) * TM1
           !unit of TM2 is (gN/m^2/day)*(m^2)* (sec)/86400/1000 = kgN
               BENFLXDN = BENFLXDN + TM2
               BENFLXDNB (BB) = BENFLXDNB (BB) + TM2
           !
               TM2 = PPFWS (BB) * ART1 (BB) * TM1
               BENFLXPP = BENFLXPP + TM2
               BENFLXPPB (BB) = BENFLXPPB (BB) + TM2
           !
               TM2 = BENPO4 (BB) * ART1 (BB) * TM1
               BENFLXDP = BENFLXDP + TM2
               BENFLXDPB (BB) = BENFLXDPB (BB) + TM2
           !
               TM2 = PCFWS (BB) * ART1 (BB) * TM1
               BENFLXPC = BENFLXPC + TM2
               BENFLXPCB (BB) = BENFLXPCB (BB) + TM2
            End Do
        !
        !       *** Change in nutrient mass due to kinetics
        !
            TM1 = ANDC * DLT / 86400000.
            TM2 = DLT / 86400000.
            Do K = 1, KBM1
               Do I = 1, MLOC
                  DTMP1 = DENIT (I, K) * ART1 (I) * D (I) * DZ2D (I,K) * &
                 & TM1
                  DLWCKMNB (I, K) = DLWCKMNB (I, K) - DTMP1
                  DLWCKMN = DLWCKMN - DTMP1
              !WLong: need to check DENIT and DDLALGC etc units to make sure
              !consistent
                  DTMP1 = (DLALGC(I, K)-RESPC(I, K)-MNLLDOC(I, &
                 & K)-MNLRDOC(I, K)-DENIT(I, K)) * ART1 (I) * D (I) * &
                 & DZ2D (I,K) * TM2
                  DLWCKMCB (I, K) = DLWCKMCB (I, K) + DTMP1
                  DLWCKMC = DLWCKMC + DTMP1
               End Do
            End Do
        !
        !       *** Boundary fluxes
        !
            Do JCB = 1, INFLOW
               F = IFLOWP (JCB)
               BNDN = FLXTNH4 (F) + FLXTNO3 (F) + FLXTLDON (F) + &
              & FLXTRDON (F) + FLXTLPON (F) + FLXTRPON (F) + ANC1 * &
              & FLXT1 (F) + ANC2 * FLXT2 (F) + ANC3 * FLXT3 (F) + ANCSZ &
              & * FLXTSZ (F) + ANCLZ * FLXTLZ (F)
               BNDP = FLXTPO4 (F) + FLXTLDOP (F) + FLXTRDOP (F) + &
              & FLXTLPOP (F) + FLXTRPOP (F) + APCSZ * FLXTSZ (F) + &
              & APCLZ * FLXTLZ (F) + FLXTPIB1 (F) + FLXTPIB2 (F) + &
              & FLXTPIB3 (F)
               BNDC = FLXTLDOC (F) + FLXTRDOC (F) + FLXTLPOC (F) + &
              & FLXTRPOC (F) + FLXT1 (F) + FLXT2 (F) + FLXT3 (F) + &
              & FLXTSZ (F) + FLXTLZ (F)
               If (LEFT_FLOWB(F)) Then
                  BNDFLXN = BNDFLXN + BNDN
                  BNDFLXP = BNDFLXP + BNDP
                  BNDFLXC = BNDFLXC + BNDC
               Else
                  BNDFLXN = BNDFLXN - BNDN
                  BNDFLXP = BNDFLXP - BNDP
                  BNDFLXC = BNDFLXC - BNDC
               End If
            End Do
        !
        !       *** Water column mass balance % error
        !
            DLWCSSMN = BNDFLXN + BENFLXPN + BENFLXDN + S1FLXN + S2FLXN &
           & + S3FLXN + ATMFLXN + DLWCKMN
            DLWCSSMP = BNDFLXP + BENFLXPP + BENFLXDP + S1FLXP + S2FLXP &
           & + S3FLXP + ATMFLXP
            DLWCSSMC = BNDFLXC + BENFLXPC + S1FLXC + S2FLXC + S3FLXC + &
           & DLWCKMC
        !
            ERRWCN = 0.0
            ERRWCP = 0.0
            ERRWCC = 0.0
            If (DLWCMN /= 0.0) ERRWCN = (DLWCMN-DLWCSSMN) / DLWCMN * &
           & 100.
            If (DLWCMP /= 0.0) ERRWCP = (DLWCMP-DLWCSSMP) / DLWCMP * &
           & 100.
            If (DLWCMC /= 0.0) ERRWCC = (DLWCMC-DLWCSSMC) / DLWCMC * &
           & 100.
        !
        !       *** Change in sediment concentration due to burial and kinetics
        !
            DTMP1 = DLTS / 1.E6
            DTMP2 = DLTS / 1.E3
            Do BB = 1, MLOC
           !
               DFTMP = DTMP1 * ART1 (BB)!
           !
               BURIALFLXCB (BB) = BURIALFLXCB (BB) - BURIALC (BB) * &
              & DFTMP !kgC
               BURIALFLXNB (BB) = BURIALFLXNB (BB) - BURIALN (BB) * &
              & DFTMP !(mgN/m^2/d) * d  * m^2 /1000/1000 mgN/1000/1000= gN/1000=kgN
               BURIALFLXPB (BB) = BURIALFLXPB (BB) - BURIALP (BB) * &
              & DFTMP !kgP
           !
               BURIALFLXC = BURIALFLXC - BURIALC (BB) * DFTMP !kgC
               BURIALFLXN = BURIALFLXN - BURIALN (BB) * DFTMP !kgN
               BURIALFLXP = BURIALFLXP - BURIALP (BB) * DFTMP !kgP
           !
           !
               DFTMP = DTMP2 * ART1 (BB)
           !
               DLSEDKCB (BB) = DLSEDKCB (BB) - DIAGENC (BB) * DFTMP !kgC
               DLSEDKNB (BB) = DLSEDKNB (BB) - BENDEN (BB) * DFTMP !kgN
           !Unit of DLSEDKN  is BENDEN * DTMP2* ART1
           ! ==> BENDEN *  (day/1000) * m^2
           ! ==> where  BENDEN(I)  =
           !                         (XAPP1NO3*XAPP1NO3*NO31/S+XK2NO3*NO32)/1000.
           !     has unit (m/day) * (mgN/m^3) /1000 == gN/m^2/day
           ! ==> DLSEDKN has unit  (gN/m^2/day) *(day) * m^2/1000 == kgN
           !
               DLSEDKC = DLSEDKC - DIAGENC (BB) * DFTMP !kgC
               DLSEDKN = DLSEDKN - BENDEN (BB) * DFTMP !kgN
           !
            End Do
        !
        !********* Sediment mass balance change in DLTS time time
        !
            DLSEDMN = SEDMN - ISEDMN
            DLSEDMP = SEDMP - ISEDMP
            DLSEDMC = SEDMC - ISEDMC
        !
            DLSEDSSMN = BURIALFLXN + DLSEDKN - BENFLXPN - BENFLXDN
            DLSEDSSMP = BURIALFLXP - BENFLXPP - BENFLXDP
            DLSEDSSMC = BURIALFLXC + DLSEDKC - BENFLXPC
        !
        !         *** Sediment mass balance % error
        !
            If (SEDIMENT_CALC) Then
               ERRSEDN = 0.0
               ERRSEDP = 0.0
               ERRSEDC = 0.0
               If (DLSEDMN /= 0.0) ERRSEDN = (DLSEDMN-DLSEDSSMN) / &
              & DLSEDMN * 100.
               If (DLSEDMP /= 0.0) ERRSEDP = (DLSEDMP-DLSEDSSMP) / &
              & DLSEDMP * 100.
               If (DLSEDMC /= 0.0) ERRSEDC = (DLSEDMC-DLSEDSSMC) / &
              & DLSEDMC * 100.
            End If
         End If !END MASS_BALANCE

!
         Do I = 1, NTLOC
            DT1 (I) = DT (NV(I, 1)) + DT (NV(I, 2)) + DT (NV(I, 3))
            DT1 (I) = DT1 (I) / 3.0
         End Do
     !
     !RGL commented below
     !     T(:,:)    = C2(:,:,1)
     !     SALT(:,:) = C2(:,:,2)
!
         SSI (:, :) = C2 (:, :, 3)
         B1 (:, :) = C2 (:, :, 4)
         B2 (:, :) = C2 (:, :, 5)
         B3 (:, :) = C2 (:, :, 6)
         SZ (:, :) = C2 (:, :, 7)
         LZ (:, :) = C2 (:, :, 8)
         LDOC (:, :) = C2 (:, :, 9)
         RDOC (:, :) = C2 (:, :, 10)
         LPOC (:, :) = C2 (:, :, 11)
         RPOC (:, :) = C2 (:, :, 12)
         NH4 (:, :) = C2 (:, :, 13)
         NO3 (:, :) = C2 (:, :, 14)
         UREA (:, :) = C2 (:, :, 15)
         LDON (:, :) = C2 (:, :, 16)
         RDON (:, :) = C2 (:, :, 17)
         LPON (:, :) = C2 (:, :, 18)
         RPON (:, :) = C2 (:, :, 19)
         PO4 (:, :) = C2 (:, :, 20)
         LDOP (:, :) = C2 (:, :, 21)
         RDOP (:, :) = C2 (:, :, 22)
         LPOP (:, :) = C2 (:, :, 23)
         RPOP (:, :) = C2 (:, :, 24)
         PIP (:, :) = C2 (:, :, 25)
         COD (:, :) = C2 (:, :, 26)
         DOXG (:, :) = C2 (:, :, 27)
         SIUPB (:, :) = C2 (:, :, 28)
         SIAT (:, :) = C2 (:, :, 29)
         PIB1 (:, :) = C2 (:, :, 30)
         PIB2 (:, :) = C2 (:, :, 31)
         PIB3 (:, :) = C2 (:, :, 32)
         TDIC (:, :) = C2 (:, :, 33)!LB
         TALK (:, :) = C2 (:, :, 34)!LB


         CALL DebugNan()

        !-------------------------------------- For tracer/pollutant module----------------------------------
        IF (TcrKntOn) THEN 
            numTcrtVrb = NCP - NBTCR + 1

            JJ=1
            DO I=numTcrtVrb,NCP
                ConTcr(:,:,JJ) =  C2(:,:,I)
                JJ = JJ + 1
            END DO

            CALL ExtractTcrData
        END IF
        !----------------------------------------------------------------------------------------------------

!=========================================================================================================== 
!---------------------------------------- Outputting Results -----------------------------------------------


!--------------------------------------------NETCDF OUTPUT---------------------------------------------------

#if defined (OUTPUT_NETCDF)
		IF(CDF_INT /= 0 .AND. CDF_OUT)THEN
			IF(MOD(IINT,CDF_INT)==0) CALL out_netcdf_data(IREPORT)
		ENDIF
#endif


         Call WQM_OUTPUT (NTHIS_OLD, NTSTN_OLD, NTHIS, NTSTN, CTR_HIS, &
        & CTR_STN)

                !IF(MSR) THEN
                      !IF(MOD(NIT,20) == 0.0) THEN
                          !VTK_FLNUM = VTK_FLNUM + 1
                          !CALL DILU_OUTPUT(VTK_FLNUM,1)
                      !END IF
                ! END IF

!!---------------------- Writing restart files according to given restart points ----------------------------
     ! KURT GLAESEMANN - restarts
    If (RESTART_OUT) Then
            If (JDAY >= RSOD(RSODP) .Or. END_RUN) Then

# if defined (MULTIPROCESSOR)
            !ALLOCATE (C2_GL(MGL,KBM1,NCP))
            !C2_GL = 0.0

            IF(TcrKntOn == .TRUE.) THEN 
                ALLOCATE(ConTcrSdm1_GL(MGL,NBTCR))
                ConTcrSdm1_GL = 0.0
                !ALLOCATE(ConTcrSdm2_GL(MGL,NBTCR))
                !ConTcrSdm2_GL = 0.0
            END IF

    !----------------------------------------------- Global arrays for toxic related variables for restart-------------------        
            IF(TxcKntOn == .TRUE.) THEN 
                ALLOCATE(CONPCB_S1GL(MGL,NTXVB))
                CONPCB_S1GL = 0.0
                ALLOCATE(CONPCB_S2GL(MGL,NTXVB))
                CONPCB_S2GL = 0.0
                IF(BioAcc == .TRUE.) THEN 
                    ALLOCATE(PcbConWSZ_GL(MGL,KBM1,NTXVB))
                    PcbConWSZ_GL = 0.0
                    ALLOCATE(PcbConWLZ_GL(MGL,KBM1,NTXVB))
                    PcbConWLZ_GL = 0.0
                END IF
            END IF
    !-----------------------------------------------------------------------------------------------------------------------
            
            If (PAR) Then
                  Do J = 1, NCP
                     Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, &
                    & NPROCS, NMAP, C2(0, 1, J), C2_GL(1, 1, J))
                  End Do

                  IF(TcrKntOn == .TRUE.) THEN 
                        Call GATHER (1, MTLOC, MLOC, MGL, NBTCR, MYID, &
                        & NPROCS, NMAP, ConTcrSdm1, ConTcrSdm1_GL)
                  !     Call GATHER (1, MTLOC, MLOC, MGL, NBTCR, MYID, &
                  !      & NPROCS, NMAP, ConTcrSdm2, ConTcrSdm2_GL)
                  END IF
    !---------------------------------------------- Gathering of toxic related variables for restart files---------------------------              
                  IF(TxcKntOn == .TRUE.) THEN 
                        Call GATHER (1, MTLOC, MLOC, MGL, NTXVB, MYID, &
                        & NPROCS, NMAP, CONPCB_S1, CONPCB_S1GL)
                       Call GATHER (1, MTLOC, MLOC, MGL, NTXVB, MYID, &
                        & NPROCS, NMAP, CONPCB_S2, CONPCB_S2GL)

                        IF(BioAcc == .TRUE.) THEN 
                            DO J = 1, NTXVB
                                CALL GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, &
                                        & NPROCS, NMAP, PcbConWSZ(0, 1, J), PcbConWSZ_GL(1, 1, J))
                                CALL GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, &
                                        & NPROCS, NMAP, PcbConWLZ(0, 1, J), PcbConWLZ_GL(1, 1, J))
                            END DO
                        END IF
                  END IF
    !---------------------------------------------------------------------------------------------------------------------------------

                  If (SEDIMENT_CALC) Then
                     Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, &
                    & NMAP, CTEMP, CTEMP_GL)
                     Call GATHER (1, MTLOC, MLOC, MGL, 3, MYID, NPROCS, &
                    & NMAP, CPOP, CPOP_GL)
                     Call GATHER (1, MTLOC, MLOC, MGL, 3, MYID, NPROCS, &
                    & NMAP, CPON, CPON_GL)
                     Call GATHER (1, MTLOC, MLOC, MGL, 3, MYID, NPROCS, &
                    & NMAP, CPOC, CPOC_GL)
                     Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, &
                    & NMAP, CPOS, CPOS_GL)
                     Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, &
                    & NMAP, PO4T2TM1S, PO4T2TM1S_GL)
                     Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, &
                    & NMAP, NH4T2TM1S, NH4T2TM1S_GL)
                     Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, &
                    & NMAP, NO3T2TM1S, NO3T2TM1S_GL)
                     Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, &
                    & NMAP, HST2TM1S, HST2TM1S_GL)
                     Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, &
                    & NMAP, CH4T2TM1S, CH4T2TM1S_GL)
                     Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, &
                    & NMAP, CH41TM1S, CH41TM1S_GL)
                     Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, &
                    & NMAP, SO4T2TM1S, SO4T2TM1S_GL)
                     Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, &
                    & NMAP, SIT2TM1S, SIT2TM1S_GL)
                     Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, &
                    & NMAP, BENSTRTM1S, BENSTRTM1S_GL)
                 !call gather(1,MTLOC,MLOC,MGL,1,MYID,NPROCS,NMAP,SODTM1S, SODTM1S_GL)

                 !LB: save other sediment variables needed for restarts
                     Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, &
                    & NMAP, PO41TM1S, PO41TM1S_GL)
                     Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, &
                    & NMAP, NH41TM1S, NH41TM1S_GL)
                     Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, &
                    & NMAP, NO31TM1S, NO31TM1S_GL)
                     Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, &
                    & NMAP, HS1TM1S, HS1TM1S_GL)
                     Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, &
                    & NMAP, SI1TM1S, SI1TM1S_GL)
                 !
                     If (BALGAE_CALC) Then
                        Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, &
                       & NPROCS, NMAP, BBM, BBM_GL)
                     End If
                 !
                  End If

                  	If (SAV_CALC) Then
                        Call GATHER (1, MTLOC, MLOC, MGL, NSAVM, MYID, &
                       & NPROCS, NMAP, LEAF, LEAF_GL)
					    Call GATHER (1, MTLOC, MLOC, MGL, NSAVM, MYID, &
                       & NPROCS, NMAP, STEM, STEM_GL)
					    Call GATHER (1, MTLOC, MLOC, MGL, NSAVM, MYID, &
                       & NPROCS, NMAP, ROOT, ROOT_GL)
					    Call GATHER (1, MTLOC, MLOC, MGL, NSAVM, MYID, &
                       & NPROCS, NMAP, TUBER, TUBER_GL)
					    Call GATHER (1, MTLOC, MLOC, MGL, NSAVM, MYID, &
                       & NPROCS, NMAP, EP, EP_GL)
                     End If

                  If (DFEEDER) Then
                     Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, &
                    & NMAP, DFEEDM1S, DFEEDM1S_GL)
                  End If
                  If (SFEEDER) Then
                     Call GATHER (1, MTLOC, MLOC, MGL, NSSFP, MYID, &
                    & NPROCS, NMAP, SFEED, SFEED_GL)
                  End If
              !
               Else
#endif
                    !ALLOCATE (C2_GL(MGL,KBM1,NCP))
                    !C2_GL = 0.0  

                    Do J = 1, NCP
                        C2_GL (1:MGL, 1:KBM1, J) = C2 (1:MGL, 1:KBM1, J)
                     End Do
                  If (SEDIMENT_CALC) Then
                     CTEMP_GL (1:MGL) = CTEMP (1:MGL)
                     CPOP_GL (1:MGL, 1:3) = CPOP (1:MGL, 1:3)
                     CPON_GL (1:MGL, 1:3) = CPON (1:MGL, 1:3)
                     CPOC_GL (1:MGL, 1:3) = CPOC (1:MGL, 1:3)
                     CPOS_GL (1:MGL) = CPOS (1:MGL)
                     PO4T2TM1S_GL (1:MGL) = PO4T2TM1S (1:MGL)
                     NH4T2TM1S_GL (1:MGL) = NH4T2TM1S (1:MGL)
                     NO3T2TM1S_GL (1:MGL) = NO3T2TM1S (1:MGL)
                     HST2TM1S_GL (1:MGL) = HST2TM1S (1:MGL)
                     CH4T2TM1S_GL (1:MGL) = CH4T2TM1S (1:MGL)
                     CH41TM1S_GL (1:MGL) = CH41TM1S (1:MGL)
                     SO4T2TM1S_GL (1:MGL) = SO4T2TM1S (1:MGL)
                     SIT2TM1S_GL (1:MGL) = SIT2TM1S (1:MGL)
                     BENSTRTM1S_GL (1:MGL) = BENSTRTM1S (1:MGL)
                    !SODTM1S_GL(1:MGL)     = SODTM1S(1:MGL)
                 !
                 !LB: save other sediment variables needed for restarts
                     PO41TM1S_GL (1:MGL) = PO41TM1S (1:MGL)
                     NH41TM1S_GL (1:MGL) = NH41TM1S (1:MGL)
                     NO31TM1S_GL (1:MGL) = NO31TM1S (1:MGL)
                     HS1TM1S_GL (1:MGL) = HS1TM1S (1:MGL)
                     SI1TM1S_GL (1:MGL) = SI1TM1S (1:MGL)
                 !
                 !changed below for consistency RGL/Kurt
                     If (BALGAE_CALC) Then
                        BBM_GL (1:MGL) = BBM (1:MGL)
                     End If

                  End If

                  	If (SAV_CALC) Then ! AN SAV
                        LEAF_GL (1:MGL, 1:NSAVM) = LEAF (1:MGL, 1:NSAVM)
						STEM_GL (1:MGL, 1:NSAVM) = STEM (1:MGL, 1:NSAVM)
						ROOT_GL (1:MGL, 1:NSAVM) = ROOT (1:MGL, 1:NSAVM)
						TUBER_GL (1:MGL, 1:NSAVM) = TUBER (1:MGL, 1:NSAVM)
						EP_GL (1:MGL, 1:NSAVM) = EP (1:MGL, 1:NSAVM)
                    End If


                  If (DFEEDER) Then
                     DFEEDM1S_GL (1:MGL) = DFEEDM1S (1:MGL)
                  End If
                  If (SFEEDER) Then
                     SFEED_GL (1:MGL, 1:NSPECIES) = SFEED (1:MGL, &
                    & 1:NSPECIES)
                  End If
# if defined (MULTIPROCESSOR)
               End If
#endif
               RSODP = RSODP + 1
               If (MSR) Then
                  LENGTH = INDEX (RSOFN, '.')
                  IJDAY = AINT (JDAY)
                  If (IJDAY < 10) Then
                     Write (EXT1, '(I1)') IJDAY
                     FILENAME = RSOFN (1:LENGTH) // EXT1
                  Else If (IJDAY < 100) Then
                     Write (EXT2, '(I2)') IJDAY
                     FILENAME = RSOFN (1:LENGTH) // EXT2
                  Else If (IJDAY < 1000) Then
                     Write (EXT3, '(I3)') IJDAY
                     FILENAME = RSOFN (1:LENGTH) // EXT3
                  Else
                     Write (EXT4, '(I4)') IJDAY
                     FILENAME = RSOFN (1:LENGTH) // EXT4
                  End If
              !
                  Open (RSO, File=FILENAME, Form='UNFORMATTED')
                  Write (RSO) TITLE
                  Write (RSO) (((C2_GL(I, K, J), I=1, MGL), K=1, KBM1), &
                 & J=1, NCP)
              !
                  If (SEDIMENT_CALC) Then
                 !
                     Write (RSO) (CTEMP_GL(B), B=1, MGL), ((CPOP_GL(B, &
                    & J), B=1, MGL), J=1, 3), ((CPON_GL(B, J), B=1, &
                    & MGL), J=1, 3), ((CPOC_GL(B, J), B=1, MGL), J=1, &
                    & 3), (CPOS_GL(B), B=1, MGL), (PO4T2TM1S_GL(B), &
                    & B=1, MGL), (NH4T2TM1S_GL(B), B=1, MGL), &
                    & (NO3T2TM1S_GL(B), B=1, MGL), (HST2TM1S_GL(B), &
                    & B=1, MGL), (CH4T2TM1S_GL(B), B=1, MGL), &
                    & (CH41TM1S_GL(B), B=1, MGL), (SO4T2TM1S_GL(B), &
                    & B=1, MGL), (SIT2TM1S_GL(B), B=1, MGL), &
                    & (BENSTRTM1S_GL(B), B=1, MGL), (SODTM1S_GL(B), &
                    & B=1, MGL), (PO41TM1S_GL(B), B=1, MGL), &
                    & (NH41TM1S_GL(B), B=1, MGL), (NO31TM1S_GL(B), B=1, &
                    & MGL), (HS1TM1S_GL(B), B=1, MGL), (SI1TM1S_GL(B), &
                    & B=1, MGL)
                 !
                     If (BALGAE_CALC) Then
                        Write (RSO) (BBM_GL(B), B=1, MGL)
                     End If
                  End If

                  	If (SAV_CALC) Then
                        Write (RSO) ((LEAF_GL(B,K), B=1, MGL),K=1,NSAVM), &
					   &((STEM_GL(B,K), B=1, MGL),K=1,NSAVM), &
					   &((ROOT_GL(B,K), B=1, MGL),K=1,NSAVM), &
					   &((TUBER_GL(B,K), B=1, MGL),K=1,NSAVM), &
					   &((EP_GL(B,K), B=1, MGL),K=1,NSAVM)
                     End If !SAV AN

                  If (DFEEDER) WRITE (RSO) (DFEEDM1S_GL(B), B=1, MGL)
                  If (SFEEDER) WRITE (RSO) ((SFEED_GL(B, K), B=1, MGL), &
                 & K=1, NSPECIES)
              !
                  Close (RSO)


               End If
!---------------------------------- finish writing the WQ parameters for restart ---------------------- 
!======================================================================================================
!---------------------------------- Writing tracer concentrations for restart--------------------------
            
            IF(TcrKntOn == .TRUE.) THEN
                IF(MSR) THEN
                    WRITE(FileNum,FMT='(I3.3)')IJDAY
                    TcrWCRestartFile = "outputs/TcrWCRestart"//TRIM(FileNum)//".dat"
                    OPEN(UNIT=888, FILE=TcrWCRestartFile, ACTION = 'WRITE')
                    WRITE(888,'(A)')'Hot start for tracers in water column'
                        DO ii=1,MGL
                            DO jj=1,KBM1
                                WRITE(888,'(<numTcrtVrb>(e10.4,3x))')(C2_GL(ii,jj,kk),kk=numTcrtVrb,NCP)
                            END DO
                        END DO
                    CLOSE(888)
                    TcrSDRestartFile = "outputs/TcrSDRestart"//TRIM(FileNum)//".dat"
                    OPEN(UNIT=889, FILE=TcrSDRestartFile, ACTION = 'WRITE')
                    WRITE(889,'(A)')'Hot start for tracers in water column'
                        DO ii=1,MGL
                                WRITE(889,'(<numTcrtVrb>(e10.4,3x))')(ConTcrSdm1_GL(ii,jj),jj=1,NBTCR)
                        END DO
                        !DO ii=1,MGL
                        !        WRITE(889,'(<numTcrtVrb>(e10.4,3x))')(ConTcrSdm2_GL(ii,jj),jj=1,NBTCR)
                        !END DO
                    CLOSE(889)
                END IF
            END IF

!------------------------------------ Writing toxics concentration for restart--------------------------  

            IF(TxcKntOn == .TRUE.) THEN
                numTxcVrb = NCP - NTXVB - NBTCR + 1
                IF(MSR) THEN
                    WRITE(FileNum,FMT='(I3.3)')IJDAY
                    TxcWCRestartFile = "outputs/TxcWCRestart"//TRIM(FileNum)//".dat"
                    OPEN(UNIT=875, FILE=TxcWCRestartFile, ACTION = 'WRITE')
                    WRITE(875,'(A)')'Hot start for toxics in water column'
                        DO ii=1,MGL
                            DO jj=1,KBM1
                                WRITE(875,'(<numTxcVrb>(e10.4,3x))')(C2_GL(ii,jj,kk),kk=numTxcVrb,NCP)
                            END DO
                        END DO
                    CLOSE(875)

                    TxcSD1RestartFile = "outputs/TxcSD1Restart"//TRIM(FileNum)//".dat"
                    OPEN(UNIT=876, FILE=TxcSD1RestartFile, ACTION = 'WRITE')
                    WRITE(876,'(A)')'Hot start for toxics in water column'
                        DO ii=1,MGL
                                WRITE(876,'(<numTxcVrb>(e10.4,3x))')(CONPCB_S1GL(ii,jj),jj=1,NTXVB)
                        END DO
                    CLOSE(876)

                    TxcSD2RestartFile = "outputs/TxcSD2Restart"//TRIM(FileNum)//".dat"
                    OPEN(UNIT=877, FILE=TxcSD2RestartFile, ACTION = 'WRITE')
                    WRITE(877,'(A)')'Hot start for toxics in water column'
                        DO ii=1,MGL
                                WRITE(877,'(<numTxcVrb>(e10.4,3x))')(CONPCB_S2GL(ii,jj),jj=1,NTXVB)
                        END DO
                    CLOSE(877)

                    IF(BioAcc == .TRUE.) THEN
                        TxcAccmSZRstrFile = "outputs/TxcAccSZRestart"//TRIM(FileNum)//".dat"
                        OPEN(UNIT=900, FILE=TxcAccmSZRstrFile, ACTION = 'WRITE')
                        WRITE(900,'(A)')'Hot start for toxics accumulation in SZ'
                            DO ii=1,MGL
                                DO jj=1,KBM1
                                    WRITE(900,'(<numTxcVrb>(e10.4,3x))')(PcbConWSZ_GL(ii,jj,kk),kk=1,NTXVB)
                                END DO
                            END DO
                        CLOSE(900)

                        TxcAccmLZRstrFile = "outputs/TxcAccLZRestart"//TRIM(FileNum)//".dat"
                        OPEN(UNIT=901, FILE=TxcAccmLZRstrFile, ACTION = 'WRITE')
                        WRITE(901,'(A)')'Hot start for toxics accumulation in LZ'
                            DO ii=1,MGL
                                DO jj=1,KBM1
                                    WRITE(901,'(<numTxcVrb>(e10.4,3x))')(PcbConWLZ_GL(ii,jj,kk),kk=1,NTXVB)
                                END DO
                            END DO
                        CLOSE(901)
                    END IF
                END IF
            END IF

!----------------------------------------------------------------------------------------------------
            End If   ! RSOD points
        End If !END RESTART_OUT

        !IF(ALLOCATED(C2_GL)) DEALLOCATE(C2_GL)
        IF(ALLOCATED(ConTcrSdm1_GL)) DEALLOCATE(ConTcrSdm1_GL)
        !IF(ALLOCATED(ConTcrSdm2_GL)) DEALLOCATE(ConTcrSdm2_GL)
        IF(ALLOCATED(CONPCB_S1GL)) DEALLOCATE(CONPCB_S1GL)
        IF(ALLOCATED(CONPCB_S2GL)) DEALLOCATE(CONPCB_S2GL)
        IF(ALLOCATED(PcbConWSZ_GL)) DEALLOCATE(PcbConWSZ_GL)
        IF(ALLOCATED(PcbConWLZ_GL)) DEALLOCATE(PcbConWLZ_GL)
!-----------------------------------------------------------------------------------------------------
         !IF(MSR) THEN
        !    IF(MOD(NIT,3600) == 0.0) THEN
        !         VTK_FLNUM = VTK_FLNUM + 1
        !         CALL DILU_OUTPUT(VTK_FLNUM,1)
        !         !CALL OUTPUT_SLD(JDAY)
        !     END IF
        ! END IF
!-----------------------------------------------------------------------------------------------------

 End Do !END DO WHILE(.NOT.END_RUN)

  !************************************************************************
  !**                          End Simulation                            **
  !************************************************************************

  IF (MSR) THEN
    CLOSE(975)
  END IF

  !WLong, close station and history outputs if open
  !
      If (MSR) Then
         If (OPEN_HIS) Then
            Select Case (HISFN_SPLIT_BYLEVEL)
            Case (.True.)
               Do K = 1, KBM1
                  Close (UNIT_HIS+K)
               End Do
            Case (.False.)
               Close (UNIT_HIS)
            End Select
         End If
         If (OPEN_STN) CLOSE (UNIT_STN)
      End If
  !
  !
  !***** Write results in binary form for use as initial conditions*****
  !
      If (ICOND_OUT) Then
     !
     !***** Integrate sediments G3 POM to steady state     			!MNOEL 2-5-93
     !
         If (STEADY_STATE_SED_G3) Call SED_DIAGENESIS_G3
     !
# if defined (KILLTREES)
     !
     ! KURT GLAESEMANN - only print to MSR
     !            If (MSR) Then
     !               Open (ICO, File=ICOFN, Form='UNFORMATTED')
     !               Write (ICO) TITLE
     !               Write (ICO) (((C1(I, K, J), I=0, MLOC), K=1, KBM1), J=1, &
     !              & NCP)!Wen Long, here we need to read global arrays!
     !!
     !               If (SEDIMENT_CALC) WRITE (ICO) (CTEMP(B), B=1, MLOC), &
     !              & ((CPOP(B, J), B=1, MLOC), J=1, 3), ((CPON(B, J), B=1, &
     !              & MLOC), J=1, 3), ((CPOC(B, J), B=1, MLOC), J=1, 3), &
     !              & (CPOS(B), B=1, MLOC), (PO4T2TM1S(B), B=1, MLOC), &
     !              & (NH4T2TM1S(B), B=1, MLOC), (NO3T2TM1S(B), B=1, MLOC), &
     !              & (HST2TM1S(B), B=1, MLOC), (CH4T2TM1S(B), B=1, MLOC), &
     !              & (CH41TM1S(B), B=1, MLOC), (SO4T2TM1S(B), B=1, MLOC), &
     !              & (SIT2TM1S(B), B=1, MLOC), (BENSTRTM1S(B), B=1, MLOC), &
     !              & (BBM(B), B=1, MLOC)
     !!
     !               If (DFEEDER) WRITE (ICO) (DFEEDM1S(B), B=1, MLOC)
     !               If (SFEEDER) WRITE (ICO) ((SFEED(B, K), B=1, MLOC), K=1, &
     !              & NSPECIES)
     !!
     !!
     !               Close (ICO)
     !            End If
# endif
      End If
!
#if defined (KILLTREES)
  !
  !***** Snapshot FORMAT statements
  !
3000  Format ('1')
3010  Format (1 X, A72)
3020  Format (/ / 1 X, 'Time related variables' / '+', 22 ('_') // 3 X, &
     & 'Julian day', T25, '=', F8.2 / 3 X, 'Elapsed time', T25, '=', &
     & I8, ' days ', F6.2, ' hours' / 3 X, 'Number of iterations', T25, &
     & '=', I8 / 3 X, 'Timestep', T25, '=', F8.0, ' sec' / 3 X, 'Averag&
     &e time step', T25, '=', F8.0)
3025  Format (/ / 1 X, 'After steady-state sediment computation' / '+', &
     & 22 ('_') // 3 X, 'Julian day', T25, '=', F8.2 / 3 X, 'Elapsed ti&
     &me', T25, '=', I8, ' days ', F6.2, ' hours' / 3 X, 'Number of ite&
     &rations', T25, '=', I8 / 3 X, 'Timestep', T25, '=', F8.0, ' sec' &
     & / 3 X, 'Average time step', T25, '=', F8.0)
3030  Format (/ 1 X, A24 / 10 (/ 10 F8.4))
3032  Format (/ 1 X, A20 / 10 (/ 10 F8.3))
3034  Format (/ 1 X, A20 / 10 (/ 10 (1 PE8.2E1)))
3035  Format (/ 1 X, 'G', I1, 1 X, A20 / 10 (/ 10 (1 PE8.2E1)))
3036  Format (/ 1 X, 'N', I1, 1 X, A20 / 10 (/ 10 (1 PE8.2E1)))
  !
  !***** Diagnostic FORMAT statements
  !
4000  Format (/ 1 X, 79 ('*') // 1 X, 'Date         =', I8, '  days ', &
     & F5.2, ' hours' / 1 X, '# iterations =', I8)
4010  Format (1 X, 'Timestep', T15, '=', I8, 2 X, 'Average timestep', &
     & T43, '=', I8/1 X, 'Courant #', T15, '=', F8.3/1 X, 'at face', &
     & T15, '=', I8, 2 X, 'and box', T43, '=', I8 /)
4012  Format (1 X, 'Timestep', T15, '=', I8, 2 X, 'Average timestep', &
     & T43, '=', I8/1 X, 'Diffusion #', T15, '=', F8.3/1 X, 'at face', &
     & T15, '=', I8, 2 X, 'and box', T43, '=', I8 /)
4020  Format (/ 1 X, A14, ' performed at ', I6, ' days and ', F5.2, ' h&
     &ours')
4030  Format (5 X, 'V2(', I4, ') =', 1 PE15.6E1, ' m*3', 3 X, 'HMV(', &
     & I4, ') =', 1 PE15.6E1, ' m*3', 3 X, 'Difference =', 1 PE15.6E1, &
     & ' m*3')
4040  Format (/ 3 X, 'Water quality model volume      =', 1 PE20.12E2, &
     & ' m*3' / 3 X, 'Difference in total grid volume =', 1 PE20.3E2, '&
     & %' / 3 X, 'Total difference in volume      =', 1 PE20.3E2, ' m*3&
     &')
4050  Format (/ 1 X, A22 // (:3 X, A24, T31, '=', 1 PE16.8E2, ' kg'))
4060  Format (/ 1 X, 'Water column mass balance error' // 3 X, 'Nitroge&
     &n   = ', 1 PE10.3E2, ' %' / 3 X, 'Phosphorus = ', 1 PE10.3E2, ' %&
     &' / 3 X, 'Carbon     = ', 1 PE10.3E2, ' %' // 1 X, 'Sediment mass&
     & balance error' // 3 X, 'Nitrogen   = ', 1 PE10.3E2, ' %' / 3 X, &
     & 'Phosphorus = ', 1 PE10.3E2, ' %' / 3 X, 'Carbon     = ', 1 &
     & PE10.3E2, ' %' /)
4070  Format (9 F8.2)
4080  Format (8 X, A24)
  !
  !***** Mass balance FORMATs
  !
5000  Format (10(1 X, 1 PE10.2E2))
#endif
  !
      Call WQM_DEALLOC !WLong repaced the above section by WQM_DEALLOC
  !
      Call ZOOP_DEALLOC !WLong replaced the above by ZOOP_DEALLOC()
  !
      If (LIGHT_EXTINCTION) Call OWQ_DEALLOC !WLong replace the above with OWQ_DEALLOC
      If (SAV_CALC) Then
         Call SAV_DEALLOC
      End If
  !Wen Long: deallocate SAV fluxes to water column if SAV_LOADS is on
      If (SAV_LOADS) Then
         Call SAV_LOADS_DEALLOC
      End If
      If (SFEEDER) Then
         Call SF_DEALLOC
      End If
      Call WQMINIT_DEALLOC    !WLong  replaced the above by WQMINIT_DEALLOC()
      Call HYDRO_GEOM_DEALLOC !WLong replaced the above by HYDRO_GEOM_DEALLOC()
      Call OBCS_DEALLOC
      Call HYDRO_DEALLOC
      Call BCS_DEALLOC
      Call BCMAP_DEALLOC
      Call TGE_DEALLOC

#if defined (WET_DRY)
	  Call WD_DEALLOC
#endif
!
  !WLong moved to mod_tge.F and remove NBE usage in wqm_main.F here
  !should move to mod_hydrovars.F
#if defined (MULTIPROCESSOR)
      If (PAR) Then
         Do I = 1, NPROCS
            If (ALLOCATED(EMAP(I)%LOC_2_GL)) DEALLOCATE &
           & (EMAP(I)%LOC_2_GL)
            If (ALLOCATED(NMAP(I)%LOC_2_GL)) DEALLOCATE &
           & (NMAP(I)%LOC_2_GL)
            If (ALLOCATED(EC(I)%RCVP)) DEALLOCATE (EC(I)%RCVP)
            If (ALLOCATED(EC(I)%SNDP)) DEALLOCATE (EC(I)%SNDP)
            If (ALLOCATED(BNC(I)%RCVP)) DEALLOCATE (BNC(I)%RCVP)
            If (ALLOCATED(BNC(I)%SNDP)) DEALLOCATE (BNC(I)%SNDP)
            If (ALLOCATED(BNC(I)%MLTP)) DEALLOCATE (BNC(I)%MLTP)
            If (ALLOCATED(NC(I)%RCVP)) DEALLOCATE (NC(I)%RCVP)
            If (ALLOCATED(NC(I)%SNDP)) DEALLOCATE (NC(I)%SNDP)
         End Do
      End If
!
      Call PAR_DEALLOC
#endif
  !
      If (SEDIMENT_CALC) Then
     !Wen Long deallocating sediment diagenesis related variables
         Call SED_DEALLOC
     !Wen Long deallocating benthic algae related variables
         If (BALGAE_CALC) Call BA_DEALLOC
         If (SAV_CALC) Call SED_SAV_EXCHANGE_DEALLOC
     !WLong deallocating deposition feeder related variables
         If (DFEEDER) Then
            Call DF_DEALLOC
            Call SED_DF_EXCHANGE_DEALLOC
         End If
         If (SEDIMENT_CALC) Then
            Call SED_SF_EXCHANGE_DEALLOC
         End If
      End If
  !
  !Wen Long, report model time
  !
      If (MSR) Then
         TOTAL_TIME = TOTAL_TIME + DTIME (ELAPSED)!CALL DTIME() and return user time in ELAPSED(1), system time in ELAPSED(2)
         WTIME_MOD = WTIME_MOD + ELAPSED (1)
         WTIME_SYS = WTIME_SYS + ELAPSED (2)
         Write (*, '(A22,EN15.3,A8)') "Time used by model   =", &
        & WTIME_MOD, " seconds"
         Write (*, '(A22,EN15.3,A8)') "Time used by system  =", &
        & WTIME_SYS, " seconds"
         Write (*, '(A22,EN15.3,A8)') "Total time used      =", &
        & TOTAL_TIME, " seconds"
      End If

      CALL PSTOP     
!#if defined (MULTIPROCESSOR)
!      Call MPI_FINALIZE (IERR)
!#endif

End Program FVCOMICM
!
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
!
Subroutine EXCHANGE_WQM (CCC)
#if defined (MULTIPROCESSOR)
  !==============================================================================!
  !     PERFORM DATA EXCHANGE FOR WATER QUALITY VARIABLES                        |
  !==============================================================================!
      Use MOD_PREC, Only: SP
  !
      Use MOD_PAR, Only: NBN, BN_LOC, BN_MLT, NC, BNC, NODE_MATCH_ALL, &
     & EXCHANGE_ALL
      Use MOD_SIZES, Only: NCP
  !
      Use MOD_LIMS, Only: NLOC, KBM1, MYID, NPROCS, MTLOC
  !
  !Wen Long took MOD_CONTROL out of MOD_HYDROVARS and put the used variables here
      Use MOD_CONTROL, Only: MSR, PAR
  !
      Implicit None
      Real (SP), Intent (Inout) :: CCC (0:MTLOC, KBM1, NCP)
      Integer :: NJ, JCON, NJ1, NJ2, NJ3, EXTRA
  !
      If (PAR) Then
     ! KURT GLAESEMANN 16 SEPT 2009 ONLY LOOP OVER USED VARIABLES
     !    DO NJ = 1,NCP
     !    DO JCON=1,NAC
     !      NJ = AC(JCON)
     !    KURT GLAESEMANN - fix dimensions
     !      CALL EXCHANGE(NC,MTLOC,KB,MYID,NPROCS,C2F(:,:,NJ))
     !      CALL EXCHANGE(NC,MTLOC,KBM1,MYID,NPROCS,C2F(:,:,NJ))
     !    ENDDO
     ! KURT GLAESEMANN 17 SEPT 2009 DO MULTIPLE VARIABLES AT ONCE

     !
#if(0)
         EXTRA = Mod (NAC, 3)
     !IF(MSR)write(*,*)'LBnote: extra=',EXTRA
         If (EXTRA .Eq. 1) Then
            NJ1 = AC (1)
            Call EXCHANGE (NC, MTLOC, KBM1, MYID, NPROCS, CCC(:, :, &
           & NJ1))
         Else If (EXTRA .Eq. 2) Then
            NJ1 = AC (1)
            NJ2 = AC (2)
            Call EXCHANGE (NC, MTLOC, KBM1, MYID, NPROCS, CCC(:, :, &
           & NJ1), CCC(:, :, NJ2))
         End If

         Do JCON = EXTRA + 1, NAC - 2, 3
            NJ1 = AC (JCON+0)
            NJ2 = AC (JCON+1)
            NJ3 = AC (JCON+2)
            Call EXCHANGE (NC, MTLOC, KBM1, MYID, NPROCS, CCC(:, :, &
           & NJ1), CCC(:, :, NJ2), CCC(:, :, NJ3))
         End Do

#else
     !  KURT GLAESEMANN 22 SEPT 2009 - DO ALL AT ONCE
         Call EXCHANGE_ALL (NC, MTLOC, KBM1, NCP, MYID, NPROCS, CCC)

#endif
         Call NODE_MATCH_ALL (1, NBN, BN_MLT, BN_LOC, BNC, MTLOC, KBM1, &
        & NCP, MYID, NPROCS, CCC)
      End If

#else
	  ! KURT GLAESEMANN 6 MAY 2016.  Make debug happy in serial
      Use MOD_PREC, Only: SP
      Use MOD_LIMS, Only: MTLOC, KBM1
      Use MOD_SIZES, Only: NCP
      Implicit None
      Real (SP) :: CCC (0:MTLOC, KBM1, NCP)
#endif
      Return
End Subroutine EXCHANGE_WQM
!==============================================================================!
!

!--------------- Outputting data -----------------------------------------------
!-------------------------------------------------------------------------------
Subroutine WQM_OUTPUT (NTHIS_OLD, NTSTN_OLD, NTHIS, NTSTN, CTR_HIS, &
& CTR_STN)
  !
  !subroutine to write history and station outputs
  !
  !
      Use MOD_PREC, Only: SP
      Use MOD_SIZES, Only: NCP, NSSFP, NSAVM, NTXVB
      Use MOD_LIMS, Only: MYID, NPROCS, IINT, NTLOC, MLOC
  !
      Use MOD_TGE, Only: NV, TGE_DEALLOC
  !
      Use MOD_HYDROVARS, Only: PI, ART1, Z, ZZ,ZZ2D, DZ, DZ2D, DZZ, H, D, DT, DT1, &
     & EL, ET, DTFA, UU, VV, UUT, VVT, WTST, UARD_OBCNT, XFLUX_OBCT, &
     & DTFAT, WTS, UARD_OBCN, XFLUX_OBC, KH, VISCOFH, UNC1, VNC1, WNC1, &
     & WTSNC1, UARD_OBCNNC1, XFLUX_OBCNC1, DTFANC1, KHNC1, TNC1, SNC1, &
     & ELNC1, UNC2, VNC2, WNC2, WTSNC2, UARD_OBCNNC2, XFLUX_OBCNC2, &
     & DTFANC2, KHNC2, TNC2, SNC2, ELNC2, THOUR, t_his_start, &
     & t_his_end, t_his_dlt, Nstation, NstationNum_GL, t_stn_start, &
     & t_stn_end, t_stn_dlt, STNFN, HISFN, HISFN_PREFIX, HISFN_EXT, &
     & HISFN_FINAL, HISFN_SPLIT_BYLEVEL, HYDRO_GEOM_DEALLOC, &
     & HYDRO_DEALLOC
  !
      Use MOD_BCMAP, Only: BCMAP_DEALLOC
  !
  !Wen Long took MOD_CONTROL out of MOD_HYDROVARS and put the used variables here
      Use MOD_CONTROL, Only: SERIAL, MSR, PAR, CASENAME
      Use MOD_FILEINFO, Only: DIA, BFO, KFL, RSO, SNP, PLT, APL, TFL, &
     & OPL, SFO, ICO, MBL, UNIT_STN, UNIT_HIS, CNAME, INIT_FILE_INFO
!
      Use MOD_SF, Only: SFEEDER, NSPECIES, SFEED, SFEED_GL, SUSPFEED, &
     & SF_DEALLOC
!
      Use MOD_WQM, Only: JDAY, DLT, AHMDLT, DLTAV, ELTMS, ELTMS1, &
     & ELTMJD, SEDIMENT_CALC, LIGHT_EXTINCTION, T, C2, SALT, SSI, B1, &
     & B2, B3, LDOC, RDOC, LPOC, RPOC, NH4, NO3, UREA, LDON, RDON, &
     & LPON, RPON, PO4, LDOP, RDOP, LPOP, RPOP, PIP, COD, DOXG, SIUPB, &
     & SIAT, PIB1, PIB2, PIB3, Q1, Q2, Q3, BENTHIC_FLUXES, DSSR, DTC, &
     & FD, FLOW, I0, IT, LIGHT_EXTINCTION, RELTMS, SAV_CALC, SAVENT, &
     & SEDENT, TODS, TTSS, Z_DIFFUSION, AC, ANC1, ANC2, ANC3, ASC1, &
     & ASC2, ASC3, C1, C2F, CMASS, DTM, FLUXS, FNDLT, KBM1, &
     & MASS_BALANCE, NAC, NIT, SFENT, SOLIDS_CALC, TMEND, TMSTRT, WCMC, &
     & WCMN, WCMP, WCMS, XYDF, XY_DIFFUSION, ATMFLXN, ATMFLXNB, &
     & ATMFLXP, ATMFLXPB, ATMLDON, ATMLDOP, ATMNH4, ATMNO3, ATMPO4, &
     & ATMRDON, ATMRDOP, BENFLXDN, BENFLXPN, BENFLXPNB, DLWCMC, DLWCMN, &
     & DLWCMP, IWCMC, IWCMN, IWCMP, PRECIP, SFATOT, ANDC, BENFLXDNB, &
     & BENFLXDP, BENFLXDPB, BENFLXPC, BENFLXPCB, BENFLXPP, BENFLXPPB, &
     & BENNH4, BENNO3, BENSA, BENPO4, BNDN, DENIT, DLALGC, DLWCKMC, &
     & DLWCKMCB, DLWCKMN, DLWCKMNB, DTMP1, FLXTLDON, FLXTLPON, FLXTNH4, &
     & FLXTNO3, FLXTRDON, INFLOW, JCB, MNLLDOC, MNLRDOC, PCFWS, PPFWS, &
     & RESPC, BNDC, BNDFLXC, BNDFLXN, BNDFLXP, BNDP, DLWCSSMN, &
     & DLWCSSMP, FLXT1, FLXT2, FLXT3, FLXTLDOC, FLXTLDOP, FLXTLPOC, &
     & FLXTLPOP, FLXTLZ, FLXTPIB1, FLXTPIB2, FLXTPIB3, FLXTPO4, &
     & FLXTRDOC, FLXTRDOP, FLXTRPOC, FLXTRPON, FLXTRPOP, FLXTSZ, &
     & S1FLXN, S1FLXP, S2FLXN, S3FLXN, BENDEN, BURIALFLXC, BURIALFLXCB, &
     & BURIALFLXN, BURIALFLXNB, BURIALFLXP, BURIALFLXPB, DFTMP, &
     & DLSEDKC, DLSEDKCB, DLSEDKN, DLSEDKNB, DLSEDMC, DLSEDMN, DLSEDMP, &
     & DLSEDSSMC, DLSEDSSMN, DLSEDSSMP, DLWCSSMC, DTMP2, ERRSEDN, &
     & ERRWCC, ERRWCN, ERRWCP, S1FLXC, S2FLXC, S2FLXP, S3FLXC, S3FLXP, &
     & C2_GL, CCHL1, CCHL1_GL, D_GL, EL_GL, ERRSEDC, ERRSEDP, H_GL, &
     & MGL, MTLOC, P1, S_GL, T_GL, B1SZ_GL, BM1, BM1_GL, BM2, BM2_GL, &
     & FI1, FI1_GL, FI2, FI2_GL, NL1, NL1_GL, NL2, NL2_GL, P1_GL, P2, &
     & P2_GL, PL1, PL1_GL, PL2, PL2_GL, B1LZ_GL, B2LZ_GL, B2SZ_GL, &
     & DCOD, DCOD_GL, DDOC, DDOC_GL, IAVG_GL, IK1, IK1_GL, IK2, IK2_GL, &
     & NITRIF, NITRIF_GL, PR1, PR1_GL, PR2, PR2_GL, GPP_GL, NPP_GL, &
     & GPP, NPP, total_netPP, total_netPP_GL, BFLUX, BFLUX_GL, COD_GL, &
     & COURMX, DIAGNOSTICS, DOLZ_GL, DOPR, DOPR_GL, DORALG, DORALG_GL, &
     & DOSZ_GL, FTCOD, FTCOD_GL, REAERDO, REAERDO_GL, RESP, RESP_GL, &
     & PNFWS, CTEMP, DIFFMX, B, BB, IJDAY, LENGTH, SAV_LOADS, &
     & WQM_DEALLOC, TDIC, TALK, pH, pCO2, pH_GL, pCO2_GL, ADVFLUX, &
     & ADVFLUX_GL, VDIFFLUX, VDIFFLUX_GL, &
	 & TSS_GL, SSI_GL, TURB_GL, KESS_GL, PARAD_GL, LEAF_GL, STEM_GL, &
	 & ROOT_GL, TUBER_GL, EP_GL, TTL_MASS, TTL_MASS_GL, TTL_MASS2, TTL_MASS2_GL, &
   & TTL_MASS3, TTL_MASS3_GL, TxcKntOn
	 !LB added ADV,VDIFF FLUXES !Adi-2/13/18 added TSS_GL, SSI_GL, TURB_GL, KESS_GL
  !
      Use MOD_OWQ, Only: INTKE, NXKEI, IAVG, KESS, OWQ_DEALLOC, &
     & LGHT_ATTN, TSS, TURB, PARAD !Adi-2/13/18, Added TSS, TURB
     ! Use MOD_OWQ, Only: INTKE, NXKEI, IAVG, KESS, OWQ_DEALLOC, &
     !& LGHT_ATTN
  !
      Use MOD_WQMINIT, Only: RSOD, RSODP, RSOFN, DLTD, DLTDP, DIAD, &
     & DIADP, DIAF, COURFS, COURBS, F, SB, DIFFFS, DIFFBS, NXTVD, &
     & NXDIA, MXDLT, TM1, TM2, EXT1, EXT2, EXT3, EXT4, TITLE, FILENAME, &
     & RESTART_OUT, END_RUN, VOLUME_BALANCE, ICOND_OUT, AUTO_STEPPING, &
     & NEW_VOLUMES, ALGAE_CALC, ZOO_CALC, CARBON_CALC, NITROGEN_CALC, &
     & PHOSPHORUS_CALC, COD_CALC, OXYGEN_CALC, SILICA_CALC, LEFT_FLOWB, &
     & IFLOWP, C1MIN, C1MAX, DOVDAYS, ACC, DLTVAL, DLTMAX, DLTFTN, &
     & WQMINIT_DEALLOC
  !
      Use MOD_ALGAL, Only: ALGAE
  !
      Use MOD_ZOOP, Only: ANCSZ, ANCLZ, APCSZ, APCLZ, CLSZ, CLLZ, B1SZ, &
     & B2SZ, B1LZ, B2LZ, DOSZ, DOLZ, ACLSZ, ACLLZ, ARSZ, ARLZ, ABMSZ, &
     & ABMLZ, AMSZ, AMLZ, APRSZLZ, AGSZ, AGLZ, ADOCSZ, APOCSZ, ADOCLZ, &
     & APOCLZ, ANH4SZ, ADONSZ, APONSZ, ANH4LZ, ADONLZ, APONLZ, APO4SZ, &
     & ADOPSZ, APOPSZ, APO4LZ, ADOPLZ, APOPLZ, APRSZ, APRLZ, APISZ, &
     & APILZ, AB1SZ, AB2SZ, AB3SZ, AB1LZ, AB2LZ, AB3LZ, ADOSZ, ADOLZ, &
     & ASASZ, ASUSZ, ASALZ, ASULZ, SZ, LZ, ZOOP_DEALLOC
!
      Use MOD_KIN, Only: TEMPER, SOLIDS, ZOOPL, CARBON, NITROG, PHOSPH, &
     & CODMND, OXYGEN, SILICA, BEN_FLUX
  !
      Use MOD_SED, Only: STEADY_STATE_SED_G3, STEADY_STATE_SED_IC, &
     & DLTS, IERR, ISEDMN, ISEDMP, ISEDMC, SEDMN, SEDMP, SEDMC, &
     & BENSTRTM1S, SODTM1S, JNH4TM1S, JNO3TM1S, JPO4TM1S, JCH4TM1S, &
     & JCH4GTM1S, JHSTM1S, JSITM1S, NH41TM1S, NH42TM1S, NH4T2TM1S, &
     & NO31TM1S, NO32TM1S, NO3T2TM1S, HS1TM1S, HS2TM1S, HST2TM1S, &
     & SI1TM1S, SI2TM1S, SIT2TM1S, PO41TM1S, PO42TM1S, PO4T2TM1S, &
     & CH41TM1S, CH42TM1S, CH4T2TM1S, SO4T2TM1S, BURIALC, BURIALN, &
     & BURIALP, DIAGENC, CTEMP_GL, CPOP_GL, CPON_GL, CPOC_GL, CPOS_GL, &
     & PO4T2TM1S_GL, NH4T2TM1S_GL, NO3T2TM1S_GL, HST2TM1S_GL, &
     & CH4T2TM1S_GL, CH41TM1S_GL, SO4T2TM1S_GL, SIT2TM1S_GL, &
     & BENSTRTM1S_GL, SODTM1S_GL, PO41TM1S_GL, NH41TM1S_GL, &
     & NO31TM1S_GL, HS1TM1S_GL, SI1TM1S_GL, BENDEN_GL, JPOC_GL, &
     & JPON_GL, JPOP_GL, JPOS_GL, CH40_GL, JNH4_GL, JNO3_GL, JCH4_GL, &
     & JCH4G_GL, JHS_GL, JPO4_GL, JSI_GL, NH41_GL, NH42_GL, NO31_GL, &
     & NO32_GL, PO41_GL, PO42_GL, SI1_GL, SI2_GL, CH42_GL, HS1_GL, &
     & HS2_GL, HSED1_GL, HSED1, JPOC, JPON, JPOP, JPOS, CPOC, CPON, &
     & CPOP, CPOS, CPO4, CNH4, CNO3, CHS, CSI, DIAGN, SED_INIT2, &
     & SED_DEALLOC, SED_CALC, SED_DIAGENESIS_G3, HSED, HSED_GL
!
      Use MOD_SED_DF_EXCHANGE_VARS, Only: SED_DF_EXCHANGE_ALLOC, &
     & SED_DF_EXCHANGE_DEALLOC
  !
      Use MOD_SED_SF_EXCHANGE_VARS, Only: SED_SF_EXCHANGE_ALLOC, &
     & SED_SF_EXCHANGE_DEALLOC
  !
      Use MOD_SED_SAV_EXCHANGE_VARS, Only: SED_SAV_EXCHANGE_ALLOC, &
     & SED_SAV_EXCHANGE_DEALLOC
  !
      Use MOD_BA, Only: BALGAE_CALC, BBM, BBM_GL, BA_DEALLOC
  !
      Use MOD_OBCS, Only: SETUP_OBC, OBCS_DEALLOC
  !
      Use MOD_BCS, Only: BCS_DEALLOC
  !
      Use MOD_SAV, Only: SAV_DEALLOC, SAV_LOADS_DEALLOC, SAV_COMP, &
	 & LEAF, STEM, ROOT, TUBER, EP
  !
#  if defined (MULTIPROCESSOR)
      Use MOD_PAR, Only: EC, NC, BNC, EMAP, NMAP, NODE_MATCH_ALL, &
     & EXCHANGE_ALL, GATHER, INIT_MPI_ENV, PAR_DEALLOC
#  endif
  !
      Use MOD_FILENAMES, Only: FNAMEPREFIX, FNAMEEXT !, ifindext
  !
#  if defined (MULTIPROCESSOR)
      Use MOD_BUFFERS, Only: RCVBUF, SNDBUF
#  endif
  !
      Use MOD_DF, Only: DFEEDER, DFEEDM1S_GL, DFEEDM1S, DF_DEALLOC
!
      Use MOD_CO2SYS, Only: ALKNH4, ALKNO3, ALKNIT, ALKDEN, ALKREM, &
     & ALKNH4SED, ALKNO3SED, DICUPT, DICBMP, DICPRD, DICMNL, DICDEN, &
     & DICGAS, DICSED, ALKNH4_GL, ALKNO3_GL, ALKNIT_GL, ALKDEN_GL, &
     & ALKREM_GL, ALKNH4SED_GL, ALKNO3SED_GL, DICUPT_GL, DICBMP_GL, &
     & DICPRD_GL, DICMNL_GL, DICDEN_GL, DICGAS_GL, DICSED_GL !DICNIT, DICNIT_GL,

     USE TXC_KNT, ONLY: SSD_MSS, SSIWC_GL, SSDL_GL, PSRT1, PSRT1_GL, CONPCB_S1, CONPCB_W, &
                        & CONPCB_S1GL, CONPCB_S2, CONPCB_S2GL, CONPCB_WGL, PSRT2, PSRT2_GL, &
                        & PcbCon_W, PcbCon_AlgW, PcbCon_ZplW, PcbCon_SldW, PcbCon_POCW, &
                        & PcbConW_S1, PcbCon_POCS1, PcbCon_SldS1, PcbConW_S2, PcbCon_POCS2, &
                        & PcbCon_SldS2,PcbCon_AlgW1,PcbCon_AlgW2
      Implicit None
# if defined (MULTIPROCESSOR)
      Include "mpif.h"
# endif
  !
      Integer :: I, K, J, VTK_FLNUM
  !
      Character (Len=1024) :: TITLE_STN
      Logical :: OPEN_HIS, PRINT_HIS
      Logical :: OPEN_STN, PRINT_STN
      Integer (4) :: CTR_HIS, CTR_STN !record counter for station and history outputs
  !
      Integer (4) :: NTSTN_OLD, NTSTN
      Integer (4) :: NTHIS_OLD, NTHIS
  !
  !   CHARACTER(1024):: fnameprefix, fnameext
      Character (1024) :: HISFN_FILENUMBER
  !
      Real (SP) :: JDAY_TMP
      Real (SP) :: auxLBdt
  !
      NTHIS = FLOOR ((JDAY-t_his_start)/t_his_dlt)
      NTSTN = FLOOR ((JDAY-t_stn_start)/t_stn_dlt)


  !
      If (((NTHIS > NTHIS_OLD) .And. (JDAY >= t_his_start) .And. (JDAY &
     & <= t_his_end)) .Or. ((NTSTN > NTSTN_OLD) .And. (JDAY >= &
     & t_stn_start) .And. (JDAY <= t_stn_end))) Then
     !
# if defined (MULTIPROCESSOR)
     !
         If (PAR) Then


			! We are doing all K for some variables, K=1 for others

            Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & CCHL1(1, 1), CCHL1_GL(1, 1))

            Call GATHER (0, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
            & total_netPP(0), total_netPP_GL(1))

			!call gather(1,MTLOC,MLOC,MGL,1,   MYID,NPROCS,NMAP,total_netPP(1),total_netPP_GL(1))  !LB

            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & C2(0, 1, 27), C2_GL(1, 1, 27))
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & C2(0, 1, 9), C2_GL(1, 1, 9))
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & C2(0, 1, 10), C2_GL(1, 1, 10))
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & C2(0, 1, 11), C2_GL(1, 1, 11))
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & C2(0, 1, 12), C2_GL(1, 1, 12))
		  If (SOLIDS_CALC) Then
			Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & C2(0, 1, 3), C2_GL(1, 1, 3))  ! AN for SSI
		  End If
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & C2(0, 1, 4), C2_GL(1, 1, 4))
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & C2(0, 1, 5), C2_GL(1, 1, 5))
		  If (ZOO_CALC) Then !AN ZOO
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & C2(0, 1, 7), C2_GL(1, 1, 7))
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & C2(0, 1, 8), C2_GL(1, 1, 8))
          End If
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & C2(0, 1, 13), C2_GL(1, 1, 13))
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & C2(0, 1, 14), C2_GL(1, 1, 14))
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & C2(0, 1, 20), C2_GL(1, 1, 20))

            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & T(0, 1), T_GL(1, 1))!Wen Long, correction, pass all layers
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & SALT(0, 1), S_GL(1, 1))!Wen Long, correction, pass all layers
            Call GATHER (0, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
           & D(0), D_GL(1))!Wen Long, add D_GL for total depth
            Call GATHER (0, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
           & EL(0), EL_GL(1))!Wen Long, add EL_GL for surface elevation
            Call GATHER (0, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
           & H(0), H_GL(1))!Wen Long, add H_GL for still water depth


            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & C2(0, 1, 33), C2_GL(1, 1, 33))!LB
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & C2(0, 1, 34), C2_GL(1, 1, 34))!LB
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & pH(0, 1), pH_GL(1, 1))!LB
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & pCO2(0, 1), pCO2_GL(1, 1))!LB

        !LB adding LDON, RDON, LPON, RPON  23jun2016
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & C2(0, 1, 16), C2_GL(1, 1, 16))
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & C2(0, 1, 17), C2_GL(1, 1, 17))
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & C2(0, 1, 18), C2_GL(1, 1, 18))
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & C2(0, 1, 19), C2_GL(1, 1, 19))

          !------------------------------------------------------------------------------------


        ! Wen Long added the following for debugging algae
        !
            Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & P1(1, 1), P1_GL(1, 1))
            Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & P2(1, 1), P2_GL(1, 1))
            Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & BM1(1, 1), BM1_GL(1, 1))
            Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & BM2(1, 1), BM2_GL(1, 1))
            Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & NL1(1, 1), NL1_GL(1, 1))
            Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & NL2(1, 1), NL2_GL(1, 1))
            Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & PL1(1, 1), PL1_GL(1, 1))
            Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & PL2(1, 1), PL2_GL(1, 1))
            Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & B1SZ(1, 1), B1SZ_GL(1, 1))
            Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
          & B2SZ(1, 1), B2SZ_GL(1, 1))
            Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & B1LZ(1, 1), B1LZ_GL(1, 1))
            Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & B2LZ(1, 1), B2LZ_GL(1, 1))
            Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & PR1(1, 1), PR1_GL(1, 1))
            Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & PR2(1, 1), PR2_GL(1, 1))

            If (LIGHT_EXTINCTION) Then
               Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, &
              & NMAP, IAVG(1, 1), IAVG_GL(1, 1))
               Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, &
              & NMAP, FI1(1, 1), FI1_GL(1, 1))
               Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, &
              & NMAP, FI2(1, 1), FI2_GL(1, 1))
            End If


            Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & IK1(1, 1), IK1_GL(1, 1))
            Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & IK2(1, 1), IK2_GL(1, 1))

            Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & GPP(1, 1), GPP_GL(1, 1))!LB

            Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & NPP(1, 1), NPP_GL(1, 1))!LB

        !
        !LB added the following to save TDIC and TALK fluxes in output
		!use 0 as start point as local array DICUPT is from 0
        !
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & DICUPT(0, 1), DICUPT_GL(1, 1))
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & DICBMP(0, 1), DICBMP_GL(1, 1))
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & DICPRD(0, 1), DICPRD_GL(1, 1))
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & DICMNL(0, 1), DICMNL_GL(1, 1))
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & DICDEN(0, 1), DICDEN_GL(1, 1))
        ! Call GATHER (0,MTLOC,MLOC,MGL,KBM1,MYID,NPROCS, &
             !& NMAP,DICNIT(0,1),DICNIT_GL(1,1))
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & DICGAS(0, 1), DICGAS_GL(1, 1))
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & DICSED(0, 1), DICSED_GL(1, 1))
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & ALKNH4(0, 1), ALKNH4_GL(1, 1))
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & ALKNO3(0, 1), ALKNO3_GL(1, 1))
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & ALKNIT(0, 1), ALKNIT_GL(1, 1))
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & ALKDEN(0, 1), ALKDEN_GL(1, 1))
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & ALKREM(0, 1), ALKREM_GL(1, 1))
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & ALKNH4SED(0, 1), ALKNH4SED_GL(1, 1))
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & ALKNO3SED(0, 1), ALKNO3SED_GL(1, 1))

		!
        !LB added the following to save Advection+horizDiff fluxes in output
		!WLong:use 0 as starting index, as local array ADVFLUX is from 0
        !   (Only saving TALK and TDIC in output)
        !    Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
        !   & ADVFLUX(0, 1, 33), ADVFLUX_GL(1, 1, 33))!TDIC
        !    Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
        !   & ADVFLUX(0, 1, 34), ADVFLUX_GL(1, 1, 34))!TALK
        !    Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
        !   & VDIFFLUX(0, 1, 33), VDIFFLUX_GL(1, 1, 33))!TDIC
        !    Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
        !   & VDIFFLUX(0, 1, 34), VDIFFLUX_GL(1, 1, 34))!TALK


        ! Wen Long added the following for debugging DOXG
        !
            Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & DDOC(1, 1), DDOC_GL(1, 1))
            Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & DCOD(1, 1), DCOD_GL(1, 1))
            Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & NITRIF(1, 1), NITRIF_GL(1, 1))
            Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & DOSZ(1, 1), DOSZ_GL(1, 1))
            Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & DOLZ(1, 1), DOLZ_GL(1, 1))
            Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & DOPR(1, 1), DOPR_GL(1, 1))
            Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & DORALG(1, 1), DORALG_GL(1, 1))
            Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & RESP(1, 1), RESP_GL(1, 1))
            Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & FTCOD(1, 1), FTCOD_GL(1, 1))
            Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & COD(0, 1), COD_GL(1, 1))
            Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, NMAP, &
           & REAERDO(1, 1), REAERDO_GL(1, 1))
        !
            If (SEDIMENT_CALC) Then
               Call GATHER (1, MTLOC, MLOC, MGL, 3, MYID, NPROCS, NMAP, &
              & JPOC(1, 1), JPOC_GL(1, 1))
               Call GATHER (1, MTLOC, MLOC, MGL, 3, MYID, NPROCS, NMAP, &
              & JPON(1, 1), JPON_GL(1, 1))
               Call GATHER (1, MTLOC, MLOC, MGL, 3, MYID, NPROCS, NMAP, &
              & JPOP(1, 1), JPOP_GL(1, 1))
               Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
              & JPOS(1), JPOS_GL(1))
           !

               Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
              & CTEMP(1), CTEMP_GL(1))

               CH40_GL = 0.d0 !temporarily set to zero

               Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
              & SODTM1S(1), SODTM1S_GL(1))

               Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
              & JNH4TM1S(1), JNH4_GL(1))

               Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
              & JNO3TM1S(1), JNO3_GL(1))

               Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
              & BENDEN(1), BENDEN_GL(1))

               Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
              & JCH4TM1S(1), JCH4_GL(1))

               Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
              & JCH4GTM1S(1), JCH4G_GL(1))

               Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
              & JHSTM1S(1), JHS_GL(1))

               Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
              & JPO4TM1S(1), JPO4_GL(1))

               Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
              & JSITM1S(1), JSI_GL(1))

               Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
              & NH41TM1S(1), NH41_GL(1))

               Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
              & NH42TM1S(1), NH42_GL(1))

               Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
              & NO31TM1S(1), NO31_GL(1))

               Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
              & NO32TM1S(1), NO32_GL(1))

               Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
              & PO41TM1S(1), PO41_GL(1))

               Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
              & PO42TM1S(1), PO42_GL(1))

               Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
              & SI1TM1S(1), SI1_GL(1))

               Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
              & SI2TM1S(1), SI2_GL(1))

               Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
              & CH41TM1S(1), CH41TM1S_GL(1))

               Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
              & CH42TM1S(1), CH42_GL(1))

               Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
              & HS1TM1S(1), HS1_GL(1))

               Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
              & HS2TM1S(1), HS2_GL(1))

               Call GATHER (1, MTLOC, MLOC, MGL, 3, MYID, NPROCS, NMAP, &
              & CPOP(1, 1), CPOP_GL(1, 1))

               Call GATHER (1, MTLOC, MLOC, MGL, 3, MYID, NPROCS, NMAP, &
              & CPON(1, 1), CPON_GL(1, 1))

               Call GATHER (1, MTLOC, MLOC, MGL, 3, MYID, NPROCS, NMAP, &
              & CPOC(1, 1), CPOC_GL(1, 1))

               Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
              & CPOS(1), CPOS_GL(1))

               Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
              & HSED1(1), HSED1_GL(1))


               Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
              & BENSTRTM1S(1), BENSTRTM1S_GL(1))

                Call GATHER (1, MTLOC, MLOC, MGL, 1, MYID, NPROCS, NMAP, &
             & HSED(1), HSED_GL(1))

           
            End If !!end LBcomment


			If (SOLIDS_CALC) Then !Adi-2/13/18
               Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, &
              & NMAP, TSS(1, 1), TSS_GL(1, 1))
               Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, &
              & NMAP, KESS(1, 1), KESS_GL(1, 1))
               Call GATHER (0, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, &
              & NMAP, SSI(0, 1), SSI_GL(1, 1))
			   Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, &
              & NMAP, TURB(1, 1), TURB_GL(1, 1))
			   Call GATHER (1, MTLOC, MLOC, MGL, KBM1, MYID, NPROCS, &
              & NMAP, PARAD(1, 1), PARAD_GL(1, 1))
            End If
			!
			 If (SAV_CALC) Then !AN
               Call GATHER (1, MTLOC, MLOC, MGL, NSAVM, MYID, NPROCS, &
              & NMAP, LEAF(1, 1), LEAF_GL(1, 1))
               Call GATHER (1, MTLOC, MLOC, MGL, NSAVM, MYID, NPROCS, &
              & NMAP, STEM(1, 1), STEM_GL(1, 1))
               Call GATHER (1, MTLOC, MLOC, MGL, NSAVM, MYID, NPROCS, &
              & NMAP, ROOT(1, 1), ROOT_GL(1, 1))
			   Call GATHER (1, MTLOC, MLOC, MGL, NSAVM, MYID, NPROCS, &
              & NMAP, TUBER(1, 1), TUBER_GL(1, 1))
			   Call GATHER (1, MTLOC, MLOC, MGL, NSAVM, MYID, NPROCS, &
              & NMAP, EP(1, 1), EP_GL(1, 1))
            End If

         Else
# endif
            CCHL1_GL (1:MGL, 1:KBM1) = CCHL1 (1:MGL, 1:KBM1)
            total_netPP_GL (1:MGL) = total_netPP (1:MGL)
            C2_GL (1:MGL, 1:KBM1, 27) = C2 (1:MGL, 1:KBM1, 27)
            C2_GL (1:MGL, 1:KBM1, 9) = C2 (1:MGL, 1:KBM1, 9)
            C2_GL (1:MGL, 1:KBM1, 10) = C2 (1:MGL, 1:KBM1, 10)
            C2_GL (1:MGL, 1:KBM1, 11) = C2 (1:MGL, 1:KBM1, 11)
            C2_GL (1:MGL, 1:KBM1, 12) = C2 (1:MGL, 1:KBM1, 12)
			If (SOLIDS_CALC) Then
			 C2_GL (1:MGL, 1:KBM1, 3) = C2 (1:MGL, 1:KBM1, 3) ! AN for SSI
			End If
            C2_GL (1:MGL, 1:KBM1, 4) = C2 (1:MGL, 1:KBM1, 4)
            C2_GL (1:MGL, 1:KBM1, 5) = C2 (1:MGL, 1:KBM1, 5)
			If (ZOO_CALC) Then !AN
			 C2_GL (1:MGL, 1:KBM1, 7) = C2 (1:MGL, 1:KBM1, 7) ! AN for Zoo
			 C2_GL (1:MGL, 1:KBM1, 8) = C2 (1:MGL, 1:KBM1, 8) ! AN for Zoo
            End If
            C2_GL (1:MGL, 1:KBM1, 13) = C2 (1:MGL, 1:KBM1, 13)
            C2_GL (1:MGL, 1:KBM1, 14) = C2 (1:MGL, 1:KBM1, 14)
            C2_GL (1:MGL, 1:KBM1, 20) = C2 (1:MGL, 1:KBM1, 20)
        !
            T_GL (1:MGL, 1:KBM1) = T (1:MGL, 1:KBM1)! Wen Long, correction to assign values for all layers
            S_GL (1:MGL, 1:KBM1) = SALT (1:MGL, 1:KBM1)! Wen Long, correction to assign values for all layers
        !
            D_GL (1:MGL) = D (1:MGL)! Wen Long, add D_GL, EL_GL, H_GL
            EL_GL (1:MGL) = EL (1:MGL)
            H_GL (1:MGL) = H (1:MGL)
        !
            C2_GL (1:MGL, 1:KBM1, 33) = C2 (1:MGL, 1:KBM1, 33)!LB
            C2_GL (1:MGL, 1:KBM1, 34) = C2 (1:MGL, 1:KBM1, 34)!LB
            pH_GL (1:MGL, 1:KBM1) = pH (1:MGL, 1:KBM1)!LB
            pCO2_GL (1:MGL, 1:KBM1) = pCO2 (1:MGL, 1:KBM1)!LB
!
        !LB adding LDON, RDON, LPON, RPON  23jun2016
            C2_GL (1:MGL, 1:KBM1, 16) = C2 (1:MGL, 1:KBM1, 16)
            C2_GL (1:MGL, 1:KBM1, 17) = C2 (1:MGL, 1:KBM1, 17)
            C2_GL (1:MGL, 1:KBM1, 18) = C2 (1:MGL, 1:KBM1, 18)
            C2_GL (1:MGL, 1:KBM1, 19) = C2 (1:MGL, 1:KBM1, 19)

        !
           P1_GL = P1
           P2_GL = P2
           BM1_GL = BM1
           BM2_GL = BM2
           NL1_GL = NL1
           NL2_GL = NL2
           PL1_GL = PL1
           PL2_GL = PL2
           B1SZ_GL = B1SZ
           B2SZ_GL = B2SZ
           B1LZ_GL = B1LZ
           B2LZ_GL = B2LZ
           PR1_GL = PR1
           PR2_GL = PR2
            If (LIGHT_EXTINCTION) Then
               IAVG_GL = IAVG
               FI1_GL = FI1
               FI2_GL = FI2
            End If
            IK1_GL = IK1
            IK2_GL = IK2
        !
            GPP_GL = GPP !LB
            NPP_GL = NPP !LB
        !
        ! Wen Long added the following for debugging DOXG
        !
            DDOC_GL = DDOC
            DCOD_GL = DCOD
            NITRIF_GL = NITRIF
            DOSZ_GL = DOSZ
            DOLZ_GL = DOLZ
            DOPR_GL = DOPR
            DORALG_GL = DORALG
            RESP_GL = RESP
            FTCOD_GL = FTCOD
            COD_GL = COD
            REAERDO_GL = REAERDO
        !
        ! Wen Long added the following for debuguing benthic fluxes
            BFLUX_GL = BFLUX
        !
        !LB added the following to save TDIC and TALK fluxes as output
        !
            DICUPT_GL = DICUPT
            DICBMP_GL = DICBMP
            DICPRD_GL = DICPRD
            DICMNL_GL = DICMNL
            DICDEN_GL = DICDEN
        !DICNIT_GL = DICNIT
            DICGAS_GL = DICGAS
            DICSED_GL = DICSED
            ALKNH4_GL = ALKNH4
            ALKNO3_GL = ALKNO3
            ALKNIT_GL = ALKNIT
            ALKDEN_GL = ALKDEN
            ALKREM_GL = ALKREM
            ALKNH4SED_GL = ALKNH4SED
            ALKNO3SED_GL = ALKNO3SED
        !
        !LB added the following to save TDIC and TALK advection + horiz diff AND vert diff fluxes as output
        !
            !ADVFLUX_GL = ADVFLUX
            !VDIFFLUX_GL = VDIFFLUX
        !
        !collect sediment outputs to global arrays
            If (SEDIMENT_CALC) Then
               JPOC_GL = JPOC
               JPON_GL = JPON
               JPOP_GL = JPOP
               JPOS_GL = JPOS
               CTEMP_GL = CTEMP
               CH40_GL = 0.d0 !temporarily set to zero for no CH4 in water column model !0.d0 instead of 0.0 to make it equal to serial run-
               SODTM1S_GL = SODTM1S
               JNH4_GL = JNH4TM1S
               JNO3_GL = JNO3TM1S
               BENDEN_GL = BENDEN
               JCH4_GL = JCH4TM1S
               JCH4G_GL = JCH4GTM1S
               JHS_GL = JHSTM1S
               JPO4_GL = JPO4TM1S
               JSI_GL = JSITM1S
               NH41_GL = NH41TM1S
               NH42_GL = NH42TM1S
               NO31_GL = NO31TM1S
               NO32_GL = NO32TM1S
               PO41_GL = PO41TM1S
               PO42_GL = PO42TM1S
               SI1_GL = SI1TM1S
               SI2_GL = SI2TM1S
               CH41TM1S_GL = CH41TM1S
               CH42_GL = CH42TM1S
               HS1_GL = HS1TM1S
               HS2_GL = HS2TM1S
               CPOP_GL = CPOP
               CPON_GL = CPON
               CPOC_GL = CPOC
               CPOS_GL = CPOS
               HSED1_GL = HSED1
               BENSTRTM1S_GL = BENSTRTM1S
           !
            End If
		   !Adi-21/13/2018
			If (SOLIDS_CALC) Then
               TSS_GL = TSS
			   TURB_GL = TURB
               SSI_GL(1:MGL, 1:KBM1) = SSI(1:MGL, 1:KBM1)
               KESS_GL = KESS
			   PARAD_GL = PARAD
            End If

			If (SAV_CALC) Then !AN
               LEAF_GL = LEAF
			   STEM_GL = STEM
               ROOT_GL = ROOT
               TUBER_GL = TUBER
			   EP_GL = EP
            End If
        !
# if defined (MULTIPROCESSOR)
         End If
# endif
     !    
            JDAY_TMP = JDAY

         PRINT_STN = .False.
         If (NTSTN > NTSTN_OLD) Then
            CTR_STN = CTR_STN + 1 !Counter of steps in station output file
            PRINT_STN = .True.
            NTSTN_OLD = NTSTN
         End If
     !
         If (MSR) Then
        !
            Write (*,*) 'CTR_STN=', CTR_STN
            If (CTR_STN .Eq. 1) Then !Open file for the first time
           !
           !WLong: Instead of opening one file for each station, here I simply use a single file for all
           !       stations
           !
               Write (*,*) 'Opening station output file:', TRIM (STNFN)
               Open (Unit=UNIT_STN, File=TRIM(STNFN), Status='UNKNOWN')
               Write (UNIT_STN,*) "Nstation,Nlayer"
               Write (UNIT_STN,*) Nstation, KBM1 !number of stations and number of layers
           !
               TITLE_STN = 'Variables="StationID","Node","Layer","depth&
              &(m)","DO","NO3","NH4","Alg1","Alg2","LDOC","RDOC","LPOC"&
              &,"RPOC","PO4"'
               TITLE_STN = TRIM (TITLE_STN) // ',"TDIC","TALK","pH","pCO&
              &2"' !LB
           !Wen Long debug algae
               TITLE_STN = TRIM (TITLE_STN) // ',"T","S","P1","P2","BM1&
              &","BM2","NL1","NL2","PL1","PL2","FI1","FI2","B1SZ","B2SZ&
              &","B1LZ","B2LZ","PR1","PR2","IAVG"'
           !   TITLE_STN = TRIM (TITLE_STN) // ',"DDOC","DCOD","NITR&
                !  &IF","DOSZ","DOLZ","DOPR","DORALG","RESP","FTCOD","COD&
                !  &"'
           !   TITLE_STN = TRIM (TITLE_STN) // ',"REAERDO","BFLUXDO"&
                !  &,"BFLUXNO3","BFLUXNH4","IK1","IK2"'
           !LB-fluxes TDIC and TALK
           ! TITLE_STN=TRIM(TITLE_STN)//',"DICUPT","DICBMP","DICPRD"&
                !&,"DICMNL","DICDEN","DICNIT","DICGAS","DICSED"'      !LB
               TITLE_STN = TRIM (TITLE_STN) // ',"DICUPT","DICBMP","DIC&
              &PRD","DICMNL","DICDEN","DICGAS","DICSED"' !LB
               TITLE_STN = TRIM (TITLE_STN) // ',"ALKNH4","ALKNO3","ALK&
              &NIT","ALKDEN","ALKREM","ALKNH4SED","ALKNO3SED"' !LB
!
			   If (SOLIDS_CALC) Then
			         TITLE_STN = TRIM (TITLE_STN) // ',"TSS","SSI","TURB"&
                    &,"KESS","PARAD"'
			   End If !end AN Comment
!
			   If (ZOO_CALC) Then
			         TITLE_STN = TRIM (TITLE_STN) // ',"SZ","LZ"'
			   End If !end AN Comment
!
               If (SEDIMENT_CALC) Then
                  TITLE_STN = TRIM (TITLE_STN) // ',"Jcin1","Jcin2","Jc&
                 &in3","Jnin1","Jnin2","Jnin3","Jpin1","Jpin2","Jpin3"'
                  TITLE_STN = TRIM (TITLE_STN) // ',"Jsin","O20","Depth&
                 &","Tw","NH30","NO30","PO40","SI0","CH40"'
                  TITLE_STN = TRIM (TITLE_STN) // ',"SALw","SOD","Jnh4"&
                 &,"Jno3","JDenitT"'
                  TITLE_STN = TRIM (TITLE_STN) // ',"Jch4"'
                  TITLE_STN = TRIM (TITLE_STN) // ',"Jch4g","Jhs","Jpo4&
                 &","Jsi","NH31","NH32","NO31","NO32"'
                  TITLE_STN = TRIM (TITLE_STN) // ',"PO41","PO42","Si1"&
                 &,"Si2","CH41","CH42","HS1","HS2","POC21","POC22","POC&
                 &23"'
                  TITLE_STN = TRIM (TITLE_STN) // ',"PON21","PON22","PO&
                 &N23,"POP21","POP22","POP23","POS2","H1","BEN_STR"'
              !TITLE_STN=TRIM(TITLE_STN)//',"FRPOC1","FRPOC2","FRPON1","FRPON2","FRPOP1","FRPOP2"'
               End If
!
			   If (SAV_CALC) Then ! AN
			         TITLE_STN = TRIM (TITLE_STN) // ',"LEAF","STEM","ROOT"&
                    &,"TUBER","EP"'
			   End If !end AN Comment
!
               Write (UNIT_STN, '(A)') TRIM (TITLE_STN)
           !
               OPEN_STN = .True.
            End If !END CTR_STN switch
        !
            If (PRINT_STN) Then
           !
               Write (*,*) 'Writing to station outputs, JDAY=', &
              & JDAY_TMP
               Write (UNIT_STN,*) Nstation, KBM1, JDAY_TMP
               Do I = 1, Nstation
                  Do K = 1, KBM1
                 !                        Write (UNIT_STN, '(:I8,1x,I8,1x,I8/(:8(F12.6,1X&!
                 !                       &),F12.6))') I, NstationNum_GL (I), K, - D_GL &!
                     Write (UNIT_STN, '(:I8,1x,I8,1x,I8/(:8(E13.6e3,1X),E13.6e3))') I, NstationNum_GL (I), K, - D_GL &
                    & (NstationNum_GL(I)) * ZZ2D (I,K), C2_GL (NstationNum_GL(I), K, 27), C2_GL (NstationNum_GL(I), K, 14), C2_GL &
                    & (NstationNum_GL(I), K, 13), C2_GL (NstationNum_GL(I), K, 4), C2_GL (NstationNum_GL(I), K, 5), C2_GL &
                    & (NstationNum_GL(I), K, 9), C2_GL (NstationNum_GL(I), K, 10), C2_GL (NstationNum_GL(I), K, 11), C2_GL &
                    & (NstationNum_GL(I), K, 12), C2_GL (NstationNum_GL(I), K, 20), C2_GL (NstationNum_GL(I), K, 33), C2_GL &
                    & (NstationNum_GL(I), K, 34), pH_GL (NstationNum_GL(I), K), pCO2_GL (NstationNum_GL(I), K), T_GL &
                    & (NstationNum_GL(I), K), S_GL (NstationNum_GL(I), K), P1_GL (NstationNum_GL(I), K), P2_GL (NstationNum_GL(I), &
                    & K), BM1_GL (NstationNum_GL(I), K), BM2_GL (NstationNum_GL(I), K), NL1_GL (NstationNum_GL(I), K), NL2_GL &
                    & (NstationNum_GL(I), K), PL1_GL (NstationNum_GL(I), K), PL2_GL (NstationNum_GL(I), K), FI1_GL &
                    & (NstationNum_GL(I), K), FI2_GL (NstationNum_GL(I), K), B1SZ_GL (NstationNum_GL(I), K), B2SZ_GL &
                    & (NstationNum_GL(I), K), B1LZ_GL (NstationNum_GL(I), K), B2LZ_GL (NstationNum_GL(I), K), PR1_GL &
                    & (NstationNum_GL(I), K), PR2_GL (NstationNum_GL(I), K), IAVG_GL (NstationNum_GL(I), K), DICUPT_GL &
                    & (NstationNum_GL(I), K), DICBMP_GL (NstationNum_GL(I), K), DICPRD_GL (NstationNum_GL(I), K), DICMNL_GL &
                    & (NstationNum_GL(I), K), DICDEN_GL (NstationNum_GL(I), K), DICGAS_GL (NstationNum_GL(I), K), DICSED_GL &
                    & (NstationNum_GL(I), K), &
                    & ALKNH4_GL (NstationNum_GL(I), K), ALKNO3_GL (NstationNum_GL(I), K), ALKNIT_GL (NstationNum_GL(I), K), &
                    & ALKDEN_GL (NstationNum_GL(I), K), ALKREM_GL (NstationNum_GL(I), K), ALKNH4SED_GL (NstationNum_GL(I), K), &
                    & ALKNO3SED_GL (NstationNum_GL(I), K)                       !& (NstationNum_GL(I), K), DICNIT_GL &!

                   If (SOLIDS_CALC) Then
                    Write (UNIT_STN, '(:8(E13.6e3,1X),E13.6e3)') TSS_GL (NstationNum_GL(I), K), SSI_GL &
                     & (NstationNum_GL(I), K), TURB_GL (NstationNum_GL(I), K), KESS_GL &
                     & (NstationNum_GL(I), K), PARAD_GL (NstationNum_GL(I), K)
                   End If !end Adi Comment
				   If (ZOO_CALC) Then
                    Write (UNIT_STN, '(:8(E13.6e3,1X),E13.6e3)') C2_GL (NstationNum_GL(I), K, 7), C2_GL &
                     & (NstationNum_GL(I), K, 8)
                   End If !end Adi Comment
              !
                  End Do
              !
              !write sediment diagenesis model outputs   !LBcomment
                  If (SEDIMENT_CALC) Then
                 !
                 
                     Write (UNIT_STN, '(:8(E14.6e3,1X),E14.6e3)') JPOC_GL (NstationNum_GL(I), 1) / 1000.0 * 2.667, JPOC_GL &
                    & (NstationNum_GL(I), 2) / 1000.0 * 2.667, JPOC_GL (NstationNum_GL(I), 3) / 1000.0 * 2.667, JPON_GL &
                    & (NstationNum_GL(I), 1) / 1000.0, JPON_GL (NstationNum_GL(I), 2) / 1000.0, JPON_GL (NstationNum_GL(I), 3) / &
                    & 1000.0, JPOP_GL (NstationNum_GL(I), 1) / 1000.0, JPOP_GL (NstationNum_GL(I), 2) / 1000.0, JPOP_GL &
                    & (NstationNum_GL(I), 3) / 1000.0, JPOS_GL (NstationNum_GL(I)) / 1000.0, C2_GL (NstationNum_GL(I), KBM1, 27), &
                    & D_GL (NstationNum_GL(I)), T_GL (NstationNum_GL(I), KBM1), C2_GL (NstationNum_GL(I), KBM1, 13), C2_GL &
                    & (NstationNum_GL(I), KBM1, 14), C2_GL (NstationNum_GL(I), KBM1, 20), C2_GL (NstationNum_GL(I), KBM1, 29), &
                    & CH40_GL (NstationNum_GL(I)), S_GL (NstationNum_GL(I), KBM1), SODTM1S_GL (NstationNum_GL(I)), JNH4_GL &
                    & (NstationNum_GL(I)) / 1000.0, JNO3_GL (NstationNum_GL(I)) / 1000.0, BENDEN_GL (NstationNum_GL(I)), JCH4_GL &
                    & (NstationNum_GL(I)), JCH4G_GL (NstationNum_GL(I)), JHS_GL (NstationNum_GL(I)), JPO4_GL (NstationNum_GL(I)) / &
                    & 1000.0, JSI_GL (NstationNum_GL(I)) / 1000.0, NH41_GL (NstationNum_GL(I)) / 1000.0, NH42_GL &
                    & (NstationNum_GL(I)) / 1000.0, NO31_GL (NstationNum_GL(I)) / 1000.0, NO32_GL (NstationNum_GL(I)) / 1000.0, &
                    & PO41_GL (NstationNum_GL(I)) / 1000.0, PO42_GL (NstationNum_GL(I)) / 1000.0, SI1_GL (NstationNum_GL(I)) / &
                    & 1000.0, SI2_GL (NstationNum_GL(I)) / 1000.0, CH41TM1S_GL (NstationNum_GL(I)), CH42_GL (NstationNum_GL(I)), &
                    & HS1_GL (NstationNum_GL(I)), HS2_GL (NstationNum_GL(I)), CPOC_GL (NstationNum_GL(I), 1) / 1000.0 * 2.667, &
                    & CPOC_GL (NstationNum_GL(I), 2) / 1000.0 * 2.667, CPOC_GL (NstationNum_GL(I), 3) / 1000.0 * 2.667, CPON_GL &
                    & (NstationNum_GL(I), 1) / 1000.0, CPON_GL (NstationNum_GL(I), 2) / 1000.0, CPON_GL (NstationNum_GL(I), 3) / &
                    & 1000.0, CPOP_GL (NstationNum_GL(I), 1) / 1000.0, CPOP_GL (NstationNum_GL(I), 2) / 1000.0, CPOP_GL &
                    & (NstationNum_GL(I), 3) / 1000.0, CPOS_GL (NstationNum_GL(I)) / 1000.0, HSED1_GL (NstationNum_GL(I)) * 1000.0, &
                    & BENSTRTM1S_GL (NstationNum_GL(I))!,         &           !'BEN_STR'             dimensionless        dimensionless
 
                      !
                  End If !end LBcomment
				!
				  If (SAV_CALC) Then
			         Write (UNIT_STN, '(:8(E13.6e3,1X),E13.6e3)') &
                    & (LEAF_GL (NstationNum_GL(I), J), J=1, NSAVM), &
                    & (STEM_GL (NstationNum_GL(I), J), J=1, NSAVM), &
                    & (ROOT_GL (NstationNum_GL(I), J), J=1, NSAVM), &
                    & (TUBER_GL (NstationNum_GL(I), J), J=1, NSAVM), &
                    & (EP_GL (NstationNum_GL(I), J), J=1, NSAVM)
				  End If !end Adi Comment
               End Do
           !Push the write through onto disk
               FLUSH (UNIT_STN)
            End If !END PRINT_STN SWITCH
        !
         End If !END MSR SWITCH
     !
     !LB 28jan2016: reset fluxed to 0 to start accumulation (ie, integration) after writing history file
         If (PRINT_HIS) Then !output history of fields
            DICUPT = 0.0
            DICBMP = 0.0
            DICPRD = 0.0
            DICMNL = 0.0
            DICDEN = 0.0
            !DICNIT = 0.0
            DICGAS = 0.0
            DICSED = 0.0
            ALKNH4 = 0.0
            ALKNO3 = 0.0
            ALKNIT = 0.0
            ALKDEN = 0.0
            ALKREM = 0.0
            ALKNH4SED = 0.0
            ALKNO3SED = 0.0
            !ADVFLUX = 0.0
            !VDIFFLUX = 0.0
         End If
      End If !END MOD(JDAY  ... ) SWITCH
End Subroutine WQM_OUTPUT
!---------------------------------------------------------------------------------------------


!---------------------------------------------------------------------------------------------
SUBROUTINE DebugNan()
   USE MOD_PREC, ONLY: SP
   USE MOD_LIMS, ONLY: MTLOC, MLOC, KBM1
   USE MOD_WQM, ONLY: C2
   USE MOD_SIZES, ONLY: NCP

   IMPLICIT NONE

   INTEGER :: ii,jj,kk

    DO kk=1,NCP
        DO ii=1,KBM1
            DO jj=1,MLOC
                IF(ISNAN(C2(jj,ii,kk))) THEN
                    WRITE(*,*)"This variable goes to NAN",jj,ii,kk
                    STOP
                END IF
            END DO
        END DO
    END DO
END SUBROUTINE
!---------------------------------------------------------------------------------------------

!----------------------------------------------------------------------------------
SUBROUTINE DILU_OUTPUT(fileno,LyNb)

  USE MOD_SIZES, ONLY : MGL, NGL
  USE MOD_HYDROVARS, ONLY : XG,YG,NVG
  USE MOD_WQM, ONLY : C2_GL
  USE TXC_KNT, ONLY : SSIWC_GL

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: fileno, LyNb
  INTEGER :: flid,CELL_BLOCKNO,i,j,NN1,NN2,NN3
  CHARACTER(LEN=32) :: flname

  flid = fileno + 1
  WRITE(flname,400), flid
  OPEN(UNIT = flid, FILE = './outputs/toxic_con'//TRIM(flname)//'.vtk', ACTION = 'WRITE')

  WRITE(flid,'(A)')'# vtk DataFile Version 3.1'
  WRITE(flid,'(A)')'This is an example file created for VisuSimple'
  WRITE(flid,'(A)')'ASCII'
  WRITE(flid,'(A)')'DATASET UNSTRUCTURED_GRID'
  !WRITE(111,FMT=410)
  WRITE(flid,'(A,I,1x,A)')'POINTS',MGL,'FLOAT'

  DO i=1,MGL
    WRITE(flid,FMT=525)XG(i),YG(i),0.0
  END DO
  WRITE(flid,*)

  CELL_BLOCKNO = 4*NGL
  WRITE(flid,'(A,I,I)')'CELLS',NGL,CELL_BLOCKNO

  DO j=1,NGL
    NN1 = NVG(j,1)-1
    NN2 = NVG(j,2)-1
    NN3 = NVG(j,3)-1
    WRITE(flid,FMT=600),3,NN1,NN2,NN3
  END DO
  WRITE(flid,*)

  WRITE(flid,'(A,I)')'CELL_TYPES',NGL
  WRITE(flid,FMT=700),(5, i=1,NGL)
  WRITE(flid,*)
  WRITE(flid,'(A,I)')'POINT_DATA',MGL
  WRITE(flid,'(A)')'SCALARS Cell_Concentration DOUBLE'
  WRITE(flid,'(A)')'LOOKUP_TABLE default'

  DO i=1,MGL
    WRITE(flid,*) C2_GL(i,LyNb,3)
    !WRITE(flid,*) SSIWC_GL(i)
  END DO

!FLUSH(flid)
CLOSE(flid)

400 FORMAT(I5.5)
525 FORMAT(f15.4,3x,f15.4,3x,f15.4)
450 FORMAT(f15.4)
700 FORMAT(I,3x)
600 FORMAT(4(I,x))

END SUBROUTINE
!---------------------------------------------------------------------------------


!-------------------- Initial setup for the program - added by Laki -------------------------
SUBROUTINE MainSetup
  USE MOD_WQM, ONLY: NumWqVrb,NumTxVrb,TxcKntOn, TxcOutput, NumTcrVrb, TcrKntOn, TcrtOutput
  USE MOD_WQM, ONLY: TXCF_INC,TXCF_PNT,TXCF_OBC, TXCF_INC_SD,TCRF_INC,TCRF_PNT,TCRF_OBC,TCRF_INC_SD, &
                     & TxcSdmInitType,TxcSdmUnfm,PCBTxcOn,MTLTxcOn,NumTxcOutVar,PTC_WC,PTC_SD,SED_DEN, &
                     & DFFCOEFS12,BRLVEL,MolWghtCng,HNRCFF,TcrSdmInitType,TcrSdmUnfm,BACTERIAON, &
                     & RADIONUCON,NOTCRKNTS,NumTcrOutVar,KnBact,C0SalBact,AlphaBact,ThetaBact, &
                     & RnHalfTime,RnKd,VonKrmn,Rgfh,PtcDensity,ErsnCoeff,CrtShear,SdmPrst,WqCalcOn, PCBOutVrb, &
                     & TcrOutVrb, TxcHotstart, TxcHotWC,TxcHotSD1,TxcHotSD2, BioAcc,KuptkSZ, KuptkLZ, KowPCB, & 
                     & GrwthSZ,GrwthLZ,CnbToDWSZ,LipidFracSZ,CnbToDWLZ, LipidFracLZ,TxcAccSZ,TxcAccLZ,DynUptkON, &
                     & OxgToCbnSZ,OxgToCbnLZ,OxgRspRateSZ,OxgRspRateLZ,WetToDryRtSZ, WetToDryRtLZ, EchToEoxSZ, & 
                     & EchToEoxLZ,AlphaSZ,AlphaLZ, VDiffCoff, KDecay,ExcLossSZ,ExcLossLZ,ZOO_DVM,ShpFacDVM,PeakTmDVM, &
                     & SpanDVM,WmaxSZ,WmaxLZ,PrayLaySZ,PrayLayLZ,SleepLaySZ,SleepLayLZ
  USE MOD_SIZES, ONLY: NCP, NTXVB, NBTCR, HydVersionNum
  USE MOD_CONTROL, ONLY: MSR
  IMPLICIT NONE

  CHARACTER(LEN = 64) :: FLAG_TXC, FLAG_TCR
  INTEGER :: end_file,i
  
!------------------------- Reading Main setup file ------------------------------ 
  !INQUIRE(FILE='inputs/WQM_welcome.nml', IOSTAT = end_file)
  INQUIRE(FILE='WQM_welcome.nml', IOSTAT = end_file)

  IF(end_file /= 0) THEN 
    WRITE(*,*)'The file WQM_welcome.nml does not exist'
    STOP
  END IF

  NAMELIST /WQM_SETUP/ HydVersionNum,NumWqVrb,WqCalcOn,TxcKntOn,TcrKntOn,ZOO_DVM,NumTxVrb,NumTcrVrb
  OPEN (UNIT=377,FILE='WQM_welcome.nml', ACTION = 'READ')

  READ(377,NML = WQM_SETUP, IOSTAT = end_file)
  IF(end_file /= 0) THEN 
    WRITE(*,*)'Invalid namelist format'
    STOP 
  END IF
  !------------------------------------------------------------------------------ 
  IF(TxcKntOn == .TRUE.) THEN
    NAMELIST /TXC_SETUP/ TXCF_INC,TXCF_PNT,TXCF_OBC,TXCF_INC_SD,TxcSdmInitType, &
                        & TxcSdmUnfm,PCBTxcOn,MTLTxcOn,BioAcc,NumTxcOutVar,TxcHotstart, &
                        & TxcHotWC,TxcHotSD1,TxcHotSD2
    READ(377,NML = TXC_SETUP, IOSTAT = end_file)
    IF(end_file /= 0) THEN
        WRITE(*,*)'Invalid namelist format for toxics'
    END IF
    IF(PCBTxcOn .AND. MTLTxcOn) THEN 
        WRITE(*,*)'Only one type of toxic kinetics can be activated at a time ......'
        STOP
    END IF

    IF(PCBTxcOn) THEN
        ALLOCATE(MolWghtCng(NumTxVrb))
        NAMELIST /TXC_PARAMETER_PCB/ PTC_WC,PTC_SD,SED_DEN,DFFCOEFS12,BRLVEL,MolWghtCng, &
                            & HNRCFF,KowPCB,VDiffCoff,KDecay
        READ(377,NML = TXC_PARAMETER_PCB, IOSTAT = end_file)
        ALLOCATE(PCBOutVrb(NumTxcOutVar))
        NAMELIST /TXC_OUT_PCB/PCBOutVrb
        READ(377,NML = TXC_OUT_PCB, IOSTAT = end_file)
    END IF

    IF(BioAcc) THEN 
        NAMELIST /TXC_BIOACCM/TxcAccSZ,TxcAccLZ,KuptkSZ,KuptkLZ,GrwthSZ,GrwthLZ,CnbToDWSZ,CnbToDWLZ, &
                    & LipidFracSZ, LipidFracLZ, AlphaSZ, AlphaLZ, ExcLossSZ, ExcLossLZ, DynUptkON
        READ(377,NML = TXC_BIOACCM, IOSTAT = end_file)
    END IF

    IF((BioAcc == .TRUE.) .AND. (DynUptkON == .TRUE.)) THEN 
        NAMELIST /TXC_BIOACCM_UPTAKE/OxgToCbnSZ,OxgToCbnLZ,OxgRspRateSZ,OxgRspRateLZ,WetToDryRtSZ, &
                    & WetToDryRtLZ, EchToEoxSZ, EchToEoxLZ 
        READ(377,NML = TXC_BIOACCM_UPTAKE, IOSTAT = end_file)
    END IF

  END IF
  !------------------------------------------------------------------------------
  IF(TcrKntOn == .TRUE.) THEN 
    NAMELIST /TCR_SETUP/ TCRF_INC,TCRF_PNT,TCRF_OBC,TCRF_INC_SD,TcrSdmInitType, &
                & TcrSdmUnfm,BACTERIAON,RADIONUCON,NOTCRKNTS,NumTcrOutVar
    READ(377,NML = TCR_SETUP, IOSTAT = end_file)
    IF(end_file /= 0) THEN 
        WRITE(*,*)'Invalid namelist format for tracer'
    END IF
    
    IF((BACTERIAON .AND. RADIONUCON) .OR. (BACTERIAON .AND. NOTCRKNTS) .OR. &
           & (RADIONUCON .AND. NOTCRKNTS)) THEN 
        WRITE(*,*)'Only one type of tracer kinetics can be activated at a time ......'
        STOP 
    END IF

    IF(BACTERIAON) THEN 
        NAMELIST /TCR_PARAMETER_FB/KnBact,C0SalBact,AlphaBact,ThetaBact
        READ(377,NML = TCR_PARAMETER_FB, IOSTAT = end_file)
    END IF

    IF(RADIONUCON) THEN 
        ALLOCATE(RnHalfTime(NumTcrVrb))
        ALLOCATE(RnKd(NumTcrVrb))
        NAMELIST /TCR_PARAMETER_RN/RnHalfTime,RnKd
        READ(377,NML = TCR_PARAMETER_RN, IOSTAT = end_file)
        NAMELIST /SEDIMENT_PARA/VonKrmn,Rgfh,PtcDensity,ErsnCoeff,CrtShear,SdmPrst
        READ(377,NML = SEDIMENT_PARA, IOSTAT = end_file)
    END IF

    ALLOCATE(TcrOutVrb(NumTcrOutVar))
    NAMELIST /TCR_OUT/TcrOutVrb
    READ(377,NML = TCR_OUT, IOSTAT = end_file) 

  END IF
  !------------------------------------------------------------------------------

  !----------------------- zooplankton DVM settings -----------------------------
  IF(ZOO_DVM == .TRUE.) THEN
        NAMELIST /DVM_ZOOP/ShpFacDVM,PeakTmDVM,SpanDVM,WmaxSZ,WmaxLZ,PrayLaySZ,&
                    PrayLayLZ,SleepLaySZ,SleepLayLZ
        READ(377,NML = DVM_ZOOP, IOSTAT = end_file)
  END IF

 IF((TxcKntOn == .FALSE.) .AND. (NumTxVrb > 0)) THEN
    NumTxVrb = 0
 END IF

 IF((TcrKntOn == .FALSE.) .AND. (NumTcrVrb > 0)) THEN
    NumTcrVrb = 0
 END IF

  IF((TxcKntOn == .TRUE.) .AND. (NumTxVrb == 0)) THEN
    WRITE(*,*)"Model needs at least one toxic variable to perform a toxic based water quality analysis"
    WRITE(*,*)'Please enter a non-zero number of toxic variables'
    STOP
  END IF

  IF((TcrKntOn == .TRUE.) .AND. (NumTcrVrb == 0)) THEN
    WRITE(*,*)"Model needs at least one tracer/Pollutant variable to perform a tracer/Pollutant fate and transport"
    WRITE(*,*)'Please enter a non-zero number of tracer/Pollutant variables'
    STOP
  END IF

 CLOSE(377)

    NCP = NumWqVrb + NumTxVrb + NumTcrVrb
    NTXVB = NumTxVrb
    NBTCR = NumTcrVrb

WRITE(*,*)'Main setup was completed........'


END SUBROUTINE
!--------------------------------------------------------------------------------


!--------------------- for initial allocation -----------------------------------
SUBROUTINE Init_ALLOC
   USE MOD_WQM, ONLY: CMASS, S1L,S2L,S3L,S1LB,S2LB,S3LB,CB,CBNX,CBOLD,FLUXT, &
                      & S1LN,S2LN,S3LN,AC,NCB,DLWCMCB
  USE MOD_SIZES, ONLY: NCP, NQFP, NS1P, NS2P, NS3P, NBCP
  USE MOD_WQMINIT, ONLY: ACC, CIC, COUT

  IMPLICIT NONE
!------------------------------- added by Laki ---------------------------------
           ALLOCATE(CMASS(NCP))
           CMASS = 0.0
           ALLOCATE(S1L(NS1P,NCP))
           S1L = 0.0
           ALLOCATE(S2L(NS2P,NCP))
           S2L = 0.0
           ALLOCATE(S3L(NS3P,NCP))
           S3L = 0.0
           ALLOCATE(S1LB(NS1P,NCP))
           S1LB = 0
           ALLOCATE(S2LB(NS2P,NCP))
           S2LB = 0
           ALLOCATE(S3LB(NS3P,NCP))
           S3LB = 0
           ALLOCATE(CB(NBCP,NCP))
           CB = 0.0
           ALLOCATE(CBNX(NBCP,NCP))
           CBNX = 0.0
           ALLOCATE(CBOLD(NBCP,NCP))
           CBOLD = 0.0
           ALLOCATE(FLUXT(0:NQFP,NCP))
           FLUXT = 0.0
           ALLOCATE(S1LN(NCP))
           S1LN = 0
           ALLOCATE(S2LN(NCP))
           S2LN = 0
           ALLOCATE(S3LN(NCP))
           S3LN = 0
           ALLOCATE(AC(NCP))
           AC = 0
           ALLOCATE(NCB(NCP))
           NCB = 0
           ALLOCATE(DLWCMCB(NCP))
           DLWCMCB = 0.0

           ALLOCATE(ACC(NCP))
           ALLOCATE(CIC(NCP))
           ALLOCATE(COUT(NCP))

  !-------------------------------------------------------------------------------

END SUBROUTINE
!--------------------------------------------------------------------------------


!------------------------------------------------------------------------------- 
SUBROUTINE WRITE_HOTSTRT(DayNbm)
USE TXC_KNT, ONLY: CONPCB_WGL
USE MOD_WQM, ONLY: NumTxVrb
USE MOD_SIZES, ONLY: MGL
USE MOD_LIMS, ONLY: KBM1
IMPLICIT NONE 

INTEGER, INTENT(IN) :: DayNbm
CHARACTER(LEN = 64) :: flname
INTEGER :: i,j,k

WRITE(flname,400), INT(DayNbm)
OPEN(UNIT = 77, FILE = './outputs/Txc_Hotstr'//TRIM(flname)//'.dat', ACTION = 'WRITE')

WRITE(77, FMT='(A)')'Initial conditions for toxics'
DO i=1,MGL
  DO j=1,KBM1 
    WRITE(77,*)(CONPCB_WGL(i,j,k), k=1,NumTxVrb)
  END DO
END DO
cLOSE(77)

400 FORMAT(I5.5)
END SUBROUTINE
!--------------------------------------------------------------------------------
