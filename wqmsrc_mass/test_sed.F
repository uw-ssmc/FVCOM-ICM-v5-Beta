Program TEST_SED !program for testing mod_sed.F of the fvcom-icm model.
  !
  !
  !include the modules used by mod_sed.F
  !
  !MOD_PREC
  !MOD_SIZES
  !MOD_WQM
  !MOD_FILEINFO
  !MOD_LIMS
  !MOD_HYDROVARS
  !MOD_CONTROL
  !MOD_SF
  !MOD_BA
  !MOD_DF
  !MOD_SED_DF_EXCHANGE_VARS
  !MOD_SED_SAV_EXCHANGE_VARS
  !MOD_PAR
  !MOD_SAV
!
      Use MOD_PREC, Only: SP
  !
      Use MOD_SIZES, Only: MGL, NFLP
  !
      Use MOD_WQM, Only: BFOFN, JDAY, DLT, TMSTRT, CTEMP, SETTLING, &
     & SAV_CALC, B, Q1, Q2, Q3, ANC1, ANC2, ANC3, ASC1, ASC2, ASC3, &
     & WS1NET, WS2NET, WS3NET, WSLNET, WSRNET, WSSNET, WSUNET, KADPO4, &
     & KADSA, T, SALT, SSI, B1, B2, B3, LPOC, RPOC, NH4, NO3, LPON, &
     & RPON, PO4, LPOP, RPOP, PIP, COD, DOXG, SIUPB, SIAT, PCFWS, &
     & PNFWS, PPFWS, PSFWS, SSFWS, BENCH4A, BENCH4G, BENCOD, BENDEN, &
     & BENDO, BENDOC, BENNH4, BENNO3, BENPO4, BENSA, BFIFN
  !
      Use MOD_FILEINFO, Only: BFI, BFO, WCL
!
      Use MOD_LIMS, Only: MLOC, MTLOC, KBM1
  !
      Use MOD_HYDROVARS, Only: ART1, DZ, D !CURRENT DEPTH
  !
      Use MOD_CONTROL, Only: SERIAL, MSR, PAR !TRUE IF MULTIPROCESSOR RUN
  !
      Use MOD_SF, Only: SFEEDER, SF_JLPOC, SF_JLPON, SF_JLPOP, &
     & SF_JRPOC, SF_JRPON, SF_JRPOP, JNH4SF, JPO4SF, SODSF, JSASF, &
     & JSUSF, SF_SSI, SF_SU, SF_SA, SF_PIP, SFEED, SF_ALLOC, SF_DEALLOC !
  !
      Use MOD_BA, Only: BALGAE_CALC, BA_CALC, BANH4, BANO3, BAPO4, &
     & BADOC, BADO, DTPOC1_BA, DTPOC2_BA, DTPOC3_BA, DTPON1_BA, &
     & DTPON2_BA, DTPON3_BA, DTPOP1_BA, DTPOP2_BA, DTPOP3_BA, &
     & DTAG3CFL_BA, DTAG3NFL_BA, DTAG3PFL_BA, BA_ALLOC, BA_DEALLOC !
  !
      Use MOD_DF, Only: DFEEDER, DF_GROW_POC1, DF_GROW_POC2, DF_MORT, &
     & DF_RESP, DF_PRED, DF_SOD, DFEEDM1S, AMCN, AMCP, DF_CALC, &
     & DF_ALLOC, DF_DEALLOC !
!
      Use MOD_SED_DF_EXCHANGE_VARS, Only: POC1TM1S_SED_DF, &
     & POC2TM1S_SED_DF, M1_SED_DF, M2_SED_DF, SED_DF_EXCHANGE_ALLOC, &
     & SED_DF_EXCHANGE_DEALLOC !
  !
#  if defined (MULTIPROCESSOR)
      Use MOD_PAR, Only: NGID, NHN, HN_LST !		LIST OF HALO NODES
#  endif
  !
      Use MOD_SAV, Only: NSAVCELL, SAVCELL, NSAVSPC, LEAF, STEM, &
     & SAVFRAC, WSSSAV, WSLSAV, WSRSAV, WS1SAV, WS2SAV, WS3SAV, WSUSAV, &
     & SEDPOCSAV, FRPOCSAV, SEDPONSAV, FRPONSAV, SEDPOPSAV, FRPOPSAV, &
     & SEDNH4SAV, SEDNO3SAV, SEDPO4SAV, SAV_ALLOC, SAV_DEALLOC
  !
      Use MOD_SED_SAV_EXCHANGE_VARS, Only: SED_SAV_EXCHANGE_ALLOC, &
     & SED_SAV_EXCHANGE_DEALLOC
!
  !
  !
      Use MOD_SED !All variables in mod_sed are used
!
      Implicit None
!
      Real (SP) :: ELTMS, ELTMJD, TMEND
      Integer :: IINT
      Logical :: END_RUN
!
      Integer :: I, JG, IDEBUG_TEST, K
  !
#if defined (test2a)       /*steady state test case*/
  !
  !test correspondign to SedFlux_ver17b12_test2a.xlsm
  !
  !
  !initialize the variables used in the sediment module in the upper
  !modules
  !
  !
  !disable SAV, BA, DF, SF
      SAV_CALC = .False.
      BALGAE_CALC = .False.
      DFEEDER = .False.
      SFEEDER = .False.
      BENTHIC_OUTPUT = .True.
!
  !serial run on master processor
      SERIAL = .True.
      PAR = .False.
      MSR = .True.
!
  !
  !set dimension limits
  !
!
      MLOC = 1 !one node
      MTLOC = 1 !one local node
      KBM1 = 10 !10 vertical water column layers
      MGL = 1 !one node
      KWC = KBM1 !
!
  !
  !allocate hydrodynamic variables (no velocities needed, only depth and  box area)
  !
  !
  !		ART1	&!AREA OF NODE-BASE CONTROl VOLUME
  !		,DZ		&!DELTA-SIGMA VALUE of each layer
  !		,D		 !CURRENT DEPTH
!
  !WLong moved from cell_area.F to here
  !
      Allocate (ART1(0:MTLOC))
      ART1 = 0.0_SP !!AREA OF NODE-BASE CONTROl VOLUME
      Allocate (DZ(KBM1+1))
      DZ = 0.0_SP !!DELTA-SIGMA VALUE
      Allocate (D(0:MTLOC))
      D = 0.0
!
      ART1 = 100.d0 !100m^2
      D = 2.0 !2m deep
      DZ = 1.0 / KBM1 !0.1 as KBM1=10
!
  !
  !allocate water column water quality variables
  !
!
      SETTLING = .True.
!
  !settling rates
      Allocate (WSSNET(MTLOC))
      WSSNET = 0.0 !suspended sediments settling rate (m/d)
      Allocate (WSLNET(MTLOC))
      WSLNET = 0.0 !LPOM (labile detritus) settling rate (m/d)
      Allocate (WSRNET(MTLOC))
      WSRNET = 0.0 !RPOM (refractory detriuts) settling rate (m/d)
      Allocate (WS1NET(MTLOC))
      WS1NET = 0.0 !alg 1 settling rate (m/d)
      Allocate (WS2NET(MTLOC))
      WS2NET = 0.0 !alg 2 settling rate (m/d)
      Allocate (WS3NET(MTLOC))
      WS3NET = 0.0 !alg 3 settling rate (m/d)
      Allocate (WSUNET(MTLOC))
      WSUNET = 0.0 !particulate biogenic silica settling rate (m/d)
  !
  !water column constituents
      Allocate (T(0:MTLOC, KBM1))
      T = 0.0 !1
      Allocate (SALT(0:MTLOC, KBM1))
      SALT = 0.0 !2
      Allocate (SSI(0:MTLOC, KBM1))
      SSI = 0.0 !3
      Allocate (B1(0:MTLOC, KBM1))
      B1 = 0.0 !4
      Allocate (B2(0:MTLOC, KBM1))
      B2 = 0.0 !5
      Allocate (B3(0:MTLOC, KBM1))
      B3 = 0.0 !6
      Allocate (LPOC(0:MTLOC, KBM1))
      LPOC = 0.0 !11
      Allocate (RPOC(0:MTLOC, KBM1))
      RPOC = 0.0 !12
      Allocate (NH4(0:MTLOC, KBM1))
      NH4 = 0.0 !13
      Allocate (NO3(0:MTLOC, KBM1))
      NO3 = 0.0 !14
      Allocate (LPON(0:MTLOC, KBM1))
      LPON = 0.0 !18
      Allocate (RPON(0:MTLOC, KBM1))
      RPON = 0.0 !19
      Allocate (PO4(0:MTLOC, KBM1))
      PO4 = 0.0 !20
      Allocate (LPOP(0:MTLOC, KBM1))
      LPOP = 0.0 !23
      Allocate (RPOP(0:MTLOC, KBM1))
      RPOP = 0.0 !24
      Allocate (PIP(0:MTLOC, KBM1))
      PIP = 0.0 !25
      Allocate (COD(0:MTLOC, KBM1))
      COD = 0.0 !26
      Allocate (DOXG(0:MTLOC, KBM1))
      DOXG = 0.0 !27
      Allocate (SIUPB(0:MTLOC, KBM1))
      SIUPB = 0.0 !28
      Allocate (SIAT(0:MTLOC, KBM1))
      SIAT = 0.0 !29
  !
  !sediment temperature
      Allocate (CTEMP(MTLOC))
      CTEMP = 0.0
!
  !P to C ratio (gP/gC) of alg 1 2 and 3
      Allocate (Q1(MTLOC, KBM1))
      Q1 = 0.0
      Allocate (Q2(MTLOC, KBM1))
      Q2 = 0.0
      Allocate (Q3(MTLOC, KBM1))
      Q3 = 0.0
!
  !particle (C, N, P, Si, Suspedned Solids) fluxes
!
      Allocate (PCFWS(MTLOC))
      PCFWS = 0.0
      Allocate (PNFWS(MTLOC))
      PNFWS = 0.0
      Allocate (PPFWS(MTLOC))
      PPFWS = 0.0
      Allocate (PSFWS(MTLOC))
      PSFWS = 0.0
      Allocate (SSFWS(MTLOC))
      SSFWS = 0.0 !this is done in mod_wqm.F
  !
  !benthic fluxes to water column
!
      Allocate (BENCH4A(MTLOC))
      BENCH4A = 0.0
      Allocate (BENCH4G(MTLOC))
      BENCH4G = 0.0
      Allocate (BENCOD(MTLOC))
      BENCOD = 0.0
      Allocate (BENDEN(MTLOC))
      BENDEN = 0.0
      Allocate (BENDO(MTLOC))
      BENDO = 0.0
      Allocate (BENDOC(MTLOC))
      BENDOC = 0.0
      Allocate (BENNH4(MTLOC))
      BENNH4 = 0.0
      Allocate (BENNO3(MTLOC))
      BENNO3 = 0.0
      Allocate (BENPO4(MTLOC))
      BENPO4 = 0.0
      Allocate (BENSA(MTLOC))
      BENSA = 0.0
  !
  !JDAY,           &!
       !DLT,            &!			time step (sec)
  !B,              &!
       !BB,             &!
!
  !
  !allocate sediment module variables
  !
!
      Call SED_INIT !initialize sediment consituent names and allocate variables for sediment module
!
  !
  !allocate sav module variables
  !
      If (SAV_CALC) Then
         Call SAV_ALLOC
         Call SED_SAV_EXCHANGE_ALLOC
      End If
!
  !
  !allocate df module variables
  !
      If (DFEEDER) Then
         Call DF_ALLOC
         Call SED_DF_EXCHANGE_ALLOC
      End If
  !
  !
  !allocate sf module variables
  !
  !
      If (SFEEDER) Call SF_ALLOC
  !
  !allocate ba module variables
  !
      If (BALGAE_CALC) Call BA_ALLOC
  !
!
  !
  !input and output files for the sediment module
  !
!
!
  !file handles
      BFI = 1
      BFO = 12
!
  !file names
      BFIFN (1) = 'sedtest_2a.inp'
      BFOFN = 'sedtest_2a.chk'
  !
  !
  !set sediment control parameters or read the sediment module control
  !parameters from file BFI
  !
!
      Open (BFI, File=BFIFN(1), Status='OLD')
  !
      Call SED_READ !read some of the basic variables but it will be over written again afterwards
  !
  !particle (C, N, P, Si, Suspedned Solids) fluxes
!
      PCFWS = 0.0
      PNFWS = 0.0
      PPFWS = 0.0
      PSFWS = 0.0
      SSFWS = 0.0
!
  !
  !
  !driving POC, PON, POP, POS, SSI etc fluxes from water colum that should match Jcin, Jnin, Jpin, Jsin,
  !as well as overlying water concentrations
  !
!
  !
  !	TCE_ID	Date+time			Jcin		Jnin		Jpin		Jsin		O20			Depth	Tw		NH30	NO30	PO40	SI0			CH40		SALw
  !      							(gO2/m^2/d)	(gN/m^2/d)	(gP/m^2/d)	(gSi/m^2/d)	(mgO2/L)	(m)		(deg C)	(mgN/L)	(mgN/L)	(mgP/L)	(mgSi/L)	(mgO2/L)	(ppt)
  !		1	1/1/00 12:00 AM		0.300000	0.005000	0.003000	0.009375	5.0			2.00	15.0	0.015	0.100	0.004	0.216		0.0			30.0
  !
  !
  !
  !
  !Set water coumn variables for sediment diagenesis run
  !
  !	Fortran													Excel
  !------------------------------------------------------------------------------------
      T = 15.0 !1  	degC						15				degC
      CTEMPI = 15.0
      SALT = 30.0 !2		ppt							30				ppt
      SSI = 0.0 !3		gSolids/m^3					-
      B1 = 1.0 !4		gC/m^3						-
      B2 = 0.0 !5		gC/m^3						-
      B3 = 0.0 !6		gC/m^3						-
      LPOC = 0.0 !11		gC/m^3						-
      RPOC = 0.0 !12		gC/m^3						-
      NH4 = 0.015 !13		gN/m^3						0.015			mgN/L
      NO3 = 0.1 !14		gN/m^3						0.1				mgN/L
      LPON = 0.0 !18		gN/m^3						-
      RPON = 0.0 !19		gN/m^3						-
      PO4 = 0.004 !20		gP/m^3						0.004			mgP/L
      LPOP = 0.0 !23		gP/m^3						-
      RPOP = 0.0 !24		gP/m^3						-
      PIP = 0.0 !25		gP/m^3						-
      COD = 0.0 !26		gO2/m^3						-
      DOXG = 5.0 !27		gO2/m^3						5				mgO2/L
      SIUPB = 0.0 !28		gSi/m^3						0.0				mgSi/L (i.e. gSi/m^3)
      SIAT = 0.215625 !29		gSi/m^3						0.216			mgSi/L (i.e. gSi/m^3)
!
  !
  !parameters related to water colum
  !
  !
  !read from MRL (adsorption coefficients of PO4 and Silicate
      KADPO4 = 0.0004 !m^3/gSolids		!READ (MRL,1080)  KADPO4, KADSA
!
  !basically PIE1, see WLong notes (4-7-5) =PIE2PO4*DPIE2PO4=100*300 L/kg
  !=30000L/kg = 30m^3/kg=30/1000 (m^3/g) = 0.3 m^3/g
!
!
      KADSA = 0.001 !m^3/g		!basically PIE2, see WLong notes (4-6-7-1) where
  !m2 is SSI (gSolids/m^3), pie2= should have unit m^3/gSolids
  !i.e. 100L/kg ~ 0.1m^3/kg ~ 0.1m^3/(1000g) = 0.0001 m^3/gSi
  !here it is used to calculate equilibrium speciation of dissolved Si
  !and particulate Si based on suspended solids concentration (gSolids/m^3)
  !in water column
  !
  !Relation between m^3/g and 100L/kg :	0.0001m^3/g = 0.1m^3/kg = 100L/(1kg) = 100L/kg
  !
  !
  !for water column, there is plenty  of oxygen, according to WLong notes (4-6-5), we should use PIE1,Si
  !which is 100L/kg * DPIE1SI = 1000L/kg = 1m^3/1000g = 0.001m^3/g, where DPIE1SI=10 im DMD book page 573
  !
  !
  !	POM flux				Fortran	Formula							Excel						Excel Formula
  !-------------------------------------------------------------------------------------------------------------------
  !	Jcin 			1000*0.3/2.667=112.48		mgC/m^2/d			0.3000   gO2/m^2/d			JPOC(i) = Jcin*fpoc(i)
!
  !JPOC(I,1) = WB1NETMMD*FRCALG1(1)*B1(I,KWC)+										frpoc1=0.65
  !			WB2NETMMD*FRCALG2(1)*B2(I,KWC)+
  !			WB3NETMMD*FRCALG3(1)*B3(I,KWC)+
  !			WLPOMNETMMD*LPOC(I,KWC)
!
  !JPOC(I,2) = WB1NETMMD*FRCALG1(2)*B1(I,KWC)+        								frpoc2=0.2
  !			WB2NETMMD*FRCALG2(2)*B2(I,KWC)+
  !			WB3NETMMD*FRCALG3(2)*B3(I,KWC)+
  !			WRPOMNETMMD*RPOC(I,KWC)*FRPOC(I,2)/
  !			(FRPOC(I,2)+FRPOC(I,3))
!
  !JPOC(I,3) = WB1NETMMD*FRCALG1(3)*B1(I,KWC)+ 										frpoc3=0.15
  !			WB2NETMMD*FRCALG2(3)*B2(I,KWC)+
  !			WB3NETMMD*FRCALG3(3)*B3(I,KWC)+
  !			WRPOMNETMMD*RPOC(I,KWC)*FRPOC(I,3)/
  !				(FRPOC(I,2)+FRPOC(I,3))
!
  !	Jnin 			ANC*1000*0.005/2.667/FRNALG1(1)	mgN/m^2/d		0.005    gN/m^2/d			JPON(i) = Jnin * fpon(i)
  !JPON(I,1) = WB1NETMMD*ANC1*FRNALG1(1)*B1(I,KWC)+									frpon1=0.65
  !			WB2NETMMD*ANC2*FRNALG2(1)*B2(I,KWC)+
  !			WB3NETMMD*ANC3*FRNALG3(1)*B3(I,KWC)+
  !			WLPOMNETMMD*LPON(I,KWC)
  !
  !JPON(I,2) = WB1NETMMD*ANC1*FRNALG1(2)*B1(I,KWC)									frpon2=0.25
  !			+WB2NETMMD*ANC2*FRNALG2(2)*B2(I,KWC)
  !			+WB3NETMMD*ANC3*FRNALG3(2)*B3(I,KWC)
  !			+WRPOMNETMMD*RPON(I,KWC)*FRPON(I,2)/
  !			(FRPON(I,2)+FRPON(I,3))
  !
  !JPON(I,3) = WB1NETMMD*ANC1*FRNALG1(3)*B1(I,KWC) 									frpon3=0.1
  !			+WB2NETMMD*ANC2*FRNALG2(3)*B2(I,KWC)
  !			+WB3NETMMD*ANC3*FRNALG3(3)*B3(I,KWC)
  !			+WRPOMNETMMD*RPON(I,KWC)*FRPON(I,3)/
  !			(FRPON(I,2)+FRPON(I,3))
!
  !	Jpin 			Q*1000*0.003/2.667	mgP/m^2/d					0.003    gP/m^2/d			JPOP(i) = Jpin * fpop(i)
  !JPOP(I,1) = WB1NETMMD*Q1(I,KWC)*FRPALG1(1)*B1(I,KWC)+								frpop1=0.65
  !			WB2NETMMD*Q2(I,KWC)*FRPALG2(1)*B2(I,KWC)+
  !			WB3NETMMD*Q3(I,KWC)*FRPALG3(1)*B3(I,KWC)
  !			WLPOMNETMMD*LPOP(I,KWC)
!
  !JPOP(I,2) = WB1NETMMD*Q1(I,KWC)*FRPALG1(2)*B1(I,KWC)+								frpop2=0.2
  !			WB2NETMMD*Q2(I,KWC)*FRPALG2(2)*B2(I,KWC)+
  !			WB3NETMMD*Q3(I,KWC)*FRPALG3(2)*B3(I,KWC)+
  !			WRPOMNETMMD*RPOP(I,KWC)*FRPOP(I,2)/
  !			(FRPOP(I,2)+FRPOP(I,3))
!
  !JPOP(I,3) = WB1NETMMD*Q1(I,KWC)*FRPALG1(3)*B1(I,KWC)+								frpop3=0.15
  !			+WB2NETMMD*Q2(I,KWC)*FRPALG2(3)*B2(I,KWC)+
  !			+WB3NETMMD*Q3(I,KWC)*FRPALG3(3)*B3(I,KWC)+
  !			+WRPOMNETMMD*RPOP(I,KWC)*FRPOP(I,3)/
  !			(FRPOP(I,2)+FRPOP(I,3))
!
  !	Jsin 			ASC*1000*0.009375/2.667	mgSi/m^2/d					0.009375 gSi/m^2/d			POS2 = (Jsin * tc / H2 + POS2) /
  !																										(1 + KSI * THTASI ^ (Tw - 20) * tc + w2 * tc / H2)
  !JPOS(I) = 	 WB1NETMMD*ASC1*B1(I,KWC)
  !		  	+WB2NETMMD*ASC2*B2(I,KWC)
  !			+WB3NETMMD*ASC3*B3(I,KWC)
  !			+WPSINETMMD*SIUPB(I,KWC)
!
  !
  !POC distribution into G1,G2,G3 by ALG 1,2,3
  !G1 	Alg1				Alg2				Alg3
      FRCALG1 (1) = 0.65
      FRCALG2 (1) = 0.65
      FRCALG3 (1) = 0.65
!
  !G2
      FRCALG1 (2) = 0.20
      FRCALG2 (2) = 0.20
      FRCALG3 (2) = 0.20
!
  !G3
      FRCALG1 (3) = 0.15
      FRCALG2 (3) = 0.15
      FRCALG3 (3) = 0.15
!
!
  !PON distribution into G1,G2,G3 by ALG 1,2,3
  !G1 	Alg1				Alg2				Alg3
      FRNALG1 (1) = 0.65
      FRNALG2 (1) = 0.65
      FRNALG3 (1) = 0.65
!
  !G2
      FRNALG1 (2) = 0.25
      FRNALG2 (2) = 0.25
      FRNALG3 (2) = 0.25
!
  !G3
      FRNALG1 (3) = 0.10
      FRNALG2 (3) = 0.10
      FRNALG3 (3) = 0.10
!
  !POP distribution into G1,G2,G3 by ALG 1,2,3
  !G1 	Alg1				Alg2				Alg3
      FRPALG1 (1) = 0.65
      FRPALG2 (1) = 0.65
      FRPALG3 (1) = 0.65
!
  !G2
      FRPALG1 (2) = 0.20
      FRPALG2 (2) = 0.20
      FRPALG3 (2) = 0.20
!
  !G3
      FRPALG1 (3) = 0.15
      FRPALG2 (3) = 0.15
      FRPALG3 (3) = 0.15
!
  !The following values are given in order to match Jcin, Jnin, Jpin,Jsin in Excel version
      B1 = 1.0
  !only Alg1 settling contributes to POM in sediments
!
      B2 = 0.0
      B3 = 0.0
  !all other particulate organics set to zero
      LPOC = 0.0
      RPOC = 0.0
      LPON = 0.0
      RPON = 0.0
      LPOP = 0.0
      RPOP = 0.0
      SIUPB = 0.0
!
  !Only settling rates of B1 matters for JPOC
      WB1NETMMD = (0.3/2.667) * 1000.0 / B1 (1, KWC)!/FRCALG1(1)  !==>112.4859mm/d 	!here 0.3 is Jcin (gO2/m^2/d) in Excel version

      WB2NETMMD = 1000.0 !1000mm/d  	~1m/d
      WB3NETMMD = 1000.0 !1000mm/d	~1m/d
      WLPOMNETMMD = 1000.0 !1000mm/d	~1m/d
      WRPOMNETMMD = 1000.0 !1000mm/d  ~1m/d
      WPSINETMMD = 1000.0 !1000mm/d	~1m/d
!
  !reset WSLNET etc
      WSLBNET (1) = WLPOMNETMMD / 1000.
      WSRBNET (1) = WRPOMNETMMD / 1000.
      WS1BNET (1) = WB1NETMMD / 1000.0
      WS2BNET (1) = WB2NETMMD / 1000.0
      WS3BNET (1) = WB3NETMMD / 1000.0
      WSUBNET (1) = WPSINETMMD / 1000.0
!
      WSSBNET (1) = 1.0 !1m/d
!
  !ANC1, Q1, and ASC1 are adjusted to match Jnin, Jpin, Jsin based on Jcin
  !
  !redfield ratios of C,N,P for algae and detritus (modified to match Jnin, Jpin, Jsin of Excel vresion)
  !
  !JPON=ANC1*WB1NETMMD*B1*FRNALG1(1)/1000=0.005
!
  !
      ANC1 = (0.005) * 1000.0 / B1 (1, KWC) / WB1NETMMD !==>0.0444 !here 0.005 is Jnin (gN/m^2/d) in Excel version
  !ANC1    =	0.175 	!gN/gC
      ANC2 = 0.175
      ANC3 = 0.175
!
  !P to C ratio (gP/gC) of alg 1 2 and 3 (from AGR file)
      Q1 = (0.003) * 1000.0 / B1 (1, KWC) / WB1NETMMD !==>0.0267	!here 0.003 is Jpin (gP/m^2/d) in Excel version
  !Q1    	= 1/41.1		!see DMD book page 10 Table 1.1
      Q2 = 1 / 41.1 !see DMD book page 10 Table 1.1
      Q3 = 1 / 41.1 !see DMD book page 10 Table 1.1
  !
      ASC1 = (0.009375) * 1000.0 / B1 (1, KWC) / WB1NETMMD !==>0.0833	!here 0.009375 is Jsin in gSi/m^2/d in Excel version
  !ASC1	=	0.333 	!gSi/gC
      ASC2 = 0.333
      ASC3 = 0.333
  !
  !
  !set parameters related to SedFlux_ver17b12_test2a.xlsm
  !
!
  !the parameter lists in SedFlux_ver17b12_test2a.xlsm are listed as follows
  !-------------------------------------------------------------------------------------------------------------------------------------------------
  !parameter		parameter		parameter_meaning										unit			default_value		unit		equation
  !(excel)		(mod_sed.F)																(excel)				(excel)			(mod_sed)	(DMD book)
  !-------------------------------------------------------------------------------------------------------------------------------------------------
  !Particle mixing:
  !
  !m1				M1				solids concentration in aerobic layer 1					kgD/L			0.5					kg/L
  !m2				M2				solids concentration in anaerobic layer 2				kgD/L			0.5					kg/L
  !Dp				DPP	(VPMIX)		bioturbation particle mixing coefficient				m^2/d			0.00006				m^2/d
  !Dd				DDP	(VDMIX)		pore water diffusion coefficient						m^2/d			0.0025				m^2/d
  !w2				W2 (VSED)		deep burial velocity									m/d				6.85E-06			cm/yr *
  !H2				H2(HSED,HSEDALL)thickness of sediment anaerobic layer 2					m				0.1					cm *		*input is cm
  !
      M1 = 0.5
      M2 = 0.5
      DPP = 0.00006
      DDP = 0.0025
      HSEDALL = 10.0 !cm
      Do I = 1, MLOC
         HSED (I) = HSEDALL * 0.01 !m
         VSED (I) = 6.85E-06 !m/d  (W2)
         VPMIX (I) = 0.00006 !m^2/d
         VDMIX (I) = 0.0025 !m^2/d
      End Do
!
  !Reaction velocities (depth integrated reaction rate = reaction velocity (m/d))
  !
  !KappaNH3f		KAPPNH4F		freshwater nitrification velocity						m/d				0.131				m/d
  !KappaNH3s		KAPPNH4S		saltwater nitrification velocity						m/d				0.131				m/d
  !KappaNO3_1f	KAPPNO3F		freshwater denitrification velocity in layer 1			m/d				0.1					m/d
  !KappaNO3_1s	KAPPNO3S		saltwater denitrification velocity in layer 1			m/d				0.1					m/d
  !KappaNO3_2		K2NO3			denitrfication in the anaerobic layer 2					m/d				0.25				m/d
  !KappaCH4		KAPPCH4			methane oxidation in the aerobic layer 1				m/d				0.7					m/d
  !
      KAPPNH4F = 0.131 !m/d
      KAPPNH4S = 0.131 !m/d
      KAPPNO3F = 0.1 !m/d
      KAPPNO3S = 0.1 !m/d
      K2NO3 = 0.25 !m/d		!DMD P.105 Table 4.1
      KAPPCH4 = 0.7 !m/d
!
  !Half saturation constants
  !
  !KM_NH3			KMNH4			nitrification half saturation for NH4N					mgN/L			0.728				mgN/m^3	*
  !KM_O2_NH3		KMNH4O2			nitrification half saturation for O2					mgO2/L			0.37				mgO2/L
  !
      KMNH4 = 0.728 * 1000. !(mgN/m^3)		!see DMD book p73 Table 3.1 (b) (and Median)
  !maybe DMD book is wrong on units
      KMNH4O2 = 0.37 !mgO2/L			!See DMD book p73 Tabl3 3.1 (Median)
!
  !Partitioning coefficients
  !
  !KdNH3			PIENH4			partition coefficient for NH4 in layer 1 and 2			L/kgD			1					L/kg
  !KdPO42			PIE2PO4			partition coefficient for PO4 in layer 2				L/kgD			20					L/kg
  !dKDPO41f		DPIE1PO4F		freshwater factor that increases the aerobic 			unitless		20					L/kg
  !									layer partition coefficient of inorganic P
  !dKDPO41s		DPIE1PO4S		saltwater factor that increases the aerobic 			unitless		20					L/kg
  !									layer partition coefficient of inorganic P
  !O2critPO4		O2CRITPO4		critical O2 concentration in layer 1 for 				mgO2/L			2					mgO2/L
  !									adjustment of partition coefficient
  !									for inorganic P
  !
      PIENH4 = 1.0 !L/kg
      PIE2PO4 = 20.0 !L/kg
      DPIE1PO4F = 20.0 !WLong?? Unitless?	Yes! Note DMD book pp 572 says it should be about 300
      DPIE1PO4S = 20.0 !WLong?? Unitless?
      O2CRITPO4 = 2.0 !mgO2/L
!
  !Temperature coefficients
  !
  !ThtaDp			THTADP			temperature theta for bioturbation mixing between 		unitless		1.117				unitless
  !									layers 1 and 2
  !ThtaDd			THTADD			temperature theta for pore water diffusion between 		unitless		1.08				unitless
  !									layers 1 and 2
  !ThtaKmNH3		THTAKMNH4		temperature theta for nitrification half saturation 	unitless		1					unitless
  !									for NH4N
  !ThtaNH3		THTANH4			temperature theta for nitrification rate				unitless		1.123				unitless
  !ThtaNO3		THTANO3			temperature theta for denitrification rate				unitless		1.08				unitless
  !ThtaCH4		THTACH4			temperature theta for methane oxidation rate			unitless		1.079				unitless
  !
      THTADP = 1.117
      THTADD = 1.08
  !!ThtaKmNH3	=	!Applied to KM_NH3 in Excel, should be applied to KMNH4 in Fortran code
      THTAKMNH4 = 1.0 !not effective, since it value is 1 anyway
  !See DMD book p.73 Table 3.1 (median value is 1.125)
      THTANH4 = 1.123 !See DMD book p.73 Table 3.1 (median value is 1.123)
      THTANO3 = 1.08
      THTACH4 = 1.079
!
  !Salinity thresholds
  !
  !SALTSW			SALTSW			salinity above which sulfide rather than 				psu				1					ppt
  !									methane is produced from C diagenesis
  !SALTND			SALTND			salinity above which saltwater 							psu				1					ppt
  !									nitrification/denitrification rates are
  !									used for aerobic layer
      SALTSW = 1.0 !ppt
      SALTND = 1.0 !ppt
!
  !Sulfide constants
  !
  !KappaH2Sd1		KAPP1HSD		aerobic layer reaction velocity for dissolved 			m/d				0.2					m/d
  !								sulfide oxidation
  !KappaH2Sp1		KAPP1HSP		aerobic layer reaction velocity for particulate 		m/d				0.4					m/d
  !								sulfide oxidation
  !ThtaH2S		THTAH2S			temperature coefficient for sulfide oxidation			unitless		1.079				unitless
  !KMHSO2			KMHSO2			sulfide oxidation normalization constant for O2			mgO2/L			4					mgO2/L
  !KdH2S1			PIE1HS			partition coefficient for sulfide in aerobic layer 1	L/kgD			100					L/kg	*
  !KdH2S2			PIE2HS			partition coefficient for sulfide in anaerobic layer 2	L/kgD			100					L/kg	*
  !
      KAPP1HSD = 0.2 !m/d
      KAPP1HSP = 0.4 !m/d
      THTAH2S = 1.079
      KMHSO2 = 4.0 !mgO2/L
      PIE1HS = 100 !L/kg
      PIE2HS = 100 !L/kg
  !
!
  !Fractions of G classes 1 and 2 for settling RPON, RPOC, and RPOP
  !
  !frpon1			FRPON(:,1)		fraction of class 1 pon									unitless		0.65				unitless * only for detritus
  !frpon2			FRPON(:,2)		fraction of class 2 pon									unitless		0.25				unitless * only for detritus
!
  !frpoc1			FRPOC(:,1)		fraction of class 1 poc									unitless		0.65				unitless * only for detritus
  !frpoc2			FRPOC(:,2)		fraction of class 2 poc									unitless		0.2					unitless * only for detritus
!
  !frpop1			FRPOP(:,1)		fraction of class 1 pop									unitless		0.65				unitless * only for detritus
  !frpop2			FRPOP(:,2)		fraction of class 2 pop									unitless		0.2					unitless * only for detritus
!
  !*******WLong: Fortran code used this completely differently!!!!*****
  !Fortran code uses these as fraction of RPOM to be split in G2 and G3
  !G1 has no share of RPOM
!
      Do I = 1, MLOC
         FRPON (I, 1) = 0.0 !No share for G1
         FRPON (I, 2) = 0.65
         FRPON (I, 3) = 1 - FRPON (I, 2) - FRPON (I, 1)
!
         FRPOC (I, 1) = 0.0 !No share for G1
         FRPOC (I, 2) = 0.65
         FRPOC (I, 3) = 1 - FRPOC (I, 1) - FRPOC (I, 2)
!
         FRPOP (I, 1) = 0.0 !No share for G1
         FRPOP (I, 2) = 0.65
         FRPOP (I, 3) = 1 - FRPOP (I, 1) - FRPOP (I, 2)
     !
      End Do
!
  !Diagenesis rate constants for G clase 1, 2, and 3 N/C/P
  !
  !kpon1			KPON1			G class 1 pon mineralization							day^-1			0.035				1/d
  !kpon2			KPON2			G class 2 pon mineralization							day^-1			0.0018				1/d
  !kpon3			KPON3			G class 3 pon mineralization							day^-1			0					1/d
  !kpoc1			KPOC1			G class 1 poc mineralization							day^-1			0.035				1/d
  !kpoc2			KPOC2			G class 2 poc mineralization							day^-1			0.0018				1/d
  !kpoc3			KPOC3			G class 3 poc mineralization							day^-1			0					1/d
  !kpop1			KPOP1			G class 1 pop mineralization							day^-1			0.035				1/d
  !kpop2			KPOP2			G class 2 pop mineralization							day^-1			0.0018				1/d
  !kpop3			KPOP3			G class 3 pop mineralization							day^-1			0					1/d
  !
      KPON1 = 0.035 !1/d
      KPON2 = 0.0018 !1/d
      KPON3 = 0. !1/d
!
      KPOC1 = 0.035 !1/d
      KPOC2 = 0.0018 !1/d
      KPOC3 = 0. !1/d
!
      KPOP1 = 0.035 !1/d
      KPOP2 = 0.0018 !1/d
      KPOP3 = 0. !1/d
  !
  !Temperature coefficients for G class 1, 2, and 3 mineralization
  !
  !ThtaPON1		THTAPON1		temperature theta for G class 1 pon						unitless		1.1
  !ThtaPON2		THTAPON2		temperature theta for G class 2 pon						unitless		1.15
  !ThtaPON3		THTAPON3		temperature theta for G class 3 pon						unitless		1.17
  !ThtaPOC1		THTAPOC1		temperature theta for G class 1 poc						unitless		1.1
  !ThtaPOC2		THTAPOC2		temperature theta for G class 2 poc						unitless		1.15
  !ThtaPOC3		THTAPOC3		temperature theta for G class 3 poc						unitless		1.17
  !ThtaPOP1		THTAPOP1		temperature theta for G class 1 pop						unitless		1.1
  !ThtaPOP2		THTAPOP2		temperature theta for G class 2 pop						unitless		1.15
  !ThtaPOP3		THTAPOP3		temperature theta for G class 3 pop						unitless		1.17
  !
      THTAPON1 = 1.1
      THTAPON2 = 1.15
      THTAPON3 = 1.17
!
      THTAPOC1 = 1.1
      THTAPOC2 = 1.15
      THTAPOC3 = 1.17
!
      THTAPOP1 = 1.1
      THTAPOP2 = 1.15
      THTAPOP3 = 1.17
!
  !Parameters for partical mixing and benthic stress
  !
  !POC1R			POC1R			reference G1 at which w12base = Dp / H2 at 				mgO2/gD			0.2667				mgC/gD			*Not available
  !									20 degC for DiToro eqn 13.1
  !
  !kBEN_STR		KBENSTR 		first-order decay rate constant for benthic  			day^-1			0.03				1/d
  !									stress (d^-1) for DiToro eqn 13.3
  !
  !KM_O2_Dp		KMO2DP			particle mixing half-saturation constant 				mgO2/L			4					mgO2/L
  !									for O2 (mgO2/L)
!
  ! ??			KLBNTH			ratio of bio-irrigation to bioturbation					unitless
  ! ?? 			DPMIN			minimum particule mixing rate (m^2/day)
!
      POC1R = 0.1 !mgC/gSediment  DMD eqn (13.1) !0.2667/2.667
  !WLong notes eqn(3-26) (DMD book page 278, table 13.1, eqn(13.1))
      KBENSTR = 0.03 !1/d
      KMO2DP = 4.0 !mgO2/L  ~ gO2/m^3
!
      KLBNTH = 0.d0 !to make sure no bioturbation
      DPMIN = 0.d0 !to make minimum  particle diffusion rate zero
  !
  !Parameters for silica
  !
  !KSI			KSI				reaction rate for particulate biogenic Si 				day^-1			0.5					1/d
  !THTASI			THTASI			temperature theta for KSI								unitless		1.1					unitless
  !CSISAT			CSISATT20		saturation concentration for porewater Si				mgSi/L			40					mg Si/m3 	*
  !THTASISAT		THTASISAT		temperature theta for CSISAT							unitless		1.023							*Not available
  !DPIE1SI		DPIE1SI			incremental partition coefficient for Si in layer 1		L/kgD			10					L/kg
  !PIE2SI			PIE2SI			partition coefficient for Si in layer 2					L/kgD			100					L/kg
  !KMPSI			KMPSI			particulate biogenic Si half saturation 				mgSi/L			50000				mg Si/m3	*
  !									constant for dissolution
  !O2CRITSI		O2CRITSI		critical O2 concentration for layer 1 					mgO2/L			2					O2CRITSI
  !									incremental Si sorption
  !
      KSI = 0.5 !1/d
      THTASI = 1.1 !dimensionless		!DMD book p155 Tabel 7.1
      CSISATT20 = 40 * 1000.0 !mgSi/m^3
      THTASISAT = 1.023 !dimensionless
      DPIE1SI = 10 !L/kg (??)
      PIE2SI = 100 !L/kg
      KMPSI = 50000.0 * 1000 !mgSi/m^3			!DMD book p155  gives 19.8 to 100mg/L, P573 gives 50000mgSi/L ?
      O2CRITSI = 2.0 !mgO2/L ~ gO2/m^3 !used for calculating PIE1SI
  !
  !
  !Parameters for temperature
  !
      DIFFT = 1000.0 !m^2/sec      !used for calculating temperature in sediments
!
!
  !
  !initial conditions for sediment concentratins
  !
  !----------------------------------------------------------------------------------------------------------------------------------------------------------------
  !parameter		parameter		parameter_meaning										unit			initial_value		unit			IC			equation
  !(excel)		(mod_sed.F)																(excel)				(excel)			(mod_sed)		(fortran)	(DMD book)
  !---------------------------------------------------------------------------------------------------------------------------------------------------------------
  !
  !Particulate organic C, N, and P in layer 2
!
  !POC2(1)		CPOCI(1)		G class 1 POC in layer 2								gO2/m^3			89.446				mgC/m^3	*		(89.446/2.667)*1000
  !POC2(2)		CPOCI(2)		G class 2 POC in layer 2								gO2/m^3			622.783				mgC/m^3	*		(622.783/2.667)*1000
  !POC2(3)		CPOCI(3)		G class 3 POC in layer 2								gO2/m^3			6569.343			mgC/m^3	*		(6569.343/2.667)*1000
  !PON2(1)		CPONI(1)		G class 1 PON in layer 2								gN/m^3			1.491				mgN/m^3			1.491*1000
  !PON2(2)		CPONI(2)		G class 2 PON in layer 2								gN/m^3			12.975				mgN/m^3			12.975*1000
  !PON2(3)		CPONI(3)		G class 3 PON in layer 2								gN/m^3			72.993				mgN/m^3			72.993*1000
  !POP2(1)		CPOPI(1)		G class 1 POP in layer 2								gP/m^3			0.894				mgP/m^3			0.894*1000
  !POP2(2)		CPOPI(2)		G class 2 POP in layer 2								gP/m^3			6.228				mgP/m^3			6.228*1000
  !POP2(3)		CPOPI(3)		G class 3 POP in layer 2								gP/m^3			65.693				mgP/m^3			65.693*1000
  !POS2			CPOSI			Particulate biogenic Si in layer 2						gSi/m^3			0.302				mgSi/m^3		0.302*1000
  !
  !Sediment temperature and benthic stress
  !*				CTEMPI																					??					degC			15.0
  !*				BENSTI																					0.0									0.0
!
  !Dissolved constituents in layer 1 and 2 porewater
!
  !NH3(1)			*NH4T1I			Dissolved ammonia N in layer 1 porewater				mgN/L			0.024				mgN/m^3			0.024*1000
  !NH3(2)			NH4T2I			Dissolved ammonia N in layer 2 porewater				mgN/L			0.152				mgN/m^3			0.152*1000
  !NO3(1)			*NO3T1I			Dissolved nitrate+nitrite N in layer 1 porewater		mgN/L			0.041				mgN/m^3			0.041*1000
  !NO3(2)			NO3T2I			Dissolved nitrate+nitrite N in layer 2 porewater		mgN/L			0.007				mgN/m^3			0.007*1000
  !PO4(1)			*PO4T1I			Dissolved phosphate P in layer 1 porewater				mgP/L			0.054				mgP/m^3			0.054*1000
  !PO4(2)			PO4T2I			Dissolved phosphate P in layer 2 porewater				mgP/L			0.290				mgP/m^3			0.290*1000
  !Si(1)			*SIT1I			Dissolved Si in layer 1 porewater						mgSi/L			0.209				mgSi/m^3        0.209*1000
  !Si(2)			SIT2I			Dissolved Si in layer 2 porewater						mgSi/L			1.160				mgSi/m^3		1.160*1000
!
  !HS(1)			*HST1I			Dissolved sulfide in layer 1 porewater 					mgO2/L			0.002               gO2/m^3			0.002
  !									(used if salinity >= 1 psu)
  !HS(2)			HST2I			Dissolved sulfide in layer 2 porewater					mgO2/L			3.348				gO2/m^3			3.348
  !								 	(used if salinity >= 1 psu)
  !CH4(1)			CH41TI			Dissolved methane in layer 1 porewater 					mgO2/L			0.000				gO2/m^3			0.000
  !								 	(used if salinity >= 1 psu)
  !CH4(2)			CH4T2I			Dissolved methane in layer 2 porewater 					mgO2/L			0.000				gO2/m^3			0.000
  !									(used if salinity >= 1 psu)
  !**				SO4T2I																	mgO2/L			??					mgO2/L			0.000
  !*				SO4T2I																	mgO2/L			??					mgO2/L			0.000
!
!
      CPOCI (1) = 1000 * 89.446 / 2.667 !mgC/m^3
      CPOCI (2) = 1000 * 622.783 / 2.667 !mgC/m^3
      CPOCI (3) = 1000 * 6569.343 / 2.667 !mgC/m^3
      CPONI (1) = 1000 * 1.491 !mgN/m^3
      CPONI (2) = 1000 * 12.975 !mgN/m^3
      CPONI (3) = 1000 * 72.993 !mgN/m^3
      CPOPI (1) = 1000 * 0.894 !mgP/m^3
      CPOPI (2) = 1000 * 6.228 !mgP/m^3
      CPOPI (3) = 1000 * 65.693 !mgP/m^3
      BENSTI = 0.0 !unitless		!Fortran only ??
      CPOSI = 1000 * 0.302 !mgSi/m^3
!
  !
  !Note that in Excel version, concentrations are given as dissolved , then total concenration is calculated based on m1,m2 and PIENH4
  !
  !fd1 = (1# / (1# + m1 * KdNH3))
  !fd2 = (1# / (1# + m2 * KdNH3))
  !NH3Tp2(1) = NH3p2(1) / fd1
  !NH3Tp2(2) = NH3p2(2) / fd2
  !
  !m1=m2=0.5, PIENH4=1 ==> fd1=(1/(1+0.5*1))=1/1.5=0.3333
  !						fd2=(1/(1+0.5*1))=1/1.5=0.3333
  !
  !
      PIE1 = PIENH4
      PIE2 = PIENH4
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      NH41TI = 1000 * 0.024 !mgN/m^3		!Excel only
      NH4T2I = 1000 * (0.152) / FD2 !mgN/m^3
!
      NO31TI = 1000 * 0.041 !mgN/m^3		!Excel only
      NO3T2I = 1000 * 0.007 !mgN/m^3
!
!
  !also make sure PO4T2I is total PO4
      I = 1
!
      If (SALT(I, KBM1) < SALTND) Then
         DPIE1PO4 = DPIE1PO4F
      Else
         DPIE1PO4 = DPIE1PO4S
      End If
      If (O20 > O2CRITPO4) Then
         PIE1 = PIE2PO4 * DPIE1PO4
      Else
         PIE1 = PIE2PO4 * DPIE1PO4 ** (O20/O2CRITPO4)
      End If
      PIE2 = PIE2PO4
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      PO41TI = 1000 * 0.054 !mgP/m^3		!Excel only
      PO4T2I = 1000 * 0.290 / FD2 !converted to total value
!
      If (O20 > O2CRITSI) Then
         PIE1 = PIE2SI * DPIE1SI
      Else
         PIE1 = PIE2SI * DPIE1SI ** (O20/O2CRITSI)
      End If
      PIE2 = PIE2SI
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      SI1TI = 1000 * 0.209 !mgSi/m^3		!Excel only
      SIT2I = 1000 * 1.160 / FD2 !mgSi/m^3		!converted to total Si in layer 2
!
!
      PIE1 = PIE1HS
      PIE2 = PIE2HS
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      HS1TI = 0.002 !mgO2/L			!Excel only
      HST2I = 3.348 / FD2 !mgO2/L			!converted to total HS
!
      CH4T2I = 0.0 !mgO2/L
      CH41TI = 0.0 !mgO2/L
!
      SO41TI = 0.0 !mgO2/L			!Neither Excel Nor Fortran
      SO4T2I = 0.0 !mgO2/L			!Fortran only
  !
      SODI = 0.1 !mgO2/m^2/d		!Initialize SOD !WLong: may need to call this SODI
  !Greg Pelletier's Excel used JSOD=0.1 mgO2/m^2/d
      Do I = 1, MLOC
!
         CTEMP (I) = CTEMPI !Sediment temperature
!
         Do JG = 1, 3
            CPOC (I, JG) = CPOCI (JG)
            CPON (I, JG) = CPONI (JG)
            CPOP (I, JG) = CPOPI (JG)
         End Do
!
         CPOS (I) = CPOSI
         BENSTRTM1S (I) = BENSTI !*not in excel
         SODTM1S (I) = SODI
         NH41TM1S (I) = NH41TI
         NH4T2TM1S (I) = NH4T2I
!
         NO31TM1S (I) = NO31TI
         NO3T2TM1S (I) = NO3T2I
!
         PO41TM1S (I) = PO41TI
         PO4T2TM1S (I) = PO4T2I
!
         SI1TM1S (I) = SI1TI
         SIT2TM1S (I) = SIT2I
!
         HS1TM1S (I) = HS1TI
         HST2TM1S (I) = HST2I
!
         CH41TM1S (I) = CH41TI
         CH4T2TM1S (I) = CH4T2I
!
         SO4T2TM1S (I) = SO4T2I !*not in excel
     !
      End Do
!
!
  !reset the intial values of varables that involve some of the above parameters
      Call SED_INIT2
  !
  !open output file
!
      BFOFN = 'sedtest_2a.csv'
  !
      If (MSR) Then
!
         Open (Unit=BFO, File=BFOFN)
         Rewind (BFO)
!
     !
     !write the headline the same as excel file "SedFlux_ver17b12_test2a.xlsm"
     !
!
     !Segment ID	JDAY+Time	Jcin	Jnin	Jpin	Jsin	O20	Depth	Tw	NH30	NO30	PO40	SI0	CH40	SALw
     !				SOD	Jnh4	Jno3	JDenitT	Jch4	Jch4g	Jhs	Jpo4	Jsi	Jcout	Jnout	Jpout	NH3(1)	NH3(2)
     !				NO3(1)	NO3(2)	PO4(1)	PO4(2)	Si(1)	Si(2)	CH4(1)	CH4(2)	HS(1)	HS(2)	POC2(1)	POC2(2)	POC2(3)
     !				PON2(1)	PON2(2)	PON2(3)	POP2(1)	POP2(2)	POP2(3)	POS2 H1	BEN_STR
     !
         I = 1
         Write (BFO, 1001) 'SegmentID', ',', 'JDAY', ',', 'Jcin', ',', 'Jnin', ',', 'Jpin', ',', 'Jsin', ',', 'O20', ',', 'Depth', ',', 'Tw', ',', 'NH40', ',', 'NO30', ',', 'PO40', ',', 'SI0', ',', 'CH40', ',', 'SALw', ',', 'SOD', ',', 'Jnh4', ',', 'Jno3',&
        &  ',', 'JDenitT', ',', 'Jch4', ',', 'Jch4g', ',', 'Jhs', ',', 'Jpo4', ',', 'Jsi', ',', 'Jcout', ',', 'Jnout', ',', 'Jpout', ',', 'NH3(1)', ',', 'NH3(2)', ',', 'NO3(1)', ',', 'NO3(2)', ',', 'PO4(1)', ',', 'PO4(2)', ',', 'Si(1)', ',', 'Si(2)', ',', &
        & 'CH4(1)', ',', 'CH4(2)', ',', 'HS(1)', ',', 'HS(2)', ',', 'POC2(1)', ',', 'POC2(2)', ',', 'POC2(3)', ',', 'PON2(1)', ',', 'PON2(2)', ',', 'PON2(3)', ',', 'POP2(1)', ',', 'POP2(2)', ',', 'POP2(3)', ',', 'POS2', ',', 'H1', ',', 'BEN_STR'
!
      End If
1000  Format (I10, 1000(1 X, A10, F15.8))
1001  Format (A10, 1000(1 X, A10, A15))
  !
  !do test 2a calculations
!
      TMSTRT = 0.0 !reference time
      TMEND = 70.0 !700.0		!end time
      JDAY = 0.0 !time in days
      DLT = 0.01 * 86400 !10.0		!time step in sec = 864sec=0.01 day
      ELTMS = 0.0 !time in sec
      ELTMJD = 0.0 !time in days after reference time
      DLTS = DLT / 86400.
!
      END_RUN = .False.
!
      IINT = 0 !time step counter
!
      Do WHILE ( .Not. END_RUN)
     !
         If (MSR) WRITE (*,*) 'JDAY=', JDAY
!
         Call POM_ACCUMUL
     !
     !average the accumulated POM flux over NDTSED steps
         Do I = 1, MLOC
            Do K = 1, 3
               JPOC (I, K) = JPOCaccum (I, K)
               JPON (I, K) = JPONaccum (I, K)
               JPOP (I, K) = JPOPaccum (I, K)
            End Do
            JPOS (I) = JPOSaccum (I)
         End Do
     !
         Call SED_CALC (.True.)!calculate with state state solution
     !
     !set accumulative fluxes back to zero
         JPOCaccum = 0.0
         JPONaccum = 0.0
         JPOPaccum = 0.0
         JPOSaccum = 0.0
     !
!
     !
     !write out outputs
     !
         If (MSR) Then
        !Fortran											Excel				FortranUnit			ExcelUnit
            I = 1
            Write (BFO, 1000) I, ',', JDAY, ',', (JPOC(I, 1)+JPOC(I, 2)+&
           & JPOC(I, 3)) / 1000.0 * 2.667, ',', (JPON(I, 1)+JPON(I, 2)+&
           & JPON(I, 3)) / 1000.0, ',', (JPOP(I, 1)+JPOP(I, 2)+JPOP(I, &
           & 3)) / 1000.0, ',', JPOS (I) / 1000.0, ',', O20, ',', D &
           & (I), ',', T (I, KBM1), ',', NH40 / 1000, ',', NO30 / 1000, &
           & ',', PO40 / 1000, ',', SI0 / 1000, ',', CH40, ',', SAL, ',&
           &', SOD, ',', JNH4 / 1000, ',', JNO3 / 1000, ',', BENDEN &
           & (1), ',', JCH4, ',', JCH4G, ',', JHS, ',', JPO4 / 1000, ',&
           &', JSI / 1000, ',', 0.0, ',', 0.0, ',', 0.0, ',', NH41 / &
           & 1000, ',', NH42 / 1000, ',', NO31 / 1000, ',', NO32 / &
           & 1000, ',', PO41 / 1000, ',', PO42 / 1000, ',', SI1 / 1000, &
           & ',', SI2 / 1000, ',', CH41, ',', CH42, ',', HS1, ',', HS2, &
           & ',', POC1 / 1000 * 2.667, ',', POC2 / 1000 * 2.667, ',', &
           & POC3 / 1000 * 2.667, ',', PON1 / 1000, ',', PON2 / 1000, '&
           &,', PON3 / 1000, ',', POP1 / 1000, ',', POP2 / 1000, ',', &
           & POP3 / 1000, ',', PSISED / 1000, ',', HSED1 (I) * 1000.0, &
           & ',', BENSTR !'BEN_STR' 			dimensionless		dimensionless
        ! WRITE(BFO,1000)I,',',JDAY,			&
        !	',',(JPOC(I,1)+JPOC(I,2)+JPOC(I,3))/1000.0*2.667,	&	!'Jcin'				mgC/m^2/d			gO2/m^2/d
        !	',',(JPON(I,1)+JPON(I,2)+JPON(I,3))/1000.0,	&			!'Jnin'				*mgN/m^2/d			gN/m^2/d
        !	',',(JPOP(I,1)+JPOP(I,2)+JPOP(I,3))/1000.0,	&			!'Jpin'				*mgP/m^2/d			gP/m^2/d
        !	',',JPOS(I)/1000.0,	&									!'Jsin		 		*mgSi/m^2/d			gSi/m^2/d
        !	',',O20,			&									!'O20'				mgO2/L				mgO2/L
        !	',',D(I),			&									!'Depth'			m					m
        !	',',T(I,KBM1),		&									!'Tw'				degC				degC
        !	',',NH40/1000,		&									!'NH30'				*mgN/m^3			mgN/L
        !	',',NO30/1000,		&									!'NO30'				*mgN/m^3			mgN/L
        !	',',PO40/1000,		&									!'PO40'				*mgP/m^3			mgP/L
        !	',',SI0/1000,		&									!'SI0'				*mgSi/m^3 			mgSi/L				(dissiolved SIAT)
        !	',',CH40,			&									!'CH40'				gO2/m^3				mgO2/L
        !	',',SAL,			&									!'SALw'  			ppt					ppt					!i.e. SALT(I,KWC), ppt
        !	',',SOD,			&									!'SOD'				gO2/m^2/day			gO2/m^2/d
        !	',',JNH4/1000,		&									!'Jnh4				*mgN/m^2/day		gN/m^2/d
        !	',',JNO3/1000,		&									!'Jno3				*mgN/m^2/day		gN/m^2/d
        !	',',BENDEN(1),		&									!'JDenitT'			gN/m^2/day			gN/m^2/d
        !							!JDenit(1) = Denit(1) * NO3(1)
        !							!JDenit(2) = Denit(2) * NO3(2)
        !							!JDenitT = JDenit(1) + JDenit(2)
        !	',',JCH4,			&									!'Jch4'				gO2/m^2/day			gO2/m^2/d
        !	',',JCH4G,			&									!'Jch4g'			gO2/m^2/day			gO2/m^2/d
        !	',',JHS,			&									!'Jhs'				gO2/m^2/day			gO2/m^2/d
        !	',',JPO4/1000,		&									!'Jpo4'				*mgP/m^2/day		gP/m^2/d
        !	',',JSI/1000,		&									!'Jsi'				*mgSi/m^2/day		gSi/m^2/d
        !	',',0.0,			&									!'Jcout'
        !	',',0.0,			&									!'Jnout'
        !	',',0.0,			&									!'Jpout'
        !         !
        ! Excel calculations here, should correspond to Jcin, Jnin and Jpin
        !Jcout = SOD + JCH4 + JCH4g + JHS +  	(POC2(1) + POC2(2) + POC2(3)) * w2 +
        !						(		(POC2(1) + POC2(2) + POC2(3))
        !							- 	(POC22(1) + POC22(2) + POC22(3))
        !						) * H2 / tc
        !Jnout = JNO3 + JNH4 + JDenitT + 		(PON2(1) + PON2(2) + PON2(3)) * w2 +
        !						(		(PON2(1) + PON2(2) + PON2(3))
        !							- 	(PON22(1) + PON22(2) + PON22(3))
        !						) * H2 / tc
        !Jpout = JPO4 + 						(POP2(1) + POP2(2) + POP2(3)) * w2 +
        !						((POP2(1) + POP2(2) + POP2(3))
        !						 - (POP22(1) + POP22(2) + POP22(3))
        !						 ) * H2 / tc
        !
        !	',',NH41/1000,		&					!'NH3(1)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',NH42/1000,		&					!'NH3(2)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',NO31/1000,		&					!'NO3(1)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',NO32/1000,		&					!'NO3(2)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',PO41/1000,		&					!'PO4(1)'			*mgP/m^3 			mgP/L	(dissolved)
        !	',',PO42/1000,		&					!'PO4(2)'			*mgP/m^3 			mgP/L	(dissolved)
        !	',',SI1/1000,		&					!'Si(1)'			*mgSi/m^3 			mgSi/L	(dissolved)
        !	',',SI2/1000,		&					!'Si(2)'			*mgSi/m^3 			mgSi/L	(dissolved)
        !	',',CH41,			&					!'CH4(1)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',CH42,			&					!'CH4(2)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',HS1,			&					!'HS(1)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',HS2,			&					!'HS(2)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',POC1/1000*2.667,&					!'POC2(1)'			*mgC/m^3 			gO2/m^3	(G1 of POC in layer 2)
        !	',',POC2/1000*2.667,&					!'POC2(2)' 			*mgC/m^3 			gO2/m^3	(G2 of POC in layer 2)
        !	',',POC3/1000*2.667,&					!'POC2(3)' 			*mgC/m^3 			gO2/m^3	(G3 of POC in layer 2)
        !	',',PON1/1000,		&					!'PON2(1)'			*mgN/m^3 			gN/m^3	(G1 of PON in layer 2)
        !	',',PON2/1000,		&					!'PON2(2)' 			*mgN/m^3 			gN/m^3	(G2 of PON in layer 2)
        !	',',PON3/1000,		&					!'PON2(3)' 			*mgN/m^3 			gN/m^3	(G3 of PON in layer 2)
        !	',',POP1/1000,		&					!'POP2(1)'			*mgP/m^3 			gP/m^3	(G1 of POP in layer 2)
        !	',',POP2/1000,		&					!'POP2(2)'			*mgP/m^3 			gP/m^3	(G2 of POP in layer 2)
        !	',',POP3/1000,		&					!'POP2(3)' 			*mgP/m^3 			gP/m^3	(G3 of POP in layer 2)
        !	',',PSISED/1000,	&					!'POS2'				*mgSi/m^3 			gSi/m^3	(particulate biogenic Silicate in layer2)
        !	',',HSED1(I)*1000.0,&					!'H1'				m					mm
        !	',',BENSTR			 					!'BEN_STR' 			dimensionless		dimensionless
        !
            Write (*,*) 1, ',', JDAY
            Write (*,*) 'JPOC=,', (JPOC(I, 1)+JPOC(I, 2)+JPOC(I, 3)) / &
           & 1000.0 * 2.667, 'gO2/m^2/d' !'Jcin'				mgC/m^2/d			gO2/m^2/d
            Write (*,*) 'JPON=,', (JPON(I, 1)+JPON(I, 2)+JPON(I, 3)) / &
           & 1000.0, 'gN/m^2/d' !'Jnin'				*mgN/m^2/d			gN/m^2/d
            Write (*,*) 'JPOP=,', (JPOP(I, 1)+JPOP(I, 2)+JPOP(I, 3)) / &
           & 1000.0, 'gP/m^2/d' !'Jpin'				*mgP/m^2/d			gP/m^2/d
            Write (*,*) 'JPOS=,', JPOS (I) / 1000.0, 'gSi/m^2/d' !'Jsin		 		*mgSi/m^2/d			gSi/m^2/d
            Write (*,*) 'O20=,', O20 !,			&					!'O20'				mgO2/L				mgO2/L
            Write (*,*) 'D=,', D (I)!,			&					!'Depth'			m					m
            Write (*,*) 'T=,', T (I, KBM1)!,		&					!'Tw'				degC				degC
            Write (*,*) 'NH40=,', NH40 / 1000 !,	&					!'NH30'				*mgN/m^3			mgN/L
            Write (*,*) 'NO30=,', NO30 / 1000 !,	&					!'NO30'				*mgN/m^3			mgN/L
            Write (*,*) 'PO40=,', PO40 / 1000 !,	&					!'PO40'				*mgP/m^3			mgP/L
            Write (*,*) 'SI0=,', SI0 / 1000 !,		&					!'SI0'				*mgSi/m^3 			mgSi/L				(dissiolved SIAT)
            Write (*,*) 'CH40=,', CH40 !,			&					!'CH40'				gO2/m^3				mgO2/L
            Write (*,*) 'SAL=,', SAL !,			&					!'SALw'  			ppt					ppt					!i.e. SALT(I,KWC), ppt
            Write (*,*) 'SOD=,', SOD !,			&					!'SOD'				gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JNH4=,', JNH4 / 1000 !,	&					!'Jnh4				*mgN/m^2/day		gN/m^2/d
            Write (*,*) 'JNO3=,', JNO3 / 1000 !,	&					!'Jno3				*mgN/m^2/day		gN/m^2/d
            Write (*,*) 'JDeniT=,', BENDEN (1)!,	&					!'JDenitT'			gN/m^2/day			gN/m^2/d
        !JDenit(1) = Denit(1) * NO3(1)
        !JDenit(2) = Denit(2) * NO3(2)
        !JDenitT = JDenit(1) + JDenit(2)
            Write (*,*) 'JCH4=,', JCH4 !,			&					!'Jch4'				gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JCH4G=,', JCH4G !,		&					!'Jch4g'			gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JHS=,', JHS !,			&					!'Jhs'				gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JPO4=,', JPO4 / 1000 !,	&					!'Jpo4'				*mgP/m^2/day		gP/m^2/d
            Write (*,*) 'JSI=,', JSI / 1000 !,		&					!'Jsi'				*mgSi/m^2/day		gSi/m^2/d
!
        !						',',0.0,				&					!'Jcout'
        !						',',0.0,				&					!'Jnout'
        !						',',0.0,				&					!'Jpout'
!
        !
        !WLong: Excel calculations here, should correspond to Jcin, Jnin and Jpin
        !Jcout = SOD + JCH4 + JCH4g + JHS +  	(POC2(1) + POC2(2) + POC2(3)) * w2 +
        !										(		(POC2(1) + POC2(2) + POC2(3))
        !											- 	(POC22(1) + POC22(2) + POC22(3))
        !										) * H2 / tc
        !Jnout = JNO3 + JNH4 + JDenitT + 		(PON2(1) + PON2(2) + PON2(3)) * w2 +
        !										(		(PON2(1) + PON2(2) + PON2(3))
        !											- 	(PON22(1) + PON22(2) + PON22(3))
        !										) * H2 / tc
        !Jpout = JPO4 + 						(POP2(1) + POP2(2) + POP2(3)) * w2 +
        !										((POP2(1) + POP2(2) + POP2(3))
        !										 - (POP22(1) + POP22(2) + POP22(3))
        !										 ) * H2 / tc
        !
            Write (*,*) 'NH41=,', NH41 / 1000 !,		&					!'NH3(1)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'NH42=,', NH42 / 1000 !,		&					!'NH3(2)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'NO31=,', NO31 / 1000 !,		&					!'NO3(1)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'NO32=,', NO32 / 1000 !,		&					!'NO3(2)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'PO41=,', PO41 / 1000 !,		&					!'PO4(1)'			*mgP/m^3 			mgP/L	(dissolved)
            Write (*,*) 'PO42=,', PO42 / 1000 !,		&					!'PO4(2)'			*mgP/m^3 			mgP/L	(dissolved)
            Write (*,*) 'SI1=,', SI1 / 1000 !,			&					!'Si(1)'			*mgSi/m^3 			mgSi/L	(dissolved)
            Write (*,*) 'SI2=,', SI2 / 1000 !,			&					!'Si(2)'			*mgSi/m^3 			mgSi/L	(dissolved)
            Write (*,*) 'CH41=,', CH41 !,				&					!'CH4(1)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'CH42=,', CH42 !,				&					!'CH4(2)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'SO41=,', SO41 !,				&					!'SO4(1)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'SO42=,', SO42 !,				&					!'SO4(2)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'HSO4=,', HSO4 !,				&					!'HSO4'				m	 				m
            Write (*,*) 'HS1=,', HS1 !,				&					!'HS(1)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'HS2=,', HS2 !,				&					!'HS(2)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'POC1=,', POC1 / 1000 * 2.667 !,	&					!'POC2(1)'			*mgC/m^3 			gO2/m^3	(G1 of POC in layer 2)
            Write (*,*) 'POC2=,', POC2 / 1000 * 2.667 !,	&					!'POC2(2)' 			*mgC/m^3 			gO2/m^3	(G2 of POC in layer 2)
            Write (*,*) 'POC3=,', POC3 / 1000 * 2.667 !,	&					!'POC2(3)' 			*mgC/m^3 			gO2/m^3	(G3 of POC in layer 2)
            Write (*,*) 'PON1=,', PON1 / 1000 !,		&					!'PON2(1)'			*mgN/m^3 			gN/m^3	(G1 of PON in layer 2)
            Write (*,*) 'PON2=,', PON2 / 1000 !,		&					!'PON2(2)' 			*mgN/m^3 			gN/m^3	(G2 of PON in layer 2)
            Write (*,*) 'PON3=,', PON3 / 1000 !,		&					!'PON2(3)' 			*mgN/m^3 			gN/m^3	(G3 of PON in layer 2)
            Write (*,*) 'POP1=,', POP1 / 1000 !,		&					!'POP2(1)'			*mgP/m^3 			gP/m^3	(G1 of POP in layer 2)
            Write (*,*) 'POP2=,', POP2 / 1000 !,		&					!'POP2(2)'			*mgP/m^3 			gP/m^3	(G2 of POP in layer 2)
            Write (*,*) 'POP3=,', POP3 / 1000 !,		&					!'POP2(3)' 			*mgP/m^3 			gP/m^3	(G3 of POP in layer 2)
            Write (*,*) 'PSISED=,', PSISED / 1000 !,	&					!'POS2'				*mgSi/m^3 			gSi/m^3	(particulate biogenic Silicate in layer2)
            Write (*,*) 'CTEMP=', CTEMP (I)
            Write (*,*) 'H1=,', HSED1 (I) * 1000 !,			&					!'H1'				m					mm
            Write (*,*) 'BENSTR=,', BENSTR !'BEN_STR' 			dimensionless		dimensionless
        !
        !		READ(*,*)
         End If
!
         IINT = IINT + 1
         ELTMS = ELTMS + DLT
         JDAY = ELTMS / 86400.
         ELTMJD = JDAY - TMSTRT
     !
         If (JDAY .Ge. TMEND) END_RUN = .True.
!
      End Do
!
  !
  !close files
  !
      Close (BFI)
      Close (BFO)
!
  !deallocate sediment module variables
!
      Call SED_DEALLOC
  !
  !
  !deallocate sav module variables
  !
      If (SAV_CALC) Then
         Call SAV_DEALLOC
         Call SED_SAV_EXCHANGE_DEALLOC
      End If
!
  !
  !deallocate df module variables
  !
      If (DFEEDER) Then
         Call DF_DEALLOC
         Call SED_DF_EXCHANGE_DEALLOC
      End If
  !
  !deallocate sf module variables
  !
  !
      If (SFEEDER) Call SF_DEALLOC
  !
  !deallocate ba module variables
  !
      If (BALGAE_CALC) Call BA_DEALLOC
!
      Stop 'Finished test2a!'
  !
#endif
!
!
#if defined (test2aChannel)       /*steady state test case*/
  !
  !test correspondign to SedFlux_ver17b12_test2a.xlsm
  !
  !
  !initialize the variables used in the sediment module in the upper
  !modules
  !
  !
  !disable SAV, BA, DF, SF
      SAV_CALC = .False.
      BALGAE_CALC = .False.
      DFEEDER = .False.
      SFEEDER = .False.
      BENTHIC_OUTPUT = .True.
!
  !serial run on master processor
      SERIAL = .True.
      PAR = .False.
      MSR = .True.
!
  !
  !set dimension limits
  !
!
      MLOC = 1 !one node
      MTLOC = 1 !one local node
      KBM1 = 10 !10 vertical water column layers
      MGL = 1 !one node
      KWC = KBM1 !
!
  !
  !allocate hydrodynamic variables (no velocities needed, only depth and  box area)
  !
  !
  !		ART1	&!AREA OF NODE-BASE CONTROl VOLUME
  !		,DZ		&!DELTA-SIGMA VALUE of each layer
  !		,D		 !CURRENT DEPTH
!
  !WLong moved from cell_area.F to here
  !
      Allocate (ART1(0:MTLOC))
      ART1 = 0.0_SP !!AREA OF NODE-BASE CONTROl VOLUME
      Allocate (DZ(KBM1+1))
      DZ = 0.0_SP !!DELTA-SIGMA VALUE
      Allocate (D(0:MTLOC))
      D = 0.0
!
      ART1 = 100.d0 !100m^2
      D = 2.0 !2m deep
      DZ = 1.0 / KBM1 !0.1 as KBM1=10
!
  !
  !allocate water column water quality variables
  !
!
      SETTLING = .True.
!
  !settling rates
      Allocate (WSSNET(MTLOC))
      WSSNET = 0.0 !suspended sediments settling rate (m/d)
      Allocate (WSLNET(MTLOC))
      WSLNET = 0.0 !LPOM (labile detritus) settling rate (m/d)
      Allocate (WSRNET(MTLOC))
      WSRNET = 0.0 !RPOM (refractory detriuts) settling rate (m/d)
      Allocate (WS1NET(MTLOC))
      WS1NET = 0.0 !alg 1 settling rate (m/d)
      Allocate (WS2NET(MTLOC))
      WS2NET = 0.0 !alg 2 settling rate (m/d)
      Allocate (WS3NET(MTLOC))
      WS3NET = 0.0 !alg 3 settling rate (m/d)
      Allocate (WSUNET(MTLOC))
      WSUNET = 0.0 !particulate biogenic silica settling rate (m/d)
  !
  !water column constituents
      Allocate (T(0:MTLOC, KBM1))
      T = 0.0 !1
      Allocate (SALT(0:MTLOC, KBM1))
      SALT = 0.0 !2
      Allocate (SSI(0:MTLOC, KBM1))
      SSI = 0.0 !3
      Allocate (B1(0:MTLOC, KBM1))
      B1 = 0.0 !4
      Allocate (B2(0:MTLOC, KBM1))
      B2 = 0.0 !5
      Allocate (B3(0:MTLOC, KBM1))
      B3 = 0.0 !6
      Allocate (LPOC(0:MTLOC, KBM1))
      LPOC = 0.0 !11
      Allocate (RPOC(0:MTLOC, KBM1))
      RPOC = 0.0 !12
      Allocate (NH4(0:MTLOC, KBM1))
      NH4 = 0.0 !13
      Allocate (NO3(0:MTLOC, KBM1))
      NO3 = 0.0 !14
      Allocate (LPON(0:MTLOC, KBM1))
      LPON = 0.0 !18
      Allocate (RPON(0:MTLOC, KBM1))
      RPON = 0.0 !19
      Allocate (PO4(0:MTLOC, KBM1))
      PO4 = 0.0 !20
      Allocate (LPOP(0:MTLOC, KBM1))
      LPOP = 0.0 !23
      Allocate (RPOP(0:MTLOC, KBM1))
      RPOP = 0.0 !24
      Allocate (PIP(0:MTLOC, KBM1))
      PIP = 0.0 !25
      Allocate (COD(0:MTLOC, KBM1))
      COD = 0.0 !26
      Allocate (DOXG(0:MTLOC, KBM1))
      DOXG = 0.0 !27
      Allocate (SIUPB(0:MTLOC, KBM1))
      SIUPB = 0.0 !28
      Allocate (SIAT(0:MTLOC, KBM1))
      SIAT = 0.0 !29
  !
  !sediment temperature
      Allocate (CTEMP(MTLOC))
      CTEMP = 0.0
!
  !P to C ratio (gP/gC) of alg 1 2 and 3
      Allocate (Q1(MTLOC, KBM1))
      Q1 = 0.0
      Allocate (Q2(MTLOC, KBM1))
      Q2 = 0.0
      Allocate (Q3(MTLOC, KBM1))
      Q3 = 0.0
!
  !particle (C, N, P, Si, Suspedned Solids) fluxes
!
      Allocate (PCFWS(MTLOC))
      PCFWS = 0.0
      Allocate (PNFWS(MTLOC))
      PNFWS = 0.0
      Allocate (PPFWS(MTLOC))
      PPFWS = 0.0
      Allocate (PSFWS(MTLOC))
      PSFWS = 0.0
      Allocate (SSFWS(MTLOC))
      SSFWS = 0.0 !this is done in mod_wqm.F
  !
  !benthic fluxes to water column
!
      Allocate (BENCH4A(MTLOC))
      BENCH4A = 0.0
      Allocate (BENCH4G(MTLOC))
      BENCH4G = 0.0
      Allocate (BENCOD(MTLOC))
      BENCOD = 0.0
      Allocate (BENDEN(MTLOC))
      BENDEN = 0.0
      Allocate (BENDO(MTLOC))
      BENDO = 0.0
      Allocate (BENDOC(MTLOC))
      BENDOC = 0.0
      Allocate (BENNH4(MTLOC))
      BENNH4 = 0.0
      Allocate (BENNO3(MTLOC))
      BENNO3 = 0.0
      Allocate (BENPO4(MTLOC))
      BENPO4 = 0.0
      Allocate (BENSA(MTLOC))
      BENSA = 0.0
  !
  !JDAY,           &!
       !DLT,            &!			time step (sec)
  !B,              &!
       !BB,             &!
!
  !
  !allocate sediment module variables
  !
!
      Call SED_INIT !initialize sediment consituent names and allocate variables for sediment module
!
  !
  !allocate sav module variables
  !
      If (SAV_CALC) Then
         Call SAV_ALLOC
         Call SED_SAV_EXCHANGE_ALLOC
      End If
!
  !
  !allocate df module variables
  !
      If (DFEEDER) Then
         Call DF_ALLOC
         Call SED_DF_EXCHANGE_ALLOC
      End If
  !
  !
  !allocate sf module variables
  !
  !
      If (SFEEDER) Call SF_ALLOC
  !
  !allocate ba module variables
  !
      If (BALGAE_CALC) Call BA_ALLOC
  !
!
  !
  !input and output files for the sediment module
  !
!
!
  !file handles
      BFI = 1
      BFO = 12
!
  !file names
      BFIFN (1) = 'sedtest_2a.inp'
      BFOFN = 'sedtest_2a.chk'
  !
  !
  !set sediment control parameters or read the sediment module control
  !parameters from file BFI
  !
!
      Open (BFI, File=BFIFN(1), Status='OLD')
  !
      Call SED_READ !read some of the basic variables but it will be over written again afterwards
  !
  !particle (C, N, P, Si, Suspedned Solids) fluxes
!
      PCFWS = 0.0
      PNFWS = 0.0
      PPFWS = 0.0
      PSFWS = 0.0
      SSFWS = 0.0
!
  !
  !
  !driving POC, PON, POP, POS, SSI etc fluxes from water colum that should match Jcin, Jnin, Jpin, Jsin,
  !as well as overlying water concentrations
  !
!
  !
  !	TCE_ID	Date+time			Jcin		Jnin		Jpin		Jsin		O20			Depth	Tw		NH30	NO30	PO40	SI0			CH40		SALw
  !      							(gO2/m^2/d)	(gN/m^2/d)	(gP/m^2/d)	(gSi/m^2/d)	(mgO2/L)	(m)		(deg C)	(mgN/L)	(mgN/L)	(mgP/L)	(mgSi/L)	(mgO2/L)	(ppt)
  !		1	1/1/00 12:00 AM		0.300000	0.005000	0.003000	0.009375	5.0			2.00	15.0	0.015	0.100	0.004	0.216		0.0			30.0
  !
  !
  !
  !
  !Set water coumn variables for sediment diagenesis run
  !
  !	Fortran													Excel
  !------------------------------------------------------------------------------------
      T = 8.02146 !1  	degC						15				degC
      CTEMPI = 8.02146 !degC
      SALT = 29.9658 !2		ppt							30				ppt
      SSI = 0.0 !3		gSolids/m^3					-
      B1 = 1.0 !4		gC/m^3						-
      B2 = 0.0 !5		gC/m^3						-
      B3 = 0.0 !6		gC/m^3						-
      LPOC = 0.0 !11		gC/m^3						-
      RPOC = 0.0 !12		gC/m^3						-
      NH4 = 0.002 !13		gN/m^3						0.015			mgN/L
      NO3 = 0.43 !14		gN/m^3						0.1				mgN/L
      LPON = 0.0 !18		gN/m^3						-
      RPON = 0.0 !19		gN/m^3						-
      PO4 = 0.08 !20		gP/m^3						0.004			mgP/L
      LPOP = 0.0 !23		gP/m^3						-
      RPOP = 0.0 !24		gP/m^3						-
      PIP = 0.0 !25		gP/m^3						-
      COD = 0.0 !26		gO2/m^3						-
      DOXG = 8.0 !27		gO2/m^3						5				mgO2/L
      SIUPB = 0.0 !28		gSi/m^3						0.0				mgSi/L (i.e. gSi/m^3)
      SIAT = 0.0 !29		gSi/m^3						0.216			mgSi/L (i.e. gSi/m^3)
!
      D = 25.239
  !
  !
  !parameters related to water colum
  !
  !
  !read from MRL (adsorption coefficients of PO4 and Silicate
      KADPO4 = 0.0004 !m^3/gSolids		!READ (MRL,1080)  KADPO4, KADSA
!
  !basically PIE1, see WLong notes (4-7-5) =PIE2PO4*DPIE2PO4=100*300 L/kg
  !=30000L/kg = 30m^3/kg=30/1000 (m^3/g) = 0.3 m^3/g
!
!
      KADSA = 0.001 !m^3/g		!basically PIE2, see WLong notes (4-6-7-1) where
  !m2 is SSI (gSolids/m^3), pie2= should have unit m^3/gSolids
  !i.e. 100L/kg ~ 0.1m^3/kg ~ 0.1m^3/(1000g) = 0.0001 m^3/gSi
  !here it is used to calculate equilibrium speciation of dissolved Si
  !and particulate Si based on suspended solids concentration (gSolids/m^3)
  !in water column
  !
  !Relation between m^3/g and 100L/kg :	0.0001m^3/g = 0.1m^3/kg = 100L/(1kg) = 100L/kg
  !
  !
  !for water column, there is plenty  of oxygen, according to WLong notes (4-6-5), we should use PIE1,Si
  !which is 100L/kg * DPIE1SI = 1000L/kg = 1m^3/1000g = 0.001m^3/g, where DPIE1SI=10 im DMD book page 573
  !
  !
  !	POM flux				Fortran	Formula							Excel						Excel Formula
  !-------------------------------------------------------------------------------------------------------------------
  !	Jcin 			1000*0.3/2.667=112.48		mgC/m^2/d			0.3000   gO2/m^2/d			JPOC(i) = Jcin*fpoc(i)
!
  !JPOC(I,1) = WB1NETMMD*FRCALG1(1)*B1(I,KWC)+										frpoc1=0.65
  !			WB2NETMMD*FRCALG2(1)*B2(I,KWC)+
  !			WB3NETMMD*FRCALG3(1)*B3(I,KWC)+
  !			WLPOMNETMMD*LPOC(I,KWC)
!
  !JPOC(I,2) = WB1NETMMD*FRCALG1(2)*B1(I,KWC)+        								frpoc2=0.2
  !			WB2NETMMD*FRCALG2(2)*B2(I,KWC)+
  !			WB3NETMMD*FRCALG3(2)*B3(I,KWC)+
  !			WRPOMNETMMD*RPOC(I,KWC)*FRPOC(I,2)/
  !			(FRPOC(I,2)+FRPOC(I,3))
!
  !JPOC(I,3) = WB1NETMMD*FRCALG1(3)*B1(I,KWC)+ 										frpoc3=0.15
  !			WB2NETMMD*FRCALG2(3)*B2(I,KWC)+
  !			WB3NETMMD*FRCALG3(3)*B3(I,KWC)+
  !			WRPOMNETMMD*RPOC(I,KWC)*FRPOC(I,3)/
  !				(FRPOC(I,2)+FRPOC(I,3))
!
  !	Jnin 			ANC*1000*0.005/2.667/FRNALG1(1)	mgN/m^2/d		0.005    gN/m^2/d			JPON(i) = Jnin * fpon(i)
  !JPON(I,1) = WB1NETMMD*ANC1*FRNALG1(1)*B1(I,KWC)+									frpon1=0.65
  !			WB2NETMMD*ANC2*FRNALG2(1)*B2(I,KWC)+
  !			WB3NETMMD*ANC3*FRNALG3(1)*B3(I,KWC)+
  !			WLPOMNETMMD*LPON(I,KWC)
  !
  !JPON(I,2) = WB1NETMMD*ANC1*FRNALG1(2)*B1(I,KWC)									frpon2=0.25
  !			+WB2NETMMD*ANC2*FRNALG2(2)*B2(I,KWC)
  !			+WB3NETMMD*ANC3*FRNALG3(2)*B3(I,KWC)
  !			+WRPOMNETMMD*RPON(I,KWC)*FRPON(I,2)/
  !			(FRPON(I,2)+FRPON(I,3))
  !
  !JPON(I,3) = WB1NETMMD*ANC1*FRNALG1(3)*B1(I,KWC) 									frpon3=0.1
  !			+WB2NETMMD*ANC2*FRNALG2(3)*B2(I,KWC)
  !			+WB3NETMMD*ANC3*FRNALG3(3)*B3(I,KWC)
  !			+WRPOMNETMMD*RPON(I,KWC)*FRPON(I,3)/
  !			(FRPON(I,2)+FRPON(I,3))
!
  !	Jpin 			Q*1000*0.003/2.667	mgP/m^2/d					0.003    gP/m^2/d			JPOP(i) = Jpin * fpop(i)
  !JPOP(I,1) = WB1NETMMD*Q1(I,KWC)*FRPALG1(1)*B1(I,KWC)+								frpop1=0.65
  !			WB2NETMMD*Q2(I,KWC)*FRPALG2(1)*B2(I,KWC)+
  !			WB3NETMMD*Q3(I,KWC)*FRPALG3(1)*B3(I,KWC)
  !			WLPOMNETMMD*LPOP(I,KWC)
!
  !JPOP(I,2) = WB1NETMMD*Q1(I,KWC)*FRPALG1(2)*B1(I,KWC)+								frpop2=0.2
  !			WB2NETMMD*Q2(I,KWC)*FRPALG2(2)*B2(I,KWC)+
  !			WB3NETMMD*Q3(I,KWC)*FRPALG3(2)*B3(I,KWC)+
  !			WRPOMNETMMD*RPOP(I,KWC)*FRPOP(I,2)/
  !			(FRPOP(I,2)+FRPOP(I,3))
!
  !JPOP(I,3) = WB1NETMMD*Q1(I,KWC)*FRPALG1(3)*B1(I,KWC)+								frpop3=0.15
  !			+WB2NETMMD*Q2(I,KWC)*FRPALG2(3)*B2(I,KWC)+
  !			+WB3NETMMD*Q3(I,KWC)*FRPALG3(3)*B3(I,KWC)+
  !			+WRPOMNETMMD*RPOP(I,KWC)*FRPOP(I,3)/
  !			(FRPOP(I,2)+FRPOP(I,3))
!
  !	Jsin 			ASC*1000*0.009375/2.667	mgSi/m^2/d					0.009375 gSi/m^2/d			POS2 = (Jsin * tc / H2 + POS2) /
  !																										(1 + KSI * THTASI ^ (Tw - 20) * tc + w2 * tc / H2)
  !JPOS(I) = 	 WB1NETMMD*ASC1*B1(I,KWC)
  !		  	+WB2NETMMD*ASC2*B2(I,KWC)
  !			+WB3NETMMD*ASC3*B3(I,KWC)
  !			+WPSINETMMD*SIUPB(I,KWC)
!
  !
  !POC distribution into G1,G2,G3 by ALG 1,2,3
  !G1 	Alg1				Alg2				Alg3
      FRCALG1 (1) = 0.65
      FRCALG2 (1) = 0.65
      FRCALG3 (1) = 0.65
!
  !G2
      FRCALG1 (2) = 0.20
      FRCALG2 (2) = 0.20
      FRCALG3 (2) = 0.20
!
  !G3
      FRCALG1 (3) = 0.15
      FRCALG2 (3) = 0.15
      FRCALG3 (3) = 0.15
!
!
  !PON distribution into G1,G2,G3 by ALG 1,2,3
  !G1 	Alg1				Alg2				Alg3
      FRNALG1 (1) = 0.65
      FRNALG2 (1) = 0.65
      FRNALG3 (1) = 0.65
!
  !G2
      FRNALG1 (2) = 0.25
      FRNALG2 (2) = 0.25
      FRNALG3 (2) = 0.25
!
  !G3
      FRNALG1 (3) = 0.10
      FRNALG2 (3) = 0.10
      FRNALG3 (3) = 0.10
!
  !POP distribution into G1,G2,G3 by ALG 1,2,3
  !G1 	Alg1				Alg2				Alg3
      FRPALG1 (1) = 0.65
      FRPALG2 (1) = 0.65
      FRPALG3 (1) = 0.65
!
  !G2
      FRPALG1 (2) = 0.20
      FRPALG2 (2) = 0.20
      FRPALG3 (2) = 0.20
!
  !G3
      FRPALG1 (3) = 0.15
      FRPALG2 (3) = 0.15
      FRPALG3 (3) = 0.15
!
  !The following values are given in order to match Jcin, Jnin, Jpin,Jsin in Excel version
      B1 = 1.0
  !only Alg1 settling contributes to POM in sediments
!
      B2 = 0.0
      B3 = 0.0
  !all other particulate organics set to zero
      LPOC = 0.0
      RPOC = 0.0
      LPON = 0.0
      RPON = 0.0
      LPOP = 0.0
      RPOP = 0.0
      SIUPB = 0.0
!
  !Only settling rates of B1 matters for JPOC
      WB1NETMMD = (0.125749/2.667) * 1000.0 / B1 (1, KWC)!/FRCALG1(1)  !==>112.4859mm/d 	!here 0.3 is Jcin (gO2/m^2/d) in Excel version
!
  !IDEBUG_TEST=3
  !IF(MSR)WRITE(*,*)'IDEBUG_TEST=',IDEBUG_TEST
  !IF(MSR)WRITE(*,*)'WB1NETMMD=',WB1NETMMD
  !IF(MSR)READ(*,*)
!
      WB2NETMMD = 1000.0 !1000mm/d  	~1m/d
      WB3NETMMD = 1000.0 !1000mm/d	~1m/d
      WLPOMNETMMD = 1000.0 !1000mm/d	~1m/d
      WRPOMNETMMD = 1000.0 !1000mm/d  ~1m/d
      WPSINETMMD = 1000.0 !1000mm/d	~1m/d
!
  !reset WSLNET etc
      WSLBNET (1) = WLPOMNETMMD / 1000.
      WSRBNET (1) = WRPOMNETMMD / 1000.
      WS1BNET (1) = WB1NETMMD / 1000.0
      WS2BNET (1) = WB2NETMMD / 1000.0
      WS3BNET (1) = WB3NETMMD / 1000.0
      WSUBNET (1) = WPSINETMMD / 1000.0
!
      WSSBNET (1) = 1.0 !1m/d
!
  !ANC1, Q1, and ASC1 are adjusted to match Jnin, Jpin, Jsin based on Jcin
  !
  !redfield ratios of C,N,P for algae and detritus (modified to match Jnin, Jpin, Jsin of Excel vresion)
  !
  !JPON=ANC1*WB1NETMMD*B1*FRNALG1(1)/1000=0.005
!
  !
      ANC1 = (0.00125125) * 1000.0 / B1 (1, KWC) / WB1NETMMD !==>0.0444 !here 0.005 is Jnin (gN/m^2/d) in Excel version
  !ANC1    =	0.175 	!gN/gC
      ANC2 = 0.175
      ANC3 = 0.175
!
  !P to C ratio (gP/gC) of alg 1 2 and 3 (from AGR file)
      Q1 = (0.00017375) * 1000.0 / B1 (1, KWC) / WB1NETMMD !==>0.0267	!here 0.003 is Jpin (gP/m^2/d) in Excel version
  !Q1    	= 1/41.1		!see DMD book page 10 Table 1.1
      Q2 = 1 / 41.1 !see DMD book page 10 Table 1.1
      Q3 = 1 / 41.1 !see DMD book page 10 Table 1.1
  !
      ASC1 = (0.0017316) * 1000.0 / B1 (1, KWC) / WB1NETMMD !==>0.0833	!here 0.009375 is Jsin in gSi/m^2/d in Excel version
  !ASC1	=	0.333 	!gSi/gC
      ASC2 = 0.333
      ASC3 = 0.333
  !
  !
  !set parameters related to SedFlux_ver17b12_test2a.xlsm
  !
!
  !the parameter lists in SedFlux_ver17b12_test2a.xlsm are listed as follows
  !-------------------------------------------------------------------------------------------------------------------------------------------------
  !parameter		parameter		parameter_meaning										unit			default_value		unit		equation
  !(excel)		(mod_sed.F)																(excel)				(excel)			(mod_sed)	(DMD book)
  !-------------------------------------------------------------------------------------------------------------------------------------------------
  !Particle mixing:
  !
  !m1				M1				solids concentration in aerobic layer 1					kgD/L			0.5					kg/L
  !m2				M2				solids concentration in anaerobic layer 2				kgD/L			0.5					kg/L
  !Dp				DPP	(VPMIX)		bioturbation particle mixing coefficient				m^2/d			0.00006				m^2/d
  !Dd				DDP	(VDMIX)		pore water diffusion coefficient						m^2/d			0.0025				m^2/d
  !w2				W2 (VSED)		deep burial velocity									m/d				6.85E-06			cm/yr *
  !H2				H2(HSED,HSEDALL)thickness of sediment anaerobic layer 2					m				0.1					cm *		*input is cm
  !
      M1 = 0.5
      M2 = 0.5
      DPP = 0.00006
      DDP = 0.0025
      HSEDALL = 10.0 !cm
      Do I = 1, MLOC
         HSED (I) = HSEDALL * 0.01 !m
         VSED (I) = 6.85E-06 !m/d  (W2)
         VPMIX (I) = 0.00006 !m^2/d
         VDMIX (I) = 0.0025 !m^2/d
      End Do
!
  !Reaction velocities (depth integrated reaction rate = reaction velocity (m/d))
  !
  !KappaNH3f		KAPPNH4F		freshwater nitrification velocity						m/d				0.131				m/d
  !KappaNH3s		KAPPNH4S		saltwater nitrification velocity						m/d				0.131				m/d
  !KappaNO3_1f	KAPPNO3F		freshwater denitrification velocity in layer 1			m/d				0.1					m/d
  !KappaNO3_1s	KAPPNO3S		saltwater denitrification velocity in layer 1			m/d				0.1					m/d
  !KappaNO3_2		K2NO3			denitrfication in the anaerobic layer 2					m/d				0.25				m/d
  !KappaCH4		KAPPCH4			methane oxidation in the aerobic layer 1				m/d				0.7					m/d
  !
      KAPPNH4F = 0.131 !m/d
      KAPPNH4S = 0.131 !m/d
      KAPPNO3F = 0.1 !m/d
      KAPPNO3S = 0.1 !m/d
      K2NO3 = 0.25 !m/d		!DMD P.105 Table 4.1
      KAPPCH4 = 0.7 !m/d
!
  !Half saturation constants
  !
  !KM_NH3			KMNH4			nitrification half saturation for NH4N					mgN/L			0.728				mgN/m^3	*
  !KM_O2_NH3		KMNH4O2			nitrification half saturation for O2					mgO2/L			0.37				mgO2/L
  !
      KMNH4 = 0.728 * 1000. !(mgN/m^3)		!see DMD book p73 Table 3.1 (b) (and Median)
  !maybe DMD book is wrong on units
      KMNH4O2 = 0.37 !mgO2/L			!See DMD book p73 Tabl3 3.1 (Median)
!
  !Partitioning coefficients
  !
  !KdNH3			PIENH4			partition coefficient for NH4 in layer 1 and 2			L/kgD			1					L/kg
  !KdPO42			PIE2PO4			partition coefficient for PO4 in layer 2				L/kgD			20					L/kg
  !dKDPO41f		DPIE1PO4F		freshwater factor that increases the aerobic 			unitless		20					L/kg
  !									layer partition coefficient of inorganic P
  !dKDPO41s		DPIE1PO4S		saltwater factor that increases the aerobic 			unitless		20					L/kg
  !									layer partition coefficient of inorganic P
  !O2critPO4		O2CRITPO4		critical O2 concentration in layer 1 for 				mgO2/L			2					mgO2/L
  !									adjustment of partition coefficient
  !									for inorganic P
  !
      PIENH4 = 1.0 !L/kg
      PIE2PO4 = 20.0 !L/kg
      DPIE1PO4F = 20.0 !WLong?? Unitless?	Yes! Note DMD book pp 572 says it should be about 300
      DPIE1PO4S = 20.0 !WLong?? Unitless?
      O2CRITPO4 = 2.0 !mgO2/L
!
  !Temperature coefficients
  !
  !ThtaDp			THTADP			temperature theta for bioturbation mixing between 		unitless		1.117				unitless
  !									layers 1 and 2
  !ThtaDd			THTADD			temperature theta for pore water diffusion between 		unitless		1.08				unitless
  !									layers 1 and 2
  !ThtaKmNH3		THTAKMNH4		temperature theta for nitrification half saturation 	unitless		1					unitless
  !									for NH4N
  !ThtaNH3		THTANH4			temperature theta for nitrification rate				unitless		1.123				unitless
  !ThtaNO3		THTANO3			temperature theta for denitrification rate				unitless		1.08				unitless
  !ThtaCH4		THTACH4			temperature theta for methane oxidation rate			unitless		1.079				unitless
  !
      THTADP = 1.117
      THTADD = 1.08
  !!ThtaKmNH3	=	!Applied to KM_NH3 in Excel, should be applied to KMNH4 in Fortran code
      THTAKMNH4 = 1.0 !not effective, since it value is 1 anyway
  !See DMD book p.73 Table 3.1 (median value is 1.125)
      THTANH4 = 1.123 !See DMD book p.73 Table 3.1 (median value is 1.123)
      THTANO3 = 1.08
      THTACH4 = 1.079
!
  !Salinity thresholds
  !
  !SALTSW			SALTSW			salinity above which sulfide rather than 				psu				1					ppt
  !									methane is produced from C diagenesis
  !SALTND			SALTND			salinity above which saltwater 							psu				1					ppt
  !									nitrification/denitrification rates are
  !									used for aerobic layer
      SALTSW = 1.0 !ppt
      SALTND = 1.0 !ppt
!
  !Sulfide constants
  !
  !KappaH2Sd1		KAPP1HSD		aerobic layer reaction velocity for dissolved 			m/d				0.2					m/d
  !								sulfide oxidation
  !KappaH2Sp1		KAPP1HSP		aerobic layer reaction velocity for particulate 		m/d				0.4					m/d
  !								sulfide oxidation
  !ThtaH2S		THTAH2S			temperature coefficient for sulfide oxidation			unitless		1.079				unitless
  !KMHSO2			KMHSO2			sulfide oxidation normalization constant for O2			mgO2/L			4					mgO2/L
  !KdH2S1			PIE1HS			partition coefficient for sulfide in aerobic layer 1	L/kgD			100					L/kg	*
  !KdH2S2			PIE2HS			partition coefficient for sulfide in anaerobic layer 2	L/kgD			100					L/kg	*
  !
      KAPP1HSD = 0.2 !m/d
      KAPP1HSP = 0.4 !m/d
      THTAH2S = 1.079
      KMHSO2 = 4.0 !mgO2/L
      PIE1HS = 100 !L/kg
      PIE2HS = 100 !L/kg
  !
!
  !Fractions of G classes 1 and 2 for settling RPON, RPOC, and RPOP
  !
  !frpon1			FRPON(:,1)		fraction of class 1 pon									unitless		0.65				unitless * only for detritus
  !frpon2			FRPON(:,2)		fraction of class 2 pon									unitless		0.25				unitless * only for detritus
!
  !frpoc1			FRPOC(:,1)		fraction of class 1 poc									unitless		0.65				unitless * only for detritus
  !frpoc2			FRPOC(:,2)		fraction of class 2 poc									unitless		0.2					unitless * only for detritus
!
  !frpop1			FRPOP(:,1)		fraction of class 1 pop									unitless		0.65				unitless * only for detritus
  !frpop2			FRPOP(:,2)		fraction of class 2 pop									unitless		0.2					unitless * only for detritus
!
  !*******WLong: Fortran code used this completely differently!!!!*****
  !Fortran code uses these as fraction of RPOM to be split in G2 and G3
  !G1 has no share of RPOM
!
      Do I = 1, MLOC
         FRPON (I, 1) = 0.0 !No share for G1
         FRPON (I, 2) = 0.65
         FRPON (I, 3) = 1 - FRPON (I, 2) - FRPON (I, 1)
!
         FRPOC (I, 1) = 0.0 !No share for G1
         FRPOC (I, 2) = 0.65
         FRPOC (I, 3) = 1 - FRPOC (I, 1) - FRPOC (I, 2)
!
         FRPOP (I, 1) = 0.0 !No share for G1
         FRPOP (I, 2) = 0.65
         FRPOP (I, 3) = 1 - FRPOP (I, 1) - FRPOP (I, 2)
     !
      End Do
!
  !Diagenesis rate constants for G clase 1, 2, and 3 N/C/P
  !
  !kpon1			KPON1			G class 1 pon mineralization							day^-1			0.035				1/d
  !kpon2			KPON2			G class 2 pon mineralization							day^-1			0.0018				1/d
  !kpon3			KPON3			G class 3 pon mineralization							day^-1			0					1/d
  !kpoc1			KPOC1			G class 1 poc mineralization							day^-1			0.035				1/d
  !kpoc2			KPOC2			G class 2 poc mineralization							day^-1			0.0018				1/d
  !kpoc3			KPOC3			G class 3 poc mineralization							day^-1			0					1/d
  !kpop1			KPOP1			G class 1 pop mineralization							day^-1			0.035				1/d
  !kpop2			KPOP2			G class 2 pop mineralization							day^-1			0.0018				1/d
  !kpop3			KPOP3			G class 3 pop mineralization							day^-1			0					1/d
  !
      KPON1 = 0.035 !1/d
      KPON2 = 0.0018 !1/d
      KPON3 = 0. !1/d
!
      KPOC1 = 0.035 !1/d
      KPOC2 = 0.0018 !1/d
      KPOC3 = 0. !1/d
!
      KPOP1 = 0.035 !1/d
      KPOP2 = 0.0018 !1/d
      KPOP3 = 0. !1/d
  !
  !Temperature coefficients for G class 1, 2, and 3 mineralization
  !
  !ThtaPON1		THTAPON1		temperature theta for G class 1 pon						unitless		1.1
  !ThtaPON2		THTAPON2		temperature theta for G class 2 pon						unitless		1.15
  !ThtaPON3		THTAPON3		temperature theta for G class 3 pon						unitless		1.17
  !ThtaPOC1		THTAPOC1		temperature theta for G class 1 poc						unitless		1.1
  !ThtaPOC2		THTAPOC2		temperature theta for G class 2 poc						unitless		1.15
  !ThtaPOC3		THTAPOC3		temperature theta for G class 3 poc						unitless		1.17
  !ThtaPOP1		THTAPOP1		temperature theta for G class 1 pop						unitless		1.1
  !ThtaPOP2		THTAPOP2		temperature theta for G class 2 pop						unitless		1.15
  !ThtaPOP3		THTAPOP3		temperature theta for G class 3 pop						unitless		1.17
  !
      THTAPON1 = 1.1
      THTAPON2 = 1.15
      THTAPON3 = 1.17
!
      THTAPOC1 = 1.1
      THTAPOC2 = 1.15
      THTAPOC3 = 1.17
!
      THTAPOP1 = 1.1
      THTAPOP2 = 1.15
      THTAPOP3 = 1.17
!
  !Parameters for partical mixing and benthic stress
  !
  !POC1R			POC1R			reference G1 at which w12base = Dp / H2 at 				mgO2/gD			0.2667				mgC/gD			*Not available
  !									20 degC for DiToro eqn 13.1
  !
  !kBEN_STR		KBENSTR 		first-order decay rate constant for benthic  			day^-1			0.03				1/d
  !									stress (d^-1) for DiToro eqn 13.3
  !
  !KM_O2_Dp		KMO2DP			particle mixing half-saturation constant 				mgO2/L			4					mgO2/L
  !									for O2 (mgO2/L)
!
  ! ??			KLBNTH			ratio of bio-irrigation to bioturbation					unitless
  ! ?? 			DPMIN			minimum particule mixing rate (m^2/day)
!
      POC1R = 0.1 !mgC/gSediment  DMD eqn (13.1) !0.2667/2.667
  !WLong notes eqn(3-26) (DMD book page 278, table 13.1, eqn(13.1))
      KBENSTR = 0.03 !1/d
      KMO2DP = 4.0 !mgO2/L  ~ gO2/m^3
!
      KLBNTH = 0.d0 !to make sure no bioturbation
      DPMIN = 0.d0 !to make minimum  particle diffusion rate zero
  !
  !Parameters for silica
  !
  !KSI			KSI				reaction rate for particulate biogenic Si 				day^-1			0.5					1/d
  !THTASI			THTASI			temperature theta for KSI								unitless		1.1					unitless
  !CSISAT			CSISATT20		saturation concentration for porewater Si				mgSi/L			40					mg Si/m3 	*
  !THTASISAT		THTASISAT		temperature theta for CSISAT							unitless		1.023							*Not available
  !DPIE1SI		DPIE1SI			incremental partition coefficient for Si in layer 1		L/kgD			10					L/kg
  !PIE2SI			PIE2SI			partition coefficient for Si in layer 2					L/kgD			100					L/kg
  !KMPSI			KMPSI			particulate biogenic Si half saturation 				mgSi/L			50000				mg Si/m3	*
  !									constant for dissolution
  !O2CRITSI		O2CRITSI		critical O2 concentration for layer 1 					mgO2/L			2					O2CRITSI
  !									incremental Si sorption
  !
      KSI = 0.5 !1/d
      THTASI = 1.1 !dimensionless		!DMD book p155 Tabel 7.1
      CSISATT20 = 40 * 1000.0 !mgSi/m^3
      THTASISAT = 1.023 !dimensionless
      DPIE1SI = 10 !L/kg (??)
      PIE2SI = 100 !L/kg
      KMPSI = 50000.0 * 1000 !mgSi/m^3			!DMD book p155  gives 19.8 to 100mg/L, P573 gives 50000mgSi/L ?
      O2CRITSI = 2.0 !mgO2/L ~ gO2/m^3 !used for calculating PIE1SI
  !
  !
  !Parameters for temperature
  !
      DIFFT = 1000.0 !m^2/sec      !used for calculating temperature in sediments
!
!
  !
  !initial conditions for sediment concentratins
  !
  !----------------------------------------------------------------------------------------------------------------------------------------------------------------
  !parameter		parameter		parameter_meaning										unit			initial_value		unit			IC			equation
  !(excel)		(mod_sed.F)																(excel)				(excel)			(mod_sed)		(fortran)	(DMD book)
  !---------------------------------------------------------------------------------------------------------------------------------------------------------------
  !
  !Particulate organic C, N, and P in layer 2
!
  !POC2(1)		CPOCI(1)		G class 1 POC in layer 2								gO2/m^3			89.446				mgC/m^3	*		(89.446/2.667)*1000
  !POC2(2)		CPOCI(2)		G class 2 POC in layer 2								gO2/m^3			622.783				mgC/m^3	*		(622.783/2.667)*1000
  !POC2(3)		CPOCI(3)		G class 3 POC in layer 2								gO2/m^3			6569.343			mgC/m^3	*		(6569.343/2.667)*1000
  !PON2(1)		CPONI(1)		G class 1 PON in layer 2								gN/m^3			1.491				mgN/m^3			1.491*1000
  !PON2(2)		CPONI(2)		G class 2 PON in layer 2								gN/m^3			12.975				mgN/m^3			12.975*1000
  !PON2(3)		CPONI(3)		G class 3 PON in layer 2								gN/m^3			72.993				mgN/m^3			72.993*1000
  !POP2(1)		CPOPI(1)		G class 1 POP in layer 2								gP/m^3			0.894				mgP/m^3			0.894*1000
  !POP2(2)		CPOPI(2)		G class 2 POP in layer 2								gP/m^3			6.228				mgP/m^3			6.228*1000
  !POP2(3)		CPOPI(3)		G class 3 POP in layer 2								gP/m^3			65.693				mgP/m^3			65.693*1000
  !POS2			CPOSI			Particulate biogenic Si in layer 2						gSi/m^3			0.302				mgSi/m^3		0.302*1000
  !
  !Sediment temperature and benthic stress
  !*				CTEMPI																					??					degC			15.0
  !*				BENSTI																					0.0									0.0
!
  !Dissolved constituents in layer 1 and 2 porewater
!
  !NH3(1)			*NH4T1I			Dissolved ammonia N in layer 1 porewater				mgN/L			0.024				mgN/m^3			0.024*1000
  !NH3(2)			NH4T2I			Dissolved ammonia N in layer 2 porewater				mgN/L			0.152				mgN/m^3			0.152*1000
  !NO3(1)			*NO3T1I			Dissolved nitrate+nitrite N in layer 1 porewater		mgN/L			0.041				mgN/m^3			0.041*1000
  !NO3(2)			NO3T2I			Dissolved nitrate+nitrite N in layer 2 porewater		mgN/L			0.007				mgN/m^3			0.007*1000
  !PO4(1)			*PO4T1I			Dissolved phosphate P in layer 1 porewater				mgP/L			0.054				mgP/m^3			0.054*1000
  !PO4(2)			PO4T2I			Dissolved phosphate P in layer 2 porewater				mgP/L			0.290				mgP/m^3			0.290*1000
  !Si(1)			*SIT1I			Dissolved Si in layer 1 porewater						mgSi/L			0.209				mgSi/m^3        0.209*1000
  !Si(2)			SIT2I			Dissolved Si in layer 2 porewater						mgSi/L			1.160				mgSi/m^3		1.160*1000
!
  !HS(1)			*HST1I			Dissolved sulfide in layer 1 porewater 					mgO2/L			0.002               gO2/m^3			0.002
  !									(used if salinity >= 1 psu)
  !HS(2)			HST2I			Dissolved sulfide in layer 2 porewater					mgO2/L			3.348				gO2/m^3			3.348
  !								 	(used if salinity >= 1 psu)
  !CH4(1)			CH41TI			Dissolved methane in layer 1 porewater 					mgO2/L			0.000				gO2/m^3			0.000
  !								 	(used if salinity >= 1 psu)
  !CH4(2)			CH4T2I			Dissolved methane in layer 2 porewater 					mgO2/L			0.000				gO2/m^3			0.000
  !									(used if salinity >= 1 psu)
  !**				SO4T2I																	mgO2/L			??					mgO2/L			0.000
  !*				SO4T2I																	mgO2/L			??					mgO2/L			0.000
!
  !
      CPOCI (1) = 1000 * 89.446 / 2.667 !mgC/m^3
      CPOCI (2) = 1000 * 622.783 / 2.667 !mgC/m^3
      CPOCI (3) = 1000 * 6569.343 / 2.667 !mgC/m^3
      CPONI (1) = 1000 * 1.491 !mgN/m^3
      CPONI (2) = 1000 * 12.975 !mgN/m^3
      CPONI (3) = 1000 * 72.993 !mgN/m^3
      CPOPI (1) = 1000 * 0.894 !mgP/m^3
      CPOPI (2) = 1000 * 6.228 !mgP/m^3
      CPOPI (3) = 1000 * 65.693 !mgP/m^3
      BENSTI = 0.0 !unitless		!Fortran only ??
      CPOSI = 1000 * 0.302 !mgSi/m^3
!
  !
  !Note that in Excel version, concentrations are given as dissolved , then total concenration is calculated based on m1,m2 and PIENH4
  !
  !fd1 = (1# / (1# + m1 * KdNH3))
  !fd2 = (1# / (1# + m2 * KdNH3))
  !NH3Tp2(1) = NH3p2(1) / fd1
  !NH3Tp2(2) = NH3p2(2) / fd2
  !
  !m1=m2=0.5, PIENH4=1 ==> fd1=(1/(1+0.5*1))=1/1.5=0.3333
  !						fd2=(1/(1+0.5*1))=1/1.5=0.3333
  !
  !
      PIE1 = PIENH4
      PIE2 = PIENH4
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      NH41TI = 1000 * 0.024 !mgN/m^3		!Excel only
      NH4T2I = 1000 * (0.152) / FD2 !mgN/m^3
!
      NO31TI = 1000 * 0.041 !mgN/m^3		!Excel only
      NO3T2I = 1000 * 0.007 !mgN/m^3
!
!
  !also make sure PO4T2I is total PO4
      I = 1
!
      If (SALT(I, KBM1) < SALTND) Then
         DPIE1PO4 = DPIE1PO4F
      Else
         DPIE1PO4 = DPIE1PO4S
      End If
      If (O20 > O2CRITPO4) Then
         PIE1 = PIE2PO4 * DPIE1PO4
      Else
         PIE1 = PIE2PO4 * DPIE1PO4 ** (O20/O2CRITPO4)
      End If
      PIE2 = PIE2PO4
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      PO41TI = 1000 * 0.054 !mgP/m^3		!Excel only
      PO4T2I = 1000 * 0.290 / FD2 !converted to total value
!
      If (O20 > O2CRITSI) Then
         PIE1 = PIE2SI * DPIE1SI
      Else
         PIE1 = PIE2SI * DPIE1SI ** (O20/O2CRITSI)
      End If
      PIE2 = PIE2SI
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      SI1TI = 1000 * 0.209 !mgSi/m^3		!Excel only
      SIT2I = 1000 * 1.160 / FD2 !mgSi/m^3		!converted to total Si in layer 2
!
!
      PIE1 = PIE1HS
      PIE2 = PIE2HS
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      HS1TI = 0.002 !mgO2/L			!Excel only
      HST2I = 3.348 / FD2 !mgO2/L			!converted to total HS
!
      CH4T2I = 0.0 !mgO2/L
      CH41TI = 0.0 !mgO2/L
!
      SO41TI = 0.0 !mgO2/L			!Neither Excel Nor Fortran
      SO4T2I = 0.0 !mgO2/L			!Fortran only
  !
      SODI = 0.1 !mgO2/m^2/d		!Initialize SOD !WLong: may need to call this SODI
  !Greg Pelletier's Excel used JSOD=0.1 mgO2/m^2/d
      Do I = 1, MLOC
!
         CTEMP (I) = CTEMPI !Sediment temperature
!
         Do JG = 1, 3
            CPOC (I, JG) = CPOCI (JG)
            CPON (I, JG) = CPONI (JG)
            CPOP (I, JG) = CPOPI (JG)
         End Do
!
         CPOS (I) = CPOSI
         BENSTRTM1S (I) = BENSTI !*not in excel
         SODTM1S (I) = SODI
         NH41TM1S (I) = NH41TI
         NH4T2TM1S (I) = NH4T2I
!
         NO31TM1S (I) = NO31TI
         NO3T2TM1S (I) = NO3T2I
!
         PO41TM1S (I) = PO41TI
         PO4T2TM1S (I) = PO4T2I
!
         SI1TM1S (I) = SI1TI
         SIT2TM1S (I) = SIT2I
!
         HS1TM1S (I) = HS1TI
         HST2TM1S (I) = HST2I
!
         CH41TM1S (I) = CH41TI
         CH4T2TM1S (I) = CH4T2I
!
         SO4T2TM1S (I) = SO4T2I !*not in excel
     !
      End Do
!
!
  !reset the intial values of varables that involve some of the above parameters
      Call SED_INIT2
  !
  !open output file
!
      BFOFN = 'sedtest_2aChannel.csv'
  !
      If (MSR) Then
!
         Open (Unit=BFO, File=BFOFN)
         Rewind (BFO)
!
         IDEBUG_TEST = 2
         If (MSR) WRITE (*,*) 'IDEBUG_TEST=', IDEBUG_TEST
     !
!
     !
     !write the headline the same as excel file "SedFlux_ver17b12_test2a.xlsm"
     !
!
     !Segment ID	JDAY+Time	Jcin	Jnin	Jpin	Jsin	O20	Depth	Tw	NH30	NO30	PO40	SI0	CH40	SALw
     !				SOD	Jnh4	Jno3	JDenitT	Jch4	Jch4g	Jhs	Jpo4	Jsi	Jcout	Jnout	Jpout	NH3(1)	NH3(2)
     !				NO3(1)	NO3(2)	PO4(1)	PO4(2)	Si(1)	Si(2)	CH4(1)	CH4(2)	HS(1)	HS(2)	POC2(1)	POC2(2)	POC2(3)
     !				PON2(1)	PON2(2)	PON2(3)	POP2(1)	POP2(2)	POP2(3)	POS2 H1	BEN_STR
     !
         I = 1
         Write (BFO, 1001) 'SegmentID', ',', 'JDAY', ',', 'Jcin', ',', 'Jnin', ',', 'Jpin', ',', 'Jsin', ',', 'O20', ',', 'Depth', ',', 'Tw', ',', 'NH40', ',', 'NO30', ',', 'PO40', ',', 'SI0', ',', 'CH40', ',', 'SALw', ',', 'SOD', ',', 'Jnh4', ',', 'Jno3',&
        &  ',', 'JDenitT', ',', 'Jch4', ',', 'Jch4g', ',', 'Jhs', ',', 'Jpo4', ',', 'Jsi', ',', 'Jcout', ',', 'Jnout', ',', 'Jpout', ',', 'NH3(1)', ',', 'NH3(2)', ',', 'NO3(1)', ',', 'NO3(2)', ',', 'PO4(1)', ',', 'PO4(2)', ',', 'Si(1)', ',', 'Si(2)', ',', &
        & 'CH4(1)', ',', 'CH4(2)', ',', 'HS(1)', ',', 'HS(2)', ',', 'POC2(1)', ',', 'POC2(2)', ',', 'POC2(3)', ',', 'PON2(1)', ',', 'PON2(2)', ',', 'PON2(3)', ',', 'POP2(1)', ',', 'POP2(2)', ',', 'POP2(3)', ',', 'POS2', ',', 'H1', ',', 'BEN_STR'
!
      End If
1000  Format (I10, 1000(1 X, A10, F15.8))
1001  Format (A10, 1000(1 X, A10, A15))
  !
  !do test 2a calculations
!
      TMSTRT = 0.0 !reference time
      TMEND = 70.0 !700.0		!end time
      JDAY = 0.0 !time in days
      DLT = 0.01 * 86400 !10.0		!time step in sec = 864sec=0.01 day
      ELTMS = 0.0 !time in sec
      ELTMJD = 0.0 !time in days after reference time
      DLTS = DLT / 86400.
!
      END_RUN = .False.
!
      IINT = 0 !time step counter
!
      Do WHILE ( .Not. END_RUN)
     !
         If (MSR) WRITE (*,*) 'JDAY=', JDAY
     !
         Call POM_ACCUMUL
     !
     !average the accumulated POM flux over NDTSED steps
         Do I = 1, MLOC
            Do K = 1, 3
               JPOC (I, K) = JPOCaccum (I, K)
               JPON (I, K) = JPONaccum (I, K)
               JPOP (I, K) = JPOPaccum (I, K)
            End Do
            JPOS (I) = JPOSaccum (I)
         End Do
     !
!
         Call SED_CALC (.True.)!calculate with state state solution
!
     !
         JPOCaccum = 0.0
         JPONaccum = 0.0
         JPOPaccum = 0.0
         JPOSaccum = 0.0
     !
     !
     !write out outputs
     !
         If (MSR) Then
        !Fortran											Excel				FortranUnit			ExcelUnit
            I = 1
            Write (BFO, 1000) I, ',', JDAY, ',', (JPOC(I, 1)+JPOC(I, 2)+&
           & JPOC(I, 3)) / 1000.0 * 2.667, ',', (JPON(I, 1)+JPON(I, 2)+&
           & JPON(I, 3)) / 1000.0, ',', (JPOP(I, 1)+JPOP(I, 2)+JPOP(I, &
           & 3)) / 1000.0, ',', JPOS (I) / 1000.0, ',', O20, ',', D &
           & (I), ',', T (I, KBM1), ',', NH40 / 1000, ',', NO30 / 1000, &
           & ',', PO40 / 1000, ',', SI0 / 1000, ',', CH40, ',', SAL, ',&
           &', SOD, ',', JNH4 / 1000, ',', JNO3 / 1000, ',', BENDEN &
           & (1), ',', JCH4, ',', JCH4G, ',', JHS, ',', JPO4 / 1000, ',&
           &', JSI / 1000, ',', 0.0, ',', 0.0, ',', 0.0, ',', NH41 / &
           & 1000, ',', NH42 / 1000, ',', NO31 / 1000, ',', NO32 / &
           & 1000, ',', PO41 / 1000, ',', PO42 / 1000, ',', SI1 / 1000, &
           & ',', SI2 / 1000, ',', CH41, ',', CH42, ',', HS1, ',', HS2, &
           & ',', POC1 / 1000 * 2.667, ',', POC2 / 1000 * 2.667, ',', &
           & POC3 / 1000 * 2.667, ',', PON1 / 1000, ',', PON2 / 1000, '&
           &,', PON3 / 1000, ',', POP1 / 1000, ',', POP2 / 1000, ',', &
           & POP3 / 1000, ',', PSISED / 1000, ',', HSED1 (I) * 1000.0, &
           & ',', BENSTR !'BEN_STR' 			dimensionless		dimensionless
        ! WRITE(BFO,1000)I,',',JDAY,			&
        !	',',(JPOC(I,1)+JPOC(I,2)+JPOC(I,3))/1000.0*2.667,	&	!'Jcin'				mgC/m^2/d			gO2/m^2/d
        !	',',(JPON(I,1)+JPON(I,2)+JPON(I,3))/1000.0,	&			!'Jnin'				*mgN/m^2/d			gN/m^2/d
        !	',',(JPOP(I,1)+JPOP(I,2)+JPOP(I,3))/1000.0,	&			!'Jpin'				*mgP/m^2/d			gP/m^2/d
        !	',',JPOS(I)/1000.0,	&									!'Jsin		 		*mgSi/m^2/d			gSi/m^2/d
        !	',',O20,			&									!'O20'				mgO2/L				mgO2/L
        !	',',D(I),			&									!'Depth'			m					m
        !	',',T(I,KBM1),		&									!'Tw'				degC				degC
        !	',',NH40/1000,		&									!'NH30'				*mgN/m^3			mgN/L
        !	',',NO30/1000,		&									!'NO30'				*mgN/m^3			mgN/L
        !	',',PO40/1000,		&									!'PO40'				*mgP/m^3			mgP/L
        !	',',SI0/1000,		&									!'SI0'				*mgSi/m^3 			mgSi/L				(dissiolved SIAT)
        !	',',CH40,			&									!'CH40'				gO2/m^3				mgO2/L
        !	',',SAL,			&									!'SALw'  			ppt					ppt					!i.e. SALT(I,KWC), ppt
        !	',',SOD,			&									!'SOD'				gO2/m^2/day			gO2/m^2/d
        !	',',JNH4/1000,		&									!'Jnh4				*mgN/m^2/day		gN/m^2/d
        !	',',JNO3/1000,		&									!'Jno3				*mgN/m^2/day		gN/m^2/d
        !	',',BENDEN(1),		&									!'JDenitT'			gN/m^2/day			gN/m^2/d
        !							!JDenit(1) = Denit(1) * NO3(1)
        !							!JDenit(2) = Denit(2) * NO3(2)
        !							!JDenitT = JDenit(1) + JDenit(2)
        !	',',JCH4,			&									!'Jch4'				gO2/m^2/day			gO2/m^2/d
        !	',',JCH4G,			&									!'Jch4g'			gO2/m^2/day			gO2/m^2/d
        !	',',JHS,			&									!'Jhs'				gO2/m^2/day			gO2/m^2/d
        !	',',JPO4/1000,		&									!'Jpo4'				*mgP/m^2/day		gP/m^2/d
        !	',',JSI/1000,		&									!'Jsi'				*mgSi/m^2/day		gSi/m^2/d
        !	',',0.0,			&									!'Jcout'
        !	',',0.0,			&									!'Jnout'
        !	',',0.0,			&									!'Jpout'
        !         !
        ! WLong: Excel calculations here, should correspond to Jcin, Jnin and Jpin
        ! Jcout = SOD + JCH4 + JCH4g + JHS +  	(POC2(1) + POC2(2) + POC2(3)) * w2 +
        !						(		(POC2(1) + POC2(2) + POC2(3))
        !							- 	(POC22(1) + POC22(2) + POC22(3))
        !						) * H2 / tc
        ! Jnout = JNO3 + JNH4 + JDenitT + 		(PON2(1) + PON2(2) + PON2(3)) * w2 +
        !						(		(PON2(1) + PON2(2) + PON2(3))
        !							- 	(PON22(1) + PON22(2) + PON22(3))
        !						) * H2 / tc
        ! Jpout = JPO4 + 						(POP2(1) + POP2(2) + POP2(3)) * w2 +
        !						((POP2(1) + POP2(2) + POP2(3))
        !						 - (POP22(1) + POP22(2) + POP22(3))
        !						 ) * H2 / tc
        !
        !	',',NH41/1000,		&					!'NH3(1)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',NH42/1000,		&					!'NH3(2)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',NO31/1000,		&					!'NO3(1)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',NO32/1000,		&					!'NO3(2)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',PO41/1000,		&					!'PO4(1)'			*mgP/m^3 			mgP/L	(dissolved)
        !	',',PO42/1000,		&					!'PO4(2)'			*mgP/m^3 			mgP/L	(dissolved)
        !	',',SI1/1000,		&					!'Si(1)'			*mgSi/m^3 			mgSi/L	(dissolved)
        !	',',SI2/1000,		&					!'Si(2)'			*mgSi/m^3 			mgSi/L	(dissolved)
        !	',',CH41,			&					!'CH4(1)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',CH42,			&					!'CH4(2)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',HS1,			&					!'HS(1)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',HS2,			&					!'HS(2)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',POC1/1000*2.667,&					!'POC2(1)'			*mgC/m^3 			gO2/m^3	(G1 of POC in layer 2)
        !	',',POC2/1000*2.667,&					!'POC2(2)' 			*mgC/m^3 			gO2/m^3	(G2 of POC in layer 2)
        !	',',POC3/1000*2.667,&					!'POC2(3)' 			*mgC/m^3 			gO2/m^3	(G3 of POC in layer 2)
        !	',',PON1/1000,		&					!'PON2(1)'			*mgN/m^3 			gN/m^3	(G1 of PON in layer 2)
        !	',',PON2/1000,		&					!'PON2(2)' 			*mgN/m^3 			gN/m^3	(G2 of PON in layer 2)
        !	',',PON3/1000,		&					!'PON2(3)' 			*mgN/m^3 			gN/m^3	(G3 of PON in layer 2)
        !	',',POP1/1000,		&					!'POP2(1)'			*mgP/m^3 			gP/m^3	(G1 of POP in layer 2)
        !	',',POP2/1000,		&					!'POP2(2)'			*mgP/m^3 			gP/m^3	(G2 of POP in layer 2)
        !	',',POP3/1000,		&					!'POP2(3)' 			*mgP/m^3 			gP/m^3	(G3 of POP in layer 2)
        !	',',PSISED/1000,	&					!'POS2'				*mgSi/m^3 			gSi/m^3	(particulate biogenic Silicate in layer2)
        !	',',HSED1(I)*1000.0,&					!'H1'				m					mm
        !	',',BENSTR			 					!'BEN_STR' 			dimensionless		dimensionless
        !
            Write (*,*) 1, ',', JDAY
            Write (*,*) 'JPOC=,', (JPOC(I, 1)+JPOC(I, 2)+JPOC(I, 3)) / &
           & 1000.0 * 2.667, 'gO2/m^2/d' !'Jcin'				mgC/m^2/d			gO2/m^2/d
            Write (*,*) 'JPON=,', (JPON(I, 1)+JPON(I, 2)+JPON(I, 3)) / &
           & 1000.0, 'gN/m^2/d' !'Jnin'				*mgN/m^2/d			gN/m^2/d
            Write (*,*) 'JPOP=,', (JPOP(I, 1)+JPOP(I, 2)+JPOP(I, 3)) / &
           & 1000.0, 'gP/m^2/d' !'Jpin'				*mgP/m^2/d			gP/m^2/d
            Write (*,*) 'JPOS=,', JPOS (I) / 1000.0, 'gSi/m^2/d' !'Jsin		 		*mgSi/m^2/d			gSi/m^2/d
            Write (*,*) 'O20=,', O20 !,			&					!'O20'				mgO2/L				mgO2/L
            Write (*,*) 'D=,', D (I)!,			&					!'Depth'			m					m
            Write (*,*) 'T=,', T (I, KBM1)!,		&					!'Tw'				degC				degC
            Write (*,*) 'NH40=,', NH40 / 1000 !,	&					!'NH30'				*mgN/m^3			mgN/L
            Write (*,*) 'NO30=,', NO30 / 1000 !,	&					!'NO30'				*mgN/m^3			mgN/L
            Write (*,*) 'PO40=,', PO40 / 1000 !,	&					!'PO40'				*mgP/m^3			mgP/L
            Write (*,*) 'SI0=,', SI0 / 1000 !,		&					!'SI0'				*mgSi/m^3 			mgSi/L				(dissiolved SIAT)
            Write (*,*) 'CH40=,', CH40 !,			&					!'CH40'				gO2/m^3				mgO2/L
            Write (*,*) 'SAL=,', SAL !,			&					!'SALw'  			ppt					ppt					!i.e. SALT(I,KWC), ppt
            Write (*,*) 'SOD=,', SOD !,			&					!'SOD'				gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JNH4=,', JNH4 / 1000 !,	&					!'Jnh4				*mgN/m^2/day		gN/m^2/d
            Write (*,*) 'JNO3=,', JNO3 / 1000 !,	&					!'Jno3				*mgN/m^2/day		gN/m^2/d
            Write (*,*) 'JDeniT=,', BENDEN (1)!,	&					!'JDenitT'			gN/m^2/day			gN/m^2/d
        !JDenit(1) = Denit(1) * NO3(1)
        !JDenit(2) = Denit(2) * NO3(2)
        !JDenitT = JDenit(1) + JDenit(2)
            Write (*,*) 'JCH4=,', JCH4 !,			&					!'Jch4'				gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JCH4G=,', JCH4G !,		&					!'Jch4g'			gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JHS=,', JHS !,			&					!'Jhs'				gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JPO4=,', JPO4 / 1000 !,	&					!'Jpo4'				*mgP/m^2/day		gP/m^2/d
            Write (*,*) 'JSI=,', JSI / 1000 !,		&					!'Jsi'				*mgSi/m^2/day		gSi/m^2/d
!
        !						',',0.0,				&					!'Jcout'
        !						',',0.0,				&					!'Jnout'
        !						',',0.0,				&					!'Jpout'
!
        !
        !WLong: Excel calculations here, should correspond to Jcin, Jnin and Jpin
        !Jcout = SOD + JCH4 + JCH4g + JHS +  	(POC2(1) + POC2(2) + POC2(3)) * w2 +
        !										(		(POC2(1) + POC2(2) + POC2(3))
        !											- 	(POC22(1) + POC22(2) + POC22(3))
        !										) * H2 / tc
        !Jnout = JNO3 + JNH4 + JDenitT + 		(PON2(1) + PON2(2) + PON2(3)) * w2 +
        !										(		(PON2(1) + PON2(2) + PON2(3))
        !											- 	(PON22(1) + PON22(2) + PON22(3))
        !										) * H2 / tc
        !Jpout = JPO4 + 						(POP2(1) + POP2(2) + POP2(3)) * w2 +
        !										((POP2(1) + POP2(2) + POP2(3))
        !										 - (POP22(1) + POP22(2) + POP22(3))
        !										 ) * H2 / tc
        !
            Write (*,*) 'NH41=,', NH41 / 1000 !,		&					!'NH3(1)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'NH42=,', NH42 / 1000 !,		&					!'NH3(2)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'NO31=,', NO31 / 1000 !,		&					!'NO3(1)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'NO32=,', NO32 / 1000 !,		&					!'NO3(2)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'PO41=,', PO41 / 1000 !,		&					!'PO4(1)'			*mgP/m^3 			mgP/L	(dissolved)
            Write (*,*) 'PO42=,', PO42 / 1000 !,		&					!'PO4(2)'			*mgP/m^3 			mgP/L	(dissolved)
            Write (*,*) 'SI1=,', SI1 / 1000 !,			&					!'Si(1)'			*mgSi/m^3 			mgSi/L	(dissolved)
            Write (*,*) 'SI2=,', SI2 / 1000 !,			&					!'Si(2)'			*mgSi/m^3 			mgSi/L	(dissolved)
            Write (*,*) 'CH41=,', CH41 !,				&					!'CH4(1)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'CH42=,', CH42 !,				&					!'CH4(2)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'SO41=,', SO41 !,				&					!'SO4(1)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'SO42=,', SO42 !,				&					!'SO4(2)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'HSO4=,', HSO4 !,				&					!'HSO4'				m	 				m
            Write (*,*) 'HS1=,', HS1 !,				&					!'HS(1)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'HS2=,', HS2 !,				&					!'HS(2)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'POC1=,', POC1 / 1000 * 2.667 !,	&					!'POC2(1)'			*mgC/m^3 			gO2/m^3	(G1 of POC in layer 2)
            Write (*,*) 'POC2=,', POC2 / 1000 * 2.667 !,	&					!'POC2(2)' 			*mgC/m^3 			gO2/m^3	(G2 of POC in layer 2)
            Write (*,*) 'POC3=,', POC3 / 1000 * 2.667 !,	&					!'POC2(3)' 			*mgC/m^3 			gO2/m^3	(G3 of POC in layer 2)
            Write (*,*) 'PON1=,', PON1 / 1000 !,		&					!'PON2(1)'			*mgN/m^3 			gN/m^3	(G1 of PON in layer 2)
            Write (*,*) 'PON2=,', PON2 / 1000 !,		&					!'PON2(2)' 			*mgN/m^3 			gN/m^3	(G2 of PON in layer 2)
            Write (*,*) 'PON3=,', PON3 / 1000 !,		&					!'PON2(3)' 			*mgN/m^3 			gN/m^3	(G3 of PON in layer 2)
            Write (*,*) 'POP1=,', POP1 / 1000 !,		&					!'POP2(1)'			*mgP/m^3 			gP/m^3	(G1 of POP in layer 2)
            Write (*,*) 'POP2=,', POP2 / 1000 !,		&					!'POP2(2)'			*mgP/m^3 			gP/m^3	(G2 of POP in layer 2)
            Write (*,*) 'POP3=,', POP3 / 1000 !,		&					!'POP2(3)' 			*mgP/m^3 			gP/m^3	(G3 of POP in layer 2)
            Write (*,*) 'PSISED=,', PSISED / 1000 !,	&					!'POS2'				*mgSi/m^3 			gSi/m^3	(particulate biogenic Silicate in layer2)
            Write (*,*) 'CTEMP=', CTEMP (I)
            Write (*,*) 'H1=,', HSED1 (I) * 1000 !,			&					!'H1'				m					mm
            Write (*,*) 'BENSTR=,', BENSTR !'BEN_STR' 			dimensionless		dimensionless
        !
        !		READ(*,*)
         End If
!
         IINT = IINT + 1
         ELTMS = ELTMS + DLT
         JDAY = ELTMS / 86400.
         ELTMJD = JDAY - TMSTRT
     !
         If (JDAY .Ge. TMEND) END_RUN = .True.
!
      End Do
!
  !
  !close files
  !
      Close (BFI)
      Close (BFO)
!
  !deallocate sediment module variables
!
      Call SED_DEALLOC
  !
  !
  !deallocate sav module variables
  !
      If (SAV_CALC) Then
         Call SAV_DEALLOC
         Call SED_SAV_EXCHANGE_DEALLOC
      End If
!
  !
  !deallocate df module variables
  !
      If (DFEEDER) Then
         Call DF_DEALLOC
         Call SED_DF_EXCHANGE_DEALLOC
      End If
  !
  !deallocate sf module variables
  !
  !
      If (SFEEDER) Call SF_DEALLOC
  !
  !deallocate ba module variables
  !
      If (BALGAE_CALC) Call BA_DEALLOC
!
      Stop 'Finished test2aChannel!'
  !
#endif
  !
#if defined (test2b)
!
  !
  !test corresponding to SedFlux_ver17b12_test2b.xlsm
  !
  !Time-variable solution using assumed initial conditions for G classes of POM and assumed constant deposition of POM and constant overlying water quality
  !
!
  !
  !initialize the variables used in the sediment module in the upper
  !modules
  !
  !
  !disable SAV, BA, DF, SF
      SAV_CALC = .False.
      BALGAE_CALC = .False.
      DFEEDER = .False.
      SFEEDER = .False.
!
      BENTHIC_OUTPUT = .True.
!
  !serial run on master processor
      SERIAL = .True.
      PAR = .False.
      MSR = .True.
!
  !
  !set dimension limits
  !
!
      MLOC = 1 !one node
      MTLOC = 1 !one local node
      KBM1 = 10 !10 vertical water column layers
      MGL = 1 !one node
      KWC = KBM1 !
!
  !
  !allocate hydrodynamic variables (no velocities needed, only depth and  box area)
  !
  !
  !		ART1	&!AREA OF NODE-BASE CONTROl VOLUME
  !		,DZ		&!DELTA-SIGMA VALUE of each layer
  !		,D		 !CURRENT DEPTH
!
  !WLong moved from cell_area.F to here
  !
      Allocate (ART1(0:MTLOC))
      ART1 = 0.0_SP !!AREA OF NODE-BASE CONTROl VOLUME
      Allocate (DZ(KBM1+1))
      DZ = 0.0_SP !!DELTA-SIGMA VALUE
      Allocate (D(0:MTLOC))
      D = 0.0
!
      ART1 = 100.d0 !100m^2
      D = 2.0 !2m deep
      DZ = 1.0 / KBM1 !0.1 as KBM1=10
!
  !
  !allocate water column water quality variables
  !
!
      SETTLING = .True.
!
  !settling rates
      Allocate (WSSNET(MTLOC))
      WSSNET = 0.0 !suspended sediments settling rate (m/d)
      Allocate (WSLNET(MTLOC))
      WSLNET = 0.0 !LPOM (labile detritus) settling rate (m/d)
      Allocate (WSRNET(MTLOC))
      WSRNET = 0.0 !RPOM (refractory detriuts) settling rate (m/d)
      Allocate (WS1NET(MTLOC))
      WS1NET = 0.0 !alg 1 settling rate (m/d)
      Allocate (WS2NET(MTLOC))
      WS2NET = 0.0 !alg 2 settling rate (m/d)
      Allocate (WS3NET(MTLOC))
      WS3NET = 0.0 !alg 3 settling rate (m/d)
      Allocate (WSUNET(MTLOC))
      WSUNET = 0.0 !particulate biogenic silica settling rate (m/d)
  !
  !water column constituents
      Allocate (T(0:MTLOC, KBM1))
      T = 0.0 !1
      Allocate (SALT(0:MTLOC, KBM1))
      SALT = 0.0 !2
      Allocate (SSI(0:MTLOC, KBM1))
      SSI = 0.0 !3
      Allocate (B1(0:MTLOC, KBM1))
      B1 = 0.0 !4
      Allocate (B2(0:MTLOC, KBM1))
      B2 = 0.0 !5
      Allocate (B3(0:MTLOC, KBM1))
      B3 = 0.0 !6
      Allocate (LPOC(0:MTLOC, KBM1))
      LPOC = 0.0 !11
      Allocate (RPOC(0:MTLOC, KBM1))
      RPOC = 0.0 !12
      Allocate (NH4(0:MTLOC, KBM1))
      NH4 = 0.0 !13
      Allocate (NO3(0:MTLOC, KBM1))
      NO3 = 0.0 !14
      Allocate (LPON(0:MTLOC, KBM1))
      LPON = 0.0 !18
      Allocate (RPON(0:MTLOC, KBM1))
      RPON = 0.0 !19
      Allocate (PO4(0:MTLOC, KBM1))
      PO4 = 0.0 !20
      Allocate (LPOP(0:MTLOC, KBM1))
      LPOP = 0.0 !23
      Allocate (RPOP(0:MTLOC, KBM1))
      RPOP = 0.0 !24
      Allocate (PIP(0:MTLOC, KBM1))
      PIP = 0.0 !25
      Allocate (COD(0:MTLOC, KBM1))
      COD = 0.0 !26
      Allocate (DOXG(0:MTLOC, KBM1))
      DOXG = 0.0 !27
      Allocate (SIUPB(0:MTLOC, KBM1))
      SIUPB = 0.0 !28
      Allocate (SIAT(0:MTLOC, KBM1))
      SIAT = 0.0 !29
  !
  !sediment temperature
      Allocate (CTEMP(MTLOC))
      CTEMP = 0.0
!
  !P to C ratio (gP/gC) of alg 1 2 and 3
      Allocate (Q1(MTLOC, KBM1))
      Q1 = 0.0
      Allocate (Q2(MTLOC, KBM1))
      Q2 = 0.0
      Allocate (Q3(MTLOC, KBM1))
      Q3 = 0.0
!
  !particle (C, N, P, Si, Suspedned Solids) fluxes
!
      Allocate (PCFWS(MTLOC))
      PCFWS = 0.0
      Allocate (PNFWS(MTLOC))
      PNFWS = 0.0
      Allocate (PPFWS(MTLOC))
      PPFWS = 0.0
      Allocate (PSFWS(MTLOC))
      PSFWS = 0.0
      Allocate (SSFWS(MTLOC))
      SSFWS = 0.0 !this is done in mod_wqm.F
  !
  !benthic fluxes to water column
!
      Allocate (BENCH4A(MTLOC))
      BENCH4A = 0.0
      Allocate (BENCH4G(MTLOC))
      BENCH4G = 0.0
      Allocate (BENCOD(MTLOC))
      BENCOD = 0.0
      Allocate (BENDEN(MTLOC))
      BENDEN = 0.0
      Allocate (BENDO(MTLOC))
      BENDO = 0.0
      Allocate (BENDOC(MTLOC))
      BENDOC = 0.0
      Allocate (BENNH4(MTLOC))
      BENNH4 = 0.0
      Allocate (BENNO3(MTLOC))
      BENNO3 = 0.0
      Allocate (BENPO4(MTLOC))
      BENPO4 = 0.0
      Allocate (BENSA(MTLOC))
      BENSA = 0.0
  !
  !
  !
  !allocate sediment module variables
  !
!
      Call SED_INIT !initialize sediment consituent names and allocate variables for sediment module
!
  !
  !allocate sav module variables
  !
      If (SAV_CALC) Then
         Call SAV_ALLOC
         Call SED_SAV_EXCHANGE_ALLOC
      End If
!
  !
  !allocate df module variables
  !
      If (DFEEDER) Then
         Call DF_ALLOC
         Call SED_DF_EXCHANGE_ALLOC
      End If
  !
  !
  !allocate sf module variables
  !
  !
      If (SFEEDER) Call SF_ALLOC
  !
  !allocate ba module variables
  !
      If (BALGAE_CALC) Call BA_ALLOC
  !
!
  !
  !input and output files for the sediment module
  !
!
!
  !file handles
      BFI = 1
      BFO = 12
!
  !file names
      BFIFN (1) = 'sedtest_2b.inp'
      BFOFN = 'sedtest_2b.chk'
  !
  !
  !set sediment control parameters or read the sediment module control
  !parameters from file BFI
  !
!
      Open (BFI, File=BFIFN(1), Status='OLD')
  !
      Call SED_READ !read some of the basic variables but it will be over written again afterwards
!
  !particle (C, N, P, Si, Suspedned Solids) fluxes
!
      PCFWS = 0.0
      PNFWS = 0.0
      PPFWS = 0.0
      PSFWS = 0.0
      SSFWS = 0.0
  !
  !
  !driving POC, PON, POP, POS, SSI etc fluxes from water colum that should match Jcin, Jnin, Jpin, Jsin,
  !as well as overlying water concentrations
  !
!
  !
  !	TCE_ID	Date+time			Jcin		Jnin		Jpin		Jsin		O20			Depth	Tw		NH30	NO30	PO40	SI0			CH40		SALw
  !      							(gO2/m^2/d)	(gN/m^2/d)	(gP/m^2/d)	(gSi/m^2/d)	(mgO2/L)	(m)		(deg C)	(mgN/L)	(mgN/L)	(mgP/L)	(mgSi/L)	(mgO2/L)	(ppt)
  !		1	1/1/00 12:00 AM		0.300000	0.005000	0.003000	0.009375	5.0			2.00	15.0	0.015	0.100	0.004	0.216		0.0			30.0
  !
  !
  !
  !
  !Set water coumn variables for sediment diagenesis run
  !
  !	Fortran													Excel
  !------------------------------------------------------------------------------------
      T = 15.0 !1  	degC						15				degC
      CTEMPI = 15.0
      SALT = 30.0 !2		ppt							30				ppt
      SSI = 0.0 !3		gSolids/m^3					-
      B1 = 1.0 !4		gC/m^3						-
      B2 = 0.0 !5		gC/m^3						-
      B3 = 0.0 !6		gC/m^3						-
      LPOC = 0.0 !11		gC/m^3						-
      RPOC = 0.0 !12		gC/m^3						-
      NH4 = 0.015 !13		gN/m^3						0.015			mgN/L
      NO3 = 0.1 !14		gN/m^3						0.1				mgN/L
      LPON = 0.0 !18		gN/m^3						-
      RPON = 0.0 !19		gN/m^3						-
      PO4 = 0.004 !20		gP/m^3						0.004			mgP/L
      LPOP = 0.0 !23		gP/m^3						-
      RPOP = 0.0 !24		gP/m^3						-
      PIP = 0.0 !25		gP/m^3						-
      COD = 0.0 !26		gO2/m^3						-
      DOXG = 5.0 !27		gO2/m^3						5				mgO2/L
      SIUPB = 0.0 !28		gSi/m^3						0.0				mgSi/L (i.e. gSi/m^3)
      SIAT = 0.215625 !29		gSi/m^3						0.216			mgSi/L (i.e. gSi/m^3)
  !
  !
  !parameters related to water colum
  !
  !
  !read from MRL (adsorption coefficients of PO4 and Silicate
      KADPO4 = 0.0004 !m^3/gSolids		!READ (MRL,1080)  KADPO4, KADSA
!
  !basically PIE1, see WLong notes (4-7-5) =PIE2PO4*DPIE2PO4=100*300 L/kg
  !=30000L/kg = 30m^3/kg=30/1000 (m^3/g) = 0.3 m^3/g
!
!
      KADSA = 0.001 !m^3/g		!basically PIE2, see WLong notes (4-6-7-1) where
  !m2 is SSI (gSolids/m^3), pie2= should have unit m^3/gSolids
  !i.e. 100L/kg ~ 0.1m^3/kg ~ 0.1m^3/(1000g) = 0.0001 m^3/gSi
  !here it is used to calculate equilibrium speciation of dissolved Si
  !and particulate Si based on suspended solids concentration (gSolids/m^3)
  !in water column
  !
  !Relation between m^3/g and 100L/kg :	0.0001m^3/g = 0.1m^3/kg = 100L/(1kg) = 100L/kg
  !
  !
  !for water column, there is plenty of oxygen, according to WLong notes (4-6-5), we should use PIE1,Si
  !which is 100L/kg * DPIE1SI = 1000L/kg = 1m^3/1000g = 0.001m^3/g
  !
  !
  !	POM flux				Fortran	Formula							Excel						Excel Formula
  !-------------------------------------------------------------------------------------------------------------------
  !	Jcin 			1000*0.3/2.667=112.48		mgC/m^2/d			0.3000   gO2/m^2/d			JPOC(i) = Jcin*fpoc(i)
!
  !JPOC(I,1) = WB1NETMMD*FRCALG1(1)*B1(I,KWC)+										frpoc1=0.65
  !			WB2NETMMD*FRCALG2(1)*B2(I,KWC)+
  !			WB3NETMMD*FRCALG3(1)*B3(I,KWC)+
  !			WLPOMNETMMD*LPOC(I,KWC)
!
  !JPOC(I,2) = WB1NETMMD*FRCALG1(2)*B1(I,KWC)+        								frpoc2=0.2
  !			WB2NETMMD*FRCALG2(2)*B2(I,KWC)+
  !			WB3NETMMD*FRCALG3(2)*B3(I,KWC)+
  !			WRPOMNETMMD*RPOC(I,KWC)*FRPOC(I,2)/
  !			(FRPOC(I,2)+FRPOC(I,3))
!
  !JPOC(I,3) = WB1NETMMD*FRCALG1(3)*B1(I,KWC)+ 										frpoc3=0.15
  !			WB2NETMMD*FRCALG2(3)*B2(I,KWC)+
  !			WB3NETMMD*FRCALG3(3)*B3(I,KWC)+
  !			WRPOMNETMMD*RPOC(I,KWC)*FRPOC(I,3)/
  !				(FRPOC(I,2)+FRPOC(I,3))
!
  !	Jnin 			ANC*1000*0.005/2.667/FRNALG1(1)	mgN/m^2/d		0.005    gN/m^2/d			JPON(i) = Jnin * fpon(i)
  !JPON(I,1) = WB1NETMMD*ANC1*FRNALG1(1)*B1(I,KWC)+									frpon1=0.65
  !			WB2NETMMD*ANC2*FRNALG2(1)*B2(I,KWC)+
  !			WB3NETMMD*ANC3*FRNALG3(1)*B3(I,KWC)+
  !			WLPOMNETMMD*LPON(I,KWC)
  !
  !JPON(I,2) = WB1NETMMD*ANC1*FRNALG1(2)*B1(I,KWC)									frpon2=0.25
  !			+WB2NETMMD*ANC2*FRNALG2(2)*B2(I,KWC)
  !			+WB3NETMMD*ANC3*FRNALG3(2)*B3(I,KWC)
  !			+WRPOMNETMMD*RPON(I,KWC)*FRPON(I,2)/
  !			(FRPON(I,2)+FRPON(I,3))
  !
  !JPON(I,3) = WB1NETMMD*ANC1*FRNALG1(3)*B1(I,KWC) 									frpon3=0.1
  !			+WB2NETMMD*ANC2*FRNALG2(3)*B2(I,KWC)
  !			+WB3NETMMD*ANC3*FRNALG3(3)*B3(I,KWC)
  !			+WRPOMNETMMD*RPON(I,KWC)*FRPON(I,3)/
  !			(FRPON(I,2)+FRPON(I,3))
!
  !	Jpin 			Q*1000*0.003/2.667	mgP/m^2/d					0.003    gP/m^2/d			JPOP(i) = Jpin * fpop(i)
  !JPOP(I,1) = WB1NETMMD*Q1(I,KWC)*FRPALG1(1)*B1(I,KWC)+								frpop1=0.65
  !			WB2NETMMD*Q2(I,KWC)*FRPALG2(1)*B2(I,KWC)+
  !			WB3NETMMD*Q3(I,KWC)*FRPALG3(1)*B3(I,KWC)
  !			WLPOMNETMMD*LPOP(I,KWC)
!
  !JPOP(I,2) = WB1NETMMD*Q1(I,KWC)*FRPALG1(2)*B1(I,KWC)+								frpop2=0.2
  !			WB2NETMMD*Q2(I,KWC)*FRPALG2(2)*B2(I,KWC)+
  !			WB3NETMMD*Q3(I,KWC)*FRPALG3(2)*B3(I,KWC)+
  !			WRPOMNETMMD*RPOP(I,KWC)*FRPOP(I,2)/
  !			(FRPOP(I,2)+FRPOP(I,3))
!
  !JPOP(I,3) = WB1NETMMD*Q1(I,KWC)*FRPALG1(3)*B1(I,KWC)+								frpop3=0.15
  !			+WB2NETMMD*Q2(I,KWC)*FRPALG2(3)*B2(I,KWC)+
  !			+WB3NETMMD*Q3(I,KWC)*FRPALG3(3)*B3(I,KWC)+
  !			+WRPOMNETMMD*RPOP(I,KWC)*FRPOP(I,3)/
  !			(FRPOP(I,2)+FRPOP(I,3))
!
  !	Jsin 			ASC*1000*0.009375/2.667	mgSi/m^2/d					0.009375 gSi/m^2/d			POS2 = (Jsin * tc / H2 + POS2) /
  !																										(1 + KSI * THTASI ^ (Tw - 20) * tc + w2 * tc / H2)
  !JPOS(I) = 	 WB1NETMMD*ASC1*B1(I,KWC)
  !		  	+WB2NETMMD*ASC2*B2(I,KWC)
  !			+WB3NETMMD*ASC3*B3(I,KWC)
  !			+WPSINETMMD*SIUPB(I,KWC)
!
  !
  !POC distribution into G1,G2,G3 by ALG 1,2,3
  !G1 	Alg1				Alg2				Alg3
      FRCALG1 (1) = 0.65
      FRCALG2 (1) = 0.65
      FRCALG3 (1) = 0.65
!
  !G2
      FRCALG1 (2) = 0.20
      FRCALG2 (2) = 0.20
      FRCALG3 (2) = 0.20
!
  !G3
      FRCALG1 (3) = 0.15
      FRCALG2 (3) = 0.15
      FRCALG3 (3) = 0.15
!
!
  !PON distribution into G1,G2,G3 by ALG 1,2,3
  !G1 	Alg1				Alg2				Alg3
      FRNALG1 (1) = 0.65
      FRNALG2 (1) = 0.65
      FRNALG3 (1) = 0.65
!
  !G2
      FRNALG1 (2) = 0.25
      FRNALG2 (2) = 0.25
      FRNALG3 (2) = 0.25
!
  !G3
      FRNALG1 (3) = 0.10
      FRNALG2 (3) = 0.10
      FRNALG3 (3) = 0.10
!
  !POP distribution into G1,G2,G3 by ALG 1,2,3
  !G1 	Alg1				Alg2				Alg3
      FRPALG1 (1) = 0.65
      FRPALG2 (1) = 0.65
      FRPALG3 (1) = 0.65
!
  !G2
      FRPALG1 (2) = 0.20
      FRPALG2 (2) = 0.20
      FRPALG3 (2) = 0.20
!
  !G3
      FRPALG1 (3) = 0.15
      FRPALG2 (3) = 0.15
      FRPALG3 (3) = 0.15
!
  !The following values are given in order to match Jcin, Jnin, Jpin,Jsin in Excel version
      B1 = 1.0
  !only Alg1 settling contributes to POM in sediments
!
      B2 = 0.0
      B3 = 0.0
  !all other particulate organics set to zero
      LPOC = 0.0
      RPOC = 0.0
      LPON = 0.0
      RPON = 0.0
      LPOP = 0.0
      RPOP = 0.0
      SIUPB = 0.0
!
  !Only settling rates of B1 matters for JPOC
      WB1NETMMD = (0.3/2.667) * 1000.0 / B1 (1, KWC)!==>112.4859mm/d 	!here 0.3 is Jcin (gO2/m^2/d) in Excel version
!
  !IDEBUG_TEST=3
  !IF(MSR)WRITE(*,*)'IDEBUG_TEST=',IDEBUG_TEST
  !IF(MSR)WRITE(*,*)'WB1NETMMD=',WB1NETMMD
  !IF(MSR)READ(*,*)
!
      WB2NETMMD = 1000.0 !1000mm/d  	~1m/d
      WB3NETMMD = 1000.0 !1000mm/d	~1m/d
      WLPOMNETMMD = 1000.0 !1000mm/d	~1m/d
      WRPOMNETMMD = 1000.0 !1000mm/d  ~1m/d
      WPSINETMMD = 1000.0 !1000mm/d	~1m/d
!
  !reset WSLNET etc
      WSSBNET (1) = 1.0 !1m/d
      WSLBNET (1) = WLPOMNETMMD / 1000.
      WSRBNET (1) = WRPOMNETMMD / 1000.
      WS1BNET (1) = WB1NETMMD / 1000.0
      WS2BNET (1) = WB2NETMMD / 1000.0
      WS3BNET (1) = WB3NETMMD / 1000.0
      WSUBNET (1) = WPSINETMMD / 1000.0
  !
!
  !ANC1, Q1, and ASC1 are adjusted to match Jnin, Jpin, Jsin based on Jcin
  !
  !redfield ratios of C,N,P for algae and detritus (modified to match Jcin, Jnin, Jpin, Jsin of Excel vresion)
  !
  !JPON=ANC1*WB1NETMMD*B1*FRNALG1(1)*2.667/1000=0.005
!
      ANC1 = (0.005) * 1000.0 / B1 (1, KWC) / WB1NETMMD !==>0.0444 !here 0.005 is Jnin (gN/m^2/d) in Excel version
  !ANC1    =	0.175 	!gN/gC
      ANC2 = 0.175
      ANC3 = 0.175
!
  !P to C ratio (gP/gC) of alg 1 2 and 3 (from AGR file)
  !
      Q1 = (0.003) * 1000.0 / B1 (1, KWC) / WB1NETMMD !==>0.0267	!here 0.003 is Jpin (gP/m^2/d) in Excel version
  !Q1    	= 1/41.1		!see DMD book page 10 Table 1.1
      Q2 = 1 / 41.1 !see DMD book page 10 Table 1.1
      Q3 = 1 / 41.1 !see DMD book page 10 Table 1.1
  !
      ASC1 = (0.009375) * 1000.0 / B1 (1, KWC) / WB1NETMMD !==>0.0833 !here 0.009375 is Jsin in gSi/m^2/d in Excel version
  !ASC1	=	0.333 	!gSi/gC
      ASC2 = 0.333
      ASC3 = 0.333
  !
  !
  !set parameters related to SedFlux_ver17b12_test2b.xlsm
  !
!
  !the parameter lists in SedFlux_ver17b12_test2b.xlsm are listed as follows
  !-------------------------------------------------------------------------------------------------------------------------------------------------
  !parameter		parameter		parameter_meaning										unit			default_value		unit		equation
  !(excel)		(mod_sed.F)																(excel)				(excel)			(mod_sed)	(DMD book)
  !-------------------------------------------------------------------------------------------------------------------------------------------------
  !Particle mixing:
  !
  !
  !m1				M1				solids concentration in aerobic layer 1					kgD/L			0.5					kg/L
  !m2				M2				solids concentration in anaerobic layer 2				kgD/L			0.5					kg/L
  !Dp				DPP	(VPMIX)		bioturbation particle mixing coefficient				m^2/d			0.00006				m^2/d
  !Dd				DDP	(VDMIX)		pore water diffusion coefficient						m^2/d			0.0025				m^2/d
  !w2				W2 (VSED)		deep burial velocity									m/d				6.85E-06			cm/yr *
  !H2				H2(HSED,HSEDALL)thickness of sediment anaerobic layer 2					m				0.1					cm *		*input is cm
  !
      M1 = 0.5
      M2 = 0.5
      DPP = 0.00006
      DDP = 0.0025
      HSEDALL = 10.0 !cm
      Do I = 1, MLOC
         HSED (I) = HSEDALL * 0.01 !m
         VSED (I) = 6.85E-06 !m/d  (W2)
         VPMIX (I) = 0.00006 !m^2/d
         VDMIX (I) = 0.0025 !m^2/d
      End Do
  !
  !Reaction velocities (depth integrated reaction rate = reaction velocity (m/d))
  !
  !KappaNH3f		KAPPNH4F		freshwater nitrification velocity						m/d				0.131				m/d
  !KappaNH3s		KAPPNH4S		saltwater nitrification velocity						m/d				0.131				m/d
  !KappaNO3_1f	KAPPNO3F		freshwater denitrification velocity in layer 1			m/d				0.1					m/d
  !KappaNO3_1s	KAPPNO3S		saltwater denitrification velocity in layer 1			m/d				0.1					m/d
  !KappaNO3_2		K2NO3			denitrfication in the anaerobic layer 2					m/d				0.25				m/d
  !KappaCH4		KAPPCH4			methane oxidation in the aerobic layer 1				m/d				0.7					m/d
  !
      KAPPNH4F = 0.131 !m/d
      KAPPNH4S = 0.131 !m/d
      KAPPNO3F = 0.1 !m/d
      KAPPNO3S = 0.1 !m/d
      K2NO3 = 0.25 !m/d		!DMD P.105 Table 4.1
      KAPPCH4 = 0.7 !m/d
!
  !Half saturation constants
  !
  !KM_NH3			KMNH4			nitrification half saturation for NH4N					mgN/L			0.728				mgN/m^3	*
  !KM_O2_NH3		KMNH4O2			nitrification half saturation for O2					mgO2/L			0.37				mgO2/L
  !
      KMNH4 = 0.728 * 1000. !(mgN/m^3)		!see DMD book p73 Table 3.1 (b) (and Median)
  !maybe DMD book is wrong on units
      KMNH4O2 = 0.37 !mgO2/L			!See DMD book p73 Tabl3 3.1 (Median)
!
  !Partitioning coefficients
  !
  !KdNH3			PIENH4			partition coefficient for NH4 in layer 1 and 2			L/kgD			1					L/kg
  !KdPO42			PIE2PO4			partition coefficient for PO4 in layer 2				L/kgD			20					L/kg
  !dKDPO41f		DPIE1PO4F		freshwater factor that increases the aerobic 			unitless		20					L/kg
  !									layer partition coefficient of inorganic P
  !dKDPO41s		DPIE1PO4S		saltwater factor that increases the aerobic 			unitless		20					L/kg
  !									layer partition coefficient of inorganic P
  !O2critPO4		O2CRITPO4		critical O2 concentration in layer 1 for 				mgO2/L			2					mgO2/L
  !									adjustment of partition coefficient
  !									for inorganic P
  !
      PIENH4 = 1.0 !L/kg		(inversed of m1,m2 unit)
      PIE2PO4 = 20.0 !L/kg
      DPIE1PO4F = 20.0 !WLong?? Unitless?	Yes! Note DMD book pp 572 says it should be about 300
      DPIE1PO4S = 20.0 !WLong?? Unitless?
      O2CRITPO4 = 2.0 !mgO2/L
!
  !Temperature coefficients
  !
  !ThtaDp			THTADP			temperature theta for bioturbation mixing between 		unitless		1.117				unitless
  !									layers 1 and 2
  !ThtaDd			THTADD			temperature theta for pore water diffusion between 		unitless		1.08				unitless
  !									layers 1 and 2
  !ThtaKmNH3		THTAKMNH4		temperature theta for nitrification half saturation 	unitless		1					unitless
  !									for NH4N
  !ThtaNH3		THTANH4			temperature theta for nitrification rate				unitless		1.123				unitless
  !ThtaNO3		THTANO3			temperature theta for denitrification rate				unitless		1.08				unitless
  !ThtaCH4		THTACH4			temperature theta for methane oxidation rate			unitless		1.079				unitless
  !
      THTADP = 1.117
      THTADD = 1.08
  !!ThtaKmNH3	=	!Applied to KM_NH3 in Excel, should be applied to KMNH4 in Fortran code
      THTAKMNH4 = 1.0 !not effective, since it value is 1 anyway
  !See DMD book p.73 Table 3.1 (median value is 1.125)
      THTANH4 = 1.123 !See DMD book p.73 Table 3.1 (median value is 1.123)
      THTANO3 = 1.08
      THTACH4 = 1.079
!
  !Salinity thresholds
  !
  !SALTSW			SALTSW			salinity above which sulfide rather than 				psu				1					ppt
  !									methane is produced from C diagenesis
  !SALTND			SALTND			salinity above which saltwater 							psu				1					ppt
  !									nitrification/denitrification rates are
  !									used for aerobic layer
      SALTSW = 1.0 !ppt
      SALTND = 1.0 !ppt
!
  !Sulfide constants
  !
  !KappaH2Sd1		KAPP1HSD		aerobic layer reaction velocity for dissolved 			m/d				0.2					m/d
  !								sulfide oxidation
  !KappaH2Sp1		KAPP1HSP		aerobic layer reaction velocity for particulate 		m/d				0.4					m/d
  !								sulfide oxidation
  !ThtaH2S		THTAH2S			temperature coefficient for sulfide oxidation			unitless		1.079				unitless
  !KMHSO2			KMHSO2			sulfide oxidation normalization constant for O2			mgO2/L			4					mgO2/L
  !KdH2S1			PIE1HS			partition coefficient for sulfide in aerobic layer 1	L/kgD			100					L/kg	*
  !KdH2S2			PIE2HS			partition coefficient for sulfide in anaerobic layer 2	L/kgD			100					L/kg	*
  !
      KAPP1HSD = 0.2 !m/d
      KAPP1HSP = 0.4 !m/d
      THTAH2S = 1.079 !unitless
      KMHSO2 = 4.0 !mgO2/L
      PIE1HS = 100 !L/kg
      PIE2HS = 100 !L/kg
  !
!
  !Fractions of G classes 1 and 2 for settling RPON, RPOC, and RPOP
  !
  !frpon1			FRPON(:,1)		fraction of class 1 pon									unitless		0.65				unitless * only for detritus
  !frpon2			FRPON(:,2)		fraction of class 2 pon									unitless		0.25				unitless * only for detritus
!
  !frpoc1			FRPOC(:,1)		fraction of class 1 poc									unitless		0.65				unitless * only for detritus
  !frpoc2			FRPOC(:,2)		fraction of class 2 poc									unitless		0.2					unitless * only for detritus
!
  !frpop1			FRPOP(:,1)		fraction of class 1 pop									unitless		0.65				unitless * only for detritus
  !frpop2			FRPOP(:,2)		fraction of class 2 pop									unitless		0.2					unitless * only for detritus
!
  !*******WLong: Fortran code used this completely differently!!!!*****
  !Fortran code uses these as fraction of RPOM to be split in G2 and G3
  !G1 has no share of RPOM
!
      Do I = 1, MLOC
         FRPON (I, 1) = 0.0 !No share for G1
         FRPON (I, 2) = 0.65
         FRPON (I, 3) = 1 - FRPON (I, 2) - FRPON (I, 1)
!
         FRPOC (I, 1) = 0.0 !No share for G1
         FRPOC (I, 2) = 0.65
         FRPOC (I, 3) = 1 - FRPOC (I, 1) - FRPOC (I, 2)
!
         FRPOP (I, 1) = 0.0 !No share for G1
         FRPOP (I, 2) = 0.65
         FRPOP (I, 3) = 1 - FRPOP (I, 1) - FRPOP (I, 2)
     !
      End Do
!
  !Diagenesis rate constants for G clase 1, 2, and 3 N/C/P
  !
  !kpon1			KPON1			G class 1 pon mineralization							day^-1			0.035				1/d
  !kpon2			KPON2			G class 2 pon mineralization							day^-1			0.0018				1/d
  !kpon3			KPON3			G class 3 pon mineralization							day^-1			0					1/d
  !kpoc1			KPOC1			G class 1 poc mineralization							day^-1			0.035				1/d
  !kpoc2			KPOC2			G class 2 poc mineralization							day^-1			0.0018				1/d
  !kpoc3			KPOC3			G class 3 poc mineralization							day^-1			0					1/d
  !kpop1			KPOP1			G class 1 pop mineralization							day^-1			0.035				1/d
  !kpop2			KPOP2			G class 2 pop mineralization							day^-1			0.0018				1/d
  !kpop3			KPOP3			G class 3 pop mineralization							day^-1			0					1/d
  !
      KPON1 = 0.035 !1/d
      KPON2 = 0.0018 !1/d
      KPON3 = 0. !1/d
!
      KPOC1 = 0.035 !1/d
      KPOC2 = 0.0018 !1/d
      KPOC3 = 0. !1/d
!
      KPOP1 = 0.035 !1/d
      KPOP2 = 0.0018 !1/d
      KPOP3 = 0. !1/d
  !
  !Temperature coefficients for G class 1, 2, and 3 mineralization
  !
  !ThtaPON1		THTAPON1		temperature theta for G class 1 pon						unitless		1.1
  !ThtaPON2		THTAPON2		temperature theta for G class 2 pon						unitless		1.15
  !ThtaPON3		THTAPON3		temperature theta for G class 3 pon						unitless		1.17
  !ThtaPOC1		THTAPOC1		temperature theta for G class 1 poc						unitless		1.1
  !ThtaPOC2		THTAPOC2		temperature theta for G class 2 poc						unitless		1.15
  !ThtaPOC3		THTAPOC3		temperature theta for G class 3 poc						unitless		1.17
  !ThtaPOP1		THTAPOP1		temperature theta for G class 1 pop						unitless		1.1
  !ThtaPOP2		THTAPOP2		temperature theta for G class 2 pop						unitless		1.15
  !ThtaPOP3		THTAPOP3		temperature theta for G class 3 pop						unitless		1.17
  !
      THTAPON1 = 1.1
      THTAPON2 = 1.15
      THTAPON3 = 1.17
!
      THTAPOC1 = 1.1
      THTAPOC2 = 1.15
      THTAPOC3 = 1.17
!
      THTAPOP1 = 1.1
      THTAPOP2 = 1.15
      THTAPOP3 = 1.17
!
  !Parameters for partical mixing and benthic stress
  !
  !POC1R			POC1R			reference G1 at which w12base = Dp / H2 at 				mgO2/gD			0.2667				mgC/gD			*Not available
  !									20 degC for DiToro eqn 13.1
  !
  !kBEN_STR		KBENSTR 		first-order decay rate constant for benthic  			day^-1			0.03				1/d
  !									stress (d^-1) for DiToro eqn 13.3
  !
  !KM_O2_Dp		KMO2DP			particle mixing half-saturation constant 				mgO2/L			4					mgO2/L
  !									for O2 (mgO2/L)
!
  ! ??			KLBNTH			ratio of bio-irrigation to bioturbation					unitless
  ! ?? 			DPMIN			minimum particule mixing rate (m^2/day)
!
      POC1R = 0.1 !mgC/gSediment  DMD eqn (13.1) !0.2667/2.667
  !WLong notes eqn(3-26) (DMD book page 278, table 13.1, eqn(13.1))
      KBENSTR = 0.03 !1/d
      KMO2DP = 4.0 !mgO2/L  ~ gO2/m^3
!
      KLBNTH = 0.d0 !to make sure no bioturbation
      DPMIN = 0.d0 !to make minimum  particle diffusion rate zero
  !
  !Parameters for silica
  !
  !KSI			KSI				reaction rate for particulate biogenic Si 				day^-1			0.5					1/d
  !THTASI			THTASI			temperature theta for KSI								unitless		1.1					unitless
  !CSISAT			CSISATT20		saturation concentration for porewater Si				mgSi/L			40					mg Si/m3 	*
  !THTASISAT		THTASISAT		temperature theta for CSISAT							unitless		1.023							*Not available
  !DPIE1SI		DPIE1SI			incremental partition coefficient for Si in layer 1		Unitless		10					Unitless
  !PIE2SI			PIE2SI			partition coefficient for Si in layer 2					L/kgD			100					L/kg
  !KMPSI			KMPSI			particulate biogenic Si half saturation 				mgSi/L			50				mg Si/m3	*
  !									constant for dissolution
  !O2CRITSI		O2CRITSI		critical O2 concentration for layer 1 					mgO2/L			2					O2CRITSI
  !									incremental Si sorption
  !
      KSI = 0.5 !1/d
      THTASI = 1.1 !dimensionless		!DMD book p155 Tabel 7.1
      CSISATT20 = 40 * 1000.0 !mgSi/m^3
      THTASISAT = 1.023 !dimensionless
      DPIE1SI = 10 !unitless
      PIE2SI = 100 !L/kg
      KMPSI = 50000.0 * 1000.0 !mgSi/m^3			!DMD book p156 100mgSi/gsediment ==> p573 50000mgSi/L ==> 50,000,000 mgSi/m^3
      O2CRITSI = 2.0 !mgO2/L ~ gO2/m^3 !used for calculating PIE1SI
      JSIDETR = 0.d0 !not used for now
  !
  !Parameters for temperature
  !
  !DIFFT=1000.0 !m^2/sec      !used for calculating temperature in sediments
      DIFFT = 1000.0 / 86400000.0000000 / 5.0
  !
  !initial conditions for sediment concentratins
  !
!
  !----------------------------------------------------------------------------------------------------------------------------------------------------------------
  !parameter		parameter		parameter_meaning										unit			initial_value		unit			IC			equation
  !(excel)		(mod_sed.F)																(excel)				(excel)			(mod_sed)		(fortran)	(DMD book)
  !---------------------------------------------------------------------------------------------------------------------------------------------------------------
  !
  !Particulate organic C, N, and P in layer 2
!
  !POC2(1)		CPOCI(1)		G class 1 POC in layer 2								gO2/m^3			89.446				mgC/m^3	*		(89.446/2.667)*1000
  !POC2(2)		CPOCI(2)		G class 2 POC in layer 2								gO2/m^3			622.783				mgC/m^3	*		(622.783/2.667)*1000
  !POC2(3)		CPOCI(3)		G class 3 POC in layer 2								gO2/m^3			6569.343			mgC/m^3	*		(6569.343/2.667)*1000
  !PON2(1)		CPONI(1)		G class 1 PON in layer 2								gN/m^3			1.491				mgN/m^3			1.491*1000
  !PON2(2)		CPONI(2)		G class 2 PON in layer 2								gN/m^3			12.975				mgN/m^3			12.975*1000
  !PON2(3)		CPONI(3)		G class 3 PON in layer 2								gN/m^3			72.993				mgN/m^3			72.993*1000
  !POP2(1)		CPOPI(1)		G class 1 POP in layer 2								gP/m^3			0.894				mgP/m^3			0.894*1000
  !POP2(2)		CPOPI(2)		G class 2 POP in layer 2								gP/m^3			6.228				mgP/m^3			6.228*1000
  !POP2(3)		CPOPI(3)		G class 3 POP in layer 2								gP/m^3			65.693				mgP/m^3			65.693*1000
  !POS2			CPOSI			Particulate biogenic Si in layer 2						gSi/m^3			0.302				mgSi/m^3		0.302*1000
  !
  !Sediment temperature and benthic stress
  !*				CTEMPI																					??					degC			15.0
  !*				BENSTI																					0.0									0.0
!
  !Dissolved constituents in layer 1 and 2 porewater
!
  !NH3(1)			*NH4T1I			Dissolved ammonia N in layer 1 porewater				mgN/L			0.024				mgN/m^3			0.024*1000
  !NH3(2)			NH4T2I			Dissolved ammonia N in layer 2 porewater				mgN/L			0.152				mgN/m^3			0.152*1000
  !NO3(1)			*NO3T1I			Dissolved nitrate+nitrite N in layer 1 porewater		mgN/L			0.041				mgN/m^3			0.041*1000
  !NO3(2)			NO3T2I			Dissolved nitrate+nitrite N in layer 2 porewater		mgN/L			0.007				mgN/m^3			0.007*1000
  !PO4(1)			*PO4T1I			Dissolved phosphate P in layer 1 porewater				mgP/L			0.054				mgP/m^3			0.054*1000
  !PO4(2)			PO4T2I			Dissolved phosphate P in layer 2 porewater				mgP/L			0.290				mgP/m^3			0.290*1000
  !Si(1)			*SIT1I			Dissolved Si in layer 1 porewater						mgSi/L			0.209				mgSi/m^3        0.209*1000
  !Si(2)			SIT2I			Dissolved Si in layer 2 porewater						mgSi/L			1.160				mgSi/m^3		1.160*1000
!
  !HS(1)			*HST1I			Dissolved sulfide in layer 1 porewater 					mgO2/L			0.002               gO2/m^3			0.002
  !									(used if salinity >= 1 psu)
  !HS(2)			HST2I			Dissolved sulfide in layer 2 porewater					mgO2/L			3.348				gO2/m^3			3.348
  !								 	(used if salinity >= 1 psu)
  !CH4(1)			CH41TI			Dissolved methane in layer 1 porewater 					mgO2/L			0.000				gO2/m^3			0.000
  !								 	(used if salinity >= 1 psu)
  !CH4(2)			CH4T2I			Dissolved methane in layer 2 porewater 					mgO2/L			0.000				gO2/m^3			0.000
  !									(used if salinity >= 1 psu)
  !**				SO4T2I																	mgO2/L			??					mgO2/L			0.000
  !*				SO4T2I																	mgO2/L			??					mgO2/L			0.000
!
      CPOCI (1) = 1000 * 89.446 / 2.667 !mgC/m^3
      CPOCI (2) = 1000 * 622.783 / 2.667 !mgC/m^3
      CPOCI (3) = 1000 * 6569.343 / 2.667 !mgC/m^3
      CPONI (1) = 1000 * 1.491 !mgN/m^3
      CPONI (2) = 1000 * 12.975 !mgN/m^3
      CPONI (3) = 1000 * 72.993 !mgN/m^3
      CPOPI (1) = 1000 * 0.894 !mgP/m^3
      CPOPI (2) = 1000 * 6.228 !mgP/m^3
      CPOPI (3) = 1000 * 65.693 !mgP/m^3
      BENSTI = 0.0 !unitless		!Fortran only ??
      CPOSI = 1000 * 0.302 !mgSi/m^3
!
  !
  !Note that in Excel version, concentrations are given as dissolved , then total concenration is calculated based on m1,m2 and PIENH4
  !
  !fd1 = (1# / (1# + m1 * KdNH3))
  !fd2 = (1# / (1# + m2 * KdNH3))
  !NH3Tp2(1) = NH3p2(1) / fd1
  !NH3Tp2(2) = NH3p2(2) / fd2
  !
  !m1=m2=0.5, PIENH4=1 ==> fd1=(1/(1+0.5*1))=1/1.5=0.3333
  !						fd2=(1/(1+0.5*1))=1/1.5=0.3333
  !
      PIE1 = PIENH4
      PIE2 = PIENH4
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      NH41TI = 1000 * 0.024 !mgN/m^3		!Excel only
      NH4T2I = 1000 * (0.152) / FD2 !mgN/m^3
!
      NO31TI = 1000 * 0.041 !mgN/m^3		!Excel only
      NO3T2I = 1000 * 0.007 !mgN/m^3
!
!
  !also make sure PO4T2I is total PO4
      I = 1
      If (SALT(I, KBM1) < SALTND) Then
         DPIE1PO4 = DPIE1PO4F
      Else
         DPIE1PO4 = DPIE1PO4S
      End If
      If (O20 > O2CRITPO4) Then
         PIE1 = PIE2PO4 * DPIE1PO4
      Else
         PIE1 = PIE2PO4 * DPIE1PO4 ** (O20/O2CRITPO4)
      End If
      PIE2 = PIE2PO4
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      PO41TI = 1000 * 0.054 !mgP/m^3		!Excel only
      PO4T2I = 1000 * 0.290 / FD2 !converted to total value
!
      If (O20 > O2CRITSI) Then
         PIE1 = PIE2SI * DPIE1SI
      Else
         PIE1 = PIE2SI * DPIE1SI ** (O20/O2CRITSI)
      End If
      PIE2 = PIE2SI
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      SI1TI = 1000 * 0.209 !mgSi/m^3		!Excel only
      SIT2I = 1000 * 1.160 / FD2 !mgSi/m^3		!converted to total Si in layer 2
!
!
      PIE1 = PIE1HS
      PIE2 = PIE2HS
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      HS1TI = 0.002 !mgO2/L			!Excel only
      HST2I = 3.348 / FD2 !mgO2/L			!converted to total HS
!
      CH4T2I = 0.0 !mgO2/L
      CH41TI = 0.0 !mgO2/L
!
      SO41TI = 0.0 !mgO2/L			!Neither Excel Nor Fortran
      SO4T2I = 0.0 !mgO2/L			!Fortran only
  !
      SODI = 0.1 !mgO2/m^2/d		!Initialize SOD !WLong: may need to call this SODI
  !Greg Pelletier's Excel used JSOD=0.1 mgO2/m^2/d
!
      Do I = 1, MLOC
!
         CTEMP (I) = CTEMPI !Sediment temperature
!
         Do JG = 1, 3
            CPOC (I, JG) = CPOCI (JG)
            CPON (I, JG) = CPONI (JG)
            CPOP (I, JG) = CPOPI (JG)
         End Do
!
         CPOS (I) = CPOSI
         BENSTRTM1S (I) = BENSTI !*not in excel
         SODTM1S (I) = SODI
         NH41TM1S (I) = NH41TI
         NH4T2TM1S (I) = NH4T2I
!
         NO31TM1S (I) = NO31TI
         NO3T2TM1S (I) = NO3T2I
!
         PO41TM1S (I) = PO41TI
         PO4T2TM1S (I) = PO4T2I
!
         SI1TM1S (I) = SI1TI
         SIT2TM1S (I) = SIT2I
!
         HS1TM1S (I) = HS1TI
         HST2TM1S (I) = HST2I
!
         CH41TM1S (I) = CH41TI
         CH4T2TM1S (I) = CH4T2I
!
         SO4T2TM1S (I) = SO4T2I !*not in excel
     !
      End Do
!
  !reset the intial values of varables that involve some of the above parameters
      Call SED_INIT2
  !
  !open output file
!
      BFOFN = 'sedtest_2b.csv'
  !
      If (MSR) Then
!
         Open (Unit=BFO, File=BFOFN)
         Rewind (BFO)
!
         IDEBUG_TEST = 2
         If (MSR) WRITE (*,*) 'IDEBUG_TEST=', IDEBUG_TEST
!
     !
     !write the headline the same as excel file "SedFlux_ver17b12_test2b.xlsm"
     !
!
     !Segment ID	JDAY+Time	Jcin	Jnin	Jpin	Jsin	O20	Depth	Tw	NH30	NO30	PO40	SI0	CH40	SALw
     !				SOD	Jnh4	Jno3	JDenitT	Jch4	Jch4g	Jhs	Jpo4	Jsi	Jcout	Jnout	Jpout	NH3(1)	NH3(2)
     !				NO3(1)	NO3(2)	PO4(1)	PO4(2)	Si(1)	Si(2)	CH4(1)	CH4(2)	HS(1)	HS(2)	POC2(1)	POC2(2)	POC2(3)
     !				PON2(1)	PON2(2)	PON2(3)	POP2(1)	POP2(2)	POP2(3)	POS2 H1	BEN_STR
     !
         I = 1
         Write (BFO, 1001) 'SegmentID', ',', 'JDAY', ',', 'Jcin', ',', 'Jnin', ',', 'Jpin', ',', 'Jsin', ',', 'O20', ',', 'Depth', ',', 'Tw', ',', 'NH40', ',', 'NO30', ',', 'PO40', ',', 'SI0', ',', 'CH40', ',', 'SALw', ',', 'SOD', ',', 'Jnh4', ',', 'Jno3',&
        &  ',', 'JDenitT', ',', 'Jch4', ',', 'Jch4g', ',', 'Jhs', ',', 'Jpo4', ',', 'Jsi', ',', 'Jcout', ',', 'Jnout', ',', 'Jpout', ',', 'NH3(1)', ',', 'NH3(2)', ',', 'NO3(1)', ',', 'NO3(2)', ',', 'PO4(1)', ',', 'PO4(2)', ',', 'Si(1)', ',', 'Si(2)', ',', &
        & 'CH4(1)', ',', 'CH4(2)', ',', 'HS(1)', ',', 'HS(2)', ',', 'POC2(1)', ',', 'POC2(2)', ',', 'POC2(3)', ',', 'PON2(1)', ',', 'PON2(2)', ',', 'PON2(3)', ',', 'POP2(1)', ',', 'POP2(2)', ',', 'POP2(3)', ',', 'POS2', ',', 'H1', ',', 'BEN_STR'
!
      End If
1000  Format (I10, 1000(1 X, A10, F15.8))
1001  Format (A10, 1000(1 X, A10, A15))
  !
  !do test 2a calculations
!
      TMSTRT = 0.0 !reference time
      TMEND = 700.0 !end time
      JDAY = 0.0 !time in days
      DLT = 0.01 * 86400 !10.0		!time step in sec = 864sec=0.01 day=14.4min
      ELTMS = 0.0 !time in sec
      ELTMJD = 0.0 !time in days after reference time
      DLTS = DLT / 86400.
!
      END_RUN = .False.
!
      IINT = 0 !time step counter
!
      Do WHILE ( .Not. END_RUN)
     !
         If (MSR) WRITE (*,*) 'JDAY=', JDAY
!
         Call POM_ACCUMUL
     !
     !average the accumulated POM flux over NDTSED steps
         Do I = 1, MLOC
            Do K = 1, 3
               JPOC (I, K) = JPOCaccum (I, K)
               JPON (I, K) = JPONaccum (I, K)
               JPOP (I, K) = JPOPaccum (I, K)
            End Do
            JPOS (I) = JPOSaccum (I)
         End Do
     !
         Call SED_CALC (.False.)!Calculate with time variable formulation
     !
     !set accumulative fluxes back to zero
         JPOCaccum = 0.0
         JPONaccum = 0.0
         JPOPaccum = 0.0
         JPOSaccum = 0.0
     !
     !
     !write out outputs
     !
         If (MSR) Then
        !Fortran							Excel				FortranUnit			ExcelUnit
            I = 1
            Write (BFO, 1000) I, ',', JDAY, ',', (JPOC(I, 1)+JPOC(I, 2)+&
           & JPOC(I, 3)) / 1000.0 * 2.667, ',', (JPON(I, 1)+JPON(I, 2)+&
           & JPON(I, 3)) / 1000.0, ',', (JPOP(I, 1)+JPOP(I, 2)+JPOP(I, &
           & 3)) / 1000.0, ',', JPOS (I) / 1000.0, ',', O20, ',', D (I),&
           &  ',', T (I, KBM1), ',', NH40 / 1000.0, ',', NO30 / 1000.0, &
           & ',', PO40 / 1000.0, ',', SI0 / 1000.0, ',', CH40, ',', SAL,&
           &  ',', SOD, ',', JNH4 / 1000.0, ',', JNO3 / 1000.0, ',', &
           & BENDEN (1), ',', JCH4, ',', JCH4G, ',', JHS, ',', JPO4 / &
           & 1000.0, ',', JSI / 1000.0, ',', 0.0, ',', 0.0, ',', 0.0, '&
           &,', NH41 / 1000.0, ',', NH42 / 1000.0, ',', NO31 / 1000.0, &
           & ',', NO32 / 1000.0, ',', PO41 / 1000.0, ',', PO42 / 1000.0,&
           &  ',', SI1 / 1000.0, ',', SI2 / 1000.0, ',', CH41, ',', &
           & CH42, ',', HS1, ',', HS2, ',', POC1 / 1000.0 * 2.667, ',', &
           & POC2 / 1000.0 * 2.667, ',', POC3 / 1000.0 * 2.667, ',', &
           & PON1 / 1000.0, ',', PON2 / 1000.0, ',', PON3 / 1000.0, ',',&
           &  POP1 / 1000.0, ',', POP2 / 1000.0, ',', POP3 / 1000.0, ',&
           &', PSISED / 1000.0, ',', HSED1 (I) * 1000.0, ',', BENSTR !'BEN_STR' 			dimensionless		dimensionless
        ! WRITE(BFO,1000)I,',',JDAY,			&
        !	',',(JPOC(I,1)+JPOC(I,2)+JPOC(I,3))/1000.0*2.667,	&	!'Jcin'				mgC/m^2/d			gO2/m^2/d
        !	',',(JPON(I,1)+JPON(I,2)+JPON(I,3))/1000.0,	&			!'Jnin'				*mgN/m^2/d			gN/m^2/d
        !	',',(JPOP(I,1)+JPOP(I,2)+JPOP(I,3))/1000.0,	&			!'Jpin'				*mgP/m^2/d			gP/m^2/d
        !	',',JPOS(I)/1000.0,	&									!'Jsin		 		*mgSi/m^2/d			gSi/m^2/d
        !	',',O20,			&									!'O20'				mgO2/L				mgO2/L
        !	',',D(I),			&									!'Depth'			m					m
        !	',',T(I,KBM1),		&									!'Tw'				degC				degC
        !	',',NH40/1000.0,		&									!'NH30'				*mgN/m^3			mgN/L
        !	',',NO30/1000.0,		&									!'NO30'				*mgN/m^3			mgN/L
        !	',',PO40/1000.0,		&									!'PO40'				*mgP/m^3			mgP/L
        !	',',SI0/1000.0,		&									!'SI0'				*mgSi/m^3 			mgSi/L				(dissiolved SIAT)
        !	',',CH40,			&									!'CH40'				gO2/m^3				mgO2/L
        !	',',SAL,			&									!'SALw'  			ppt					ppt					!i.e. SALT(I,KWC), ppt
        !	',',SOD,			&									!'SOD'				gO2/m^2/day			gO2/m^2/d
        !	',',JNH4/1000.0,		&									!'Jnh4				*mgN/m^2/day		gN/m^2/d
        !	',',JNO3/1000.0,		&									!'Jno3				*mgN/m^2/day		gN/m^2/d
        !	',',BENDEN(1),		&									!'JDenitT'			gN/m^2/day			gN/m^2/d
        !							!JDenit(1) = Denit(1) * NO3(1)
        !							!JDenit(2) = Denit(2) * NO3(2)
        !							!JDenitT = JDenit(1) + JDenit(2)
        !	',',JCH4,			&									!'Jch4'				gO2/m^2/day			gO2/m^2/d
        !	',',JCH4G,			&									!'Jch4g'			gO2/m^2/day			gO2/m^2/d
        !	',',JHS,			&									!'Jhs'				gO2/m^2/day			gO2/m^2/d
        !	',',JPO4/1000.0,		&									!'Jpo4'				*mgP/m^2/day		gP/m^2/d
        !	',',JSI/1000.0,		&									!'Jsi'				*mgSi/m^2/day		gSi/m^2/d
        !	',',0.0,			&									!'Jcout'
        !	',',0.0,			&									!'Jnout'
        !	',',0.0,			&									!'Jpout'
        !         !
        ! WLong: Excel calculations here, should correspond to Jcin, Jnin and Jpin
        ! Jcout = SOD + JCH4 + JCH4g + JHS +  	(POC2(1) + POC2(2) + POC2(3)) * w2 +
        !						(		(POC2(1) + POC2(2) + POC2(3))
        !							- 	(POC22(1) + POC22(2) + POC22(3))
        !						) * H2 / tc
        ! Jnout = JNO3 + JNH4 + JDenitT + 		(PON2(1) + PON2(2) + PON2(3)) * w2 +
        !						(		(PON2(1) + PON2(2) + PON2(3))
        !							- 	(PON22(1) + PON22(2) + PON22(3))
        !						) * H2 / tc
        ! Jpout = JPO4 + 						(POP2(1) + POP2(2) + POP2(3)) * w2 +
        !						((POP2(1) + POP2(2) + POP2(3))
        !						 - (POP22(1) + POP22(2) + POP22(3))
        !						 ) * H2 / tc
        !
        !	',',NH41/1000.0,		&					!'NH3(1)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',NH42/1000.0,		&					!'NH3(2)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',NO31/1000.0,		&					!'NO3(1)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',NO32/1000.0,		&					!'NO3(2)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',PO41/1000.0,		&					!'PO4(1)'			*mgP/m^3 			mgP/L	(dissolved)
        !	',',PO42/1000.0,		&					!'PO4(2)'			*mgP/m^3 			mgP/L	(dissolved)
        !	',',SI1/1000.0,		&					!'Si(1)'			*mgSi/m^3 			mgSi/L	(dissolved)
        !	',',SI2/1000.0,		&					!'Si(2)'			*mgSi/m^3 			mgSi/L	(dissolved)
        !	',',CH41,			&					!'CH4(1)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',CH42,			&					!'CH4(2)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',HS1,			&					!'HS(1)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',HS2,			&					!'HS(2)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',POC1/1000.0*2.667,&					!'POC2(1)'			*mgC/m^3 			gO2/m^3	(G1 of POC in layer 2)
        !	',',POC2/1000.0*2.667,&					!'POC2(2)' 			*mgC/m^3 			gO2/m^3	(G2 of POC in layer 2)
        !	',',POC3/1000.0*2.667,&					!'POC2(3)' 			*mgC/m^3 			gO2/m^3	(G3 of POC in layer 2)
        !	',',PON1/1000.0,		&					!'PON2(1)'			*mgN/m^3 			gN/m^3	(G1 of PON in layer 2)
        !	',',PON2/1000.0,		&					!'PON2(2)' 			*mgN/m^3 			gN/m^3	(G2 of PON in layer 2)
        !	',',PON3/1000.0,		&					!'PON2(3)' 			*mgN/m^3 			gN/m^3	(G3 of PON in layer 2)
        !	',',POP1/1000.0,		&					!'POP2(1)'			*mgP/m^3 			gP/m^3	(G1 of POP in layer 2)
        !	',',POP2/1000.0,		&					!'POP2(2)'			*mgP/m^3 			gP/m^3	(G2 of POP in layer 2)
        !	',',POP3/1000.0,		&					!'POP2(3)' 			*mgP/m^3 			gP/m^3	(G3 of POP in layer 2)
        !	',',PSISED/1000.0,	&					!'POS2'				*mgSi/m^3 			gSi/m^3	(particulate biogenic Silicate in layer2)
        !	',',HSED1(I)*1000.0,&					!'H1'				m					mm
        !	',',BENSTR			 					!'BEN_STR' 			dimensionless		dimensionless
        !
            Write (*,*) 1, ',', JDAY
            Write (*,*) 'JPOC=,', (JPOC(I, 1)+JPOC(I, 2)+JPOC(I, 3)) / &
           & 1000.0 * 2.667, 'gO2/m^2/d'
        !'Jcin'				mgC/m^2/d			gO2/m^2/d
!
        !',WS1NET(1)=',WS1NET(I), &!
             !',WB1NETMMD=',WB1NETMMD, 					&	!
        !',B1=',B1(I,KWC),&!
        !',FRACALG1(1)=',FRCALG1(1),&!
        !'JPOC(I,1)=,',JPOC(I,1)/1000*2.667, &	!						*mgC/m^2/d			gO2/m^2/d
        !'JPON(I,1)=,',JPON(I,1)/1000.0, 	&	!						*mgN/m^2/d			gN/m^2/d
        !'JPOP(I,1)=,',JPOP(I,1)/1000.0, 	&	!						*mgP/m^2/d			gP/m^2/d
        !'JPOS(I)  =,',JPOS(I)/1000.0,		&	!						*mgSi/m^2/d			gSi/m^2/d
!
            Write (*,*) 'JPON=,', (JPON(I, 1)+JPON(I, 2)+JPON(I, 3)) / &
           & 1000.0, 'gN/m^2/d'
        !'Jnin'				*mgN/m^2/d			gN/m^2/d
            Write (*,*) 'JPOP=,', (JPOP(I, 1)+JPOP(I, 2)+JPOP(I, 3)) / &
           & 1000.0, 'gP/m^2/d'
        !'Jpin'				*mgP/m^2/d			gP/m^2/d
            Write (*,*) 'JPOS=,', JPOS (I) / 1000.0, 'gSi/m^2/d'
        !'Jsin		 		*mgSi/m^2/d			gSi/m^2/d
            Write (*,*)
            Write (*,*) 'O20=,', O20 !,			&					!'O20'				mgO2/L				mgO2/L
            Write (*,*) 'D=,', D (I)!,			&					!'Depth'			m					m
            Write (*,*) 'T=,', T (I, KBM1)!,		&					!'Tw'				degC				degC
            Write (*,*) 'NH40=,', NH40 / 1000 !,	&					!'NH30'				*mgN/m^3			mgN/L
            Write (*,*) 'NO30=,', NO30 / 1000 !,	&					!'NO30'				*mgN/m^3			mgN/L
            Write (*,*) 'PO40=,', PO40 / 1000 !,	&					!'PO40'				*mgP/m^3			mgP/L
            Write (*,*) 'SI0=,', SI0 / 1000 !,		&					!'SI0'				*mgSi/m^3 			mgSi/L				(dissiolved SIAT)
            Write (*,*) 'CH40=,', CH40 !,			&					!'CH40'				gO2/m^3				mgO2/L
            Write (*,*) 'SAL=,', SAL !,			&					!'SALw'  			ppt					ppt					!i.e. SALT(I,KWC), ppt
            Write (*,*) 'SOD=,', SOD !,			&					!'SOD'				gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JNH4=,', JNH4 / 1000 !,	&					!'Jnh4				*mgN/m^2/day		gN/m^2/d
            Write (*,*) 'JNO3=,', JNO3 / 1000 !,	&					!'Jno3				*mgN/m^2/day		gN/m^2/d
            Write (*,*) 'JDeniT=,', BENDEN (1)!,	&					!'JDenitT'			gN/m^2/day			gN/m^2/d
        !JDenit(1) = Denit(1) * NO3(1)
        !JDenit(2) = Denit(2) * NO3(2)
        !JDenitT = JDenit(1) + JDenit(2)
            Write (*,*) 'JCH4=,', JCH4 !,			&					!'Jch4'				gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JCH4G=,', JCH4G !,		&					!'Jch4g'			gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JHS=,', JHS !,			&					!'Jhs'				gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JPO4=,', JPO4 / 1000 !,	&					!'Jpo4'				*mgP/m^2/day		gP/m^2/d
            Write (*,*) 'JSI=,', JSI / 1000 !,		&					!'Jsi'				*mgSi/m^2/day		gSi/m^2/d
!
        !						',',0.0,				&					!'Jcout'
        !						',',0.0,				&					!'Jnout'
        !						',',0.0,				&					!'Jpout'
!
        !
        !WLong: Excel calculations here, should correspond to Jcin, Jnin and Jpin
        !Jcout = SOD + JCH4 + JCH4g + JHS +  	(POC2(1) + POC2(2) + POC2(3)) * w2 +
        !										(		(POC2(1) + POC2(2) + POC2(3))
        !											- 	(POC22(1) + POC22(2) + POC22(3))
        !										) * H2 / tc
        !Jnout = JNO3 + JNH4 + JDenitT + 		(PON2(1) + PON2(2) + PON2(3)) * w2 +
        !										(		(PON2(1) + PON2(2) + PON2(3))
        !											- 	(PON22(1) + PON22(2) + PON22(3))
        !										) * H2 / tc
        !Jpout = JPO4 + 						(POP2(1) + POP2(2) + POP2(3)) * w2 +
        !										((POP2(1) + POP2(2) + POP2(3))
        !										 - (POP22(1) + POP22(2) + POP22(3))
        !										 ) * H2 / tc
        !
            Write (*,*) 'NH41=,', NH41 / 1000 !,		&					!'NH3(1)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'NH42=,', NH42 / 1000 !,		&					!'NH3(2)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'NO31=,', NO31 / 1000 !,		&					!'NO3(1)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'NO32=,', NO32 / 1000 !,		&					!'NO3(2)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'PO41=,', PO41 / 1000 !,		&					!'PO4(1)'			*mgP/m^3 			mgP/L	(dissolved)
            Write (*,*) 'PO42=,', PO42 / 1000 !,		&					!'PO4(2)'			*mgP/m^3 			mgP/L	(dissolved)
            Write (*,*) 'SI1=,', SI1 / 1000 !,			&					!'Si(1)'			*mgSi/m^3 			mgSi/L	(dissolved)
            Write (*,*) 'SI2=,', SI2 / 1000 !,			&					!'Si(2)'			*mgSi/m^3 			mgSi/L	(dissolved)
            Write (*,*) 'CH41=,', CH41 !,				&					!'CH4(1)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'CH42=,', CH42 !,				&					!'CH4(2)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'SO41=,', SO41 !,				&					!'SO4(1)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'SO42=,', SO42 !,				&					!'SO4(2)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'HSO4=,', HSO4 !,				&					!'HSO4'				m	 				m
            Write (*,*) 'HS1=,', HS1 !,				&					!'HS(1)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'HS2=,', HS2 !,				&					!'HS(2)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'POC1=,', POC1 / 1000 * 2.667 !,	&					!'POC2(1)'			*mgC/m^3 			gO2/m^3	(G1 of POC in layer 2)
            Write (*,*) 'POC2=,', POC2 / 1000 * 2.667 !,	&					!'POC2(2)' 			*mgC/m^3 			gO2/m^3	(G2 of POC in layer 2)
            Write (*,*) 'POC3=,', POC3 / 1000 * 2.667 !,	&					!'POC2(3)' 			*mgC/m^3 			gO2/m^3	(G3 of POC in layer 2)
            Write (*,*) 'PON1=,', PON1 / 1000 !,		&					!'PON2(1)'			*mgN/m^3 			gN/m^3	(G1 of PON in layer 2)
            Write (*,*) 'PON2=,', PON2 / 1000 !,		&					!'PON2(2)' 			*mgN/m^3 			gN/m^3	(G2 of PON in layer 2)
            Write (*,*) 'PON3=,', PON3 / 1000 !,		&					!'PON2(3)' 			*mgN/m^3 			gN/m^3	(G3 of PON in layer 2)
            Write (*,*) 'POP1=,', POP1 / 1000 !,		&					!'POP2(1)'			*mgP/m^3 			gP/m^3	(G1 of POP in layer 2)
            Write (*,*) 'POP2=,', POP2 / 1000 !,		&					!'POP2(2)'			*mgP/m^3 			gP/m^3	(G2 of POP in layer 2)
            Write (*,*) 'POP3=,', POP3 / 1000 !,		&					!'POP2(3)' 			*mgP/m^3 			gP/m^3	(G3 of POP in layer 2)
            Write (*,*) 'PSISED=,', PSISED / 1000 !,	&					!'POS2'				*mgSi/m^3 			gSi/m^3	(particulate biogenic Silicate in layer2)
            Write (*,*) 'CTEMP=', CTEMP (I)
            Write (*,*) 'H1=,', HSED1 (I) * 1000 !,			&					!'H1'				m					mm
            Write (*,*) 'BENSTR=,', BENSTR !'BEN_STR' 			dimensionless		dimensionless
        !
        !		READ(*,*)
         End If
!
         IINT = IINT + 1
         ELTMS = ELTMS + DLT
         JDAY = ELTMS / 86400.
         ELTMJD = JDAY - TMSTRT
     !
         If (JDAY .Ge. TMEND) END_RUN = .True.
!
      End Do
!
  !
  !close files
  !
      Close (BFI)
      Close (BFO)
!
  !deallocate sediment module variables
!
      Call SED_DEALLOC
  !
  !
  !deallocate sav module variables
  !
      If (SAV_CALC) Then
         Call SAV_DEALLOC
         Call SED_SAV_EXCHANGE_DEALLOC
      End If
!
  !
  !deallocate df module variables
  !
      If (DFEEDER) Then
         Call DF_DEALLOC
         Call SED_DF_EXCHANGE_DEALLOC
      End If
  !
  !deallocate sf module variables
  !
  !
      If (SFEEDER) Call SF_DEALLOC
  !
  !deallocate ba module variables
  !
      If (BALGAE_CALC) Call BA_DEALLOC
      Stop 'Finished test2b!'
  !
  !
#endif
!
#if defined (test2bChannel)
!
  !
  !test corresponding to SedFlux_ver17b12_test2b.xlsm
  !
  !Time-variable solution using assumed initial conditions for G classes of POM and assumed constant deposition of POM and constant overlying water quality
  !
!
  !
  !initialize the variables used in the sediment module in the upper
  !modules
  !
  !
  !disable SAV, BA, DF, SF
      SAV_CALC = .False.
      BALGAE_CALC = .False.
      DFEEDER = .False.
      SFEEDER = .False.
!
      BENTHIC_OUTPUT = .True.
!
  !serial run on master processor
      SERIAL = .True.
      PAR = .False.
      MSR = .True.
!
  !
  !set dimension limits
  !
!
      MLOC = 1 !one node
      MTLOC = 1 !one local node
      KBM1 = 10 !10 vertical water column layers
      MGL = 1 !one node
      KWC = KBM1 !
!
  !
  !allocate hydrodynamic variables (no velocities needed, only depth and  box area)
  !
  !
  !		ART1	&!AREA OF NODE-BASE CONTROl VOLUME
  !		,DZ		&!DELTA-SIGMA VALUE of each layer
  !		,D		 !CURRENT DEPTH
!
  !WLong moved from cell_area.F to here
  !
      Allocate (ART1(0:MTLOC))
      ART1 = 0.0_SP !!AREA OF NODE-BASE CONTROl VOLUME
      Allocate (DZ(KBM1+1))
      DZ = 0.0_SP !!DELTA-SIGMA VALUE
      Allocate (D(0:MTLOC))
      D = 0.0
!
      ART1 = 100.d0 !100m^2
      D = 2.0 !2m deep
      DZ = 1.0 / KBM1 !0.1 as KBM1=10
!
  !
  !allocate water column water quality variables
  !
!
      SETTLING = .True.
!
  !settling rates
      Allocate (WSSNET(MTLOC))
      WSSNET = 0.0 !suspended sediments settling rate (m/d)
      Allocate (WSLNET(MTLOC))
      WSLNET = 0.0 !LPOM (labile detritus) settling rate (m/d)
      Allocate (WSRNET(MTLOC))
      WSRNET = 0.0 !RPOM (refractory detriuts) settling rate (m/d)
      Allocate (WS1NET(MTLOC))
      WS1NET = 0.0 !alg 1 settling rate (m/d)
      Allocate (WS2NET(MTLOC))
      WS2NET = 0.0 !alg 2 settling rate (m/d)
      Allocate (WS3NET(MTLOC))
      WS3NET = 0.0 !alg 3 settling rate (m/d)
      Allocate (WSUNET(MTLOC))
      WSUNET = 0.0 !particulate biogenic silica settling rate (m/d)
  !
  !water column constituents
      Allocate (T(0:MTLOC, KBM1))
      T = 0.0 !1
      Allocate (SALT(0:MTLOC, KBM1))
      SALT = 0.0 !2
      Allocate (SSI(0:MTLOC, KBM1))
      SSI = 0.0 !3
      Allocate (B1(0:MTLOC, KBM1))
      B1 = 0.0 !4
      Allocate (B2(0:MTLOC, KBM1))
      B2 = 0.0 !5
      Allocate (B3(0:MTLOC, KBM1))
      B3 = 0.0 !6
      Allocate (LPOC(0:MTLOC, KBM1))
      LPOC = 0.0 !11
      Allocate (RPOC(0:MTLOC, KBM1))
      RPOC = 0.0 !12
      Allocate (NH4(0:MTLOC, KBM1))
      NH4 = 0.0 !13
      Allocate (NO3(0:MTLOC, KBM1))
      NO3 = 0.0 !14
      Allocate (LPON(0:MTLOC, KBM1))
      LPON = 0.0 !18
      Allocate (RPON(0:MTLOC, KBM1))
      RPON = 0.0 !19
      Allocate (PO4(0:MTLOC, KBM1))
      PO4 = 0.0 !20
      Allocate (LPOP(0:MTLOC, KBM1))
      LPOP = 0.0 !23
      Allocate (RPOP(0:MTLOC, KBM1))
      RPOP = 0.0 !24
      Allocate (PIP(0:MTLOC, KBM1))
      PIP = 0.0 !25
      Allocate (COD(0:MTLOC, KBM1))
      COD = 0.0 !26
      Allocate (DOXG(0:MTLOC, KBM1))
      DOXG = 0.0 !27
      Allocate (SIUPB(0:MTLOC, KBM1))
      SIUPB = 0.0 !28
      Allocate (SIAT(0:MTLOC, KBM1))
      SIAT = 0.0 !29
  !
  !sediment temperature
      Allocate (CTEMP(MTLOC))
      CTEMP = 0.0
!
  !P to C ratio (gP/gC) of alg 1 2 and 3
      Allocate (Q1(MTLOC, KBM1))
      Q1 = 0.0
      Allocate (Q2(MTLOC, KBM1))
      Q2 = 0.0
      Allocate (Q3(MTLOC, KBM1))
      Q3 = 0.0
!
  !particle (C, N, P, Si, Suspedned Solids) fluxes
!
      Allocate (PCFWS(MTLOC))
      PCFWS = 0.0
      Allocate (PNFWS(MTLOC))
      PNFWS = 0.0
      Allocate (PPFWS(MTLOC))
      PPFWS = 0.0
      Allocate (PSFWS(MTLOC))
      PSFWS = 0.0
      Allocate (SSFWS(MTLOC))
      SSFWS = 0.0 !this is done in mod_wqm.F
  !
  !benthic fluxes to water column
!
      Allocate (BENCH4A(MTLOC))
      BENCH4A = 0.0
      Allocate (BENCH4G(MTLOC))
      BENCH4G = 0.0
      Allocate (BENCOD(MTLOC))
      BENCOD = 0.0
      Allocate (BENDEN(MTLOC))
      BENDEN = 0.0
      Allocate (BENDO(MTLOC))
      BENDO = 0.0
      Allocate (BENDOC(MTLOC))
      BENDOC = 0.0
      Allocate (BENNH4(MTLOC))
      BENNH4 = 0.0
      Allocate (BENNO3(MTLOC))
      BENNO3 = 0.0
      Allocate (BENPO4(MTLOC))
      BENPO4 = 0.0
      Allocate (BENSA(MTLOC))
      BENSA = 0.0
  !
  !
  !
  !allocate sediment module variables
  !
!
      Call SED_INIT !initialize sediment consituent names and allocate variables for sediment module
!
  !
  !allocate sav module variables
  !
      If (SAV_CALC) Then
         Call SAV_ALLOC
         Call SED_SAV_EXCHANGE_ALLOC
      End If
!
  !
  !allocate df module variables
  !
      If (DFEEDER) Then
         Call DF_ALLOC
         Call SED_DF_EXCHANGE_ALLOC
      End If
  !
  !
  !allocate sf module variables
  !
  !
      If (SFEEDER) Call SF_ALLOC
  !
  !allocate ba module variables
  !
      If (BALGAE_CALC) Call BA_ALLOC
  !
!
  !
  !input and output files for the sediment module
  !
!
!
  !file handles
      BFI = 1
      BFO = 12
!
  !file names
      BFIFN (1) = 'sedtest_2b.inp'
      BFOFN = 'sedtest_2b.chk'
  !
  !
  !set sediment control parameters or read the sediment module control
  !parameters from file BFI
  !
!
      Open (BFI, File=BFIFN(1), Status='OLD')
  !
      Call SED_READ !read some of the basic variables but it will be over written again afterwards
!
  !particle (C, N, P, Si, Suspedned Solids) fluxes
!
      PCFWS = 0.0
      PNFWS = 0.0
      PPFWS = 0.0
      PSFWS = 0.0
      SSFWS = 0.0
  !
  !
  !driving POC, PON, POP, POS, SSI etc fluxes from water colum that should match Jcin, Jnin, Jpin, Jsin,
  !as well as overlying water concentrations
  !
!
  !
  !	TCE_ID	Date+time			Jcin		Jnin		Jpin		Jsin		O20			Depth	Tw		NH30	NO30	PO40	SI0			CH40		SALw
  !      							(gO2/m^2/d)	(gN/m^2/d)	(gP/m^2/d)	(gSi/m^2/d)	(mgO2/L)	(m)		(deg C)	(mgN/L)	(mgN/L)	(mgP/L)	(mgSi/L)	(mgO2/L)	(ppt)
  !		1	1/1/00 12:00 AM		0.300000	0.005000	0.003000	0.009375	5.0			2.00	15.0	0.015	0.100	0.004	0.216		0.0			30.0
  !
  !
  !
  !
  !Set water coumn variables for sediment diagenesis run
  !
  !	Fortran													Excel
  !------------------------------------------------------------------------------------
      T = 15.0 !1  	degC						15				degC
      CTEMPI = 15.0
      SALT = 30.0 !2		ppt							30				ppt
      SSI = 0.0 !3		gSolids/m^3					-
      B1 = 1.0 !4		gC/m^3						-
      B2 = 0.0 !5		gC/m^3						-
      B3 = 0.0 !6		gC/m^3						-
      LPOC = 0.0 !11		gC/m^3						-
      RPOC = 0.0 !12		gC/m^3						-
      NH4 = 0.015 !13		gN/m^3						0.015			mgN/L
      NO3 = 0.1 !14		gN/m^3						0.1				mgN/L
      LPON = 0.0 !18		gN/m^3						-
      RPON = 0.0 !19		gN/m^3						-
      PO4 = 0.004 !20		gP/m^3						0.004			mgP/L
      LPOP = 0.0 !23		gP/m^3						-
      RPOP = 0.0 !24		gP/m^3						-
      PIP = 0.0 !25		gP/m^3						-
      COD = 0.0 !26		gO2/m^3						-
      DOXG = 5.0 !27		gO2/m^3						5				mgO2/L
      SIUPB = 0.0 !28		gSi/m^3						0.0				mgSi/L (i.e. gSi/m^3)
      SIAT = 0.215625 !29		gSi/m^3						0.216			mgSi/L (i.e. gSi/m^3)
  !
  !
  !parameters related to water colum
  !
  !
  !read from MRL (adsorption coefficients of PO4 and Silicate
      KADPO4 = 0.0004 !m^3/gSolids		!READ (MRL,1080)  KADPO4, KADSA
!
  !basically PIE1, see WLong notes (4-7-5) =PIE2PO4*DPIE2PO4=100*300 L/kg
  !=30000L/kg = 30m^3/kg=30/1000 (m^3/g) = 0.3 m^3/g
!
!
      KADSA = 0.001 !m^3/g		!basically PIE2, see WLong notes (4-6-7-1) where
  !m2 is SSI (gSolids/m^3), pie2= should have unit m^3/gSolids
  !i.e. 100L/kg ~ 0.1m^3/kg ~ 0.1m^3/(1000g) = 0.0001 m^3/gSi
  !here it is used to calculate equilibrium speciation of dissolved Si
  !and particulate Si based on suspended solids concentration (gSolids/m^3)
  !in water column
  !
  !Relation between m^3/g and 100L/kg :	0.0001m^3/g = 0.1m^3/kg = 100L/(1kg) = 100L/kg
  !
  !
  !for water column, there is plenty of oxygen, according to WLong notes (4-6-5), we should use PIE1,Si
  !which is 100L/kg * DPIE1SI = 1000L/kg = 1m^3/1000g = 0.001m^3/g
  !
  !
  !	POM flux				Fortran	Formula							Excel						Excel Formula
  !-------------------------------------------------------------------------------------------------------------------
  !	Jcin 			1000*0.3/2.667=112.48		mgC/m^2/d			0.3000   gO2/m^2/d			JPOC(i) = Jcin*fpoc(i)
!
  !JPOC(I,1) = WB1NETMMD*FRCALG1(1)*B1(I,KWC)+										frpoc1=0.65
  !			WB2NETMMD*FRCALG2(1)*B2(I,KWC)+
  !			WB3NETMMD*FRCALG3(1)*B3(I,KWC)+
  !			WLPOMNETMMD*LPOC(I,KWC)
!
  !JPOC(I,2) = WB1NETMMD*FRCALG1(2)*B1(I,KWC)+        								frpoc2=0.2
  !			WB2NETMMD*FRCALG2(2)*B2(I,KWC)+
  !			WB3NETMMD*FRCALG3(2)*B3(I,KWC)+
  !			WRPOMNETMMD*RPOC(I,KWC)*FRPOC(I,2)/
  !			(FRPOC(I,2)+FRPOC(I,3))
!
  !JPOC(I,3) = WB1NETMMD*FRCALG1(3)*B1(I,KWC)+ 										frpoc3=0.15
  !			WB2NETMMD*FRCALG2(3)*B2(I,KWC)+
  !			WB3NETMMD*FRCALG3(3)*B3(I,KWC)+
  !			WRPOMNETMMD*RPOC(I,KWC)*FRPOC(I,3)/
  !				(FRPOC(I,2)+FRPOC(I,3))
!
  !	Jnin 			ANC*1000*0.005/2.667/FRNALG1(1)	mgN/m^2/d		0.005    gN/m^2/d			JPON(i) = Jnin * fpon(i)
  !JPON(I,1) = WB1NETMMD*ANC1*FRNALG1(1)*B1(I,KWC)+									frpon1=0.65
  !			WB2NETMMD*ANC2*FRNALG2(1)*B2(I,KWC)+
  !			WB3NETMMD*ANC3*FRNALG3(1)*B3(I,KWC)+
  !			WLPOMNETMMD*LPON(I,KWC)
  !
  !JPON(I,2) = WB1NETMMD*ANC1*FRNALG1(2)*B1(I,KWC)									frpon2=0.25
  !			+WB2NETMMD*ANC2*FRNALG2(2)*B2(I,KWC)
  !			+WB3NETMMD*ANC3*FRNALG3(2)*B3(I,KWC)
  !			+WRPOMNETMMD*RPON(I,KWC)*FRPON(I,2)/
  !			(FRPON(I,2)+FRPON(I,3))
  !
  !JPON(I,3) = WB1NETMMD*ANC1*FRNALG1(3)*B1(I,KWC) 									frpon3=0.1
  !			+WB2NETMMD*ANC2*FRNALG2(3)*B2(I,KWC)
  !			+WB3NETMMD*ANC3*FRNALG3(3)*B3(I,KWC)
  !			+WRPOMNETMMD*RPON(I,KWC)*FRPON(I,3)/
  !			(FRPON(I,2)+FRPON(I,3))
!
  !	Jpin 			Q*1000*0.003/2.667	mgP/m^2/d					0.003    gP/m^2/d			JPOP(i) = Jpin * fpop(i)
  !JPOP(I,1) = WB1NETMMD*Q1(I,KWC)*FRPALG1(1)*B1(I,KWC)+								frpop1=0.65
  !			WB2NETMMD*Q2(I,KWC)*FRPALG2(1)*B2(I,KWC)+
  !			WB3NETMMD*Q3(I,KWC)*FRPALG3(1)*B3(I,KWC)
  !			WLPOMNETMMD*LPOP(I,KWC)
!
  !JPOP(I,2) = WB1NETMMD*Q1(I,KWC)*FRPALG1(2)*B1(I,KWC)+								frpop2=0.2
  !			WB2NETMMD*Q2(I,KWC)*FRPALG2(2)*B2(I,KWC)+
  !			WB3NETMMD*Q3(I,KWC)*FRPALG3(2)*B3(I,KWC)+
  !			WRPOMNETMMD*RPOP(I,KWC)*FRPOP(I,2)/
  !			(FRPOP(I,2)+FRPOP(I,3))
!
  !JPOP(I,3) = WB1NETMMD*Q1(I,KWC)*FRPALG1(3)*B1(I,KWC)+								frpop3=0.15
  !			+WB2NETMMD*Q2(I,KWC)*FRPALG2(3)*B2(I,KWC)+
  !			+WB3NETMMD*Q3(I,KWC)*FRPALG3(3)*B3(I,KWC)+
  !			+WRPOMNETMMD*RPOP(I,KWC)*FRPOP(I,3)/
  !			(FRPOP(I,2)+FRPOP(I,3))
!
  !	Jsin 			ASC*1000*0.009375/2.667	mgSi/m^2/d					0.009375 gSi/m^2/d			POS2 = (Jsin * tc / H2 + POS2) /
  !																										(1 + KSI * THTASI ^ (Tw - 20) * tc + w2 * tc / H2)
  !JPOS(I) = 	 WB1NETMMD*ASC1*B1(I,KWC)
  !		  	+WB2NETMMD*ASC2*B2(I,KWC)
  !			+WB3NETMMD*ASC3*B3(I,KWC)
  !			+WPSINETMMD*SIUPB(I,KWC)
!
  !
  !POC distribution into G1,G2,G3 by ALG 1,2,3
  !G1 	Alg1				Alg2				Alg3
      FRCALG1 (1) = 0.65
      FRCALG2 (1) = 0.65
      FRCALG3 (1) = 0.65
!
  !G2
      FRCALG1 (2) = 0.20
      FRCALG2 (2) = 0.20
      FRCALG3 (2) = 0.20
!
  !G3
      FRCALG1 (3) = 0.15
      FRCALG2 (3) = 0.15
      FRCALG3 (3) = 0.15
!
!
  !PON distribution into G1,G2,G3 by ALG 1,2,3
  !G1 	Alg1				Alg2				Alg3
      FRNALG1 (1) = 0.65
      FRNALG2 (1) = 0.65
      FRNALG3 (1) = 0.65
!
  !G2
      FRNALG1 (2) = 0.25
      FRNALG2 (2) = 0.25
      FRNALG3 (2) = 0.25
!
  !G3
      FRNALG1 (3) = 0.10
      FRNALG2 (3) = 0.10
      FRNALG3 (3) = 0.10
!
  !POP distribution into G1,G2,G3 by ALG 1,2,3
  !G1 	Alg1				Alg2				Alg3
      FRPALG1 (1) = 0.65
      FRPALG2 (1) = 0.65
      FRPALG3 (1) = 0.65
!
  !G2
      FRPALG1 (2) = 0.20
      FRPALG2 (2) = 0.20
      FRPALG3 (2) = 0.20
!
  !G3
      FRPALG1 (3) = 0.15
      FRPALG2 (3) = 0.15
      FRPALG3 (3) = 0.15
!
  !The following values are given in order to match Jcin, Jnin, Jpin,Jsin in Excel version
      B1 = 1.0
  !only Alg1 settling contributes to POM in sediments
!
      B2 = 0.0
      B3 = 0.0
  !all other particulate organics set to zero
      LPOC = 0.0
      RPOC = 0.0
      LPON = 0.0
      RPON = 0.0
      LPOP = 0.0
      RPOP = 0.0
      SIUPB = 0.0
!
  !Only settling rates of B1 matters for JPOC
      WB1NETMMD = (0.3/2.667) * 1000.0 / B1 (1, KWC)!==>112.4859mm/d 	!here 0.3 is Jcin (gO2/m^2/d) in Excel version
!
  !IDEBUG_TEST=3
  !IF(MSR)WRITE(*,*)'IDEBUG_TEST=',IDEBUG_TEST
  !IF(MSR)WRITE(*,*)'WB1NETMMD=',WB1NETMMD
  !IF(MSR)READ(*,*)
!
      WB2NETMMD = 1000.0 !1000mm/d  	~1m/d
      WB3NETMMD = 1000.0 !1000mm/d	~1m/d
      WLPOMNETMMD = 1000.0 !1000mm/d	~1m/d
      WRPOMNETMMD = 1000.0 !1000mm/d  ~1m/d
      WPSINETMMD = 1000.0 !1000mm/d	~1m/d
!
  !reset WSLNET etc
      WSSBNET (1) = 1.0 !1m/d
      WSLBNET (1) = WLPOMNETMMD / 1000.
      WSRBNET (1) = WRPOMNETMMD / 1000.
      WS1BNET (1) = WB1NETMMD / 1000.0
      WS2BNET (1) = WB2NETMMD / 1000.0
      WS3BNET (1) = WB3NETMMD / 1000.0
      WSUBNET (1) = WPSINETMMD / 1000.0
  !
!
  !ANC1, Q1, and ASC1 are adjusted to match Jnin, Jpin, Jsin based on Jcin
  !
  !redfield ratios of C,N,P for algae and detritus (modified to match Jcin, Jnin, Jpin, Jsin of Excel vresion)
  !
  !JPON=ANC1*WB1NETMMD*B1*FRNALG1(1)*2.667/1000=0.005
!
      ANC1 = (0.005) * 1000.0 / B1 (1, KWC) / WB1NETMMD !==>0.0444 !here 0.005 is Jnin (gN/m^2/d) in Excel version
  !ANC1    =	0.175 	!gN/gC
      ANC2 = 0.175
      ANC3 = 0.175
!
  !P to C ratio (gP/gC) of alg 1 2 and 3 (from AGR file)
  !
      Q1 = (0.003) * 1000.0 / B1 (1, KWC) / WB1NETMMD !==>0.0267	!here 0.003 is Jpin (gP/m^2/d) in Excel version
  !Q1    	= 1/41.1		!see DMD book page 10 Table 1.1
      Q2 = 1 / 41.1 !see DMD book page 10 Table 1.1
      Q3 = 1 / 41.1 !see DMD book page 10 Table 1.1
  !
      ASC1 = (0.009375) * 1000.0 / B1 (1, KWC) / WB1NETMMD !==>0.0833 !here 0.009375 is Jsin in gSi/m^2/d in Excel version
  !ASC1	=	0.333 	!gSi/gC
      ASC2 = 0.333
      ASC3 = 0.333
  !
  !
  !set parameters related to SedFlux_ver17b12_test2b.xlsm
  !
!
  !the parameter lists in SedFlux_ver17b12_test2b.xlsm are listed as follows
  !-------------------------------------------------------------------------------------------------------------------------------------------------
  !parameter		parameter		parameter_meaning										unit			default_value		unit		equation
  !(excel)		(mod_sed.F)																(excel)				(excel)			(mod_sed)	(DMD book)
  !-------------------------------------------------------------------------------------------------------------------------------------------------
  !Particle mixing:
  !
  !
  !m1				M1				solids concentration in aerobic layer 1					kgD/L			0.5					kg/L
  !m2				M2				solids concentration in anaerobic layer 2				kgD/L			0.5					kg/L
  !Dp				DPP	(VPMIX)		bioturbation particle mixing coefficient				m^2/d			0.00006				m^2/d
  !Dd				DDP	(VDMIX)		pore water diffusion coefficient						m^2/d			0.0025				m^2/d
  !w2				W2 (VSED)		deep burial velocity									m/d				6.85E-06			cm/yr *
  !H2				H2(HSED,HSEDALL)thickness of sediment anaerobic layer 2					m				0.1					cm *		*input is cm
  !
      M1 = 0.5
      M2 = 0.5
      DPP = 0.00006
      DDP = 0.0025
      HSEDALL = 10.0 !cm
      Do I = 1, MLOC
         HSED (I) = HSEDALL * 0.01 !m
         VSED (I) = 6.85E-06 !m/d  (W2)
         VPMIX (I) = 0.00006 !m^2/d
         VDMIX (I) = 0.0025 !m^2/d
      End Do
  !
  !Reaction velocities (depth integrated reaction rate = reaction velocity (m/d))
  !
  !KappaNH3f		KAPPNH4F		freshwater nitrification velocity						m/d				0.131				m/d
  !KappaNH3s		KAPPNH4S		saltwater nitrification velocity						m/d				0.131				m/d
  !KappaNO3_1f	KAPPNO3F		freshwater denitrification velocity in layer 1			m/d				0.1					m/d
  !KappaNO3_1s	KAPPNO3S		saltwater denitrification velocity in layer 1			m/d				0.1					m/d
  !KappaNO3_2		K2NO3			denitrfication in the anaerobic layer 2					m/d				0.25				m/d
  !KappaCH4		KAPPCH4			methane oxidation in the aerobic layer 1				m/d				0.7					m/d
  !
      KAPPNH4F = 0.131 !m/d
      KAPPNH4S = 0.131 !m/d
      KAPPNO3F = 0.1 !m/d
      KAPPNO3S = 0.1 !m/d
      K2NO3 = 0.25 !m/d		!DMD P.105 Table 4.1
      KAPPCH4 = 0.7 !m/d
!
  !Half saturation constants
  !
  !KM_NH3			KMNH4			nitrification half saturation for NH4N					mgN/L			0.728				mgN/m^3	*
  !KM_O2_NH3		KMNH4O2			nitrification half saturation for O2					mgO2/L			0.37				mgO2/L
  !
      KMNH4 = 0.728 * 1000. !(mgN/m^3)		!see DMD book p73 Table 3.1 (b) (and Median)
  !maybe DMD book is wrong on units
      KMNH4O2 = 0.37 !mgO2/L			!See DMD book p73 Tabl3 3.1 (Median)
!
  !Partitioning coefficients
  !
  !KdNH3			PIENH4			partition coefficient for NH4 in layer 1 and 2			L/kgD			1					L/kg
  !KdPO42			PIE2PO4			partition coefficient for PO4 in layer 2				L/kgD			20					L/kg
  !dKDPO41f		DPIE1PO4F		freshwater factor that increases the aerobic 			unitless		20					L/kg
  !									layer partition coefficient of inorganic P
  !dKDPO41s		DPIE1PO4S		saltwater factor that increases the aerobic 			unitless		20					L/kg
  !									layer partition coefficient of inorganic P
  !O2critPO4		O2CRITPO4		critical O2 concentration in layer 1 for 				mgO2/L			2					mgO2/L
  !									adjustment of partition coefficient
  !									for inorganic P
  !
      PIENH4 = 1.0 !L/kg		(inversed of m1,m2 unit)
      PIE2PO4 = 20.0 !L/kg
      DPIE1PO4F = 20.0 !WLong?? Unitless?	Yes! Note DMD book pp 572 says it should be about 300
      DPIE1PO4S = 20.0 !WLong?? Unitless?
      O2CRITPO4 = 2.0 !mgO2/L
!
  !Temperature coefficients
  !
  !ThtaDp			THTADP			temperature theta for bioturbation mixing between 		unitless		1.117				unitless
  !									layers 1 and 2
  !ThtaDd			THTADD			temperature theta for pore water diffusion between 		unitless		1.08				unitless
  !									layers 1 and 2
  !ThtaKmNH3		THTAKMNH4		temperature theta for nitrification half saturation 	unitless		1					unitless
  !									for NH4N
  !ThtaNH3		THTANH4			temperature theta for nitrification rate				unitless		1.123				unitless
  !ThtaNO3		THTANO3			temperature theta for denitrification rate				unitless		1.08				unitless
  !ThtaCH4		THTACH4			temperature theta for methane oxidation rate			unitless		1.079				unitless
  !
      THTADP = 1.117
      THTADD = 1.08
  !!ThtaKmNH3	=	!Applied to KM_NH3 in Excel, should be applied to KMNH4 in Fortran code
      THTAKMNH4 = 1.0 !not effective, since it value is 1 anyway
  !See DMD book p.73 Table 3.1 (median value is 1.125)
      THTANH4 = 1.123 !See DMD book p.73 Table 3.1 (median value is 1.123)
      THTANO3 = 1.08
      THTACH4 = 1.079
!
  !Salinity thresholds
  !
  !SALTSW			SALTSW			salinity above which sulfide rather than 				psu				1					ppt
  !									methane is produced from C diagenesis
  !SALTND			SALTND			salinity above which saltwater 							psu				1					ppt
  !									nitrification/denitrification rates are
  !									used for aerobic layer
      SALTSW = 1.0 !ppt
      SALTND = 1.0 !ppt
!
  !Sulfide constants
  !
  !KappaH2Sd1		KAPP1HSD		aerobic layer reaction velocity for dissolved 			m/d				0.2					m/d
  !								sulfide oxidation
  !KappaH2Sp1		KAPP1HSP		aerobic layer reaction velocity for particulate 		m/d				0.4					m/d
  !								sulfide oxidation
  !ThtaH2S		THTAH2S			temperature coefficient for sulfide oxidation			unitless		1.079				unitless
  !KMHSO2			KMHSO2			sulfide oxidation normalization constant for O2			mgO2/L			4					mgO2/L
  !KdH2S1			PIE1HS			partition coefficient for sulfide in aerobic layer 1	L/kgD			100					L/kg	*
  !KdH2S2			PIE2HS			partition coefficient for sulfide in anaerobic layer 2	L/kgD			100					L/kg	*
  !
      KAPP1HSD = 0.2 !m/d
      KAPP1HSP = 0.4 !m/d
      THTAH2S = 1.079 !unitless
      KMHSO2 = 4.0 !mgO2/L
      PIE1HS = 100 !L/kg
      PIE2HS = 100 !L/kg
  !
!
  !Fractions of G classes 1 and 2 for settling RPON, RPOC, and RPOP
  !
  !frpon1			FRPON(:,1)		fraction of class 1 pon									unitless		0.65				unitless * only for detritus
  !frpon2			FRPON(:,2)		fraction of class 2 pon									unitless		0.25				unitless * only for detritus
!
  !frpoc1			FRPOC(:,1)		fraction of class 1 poc									unitless		0.65				unitless * only for detritus
  !frpoc2			FRPOC(:,2)		fraction of class 2 poc									unitless		0.2					unitless * only for detritus
!
  !frpop1			FRPOP(:,1)		fraction of class 1 pop									unitless		0.65				unitless * only for detritus
  !frpop2			FRPOP(:,2)		fraction of class 2 pop									unitless		0.2					unitless * only for detritus
!
  !*******WLong: Fortran code used this completely differently!!!!*****
  !Fortran code uses these as fraction of RPOM to be split in G2 and G3
  !G1 has no share of RPOM
!
      Do I = 1, MLOC
         FRPON (I, 1) = 0.0 !No share for G1
         FRPON (I, 2) = 0.65
         FRPON (I, 3) = 1 - FRPON (I, 2) - FRPON (I, 1)
!
         FRPOC (I, 1) = 0.0 !No share for G1
         FRPOC (I, 2) = 0.65
         FRPOC (I, 3) = 1 - FRPOC (I, 1) - FRPOC (I, 2)
!
         FRPOP (I, 1) = 0.0 !No share for G1
         FRPOP (I, 2) = 0.65
         FRPOP (I, 3) = 1 - FRPOP (I, 1) - FRPOP (I, 2)
     !
      End Do
!
  !Diagenesis rate constants for G clase 1, 2, and 3 N/C/P
  !
  !kpon1			KPON1			G class 1 pon mineralization							day^-1			0.035				1/d
  !kpon2			KPON2			G class 2 pon mineralization							day^-1			0.0018				1/d
  !kpon3			KPON3			G class 3 pon mineralization							day^-1			0					1/d
  !kpoc1			KPOC1			G class 1 poc mineralization							day^-1			0.035				1/d
  !kpoc2			KPOC2			G class 2 poc mineralization							day^-1			0.0018				1/d
  !kpoc3			KPOC3			G class 3 poc mineralization							day^-1			0					1/d
  !kpop1			KPOP1			G class 1 pop mineralization							day^-1			0.035				1/d
  !kpop2			KPOP2			G class 2 pop mineralization							day^-1			0.0018				1/d
  !kpop3			KPOP3			G class 3 pop mineralization							day^-1			0					1/d
  !
      KPON1 = 0.035 !1/d
      KPON2 = 0.0018 !1/d
      KPON3 = 0. !1/d
!
      KPOC1 = 0.035 !1/d
      KPOC2 = 0.0018 !1/d
      KPOC3 = 0. !1/d
!
      KPOP1 = 0.035 !1/d
      KPOP2 = 0.0018 !1/d
      KPOP3 = 0. !1/d
  !
  !Temperature coefficients for G class 1, 2, and 3 mineralization
  !
  !ThtaPON1		THTAPON1		temperature theta for G class 1 pon						unitless		1.1
  !ThtaPON2		THTAPON2		temperature theta for G class 2 pon						unitless		1.15
  !ThtaPON3		THTAPON3		temperature theta for G class 3 pon						unitless		1.17
  !ThtaPOC1		THTAPOC1		temperature theta for G class 1 poc						unitless		1.1
  !ThtaPOC2		THTAPOC2		temperature theta for G class 2 poc						unitless		1.15
  !ThtaPOC3		THTAPOC3		temperature theta for G class 3 poc						unitless		1.17
  !ThtaPOP1		THTAPOP1		temperature theta for G class 1 pop						unitless		1.1
  !ThtaPOP2		THTAPOP2		temperature theta for G class 2 pop						unitless		1.15
  !ThtaPOP3		THTAPOP3		temperature theta for G class 3 pop						unitless		1.17
  !
      THTAPON1 = 1.1
      THTAPON2 = 1.15
      THTAPON3 = 1.17
!
      THTAPOC1 = 1.1
      THTAPOC2 = 1.15
      THTAPOC3 = 1.17
!
      THTAPOP1 = 1.1
      THTAPOP2 = 1.15
      THTAPOP3 = 1.17
!
  !Parameters for partical mixing and benthic stress
  !
  !POC1R			POC1R			reference G1 at which w12base = Dp / H2 at 				mgO2/gD			0.2667				mgC/gD			*Not available
  !									20 degC for DiToro eqn 13.1
  !
  !kBEN_STR		KBENSTR 		first-order decay rate constant for benthic  			day^-1			0.03				1/d
  !									stress (d^-1) for DiToro eqn 13.3
  !
  !KM_O2_Dp		KMO2DP			particle mixing half-saturation constant 				mgO2/L			4					mgO2/L
  !									for O2 (mgO2/L)
!
  ! ??			KLBNTH			ratio of bio-irrigation to bioturbation					unitless
  ! ?? 			DPMIN			minimum particule mixing rate (m^2/day)
!
      POC1R = 0.1 !mgC/gSediment  DMD eqn (13.1) !0.2667/2.667
  !WLong notes eqn(3-26) (DMD book page 278, table 13.1, eqn(13.1))
      KBENSTR = 0.03 !1/d
      KMO2DP = 4.0 !mgO2/L  ~ gO2/m^3
!
      KLBNTH = 0.d0 !to make sure no bioturbation
      DPMIN = 0.d0 !to make minimum  particle diffusion rate zero
  !
  !Parameters for silica
  !
  !KSI			KSI				reaction rate for particulate biogenic Si 				day^-1			0.5					1/d
  !THTASI			THTASI			temperature theta for KSI								unitless		1.1					unitless
  !CSISAT			CSISATT20		saturation concentration for porewater Si				mgSi/L			40					mg Si/m3 	*
  !THTASISAT		THTASISAT		temperature theta for CSISAT							unitless		1.023							*Not available
  !DPIE1SI		DPIE1SI			incremental partition coefficient for Si in layer 1		Unitless		10					Unitless
  !PIE2SI			PIE2SI			partition coefficient for Si in layer 2					L/kgD			100					L/kg
  !KMPSI			KMPSI			particulate biogenic Si half saturation 				mgSi/L			50				mg Si/m3	*
  !									constant for dissolution
  !O2CRITSI		O2CRITSI		critical O2 concentration for layer 1 					mgO2/L			2					O2CRITSI
  !									incremental Si sorption
  !
      KSI = 0.5 !1/d
      THTASI = 1.1 !dimensionless		!DMD book p155 Tabel 7.1
      CSISATT20 = 40 * 1000.0 !mgSi/m^3
      THTASISAT = 1.023 !dimensionless
      DPIE1SI = 10 !unitless
      PIE2SI = 100 !L/kg
      KMPSI = 50000.0 * 1000.0 !mgSi/m^3			!DMD book p156 100mgSi/gsediment ==> p573 50000mgSi/L ==> 50,000,000 mgSi/m^3
      O2CRITSI = 2.0 !mgO2/L ~ gO2/m^3 !used for calculating PIE1SI
      JSIDETR = 0.d0 !not used for now
  !
  !Parameters for temperature
  !
  !DIFFT=1000.0 !m^2/sec      !used for calculating temperature in sediments
      DIFFT = 1000.0 / 86400000.0000000 / 5.0
  !
  !initial conditions for sediment concentratins
  !
!
  !----------------------------------------------------------------------------------------------------------------------------------------------------------------
  !parameter		parameter		parameter_meaning										unit			initial_value		unit			IC			equation
  !(excel)		(mod_sed.F)																(excel)				(excel)			(mod_sed)		(fortran)	(DMD book)
  !---------------------------------------------------------------------------------------------------------------------------------------------------------------
  !
  !Particulate organic C, N, and P in layer 2
!
  !POC2(1)		CPOCI(1)		G class 1 POC in layer 2								gO2/m^3			89.446				mgC/m^3	*		(89.446/2.667)*1000
  !POC2(2)		CPOCI(2)		G class 2 POC in layer 2								gO2/m^3			622.783				mgC/m^3	*		(622.783/2.667)*1000
  !POC2(3)		CPOCI(3)		G class 3 POC in layer 2								gO2/m^3			6569.343			mgC/m^3	*		(6569.343/2.667)*1000
  !PON2(1)		CPONI(1)		G class 1 PON in layer 2								gN/m^3			1.491				mgN/m^3			1.491*1000
  !PON2(2)		CPONI(2)		G class 2 PON in layer 2								gN/m^3			12.975				mgN/m^3			12.975*1000
  !PON2(3)		CPONI(3)		G class 3 PON in layer 2								gN/m^3			72.993				mgN/m^3			72.993*1000
  !POP2(1)		CPOPI(1)		G class 1 POP in layer 2								gP/m^3			0.894				mgP/m^3			0.894*1000
  !POP2(2)		CPOPI(2)		G class 2 POP in layer 2								gP/m^3			6.228				mgP/m^3			6.228*1000
  !POP2(3)		CPOPI(3)		G class 3 POP in layer 2								gP/m^3			65.693				mgP/m^3			65.693*1000
  !POS2			CPOSI			Particulate biogenic Si in layer 2						gSi/m^3			0.302				mgSi/m^3		0.302*1000
  !
  !Sediment temperature and benthic stress
  !*				CTEMPI																					??					degC			15.0
  !*				BENSTI																					0.0									0.0
!
  !Dissolved constituents in layer 1 and 2 porewater
!
  !NH3(1)			*NH4T1I			Dissolved ammonia N in layer 1 porewater				mgN/L			0.024				mgN/m^3			0.024*1000
  !NH3(2)			NH4T2I			Dissolved ammonia N in layer 2 porewater				mgN/L			0.152				mgN/m^3			0.152*1000
  !NO3(1)			*NO3T1I			Dissolved nitrate+nitrite N in layer 1 porewater		mgN/L			0.041				mgN/m^3			0.041*1000
  !NO3(2)			NO3T2I			Dissolved nitrate+nitrite N in layer 2 porewater		mgN/L			0.007				mgN/m^3			0.007*1000
  !PO4(1)			*PO4T1I			Dissolved phosphate P in layer 1 porewater				mgP/L			0.054				mgP/m^3			0.054*1000
  !PO4(2)			PO4T2I			Dissolved phosphate P in layer 2 porewater				mgP/L			0.290				mgP/m^3			0.290*1000
  !Si(1)			*SIT1I			Dissolved Si in layer 1 porewater						mgSi/L			0.209				mgSi/m^3        0.209*1000
  !Si(2)			SIT2I			Dissolved Si in layer 2 porewater						mgSi/L			1.160				mgSi/m^3		1.160*1000
!
  !HS(1)			*HST1I			Dissolved sulfide in layer 1 porewater 					mgO2/L			0.002               gO2/m^3			0.002
  !									(used if salinity >= 1 psu)
  !HS(2)			HST2I			Dissolved sulfide in layer 2 porewater					mgO2/L			3.348				gO2/m^3			3.348
  !								 	(used if salinity >= 1 psu)
  !CH4(1)			CH41TI			Dissolved methane in layer 1 porewater 					mgO2/L			0.000				gO2/m^3			0.000
  !								 	(used if salinity >= 1 psu)
  !CH4(2)			CH4T2I			Dissolved methane in layer 2 porewater 					mgO2/L			0.000				gO2/m^3			0.000
  !									(used if salinity >= 1 psu)
  !**				SO4T2I																	mgO2/L			??					mgO2/L			0.000
  !*				SO4T2I																	mgO2/L			??					mgO2/L			0.000
!
  !
      CPOCI (1) = 1000 * 89.446 / 2.667 !mgC/m^3
      CPOCI (2) = 1000 * 622.783 / 2.667 !mgC/m^3
      CPOCI (3) = 1000 * 6569.343 / 2.667 !mgC/m^3
      CPONI (1) = 1000 * 1.491 !mgN/m^3
      CPONI (2) = 1000 * 12.975 !mgN/m^3
      CPONI (3) = 1000 * 72.993 !mgN/m^3
      CPOPI (1) = 1000 * 0.894 !mgP/m^3
      CPOPI (2) = 1000 * 6.228 !mgP/m^3
      CPOPI (3) = 1000 * 65.693 !mgP/m^3
      BENSTI = 0.0 !unitless		!Fortran only ??
      CPOSI = 1000 * 0.302 !mgSi/m^3
!
  !
  !Note that in Excel version, concentrations are given as dissolved , then total concenration is calculated based on m1,m2 and PIENH4
  !
  !fd1 = (1# / (1# + m1 * KdNH3))
  !fd2 = (1# / (1# + m2 * KdNH3))
  !NH3Tp2(1) = NH3p2(1) / fd1
  !NH3Tp2(2) = NH3p2(2) / fd2
  !
  !m1=m2=0.5, PIENH4=1 ==> fd1=(1/(1+0.5*1))=1/1.5=0.3333
  !						fd2=(1/(1+0.5*1))=1/1.5=0.3333
  !
      PIE1 = PIENH4
      PIE2 = PIENH4
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      NH41TI = 1000 * 0.024 !mgN/m^3		!Excel only
      NH4T2I = 1000 * (0.152) / FD2 !mgN/m^3
!
      NO31TI = 1000 * 0.041 !mgN/m^3		!Excel only
      NO3T2I = 1000 * 0.007 !mgN/m^3
!
!
  !also make sure PO4T2I is total PO4
!
      I = 1
!
      If (SALT(I, KBM1) < SALTND) Then
         DPIE1PO4 = DPIE1PO4F
      Else
         DPIE1PO4 = DPIE1PO4S
      End If
      If (O20 > O2CRITPO4) Then
         PIE1 = PIE2PO4 * DPIE1PO4
      Else
         PIE1 = PIE2PO4 * DPIE1PO4 ** (O20/O2CRITPO4)
      End If
      PIE2 = PIE2PO4
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      PO41TI = 1000 * 0.054 !mgP/m^3		!Excel only
      PO4T2I = 1000 * 0.290 / FD2 !converted to total value
!
      If (O20 > O2CRITSI) Then
         PIE1 = PIE2SI * DPIE1SI
      Else
         PIE1 = PIE2SI * DPIE1SI ** (O20/O2CRITSI)
      End If
      PIE2 = PIE2SI
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      SI1TI = 1000 * 0.209 !mgSi/m^3		!Excel only
      SIT2I = 1000 * 1.160 / FD2 !mgSi/m^3		!converted to total Si in layer 2
!
!
      PIE1 = PIE1HS
      PIE2 = PIE2HS
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      HS1TI = 0.002 !mgO2/L			!Excel only
      HST2I = 3.348 / FD2 !mgO2/L			!converted to total HS
!
      CH4T2I = 0.0 !mgO2/L
      CH41TI = 0.0 !mgO2/L
!
      SO41TI = 0.0 !mgO2/L			!Neither Excel Nor Fortran
      SO4T2I = 0.0 !mgO2/L			!Fortran only
  !
      SODI = 0.1 !mgO2/m^2/d		!Initialize SOD !WLong: may need to call this SODI
  !Greg Pelletier's Excel used JSOD=0.1 mgO2/m^2/d
!
      Do I = 1, MLOC
!
         CTEMP (I) = CTEMPI !Sediment temperature
!
         Do JG = 1, 3
            CPOC (I, JG) = CPOCI (JG)
            CPON (I, JG) = CPONI (JG)
            CPOP (I, JG) = CPOPI (JG)
         End Do
!
         CPOS (I) = CPOSI
         BENSTRTM1S (I) = BENSTI !*not in excel
         SODTM1S (I) = SODI
         NH41TM1S (I) = NH41TI
         NH4T2TM1S (I) = NH4T2I
!
         NO31TM1S (I) = NO31TI
         NO3T2TM1S (I) = NO3T2I
!
         PO41TM1S (I) = PO41TI
         PO4T2TM1S (I) = PO4T2I
!
         SI1TM1S (I) = SI1TI
         SIT2TM1S (I) = SIT2I
!
         HS1TM1S (I) = HS1TI
         HST2TM1S (I) = HST2I
!
         CH41TM1S (I) = CH41TI
         CH4T2TM1S (I) = CH4T2I
!
         SO4T2TM1S (I) = SO4T2I !*not in excel
     !
      End Do
!
  !reset the intial values of varables that involve some of the above parameters
      Call SED_INIT2
  !
  !open output file
!
      BFOFN = 'sedtest_2bChannel.csv'
  !
      If (MSR) Then
!
         Open (Unit=BFO, File=BFOFN)
         Rewind (BFO)
!
         IDEBUG_TEST = 2
         If (MSR) WRITE (*,*) 'IDEBUG_TEST=', IDEBUG_TEST
!
     !
     !write the headline the same as excel file "SedFlux_ver17b12_test2b.xlsm"
     !
!
     !Segment ID	JDAY+Time	Jcin	Jnin	Jpin	Jsin	O20	Depth	Tw	NH30	NO30	PO40	SI0	CH40	SALw
     !				SOD	Jnh4	Jno3	JDenitT	Jch4	Jch4g	Jhs	Jpo4	Jsi	Jcout	Jnout	Jpout	NH3(1)	NH3(2)
     !				NO3(1)	NO3(2)	PO4(1)	PO4(2)	Si(1)	Si(2)	CH4(1)	CH4(2)	HS(1)	HS(2)	POC2(1)	POC2(2)	POC2(3)
     !				PON2(1)	PON2(2)	PON2(3)	POP2(1)	POP2(2)	POP2(3)	POS2 H1	BEN_STR
     !
         I = 1
         Write (BFO, 1001) 'SegmentID', ',', 'JDAY', ',', 'Jcin', ',', 'Jnin', ',', 'Jpin', ',', 'Jsin', ',', 'O20', ',', 'Depth', ',', 'Tw', ',', 'NH40', ',', 'NO30', ',', 'PO40', ',', 'SI0', ',', 'CH40', ',', 'SALw', ',', 'SOD', ',', 'Jnh4', ',', 'Jno3',&
        &  ',', 'JDenitT', ',', 'Jch4', ',', 'Jch4g', ',', 'Jhs', ',', 'Jpo4', ',', 'Jsi', ',', 'Jcout', ',', 'Jnout', ',', 'Jpout', ',', 'NH3(1)', ',', 'NH3(2)', ',', 'NO3(1)', ',', 'NO3(2)', ',', 'PO4(1)', ',', 'PO4(2)', ',', 'Si(1)', ',', 'Si(2)', ',', &
        & 'CH4(1)', ',', 'CH4(2)', ',', 'HS(1)', ',', 'HS(2)', ',', 'POC2(1)', ',', 'POC2(2)', ',', 'POC2(3)', ',', 'PON2(1)', ',', 'PON2(2)', ',', 'PON2(3)', ',', 'POP2(1)', ',', 'POP2(2)', ',', 'POP2(3)', ',', 'POS2', ',', 'H1', ',', 'BEN_STR'
!
      End If
1000  Format (I10, 1000(1 X, A10, F15.8))
1001  Format (A10, 1000(1 X, A10, A15))
  !
  !do test 2a calculations
!
      TMSTRT = 0.0 !reference time
      TMEND = 700.0 !end time
      JDAY = 0.0 !time in days
      DLT = 0.01 * 86400 !10.0		!time step in sec = 864sec=0.01 day=14.4min
      ELTMS = 0.0 !time in sec
      ELTMJD = 0.0 !time in days after reference time
      DLTS = DLT / 86400.
!
      END_RUN = .False.
!
      IINT = 0 !time step counter
!
      Do WHILE ( .Not. END_RUN)
     !
         If (MSR) WRITE (*,*) 'JDAY=', JDAY
     !
         Call POM_ACCUMUL
     !
     !average the accumulated POM flux over NDTSED steps
         Do I = 1, MLOC
            Do K = 1, 3
               JPOC (I, K) = JPOCaccum (I, K)
               JPON (I, K) = JPONaccum (I, K)
               JPOP (I, K) = JPOPaccum (I, K)
            End Do
            JPOS (I) = JPOSaccum (I)
         End Do
     !
         Call SED_CALC (.False.)!Calculate with time variable formulation
     !
     !set accumulative fluxes back to zero
         JPOCaccum = 0.0
         JPONaccum = 0.0
         JPOPaccum = 0.0
         JPOSaccum = 0.0
     !
     !write out outputs
     !
         If (MSR) Then
        !Fortran							Excel				FortranUnit			ExcelUnit
            I = 1
            Write (BFO, 1000) I, ',', JDAY, ',', (JPOC(I, 1)+JPOC(I, 2)+&
           & JPOC(I, 3)) / 1000.0 * 2.667, ',', (JPON(I, 1)+JPON(I, 2)+&
           & JPON(I, 3)) / 1000.0, ',', (JPOP(I, 1)+JPOP(I, 2)+JPOP(I, &
           & 3)) / 1000.0, ',', JPOS (I) / 1000.0, ',', O20, ',', D (I),&
           &  ',', T (I, KBM1), ',', NH40 / 1000.0, ',', NO30 / 1000.0, &
           & ',', PO40 / 1000.0, ',', SI0 / 1000.0, ',', CH40, ',', SAL,&
           &  ',', SOD, ',', JNH4 / 1000.0, ',', JNO3 / 1000.0, ',', &
           & BENDEN (1), ',', JCH4, ',', JCH4G, ',', JHS, ',', JPO4 / &
           & 1000.0, ',', JSI / 1000.0, ',', 0.0, ',', 0.0, ',', 0.0, '&
           &,', NH41 / 1000.0, ',', NH42 / 1000.0, ',', NO31 / 1000.0, &
           & ',', NO32 / 1000.0, ',', PO41 / 1000.0, ',', PO42 / 1000.0,&
           &  ',', SI1 / 1000.0, ',', SI2 / 1000.0, ',', CH41, ',', &
           & CH42, ',', HS1, ',', HS2, ',', POC1 / 1000.0 * 2.667, ',', &
           & POC2 / 1000.0 * 2.667, ',', POC3 / 1000.0 * 2.667, ',', &
           & PON1 / 1000.0, ',', PON2 / 1000.0, ',', PON3 / 1000.0, ',',&
           &  POP1 / 1000.0, ',', POP2 / 1000.0, ',', POP3 / 1000.0, ',&
           &', PSISED / 1000.0, ',', HSED1 (I) * 1000.0, ',', BENSTR !'BEN_STR' 			dimensionless		dimensionless
        ! WRITE(BFO,1000)I,',',JDAY,			&
        !	',',(JPOC(I,1)+JPOC(I,2)+JPOC(I,3))/1000.0*2.667,	&	!'Jcin'				mgC/m^2/d			gO2/m^2/d
        !	',',(JPON(I,1)+JPON(I,2)+JPON(I,3))/1000.0,	&			!'Jnin'				*mgN/m^2/d			gN/m^2/d
        !	',',(JPOP(I,1)+JPOP(I,2)+JPOP(I,3))/1000.0,	&			!'Jpin'				*mgP/m^2/d			gP/m^2/d
        !	',',JPOS(I)/1000.0,	&									!'Jsin		 		*mgSi/m^2/d			gSi/m^2/d
        !	',',O20,			&									!'O20'				mgO2/L				mgO2/L
        !	',',D(I),			&									!'Depth'			m					m
        !	',',T(I,KBM1),		&									!'Tw'				degC				degC
        !	',',NH40/1000.0,		&									!'NH30'				*mgN/m^3			mgN/L
        !	',',NO30/1000.0,		&									!'NO30'				*mgN/m^3			mgN/L
        !	',',PO40/1000.0,		&									!'PO40'				*mgP/m^3			mgP/L
        !	',',SI0/1000.0,		&									!'SI0'				*mgSi/m^3 			mgSi/L				(dissiolved SIAT)
        !	',',CH40,			&									!'CH40'				gO2/m^3				mgO2/L
        !	',',SAL,			&									!'SALw'  			ppt					ppt					!i.e. SALT(I,KWC), ppt
        !	',',SOD,			&									!'SOD'				gO2/m^2/day			gO2/m^2/d
        !	',',JNH4/1000.0,		&									!'Jnh4				*mgN/m^2/day		gN/m^2/d
        !	',',JNO3/1000.0,		&									!'Jno3				*mgN/m^2/day		gN/m^2/d
        !	',',BENDEN(1),		&									!'JDenitT'			gN/m^2/day			gN/m^2/d
        !							!JDenit(1) = Denit(1) * NO3(1)
        !							!JDenit(2) = Denit(2) * NO3(2)
        !							!JDenitT = JDenit(1) + JDenit(2)
        !	',',JCH4,			&									!'Jch4'				gO2/m^2/day			gO2/m^2/d
        !	',',JCH4G,			&									!'Jch4g'			gO2/m^2/day			gO2/m^2/d
        !	',',JHS,			&									!'Jhs'				gO2/m^2/day			gO2/m^2/d
        !	',',JPO4/1000.0,		&									!'Jpo4'				*mgP/m^2/day		gP/m^2/d
        !	',',JSI/1000.0,		&									!'Jsi'				*mgSi/m^2/day		gSi/m^2/d
        !	',',0.0,			&									!'Jcout'
        !	',',0.0,			&									!'Jnout'
        !	',',0.0,			&									!'Jpout'
        !         !
        ! WLong: Excel calculations here, should correspond to Jcin, Jnin and Jpin
        ! Jcout = SOD + JCH4 + JCH4g + JHS +  	(POC2(1) + POC2(2) + POC2(3)) * w2 +
        !						(		(POC2(1) + POC2(2) + POC2(3))
        !							- 	(POC22(1) + POC22(2) + POC22(3))
        !						) * H2 / tc
        ! Jnout = JNO3 + JNH4 + JDenitT + 		(PON2(1) + PON2(2) + PON2(3)) * w2 +
        !						(		(PON2(1) + PON2(2) + PON2(3))
        !							- 	(PON22(1) + PON22(2) + PON22(3))
        !						) * H2 / tc
        ! Jpout = JPO4 + 						(POP2(1) + POP2(2) + POP2(3)) * w2 +
        !						((POP2(1) + POP2(2) + POP2(3))
        !						 - (POP22(1) + POP22(2) + POP22(3))
        !						 ) * H2 / tc
        !
        !	',',NH41/1000.0,		&					!'NH3(1)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',NH42/1000.0,		&					!'NH3(2)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',NO31/1000.0,		&					!'NO3(1)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',NO32/1000.0,		&					!'NO3(2)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',PO41/1000.0,		&					!'PO4(1)'			*mgP/m^3 			mgP/L	(dissolved)
        !	',',PO42/1000.0,		&					!'PO4(2)'			*mgP/m^3 			mgP/L	(dissolved)
        !	',',SI1/1000.0,		&					!'Si(1)'			*mgSi/m^3 			mgSi/L	(dissolved)
        !	',',SI2/1000.0,		&					!'Si(2)'			*mgSi/m^3 			mgSi/L	(dissolved)
        !	',',CH41,			&					!'CH4(1)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',CH42,			&					!'CH4(2)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',HS1,			&					!'HS(1)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',HS2,			&					!'HS(2)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',POC1/1000.0*2.667,&					!'POC2(1)'			*mgC/m^3 			gO2/m^3	(G1 of POC in layer 2)
        !	',',POC2/1000.0*2.667,&					!'POC2(2)' 			*mgC/m^3 			gO2/m^3	(G2 of POC in layer 2)
        !	',',POC3/1000.0*2.667,&					!'POC2(3)' 			*mgC/m^3 			gO2/m^3	(G3 of POC in layer 2)
        !	',',PON1/1000.0,		&					!'PON2(1)'			*mgN/m^3 			gN/m^3	(G1 of PON in layer 2)
        !	',',PON2/1000.0,		&					!'PON2(2)' 			*mgN/m^3 			gN/m^3	(G2 of PON in layer 2)
        !	',',PON3/1000.0,		&					!'PON2(3)' 			*mgN/m^3 			gN/m^3	(G3 of PON in layer 2)
        !	',',POP1/1000.0,		&					!'POP2(1)'			*mgP/m^3 			gP/m^3	(G1 of POP in layer 2)
        !	',',POP2/1000.0,		&					!'POP2(2)'			*mgP/m^3 			gP/m^3	(G2 of POP in layer 2)
        !	',',POP3/1000.0,		&					!'POP2(3)' 			*mgP/m^3 			gP/m^3	(G3 of POP in layer 2)
        !	',',PSISED/1000.0,	&					!'POS2'				*mgSi/m^3 			gSi/m^3	(particulate biogenic Silicate in layer2)
        !	',',HSED1(I)*1000.0,&					!'H1'				m					mm
        !	',',BENSTR			 					!'BEN_STR' 			dimensionless		dimensionless
        !
            Write (*,*) 1, ',', JDAY
            Write (*,*) 'JPOC=,', (JPOC(I, 1)+JPOC(I, 2)+JPOC(I, 3)) / &
           & 1000.0 * 2.667, 'gO2/m^2/d'
        !'Jcin'				mgC/m^2/d			gO2/m^2/d
!
        !',WS1NET(1)=',WS1NET(I), &!
             !',WB1NETMMD=',WB1NETMMD, 					&	!
        !',B1=',B1(I,KWC),&!
        !',FRACALG1(1)=',FRCALG1(1),&!
        !'JPOC(I,1)=,',JPOC(I,1)/1000*2.667, &	!						*mgC/m^2/d			gO2/m^2/d
        !'JPON(I,1)=,',JPON(I,1)/1000.0, 	&	!						*mgN/m^2/d			gN/m^2/d
        !'JPOP(I,1)=,',JPOP(I,1)/1000.0, 	&	!						*mgP/m^2/d			gP/m^2/d
        !'JPOS(I)  =,',JPOS(I)/1000.0,		&	!						*mgSi/m^2/d			gSi/m^2/d
!
            Write (*,*) 'JPON=,', (JPON(I, 1)+JPON(I, 2)+JPON(I, 3)) / &
           & 1000.0, 'gN/m^2/d'
        !'Jnin'				*mgN/m^2/d			gN/m^2/d
            Write (*,*) 'JPOP=,', (JPOP(I, 1)+JPOP(I, 2)+JPOP(I, 3)) / &
           & 1000.0, 'gP/m^2/d'
        !'Jpin'				*mgP/m^2/d			gP/m^2/d
            Write (*,*) 'JPOS=,', JPOS (I) / 1000.0, 'gSi/m^2/d'
        !'Jsin		 		*mgSi/m^2/d			gSi/m^2/d
            Write (*,*)
            Write (*,*) 'O20=,', O20 !,			&					!'O20'				mgO2/L				mgO2/L
            Write (*,*) 'D=,', D (I)!,			&					!'Depth'			m					m
            Write (*,*) 'T=,', T (I, KBM1)!,		&					!'Tw'				degC				degC
            Write (*,*) 'NH40=,', NH40 / 1000 !,	&					!'NH30'				*mgN/m^3			mgN/L
            Write (*,*) 'NO30=,', NO30 / 1000 !,	&					!'NO30'				*mgN/m^3			mgN/L
            Write (*,*) 'PO40=,', PO40 / 1000 !,	&					!'PO40'				*mgP/m^3			mgP/L
            Write (*,*) 'SI0=,', SI0 / 1000 !,		&					!'SI0'				*mgSi/m^3 			mgSi/L				(dissiolved SIAT)
            Write (*,*) 'CH40=,', CH40 !,			&					!'CH40'				gO2/m^3				mgO2/L
            Write (*,*) 'SAL=,', SAL !,			&					!'SALw'  			ppt					ppt					!i.e. SALT(I,KWC), ppt
            Write (*,*) 'SOD=,', SOD !,			&					!'SOD'				gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JNH4=,', JNH4 / 1000 !,	&					!'Jnh4				*mgN/m^2/day		gN/m^2/d
            Write (*,*) 'JNO3=,', JNO3 / 1000 !,	&					!'Jno3				*mgN/m^2/day		gN/m^2/d
            Write (*,*) 'JDeniT=,', BENDEN (1)!,	&					!'JDenitT'			gN/m^2/day			gN/m^2/d
        !JDenit(1) = Denit(1) * NO3(1)
        !JDenit(2) = Denit(2) * NO3(2)
        !JDenitT = JDenit(1) + JDenit(2)
            Write (*,*) 'JCH4=,', JCH4 !,			&					!'Jch4'				gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JCH4G=,', JCH4G !,		&					!'Jch4g'			gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JHS=,', JHS !,			&					!'Jhs'				gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JPO4=,', JPO4 / 1000 !,	&					!'Jpo4'				*mgP/m^2/day		gP/m^2/d
            Write (*,*) 'JSI=,', JSI / 1000 !,		&					!'Jsi'				*mgSi/m^2/day		gSi/m^2/d
        !
        !						',',0.0,				&					!'Jcout'
        !						',',0.0,				&					!'Jnout'
        !						',',0.0,				&					!'Jpout'
        !
        !
        !WLong: Excel calculations here, should correspond to Jcin, Jnin and Jpin
        !Jcout = SOD + JCH4 + JCH4g + JHS +  	(POC2(1) + POC2(2) + POC2(3)) * w2 +
        !										(		(POC2(1) + POC2(2) + POC2(3))
        !											- 	(POC22(1) + POC22(2) + POC22(3))
        !										) * H2 / tc
        !Jnout = JNO3 + JNH4 + JDenitT + 		(PON2(1) + PON2(2) + PON2(3)) * w2 +
        !										(		(PON2(1) + PON2(2) + PON2(3))
        !											- 	(PON22(1) + PON22(2) + PON22(3))
        !										) * H2 / tc
        !Jpout = JPO4 + 						(POP2(1) + POP2(2) + POP2(3)) * w2 +
        !										((POP2(1) + POP2(2) + POP2(3))
        !										 - (POP22(1) + POP22(2) + POP22(3))
        !										 ) * H2 / tc
        !
            Write (*,*) 'NH41=,', NH41 / 1000 !,		&					!'NH3(1)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'NH42=,', NH42 / 1000 !,		&					!'NH3(2)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'NO31=,', NO31 / 1000 !,		&					!'NO3(1)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'NO32=,', NO32 / 1000 !,		&					!'NO3(2)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'PO41=,', PO41 / 1000 !,		&					!'PO4(1)'			*mgP/m^3 			mgP/L	(dissolved)
            Write (*,*) 'PO42=,', PO42 / 1000 !,		&					!'PO4(2)'			*mgP/m^3 			mgP/L	(dissolved)
            Write (*,*) 'SI1=,', SI1 / 1000 !,			&					!'Si(1)'			*mgSi/m^3 			mgSi/L	(dissolved)
            Write (*,*) 'SI2=,', SI2 / 1000 !,			&					!'Si(2)'			*mgSi/m^3 			mgSi/L	(dissolved)
            Write (*,*) 'CH41=,', CH41 !,				&					!'CH4(1)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'CH42=,', CH42 !,				&					!'CH4(2)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'SO41=,', SO41 !,				&					!'SO4(1)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'SO42=,', SO42 !,				&					!'SO4(2)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'HSO4=,', HSO4 !,				&					!'HSO4'				m	 				m
            Write (*,*) 'HS1=,', HS1 !,				&					!'HS(1)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'HS2=,', HS2 !,				&					!'HS(2)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'POC1=,', POC1 / 1000 * 2.667 !,	&					!'POC2(1)'			*mgC/m^3 			gO2/m^3	(G1 of POC in layer 2)
            Write (*,*) 'POC2=,', POC2 / 1000 * 2.667 !,	&					!'POC2(2)' 			*mgC/m^3 			gO2/m^3	(G2 of POC in layer 2)
            Write (*,*) 'POC3=,', POC3 / 1000 * 2.667 !,	&					!'POC2(3)' 			*mgC/m^3 			gO2/m^3	(G3 of POC in layer 2)
            Write (*,*) 'PON1=,', PON1 / 1000 !,		&					!'PON2(1)'			*mgN/m^3 			gN/m^3	(G1 of PON in layer 2)
            Write (*,*) 'PON2=,', PON2 / 1000 !,		&					!'PON2(2)' 			*mgN/m^3 			gN/m^3	(G2 of PON in layer 2)
            Write (*,*) 'PON3=,', PON3 / 1000 !,		&					!'PON2(3)' 			*mgN/m^3 			gN/m^3	(G3 of PON in layer 2)
            Write (*,*) 'POP1=,', POP1 / 1000 !,		&					!'POP2(1)'			*mgP/m^3 			gP/m^3	(G1 of POP in layer 2)
            Write (*,*) 'POP2=,', POP2 / 1000 !,		&					!'POP2(2)'			*mgP/m^3 			gP/m^3	(G2 of POP in layer 2)
            Write (*,*) 'POP3=,', POP3 / 1000 !,		&					!'POP2(3)' 			*mgP/m^3 			gP/m^3	(G3 of POP in layer 2)
            Write (*,*) 'PSISED=,', PSISED / 1000 !,	&					!'POS2'				*mgSi/m^3 			gSi/m^3	(particulate biogenic Silicate in layer2)
            Write (*,*) 'CTEMP=', CTEMP (I)
            Write (*,*) 'H1=,', HSED1 (I) * 1000 !,			&					!'H1'				m					mm
            Write (*,*) 'BENSTR=,', BENSTR !'BEN_STR' 			dimensionless		dimensionless
        !
        !		READ(*,*)
         End If
!
         IINT = IINT + 1
         ELTMS = ELTMS + DLT
         JDAY = ELTMS / 86400.
         ELTMJD = JDAY - TMSTRT
     !
         If (JDAY .Ge. TMEND) END_RUN = .True.
!
      End Do
!
  !
  !close files
  !
      Close (BFI)
      Close (BFO)
!
  !deallocate sediment module variables
!
      Call SED_DEALLOC
  !
  !
  !deallocate sav module variables
  !
      If (SAV_CALC) Then
         Call SAV_DEALLOC
         Call SED_SAV_EXCHANGE_DEALLOC
      End If
!
  !
  !deallocate df module variables
  !
      If (DFEEDER) Then
         Call DF_DEALLOC
         Call SED_DF_EXCHANGE_DEALLOC
      End If
  !
  !deallocate sf module variables
  !
  !
      If (SFEEDER) Call SF_DEALLOC
  !
  !deallocate ba module variables
  !
      If (BALGAE_CALC) Call BA_DEALLOC
      Stop 'Finished test2bChannel!'
  !
  !
#endif
  !
#if defined (test2c)
  !
  !test correspondign to SedFlux_ver17b12_test2c.xlsm
  !
  !c. Time-variable solution using assumed initial conditions for G classes of POM and assumed time-variable deposition of POM and time-variable overlying water quality
  !
!
  !
  !initialize the variables used in the sediment module in the upper
  !modules
  !
  !
  !disable SAV, BA, DF, SF
      SAV_CALC = .False.
      BALGAE_CALC = .False.
      DFEEDER = .False.
      SFEEDER = .False.
!
      BENTHIC_OUTPUT = .True.
!
  !serial run on master processor
      SERIAL = .True.
      PAR = .False.
      MSR = .True.
!
  !
  !set dimension limits
  !
!
      MLOC = 1 !one node
      MTLOC = 1 !one local node
      KBM1 = 10 !10 vertical water column layers
      MGL = 1 !one node
      KWC = KBM1 !overlying water column layer
!
  !
  !allocate hydrodynamic variables (no velocities needed, only depth and  box area)
  !
  !
  !		ART1	&!AREA OF NODE-BASE CONTROl VOLUME
  !		,DZ		&!DELTA-SIGMA VALUE of each layer
  !		,D		 !CURRENT DEPTH
!
  !WLong moved from cell_area.F to here
  !
      Allocate (ART1(0:MTLOC))
      ART1 = 0.0_SP !!AREA OF NODE-BASE CONTROl VOLUME
      Allocate (DZ(KBM1+1))
      DZ = 0.0_SP !!DELTA-SIGMA VALUE
      Allocate (D(0:MTLOC))
      D = 0.0
!
      ART1 = 100.d0 !100m^2
      D = 2.0 !2m deep
      DZ = 1.0 / KBM1 !0.1 as KBM1=10  !sigma layer
!
  !
  !allocate water column water quality variables
  !
!
      SETTLING = .True.
!
  !settling rates
      Allocate (WSSNET(MTLOC))
      WSSNET = 0.0 !suspended sediments settling rate (m/d)
      Allocate (WSLNET(MTLOC))
      WSLNET = 0.0 !LPOM (labile detritus) settling rate (m/d)
      Allocate (WSRNET(MTLOC))
      WSRNET = 0.0 !RPOM (refractory detriuts) settling rate (m/d)
      Allocate (WS1NET(MTLOC))
      WS1NET = 0.0 !alg 1 settling rate (m/d)
      Allocate (WS2NET(MTLOC))
      WS2NET = 0.0 !alg 2 settling rate (m/d)
      Allocate (WS3NET(MTLOC))
      WS3NET = 0.0 !alg 3 settling rate (m/d)
      Allocate (WSUNET(MTLOC))
      WSUNET = 0.0 !particulate biogenic silica settling rate (m/d)
  !
  !water column constituents
      Allocate (T(0:MTLOC, KBM1))
      T = 0.0 !1
      Allocate (SALT(0:MTLOC, KBM1))
      SALT = 0.0 !2
      Allocate (SSI(0:MTLOC, KBM1))
      SSI = 0.0 !3
      Allocate (B1(0:MTLOC, KBM1))
      B1 = 0.0 !4
      Allocate (B2(0:MTLOC, KBM1))
      B2 = 0.0 !5
      Allocate (B3(0:MTLOC, KBM1))
      B3 = 0.0 !6
      Allocate (LPOC(0:MTLOC, KBM1))
      LPOC = 0.0 !11
      Allocate (RPOC(0:MTLOC, KBM1))
      RPOC = 0.0 !12
      Allocate (NH4(0:MTLOC, KBM1))
      NH4 = 0.0 !13
      Allocate (NO3(0:MTLOC, KBM1))
      NO3 = 0.0 !14
      Allocate (LPON(0:MTLOC, KBM1))
      LPON = 0.0 !18
      Allocate (RPON(0:MTLOC, KBM1))
      RPON = 0.0 !19
      Allocate (PO4(0:MTLOC, KBM1))
      PO4 = 0.0 !20
      Allocate (LPOP(0:MTLOC, KBM1))
      LPOP = 0.0 !23
      Allocate (RPOP(0:MTLOC, KBM1))
      RPOP = 0.0 !24
      Allocate (PIP(0:MTLOC, KBM1))
      PIP = 0.0 !25
      Allocate (COD(0:MTLOC, KBM1))
      COD = 0.0 !26
      Allocate (DOXG(0:MTLOC, KBM1))
      DOXG = 0.0 !27
      Allocate (SIUPB(0:MTLOC, KBM1))
      SIUPB = 0.0 !28
      Allocate (SIAT(0:MTLOC, KBM1))
      SIAT = 0.0 !29
  !
  !sediment temperature
      Allocate (CTEMP(MTLOC))
      CTEMP = 0.0
!
  !P to C ratio (gP/gC) of alg 1 2 and 3
      Allocate (Q1(MTLOC, KBM1))
      Q1 = 0.0
      Allocate (Q2(MTLOC, KBM1))
      Q2 = 0.0
      Allocate (Q3(MTLOC, KBM1))
      Q3 = 0.0
!
  !particle (C, N, P, Si, Suspedned Solids) fluxes
!
      Allocate (PCFWS(MTLOC))
      PCFWS = 0.0
      Allocate (PNFWS(MTLOC))
      PNFWS = 0.0
      Allocate (PPFWS(MTLOC))
      PPFWS = 0.0
      Allocate (PSFWS(MTLOC))
      PSFWS = 0.0
      Allocate (SSFWS(MTLOC))
      SSFWS = 0.0 !this is done in mod_wqm.F
  !
  !benthic fluxes to water column
!
      Allocate (BENCH4A(MTLOC))
      BENCH4A = 0.0
      Allocate (BENCH4G(MTLOC))
      BENCH4G = 0.0
      Allocate (BENCOD(MTLOC))
      BENCOD = 0.0
      Allocate (BENDEN(MTLOC))
      BENDEN = 0.0
      Allocate (BENDO(MTLOC))
      BENDO = 0.0
      Allocate (BENDOC(MTLOC))
      BENDOC = 0.0
      Allocate (BENNH4(MTLOC))
      BENNH4 = 0.0
      Allocate (BENNO3(MTLOC))
      BENNO3 = 0.0
      Allocate (BENPO4(MTLOC))
      BENPO4 = 0.0
      Allocate (BENSA(MTLOC))
      BENSA = 0.0
  !
  !
  !
  !allocate sediment module variables
  !
!
      Call SED_INIT !initialize sediment consituent names and allocate variables for sediment module
!
  !
  !allocate sav module variables
  !
      If (SAV_CALC) Then
         Call SAV_ALLOC
         Call SED_SAV_EXCHANGE_ALLOC
      End If
!
  !
  !allocate df module variables
  !
      If (DFEEDER) Then
         Call DF_ALLOC
         Call SED_DF_EXCHANGE_ALLOC
      End If
  !
  !
  !allocate sf module variables
  !
  !
      If (SFEEDER) Call SF_ALLOC
  !
  !allocate ba module variables
  !
      If (BALGAE_CALC) Call BA_ALLOC
  !
!
  !
  !input and output files for the sediment module
  !
!
!
  !file handles
      BFI = 1
      BFO = 12
      WCL = 13
!
  !file names
      BFIFN (1) = 'sedtest_2c.inp'
      BFOFN = 'sedtest_2c.chk'
  !
  !
  !set sediment control parameters or read the sediment module control
  !parameters from file BFI
  !
!
      Open (BFI, File=BFIFN(1), Status='OLD')
  !
      Call SED_READ !read some of the basic variables but it will be over written again afterwards
!
  !particle (C, N, P, Si, Suspedned Solids) fluxes
!
      PCFWS = 0.0
      PNFWS = 0.0
      PPFWS = 0.0
      PSFWS = 0.0
      SSFWS = 0.0
  !
  !
  !driving POC, PON, POP, POS, SSI etc fluxes from water colum that should match Jcin, Jnin, Jpin, Jsin,
  !as well as overlying water concentrations
  !
!
  !
  !	TCE_ID	Date+time			Jcin		Jnin		Jpin		Jsin		O20			Depth	Tw		NH30	NO30	PO40	SI0			CH40		SALw
  !      							(gO2/m^2/d)	(gN/m^2/d)	(gP/m^2/d)	(gSi/m^2/d)	(mgO2/L)	(m)		(deg C)	(mgN/L)	(mgN/L)	(mgP/L)	(mgSi/L)	(mgO2/L)	(ppt)
  !		1	1/1/00 12:00 AM		0.093450	0.051800	0.000750	0.097125	12.0		4.30	0.0		0.100	0.300	0.040	0.750		1.3			20.0
  !
  !
  !
  !
  !Set water coumn variables for sediment diagenesis run
  !
  !	Fortran													Excel
  !------------------------------------------------------------------------------------
      T = 0.0 !1  	degC						15				degC
      CTEMPI = 0.0
      SALT = 20.0 !2		ppt							30				ppt
      SSI = 0.0 !3		gSolids/m^3					-
      B1 = 1.0 !4		gC/m^3						-
      B2 = 0.0 !5		gC/m^3						-
      B3 = 0.0 !6		gC/m^3						-
      LPOC = 0.0 !11		gC/m^3						-
      RPOC = 0.0 !12		gC/m^3						-
      NH4 = 0.1 !13		gN/m^3						0.015			mgN/L
      NO3 = 0.3 !14		gN/m^3						0.1				mgN/L
      LPON = 0.0 !18		gN/m^3						-
      RPON = 0.0 !19		gN/m^3						-
      PO4 = 0.04 !20		gP/m^3						0.004			mgP/L
      LPOP = 0.0 !23		gP/m^3						-
      RPOP = 0.0 !24		gP/m^3						-
      PIP = 0.0 !25		gP/m^3						-
      COD = 0.0 !26		gO2/m^3						-
      DOXG = 12.0 !27		gO2/m^3						5				mgO2/L
      SIUPB = 0.0 !28		gSi/m^3						0.0				mgSi/L (i.e. gSi/m^3)
      SIAT = 0.750 !29		gSi/m^3						0.216			mgSi/L (i.e. gSi/m^3)
!
  !
  !parameters related to water colum
  !
  !
  !read from MRL (adsorption coefficients of PO4 and Silicate
      KADPO4 = 0.0004 !m^3/gSolids		!READ (MRL,1080)  KADPO4, KADSA
!
  !basically PIE1, see WLong notes (4-7-5) =PIE2PO4*DPIE2PO4=100*300 L/kg
  !=30000L/kg = 30m^3/kg=30/1000 (m^3/g) = 0.3 m^3/g
!
  !=20 L/kg *20 = 400L/kg = 0.4m^3/kg = 0.4/1000 (m^3/g) 0.0004(m^3/kg)
!
      KADSA = 0.001 !m^3/g	!basically PIE2, see WLong notes (4-6-7-1) where
  !m2 is SSI (gSolids/m^3), pie2= should have unit m^3/gSolids
  !i.e. 100L/kg ~ 0.1m^3/kg ~ 0.1m^3/(1000g) = 0.0001 m^3/gSi
  !here it is used to calculate equilibrium speciation of dissolved Si
  !and particulate Si based on suspended solids concentration (gSolids/m^3)
  !in water column
  !
  !Relation between m^3/g and 100L/kg :	0.0001m^3/g = 0.1m^3/kg = 100L/(1kg) = 100L/kg
  !
  !
  !for water column, there is plenty of oxygen, according to WLong notes (4-6-5), we should use PIE1,Si
  !which is 100L/kg * DPIE1SI = 1000L/kg = 1m^3/1000g = 0.001m^3/g
  !
  !
  !	POM flux				Fortran	Formula							Excel						Excel Formula
  !-------------------------------------------------------------------------------------------------------------------
  !	Jcin 			1000*0.3/2.667=112.48		mgC/m^2/d			0.093450   gO2/m^2/d			JPOC(i) = Jcin*fpoc(i)
!
  !JPOC(I,1) = WB1NETMMD*FRCALG1(1)*B1(I,KWC)+										frpoc1=0.65
  !			WB2NETMMD*FRCALG2(1)*B2(I,KWC)+
  !			WB3NETMMD*FRCALG3(1)*B3(I,KWC)+
  !			WLPOMNETMMD*LPOC(I,KWC)
!
  !JPOC(I,2) = WB1NETMMD*FRCALG1(2)*B1(I,KWC)+        								frpoc2=0.2
  !			WB2NETMMD*FRCALG2(2)*B2(I,KWC)+
  !			WB3NETMMD*FRCALG3(2)*B3(I,KWC)+
  !			WRPOMNETMMD*RPOC(I,KWC)*FRPOC(I,2)/
  !			(FRPOC(I,2)+FRPOC(I,3))
!
  !JPOC(I,3) = WB1NETMMD*FRCALG1(3)*B1(I,KWC)+ 										frpoc3=0.15
  !			WB2NETMMD*FRCALG2(3)*B2(I,KWC)+
  !			WB3NETMMD*FRCALG3(3)*B3(I,KWC)+
  !			WRPOMNETMMD*RPOC(I,KWC)*FRPOC(I,3)/
  !				(FRPOC(I,2)+FRPOC(I,3))
!
  !	Jnin 			ANC*1000*0.005/2.667/FRNALG1(1)	mgN/m^2/d		0.051800    gN/m^2/d			JPON(i) = Jnin * fpon(i)
  !JPON(I,1) = WB1NETMMD*ANC1*FRNALG1(1)*B1(I,KWC)+									frpon1=0.65
  !			WB2NETMMD*ANC2*FRNALG2(1)*B2(I,KWC)+
  !			WB3NETMMD*ANC3*FRNALG3(1)*B3(I,KWC)+
  !			WLPOMNETMMD*LPON(I,KWC)
  !
  !JPON(I,2) = WB1NETMMD*ANC1*FRNALG1(2)*B1(I,KWC)									frpon2=0.25
  !			+WB2NETMMD*ANC2*FRNALG2(2)*B2(I,KWC)
  !			+WB3NETMMD*ANC3*FRNALG3(2)*B3(I,KWC)
  !			+WRPOMNETMMD*RPON(I,KWC)*FRPON(I,2)/
  !			(FRPON(I,2)+FRPON(I,3))
  !
  !JPON(I,3) = WB1NETMMD*ANC1*FRNALG1(3)*B1(I,KWC) 									frpon3=0.1
  !			+WB2NETMMD*ANC2*FRNALG2(3)*B2(I,KWC)
  !			+WB3NETMMD*ANC3*FRNALG3(3)*B3(I,KWC)
  !			+WRPOMNETMMD*RPON(I,KWC)*FRPON(I,3)/
  !			(FRPON(I,2)+FRPON(I,3))
!
  !	Jpin 			Q*1000*0.003/2.667	mgP/m^2/d					0.000750    gP/m^2/d			JPOP(i) = Jpin * fpop(i)
  !JPOP(I,1) = WB1NETMMD*Q1(I,KWC)*FRPALG1(1)*B1(I,KWC)+								frpop1=0.65
  !			WB2NETMMD*Q2(I,KWC)*FRPALG2(1)*B2(I,KWC)+
  !			WB3NETMMD*Q3(I,KWC)*FRPALG3(1)*B3(I,KWC)
  !			WLPOMNETMMD*LPOP(I,KWC)
!
  !JPOP(I,2) = WB1NETMMD*Q1(I,KWC)*FRPALG1(2)*B1(I,KWC)+								frpop2=0.2
  !			WB2NETMMD*Q2(I,KWC)*FRPALG2(2)*B2(I,KWC)+
  !			WB3NETMMD*Q3(I,KWC)*FRPALG3(2)*B3(I,KWC)+
  !			WRPOMNETMMD*RPOP(I,KWC)*FRPOP(I,2)/
  !			(FRPOP(I,2)+FRPOP(I,3))
!
  !JPOP(I,3) = WB1NETMMD*Q1(I,KWC)*FRPALG1(3)*B1(I,KWC)+								frpop3=0.15
  !			+WB2NETMMD*Q2(I,KWC)*FRPALG2(3)*B2(I,KWC)+
  !			+WB3NETMMD*Q3(I,KWC)*FRPALG3(3)*B3(I,KWC)+
  !			+WRPOMNETMMD*RPOP(I,KWC)*FRPOP(I,3)/
  !			(FRPOP(I,2)+FRPOP(I,3))
!
  !	Jsin 			ASC*1000*0.009375/2.667	mgSi/m^2/d					0.097125 gSi/m^2/d			POS2 = (Jsin * tc / H2 + POS2) /
  !																										(1 + KSI * THTASI ^ (Tw - 20) * tc + w2 * tc / H2)
  !JPOS(I) = 	 WB1NETMMD*ASC1*B1(I,KWC)
  !		  	+WB2NETMMD*ASC2*B2(I,KWC)
  !			+WB3NETMMD*ASC3*B3(I,KWC)
  !			+WPSINETMMD*SIUPB(I,KWC)
!
  !
  !POC distribution into G1,G2,G3 by ALG 1,2,3
  !G1 	Alg1				Alg2				Alg3
      FRCALG1 (1) = 0.65
      FRCALG2 (1) = 0.65
      FRCALG3 (1) = 0.65
!
  !G2
      FRCALG1 (2) = 0.20
      FRCALG2 (2) = 0.20
      FRCALG3 (2) = 0.20
!
  !G3
      FRCALG1 (3) = 0.15
      FRCALG2 (3) = 0.15
      FRCALG3 (3) = 0.15
!
!
  !PON distribution into G1,G2,G3 by ALG 1,2,3
  !G1 	Alg1				Alg2				Alg3
      FRNALG1 (1) = 0.65
      FRNALG2 (1) = 0.65
      FRNALG3 (1) = 0.65
!
  !G2
      FRNALG1 (2) = 0.25
      FRNALG2 (2) = 0.25
      FRNALG3 (2) = 0.25
!
  !G3
      FRNALG1 (3) = 0.10
      FRNALG2 (3) = 0.10
      FRNALG3 (3) = 0.10
!
  !POP distribution into G1,G2,G3 by ALG 1,2,3
  !G1 	Alg1				Alg2				Alg3
      FRPALG1 (1) = 0.65
      FRPALG2 (1) = 0.65
      FRPALG3 (1) = 0.65
!
  !G2
      FRPALG1 (2) = 0.2
      FRPALG2 (2) = 0.2
      FRPALG3 (2) = 0.2
!
  !G3
      FRPALG1 (3) = 0.15
      FRPALG2 (3) = 0.15
      FRPALG3 (3) = 0.15
!
  !The following values are given in order to match Jcin, Jnin, Jpin,Jsin in Excel version
      B1 = 1.0
  !only Alg1 settling contributes to POM in sediments
!
      B2 = 0.0
      B3 = 0.0
  !all other particulate organics set to zero
      LPOC = 0.0
      RPOC = 0.0
      LPON = 0.0
      RPON = 0.0
      LPOP = 0.0
      RPOP = 0.0
      SIUPB = 0.0
!
  !Only settling rates of B1 matters for JPOC
      WB1NETMMD = (0.093450/2.667) * 1000.0 / B1 (1, KWC)!==>35.0394mm/d 	!here 0.093450 is Jcin (gO2/m^2/d) in Excel version
!
      WB2NETMMD = 1000.0 !1000mm/d  	~1m/d
      WB3NETMMD = 1000.0 !1000mm/d	~1m/d
      WLPOMNETMMD = 1000.0 !1000mm/d	~1m/d
      WRPOMNETMMD = 1000.0 !1000mm/d  ~1m/d
      WPSINETMMD = 1000.0 !1000mm/d	~1m/d
!
  !reset WSLNET etc
      WSLBNET (1) = WLPOMNETMMD / 1000.
      WSRBNET (1) = WRPOMNETMMD / 1000.
      WS1BNET (1) = WB1NETMMD / 1000.0
      WS2BNET (1) = WB2NETMMD / 1000.0
      WS3BNET (1) = WB3NETMMD / 1000.0
      WSUBNET (1) = WPSINETMMD / 1000.0
      WSSBNET (1) = 1.0 !1m/d
!
  !ANC1, Q1, and ASC1 are adjusted to match Jnin, Jpin, Jsin based on Jcin
  !
  !redfield ratios of C,N,P for algae and detritus (modified to match Jcin, Jnin, Jpin, Jsin of Excel vresion)
  !
  !JPON=ANC1*WB1NETMMD*B1*FRNALG1(1)*2.667/1000=0.0518
!
  !
      ANC1 = (0.051800) * 1000.0 / B1 (1, KWC) / WB1NETMMD !==>1.4783 !here 0.051800 is Jnin (gN/m^2/d) in Excel version
  !ANC1    =	0.175 	!gN/gC
      ANC2 = 0.175
      ANC3 = 0.175
!
  !P to C ratio (gP/gC) of alg 1 2 and 3 (from AGR file)
      Q1 = (0.000750) * 1000.0 / B1 (1, KWC) / WB1NETMMD !==>0.0214	!here 0.000750 is Jpin (gP/m^2/d) in Excel version
  !Q1    	= 1/41.1		!see DMD book page 10 Table 1.1
      Q2 = 1 / 41.1 !see DMD book page 10 Table 1.1
      Q3 = 1 / 41.1 !see DMD book page 10 Table 1.1
  !
      ASC1 = (0.097125) * 1000.0 / B1 (1, KWC) / WB1NETMMD !==>2.7719 	!here 0.097125 is Jsin in gSi/m^2/d in Excel version
  !ASC1	=	0.333 	!gSi/gC
      ASC2 = 0.333
      ASC3 = 0.333
  !
  !
  !set parameters related to SedFlux_ver17b12_test2c.xlsm
  !
!
  !the parameter lists in SedFlux_ver17b12_test2c.xlsm are listed as follows
  !-------------------------------------------------------------------------------------------------------------------------------------------------
  !parameter		parameter		parameter_meaning										unit			default_value		unit		equation
  !(excel)		(mod_sed.F)																(excel)				(excel)			(mod_sed)	(DMD book)
  !-------------------------------------------------------------------------------------------------------------------------------------------------
  !Particle mixing:
  !
  !
  !m1				M1				solids concentration in aerobic layer 1					kgD/L			0.5					kg/L
  !m2				M2				solids concentration in anaerobic layer 2				kgD/L			0.5					kg/L
  !Dp				DPP	(VPMIX)		bioturbation particle mixing coefficient				m^2/d			0.00006				m^2/d
  !Dd				DDP	(VDMIX)		pore water diffusion coefficient						m^2/d			0.0025				m^2/d
  !w2				W2 (VSED)		deep burial velocity									m/d				6.85E-06			cm/yr *
  !H2				H2(HSED,HSEDALL)thickness of sediment anaerobic layer 2					m				0.1					cm *		*input is cm
  !
      M1 = 0.5
      M2 = 0.5
      DPP = 0.00006
      DDP = 0.0025
      HSEDALL = 10.0 !cm
      Do I = 1, MLOC
         HSED (I) = HSEDALL * 0.01 !m
         VSED (I) = 6.85E-06 !m/d  (W2)
         VPMIX (I) = 0.00006 !m^2/d
         VDMIX (I) = 0.0025 !m^2/d
      End Do
  !
  !Reaction velocities (depth integrated reaction rate = reaction velocity (m/d))
  !
  !KappaNH3f		KAPPNH4F		freshwater nitrification velocity						m/d				0.131				m/d
  !KappaNH3s		KAPPNH4S		saltwater nitrification velocity						m/d				0.131				m/d
  !KappaNO3_1f	KAPPNO3F		freshwater denitrification velocity in layer 1			m/d				0.1					m/d
  !KappaNO3_1s	KAPPNO3S		saltwater denitrification velocity in layer 1			m/d				0.1					m/d
  !KappaNO3_2		K2NO3			denitrfication in the anaerobic layer 2					m/d				0.25				m/d
  !KappaCH4		KAPPCH4			methane oxidation in the aerobic layer 1				m/d				0.7					m/d
  !
      KAPPNH4F = 0.131 !m/d
      KAPPNH4S = 0.131 !m/d
      KAPPNO3F = 0.1 !m/d
      KAPPNO3S = 0.1 !m/d
      K2NO3 = 0.25 !m/d		!DMD P.105 Table 4.1
      KAPPCH4 = 0.7 !m/d
!
  !Half saturation constants
  !
  !KM_NH3			KMNH4			nitrification half saturation for NH4N					mgN/L			0.728				mgN/m^3	*
  !KM_O2_NH3		KMNH4O2			nitrification half saturation for O2					mgO2/L			0.37				mgO2/L
  !
      KMNH4 = 0.728 * 1000. !(mgN/m^3)		!see DMD book p73 Table 3.1 (b) (and Median)
  !maybe DMD book is wrong on units
      KMNH4O2 = 0.37 !mgO2/L			!See DMD book p73 Tabl3 3.1 (Median)
!
  !Partitioning coefficients
  !
  !KdNH3			PIENH4			partition coefficient for NH4 in layer 1 and 2			L/kgD			1					L/kg
  !KdPO42			PIE2PO4			partition coefficient for PO4 in layer 2				L/kgD			20					L/kg
  !dKDPO41f		DPIE1PO4F		freshwater factor that increases the aerobic 			unitless		20					L/kg
  !									layer partition coefficient of inorganic P
  !dKDPO41s		DPIE1PO4S		saltwater factor that increases the aerobic 			unitless		20					L/kg
  !									layer partition coefficient of inorganic P
  !O2critPO4		O2CRITPO4		critical O2 concentration in layer 1 for 				mgO2/L			2					mgO2/L
  !									adjustment of partition coefficient
  !									for inorganic P
  !
      PIENH4 = 1.0 !L/kg		(inversed of m1,m2 unit)
      PIE2PO4 = 20.0 !L/kg
      DPIE1PO4F = 20.0 !WLong?? Unitless?	Yes! Note DMD book pp 572 says it should be about 300
      DPIE1PO4S = 20.0 !WLong?? Unitless?
      O2CRITPO4 = 2.0 !mgO2/L
!
  !Temperature coefficients
  !
  !ThtaDp			THTADP			temperature theta for bioturbation mixing between 		unitless		1.117				unitless
  !									layers 1 and 2
  !ThtaDd			THTADD			temperature theta for pore water diffusion between 		unitless		1.08				unitless
  !									layers 1 and 2
  !ThtaKmNH3		THTAKMNH4		temperature theta for nitrification half saturation 	unitless		1.125				unitless
  !									for NH4N
  !ThtaNH3		THTANH4			temperature theta for nitrification rate				unitless		1.123				unitless
  !ThtaNO3		THTANO3			temperature theta for denitrification rate				unitless		1.08				unitless
  !ThtaCH4		THTACH4			temperature theta for methane oxidation rate			unitless		1.079				unitless
  !
      THTADP = 1.117
      THTADD = 1.08
  !!ThtaKmNH3	=	!Applied to KM_NH3 in Excel, should be applied to KMNH4 in Fortran code
      THTAKMNH4 = 1.125
  !See DMD book p.73 Table 3.1 (median value is 1.125)
      THTANH4 = 1.123 !See DMD book p.73 Table 3.1 (median value is 1.123)
      THTANO3 = 1.08
      THTACH4 = 1.079
!
  !Salinity thresholds
  !
  !SALTSW			SALTSW			salinity above which sulfide rather than 				psu				1					ppt
  !									methane is produced from C diagenesis
  !SALTND			SALTND			salinity above which saltwater 							psu				1					ppt
  !									nitrification/denitrification rates are
  !									used for aerobic layer
      SALTSW = 1.0 !ppt
      SALTND = 1.0 !ppt
!
  !Sulfide constants
  !
  !KappaH2Sd1		KAPP1HSD		aerobic layer reaction velocity for dissolved 			m/d				0.2					m/d
  !								sulfide oxidation
  !KappaH2Sp1		KAPP1HSP		aerobic layer reaction velocity for particulate 		m/d				0.4					m/d
  !								sulfide oxidation
  !ThtaH2S		THTAH2S			temperature coefficient for sulfide oxidation			unitless		1.079				unitless
  !KMHSO2			KMHSO2			sulfide oxidation normalization constant for O2			mgO2/L			4					mgO2/L
  !KdH2S1			PIE1HS			partition coefficient for sulfide in aerobic layer 1	L/kgD			100					L/kg	*
  !KdH2S2			PIE2HS			partition coefficient for sulfide in anaerobic layer 2	L/kgD			100					L/kg	*
  !
      KAPP1HSD = 0.2 !m/d
      KAPP1HSP = 0.4 !m/d
      THTAH2S = 1.079 !unitless
      KMHSO2 = 4.0 !mgO2/L
      PIE1HS = 100 !L/kg
      PIE2HS = 100 !L/kg
  !
!
  !Fractions of G classes 1 and 2 for settling RPON, RPOC, and RPOP
  !
  !frpon1			FRPON(:,1)		fraction of class 1 pon									unitless		0.65				unitless * only for detritus
  !frpon2			FRPON(:,2)		fraction of class 2 pon									unitless		0.25				unitless * only for detritus
!
  !frpoc1			FRPOC(:,1)		fraction of class 1 poc									unitless		0.65				unitless * only for detritus
  !frpoc2			FRPOC(:,2)		fraction of class 2 poc									unitless		0.2					unitless * only for detritus
!
  !frpop1			FRPOP(:,1)		fraction of class 1 pop									unitless		0.65				unitless * only for detritus
  !frpop2			FRPOP(:,2)		fraction of class 2 pop									unitless		0.2					unitless * only for detritus
!
  !*******WLong: Fortran code used this completely differently!!!!*****
  !Fortran code uses these as fraction of RPOM to be split in G2 and G3
  !G1 has no share of RPOM
!
      Do I = 1, MLOC
         FRPON (I, 1) = 0.0 !No share for G1
         FRPON (I, 2) = 0.65
         FRPON (I, 3) = 1 - FRPON (I, 2) - FRPON (I, 1)
!
         FRPOC (I, 1) = 0.0 !No share for G1
         FRPOC (I, 2) = 0.65
         FRPOC (I, 3) = 1 - FRPOC (I, 1) - FRPOC (I, 2)
!
         FRPOP (I, 1) = 0.0 !No share for G1
         FRPOP (I, 2) = 0.65
         FRPOP (I, 3) = 1 - FRPOP (I, 1) - FRPOP (I, 2)
     !
      End Do
!
  !Diagenesis rate constants for G clase 1, 2, and 3 N/C/P
  !
  !kpon1			KPON1			G class 1 pon mineralization							day^-1			0.035				1/d
  !kpon2			KPON2			G class 2 pon mineralization							day^-1			0.0018				1/d
  !kpon3			KPON3			G class 3 pon mineralization							day^-1			0					1/d
  !kpoc1			KPOC1			G class 1 poc mineralization							day^-1			0.035				1/d
  !kpoc2			KPOC2			G class 2 poc mineralization							day^-1			0.0018				1/d
  !kpoc3			KPOC3			G class 3 poc mineralization							day^-1			0					1/d
  !kpop1			KPOP1			G class 1 pop mineralization							day^-1			0.035				1/d
  !kpop2			KPOP2			G class 2 pop mineralization							day^-1			0.0018				1/d
  !kpop3			KPOP3			G class 3 pop mineralization							day^-1			0					1/d
  !
      KPON1 = 0.035 !1/d
      KPON2 = 0.0018 !1/d
      KPON3 = 0. !1/d
!
      KPOC1 = 0.035 !1/d
      KPOC2 = 0.0018 !1/d
      KPOC3 = 0. !1/d
!
      KPOP1 = 0.035 !1/d
      KPOP2 = 0.0018 !1/d
      KPOP3 = 0. !1/d
  !
  !Temperature coefficients for G class 1, 2, and 3 mineralization
  !
  !ThtaPON1		THTAPON1		temperature theta for G class 1 pon						unitless		1.1
  !ThtaPON2		THTAPON2		temperature theta for G class 2 pon						unitless		1.15
  !ThtaPON3		THTAPON3		temperature theta for G class 3 pon						unitless		1.17
  !ThtaPOC1		THTAPOC1		temperature theta for G class 1 poc						unitless		1.1
  !ThtaPOC2		THTAPOC2		temperature theta for G class 2 poc						unitless		1.15
  !ThtaPOC3		THTAPOC3		temperature theta for G class 3 poc						unitless		1.17
  !ThtaPOP1		THTAPOP1		temperature theta for G class 1 pop						unitless		1.1
  !ThtaPOP2		THTAPOP2		temperature theta for G class 2 pop						unitless		1.15
  !ThtaPOP3		THTAPOP3		temperature theta for G class 3 pop						unitless		1.17
  !
      THTAPON1 = 1.1
      THTAPON2 = 1.15
      THTAPON3 = 1.17
!
      THTAPOC1 = 1.1
      THTAPOC2 = 1.15
      THTAPOC3 = 1.17
!
      THTAPOP1 = 1.1
      THTAPOP2 = 1.15
      THTAPOP3 = 1.17
!
  !Parameters for partical mixing and benthic stress
  !
  !POC1R			POC1R			reference G1 at which w12base = Dp / H2 at 				mgO2/gD			0.2667				mgC/gD			*Not available
  !									20 degC for DiToro eqn 13.1
  !
  !kBEN_STR		KBENSTR 		first-order decay rate constant for benthic  			day^-1			0.03				1/d
  !									stress (d^-1) for DiToro eqn 13.3
  !
  !KM_O2_Dp		KMO2DP			particle mixing half-saturation constant 				mgO2/L			4					mgO2/L
  !									for O2 (mgO2/L)
!
  ! ??			KLBNTH			ratio of bio-irrigation to bioturbation					unitless
  ! ?? 			DPMIN			minimum particule mixing rate (m^2/day)
!
      POC1R = 0.1 !mgC/gSediment  DMD eqn (13.1) !0.2667/2.667
  !WLong notes eqn(3-26) (DMD book page 278, table 13.1, eqn(13.1))
      KBENSTR = 0.03 !1/d
      KMO2DP = 4.0 !mgO2/L  ~ gO2/m^3
!
      KLBNTH = 0.d0 !to make sure no bioturbation
      DPMIN = 0.d0 !to make minimum  particle diffusion rate zero
  !
  !Parameters for silica
  !
  !KSI			KSI				reaction rate for particulate biogenic Si 				day^-1			0.5					1/d
  !THTASI			THTASI			temperature theta for KSI								unitless		1.1					unitless
  !CSISAT			CSISATT20		saturation concentration for porewater Si				mgSi/L			40					mg Si/m3 	*
  !THTASISAT		THTASISAT		temperature theta for CSISAT							unitless		1.023							*Not available
  !DPIE1SI		DPIE1SI			incremental partition coefficient for Si in layer 1		Unitless		10					Unitless
  !PIE2SI			PIE2SI			partition coefficient for Si in layer 2					L/kgD			100					L/kg
  !KMPSI			KMPSI			particulate biogenic Si half saturation 				mgSi/L			50				mg Si/m3	*
  !									constant for dissolution
  !O2CRITSI		O2CRITSI		critical O2 concentration for layer 1 					mgO2/L			2					O2CRITSI
  !									incremental Si sorption
  !
      KSI = 0.5 !1/d
      THTASI = 1.1 !dimensionless		!DMD book p155 Tabel 7.1
      CSISATT20 = 40 * 1000.0 !mgSi/m^3
      THTASISAT = 1.023 !dimensionless
      DPIE1SI = 10 !unitless
      PIE2SI = 100 !L/kg
      KMPSI = 50000.0 * 1000.0 !mgSi/m^3			!DMD book p156 100mgSi/gsediment ==> p573 50000mgSi/L ==> 50,000,000 mgSi/m^3
      O2CRITSI = 2.0 !mgO2/L ~ gO2/m^3 !used for calculating PIE1SI
  !
  !
  !Parameters for temperature
  !
  !need to make sure DIFFT*DLT/HSED/HSED <0.5 to be stable
      DIFFT = 1000.0 / 86400000.0000000 / 5.0 !m^2/sec      !used for calculating temperature in sediments
!
  !
  !initial conditions for sediment concentratins
  !
  !----------------------------------------------------------------------------------------------------------------------------------------------------------------
  !parameter		parameter		parameter_meaning										unit			initial_value		unit			IC			equation
  !(excel)		(mod_sed.F)																(excel)				(excel)			(mod_sed)		(fortran)	(DMD book)
  !---------------------------------------------------------------------------------------------------------------------------------------------------------------
  !
  !Particulate organic C, N, and P in layer 2
!
  !POC2(1)		CPOCI(1)		G class 1 POC in layer 2								gO2/m^3			89.446				mgC/m^3	*		(89.446/2.667)*1000
  !POC2(2)		CPOCI(2)		G class 2 POC in layer 2								gO2/m^3			622.783				mgC/m^3	*		(622.783/2.667)*1000
  !POC2(3)		CPOCI(3)		G class 3 POC in layer 2								gO2/m^3			6569.343			mgC/m^3	*		(6569.343/2.667)*1000
  !PON2(1)		CPONI(1)		G class 1 PON in layer 2								gN/m^3			1.491				mgN/m^3			1.491*1000
  !PON2(2)		CPONI(2)		G class 2 PON in layer 2								gN/m^3			12.975				mgN/m^3			12.975*1000
  !PON2(3)		CPONI(3)		G class 3 PON in layer 2								gN/m^3			72.993				mgN/m^3			72.993*1000
  !POP2(1)		CPOPI(1)		G class 1 POP in layer 2								gP/m^3			0.894				mgP/m^3			0.894*1000
  !POP2(2)		CPOPI(2)		G class 2 POP in layer 2								gP/m^3			6.228				mgP/m^3			6.228*1000
  !POP2(3)		CPOPI(3)		G class 3 POP in layer 2								gP/m^3			65.693				mgP/m^3			65.693*1000
  !POS2			CPOSI			Particulate biogenic Si in layer 2						gSi/m^3			0.302				mgSi/m^3		0.302*1000
  !
  !Sediment temperature and benthic stress
  !*				CTEMPI																					??					degC			15.0
  !*				BENSTI																					0.0									0.0
!
  !Dissolved constituents in layer 1 and 2 porewater
!
  !NH3(1)			*NH4T1I			Dissolved ammonia N in layer 1 porewater				mgN/L			0.024				mgN/m^3			0.024*1000
  !NH3(2)			NH4T2I			Dissolved ammonia N in layer 2 porewater				mgN/L			0.152				mgN/m^3			0.152*1000
  !NO3(1)			*NO3T1I			Dissolved nitrate+nitrite N in layer 1 porewater		mgN/L			0.041				mgN/m^3			0.041*1000
  !NO3(2)			NO3T2I			Dissolved nitrate+nitrite N in layer 2 porewater		mgN/L			0.007				mgN/m^3			0.007*1000
  !PO4(1)			*PO4T1I			Dissolved phosphate P in layer 1 porewater				mgP/L			0.054				mgP/m^3			0.054*1000
  !PO4(2)			PO4T2I			Dissolved phosphate P in layer 2 porewater				mgP/L			0.290				mgP/m^3			0.290*1000
  !Si(1)			*SIT1I			Dissolved Si in layer 1 porewater						mgSi/L			0.209				mgSi/m^3        0.209*1000
  !Si(2)			SIT2I			Dissolved Si in layer 2 porewater						mgSi/L			1.160				mgSi/m^3		1.160*1000
!
  !HS(1)			*HST1I			Dissolved sulfide in layer 1 porewater 					mgO2/L			0.002               gO2/m^3			0.002
  !									(used if salinity >= 1 psu)
  !HS(2)			HST2I			Dissolved sulfide in layer 2 porewater					mgO2/L			3.348				gO2/m^3			3.348
  !								 	(used if salinity >= 1 psu)
  !CH4(1)			CH41TI			Dissolved methane in layer 1 porewater 					mgO2/L			0.000				gO2/m^3			0.000
  !								 	(used if salinity >= 1 psu)
  !CH4(2)			CH4T2I			Dissolved methane in layer 2 porewater 					mgO2/L			0.000				gO2/m^3			0.000
  !									(used if salinity >= 1 psu)
  !**				SO4T2I																	mgO2/L			??					mgO2/L			0.000
  !*				SO4T2I																	mgO2/L			??					mgO2/L			0.000
!
  !
      CPOCI (1) = 1000 * 115.238 / 2.667 !mgC/m^3
      CPOCI (2) = 1000 * 1047.173 / 2.667 !mgC/m^3
      CPOCI (3) = 1000 * 2046.350 / 2.667 !mgC/m^3
      CPONI (1) = 1000 * 63.877 !mgN/m^3
      CPONI (2) = 1000 * 725.569 !mgN/m^3
      CPONI (3) = 1000 * 756.204 !mgN/m^3
      CPOPI (1) = 1000 * 0.925 !mgP/m^3
      CPOPI (2) = 1000 * 8.404 !mgP/m^3
      CPOPI (3) = 1000 * 16.423 !mgP/m^3
      BENSTI = 0.0 !unitless		!Fortran only ??
      CPOSI = 1000 * 2898.096 !mgSi/m^3
!
  !
  !Note that in Excel version, concentrations are given as dissolved , then total concenration is calculated based on m1,m2 and PIENH4
  !
  !fd1 = (1# / (1# + m1 * KdNH3))
  !fd2 = (1# / (1# + m2 * KdNH3))
  !NH3Tp2(1) = NH3p2(1) / fd1
  !NH3Tp2(2) = NH3p2(2) / fd2
  !
  !m1=m2=0.5, PIENH4=1 ==> fd1=(1/(1+0.5*1))=1/1.5=0.3333
  !						fd2=(1/(1+0.5*1))=1/1.5=0.3333
  !
      PIE1 = PIENH4
      PIE2 = PIENH4
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      NH41TI = 1000 * 0.562 !mgN/m^3		!Excel only
      NH4T2I = 1000 * (4.377) / FD2 !mgN/m^3
!
      NO31TI = 1000 * 0.229 !mgN/m^3		!Excel only
      NO3T2I = 1000 * 0.038 !mgN/m^3
!
  !also make sure PO4T2I is total PO4
      I = 1
      If (SALT(I, KBM1) < SALTND) Then
         DPIE1PO4 = DPIE1PO4F
      Else
         DPIE1PO4 = DPIE1PO4S
      End If
      If (O20 > O2CRITPO4) Then
         PIE1 = PIE2PO4 * DPIE1PO4
      Else
         PIE1 = PIE2PO4 * DPIE1PO4 ** (O20/O2CRITPO4)
      End If
      PIE2 = PIE2PO4
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      PO41TI = 1000 * 0.077 !mgP/m^3		!Excel only
      PO4T2I = 1000 * 0.293 / FD2 !converted to total value
!
      If (O20 > O2CRITSI) Then
         PIE1 = PIE2SI * DPIE1SI
      Else
         PIE1 = PIE2SI * DPIE1SI ** (O20/O2CRITSI)
      End If
      PIE2 = PIE2SI
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      SI1TI = 1000 * 1.483 !mgSi/m^3		!Excel only
      SIT2I = 1000 * 8.278 / FD2 !mgSi/m^3		!converted to total Si in layer 2
!
      PIE1 = PIE1HS
      PIE2 = PIE2HS
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      HS1TI = 0.000 !mgO2/L			!Excel only
      HST2I = 0.000 / FD2 !mgO2/L			!converted to total HS
!
      CH4T2I = 0.0 !mgO2/L
      CH41TI = 0.0 !mgO2/L
!
      SO41TI = 0.0 !mgO2/L			!Neither Excel Nor Fortran
      SO4T2I = 0.0 !mgO2/L			!Fortran only
  !
      SODI = 0.1 !mgO2/m^2/d		!Initialize SOD !WLong: may need to call this SODI
  !Greg Pelletier's Excel used JSOD=0.1 mgO2/m^2/d
      Do I = 1, MLOC
!
         CTEMP (I) = CTEMPI !Sediment temperature
!
         Do JG = 1, 3
            CPOC (I, JG) = CPOCI (JG)
            CPON (I, JG) = CPONI (JG)
            CPOP (I, JG) = CPOPI (JG)
         End Do
!
         CPOS (I) = CPOSI
         BENSTRTM1S (I) = BENSTI !*not in excel
         SODTM1S (I) = SODI
         NH41TM1S (I) = NH41TI
         NH4T2TM1S (I) = NH4T2I
!
         NO31TM1S (I) = NO31TI
         NO3T2TM1S (I) = NO3T2I
!
         PO41TM1S (I) = PO41TI
         PO4T2TM1S (I) = PO4T2I
!
         SI1TM1S (I) = SI1TI
         SIT2TM1S (I) = SIT2I
!
         HS1TM1S (I) = HS1TI
         HST2TM1S (I) = HST2I
!
         CH41TM1S (I) = CH41TI
         CH4T2TM1S (I) = CH4T2I
!
         SO4T2TM1S (I) = SO4T2I !*not in excel
     !
      End Do
!
  !reset the intial values of varables that involve some of the above parameters
      Call SED_INIT2
  !
  !open output file
!
      BFOFN = 'sedtest_2c.csv'
  !
      If (MSR) Then
!
         Open (Unit=BFO, File=BFOFN)
         Rewind (BFO)
!
         IDEBUG_TEST = 2
         If (MSR) WRITE (*,*) 'IDEBUG_TEST=', IDEBUG_TEST
!
     !
     !write the headline the same as excel file "SedFlux_ver17b12_test2c.xlsm"
     !
!
     !Segment ID	JDAY+Time	Jcin	Jnin	Jpin	Jsin	O20	Depth	Tw	NH30	NO30	PO40	SI0	CH40	SALw
     !				SOD	Jnh4	Jno3	JDenitT	Jch4	Jch4g	Jhs	Jpo4	Jsi	Jcout	Jnout	Jpout	NH3(1)	NH3(2)
     !				NO3(1)	NO3(2)	PO4(1)	PO4(2)	Si(1)	Si(2)	CH4(1)	CH4(2)	HS(1)	HS(2)	POC2(1)	POC2(2)	POC2(3)
     !				PON2(1)	PON2(2)	PON2(3)	POP2(1)	POP2(2)	POP2(3)	POS2 H1	BEN_STR
     !
         I = 1
         Write (BFO, 1001) 'SegmentID', ',', 'JDAY', ',', 'Jcin', ',', 'Jnin', ',', 'Jpin', ',', 'Jsin', ',', 'O20', ',', 'Depth', ',', 'Tw', ',', 'NH40', ',', 'NO30', ',', 'PO40', ',', 'SI0', ',', 'CH40', ',', 'SALw', ',', 'SOD', ',', 'Jnh4', ',', 'Jno3',&
        &  ',', 'JDenitT', ',', 'Jch4', ',', 'Jch4g', ',', 'Jhs', ',', 'Jpo4', ',', 'Jsi', ',', 'Jcout', ',', 'Jnout', ',', 'Jpout', ',', 'NH3(1)', ',', 'NH3(2)', ',', 'NO3(1)', ',', 'NO3(2)', ',', 'PO4(1)', ',', 'PO4(2)', ',', 'Si(1)', ',', 'Si(2)', ',', &
        & 'CH4(1)', ',', 'CH4(2)', ',', 'HS(1)', ',', 'HS(2)', ',', 'POC2(1)', ',', 'POC2(2)', ',', 'POC2(3)', ',', 'PON2(1)', ',', 'PON2(2)', ',', 'PON2(3)', ',', 'POP2(1)', ',', 'POP2(2)', ',', 'POP2(3)', ',', 'POS2', ',', 'H1', ',', 'BEN_STR'
!
      End If
1000  Format (I10, 1000(1 X, A10, F15.8))
1001  Format (A10, 1000(1 X, A10, A15))
  !
  !do test 2a calculations
!
      TMSTRT = 0.0 !reference time
      TMEND = 1459.75 !end time
      JDAY = 0.0 !time in days
      DLT = 0.25 * 86400 !10.0		!time step in sec = 864sec=0.01 day=14.4min
      ELTMS = 0.0 !time in sec
      ELTMJD = 0.0 !time in days after reference time
      DLTS = DLT / 86400.
!
      END_RUN = .False.
  !
  !
  !initialize NXJDAY and NXWCL for updating watercolumn conditions
  !
!
      NXJDAY = 0.0
      NXWCL = 0.0
!
      IINT = 0 !time step counter
!
      Do WHILE ( .Not. END_RUN)
     !
         If (MSR) WRITE (*,*) 'JDAY=', JDAY
     !
     !update overlying water column concentration
         If (ITVWCLF == 1) Then
            If (JDAY >= NXJDAY) Call SEDTEST_UPDATE_WATERCOLUMN &
           & (NXJDAY)!read overlying water column concentration etc
!
        !set Overlying water colum concentrations according to the values read fromt the file
        !NXWCL,		&!jday of next record
        !JCIN_R1,	&!C OM flux
        !JNIN_R1,	&!N OM flux
        !JPIN_R1,	&!P OM flux
        !JSIN_R1,	&!Si OM flux
!
        !O20_R1,		&!O20
        !D_R1,		&!total water depth
        !TW_R1,		&!water temperature
        !NH30_R1,	&!nitrate
        !NO30_R1,	&!ammonia
        !PO40_R1,	&!phosphate concentration
        !SIAT0_R1,	&!silicate
        !CH40_R1,	&!methane
        !SALT0_R1	 !salinity
!
            Do I = 1, MLOC
           !		Fortran									InputUnit
               T (I, KWC) = TW_R1 !1  	degC						15				degC
               SALT (I, KWC) = SALT0_R1 !2		ppt							30				ppt
               NH4 (I, KWC) = NH30_R1 !13		gN/m^3						0.015			mgN/L
               NO3 (I, KWC) = NO30_R1 !14		gN/m^3						0.1				mgN/L
               PO4 (I, KWC) = PO40_R1 !20		gP/m^3						0.004			mgP/L
               DOXG (I, KWC) = O20_R1 !27		mgO2/L						5				mgO2/L
               SIAT (I, KWC) = SIAT0_R1 !29		gSi/m^3						0.216			mgSi/L (i.e. gSi/m^3)
               D (I) = D_R1 !		m							2.0				m
!
           !missing CH40	~set to zero in mod_sed
           !missing SO40	~set to SO40MG*0.65306122 in mod_sed
           !missing HS0 	~set to COD(I,KWC) in mod_sed.F and COD is given zero above
            End Do
         End If
!
         Call POM_ACCUMUL
     !
     !average the accumulated POM flux over NDTSED steps
         Do I = 1, MLOC
            Do K = 1, 3
               JPOC (I, K) = JPOCaccum (I, K)
               JPON (I, K) = JPONaccum (I, K)
               JPOP (I, K) = JPOPaccum (I, K)
            End Do
            JPOS (I) = JPOSaccum (I)
         End Do
     !
         Call SED_CALC (.False.)!calculate with time variable formulation
     !
     !set accumulative fluxes back to zero
         JPOCaccum = 0.0
         JPONaccum = 0.0
         JPOPaccum = 0.0
         JPOSaccum = 0.0
     !
     !write out outputs
     !
         If (MSR) Then
        !Fortran							Excel				FortranUnit			ExcelUnit
            I = 1
            Write (BFO, 1000) I, ',', JDAY, ',', (JPOC(I, 1)+JPOC(I, 2)+&
           & JPOC(I, 3)) / 1000.0 * 2.667, ',', (JPON(I, 1)+JPON(I, 2)+&
           & JPON(I, 3)) / 1000.0, ',', (JPOP(I, 1)+JPOP(I, 2)+JPOP(I, &
           & 3)) / 1000.0, ',', JPOS (I) / 1000.0, ',', O20, ',', D &
           & (I), ',', T (I, KBM1), ',', NH40 / 1000, ',', NO30 / 1000, &
           & ',', PO40 / 1000, ',', SI0 / 1000, ',', CH40, ',', SAL, ',&
           &', SOD, ',', JNH4 / 1000, ',', JNO3 / 1000, ',', BENDEN &
           & (1), ',', JCH4, ',', JCH4G, ',', JHS, ',', JPO4 / 1000, ',&
           &', JSI / 1000, ',', 0.0, ',', 0.0, ',', 0.0, ',', NH41 / &
           & 1000, ',', NH42 / 1000, ',', NO31 / 1000, ',', NO32 / &
           & 1000, ',', PO41 / 1000, ',', PO42 / 1000, ',', SI1 / 1000, &
           & ',', SI2 / 1000, ',', CH41, ',', CH42, ',', HS1, ',', HS2, &
           & ',', POC1 / 1000 * 2.667, ',', POC2 / 1000 * 2.667, ',', &
           & POC3 / 1000 * 2.667, ',', PON1 / 1000, ',', PON2 / 1000, '&
           &,', PON3 / 1000, ',', POP1 / 1000, ',', POP2 / 1000, ',', &
           & POP3 / 1000, ',', PSISED / 1000, ',', HSED1 (I) * 1000.0, &
           & ',', BENSTR !'BEN_STR' 			dimensionless		dimensionless
        ! WRITE(BFO,1000)I,',',JDAY,			&
        !	',',(JPOC(I,1)+JPOC(I,2)+JPOC(I,3))/1000.0*2.667,	&	!'Jcin'				mgC/m^2/d			gO2/m^2/d
        !	',',(JPON(I,1)+JPON(I,2)+JPON(I,3))/1000.0,	&			!'Jnin'				*mgN/m^2/d			gN/m^2/d
        !	',',(JPOP(I,1)+JPOP(I,2)+JPOP(I,3))/1000.0,	&			!'Jpin'				*mgP/m^2/d			gP/m^2/d
        !	',',JPOS(I)/1000.0,	&									!'Jsin		 		*mgSi/m^2/d			gSi/m^2/d
        !	',',O20,			&									!'O20'				mgO2/L				mgO2/L
        !	',',D(I),			&									!'Depth'			m					m
        !	',',T(I,KBM1),		&									!'Tw'				degC				degC
        !	',',NH40/1000,		&									!'NH30'				*mgN/m^3			mgN/L
        !	',',NO30/1000,		&									!'NO30'				*mgN/m^3			mgN/L
        !	',',PO40/1000,		&									!'PO40'				*mgP/m^3			mgP/L
        !	',',SI0/1000,		&									!'SI0'				*mgSi/m^3 			mgSi/L				(dissiolved SIAT)
        !	',',CH40,			&									!'CH40'				gO2/m^3				mgO2/L
        !	',',SAL,			&									!'SALw'  			ppt					ppt					!i.e. SALT(I,KWC), ppt
        !	',',SOD,			&									!'SOD'				gO2/m^2/day			gO2/m^2/d
        !	',',JNH4/1000,		&									!'Jnh4				*mgN/m^2/day		gN/m^2/d
        !	',',JNO3/1000,		&									!'Jno3				*mgN/m^2/day		gN/m^2/d
        !	',',BENDEN(1),		&									!'JDenitT'			gN/m^2/day			gN/m^2/d
        !							!JDenit(1) = Denit(1) * NO3(1)
        !							!JDenit(2) = Denit(2) * NO3(2)
        !							!JDenitT = JDenit(1) + JDenit(2)
        !	',',JCH4,			&									!'Jch4'				gO2/m^2/day			gO2/m^2/d
        !	',',JCH4G,			&									!'Jch4g'			gO2/m^2/day			gO2/m^2/d
        !	',',JHS,			&									!'Jhs'				gO2/m^2/day			gO2/m^2/d
        !	',',JPO4/1000,		&									!'Jpo4'				*mgP/m^2/day		gP/m^2/d
        !	',',JSI/1000,		&									!'Jsi'				*mgSi/m^2/day		gSi/m^2/d
        !	',',0.0,			&									!'Jcout'
        !	',',0.0,			&									!'Jnout'
        !	',',0.0,			&									!'Jpout'
        !         !
        ! WLong: Excel calculations here, should correspond to Jcin, Jnin and Jpin
        ! Jcout = SOD + JCH4 + JCH4g + JHS +  	(POC2(1) + POC2(2) + POC2(3)) * w2 +
        !						(		(POC2(1) + POC2(2) + POC2(3))
        !							- 	(POC22(1) + POC22(2) + POC22(3))
        !						) * H2 / tc
        ! Jnout = JNO3 + JNH4 + JDenitT + 		(PON2(1) + PON2(2) + PON2(3)) * w2 +
        !						(		(PON2(1) + PON2(2) + PON2(3))
        !							- 	(PON22(1) + PON22(2) + PON22(3))
        !						) * H2 / tc
        ! Jpout = JPO4 + 						(POP2(1) + POP2(2) + POP2(3)) * w2 +
        !						((POP2(1) + POP2(2) + POP2(3))
        !						 - (POP22(1) + POP22(2) + POP22(3))
        !						 ) * H2 / tc
        !
        !	',',NH41/1000,		&					!'NH3(1)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',NH42/1000,		&					!'NH3(2)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',NO31/1000,		&					!'NO3(1)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',NO32/1000,		&					!'NO3(2)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',PO41/1000,		&					!'PO4(1)'			*mgP/m^3 			mgP/L	(dissolved)
        !	',',PO42/1000,		&					!'PO4(2)'			*mgP/m^3 			mgP/L	(dissolved)
        !	',',SI1/1000,		&					!'Si(1)'			*mgSi/m^3 			mgSi/L	(dissolved)
        !	',',SI2/1000,		&					!'Si(2)'			*mgSi/m^3 			mgSi/L	(dissolved)
        !	',',CH41,			&					!'CH4(1)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',CH42,			&					!'CH4(2)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',HS1,			&					!'HS(1)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',HS2,			&					!'HS(2)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',POC1/1000*2.667,&					!'POC2(1)'			*mgC/m^3 			gO2/m^3	(G1 of POC in layer 2)
        !	',',POC2/1000*2.667,&					!'POC2(2)' 			*mgC/m^3 			gO2/m^3	(G2 of POC in layer 2)
        !	',',POC3/1000*2.667,&					!'POC2(3)' 			*mgC/m^3 			gO2/m^3	(G3 of POC in layer 2)
        !	',',PON1/1000,		&					!'PON2(1)'			*mgN/m^3 			gN/m^3	(G1 of PON in layer 2)
        !	',',PON2/1000,		&					!'PON2(2)' 			*mgN/m^3 			gN/m^3	(G2 of PON in layer 2)
        !	',',PON3/1000,		&					!'PON2(3)' 			*mgN/m^3 			gN/m^3	(G3 of PON in layer 2)
        !	',',POP1/1000,		&					!'POP2(1)'			*mgP/m^3 			gP/m^3	(G1 of POP in layer 2)
        !	',',POP2/1000,		&					!'POP2(2)'			*mgP/m^3 			gP/m^3	(G2 of POP in layer 2)
        !	',',POP3/1000,		&					!'POP2(3)' 			*mgP/m^3 			gP/m^3	(G3 of POP in layer 2)
        !	',',PSISED/1000,	&					!'POS2'				*mgSi/m^3 			gSi/m^3	(particulate biogenic Silicate in layer2)
        !	',',HSED1(I)*1000.0,&					!'H1'				m					mm
        !	',',BENSTR			 					!'BEN_STR' 			dimensionless		dimensionless
        !
            Write (*,*) 1, ',', JDAY
            Write (*,*) 'JPOC=,', (JPOC(I, 1)+JPOC(I, 2)+JPOC(I, 3)) / &
           & 1000.0 * 2.667, 'gO2/m^2/d'
        !'Jcin'				mgC/m^2/d			gO2/m^2/d
!
        !',WS1NET(1)=',WS1NET(I), &!
             !',WB1NETMMD=',WB1NETMMD, 					&	!
        !',B1=',B1(I,KWC),&!
        !',FRACALG1(1)=',FRCALG1(1),&!
        !'JPOC(I,1)=,',JPOC(I,1)/1000*2.667, &	!						*mgC/m^2/d			gO2/m^2/d
        !'JPON(I,1)=,',JPON(I,1)/1000.0, 	&	!						*mgN/m^2/d			gN/m^2/d
        !'JPOP(I,1)=,',JPOP(I,1)/1000.0, 	&	!						*mgP/m^2/d			gP/m^2/d
        !'JPOS(I)  =,',JPOS(I)/1000.0,		&	!						*mgSi/m^2/d			gSi/m^2/d
!
            Write (*,*) 'JPON=,', (JPON(I, 1)+JPON(I, 2)+JPON(I, 3)) / &
           & 1000.0, 'gN/m^2/d'
        !'Jnin'				*mgN/m^2/d			gN/m^2/d
            Write (*,*) 'JPOP=,', (JPOP(I, 1)+JPOP(I, 2)+JPOP(I, 3)) / &
           & 1000.0, 'gP/m^2/d'
        !'Jpin'				*mgP/m^2/d			gP/m^2/d
            Write (*,*) 'JPOS=,', JPOS (I) / 1000.0, 'gSi/m^2/d'
        !'Jsin		 		*mgSi/m^2/d			gSi/m^2/d
            Write (*,*)
            Write (*,*) 'O20=,', O20 !,			&					!'O20'				mgO2/L				mgO2/L
            Write (*,*) 'D=,', D (I)!,			&					!'Depth'			m					m
            Write (*,*) 'T=,', T (I, KBM1)!,		&					!'Tw'				degC				degC
            Write (*,*) 'NH40=,', NH40 / 1000 !,	&					!'NH30'				*mgN/m^3			mgN/L
            Write (*,*) 'NO30=,', NO30 / 1000 !,	&					!'NO30'				*mgN/m^3			mgN/L
            Write (*,*) 'PO40=,', PO40 / 1000 !,	&					!'PO40'				*mgP/m^3			mgP/L
            Write (*,*) 'SI0=,', SI0 / 1000 !,		&					!'SI0'				*mgSi/m^3 			mgSi/L				(dissiolved SIAT)
            Write (*,*) 'CH40=,', CH40 !,			&					!'CH40'				gO2/m^3				mgO2/L
            Write (*,*) 'SAL=,', SAL !,			&					!'SALw'  			ppt					ppt					!i.e. SALT(I,KWC), ppt
            Write (*,*) 'SOD=,', SOD !,			&					!'SOD'				gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JNH4=,', JNH4 / 1000 !,	&					!'Jnh4				*mgN/m^2/day		gN/m^2/d
            Write (*,*) 'JNO3=,', JNO3 / 1000 !,	&					!'Jno3				*mgN/m^2/day		gN/m^2/d
            Write (*,*) 'JDeniT=,', BENDEN (1)!,	&					!'JDenitT'			gN/m^2/day			gN/m^2/d
        !JDenit(1) = Denit(1) * NO3(1)
        !JDenit(2) = Denit(2) * NO3(2)
        !JDenitT = JDenit(1) + JDenit(2)
            Write (*,*) 'JCH4=,', JCH4 !,			&					!'Jch4'				gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JCH4G=,', JCH4G !,		&					!'Jch4g'			gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JHS=,', JHS !,			&					!'Jhs'				gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JPO4=,', JPO4 / 1000 !,	&					!'Jpo4'				*mgP/m^2/day		gP/m^2/d
            Write (*,*) 'JSI=,', JSI / 1000 !,		&					!'Jsi'				*mgSi/m^2/day		gSi/m^2/d
!
        !						',',0.0,				&					!'Jcout'
        !						',',0.0,				&					!'Jnout'
        !						',',0.0,				&					!'Jpout'
!
        !
        !WLong: Excel calculations here, should correspond to Jcin, Jnin and Jpin
        !Jcout = SOD + JCH4 + JCH4g + JHS +  	(POC2(1) + POC2(2) + POC2(3)) * w2 +
        !										(		(POC2(1) + POC2(2) + POC2(3))
        !											- 	(POC22(1) + POC22(2) + POC22(3))
        !										) * H2 / tc
        !Jnout = JNO3 + JNH4 + JDenitT + 		(PON2(1) + PON2(2) + PON2(3)) * w2 +
        !										(		(PON2(1) + PON2(2) + PON2(3))
        !											- 	(PON22(1) + PON22(2) + PON22(3))
        !										) * H2 / tc
        !Jpout = JPO4 + 						(POP2(1) + POP2(2) + POP2(3)) * w2 +
        !										((POP2(1) + POP2(2) + POP2(3))
        !										 - (POP22(1) + POP22(2) + POP22(3))
        !										 ) * H2 / tc
        !
            Write (*,*) 'NH41=,', NH41 / 1000 !,		&					!'NH3(1)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'NH42=,', NH42 / 1000 !,		&					!'NH3(2)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'NO31=,', NO31 / 1000 !,		&					!'NO3(1)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'NO32=,', NO32 / 1000 !,		&					!'NO3(2)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'PO41=,', PO41 / 1000 !,		&					!'PO4(1)'			*mgP/m^3 			mgP/L	(dissolved)
            Write (*,*) 'PO42=,', PO42 / 1000 !,		&					!'PO4(2)'			*mgP/m^3 			mgP/L	(dissolved)
            Write (*,*) 'SI1=,', SI1 / 1000 !,			&					!'Si(1)'			*mgSi/m^3 			mgSi/L	(dissolved)
            Write (*,*) 'SI2=,', SI2 / 1000 !,			&					!'Si(2)'			*mgSi/m^3 			mgSi/L	(dissolved)
            Write (*,*) 'CH41=,', CH41 !,				&					!'CH4(1)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'CH42=,', CH42 !,				&					!'CH4(2)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'SO41=,', SO41 !,				&					!'SO4(1)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'SO42=,', SO42 !,				&					!'SO4(2)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'HSO4=,', HSO4 !,				&					!'HSO4'				m	 				m
            Write (*,*) 'HS1=,', HS1 !,				&					!'HS(1)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'HS2=,', HS2 !,				&					!'HS(2)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'POC1=,', POC1 / 1000 * 2.667 !,	&					!'POC2(1)'			*mgC/m^3 			gO2/m^3	(G1 of POC in layer 2)
            Write (*,*) 'POC2=,', POC2 / 1000 * 2.667 !,	&					!'POC2(2)' 			*mgC/m^3 			gO2/m^3	(G2 of POC in layer 2)
            Write (*,*) 'POC3=,', POC3 / 1000 * 2.667 !,	&					!'POC2(3)' 			*mgC/m^3 			gO2/m^3	(G3 of POC in layer 2)
            Write (*,*) 'PON1=,', PON1 / 1000 !,		&					!'PON2(1)'			*mgN/m^3 			gN/m^3	(G1 of PON in layer 2)
            Write (*,*) 'PON2=,', PON2 / 1000 !,		&					!'PON2(2)' 			*mgN/m^3 			gN/m^3	(G2 of PON in layer 2)
            Write (*,*) 'PON3=,', PON3 / 1000 !,		&					!'PON2(3)' 			*mgN/m^3 			gN/m^3	(G3 of PON in layer 2)
            Write (*,*) 'POP1=,', POP1 / 1000 !,		&					!'POP2(1)'			*mgP/m^3 			gP/m^3	(G1 of POP in layer 2)
            Write (*,*) 'POP2=,', POP2 / 1000 !,		&					!'POP2(2)'			*mgP/m^3 			gP/m^3	(G2 of POP in layer 2)
            Write (*,*) 'POP3=,', POP3 / 1000 !,		&					!'POP2(3)' 			*mgP/m^3 			gP/m^3	(G3 of POP in layer 2)
            Write (*,*) 'PSISED=,', PSISED / 1000 !,	&					!'POS2'				*mgSi/m^3 			gSi/m^3	(particulate biogenic Silicate in layer2)
            Write (*,*) 'CTEMP=', CTEMP (I)
            Write (*,*) 'H1=,', HSED1 (I) * 1000 !,			&					!'H1'				m					mm
            Write (*,*) 'BENSTR=,', BENSTR !'BEN_STR' 			dimensionless		dimensionless
        !
        !		READ(*,*)
         End If
!
         IINT = IINT + 1
         ELTMS = ELTMS + DLT
         JDAY = ELTMS / 86400.
         ELTMJD = JDAY - TMSTRT
     !
         If (JDAY .Ge. TMEND) END_RUN = .True.
!
      End Do
  !
  !
  !close files
  !
  !
      Close (BFI)
      Close (BFO)
  !
      If (ITVWCLF == 1) Then
         Close (WCL)
      End If
  !
  !
  !deallocate sediment module variables
  !
  !
      Call SED_DEALLOC
  !
  !
  !deallocate sav module variables
  !
      If (SAV_CALC) Then
         Call SAV_DEALLOC
         Call SED_SAV_EXCHANGE_DEALLOC
      End If
!
  !
  !deallocate df module variables
  !
      If (DFEEDER) Then
         Call DF_DEALLOC
         Call SED_DF_EXCHANGE_DEALLOC
      End If
  !
  !deallocate sf module variables
  !
  !
      If (SFEEDER) Call SF_DEALLOC
  !
  !deallocate ba module variables
  !
      If (BALGAE_CALC) Call BA_DEALLOC
      Stop 'Finished test2c!'
  !
#endif
  !
#if defined (test2d)
  !
  !test corresponding to SedFlux_ver17b12_test2d.xlsm
  !
  !d. Time-variable solution using initial conditions computed assuming steady state with assumed constant deposition of POM and constant overlying water quality
  !
  !
  !initialize the variables used in the sediment module in the upper
  !modules
  !
  !
  !disable SAV, BA, DF, SF
      SAV_CALC = .False.
      BALGAE_CALC = .False.
      DFEEDER = .False.
      SFEEDER = .False.
!
      BENTHIC_OUTPUT = .True.
!
  !serial run on master processor
      SERIAL = .True.
      PAR = .False.
      MSR = .True.
!
  !
  !set dimension limits
  !
!
      MLOC = 1 !one node
      MTLOC = 1 !one local node
      KBM1 = 10 !10 vertical water column layers
      MGL = 1 !one node
      KWC = KBM1 !overlying water column layer
!
  !
  !allocate hydrodynamic variables (no velocities needed, only depth and  box area)
  !
  !
  !		ART1	&!AREA OF NODE-BASE CONTROl VOLUME
  !		,DZ		&!DELTA-SIGMA VALUE of each layer
  !		,D		 !CURRENT DEPTH
!
  !WLong moved from cell_area.F to here
  !
      Allocate (ART1(0:MTLOC))
      ART1 = 0.0_SP !!AREA OF NODE-BASE CONTROl VOLUME
      Allocate (DZ(KBM1+1))
      DZ = 0.0_SP !!DELTA-SIGMA VALUE
      Allocate (D(0:MTLOC))
      D = 0.0
!
      ART1 = 100.d0 !100m^2
      D = 2.0 !2m deep
      DZ = 1.0 / KBM1 !0.1 as KBM1=10  !sigma layer
!
  !
  !allocate water column water quality variables
  !
!
      SETTLING = .True.
!
  !settling rates
      Allocate (WSSNET(MTLOC))
      WSSNET = 0.0 !suspended sediments settling rate (m/d)
      Allocate (WSLNET(MTLOC))
      WSLNET = 0.0 !LPOM (labile detritus) settling rate (m/d)
      Allocate (WSRNET(MTLOC))
      WSRNET = 0.0 !RPOM (refractory detriuts) settling rate (m/d)
      Allocate (WS1NET(MTLOC))
      WS1NET = 0.0 !alg 1 settling rate (m/d)
      Allocate (WS2NET(MTLOC))
      WS2NET = 0.0 !alg 2 settling rate (m/d)
      Allocate (WS3NET(MTLOC))
      WS3NET = 0.0 !alg 3 settling rate (m/d)
      Allocate (WSUNET(MTLOC))
      WSUNET = 0.0 !particulate biogenic silica settling rate (m/d)
  !
  !water column constituents
      Allocate (T(0:MTLOC, KBM1))
      T = 0.0 !1
      Allocate (SALT(0:MTLOC, KBM1))
      SALT = 0.0 !2
      Allocate (SSI(0:MTLOC, KBM1))
      SSI = 0.0 !3
      Allocate (B1(0:MTLOC, KBM1))
      B1 = 0.0 !4
      Allocate (B2(0:MTLOC, KBM1))
      B2 = 0.0 !5
      Allocate (B3(0:MTLOC, KBM1))
      B3 = 0.0 !6
      Allocate (LPOC(0:MTLOC, KBM1))
      LPOC = 0.0 !11
      Allocate (RPOC(0:MTLOC, KBM1))
      RPOC = 0.0 !12
      Allocate (NH4(0:MTLOC, KBM1))
      NH4 = 0.0 !13
      Allocate (NO3(0:MTLOC, KBM1))
      NO3 = 0.0 !14
      Allocate (LPON(0:MTLOC, KBM1))
      LPON = 0.0 !18
      Allocate (RPON(0:MTLOC, KBM1))
      RPON = 0.0 !19
      Allocate (PO4(0:MTLOC, KBM1))
      PO4 = 0.0 !20
      Allocate (LPOP(0:MTLOC, KBM1))
      LPOP = 0.0 !23
      Allocate (RPOP(0:MTLOC, KBM1))
      RPOP = 0.0 !24
      Allocate (PIP(0:MTLOC, KBM1))
      PIP = 0.0 !25
      Allocate (COD(0:MTLOC, KBM1))
      COD = 0.0 !26
      Allocate (DOXG(0:MTLOC, KBM1))
      DOXG = 0.0 !27
      Allocate (SIUPB(0:MTLOC, KBM1))
      SIUPB = 0.0 !28
      Allocate (SIAT(0:MTLOC, KBM1))
      SIAT = 0.0 !29
  !
  !sediment temperature
      Allocate (CTEMP(MTLOC))
      CTEMP = 0.0
!
  !P to C ratio (gP/gC) of alg 1 2 and 3
      Allocate (Q1(MTLOC, KBM1))
      Q1 = 0.0
      Allocate (Q2(MTLOC, KBM1))
      Q2 = 0.0
      Allocate (Q3(MTLOC, KBM1))
      Q3 = 0.0
!
  !particle (C, N, P, Si, Suspedned Solids) fluxes
!
      Allocate (PCFWS(MTLOC))
      PCFWS = 0.0
      Allocate (PNFWS(MTLOC))
      PNFWS = 0.0
      Allocate (PPFWS(MTLOC))
      PPFWS = 0.0
      Allocate (PSFWS(MTLOC))
      PSFWS = 0.0
      Allocate (SSFWS(MTLOC))
      SSFWS = 0.0 !this is done in mod_wqm.F
  !
  !benthic fluxes to water column
!
      Allocate (BENCH4A(MTLOC))
      BENCH4A = 0.0
      Allocate (BENCH4G(MTLOC))
      BENCH4G = 0.0
      Allocate (BENCOD(MTLOC))
      BENCOD = 0.0
      Allocate (BENDEN(MTLOC))
      BENDEN = 0.0
      Allocate (BENDO(MTLOC))
      BENDO = 0.0
      Allocate (BENDOC(MTLOC))
      BENDOC = 0.0
      Allocate (BENNH4(MTLOC))
      BENNH4 = 0.0
      Allocate (BENNO3(MTLOC))
      BENNO3 = 0.0
      Allocate (BENPO4(MTLOC))
      BENPO4 = 0.0
      Allocate (BENSA(MTLOC))
      BENSA = 0.0
  !
  !
  !
  !allocate sediment module variables
  !
!
      Call SED_INIT !initialize sediment consituent names and allocate variables for sediment module
!
  !
  !allocate sav module variables
  !
      If (SAV_CALC) Then
         Call SAV_ALLOC
         Call SED_SAV_EXCHANGE_ALLOC
      End If
!
  !
  !allocate df module variables
  !
      If (DFEEDER) Then
         Call DF_ALLOC
         Call SED_DF_EXCHANGE_ALLOC
      End If
  !
  !
  !allocate sf module variables
  !
  !
      If (SFEEDER) Call SF_ALLOC
  !
  !allocate ba module variables
  !
      If (BALGAE_CALC) Call BA_ALLOC
  !
!
  !
  !input and output files for the sediment module
  !
!
!
  !file handles
      BFI = 1
      BFO = 12
      WCL = 13
!
  !file names
      BFIFN (1) = 'sedtest_2d.inp'
      BFOFN = 'sedtest_2d.chk'
  !
  !
  !set sediment control parameters or read the sediment module control
  !parameters from file BFI
  !
!
      Open (BFI, File=BFIFN(1), Status='OLD')
  !
      Call SED_READ !read some of the basic variables but it will be over written again afterwards
!
!
      SSTATEIC = 1 !set SSTATEIC=1, so we are going to use steady sate calculation to setup initial conditions
  !and overwrite initial conditions
      STEADY_STATE_SED_IC = SSTATEIC == 1
!
  !particle (C, N, P, Si, Suspedned Solids) fluxes
!
      PCFWS = 0.0
      PNFWS = 0.0
      PPFWS = 0.0
      PSFWS = 0.0
      SSFWS = 0.0
  !
  !
  !driving POC, PON, POP, POS, SSI etc fluxes from water colum that should match Jcin, Jnin, Jpin, Jsin,
  !as well as overlying water concentrations
  !
!
  !	SegmentID	Date+time			Jcin		Jnin		Jpin		Jsin		O20			Depth	Tw		NH30	NO30	PO40	SI0		CH40	SALw
  !     	     						(gO2/m^2/d)	(gN/m^2/d)	(gP/m^2/d)	(gSi/m^2/d)	(mgO2/L)	(m)		(deg C)	(mgN/L)	(mgN/L)	(mgP/L)	(mgSi/L)		(mgO2/L)	(ppt)
  !		1		1/1/00 12:00 AM		0.300000	0.005000	0.003000	0.009375	5.0			2.00	15.0	0.015	0.100	0.004	0.216	0.0		30.0
  !
  !
  !Set water coumn variables for sediment diagenesis run
  !
  !	Fortran													Excel
  !------------------------------------------------------------------------------------
      T = 15.0 !1  	degC						15				degC
      CTEMPI = 15.0
      SALT = 30.0 !2		ppt							30				ppt
      SSI = 0.0 !3		gSolids/m^3					-
      B1 = 1.0 !4		gC/m^3						-
      B2 = 0.0 !5		gC/m^3						-
      B3 = 0.0 !6		gC/m^3						-
      LPOC = 0.0 !11		gC/m^3						-
      RPOC = 0.0 !12		gC/m^3						-
      NH4 = 0.015 !13		gN/m^3						0.015			mgN/L
      NO3 = 0.1 !14		gN/m^3						0.1				mgN/L
      LPON = 0.0 !18		gN/m^3						-
      RPON = 0.0 !19		gN/m^3						-
      PO4 = 0.004 !20		gP/m^3						0.004			mgP/L
      LPOP = 0.0 !23		gP/m^3						-
      RPOP = 0.0 !24		gP/m^3						-
      PIP = 0.0 !25		gP/m^3						-
      COD = 0.0 !26		gO2/m^3						-
      DOXG = 5.0 !27		gO2/m^3						5				mgO2/L
      SIUPB = 0.0 !28		gSi/m^3						0.0				mgSi/L (i.e. gSi/m^3)
      SIAT = 0.21562500 !29		gSi/m^3				0.21562500		mgSi/L (i.e. gSi/m^3)
!
  !
  !parameters related to water colum
  !
  !
  !read from MRL (adsorption coefficients of PO4 and Silicate
      KADPO4 = 0.0004 !m^3/gSolids		!READ (MRL,1080)  KADPO4, KADSA
!
  !basically PIE1, see WLong notes (4-7-5) =PIE2PO4*DPIE2PO4=100*300 L/kg
  !=30000L/kg = 30m^3/kg=30/1000 (m^3/g) = 0.3 m^3/g
!
  !=20 L/kg *20 = 400L/kg = 0.4m^3/kg = 0.4/1000 (m^3/g) 0.0004(m^3/g)
!
      KADSA = 0.001 !m^3/g	!basically PIE2, see WLong notes (4-6-7-1) where
  !m2 is SSI (gSolids/m^3), pie2= should have unit m^3/gSolids
  !i.e. 100L/kg ~ 0.1m^3/kg ~ 0.1m^3/(1000g) = 0.0001 m^3/gSi
  !here it is used to calculate equilibrium speciation of dissolved Si
  !and particulate Si based on suspended solids concentration (gSolids/m^3)
  !in water column
  !
  !Relation between m^3/g and 100L/kg :	0.0001m^3/g = 0.1m^3/kg = 100L/(1kg) = 100L/kg
  !
  !
  !for water column, there is plenty of oxygen, according to WLong notes (4-6-5), we should use PIE1,Si
  !which is 100L/kg * DPIE1SI = 1000L/kg = 1m^3/1000g = 0.001m^3/g
  !
  !
  !	POM flux				Fortran	Formula							Excel						Excel Formula
  !-------------------------------------------------------------------------------------------------------------------
  !	Jcin 			1000*0.3/2.667=112.48		mgC/m^2/d			0.3		  gO2/m^2/d			JPOC(i) = Jcin*fpoc(i)
!
  !JPOC(I,1) = WB1NETMMD*FRCALG1(1)*B1(I,KWC)+										frpoc1=0.65
  !			WB2NETMMD*FRCALG2(1)*B2(I,KWC)+
  !			WB3NETMMD*FRCALG3(1)*B3(I,KWC)+
  !			WLPOMNETMMD*LPOC(I,KWC)
!
  !JPOC(I,2) = WB1NETMMD*FRCALG1(2)*B1(I,KWC)+        								frpoc2=0.2
  !			WB2NETMMD*FRCALG2(2)*B2(I,KWC)+
  !			WB3NETMMD*FRCALG3(2)*B3(I,KWC)+
  !			WRPOMNETMMD*RPOC(I,KWC)*FRPOC(I,2)/
  !			(FRPOC(I,2)+FRPOC(I,3))
!
  !JPOC(I,3) = WB1NETMMD*FRCALG1(3)*B1(I,KWC)+ 										frpoc3=0.15
  !			WB2NETMMD*FRCALG2(3)*B2(I,KWC)+
  !			WB3NETMMD*FRCALG3(3)*B3(I,KWC)+
  !			WRPOMNETMMD*RPOC(I,KWC)*FRPOC(I,3)/
  !				(FRPOC(I,2)+FRPOC(I,3))
!
  !	Jnin 			ANC*1000*0.005/2.667/FRNALG1(1)	mgN/m^2/d		0.005   gN/m^2/d			JPON(i) = Jnin * fpon(i)
  !JPON(I,1) = WB1NETMMD*ANC1*FRNALG1(1)*B1(I,KWC)+									frpon1=0.65
  !			WB2NETMMD*ANC2*FRNALG2(1)*B2(I,KWC)+
  !			WB3NETMMD*ANC3*FRNALG3(1)*B3(I,KWC)+
  !			WLPOMNETMMD*LPON(I,KWC)
  !
  !JPON(I,2) = WB1NETMMD*ANC1*FRNALG1(2)*B1(I,KWC)									frpon2=0.25
  !			+WB2NETMMD*ANC2*FRNALG2(2)*B2(I,KWC)
  !			+WB3NETMMD*ANC3*FRNALG3(2)*B3(I,KWC)
  !			+WRPOMNETMMD*RPON(I,KWC)*FRPON(I,2)/
  !			(FRPON(I,2)+FRPON(I,3))
  !
  !JPON(I,3) = WB1NETMMD*ANC1*FRNALG1(3)*B1(I,KWC) 									frpon3=0.1
  !			+WB2NETMMD*ANC2*FRNALG2(3)*B2(I,KWC)
  !			+WB3NETMMD*ANC3*FRNALG3(3)*B3(I,KWC)
  !			+WRPOMNETMMD*RPON(I,KWC)*FRPON(I,3)/
  !			(FRPON(I,2)+FRPON(I,3))
!
  !	Jpin 			Q*1000*0.003/2.667	mgP/m^2/d					0.003	gP/m^2/d			JPOP(i) = Jpin * fpop(i)
  !JPOP(I,1) = WB1NETMMD*Q1(I,KWC)*FRPALG1(1)*B1(I,KWC)+								frpop1=0.65
  !			WB2NETMMD*Q2(I,KWC)*FRPALG2(1)*B2(I,KWC)+
  !			WB3NETMMD*Q3(I,KWC)*FRPALG3(1)*B3(I,KWC)
  !			WLPOMNETMMD*LPOP(I,KWC)
!
  !JPOP(I,2) = WB1NETMMD*Q1(I,KWC)*FRPALG1(2)*B1(I,KWC)+								frpop2=0.2
  !			WB2NETMMD*Q2(I,KWC)*FRPALG2(2)*B2(I,KWC)+
  !			WB3NETMMD*Q3(I,KWC)*FRPALG3(2)*B3(I,KWC)+
  !			WRPOMNETMMD*RPOP(I,KWC)*FRPOP(I,2)/
  !			(FRPOP(I,2)+FRPOP(I,3))
!
  !JPOP(I,3) = WB1NETMMD*Q1(I,KWC)*FRPALG1(3)*B1(I,KWC)+								frpop3=0.15
  !			+WB2NETMMD*Q2(I,KWC)*FRPALG2(3)*B2(I,KWC)+
  !			+WB3NETMMD*Q3(I,KWC)*FRPALG3(3)*B3(I,KWC)+
  !			+WRPOMNETMMD*RPOP(I,KWC)*FRPOP(I,3)/
  !			(FRPOP(I,2)+FRPOP(I,3))
!
  !	Jsin 			ASC*1000*0.009375/2.667	mgSi/m^2/d				0.009375 gSi/m^2/d			POS2 = (Jsin * tc / H2 + POS2) /
  !																										(1 + KSI * THTASI ^ (Tw - 20) * tc + w2 * tc / H2)
  !JPOS(I) = 	 WB1NETMMD*ASC1*B1(I,KWC)
  !		  	+WB2NETMMD*ASC2*B2(I,KWC)
  !			+WB3NETMMD*ASC3*B3(I,KWC)
  !			+WPSINETMMD*SIUPB(I,KWC)
!
  !
  !POC distribution into G1,G2,G3 by ALG 1,2,3
  !G1 	Alg1				Alg2				Alg3
      FRCALG1 (1) = 0.65
      FRCALG2 (1) = 0.65
      FRCALG3 (1) = 0.65
!
  !G2
      FRCALG1 (2) = 0.20
      FRCALG2 (2) = 0.20
      FRCALG3 (2) = 0.20
!
  !G3
      FRCALG1 (3) = 0.15
      FRCALG2 (3) = 0.15
      FRCALG3 (3) = 0.15
!
!
  !PON distribution into G1,G2,G3 by ALG 1,2,3
  !G1 	Alg1				Alg2				Alg3
      FRNALG1 (1) = 0.65
      FRNALG2 (1) = 0.65
      FRNALG3 (1) = 0.65
!
  !G2
      FRNALG1 (2) = 0.25
      FRNALG2 (2) = 0.25
      FRNALG3 (2) = 0.25
!
  !G3
      FRNALG1 (3) = 0.10
      FRNALG2 (3) = 0.10
      FRNALG3 (3) = 0.10
!
  !POP distribution into G1,G2,G3 by ALG 1,2,3
  !G1 	Alg1				Alg2				Alg3
      FRPALG1 (1) = 0.65
      FRPALG2 (1) = 0.65
      FRPALG3 (1) = 0.65
!
  !G2
      FRPALG1 (2) = 0.2
      FRPALG2 (2) = 0.2
      FRPALG3 (2) = 0.2
!
  !G3
      FRPALG1 (3) = 0.15
      FRPALG2 (3) = 0.15
      FRPALG3 (3) = 0.15
!
  !The following values are given in order to match Jcin, Jnin, Jpin,Jsin in Excel version
      B1 = 1.0
  !only Alg1 settling contributes to POM in sediments
!
      B2 = 0.0
      B3 = 0.0
  !all other particulate organics set to zero
      LPOC = 0.0
      RPOC = 0.0
      LPON = 0.0
      RPON = 0.0
      LPOP = 0.0
      RPOP = 0.0
      SIUPB = 0.0
!
  !Only settling rates of B1 matters for JPOC
      WB1NETMMD = (0.3/2.667) * 1000.0 / B1 (1, KWC)!==>112.4859mm/d 	!here 0.3	 is Jcin (gO2/m^2/d) in Excel version
!
      WB2NETMMD = 1000.0 !1000mm/d  	~1m/d
      WB3NETMMD = 1000.0 !1000mm/d	~1m/d
      WLPOMNETMMD = 1000.0 !1000mm/d	~1m/d
      WRPOMNETMMD = 1000.0 !1000mm/d  ~1m/d
      WPSINETMMD = 1000.0 !1000mm/d	~1m/d
!
  !reset WSLNET etc
      WSLBNET (1) = WLPOMNETMMD / 1000.
      WSRBNET (1) = WRPOMNETMMD / 1000.
      WS1BNET (1) = WB1NETMMD / 1000.0
      WS2BNET (1) = WB2NETMMD / 1000.0
      WS3BNET (1) = WB3NETMMD / 1000.0
      WSUBNET (1) = WPSINETMMD / 1000.0
      WSSBNET (1) = 1.0 !1m/d
!
  !ANC1, Q1, and ASC1 are adjusted to match Jnin, Jpin, Jsin based on Jcin
  !
  !redfield ratios of C,N,P for algae and detritus (modified to match Jcin, Jnin, Jpin, Jsin of Excel vresion)
  !
  !JPON=ANC1*WB1NETMMD*B1*FRNALG1(1)*2.667/1000=0.0518
!
  !
      ANC1 = (0.005) * 1000.0 / B1 (1, KWC) / WB1NETMMD !==>0.0444 !here 0.005 is Jnin (gN/m^2/d) in Excel version
  !ANC1    =	0.175 	!gN/gC
      ANC2 = 0.175
      ANC3 = 0.175
!
  !P to C ratio (gP/gC) of alg 1 2 and 3 (from AGR file)
      Q1 = (0.003) * 1000.0 / B1 (1, KWC) / WB1NETMMD !==>0.0267		!here 0.003 is Jpin (gP/m^2/d) in Excel version
  !Q1    	= 1/41.1		!see DMD book page 10 Table 1.1
      Q2 = 1 / 41.1 !see DMD book page 10 Table 1.1
      Q3 = 1 / 41.1 !see DMD book page 10 Table 1.1
  !
      ASC1 = (0.009375) * 1000.0 / B1 (1, KWC) / WB1NETMMD !==>0.0833	!here 0.009375 is Jsin in gSi/m^2/d in Excel version
  !ASC1	=	0.333 	!gSi/gC
      ASC2 = 0.333
      ASC3 = 0.333
  !
  !
  !set parameters related to SedFlux_ver17b12_test2d.xlsm
  !
!
  !the parameter lists in SedFlux_ver17b12_test2d.xlsm are listed as follows
  !-------------------------------------------------------------------------------------------------------------------------------------------------
  !parameter		parameter		parameter_meaning										unit			default_value		unit		equation
  !(excel)		(mod_sed.F)																(excel)				(excel)			(mod_sed)	(DMD book)
  !-------------------------------------------------------------------------------------------------------------------------------------------------
  !Particle mixing:
  !
  !
  !m1				M1				solids concentration in aerobic layer 1					kgD/L			0.5					kg/L
  !m2				M2				solids concentration in anaerobic layer 2				kgD/L			0.5					kg/L
  !Dp				DPP	(VPMIX)		bioturbation particle mixing coefficient				m^2/d			0.00006				m^2/d
  !Dd				DDP	(VDMIX)		pore water diffusion coefficient						m^2/d			0.0025				m^2/d
  !w2				W2 (VSED)		deep burial velocity									m/d				6.85E-06			cm/yr *
  !H2				H2(HSED,HSEDALL)thickness of sediment anaerobic layer 2					m				0.1					cm *		*input is cm
  !
      M1 = 0.5
      M2 = 0.5
      DPP = 0.00006
      DDP = 0.0025
      HSEDALL = 10.0 !cm
      Do I = 1, MLOC
         HSED (I) = HSEDALL * 0.01 !m
         VSED (I) = 6.85E-06 !m/d  (W2)
         VPMIX (I) = 0.00006 !m^2/d
         VDMIX (I) = 0.0025 !m^2/d
      End Do
  !
  !Reaction velocities (depth integrated reaction rate = reaction velocity (m/d))
  !
  !KappaNH3f		KAPPNH4F		freshwater nitrification velocity						m/d				0.131				m/d
  !KappaNH3s		KAPPNH4S		saltwater nitrification velocity						m/d				0.131				m/d
  !KappaNO3_1f	KAPPNO3F		freshwater denitrification velocity in layer 1			m/d				0.1					m/d
  !KappaNO3_1s	KAPPNO3S		saltwater denitrification velocity in layer 1			m/d				0.1					m/d
  !KappaNO3_2		K2NO3			denitrfication in the anaerobic layer 2					m/d				0.25				m/d
  !KappaCH4		KAPPCH4			methane oxidation in the aerobic layer 1				m/d				0.7					m/d
  !
      KAPPNH4F = 0.131 !m/d
      KAPPNH4S = 0.131 !m/d
      KAPPNO3F = 0.1 !m/d
      KAPPNO3S = 0.1 !m/d
      K2NO3 = 0.25 !m/d		!DMD P.105 Table 4.1
      KAPPCH4 = 0.7 !m/d
!
  !Half saturation constants
  !
  !KM_NH3			KMNH4			nitrification half saturation for NH4N					mgN/L			0.728				mgN/m^3	*
  !KM_O2_NH3		KMNH4O2			nitrification half saturation for O2					mgO2/L			0.37				mgO2/L
  !
      KMNH4 = 0.728 * 1000. !(mgN/m^3)		!see DMD book p73 Table 3.1 (b) (and Median)
  !maybe DMD book is wrong on units
      KMNH4O2 = 0.37 !mgO2/L			!See DMD book p73 Tabl3 3.1 (Median)
!
  !Partitioning coefficients
  !
  !KdNH3			PIENH4			partition coefficient for NH4 in layer 1 and 2			L/kgD			1					L/kg
  !KdPO42			PIE2PO4			partition coefficient for PO4 in layer 2				L/kgD			20					L/kg
  !dKDPO41f		DPIE1PO4F		freshwater factor that increases the aerobic 			unitless		20					L/kg
  !									layer partition coefficient of inorganic P
  !dKDPO41s		DPIE1PO4S		saltwater factor that increases the aerobic 			unitless		20					L/kg
  !									layer partition coefficient of inorganic P
  !O2critPO4		O2CRITPO4		critical O2 concentration in layer 1 for 				mgO2/L			2					mgO2/L
  !									adjustment of partition coefficient
  !									for inorganic P
  !
      PIENH4 = 1.0 !L/kg		(inversed of m1,m2 unit)
      PIE2PO4 = 20.0 !L/kg
      DPIE1PO4F = 20.0 !WLong?? Unitless?	Yes! Note DMD book pp 572 says it should be about 300
      DPIE1PO4S = 20.0 !WLong?? Unitless?
      O2CRITPO4 = 2.0 !mgO2/L
!
  !Temperature coefficients
  !
  !ThtaDp			THTADP			temperature theta for bioturbation mixing between 		unitless		1.117				unitless
  !									layers 1 and 2
  !ThtaDd			THTADD			temperature theta for pore water diffusion between 		unitless		1.08				unitless
  !									layers 1 and 2
  !ThtaKmNH3		THTAKMNH4		temperature theta for nitrification half saturation 	unitless		1.125				unitless
  !									for NH4
  !ThtaNH3		THTANH4			temperature theta for nitrification rate				unitless		1.123				unitless
  !ThtaNO3		THTANO3			temperature theta for denitrification rate				unitless		1.08				unitless
  !ThtaCH4		THTACH4			temperature theta for methane oxidation rate			unitless		1.079				unitless
  !
      THTADP = 1.117
      THTADD = 1.08
  !!ThtaKmNH3	=	!Applied to KM_NH3 in Excel, should be applied to KMNH4 in Fortran code
      THTAKMNH4 = 1.0
  !See DMD book p.73 Table 3.1 (median value is 1.125)
      THTANH4 = 1.123 !See DMD book p.73 Table 3.1 (median value is 1.123)
      THTANO3 = 1.08
      THTACH4 = 1.079
!
  !Salinity thresholds
  !
  !SALTSW			SALTSW			salinity above which sulfide rather than 				psu				1					ppt
  !									methane is produced from C diagenesis
  !SALTND			SALTND			salinity above which saltwater 							psu				1					ppt
  !									nitrification/denitrification rates are
  !									used for aerobic layer
      SALTSW = 1.0 !ppt
      SALTND = 1.0 !ppt
!
  !Sulfide constants
  !
  !KappaH2Sd1		KAPP1HSD		aerobic layer reaction velocity for dissolved 			m/d				0.2					m/d
  !								sulfide oxidation
  !KappaH2Sp1		KAPP1HSP		aerobic layer reaction velocity for particulate 		m/d				0.4					m/d
  !								sulfide oxidation
  !ThtaH2S		THTAH2S			temperature coefficient for sulfide oxidation			unitless		1.079				unitless
  !KMHSO2			KMHSO2			sulfide oxidation normalization constant for O2			mgO2/L			4					mgO2/L
  !KdH2S1			PIE1HS			partition coefficient for sulfide in aerobic layer 1	L/kgD			100					L/kg	*
  !KdH2S2			PIE2HS			partition coefficient for sulfide in anaerobic layer 2	L/kgD			100					L/kg	*
  !
      KAPP1HSD = 0.2 !m/d
      KAPP1HSP = 0.4 !m/d
      THTAH2S = 1.079 !unitless
      KMHSO2 = 4.0 !mgO2/L
      PIE1HS = 100 !L/kg
      PIE2HS = 100 !L/kg
  !
!
  !Fractions of G classes 1 and 2 for settling RPON, RPOC, and RPOP
  !
  !frpon1			FRPON(:,1)		fraction of class 1 pon									unitless		0.65				unitless * only for detritus
  !frpon2			FRPON(:,2)		fraction of class 2 pon									unitless		0.25				unitless * only for detritus
!
  !frpoc1			FRPOC(:,1)		fraction of class 1 poc									unitless		0.65				unitless * only for detritus
  !frpoc2			FRPOC(:,2)		fraction of class 2 poc									unitless		0.2					unitless * only for detritus
!
  !frpop1			FRPOP(:,1)		fraction of class 1 pop									unitless		0.65				unitless * only for detritus
  !frpop2			FRPOP(:,2)		fraction of class 2 pop									unitless		0.2					unitless * only for detritus
!
  !*******WLong: Fortran code used this completely differently!!!!*****
  !Fortran code uses these as fraction of RPOM to be split in G2 and G3
  !G1 has no share of RPOM
!
      Do I = 1, MLOC
         FRPON (I, 1) = 0.0 !No share for G1
         FRPON (I, 2) = 0.65
         FRPON (I, 3) = 1 - FRPON (I, 2) - FRPON (I, 1)
!
         FRPOC (I, 1) = 0.0 !No share for G1
         FRPOC (I, 2) = 0.65
         FRPOC (I, 3) = 1 - FRPOC (I, 1) - FRPOC (I, 2)
!
         FRPOP (I, 1) = 0.0 !No share for G1
         FRPOP (I, 2) = 0.65
         FRPOP (I, 3) = 1 - FRPOP (I, 1) - FRPOP (I, 2)
     !
      End Do
!
  !Diagenesis rate constants for G clase 1, 2, and 3 N/C/P
  !
  !kpon1			KPON1			G class 1 pon mineralization							day^-1			0.035				1/d
  !kpon2			KPON2			G class 2 pon mineralization							day^-1			0.0018				1/d
  !kpon3			KPON3			G class 3 pon mineralization							day^-1			0					1/d
  !kpoc1			KPOC1			G class 1 poc mineralization							day^-1			0.035				1/d
  !kpoc2			KPOC2			G class 2 poc mineralization							day^-1			0.0018				1/d
  !kpoc3			KPOC3			G class 3 poc mineralization							day^-1			0					1/d
  !kpop1			KPOP1			G class 1 pop mineralization							day^-1			0.035				1/d
  !kpop2			KPOP2			G class 2 pop mineralization							day^-1			0.0018				1/d
  !kpop3			KPOP3			G class 3 pop mineralization							day^-1			0					1/d
  !
      KPON1 = 0.035 !1/d
      KPON2 = 0.0018 !1/d
      KPON3 = 0. !1/d
!
      KPOC1 = 0.035 !1/d
      KPOC2 = 0.0018 !1/d
      KPOC3 = 0. !1/d
!
      KPOP1 = 0.035 !1/d
      KPOP2 = 0.0018 !1/d
      KPOP3 = 0. !1/d
  !
  !Temperature coefficients for G class 1, 2, and 3 mineralization
  !
  !ThtaPON1		THTAPON1		temperature theta for G class 1 pon						unitless		1.1
  !ThtaPON2		THTAPON2		temperature theta for G class 2 pon						unitless		1.15
  !ThtaPON3		THTAPON3		temperature theta for G class 3 pon						unitless		1.17
  !ThtaPOC1		THTAPOC1		temperature theta for G class 1 poc						unitless		1.1
  !ThtaPOC2		THTAPOC2		temperature theta for G class 2 poc						unitless		1.15
  !ThtaPOC3		THTAPOC3		temperature theta for G class 3 poc						unitless		1.17
  !ThtaPOP1		THTAPOP1		temperature theta for G class 1 pop						unitless		1.1
  !ThtaPOP2		THTAPOP2		temperature theta for G class 2 pop						unitless		1.15
  !ThtaPOP3		THTAPOP3		temperature theta for G class 3 pop						unitless		1.17
  !
      THTAPON1 = 1.1
      THTAPON2 = 1.15
      THTAPON3 = 1.17
!
      THTAPOC1 = 1.1
      THTAPOC2 = 1.15
      THTAPOC3 = 1.17
!
      THTAPOP1 = 1.1
      THTAPOP2 = 1.15
      THTAPOP3 = 1.17
!
  !Parameters for partical mixing and benthic stress
  !
  !POC1R			POC1R			reference G1 at which w12base = Dp / H2 at 				mgO2/gD			0.2667				mgC/gD			*Not available
  !									20 degC for DiToro eqn 13.1
  !
  !kBEN_STR		KBENSTR 		first-order decay rate constant for benthic  			day^-1			0.03				1/d
  !									stress (d^-1) for DiToro eqn 13.3
  !
  !KM_O2_Dp		KMO2DP			particle mixing half-saturation constant 				mgO2/L			4					mgO2/L
  !									for O2 (mgO2/L)
!
  ! ??			KLBNTH			ratio of bio-irrigation to bioturbation					unitless
  ! ?? 			DPMIN			minimum particule mixing rate (m^2/day)
!
      POC1R = 0.1 !mgC/gSediment  DMD eqn (13.1) !0.2667/2.667
  !WLong notes eqn(3-26) (DMD book page 278, table 13.1, eqn(13.1))
      KBENSTR = 0.03 !1/d
      KMO2DP = 4.0 !mgO2/L  ~ gO2/m^3
!
      KLBNTH = 0.d0 !to make sure no bioturbation
      DPMIN = 0.d0 !to make minimum  particle diffusion rate zero
  !
  !Parameters for silica
  !
  !KSI			KSI				reaction rate for particulate biogenic Si 				day^-1			0.5					1/d
  !THTASI			THTASI			temperature theta for KSI								unitless		1.1					unitless
  !CSISAT			CSISATT20		saturation concentration for porewater Si				mgSi/L			40					mg Si/m3 	*
  !THTASISAT		THTASISAT		temperature theta for CSISAT							unitless		1.023							*Not available
  !DPIE1SI		DPIE1SI			incremental partition coefficient for Si in layer 1		Unitless		10					Unitless
  !PIE2SI			PIE2SI			partition coefficient for Si in layer 2					L/kgD			100					L/kg
  !KMPSI			KMPSI			particulate biogenic Si half saturation 				mgSi/L			50				mg Si/m3	*
  !									constant for dissolution
  !O2CRITSI		O2CRITSI		critical O2 concentration for layer 1 					mgO2/L			2					O2CRITSI
  !									incremental Si sorption
  !
      KSI = 0.5 !1/d
      THTASI = 1.1 !dimensionless		!DMD book p155 Tabel 7.1
      CSISATT20 = 40 * 1000.0 !mgSi/m^3
      THTASISAT = 1.023 !dimensionless
      DPIE1SI = 10 !unitless
      PIE2SI = 100 !L/kg
      KMPSI = 50000.0 * 1000.0 !mgSi/m^3			!DMD book p156 100mgSi/gsediment ==> p573 50000mgSi/L ==> 50,000,000 mgSi/m^3
      O2CRITSI = 2.0 !mgO2/L ~ gO2/m^3 !used for calculating PIE1SI
  !
  !
  !Parameters for temperature
  !
  !need to make sure DIFFT*DLT/HSED/HSED <0.5 to be stable
      DIFFT = 1000.0 / 86400000.0000000 / 5.0 !m^2/sec      !used for calculating temperature in sediments
!
  !
  !initial conditions for sediment concentratins
  !
  !----------------------------------------------------------------------------------------------------------------------------------------------------------------
  !parameter		parameter		parameter_meaning										unit			initial_value		unit			IC			equation
  !(excel)		(mod_sed.F)																(excel)				(excel)			(mod_sed)		(fortran)	(DMD book)
  !---------------------------------------------------------------------------------------------------------------------------------------------------------------
  !
  !Particulate organic C, N, and P in layer 2
!
  !POC2(1)		CPOCI(1)		G class 1 POC in layer 2								gO2/m^3			89.446				mgC/m^3	*		(89.446/2.667)*1000
  !POC2(2)		CPOCI(2)		G class 2 POC in layer 2								gO2/m^3			622.783				mgC/m^3	*		(622.783/2.667)*1000
  !POC2(3)		CPOCI(3)		G class 3 POC in layer 2								gO2/m^3			6569.343			mgC/m^3	*		(6569.343/2.667)*1000
  !PON2(1)		CPONI(1)		G class 1 PON in layer 2								gN/m^3			1.491				mgN/m^3			1.491*1000
  !PON2(2)		CPONI(2)		G class 2 PON in layer 2								gN/m^3			12.975				mgN/m^3			12.975*1000
  !PON2(3)		CPONI(3)		G class 3 PON in layer 2								gN/m^3			72.993				mgN/m^3			72.993*1000
  !POP2(1)		CPOPI(1)		G class 1 POP in layer 2								gP/m^3			0.894				mgP/m^3			0.894*1000
  !POP2(2)		CPOPI(2)		G class 2 POP in layer 2								gP/m^3			6.228				mgP/m^3			6.228*1000
  !POP2(3)		CPOPI(3)		G class 3 POP in layer 2								gP/m^3			65.693				mgP/m^3			65.693*1000
  !POS2			CPOSI			Particulate biogenic Si in layer 2						gSi/m^3			0.302				mgSi/m^3		0.302*1000
  !
  !Sediment temperature and benthic stress
  !*				CTEMPI																					??					degC			15.0
  !*				BENSTI																					0.0									0.0
!
  !Dissolved constituents in layer 1 and 2 porewater
!
  !NH3(1)			*NH4T1I			Dissolved ammonia N in layer 1 porewater				mgN/L			0.024				mgN/m^3			0.024*1000
  !NH3(2)			NH4T2I			Dissolved ammonia N in layer 2 porewater				mgN/L			0.152				mgN/m^3			0.152*1000
  !NO3(1)			*NO3T1I			Dissolved nitrate+nitrite N in layer 1 porewater		mgN/L			0.041				mgN/m^3			0.041*1000
  !NO3(2)			NO3T2I			Dissolved nitrate+nitrite N in layer 2 porewater		mgN/L			0.007				mgN/m^3			0.007*1000
  !PO4(1)			*PO4T1I			Dissolved phosphate P in layer 1 porewater				mgP/L			0.054				mgP/m^3			0.054*1000
  !PO4(2)			PO4T2I			Dissolved phosphate P in layer 2 porewater				mgP/L			0.290				mgP/m^3			0.290*1000
  !Si(1)			*SIT1I			Dissolved Si in layer 1 porewater						mgSi/L			0.209				mgSi/m^3        0.209*1000
  !Si(2)			SIT2I			Dissolved Si in layer 2 porewater						mgSi/L			1.160				mgSi/m^3		1.160*1000
!
  !HS(1)			*HST1I			Dissolved sulfide in layer 1 porewater 					mgO2/L			0.002               gO2/m^3			0.002
  !									(used if salinity >= 1 psu)
  !HS(2)			HST2I			Dissolved sulfide in layer 2 porewater					mgO2/L			3.348				gO2/m^3			3.348
  !								 	(used if salinity >= 1 psu)
  !CH4(1)			CH41TI			Dissolved methane in layer 1 porewater 					mgO2/L			0.000				gO2/m^3			0.000
  !								 	(used if salinity >= 1 psu)
  !CH4(2)			CH4T2I			Dissolved methane in layer 2 porewater 					mgO2/L			0.000				gO2/m^3			0.000
  !									(used if salinity >= 1 psu)
  !**				SO4T2I																	mgO2/L			??					mgO2/L			0.000
  !*				SO4T2I																	mgO2/L			??					mgO2/L			0.000
!
  !
      CPOCI (1) = 1000 * 89.446 / 2.667 !mgC/m^3
      CPOCI (2) = 1000 * 622.783 / 2.667 !mgC/m^3
      CPOCI (3) = 1000 * 6569.343 / 2.667 !mgC/m^3
      CPONI (1) = 1000 * 1.491 !mgN/m^3
      CPONI (2) = 1000 * 12.975 !mgN/m^3
      CPONI (3) = 1000 * 72.993 !mgN/m^3
      CPOPI (1) = 1000 * 0.894 !mgP/m^3
      CPOPI (2) = 1000 * 6.228 !mgP/m^3
      CPOPI (3) = 1000 * 65.693 !mgP/m^3
      BENSTI = 0.0 !unitless		!Fortran only ??
      CPOSI = 1000 * 0.302 !mgSi/m^3
!
  !
  !Note that in Excel version, concentrations are given as dissolved , then total concenration is calculated based on m1,m2 and PIENH4
  !
  !fd1 = (1# / (1# + m1 * KdNH3))
  !fd2 = (1# / (1# + m2 * KdNH3))
  !NH3Tp2(1) = NH3p2(1) / fd1
  !NH3Tp2(2) = NH3p2(2) / fd2
  !
  !m1=m2=0.5, PIENH4=1 ==> fd1=(1/(1+0.5*1))=1/1.5=0.3333
  !						fd2=(1/(1+0.5*1))=1/1.5=0.3333
  !
      PIE1 = PIENH4
      PIE2 = PIENH4
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      NH41TI = 1000 * 0.024 !mgN/m^3		!Excel only
      NH4T2I = 1000 * (0.152) / FD2 !mgN/m^3
!
      NO31TI = 1000 * 0.041 !mgN/m^3		!Excel only
      NO3T2I = 1000 * 0.007 !mgN/m^3
!
  !also make sure PO4T2I is total PO4
      I = 1
      If (SALT(I, KBM1) < SALTND) Then
         DPIE1PO4 = DPIE1PO4F
      Else
         DPIE1PO4 = DPIE1PO4S
      End If
      If (O20 > O2CRITPO4) Then
         PIE1 = PIE2PO4 * DPIE1PO4
      Else
         PIE1 = PIE2PO4 * DPIE1PO4 ** (O20/O2CRITPO4)
      End If
      PIE2 = PIE2PO4
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      PO41TI = 1000 * 0.054 !mgP/m^3		!Excel only
      PO4T2I = 1000 * 0.290 / FD2 !converted to total value
!
      If (O20 > O2CRITSI) Then
         PIE1 = PIE2SI * DPIE1SI
      Else
         PIE1 = PIE2SI * DPIE1SI ** (O20/O2CRITSI)
      End If
      PIE2 = PIE2SI
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      SI1TI = 1000 * 0.209 !mgSi/m^3		!Excel only
      SIT2I = 1000 * 1.160 / FD2 !mgSi/m^3		!converted to total Si in layer 2
!
      PIE1 = PIE1HS
      PIE2 = PIE2HS
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      HS1TI = 0.002 !mgO2/L			!Excel only
      HST2I = 3.348 / FD2 !mgO2/L			!converted to total HS
!
      CH4T2I = 0.0 !mgO2/L
      CH41TI = 0.0 !mgO2/L
!
      SO41TI = 0.0 !mgO2/L			!Neither Excel Nor Fortran
      SO4T2I = 0.0 !mgO2/L			!Fortran only
  !
      SODI = 0.1 !mgO2/m^2/d		!Initialize SOD !WLong: may need to call this SODI
  !Greg Pelletier's Excel used JSOD=0.1 mgO2/m^2/d
      Do I = 1, MLOC
!
         CTEMP (I) = CTEMPI !Sediment temperature
!
         Do JG = 1, 3
            CPOC (I, JG) = CPOCI (JG)
            CPON (I, JG) = CPONI (JG)
            CPOP (I, JG) = CPOPI (JG)
         End Do
!
         CPOS (I) = CPOSI
         BENSTRTM1S (I) = BENSTI !*not in excel
         SODTM1S (I) = SODI
         NH41TM1S (I) = NH41TI
         NH4T2TM1S (I) = NH4T2I
!
         NO31TM1S (I) = NO31TI
         NO3T2TM1S (I) = NO3T2I
!
         PO41TM1S (I) = PO41TI
         PO4T2TM1S (I) = PO4T2I
!
         SI1TM1S (I) = SI1TI
         SIT2TM1S (I) = SIT2I
!
         HS1TM1S (I) = HS1TI
         HST2TM1S (I) = HST2I
!
         CH41TM1S (I) = CH41TI
         CH4T2TM1S (I) = CH4T2I
!
         SO4T2TM1S (I) = SO4T2I !*not in excel
     !
      End Do
!
  !reset the intial values of varables that involve some of the above parameters
      Call SED_INIT2
  !
  !open output file
!
      BFOFN = 'sedtest_2d.csv'
  !
      If (MSR) Then
!
         Open (Unit=BFO, File=BFOFN)
         Rewind (BFO)
!
         IDEBUG_TEST = 2
         If (MSR) WRITE (*,*) 'IDEBUG_TEST=', IDEBUG_TEST
!
     !
     !write the headline the same as excel file "SedFlux_ver17b12_test2d.xlsm"
     !
!
     !Segment ID	JDAY+Time	Jcin	Jnin	Jpin	Jsin	O20	Depth	Tw	NH30	NO30	PO40	SI0	CH40	SALw
     !				SOD	Jnh4	Jno3	JDenitT	Jch4	Jch4g	Jhs	Jpo4	Jsi	Jcout	Jnout	Jpout	NH3(1)	NH3(2)
     !				NO3(1)	NO3(2)	PO4(1)	PO4(2)	Si(1)	Si(2)	CH4(1)	CH4(2)	HS(1)	HS(2)	POC2(1)	POC2(2)	POC2(3)
     !				PON2(1)	PON2(2)	PON2(3)	POP2(1)	POP2(2)	POP2(3)	POS2 H1	BEN_STR
     !
         I = 1
         Write (BFO, 1001) 'SegmentID', ',', 'JDAY', ',', 'Jcin', ',', 'Jnin', ',', 'Jpin', ',', 'Jsin', ',', 'O20', ',', 'Depth', ',', 'Tw', ',', 'NH40', ',', 'NO30', ',', 'PO40', ',', 'SI0', ',', 'CH40', ',', 'SALw', ',', 'SOD', ',', 'Jnh4', ',', 'Jno3',&
        &  ',', 'JDenitT', ',', 'Jch4', ',', 'Jch4g', ',', 'Jhs', ',', 'Jpo4', ',', 'Jsi', ',', 'Jcout', ',', 'Jnout', ',', 'Jpout', ',', 'NH3(1)', ',', 'NH3(2)', ',', 'NO3(1)', ',', 'NO3(2)', ',', 'PO4(1)', ',', 'PO4(2)', ',', 'Si(1)', ',', 'Si(2)', ',', &
        & 'CH4(1)', ',', 'CH4(2)', ',', 'HS(1)', ',', 'HS(2)', ',', 'POC2(1)', ',', 'POC2(2)', ',', 'POC2(3)', ',', 'PON2(1)', ',', 'PON2(2)', ',', 'PON2(3)', ',', 'POP2(1)', ',', 'POP2(2)', ',', 'POP2(3)', ',', 'POS2', ',', 'H1', ',', 'BEN_STR'
!
      End If
1000  Format (I10, 1000(1 X, A10, F15.8))
1001  Format (A10, 1000(1 X, A10, A15))
  !
  !do test 2a calculations
!
      TMSTRT = 0.0 !reference time
      TMEND = 700.0 !end time
      JDAY = 0.0 !time in days
      DLT = 0.01 * 86400 !10.0		!time step in sec = 864sec=0.01 day=14.4min
      ELTMS = 0.0 !time in sec
      ELTMJD = 0.0 !time in days after reference time
      DLTS = DLT / 86400.
!
      END_RUN = .False.
  !
  !
  !initialize NXJDAY and NXWCL for updating watercolumn conditions
  !
!
      NXJDAY = 0.0
      NXWCL = 0.0
!
      IINT = 0 !time step counter
!
      Do WHILE ( .Not. END_RUN)
     !
         If (MSR) WRITE (*,*) 'JDAY=', JDAY
     !
     !update overlying water column concentration
         If (ITVWCLF == 1) Then
            If (JDAY >= NXJDAY) Call SEDTEST_UPDATE_WATERCOLUMN &
           & (NXJDAY)!read overlying water column concentration etc
!
        !set Overlying water colum concentrations according to the values read fromt the file
        !NXWCL,		&!jday of next record
        !JCIN_R1,	&!C OM flux
        !JNIN_R1,	&!N OM flux
        !JPIN_R1,	&!P OM flux
        !JSIN_R1,	&!Si OM flux
!
        !O20_R1,		&!O20
        !D_R1,		&!total water depth
        !TW_R1,		&!water temperature
        !NH30_R1,	&!nitrate
        !NO30_R1,	&!ammonia
        !PO40_R1,	&!phosphate concentration
        !SIAT0_R1,	&!silicate
        !CH40_R1,	&!methane
        !SALT0_R1	 !salinity
!
            Do I = 1, MLOC
           !		Fortran									InputUnit
               T (I, KWC) = TW_R1 !1  	degC						15				degC
               SALT (I, KWC) = SALT0_R1 !2		ppt							30				ppt
               NH4 (I, KWC) = NH30_R1 !13		gN/m^3						0.015			mgN/L
               NO3 (I, KWC) = NO30_R1 !14		gN/m^3						0.1				mgN/L
               PO4 (I, KWC) = PO40_R1 !20		gP/m^3						0.004			mgP/L
               DOXG (I, KWC) = O20_R1 !27		mgO2/L						5				mgO2/L
               SIAT (I, KWC) = SIAT0_R1 !29		gSi/m^3						0.216			mgSi/L (i.e. gSi/m^3)
               D (I) = D_R1 !		m							2.0				m
!
           !missing CH40	~set to zero in mod_sed
           !missing SO40	~set to SO40MG*0.65306122 in mod_sed
           !missing HS0 	~set to COD(I,KWC) in mod_sed.F and COD is given zero above
            End Do
         End If
!
     !
         Call POM_ACCUMUL
     !
     !average the accumulated POM flux over NDTSED steps
         Do I = 1, MLOC
            Do K = 1, 3
               JPOC (I, K) = JPOCaccum (I, K)
               JPON (I, K) = JPONaccum (I, K)
               JPOP (I, K) = JPOPaccum (I, K)
            End Do
            JPOS (I) = JPOSaccum (I)
         End Do
     !
     !
     !
     !set up initial condition by doing one-time calculation using steady state formulation with the
     !forcing given above
     !
!
         If ((JDAY < TMSTRT+DLTS) .And. STEADY_STATE_SED_IC) Then
            Call SED_CALC (.True.)!calculate sediment state variables using steady state formulation
            Call SED_INIT2 !set the initial conditions using the calcualated value
         Else
            Call SED_CALC (.False.)!calculate with time variable solution
         End If
     !
     !set accumulative fluxes back to zero
         JPOCaccum = 0.0
         JPONaccum = 0.0
         JPOPaccum = 0.0
         JPOSaccum = 0.0
     !
     !write out outputs
     !
         If (MSR) Then
        !Fortran							Excel				FortranUnit			ExcelUnit
            I = 1
            Write (BFO, 1000) I, ',', JDAY, ',', (JPOC(I, 1)+JPOC(I, 2)+&
           & JPOC(I, 3)) / 1000.0 * 2.667, ',', (JPON(I, 1)+JPON(I, 2)+&
           & JPON(I, 3)) / 1000.0, ',', (JPOP(I, 1)+JPOP(I, 2)+JPOP(I, &
           & 3)) / 1000.0, ',', JPOS (I) / 1000.0, ',', O20, ',', D &
           & (I), ',', T (I, KBM1), ',', NH40 / 1000, ',', NO30 / 1000, &
           & ',', PO40 / 1000, ',', SI0 / 1000, ',', CH40, ',', SAL, ',&
           &', SOD, ',', JNH4 / 1000, ',', JNO3 / 1000, ',', BENDEN &
           & (1), ',', JCH4, ',', JCH4G, ',', JHS, ',', JPO4 / 1000, ',&
           &', JSI / 1000, ',', 0.0, ',', 0.0, ',', 0.0, ',', NH41 / &
           & 1000, ',', NH42 / 1000, ',', NO31 / 1000, ',', NO32 / &
           & 1000, ',', PO41 / 1000, ',', PO42 / 1000, ',', SI1 / 1000, &
           & ',', SI2 / 1000, ',', CH41, ',', CH42, ',', HS1, ',', HS2, &
           & ',', POC1 / 1000 * 2.667, ',', POC2 / 1000 * 2.667, ',', &
           & POC3 / 1000 * 2.667, ',', PON1 / 1000, ',', PON2 / 1000, '&
           &,', PON3 / 1000, ',', POP1 / 1000, ',', POP2 / 1000, ',', &
           & POP3 / 1000, ',', PSISED / 1000, ',', HSED1 (I) * 1000.0, &
           & ',', BENSTR !'BEN_STR' 			dimensionless		dimensionless
        ! WRITE(BFO,1000)I,',',JDAY,			&
        !	',',(JPOC(I,1)+JPOC(I,2)+JPOC(I,3))/1000.0*2.667,	&	!'Jcin'				mgC/m^2/d			gO2/m^2/d
        !	',',(JPON(I,1)+JPON(I,2)+JPON(I,3))/1000.0,	&			!'Jnin'				*mgN/m^2/d			gN/m^2/d
        !	',',(JPOP(I,1)+JPOP(I,2)+JPOP(I,3))/1000.0,	&			!'Jpin'				*mgP/m^2/d			gP/m^2/d
        !	',',JPOS(I)/1000.0,	&									!'Jsin		 		*mgSi/m^2/d			gSi/m^2/d
        !	',',O20,			&									!'O20'				mgO2/L				mgO2/L
        !	',',D(I),			&									!'Depth'			m					m
        !	',',T(I,KBM1),		&									!'Tw'				degC				degC
        !	',',NH40/1000,		&									!'NH30'				*mgN/m^3			mgN/L
        !	',',NO30/1000,		&									!'NO30'				*mgN/m^3			mgN/L
        !	',',PO40/1000,		&									!'PO40'				*mgP/m^3			mgP/L
        !	',',SI0/1000,		&									!'SI0'				*mgSi/m^3 			mgSi/L				(dissiolved SIAT)
        !	',',CH40,			&									!'CH40'				gO2/m^3				mgO2/L
        !	',',SAL,			&									!'SALw'  			ppt					ppt					!i.e. SALT(I,KWC), ppt
        !	',',SOD,			&									!'SOD'				gO2/m^2/day			gO2/m^2/d
        !	',',JNH4/1000,		&									!'Jnh4				*mgN/m^2/day		gN/m^2/d
        !	',',JNO3/1000,		&									!'Jno3				*mgN/m^2/day		gN/m^2/d
        !	',',BENDEN(1),		&									!'JDenitT'			gN/m^2/day			gN/m^2/d
        !							!JDenit(1) = Denit(1) * NO3(1)
        !							!JDenit(2) = Denit(2) * NO3(2)
        !							!JDenitT = JDenit(1) + JDenit(2)
        !	',',JCH4,			&									!'Jch4'				gO2/m^2/day			gO2/m^2/d
        !	',',JCH4G,			&									!'Jch4g'			gO2/m^2/day			gO2/m^2/d
        !	',',JHS,			&									!'Jhs'				gO2/m^2/day			gO2/m^2/d
        !	',',JPO4/1000,		&									!'Jpo4'				*mgP/m^2/day		gP/m^2/d
        !	',',JSI/1000,		&									!'Jsi'				*mgSi/m^2/day		gSi/m^2/d
        !	',',0.0,			&									!'Jcout'
        !	',',0.0,			&									!'Jnout'
        !	',',0.0,			&									!'Jpout'
        !         !
        ! WLong: Excel calculations here, should correspond to Jcin, Jnin and Jpin
        ! Jcout = SOD + JCH4 + JCH4g + JHS +  	(POC2(1) + POC2(2) + POC2(3)) * w2 +
        !						(		(POC2(1) + POC2(2) + POC2(3))
        !							- 	(POC22(1) + POC22(2) + POC22(3))
        !						) * H2 / tc
        ! Jnout = JNO3 + JNH4 + JDenitT + 		(PON2(1) + PON2(2) + PON2(3)) * w2 +
        !						(		(PON2(1) + PON2(2) + PON2(3))
        !							- 	(PON22(1) + PON22(2) + PON22(3))
        !						) * H2 / tc
        ! Jpout = JPO4 + 						(POP2(1) + POP2(2) + POP2(3)) * w2 +
        !						((POP2(1) + POP2(2) + POP2(3))
        !						 - (POP22(1) + POP22(2) + POP22(3))
        !						 ) * H2 / tc
        !
        !	',',NH41/1000,		&					!'NH3(1)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',NH42/1000,		&					!'NH3(2)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',NO31/1000,		&					!'NO3(1)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',NO32/1000,		&					!'NO3(2)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',PO41/1000,		&					!'PO4(1)'			*mgP/m^3 			mgP/L	(dissolved)
        !	',',PO42/1000,		&					!'PO4(2)'			*mgP/m^3 			mgP/L	(dissolved)
        !	',',SI1/1000,		&					!'Si(1)'			*mgSi/m^3 			mgSi/L	(dissolved)
        !	',',SI2/1000,		&					!'Si(2)'			*mgSi/m^3 			mgSi/L	(dissolved)
        !	',',CH41,			&					!'CH4(1)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',CH42,			&					!'CH4(2)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',HS1,			&					!'HS(1)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',HS2,			&					!'HS(2)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',POC1/1000*2.667,&					!'POC2(1)'			*mgC/m^3 			gO2/m^3	(G1 of POC in layer 2)
        !	',',POC2/1000*2.667,&					!'POC2(2)' 			*mgC/m^3 			gO2/m^3	(G2 of POC in layer 2)
        !	',',POC3/1000*2.667,&					!'POC2(3)' 			*mgC/m^3 			gO2/m^3	(G3 of POC in layer 2)
        !	',',PON1/1000,		&					!'PON2(1)'			*mgN/m^3 			gN/m^3	(G1 of PON in layer 2)
        !	',',PON2/1000,		&					!'PON2(2)' 			*mgN/m^3 			gN/m^3	(G2 of PON in layer 2)
        !	',',PON3/1000,		&					!'PON2(3)' 			*mgN/m^3 			gN/m^3	(G3 of PON in layer 2)
        !	',',POP1/1000,		&					!'POP2(1)'			*mgP/m^3 			gP/m^3	(G1 of POP in layer 2)
        !	',',POP2/1000,		&					!'POP2(2)'			*mgP/m^3 			gP/m^3	(G2 of POP in layer 2)
        !	',',POP3/1000,		&					!'POP2(3)' 			*mgP/m^3 			gP/m^3	(G3 of POP in layer 2)
        !	',',PSISED/1000,	&					!'POS2'				*mgSi/m^3 			gSi/m^3	(particulate biogenic Silicate in layer2)
        !	',',HSED1(I)*1000.0,&					!'H1'				m					mm
        !	',',BENSTR			 					!'BEN_STR' 			dimensionless		dimensionless
        !
            Write (*,*) 1, ',', JDAY
            Write (*,*) 'JPOC=,', (JPOC(I, 1)+JPOC(I, 2)+JPOC(I, 3)) / &
           & 1000.0 * 2.667, 'gO2/m^2/d'
        !'Jcin'				mgC/m^2/d			gO2/m^2/d
!
        !',WS1NET(1)=',WS1NET(I), &!
             !',WB1NETMMD=',WB1NETMMD, 					&	!
        !',B1=',B1(I,KWC),&!
        !',FRACALG1(1)=',FRCALG1(1),&!
        !'JPOC(I,1)=,',JPOC(I,1)/1000*2.667, &	!						*mgC/m^2/d			gO2/m^2/d
        !'JPON(I,1)=,',JPON(I,1)/1000.0, 	&	!						*mgN/m^2/d			gN/m^2/d
        !'JPOP(I,1)=,',JPOP(I,1)/1000.0, 	&	!						*mgP/m^2/d			gP/m^2/d
        !'JPOS(I)  =,',JPOS(I)/1000.0,		&	!						*mgSi/m^2/d			gSi/m^2/d
!
            Write (*,*) 'JPON=,', (JPON(I, 1)+JPON(I, 2)+JPON(I, 3)) / &
           & 1000.0, 'gN/m^2/d'
        !'Jnin'				*mgN/m^2/d			gN/m^2/d
            Write (*,*) 'JPOP=,', (JPOP(I, 1)+JPOP(I, 2)+JPOP(I, 3)) / &
           & 1000.0, 'gP/m^2/d'
        !'Jpin'				*mgP/m^2/d			gP/m^2/d
            Write (*,*) 'JPOS=,', JPOS (I) / 1000.0, 'gSi/m^2/d'
        !'Jsin		 		*mgSi/m^2/d			gSi/m^2/d
            Write (*,*)
            Write (*,*) 'O20=,', O20 !,			&					!'O20'				mgO2/L				mgO2/L
            Write (*,*) 'D=,', D (I)!,			&					!'Depth'			m					m
            Write (*,*) 'T=,', T (I, KBM1)!,		&					!'Tw'				degC				degC
            Write (*,*) 'NH40=,', NH40 / 1000 !,	&					!'NH30'				*mgN/m^3			mgN/L
            Write (*,*) 'NO30=,', NO30 / 1000 !,	&					!'NO30'				*mgN/m^3			mgN/L
            Write (*,*) 'PO40=,', PO40 / 1000 !,	&					!'PO40'				*mgP/m^3			mgP/L
            Write (*,*) 'SI0=,', SI0 / 1000 !,		&					!'SI0'				*mgSi/m^3 			mgSi/L				(dissiolved SIAT)
            Write (*,*) 'CH40=,', CH40 !,			&					!'CH40'				gO2/m^3				mgO2/L
            Write (*,*) 'SAL=,', SAL !,			&					!'SALw'  			ppt					ppt					!i.e. SALT(I,KWC), ppt
            Write (*,*) 'SOD=,', SOD !,			&					!'SOD'				gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JNH4=,', JNH4 / 1000 !,	&					!'Jnh4				*mgN/m^2/day		gN/m^2/d
            Write (*,*) 'JNO3=,', JNO3 / 1000 !,	&					!'Jno3				*mgN/m^2/day		gN/m^2/d
            Write (*,*) 'JDeniT=,', BENDEN (1)!,	&					!'JDenitT'			gN/m^2/day			gN/m^2/d
        !JDenit(1) = Denit(1) * NO3(1)
        !JDenit(2) = Denit(2) * NO3(2)
        !JDenitT = JDenit(1) + JDenit(2)
            Write (*,*) 'JCH4=,', JCH4 !,			&					!'Jch4'				gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JCH4G=,', JCH4G !,		&					!'Jch4g'			gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JHS=,', JHS !,			&					!'Jhs'				gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JPO4=,', JPO4 / 1000 !,	&					!'Jpo4'				*mgP/m^2/day		gP/m^2/d
            Write (*,*) 'JSI=,', JSI / 1000 !,		&					!'Jsi'				*mgSi/m^2/day		gSi/m^2/d
!
        !						',',0.0,				&					!'Jcout'
        !						',',0.0,				&					!'Jnout'
        !						',',0.0,				&					!'Jpout'
!
        !
        !WLong: Excel calculations here, should correspond to Jcin, Jnin and Jpin
        !Jcout = SOD + JCH4 + JCH4g + JHS +  	(POC2(1) + POC2(2) + POC2(3)) * w2 +
        !										(		(POC2(1) + POC2(2) + POC2(3))
        !											- 	(POC22(1) + POC22(2) + POC22(3))
        !										) * H2 / tc
        !Jnout = JNO3 + JNH4 + JDenitT + 		(PON2(1) + PON2(2) + PON2(3)) * w2 +
        !										(		(PON2(1) + PON2(2) + PON2(3))
        !											- 	(PON22(1) + PON22(2) + PON22(3))
        !										) * H2 / tc
        !Jpout = JPO4 + 						(POP2(1) + POP2(2) + POP2(3)) * w2 +
        !										((POP2(1) + POP2(2) + POP2(3))
        !										 - (POP22(1) + POP22(2) + POP22(3))
        !										 ) * H2 / tc
        !
            Write (*,*) 'NH41=,', NH41 / 1000 !,		&					!'NH3(1)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'NH42=,', NH42 / 1000 !,		&					!'NH3(2)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'NO31=,', NO31 / 1000 !,		&					!'NO3(1)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'NO32=,', NO32 / 1000 !,		&					!'NO3(2)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'PO41=,', PO41 / 1000 !,		&					!'PO4(1)'			*mgP/m^3 			mgP/L	(dissolved)
            Write (*,*) 'PO42=,', PO42 / 1000 !,		&					!'PO4(2)'			*mgP/m^3 			mgP/L	(dissolved)
            Write (*,*) 'SI1=,', SI1 / 1000 !,			&					!'Si(1)'			*mgSi/m^3 			mgSi/L	(dissolved)
            Write (*,*) 'SI2=,', SI2 / 1000 !,			&					!'Si(2)'			*mgSi/m^3 			mgSi/L	(dissolved)
            Write (*,*) 'CH41=,', CH41 !,				&					!'CH4(1)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'CH42=,', CH42 !,				&					!'CH4(2)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'SO41=,', SO41 !,				&					!'SO4(1)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'SO42=,', SO42 !,				&					!'SO4(2)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'HSO4=,', HSO4 !,				&					!'HSO4'				m	 				m
            Write (*,*) 'HS1=,', HS1 !,				&					!'HS(1)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'HS2=,', HS2 !,				&					!'HS(2)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'POC1=,', POC1 / 1000 * 2.667 !,	&					!'POC2(1)'			*mgC/m^3 			gO2/m^3	(G1 of POC in layer 2)
            Write (*,*) 'POC2=,', POC2 / 1000 * 2.667 !,	&					!'POC2(2)' 			*mgC/m^3 			gO2/m^3	(G2 of POC in layer 2)
            Write (*,*) 'POC3=,', POC3 / 1000 * 2.667 !,	&					!'POC2(3)' 			*mgC/m^3 			gO2/m^3	(G3 of POC in layer 2)
            Write (*,*) 'PON1=,', PON1 / 1000 !,		&					!'PON2(1)'			*mgN/m^3 			gN/m^3	(G1 of PON in layer 2)
            Write (*,*) 'PON2=,', PON2 / 1000 !,		&					!'PON2(2)' 			*mgN/m^3 			gN/m^3	(G2 of PON in layer 2)
            Write (*,*) 'PON3=,', PON3 / 1000 !,		&					!'PON2(3)' 			*mgN/m^3 			gN/m^3	(G3 of PON in layer 2)
            Write (*,*) 'POP1=,', POP1 / 1000 !,		&					!'POP2(1)'			*mgP/m^3 			gP/m^3	(G1 of POP in layer 2)
            Write (*,*) 'POP2=,', POP2 / 1000 !,		&					!'POP2(2)'			*mgP/m^3 			gP/m^3	(G2 of POP in layer 2)
            Write (*,*) 'POP3=,', POP3 / 1000 !,		&					!'POP2(3)' 			*mgP/m^3 			gP/m^3	(G3 of POP in layer 2)
            Write (*,*) 'PSISED=,', PSISED / 1000 !,	&					!'POS2'				*mgSi/m^3 			gSi/m^3	(particulate biogenic Silicate in layer2)
            Write (*,*) 'CTEMP=', CTEMP (I)
            Write (*,*) 'H1=,', HSED1 (I) * 1000 !,			&					!'H1'				m					mm
            Write (*,*) 'BENSTR=,', BENSTR !'BEN_STR' 			dimensionless		dimensionless
        !
        !		READ(*,*)
         End If
!
         IINT = IINT + 1
         ELTMS = ELTMS + DLT
         JDAY = ELTMS / 86400.
         ELTMJD = JDAY - TMSTRT
     !
         If (JDAY .Ge. TMEND) END_RUN = .True.
!
      End Do
  !
  !
  !close files
  !
  !
      Close (BFI)
      Close (BFO)
  !
      If (ITVWCLF == 1) Then
         Close (WCL)
      End If
  !
  !
  !deallocate sediment module variables
  !
  !
      Call SED_DEALLOC
  !
  !
  !deallocate sav module variables
  !
      If (SAV_CALC) Then
         Call SAV_DEALLOC
         Call SED_SAV_EXCHANGE_DEALLOC
      End If
!
  !
  !deallocate df module variables
  !
      If (DFEEDER) Then
         Call DF_DEALLOC
         Call SED_DF_EXCHANGE_DEALLOC
      End If
  !
  !deallocate sf module variables
  !
  !
      If (SFEEDER) Call SF_DEALLOC
  !
  !deallocate ba module variables
  !
      If (BALGAE_CALC) Call BA_DEALLOC
      Stop 'Finished test2d!'
  !
#endif
  !
#if defined (test2e)
  !
  !test corresponding to SedFlux_ver17b12_test2e.xlsm
!
  !e. Time-variable solution using initial conditions computed assuming steady state with assumed time-variable deposition of POM and time-variable overlying water quality
  !
  !
  !initialize the variables used in the sediment module in the upper
  !modules
  !
  !
  !disable SAV, BA, DF, SF
      SAV_CALC = .False.
      BALGAE_CALC = .False.
      DFEEDER = .False.
      SFEEDER = .False.
!
      BENTHIC_OUTPUT = .True.
!
  !serial run on master processor
      SERIAL = .True.
      PAR = .False.
      MSR = .True.
!
  !
  !set dimension limits
  !
!
      MLOC = 1 !one node
      MTLOC = 1 !one local node
      KBM1 = 10 !10 vertical water column layers
      MGL = 1 !one node
      KWC = KBM1 !overlying water column layer
!
  !
  !allocate hydrodynamic variables (no velocities needed, only depth and  box area)
  !
  !
  !		ART1	&!AREA OF NODE-BASE CONTROl VOLUME
  !		,DZ		&!DELTA-SIGMA VALUE of each layer
  !		,D		 !CURRENT DEPTH
!
  !WLong moved from cell_area.F to here
  !
      Allocate (ART1(0:MTLOC))
      ART1 = 0.0_SP !!AREA OF NODE-BASE CONTROl VOLUME
      Allocate (DZ(KBM1+1))
      DZ = 0.0_SP !!DELTA-SIGMA VALUE
      Allocate (D(0:MTLOC))
      D = 0.0
!
      ART1 = 100.d0 !100m^2
      D = 2.0 !2m deep
      DZ = 1.0 / KBM1 !0.1 as KBM1=10  !sigma layer
!
  !
  !allocate water column water quality variables
  !
!
      SETTLING = .True.
!
  !settling rates
      Allocate (WSSNET(MTLOC))
      WSSNET = 0.0 !suspended sediments settling rate (m/d)
      Allocate (WSLNET(MTLOC))
      WSLNET = 0.0 !LPOM (labile detritus) settling rate (m/d)
      Allocate (WSRNET(MTLOC))
      WSRNET = 0.0 !RPOM (refractory detriuts) settling rate (m/d)
      Allocate (WS1NET(MTLOC))
      WS1NET = 0.0 !alg 1 settling rate (m/d)
      Allocate (WS2NET(MTLOC))
      WS2NET = 0.0 !alg 2 settling rate (m/d)
      Allocate (WS3NET(MTLOC))
      WS3NET = 0.0 !alg 3 settling rate (m/d)
      Allocate (WSUNET(MTLOC))
      WSUNET = 0.0 !particulate biogenic silica settling rate (m/d)
  !
  !water column constituents
      Allocate (T(0:MTLOC, KBM1))
      T = 0.0 !1
      Allocate (SALT(0:MTLOC, KBM1))
      SALT = 0.0 !2
      Allocate (SSI(0:MTLOC, KBM1))
      SSI = 0.0 !3
      Allocate (B1(0:MTLOC, KBM1))
      B1 = 0.0 !4
      Allocate (B2(0:MTLOC, KBM1))
      B2 = 0.0 !5
      Allocate (B3(0:MTLOC, KBM1))
      B3 = 0.0 !6
      Allocate (LPOC(0:MTLOC, KBM1))
      LPOC = 0.0 !11
      Allocate (RPOC(0:MTLOC, KBM1))
      RPOC = 0.0 !12
      Allocate (NH4(0:MTLOC, KBM1))
      NH4 = 0.0 !13
      Allocate (NO3(0:MTLOC, KBM1))
      NO3 = 0.0 !14
      Allocate (LPON(0:MTLOC, KBM1))
      LPON = 0.0 !18
      Allocate (RPON(0:MTLOC, KBM1))
      RPON = 0.0 !19
      Allocate (PO4(0:MTLOC, KBM1))
      PO4 = 0.0 !20
      Allocate (LPOP(0:MTLOC, KBM1))
      LPOP = 0.0 !23
      Allocate (RPOP(0:MTLOC, KBM1))
      RPOP = 0.0 !24
      Allocate (PIP(0:MTLOC, KBM1))
      PIP = 0.0 !25
      Allocate (COD(0:MTLOC, KBM1))
      COD = 0.0 !26
      Allocate (DOXG(0:MTLOC, KBM1))
      DOXG = 0.0 !27
      Allocate (SIUPB(0:MTLOC, KBM1))
      SIUPB = 0.0 !28
      Allocate (SIAT(0:MTLOC, KBM1))
      SIAT = 0.0 !29
  !
  !sediment temperature
      Allocate (CTEMP(MTLOC))
      CTEMP = 0.0
!
  !P to C ratio (gP/gC) of alg 1 2 and 3
      Allocate (Q1(MTLOC, KBM1))
      Q1 = 0.0
      Allocate (Q2(MTLOC, KBM1))
      Q2 = 0.0
      Allocate (Q3(MTLOC, KBM1))
      Q3 = 0.0
!
  !particle (C, N, P, Si, Suspedned Solids) fluxes
!
      Allocate (PCFWS(MTLOC))
      PCFWS = 0.0
      Allocate (PNFWS(MTLOC))
      PNFWS = 0.0
      Allocate (PPFWS(MTLOC))
      PPFWS = 0.0
      Allocate (PSFWS(MTLOC))
      PSFWS = 0.0
      Allocate (SSFWS(MTLOC))
      SSFWS = 0.0 !this is done in mod_wqm.F
  !
  !benthic fluxes to water column
!
      Allocate (BENCH4A(MTLOC))
      BENCH4A = 0.0
      Allocate (BENCH4G(MTLOC))
      BENCH4G = 0.0
      Allocate (BENCOD(MTLOC))
      BENCOD = 0.0
      Allocate (BENDEN(MTLOC))
      BENDEN = 0.0
      Allocate (BENDO(MTLOC))
      BENDO = 0.0
      Allocate (BENDOC(MTLOC))
      BENDOC = 0.0
      Allocate (BENNH4(MTLOC))
      BENNH4 = 0.0
      Allocate (BENNO3(MTLOC))
      BENNO3 = 0.0
      Allocate (BENPO4(MTLOC))
      BENPO4 = 0.0
      Allocate (BENSA(MTLOC))
      BENSA = 0.0
  !
  !
  !allocate sediment module variables
  !
!
      Call SED_INIT !initialize sediment consituent names and allocate variables for sediment module
!
  !
  !allocate sav module variables
  !
      If (SAV_CALC) Then
         Call SAV_ALLOC
         Call SED_SAV_EXCHANGE_ALLOC
      End If
!
  !
  !allocate df module variables
  !
      If (DFEEDER) Then
         Call DF_ALLOC
         Call SED_DF_EXCHANGE_ALLOC
      End If
  !
  !
  !allocate sf module variables
  !
  !
      If (SFEEDER) Call SF_ALLOC
  !
  !allocate ba module variables
  !
      If (BALGAE_CALC) Call BA_ALLOC
  !
!
  !
  !input and output files for the sediment module
  !
!
!
  !file handles
      BFI = 1
      BFO = 12
      WCL = 13
!
  !file names
      BFIFN (1) = 'sedtest_2e.inp'
      BFOFN = 'sedtest_2e.chk'
  !
  !
  !set sediment control parameters or read the sediment module control
  !parameters from file BFI
  !
!
      Open (BFI, File=BFIFN(1), Status='OLD')
  !
      Call SED_READ !read some of the basic variables but it will be over written again afterwards
!
!
      SSTATEIC = 1 !set SSTATEIC=1, so we are going to use steady sate calculation to setup initial conditions
  !and overwrite initial conditions
      STEADY_STATE_SED_IC = SSTATEIC == 1
!
  !particle (C, N, P, Si, Suspedned Solids) fluxes
!
      PCFWS = 0.0
      PNFWS = 0.0
      PPFWS = 0.0
      PSFWS = 0.0
      SSFWS = 0.0
  !
  !
  !driving POC, PON, POP, POS, SSI etc fluxes from water colum that should match Jcin, Jnin, Jpin, Jsin,
  !as well as overlying water concentrations
  !
!
  !	SegmentID	Date+time			Jcin		Jnin		Jpin		Jsin		O20			Depth	Tw		NH30	NO30	PO40	SI0		CH40	SALw
  !     	     						(gO2/m^2/d)	(gN/m^2/d)	(gP/m^2/d)	(gSi/m^2/d)	(mgO2/L)	(m)		(deg C)	(mgN/L)	(mgN/L)	(mgP/L)	(mgSi/L)		(mgO2/L)	(ppt)
  !		1		1/1/00 12:00 AM		0.300000	0.005000	0.003000	0.009375	5.0			2.00	15.0	0.015	0.100	0.004	0.216	0.0		30.0
  !
  !
  !Set water coumn variables for sediment diagenesis run
  !
  !	Fortran													Excel
  !------------------------------------------------------------------------------------
      T = 0.0 !1  	degC						15				degC
      CTEMPI = 0.0 !degC
      SALT = 20.0 !2		ppt							30				ppt
      SSI = 0.0 !3		gSolids/m^3					-
      B1 = 1.0 !4		gC/m^3						-
      B2 = 0.0 !5		gC/m^3						-
      B3 = 0.0 !6		gC/m^3						-
      LPOC = 0.0 !11		gC/m^3						-
      RPOC = 0.0 !12		gC/m^3						-
      NH4 = 0.1 !13		gN/m^3						0.015			mgN/L
      NO3 = 0.3 !14		gN/m^3						0.1				mgN/L
      LPON = 0.0 !18		gN/m^3						-
      RPON = 0.0 !19		gN/m^3						-
      PO4 = 0.04 !20		gP/m^3						0.004			mgP/L
      LPOP = 0.0 !23		gP/m^3						-
      RPOP = 0.0 !24		gP/m^3						-
      PIP = 0.0 !25		gP/m^3						-
      COD = 0.0 !26		gO2/m^3						-
      DOXG = 12.0 !27		gO2/m^3						5				mgO2/L
      SIUPB = 0.0 !28		gSi/m^3						0.0				mgSi/L (i.e. gSi/m^3)
      SIAT = 0.750 !29		gSi/m^3						0.216			mgSi/L (i.e. gSi/m^3)
!
!
  !
  !parameters related to water colum
  !
  !
  !read from MRL (adsorption coefficients of PO4 and Silicate
      KADPO4 = 0.0004 !m^3/gSolids		!READ (MRL,1080)  KADPO4, KADSA
!
  !basically PIE1, see WLong notes (4-7-5) =PIE2PO4*DPIE2PO4=100*300 L/kg
  !=30000L/kg = 30m^3/kg=30/1000 (m^3/g) = 0.3 m^3/g
!
  !=20 L/kg *20 = 400L/kg = 0.4m^3/kg = 0.4/1000 (m^3/g) 0.0004(m^3/kg)
!
      KADSA = 0.001 !m^3/g	!basically PIE2, see WLong notes (4-6-7-1) where
  !m2 is SSI (gSolids/m^3), pie2= should have unit m^3/gSolids
  !i.e. 100L/kg ~ 0.1m^3/kg ~ 0.1m^3/(1000g) = 0.0001 m^3/gSi
  !here it is used to calculate equilibrium speciation of dissolved Si
  !and particulate Si based on suspended solids concentration (gSolids/m^3)
  !in water column
  !
  !Relation between m^3/g and 100L/kg :	0.0001m^3/g = 0.1m^3/kg = 100L/(1kg) = 100L/kg
  !
  !
  !for water column, there is plenty of oxygen, according to WLong notes (4-6-5), we should use PIE1,Si
  !which is 100L/kg * DPIE1SI = 1000L/kg = 1m^3/1000g = 0.001m^3/g
  !
  !
  !	POM flux				Fortran	Formula							Excel						Excel Formula
  !-------------------------------------------------------------------------------------------------------------------
  !	Jcin 			1000*0.3/2.667=112.48		mgC/m^2/d			0.3		  gO2/m^2/d			JPOC(i) = Jcin*fpoc(i)
!
  !JPOC(I,1) = WB1NETMMD*FRCALG1(1)*B1(I,KWC)+										frpoc1=0.65
  !			WB2NETMMD*FRCALG2(1)*B2(I,KWC)+
  !			WB3NETMMD*FRCALG3(1)*B3(I,KWC)+
  !			WLPOMNETMMD*LPOC(I,KWC)
!
  !JPOC(I,2) = WB1NETMMD*FRCALG1(2)*B1(I,KWC)+        								frpoc2=0.2
  !			WB2NETMMD*FRCALG2(2)*B2(I,KWC)+
  !			WB3NETMMD*FRCALG3(2)*B3(I,KWC)+
  !			WRPOMNETMMD*RPOC(I,KWC)*FRPOC(I,2)/
  !			(FRPOC(I,2)+FRPOC(I,3))
!
  !JPOC(I,3) = WB1NETMMD*FRCALG1(3)*B1(I,KWC)+ 										frpoc3=0.15
  !			WB2NETMMD*FRCALG2(3)*B2(I,KWC)+
  !			WB3NETMMD*FRCALG3(3)*B3(I,KWC)+
  !			WRPOMNETMMD*RPOC(I,KWC)*FRPOC(I,3)/
  !				(FRPOC(I,2)+FRPOC(I,3))
!
  !	Jnin 			ANC*1000*0.005/2.667/FRNALG1(1)	mgN/m^2/d		0.005   gN/m^2/d			JPON(i) = Jnin * fpon(i)
  !JPON(I,1) = WB1NETMMD*ANC1*FRNALG1(1)*B1(I,KWC)+									frpon1=0.65
  !			WB2NETMMD*ANC2*FRNALG2(1)*B2(I,KWC)+
  !			WB3NETMMD*ANC3*FRNALG3(1)*B3(I,KWC)+
  !			WLPOMNETMMD*LPON(I,KWC)
  !
  !JPON(I,2) = WB1NETMMD*ANC1*FRNALG1(2)*B1(I,KWC)									frpon2=0.25
  !			+WB2NETMMD*ANC2*FRNALG2(2)*B2(I,KWC)
  !			+WB3NETMMD*ANC3*FRNALG3(2)*B3(I,KWC)
  !			+WRPOMNETMMD*RPON(I,KWC)*FRPON(I,2)/
  !			(FRPON(I,2)+FRPON(I,3))
  !
  !JPON(I,3) = WB1NETMMD*ANC1*FRNALG1(3)*B1(I,KWC) 									frpon3=0.1
  !			+WB2NETMMD*ANC2*FRNALG2(3)*B2(I,KWC)
  !			+WB3NETMMD*ANC3*FRNALG3(3)*B3(I,KWC)
  !			+WRPOMNETMMD*RPON(I,KWC)*FRPON(I,3)/
  !			(FRPON(I,2)+FRPON(I,3))
!
  !	Jpin 			Q*1000*0.003/2.667	mgP/m^2/d					0.003	gP/m^2/d			JPOP(i) = Jpin * fpop(i)
  !JPOP(I,1) = WB1NETMMD*Q1(I,KWC)*FRPALG1(1)*B1(I,KWC)+								frpop1=0.65
  !			WB2NETMMD*Q2(I,KWC)*FRPALG2(1)*B2(I,KWC)+
  !			WB3NETMMD*Q3(I,KWC)*FRPALG3(1)*B3(I,KWC)
  !			WLPOMNETMMD*LPOP(I,KWC)
!
  !JPOP(I,2) = WB1NETMMD*Q1(I,KWC)*FRPALG1(2)*B1(I,KWC)+								frpop2=0.2
  !			WB2NETMMD*Q2(I,KWC)*FRPALG2(2)*B2(I,KWC)+
  !			WB3NETMMD*Q3(I,KWC)*FRPALG3(2)*B3(I,KWC)+
  !			WRPOMNETMMD*RPOP(I,KWC)*FRPOP(I,2)/
  !			(FRPOP(I,2)+FRPOP(I,3))
!
  !JPOP(I,3) = WB1NETMMD*Q1(I,KWC)*FRPALG1(3)*B1(I,KWC)+								frpop3=0.15
  !			+WB2NETMMD*Q2(I,KWC)*FRPALG2(3)*B2(I,KWC)+
  !			+WB3NETMMD*Q3(I,KWC)*FRPALG3(3)*B3(I,KWC)+
  !			+WRPOMNETMMD*RPOP(I,KWC)*FRPOP(I,3)/
  !			(FRPOP(I,2)+FRPOP(I,3))
!
  !	Jsin 			ASC*1000*0.009375/2.667	mgSi/m^2/d				0.009375 gSi/m^2/d			POS2 = (Jsin * tc / H2 + POS2) /
  !																										(1 + KSI * THTASI ^ (Tw - 20) * tc + w2 * tc / H2)
  !JPOS(I) = 	 WB1NETMMD*ASC1*B1(I,KWC)
  !		  	+WB2NETMMD*ASC2*B2(I,KWC)
  !			+WB3NETMMD*ASC3*B3(I,KWC)
  !			+WPSINETMMD*SIUPB(I,KWC)
!
  !
  !POC distribution into G1,G2,G3 by ALG 1,2,3
  !G1 	Alg1				Alg2				Alg3
      FRCALG1 (1) = 0.65
      FRCALG2 (1) = 0.65
      FRCALG3 (1) = 0.65
!
  !G2
      FRCALG1 (2) = 0.20
      FRCALG2 (2) = 0.20
      FRCALG3 (2) = 0.20
!
  !G3
      FRCALG1 (3) = 0.15
      FRCALG2 (3) = 0.15
      FRCALG3 (3) = 0.15
!
!
  !PON distribution into G1,G2,G3 by ALG 1,2,3
  !G1 	Alg1				Alg2				Alg3
      FRNALG1 (1) = 0.65
      FRNALG2 (1) = 0.65
      FRNALG3 (1) = 0.65
!
  !G2
      FRNALG1 (2) = 0.25
      FRNALG2 (2) = 0.25
      FRNALG3 (2) = 0.25
!
  !G3
      FRNALG1 (3) = 0.10
      FRNALG2 (3) = 0.10
      FRNALG3 (3) = 0.10
!
  !POP distribution into G1,G2,G3 by ALG 1,2,3
  !G1 	Alg1				Alg2				Alg3
      FRPALG1 (1) = 0.65
      FRPALG2 (1) = 0.65
      FRPALG3 (1) = 0.65
!
  !G2
      FRPALG1 (2) = 0.2
      FRPALG2 (2) = 0.2
      FRPALG3 (2) = 0.2
!
  !G3
      FRPALG1 (3) = 0.15
      FRPALG2 (3) = 0.15
      FRPALG3 (3) = 0.15
!
  !The following values are given in order to match Jcin, Jnin, Jpin,Jsin in Excel version
      B1 = 1.0
  !only Alg1 settling contributes to POM in sediments
!
      B2 = 0.0
      B3 = 0.0
  !all other particulate organics set to zero
      LPOC = 0.0
      RPOC = 0.0
      LPON = 0.0
      RPON = 0.0
      LPOP = 0.0
      RPOP = 0.0
      SIUPB = 0.0
!
  !Only settling rates of B1 matters for JPOC
      WB1NETMMD = (0.093450/2.667) * 1000.0 / B1 (1, KWC)!==>35.0394mm/d 	!here 0.093450 is Jcin (gO2/m^2/d) in Excel version
!
      WB2NETMMD = 1000.0 !1000mm/d  	~1m/d
      WB3NETMMD = 1000.0 !1000mm/d	~1m/d
      WLPOMNETMMD = 1000.0 !1000mm/d	~1m/d
      WRPOMNETMMD = 1000.0 !1000mm/d  ~1m/d
      WPSINETMMD = 1000.0 !1000mm/d	~1m/d
!
  !reset WSLNET etc
      WSLBNET (1) = WLPOMNETMMD / 1000.
      WSRBNET (1) = WRPOMNETMMD / 1000.
      WS1BNET (1) = WB1NETMMD / 1000.0
      WS2BNET (1) = WB2NETMMD / 1000.0
      WS3BNET (1) = WB3NETMMD / 1000.0
      WSUBNET (1) = WPSINETMMD / 1000.0
      WSSBNET (1) = 1.0 !1m/d
!
  !ANC1, Q1, and ASC1 are adjusted to match Jnin, Jpin, Jsin based on Jcin
  !
  !redfield ratios of C,N,P for algae and detritus (modified to match Jcin, Jnin, Jpin, Jsin of Excel vresion)
  !
  !JPON=ANC1*WB1NETMMD*B1*FRNALG1(1)*2.667/1000=0.0518
  !
      ANC1 = (0.051800) * 1000.0 / B1 (1, KWC) / WB1NETMMD !==>1.4783 !here 0.051800 is Jnin (gN/m^2/d) in Excel version
  !ANC1    =	0.175 	!gN/gC
      ANC2 = 0.175
      ANC3 = 0.175
!
  !P to C ratio (gP/gC) of alg 1 2 and 3 (from AGR file)
      Q1 = (0.00075) * 1000.0 / B1 (1, KWC) / WB1NETMMD !==>0.0214	!here 0.00075is Jpin (gP/m^2/d) in Excel version
  !Q1    	= 1/41.1		!see DMD book page 10 Table 1.1
      Q2 = 1 / 41.1 !see DMD book page 10 Table 1.1
      Q3 = 1 / 41.1 !see DMD book page 10 Table 1.1
  !
      ASC1 = (0.09713) * 1000.0 / B1 (1, KWC) / WB1NETMMD !==>2.7719 	!here 0.097125 is Jsin in gSi/m^2/d in Excel version
  !ASC1	=	0.333 	!gSi/gC
      ASC2 = 0.333
      ASC3 = 0.333
  !
  !
  !set parameters related to SedFlux_ver17b12_test2e.xlsm
  !
!
  !the parameter lists in SedFlux_ver17b12_test2e.xlsm are listed as follows
  !-------------------------------------------------------------------------------------------------------------------------------------------------
  !parameter		parameter		parameter_meaning										unit			default_value		unit		equation
  !(excel)		(mod_sed.F)																(excel)				(excel)			(mod_sed)	(DMD book)
  !-------------------------------------------------------------------------------------------------------------------------------------------------
  !Particle mixing:
  !
  !
  !m1				M1				solids concentration in aerobic layer 1					kgD/L			0.5					kg/L
  !m2				M2				solids concentration in anaerobic layer 2				kgD/L			0.5					kg/L
  !Dp				DPP	(VPMIX)		bioturbation particle mixing coefficient				m^2/d			0.00006				m^2/d
  !Dd				DDP	(VDMIX)		pore water diffusion coefficient						m^2/d			0.0025				m^2/d
  !w2				W2 (VSED)		deep burial velocity									m/d				6.85E-06			cm/yr *
  !H2				H2(HSED,HSEDALL)thickness of sediment anaerobic layer 2					m				0.1					cm *		*input is cm
  !
      M1 = 0.5
      M2 = 0.5
      DPP = 0.00006
      DDP = 0.0025
      HSEDALL = 10.0 !cm
      Do I = 1, MLOC
         HSED (I) = HSEDALL * 0.01 !m
         VSED (I) = 6.85E-06 !m/d  (W2)
         VPMIX (I) = 0.00006 !m^2/d
         VDMIX (I) = 0.0025 !m^2/d
      End Do
  !
  !Reaction velocities (depth integrated reaction rate = reaction velocity (m/d))
  !
  !KappaNH3f		KAPPNH4F		freshwater nitrification velocity						m/d				0.131				m/d
  !KappaNH3s		KAPPNH4S		saltwater nitrification velocity						m/d				0.131				m/d
  !KappaNO3_1f	KAPPNO3F		freshwater denitrification velocity in layer 1			m/d				0.1					m/d
  !KappaNO3_1s	KAPPNO3S		saltwater denitrification velocity in layer 1			m/d				0.1					m/d
  !KappaNO3_2		K2NO3			denitrfication in the anaerobic layer 2					m/d				0.25				m/d
  !KappaCH4		KAPPCH4			methane oxidation in the aerobic layer 1				m/d				0.7					m/d
  !
      KAPPNH4F = 0.131 !m/d
      KAPPNH4S = 0.131 !m/d
      KAPPNO3F = 0.1 !m/d
      KAPPNO3S = 0.1 !m/d
      K2NO3 = 0.25 !m/d		!DMD P.105 Table 4.1
      KAPPCH4 = 0.7 !m/d
!
  !Half saturation constants
  !
  !KM_NH3			KMNH4			nitrification half saturation for NH4N					mgN/L			0.728				mgN/m^3	*
  !KM_O2_NH3		KMNH4O2			nitrification half saturation for O2					mgO2/L			0.37				mgO2/L
  !
      KMNH4 = 0.728 * 1000. !(mgN/m^3)		!see DMD book p73 Table 3.1 (b) (and Median)
  !maybe DMD book is wrong on units
      KMNH4O2 = 0.37 !mgO2/L			!See DMD book p73 Tabl3 3.1 (Median)
!
  !Partitioning coefficients
  !
  !KdNH3			PIENH4			partition coefficient for NH4 in layer 1 and 2			L/kgD			1					L/kg
  !KdPO42			PIE2PO4			partition coefficient for PO4 in layer 2				L/kgD			20					L/kg
  !dKDPO41f		DPIE1PO4F		freshwater factor that increases the aerobic 			unitless		20					L/kg
  !									layer partition coefficient of inorganic P
  !dKDPO41s		DPIE1PO4S		saltwater factor that increases the aerobic 			unitless		20					L/kg
  !									layer partition coefficient of inorganic P
  !O2critPO4		O2CRITPO4		critical O2 concentration in layer 1 for 				mgO2/L			2					mgO2/L
  !									adjustment of partition coefficient
  !									for inorganic P
  !
      PIENH4 = 1.0 !L/kg		(inversed of m1,m2 unit)
      PIE2PO4 = 20.0 !L/kg
      DPIE1PO4F = 20.0 !WLong?? Unitless?	Yes! Note DMD book pp 572 says it should be about 300
      DPIE1PO4S = 20.0 !WLong?? Unitless?
      O2CRITPO4 = 2.0 !mgO2/L
!
  !Temperature coefficients
  !
  !ThtaDp			THTADP			temperature theta for bioturbation mixing between 		unitless		1.117				unitless
  !									layers 1 and 2
  !ThtaDd			THTADD			temperature theta for pore water diffusion between 		unitless		1.08				unitless
  !									layers 1 and 2
  !ThtaKmNH3		THTAKMNH4		temperature theta for nitrification half saturation 	unitless		1.125				unitless
  !									for NH4
  !ThtaNH3		THTANH4			temperature theta for nitrification rate				unitless		1.123				unitless
  !ThtaNO3		THTANO3			temperature theta for denitrification rate				unitless		1.08				unitless
  !ThtaCH4		THTACH4			temperature theta for methane oxidation rate			unitless		1.079				unitless
  !
      THTADP = 1.117
      THTADD = 1.08
  !!ThtaKmNH3	=	!Applied to KM_NH3 in Excel, should be applied to KMNH4 in Fortran code
      THTAKMNH4 = 1.125
  !See DMD book p.73 Table 3.1 (median value is 1.125)
      THTANH4 = 1.123 !See DMD book p.73 Table 3.1 (median value is 1.123)
      THTANO3 = 1.08
      THTACH4 = 1.079
!
  !Salinity thresholds
  !
  !SALTSW			SALTSW			salinity above which sulfide rather than 				psu				1					ppt
  !									methane is produced from C diagenesis
  !SALTND			SALTND			salinity above which saltwater 							psu				1					ppt
  !									nitrification/denitrification rates are
  !									used for aerobic layer
      SALTSW = 1.0 !ppt
      SALTND = 1.0 !ppt
!
  !Sulfide constants
  !
  !KappaH2Sd1		KAPP1HSD		aerobic layer reaction velocity for dissolved 			m/d				0.2					m/d
  !								sulfide oxidation
  !KappaH2Sp1		KAPP1HSP		aerobic layer reaction velocity for particulate 		m/d				0.4					m/d
  !								sulfide oxidation
  !ThtaH2S		THTAH2S			temperature coefficient for sulfide oxidation			unitless		1.079				unitless
  !KMHSO2			KMHSO2			sulfide oxidation normalization constant for O2			mgO2/L			4					mgO2/L
  !KdH2S1			PIE1HS			partition coefficient for sulfide in aerobic layer 1	L/kgD			100					L/kg	*
  !KdH2S2			PIE2HS			partition coefficient for sulfide in anaerobic layer 2	L/kgD			100					L/kg	*
  !
      KAPP1HSD = 0.2 !m/d
      KAPP1HSP = 0.4 !m/d
      THTAH2S = 1.079 !unitless
      KMHSO2 = 4.0 !mgO2/L
      PIE1HS = 100 !L/kg
      PIE2HS = 100 !L/kg
  !
!
  !Fractions of G classes 1 and 2 for settling RPON, RPOC, and RPOP
  !
  !frpon1			FRPON(:,1)		fraction of class 1 pon									unitless		0.65				unitless * only for detritus
  !frpon2			FRPON(:,2)		fraction of class 2 pon									unitless		0.25				unitless * only for detritus
!
  !frpoc1			FRPOC(:,1)		fraction of class 1 poc									unitless		0.65				unitless * only for detritus
  !frpoc2			FRPOC(:,2)		fraction of class 2 poc									unitless		0.2					unitless * only for detritus
!
  !frpop1			FRPOP(:,1)		fraction of class 1 pop									unitless		0.65				unitless * only for detritus
  !frpop2			FRPOP(:,2)		fraction of class 2 pop									unitless		0.2					unitless * only for detritus
!
  !*******WLong: Fortran code used this completely differently!!!!*****
  !Fortran code uses these as fraction of RPOM to be split in G2 and G3
  !G1 has no share of RPOM
!
      Do I = 1, MLOC
         FRPON (I, 1) = 0.0 !No share for G1
         FRPON (I, 2) = 0.65
         FRPON (I, 3) = 1 - FRPON (I, 2) - FRPON (I, 1)
!
         FRPOC (I, 1) = 0.0 !No share for G1
         FRPOC (I, 2) = 0.65
         FRPOC (I, 3) = 1 - FRPOC (I, 1) - FRPOC (I, 2)
!
         FRPOP (I, 1) = 0.0 !No share for G1
         FRPOP (I, 2) = 0.65
         FRPOP (I, 3) = 1 - FRPOP (I, 1) - FRPOP (I, 2)
     !
      End Do
!
  !Diagenesis rate constants for G clase 1, 2, and 3 N/C/P
  !
  !kpon1			KPON1			G class 1 pon mineralization							day^-1			0.035				1/d
  !kpon2			KPON2			G class 2 pon mineralization							day^-1			0.0018				1/d
  !kpon3			KPON3			G class 3 pon mineralization							day^-1			0					1/d
  !kpoc1			KPOC1			G class 1 poc mineralization							day^-1			0.035				1/d
  !kpoc2			KPOC2			G class 2 poc mineralization							day^-1			0.0018				1/d
  !kpoc3			KPOC3			G class 3 poc mineralization							day^-1			0					1/d
  !kpop1			KPOP1			G class 1 pop mineralization							day^-1			0.035				1/d
  !kpop2			KPOP2			G class 2 pop mineralization							day^-1			0.0018				1/d
  !kpop3			KPOP3			G class 3 pop mineralization							day^-1			0					1/d
  !
      KPON1 = 0.035 !1/d
      KPON2 = 0.0018 !1/d
      KPON3 = 0. !1/d
!
      KPOC1 = 0.035 !1/d
      KPOC2 = 0.0018 !1/d
      KPOC3 = 0. !1/d
!
      KPOP1 = 0.035 !1/d
      KPOP2 = 0.0018 !1/d
      KPOP3 = 0. !1/d
  !
  !Temperature coefficients for G class 1, 2, and 3 mineralization
  !
  !ThtaPON1		THTAPON1		temperature theta for G class 1 pon						unitless		1.1
  !ThtaPON2		THTAPON2		temperature theta for G class 2 pon						unitless		1.15
  !ThtaPON3		THTAPON3		temperature theta for G class 3 pon						unitless		1.17
  !ThtaPOC1		THTAPOC1		temperature theta for G class 1 poc						unitless		1.1
  !ThtaPOC2		THTAPOC2		temperature theta for G class 2 poc						unitless		1.15
  !ThtaPOC3		THTAPOC3		temperature theta for G class 3 poc						unitless		1.17
  !ThtaPOP1		THTAPOP1		temperature theta for G class 1 pop						unitless		1.1
  !ThtaPOP2		THTAPOP2		temperature theta for G class 2 pop						unitless		1.15
  !ThtaPOP3		THTAPOP3		temperature theta for G class 3 pop						unitless		1.17
  !
      THTAPON1 = 1.1
      THTAPON2 = 1.15
      THTAPON3 = 1.17
!
      THTAPOC1 = 1.1
      THTAPOC2 = 1.15
      THTAPOC3 = 1.17
!
      THTAPOP1 = 1.1
      THTAPOP2 = 1.15
      THTAPOP3 = 1.17
!
  !Parameters for partical mixing and benthic stress
  !
  !POC1R			POC1R			reference G1 at which w12base = Dp / H2 at 				mgO2/gD			0.2667				mgC/gD			*Not available
  !									20 degC for DiToro eqn 13.1
  !
  !kBEN_STR		KBENSTR 		first-order decay rate constant for benthic  			day^-1			0.03				1/d
  !									stress (d^-1) for DiToro eqn 13.3
  !
  !KM_O2_Dp		KMO2DP			particle mixing half-saturation constant 				mgO2/L			4					mgO2/L
  !									for O2 (mgO2/L)
!
  ! ??			KLBNTH			ratio of bio-irrigation to bioturbation					unitless
  ! ?? 			DPMIN			minimum particule mixing rate (m^2/day)
!
      POC1R = 0.1 !mgC/gSediment  DMD eqn (13.1) !0.2667/2.667
  !WLong notes eqn(3-26) (DMD book page 278, table 13.1, eqn(13.1))
      KBENSTR = 0.03 !1/d
      KMO2DP = 4.0 !mgO2/L  ~ gO2/m^3
!
      KLBNTH = 0.d0 !to make sure no bioturbation
      DPMIN = 0.d0 !to make minimum  particle diffusion rate zero
  !
  !Parameters for silica
  !
  !KSI			KSI				reaction rate for particulate biogenic Si 				day^-1			0.5					1/d
  !THTASI			THTASI			temperature theta for KSI								unitless		1.1					unitless
  !CSISAT			CSISATT20		saturation concentration for porewater Si				mgSi/L			40					mg Si/m3 	*
  !THTASISAT		THTASISAT		temperature theta for CSISAT							unitless		1.023							*Not available
  !DPIE1SI		DPIE1SI			incremental partition coefficient for Si in layer 1		Unitless		10					Unitless
  !PIE2SI			PIE2SI			partition coefficient for Si in layer 2					L/kgD			100					L/kg
  !KMPSI			KMPSI			particulate biogenic Si half saturation 				mgSi/L			50				mg Si/m3	*
  !									constant for dissolution
  !O2CRITSI		O2CRITSI		critical O2 concentration for layer 1 					mgO2/L			2					O2CRITSI
  !									incremental Si sorption
  !
      KSI = 0.5 !1/d
      THTASI = 1.1 !dimensionless		!DMD book p155 Tabel 7.1
      CSISATT20 = 40 * 1000.0 !mgSi/m^3
      THTASISAT = 1.023 !dimensionless
      DPIE1SI = 10 !unitless
      PIE2SI = 100 !L/kg
      KMPSI = 50000.0 * 1000.0 !mgSi/m^3			!DMD book p156 100mgSi/gsediment ==> p573 50000mgSi/L ==> 50,000,000 mgSi/m^3
      O2CRITSI = 2.0 !mgO2/L ~ gO2/m^3 !used for calculating PIE1SI
  !
  !
  !Parameters for temperature
  !
  !need to make sure DIFFT*DLT/HSED/HSED <0.5 to be stable
      DIFFT = 1000.0 / 86400000.0000000 / 5.0 !m^2/sec      !used for calculating temperature in sediments
!
  !
  !initial conditions for sediment concentratins
  !
  !----------------------------------------------------------------------------------------------------------------------------------------------------------------
  !parameter		parameter		parameter_meaning										unit			initial_value		unit			IC			equation
  !(excel)		(mod_sed.F)																(excel)				(excel)			(mod_sed)		(fortran)	(DMD book)
  !---------------------------------------------------------------------------------------------------------------------------------------------------------------
  !
  !Particulate organic C, N, and P in layer 2
!
  !POC2(1)		CPOCI(1)		G class 1 POC in layer 2								gO2/m^3			89.446				mgC/m^3	*		(89.446/2.667)*1000
  !POC2(2)		CPOCI(2)		G class 2 POC in layer 2								gO2/m^3			622.783				mgC/m^3	*		(622.783/2.667)*1000
  !POC2(3)		CPOCI(3)		G class 3 POC in layer 2								gO2/m^3			6569.343			mgC/m^3	*		(6569.343/2.667)*1000
  !PON2(1)		CPONI(1)		G class 1 PON in layer 2								gN/m^3			1.491				mgN/m^3			1.491*1000
  !PON2(2)		CPONI(2)		G class 2 PON in layer 2								gN/m^3			12.975				mgN/m^3			12.975*1000
  !PON2(3)		CPONI(3)		G class 3 PON in layer 2								gN/m^3			72.993				mgN/m^3			72.993*1000
  !POP2(1)		CPOPI(1)		G class 1 POP in layer 2								gP/m^3			0.894				mgP/m^3			0.894*1000
  !POP2(2)		CPOPI(2)		G class 2 POP in layer 2								gP/m^3			6.228				mgP/m^3			6.228*1000
  !POP2(3)		CPOPI(3)		G class 3 POP in layer 2								gP/m^3			65.693				mgP/m^3			65.693*1000
  !POS2			CPOSI			Particulate biogenic Si in layer 2						gSi/m^3			0.302				mgSi/m^3		0.302*1000
  !
  !Sediment temperature and benthic stress
  !*				CTEMPI																					??					degC			15.0
  !*				BENSTI																					0.0									0.0
!
  !Dissolved constituents in layer 1 and 2 porewater
!
  !NH3(1)			*NH4T1I			Dissolved ammonia N in layer 1 porewater				mgN/L			0.024				mgN/m^3			0.024*1000
  !NH3(2)			NH4T2I			Dissolved ammonia N in layer 2 porewater				mgN/L			0.152				mgN/m^3			0.152*1000
  !NO3(1)			*NO3T1I			Dissolved nitrate+nitrite N in layer 1 porewater		mgN/L			0.041				mgN/m^3			0.041*1000
  !NO3(2)			NO3T2I			Dissolved nitrate+nitrite N in layer 2 porewater		mgN/L			0.007				mgN/m^3			0.007*1000
  !PO4(1)			*PO4T1I			Dissolved phosphate P in layer 1 porewater				mgP/L			0.054				mgP/m^3			0.054*1000
  !PO4(2)			PO4T2I			Dissolved phosphate P in layer 2 porewater				mgP/L			0.290				mgP/m^3			0.290*1000
  !Si(1)			*SIT1I			Dissolved Si in layer 1 porewater						mgSi/L			0.209				mgSi/m^3        0.209*1000
  !Si(2)			SIT2I			Dissolved Si in layer 2 porewater						mgSi/L			1.160				mgSi/m^3		1.160*1000
!
  !HS(1)			*HST1I			Dissolved sulfide in layer 1 porewater 					mgO2/L			0.002               gO2/m^3			0.002
  !									(used if salinity >= 1 psu)
  !HS(2)			HST2I			Dissolved sulfide in layer 2 porewater					mgO2/L			3.348				gO2/m^3			3.348
  !								 	(used if salinity >= 1 psu)
  !CH4(1)			CH41TI			Dissolved methane in layer 1 porewater 					mgO2/L			0.000				gO2/m^3			0.000
  !								 	(used if salinity >= 1 psu)
  !CH4(2)			CH4T2I			Dissolved methane in layer 2 porewater 					mgO2/L			0.000				gO2/m^3			0.000
  !									(used if salinity >= 1 psu)
  !**				SO4T2I																	mgO2/L			??					mgO2/L			0.000
  !*				SO4T2I																	mgO2/L			??					mgO2/L			0.000
!
  !
      CPOCI (1) = 1000 * 115.24 / 2.667 !mgC/m^3
      CPOCI (2) = 1000 * 1047.17 / 2.667 !mgC/m^3
      CPOCI (3) = 1000 * 2046.35 / 2.667 !mgC/m^3
      CPONI (1) = 1000 * 63.88 !mgN/m^3
      CPONI (2) = 1000 * 725.57 !mgN/m^3
      CPONI (3) = 1000 * 756.20 !mgN/m^3
      CPOPI (1) = 1000 * 0.925 !mgP/m^3
      CPOPI (2) = 1000 * 8.404 !mgP/m^3
      CPOPI (3) = 1000 * 16.423 !mgP/m^3
      BENSTI = 0.0 !unitless		!Fortran only ??
      CPOSI = 1000 * 2898.096 !mgSi/m^3
!
  !
  !Note that in Excel version, concentrations are given as dissolved , then total concenration is calculated based on m1,m2 and PIENH4
  !
  !fd1 = (1# / (1# + m1 * KdNH3))
  !fd2 = (1# / (1# + m2 * KdNH3))
  !NH3Tp2(1) = NH3p2(1) / fd1
  !NH3Tp2(2) = NH3p2(2) / fd2
  !
  !m1=m2=0.5, PIENH4=1 ==> fd1=(1/(1+0.5*1))=1/1.5=0.3333
  !						fd2=(1/(1+0.5*1))=1/1.5=0.3333
  !
      PIE1 = PIENH4
      PIE2 = PIENH4
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      NH41TI = 1000 * 0.562 !mgN/m^3		!Excel only
      NH4T2I = 1000 * (4.377) / FD2 !mgN/m^3
!
      NO31TI = 1000 * 0.229 !mgN/m^3		!Excel only
      NO3T2I = 1000 * 0.038 !mgN/m^3
!
  !also make sure PO4T2I is total PO4
      I = 1
      If (SALT(I, KBM1) < SALTND) Then
         DPIE1PO4 = DPIE1PO4F
      Else
         DPIE1PO4 = DPIE1PO4S
      End If
      If (O20 > O2CRITPO4) Then
         PIE1 = PIE2PO4 * DPIE1PO4
      Else
         PIE1 = PIE2PO4 * DPIE1PO4 ** (O20/O2CRITPO4)
      End If
      PIE2 = PIE2PO4
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      PO41TI = 1000 * 0.077 !mgP/m^3		!Excel only
      PO4T2I = 1000 * 0.293 / FD2 !converted to total value
!
      If (O20 > O2CRITSI) Then
         PIE1 = PIE2SI * DPIE1SI
      Else
         PIE1 = PIE2SI * DPIE1SI ** (O20/O2CRITSI)
      End If
      PIE2 = PIE2SI
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      SI1TI = 1000 * 1.483 !mgSi/m^3		!Excel only
      SIT2I = 1000 * 8.287 / FD2 !mgSi/m^3		!converted to total Si in layer 2
!
      PIE1 = PIE1HS
      PIE2 = PIE2HS
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      HS1TI = 0.000 !mgO2/L			!Excel only
      HST2I = 0.000 / FD2 !mgO2/L			!converted to total HS
!
      CH4T2I = 0.0 !mgO2/L
      CH41TI = 0.0 !mgO2/L
!
      SO41TI = 0.0 !mgO2/L			!Neither Excel Nor Fortran
      SO4T2I = 0.0 !mgO2/L			!Fortran only
  !
      SODI = 0.1 !mgO2/m^2/d		!Initialize SOD !WLong: may need to call this SODI
  !Greg Pelletier's Excel used JSOD=0.1 mgO2/m^2/d
      Do I = 1, MLOC
!
         CTEMP (I) = CTEMPI !Sediment temperature
!
         Do JG = 1, 3
            CPOC (I, JG) = CPOCI (JG)
            CPON (I, JG) = CPONI (JG)
            CPOP (I, JG) = CPOPI (JG)
         End Do
!
         CPOS (I) = CPOSI
         BENSTRTM1S (I) = BENSTI !*not in excel
         SODTM1S (I) = SODI
         NH41TM1S (I) = NH41TI
         NH4T2TM1S (I) = NH4T2I
!
         NO31TM1S (I) = NO31TI
         NO3T2TM1S (I) = NO3T2I
!
         PO41TM1S (I) = PO41TI
         PO4T2TM1S (I) = PO4T2I
!
         SI1TM1S (I) = SI1TI
         SIT2TM1S (I) = SIT2I
!
         HS1TM1S (I) = HS1TI
         HST2TM1S (I) = HST2I
!
         CH41TM1S (I) = CH41TI
         CH4T2TM1S (I) = CH4T2I
!
         SO4T2TM1S (I) = SO4T2I !*not in excel
     !
      End Do
!
  !reset the intial values of varables that involve some of the above parameters
      Call SED_INIT2
  !
  !open output file
!
      BFOFN = 'sedtest_2e.csv'
  !
      If (MSR) Then
!
         Open (Unit=BFO, File=BFOFN)
         Rewind (BFO)
!
         IDEBUG_TEST = 2
         If (MSR) WRITE (*,*) 'IDEBUG_TEST=', IDEBUG_TEST
!
     !
     !write the headline the same as excel file "SedFlux_ver17b12_test2e.xlsm"
     !
!
     !Segment ID	JDAY+Time	Jcin	Jnin	Jpin	Jsin	O20	Depth	Tw	NH30	NO30	PO40	SI0	CH40	SALw
     !				SOD	Jnh4	Jno3	JDenitT	Jch4	Jch4g	Jhs	Jpo4	Jsi	Jcout	Jnout	Jpout	NH3(1)	NH3(2)
     !				NO3(1)	NO3(2)	PO4(1)	PO4(2)	Si(1)	Si(2)	CH4(1)	CH4(2)	HS(1)	HS(2)	POC2(1)	POC2(2)	POC2(3)
     !				PON2(1)	PON2(2)	PON2(3)	POP2(1)	POP2(2)	POP2(3)	POS2 H1	BEN_STR
     !
         I = 1
         Write (BFO, 1001) 'SegmentID', ',', 'JDAY', ',', 'Jcin', ',', 'Jnin', ',', 'Jpin', ',', 'Jsin', ',', 'O20', ',', 'Depth', ',', 'Tw', ',', 'NH40', ',', 'NO30', ',', 'PO40', ',', 'SI0', ',', 'CH40', ',', 'SALw', ',', 'SOD', ',', 'Jnh4', ',', 'Jno3',&
        &  ',', 'JDenitT', ',', 'Jch4', ',', 'Jch4g', ',', 'Jhs', ',', 'Jpo4', ',', 'Jsi', ',', 'Jcout', ',', 'Jnout', ',', 'Jpout', ',', 'NH3(1)', ',', 'NH3(2)', ',', 'NO3(1)', ',', 'NO3(2)', ',', 'PO4(1)', ',', 'PO4(2)', ',', 'Si(1)', ',', 'Si(2)', ',', &
        & 'CH4(1)', ',', 'CH4(2)', ',', 'HS(1)', ',', 'HS(2)', ',', 'POC2(1)', ',', 'POC2(2)', ',', 'POC2(3)', ',', 'PON2(1)', ',', 'PON2(2)', ',', 'PON2(3)', ',', 'POP2(1)', ',', 'POP2(2)', ',', 'POP2(3)', ',', 'POS2', ',', 'H1', ',', 'BEN_STR'
!
      End If
1000  Format (I10, 1000(1 X, A10, F15.8))
1001  Format (A10, 1000(1 X, A10, A15))
  !
  !do test 2a calculations
!
      TMSTRT = 0.0 !reference time
      TMEND = 1459.75 !end time
      JDAY = 0.0 !time in days
      DLT = 0.25 * 86400 !time step in seconds
      ELTMS = 0.0 !time in sec
      ELTMJD = 0.0 !time in days after reference time
      DLTS = DLT / 86400.
!
      END_RUN = .False.
  !
  !
  !initialize NXJDAY and NXWCL for updating watercolumn conditions
  !
!
      NXJDAY = 0.0
      NXWCL = 0.0
!
      IINT = 0 !time step counter
!
      Do WHILE ( .Not. END_RUN)
     !
         If (MSR) WRITE (*,*) 'JDAY=', JDAY
     !
     !update overlying water column concentration
         If (ITVWCLF == 1) Then
            If (JDAY >= NXJDAY) Call SEDTEST_UPDATE_WATERCOLUMN &
           & (NXJDAY)!read overlying water column concentration etc
!
        !set Overlying water colum concentrations according to the values read fromt the file
        !NXWCL,		&!jday of next record
        !JCIN_R1,	&!C OM flux
        !JNIN_R1,	&!N OM flux
        !JPIN_R1,	&!P OM flux
        !JSIN_R1,	&!Si OM flux
!
        !O20_R1,		&!O20
        !D_R1,		&!total water depth
        !TW_R1,		&!water temperature
        !NH30_R1,	&!nitrate
        !NO30_R1,	&!ammonia
        !PO40_R1,	&!phosphate concentration
        !SIAT0_R1,	&!silicate
        !CH40_R1,	&!methane
        !SALT0_R1	 !salinity
!
            Do I = 1, MLOC
           !		Fortran									InputUnit
               T (I, KWC) = TW_R1 !1  	degC						15				degC
               SALT (I, KWC) = SALT0_R1 !2		ppt							30				ppt
               NH4 (I, KWC) = NH30_R1 !13		gN/m^3						0.015			mgN/L
               NO3 (I, KWC) = NO30_R1 !14		gN/m^3						0.1				mgN/L
               PO4 (I, KWC) = PO40_R1 !20		gP/m^3						0.004			mgP/L
               DOXG (I, KWC) = O20_R1 !27		mgO2/L						5				mgO2/L
               SIAT (I, KWC) = SIAT0_R1 !29		gSi/m^3						0.216			mgSi/L (i.e. gSi/m^3)
               D (I) = D_R1 !		m							2.0				m
!
           !missing CH40	~set to zero in mod_sed
           !missing SO40	~set to SO40MG*0.65306122 in mod_sed
           !missing HS0 	~set to COD(I,KWC) in mod_sed.F and COD is given zero above
            End Do
         End If
!
     !
         Call POM_ACCUMUL
     !
     !average the accumulated POM flux over NDTSED steps
         Do I = 1, MLOC
            Do K = 1, 3
               JPOC (I, K) = JPOCaccum (I, K)
               JPON (I, K) = JPONaccum (I, K)
               JPOP (I, K) = JPOPaccum (I, K)
            End Do
            JPOS (I) = JPOSaccum (I)
         End Do
     !
     !
     !set up initial condition by doing one-time calculation using steady state formulation with the
     !forcing given above
     !
!
         If ((JDAY < TMSTRT+DLTS) .And. STEADY_STATE_SED_IC) Then
            Call SED_CALC (.True.)!calculate sediment state variables using steady state formulation
            Call SED_INIT2 !set the initial conditions using the calcualated value
         Else
            Call SED_CALC (.False.)!calculate with time variable solution
         End If
     !
     !set accumulative fluxes back to zero
         JPOCaccum = 0.0
         JPONaccum = 0.0
         JPOPaccum = 0.0
         JPOSaccum = 0.0
     !
     !
     !write out outputs
     !
         If (MSR) Then
        !Fortran							Excel				FortranUnit			ExcelUnit
            I = 1
            Write (BFO, 1000) I, ',', JDAY, ',', (JPOC(I, 1)+JPOC(I, 2)+&
           & JPOC(I, 3)) / 1000.0 * 2.667, ',', (JPON(I, 1)+JPON(I, 2)+&
           & JPON(I, 3)) / 1000.0, ',', (JPOP(I, 1)+JPOP(I, 2)+JPOP(I, &
           & 3)) / 1000.0, ',', JPOS (I) / 1000.0, ',', O20, ',', D &
           & (I), ',', T (I, KBM1), ',', NH40 / 1000, ',', NO30 / 1000, &
           & ',', PO40 / 1000, ',', SI0 / 1000, ',', CH40, ',', SAL, ',&
           &', SOD, ',', JNH4 / 1000, ',', JNO3 / 1000, ',', BENDEN &
           & (1), ',', JCH4, ',', JCH4G, ',', JHS, ',', JPO4 / 1000, ',&
           &', JSI / 1000, ',', 0.0, ',', 0.0, ',', 0.0, ',', NH41 / &
           & 1000, ',', NH42 / 1000, ',', NO31 / 1000, ',', NO32 / &
           & 1000, ',', PO41 / 1000, ',', PO42 / 1000, ',', SI1 / 1000, &
           & ',', SI2 / 1000, ',', CH41, ',', CH42, ',', HS1, ',', HS2, &
           & ',', POC1 / 1000 * 2.667, ',', POC2 / 1000 * 2.667, ',', &
           & POC3 / 1000 * 2.667, ',', PON1 / 1000, ',', PON2 / 1000, '&
           &,', PON3 / 1000, ',', POP1 / 1000, ',', POP2 / 1000, ',', &
           & POP3 / 1000, ',', PSISED / 1000, ',', HSED1 (I) * 1000.0, &
           & ',', BENSTR !'BEN_STR' 			dimensionless		dimensionless
        ! WRITE(BFO,1000)I,',',JDAY,			&
        !	',',(JPOC(I,1)+JPOC(I,2)+JPOC(I,3))/1000.0*2.667,	&	!'Jcin'				mgC/m^2/d			gO2/m^2/d
        !	',',(JPON(I,1)+JPON(I,2)+JPON(I,3))/1000.0,	&			!'Jnin'				*mgN/m^2/d			gN/m^2/d
        !	',',(JPOP(I,1)+JPOP(I,2)+JPOP(I,3))/1000.0,	&			!'Jpin'				*mgP/m^2/d			gP/m^2/d
        !	',',JPOS(I)/1000.0,	&									!'Jsin		 		*mgSi/m^2/d			gSi/m^2/d
        !	',',O20,			&									!'O20'				mgO2/L				mgO2/L
        !	',',D(I),			&									!'Depth'			m					m
        !	',',T(I,KBM1),		&									!'Tw'				degC				degC
        !	',',NH40/1000,		&									!'NH30'				*mgN/m^3			mgN/L
        !	',',NO30/1000,		&									!'NO30'				*mgN/m^3			mgN/L
        !	',',PO40/1000,		&									!'PO40'				*mgP/m^3			mgP/L
        !	',',SI0/1000,		&									!'SI0'				*mgSi/m^3 			mgSi/L				(dissiolved SIAT)
        !	',',CH40,			&									!'CH40'				gO2/m^3				mgO2/L
        !	',',SAL,			&									!'SALw'  			ppt					ppt					!i.e. SALT(I,KWC), ppt
        !	',',SOD,			&									!'SOD'				gO2/m^2/day			gO2/m^2/d
        !	',',JNH4/1000,		&									!'Jnh4				*mgN/m^2/day		gN/m^2/d
        !	',',JNO3/1000,		&									!'Jno3				*mgN/m^2/day		gN/m^2/d
        !	',',BENDEN(1),		&									!'JDenitT'			gN/m^2/day			gN/m^2/d
        !							!JDenit(1) = Denit(1) * NO3(1)
        !							!JDenit(2) = Denit(2) * NO3(2)
        !							!JDenitT = JDenit(1) + JDenit(2)
        !	',',JCH4,			&									!'Jch4'				gO2/m^2/day			gO2/m^2/d
        !	',',JCH4G,			&									!'Jch4g'			gO2/m^2/day			gO2/m^2/d
        !	',',JHS,			&									!'Jhs'				gO2/m^2/day			gO2/m^2/d
        !	',',JPO4/1000,		&									!'Jpo4'				*mgP/m^2/day		gP/m^2/d
        !	',',JSI/1000,		&									!'Jsi'				*mgSi/m^2/day		gSi/m^2/d
        !	',',0.0,			&									!'Jcout'
        !	',',0.0,			&									!'Jnout'
        !	',',0.0,			&									!'Jpout'
        !         !
        ! WLong: Excel calculations here, should correspond to Jcin, Jnin and Jpin
        ! Jcout = SOD + JCH4 + JCH4g + JHS +  	(POC2(1) + POC2(2) + POC2(3)) * w2 +
        !						(		(POC2(1) + POC2(2) + POC2(3))
        !							- 	(POC22(1) + POC22(2) + POC22(3))
        !						) * H2 / tc
        ! Jnout = JNO3 + JNH4 + JDenitT + 		(PON2(1) + PON2(2) + PON2(3)) * w2 +
        !						(		(PON2(1) + PON2(2) + PON2(3))
        !							- 	(PON22(1) + PON22(2) + PON22(3))
        !						) * H2 / tc
        ! Jpout = JPO4 + 						(POP2(1) + POP2(2) + POP2(3)) * w2 +
        !						((POP2(1) + POP2(2) + POP2(3))
        !						 - (POP22(1) + POP22(2) + POP22(3))
        !						 ) * H2 / tc
        !
        !	',',NH41/1000,		&					!'NH3(1)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',NH42/1000,		&					!'NH3(2)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',NO31/1000,		&					!'NO3(1)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',NO32/1000,		&					!'NO3(2)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',PO41/1000,		&					!'PO4(1)'			*mgP/m^3 			mgP/L	(dissolved)
        !	',',PO42/1000,		&					!'PO4(2)'			*mgP/m^3 			mgP/L	(dissolved)
        !	',',SI1/1000,		&					!'Si(1)'			*mgSi/m^3 			mgSi/L	(dissolved)
        !	',',SI2/1000,		&					!'Si(2)'			*mgSi/m^3 			mgSi/L	(dissolved)
        !	',',CH41,			&					!'CH4(1)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',CH42,			&					!'CH4(2)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',HS1,			&					!'HS(1)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',HS2,			&					!'HS(2)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',POC1/1000*2.667,&					!'POC2(1)'			*mgC/m^3 			gO2/m^3	(G1 of POC in layer 2)
        !	',',POC2/1000*2.667,&					!'POC2(2)' 			*mgC/m^3 			gO2/m^3	(G2 of POC in layer 2)
        !	',',POC3/1000*2.667,&					!'POC2(3)' 			*mgC/m^3 			gO2/m^3	(G3 of POC in layer 2)
        !	',',PON1/1000,		&					!'PON2(1)'			*mgN/m^3 			gN/m^3	(G1 of PON in layer 2)
        !	',',PON2/1000,		&					!'PON2(2)' 			*mgN/m^3 			gN/m^3	(G2 of PON in layer 2)
        !	',',PON3/1000,		&					!'PON2(3)' 			*mgN/m^3 			gN/m^3	(G3 of PON in layer 2)
        !	',',POP1/1000,		&					!'POP2(1)'			*mgP/m^3 			gP/m^3	(G1 of POP in layer 2)
        !	',',POP2/1000,		&					!'POP2(2)'			*mgP/m^3 			gP/m^3	(G2 of POP in layer 2)
        !	',',POP3/1000,		&					!'POP2(3)' 			*mgP/m^3 			gP/m^3	(G3 of POP in layer 2)
        !	',',PSISED/1000,	&					!'POS2'				*mgSi/m^3 			gSi/m^3	(particulate biogenic Silicate in layer2)
        !	',',HSED1(I)*1000.0,&					!'H1'				m					mm
        !	',',BENSTR			 					!'BEN_STR' 			dimensionless		dimensionless
        !
            Write (*,*) 1, ',', JDAY
            Write (*,*) 'JPOC=,', (JPOC(I, 1)+JPOC(I, 2)+JPOC(I, 3)) / &
           & 1000.0 * 2.667, 'gO2/m^2/d'
        !'Jcin'				mgC/m^2/d			gO2/m^2/d
!
        !',WS1NET(1)=',WS1NET(I), &!
             !',WB1NETMMD=',WB1NETMMD, 					&	!
        !',B1=',B1(I,KWC),&!
        !',FRACALG1(1)=',FRCALG1(1),&!
        !'JPOC(I,1)=,',JPOC(I,1)/1000*2.667, &	!						*mgC/m^2/d			gO2/m^2/d
        !'JPON(I,1)=,',JPON(I,1)/1000.0, 	&	!						*mgN/m^2/d			gN/m^2/d
        !'JPOP(I,1)=,',JPOP(I,1)/1000.0, 	&	!						*mgP/m^2/d			gP/m^2/d
        !'JPOS(I)  =,',JPOS(I)/1000.0,		&	!						*mgSi/m^2/d			gSi/m^2/d
!
            Write (*,*) 'JPON=,', (JPON(I, 1)+JPON(I, 2)+JPON(I, 3)) / &
           & 1000.0, 'gN/m^2/d'
        !'Jnin'				*mgN/m^2/d			gN/m^2/d
            Write (*,*) 'JPOP=,', (JPOP(I, 1)+JPOP(I, 2)+JPOP(I, 3)) / &
           & 1000.0, 'gP/m^2/d'
        !'Jpin'				*mgP/m^2/d			gP/m^2/d
            Write (*,*) 'JPOS=,', JPOS (I) / 1000.0, 'gSi/m^2/d'
        !'Jsin		 		*mgSi/m^2/d			gSi/m^2/d
            Write (*,*)
            Write (*,*) 'O20=,', O20 !,			&					!'O20'				mgO2/L				mgO2/L
            Write (*,*) 'D=,', D (I)!,			&					!'Depth'			m					m
            Write (*,*) 'T=,', T (I, KBM1)!,		&					!'Tw'				degC				degC
            Write (*,*) 'NH40=,', NH40 / 1000 !,	&					!'NH30'				*mgN/m^3			mgN/L
            Write (*,*) 'NO30=,', NO30 / 1000 !,	&					!'NO30'				*mgN/m^3			mgN/L
            Write (*,*) 'PO40=,', PO40 / 1000 !,	&					!'PO40'				*mgP/m^3			mgP/L
            Write (*,*) 'SI0=,', SI0 / 1000 !,		&					!'SI0'				*mgSi/m^3 			mgSi/L				(dissiolved SIAT)
            Write (*,*) 'CH40=,', CH40 !,			&					!'CH40'				gO2/m^3				mgO2/L
            Write (*,*) 'SAL=,', SAL !,			&					!'SALw'  			ppt					ppt					!i.e. SALT(I,KWC), ppt
            Write (*,*) 'SOD=,', SOD !,			&					!'SOD'				gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JNH4=,', JNH4 / 1000 !,	&					!'Jnh4				*mgN/m^2/day		gN/m^2/d
            Write (*,*) 'JNO3=,', JNO3 / 1000 !,	&					!'Jno3				*mgN/m^2/day		gN/m^2/d
            Write (*,*) 'JDeniT=,', BENDEN (1)!,	&					!'JDenitT'			gN/m^2/day			gN/m^2/d
        !JDenit(1) = Denit(1) * NO3(1)
        !JDenit(2) = Denit(2) * NO3(2)
        !JDenitT = JDenit(1) + JDenit(2)
            Write (*,*) 'JCH4=,', JCH4 !,			&					!'Jch4'				gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JCH4G=,', JCH4G !,		&					!'Jch4g'			gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JHS=,', JHS !,			&					!'Jhs'				gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JPO4=,', JPO4 / 1000 !,	&					!'Jpo4'				*mgP/m^2/day		gP/m^2/d
            Write (*,*) 'JSI=,', JSI / 1000 !,		&					!'Jsi'				*mgSi/m^2/day		gSi/m^2/d
!
        !						',',0.0,				&					!'Jcout'
        !						',',0.0,				&					!'Jnout'
        !						',',0.0,				&					!'Jpout'
!
        !
        !WLong: Excel calculations here, should correspond to Jcin, Jnin and Jpin
        !Jcout = SOD + JCH4 + JCH4g + JHS +  	(POC2(1) + POC2(2) + POC2(3)) * w2 +
        !										(		(POC2(1) + POC2(2) + POC2(3))
        !											- 	(POC22(1) + POC22(2) + POC22(3))
        !										) * H2 / tc
        !Jnout = JNO3 + JNH4 + JDenitT + 		(PON2(1) + PON2(2) + PON2(3)) * w2 +
        !										(		(PON2(1) + PON2(2) + PON2(3))
        !											- 	(PON22(1) + PON22(2) + PON22(3))
        !										) * H2 / tc
        !Jpout = JPO4 + 						(POP2(1) + POP2(2) + POP2(3)) * w2 +
        !										((POP2(1) + POP2(2) + POP2(3))
        !										 - (POP22(1) + POP22(2) + POP22(3))
        !										 ) * H2 / tc
        !
            Write (*,*) 'NH41=,', NH41 / 1000 !,		&					!'NH3(1)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'NH42=,', NH42 / 1000 !,		&					!'NH3(2)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'NO31=,', NO31 / 1000 !,		&					!'NO3(1)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'NO32=,', NO32 / 1000 !,		&					!'NO3(2)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'PO41=,', PO41 / 1000 !,		&					!'PO4(1)'			*mgP/m^3 			mgP/L	(dissolved)
            Write (*,*) 'PO42=,', PO42 / 1000 !,		&					!'PO4(2)'			*mgP/m^3 			mgP/L	(dissolved)
            Write (*,*) 'SI1=,', SI1 / 1000 !,			&					!'Si(1)'			*mgSi/m^3 			mgSi/L	(dissolved)
            Write (*,*) 'SI2=,', SI2 / 1000 !,			&					!'Si(2)'			*mgSi/m^3 			mgSi/L	(dissolved)
            Write (*,*) 'CH41=,', CH41 !,				&					!'CH4(1)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'CH42=,', CH42 !,				&					!'CH4(2)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'SO41=,', SO41 !,				&					!'SO4(1)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'SO42=,', SO42 !,				&					!'SO4(2)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'HSO4=,', HSO4 !,				&					!'HSO4'				m	 				m
            Write (*,*) 'HS1=,', HS1 !,				&					!'HS(1)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'HS2=,', HS2 !,				&					!'HS(2)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'POC1=,', POC1 / 1000 * 2.667 !,	&					!'POC2(1)'			*mgC/m^3 			gO2/m^3	(G1 of POC in layer 2)
            Write (*,*) 'POC2=,', POC2 / 1000 * 2.667 !,	&					!'POC2(2)' 			*mgC/m^3 			gO2/m^3	(G2 of POC in layer 2)
            Write (*,*) 'POC3=,', POC3 / 1000 * 2.667 !,	&					!'POC2(3)' 			*mgC/m^3 			gO2/m^3	(G3 of POC in layer 2)
            Write (*,*) 'PON1=,', PON1 / 1000 !,		&					!'PON2(1)'			*mgN/m^3 			gN/m^3	(G1 of PON in layer 2)
            Write (*,*) 'PON2=,', PON2 / 1000 !,		&					!'PON2(2)' 			*mgN/m^3 			gN/m^3	(G2 of PON in layer 2)
            Write (*,*) 'PON3=,', PON3 / 1000 !,		&					!'PON2(3)' 			*mgN/m^3 			gN/m^3	(G3 of PON in layer 2)
            Write (*,*) 'POP1=,', POP1 / 1000 !,		&					!'POP2(1)'			*mgP/m^3 			gP/m^3	(G1 of POP in layer 2)
            Write (*,*) 'POP2=,', POP2 / 1000 !,		&					!'POP2(2)'			*mgP/m^3 			gP/m^3	(G2 of POP in layer 2)
            Write (*,*) 'POP3=,', POP3 / 1000 !,		&					!'POP2(3)' 			*mgP/m^3 			gP/m^3	(G3 of POP in layer 2)
            Write (*,*) 'PSISED=,', PSISED / 1000 !,	&					!'POS2'				*mgSi/m^3 			gSi/m^3	(particulate biogenic Silicate in layer2)
            Write (*,*) 'CTEMP=', CTEMP (I)
            Write (*,*) 'H1=,', HSED1 (I) * 1000 !,			&					!'H1'				m					mm
            Write (*,*) 'BENSTR=,', BENSTR !'BEN_STR' 			dimensionless		dimensionless
        !
        !		READ(*,*)
         End If
!
         IINT = IINT + 1
         ELTMS = ELTMS + DLT
         JDAY = ELTMS / 86400.
         ELTMJD = JDAY - TMSTRT
     !
         If (JDAY .Ge. TMEND) END_RUN = .True.
!
      End Do
  !
  !
  !close files
  !
  !
      Close (BFI)
      Close (BFO)
  !
      If (ITVWCLF == 1) Then
         Close (WCL)
      End If
  !
  !
  !deallocate sediment module variables
  !
  !
      Call SED_DEALLOC
  !
  !
  !deallocate sav module variables
  !
      If (SAV_CALC) Then
         Call SAV_DEALLOC
         Call SED_SAV_EXCHANGE_DEALLOC
      End If
!
  !
  !deallocate df module variables
  !
      If (DFEEDER) Then
         Call DF_DEALLOC
         Call SED_DF_EXCHANGE_DEALLOC
      End If
  !
  !deallocate sf module variables
  !
  !
      If (SFEEDER) Call SF_DEALLOC
  !
  !deallocate ba module variables
  !
      If (BALGAE_CALC) Call BA_DEALLOC
      Stop 'Finished test2e!'
#endif
  !
!
#if defined (test2f)
  !
  !test corresponding to SedFlux_ver17b14_Boulder.xlsm
!
  !e. Time-variable solution using initial conditions computed assuming steady state with assumed time-variable deposition of POM and time-variable overlying water quality
  !
  !
  !initialize the variables used in the sediment module in the upper
  !modules
  !
  !
  !disable SAV, BA, DF, SF
      SAV_CALC = .False.
      BALGAE_CALC = .False.
      DFEEDER = .False.
      SFEEDER = .False.
!
      BENTHIC_OUTPUT = .True.
!
  !serial run on master processor
      SERIAL = .True.
      PAR = .False.
      MSR = .True.
!
  !
  !set dimension limits
  !
!
      MLOC = 1 !one node
      MTLOC = 1 !one local node
      KBM1 = 10 !10 vertical water column layers
      MGL = 1 !one node
      KWC = KBM1 !overlying water column layer
!
  !
  !allocate hydrodynamic variables (no velocities needed, only depth and  box area)
  !
  !
  !		ART1	&!AREA OF NODE-BASE CONTROl VOLUME
  !		,DZ		&!DELTA-SIGMA VALUE of each layer
  !		,D		 !CURRENT DEPTH
!
  !WLong moved from cell_area.F to here
  !
      Allocate (ART1(0:MTLOC))
      ART1 = 0.0_SP !!AREA OF NODE-BASE CONTROl VOLUME
      Allocate (DZ(KBM1+1))
      DZ = 0.0_SP !!DELTA-SIGMA VALUE
      Allocate (D(0:MTLOC))
      D = 0.0
!
      ART1 = 100.d0 !100m^2
      D = 2.0 !2m deep
      DZ = 1.0 / KBM1 !0.1 as KBM1=10  !sigma layer
!
  !
  !allocate water column water quality variables
  !
!
      SETTLING = .True.
!
  !settling rates
      Allocate (WSSNET(MTLOC))
      WSSNET = 0.0 !suspended sediments settling rate (m/d)
      Allocate (WSLNET(MTLOC))
      WSLNET = 0.0 !LPOM (labile detritus) settling rate (m/d)
      Allocate (WSRNET(MTLOC))
      WSRNET = 0.0 !RPOM (refractory detriuts) settling rate (m/d)
      Allocate (WS1NET(MTLOC))
      WS1NET = 0.0 !alg 1 settling rate (m/d)
      Allocate (WS2NET(MTLOC))
      WS2NET = 0.0 !alg 2 settling rate (m/d)
      Allocate (WS3NET(MTLOC))
      WS3NET = 0.0 !alg 3 settling rate (m/d)
      Allocate (WSUNET(MTLOC))
      WSUNET = 0.0 !particulate biogenic silica settling rate (m/d)
  !
  !water column constituents
      Allocate (T(0:MTLOC, KBM1))
      T = 0.0 !1
      Allocate (SALT(0:MTLOC, KBM1))
      SALT = 0.0 !2
      Allocate (SSI(0:MTLOC, KBM1))
      SSI = 0.0 !3
      Allocate (B1(0:MTLOC, KBM1))
      B1 = 0.0 !4
      Allocate (B2(0:MTLOC, KBM1))
      B2 = 0.0 !5
      Allocate (B3(0:MTLOC, KBM1))
      B3 = 0.0 !6
      Allocate (LPOC(0:MTLOC, KBM1))
      LPOC = 0.0 !11
      Allocate (RPOC(0:MTLOC, KBM1))
      RPOC = 0.0 !12
      Allocate (NH4(0:MTLOC, KBM1))
      NH4 = 0.0 !13
      Allocate (NO3(0:MTLOC, KBM1))
      NO3 = 0.0 !14
      Allocate (LPON(0:MTLOC, KBM1))
      LPON = 0.0 !18
      Allocate (RPON(0:MTLOC, KBM1))
      RPON = 0.0 !19
      Allocate (PO4(0:MTLOC, KBM1))
      PO4 = 0.0 !20
      Allocate (LPOP(0:MTLOC, KBM1))
      LPOP = 0.0 !23
      Allocate (RPOP(0:MTLOC, KBM1))
      RPOP = 0.0 !24
      Allocate (PIP(0:MTLOC, KBM1))
      PIP = 0.0 !25
      Allocate (COD(0:MTLOC, KBM1))
      COD = 0.0 !26
      Allocate (DOXG(0:MTLOC, KBM1))
      DOXG = 0.0 !27
      Allocate (SIUPB(0:MTLOC, KBM1))
      SIUPB = 0.0 !28
      Allocate (SIAT(0:MTLOC, KBM1))
      SIAT = 0.0 !29
  !
  !sediment temperature
      Allocate (CTEMP(MTLOC))
      CTEMP = 0.0
!
  !P to C ratio (gP/gC) of alg 1 2 and 3
      Allocate (Q1(MTLOC, KBM1))
      Q1 = 0.0
      Allocate (Q2(MTLOC, KBM1))
      Q2 = 0.0
      Allocate (Q3(MTLOC, KBM1))
      Q3 = 0.0
!
  !particle (C, N, P, Si, Suspedned Solids) fluxes
!
      Allocate (PCFWS(MTLOC))
      PCFWS = 0.0
      Allocate (PNFWS(MTLOC))
      PNFWS = 0.0
      Allocate (PPFWS(MTLOC))
      PPFWS = 0.0
      Allocate (PSFWS(MTLOC))
      PSFWS = 0.0
      Allocate (SSFWS(MTLOC))
      SSFWS = 0.0 !this is done in mod_wqm.F
  !
  !benthic fluxes to water column
!
      Allocate (BENCH4A(MTLOC))
      BENCH4A = 0.0
      Allocate (BENCH4G(MTLOC))
      BENCH4G = 0.0
      Allocate (BENCOD(MTLOC))
      BENCOD = 0.0
      Allocate (BENDEN(MTLOC))
      BENDEN = 0.0
      Allocate (BENDO(MTLOC))
      BENDO = 0.0
      Allocate (BENDOC(MTLOC))
      BENDOC = 0.0
      Allocate (BENNH4(MTLOC))
      BENNH4 = 0.0
      Allocate (BENNO3(MTLOC))
      BENNO3 = 0.0
      Allocate (BENPO4(MTLOC))
      BENPO4 = 0.0
      Allocate (BENSA(MTLOC))
      BENSA = 0.0
  !
  !
  !allocate sediment module variables
  !
!
      Call SED_INIT !initialize sediment consituent names and allocate variables for sediment module
!
  !
  !allocate sav module variables
  !
      If (SAV_CALC) Then
         Call SAV_ALLOC
         Call SED_SAV_EXCHANGE_ALLOC
      End If
!
  !
  !allocate df module variables
  !
      If (DFEEDER) Then
         Call DF_ALLOC
         Call SED_DF_EXCHANGE_ALLOC
      End If
  !
  !
  !allocate sf module variables
  !
  !
      If (SFEEDER) Call SF_ALLOC
  !
  !allocate ba module variables
  !
      If (BALGAE_CALC) Call BA_ALLOC
  !
!
  !
  !input and output files for the sediment module
  !
!
!
  !file handles
      BFI = 1
      BFO = 12
      WCL = 13
!
  !file names
      BFIFN (1) = 'sedtest_2f.inp'
      BFOFN = 'sedtest_2f.chk'
  !
  !
  !set sediment control parameters or read the sediment module control
  !parameters from file BFI
  !
!
      Open (BFI, File=BFIFN(1), Status='OLD')
  !
      Call SED_READ !read some of the basic variables but it will be over written again afterwards
!
!
      SSTATEIC = 1 !set SSTATEIC=1, so we are going to use steady sate calculation to setup initial conditions
  !and overwrite initial conditions
      STEADY_STATE_SED_IC = SSTATEIC == 1
!
  !particle (C, N, P, Si, Suspedned Solids) fluxes
!
      PCFWS = 0.0
      PNFWS = 0.0
      PPFWS = 0.0
      PSFWS = 0.0
      SSFWS = 0.0
  !
  !
  !driving POC, PON, POP, POS, SSI etc fluxes from water colum that should match Jcin, Jnin, Jpin, Jsin,
  !as well as overlying water concentrations
  !
!
  !	SegmentID	Date+time			Jcin		Jnin		Jpin		Jsin		O20			Depth	Tw		NH30	NO30	PO40	SI0		CH40	SALw
  !     	     						(gO2/m^2/d)	(gN/m^2/d)	(gP/m^2/d)	(gSi/m^2/d)	(mgO2/L)	(m)		(deg C)	(mgN/L)	(mgN/L)	(mgP/L)	(mgSi/L)		(mgO2/L)	(ppt)
  !		1		1/1/00 12:00 AM		0.300000	0.005000	0.003000	0.009375	5.0			2.00	15.0	0.015	0.100	0.004	0.216	0.0		30.0
  !
  !
  !Set water coumn variables for sediment diagenesis run
  !
  !	Fortran													Excel
  !------------------------------------------------------------------------------------
      T = 14.9 !1  	degC						15				degC
      CTEMPI = 14.9 !degC
      SALT = 0.165394891 !2		ppt							30				ppt
      SSI = 0.0 !3		gSolids/m^3					-
      B1 = 1.0 !4		gC/m^3						-
      B2 = 0.0 !5		gC/m^3						-
      B3 = 0.0 !6		gC/m^3						-
      LPOC = 0.0 !11		gC/m^3						-
      RPOC = 0.0 !12		gC/m^3						-
      NH4 = 0.0875929089 !13		gN/m^3						0.015			mgN/L
      NO3 = 0.165555544 !14		gN/m^3						0.1				mgN/L
      LPON = 0.0 !18		gN/m^3						-
      RPON = 0.0 !19		gN/m^3						-
      PO4 = 0.073463985 !20		gP/m^3						0.004			mgP/L
      LPOP = 0.0 !23		gP/m^3						-
      RPOP = 0.0 !24		gP/m^3						-
      PIP = 0.0 !25		gP/m^3						-
      COD = 0.0 !26		gO2/m^3						-
      DOXG = 6.980586 !27		gO2/m^3						5				mgO2/L
      SIUPB = 0.0 !28		gSi/m^3						0.0				mgSi/L (i.e. gSi/m^3)
      SIAT = 0.0 !29		gSi/m^3						0.216			mgSi/L (i.e. gSi/m^3)
!
!
  !
  !parameters related to water colum
  !
  !
  !read from MRL (adsorption coefficients of PO4 and Silicate
      KADPO4 = 0.0004 !m^3/gSolids		!READ (MRL,1080)  KADPO4, KADSA
!
  !basically PIE1, see WLong notes (4-7-5) =PIE2PO4*DPIE2PO4=100*300 L/kg
  !=30000L/kg = 30m^3/kg=30/1000 (m^3/g) = 0.3 m^3/g
!
  !=20 L/kg *20 = 400L/kg = 0.4m^3/kg = 0.4/1000 (m^3/g) 0.0004(m^3/kg)
!
      KADSA = 0.001 !m^3/g	!basically PIE2, see WLong notes (4-6-7-1) where
  !m2 is SSI (gSolids/m^3), pie2= should have unit m^3/gSolids
  !i.e. 100L/kg ~ 0.1m^3/kg ~ 0.1m^3/(1000g) = 0.0001 m^3/gSi
  !here it is used to calculate equilibrium speciation of dissolved Si
  !and particulate Si based on suspended solids concentration (gSolids/m^3)
  !in water column
  !
  !Relation between m^3/g and 100L/kg :	0.0001m^3/g = 0.1m^3/kg = 100L/(1kg) = 100L/kg
  !
  !
  !for water column, there is plenty of oxygen, according to WLong notes (4-6-5), we should use PIE1,Si
  !which is 100L/kg * DPIE1SI = 1000L/kg = 1m^3/1000g = 0.001m^3/g
  !
  !
  !	POM flux				Fortran	Formula							Excel						Excel Formula
  !-------------------------------------------------------------------------------------------------------------------
  !	Jcin 			1000*0.3/2.667=112.48		mgC/m^2/d			0.3		  gO2/m^2/d			JPOC(i) = Jcin*fpoc(i)
!
  !JPOC(I,1) = WB1NETMMD*FRCALG1(1)*B1(I,KWC)+										frpoc1=0.65
  !			WB2NETMMD*FRCALG2(1)*B2(I,KWC)+
  !			WB3NETMMD*FRCALG3(1)*B3(I,KWC)+
  !			WLPOMNETMMD*LPOC(I,KWC)
!
  !JPOC(I,2) = WB1NETMMD*FRCALG1(2)*B1(I,KWC)+        								frpoc2=0.2
  !			WB2NETMMD*FRCALG2(2)*B2(I,KWC)+
  !			WB3NETMMD*FRCALG3(2)*B3(I,KWC)+
  !			WRPOMNETMMD*RPOC(I,KWC)*FRPOC(I,2)/
  !			(FRPOC(I,2)+FRPOC(I,3))
!
  !JPOC(I,3) = WB1NETMMD*FRCALG1(3)*B1(I,KWC)+ 										frpoc3=0.15
  !			WB2NETMMD*FRCALG2(3)*B2(I,KWC)+
  !			WB3NETMMD*FRCALG3(3)*B3(I,KWC)+
  !			WRPOMNETMMD*RPOC(I,KWC)*FRPOC(I,3)/
  !				(FRPOC(I,2)+FRPOC(I,3))
!
  !	Jnin 			ANC*1000*0.005/2.667/FRNALG1(1)	mgN/m^2/d		0.005   gN/m^2/d			JPON(i) = Jnin * fpon(i)
  !JPON(I,1) = WB1NETMMD*ANC1*FRNALG1(1)*B1(I,KWC)+									frpon1=0.65
  !			WB2NETMMD*ANC2*FRNALG2(1)*B2(I,KWC)+
  !			WB3NETMMD*ANC3*FRNALG3(1)*B3(I,KWC)+
  !			WLPOMNETMMD*LPON(I,KWC)
  !
  !JPON(I,2) = WB1NETMMD*ANC1*FRNALG1(2)*B1(I,KWC)									frpon2=0.25
  !			+WB2NETMMD*ANC2*FRNALG2(2)*B2(I,KWC)
  !			+WB3NETMMD*ANC3*FRNALG3(2)*B3(I,KWC)
  !			+WRPOMNETMMD*RPON(I,KWC)*FRPON(I,2)/
  !			(FRPON(I,2)+FRPON(I,3))
  !
  !JPON(I,3) = WB1NETMMD*ANC1*FRNALG1(3)*B1(I,KWC) 									frpon3=0.1
  !			+WB2NETMMD*ANC2*FRNALG2(3)*B2(I,KWC)
  !			+WB3NETMMD*ANC3*FRNALG3(3)*B3(I,KWC)
  !			+WRPOMNETMMD*RPON(I,KWC)*FRPON(I,3)/
  !			(FRPON(I,2)+FRPON(I,3))
!
  !	Jpin 			Q*1000*0.003/2.667	mgP/m^2/d					0.003	gP/m^2/d			JPOP(i) = Jpin * fpop(i)
  !JPOP(I,1) = WB1NETMMD*Q1(I,KWC)*FRPALG1(1)*B1(I,KWC)+								frpop1=0.65
  !			WB2NETMMD*Q2(I,KWC)*FRPALG2(1)*B2(I,KWC)+
  !			WB3NETMMD*Q3(I,KWC)*FRPALG3(1)*B3(I,KWC)
  !			WLPOMNETMMD*LPOP(I,KWC)
!
  !JPOP(I,2) = WB1NETMMD*Q1(I,KWC)*FRPALG1(2)*B1(I,KWC)+								frpop2=0.2
  !			WB2NETMMD*Q2(I,KWC)*FRPALG2(2)*B2(I,KWC)+
  !			WB3NETMMD*Q3(I,KWC)*FRPALG3(2)*B3(I,KWC)+
  !			WRPOMNETMMD*RPOP(I,KWC)*FRPOP(I,2)/
  !			(FRPOP(I,2)+FRPOP(I,3))
!
  !JPOP(I,3) = WB1NETMMD*Q1(I,KWC)*FRPALG1(3)*B1(I,KWC)+								frpop3=0.15
  !			+WB2NETMMD*Q2(I,KWC)*FRPALG2(3)*B2(I,KWC)+
  !			+WB3NETMMD*Q3(I,KWC)*FRPALG3(3)*B3(I,KWC)+
  !			+WRPOMNETMMD*RPOP(I,KWC)*FRPOP(I,3)/
  !			(FRPOP(I,2)+FRPOP(I,3))
!
  !	Jsin 			ASC*1000*0.009375/2.667	mgSi/m^2/d				0.009375 gSi/m^2/d			POS2 = (Jsin * tc / H2 + POS2) /
  !																										(1 + KSI * THTASI ^ (Tw - 20) * tc + w2 * tc / H2)
  !JPOS(I) = 	 WB1NETMMD*ASC1*B1(I,KWC)
  !		  	+WB2NETMMD*ASC2*B2(I,KWC)
  !			+WB3NETMMD*ASC3*B3(I,KWC)
  !			+WPSINETMMD*SIUPB(I,KWC)
!
  !
  !POC distribution into G1,G2,G3 by ALG 1,2,3
  !G1 	Alg1				Alg2				Alg3
      FRCALG1 (1) = 0.65
      FRCALG2 (1) = 0.65
      FRCALG3 (1) = 0.65
!
  !G2
      FRCALG1 (2) = 0.20
      FRCALG2 (2) = 0.20
      FRCALG3 (2) = 0.20
!
  !G3
      FRCALG1 (3) = 0.15
      FRCALG2 (3) = 0.15
      FRCALG3 (3) = 0.15
!
!
  !PON distribution into G1,G2,G3 by ALG 1,2,3
  !G1 	Alg1				Alg2				Alg3
      FRNALG1 (1) = 0.65
      FRNALG2 (1) = 0.65
      FRNALG3 (1) = 0.65
!
  !G2
      FRNALG1 (2) = 0.25
      FRNALG2 (2) = 0.25
      FRNALG3 (2) = 0.25
!
  !G3
      FRNALG1 (3) = 0.10
      FRNALG2 (3) = 0.10
      FRNALG3 (3) = 0.10
!
  !POP distribution into G1,G2,G3 by ALG 1,2,3
  !G1 	Alg1				Alg2				Alg3
      FRPALG1 (1) = 0.65
      FRPALG2 (1) = 0.65
      FRPALG3 (1) = 0.65
!
  !G2
      FRPALG1 (2) = 0.2
      FRPALG2 (2) = 0.2
      FRPALG3 (2) = 0.2
!
  !G3
      FRPALG1 (3) = 0.15
      FRPALG2 (3) = 0.15
      FRPALG3 (3) = 0.15
!
  !The following values are given in order to match Jcin, Jnin, Jpin,Jsin in Excel version
      B1 = 1.0
  !only Alg1 settling contributes to POM in sediments
      B2 = 0.0
      B3 = 0.0
  !all other particulate organics set to zero
      LPOC = 0.0
      RPOC = 0.0
      LPON = 0.0
      RPON = 0.0
      LPOP = 0.0
      RPOP = 0.0
      SIUPB = 0.0
!
  !Only settling rates of B1 matters for JPOC
      WB1NETMMD = (2.34315154/2.667) * 1000.0 / B1 (1, KWC)!	!here 2.34315154 is Jcin (gO2/m^2/d) in Excel version
      WB2NETMMD = 1000.0 !1000mm/d  	~1m/d
      WB3NETMMD = 1000.0 !1000mm/d	~1m/d
      WLPOMNETMMD = 1000.0 !1000mm/d	~1m/d
      WRPOMNETMMD = 1000.0 !1000mm/d  ~1m/d
      WPSINETMMD = 1000.0 !1000mm/d	~1m/d
!
  !reset WSLNET etc
      WSLBNET (1) = WLPOMNETMMD / 1000.
      WSRBNET (1) = WRPOMNETMMD / 1000.
      WS1BNET (1) = WB1NETMMD / 1000.0
      WS2BNET (1) = WB2NETMMD / 1000.0
      WS3BNET (1) = WB3NETMMD / 1000.0
      WSUBNET (1) = WPSINETMMD / 1000.0
      WSSBNET (1) = 1.0 !1m/d
!
  !ANC1, Q1, and ASC1 are adjusted to match Jnin, Jpin, Jsin based on Jcin
  !
  !redfield ratios of C,N,P for algae and detritus (modified to match Jcin, Jnin, Jpin, Jsin of Excel vresion)
  !
  !JPON=ANC1*WB1NETMMD*B1*FRNALG1(1)*2.667/1000=0.0518
  !
      ANC1 = (0.278038) * 1000.0 / B1 (1, KWC) / WB1NETMMD !==>1.4783 !here 0.278038 is Jnin (gN/m^2/d) in Excel version
  !ANC1    =	0.175 	!gN/gC
      ANC2 = 0.175
      ANC3 = 0.175
!
  !P to C ratio (gP/gC) of alg 1 2 and 3 (from AGR file)
      Q1 = (0.014202) * 1000.0 / B1 (1, KWC) / WB1NETMMD !==>0.0214	!here 0.014202 is Jpin (gP/m^2/d) in Excel version
  !Q1    	= 1/41.1		!see DMD book page 10 Table 1.1
      Q2 = 1 / 41.1 !see DMD book page 10 Table 1.1
      Q3 = 1 / 41.1 !see DMD book page 10 Table 1.1
  !
      ASC1 = (0.000000) * 1000.0 / B1 (1, KWC) / WB1NETMMD !==>2.7719 	!here 0.000000 is Jsin in gSi/m^2/d in Excel version
  !ASC1	=	0.333 	!gSi/gC
      ASC2 = 0.333
      ASC3 = 0.333
  !
  !
  !set parameters related to SedFlux_ver17b12_test2f.xlsm
  !
!
  !the parameter lists in SedFlux_ver17b12_test2f.xlsm are listed as follows
  !-------------------------------------------------------------------------------------------------------------------------------------------------
  !parameter		parameter		parameter_meaning										unit			default_value		unit		equation
  !(excel)		(mod_sed.F)																(excel)				(excel)			(mod_sed)	(DMD book)
  !-------------------------------------------------------------------------------------------------------------------------------------------------
  !Particle mixing:
  !
  !
  !m1				M1				solids concentration in aerobic layer 1					kgD/L			0.5					kg/L
  !m2				M2				solids concentration in anaerobic layer 2				kgD/L			0.5					kg/L
  !Dp				DPP	(VPMIX)		bioturbation particle mixing coefficient				m^2/d			0.00006				m^2/d
  !Dd				DDP	(VDMIX)		pore water diffusion coefficient						m^2/d			0.0025				m^2/d
  !w2				W2 (VSED)		deep burial velocity									m/d				6.85E-06			cm/yr *
  !H2				H2(HSED,HSEDALL)thickness of sediment anaerobic layer 2					m				0.1					cm *		*input is cm
  !
      M1 = 0.5
      M2 = 0.5
      DPP = 0.00006
      DDP = 0.0025
      HSEDALL = 10.0 !cm
      Do I = 1, MLOC
         HSED (I) = HSEDALL * 0.01 !m
         VSED (I) = 6.85E-06 !m/d  (W2)
         VPMIX (I) = 0.00006 !m^2/d
         VDMIX (I) = 0.0025 !m^2/d
      End Do
  !
  !Reaction velocities (depth integrated reaction rate = reaction velocity (m/d))
  !
  !KappaNH3f		KAPPNH4F		freshwater nitrification velocity						m/d				0.131				m/d
  !KappaNH3s		KAPPNH4S		saltwater nitrification velocity						m/d				0.131				m/d
  !KappaNO3_1f	KAPPNO3F		freshwater denitrification velocity in layer 1			m/d				0.1					m/d
  !KappaNO3_1s	KAPPNO3S		saltwater denitrification velocity in layer 1			m/d				0.1					m/d
  !KappaNO3_2		K2NO3			denitrfication in the anaerobic layer 2					m/d				0.25				m/d
  !KappaCH4		KAPPCH4			methane oxidation in the aerobic layer 1				m/d				0.7					m/d
  !
      KAPPNH4F = 0.131 !m/d
      KAPPNH4S = 0.131 !m/d
      KAPPNO3F = 0.1 !m/d
      KAPPNO3S = 0.1 !m/d
      K2NO3 = 0.25 !m/d		!DMD P.105 Table 4.1
      KAPPCH4 = 0.7 !m/d
!
  !Half saturation constants
  !
  !KM_NH3			KMNH4			nitrification half saturation for NH4N					mgN/L			0.728				mgN/m^3	*
  !KM_O2_NH3		KMNH4O2			nitrification half saturation for O2					mgO2/L			0.37				mgO2/L
  !
      KMNH4 = 0.728 * 1000. !(mgN/m^3)		!see DMD book p73 Table 3.1 (b) (and Median)
  !maybe DMD book is wrong on units
      KMNH4O2 = 0.37 !mgO2/L			!See DMD book p73 Tabl3 3.1 (Median)
!
  !Partitioning coefficients
  !
  !KdNH3			PIENH4			partition coefficient for NH4 in layer 1 and 2			L/kgD			1					L/kg
  !KdPO42			PIE2PO4			partition coefficient for PO4 in layer 2				L/kgD			20					L/kg
  !dKDPO41f		DPIE1PO4F		freshwater factor that increases the aerobic 			unitless		20					L/kg
  !									layer partition coefficient of inorganic P
  !dKDPO41s		DPIE1PO4S		saltwater factor that increases the aerobic 			unitless		20					L/kg
  !									layer partition coefficient of inorganic P
  !O2critPO4		O2CRITPO4		critical O2 concentration in layer 1 for 				mgO2/L			2					mgO2/L
  !									adjustment of partition coefficient
  !									for inorganic P
  !
      PIENH4 = 1.0 !L/kg		(inversed of m1,m2 unit)
      PIE2PO4 = 20.0 !L/kg
      DPIE1PO4F = 20.0 !WLong?? Unitless?	Yes! Note DMD book pp 572 says it should be about 300
      DPIE1PO4S = 20.0 !WLong?? Unitless?
      O2CRITPO4 = 2.0 !mgO2/L
!
  !Temperature coefficients
  !
  !ThtaDp			THTADP			temperature theta for bioturbation mixing between 		unitless		1.117				unitless
  !									layers 1 and 2
  !ThtaDd			THTADD			temperature theta for pore water diffusion between 		unitless		1.08				unitless
  !									layers 1 and 2
  !ThtaKmNH3		THTAKMNH4		temperature theta for nitrification half saturation 	unitless		1.125				unitless
  !									for NH4
  !ThtaNH3		THTANH4			temperature theta for nitrification rate				unitless		1.123				unitless
  !ThtaNO3		THTANO3			temperature theta for denitrification rate				unitless		1.08				unitless
  !ThtaCH4		THTACH4			temperature theta for methane oxidation rate			unitless		1.079				unitless
  !
      THTADP = 1.117
      THTADD = 1.08
  !!ThtaKmNH3	=	!Applied to KM_NH3 in Excel, should be applied to KMNH4 in Fortran code
      THTAKMNH4 = 1.0
  !See DMD book p.73 Table 3.1 (median value is 1.125)
      THTANH4 = 1.123 !See DMD book p.73 Table 3.1 (median value is 1.123)
      THTANO3 = 1.08
      THTACH4 = 1.079
!
  !Salinity thresholds
  !
  !SALTSW			SALTSW			salinity above which sulfide rather than 				psu				1					ppt
  !									methane is produced from C diagenesis
  !SALTND			SALTND			salinity above which saltwater 							psu				1					ppt
  !									nitrification/denitrification rates are
  !									used for aerobic layer
      SALTSW = 0.0 !ppt
      SALTND = 1.0 !ppt
!
  !Sulfide constants
  !
  !KappaH2Sd1		KAPP1HSD		aerobic layer reaction velocity for dissolved 			m/d				0.2					m/d
  !								sulfide oxidation
  !KappaH2Sp1		KAPP1HSP		aerobic layer reaction velocity for particulate 		m/d				0.4					m/d
  !								sulfide oxidation
  !ThtaH2S		THTAH2S			temperature coefficient for sulfide oxidation			unitless		1.079				unitless
  !KMHSO2			KMHSO2			sulfide oxidation normalization constant for O2			mgO2/L			4					mgO2/L
  !KdH2S1			PIE1HS			partition coefficient for sulfide in aerobic layer 1	L/kgD			100					L/kg	*
  !KdH2S2			PIE2HS			partition coefficient for sulfide in anaerobic layer 2	L/kgD			100					L/kg	*
  !
      KAPP1HSD = 0.2 !m/d
      KAPP1HSP = 0.4 !m/d
      THTAH2S = 1.079 !unitless
      KMHSO2 = 4.0 !mgO2/L
      PIE1HS = 100 !L/kg
      PIE2HS = 100 !L/kg
  !
!
  !Fractions of G classes 1 and 2 for settling RPON, RPOC, and RPOP
  !
  !frpon1			FRPON(:,1)		fraction of class 1 pon									unitless		0.65				unitless * only for detritus
  !frpon2			FRPON(:,2)		fraction of class 2 pon									unitless		0.25				unitless * only for detritus
!
  !frpoc1			FRPOC(:,1)		fraction of class 1 poc									unitless		0.65				unitless * only for detritus
  !frpoc2			FRPOC(:,2)		fraction of class 2 poc									unitless		0.2					unitless * only for detritus
!
  !frpop1			FRPOP(:,1)		fraction of class 1 pop									unitless		0.65				unitless * only for detritus
  !frpop2			FRPOP(:,2)		fraction of class 2 pop									unitless		0.2					unitless * only for detritus
!
  !*******WLong: Fortran code used this completely differently!!!!*****
  !Fortran code uses these as fraction of RPOM to be split in G2 and G3
  !G1 has no share of RPOM
!
      Do I = 1, MLOC
         FRPON (I, 1) = 0.0 !No share for G1
         FRPON (I, 2) = 0.65
         FRPON (I, 3) = 1 - FRPON (I, 2) - FRPON (I, 1)
!
         FRPOC (I, 1) = 0.0 !No share for G1
         FRPOC (I, 2) = 0.65
         FRPOC (I, 3) = 1 - FRPOC (I, 1) - FRPOC (I, 2)
!
         FRPOP (I, 1) = 0.0 !No share for G1
         FRPOP (I, 2) = 0.65
         FRPOP (I, 3) = 1 - FRPOP (I, 1) - FRPOP (I, 2)
     !
      End Do
!
  !Diagenesis rate constants for G clase 1, 2, and 3 N/C/P
  !
  !kpon1			KPON1			G class 1 pon mineralization							day^-1			0.035				1/d
  !kpon2			KPON2			G class 2 pon mineralization							day^-1			0.0018				1/d
  !kpon3			KPON3			G class 3 pon mineralization							day^-1			0					1/d
  !kpoc1			KPOC1			G class 1 poc mineralization							day^-1			0.035				1/d
  !kpoc2			KPOC2			G class 2 poc mineralization							day^-1			0.0018				1/d
  !kpoc3			KPOC3			G class 3 poc mineralization							day^-1			0					1/d
  !kpop1			KPOP1			G class 1 pop mineralization							day^-1			0.035				1/d
  !kpop2			KPOP2			G class 2 pop mineralization							day^-1			0.0018				1/d
  !kpop3			KPOP3			G class 3 pop mineralization							day^-1			0					1/d
  !
      KPON1 = 0.035 !1/d
      KPON2 = 0.0018 !1/d
      KPON3 = 0. !1/d
!
      KPOC1 = 0.035 !1/d
      KPOC2 = 0.0018 !1/d
      KPOC3 = 0. !1/d
!
      KPOP1 = 0.035 !1/d
      KPOP2 = 0.0018 !1/d
      KPOP3 = 0. !1/d
  !
  !Temperature coefficients for G class 1, 2, and 3 mineralization
  !
  !ThtaPON1		THTAPON1		temperature theta for G class 1 pon						unitless		1.1
  !ThtaPON2		THTAPON2		temperature theta for G class 2 pon						unitless		1.15
  !ThtaPON3		THTAPON3		temperature theta for G class 3 pon						unitless		1.17
  !ThtaPOC1		THTAPOC1		temperature theta for G class 1 poc						unitless		1.1
  !ThtaPOC2		THTAPOC2		temperature theta for G class 2 poc						unitless		1.15
  !ThtaPOC3		THTAPOC3		temperature theta for G class 3 poc						unitless		1.17
  !ThtaPOP1		THTAPOP1		temperature theta for G class 1 pop						unitless		1.1
  !ThtaPOP2		THTAPOP2		temperature theta for G class 2 pop						unitless		1.15
  !ThtaPOP3		THTAPOP3		temperature theta for G class 3 pop						unitless		1.17
  !
      THTAPON1 = 1.1
      THTAPON2 = 1.15
      THTAPON3 = 1.17
!
      THTAPOC1 = 1.1
      THTAPOC2 = 1.15
      THTAPOC3 = 1.17
!
      THTAPOP1 = 1.1
      THTAPOP2 = 1.15
      THTAPOP3 = 1.17
!
  !Parameters for partical mixing and benthic stress
  !
  !POC1R			POC1R			reference G1 at which w12base = Dp / H2 at 				mgO2/gD			0.2667				mgC/gD			*Not available
  !									20 degC for DiToro eqn 13.1
  !
  !kBEN_STR		KBENSTR 		first-order decay rate constant for benthic  			day^-1			0.03				1/d
  !									stress (d^-1) for DiToro eqn 13.3
  !
  !KM_O2_Dp		KMO2DP			particle mixing half-saturation constant 				mgO2/L			4					mgO2/L
  !									for O2 (mgO2/L)
!
  ! ??			KLBNTH			ratio of bio-irrigation to bioturbation					unitless
  ! ?? 			DPMIN			minimum particule mixing rate (m^2/day)
!
      POC1R = 0.1 !mgC/gSediment  DMD eqn (13.1) !0.2667/2.667
  !WLong notes eqn(3-26) (DMD book page 278, table 13.1, eqn(13.1))
      KBENSTR = 0.03 !1/d
      KMO2DP = 4.0 !mgO2/L  ~ gO2/m^3
!
      KLBNTH = 0.d0 !to make sure no bioturbation
      DPMIN = 0.d0 !to make minimum  particle diffusion rate zero
  !
  !Parameters for silica
  !
  !KSI			KSI				reaction rate for particulate biogenic Si 				day^-1			0.5					1/d
  !THTASI			THTASI			temperature theta for KSI								unitless		1.1					unitless
  !CSISAT			CSISATT20		saturation concentration for porewater Si				mgSi/L			40					mg Si/m3 	*
  !THTASISAT		THTASISAT		temperature theta for CSISAT							unitless		1.023							*Not available
  !DPIE1SI		DPIE1SI			incremental partition coefficient for Si in layer 1		Unitless		10					Unitless
  !PIE2SI			PIE2SI			partition coefficient for Si in layer 2					L/kgD			100					L/kg
  !KMPSI			KMPSI			particulate biogenic Si half saturation 				mgSi/L			50				mg Si/m3	*
  !									constant for dissolution
  !O2CRITSI		O2CRITSI		critical O2 concentration for layer 1 					mgO2/L			2					O2CRITSI
  !									incremental Si sorption
  !
      KSI = 0.5 !1/d
      THTASI = 1.1 !dimensionless		!DMD book p155 Tabel 7.1
      CSISATT20 = 40 * 1000.0 !mgSi/m^3
      THTASISAT = 1.023 !dimensionless
      DPIE1SI = 10 !unitless
      PIE2SI = 100 !L/kg
      KMPSI = 50000.0 * 1000.0 !mgSi/m^3			!DMD book p156 100mgSi/gsediment ==> p573 50000mgSi/L ==> 50,000,000 mgSi/m^3
      O2CRITSI = 2.0 !mgO2/L ~ gO2/m^3 !used for calculating PIE1SI
  !
  !
  !Parameters for temperature
  !
  !need to make sure DIFFT*DLT/HSED/HSED <0.5 to be stable
      DIFFT = 1000.0 / 86400000.0000000 / 5.0 !m^2/sec      !used for calculating temperature in sediments
!
  !
  !initial conditions for sediment concentratins
  !
  !----------------------------------------------------------------------------------------------------------------------------------------------------------------
  !parameter		parameter		parameter_meaning										unit			initial_value		unit			IC			equation
  !(excel)		(mod_sed.F)																(excel)				(excel)			(mod_sed)		(fortran)	(DMD book)
  !---------------------------------------------------------------------------------------------------------------------------------------------------------------
  !
  !Particulate organic C, N, and P in layer 2
!
  !POC2(1)		CPOCI(1)		G class 1 POC in layer 2								gO2/m^3			89.446				mgC/m^3	*		(89.446/2.667)*1000
  !POC2(2)		CPOCI(2)		G class 2 POC in layer 2								gO2/m^3			622.783				mgC/m^3	*		(622.783/2.667)*1000
  !POC2(3)		CPOCI(3)		G class 3 POC in layer 2								gO2/m^3			6569.343			mgC/m^3	*		(6569.343/2.667)*1000
  !PON2(1)		CPONI(1)		G class 1 PON in layer 2								gN/m^3			1.491				mgN/m^3			1.491*1000
  !PON2(2)		CPONI(2)		G class 2 PON in layer 2								gN/m^3			12.975				mgN/m^3			12.975*1000
  !PON2(3)		CPONI(3)		G class 3 PON in layer 2								gN/m^3			72.993				mgN/m^3			72.993*1000
  !POP2(1)		CPOPI(1)		G class 1 POP in layer 2								gP/m^3			0.894				mgP/m^3			0.894*1000
  !POP2(2)		CPOPI(2)		G class 2 POP in layer 2								gP/m^3			6.228				mgP/m^3			6.228*1000
  !POP2(3)		CPOPI(3)		G class 3 POP in layer 2								gP/m^3			65.693				mgP/m^3			65.693*1000
  !POS2			CPOSI			Particulate biogenic Si in layer 2						gSi/m^3			0.302				mgSi/m^3		0.302*1000
  !
  !Sediment temperature and benthic stress
  !*				CTEMPI																					??					degC			15.0
  !*				BENSTI																					0.0									0.0
!
  !Dissolved constituents in layer 1 and 2 porewater
!
  !NH3(1)			*NH4T1I			Dissolved ammonia N in layer 1 porewater				mgN/L			0.024				mgN/m^3			0.024*1000
  !NH3(2)			NH4T2I			Dissolved ammonia N in layer 2 porewater				mgN/L			0.152				mgN/m^3			0.152*1000
  !NO3(1)			*NO3T1I			Dissolved nitrate+nitrite N in layer 1 porewater		mgN/L			0.041				mgN/m^3			0.041*1000
  !NO3(2)			NO3T2I			Dissolved nitrate+nitrite N in layer 2 porewater		mgN/L			0.007				mgN/m^3			0.007*1000
  !PO4(1)			*PO4T1I			Dissolved phosphate P in layer 1 porewater				mgP/L			0.054				mgP/m^3			0.054*1000
  !PO4(2)			PO4T2I			Dissolved phosphate P in layer 2 porewater				mgP/L			0.290				mgP/m^3			0.290*1000
  !Si(1)			*SIT1I			Dissolved Si in layer 1 porewater						mgSi/L			0.209				mgSi/m^3        0.209*1000
  !Si(2)			SIT2I			Dissolved Si in layer 2 porewater						mgSi/L			1.160				mgSi/m^3		1.160*1000
!
  !HS(1)			*HST1I			Dissolved sulfide in layer 1 porewater 					mgO2/L			0.002               gO2/m^3			0.002
  !									(used if salinity >= 1 psu)
  !HS(2)			HST2I			Dissolved sulfide in layer 2 porewater					mgO2/L			3.348				gO2/m^3			3.348
  !								 	(used if salinity >= 1 psu)
  !CH4(1)			CH41TI			Dissolved methane in layer 1 porewater 					mgO2/L			0.000				gO2/m^3			0.000
  !								 	(used if salinity >= 1 psu)
  !CH4(2)			CH4T2I			Dissolved methane in layer 2 porewater 					mgO2/L			0.000				gO2/m^3			0.000
  !									(used if salinity >= 1 psu)
  !**				SO4T2I																	mgO2/L			??					mgO2/L			0.000
  !*				SO4T2I																	mgO2/L			??					mgO2/L			0.000
!
  !
      CPOCI (1) = 1000 * 115.24 / 2.667 !mgC/m^3
      CPOCI (2) = 1000 * 1047.17 / 2.667 !mgC/m^3
      CPOCI (3) = 1000 * 2046.35 / 2.667 !mgC/m^3
      CPONI (1) = 1000 * 63.88 !mgN/m^3
      CPONI (2) = 1000 * 725.57 !mgN/m^3
      CPONI (3) = 1000 * 756.20 !mgN/m^3
      CPOPI (1) = 1000 * 0.925 !mgP/m^3
      CPOPI (2) = 1000 * 8.404 !mgP/m^3
      CPOPI (3) = 1000 * 16.423 !mgP/m^3
      BENSTI = 0.0 !unitless		!Fortran only ??
      CPOSI = 1000 * 2898.096 !mgSi/m^3
!
  !
  !Note that in Excel version, concentrations are given as dissolved , then total concenration is calculated based on m1,m2 and PIENH4
  !
  !fd1 = (1# / (1# + m1 * KdNH3))
  !fd2 = (1# / (1# + m2 * KdNH3))
  !NH3Tp2(1) = NH3p2(1) / fd1
  !NH3Tp2(2) = NH3p2(2) / fd2
  !
  !m1=m2=0.5, PIENH4=1 ==> fd1=(1/(1+0.5*1))=1/1.5=0.3333
  !						fd2=(1/(1+0.5*1))=1/1.5=0.3333
  !
      PIE1 = PIENH4
      PIE2 = PIENH4
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      NH41TI = 1000 * 0.562 !mgN/m^3		!Excel only
      NH4T2I = 1000 * (4.377) / FD2 !mgN/m^3
!
      NO31TI = 1000 * 0.229 !mgN/m^3		!Excel only
      NO3T2I = 1000 * 0.038 !mgN/m^3
!
  !also make sure PO4T2I is total PO4
      I = 1
      If (SALT(I, KBM1) < SALTND) Then
         DPIE1PO4 = DPIE1PO4F
      Else
         DPIE1PO4 = DPIE1PO4S
      End If
      If (O20 > O2CRITPO4) Then
         PIE1 = PIE2PO4 * DPIE1PO4
      Else
         PIE1 = PIE2PO4 * DPIE1PO4 ** (O20/O2CRITPO4)
      End If
      PIE2 = PIE2PO4
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      PO41TI = 1000 * 0.077 !mgP/m^3		!Excel only
      PO4T2I = 1000 * 0.293 / FD2 !converted to total value
!
      If (O20 > O2CRITSI) Then
         PIE1 = PIE2SI * DPIE1SI
      Else
         PIE1 = PIE2SI * DPIE1SI ** (O20/O2CRITSI)
      End If
      PIE2 = PIE2SI
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      SI1TI = 1000 * 1.483 !mgSi/m^3		!Excel only
      SIT2I = 1000 * 8.287 / FD2 !mgSi/m^3		!converted to total Si in layer 2
!
      PIE1 = PIE1HS
      PIE2 = PIE2HS
      FD1 = 1 / (1.0+M1*PIE1)
      FD2 = 1 / (1.0+M2*PIE2)
!
      HS1TI = 0.000 !mgO2/L			!Excel only
      HST2I = 0.000 / FD2 !mgO2/L			!converted to total HS
!
      CH4T2I = 0.0 !mgO2/L
      CH41TI = 0.0 !mgO2/L
!
      SO41TI = 0.0 !mgO2/L			!Neither Excel Nor Fortran
      SO4T2I = 0.0 !mgO2/L			!Fortran only
  !
      SODI = 0.1 !mgO2/m^2/d		!Initialize SOD !WLong: may need to call this SODI
  !Greg Pelletier's Excel used JSOD=0.1 mgO2/m^2/d
      Do I = 1, MLOC
!
         CTEMP (I) = CTEMPI !Sediment temperature
!
         Do JG = 1, 3
            CPOC (I, JG) = CPOCI (JG)
            CPON (I, JG) = CPONI (JG)
            CPOP (I, JG) = CPOPI (JG)
         End Do
!
         CPOS (I) = CPOSI
         BENSTRTM1S (I) = BENSTI !*not in excel
         SODTM1S (I) = SODI
         NH41TM1S (I) = NH41TI
         NH4T2TM1S (I) = NH4T2I
!
         NO31TM1S (I) = NO31TI
         NO3T2TM1S (I) = NO3T2I
!
         PO41TM1S (I) = PO41TI
         PO4T2TM1S (I) = PO4T2I
!
         SI1TM1S (I) = SI1TI
         SIT2TM1S (I) = SIT2I
!
         HS1TM1S (I) = HS1TI
         HST2TM1S (I) = HST2I
!
         CH41TM1S (I) = CH41TI
         CH4T2TM1S (I) = CH4T2I
!
         SO4T2TM1S (I) = SO4T2I !*not in excel
     !
      End Do
!
  !reset the intial values of varables that involve some of the above parameters
      Call SED_INIT2
  !
  !open output file
!
      BFOFN = 'sedtest_2f.csv'
  !
      If (MSR) Then
!
         Open (Unit=BFO, File=BFOFN)
         Rewind (BFO)
!
         IDEBUG_TEST = 2
         If (MSR) WRITE (*,*) 'IDEBUG_TEST=', IDEBUG_TEST
!
     !
     !write the headline the same as excel file "SedFlux_ver17b12_test2f.xlsm"
     !
!
     !Segment ID	JDAY+Time	Jcin	Jnin	Jpin	Jsin	O20	Depth	Tw	NH30	NO30	PO40	SI0	CH40	SALw
     !				SOD	Jnh4	Jno3	JDenitT	Jch4	Jch4g	Jhs	Jpo4	Jsi	Jcout	Jnout	Jpout	NH3(1)	NH3(2)
     !				NO3(1)	NO3(2)	PO4(1)	PO4(2)	Si(1)	Si(2)	CH4(1)	CH4(2)	HS(1)	HS(2)	POC2(1)	POC2(2)	POC2(3)
     !				PON2(1)	PON2(2)	PON2(3)	POP2(1)	POP2(2)	POP2(3)	POS2 H1	BEN_STR
     !
         I = 1
         Write (BFO, 1001) 'SegmentID', ',', 'JDAY', ',', 'Jcin', ',', 'Jnin', ',', 'Jpin', ',', 'Jsin', ',', 'O20', ',', 'Depth', ',', 'Tw', ',', 'NH40', ',', 'NO30', ',', 'PO40', ',', 'SI0', ',', 'CH40', ',', 'SALw', ',', 'SOD', ',', 'Jnh4', ',', 'Jno3',&
        &  ',', 'JDenitT', ',', 'Jch4', ',', 'Jch4g', ',', 'Jhs', ',', 'Jpo4', ',', 'Jsi', ',', 'Jcout', ',', 'Jnout', ',', 'Jpout', ',', 'NH3(1)', ',', 'NH3(2)', ',', 'NO3(1)', ',', 'NO3(2)', ',', 'PO4(1)', ',', 'PO4(2)', ',', 'Si(1)', ',', 'Si(2)', ',', &
        & 'CH4(1)', ',', 'CH4(2)', ',', 'HS(1)', ',', 'HS(2)', ',', 'POC2(1)', ',', 'POC2(2)', ',', 'POC2(3)', ',', 'PON2(1)', ',', 'PON2(2)', ',', 'PON2(3)', ',', 'POP2(1)', ',', 'POP2(2)', ',', 'POP2(3)', ',', 'POS2', ',', 'H1', ',', 'BEN_STR'
!
      End If
1000  Format (I10, 1000(1 X, A10, F15.8))
1001  Format (A10, 1000(1 X, A10, A15))
  !
  !do test 2a calculations
!
      TMSTRT = 0.0 !reference time
      TMEND = 4.99219 !end time
      JDAY = 0.0 !time in days
      DLT = 0.0078125 * 86400 !time step in seconds
      ELTMS = 0.0 !time in sec
      ELTMJD = 0.0 !time in days after reference time
      DLTS = DLT / 86400.
!
      END_RUN = .False.
  !
  !
  !initialize NXJDAY and NXWCL for updating watercolumn conditions
  !
!
      NXJDAY = 0.0
      NXWCL = 0.0
!
      IINT = 0 !time step counter
!
      Do WHILE ( .Not. END_RUN)
     !
         If (MSR) WRITE (*,*) 'JDAY=', JDAY
     !
     !update overlying water column concentration
         If (ITVWCLF == 1) Then
            If (JDAY >= NXJDAY) Call SEDTEST_UPDATE_WATERCOLUMN &
           & (NXJDAY)!read overlying water column concentration etc
!
        !set Overlying water colum concentrations according to the values read fromt the file
        !NXWCL,		&!jday of next record
        !JCIN_R1,	&!C OM flux
        !JNIN_R1,	&!N OM flux
        !JPIN_R1,	&!P OM flux
        !JSIN_R1,	&!Si OM flux
!
        !O20_R1,		&!O20
        !D_R1,		&!total water depth
        !TW_R1,		&!water temperature
        !NH30_R1,	&!nitrate
        !NO30_R1,	&!ammonia
        !PO40_R1,	&!phosphate concentration
        !SIAT0_R1,	&!silicate
        !CH40_R1,	&!methane
        !SALT0_R1	 !salinity
!
            Do I = 1, MLOC
           !		Fortran									InputUnit
               T (I, KWC) = TW_R1 !1  	degC						15				degC
               SALT (I, KWC) = SALT0_R1 !2		ppt							30				ppt
               NH4 (I, KWC) = NH30_R1 !13		gN/m^3						0.015			mgN/L
               NO3 (I, KWC) = NO30_R1 !14		gN/m^3						0.1				mgN/L
               PO4 (I, KWC) = PO40_R1 !20		gP/m^3						0.004			mgP/L
               DOXG (I, KWC) = O20_R1 !27		mgO2/L						5				mgO2/L
               SIAT (I, KWC) = SIAT0_R1 !29		gSi/m^3						0.216			mgSi/L (i.e. gSi/m^3)
               D (I) = D_R1 !		m							2.0				m
!
           !missing CH40	~set to zero in mod_sed
           !missing SO40	~set to SO40MG*0.65306122 in mod_sed
           !missing HS0 	~set to COD(I,KWC) in mod_sed.F and COD is given zero above
            End Do
         End If
!
     !
         Call POM_ACCUMUL
     !
     !average the accumulated POM flux over NDTSED steps
         Do I = 1, MLOC
            Do K = 1, 3
               JPOC (I, K) = JPOCaccum (I, K)
               JPON (I, K) = JPONaccum (I, K)
               JPOP (I, K) = JPOPaccum (I, K)
            End Do
            JPOS (I) = JPOSaccum (I)
         End Do
     !
     !
     !set up initial condition by doing one-time calculation using steady state formulation with the
     !forcing given above
     !
!
         If ((JDAY < TMSTRT+DLTS) .And. STEADY_STATE_SED_IC) Then
            Call SED_CALC (.True.)!calculate sediment state variables using steady state formulation
            Call SED_INIT2 !set the initial conditions using the calcualated value
         Else
            Call SED_CALC (.False.)!calculate with time variable solution
        !
         End If
!
     !set accumulative fluxes back to zero
!
         JPOCaccum = 0.0
         JPONaccum = 0.0
         JPOPaccum = 0.0
         JPOSaccum = 0.0
!
     !write out outputs
     !
         If (MSR) Then
        !Fortran							Excel				FortranUnit			ExcelUnit
            I = 1
            Write (BFO, 1000) I, ',', JDAY, ',', (JPOC(I, 1)+JPOC(I, 2)+&
           & JPOC(I, 3)) / 1000.0 * 2.667, ',', (JPON(I, 1)+JPON(I, 2)+&
           & JPON(I, 3)) / 1000.0, ',', (JPOP(I, 1)+JPOP(I, 2)+JPOP(I, &
           & 3)) / 1000.0, ',', JPOS (I) / 1000.0, ',', O20, ',', D &
           & (I), ',', T (I, KBM1), ',', NH40 / 1000, ',', NO30 / 1000, &
           & ',', PO40 / 1000, ',', SI0 / 1000, ',', CH40, ',', SAL, ',&
           &', SOD, ',', JNH4 / 1000, ',', JNO3 / 1000, ',', BENDEN &
           & (1), ',', JCH4, ',', JCH4G, ',', JHS, ',', JPO4 / 1000, ',&
           &', JSI / 1000, ',', 0.0, ',', 0.0, ',', 0.0, ',', NH41 / &
           & 1000, ',', NH42 / 1000, ',', NO31 / 1000, ',', NO32 / &
           & 1000, ',', PO41 / 1000, ',', PO42 / 1000, ',', SI1 / 1000, &
           & ',', SI2 / 1000, ',', CH41, ',', CH42, ',', HS1, ',', HS2, &
           & ',', POC1 / 1000 * 2.667, ',', POC2 / 1000 * 2.667, ',', &
           & POC3 / 1000 * 2.667, ',', PON1 / 1000, ',', PON2 / 1000, '&
           &,', PON3 / 1000, ',', POP1 / 1000, ',', POP2 / 1000, ',', &
           & POP3 / 1000, ',', PSISED / 1000, ',', HSED1 (I) * 1000.0, &
           & ',', BENSTR !'BEN_STR' 			dimensionless		dimensionless
        ! WRITE(BFO,1000)I,',',JDAY,			&
        !	',',(JPOC(I,1)+JPOC(I,2)+JPOC(I,3))/1000.0*2.667,	&	!'Jcin'				mgC/m^2/d			gO2/m^2/d
        !	',',(JPON(I,1)+JPON(I,2)+JPON(I,3))/1000.0,	&			!'Jnin'				*mgN/m^2/d			gN/m^2/d
        !	',',(JPOP(I,1)+JPOP(I,2)+JPOP(I,3))/1000.0,	&			!'Jpin'				*mgP/m^2/d			gP/m^2/d
        !	',',JPOS(I)/1000.0,	&									!'Jsin		 		*mgSi/m^2/d			gSi/m^2/d
        !	',',O20,			&									!'O20'				mgO2/L				mgO2/L
        !	',',D(I),			&									!'Depth'			m					m
        !	',',T(I,KBM1),		&									!'Tw'				degC				degC
        !	',',NH40/1000,		&									!'NH30'				*mgN/m^3			mgN/L
        !	',',NO30/1000,		&									!'NO30'				*mgN/m^3			mgN/L
        !	',',PO40/1000,		&									!'PO40'				*mgP/m^3			mgP/L
        !	',',SI0/1000,		&									!'SI0'				*mgSi/m^3 			mgSi/L				(dissiolved SIAT)
        !	',',CH40,			&									!'CH40'				gO2/m^3				mgO2/L
        !	',',SAL,			&									!'SALw'  			ppt					ppt					!i.e. SALT(I,KWC), ppt
        !	',',SOD,			&									!'SOD'				gO2/m^2/day			gO2/m^2/d
        !	',',JNH4/1000,		&									!'Jnh4				*mgN/m^2/day		gN/m^2/d
        !	',',JNO3/1000,		&									!'Jno3				*mgN/m^2/day		gN/m^2/d
        !	',',BENDEN(1),		&									!'JDenitT'			gN/m^2/day			gN/m^2/d
        !							!JDenit(1) = Denit(1) * NO3(1)
        !							!JDenit(2) = Denit(2) * NO3(2)
        !							!JDenitT = JDenit(1) + JDenit(2)
        !	',',JCH4,			&									!'Jch4'				gO2/m^2/day			gO2/m^2/d
        !	',',JCH4G,			&									!'Jch4g'			gO2/m^2/day			gO2/m^2/d
        !	',',JHS,			&									!'Jhs'				gO2/m^2/day			gO2/m^2/d
        !	',',JPO4/1000,		&									!'Jpo4'				*mgP/m^2/day		gP/m^2/d
        !	',',JSI/1000,		&									!'Jsi'				*mgSi/m^2/day		gSi/m^2/d
        !	',',0.0,			&									!'Jcout'
        !	',',0.0,			&									!'Jnout'
        !	',',0.0,			&									!'Jpout'
        !         !
        ! WLong: Excel calculations here, should correspond to Jcin, Jnin and Jpin
        ! Jcout = SOD + JCH4 + JCH4g + JHS +  	(POC2(1) + POC2(2) + POC2(3)) * w2 +
        !						(		(POC2(1) + POC2(2) + POC2(3))
        !							- 	(POC22(1) + POC22(2) + POC22(3))
        !						) * H2 / tc
        ! Jnout = JNO3 + JNH4 + JDenitT + 		(PON2(1) + PON2(2) + PON2(3)) * w2 +
        !						(		(PON2(1) + PON2(2) + PON2(3))
        !							- 	(PON22(1) + PON22(2) + PON22(3))
        !						) * H2 / tc
        ! Jpout = JPO4 + 						(POP2(1) + POP2(2) + POP2(3)) * w2 +
        !						((POP2(1) + POP2(2) + POP2(3))
        !						 - (POP22(1) + POP22(2) + POP22(3))
        !						 ) * H2 / tc
        !
        !	',',NH41/1000,		&					!'NH3(1)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',NH42/1000,		&					!'NH3(2)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',NO31/1000,		&					!'NO3(1)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',NO32/1000,		&					!'NO3(2)'			*mgN/m^3 			mgN/L	(dissolved)
        !	',',PO41/1000,		&					!'PO4(1)'			*mgP/m^3 			mgP/L	(dissolved)
        !	',',PO42/1000,		&					!'PO4(2)'			*mgP/m^3 			mgP/L	(dissolved)
        !	',',SI1/1000,		&					!'Si(1)'			*mgSi/m^3 			mgSi/L	(dissolved)
        !	',',SI2/1000,		&					!'Si(2)'			*mgSi/m^3 			mgSi/L	(dissolved)
        !	',',CH41,			&					!'CH4(1)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',CH42,			&					!'CH4(2)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',HS1,			&					!'HS(1)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',HS2,			&					!'HS(2)'			gO2/m^3 			mgO2/L	(dissolved)
        !	',',POC1/1000*2.667,&					!'POC2(1)'			*mgC/m^3 			gO2/m^3	(G1 of POC in layer 2)
        !	',',POC2/1000*2.667,&					!'POC2(2)' 			*mgC/m^3 			gO2/m^3	(G2 of POC in layer 2)
        !	',',POC3/1000*2.667,&					!'POC2(3)' 			*mgC/m^3 			gO2/m^3	(G3 of POC in layer 2)
        !	',',PON1/1000,		&					!'PON2(1)'			*mgN/m^3 			gN/m^3	(G1 of PON in layer 2)
        !	',',PON2/1000,		&					!'PON2(2)' 			*mgN/m^3 			gN/m^3	(G2 of PON in layer 2)
        !	',',PON3/1000,		&					!'PON2(3)' 			*mgN/m^3 			gN/m^3	(G3 of PON in layer 2)
        !	',',POP1/1000,		&					!'POP2(1)'			*mgP/m^3 			gP/m^3	(G1 of POP in layer 2)
        !	',',POP2/1000,		&					!'POP2(2)'			*mgP/m^3 			gP/m^3	(G2 of POP in layer 2)
        !	',',POP3/1000,		&					!'POP2(3)' 			*mgP/m^3 			gP/m^3	(G3 of POP in layer 2)
        !	',',PSISED/1000,	&					!'POS2'				*mgSi/m^3 			gSi/m^3	(particulate biogenic Silicate in layer2)
        !	',',HSED1(I)*1000.0,&					!'H1'				m					mm
        !	',',BENSTR			 					!'BEN_STR' 			dimensionless		dimensionless
        !
            Write (*,*) 1, ',', JDAY
            Write (*,*) 'JPOC=,', (JPOC(I, 1)+JPOC(I, 2)+JPOC(I, 3)) / &
           & 1000.0 * 2.667, 'gO2/m^2/d'
        !'Jcin'				mgC/m^2/d			gO2/m^2/d
!
        !',WS1NET(1)=',WS1NET(I), &!
             !',WB1NETMMD=',WB1NETMMD, 					&	!
        !',B1=',B1(I,KWC),&!
        !',FRACALG1(1)=',FRCALG1(1),&!
        !'JPOC(I,1)=,',JPOC(I,1)/1000*2.667, &	!						*mgC/m^2/d			gO2/m^2/d
        !'JPON(I,1)=,',JPON(I,1)/1000.0, 	&	!						*mgN/m^2/d			gN/m^2/d
        !'JPOP(I,1)=,',JPOP(I,1)/1000.0, 	&	!						*mgP/m^2/d			gP/m^2/d
        !'JPOS(I)  =,',JPOS(I)/1000.0,		&	!						*mgSi/m^2/d			gSi/m^2/d
!
            Write (*,*) 'JPON=,', (JPON(I, 1)+JPON(I, 2)+JPON(I, 3)) / &
           & 1000.0, 'gN/m^2/d'
        !'Jnin'				*mgN/m^2/d			gN/m^2/d
            Write (*,*) 'JPOP=,', (JPOP(I, 1)+JPOP(I, 2)+JPOP(I, 3)) / &
           & 1000.0, 'gP/m^2/d'
        !'Jpin'				*mgP/m^2/d			gP/m^2/d
            Write (*,*) 'JPOS=,', JPOS (I) / 1000.0, 'gSi/m^2/d'
        !'Jsin		 		*mgSi/m^2/d			gSi/m^2/d
            Write (*,*)
            Write (*,*) 'O20=,', O20 !,			&					!'O20'				mgO2/L				mgO2/L
            Write (*,*) 'D=,', D (I)!,			&					!'Depth'			m					m
            Write (*,*) 'T=,', T (I, KBM1)!,		&					!'Tw'				degC				degC
            Write (*,*) 'NH40=,', NH40 / 1000 !,	&					!'NH30'				*mgN/m^3			mgN/L
            Write (*,*) 'NO30=,', NO30 / 1000 !,	&					!'NO30'				*mgN/m^3			mgN/L
            Write (*,*) 'PO40=,', PO40 / 1000 !,	&					!'PO40'				*mgP/m^3			mgP/L
            Write (*,*) 'SI0=,', SI0 / 1000 !,		&					!'SI0'				*mgSi/m^3 			mgSi/L				(dissiolved SIAT)
            Write (*,*) 'CH40=,', CH40 !,			&					!'CH40'				gO2/m^3				mgO2/L
            Write (*,*) 'SAL=,', SAL !,			&					!'SALw'  			ppt					ppt					!i.e. SALT(I,KWC), ppt
            Write (*,*) 'SOD=,', SOD !,			&					!'SOD'				gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JNH4=,', JNH4 / 1000 !,	&					!'Jnh4				*mgN/m^2/day		gN/m^2/d
            Write (*,*) 'JNO3=,', JNO3 / 1000 !,	&					!'Jno3				*mgN/m^2/day		gN/m^2/d
            Write (*,*) 'JDeniT=,', BENDEN (1)!,	&					!'JDenitT'			gN/m^2/day			gN/m^2/d
        !JDenit(1) = Denit(1) * NO3(1)
        !JDenit(2) = Denit(2) * NO3(2)
        !JDenitT = JDenit(1) + JDenit(2)
            Write (*,*) 'JCH4=,', JCH4 !,			&					!'Jch4'				gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JCH4G=,', JCH4G !,		&					!'Jch4g'			gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JHS=,', JHS !,			&					!'Jhs'				gO2/m^2/day			gO2/m^2/d
            Write (*,*) 'JPO4=,', JPO4 / 1000 !,	&					!'Jpo4'				*mgP/m^2/day		gP/m^2/d
            Write (*,*) 'JSI=,', JSI / 1000 !,		&					!'Jsi'				*mgSi/m^2/day		gSi/m^2/d
!
        !						',',0.0,				&					!'Jcout'
        !						',',0.0,				&					!'Jnout'
        !						',',0.0,				&					!'Jpout'
!
        !
        !WLong: Excel calculations here, should correspond to Jcin, Jnin and Jpin
        !Jcout = SOD + JCH4 + JCH4g + JHS +  	(POC2(1) + POC2(2) + POC2(3)) * w2 +
        !										(		(POC2(1) + POC2(2) + POC2(3))
        !											- 	(POC22(1) + POC22(2) + POC22(3))
        !										) * H2 / tc
        !Jnout = JNO3 + JNH4 + JDenitT + 		(PON2(1) + PON2(2) + PON2(3)) * w2 +
        !										(		(PON2(1) + PON2(2) + PON2(3))
        !											- 	(PON22(1) + PON22(2) + PON22(3))
        !										) * H2 / tc
        !Jpout = JPO4 + 						(POP2(1) + POP2(2) + POP2(3)) * w2 +
        !										((POP2(1) + POP2(2) + POP2(3))
        !										 - (POP22(1) + POP22(2) + POP22(3))
        !										 ) * H2 / tc
        !
            Write (*,*) 'NH41=,', NH41 / 1000 !,		&					!'NH3(1)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'NH42=,', NH42 / 1000 !,		&					!'NH3(2)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'NO31=,', NO31 / 1000 !,		&					!'NO3(1)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'NO32=,', NO32 / 1000 !,		&					!'NO3(2)'			*mgN/m^3 			mgN/L	(dissolved)
            Write (*,*) 'PO41=,', PO41 / 1000 !,		&					!'PO4(1)'			*mgP/m^3 			mgP/L	(dissolved)
            Write (*,*) 'PO42=,', PO42 / 1000 !,		&					!'PO4(2)'			*mgP/m^3 			mgP/L	(dissolved)
            Write (*,*) 'SI1=,', SI1 / 1000 !,			&					!'Si(1)'			*mgSi/m^3 			mgSi/L	(dissolved)
            Write (*,*) 'SI2=,', SI2 / 1000 !,			&					!'Si(2)'			*mgSi/m^3 			mgSi/L	(dissolved)
            Write (*,*) 'CH41=,', CH41 !,				&					!'CH4(1)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'CH42=,', CH42 !,				&					!'CH4(2)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'SO41=,', SO41 !,				&					!'SO4(1)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'SO42=,', SO42 !,				&					!'SO4(2)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'HSO4=,', HSO4 !,				&					!'HSO4'				m	 				m
            Write (*,*) 'HS1=,', HS1 !,				&					!'HS(1)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'HS2=,', HS2 !,				&					!'HS(2)'			gO2/m^3 			mgO2/L	(dissolved)
            Write (*,*) 'POC1=,', POC1 / 1000 * 2.667 !,	&					!'POC2(1)'			*mgC/m^3 			gO2/m^3	(G1 of POC in layer 2)
            Write (*,*) 'POC2=,', POC2 / 1000 * 2.667 !,	&					!'POC2(2)' 			*mgC/m^3 			gO2/m^3	(G2 of POC in layer 2)
            Write (*,*) 'POC3=,', POC3 / 1000 * 2.667 !,	&					!'POC2(3)' 			*mgC/m^3 			gO2/m^3	(G3 of POC in layer 2)
            Write (*,*) 'PON1=,', PON1 / 1000 !,		&					!'PON2(1)'			*mgN/m^3 			gN/m^3	(G1 of PON in layer 2)
            Write (*,*) 'PON2=,', PON2 / 1000 !,		&					!'PON2(2)' 			*mgN/m^3 			gN/m^3	(G2 of PON in layer 2)
            Write (*,*) 'PON3=,', PON3 / 1000 !,		&					!'PON2(3)' 			*mgN/m^3 			gN/m^3	(G3 of PON in layer 2)
            Write (*,*) 'POP1=,', POP1 / 1000 !,		&					!'POP2(1)'			*mgP/m^3 			gP/m^3	(G1 of POP in layer 2)
            Write (*,*) 'POP2=,', POP2 / 1000 !,		&					!'POP2(2)'			*mgP/m^3 			gP/m^3	(G2 of POP in layer 2)
            Write (*,*) 'POP3=,', POP3 / 1000 !,		&					!'POP2(3)' 			*mgP/m^3 			gP/m^3	(G3 of POP in layer 2)
            Write (*,*) 'PSISED=,', PSISED / 1000 !,	&					!'POS2'				*mgSi/m^3 			gSi/m^3	(particulate biogenic Silicate in layer2)
            Write (*,*) 'CTEMP=', CTEMP (I)
            Write (*,*) 'H1=,', HSED1 (I) * 1000 !,			&					!'H1'				m					mm
            Write (*,*) 'BENSTR=,', BENSTR !'BEN_STR' 			dimensionless		dimensionless
        !
        !		READ(*,*)
         End If
!
         IINT = IINT + 1
         ELTMS = ELTMS + DLT
         JDAY = ELTMS / 86400.
         ELTMJD = JDAY - TMSTRT
     !
         If (JDAY .Ge. TMEND) END_RUN = .True.
!
      End Do
  !
  !
  !close files
  !
  !
      Close (BFI)
      Close (BFO)
  !
      If (ITVWCLF == 1) Then
         Close (WCL)
      End If
  !
  !
  !deallocate sediment module variables
  !
  !
      Call SED_DEALLOC
  !
  !
  !deallocate sav module variables
  !
      If (SAV_CALC) Then
         Call SAV_DEALLOC
         Call SED_SAV_EXCHANGE_DEALLOC
      End If
!
  !
  !deallocate df module variables
  !
      If (DFEEDER) Then
         Call DF_DEALLOC
         Call SED_DF_EXCHANGE_DEALLOC
      End If
  !
  !deallocate sf module variables
  !
  !
      If (SFEEDER) Call SF_DEALLOC
  !
  !deallocate ba module variables
  !
      If (BALGAE_CALC) Call BA_DEALLOC
      Stop 'Finished test2f!'
  !
  !
!
#endif
  !
  !
End Program TEST_SED
