!tvds.F
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
!**                     Adopted from CE-QUAL-ICM  Model                 **
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
!Subroutine TVDS()  !Time varying data series
!
!************************************************************************
!**                  S U B R O U T I N E   T V D S                     **
!************************************************************************
!
Subroutine TVDS (NXTVD)
  !
      Use MOD_PREC, Only: SP
      Use MOD_LIMS, Only: MLOC, MTLOC !LB added for reading time-varying pCO2atm 12feb2016
  !
      Use MOD_SIZES, Only: MGL, NCP !,       &!
  !
      Use MOD_WQM, Only: BB, BENDOC, BENTHIC_FLUXES, BFLUX, BFLUXNX, &
     & DIAGNOSTICS, FD, FILGTH, IT, ITNX, JDAY, KT, METFN, METPTR, &
     & NXBFI, NXMET, TE, TMEND, TTSS, WMS, BENCOD, BENCODB, BENDO, &
     & BENDOB, BENDOCB, BENDON, BENDOP, BENNH4, BENNH4B, BENNO3, &
     & BENNO3B, BENPO4, BENPO4B, BENSA, BENSAB, BFIFN, BFIPTR, &
     & BFLUXNX_GL, KSDOC, ATMFN, ATMLDON, ATMLDOP, ATMNH4, ATMNO3, &
     & ATMOS_LOADS, ATMPO4, ATMPTR, ATMRDON, ATMRDOP, KHSO, KSNH4, &
     & KSNO3, KSO, KSPO4, KSSA, MTCNO3, NXATM, PRECIP, SAV_LOADS, &
     & SEDNO3, TRSDOC, TRSNH4, TRSNO3, TRSO, TRSPO4, TRSSA, B, NBB, &
     & NS1P, NS2P, NS3P, NXSAV, SAVPTR, SVIFN !,          &!
  !
      Use MOD_HYDROVARS, Only: ART1
      Use MOD_CONTROL, Only: SERIAL, PAR
  !
      Use MOD_FILEINFO, Only: DIA, BFI, BAI, MET, ATM, SVI,AIRC
  !
      Use MOD_SAV, Only: SPNAM, EPINAM, KHSTOX, KESAV, KEEPI, &
     & KHEP, NLIMEPI, KHNLEAF, KHNROOT, KHPLEAF, KHPROOT, KHNEPI, &
     & KHPEPI, KHNPSAV, KHNPEPI, PMSAV, PRSPSAVB, BMSAV, BMTBRREF, &
     & FDOSR, SALMAX, ALPHSAV, ALAC, SLSAV, ACDWSAV, ANDWSAV, APDWSAV, &
     & ANCSAV, APCSAV, HCAN, ACAN, BCAN, FNISAV, FNLDSAV, FNRDSAV, &
     & FNLPSAV, FNRPSAV, FPISAV, FPLDSAV, FPRDSAV, FPLPSAV, FPRPSAV, &
     & FCLDSAV, FCRDSAV, FCLPSAV, FCRPSAV, FDOSAV, WSSSAV, WSLSAV, &
     & WSRSAV, WS1SAV, WS2SAV, WS3SAV, WSUSAV, NDDI, NSAVCELL, &
     & NSAVSPCM, PMEPI, BMEPI, PREPI, PRSPEPI, ALPHEPI, CCHLEPI, &
     & ANCEPI, APCEPI, ADWCEPI, FCLDEPI, FCRDEPI, FNIEPI, FNLDEPI, &
     & FNRDEPI, FNLPEPI, FNRPEPI, FPIEPI, FPLDEPI, FPRDEPI, FPLPEPI, &
     & FPRPEPI, FCLDPEP, FCRDPEP, FCLPPEP, FCRPPEP, FNIPEP, FNLDPEP, &
     & FNRDPEP, FNLPPEP, FNRPPEP, FPIPEP, FPLDPEP, FPRDPEP, FPLPPEP, &
     & FPRPPEP, FPSR, FPLEAF, FPROOT, FPSTEM, FPTUBER, TRTBRLF, & ! AN FTPSAV,
     & FRPOCSAV, FRPONSAV, FRPOPSAV, & !AN FTRSAV, FTPEP, FTREP, FTPREP,
     & LEAF, ROOT, STEM, TUBER, EP, SAVAREA, SAVCELL, NSAVSPC, SAVDPH, &
     & SAVFRAC, PLEAF, BMLEAF, BMSTEM, BMROOT, BMTUBER, PEP, BMEP, &
     & PREP, SLSH, NLSAV, PLSAV, FNSEDSAV, FPSEDSAV, NLEPI, PLEPI, FHS, &
     & EPATN, SAVATN, WATATN, FISH, FIEP, NPPSAV, NPPEPI, DOSAVW, &
     & LDOCSAVW, RDOCSAVW, LPOCSAVW, RPOCSAVW, NH4SAVW, NO3SAVW, &
     & LDONSAVW, RDONSAVW, LPONSAVW, RPONSAVW, PO4SAVW, LDOPSAVW, &
     & RDOPSAVW, LPOPSAVW, RPOPSAVW, DOEPIW, LDOCEPIW, RDOCEPIW, &
     & LPOCEPIW, RPOCEPIW, NH4EPIW, NO3EPIW, LDONEPIW, RDONEPIW, &
     & LPONEPIW, RPONEPIW, PO4EPIW, LDOPEPIW, RDOPEPIW, LPOPEPIW, &
     & RPOPEPIW, SEDPOCSAV, SEDPONSAV, SEDPOPSAV, SEDDOSAV, SEDNH4SAV, &
     & SEDNO3SAV, SEDPO4SAV, DGRDAYS, ALEAF, AROOT, ASTEM, ATUBER, AEP, &
     & APLEAF, ABMLEAF, ABMTUBER, APEP, ABMEP, APREP, ASLSH, ANLSAV, &
     & APLSAV, ANLEPI, APLEPI, AFNSED, AFPSED, AFHS, AEPATN, AWATATN, &
     & AFISH, AFIEP, ANPPSAV, ANPPEPI, ADOSAVW, ADOCSAVW, APOCSAVW, &
     & ANH4SAVW, ANO3SAVW, ADONSAVW, APONSAVW, APO4SAVW, ADOPSAVW, &
     & APOPSAVW, ADOEPIW, ANH4EPIW, ANO3EPIW, APO4EPIW, ADOCEPIW, &
     & APOCEPIW, ADONEPIW, APONEPIW, ADOPEPIW, APOPEPIW, ASEDDOSAV, &
     & ASEDPOCSAV, ASEDPONSAV, ASEDPOPSAV, ASEDNH4SAV, ASEDPO4SAV
#   if defined (MULTIPROCESSOR)
      Use MOD_PAR, Only: NGID, NHN, HN_LST
#   endif
!
      Use MOD_CO2SYS, Only: MAPCO2ATM, pco2atmNX, NXPCO2, pCO2atm !LB added to read time-varying pCO2atm 12feb2016
      Use MOD_CONTROL, Only: MSR 
!
      Implicit None
      Save
      Integer :: Max, I, JCON !Index for constituents
  !
  !    REAL(SP) :: FACTOR,FDNX,TENX,WMSNX,BFLUX,BFLUXNX,BFLUXNX_GL,PRECIPNX, &
  !     ANH4NX,ANO3NX,ALDONNX,ARDONNX,APO4NX,ALDOPNX,ARDOPNX,SDOCNX,  &
  !     SLPOCNX,SRPOCNX,SDONX
  !
      Real (SP) :: FACTOR, FDNX, TENX, WMSNX, PRECIPNX, ANH4NX, ANO3NX, &
     & ALDONNX, ARDONNX, APO4NX, ALDOPNX, ARDOPNX, SLDOCNX, SRDOCNX, &
     & SLPOCNX, SRPOCNX, SDONX !DO flux to water column due to SAV    (gC/m^2/day)
      Real (SP) :: S2LNX, KTNX, NXTVD, S1LNX, S3LNX
!
  !
  !Wen Long moved BFLUXNX, BFLUXNX_GL, BFLUX into wqm_module.f module name MOD_WQM!!!
  !
      Dimension S1LNX (NS1P, NCP), S2LNX (NS2P, NCP), S3LNX (NS3P, NCP)
  !
  !WL    DIMENSION S1LNX(NS1P,NCP), S2LNX(NS2P,NCP), S3LNX(NS3P,NCP),    &
  !WL              BFLUXNX(MTLOC,NCP),BFLUXNX_GL(MGL,NCP),BFLUX(MTLOC,NCP)
  !
  !******* Meteorologic data
  !
      !If(MSR) Write (*,*) 'TMEND in TVDS= ', TMEND !AN
	  NXTVD = TMEND
      !FDNX = 1 ! Kurt Glaesemann 13 April 2015 ! commented out it bugs!
      !TENX = 0 ! Kurt Glaesemann 13 April 2015 ! commented out it bugs!
	  !WMSNX = 0 ! AN
10    Continue
      Do WHILE (JDAY >= NXMET)
	 !   AN
	 !   Read (MET, 1010, END=11) NXMET, KTNX, TENX, ITNX, FDNX, WMSNX !  Wind speed (m/s)
    
         KT = KTNX				!coefficeint of heat exchange (surface)
         FD = FDNX				!fraction of day
     !
     ! CONVERT LANGLEYS TO EINSTEINS/M**2
     ! Commented conversion out since SJRWMD data already in einsteins/m**2
     !         IT = 0.093*ITNX
     ! convert w/m**2 to PAR w/m**2 - need to convert to Ein? - states PAR (E/m2/day)

     !     IT = ITNX*0.143  !If not in E/m2/day, then divide by 0.143 to convert from watt/m^2 to E/m^2/day
         IT = ITNX          !THIS is already in E/m2/day in input file
     ! RGL this all assumes that the met data are read in on a daily basis (not hourly)
     ! need to change this
     ! TIME TO SUNRISE (SECONDS OF THE DAY COUNTING FROM 00:00 (MIDNIGHT) )
         TTSS = 86400. * (1-FD) / 2.
     !
	    !If(MSR) Write (*,*) 'FD, JDAY in TVDS= ', FD, JDAY !AN
        !If(MSR) Write (*,*) 'IO input in TVDS= ', IT !AN
	 !
         TE = Max (TENX, 0.0)
         WMS = WMSNX

     !   Move up AN
	     Read (MET, 1010, END=11) NXMET, KTNX, TENX, ITNX, FDNX, WMSNX !  Wind speed (m/s)
     !
         NXMET = (METPTR-1) * FILGTH + NXMET
		 NXMET = AINT(NXMET) !AN: This code fixes the daily estimate I0

		 !If(MSR) Write (*,*) 'JDAY, NXMET, METPTR  inside loop = ', JDAY, NXMET, METPTR !AN

      End Do
      Go To 12
  !
  !******* Open next data file
  !
11    Continue
      METPTR = METPTR + 1
#if defined (KILLTREES)  /*Wen Long, put diagnostics also in killtrees */
      If (DIAGNOSTICS) WRITE (DIA,*) 'Opening meteorologic file ', &
     & METPTR, ' at day ', JDAY
#endif
      Close (MET)
      Open (MET, File=METFN(METPTR), Status='OLD')
      Read (MET, 1000)
  !
  ! INITIALIZE DEGREE DAYS TO ZERO
  !WLong: this is now initialized by ALLOC_SAV() in wqm_sav.F
  !    DO I=1,MLOC
  !      DGRDAYS(I)=0  
  !    ENDDO
  !
      Read (MET, 1010) NXMET, KTNX, TENX, ITNX, FDNX, WMSNX
      NXMET = (METPTR-1) * FILGTH + NXMET
      If (JDAY >= NXMET) Go To 10
12    Continue
      NXTVD = Min (NXTVD, NXMET)
	  !If(MSR) Write (*,*) 'NXTVD in TVDS= ', NXTVD, 'NXMET= ', NXMET !AN
  !
  
  !******* Benthic fluxes
  !
      If (BENTHIC_FLUXES) Then
126      Continue
         Do WHILE (JDAY >= NXBFI)
            Do JCON = 1, 9
               Do BB = 1, MLOC
                  BFLUX (BB, JCON) = BFLUXNX (BB, JCON)
               End Do
            End Do
        !
        !Store previously read results (without temperature effects)
        !
            BENDOC = BFLUX (:, 1)
            BENNH4 = BFLUX (:, 2)
            BENNO3 = BFLUX (:, 3)
            BENDON = BFLUX (:, 4)
            BENPO4 = BFLUX (:, 5)
            BENDOP = BFLUX (:, 6)
            BENCOD = BFLUX (:, 7)
            BENDO = BFLUX (:, 8)
            BENSA = BFLUX (:, 9)
        !
            Do BB = 1, MLOC
           !JQI          BFLUXB(BB,1) = BFLUX(BB,1)                              ! DOC
           !JQI          BFLUXB(BB,2) = BFLUX(BB,2)                              ! NH4
           !JQI          BFLUXB(BB,3) = BFLUX(BB,3)                              ! NO3
           !JQI          BFLUXB(BB,4) = BFLUX(BB,5)                              ! PO4
           !JQI          BFLUXB(BB,5) = BFLUX(BB,7)                              ! COD
           !JQI          BFLUXB(BB,6) = BFLUX(BB,8)                              ! DO
           !JQI          BFLUXB(BB,7) = BFLUX(BB,9)                              ! SIAT
           !store data without the temperature effects, temperature effects
           !will be applied in wqm_kin.F using these values
               BENDOCB (BB) = BFLUX (BB, 1)! DOC
               BENNH4B (BB) = BFLUX (BB, 2)! NH4
               BENNO3B (BB) = BFLUX (BB, 3)! NO3
               BENPO4B (BB) = BFLUX (BB, 5)! PO4
               BENCODB (BB) = BFLUX (BB, 7)! COD
               BENDOB (BB) = BFLUX (BB, 8)! DO
               BENSAB (BB) = BFLUX (BB, 9)! SIAT
            End Do
        !
            Do BB = 1, MGL
               Read (BFI, 1103, END=127) NXBFI, (BFLUXNX_GL(BB, JCON), &
              & JCON=1, 9)
           !             WRITE(*,1104)NXBFI,(BFLUXNX_GL(1,JCON),JCON=1,9)
            End Do
        !

            If (SERIAL) BFLUXNX = BFLUXNX_GL
        !
#   if defined (MULTIPROCESSOR)
            If (PAR) Then
               Do JCON = 1, 9
                  Do I = 1, MLOC
                     BFLUXNX (I, JCON) = BFLUXNX_GL (NGID(I), JCON)
                  End Do
                  Do I = 1, NHN
                     BFLUXNX (I+MLOC, JCON) = BFLUXNX_GL (HN_LST(I), &
                    & JCON)
                  End Do
               End Do
            End If
#   endif
        !

        !---------------------------------------------------
        !
            NXBFI = (BFIPTR-1) * FILGTH + NXBFI
         End Do
         Go To 128
     !
     !********* Open next data file
     !
127      Continue
         BFIPTR = BFIPTR + 1
#if defined (KILLTREES)
         If (DIAGNOSTICS) WRITE (DIA,*) 'Opening benthic flux file ', &
        & BFIPTR, ' at day ', JDAY
#endif
         Close (BFI)
         Open (BFI, File=BFIFN(BFIPTR), Status='OLD')
     !
         Read (BFI, 1041)!skip 3 title lines
         Read (BFI, 1050) KSDOC, KSNH4, KSNO3, KSPO4, KSO, KSSA
     !       WRITE(*,1051) KSDOC, KSNH4, KSNO3, KSPO4, KSO, KSSA
         Read (BFI, 1050) TRSDOC, TRSNH4, TRSNO3, TRSPO4, TRSO, TRSSA
     !       WRITE(*,1051) TRSDOC,TRSNH4,TRSNO3,TRSPO4,TRSO,TRSSA
         Read (BFI, 1050) MTCNO3, SEDNO3, KHSO
     !       WRITE(*,1051) MTCNO3, SEDNO3, KHSO
         Read (BFI, 1100)
     !
         Do BB = 1, MGL
            Read (BFI, 1103, END=127) NXBFI, (BFLUXNX_GL(BB, JCON), &
           & JCON=1, 9)
        !          WRITE(*,1104)NXBFI,(BFLUXNX_GL(1,JCON),JCON=1,9)
         End Do
     !
         NXBFI = (BFIPTR-1) * FILGTH + NXBFI
     !
         Do JCON = 1, 9
            If (SERIAL) BFLUXNX (:, JCON) = BFLUXNX_GL (:, JCON)
        !
#   if defined (MULTIPROCESSOR)
            If (PAR) Then
               Do I = 1, MLOC
                  BFLUXNX (I, JCON) = BFLUXNX_GL (NGID(I), JCON)
               End Do
               Do I = 1, NHN
                  BFLUXNX (I+MLOC, JCON) = BFLUXNX_GL (HN_LST(I), JCON)
               End Do
            End If
#   endif
         End Do
         If (JDAY >= NXBFI) Go To 126
128      Continue
         NXTVD = Min (NXTVD, NXBFI)
     !WLong debug
     !      WRITE(*,*)'Found record in file number:',BFIPTR
     !      WRITE(*,1104)NXBFI,(BFLUXNX_GL(1,JCON),JCON=1,9)
     !
      End If
  !
  !
  !******* Atmospheric Loads
  !
      If (ATMOS_LOADS) Then
133      Continue
         Do WHILE (JDAY >= NXATM)
            PRECIP = PRECIPNX / 8640000.
            ATMNH4 = ANH4NX
            ATMNO3 = ANO3NX
            ATMLDON = ALDONNX
            ATMRDON = ARDONNX
            ATMPO4 = APO4NX
            ATMLDOP = ALDOPNX
            ATMRDOP = ARDOPNX
            Read (ATM, 1010, END=134) NXATM, PRECIPNX, ANH4NX, ANO3NX, &
           & ALDONNX, ARDONNX, APO4NX, ALDOPNX, ARDOPNX
            NXATM = (ATMPTR-1) * FILGTH + NXATM
         End Do
         Go To 135
     !
     !********* Open next data file
     !
134      Continue
         ATMPTR = ATMPTR + 1
#if defined (KILLTREES)
         If (DIAGNOSTICS) WRITE (DIA,*) 'Opening atmospheric load', 'in&
        &g file ', ATMPTR, ' at ', 'day ', JDAY
#endif
         Close (ATM)
         Open (ATM, File=ATMFN(ATMPTR), Status='OLD')
         Read (ATM, 1000)
         Read (ATM, 1010) NXATM, PRECIPNX, ANH4NX, ANO3NX, ALDONNX, &
        & ARDONNX, APO4NX, ALDOPNX, ARDOPNX
         NXATM = (ATMPTR-1) * FILGTH + NXATM
         If (JDAY >= NXATM) Go To 133
135      Continue
         NXTVD = Min (NXTVD, NXATM)
      End If
  !
  !
  !******* Submerged Aquatic Vegetation
  !Wen Long, this part should be moved to wqm_sav.F as part of MOD_SAV
  !
      If (SAV_LOADS) Then
136      Continue
         Do WHILE (JDAY >= NXSAV)
            Do B = 1, MLOC
           !WL            FACTOR = SAVAREA(B)/SFA(B)           
               FACTOR = SAVAREA (B) / ART1 (B)!replaced SFA by ART1 by Wen Long
               LDOCSAVW (B) = SLDOCNX * FACTOR
               RDOCSAVW (B) = SRDOCNX * FACTOR
               LPOCSAVW (B) = SLPOCNX * FACTOR
               RPOCSAVW (B) = SRPOCNX * FACTOR
               DOSAVW (B) = SDONX * FACTOR
            End Do
            Read (SVI, 1010, END=137) NXSAV, SLDOCNX, SRDOCNX, SLPOCNX, &
           & SRPOCNX, SDONX
            NXSAV = (SAVPTR-1) * FILGTH + NXSAV
         End Do
         Go To 138
     !
     !********* Open next data file
137      Continue
         SAVPTR = SAVPTR + 1
#if defined (KILLTREES)
         If (DIAGNOSTICS) WRITE (DIA,*) 'Opening aquatic vegetation', '&
        & file ', SAVPTR, ' at day ', JDAY
#endif
         Close (SVI)
         Open (SVI, File=SVIFN(SAVPTR), Status='OLD')
         Read (SVI, 1080)
         Read (SVI, 1026) (SAVAREA(B), B=1, NBB)
         Read (SVI, 1080)
         Read (SVI, 1010) NXSAV, SLDOCNX, SRDOCNX, SLPOCNX, SRPOCNX, &
        & SDONX
         NXSAV = (SAVPTR-1) * FILGTH + NXSAV
         If (JDAY >= NXSAV) Go To 136
138      Continue
         NXTVD = Min (NXTVD, NXSAV)
      End If
  !
  !
  !  ******* Atmospheric pCO2  (by LB - 12 feb 2016)
  !          NOTE: time-varying pCO2atm only valid for spatially uniform pCO2atm
      If (MAPCO2ATM == 'TIMEVAR') Then
!
         Do WHILE (JDAY >= NXPCO2)
            Do I = 0, MTLOC
               pCO2atm (I) = pco2atmNX
            End Do
            Read (AIRC, 1031) NXPCO2, pco2atmNX
         End Do
         NXTVD = Min (NXTVD, NXPCO2)
!
         If (MSR) WRITE (*,*) 'time-varying pCO2atm=', pCO2atm (1), 'ne&
        &xt: NXPCO2=', NXPCO2, 'd pco2atmNX=', pco2atmNX
      End If
  !
  !
  !******* Input FORMATs
  !
1000  Format (/ //)
1010  Format (10 F8.0, :/(:8 X, 9 F8.0))
1020  Format (8 X, 9 F8.0, :/(:16 X, 8 F8.0))
1025  Format (16 X, F8.0)
1026  Format (/ / (8 X, 9 F8.0))
1030  Format (/ / (8 X, 9 I8))
1040  Format (/)
1041  Format (/ /)!advance two lines
1050  Format (://8 X, 9 F8.0)!go down 2 lines and read
  !but do not advance to next line when all data
  !items are processed
1051  Format (://8 X, 9 F8.4)!go down 2 lines and write, but do not
  !continue going down when writing is finished
1060  Format (8 X, 9 F8.0)
1070  Format (/ / (:8 X, 6 F8.0))
1080  Format (/)
1082  Format (/ / 8 X, 2 A8 //)
1100  Format (/)
1101  Format (1 F8.0)
1102  Format (1 F8.0)
1103  Format (10 F8.0)
1104  Format (10 F8.4)
1031  Format (F8.1, F8.3)
  !
End Subroutine TVDS
!
