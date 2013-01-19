	PROGRAM WOUPDT 
C************************************************************************
C* WOUPDT								*
C*									*
C* This program creates an updated weather watch outline update (WOU)   *
C* from decoded WOUs and Watch County Notification (WCN) messages.      *
C*									*
C* The command line input is:						*
C*									*
C*                 woupdat  dattim					*
C*			    dattim      GEMPAK date/time string		*
C*									*
C* Log:									*
C* A. Hardy/NCEP	 2/03						*
C* A. Hardy/NCEP	 3/03	Moved END IF 				*
C* A. Hardy/NCEP	 3/03	Added check for no counties     	*
C* A. Hardy/NCEP	 3/03	Changed call for GG_WWCR		*
C* A. Hardy/NCEP	 4/03	Added error check for GG_WWTP   	*
C* A. Hardy/NCEP	 7/03	Added 'mnplus' to GG_WWCR		*
C* A. Hardy/NCEP	 7/03	Added flag options '-c' & '-n'  	*
C* A. Hardy/NCEP	 8/03	Added err code '5';set value		*
C* A. Hardy/NCEP	 1/04	Added VTEC info; stop time check	*
C* B. Yin/SAIC		 3/04	Changed SS_GTIM to CSS_GTIM		*
C* A. Hardy/NCEP	 3/04	Added marine zone flag iznflg		*
C* A. Hardy/NCEP	 8/04	Added test flag to gg_wwtp		*
C* M. Li/SAIC           10/04   Replace CTB_RDWOU with CTB_RDPRF	*
C* A. Hardy/NCEP	 3/05	Added read for WCN VTEC codes		*
C* A. Hardy/NCEP	 3/05	Updated for new flags (o,u,e)		*
C* A. Hardy/NCEP	 3/05	Added irmzn to gg_wcck			*
C* A. Hardy/NCEP	 4/05	Added WCP processing			*
C* Grosshans/SPC&Yen/NCEP 8/05	Updated call to gg_wwtp for CSC.	*
C*				Added more error checking.		*
C* F. J. Yen/NCEP	 11/05	Removed init ircntm,stptim,strtim	*
C* F. J. Yen/NCEP	  1/07	Added amended SAW; fixed ier1 conflict	*
C* F. J. Yen/NCEP	  1/07	Removed systim in GG_ASAW		*
C* F. J. Yen/NCEP	  3/07	Increase dimen. of sacod from 30 to 100.*
C*				Replaced dimension LLSTFL with MAX_CNTY.*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
        CHARACTER       systim*12, strtim*20, stptim*20, ircntm*20,
     +                  type*3, cnties(MAX_CNTY)*128, tzone(100)*3
C*
        PARAMETER       (IRBUNM = 20 )
C*
        CHARACTER       srchtim*20, alias*3, zone*3, 
     +                  dattim*20, timiss(100)*20, string*25,
     +                  tmstrt(100)*20, value*120, attnln*128,
     +                  wfocty(MAX_CNTY)*128, tag*25, tmstp(100)*20
        CHARACTER       prdcod*2, offid*5, sigcd*2, stzstr*256
        CHARACTER       tblnam*72, dirsym*160, sacod(100)*4
        CHARACTER       wtype(IRBUNM)*3, wstart(IRBUNM)*20,
     +                  wend(IRBUNM)*20, wnum(IRBUNM)*5,
     +                  wlatlon(IRBUNM)*124, bndnam*25
        INTEGER         iuwtch(100), mnplus, vtecln, itype, itest,
     +                  fipsin(MAX_CNTY)
        CHARACTER       tmpfip*7, latlon*124, rlstr*10, rlstr1*10
        REAL            latout(10), lonout(10)
C*
        CHARACTER       typein*60, strtin*200, endin*200, cnum*75,
     +                  cltln*1860
        LOGICAL         mkup, extck, canck, fnlck, unqck, gettm,
     +                  create, etmchg, hvarg, dowcp, crswcn, extexb,
     +                  doasaw
C-----------------------------------------------------------------------
       iret   = 0
       mkup   = .False.
       extck  = .False.
       canck  = .False.
       fnlck  = .False.
       unqck  = .False.
       hvarg  = .False.
       dowcp  = .False.
       doasaw = .False.
       gettm  = .true.
       value  = ' '
       itype  = 1
       ibun = 0
       bndnam = 'WBCMZ_BNDS'
       callby = 0
C
C*     Initial tag for number of minutes from end time
C*     to issue a cancellation watch. 
C
       tblnam = 'woudef.tbl'
       dirsym = 'txtprd'
       tag = 'WOU_CNL_PLUS_MIN'
C
       CALL ST_NULL ( tblnam, tblnam, lens, ier )
       CALL ST_NULL ( dirsym, dirsym, lens, ier )
       CALL ST_NULL ( tag, tag, lens, ier)
       CALL CTB_RDPRF ( tblnam, dirsym, tag, value, ier )
C
C*     If tag was bad, set mnplus to 15, send error and continue.
C
       IF ( ier .ne. 0 ) THEN
           mnplus = 15
           numerr = 4
           CALL ER_WMSG ( 'WOUPDT', numerr, ' ', ier1 )
         ELSE IF ( ier .eq. 0 ) THEN
           CALL ST_LSTR ( value, lens, ier)
           CALL ST_NUMB ( value, mnplus, ier1 )
           IF ( ier1 .ne. 0 ) THEN
               mnplus = 15
               numerr = 5
               CALL ER_WMSG ( 'WOUPDT', numerr, ' ', ier1 )
           END IF
       END IF
C
C*     Retrieve VTEC line information.
C
       tag = 'UPDT_WOU_USE_VTEC'
       CALL ST_NULL ( tag, tag, lens, ier)
       CALL CTB_RDPRF ( tblnam, dirsym, tag, value, ier )
       IF ( ier .ne. 0 ) THEN
            numerr = 7
            CALL ER_WMSG ( 'WOUPDT', numerr, tblnam, ier1 )
            value = '2'
       END IF

       CALL ST_NULL ( value, value, lens, ier)
       CALL ST_NUMB (value, vtecln, ier )
C
       IF ( vtecln .ne. 0 ) THEN
           tag = 'UPDT_WOU_OFFID'
           CALL ST_NULL ( tag, tag, lens, ier)
           CALL CTB_RDPRF ( tblnam, dirsym, tag, value, ier )
           IF ( ier .eq. 0 ) THEN
               offid = value
	     ELSE
               numerr = 8
               CALL ER_WMSG ( 'WOUPDT', numerr, tblnam, ier1 )
               offid = 'KWNS'
           END IF
C
           tag = 'UPDT_WOU_SIGCD'
           CALL ST_NULL ( tag, tag, lens, ier)
           CALL CTB_RDPRF ( tblnam, dirsym, tag, value, ier )
           IF ( ier .eq. 0 ) THEN
               sigcd = value
             ELSE
               numerr = 9
               CALL ER_WMSG ( 'WOUPDT', numerr, tblnam, ier1 )
               sigcd = 'A'
           END IF
       END IF
C
C*     Check the arguments listed on the command line
C
       narg = iargc ()
       IF ( narg .gt. 0 ) THEN
           DO ii = 1, iargc()
               CALL GETARG (ii, string)
               IF ( string(1:1) .eq. '-') THEN
                   IF ( (string (:2) .eq. '-e' ) .and.
     +                       (hvarg .neqv. .True. ) ) THEN
                       extck = .True.
                       hvarg = .True.
                     ELSE IF ( (string (:2) .eq. '-o' ) .and.
     +                       (hvarg .neqv. .True. ) ) THEN
                       canck = .True.
                       hvarg = .True.
                     ELSE IF ( (string (:2) .eq. '-c' ) .and.
     +                       (hvarg .neqv. .True. ) ) THEN
                       fnlck = .True.
                       hvarg = .True.
                     ELSE IF ( (string (:2) .eq. '-u' ) .and.
     +                       (hvarg .neqv. .True. ) ) THEN
                       unqck = .True.
                       hvarg = .True.
                     ELSE IF ( (hvarg .neqv. .True. ) ) THEN
                       CALL IP_HELP  ( 'woupdt', .false., ier )
                       CALL SS_EXIT( )
                   END IF
                 ELSE
                   CALL CSS_GTIM ( itype, systim, ier )
                   CALL ST_LSTR ( string, len, ier )
                   CALL TI_STAN ( string(:len), systim, dattim, ier ) 
                   mkup = .true.
                   IF ( ier .eq. 0 ) THEN
c                      dattim = string(:len)
                       systim = ' '
                       srchtim = dattim
                     ELSE
                       numerr = -1
                       CALL ER_WMSG ( 'WOUPDT', numerr, ' ', ier1 )
                       CALL SS_EXIT( )
                   END IF
                   gettm = .false.
               END IF
           END DO
C
C*         If only flags were given and not a date/time, use system time.
C
           IF ( gettm ) THEN
               CALL CSS_GTIM ( itype, systim, ier )
               IF ( ier .eq. 0 ) THEN
                   dattim = systim
                   srchtim = systim
                 ELSE
                   numerr = -1
                   CALL ER_WMSG ( 'WOUPDT', numerr, ' ', ier1 )
                   CALL SS_EXIT( )
               END IF
           END IF
C
         ELSE
C
C*         No arguments listed, set current time and create product.
C
           CALL CSS_GTIM ( itype, systim, ier )
           dattim = systim
           srchtim = systim
           mkup = .true.
       END IF
C
C*     Check if WCP is to be created.
C
       tag = 'UPDT_WCP'
       value  = ' '
       CALL ST_NULL ( tag, tag, lens, ier)
       CALL CTB_RDPRF ( tblnam, dirsym, tag, value, ier )
       CALL ST_LSTR ( value, lens, ier)
       IF ( value(:lens) .eq. 'ON' ) THEN
           dowcp = .true.
       END IF
C
C*     Initialize WCP parameters.
C
       DO jj = 1, IRBUNM
           wtype(jj)   = ' '
           wstart (jj) = ' '
           wend (jj)   = ' '
           wnum (jj)   = ' '
           wlatlon(jj) = ' '
       END DO

C
C*     Find active watch numbers. 
C
       alias = 'WOU' 
       CALL GG_WWCR ( srchtim, alias, mnplus, iuwtch, iunum, tzone,
     +                timiss, tmstrt, tmstp, ier )
C
C*     Find list of active WCN counties for each active watch number.
C
       DO iw = 1, iunum 
C
C*         Reset product code.
C
           IF ( vtecln .ne. 0 ) THEN
               tag = 'UPDT_WOU_PROD_CODE' 
               CALL ST_NULL ( tag, tag, lens, ier)
               CALL CTB_RDPRF ( tblnam, dirsym, tag, value, ier )
               IF ( ier .ne. 0 ) THEN
                   numerr = 10
                   CALL ER_WMSG ( 'WOUPDT', numerr, tblnam, ier1 )
                   value = 'E'
               END IF
               prdcod = value
           END IF
C
C*         Initialize parameters.
C
           attnln = ' '
           create = .false.
           etmchg = .false.
           inumb  = iuwtch(iw)
           zone   = tzone(iw)
C
           DO ii  = 1, MAX_CNTY
               cnties(ii) = ' '
           END DO
C
C*         Find active watch numbers.
C
           CALL GG_WACT ( inumb, dattim, systim, type, strtim,
     +                    stptim, ncnty, cnties, wfocty, knt, etmchg,
     +                    itest, sacod, numarr, ircntm, ier1 )
	   IF ( ier1 .eq. 8 ) THEN
	       ier1 = -3
               CALL ER_WMSG ( 'WOUPDT', ier1, ' ', ier )
             ELSE IF ( ier1 .eq. 7 ) THEN
	       ier1 = -4
	   END IF
C
C*         Alphabetically sort UGCs for WCN counties.
C
           IF ( ( ier1 .eq. 0 ) .and. ( ncnty .gt. 0 ) ) THEN
               icnt = ncnty
               CALL ST_SORT ( 2, icnt, cnties, ncnty, cnties, ier )
C
C*             Alphabetically sort UGCs for WOU counties.
C
               ikcnt = knt
               CALL ST_SORT ( 2, ikcnt, wfocty, knt, wfocty, ier )
C
C*             Check for EXT, EXA, or EXB action codes.
C
               IF ( extck ) THEN
                   mkup = .false.
                   DO ii = 1, numarr
                        IF ( ( sacod (ii) .eq. 'EXT' ) .or.
     +                      ( sacod (ii) .eq. 'EXA' ) .or.
     +                      ( sacod (ii) .eq. 'EXB' ) ) THEN
                           CALL GG_WNCK ( cnties, ncnty, wfocty, knt, 
     +                                    create,  ier )
                        END IF
                   END DO
C
C*                 If there is a time extension, create the product.
C
                   IF ( etmchg ) THEN
                      create = .True.
                   END IF
               END IF 
C
C*             Check for CAN action code.
C
               IF ( canck ) THEN
                   mkup = .false.
                   DO ii = 1, numarr
                       IF ( sacod (ii) .eq. 'CAN' ) THEN
                           CALL GG_WNCK ( cnties, ncnty, wfocty, knt, 
     +                                    create, ier )
                       END IF
                   END DO
               END IF 
C
C*             Check for hourly update.
C
               IF ( unqck ) THEN
C
                   mkup = .false.
C*                 Determine the time difference between the start time
C*                 and the system time.
C
                   CALL TI_DIFF ( dattim, ircntm, nmin, ier )
C
C*                 Check for county changes between last WOU and
C*                 current WCNs.
C
                   CALL GG_WNCK ( cnties, ncnty, wfocty, knt, create,
     +                            ier )
C
C*                 Get the update threshold minutes.
C
                   value = ' '
                   tag = 'UPDT_UNIQUE_MINS' 
                   CALL ST_NULL ( tag, tag, lens, ier)
                   CALL CTB_RDPRF ( tblnam, dirsym, tag, value, ier )
                   IF ( ier .ne. 0 ) THEN
                       numerr = 11
                       CALL ER_WMSG ( 'WOUPDT', numerr, tblnam, ier2 )
                       value = '60'
                   END IF
                   CALL ST_LSTR ( value, lens, ier)
                   CALL ST_NUMB ( value, iunq, ier )
C
C*                 Compare the time differences with time threshold.
C
                   IF ( ( nmin .le. iunq ) .and. ( create ) ) THEN
                       create = .True.
                     ELSE IF ( nmin .gt. iunq ) THEN
                       create = .True.
                     ELSE 
                       create = .False.
                   END IF
               END IF 
           END IF
C
C*         Check if all counties are cleared or final WOU 
C*         needs to be issued.
C
	   crswcn = .false.
           IF ( (fnlck) .or. ( ( ncnty .eq. 0 ) .and.
     +		( (ier1 .le. 0 ) .or. ( ier1 .eq. 2 ) ) ) ) THEN 
               mkup = .false.
               irmzn = 0
               CALL GG_WCCK ( inumb, dattim, systim, irmzn, iznflg, 
     +			     icancl,  attnln, stzstr, ier )
C        
               CALL ST_RMBL ( attnln, attnln, len, ier )
               IF (  ( icancl .eq. 0 ) .and. ( ( ncnty .eq. 0 ) ) ) THEN
                   create = .true.
C
C*                 Create cancel text messages for SAW and SEL.
C
                   CALL GG_WCVF ( inumb, ier )
		   crswcn = .true.
                 ELSE 
                   mkup = .false.
                   create = .false.
               END IF
           END IF
C
C*	   Create amended SAW if there was an extension in time and no
C*	   SAW cancel was created.
C
	   extexb = .false.
	   ii = 1
	   DO WHILE ( .not. extexb .and. ii .le. numarr )
               IF ( ( sacod (ii) .eq. 'EXT' ) .or.
     +              ( sacod (ii) .eq. 'EXB' ) ) THEN
		   extexb = .true.
               END IF
	       ii = ii + 1
           END DO

C
C*         Check if amended SAW is to be created.
C
           tag = 'AMD_SAW'
           value  = ' '
           CALL ST_NULL ( tag, tag, lens, ier)
           CALL CTB_RDPRF ( tblnam, dirsym, tag, value, ier )
           CALL ST_LSTR ( value, lens, ier)
           IF ( value(:lens) .eq. 'ON' .or.
     +          value(:lens) .eq. 'TRUE' ) THEN
               doasaw = .true.
           END IF
	   IF ( doasaw .and. ( etmchg .or. extexb ) .and.
     +			.not. crswcn ) THEN
             IF ( ier1 .eq. 0 ) THEN
      	  	 CALL GG_ASAW ( inumb, dattim, stptim, ier3 )
	     END IF
	   END IF
C
C*         Create the updated WOU text product, if necessary.
C
           IF (( ier1 .ge. -1 ) .and. (( create ) .or. ( mkup ) )) THEN
C
C*	       Sort count list. Place counties before marine zones.
C
               IF ( ncnty .gt. 0 ) THEN
                   CALL GG_ZSRT ( cnties, ncnty, ier )
               END IF
C
C*	       Send information to create text product.
C
               CALL GG_WWTP ( lun, inumb, dattim, srchtim, systim, type, 
     +	 	          strtim, stptim, ncnty, cnties, zone, attnln,
     +                    vtecln, prdcod, offid, sigcd, stzstr, iznflg,
     +                    itest, callby, ier )
               IF ( ier .ne. 0 ) THEN
                    CALL ER_WMSG ( 'GG', ier, ' ', ier1 )
               END IF
C
           END IF
C
C*         Store active watch information.
C
           IF ( ( dowcp ) .and. (itest .eq. 0 )) THEN
               latlon = ' '
C
C*             Extract the FIPS numbers from the WCN county strings.
C
               DO ikl = 1, ncnty 
                   tmpfip = cnties(ikl)(10:15)
                   CALL ST_NUMB ( tmpfip, fipsin(ikl), ier )
               END DO
C
C*             Get the lat/lon of rebundled area for those active
C*             watches.
C
               npout = 0
               IF ( ncnty .gt. 0 ) THEN
                   CALL GG_CWCP ( bndnam, ncnty, fipsin, npout,
     +                            latout, lonout, ier )
               END IF
C
               IF ( npout .gt. 0 ) THEN
                   np = 3
C
C*                 Place lat/lon in single string, each pair separated
C*                 by semi colon.
C
                   DO ik = 1, npout
                       CALL ST_RLCH  ( latout(ik), np, rlstr, ier )
                       CALL ST_LSTR  ( rlstr, ilen, ier )
                       CALL ST_RLCH  ( lonout(ik), np, rlstr1, ier )
                       CALL ST_LSTR  ( rlstr1, ilen1, ier )
                       CALL ST_LSTR  ( latlon, ilen2, ier )
                       latlon = latlon(:ilen2) // rlstr(:ilen) //
     +                              ',' //rlstr1(:ilen1) //';'
                   END DO
C
C*                 Store each active watch information.
C
                   ibun = ibun + 1
                   wtype(ibun)  = type
                   wstart (ibun) = strtim
                   wend (ibun) = stptim
                   CALL ST_INCH ( inumb, wnum(ibun), ier )
                   CALL ST_LDSP ( latlon, latlon, ilen2,ier )
                   wlatlon (ibun) = latlon(:ilen2)
               END IF
C
           END IF
       END DO
C
C
C*     Create the WCP text and vgf files.
C
       IF ( ( dowcp ) .and. (itest .eq. 0 ) ) THEN
C
C*         Place arrays into single strings, '|' separated
C
           typein = ' '
           strtin = ' '
           endin = ' '
           cnum = ' '
           cltln = ' '
C
           IF ( ibun .gt. 0 ) THEN
               DO ik = ibun, 1, -1

                   CALL ST_LSTR  ( wtype(ik), ilen1, ier )
                   CALL ST_LSTR  ( wstart(ik), ilen2, ier )
                   CALL ST_LSTR  ( wend(ik), ilen3, ier )
                   CALL ST_LSTR  ( wnum(ik), ilen4, ier )
                   CALL ST_LSTR  ( wlatlon(ik), ilen5, ier )
C
                   CALL ST_LSTR  ( typein, ilen6, ier )
                   CALL ST_LSTR  ( strtin, ilen7, ier )
                   CALL ST_LSTR  ( endin, ilen8, ier )
                   CALL ST_LSTR  ( cnum, ilen9, ier )
                   CALL ST_LSTR  ( cltln, ilen10, ier )
C
                   typein = typein(:ilen6) // wtype(ik)(:ilen1) // '|'
                   strtin = strtin(:ilen7) // wstart(ik)(:ilen2) // '|'
                   endin = endin(:ilen8) // wend(ik)(:ilen3) // '|'
                   cnum = cnum(:ilen9) // wnum(ik)(:ilen4) // '|'
                   cltln = cltln(:ilen10) // wlatlon(ik)(:ilen5) // '|'
               END DO
           END IF
C
C*         Append nulls to the end of strings.
C
           CALL ST_NULL  ( typein, typein, ilen6, ier )
           CALL ST_NULL  ( strtin, strtin, ilen7, ier )
           CALL ST_NULL  ( endin, endin, ilen8, ier )
           CALL ST_NULL  ( cnum, cnum, ilen9, ier )
           CALL ST_LSTR  ( cltln, ilen10, ier )
           cltln(ilen10+1:ilen10+1) = CHNULL
C

           CALL GG_WCPB (ibun, dattim, typein, strtin, endin,
     +                    cnum, cltln, ier1 )
       END IF

       IF ( ( iunum .eq. 0 ) .and. ( ier .eq. 0 ) ) THEN
           ier = 1
           CALL ER_WMSG  ( 'WOUPDT', ier, ' ', ierr )
       END IF
C*
       END
