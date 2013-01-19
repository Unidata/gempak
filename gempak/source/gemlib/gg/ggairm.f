	SUBROUTINE GG_AIRM ( dattim, icolor, ssize, iwidth, lwidth, 
     +			     itbclr2, lvlfl, lvfil, iflags, iret )
C************************************************************************
C* GG_AIRM								*
C*									*
C* This subroutine plots the current airmets for the lower 48 states    *
C* and the adjacent coastal waters.                                     *
C*									*
C* GG_AIRM ( DATTIM, ICOLOR, SSIZE, IWIDTH, LWIDTH, ITBVCLR2, LVLFL,	*
C* 	     LVFIL, IFLAGS, IRET )					*
C*									*
C* Input parameters:							*
C*	DATTIM		CHAR*		Ending time for airmet          *
C*	ICOLOR (5)	INTEGER		Line and symbol colors		*
C*	SSIZE  (5)	REAL		Symbol sizes			*
C*	IWIDTH (5)	INTEGER		Symbol widths			*
C*	LWIDTH (5)	INTEGER		Line widths			*
C*	ITBCLR2		INTEGER		Color2 for turbulence		*
C*	LVLFL		INTEGER		Flight level			*
C*	LVFIL  (2)	INTEGER		Lower/upper filter level	*
C*	IFLAGS (4)	INTEGER		Flags for labels		*
C*					  0 = false			*
C*					  1 = true			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 7/00	                                        *
C* D. Kidwell/NCEP	 8/00	Corrected comment for label location    *
C* S. Jacobs/NCEP	 3/01	Increased file template to 48 chars	*
C* S. Jacobs/NCEP	 4/01	Changed flstrt check from gt to ge	*
C* S. Jacobs/NCEP	11/01	Increased NW from 500 to 1000		*
C* S. Jacobs/NCEP	11/01	Increased number of points from 20 to 50*
C* S. Jacobs/NCEP	11/01	Added check for > NW reports		*
C* A. Hardy/SAIC         2/02   Changed call FL_SCND			*
C* D. Kidwell/NCEP	 5/02	Improved amdt, corr, cancel processing  *
C* M. Li/SAIC		 5/03	Added color2 and level filter		*
C* A. Hardy/NCEP	10/03	Fixed low level turb filter & flt level *
C* A. Hardy/NCEP	11/03	Fixed flt lvl check for lvfil(2)  gt->ge*
C* m.gamazaychikov/SAIC 01/04   Added type SW, added check for missing 	*
C*				flight level				*
C* D. Kidwell/NCEP	 3/04	Changed iyoff for SW time label         *
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM             *
C* m.gamazaychikov/SAIC 08/04   Added selection of beginning valid time *
C*                              or issue time to plot data              *
C* T. Lee/SAIC		 9/04	Replaced FL_TMPL with CTB_DTGET		*
C* A. Hardy/NCEP	11/04   Added calls to ST_RNUL			*
C* m.gamazaychikov/SAIC 12/04   Added ion flag to CTB_DTGET CS          *
C* D. Kidwell/NCEP	 7/05	Fixed bug in cor, cancel processing     *
C* m.gamazaychikov/SAIC 01/06   Changed templ string length to MXTMPL   *
C* m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
C* J. Lewis/AWC		02/07	Added check for LLWS (WS) reports
C* F. J. Yen/NCEP	 4/08	Added bin mins & mstrct to CTB_DTGET CSC*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( NW = 1000 )
C*
	PARAMETER	( NR = 6 )
C
	PARAMETER	( JIFR = 1, JMTO = 2, JTRB = 3, JICE = 4, 
     +                    JSSW = 5, JLWS = 6 )
	PARAMETER	( JSYM = 1, JTIM = 2, JFLT = 3, JSTR = 4 )
C*
	CHARACTER*(*)	dattim
	INTEGER		icolor(*), iwidth(*), lwidth(*), iflags(*), 
     +			lvfil(*)
	REAL		ssize(*)
C*
	CHARACTER	path*25, templ*(MXTMPL), cdttm*20, dattm2*20, 
     +			buffer*100, tfile*128, label*4, dattm4*20, 
     +			tmstp4*20, tmstr4*20, carr (11)*20, tid*4, 
     +			ttype*2, bvdtim*20 
	CHARACTER*(MXFLSZ)      filnam, files (MXNMFL)
C*
	CHARACTER	type (NW)*2, timstr (NW)*20, timstp (NW)*20,
     +			waid (NW)*4, flevel (2,NW)*4, strir*3, strmo*4,
     +                  strws*4, timiss (NW)*20
	INTEGER		npt (NW), itest (NW), iupdt (NW), level(2,NW)
	REAL		rlat (50,NW), rlon (50,NW), tlat (50), tlon (50)
C*
	CHARACTER	stime*20, flstrt*160
	INTEGER		itarr (5), jtarr (5), itype
	LOGICAL		done, flvls, filter 
C-----------------------------------------------------------------------
	iret = 0
C
C*	Scan the directory for all of the airmet data files.
C
	filnam = 'AIRM'
	path  = ' '
	templ = ' '
	CALL ST_NULL ( filnam, filnam, nf, ier )
	CALL CTB_DTGET ( filnam, path, templ,
     +			 ic, is, if, ir, ii, ion, ihb, mnb, iha, mna,
     +			 mstrct, idtmch,ier)
	CALL ST_RNUL ( path, path, lens, ier )
	CALL ST_RNUL ( templ, templ, lens, ier )
	CALL ST_LSTR ( path, lenp, ier )
        nexp   = MXNMFL
	iorder = 1
	CALL FL_SCND ( path, templ, iorder, nexp, files, nfile, ier )
C
C*	Check for the last file requested by the user.
C
	CALL ST_LCUC ( dattim, dattim, ier )
	itype = 1
	IF  ( ( dattim .eq. 'LAST' ) .or.
     +	      ( dattim .eq. 'ALL' ) )  THEN
	    CALL CSS_GTIM ( itype, dattm2, ier )
	    CALL FL_MNAM ( dattm2, templ, filnam, ier )
	  ELSE
	    CALL CSS_GTIM ( itype, cdttm, ier )
	    CALL TI_STAN ( dattim, cdttm, dattm2, ier )
            IF ( ier .ne. 0 ) THEN
		CALL ER_WMSG ( 'TI', ier, dattim, ierr )
		iret = ier
		return
	    END IF        
	    CALL FL_MNAM ( dattm2, templ, filnam, ier )
	END IF
C
C*	Find the earliest file to start searching. For ALL times
C*	go back 10 days, for any other entry for dattim subtract
C*	1 day from the time given.
C
	IF  ( dattim .eq. 'ALL' )  THEN
	    minuts = 14400
	  ELSE
	    minuts = 1440
	END IF
	CALL TI_CTOI ( dattm2, itarr, ier )
	CALL TI_SUBM ( itarr, minuts, jtarr, ier )
	CALL TI_ITOC ( jtarr, stime, ier )
	CALL FL_MNAM ( stime, templ, flstrt, ier )
C
C*	Get 4-digit year to compare dates.
C
	CALL TI_DTM4 ( dattm2, dattm4, ier )
C
C*	Decode each file until the end time is reached.
C
	nairm = 0
	done  = .false.
	ifl   = 1
	DO WHILE  ( ( ifl .le. nfile ) .and. ( .not. done ) )
	    IF  ( files(ifl) .gt. filnam )  THEN
		done = .true.
	      ELSE IF  ( nairm .ge. NW )  THEN
		done = .true.
	      ELSE
		IF  ( files (ifl) .ge. flstrt )  THEN
		  tfile = path ( :lenp ) // '/' // files ( ifl )
		  CALL FL_SOPN ( tfile, lunf, ier )
C
		  iostat = 0
C
C*		  Loop on the records in the file.
C
		  DO WHILE  ( iostat .eq. 0 )
		    READ ( lunf, 2, IOSTAT = iostat ) buffer
2		    FORMAT ( A )
		    IF  ( iostat .eq. 0 )  THEN
		      IF  ( buffer ( 1:1 ) .eq. '|' )  THEN
			  CALL ST_CLST ( buffer, '|', ' ', 11, carr,
     +					 num, ier )
C
C*                        Determine if it is a new or old format AIRMET.
C
                          IF ( num .lt. 11 ) THEN
C
C*                           Make changes to the AIRMET of old format.
C
                             DO ind = 11, 6, -1
                                 carr (ind) = carr (ind-1)
                             END DO
C
C*			     Adjust actual ending valid time by 15 min.
C
                             CALL TI_CTOI ( carr (4), itarr, ier )
                             CALL TI_ADDM ( itarr, 15, itarr, ier )
                             CALL TI_ITOC ( itarr, carr (5), ier )
C
C*			     Compute actual starting valid time.
C
                             CALL TI_CTOI ( carr (5), itarr, ier )
                             CALL TI_SUBM ( itarr, 360, itarr, ier )
                             CALL TI_ITOC ( itarr, bvdtim, ier )
                             CALL TI_DIFF ( carr (3), bvdtim, nmin, ier)
                             CALL ST_NUMB ( carr (10), if1, ier)
                             CALL ST_NUMB ( carr (11), if2, ier)
                             IF ( if1 .gt. 0 .or. if2 .gt. 0 ) THEN
C
C*                               For a corrected or amended AIRMET
C*                               issued _after_ the beginning valid time
C*                               of a regular AIRMET, actual starting
C*			         valid time is equal to its issue time.
C
                                 IF ( nmin .ge. 0 ) carr (4) = carr (3)
C
C*                               For a corrected or amended AIRMET
C*                               issued _before_ the beginning valid time
C*                               of a regular AIRMET, actual starting
C*                               valid time is equal to the beginning
C*                               valid time of the corresponding
C*                               regular AIRMET.
C
                                 IF ( nmin .lt. 0 ) carr (4) = bvdtim
                               ELSE
C
C*                               For a regular AIRMET actual starting
C*                               valid time is equal to the beginning
C*                               valid time of the regular AIRMET.
C
                                 carr (4) = bvdtim
                             END IF 
                          END IF
C
			  CALL ST_NUMB ( carr ( 11 ), jcancl, ier )
			  CALL ST_NUMB ( carr ( 10 ), jflag, ier )
			  jcorr = MOD ( jflag, 3 )
			  jtest = jflag / 3
			  ttype = carr ( 2 )
			  tid   = carr ( 6 )
			  CALL ST_NUMB ( carr ( 7 ), jupdt, ier )
			  IF ( ier .lt. 0 ) jupdt = 0
C
			  IF ( ( jcorr .eq. 2 ) .or. ( jcancl .eq. 1 ) )
     +			       THEN
C
C*			      This is an amendment or a cancellation.
C*			      Change the end time of the original airmet
C*			      to the amendment issue time or to the 
C*			      cancellation issue time.
C
			      DO ii = 1, nairm
				  IF ( ( tid .eq. waid (ii) ) .and.
     +				       (carr (5) .eq. timstp (ii)) .and.
     +				       ( ttype .eq. type (ii) ) .and.
     +				       ( jupdt .gt. iupdt (ii) ) ) THEN
				      timstp ( ii ) = carr ( 3 )
				  END IF
			      END DO
			    ELSE IF ( jcorr .eq. 1 ) THEN
C
C*			      This is a correction.  Change the end time
C*			      of the original airmet to the correction
C*			      issue or start time.
C
			      DO ii = 1, nairm
				  IF ( ( tid .eq. waid (ii) ) .and.
     +				       (carr (5) .eq. timstp (ii)) .and.
     +				       (carr (3) .ne. timiss (ii)) .and.
     +				       ( ttype .eq. type (ii) ) .and.
     +				       ( jupdt .eq. iupdt (ii) ) ) THEN
C
C*                                    End time for original depends on
C*                                    the user specified flag.
C
			              IF ( iflags ( JSTR ) .eq. 1 ) THEN
				          timstp ( ii ) = carr ( 3 )
                                        ELSE
				          timstp ( ii ) = carr ( 4 )
                                      END IF
				  END IF
			      END DO
 			  END IF
C
C*			  Read the lat/lon coordinates from the file.
C
			  knt = 0
			  jostat = 0
			  DO WHILE  ( jostat .eq. 0 )
			    READ ( lunf, 2, IOSTAT = jostat ) buffer
			    IF  ( jostat .eq. 0 )  THEN
				IF  ( buffer ( 1:1 ) .eq. '|' )  THEN
				    CALL FL_BKSP ( lunf, ier )
				    jostat = -1
				  ELSE
				    knt = knt + 1
				    READ ( buffer, 1000 ) 
     +					   tlat ( knt ), tlon ( knt )
1000				    FORMAT ( 2F9.2 )
				END IF
			    END IF
			  END DO
C
C*			  Add this report if it is not a cancellation.
C
			  IF ( jcancl .eq. 0 ) THEN
			    nairm               = nairm + 1    
			    type ( nairm )      = ttype
C
C*                          Set the starting time for plotting AIRMET
C*                          based on the user specified flag.
C
			    IF ( iflags ( JSTR ) .eq. 1 ) THEN
                                timstr ( nairm ) = carr ( 3 )
                              ELSE IF ( iflags ( JSTR ) .eq. 0 ) THEN
                                timstr ( nairm ) = carr ( 4 )
                            END IF
			    timiss ( nairm )    = carr ( 3 )
			    timstp ( nairm )    = carr ( 5 )
			    waid ( nairm )      = tid
			    flevel ( 1, nairm ) = carr ( 8 )
			    flevel ( 2, nairm ) = carr ( 9 ) 
			    CALL ST_NUMB (flevel(1, nairm), 
     +			                  level(1, nairm), ier)
                            CALL ST_NUMB (flevel(2, nairm), 
     +                                    level(2, nairm), ier)
			    itest ( nairm )     = jtest  		 
			    iupdt ( nairm )     = jupdt  		 
			    DO kk = 1, knt
				rlat ( kk, nairm ) = tlat ( kk )
				rlon ( kk, nairm ) = tlon ( kk )
			    END DO
			    npt ( nairm ) = knt
			  END IF
		      END IF
		    END IF
		  END DO
C
		  CALL FL_CLOS ( lunf, ier )
C
		END IF
C
	    END IF
	    ifl = ifl + 1
	END DO
C
C*	Query current settings.
C
	CALL GQCOLR ( jcolr, ier )
	CALL GQLINE ( jtype, jtyhw, jwidth, jwdhw, ier )
	CALL GQTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr,
     +		      jrrotn, jjust, ier )
	CALL GQTURB ( szturb, jtuwid, ier )
	CALL GQICNG ( szicng, jicwid, ier )
	CALL GQSPCL ( szsswn, jswwid, ier )
C
C*	Set attributes.
C
	CALL GSTEXT ( 21, 2, 1.0, 1, 111, 1, 1, ier )
C
	DO  i = 1, NR
	    IF  ( ( ssize (i) .le.  0.0 ) .or.
     +		  ( ssize (i) .gt. 10.0 ) ) THEN
		IF ( ( i .eq. JTRB ) .or. ( i .eq. JICE ) .or.
     +               ( i .eq. JSSW ) .or. ( i .eq. JLWS ) ) THEN
		    ssize (i) = 1.5
		  ELSE
		    ssize (i) = 1.0
		END IF
	    END IF
C
	    IF  ( ( iwidth (i) .le.  0 ) .or.
     +		  ( iwidth (i) .gt. 10 ) ) THEN
		IF ( ( i .eq. JTRB ) .or. ( i .eq. JICE ) .or.
     +               ( i .eq. JSSW ) .or. ( i .eq. JLWS ) ) THEN
		    iwidth (i) = 2
		  ELSE
		    iwidth (i) = 1
		END IF
	    END IF
C
	    IF  ( ( lwidth (i) .le.  0 ) .or.
     +		  ( lwidth (i) .gt. 10 ) )  lwidth (i) = 2
	END DO
C
	syttb = 4.
	sytic = 5.
	syssw = 50.
	strir = 'IFR'
	strmo = 'MTOS'
	strws = 'LLWS'
C
C*	Plot the graphic for each valid report.
C
	DO ip = 1, nairm
C
C*	  Filter the levels.
C
	  filter = .false.
	  IF ( lvfil(1) .gt. 0 ) THEN
	      IF ( level(2,ip) .gt. 0 .and. level(2,ip) .le. lvfil(1))
     +            filter = .true.
	      IF ( level(1,ip) .gt. 0 .and. level(2,ip) .le. 0 .and.
     +		  level(1,ip) .lt. lvfil(1) ) filter = .true.
	  END IF
C
	  IF ( lvfil(2) .gt. 0 ) THEN
	      IF ( level(1,ip) .ge. lvfil(2) ) filter = .true.
              IF ( level(2,ip) .gt. 0 .and. level(1,ip) .le. 0 .and.
     +             level(2,ip) .gt. lvfil(2) ) filter = .true.
          END IF
C* 
	  IF ( .not. filter ) THEN 
	    CALL TI_DTM4 ( timstp ( ip ), tmstp4, ier )
	    CALL TI_DTM4 ( timstr ( ip ), tmstr4, ier )
	    IF  ( ( dattim .eq. 'ALL' ) .or.
     +		  ( ( tmstp4 .gt. dattm4 ) .and.
     +		    ( tmstr4 .le. dattm4 ) ) )  THEN
C
C*		Set the line type for a test or an actual report.
C
		IF ( itest ( ip ) .eq. 0 ) THEN
		    iltyp = 1
		  ELSE
		    iltyp = 2
		END IF
C
C*		Set the color and symbol based on the phenomenon type.
C
		IF  ( type ( ip ) .eq. 'IR' )  THEN
		    ic = icolor (JIFR)
		    CALL GSLINE ( iltyp, 0, lwidth (JIFR), 0, ier )
C
		  ELSE IF  ( type ( ip ) .eq. 'MO' )  THEN
		    ic = icolor (JMTO)
		    CALL GSLINE ( iltyp, 0, lwidth (JMTO), 0, ier )
C
		  ELSE IF ( type ( ip ) .eq. 'TB' ) THEN
		    ic = icolor (JTRB)
                    IF (lvlfl .ge. 0 ) THEN
		       IF ( level(1,ip) .ge. lvlfl ) ic = itbclr2
		       IF ( level(1,ip) .le. 0 .and. level(2,ip)
     +			    .gt. lvlfl ) ic = itbclr2
                    END IF

		    CALL GSTURB ( ssize (JTRB), iwidth (JTRB), ier )
		    CALL GSLINE ( iltyp, 0, lwidth (JTRB), 0, ier )
C
		  ELSE IF ( type ( ip ) .eq. 'IC' ) THEN
		    ic = icolor (JICE)
		    CALL GSICNG ( ssize (JICE), iwidth (JICE), ier )
		    CALL GSLINE ( iltyp, 0, lwidth (JICE), 0, ier )
		  ELSE IF ( type ( ip ) .eq. 'SW' ) THEN
		    ic = icolor (JSSW)
		    CALL GSSPCL ( ssize (JSSW), iwidth (JSSW), ier )
		    CALL GSLINE ( iltyp, 0, lwidth (JSSW), 0, ier )
		  ELSE IF ( type ( ip ) .eq. 'WS' ) THEN
		    ic = icolor (JLWS)
		    CALL GSLINE ( iltyp, 0, lwidth (JLWS), 0, ier )
		END IF
C
C*		Draw the shape for a bounded phenomenon and get the
C*		label location.
C
		IF ( ( ic .ne. 0 ) .and. ( npt ( ip ) .ne. 0 ) ) THEN
		    npts = npt ( ip )
		    CALL GSCOLR ( ic, ier )
		    CALL GLINE ( 'M', npts , rlat ( 1, ip ),
     +			         rlon ( 1, ip ), ier )
C
		    IF ( ( iflags ( JSYM ) .ne. 0 ) .or.
     +			 ( iflags ( JFLT ) .ne. 0 ) .or.
     +			 ( iflags ( JTIM ) .ne. 0 ) ) THEN
		        iyoff = -2
			flvls = .false.
		        CALL GG_WLBL ( npts, rlat ( 1, ip ),
     +				       rlon ( 1, ip ), alat, alon, ier )
			IF ( ( type ( ip ) .eq. 'TB' ) .or.
     +			     ( type ( ip ) .eq. 'IC' ) ) flvls = .true.
C
		        IF ( iflags ( JSYM ) .ne. 0 ) THEN
C
C*		          Plot the symbol.
C
			  IF ( ( iflags ( JFLT ) .gt. 0 ) .and.
     +			       flvls )  iyoff = -3
			  ixoff = 0
		          IF ( type ( ip ) .eq. 'IR' )  THEN
	                    CALL GSTEXT ( 21, 2, ssize(JIFR), 
     +					  iwidth(JIFR), 111, 1, 1, ier )
		            CALL GTEXT ( 'M', alat, alon, strir, 0.,
     +				         ixoff, iyoff, ier )
	                    CALL GSTEXT ( 21, 2, 1.0, 1, 111, 1, 1, 
     +					  ier )
		           ELSE IF ( type ( ip ) .eq. 'MO' )  THEN
	                    CALL GSTEXT ( 21, 2, ssize(JMTO), 
     +					  iwidth(JMTO), 111, 1, 1, ier )
		            CALL GTEXT ( 'M', alat, alon, strmo, 0.,
     +				         ixoff, iyoff, ier )
	                    CALL GSTEXT ( 21, 2, 1.0, 1, 111, 1, 1, 
     +					  ier )
		           ELSE IF ( type ( ip ) .eq. 'TB' )  THEN
		            CALL GTURB ( 'M', 1, syttb, alat, alon,
     +				         ixoff, iyoff, ier )
		           ELSE IF ( type ( ip ) .eq. 'IC' )  THEN
	                    CALL GICNG ( 'M', 1, sytic, alat, alon,
     +				         ixoff, iyoff, ier )
		           ELSE IF ( type ( ip ) .eq. 'SW' )  THEN
	                    CALL GSPCL ( 'M', 1, syssw, alat, alon,
     +				         ixoff, iyoff, ier )
			    iyoff = iyoff - 2
		           ELSE IF ( type ( ip ) .eq. 'WS' )  THEN
			    CALL GSTEXT ( 21, 2, ssize(JLWS),
     +                                    iwidth(JLWS), 111, 1, 1, ier )
                            CALL GTEXT ( 'M', alat, alon, strws, 0.,
     +                                   ixoff, iyoff, ier )
                            CALL GSTEXT ( 21, 2, 1.0, 1, 111, 1, 1,
     +                                    ier )
		          END IF
			  iyoff = iyoff - 2
		        END IF
C
		        IF ( iflags ( JFLT ) .ne. 0 ) THEN
			  IF ( flvls ) THEN
C
C*			    Plot the flight level(s).
C
			    ixoff = 4
			    iyoff = -5
			    DO ii = 1, 2
		                label  = flevel ( ii, ip )
		                CALL GTEXT ( 'M', alat, alon, label, 0.,
     +				             ixoff, iyoff, ier )
				iyoff = iyoff + 4
			    END DO
			    IF ( flevel ( 1, ip ) .ne. ' ' ) THEN
				iyoff = -7
			      ELSE
				iyoff = -5
			    END IF
			  END IF
		        END IF
C
		        IF ( iflags ( JTIM ) .ne. 0 ) THEN
C
C*		          Plot the time.
C
		          label  = timstp ( ip ) ( 8:11 )
		          CALL GTEXT ( 'M', alat, alon, label, 0.0, 0, 
     +					iyoff, ier )
			END IF
		    END IF
		END IF
	    END IF
	  END IF
	END DO	
C
C*	Reset the saved attributes.
C
	CALL GSCOLR ( jcolr, ier )
	CALL GSLINE ( jtype, jtyhw, jwidth, jwdhw, ier )
	CALL GSTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr,
     +		      jrrotn, jjust, ier )
	CALL GSTURB ( szturb, jtuwid, ier )
	CALL GSICNG ( szicng, jicwid, ier )
	CALL GSSPCL ( szsswn, jswwid, ier )
C*
	RETURN
	END
