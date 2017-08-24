	SUBROUTINE GG_ISIG ( dattim, icolor, ssize, iwidth, lwidth,
     +			     iflags, iret )
C************************************************************************
C* GG_ISIG								*
C*									*
C* This subroutine plots the current international SIGMETs.             *
C*									*
C* GG_ISIG ( DATTIM, ICOLOR, SSIZE, IWIDTH, LWIDTH, IFLAGS, IRET )	*
C*									*
C* Input parameters:							*
C*	DATTIM		CHAR*		Ending time for intl SIGMET     *
C*	ICOLOR (16)	INTEGER		Line and symbol colors		*
C*	SSIZE  (16)	REAL		Symbol sizes			*
C*	IWIDTH (16)	INTEGER		Symbol widths			*
C*	LWIDTH (16)	INTEGER		Line widths			*
C*	IFLAGS (5)	INTEGER		Flags for labels		*
C*					  0 = false			*
C*					  1 = true			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	10/99	                                        *
C* D. Kidwell/NCEP	11/99	Added types TD, TR; call GG_CTRD        *
C* A. Hardy/GSC         11/99   Increase file size from 100 -> 250      *
C* D. Kidwell/NCEP	12/99	Fixed bug in duplicate checking         *
C* D. Kidwell/NCEP	 1/00	Checked that cancel time is before end  *
C* D. Kidwell/NCEP	 3/00	Added non-closed areas, more dupe checks*
C* S. Jacobs/NCEP	 3/00	Changed calling sequence		*
C* D. Kidwell/NCEP	 4/00	Added processing for TEST intl SIGMETs  *
C* F. J. Yen/NCEP	 5/00	Added types VA and MW			*
C* D. Kidwell/NCEP	 5/00	Added attribute processing for VA, MW   *
C* M. Li/GSC             5/00   Added MXNMFL and MXFLSZ                 *
C* J. Wu/GSC             7/00   Added checks for TI_STAN return status	*
C* A. Hardy/GSC          8/00   Added TC type                           *
C* D. Kidwell/NCEP	 8/00	Added save and restore for WTHR         *
C* S. Jacobs/NCEP	 3/01	Increased file template to 48 chars	*
C* S. Jacobs/NCEP	 4/01	Changed flstrt check from gt to ge	*
C* S. Jacobs/NCEP	 5/01	Changed the num of pts for isolated circ*
C* F. J. Yen/NCEP	 8/01	Modified calling sequence for CLO_CMPDIR*
C* A. Hardy/SAIC         9/01   Modified TC and TD label creation	*
C* F. J. Yen/NCEP	11/01	Temporarily, set color to 0 for new	*
C*				phenomena and ignored "OTHER" reports	*
C* F. J. Yen/NCEP	 1/02	Processed new phenomena & "OTHER" repts;*
C*				Test for HU & TR in southern hemisphere	*
C* A. Hardy/SAIC         2/02   Changed call FL_SCND			*
C* A. Hardy/NCEP         9/02   Added check for originating station id  *
C* S. Jacobs/NCEP	 4/03	Changed the icng sym from mod/svr to svr*
C* F. J. Yen/NCEP	 5/00	Added WS type. Allowed for SFC & FZLVL.	*
C* F. J. Yen/NCEP	11/03	Added HU to plot same fields as TC.	*
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM             *
C* T. Lee/SAIC		 9/04	Replaced FL_TMPL with CTB_DTGET		*
C* A. Hardy/NCEP	11/04	Added calls to ST_RNUL; NW 500->1000	*
C* m.gamazaychikov/SAIC 12/04   Added ion flag to CTB_DTGET CS          *
C* m.gamazaychikov/SAIC 01/06   Changed templ string length to MXTMPL   *
C* m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
C* J. Lewis/AWC		09/07   Changes to correctly plot updates	*
C* F. J. Yen/NCEP        4/08   Added bin mins & mstrct to CTB_DTGET CSC*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( NW = 1000,  OFFSET = 30. * 1852. )
C
	PARAMETER	( JSVR = 1,  JTRB = 2,  JHUR = 3,  JTSM = 4,
     +			  JTDP = 5,  JVOA = 6,  JMTW = 7,  JTTC = 8,
     +			  JSQL = 9,  JCAT = 10, JICG = 11, JHGR = 12,
     +			  JDST = 13, JSST = 14, JCLB = 15, JLWS = 16 )
	PARAMETER	( JSYM = 1, JTIM = 2, JMID = 3, JMTN = 4,
     +			  JFLT = 5 )
C*
	CHARACTER*(*)	dattim
	INTEGER		icolor(*), iwidth(*), lwidth(*), iflags(*)
	REAL		ssize(*)
C*
	CHARACTER	path*25, templ*(MXTMPL), cdttm*20, dattm2*20, 
     +			buffer*100, tfile*128, label*80, dattm4*20, 
     +			tmstp4*20, tmstr4*20, carr (11)*20, tid*20, 
     +			ttype*2, tlabel*80, torg*4, tocn
	CHARACTER*(MXFLSZ)      filnam, files (MXNMFL)
C*
	CHARACTER	type (NW)*2, timstr (NW)*20, timstp (NW)*20,
     +			msgid (NW)*20, flevel (NW)*9, dir (NW)*3,
     +			speed (NW)*2, commnt (NW)*20, strmw*3, strws*4,
     +			orgid (NW)*4, updt (NW)*2, ocn (NW)
     
	INTEGER		npt (NW), itest(NW), itype
	REAL		rlat (20,NW), rlon (20,NW), rarr (2)
C*
	CHARACTER	stime*20, flstrt*160, event*2, drct*4, 
     +                  tcarr(2)*4
	INTEGER		itarr (5), jtarr (5)
	LOGICAL		done, found, circle (NW), point
C*
	INCLUDE		'ERMISS.FNC'
C*
	point (event) = ( event .eq. 'HU' ) .or. ( event .eq. 'TR' )
     +			.or. ( event .eq. 'TD' ) 
     +                  .or. ( event .eq. 'TC' )
C-----------------------------------------------------------------------
	iret = 0
C
C*	Scan the directory for all of the international SIGMET data 
C* 	files.
C
	filnam = 'ISIG'  
	path  = ' '
	templ = ' '
	CALL ST_NULL ( filnam, filnam, nf, ier )
	CALL CTB_DTGET ( filnam, path, templ, ic, is, if, ir, ii, ion,
     +          ihb, mnb, iha, mna, mstrct, idtmch, ier )
	CALL ST_RNUL ( templ, templ, ilen, ier )
	CALL ST_RNUL ( path, path, ilen, ier )
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
	    ENDIF	    
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
	nisig = 0
	done  = .false.
	ifl   = 1
	DO WHILE  ( ( ifl .le. nfile ) .and. ( .not. done ) )
	    IF  ( files(ifl) .gt. filnam )  THEN
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
			    CALL ST_CLST ( buffer, '|', ' ', 12,
     +					   carr, num, ier )
C
C*                          If 'num' equal to 12, then the
C*			    update number is in the decoded
C*                          output. Otherwise, the old file format is
C*			    being read.
C
                            IF ( num .eq. 12 ) THEN
                                ione = 2
                              ELSE
                                ione = 1
                            END IF
			    CALL ST_NUMB ( carr ( 10+ione ), jflag, ier )
			    jcorr = MOD ( jflag, 2 )
			    jtest = jflag / 2
			    ttype = carr ( 2 )
			    tid   = carr ( 5 )
			    jsig  = 0
                            IF ( ione .eq. 2 ) THEN
                                torg = carr ( 7 )
                              ELSE
                                torg  = ' '
                            END IF

                            IF ( ( torg .eq. 'KZMA' ) .or.
     +                           ( torg .eq. 'KZWY' ) .or.
     +                           ( torg .eq. 'KZHU' ) .or.
     +                           ( torg .eq. 'TJZS' ) ) THEN
                                  tocn = 'A'
			    ELSE
		                  tocn = ' '
                            END IF
				
			    found = .false.
C
C*			    See if this sigmet has already been received.
C*			    (A correction could include a correction to
C*			    the numeric part of the message id, which
C*			    is checked in the second part of the IF.)
C
			    DO ii = 1, nisig
				CALL TI_DIFF ( timstr (ii), carr (3),
     +					       nmin1, ier )
				CALL TI_DIFF ( timstp (ii), carr (4),
     +					       nmin2, ier )
				nmin1 = IABS ( nmin1 )
				nmin  = nmin2
				nmin2 = IABS ( nmin2 )
C
            			CALL TI_DTM4 ( carr (3), tmstr4, ier )
				CALL TI_DTM4 ( carr (4), tmstp4, ier )
C
				IF ( ( ( msgid ( ii ) .eq. tid  ) .and.
     +				     ( ( orgid ( ii ) .eq. torg ) .or.
     +				       ( ocn ( ii ) .eq. tocn ) ) ) .and.
     +				     ( ( type ( ii ) .eq. ttype ) .or.
     +				       ( ttype .eq. 'CN' ) ) ) 
     +  			   THEN
				    found = .true.
				    IF ( jcorr .eq. 1 ) THEN
C
C*					This is a correction.
C
					IF  ( ( dattim .eq. 'ALL' ) .or.
     +            			      ( ( tmstp4 .gt. dattm4 ) .and.
     +              			        ( tmstr4 .le. dattm4 ) ) )  THEN
					         jsig = ii
					END IF
				      ELSE IF ( ( ttype .eq. 'CN' ) 
     +					   .and. ( nmin .gt. 0 ) ) THEN
C
C*					This is a cancellation.  Update
C*					the end time only.
C
					timstp ( ii ) = carr ( 4 )
				      ELSE
C
C*					This is neither a correction nor
C*					a cancellation; treat it as a
C*					duplicate unless it is a foreign
C*					(unnamed or 'OTHER') sigmet -
C*					then check all fields before
C*					labelling it a duplicate.
C
					IF ( found .and. ( tid ( :7 ) 
     +					        .eq. 'SIGMET' .or.
     +					        commnt(ii) .eq.
     +						'OTHER' ) ) THEN
					  IF ( ( type (ii) .ne. ttype )
     +					  .or. ( nmin1 .gt. 0 ) 
     +					  .or. ( nmin2 .gt. 0 ) 
     +					  .or. ( msgid (ii) .ne. tid ) 
     +					  .or. (flevel(ii) .ne. 
     +							   carr(6+ione))
     +					  .or. ( dir (ii) .ne. 
     +							   carr(7+ione))
     +					  .or. ( speed(ii) .ne. 
     +                                                     carr(8+ione))
     +					  .or. (commnt(ii) .ne. 
     +                                                     carr(9+ione))
     +					     )  found = .false.
                                          IF ( ( ione .eq. 2 ) .and. 
     +                                       ( orgid ( ii ) .ne. torg )) 
     +					     THEN
                                                found = .false.
                                          END IF
					END IF			  
				    END IF
				END IF
			    END DO
C
			    IF ( .not. found ) THEN
C
C*				This is a new report.
C
				nisig = nisig + 1
				jsig  = nisig
			    END IF
C
			    IF ( jsig .gt. 0 ) THEN
				type   ( jsig ) = ttype
				timstr ( jsig ) = carr ( 3 )
				timstp ( jsig ) = carr ( 4 )
				msgid  ( jsig ) = tid
			        flevel ( jsig ) = carr ( 6+ione )
				dir    ( jsig ) = carr ( 7+ione )
				speed  ( jsig ) = carr ( 8+ione )
				commnt ( jsig ) = carr ( 9+ione )
				itest  ( jsig ) = jtest
                                IF ( ione .eq. 2 ) THEN
    				    updt  ( jsig ) = carr ( 6 )
				    orgid ( jsig ) = carr ( 7 )
C
C*				    Find out if this is in the Atlantic
C
				    IF ( ( orgid ( jsig ) .eq. 'KZWY' ) .or.
     +				         ( orgid ( jsig ) .eq. 'KZMA' ) .or.
     +				         ( orgid ( jsig ) .eq. 'KZHU' ) .or.
     +				         ( orgid ( jsig ) .eq. 'TJZS' ) ) THEN
				          ocn ( jsig ) = 'A'		
				    ELSE
                                          ocn ( jsig )= ' '
				    END IF
			        END IF
			    END IF
			END IF
C
C*			Read the lat/lon coordinates from the file.
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
				    IF  ( jsig .gt. 0 )  THEN
					knt = knt + 1
					READ ( buffer, 1000 )
     +					       rlat ( knt, jsig ),
     +					       rlon ( knt, jsig )
1000				    	FORMAT ( 2F9.2 )
				    END IF
				END IF
			    END IF
			END DO
C
			IF ( knt .ne. 0 ) THEN
			  npt ( jsig ) = knt
C
C*			  Check to see if this is a circle.
C 			  
			  IF ( ( knt .ne. 2 ) .or.
     +			      ( .not. ERMISS ( rlon (2, jsig) ) ) ) THEN
C
C*			    It is not a circle.  Check to see if the
C*			    line needs to be closed.
C
			    circle ( jsig ) = .false.
			    IF ( ( rlat(knt,jsig) .ne. rlat(1,jsig) )
     +		             .or.( rlon(knt,jsig) .ne. rlon(1,jsig) ) )
     +			      THEN
			        npt ( jsig ) = knt + 1
			        rlat (npt(jsig), jsig) = rlat (1, jsig)
			        rlon (npt(jsig), jsig) = rlon (1, jsig)
			    END IF
			   ELSE
C
C*			    For a circle, the first point is the center,
C*			    and rlat (2, jsig) is the radius.  Get a
C*			    second point on the circle for plotting.
C
			    circle ( jsig ) = .true.
			    dist = PR_HGNM ( rlat ( 2, jsig ) )
			    CALL CLO_DLTLN ( rlat(1,jsig),rlon (1,jsig),
     +					     dist, 180., rlat(2,jsig),
     +					     rlon(2,jsig), ier )
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
	CALL GQPWTH ( szpwth, jpwwid, ier )
	CALL GQTURB ( szturb, jtuwid, ier )
	CALL GQSPCL ( szspcl, jmkwid, ier )
	CALL GQWTHR ( szwthr, jwtwid, ier )
	CALL GQICNG ( szicng, jicwid, ier )
	CALL GQCTYP ( szctyp, jctwid, ier )
C
C*	Set attributes.
C
	CALL GSTEXT ( 21, 2, 1.0, 1, 111, 1, 1, ier )
C
	DO  i = 1, 16
	    IF  ( ( ssize(i) .le.  0.0 ) .or.
     +		  ( ssize(i) .gt. 10.0 ) )  ssize(i) = 1.5
C
	    IF  ( ( iwidth(i) .le.  0 ) .or.
     +		  ( iwidth(i) .gt. 10 ) )  iwidth(i) = 3
C
	    IF  ( ( lwidth(i) .le.  0 ) .or.
     +		  ( lwidth(i) .gt. 10 ) )  lwidth(i) = 2
	END DO
C
	sytts = 9.
	syttb = 5.
	sytnhu = 26.
	sytshu = 28.
	sytntr = 25.
	sytstr = 27.
	syttd = 33.
	syttc = 34.
	sytva = 201.
	strmw = 'MTW'
	strws = 'LLWS'
	sytsq = 18.
	sytct = 4.
	sytic = 8.
	sytgr = 87.
	sytds = 3.
	sytss = 3.
	sytcb = 9.
C
C*	Plot the graphic for each valid report.
C
	DO ip = 1, nisig
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
		IF  ( type ( ip ) .eq. 'TS' )  THEN
		    ic = icolor(JSVR)
		    CALL GSPWTH ( ssize(JSVR), iwidth(JSVR), ier )
		    CALL GSLINE ( iltyp, 0, lwidth(JSVR), 0, ier )
C
		  ELSE IF  ( type ( ip ) .eq. 'TB' )  THEN
		    ic = icolor(JTRB)
		    CALL GSTURB ( ssize(JTRB), iwidth(JTRB), ier )
		    CALL GSLINE ( iltyp, 0, lwidth(JTRB), 0, ier )
C
		  ELSE IF ( type ( ip ) .eq. 'HU' ) THEN
		    ic = icolor(JHUR)
		    CALL GSSPCL ( ssize(JHUR), iwidth(JHUR), ier )
		    CALL GSLINE ( iltyp, 0, lwidth(JHUR), 0, ier )
C
		  ELSE IF ( type ( ip ) .eq. 'TR' ) THEN
		    ic = icolor(JTSM)
		    CALL GSSPCL ( ssize(JTSM), iwidth(JTSM), ier )
		    CALL GSLINE ( iltyp, 0, lwidth(JTSM), 0, ier )
C
		  ELSE IF ( type ( ip ) .eq. 'TD' ) THEN
		    ic = icolor(JTDP)
		    CALL GSSPCL ( ssize(JTDP), iwidth(JTDP), ier )
		    CALL GSLINE ( iltyp, 0, lwidth(JTDP), 0, ier )
C
		  ELSE IF ( type ( ip ) .eq. 'VA' ) THEN
		    ic = icolor(JVOA)
		    CALL GSWTHR ( ssize(JVOA), iwidth(JVOA), ier )
		    CALL GSLINE ( iltyp, 0, lwidth(JVOA), 0, ier )
C
		  ELSE IF ( type ( ip ) .eq. 'MW' ) THEN
		    ic = icolor(JMTW)
		    CALL GSLINE ( iltyp, 0, lwidth(JMTW), 0, ier )
C
		  ELSE IF ( type ( ip ) .eq. 'TC' ) THEN
		    ic = icolor(JTTC)
		    CALL GSSPCL ( ssize(JTTC), iwidth(JTTC), ier )
		    CALL GSLINE ( iltyp, 0, lwidth(JTTC), 0, ier )
C
		  ELSE IF  ( type ( ip ) .eq. 'SQ' )  THEN
		    ic = icolor(JSQL)
		    CALL GSWTHR ( ssize(JSQL), iwidth(JSQL), ier )
		    CALL GSLINE ( iltyp, 0, lwidth(JSQL), 0, ier )
C
		  ELSE IF  ( type ( ip ) .eq. 'CT' )  THEN
		    ic = icolor(JCAT)
		    CALL GSTURB ( ssize(JCAT), iwidth(JCAT), ier )
		    CALL GSLINE ( iltyp, 0, lwidth(JCAT), 0, ier )
C
		  ELSE IF  ( type ( ip ) .eq. 'IC' )  THEN
		    ic = icolor(JICG)
		    CALL GSICNG ( ssize(JICG), iwidth(JICG), ier )
		    CALL GSLINE ( iltyp, 0, lwidth(JICG), 0, ier )
C
		  ELSE IF  ( type ( ip ) .eq. 'GR' )  THEN
		    ic = icolor(JHGR)
		    CALL GSWTHR ( ssize(JHGR), iwidth(JHGR), ier )
		    CALL GSLINE ( iltyp, 0, lwidth(JHGR), 0, ier )
C
		  ELSE IF  ( type ( ip ) .eq. 'DS' )  THEN
		    ic = icolor(JDST)
		    CALL GSPWTH ( ssize(JDST), iwidth(JDST), ier )
		    CALL GSLINE ( iltyp, 0, lwidth(JDST), 0, ier )
C
		  ELSE IF  ( type ( ip ) .eq. 'SS' )  THEN
		    ic = icolor(JSST)
		    CALL GSPWTH ( ssize(JSST), iwidth(JSST), ier )
		    CALL GSLINE ( iltyp, 0, lwidth(JSST), 0, ier )
C
		  ELSE IF  ( type ( ip ) .eq. 'CB' )  THEN
		    ic = icolor(JCLB)
		    CALL GSCTYP ( ssize(JCLB), iwidth(JCLB), ier )
		    CALL GSLINE ( iltyp, 0, lwidth(JCLB), 0, ier )
C
		  ELSE IF ( type ( ip ) .eq. 'WS' ) THEN
		    ic = icolor(JLWS)
		    CALL GSLINE ( iltyp, 0, lwidth(JLWS), 0, ier )
C
		  ELSE
		    ic = 0

		END IF
C
C*		Draw the shape for a bounded phenomenon and get the
C*		center of the bounded area and the label location.
C
		IF ( ic .ne. 0 ) THEN
		  CALL GSCOLR ( ic, ier )
		  xcen = rlat ( 1, ip )
		  ycen = rlon ( 1, ip )
		  IF  ( .not. point ( type ( ip ) ) .and.
     +			    commnt(ip) .ne. 'OTHER' ) THEN
		    IF ( .not. circle ( ip ) ) THEN
C
C*			Check to see if this is a non-closed area.
C
			IF ( commnt ( ip ) .eq. ' '
     +				.or. type (ip) .eq. 'VA' ) THEN
			    npts = npt ( ip )
			  ELSE
			    npts = npt ( ip ) - 1
			END IF
C
		        CALL GLINE ( 'M', npts , rlat ( 1, ip ),
     +			             rlon ( 1, ip ), ier )
			IF ( type ( ip ) .eq. 'VA' .and.
     +				commnt (ip) .ne. ' ' ) THEN
			    CALL ST_RLST ( commnt (ip), ',', 9999., 2,
     +					   rarr, num, ier )
			    xcen = rarr (1)
			    ycen = rarr (2)
			  ELSE
			    CALL GG_CTRD ( npt ( ip ), rlat ( 1, ip ),
     +				       rlon ( 1, ip ), xcen, ycen, ier )
			END IF
			CALL GG_WLBL ( npt ( ip ), rlat ( 1, ip ), 
     +				       rlon ( 1, ip ), alat, alon, ier )
C
C*			If the phenomenon is defined by only two points,
C*			get an offset for plotting the symbol.
C
			IF ( ( npts .eq. 2 ) .and. 
     +			     ( commnt ( ip ) .ne. ' ' .and.
     +			       type (ip) .ne. 'VA') ) THEN
			    drct = commnt ( ip ) ( :3 )
			    CALL ST_NULL ( drct, drct, lens, ier )
			    CALL CLO_CMPDIR ( drct, bear, ier )
			    IF ( ier .eq. 0 ) THEN
				CALL CLO_DLTLN ( xcen, ycen, OFFSET,
     +						 bear, xcen, ycen, ier )
			    END IF
			END IF
		      ELSE
		        CALL GCIRCL ( 'M', rlat (1, ip), rlon (1, ip),
     +			            rlat (2, ip), rlon (2, ip),
     +				    36, ier )
			alat = rlat ( 2, ip )
			alon = rlon ( 2, ip )
		    END IF
		    ixoff = 0
		  ELSE
		    alat  = xcen
		    alon  = ycen
		    ixoff = 3
		  END IF
C
C*		  Plot the symbol.
C
		  IF ( type ( ip ) .eq. 'HU' ) THEN
C
C*		    Determine whether to use northern or southern
C*		    hemisphere symbol.
C
		    IF ( alat .ge. 0 ) THEN
		      sythu = sytnhu
		     ELSE
		      sythu = sytshu
		    END IF
		    CALL GSPCL ( 'M', 1, sythu, xcen, ycen, 0, 0, ier )
C
		  ELSE IF ( type ( ip ) .eq. 'TR' ) THEN
C
C*		    Determine whether to use northern or southern
C*		    hemisphere symbol.
C
		    IF ( alat .ge. 0 ) THEN
		      syttr = sytntr
		     ELSE
		      syttr = sytstr
		    END IF
		    CALL GSPCL ( 'M', 1, syttr, xcen, ycen, 0, 0, ier )
C
		  ELSE IF ( type ( ip ) .eq. 'TD' ) THEN
		    CALL GSPCL ( 'M', 1, syttd, xcen, ycen, 0, 0, ier )
C
		  ELSE IF ( type ( ip ) .eq. 'TC' ) THEN
		    CALL GSPCL ( 'M', 1, syttc, xcen, ycen-0.05, 0, 0, 
     +                            ier )
C
		  ELSE IF ( iflags(JSYM) .ne. 0 ) THEN
		    IF ( type ( ip ) .eq. 'TS' )  THEN
                        IF ( ( type ( ip-1 ) .eq. 'TC' ) .and. 
     +                      ( (ip-1) .ne. 0 ) ) THEN
		            CALL GPWTH ( 'M', 1, sytts, xcen-0.2, ycen, 
     +				         0, 0,  ier )
                          ELSE
		            CALL GPWTH ( 'M', 1, sytts, xcen, ycen, 
     +				         0, 0,  ier )
                        END IF
		      ELSE IF ( type ( ip ) .eq. 'TB' )  THEN
		        CALL GTURB ( 'M', 1, syttb, xcen, ycen, 0, 0, 
     +				     ier )
		      ELSE IF ( type ( ip ) .eq. 'VA' )  THEN
			CALL GWTHR ( 'M', 1, sytva, xcen, ycen, 0, 0,
     +				     ier )
		      ELSE IF ( type ( ip ) .eq. 'MW' )  THEN
	                CALL GSTEXT ( 21, 2, ssize(JMTW), iwidth(JMTW),
     +				      211, 1, 1, ier )
		        CALL GTEXT ( 'M', xcen, ycen, strmw, 0., -2, 0,
     +				     ier )
	                CALL GSTEXT ( 21, 2, 1.0, 1, 111, 1, 1, ier )
		      ELSE IF ( type ( ip ) .eq. 'SQ' )  THEN
			CALL GWTHR ( 'M', 1, sytsq, xcen, ycen, 0, 0,
     +				     ier )
		      ELSE IF ( type ( ip ) .eq. 'CT' )  THEN
		        CALL GTURB ( 'M', 1, sytct, xcen, ycen, 0, 0, 
     +				     ier )
		      ELSE IF ( type ( ip ) .eq. 'IC' )  THEN
		        CALL GICNG ( 'M', 1, sytic, xcen, ycen, 0, 0, 
     +				     ier )
		      ELSE IF ( type ( ip ) .eq. 'GR' )  THEN
			CALL GWTHR ( 'M', 1, sytgr, xcen, ycen, 0, 0,
     +				     ier )
		      ELSE IF ( type ( ip ) .eq. 'SS' )  THEN
			CALL GPWTH ( 'M', 1, sytss, xcen, ycen, 0, 0,
     +				     ier )
		      ELSE IF ( type ( ip ) .eq. 'DS' )  THEN
			CALL GPWTH ( 'M', 1, sytds, xcen, ycen, 0, 0,
     +				     ier )
		      ELSE IF ( type ( ip ) .eq. 'CB' )  THEN
			CALL GCTYP ( 'M', 1, sytcb, xcen, ycen, 0, 0,
     +				     ier )
		      ELSE IF ( type ( ip ) .eq. 'WS' )  THEN
	                CALL GSTEXT ( 21, 2, ssize(JLWS), iwidth(JLWS),
     +				      211, 1, 1, ier )
		        CALL GTEXT ( 'M', xcen, ycen, strws, 0., -4, 0,
     +				     ier )
	                CALL GSTEXT ( 21, 2, 1.0, 1, 111, 1, 1, ier )
		    END IF
		  END IF
C
C*		  Build the label.
C
		  label  = CHNULL
		  lenlab = 1
		  lines  = 1
C
                  IF ( iflags(JMID) .ne. 0 ) THEN
		    CALL ST_LSTR ( msgid ( ip ), lenid, ier )
		    CALL ST_LSTR ( updt ( ip ), lenupd, ier )
		    label =  msgid ( ip ) ( :lenid ) // updt ( ip ) ( :lenupd )
		    lenlab = lenid + lenupd
		  END IF
C
		  IF ( iflags(JTIM) .ne. 0 ) THEN
		    IF ( lenlab .ne. 1 ) THEN
			tlabel = label ( :lenlab )
			label  = tlabel ( :lenlab ) // CHCR
			lenlab = lenlab + 1
		        lines  = lines + 1
		    END IF
		    tlabel = label ( :lenlab )
		    label  = tlabel (:lenlab ) // timstr ( ip ) ( 5:11 )
     +			     // '-' // timstp ( ip ) ( 5:11 )
		    lenlab = lenlab + 15
		  END IF
C
		  IF ( iflags(JMTN) .ne. 0 ) THEN
		    CALL ST_LSTR ( dir ( ip ), lendir, ier )
		    CALL ST_LSTR ( speed ( ip ), lenspd, ier )
		    IF ( ( lendir .gt. 0 ) .or. ( lenspd .gt. 0 ) ) THEN
		        IF ( lenlab .ne. 1 ) THEN
			    tlabel = label ( :lenlab )
			    label  = tlabel ( :lenlab ) // CHCR
			    lenlab = lenlab + 1
		            lines  = lines + 1
		        END IF
			tlabel = label ( :lenlab )
		        IF ( ( dir ( ip ) ( :1 ) .ne. '0' ) .or.
     +			     ( speed ( ip ) ( :1 ) .ne. '0' ) ) THEN
		            label  = tlabel ( :lenlab ) // 
     +				     dir ( ip ) ( :lendir ) // '/' //
     +			             speed ( ip ) ( :lenspd ) // 'KT'
		            lenlab = lenlab + lendir + lenspd + 3
		          ELSE
			    label  = tlabel ( :lenlab ) // 'STNR'
			    lenlab = lenlab + 4
		        END IF
		    END IF
		  END IF
C
		  IF ( iflags(JFLT) .ne. 0 ) THEN
		    CALL ST_LSTR ( flevel ( ip ), lenflv, ier )
		    IF ( lenflv .ne. 0 ) THEN
		        IF ( lenlab .ne. 1 ) THEN
			    tlabel = label ( :lenlab )
			    label  = tlabel ( :lenlab ) // CHCR
			    lenlab = lenlab + 1
		            lines  = lines + 1
		        END IF
                        IF ( (type (ip) .ne. 'TC' ) .and. 
     +                       (type (ip) .ne. 'TD' ) .and.
     +                       (type (ip) .ne. 'HU' ) .and.
     +			     (type (ip) .ne. 'TR' )) THEN
			    tlabel = label ( :lenlab )
			    IF ( flevel (ip) (:3) .eq. 'SFC' .or.
     +				    flevel (ip) (:5) .eq. 'FZLVL' ) THEN
				label = tlabel ( :lenlab ) //
     +			    	    flevel ( ip ) ( :lenflv )
				lenlab = lenlab + lenflv
			      ELSE
		                label  = tlabel ( :lenlab ) // 'FL' //
     +			            flevel ( ip ) ( :lenflv )
		                lenlab = lenlab + lenflv + 2
			    END IF
C 
C*                        Label for tropical cyclone and hurricane.
C
                          ELSE 
                            CALL ST_CLST ( flevel ( ip ) ( :lenflv ),
     +                                   '-','-', 2, tcarr, icnm, ier )
                            
		            CALL ST_LSTR ( tcarr(1), len1, ier )
		            CALL ST_LSTR ( tcarr(2), len2, ier )
			    tlabel = label ( :lenlab )
C
                            IF ( (tcarr(1)(:len1) .ne. '-') .and.
     +                              (tcarr(2)(:len2) .ne. '-') ) THEN
		                label  = tlabel ( :lenlab ) //
     +                               tcarr(1)(:len1)// 'mb-' // 
     +			             tcarr(2)(:len2)// 'KT'
                              ELSE IF (tcarr(2)(:len2) .ne. '-')  THEN
		                label  = tlabel ( :lenlab ) //
     +                                   tcarr(2)(:len2)// 'KT'
                              ELSE IF  (tcarr(1)(:len1) .ne. '-') THEN
		                label  = tlabel ( :lenlab ) // 
     +                                   tcarr(1)(:len1)// 'mb' 
                            END IF
		            CALL ST_LSTR ( label, len3, ier )
		            lenlab = lenlab + lenflv + 4
                        END IF
		    END IF
		  END IF
C
		  IF ( iflags(JSYM) .ne. 0 ) THEN
		    IF ( point ( type ( ip ) ) ) THEN
		        IF ( lenlab .ne. 1 ) THEN
			    tlabel = label ( :lenlab )
			    label  = tlabel ( :lenlab ) // CHCR
			    lenlab = lenlab + 1
		            lines  = lines + 1
		        END IF
		        CALL ST_LSTR ( commnt ( ip ), lencmt, ier )
			tlabel = label ( :lenlab )
			IF ( commnt (ip) .ne. 'OTHER' ) THEN
		             label =  tlabel ( :lenlab ) // 
     +				     commnt (ip) ( :lencmt )
			     lenlab = lenlab + lencmt
			  ELSE
		             label =  tlabel ( :lenlab ) 
			END IF
		    END IF
		  END IF
C
C*		  For a single point phenomenon, prefix a TEST report 
C*		  label with the character T.
C
		  IF ( point ( type ( ip ) ) .and. 
     +		       ( itest ( ip ) .eq. 1 ) ) THEN
		    tlabel = label ( :lenlab )
		    label  = 'T' // tlabel
		  END IF
C
		  iyoff = - ( lines + 1 )
C
C*		  Plot the text at the lowest point of the bounded area,
C*		  or adjacent to the symbol if there is no bounded area.
C
		  CALL GTEXT ( 'M', alat, alon, label, 0.0,
     +			       ixoff, iyoff, ier )
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
	CALL GSPWTH ( szpwth, jpwwid, ier )
	CALL GSTURB ( szturb, jtuwid, ier )
	CALL GSSPCL ( szspcl, jmkwid, ier )
	CALL GSWTHR ( szwthr, jwtwid, ier )
	CALL GSICNG ( szicng, jicwid, ier )
	CALL GSCTYP ( szctyp, jctwid, ier )
C*
	RETURN
	END

