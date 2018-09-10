	SUBROUTINE GG_WARN ( filtyp, dattim, numm, icolor, mrktyp, ssize,
     +			     iwidth, iflags, iret )
C************************************************************************
C* GG_WARN								*
C*									*
C* This subroutine plots the current thunderstorm, tornado and flash	*
C* flood warnings.  It also plots the current SLS thunderstorm and	*
C* tornado watches.							*
C*									*
C* GG_WARN ( FILTYP, DATTIM, NUMM, ICOLOR, MRKTYP, SSIZE, IWIDTH,	*
C*	   IFLAGS, IRET )						*
C*									*
C* Input parameters:							*
C*	FILTYP		CHAR*		File type 'WARN' or 'SVRL'	*
C*	DATTIM		CHAR*		Ending time for warning or watch*
C*	NUMM		INTEGER		Number of markers		*
C*	ICOLOR (NUM)	INTEGER		Marker symbol colors		*
C*	MRKTYP (NUM)	INTEGER		Marker symbol types		*
C*	SSIZE  (NUM)	REAL		Marker symbol sizes		*
C*	IWIDTH (NUM)	INTEGER		Marker symbol widths		*
C*	IFLAGS (4)	INTEGER		Flags for labels, outline	*
C*					  0 = false			*
C*					  1 = true			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 5/99	Copied from GG_WTCH			*
C* S. Jacobs/NCEP	 5/99	Increased file name array size to 500	*
C* A. Hardy/GSC		 6/99	Added label flag and marker type        *
C* S. Jacobs/NCEP	 6/99	Increased array sizes, NW, to 5000	*
C* S. Jacobs/NCEP	 8/99	Changed call to FL_SCND			*
C* S. Jacobs/NCEP	 8/99	Changed call to FL_TMPL			*
C* S. Jacobs/NCEP	11/99	Increased buffer from 100 to 128	*
C* S. Jacobs/NCEP	 3/00	Changed calling sequence		*
C* D. Kidwell/NCEP	 4/00	Added processing for TEST warnings      *
C* M. Li/GSC		 5/00	Added MXFLSZ and MXNMFL			*
C* J. Wu/GSC             7/00   Added checks for TI_STAN return status	*
C* F. J. Yen/NCEP	 1/01	Changed to handle SLS watches		*
C* F. J. Yen/NCEP	 1/01	Increased NC;called ER_WMSG when over NC*
C* S. Jacobs/NCEP	 3/01	Increased file template to 48 chars	*
C* F. J. Yen/NCEP	 4/01	Fixed bug to handle corrections		*
C* S. Jacobs/NCEP	 4/01	Changed flstrt check from gt to ge	*
C* S. Jacobs/NCEP	 8/01	Added check for total num warn vs. NW	*
C* S. Jacobs/NCEP	 8/01	Changed NW to 1000 and NC to 100	*
C* A. Hardy/SAIC	12/01   Added check for missing counties	*
C* R. Tian/SAIC		02/02	Added new argument num			*
C* T. Piper/SAIC	02/02	Fixed #534P1.  num was already used	*
C* A. Hardy/SAIC         2/02   Changed call FL_SCND			*
C* D.W.Plummer/NCEP	 9/02	Added capability to fill counties	*
C* D. Kidwell/NCEP	11/02	Changed fill to outline                 *
C* D. Kidwell/NCEP	11/02	Set outline width to iwidth, default=3  *
C* M. Li/SAIC		07/03	Color code SVRL watches			*
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM             *
C* A. Hardy/NCEP	 6/04	Read files within a search window	*
C* T. Lee/SAIC		 9/04	Replaced FL_TMPL with CTB_DTGET		*
C* A. Hardy/NCEP	11/04	Added calls to ST_RNUL			*
C* m.gamazaychikov/SAIC 12/04   Added ion flag to CTB_DTGET CS          *
C* m.gamazaychikov/SAIC 01/06   Changed templ string length to MXTMPL   *
C* m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
C* T. Piper/SAIC	07/07	Added reading of the warning polygon	*
C* T. Piper/SAIC	07/07	Made the polygon option dominant	*
C* F. J. Yen/NCEP	 3/08	Corrected index for polygon;removed dups*
C*				to fix bug with corrections; used ETN in*
C*				corrections; processed correction letter*
C* F. J. Yen/NCEP        4/08   Added bin mins & mstrct to CTB_DTGET CSC*
C* S. GUAN/NCEP         11/17   Modified to add snow squall warn (SQW)  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( NW = 3000, NC = 100 )
C*
	PARAMETER	( JSVR = 1, JTOR = 2, JFFW = 3, JSQW = 4 )
	PARAMETER	( JTIM = 1, JLAB = 2, JFIL = 3, JCLC = 4 )
C*
	CHARACTER*(*)	filtyp, dattim
	INTEGER		icolor(*), mrktyp(*), iwidth(*), iflags(*)
	REAL		ssize(*)
C*
	CHARACTER	path*25, templ*(MXTMPL), cdttm*20, dattm2*20, 
     +			buffer*128, tfile*128, wlabel*80, dattm4*20, 
     +			tmstp4*20, tmstr4*20, carr(7)*20, ttype*3, 
     +			poly*512, tstrt*20, tstn*7
	CHARACTER*(MXFLSZ)      filnam, files(MXNMFL), fnull
C*
	CHARACTER	wstn(NW)*7, timstr(NW)*20, timstp(NW)*20,
     +			wtype(NW)*3, wnum*4, parr(80)*6
	INTEGER		itest(NW), npt(NW), npoly(NW), iwetn(NW),
     +			icorr(NW)
	INTEGER		itype
	REAL	rlat(NC,NW), rlon(NC,NW), plat(NC,NW), plon(NC,NW)
	CHARACTER	cnnam(NC,NW)*32, fipnam(NC,NW)*8, 
     +			st*2, cn*2, wfo*20, flend*160
C*
	CHARACTER	stime*20, flstrt*160, cfips*12, bndtyp*30
	INTEGER		itarr(5), jtarr(5), mrktst(17:21), nfips(NC,NW)
	LOGICAL		done, found, dup
C*	
	DATA		mrktst / 2, 3, 4, 6, 14 /
C-----------------------------------------------------------------------
	iret = 0
C
C*  Scan the directory for all the warning & SLS watch data files.
C
	CALL ST_NULL ( filtyp, fnull, nf, ier )
	path  = ' '
	templ = ' '
	CALL CTB_DTGET ( fnull, path, templ, ic, is, if, ir, ii, ion,
     +          ihb, mnb, iha, mna, mstrct, idtmch, ier )
	CALL ST_RNUL ( path, path, lens, ier )
	CALL ST_RNUL ( templ, templ, lens, ier )
	CALL ST_LSTR ( path, lenp, ier )
	iorder = 1
	nexp = MXNMFL
	CALL FL_SCND ( path, templ, iorder, nexp, files, nfile, ier )
C
C*  Check for the last file requested by the user.
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
C*  Find the earliest file to start searching. For ALL times
C*  go back 10 days, for any other entry for dattim subtract
C*  12 hours from the time given.
C
	IF  ( dattim .eq. 'ALL' )  THEN
	    minuts = 14400
	  ELSE
	    minuts = 720
	END IF
	CALL TI_CTOI ( dattm2, itarr, ier )
	CALL TI_SUBM ( itarr, minuts, jtarr, ier )
	CALL TI_ITOC ( jtarr, stime, ier )
	CALL FL_MNAM ( stime, templ, flstrt, ier )
C
C*  Get 4-digit year to compare dates.
C
	CALL TI_DTM4 ( dattm2, dattm4, ier )
C
	minuts = 60
	CALL TI_CTOI ( dattm2, itarr, ier )
	CALL TI_ADDM ( itarr, minuts, jtarr, ier )
	CALL TI_ITOC ( jtarr, stime, ier )
	CALL FL_MNAM ( stime, templ, flend, ier )
	jfl = nfile
	done = .false.
	DO WHILE  ( ( jfl .gt. 0 ) .and. (.not. done ) )
	   IF (  files(jfl) .le. flend ) THEN
	    done = .true.
	   END IF
	   jfl = jfl - 1
	END DO
C
	IF (  jfl .lt. nfile   ) THEN
	    mfile = jfl + 1
	ELSE
	    mfile = nfile
	END IF
C
C*  Check the total number of warnings in the files.
C
	numw = 0
	done = .false.
	ifl  = mfile
	DO WHILE  ( ( ifl .gt. 0 ) .and. ( .not. done ) )
	    IF  ( files(ifl) .ge. flstrt )  THEN
		tfile = path(:lenp) // '/' // files(ifl)
		CALL FL_SOPN ( tfile, lunf, ier )
		iostat = 0
		DO WHILE  ( ( iostat .eq. 0 ) .and. ( .not. done ) )
		    READ ( lunf, 2, IOSTAT = iostat ) buffer
		    IF  ( iostat .eq. 0 )  THEN
			IF  ( buffer(1:1) .eq. '|' )  THEN
			    numw = numw + 1
			    IF  ( numw .gt. NW )  THEN
				done = .true.
				CALL ER_WMSG ( 'GG', 3, ' ', ierr )
			    END IF
			END IF
		    END IF
		END DO
		CALL FL_CLOS ( lunf, ier )
	    ELSE
		done = .true.
	    END IF
	    ifl = ifl - 1
	END DO
C
	IF  ( ifl+1 .le. nfile )  THEN
	    flstrt = files(ifl+1)
	END IF
C
C*  Decode each file until the end time is reached.
C
	nwrn = 0
	done  = .false.
	ifl = 1
	DO WHILE  ( ( ifl .le. nfile ) .and. ( .not. done ) )
	    IF  ( files(ifl) .gt. filnam )  THEN
		done = .true.
	      ELSE
		IF  ( (files(ifl) .ge. flstrt )  .and. 
     +	     ( files(ifl) .le. flend ) ) THEN
		  tfile = path(:lenp) // '/' // files(ifl)
		  CALL FL_SOPN ( tfile, lunf, ier )
C
		  iostat = 0
		  DO WHILE  ( iostat .eq. 0 )
		    READ ( lunf, 2, IOSTAT = iostat ) buffer
2		    FORMAT ( A )
		    IF  ( iostat .eq. 0 )  THEN
			dup = .false.
			IF  ( buffer(1:1) .eq. '|' )  THEN
			    CALL ST_CLST ( buffer, '|', ' ', 7,
     +					   carr, nump, ier )
C
			    CALL ST_NUMB ( carr(6), jflag, ier )
			    IF ( nump .ge. 7 ) THEN
			        CALL ST_NUMB ( carr(7), jetn, ier )
			      ELSE
				jetn = IMISSD
			    END IF
C
C*			    If the report is not a correction, then
C*			    jcorr is 0.  Prior to 5.11.3, the correction
C*			    letter was not a part of jflag and values were
C*			    less than or equal to 3.  So, if it was a
C*			    correction, then jcorr would be 1.  For release
C*			    5.11.3, if there is a correction letter, then
C*			    jcorr is the integer equivalent of the ASCII
C*			    code of the correction letter.
C
			    IF ( jflag .le. 3 ) THEN
			        jcorr = MOD ( jflag, 2 )
			        jtest = jflag / 2
			     ELSE
                                jcorr = MOD ( jflag, 256)
                                jtest = jflag / 256
			    END IF
			    ttype = carr(2)
			    tstrt = carr(3)
			    tstn  = carr(5)
C
C*			    Check for duplicates.  Note that if jetn
C*			    (Event Tracking No.--ETN) is not equal to
C*			    IMISSD then dup will be false for the same
C*			    WFO.  This makes it compatible with older
C*			    versions of decoded files (which also means
C*			    that duplicates will not be checked for 
C*			    if the decoded file is older than release
C*			    5.11.3).  Also, if it is a correction,
C*			    dup will be false, since there can be
C*			    later corrections.
C
			    dup = .false.
			    kw = nwrn
			    				
			    DO WHILE ( kw. ge. 1 .and. .not.dup .and.
     +					jetn .ne. IMISSD )
			        IF ( jetn  .eq. iwetn (kw) .and.
     +				     tstn  .eq. wstn (kw)  .and.
     +				     ttype .eq. wtype (kw) ) THEN
				    IF ( tstrt .eq. timstr (kw) ) THEN
					IF ( jcorr .eq. 0 ) THEN
C
C*					    Flag as duplicate only if
C*					    not a correction.
C
					    dup = .true.
					END IF
				    END IF
			        END IF
				kw = kw - 1
			    END DO
			    found = .false.
			    IF ( jcorr .ne. 0 ) THEN
C
C*  Allow for start time difference of 15 minutes for warnings and 60
C*  minutes for SLS watches for matching of correction reports.
C
				IF ( INDEX ( tstn, ',' ) .eq. 0 ) THEN
				    minup = 15
				ELSE
				    minup = 60
				END IF
				ii = 1
				DO WHILE ( ii .le. nwrn .and.
     +				           .not. found  .and.
     +					   .not. dup )
			            CALL TI_DIFF ( tstrt, timstr(ii),
     +						   mindif, iret)
				    IF  ((wtype(ii) .eq. ttype) .and.
     +                                   (wstn(ii)  .eq. tstn )) THEN
C
C*				        If there is an ETN, then use it
C*				        to find the matching original.
C
				        IF ( jetn .ne. IMISSD ) THEN
      				            IF ( jetn .eq. iwetn(ii)) THEN
C
C*					      Check for correction letter
C*					      which was included in 5.11.3
C
					      IF ( jcorr .ge. 65 ) THEN
					          IF ( jcorr .ge.
     +						       icorr (ii) ) THEN
						    found = .true.
					            iw = ii
						   ELSE
C
C*						    Treat older (letter)
C*						    corrections as
C*						    duplicates to
C*						    eliminate them
C
						    dup = .true.
					          END IF	
					       ELSE
      					          IF ((mindif .ge. 0)
     +     						.and.
     +                                              (mindif .le.
     +							   minup)) THEN
					            found = .true.
					            iw = ii
						  END IF
					      END IF
					    ELSE IF ( iwetn(ii) .eq.
     +							IMISSD ) THEN
      					      IF ((mindif .ge. 0) .and.
     +                                            (mindif .le.
     +					         	   minup)) THEN
				                found = .true.
				                iw = ii
					      END IF
					  END IF

				        ELSE
      					  IF ((mindif .ge. 0) .and.
     +                                         (mindif .le. minup)) THEN
				              found = .true.
				              iw = ii
					  END IF
				        END IF
				    END IF
				    ii = ii + 1
				END DO
			    END IF
C
			    IF  ( found )  THEN
				jw = iw
			    ELSE

				nwrn = nwrn + 1
				jw = nwrn
			    END IF
C
			    wtype(jw)  = carr(2)
			    timstr(jw) = carr(3)
			    timstp(jw) = carr(4)
			    wstn(jw)   = carr(5)
			    itest(jw)  = jtest
			    icorr(jw)  = jcorr
			    iwetn(jw)  = jetn
C
C*			    Read the warning polygon.
C
			    READ ( lunf, 2, IOSTAT = iostat ) poly
			    
			    IF  ( iostat .eq. 0 )  THEN
				CALL ST_CLSL ( poly, ' ', ' ', 80,
     +					parr, nums, ier )
C
				ii = 1
				jj = 1
				DO WHILE ( ii .le. nums/2 ) 
				    CALL ST_NUMB (parr(jj), ival, ier)
				    plat(ii,jw) = ival/100.0
				    jj = jj +1
				    CALL ST_NUMB (parr(jj), ival, ier)
				    plon(ii,jw) = (-ival)/100.0
				    jj = jj +1
				    ii = ii + 1
				END DO
				plat(ii,jw) = plat(1,jw)
				plon(ii,jw) = plon(1,jw)
				npoly(jw) = nums/2 + 1
			    END IF
			END IF
C
C*  Read the county information until next new report.
C
			knt = 0
			jostat = 0
			DO WHILE  ( jostat .eq. 0 )
			    READ ( lunf, 2, IOSTAT = jostat ) buffer
			    IF  ( jostat .eq. 0 )  THEN
				IF  ( buffer(1:1) .eq. '|' )  THEN
				    CALL FL_BKSP ( lunf, ier )
				    jostat = -1
				ELSE
				    knt = knt + 1
				    IF ( knt .le. NC ) THEN
				        READ (buffer, 1000) 
     +					      fipnam(knt,jw),
     +					      nfips(knt,jw), 
     +					      cnnam(knt,jw),
     +					      st, cn, rlat(knt,jw),
     +					      rlon(knt,jw), elv,
     +					      ipr, wfo
1000					FORMAT ( A, 1X, I6, 1X, A, 1X,
     +                                        A, 1X, A, 1X, F9.2, 1X,
     +                                        F9.2, 1X, F9.2, 1X, I2,
     +                                        1X, A )
				    END IF
				END IF
			    END IF
			END DO
C
C*  Finished processing county information.
C
			npt(jw) = knt
C
C*			If the entry is a duplicate, decrement number
C*			of warnings, thereby eliminating the duplicate
C*			entry.
C
			IF (dup) THEN
			    nwrn = nwrn - 1
			END IF
		    END IF
		  END DO
C
C*  Finished processing one file.
C
		  CALL FL_CLOS ( lunf, ier )
		END IF
	    END IF
	    ifl = ifl + 1
	END DO
C
C*  Finished processing all files.
C
C*  Query current settings.
C
	CALL GQCOLR ( jcolr, ier )
	CALL GQLINE ( iltyp, ilthw, ilwdth, iwhw, ier )
	CALL GQMRKR ( jmark, jmkhw, szmark, jmkwid, ier )
	CALL GQTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr,
     +		      jrrotn, jjust, ier )
C
C*  Set attributes and defaults.
C
	CALL GSLINE ( 1, 2, 2, 2, ier )
	CALL GSTEXT ( 21, 2, 1.0, 1, 111, 1, 1, ier )
C
	DO  ii = 1, numm
	    IF  ( ( mrktyp(ii) .lt. 17 ) .or.
     +		  ( mrktyp(ii) .gt. 21 ) ) THEN
		IF ( filtyp .eq. 'WARN' ) THEN
		    mrktyp(ii) = 20
		  ELSE
		    mrktyp(ii) = 18
		END IF
	    END IF
C
	    IF  ( ( ssize(ii) .le.  0.0 ) .or.
     +		  ( ssize(ii) .gt. 10.0 ) )  ssize(ii) = 1.0
C
	    IF  ( ( iwidth(ii) .le.  0 ) .or.
     +		  ( iwidth(ii) .gt. 10 ) )  iwidth(ii) = 3
	END DO
C
C*  Plot the graphic for each valid report.
C
	DO ip = 1, nwrn
	    CALL TI_DTM4 ( timstp(ip), tmstp4, ier )
	    CALL TI_DTM4 ( timstr(ip), tmstr4, ier )
	    IF  ( ( dattim .eq. 'ALL' ) .or.
     +		  ( ( tmstp4 .gt. dattm4 ) .and.
     +		    ( tmstr4 .le. dattm4 ) ) )  THEN
C
C*  Set the color and marker based on the type of warning
C*  or SLS watch.
C
		IF  ( wtype(ip) .eq. 'TOR' .or.
     +		      wtype(ip) .eq. 'TN' )  THEN
		    jtype = JTOR
		ELSE IF  ( wtype(ip) .eq. 'SVR' .or.
     +			   wtype(ip) .eq. 'TS' )  THEN
		    jtype = JSVR
		ELSE IF  ( wtype(ip) .eq. 'FFW' )  THEN
		    jtype = JFFW
                ELSE IF  ( wtype(ip) .eq. 'SQW' )  THEN
                    jtype = JSQW
		ELSE
		    jtype = 0
		END IF
C
		IF ( jtype .gt. 0 ) THEN
		    idx = jtype
C
C*  Set the color based on the last digit of watch number for SVRL,
C*  if the color code is on.
C
		    IF ( filtyp .eq. 'SVRL' .and. 
     +			 iflags(JCLC) .gt. 0 ) THEN
			ix = INDEX ( wstn(ip), ',')
			CALL ST_RMBL ( wstn(ip)(:ix-1), wnum, lenw, ier )
			CALL ST_NUMB ( wnum(:lenw), iwn, ier )
			idx = MOD(iwn,10) + 3 
		    END IF
		    ic = icolor( idx )
C			
		    IF ( itest( ip ) .eq. 0 ) THEN
			mrkr = mrktyp( idx ) 
		    ELSE
			mrkr = mrktst ( mrktyp( idx ) ) 
		    END IF
		    CALL GSMRKR ( mrkr, 0, ssize(idx), iwidth(idx),
     +				  ier )
		ELSE
		    ic = 0
		END IF
C
C*  Plot each county in the warnings or SLS watches.
C
		IF  ( ic .ne. 0 )  THEN
		    CALL GSCOLR ( ic, ier )
		    IF ( npt(ip) .gt. NC ) THEN
			npt(ip) = NC
			CALL ER_WMSG ( 'GG', 2, 'GGWARN', ierr )
		    END IF
		    IF ( npt(ip) .gt. 0 ) THEN
			DO  im = 1, npt(ip)
C
C*  Draw the storm-based polygon, county outline, or marker.
C
			    IF ( filtyp .ne. 'SVRL' .and.
     +				iflags(JCLC) .gt. 0 )  THEN
C
C*  Storm-based polygon requested... (This is the dominant option.)
C
				CALL GLINE('M', npoly(ip), plat(1,ip),
     +					plon(1,ip), ier )
			    ELSE IF ( iflags(JFIL) .gt. 0 )  THEN
C
C*  County Outline requested
C
				CALL ST_INCH ( nfips(im,ip), cfips, ier )
				bndtyp = 'CNTY_BNDS|<FIPS>'//cfips
				CALL GPLBND ( bndtyp,
     +			      0,0,0,0,0, ic, 1, iwidth(idx),
     +			      ' ',0,0,0.,0, ier)
			    ELSE
C
C*  Neither Storm-based nor County Outline requested...  (Plot marker.)
C
				CALL ST_INCH ( nfips(im,ip), cfips, ier )
				bndtyp = 'CNTY_BNDS|<FIPS>'//cfips
				CALL GPLBND ( bndtyp,
     +			      0,0,0,0,0, 0,0,0,
     +			      'MARK', ic, mrkr, ssize(idx),
     +			      iwidth(idx), ier)
			    END IF
C
C*  Plot the text.
C
			    IF ( ( iflags(JTIM) .eq. 0 ) .and.
     +			         ( iflags(JLAB) .eq. 0 ) ) THEN
				iyoff  = -1
				wlabel = ' '
C
			    ELSE IF  ( ( iflags(JTIM) .eq. 0 ) .and.
     +					 ( iflags(JLAB) .ne. 0 ) ) THEN
				iyoff  = -2
				CALL ST_LSTR ( cnnam(im,ip), lencn, ier)
				wlabel = cnnam(im,ip)(:lencn)
C
			    ELSE IF ( ( iflags(JTIM) .ne. 0 ) .and. 
     +					( iflags(JLAB) .eq. 0 ) ) THEN
			        iyoff  = -3
			        CALL ST_LSTR ( timstr(ip), lenstr, ier)
			        CALL ST_LSTR ( timstp(ip), lenstp, ier)
			        wlabel = timstr(ip) (8:11) // '-' //
     +					timstp(ip) (8:11)
C
		            ELSE IF ( ( iflags(JTIM) .ne. 0 ) .and. 
     +					( iflags(JLAB) .ne. 0 ) ) THEN
				iyoff  = -3
				CALL ST_LSTR ( cnnam(im,ip), lencn, ier)
				CALL ST_LSTR ( timstr(ip), lenstr, ier)
				CALL ST_LSTR ( timstp(ip), lenstp, ier)
				wlabel = cnnam(im,ip) (:lencn) // CHCR
     +				     // timstr(ip) (8:11) // '-' //
     +				     timstp(ip) (8:11)
C
			    END IF
C
			    CALL GTEXT ( 'M', rlat(im,ip), rlon(im,ip),
     +					  wlabel, 0.0, 0, iyoff, ier )
			END DO
		    END IF
		END IF
	    END IF
	END DO
C
C*  Reset the saved attributes.
C
	CALL GSCOLR ( jcolr, ier )
	CALL GSLINE ( iltyp, ilthw, ilwdth, iwhw, ier )
	CALL GSMRKR ( jmark, jmkhw, szmark, jmkwid, ier )
	CALL GSTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr,
     +		      jrrotn, jjust, ier )
C
	RETURN
	END
