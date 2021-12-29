	SUBROUTINE GG_HRCN ( dattim, icolor, isytyp, ssize, iwidth,
     +			     arwsiz, ahdsiz, iawdth, iflags, strnam,
     +			     iret )
C************************************************************************
C* GG_HRCN								*
C*									*
C* This subroutine plots the current tropical depressions, tropical     *
C* storms and hurricane advisories.					*
C*									*
C* GG_HRCN ( DATTIM, ICOLOR, ISYTYP, SSIZE, IWIDTH, ARWSIZ, AHDSIZ,	*
C*					IAWDTH, IFLAGS, STRNAM, IRET )	*
C*									*
C* Input parameters:							*
C*	DATTIM		CHAR*		Ending time for tropical systems*
C*	ICOLOR (4)	INTEGER		Symbol and arrow colors		*
C*	ISYTYP (4)	INTEGER		Symbol types			*
C*	SSIZE  (4)	REAL		Symbol sizes			*
C*	IWIDTH (4)	INTEGER		Symbol widths			*
C*	ARWSIZ (4)	REAL		Arrow sizes			*
C*	AHDSIZ (4)	REAL		Arrow head sizes		*
C*	IAWDTH (4)	INTEGER		Arrow widths			*
C*	IFLAGS (5)	INTEGER		Flags for labels		*
C*					  0 = false			*
C*					  1 = true			*
C*	STRNAM		CHAR*		Tropical storm name		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		 9/99	Created					*
C* S. Jacobs/NCEP       10/99	Changed call to FL_SCND and FL_TMPL     *
C* A. Hardy/GSC		10/99	Removed DO LOOP;changed xoffset 	*
C* A. Hardy/GSC		10/99	Added ALL for strnam;blank dattim;      *
C*                              can specifiy single storm name          *
C* S. Jacobs/NCEP	11/99	Changed default colors and sym size	*
C* S. Jacobs/NCEP	11/99	Changed default attributes for arrows	*
C* S. Jacobs/NCEP	 3/00	Changed calling sequence		*
C* D. Kidwell/NCEP	 4/00	Added processing for TEST reports       *
C* M. Li/GSC             5/00   Added MXNMFL and MXFLSZ                 *
C* A. Hardy/GSC		 6/00   Added ocean,quadrant and fcst trak info *
C* A. Hardy/GSC		 6/00   Fixed last display of quad./fcst track  *
C* J. Wu/GSC		 7/00	Added checks for TI_STAN return status	*
C* A. Hardy/GSC		 7/00   Changed advisory time for comparision   *
C* A. Hardy/GSC		 8/00   Added corr. flagging bad data		*
C* A. Hardy/GSC		 8/00   Changed the track color from 'ic'->'6'  *
C* S. Jacobs/NCEP	 3/01	Increased file template to 48 chars	*
C* S. Jacobs/NCEP	 4/01	Changed flstrt check from gt to ge	*
C* S. Jacobs/NCEP	 5/01	Save line attributes			*
C* S. Jacobs/NCEP	 5/01	Changed GARC to GG_ARC and GLINE	*
C* S. Jacobs/NCEP	 5/01	Reduced number of pts from 64 to 20	*
C* D. Kidwell/NCEP	 7/01	Changed to allow read of 34kt fcst radii*
C* A. Hardy/SAIC	 8/01   Changed fcst mkrs to storm symbols;     *
C*                              use ocean id for fcst line display      *
C* D. Kidwell/NCEP	 2/02	Changed to treat fcst type RL as TD     *
C* A. Hardy/SAIC         2/02   Changed call FL_SCND			*
C* D. Kidwell/NCEP	 3/02	Plot quad winds at fcst pts;NW 5000->500*
C* M. Li/SAIC		10/02	Initialize hvname			*
C* D. Kidwell/NCEP	 2/03	Modify for 5-day fcst (7 fcst times)    *
C* A. Hardy/NCEP	10/03   Modified for JTWC decoded reports	*
C* A. Hardy/NCEP	11/03   Corrected JTWC wind radii color display *
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM             *
C* D. Kidwell/NCEP	 6/04	Checked for new JTWC max wind ff 5/04   *
C* B. Yin/SAIC           7/04   Added forecast hour flags  		*
C* T. Lee/SAIC		 9/04	Replaced FL_TMPL with CTB_DTGET		*
C* A. Hardy/NCEP	11/04	Added calls to ST_RNUL			*
C* m.gamazaychikov/SAIC 12/04   Added ion flag to CTB_DTGET CS          *
C* m.gamazaychikov/SAIC 01/06   Changed templ string length to MXTMPL   *
C* m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
C* S. Gilbert/NCEP	04/06	added 6hr wind radii plots              *
C* S. Gilbert/NCEP	07/06	fixed fcst hours to be calculated from  *
C*                              synoptic time and not issuance time.    *
C* F. J. Yen/NCEP	 4/08	Added bin mins & mstrct to CTB_DTGET CSC*
C* B. Hebbard/NCEP       5/20   Modify for new 60-hr fcst in TCM        *
C*                              (SCN20-20) (now 8 fcst times)           *
C* B. Hebbard/NCEP       6/20   Synthesize F60 data for JTWC; fix wind  *
C*                              radii times; ensure synoptic minutes 0  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( NW = 500, NC = 25, NFCST = 8 )
	PARAMETER	( ITRACK = NFCST + 1 )
	PARAMETER	( MXNWFT = ITRACK * 3 + 1 )
C*
	PARAMETER	( JHUR = 1, JTSM = 2, JTDP = 3, JMTN = 4 )
	PARAMETER	( JTIM = 1, JNAM = 2, JSPD = 3, JQAD = 4 )
	PARAMETER	( I00HR = 5, I06HR = 6, I12HR = 7, I18HR = 8 )
	PARAMETER	( I24HR = 9, I30HR = 10, I36HR = 11, I42HR = 12)
	PARAMETER	(I48HR = 13, I54HR = 14, I60HR = 15, I66HR = 16)
	PARAMETER	( I72HR = 17, I78HR = 18, I84HR = 19 )
	PARAMETER	( I90HR = 20, I96HR = 21, I120HR = 22 )
C*
	CHARACTER*(*)	dattim, strnam
	INTEGER		icolor(*), isytyp(*), iwidth(*), iawdth(*),
     +			iflags(*)
	REAL		ssize(*), arwsiz(*), ahdsiz(*)
C*
	CHARACTER	path*25, templ*(MXTMPL), cdttm*20, dattm2*20, 
     +			buffer*100, tfile*128, wlabel*80, dattm4*20, 
     +			tmstr4*20, tlabel*80, carr(11)*20, ttype*2, 
     +			tstrt*20, tname*12, tocen*6, tadnm*3, tcorr,
     +                  windft(MXNWFT,NW)*60
	CHARACTER*(MXFLSZ)      filnam, files (MXNMFL)
C*
	CHARACTER	wname(NW)*12, timstr(NW)*20, wtype(NW)*3
	INTEGER		itest(NW), istrt(5), icflg(NW), numpts(NW)
	REAL		rlat(NC,NW), rlon(NC,NW), dirnum
	CHARACTER	wdir(NW)*3, wsped(NW)*2, wpres(NW)*4, 
     +                  wocen(NW)*6, wadnm(NW)*3, wcorr(NW), 
     +                  fstype (NC,NW)*2, seaid*6, fstypep*2
C*
	CHARACTER	stime*20, flstrt*160, temp4*20, ctime*20
	INTEGER		itarr(5), jtarr(5), iarr(5), itype
	LOGICAL		done, found, hvname, namflg, over
	LOGICAL		track
C*
        REAL            slat(NC), slon(NC), endpts(4), fnlpts(4,4),
     +			xout(100), yout(100)
        CHARACTER       cmpnme(10)*20, cmptim(10)*20, sys*1
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
        inpt = 20 
C
C*	Determine whether to plot track or not
C
	track = ( iflags( I06HR ) .ne. 0 ) .or.
     +		( iflags( I12HR ) .ne. 0 ) .or.
     +		( iflags( I18HR ) .ne. 0 ) .or.
     +		( iflags( I24HR ) .ne. 0 ) .or.
     +		( iflags( I30HR ) .ne. 0 ) .or.
     +		( iflags( I36HR ) .ne. 0 ) .or.
     +		( iflags( I42HR ) .ne. 0 ) .or.
     +		( iflags( I48HR ) .ne. 0 ) .or.
     +		( iflags( I54HR ) .ne. 0 ) .or.
     +		( iflags( I60HR ) .ne. 0 ) .or.
     +		( iflags( I66HR ) .ne. 0 ) .or.
     +		( iflags( I72HR ) .ne. 0 ) .or.
     +		( iflags( I78HR ) .ne. 0 ) .or.
     +		( iflags( I84HR ) .ne. 0 ) .or.
     +		( iflags( I90HR ) .ne. 0 ) .or.
     +		( iflags( I96HR ) .ne. 0 ) .or.
     +		( iflags( I120HR) .ne. 0 )
C
C*	Scan the directory for all of the hurricane data files.
C
	filnam = 'HRCN'  
	path  = ' '
	templ = ' '
	CALL ST_NULL ( filnam, filnam, nf, ier )
	CALL CTB_DTGET ( filnam, path, templ, ic, is, if, ir, ii, ion,
     +		ihb, mnb, iha, mna, mstrct, idtmch,ier )
        CALL ST_RNUL (path, path, lens, ier )
        CALL ST_RNUL (templ, templ, lens, ier )
	CALL ST_LSTR ( path, lenp, ier )
        nexp = MXNMFL
        iorder = 1
	CALL FL_SCND ( path, templ, iorder, nexp, files, nfile, ier )
C
C*	Check for the last file requested by the user.
C
	CALL ST_LCUC ( dattim, dattim, ier )
	itype = 1
	IF  ( ( dattim .eq. 'LAST' ) .or.
     +	      ( dattim .eq. 'ALL' ) .or. ( dattim .eq. ' ' ) )  THEN
	    CALL CSS_GTIM ( itype, dattm2, ier )
	    CALL FL_MNAM ( dattm2, templ, filnam, ier )
	  ELSE
	    CALL CSS_GTIM ( itype, cdttm, ier )
	    CALL TI_STAN ( dattim, cdttm, dattm2, ier )
	    IF ( ier .ne. 0 ) THEN
	    	CALL ER_WMSG ( 'TI', ier, dattim, ierr )
	    	iret = ier
	    	RETURN
	    END IF	    
	    CALL FL_MNAM ( dattm2, templ, filnam, ier )
	END IF
C
C*	Find the earliest file to start searching. For ALL times
C*	go back 10 days, for any other entry for dattim subtract
C*	1 day from the time given.
C
	IF  ( ( dattim .eq. 'ALL' ) .or.  ( dattim .eq. ' ' ) ) THEN
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
C*	Initialize correction flag array.
C
        DO ii = 1, NW
           icflg (ii) = 0
        END DO
C
C*	Decode each file until the end time is reached.
C
	nwrn = 0
	done  = .false.
	ifl = 1
        itotal = 1
	DO WHILE  ( ( ifl .le. nfile ) .and. ( .not. done ) )
	   IF  ( files(ifl) .gt. filnam )  THEN
		done = .true.
	     ELSE
	       IF ( files(ifl) .ge. flstrt )  THEN
		  tfile = path(:lenp) // '/' // files(ifl)
		  CALL FL_SOPN ( tfile, lunf, ier )
C
		  iostat = 0
		  DO WHILE  ( iostat .eq. 0 )
		    READ ( lunf, 2, IOSTAT = iostat ) buffer
2		    FORMAT ( A )
		    IF  ( iostat .eq. 0 )  THEN
		      IF  ( buffer(1:1) .eq. '|' )  THEN
			CALL ST_CLST ( buffer, '|', ' ', 11,
     +				       carr, num, ier )
C
			CALL ST_NUMB ( carr(11), jflag, ier )
			jcorr = MOD ( jflag, 2 )
			jtest = jflag / 2
			ttype = carr(2)(:2)
			tstrt = carr(3)
			tname = carr(4)
			tocen = carr(5)
                        tadnm = carr(6)
			found = .false.
                        CALL ST_INCH ( jcorr, tcorr , ier )
C
C*			Correction flag is true.
C
			IF  ( jcorr .eq. 1 )  THEN
			    DO  ii = 1, nwrn
			        IF  ((wtype(ii)(1:2) .eq. ttype) .and.
     +				     (timstr(ii) .eq. tstrt) .and.
     +				     (wname(ii)   .eq. tname ) .and.
     +                               (wocen(ii) .eq. tocen) ) THEN
				    found = .true.
				    iw = ii
				END IF
			    END DO
			END IF
C
			IF  ( found )  THEN
			    jw = iw
			  ELSE
			    nwrn = nwrn + 1
			    jw   = nwrn
			    timstr(jw) = carr(3)
			END IF
			wtype(jw)  = carr(2)
			wname(jw)  = carr(4)
			wocen(jw)  = carr(5)
                        wadnm (jw) = carr(6)
			wdir(jw)   = carr(8)
			wsped(jw)  = carr(9)
			wpres(jw)  = carr(10)
                        wcorr (jw) = carr(11)
			itest(jw)  = jtest
C
C*                      Check for corrected reports
C
                        IF ( wcorr (jw) .eq. '1' ) THEN
                            DO kk = nwrn, 1, -1
                                IF  ((wtype(jw) .eq. wtype(kk)) .and.
     +		                     (timstr (jw) .eq. timstr(kk)) .and. 
     +                               (wname (jw) .eq. wname (kk)) .and.
     +                               (wocen (jw) .eq. wocen(kk)) .and.
     +                               (wcorr (kk) .eq. '0' )) THEN
                                    icflg(kk) = 1
                                END IF
                            END DO
                        END IF

C
C*                      Read the lat/lon coordinates from the file.
C
                        nknt = 0
                        jostat = 0
                        DO ii = 1, ITRACK
                            fstype ( ii, jw ) = ' '
                            READ ( lunf, 2, IOSTAT = jostat ) buffer
                            IF  ( jostat .eq. 0 )  THEN
                                IF  ( ( buffer(1:1) .eq. '|' ) .or.
     +				      ( buffer(7:7) .eq. ' ' ) )  THEN
                                    CALL FL_BKSP ( lunf, ier )
                                    jostat = -1
                                  ELSE
                                    IF  ( jw .ne. 0 )  THEN
                                        nknt = nknt + 1
                                        READ (buffer, 1000)
     +                                                  rlat(nknt,jw),
     +                                                  rlon(nknt,jw),
     +                                                  fstype(nknt, jw)
1000                                    FORMAT ( 2F9.2, 4X, A )
                                    END IF
                                END IF
                            END IF
                        END DO
C
C*                      Remember the number of points actually read 
C*                      in for this report
C
                        numpts(jw) = nknt
C
C*			Read the wind radii and sea feet information.
C
			nwft  = nknt * 3 + 1
                        knt   = 0
			knt64 = 5
			knt50 = knt64 + 1
			knt34 = knt50 + 1	
C
C*			Set 50 and 64 kt forecast missing in case this
C*			is the old format, with only 34 kt fcst winds.
C
			IF ( jw .ne. 0 ) THEN
			    DO ii = knt64, nwft-2, 3
			        windft ( ii, jw )   = 
     +					'64 -9999 -9999 -9999 -9999'
				windft ( ii+1, jw ) =
     +					'50 -9999 -9999 -9999 -9999'
			    END DO
			END IF
C
			jostat = 0
                        DO WHILE ( jostat .eq. 0 )
                           READ ( lunf, 2, IOSTAT = jostat ) buffer
C
                           IF  ( jostat .eq. 0 )  THEN
                              IF  ( buffer(1:1) .eq. '|' )  THEN
                                    CALL FL_BKSP ( lunf, ier )
                                    jostat = -1
                                ELSE
                                  IF ( jw .ne. 0 ) THEN
                                    knt = knt + 1
C
C*				    Handle old and new formats.
C
C*                                  If first wind line is 'MW', 
C*			            determine if the  decoded report 
C*				    is from TPC or the JTWC.
C
                                    IF (buffer (5:6) .eq. 'MW') THEN
				      IF (wtype(jw)(:3) .eq. 'HUT') THEN
C
C*					  Check the date for JTWC -
C*					  reports before 6/1/04 use 100
C*					  kt winds, those after use 64
C*					  kt.
C
					  IF ( ( itarr ( 1 ) .lt. 2004 )
     +					       .or.
     +					       ( ( itarr (1) .eq. 2004 )
     +						 .and.
     +						 ( itarr (2) .lt. 6 ) ))
     +					       THEN
				              buffer(5:6) = '10'
					    ELSE
					      buffer(5:6) = '64'
					  END IF
				        ELSE
				          buffer(5:6) = '64'
				      END IF
                                    END IF
				    IF ( knt .le. 4 ) THEN
                                        windft ( knt, jw ) = buffer
				      ELSE
C
					CALL ST_CLST (buffer, ' ',' ',
     +					              5, carr, num, ier)
				        IF ((carr(1)(:2) .eq. '64').or. 
     +                                      (carr(1)(:2) .eq. '10'))THEN
					     windft (knt64, jw) = buffer
					     knt64 = knt64 + 3
					   ELSE IF (carr(1) .eq. 
     +                                                         '50')THEN
					     windft (knt50, jw) = buffer 
					     knt50 = knt50 + 3
					   ELSE IF (carr(1) .eq. 
     +                                                         '34')THEN
					     windft (knt34, jw) = buffer 
					     knt34 = knt34 + 3
				        END IF
				        IF (knt34 .gt. nwft) jostat = -1
				    END IF
                                  END IF
                              END IF
                           END IF
                        END DO
                      END IF
		    END IF
		  END DO
C
		  CALL FL_CLOS ( lunf, ier )
C
	       END IF
	   END IF
	   ifl = ifl + 1
	END DO
C
C*      Check to see if a storm name was specified.
C
	hvname = .false.
        IF ( ( strnam .ne. ' ' ) .and. ( strnam .ne. 'ALL' ) ) THEN
            CALL ST_LSTR ( strnam, lens, ier)
            over = .false.
            kl = nwrn
            DO WHILE ( .not. over )
                IF ( strnam .eq. wname(kl) ) THEN
                    over = .true.
                    seaid = wocen(kl)
                    CALL ST_LSTR ( seaid, leno, ier)
                END IF
                kl = kl - 1
                IF ( kl .lt. 1 ) THEN
                    over = .true.
                    seaid = ' '
                END IF
            END DO
            icnt = 1
            hvname = .true.
            DO inam = 1, nwrn
                IF ( seaid(:leno) .eq. wocen(inam) ) THEN
	             timstr(icnt) = timstr(inam)
	             wtype(icnt)  = wtype (inam)
		     wname(icnt)  = wname(inam)
		     wocen(icnt)  = wocen(inam)
		     wdir (icnt)  = wdir(inam)
		     wsped(icnt)  = wsped(inam)
		     wpres(icnt)  = wpres(inam)
		     itest(icnt)  = itest(inam)
                  DO jj = 1, nknt
                     rlat(jj,icnt) = rlat(jj,inam)
                     rlon(jj,icnt) = rlon(jj,inam)
                  END DO
                  DO kk = 1, nwft
                     windft(kk,icnt) = windft(kk,inam)
                  END DO
                     icnt = icnt + 1
                END IF
            END DO
            nwrn = icnt - 1
        END IF
C
C*      Handle the cases of decoded reports that have exactly 8 points,
C*      (1 analysis/observed + 7 forecast) instead of the 9 points
C*      (1 analysis/observed + 8 forecast) assumed by the display code 
C*      below.  We assume such reports do not (yet) have F60 forecast
C*      position and radii, which were added to NHC/CPHC-issued TCM 
C*      products about 15 May 2020 per SCN20-20, but were not added to
C*      JTWC.  (Note that the decoder will always output 9 points for
C*      NHC/CPHC and 8 points for JTWC, even if the original bulletin
C*      contained fewer than that number of points; in the case of, 
C*      say, a storm that dissipated before F120, the missing points 
C*      are still reported, but with 'missing' value flags (e.g., -9999).
C*
C*      If/after JTWC also adds F60 forecast data and the decoder is
C*      changed accordingly (nfcst=8), then this section of code could 
C*      (although need not) be removed.
C
        DO ip = 1, nwrn
C
            IF ((numpts(ip) .eq. 8)) THEN
C
C*              [ Optional:  Consider adding sanity check here that
C*              this is a JTWC-reported storm.  For example, could 
C*              check that first point has longitude west of 180
C*              ((rlon(1,ip) .gt. 0.0), although must be careful that
C*              for storms crossing 180 westward between F00 and F03,
C*              CPHC will likely issue the advisory (based on F00 
C*              "initial position" [per M. Brennan]), while the first
C*              point (observed position) will be F03, west of 180.
C*              Thus may want to allow for some westward travel (say,
C*              up to three hours motion) in such a check.  Eastward
C*              crossings will not have this issue, since first point
C*              for JTWC reports is at F00, when AOR is determined. ]
C*              --
C
C*              Shift so new points 7 through 9 become the ones read in
C*              as points 6 through 8.  These are for F72, F96, F120.
C
                DO k = 9, 7, -1
                    rlat(k,ip) = rlat(k-1,ip)
                    rlon(k,ip) = rlon(k-1,ip)
                    fstype(k,ip) = fstype(k-1,ip)
                END DO
C
C*              Then interpolate the new point 6 (F60) between points 
C*              5 and 7, and carry the fstype for 6 forward from 5. 
C*              Note that this will provide exactly what the user
C*              would have seen if F60 were manually enabled before.
C
                rlat(6,ip) = (rlat(5,ip) + rlat(7,ip)) / 2.0
                rlon(6,ip) = (rlon(5,ip) + rlon(7,ip)) / 2.0
                fstype(6,ip) = fstype(5,ip)
C
C*              Now we need to do the same for wind/seas radii groups.
C*              Shift the 3 lines of each new group 7 through 9 from
C*              those read in as groups 6 through 8.  (Equivalent to
C*              shifting lines 17-25 to 20-28, nondestructively; but
C*              this shows how those numbers are derived.)
C
                DO mgroup = 9, 7, -1
                    DO mline = 1, 3
                        windft(1 + (mgroup-1)*3 + mline, ip) =
     +                  windft(1 + (mgroup-2)*3 + mline, ip)
                    END DO
                END DO
C
C               Finally carry 3 lines of new group 6 (F60) forward from 
C               group 5 (F48), again replicating legacy 'interpolation'
C               behavior.  (Equivalent to copying lines 14-16 to fill
C               in the vacated lines 17-19.)
C
                DO mline = 1, 3
                    windft(1 + (6-1)*3 + mline, ip) =
     +              windft(1 + (5-1)*3 + mline, ip)
                END DO
            END IF
        END DO
C
C*	Query current settings.
C
	CALL GQCOLR ( jcolr, ier )
	CALL GQSPCL ( szspcl, jmkwid, ier )
	CALL GQTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr,
     +		      jrrotn, jjust, ier )
	CALL GQDARR ( szdarw, szdarh, idarwd, idartp, ier )
	CALL GQLINE ( jltyp, jlthw, jwidth, jwhw, ier )
C
C*	Set symbols attributes.
C
	CALL GSTEXT ( 21, 2, 1.0, 1, 111, 1, 1, ier )
C
	IF  ( isytyp(JTDP) .eq. 0 )  isytyp(JTDP) = 33 
	IF  ( isytyp(JTSM) .eq. 0 )  isytyp(JTSM) = 25
	IF  ( isytyp(JHUR) .eq. 0 )  isytyp(JHUR) = 26
C
	DO  i = 1, 3
	    IF  ( ( ssize(i) .le.  0.0 ) .or.
     +		  ( ssize(i) .gt. 10.0 ) )  ssize(i) = 1.0
C
	    IF  ( ( iwidth(i) .le.  0 ) .or.
     +		  ( iwidth(i) .gt. 10 ) )  iwidth(i) = 2
	END DO
C
	IF  ( ( arwsiz(JMTN) .le.  0.0 ) .or.
     +	      ( arwsiz(JMTN) .gt. 10.0 ) )  arwsiz(JMTN) = 1.0
C
	IF  ( ( ahdsiz(JMTN) .le.  0.0 ) .or.
     +	      ( ahdsiz(JMTN) .gt. 10.0 ) )  ahdsiz(JMTN) = 0.7
C
	IF  ( ( iawdth(JMTN) .le.  0 ) .or.
     +	      ( iawdth(JMTN) .gt. 10 ) )  iawdth(JMTN) = 3
C
C*	Set directional arrow attributes.
C
	CALL GSDARR ( arwsiz(JMTN), ahdsiz(JMTN), iawdth(JMTN),
     +		      212, iret )
C
C*	Store name of each storm and associated last date/time.
C
        namflg = .false.
        itot = 2
        icntr = 1
	cmpnme ( icntr ) = wocen(1)
	cmptim ( icntr ) = timstr(1)
        DO WHILE ( itot .le. nwrn )
           DO  jj = icntr, 1, -1
	      IF   ((wocen (itot) .eq. cmpnme(jj) ) .and.
     +             ( .not. namflg ) ) THEN
                  CALL TI_DIFF (timstr( itot ), cmptim(jj), nmin, iret )
                  IF ( nmin .gt. 0)  THEN
                      cmptim ( jj ) =  timstr(itot)
                  END IF
                  namflg = .true.
                END IF
            END DO         
C
            IF ( .not. namflg) THEN
                icntr = icntr + 1
	        cmpnme ( icntr ) = wocen(itot)
	        cmptim ( icntr ) = timstr(itot)
            END IF 
            namflg = .false.
            itot = itot + 1
        END DO
C
C*	Plot the graphic for each valid report.
C
	DO ip = 1, nwrn
C
C*          Change the start time to be 60 minutes before the actual
C*          start time of the advisory.
C
            CALL TI_CTOI ( timstr(ip), istrt, ier )
            CALL TI_SUBM ( istrt, 60, istrt, ier )
            CALL TI_ITOC ( istrt, temp4, ier )
            CALL TI_DTM4 ( temp4, tmstr4, ier )
C
	    IF  ( ( dattim .eq. 'ALL' ) .or.
     +		    ( tmstr4 .le. dattm4 )  )  THEN
C
C*		Set the color based on the type of tropical event.
C
		IF  ( wtype(ip)(:2) .eq. 'TD' ) THEN
		    ic = icolor(JTDP)
	            CALL GSSPCL ( ssize(JTDP), iwidth(JTDP), ier )
		    sym = FLOAT ( isytyp(JTDP) )
C
		  ELSE IF  ( wtype(ip)(:2) .eq. 'TS' )  THEN
		    ic = icolor(JTSM)
	            CALL GSSPCL ( ssize(JTSM), iwidth(JTSM), ier )
		    sym = FLOAT ( isytyp(JTSM) )
C
		  ELSE IF  ( wtype(ip)(:2) .eq. 'HU' )  THEN
		    ic = icolor(JHUR)
	            CALL GSSPCL ( ssize(JHUR), iwidth(JHUR), ier )
		    sym = FLOAT ( isytyp(JHUR) )
C
		END IF
C
		IF  ( (ic .ne. 0 ) .and. ( icflg(ip) .eq. 0 ) ) THEN
		    CALL GSCOLR ( ic, ier )
C
C*		    Draw the symbols.
C
		    CALL GSPCL ( 'M', 1, sym, rlat(1,ip), 
     +				 rlon(1,ip), 0, 0, ier )
C
C*		    Plot the text.
C
		    iyoff = -1
		    IF  ( ( iflags(JTIM) .eq. 0 ) .and.
     +			  ( iflags(JNAM) .eq. 0 ) .and.
     +			  ( iflags(JSPD) .eq. 0 ) )  THEN
			wlabel = ' '
C
		      ELSE IF ( ( iflags(JTIM) .eq. 0 ) .and. 
     +				( iflags(JNAM) .ne. 0 ) .and.
     +				( iflags(JSPD) .eq. 0 ) )  THEN
			CALL ST_LSTR ( wname (ip), lenam, ier )
			CALL ST_LSTR ( wpres (ip), lenpr, ier )
			wlabel = wname(ip)(:lenam) // '/' // 
     +                           wpres(ip)(:lenpr) // 'mb'
C
		      ELSE IF ( ( iflags(JTIM) .ne. 0 ) .and. 
     +				( iflags(JNAM) .eq. 0 ) .and.
     +				( iflags(JSPD) .eq. 0 ) )  THEN
			CALL ST_LSTR ( timstr (ip), lenstr, ier )
			wlabel = timstr(ip) (5:11) 
C
		      ELSE IF ( ( iflags(JTIM) .eq. 0 ) .and. 
     +				( iflags(JNAM) .eq. 0 ) .and.
     +				( iflags(JSPD) .ne. 0 ) )  THEN
			iyoff  = -2
			CALL ST_LSTR ( wsped (ip), lenspd, ier )
			wlabel = wsped(ip)(:lenspd) // 'KT'
C
		      ELSE IF ( ( iflags(JTIM) .ne. 0 ) .and. 
     +		       		( iflags(JNAM) .ne. 0 ) .and.
     +				( iflags(JSPD) .eq. 0 ) )  THEN
			CALL ST_LSTR ( timstr (ip), lenstr, ier )
			CALL ST_LSTR ( wname (ip), lenam, ier )
			CALL ST_LSTR ( wpres (ip), lenpr, ier )
			wlabel = timstr(ip) (5:11) // CHCR //
     +				 wname(ip)(:lenam) // '/' //
     +				 wpres(ip)(:lenpr) // 'mb'
C
		      ELSE IF ( ( iflags(JTIM) .eq. 0 ) .and. 
     +				( iflags(JNAM) .ne. 0 ) .and.
     +				( iflags(JSPD) .ne. 0 ) )  THEN
			CALL ST_LSTR ( wname (ip), lenam, ier )
			CALL ST_LSTR ( wpres (ip), lenpr, ier )
			CALL ST_LSTR ( wsped (ip), lenspd, ier )
			wlabel = wname(ip)(:lenam) // '/' //
     +				 wpres(ip)(:lenpr) // 'mb' // CHCR //
     +                           wsped(ip)(:lenspd) // 'KT' 
C
		      ELSE IF ( ( iflags(JTIM) .ne. 0 ) .and. 
     +				( iflags(JNAM) .eq. 0 ) .and.
     +				( iflags(JSPD) .ne. 0 ) )  THEN
			iyoff  = -3
			CALL ST_LSTR ( timstr (ip), lenstr, ier )
			CALL ST_LSTR ( wsped (ip), lenspd, ier )
			wlabel = timstr(ip) (5:11) // CHCR //
     +				 wsped(ip)(:lenspd) // 'KT' 
C
		      ELSE IF ( ( iflags(JTIM) .ne. 0 ) .and. 
     +				( iflags(JNAM) .ne. 0 ) .and.
     +				( iflags(JSPD) .ne. 0 ) )  THEN
			iyoff  = -3
			CALL ST_LSTR ( timstr (ip), lenstr, ier )
			CALL ST_LSTR ( wname (ip), lenam, ier )
			CALL ST_LSTR ( wpres (ip), lenpr, ier )
			CALL ST_LSTR ( wsped (ip), lenspd, ier )
			wlabel = timstr(ip) (5:11) // CHCR //
     +			         wname(ip)(:lenam) // '/' //
     +				 wpres(ip)(:lenpr) // 'mb' // CHCR //
     +                           wsped(ip)(:lenspd) // 'KT' 	 
		    END IF
C
C*		    For a TEST report, prefix a character 'T' to the
C*		    label.
C
		    IF ( itest ( ip ) .ne. 0 ) THEN
			tlabel = wlabel
			wlabel = 'T' // tlabel
		    END IF
C
		    CALL GTEXT ( 'M', rlat(1,ip), rlon(1,ip),
     +				 wlabel, 0.0, 3, iyoff, ier )
C
C*	            Draw directional arrow.
C
		    IF  ( icolor(JMTN) .gt. 0 )  THEN 
			CALL GSCOLR ( icolor(JMTN), ier )
			CALL ST_CRNM ( wdir(ip), dirnum, ier)
C
C*                  	Flip arrow for proper direction.
C
			IF ( dirnum .le. 180.0 ) THEN
			   dirnum = dirnum + 180.0
			  ELSE
			   dirnum = dirnum - 180.0
			END IF
C
			IF  ( ier .eq. 0 )  THEN
			    CALL GDARR ( 'M', 1, rlat(1,ip),
     +					 rlon(1,ip), dirnum, ier)
			END IF
		    END IF
		END IF
	    END IF
C
C*	    Plot the quadrant winds and sea feet. Plot for last time
C*          period of each storm.
C
            IF  ( icflg(ip).eq. 0 )  THEN
C
C*      	Determine if radii are to be plotted at track points.
C
                DO ij = 1, icntr
                    CALL  ST_LSTR ( cmpnme(ij), icln, iret)
                    CALL  ST_LSTR ( cmptim(ij), itln, iret)
                    IF ( ( ( wocen (ip) .eq. cmpnme(ij)(:icln) ).and.
     +                 ( timstr(ip) .eq. cmptim(ij)(:itln) ) ) .or.
     +                 ( ( hvname) .and. ( ip .eq. nwrn ) ) ) THEN
C
C*                     Loop over possible forecast hours
C
                        DO ihr = I00HR, I120HR
                          IF ( iflags(ihr) .NE. 0 ) THEN
C
C*                          Infer synoptic hour, assuming that the
C*                          provided time stamp resembles nominal issuance 
C*                          time 3 hours later.
C
	                    CALL TI_CTOI ( timstr(ip), jtarr, ier )
C*                          [Note:  Could consider decreasing comparison hours
C*                           (3, 8, 9, 14, etc.) by 1 to compensate if timstr
C*                           is actual intead of nominal issue time, and so a
C*                           bit before intermediate synoptic hour.]
                            IF ( jtarr(4) .ge. 3 .AND. 
     +                           jtarr(4) .le. 8 ) THEN
                               jtarr(4) = 0
                            ELSE IF ( jtarr(4) .ge. 9 .AND. 
     +                                jtarr(4) .le. 14 ) THEN
                               jtarr(4) = 6
                            ELSE IF ( jtarr(4) .ge. 15 .AND. 
     +                                jtarr(4) .le. 20 ) THEN
                               jtarr(4) = 12
                            ELSE IF ( jtarr(4) .ge. 21 .AND. 
     +                                jtarr(4) .le. 23 ) THEN
                               jtarr(4) = 18
                            ELSE IF ( jtarr(4) .ge. 0 .AND. 
     +                                jtarr(4) .le. 2 ) THEN
C                              1800Z of previous day
                               minsub = 60 * ( 6 + jtarr(4)  )
                               CALL TI_SUBM( jtarr, minsub, jtarr, ier )
                            ENDIF
C*                          Synoptic hour ==> minutes are zero.
                            jtarr(5) = 0
C
                            nradii = 3
                            IF ( ihr .EQ. I00HR ) THEN
                                nradii = 4
                                mins = 0
                                istart = 1
                                fstypep = fstype(1,ip)
                                rlatp = rlat(1,ip)
                                rlonp = rlon(1,ip)
                            ELSEIF ( ihr .EQ. I06HR ) THEN
                                mins = 60 * 6
                                istart = 1
                                fstypep = fstype(1,ip)
                                rlatp = (rlat(1,ip) + rlat(2,ip)) / 2.0
                                rlonp = (rlon(1,ip) + rlon(2,ip)) / 2.0
                            ELSEIF ( ihr .EQ. I12HR ) THEN
                                mins = 60 * 12
                                istart = 5
                                fstypep = fstype(2,ip)
                                rlatp = rlat(2,ip)
                                rlonp = rlon(2,ip)
                            ELSEIF ( ihr .EQ. I18HR ) THEN
                                mins = 60 * 18
                                istart = 5
                                fstypep = fstype(2,ip)
                                rlatp = (rlat(2,ip) + rlat(3,ip)) / 2.0
                                rlonp = (rlon(2,ip) + rlon(3,ip)) / 2.0
                            ELSEIF ( ihr .EQ. I24HR ) THEN
                                mins = 60 * 24
                                istart = 8
                                fstypep = fstype(3,ip)
                                rlatp = rlat(3,ip)
                                rlonp = rlon(3,ip)
                            ELSEIF ( ihr .EQ. I30HR ) THEN
                                mins = 60 * 30
                                istart = 8
                                fstypep = fstype(3,ip)
                                rlatp = (rlat(3,ip) + rlat(4,ip)) / 2.0
                                rlonp = (rlon(3,ip) + rlon(4,ip)) / 2.0
                            ELSEIF ( ihr .EQ. I36HR ) THEN
                                mins = 60 * 36
                                istart = 11
                                fstypep = fstype(4,ip)
                                rlatp = rlat(4,ip)
                                rlonp = rlon(4,ip)
                            ELSEIF ( ihr .EQ. I42HR ) THEN
                                mins = 60 * 42
                                istart = 11
                                fstypep = fstype(4,ip)
                                rlatp = (rlat(4,ip) + rlat(5,ip)) / 2.0
                                rlonp = (rlon(4,ip) + rlon(5,ip)) / 2.0
                            ELSEIF ( ihr .EQ. I48HR ) THEN
                                mins = 60 * 48
                                istart = 14
                                fstypep = fstype(5,ip)
                                rlatp = rlat(5,ip)
                                rlonp = rlon(5,ip)
                            ELSEIF ( ihr .EQ. I54HR ) THEN
                                mins = 60 * 54
                                istart = 14
                                fstypep = fstype(5,ip)
                                rlatp = (rlat(5,ip) + rlat(6,ip)) / 2.0
                                rlonp = (rlon(5,ip) + rlon(6,ip)) / 2.0
                            ELSEIF ( ihr .EQ. I60HR ) THEN
                                mins = 60 * 60
                                istart = 17
                                fstypep = fstype(6,ip)
                                rlatp = rlat(6,ip)
                                rlonp = rlon(6,ip)
                            ELSEIF ( ihr .EQ. I66HR ) THEN
                                mins = 60 * 66
                                istart = 17
                                fstypep = fstype(6,ip)
                                rlatp = (rlat(6,ip) + rlat(7,ip)) / 2.0
                                rlonp = (rlon(6,ip) + rlon(7,ip)) / 2.0
                            ELSEIF ( ihr .EQ. I72HR ) THEN
                                mins = 60 * 72
                                istart = 20
                                fstypep = fstype(7,ip)
                                rlatp = rlat(7,ip)
                                rlonp = rlon(7,ip)
                            ELSEIF ( ihr .EQ. I78HR ) THEN
                                mins = 60 * 78
                                istart = 20
                                fstypep = fstype(7,ip)
                                rlatp = ( 3.*rlat(7,ip) ) + rlat(8,ip)
                                rlatp = rlatp / 4.0
                                rlonp = ( 3.*rlon(7,ip) ) + rlon(8,ip)
                                rlonp = rlonp / 4.0
                            ELSEIF ( ihr .EQ. I84HR ) THEN
                                mins = 60 * 84
                                istart = 20
                                fstypep = fstype(7,ip)
                                rlatp = (rlat(7,ip) + rlat(8,ip)) / 2.0
                                rlonp = (rlon(7,ip) + rlon(8,ip)) / 2.0
                            ELSEIF ( ihr .EQ. I90HR ) THEN
                                mins = 60 * 90
                                istart = 20
                                fstypep = fstype(7,ip)
                                rlatp = rlat(7,ip) + ( 3.*rlat(8,ip) )
                                rlatp = rlatp / 4.0
                                rlonp = rlon(7,ip) + ( 3.*rlon(8,ip) )
                                rlonp = rlonp / 4.0
                            ELSEIF ( ihr .EQ. I96HR ) THEN
                                mins = 60 * 96
                                istart = 23
                                fstypep = fstype(8,ip)
                                rlatp = rlat(8,ip)
                                rlonp = rlon(8,ip)
                            ELSEIF ( ihr .EQ. I120HR ) THEN
                                mins = 60 * 120
                                istart = 26
                                fstypep = fstype(9,ip)
                                rlatp = rlat(9,ip)
                                rlonp = rlon(9,ip)
                            ENDIF
C
C*                          plot storm position
C
                            IF ( (fstypep .eq. 'TD') .or.
     +				 (fstypep .eq. 'RL') ) THEN
                                sym = FLOAT ( isytyp(JTDP) )
                              ELSE IF (fstypep .eq. 'TS') THEN
                                sym = FLOAT ( isytyp(JTSM) )
                              ELSE IF (fstypep .eq. 'HU') THEN
                                sym = FLOAT ( isytyp(JHUR) )
                            END IF
                            IF ( ihr .NE. I00HR ) THEN
                              CALL GSCOLR ( 6, ier)
                              CALL GSMRKR ( 1, 0, 1., 1, ier)
                              CALL GSPCL ( 'M', 1, sym, rlatp, rlonp,
     +				         0, 0, ier )
                            END IF
C
C*                          plot date/time
C
                            IF ( iflags(JTIM) .NE. 0 .AND. 
     +                           ihr .NE. I00HR ) THEN
	                      CALL TI_ADDM ( jtarr, mins, jtarr, ier )
	                      CALL TI_ITOC ( jtarr, ctime, ier )
		              CALL GTEXT ( 'M', rlatp, rlonp,
     +				 ctime(5:11), 0.0, 3, -1, ier )
                            END IF
C
C*                          plot wind radii
C
                            IF  ( iflags (JQAD) .ne. 0 ) THEN
                             DO ii = istart, istart + nradii - 1
                              CALL ST_ILST ( windft(ii,ip), ' ', IMISSD, 
     +                                       5, iarr, nwnd, ier)
C
C*                            See if data are available for the point. 
C*			      Radii have been as great as 900 for sea feet.
C
                              IF ( ( iarr(2) .ne. IMISSD ) .and. 
     +                             ( iarr(2) .lt. 1000 ) .and.
     +				   ( .not. ERMISS ( rlatp ) ) ) THEN
C
C*                              Set the color based on the wind speed or
C*			        sea feet.
C
	                        IF  ( iarr(1) .eq. 64 )  THEN
		                    iarclr = 2
                                    iltyp = 1
	                          ELSE IF  ( iarr(1) .eq. 10 )  THEN
		                    iarclr = 15 
                                    iltyp = 1
	                          ELSE IF  ( iarr(1) .eq. 50 )  THEN
		                    iarclr = 5
                                    iltyp = 1
	                          ELSE IF  ( iarr(1) .eq. 34 )  THEN
	    	                    iarclr = 25
                                    iltyp = 1
	                          ELSE IF  ( iarr(1) .eq. 12 )  THEN
		                    iarclr = 3
                                    iltyp = 2
	                        END IF
C
		                CALL GSCOLR ( iarclr, ier )
                                CALL GSLINE (iltyp, 0 ,2, 0, ier)
C
C*                              Calculate the radius points.
C
                                kk = 1
                                DO jj = 2,5
                                    ang1 = 90. * (jj - 2)
                                    ang2 = 90. + ang1
C
C*                                  Change NM -> meters.
C
                                    rmeters = PR_HGNM (FLOAT(iarr(jj)))
C
C*                                  Calculate the circumference lat./lon.
C
                                    disdeg = 0.0
                                    CALL CLO_DLTLN ( rlatp, 
     +                                    rlonp, rmeters, disdeg, 
     +                                    xcmf, ycmf, iret )
C
C*                                  Draw the arcs in the quadrants.
C
                                    sys = 'M'
                                    CALL GG_ARC (sys, rlatp, 
     +                                   rlonp, xcmf, ycmf, inpt, 
     +                                   ang1, ang2, 10, xout, yout,
     +					 iret )
     				    CALL GLINE ( sys, inpt, xout, yout,
     +						 ier )
				    endpts (1) = xout (1)
				    endpts (2) = yout (1)
				    endpts (3) = xout (inpt)
				    endpts (4) = yout (inpt)
C
C*                                  Fill up array with arc endpoints.
C
                                    DO ll = 1,4
                                        fnlpts (kk,ll) = endpts(ll) 
                                    END DO
                                    kk = kk + 1
                                END DO
C
C*		                Draw connecting lines between quad. 
C*				segments.
C
                                CALL GG_LNUP ( fnlpts, iarclr, iltyp, 
     +                                         ier)
                              END IF
                             END DO
                            END IF
			  END IF
                        END DO
                    END IF
                END DO
            END IF
C
C*	    Plot the forecasted track. 
C
            IF ( track .and. ( icflg(ip) .eq. 0 ) ) THEN
                DO jj = 1, icntr
                    CALL  ST_LSTR ( cmpnme(jj), iclen, iret)
                    CALL  ST_LSTR ( cmptim(jj), itlen, iret)
                    IF ( ( ( wocen (ip) .eq. cmpnme(jj)(:iclen) ) .and.
     +                 ( timstr(ip) .eq. cmptim(jj)(:itlen) ) ) .or.
     +                 ( ( hvname) .and. ( ip .eq. nwrn ) ) ) THEN
                        CALL GSCOLR ( 6, ier)
                        CALL GSMRKR ( 1, 0, 1., 1, ier)
                        slat(1) = rlat(1,ip) 
                        slon(1) = rlon(1,ip)
                        DO kk = 2, nknt
                            slat(kk) = rlat(kk,ip) 
                            slon(kk) = rlon(kk,ip)
                        END DO
                        CALL GSLINE ( 13, 0, 1, 0, ier)
	                CALL GLINE ( 'M', nknt, slat, slon, ier )
                    END IF
                END DO
            END IF
	END DO
        hvname = .false.
C
C*	Reset the saved attributes.
C
	CALL GSCOLR ( jcolr, ier )
        CALL GSSPCL ( szspcl, jmkwid, ier )
        CALL GSTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr,
     +                jrrotn, jjust, ier )
	CALL GSLINE ( jltyp, jlthw, jwidth, jwhw, ier )
C*
	RETURN
	END
