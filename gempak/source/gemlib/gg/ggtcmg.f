	SUBROUTINE GG_TCMG ( dattim, icolor, ssize, iwidth, slsize, 
     +			     iswdth, centr, iret )
C************************************************************************
C* GG_TCMG								*
C*									*
C* This subroutine plots the current tropical marine cyclone graphic.   *
C*									*
C* GG_TCMG ( DATTIM, ICOLOR, SSIZE, IWIDTH, SLSIZE, ISWDTH, CENTR,      *
C*           IRET )                                                     *
C*									*
C* Input parameters:							*
C*	DATTIM		CHAR*		Ending time for tropical systems*
C*	ICOLOR (12)	INTEGER		Symbol, spec. line, area colors	*
C*	SSIZE     	REAL		Symbol size 			*
C*	IWIDTH 		INTEGER		Symbol width			*
C*	SLSIZE    	REAL		Special line size		*
C*	ISWDTH    	INTEGER		Special line width		*
C*	CENTR		CHAR*		Hurricane center name           *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 7/01	From GG_HRCN                            *
C* D. Kidwell/NCEP	10/01	Skip over added wind radii at 12 & 36 hr*
C* D. Kidwell/NCEP	 2/02	Removed arg. idrop, added centr; used 12*
C*				& 36 hr winds; added JREM; name posn fix*
C* A. Hardy/SAIC         2/02   Changed call FL_SCND			*
C* D. Kidwell/NCEP	 3/02	Skipped over fcst 50 & 64 kt wind radii *
C* D. Kidwell/NCEP	 7/02	Used Lowx symbol for TD as well as RL   *
C* D. Kidwell/NCEP	 2/03	Used Lowx symbol for all if subtropical *
C* D. Kidwell/NCEP	 2/03	Changed comment for forecast times      *
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM             *
C* D. Kidwell/NCEP	 5/04	Fixed bug for CPHC label placement      *
C* T. Lee/SAIC		 9/04	Replaced FL_TMPL with CTB_DTGET		*
C* A. Hardy/NCEP	11/04	Added calls to ST_RNUL			*
C* m.gamazaychikov/SAIC 12/04   Added ion flag to CTB_DTGET CS          *
C* m.gamazaychikov/SAIC 01/06   Changed templ string length to MXTMPL   *
C* m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
C* m.gamazaychikov/SAIC 04/08   Add code to plot extratropical          *
C* F. J. Yen/NCEP        4/08   Added bin mins & mstrct to CTB_DTGET CSC*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( NW = 500 )
	PARAMETER	( NCLR = 12, ITRACK = 6 )
C*
	PARAMETER	( JHUR = 1, JTSM = 2, JREM = 3 )
     	PARAMETER	( JSYM = 1, JARR = 2 )
C*
	CHARACTER*(*)	dattim, centr
	INTEGER		icolor (*), itype
C*
	CHARACTER	path*25, templ*(MXTMPL), cdttm*20, dattm2*20, 
     +			buffer*100, tfile*128, wlabel*40, dattm4*20, 
     +			tmstr4*20, tlabel*40, carr (11)*20, ttype*2, 
     +			tstrt*20, tname*12, tocen*6, tadnm*2, 
     +         		wname (NW)*12, timstr (NW)*20, wtype (NW)*2,
     +	        	wocen (NW)*6, wadnm (NW)*2, ocean*2,
     +			fstype (ITRACK,NW)*2, f34kt (ITRACK,NW)*60, 
     +			stime*20, flstrt*160, temp4*20, timlbl*20, 
     +			year4*4, month (12)*10,	dayw (7)*4, valid*40
	CHARACTER*(MXFLSZ)      filnam, files (MXNMFL)
	INTEGER		itest (NW), istrt (5), isytyp (3), itarr (5),
     +			jtarr (5), icorr (NW), lmonth (12), isbflg (NW)
	LOGICAL		done, found, good (NW), cphc
	REAL		rlat (ITRACK,NW), rlon (ITRACK,NW), elat (4), 
     +			elon (4), radii (ITRACK,2)
C*
	DATA		month  / 'JANUARY ', 'FEBRUARY ',     'MARCH ', 
     +			           'APRIL ',      'MAY ',      'JUNE ',
     +			            'JULY ',   'AUGUST ', 'SEPTEMBER ',
     +			         'OCTOBER ', 'NOVEMBER ',  'DECEMBER ' /
	DATA		lmonth / 8, 9, 6, 6, 4, 5, 5, 7,10, 8, 9, 9 / 
	DATA		dayw   / 'SUN ', 'MON ', 'TUE ', 'WED ', 'THU ',
     +				 'FRI ', 'SAT ' /
C-----------------------------------------------------------------------
	iret = 0
C
C*	Scan the directory for all of the hurricane data files.
C
	filnam = 'HRCN'
	path   = ' '
	templ  = ' '
	CALL ST_NULL ( filnam, filnam, nf, ier )
	CALL CTB_DTGET ( filnam, path, templ, ic, is, if, ir, ii, ion,
     +          ihb, mnb, iha, mna, mstrct, idtmch, ier )
	CALL ST_RNUL ( path, path, lens, ier )
	CALL ST_RNUL ( templ, templ, lens, ier )
	CALL ST_LSTR ( path, lenp, ier )
        nexp = MXNMFL
        iorder = 1
	CALL FL_SCND ( path, templ, iorder, nexp, files, nfile, ier )
C
C*	Check for the last file requested by the user.
C
	CALL ST_LCUC ( dattim, dattim, ier )
	itype = 1
	IF  ( ( dattim .eq. 'LAST' ) .or. ( dattim .eq. ' ' ) )  THEN
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
C*	Find the earliest file to start searching.  Subtract 6 hours
C*	from the time given.
C
	minuts = 360
	CALL TI_CTOI ( dattm2, itarr, ier )
	IF ( ier .ne. 0 ) THEN
	    CALL ER_WMSG ( 'TI', ier, dattim, ierr )
	    iret = ier
	    RETURN
	END IF	    
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
	ntrpcy = 0
	done   = .false.
	ifl    = 1
	DO WHILE  ( ( ifl .le. nfile ) .and. ( .not. done ) )
	  IF  ( files(ifl) .gt. filnam )  THEN
	      done = .true.
	    ELSE
	      IF  ( files(ifl) .ge. flstrt )  THEN
		tfile = path(:lenp) // '/' // files(ifl)
		CALL FL_SOPN ( tfile, lunf, ier )
C
		iostat = 0
		DO WHILE  ( iostat .eq. 0 )
		  READ ( lunf, 2, IOSTAT = iostat ) buffer
2		  FORMAT ( A )
		  IF  ( iostat .eq. 0 )  THEN
		    IF  ( buffer(1:1) .eq. '|' )  THEN
			CALL ST_CLST ( buffer, '|', ' ', 11,
     +				       carr, num, ier )
C
			CALL ST_NUMB ( carr(11), jflag, ier )
			jcorr = MOD ( jflag, 2 )
			jtest = jflag / 2
			ttype = carr(2)
			IF ( ( carr(2) ( 3:3 ) .eq. 'S' ) .or.
     +			     ( carr(2) ( 3:3 ) .eq. 'B' ) ) THEN
			    jsbflg = 1
			  ELSE
			    jsbflg = 0
			END IF
			tstrt = carr(3)
			tname = carr(4)
			tocen = carr(5)
                        tadnm = carr(6)
			found = .false.
C
C*			Check for later advisory or correction.  A later
C*			advisory check looks for a match on the storm 
C*			identifier with a later time.  A correction 
C*			looks for a match on the storm identifier and
C*			either the time or the advisory number.
C
			DO ii = 1, ntrpcy
			    IF ( tocen .eq. wocen (ii) ) THEN
				IF ( tstrt .gt. timstr (ii) ) THEN
				    found = .true.
			            iw    = ii
				  ELSE IF ( jcorr .eq. 1 ) THEN
				    IF ( ( tadnm .eq. wadnm (ii) ) .or.
     +					 ( tstrt .eq. timstr (ii) ) )
     +				         THEN
					 found = .true.
			                 iw    = ii
				    END IF
				END IF
			    END IF	
			END DO
C
C*			Check for a correction to a storm identifier.
C*			The time, storm type, storm name and advisory
C*			number must match.
C
			IF ( ( .not. found ) .and. jcorr .eq. 1 ) THEN
			    DO ii = 1, ntrpcy
				IF ( ( tstrt .eq. timstr (ii) ) .and.
     +				     ( ttype .eq. wtype (ii) ) .and.
     +				     ( jsbflg .eq. isbflg (ii) ) .and.
     +				     ( tname .eq. wname (ii) ) .and.
     +				     ( tadnm .eq. wadnm (ii) ) ) THEN
				    found = .true.
				    iw    = ii
				END IF
			    END DO
			END IF
C
			IF  ( found )  THEN
			    jw = iw
			  ELSE
			    ntrpcy = ntrpcy + 1
			    jw = ntrpcy
			END IF
C
			timstr (jw) = tstrt
			wtype (jw)  = ttype
			isbflg (jw) = jsbflg
			wname (jw)  = tname
			wocen (jw)  = tocen
                        wadnm (jw)  = tadnm
			itest (jw)  = jtest
			icorr (jw)  = jcorr
C
C*                      Read the lat/lon values and storm type.
C
                        nknt   = 0
                        jostat = 0
                        DO ii = 1, ITRACK
			    fstype ( ii, jw ) = ' ' 
                            READ ( lunf, 2, IOSTAT = jostat ) buffer
                            IF  ( jostat .eq. 0 )  THEN
                                IF  ( buffer (1:1) .eq. '|' )  THEN
                                    CALL FL_BKSP ( lunf, ier )
                                    jostat = -1
                                  ELSE
                                    nknt = nknt + 1
                                    READ (buffer, 1000)
     +                                    rlat ( nknt, jw ), 
     +					  rlon ( nknt, jw ),
     +					  fstype ( nknt, jw )
1000                                FORMAT ( 2F9.2, 4X, A )
                                END IF
                            END IF
                        END DO
C
C*			Read the wind radii and sea feet data.  64 and
C*			50kt forecast winds, if present, will be 
C*			ignored.
C
                        knt    = 0
			jostat = 0
                        DO WHILE ( jostat .eq. 0 )
                            READ ( lunf, 2, IOSTAT = jostat ) buffer
                            IF  ( jostat .eq. 0 )  THEN
                                IF  ( buffer(1:1) .eq. '|' )  THEN
                                    CALL FL_BKSP ( lunf, ier )
                                    jostat = -1
                                  ELSE
				    CALL ST_CLST ( buffer, ' ', ' ', 5,
     +						   carr, num, ier )
C
C*				    Use 34kt forecast wind radii at 
C*				    current, 12, 24, 36, 48 and 72 hrs.
C
				    IF ( carr ( 1 ) .eq. '34' ) THEN
                                        knt = knt + 1
C
C*					Check that at most ITRACK points
C*					are saved.
C
					IF  ( knt .le. ITRACK )
     +                                        f34kt ( knt, jw ) = buffer
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
C
	  END IF
	  ifl = ifl + 1
	END DO
C
C*	Query current settings.
C
	CALL GQCOLR ( jcolr, ier )
	CALL GQFILL ( szfil, jftyp, ier )
	CALL GQSPCL ( jspcl, jmkhw, szspcl, jmkwid, ier )
	CALL GQTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr, jrrotn, jjust,
     +		      ier )
	CALL GQSPLN ( jsltyp, jslstr, jsldir, slsiz, jslwid, ier )
	CALL GQLINE ( jltyp, jlthw, jwidth, jwhw, ier )
C
	CALL GSTEXT ( 1, 2, 1.0, 1, 111, 1, 1, ier )
C
C*	Set symbol attributes.
C
	isytyp ( JHUR ) = 26
	isytyp ( JTSM ) = 25
	isytyp ( JREM ) = 38
C
	IF  ( ( ssize .le.  0.0 ) .or.
     +	      ( ssize .gt. 10.0 ) ) THEN
	    ssize  = 1.8
	    ssizer = 1.5
	  ELSE
	    ssizer = ssize
	END IF
	IF  ( ( iwidth .le.  0 ) .or.
     +	      ( iwidth .gt. 10 ) )  iwidth = 2
C
C*	Set special line attributes.
C
        IF  ( ( slsize .le.  0.0 ) .or.
     +        ( slsize .gt. 10.0 ) )  slsize = 0.4
	IF  ( ( iswdth .le.  0 ) .or.
     +	      ( iswdth .gt. 10 ) )  iswdth = 2
C
	CALL GSSPLN ( 6, 0, 1, slsize, iswdth, ier )
C
C*	Make sure there is only one report for each storm identifier.
C
	DO ip = 1, ntrpcy
	    good ( ip ) = .true.
	END DO
	DO ip = 1, ntrpcy - 1
	    IF ( good ( ip ) ) THEN
	        DO jj = ip+1, ntrpcy
		    IF ( wocen ( ip ) .eq. wocen ( jj ) ) THEN
		        IF ( timstr ( ip ) .ge. timstr ( jj ) ) THEN
			    IF ( ( icorr ( jj ) .eq. 1 ) .and.
     +				 ( timstr (ip) .eq. timstr (jj) ) ) THEN
				good ( ip ) = .false.
			      ELSE
			        good ( jj ) = .false.
			    END IF
		          ELSE
			    good ( ip ) = .false.
		        END IF
		    END IF
	        END DO
	    END IF
	END DO
C
C*	Skip over Atlantic storms if center is CPHC.
C
	cphc = centr ( 1:4 ) .eq. 'CPHC'
	IF ( cphc ) THEN
	    DO ip = 1, ntrpcy
	        IF ( good ( ip ) ) THEN
		    IF ( wocen ( ip ) ( 1:2 ) .eq. 'AL' ) THEN
      		        good ( ip ) = .false.
		    END IF
	        END IF
	    END DO
	END IF
C
C*	Get the danger area radii from the table.
C
	CALL GG_TCTB ( 'tcmgda.tbl', 'hcnadv', ITRACK, radii, ier )
	IF ( ier .ne. 0 ) THEN
	    ntrpcy = 0
	    iret   = ier
	END IF
C
C*	Plot the graphic for each valid advisory.
C
	iatl  = 2
	ipac  = 2
	found = .false.
	DO ip = 1, ntrpcy
	  IF ( good ( ip ) ) THEN
C
C*          Change the start time to be 60 minutes before the actual
C*          start time of the advisory.
C
            CALL TI_CTOI ( timstr ( ip ), istrt, ier )
            CALL TI_SUBM ( istrt, 60, istrt, ier )
            CALL TI_ITOC ( istrt, temp4, ier )
            CALL TI_DTM4 ( temp4, tmstr4, ier )
C
	    IF ( tmstr4 .le. dattm4 )  THEN
C
C*		Set the color for the danger area.  The same color
C*		sequence is used for Atlantic and Pacific regions.
C
		ocean = wocen ( ip ) ( 1:2 )
		icen  = 1
		IF ( ocean .eq. 'AL' ) THEN
		    iatl = iatl + 1
		    IF ( iatl .gt. NCLR ) iatl = NCLR
		    icda  = icolor ( iatl ) 
		  ELSE
		    ipac = ipac + 1
		    IF ( ipac .gt. NCLR ) ipac = NCLR
		    icda  = icolor ( ipac )
		    IF ( cphc ) THEN
			ocean = 'CP'
			icen  = 2
		    END IF
		END IF
C
		IF  ( icda .ne. 0 )  CALL GSCOLR ( icda, ier )
		CALL GSFILL ( 0., 2, ier )
C
C*		Draw the danger area.
C
     		CALL GG_TCDA ( rlat ( 1, ip ), rlon ( 1, ip ), 
     +			       f34kt ( 1, ip ), ocean,
     +			       radii ( 1, icen ), elat, elon, ier )
C
C*		Plot the storm symbols at each track point.
C
		ic = icolor ( JSYM )
		IF  ( ic .ne. 0 )  CALL GSCOLR ( ic, ier )
		ipts = 0
		DO im = 1, ITRACK
		    sym  = -1.
	            CALL GSSPCL ( ssize, iwidth, ier )
		    IF ( ( fstype ( im, ip ) .eq. 'TD' ) .or.
     +		         ( fstype ( im, ip ) .eq. 'RL' ) .or.
     +                   ( fstype ( im, ip ) .eq. 'EX' ) .or.
     +			 ( isbflg ( ip ) .eq. 1 ) ) THEN
	                CALL GSSPCL ( ssizer, iwidth, ier )
		        sym  = FLOAT ( isytyp ( JREM ) )
		      ELSE IF  ( fstype ( im, ip ) .eq. 'TS' )  THEN
		        sym  = FLOAT ( isytyp ( JTSM ) )
		      ELSE IF  ( fstype ( im, ip ) .eq. 'HU' )  THEN
		        sym  = FLOAT ( isytyp ( JHUR ) )
		    END IF
C
C*		    Draw the symbols.
C
		    IF ( sym .ge. 0. ) THEN
		        CALL GSPCL ( 'M', 1, sym, rlat ( im, ip ), 
     +				     rlon ( im, ip ), 0, 0, ier )
			ipts = im
		    END IF
		END DO
C
C*		Plot the storm name.
C
		IF ( ipts .gt. 0 ) THEN
C
C*		    Try to get an optimum position for the storm name.
C
		    ipos  = 1
		    ixoff = 0
		    iyoff = -3
		    IF ( fstype ( 1, ip ) .ne. 'TD' )  THEN
			cplon = -150.
		      ELSE
			cplon = -160.
		    END IF
C
		    IF ( ( elat (1) .lt. 12. ) .or.
     +		         ( cphc .and. ( elon (1) .gt. cplon ) ) ) THEN
			ipos  = 2
			ixoff = 3
			iyoff = 0
			IF ( ( elat (2) .lt. 11. ) .or. ( cphc .and.
     +			     ( elon (2) .gt. cplon ) ) ) THEN
			    ipos  = 3
			    ixoff = 0
			    iyoff = 3
			    IF ( ocean .ne. 'AL' ) THEN
      				IF ( ( elat (3) .gt. 38. ) .or. ( cphc
     +				       .and. ( elon (3) .gt. cplon ) ) )
     +				       THEN 
				    ipos  = 4
				    ixoff = 0
				    iyoff = -6
				    IF ( cphc .and. 
     +					 ( elon (3) .gt. cplon ) .and.
     +					 ( elon (4) .lt. -140. ) ) THEN
     				        elon ( 4 ) = AMIN1 ( elon ( 4 ),
     +					                     cplon )
				    END IF
				END IF
			    END IF
			END IF
		    END IF
		    wlabel = wname ( ip )
C
C*		    For a TEST report, prefix a character 'T' to the 
C*		    label.
C
		    IF ( itest ( ip ) .ne. 0 ) THEN
		        tlabel = wlabel
		        wlabel = 'T' // tlabel
		    END IF
C
		    IF ( icda .ne. 0 )  CALL GSCOLR ( icda, ier )
	            CALL GSTEXT ( 22, 0, 1.3, 0, 0, 0, 0, ier )
		    IF ( fstype ( 1, ip ) .eq. 'TD' )  THEN
	                CALL GSTEXT ( 2, 0, 0.0, 0, 0, 0, 0, ier )
			tlabel = wlabel
			wlabel = 'TROPICAL' // CHCR // 'DEPRESSION' //
     +				 ' ' // tlabel
			IF ( ipos .eq. 1 ) THEN
			    ixoff = 3
			    iyoff = -4
			END IF
		    END IF
		    CALL GTEXT ( 'M', elat ( ipos ), elon ( ipos ),
     +			         wlabel, 0.0, ixoff, iyoff, ier )
	            CALL GSTEXT ( 1, 0, 1.0, 0, 0, 0, 0, ier )
		END IF
C
C*		Draw the special lines (arrows between track points).
C
		ic = icolor ( JARR )
		IF ( ic .ne. 0)  CALL GSCOLR ( ic, ier )
		DO ii = 1, ipts - 1
		    CALL GSPLN ( 'M', 2, rlat (ii, ip), rlon (ii, ip),
     +			         ier )
		END DO
C
C*		Get the latest valid time to use for the label.
C
		IF ( .not. found ) THEN
		    found = .true.
		    timlbl = timstr ( ip )    
		  ELSE
		    IF ( timlbl .lt. timstr (ip) ) timlbl = timstr (ip)
		END IF
	    END IF
	  END IF
	END DO
C
	IF ( found ) THEN
	    CALL GSCOLR ( 1, ier )
C
C*	    Get the full time string for the label.
C
	    CALL TI_CTOI ( timlbl, itarr, ier )
	    CALL ST_INCH ( itarr ( 1 ), year4, ier )
	    CALL TI_DAYW ( itarr, idayw, ier )	
	    imo   = itarr ( 2 )
	    IF ( timlbl ( 5:5 ) .eq. '0' ) THEN
	        timlbl ( 5:5 ) = ' '
		lenm = lmonth ( imo ) - 1
	      ELSE
		lenm = lmonth ( imo )
	    END IF
	    valid = 'VALID:  ' // timlbl ( 8:11 ) // ' UTC ' //
     +		    dayw ( idayw ) // month ( imo ) ( :lenm ) //
     +		    timlbl ( 5:6 ) // ', ' // year4
C
	    IF ( iatl .gt. 2 ) THEN
	    	CALL GG_TCLB ( valid, 1, ier )
	    END IF
	    IF ( ipac .gt. 2 ) THEN
		IF ( cphc ) THEN
		    ibasin = 3
		  ELSE
		    ibasin = 2
		END IF
	    	CALL GG_TCLB ( valid, ibasin, ier )
	    END IF
	END IF
C
C*	Reset the saved attributes.
C
	CALL GSCOLR ( jcolr, ier )
	CALL GSFILL ( szfil, jftyp, ier )
        CALL GSSPCL ( szspcl, jmkwid, ier )
        CALL GSTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr, jrrotn, jjust,
     +		      ier )
	CALL GSSPLN ( jsltyp, jslstr, jsldir, slsiz, jslwid, ier )
	CALL GSLINE ( jltyp, jlthw, jwidth, jwhw, ier )
C*
	RETURN
	END
