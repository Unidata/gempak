	SUBROUTINE GG_WCP ( date, icolor, iwidth, iflags, iret)
C************************************************************************
C* GG_WCP								*
C*									*
C* This subroutine reads the decoded WCP files and displays the watch	*
C* areas.								*
C*									*
C* GG_WCP ( DATE, ICOLOR, IWIDTH, IFLAGS, IRET)				*
C*									*
C* Input parameters:							*
C*	DATE		CHAR*		Ending time for watches		*
C*	ICOLOR (12)	INTEGER		Line colors			*
C*	IWIDTH (12)	INTEGER		Line widths			*
C*	IFLAGS (3)	INTEGER		Flags for labels		*
C*					   0 = false			*
C*					   1 = true			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 4/05	Parts extracted from GG_WCUR and GG_WTCH*
C* m.gamazaychikov/SAIC 01/06   Changed templ string length to MXTMPL   *
C* m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
C* F. J. Yen/NCEP        4/08   Added bin mins & mstrct to CTB_DTGET CSC*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ggcmn.cmn'
C*
        PARAMETER       ( JSVR = 1, JTOR = 2 )
        PARAMETER       ( JWTM = 1, JWNM = 2, JCLC = 3 )
C*
	CHARACTER*(*)	date
        INTEGER		icolor(*), iwidth(*), iflags(*)
C*
	CHARACTER	path*25, templ*(MXTMPL), cdttm*20, dattm2*20, 
     +			buffer*100, tfile*128, tmstr4*20,
     +			tmstp4*20, tmiss4*20, carr(7)*20, tnum*4, 
     +			temp4*20, tmissu*20
C*
	CHARACTER	stime*20, flstrt*160
        CHARACTER       wlabel*80, t90*20
	INTEGER		itarr(5), jtarr(5), itype 
        INTEGER         istrt(5)
C*
	LOGICAL		done, done2
C-----------------------------------------------------------------------
	iret = 0
        dattim = date
C
C*	Scan the directory for all of the watch data files.
C
	filnam = 'WCP'
	path   = ' '
	templ  = ' '
	CALL ST_NULL ( filnam, filnam, nf, ier )
	CALL CTB_DTGET ( filnam, path, templ, ic, is, if, ir, ii, ion,
     +          ihb, mnb, iha, mna, mstrct, idtmch, ier )
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
	nwtch = 0
	wnum ( 1 ) = ' '
	done  = .false.
	ifl = 1
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
 2		      FORMAT ( A )
		      IF ( iostat .eq. 0 .and. nwtch .ge. NW) THEN
			  CALL ER_WMSG ( 'GG', 5, ' ', ierr)
			  iostat = -4
		      END IF
		      IF  ( iostat .eq. 0 )  THEN
			  IF  ( buffer(1:1) .eq. '|' )  THEN
			    CALL ST_CLST ( buffer, '|', ' ', 7,
     +				       carr, num, ier )
			    CALL ST_NUMB ( carr ( 6 ), nmwtch, ier )
			    CALL ST_INCH ( nmwtch, tnum, ier )
			    carr ( 6 ) = tnum
			    CALL ST_NUMB ( carr ( 7 ), ipoints, ier )
C
C*			    Add a new watch (wtype ="TS" or "TN") or
C*			    entry with wtype "NA"
C			  
			    nwtch = nwtch + 1
			    jw    = nwtch
			    wtype (jw)  = carr (2)
			    timist (jw) = carr (3)
			    timstr (jw) = carr (4)
			    timstp (jw) = carr (5)
			    wnum (jw)   = carr (6)
			    npt (jw)    = 0
			  END IF
		   	  IF ( carr(2) .ne. 'NA' ) THEN
C
C*		            Read the lat/lon coordinates from the file.
C
		            knt    = 0
		            jostat = 0
		            DO WHILE  ( jostat .eq. 0 )
			      READ ( lunf, 2, IOSTAT = jostat ) buffer
			      IF  ( jostat .eq. 0 )  THEN
			        IF  ( buffer(1:1) .eq. '|' )  THEN
			          CALL FL_BKSP ( lunf, ier )
		       	          jostat = -1
			         ELSE
			          IF  ( jw .ne. 0 )  THEN
			            knt = knt + 1
				    READ (buffer, 1000)  rlat(knt,jw),
     +				        	 rlon(knt,jw)
1000				    FORMAT ( 2F9.2 )
			          END IF
			        END IF
			      END IF
		            END DO
C
			    IF ( jw .ne. 0 )  npt(jw) = knt
		            IF  ( knt .ne. 0 )  THEN
C
C*			      If line is not already closed, close it.
C
		              IF ( rlat(knt,jw) .ne. rlat(1,jw) .or.
     +				   rlon(knt,jw) .ne. rlon(1,jw) ) THEN
			   
		 	        npt(jw) = knt + 1
			        rlat(npt(jw),jw) = rlat(1,jw)
			        rlon(npt(jw),jw) = rlon(1,jw)
			      END IF
		            END IF
			  END IF
C
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
        ij = nwtch
C
C*	Search backwards to find most recent watches
C*	Plot only the watches with the same issue time.
C
	tmissu = ' '
	done2 = .false.
	ii = nwtch
	DO WHILE ( ( ii .ge. 1 ) .and. ( .not. done2 ) )
C
          CALL TI_DTM4 ( timstp ( ii ), tmstp4, ier )
	  IF ( ier .ne. 0 ) THEN
	      CALL ER_WMSG ( 'TI', ier, timstp(ii), ierr )
	  ENDIF	    		    
          CALL TI_DTM4 ( timstr ( ii ), tmstr4, ier1 )
	  IF ( ier1 .ne. 0 ) THEN
	      CALL ER_WMSG ( 'TI', ier1, timstr(ii), ierr )
	  ENDIF	    		    
          CALL TI_DTM4 ( timist ( ii ), tmiss4, ier2 )
	  IF ( ier2 .ne. 0 ) THEN
	      CALL ER_WMSG ( 'TI', ier2, timist(ii), ierr )
	  ENDIF	    		    
	      ier = ier + ier1 + ier2
	  IF ( ier .eq. 0 ) THEN
C
	      IF ( wtype (ii) .ne. 'NA' ) THEN
                  IF ( (dattim .eq. 'ALL' ) .or.
     +               ( ( ( tmstp4 .gt. dattm4 ) .and.
     +                   ( tmstr4 .le. dattm4 ) ) .and.
     +		     ( tmiss4 .le. dattm4 ) ) ) THEN
	            IF ( tmissu .eq. ' ' ) tmissu = tmiss4
	            IF ( tmissu .eq. tmiss4 .or.
     +				dattim .eq. 'ALL') THEN
                        wnum(ij) = wnum(ii)
                        timstr(ij) = tmstr4
                        timstp(ij) = tmstp4
                        wtype(ij) =  wtype(ii)
                        npt(ij) =  npt(ii)
C
                        DO jj = 1, npt(ii)
		          rlat(jj,ij) = rlat(jj,ii)
                          rlon(jj,ij) = rlon(jj,ii)
                        END DO
                        ij = ij - 1
	             END IF
	           ELSE
C
C*	             If ( ij .ne. nwtch) then found watches with a
C*	             later issue time, so done.
C
	             IF ( ij .ne. nwtch .and. tmiss4 .gt. dattm4 )
     +				done2 = .true.
                  END IF
	        ELSE
C
C*	        no active watches
C
	          IF ( tmstr4 .le. dattm4 .and. dattim .ne. 'ALL' )
     +				done2 = .true. 
	      END IF
	  END IF
	    ii = ii - 1
        END DO
C
C*	nwtch is the number of entries (watches and "NA"s).
C	nmwtch is the actual number of watches.
C
        nmwtch = nwtch - ij 
	IF ( nmwtch .le. 0 ) RETURN
C
C*	Query current settings.
C
	CALL GQCOLR ( jcolr, ier )
	CALL GQLINE ( jtype, jtyhw, jwidth, jwdhw, ier )
	CALL GQSPLN ( jsltyp, jslstr, jsldir, sizsp, jslwid, ier )
	CALL GQTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr,
     +		      jrrotn, jjust, ier )
C
C*	Set attributes and defaults.
C
	CALL GSTEXT ( 21, 2, 1.0, 1, 111, 1, 1, ier )
C
	DO  i = 1, 12
	  IF  ( ( iwidth(i) .le.  0 ) .or.
     +		( iwidth(i) .gt. 10 ) )  iwidth(i) = 3
	END DO
C
C*	Plot the graphic for each valid report.
C
	DO ip = ij + 1, nwtch
C
 	  CALL TI_DTM4 ( timstp ( ip ), tmstp4, ier )
 	  CALL TI_DTM4 ( timstr(ip), tmstr4, ier )
C
	  IF ( ier .eq. 0 ) THEN
  	      IF  ( ( dattim .eq. 'ALL' ) .or. 
     +              ( ( tmstp4 .gt. dattm4 ) .and.
     +		    ( tmstr4 .le. dattm4 ) ) )  THEN

 	    	  CALL TI_CTOI ( timstr(ip), istrt, ier )
C
C*		  Set the line type for watch.
C
		  iltyp  = 1
		  isltyp = 6
C
C*		  Check if the watch is within 90 minutes of 
C*		  expiration and change the line type.
C
		  CALL TI_CTOI ( tmstp4, istrt, ier )
	  	  CALL TI_SUBM ( istrt, 90, istrt, ier )
		  CALL TI_ITOC ( istrt, temp4, ier )
		  CALL TI_DTM4 ( temp4, t90, ier )
		  IF ( t90 .le. dattm4 ) iltyp = 12
C
C*		  Set the color based on the type of watch if the color
C*		  code is 0, or on the last digit of watch number if the
C*		  color code is 1.
C
	  	  CALL ST_RMBL ( wnum(ip), wnum(ip), lenw, ier )
		  CALL ST_NUMB ( wnum(ip)(lenw:lenw), iwn, ier )
		  idx = iwn + 3
		  IF  ( wtype(ip) .eq. 'TN' )  THEN
		      IF ( iflags(JCLC) .eq. 0 ) idx = JTOR 
		      ic = icolor(idx)
		      CALL GSLINE ( iltyp, 0, iwidth(idx), 0, ier )
		    ELSE IF  ( wtype(ip) .eq. 'TS' )  THEN
		      IF ( iflags(JCLC) .eq. 0 ) idx = JSVR
                      ic = icolor(idx)
		      CALL GSLINE ( iltyp, 0, iwidth(idx), 0, ier )
		  END IF
C
C*		  Draw the polygon.
C
		  IF  ( ic .ne. 0 )  THEN
		      CALL GSCOLR ( ic, ier )
		      CALL GLINE ( 'M', npt(ip), rlat(1,ip),
     +			             rlon(1,ip), ier )
C
C*		      Plot the text at the lower left portion of
C*		      the polygon.
C
		
		      IF  ( wtype(ip) .ne. 'NA' )  THEN
			CALL GG_WLBL ( npt ( ip ), rlat ( 1,ip ),
     +				       rlon ( 1,ip ), alat, alon,
     +				       ier )
		      END IF
C
		      IF  ( ( iflags(JWTM) .eq. 0 ) .and. 
     +		          ( iflags(JWNM) .eq. 0 ) )   THEN
			wlabel = ' '
C
		       ELSE IF  ( ( iflags(JWTM) .eq. 0 ) .and. 
     +			         ( iflags(JWNM) .ne. 0 ) )   THEN
			iyoff  = -2
			wlabel = wnum(ip)
C
		       ELSE IF ( ( iflags(JWTM) .ne. 0 ) .and. 
     +			        ( iflags(JWNM) .eq. 0 ) )   THEN
			iyoff  = -2
		        wlabel = timstr(ip) (10:13) // '-' //
     +		                 timstp(ip) (10:13)
C
		       ELSE IF ( ( iflags(JWTM) .ne. 0 ) .and. 
     +			        ( iflags(JWNM) .ne. 0 ) )   THEN
			iyoff  = -3
			wlabel = wnum(ip) // CHCR //
     +				 timstr(ip) (10:13) // '-' //
     +				 timstp(ip) (10:13)
		      END IF
	
C
C*		      Plot the text label for the polygon.
C	
		      CALL GTEXT ( 'M', alat, alon, wlabel,
     +			             0.0, 0, iyoff, ier )
		  END IF
  	      END IF
	  END IF
	END DO
C
C*	Reset the saved attributes.
C
	CALL GSCOLR ( jcolr, ier )
	CALL GSLINE ( jtype, jtyhw, jwidth, jwdhw, ier )
	CALL GSSPLN ( jsltyp, jslstr, jsldir, sizsp, jslwid, ier )
	CALL GSTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr,
     +		      jrrotn, jjust, ier )
C*

	RETURN
	END
