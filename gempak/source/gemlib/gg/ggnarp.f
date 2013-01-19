	SUBROUTINE GG_NARP ( trkfil, dattm2, icolor, mrktyp, ssize,
     +			     iwidth, lwidth, iflags, strnam, models, 
     +			     match, iret )
C************************************************************************
C* GG_NARP								*
C*									*
C* This subroutine reads and plots ATCF data from a single storm file,  *
C* for specified models and time(s).                                    *
C*									*
C* GG_NARP ( TRKFIL, DATTM2, ICOLOR, MRKTYP, SSIZE, IWIDTH, LWIDTH,     *
C*	     IFLAGS, STRNAM, MODELS, MATCH, IRET )                      *
C*									*
C* Input parameters:							*
C*	TRKFIL		CHAR*		ATCF track file name            *
C*	DATTM2		CHAR*		Initial time for ATCF track     *
C*	ICOLOR(NA)	INTEGER		Line and marker colors		*
C*	MRKTYP(NA)	INTEGER		Marker symbol numbers           *
C*	SSIZE (NA)	REAL		Marker sizes			*
C*	IWIDTH(NA)	INTEGER		Marker line widths		*
C*	LWIDTH(NA)	INTEGER		Line widths			*
C*	IFLAGS (4)	INTEGER		Flags for labels		*
C*					  0 = false			*
C*					  1 = true			*
C*	STRNAM		CHAR*		Tropical storm name or number   *
C*	MODELS(NA)	CHAR*		Array of all valid model names  *
C*									*
C* Output parameters:							*
C*	MATCH		LOGICAL		Flag for match on storm name    *
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 6/00	                                        *
C* D. Kidwell/NCEP	 6/00	Rounded initial time down to match data *
C* D. Kidwell/NCEP	 7/00	Fixed bug in actual track plot          *
C* D. Kidwell/NCEP	 8/00	Added check to drop old 90 series data  *
C* F. J. Yen/NCEP	 5/01	Updated for new ATCF format.  Renamed	*
C*				temporarily from GG_ATRP to GG_NARP.	*
C* F. J. Yen/NCEP	 5/01	Used default storm name if not given	*
C* F. J. Yen/NCEP	 5/01	Used latest storm name given by CARQ.	*
C* F. J. Yen/NCEP	 6/01   Plot all forecast hours.		*
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM             *
C* m.gamazaychikov/SAIC 07/08	Changed the size of tlat, tlon to 500   *
C* T. Piper/SAIC	09/08	Increased NA to 25 as in gg_natc	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( NA = 25, NPTS = 22 )
	PARAMETER	( JTIM = 1, JNAM = 2, JSPD = 3, JMKR = 4 )
C*
	CHARACTER*(*)	trkfil, dattm2, strnam, models(*)
	INTEGER		icolor(*), mrktyp(*), iwidth(*), lwidth(*),
     +		        iflags (*), itype
	REAL		ssize(*)
	LOGICAL		match
C*
        CHARACTER	record (NPTS,NA)*195, buffer*195,
     +			carq*4, modl*6, name*10, nblnam*10,
     +			timstr*8, cdttm*20, lastim*20, buftim*11,
     +			tau*3, tmpll*4, dfname*10, ocname*10, bname*10
	INTEGER		idtarr(5)  
	REAL		alat(NPTS), alon(NPTS), tlat(500), tlon(500)
	LOGICAL		done, good(NPTS), legnd(NA), timtch, noplot
C*
	DATA		carq / 'CARQ' /
C-----------------------------------------------------------------------
	iret  = 0
	match = .false.

C
C*  Initialize the actual track latlon arrays
C
	DO itrk = 1, 500
	    tlat (itrk) = RMISSD
	    tlon (itrk) = RMISSD
	END DO
C
C*  Open the specified data file.
C
	CALL FL_SOPN ( trkfil, lund, ier )
C
C*  Check for a 90 series (unnamed) disturbance.
C
	noplot = ( MAX ( INDEX ( trkfil, 'al9' ),
     +			 INDEX ( trkfil, 'ep9' ) ) .gt. 0 )
C
	IF ( strnam .ne. ' ' ) THEN
C
C*  A storm name or number was specified.
C
	    CALL ST_ALNM ( strnam ( 3:3 ), ityp, ier ) 
	    IF ( ityp .eq. 1 ) THEN
C
C*  A storm number (id) of form (AL|EP)nn is assumed.
C
		match = .true.
	    ELSE
C
C*  An alphabetic storm name is assumed.
C
		iostat = 0
		DO WHILE ( ( iostat .eq. 0 ) .and. ( .not. match ) ) 
	            READ ( lund, 3, IOSTAT = iostat ) buffer
		    IF ( iostat .eq. 0 ) THEN
		        IF ( buffer ( 25:28 ) .eq. carq ) THEN
			    CALL ST_RMBL ( buffer ( 150:159 ),
     +				    nblnam, ln, ier )
			    IF ( nblnam (1:ln) .eq. strnam )
     +				    match = .true.
		        END IF
		    END IF
		END DO
C
	        IF ( .not. match ) THEN
		    CALL FL_CLOS ( lund, ier )
 	            RETURN
	        ELSE
	            CALL FL_REWD ( lund, ier )
	        END IF
	    END IF
	END IF
C
C*	Check for special case of LAST, and get the latest time from
C*	the file.  Ignore the first two year digits in the ATCF file
C*	to be compatible with the gempak date string and subroutines.
C*
C
	IF ( dattm2 ( :4 ) .eq. 'LAST' ) THEN
	    timstr = '00000000'
	    iostat = 0
	    DO WHILE ( iostat .eq. 0 ) 
	        READ ( lund, 3, IOSTAT = iostat ) buffer
		IF ( iostat .eq. 0 ) THEN
		    IF ( buffer ( 11:18 ) .gt. timstr )
     +			 timstr = buffer ( 11:18 )
		END IF
	    END DO
C
C*  Compare latest time to current time to see if this is an
C*  active storm.
C
	    itype = 1
	    CALL CSS_GTIM ( itype, cdttm, ier )
	    lastim = timstr ( :6 ) // '/' // timstr ( 7:8 ) // '00'
	    CALL TI_DIFF ( cdttm, lastim, nmin, ier )
C
C*    If the current time is more than 9 hours after the latest
C*    storm time, assume this is NOT a current storm.  This number
C*    may need to be adusted up or down, depending on timeliness
C*    of data receipt.
C
	    IF ( nmin .gt. 540 ) THEN
		CALL FL_CLOS ( lund, ier )
		RETURN
	      ELSE
	        CALL FL_REWD ( lund, ier )
	    END IF
	  ELSE
C
C*    Round the time down to the nearest six hours to match the
C*    time in the file.
C
	    CALL TI_CTOI ( dattm2, idtarr, ier )
	    iover = MOD ( idtarr ( 4 ), 6 )
	    IF ( iover .ne. 0 ) THEN
		imins = iover * 60
		CALL TI_SUBM ( idtarr, imins, idtarr, ier )
		CALL TI_ITOC ( idtarr, lastim, ier )
	    ELSE
		lastim = dattm2
	    END IF
	    timstr = lastim ( :6 ) // lastim ( 8:9 )
	END IF
C
	itrak  = 0
	icurnt = 0
	blat   = 90.
	blon   = 180.
	iostat = 0
	timtch = .false.
	name   = strnam
	ocname = ' '
	dfname = ' '
C
	DO ii  = 1, NA
	    DO jj = 1, NPTS
	        record ( jj, ii ) = ' '
	    END DO
	    legnd ( ii )  = .false.
	END DO
C
C*	Loop on all records in the file.
C
	DO WHILE ( iostat .eq. 0 )
	    READ ( lund, 3, IOSTAT = iostat ) buffer
3	    FORMAT ( A )
	    IF ( iostat .eq. 0 )  THEN
	        modl = buffer ( 25:28 )
		IF ( modl .ne. carq ) THEN
		    IF ( buffer ( 11:18 ) .eq. timstr ) THEN
C
C*	A match was found on the time.
C*	Get default storm name if have not already.
C
		        IF ( dfname .eq. ' ' ) dfname = buffer (1:2) //
     +		                 buffer (5:6) // buffer (11:12)
			timtch = .true.
		        done   = .false.
		        ii     = 1
		        DO WHILE ( .not. done )
			    IF ( modl .eq. models ( ii )(3:) )  THEN
C
C*  A match was found on the model name.
C
				IF (buffer(36:38) .ne. '  0'       .and.
     +					buffer(42:45) .ne. '   0'  .and.
     +					icolor ( ii ) .gt. 0 ) THEN
C
C*				    Check if the forecast period is in
C*				    the set:  (0, 6, 12, 18, ... 126).
C*				    and determine the position to be
C*				    stored (1, 2, 3, 4, ... 22)
C*				    respectively.
C
				    tau = buffer ( 31:33)
				    CALL ST_RMBL ( tau, tau, ln, ier )
				    CALL ST_INTG ( tau(1:ln), itau, ier)
				    IF ( itau .ge. 0 .and. itau .le. 126 ) THEN
				        IF (MOD (itau, 6) .eq. 0) THEN
					    ip = itau / 6 + 1
			            	    record (ip,ii) = buffer
					END IF
				    END IF
			        END IF
			        done = .true.
			      ELSE
			        ii = ii + 1
			        IF ( ii .gt. NA )  done = .true.
			    END IF
		        END DO
		    END IF
		  ELSE
C 
C*		    Get the current storm name from CARQ in the file
C
		    bname = buffer ( 150:159 )
		    IF ( bname .ne. ' ' ) ocname = bname
		    IF ( buffer ( 31:33 ) .eq. '  0' ) THEN
C
C*		        Check to see if this is really data from an old
C*		        90 series disturbance.  The 90 series
C*			identifiers can be reused, but the old data
C*			remains in the file.  (4320 minutes is 3 days.)
C
		        IF ( noplot ) THEN
			    buftim = buffer ( 11:16 ) // '/' //
     +				     buffer ( 17:18 ) // '00'
			    CALL TI_DIFF ( lastim, buftim, nmin, ier )
			    IF ( nmin .lt. 4320) noplot = .false.
		        END IF
		        IF ( .not. noplot ) THEN
C
C*  Save the actual track - tau=0 and modl=CARQ
C
			    dirns = 0.
			    IF ( buffer (39:39) .eq. 'N' ) THEN
			        dirns = 1.
			    ELSE IF ( buffer (39:39) .eq. 'S' ) THEN
			        dirns = -1.
			    END IF
			    direw = 0.
			    IF ( buffer (46:46) .eq. 'E' ) THEN
			        direw = 1.
			    ELSE IF ( buffer (46:46) .eq. 'W' ) THEN
			        direw = -1.
			    END IF
			    tmpll = buffer (36:38)
			    CALL ST_RMBL ( tmpll, tmpll, ln, ier )
			    CALL ST_INTG ( tmpll (1:ln), ilat, ier )
			    tmpll = buffer (42:45)
			    CALL ST_RMBL ( tmpll, tmpll, ln, ier )
			    CALL ST_INTG ( tmpll (1:ln), ilon, ier )
			    itrak = itrak + 1
			    tlat ( itrak ) = ilat / 10.0 * dirns
			    tlon ( itrak ) = ilon / 10.0 * direw
			    IF ( buffer ( 11:18 ) .eq. timstr ) THEN
			        icurnt = itrak
			    ELSE IF ( buffer ( 7:14 ) .gt. timstr )
     +						THEN
			        IF ( icurnt .eq. 0 ) icurnt = itrak
			    END IF
		        END IF
		    END IF
		END IF
	    END IF
	END DO
C
C*	Decode and plot all requested tracks for this time and
C*	(optionally) a specific storm.
C*	Loop on all models.
C
	DO ii = 1, NA
	    knt = 0
	    DO k = 1, NPTS
	        IF ( record (k,ii)(36:39) .ne. ' ' .and.
     +			record (k,ii)(42:46) .ne. ' ' ) THEN
		    good (k) = .true.
		    IF ( record (k,ii)(39:39) .eq. 'N' ) THEN
		        dirns = 1.
		      ELSE IF ( record(k,ii)(39:39) .eq. 'S' ) THEN
		        dirns = -1.
		      ELSE
		        good (k) = .false.
		    END IF
		    IF ( record (k,ii)(46:46) .eq. 'E' ) THEN
		        direw = 1.
		      ELSE IF ( record(k,ii)(46:46) .eq. 'W' ) THEN
		        direw = -1.
		      ELSE
		        good (k) = .false.
		    END IF
		  ELSE
		    good (k) = .false.
		END IF
		IF ( good (k) ) THEN
C
C*		    Get good location for a model.
C
		    tmpll = record (k,ii) (36:38)
		    CALL ST_RMBL ( tmpll, tmpll, ln, ier )
		    CALL ST_INTG ( tmpll (1:ln), ilat, ier )
		    tmpll = record (k,ii) (42:45)
		    CALL ST_RMBL ( tmpll, tmpll, ln, ier )
		    CALL ST_INTG ( tmpll (1:ln), ilon, ier )
		    knt = knt + 1
		    alat ( knt ) = ilat / 10.0 * dirns
		    alon ( knt ) = ilon / 10.0 * direw
		    blat = MIN ( alat ( knt ), blat )
		    blon = MIN ( alon ( knt ), blon )
		END IF
	    END DO
C
C*		Draw one track and plot markers if requested.
C
	    CALL GSCOLR ( icolor ( ii ), ier )
	    CALL GSLINE ( 1, 0, lwidth ( ii ), 0, ier )
	    CALL GLINE  ( 'M', knt, alat, alon, ier )
	    IF ( knt .gt. 0 ) legnd ( ii ) = .true.
	    IF ( iflags ( JMKR ) .ne. 0 ) THEN
	        CALL GSMRKR ( mrktyp ( ii ), 0, ssize ( ii ),
     +		      iwidth ( ii ), ier )
	        CALL GMARK  ( 'M', knt, alat, alon, ier )
	    END IF
C
C*	    Plot wind speeds if requested.
C
	    IF ( iflags ( JSPD ) .ne. 0 ) THEN
		jk = 0
		DO  j = 1, NPTS
		    IF ( good (j) ) THEN
		        tmpll = record (j,ii)(49:51)
		        CALL ST_RMBL ( tmpll, tmpll, ln, ier )
		        CALL ST_INTG ( tmpll (1:ln), ispd, ier )
		        IF ( ispd  .gt. 0 ) THEN
			    jk = jk + 1
			    CALL GTEXT ( 'M', alat(jk),
     +				    alon(jk), record(j,ii)(49:51),
     +				    0.0, 2, 2, ier)
		        END IF
		    END IF
		END DO
	    END IF
	END DO
C
	IF ( blat .gt. 80. ) timtch = .false.
	IF ( timtch ) THEN
C
C*	    Plot a legend.
C
	    CALL GSTEXT ( 0, 0, 0, 0, 121, 0, 3, ier )
	    iyoff = 0
	    blat  = blat - 1.0
	    blon  = blon - 1.0
	    DO  ii = 1, NA
	        IF ( legnd ( ii ) ) THEN
	            CALL GSCOLR ( icolor ( ii ), ier )
	            CALL GTEXT  ( 'M', blat, blon, models ( ii )(3:6),
     +				  0.0, 0, iyoff, ier )
	            iyoff = iyoff + 4
	        END IF
	    END DO
C
C*	    Plot time and name if requested.
C
	    CALL GSCOLR ( 1, ier )
	    IF ( iflags ( JNAM ) .ne. 0 ) THEN
		IF ( strnam .eq. ' ' ) THEN
		    IF ( ocname .ne. ' ' ) THEN
			name = ocname
		      ELSE
			name = dfname
		    END IF
		END IF
		CALL GTEXT ( 'M', blat, blon, name, 0.0, 0, iyoff, ier )
		iyoff = iyoff + 4
	    END IF
	    IF ( iflags ( JTIM ) .ne. 0 ) THEN
		CALL GTEXT ( 'M', blat, blon, timstr ( 5:8 ), 0.0, 0, 
     +			     iyoff, ier ) 
	    END IF
	    CALL GSTEXT ( 0, 0, 0, 0, 111, 0, 1, ier )
C
C*	    Plot the actual track.
C
	    IF ( icurnt .eq. 0 ) icurnt = itrak
	    IF ( itrak .eq. 2 ) THEN
		IF ( ( tlat ( 1 ) .eq. tlat ( 2 ) ) .and.
     +		     ( tlon ( 1 ) .eq. tlon ( 2 ) ) ) itrak = 1
	    END IF
	    IF ( itrak .gt. 1 ) THEN
	        CALL GSLINE ( 1, 0, 3, 0, ier )
	        CALL GLINE ( 'M', icurnt, tlat, tlon, ier )
	        CALL GSLINE ( 13, 0, 3, 0, ier )
	        CALL GLINE ( 'M', itrak - icurnt + 1, tlat ( icurnt ),
     +		             tlon ( icurnt ), ier )
	      ELSE IF ( itrak .eq. 1 ) THEN
		CALL GSMRKR ( 5, 0, 1.0, 2, ier )
		CALL GMARK  ( 'M', 1, tlat ( 1 ), tlon ( 1 ), ier )
	    END IF
	END IF
C
	CALL FL_CLOS ( lund, ier )
C*
	RETURN
	END
