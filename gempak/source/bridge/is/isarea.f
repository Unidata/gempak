	SUBROUTINE IS_AREA ( report, lenr, mxp, origin, npt, rlat, rlon,
     +			     irad, iptr, iret )
C************************************************************************
C* IS_AREA 								*
C*									*
C* This subroutine gets the boundary points of the area covered by an   *
C* international sigmet phenomenon.                                     *
C*                                                                      *
C* IS_AREA ( REPORT, LENR, MXP, ORIGIN, NPT, RLAT, RLON, IRAD, IPTR,	*
C*	     IRET )							*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		Partial sigmet report string    *
C*	LENR		INTEGER		Length of string                *
C*	MXP		INTEGER		Maximum number of points        *
C*	ORIGIN		CHAR*		Originating station ID		*
C*									*
C* Output parameters:							*
C*	NPT 		INTEGER		Number of boundary points       *
C*      RLAT(*)		REAL		Latitudes of boundary points    *
C*	RLON(*)		REAL		Longitudes of boundary points   *
C*	IRAD		INTEGER		Radius if area is a circle      *
C*	IPTR		INTEGER		Pointer to location after area  *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = bad area definition       *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	10/99	                                        *
C* D. Kidwell/NCEP	10/99	Allowed 'IN' as beginning of area def.  *
C* D. Kidwell/NCEP	11/99	Checked for embedded blanks in lat/lon; *
C*				generalized                             *
C* F. J. Yen/NCEP	11/99	Allowed 'WITHIN' as start of area def.	*
C* D. Kidwell/NCEP	11/99	Checked limit on num pts input to isesol*
C* F. J. Yen/NCEP	11/99	Allowed no keyword to begin area def.	*
C* D. Kidwell/NCEP	 1/00	Checked length of latitude string	*
C* F. J. Yen/NCEP	 1/00	Included 'AXIS' for either side of line.*
C* D. Kidwell/NCEP	 3/00	Included 'LNN' for esol, added err check*
C* F. J. Yen/NCEP	 4/00	Included 'FM' for VA bounded after loc	*
C* A. Hardy/GSC          8/00   Remove check for 'OF' for finding CENTER*
C* D. Kidwell/NCEP	10/01	Used ST_WORD to check for lat/lon       *
C* F. J. Yen/NCEP	 9/03	Extended keywords and rewrote search.	*
C*				Allowed for '-' to separate lat/lon pos.*
C*				Decoded VOR vertices issued by PANC. CSC*
C* F. J. Yen/NCEP	10/03	Changed "NM" to 'NM'.			*
C* F. J. Yen/NCEP	12/03	Allowed 'WTN' to start area def.(CANADA)*
C* F. J. Yen/NCEP	 2/04	Allowed for encoded version of a side	*
C*				of line (ie, unabbrev. cardinal points).*
C* J. Lewis/AWC		 4/05	ADD PAWU to origin check                *
C* T. Piper/SAIC	10/05	Removed check on old origin id PANC	*
C* S. Jacobs/NCEP	 2/11	Do not reset the record pointer - iloc	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report, origin
	REAL		rlat (*), rlon (*)
C*
	PARAMETER       ( MAXPTS = 20 )
	CHARACTER	carr (20)*12, work*20, card*2, card5*5
	CHARACTER	keywd (9)*12, trep*1250
	INTEGER		nkw (9)
	REAL		tlat (MAXPTS), tlon (MAXPTS)
	LOGICAL		done, aside, nokeyw, ilocfm, latlon
	DATA		keywd / ' WI ', ' WITHIN ', ' WTN ',
     +				' FOR AREA ', ' IN AN AREA ',
     +				' IN AREA ', ' BOUNDED ',
     +				' FROM ', ' FM ' /	
	DATA		nkw   / 4, 8, 5, 9, 12, 9, 9, 6, 4 /
	DATA		numkw / 9 /
C------------------------------------------------------------------------
	iret = 0
	nokeyw = .true.
	iptr = 0
	npt  = 0
	irad = IMISSD
	DO i = 1, mxp
	    rlat ( i ) = RMISSD
	    rlon ( i ) = RMISSD
	END DO 
C
C*	Look for area definition beginning with a string in array keywd.
C
	nokeyw = .true.
	ilocfm = .false.
	len = MIN ( 260, lenr ) 
	num = 1
	DO WHILE ( num .le. numkw )
	    IF ( num .le. 6 ) THEN
		lenn = len
	      ELSE IF ( num .eq. 7 ) THEN
      		lenn = MIN ( 11, len )
	      ELSE
	        lenn = MIN ( 9, len )
	    END IF
	    iloc = INDEX ( report ( :lenn ), keywd (num) (1:nkw(num)))
	    IF ( iloc .ne. 0 ) THEN
		iloc = iloc + nkw (num)
		nokeyw = .false.
		IF ( num .ge. 6 ) THEN
		    ilocfm = .true.
		END IF
		num = 100
	    END IF
	    num = num + 1
	END DO

	IF ( nokeyw ) iloc = 2
C
C* 	Check for distance (in nm) from a line or from center.
C
	lenn = MIN (iloc+25, lenr )
	inm   = INDEX ( report ( iloc:lenn ), 'NM ' )
	IF ( inm .eq. 0 ) THEN
C
C*	    Check for 'MILES' instead of 'NM ' (PGUM)
C
	    inm = INDEX ( report ( iloc:lenn ), ' MILES ' )
	END IF
	IF ( ilocfm ) THEN
C
C*	    Check to make sure "NM" is not part of a geographical
C*	    reference when key word of "FM" found for area.  Check
C*	    that " OF " follows " NM ".  In the following:
C*	    "ISOL EMBEDDED TSTMS 40 NM OF A LINE FM 60 NM NE BGQ TO",
C*	    the position inm would have been found for the NM in "60 NM"
C*	    from the geographical reference instead of the "40 NM".
C*	    Also check for EITHER, NORTH, SOUTH, WEST, EAST OF LINE (LN)"
C
	    IF ( INDEX ( report ( iloc+inm:iloc+inm+6 ), ' OF ' )
     +		 .eq. 0 .and.
     +		 INDEX ( report ( iloc+inm:iloc+inm+10), ' EITHER ') 
     +		 .eq. 0 ) THEN
		IF ( inm .gt. 2 ) THEN
		    inm1 = INDEX ( report ( 2:inm ), 'NM ' )
		    IF ( inm1 .ne. 0 ) THEN
			inm = inm1
			iloc = 2
		    END IF
		END IF
	    END IF
	END IF
	aside = .false.
	ibeg = -1
	ibegsv = ibeg
	IF ( inm .ne. 0 ) THEN
	    iend = iloc + inm -2
	    IF ( report ( iend:iend ) .eq. CHSPAC ) iend = iend - 1
	    ibeg = MAX ( iloc, iend - 2 )
	    ibegsv = ibeg
	    CALL ST_NUMB ( report ( ibeg:iend ), iside, ier )
	    IF ( ier .ne. 0 ) THEN
	        iret = -4
   		RETURN
            END IF
	    iloc = iloc + inm + 2
C
C*	    Check to see if reference is made to a previously defined
C*	    center.
C
	    icen = INDEX ( report ( iloc:iloc + 16 ), 'CENTER' )
	    IF ( ( icen .ne. 0 ) ) THEN
		irad = iside
		iptr = iloc + icen + 7
		RETURN
	      ELSE 
		icen = INDEX ( report ( iloc:iloc + 16 ), 'CNTR' )
		IF ( ( icen .ne. 0 ) ) THEN
		    irad = iside
		    iptr = iloc + icen + 7
		    RETURN
		  ELSE
C
C*		    Check to see if a cardinal point is given.
C
		    card = report ( iloc:iloc + 1 )
		    IF ( ( card .eq. 'N ' ) .or. ( card .eq. 'E ' )
     +			        .or. ( card .eq. 'S ' )
     +				.or. ( card .eq. 'W ' ) ) THEN
		        aside = .true.
		      ELSE
			card5 = report ( iloc:iloc + 4 )
			IF ( ( card5 .eq. 'NORTH')
     +			        .or. ( card5 .eq. 'EAST ' )
     +			        .or. ( card5 .eq. 'SOUTH' )
     +			        .or. ( card5 .eq. 'WEST ' ) ) THEN
			    aside = .true.
			    card = card5 ( 1:1 ) // ' '
			END IF
		    END IF
		END IF
	    END IF
	END IF
C
C*	Find the beginning and end of the point definition string.
C*	(Find beginning for latlon format)
C
	iend = INDEX ( report ( iloc:len ), '. ' )
	iend = iloc + iend - 2
	IF ( iend .eq. 0 ) THEN
	    iret = -4
	    RETURN
	END IF
	latlon = .false.
	ii   = iloc
	done = .false.
	DO WHILE ( .not. done )
	    CALL ST_ALNM ( report ( ii:ii ), ityp, ier )
	    IF ( ityp .eq. 1 ) THEN
C
C*		Check if numeric is for distance 'NM' from a
C*		geographical point
C
		minii = MIN ( iend, ii+7 )
		nmiles = INDEX ( report ( ii+1:minii+7 ), 'NM' )
   	 	IF ( nmiles .eq. 0 ) THEN
		    done = .true.
		    latlon = .true.
		    ibeg = ii - 1
		  ELSE
		    ii = ii + 1
		    IF ( ii .gt. iend ) THEN
			done = .true.
		    END IF
		END IF
	      ELSE
		ii = ii + 1
		IF ( ii .gt. iend ) THEN
		    done = .true.
		END IF
	    END IF
	END DO
C
C*	Determine if this is a circle, a bounded area, or either side of
C*	line.
C
	IF ( inm .ne. 0 ) THEN
	    IF ( ( INDEX ( report (iloc:iend), 'LINE' ) .eq. 0 ) .and.
     +           ( INDEX ( report (iloc:iend), ' LN ' ) .eq. 0 ) .and.
     +           ( INDEX ( report (iloc:iend), ' LNN' ) .eq. 0 ) .and.
     +           ( INDEX ( report (iloc:iend), 'AXIS' ) .eq. 0 ) ) THEN
C
C*		This is a circle. 
C
		irad  = iside
		iside = 0
	      ELSE
C
C*		This is one or both sides of a line.
C
		nptt = 0
	    END IF
	  ELSE
C
C*	    This is a bounded area.
C
	    iside = -1
	END IF
C
C*	Get the points.
C
	iptr = iend + 3
	IF ( ibegsv .eq. -1 ) ibegsv = 2
	IF ( ibegsv .le. iend ) THEN
C-SJ
C-SJ	    This does not seem to be necessary and is actually
C-SJ	    harmful by not using the value of iloc calculated above.
C	    iloc = ibegsv
C
C*	    Check for '-' and replace with space before breaking
C*	    into array of strings.
C
	    trep = report ( iloc:iend )
	    ipos = -1
	    DO WHILE ( ipos .ne. 0 )
                CALL ST_RPSL ( trep, '-', 1, ' ', 1, ipos,
     +                         trep, ier )
            END DO
	    lntrep = iend - iloc + 1
	    CALL ST_CLST ( trep ( :lntrep ), ' ', ' ', mxp, 
     +		    carr, npts, ier )
	  ELSE
	    npts = 0
	END IF
	ier = 0
	ii = 1
	IF ( latlon ) THEN
	  DO WHILE ( ii .le. npts )
	    work = carr ( ii )
	    CALL ST_LSTR ( work, lenw, ier )
	    IF ( lenw .le. 5 ) THEN
	 	CALL ST_WORD ( work ( :lenw ), ityp, ier )
		IF ( ityp .eq. 1 ) THEN
		    ii   = ii + 1
		    work = work ( :lenw ) // carr ( ii )
		END IF
	    END IF
C
	    CALL IS_LTLN ( work, xlat, xlon, ier )
	    IF ( ier .eq. 0 ) THEN
		IF ( iside .le. 0 ) THEN
		    npt = npt + 1
		    rlat ( npt ) = xlat
		    rlon ( npt ) = xlon
		  ELSE 
		    IF ( nptt .lt. MAXPTS ) THEN
		        nptt = nptt + 1
		        tlat ( nptt ) = xlat
		        tlon ( nptt ) = xlon
		    END IF
		END IF 
	    END IF
	    ii = ii + 1
	  END DO
	END IF
	IF ( ( origin .eq. 'PAWU' ) .and.
     +	     (.not. latlon .or. ier .ne. 0 ) ) THEN
	    IF ( latlon ) iloc = ibegsv
	    IF ( iside .le. 0 ) THEN
                CALL IS_CBND ( report ( iloc:iend ), MAXPTS,
     +		        npt, rlat, rlon, iptr, ier )
		IF ( ier .ne. 0 .or. npt .eq. 0 ) THEN
		    iret = -4
		    RETURN
		END IF
	      ELSE
                CALL IS_CBND ( report ( iloc:iend ), MAXPTS,
     +		        nptt, tlat, tlon, iptr, ier )
		IF ( ier .ne. 0 .or. nptt .eq. 0 ) THEN
		    iret = -4
		    RETURN
		END IF
	    END IF
    	END IF 
C
	IF ( iside .gt. 0 ) THEN
	    IF ( nptt .gt. 1 ) THEN
C
C*		Get the points on either side or one side of a line.
C
		IF ( .not. aside ) THEN
		    CALL IS_ESOL ( tlat, tlon, nptt, iside, rlat, rlon,
     +			           npt, iret )
		  ELSE
		    CALL IS_OSOL ( tlat, tlon, nptt, iside, card, rlat,
     +				   rlon, npt, iret )
		END IF
	      ELSE
		iret = -4
	    END IF
	  ELSE IF ( ( iside .lt. 0 ) .and. ( npt .eq. 0 ) ) THEN
C
C*	    Check for the configuration lat1 TO lat2 FROM lon1 TO lon2.
C
	    IF ( npts .lt. 7 ) THEN
		done = .true.
		iret = -4
	      ELSE
		done = .false.
	        ii   = 2
	    END IF
	    DO WHILE ( .not. done )
		IF ( ( carr (ii)   (:2) .eq. 'TO' ) .and.
     +		     ( carr (ii+2) (:4) .eq. 'FROM' ) .and.
     +		     ( carr (ii+4) (:2) .eq. 'TO' ) ) THEN
		    done = .true.
		    CALL IS_BDLN ( carr ( ii-1 ), carr ( ii+1 ),
     +				   carr ( ii+3 ), carr ( ii+5 ),
     +			           rlat, rlon, npt, iret )
	          ELSE
		    ii = ii + 1
		    IF ( ( ii + 5 ) .gt. npts ) THEN
		        done = .true.
		        iret = -4
		    END IF
		END IF
	    END DO
	  ELSE IF ( ( iside .eq. 0 ) .and. ( npt .ne. 1 ) ) THEN
	    iret = -4
	END IF
C*
	RETURN
	END
