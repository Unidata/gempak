	SUBROUTINE IS_EGAR ( report, lenr, mxp, ifeggy, npt, rlat, rlon,
     +			     irad, iptr, iret )
C************************************************************************
C* IS_EGAR 								*
C*									*
C* This subroutine gets the boundary points of the area covered by an   *
C* EGGY, RJAA, NTAA, or MUHA international sigmet phenomenon.           *
C*                                                                      *
C* IS_EGAR ( REPORT, LENR, MXP, IFEGGY, NPT, RLAT, RLON, IRAD, IPTR,	*
C*           IRET )     						*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		Partial sigmet report string    *
C*	LENR		INTEGER		Length of string                *
C*	MXP		INTEGER		Maximum number of points        *
C*	IFEGGY		INTEGER		Country ID:  1 if EGGY; 2 if	*
C*					RJAA; 3 if NTAA; 4 if MUHA	*
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
C* F. J. Yen/NCEP	 1/00	Converted IS_AREA for EGGY. Added "AXIS"*
C* F. J. Yen/NCEP	 2/00	Expanded for RJAA.			*
C* F. J. Yen/NCEP	 4/00	Included 'LNN' for esol, added err check*
C* F. J. Yen/NCEP	 5/01	Expanded for NTAA.			*
C* F. J. Yen/NCEP	 8/01	Expanded search for phenom terminator.	*
C*				Added parameter ifeggy.			*
C* F. J. Yen/NCEP	10/01	Expanded for MUHA.			*
C* F. J. Yen/NCEP	10/01	Added processing of no key word for	*
C*				MUHA and cleaned up area string 	*
C* D. Kidwell/NCEP	10/01	Used ST_WORD to check for lat/lon       *
C* F. J. Yen/NCEP	 1/02	Expanded no key word processing for MUHA*
C* F. J. Yen/NCEP	 6/02	Added NTAA multi-area.  Fixed ifeggy sp.*
C*				Removed false dec. point at end of area.*
C* F. J. Yen/NCEP	12/03	Handled 'RADAR/SATELLITE' for MUHA area.*
C* F. J. Yen/NCEP	 2/04	Allowed for unabbrev. cardinal point for*
C*				a side of line.				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report
	REAL		rlat (*), rlon (*)
C*
    	CHARACTER	carr (20)*12, work*20, work2*20, card*2, card5*5
        CHARACTER	trpt*1000
	REAL		tlat (10), tlon (10)
	LOGICAL		done, aside
C------------------------------------------------------------------------
	iret = 0
	nokeyw = 0
	iptr = 0
	npt  = 0
	irad = IMISSD
	DO i = 1, mxp
	    rlat ( i ) = RMISSD
	    rlon ( i ) = RMISSD
	END DO 
C
C*	Look for area definition beginning with string ' WI ' (within)
C*	or ' IN AREA ' or ' BOUNDED ' or ' WITHIN'.
C*	Also ' IN ' or ' AT ' if MUHA.
C
	len  = MIN ( 260, lenr ) 
	iloc = INDEX ( report ( :len ), ' WI ' )
	IF ( iloc .eq. 0 ) THEN
	    iloc = INDEX ( report ( :len ), ' WITHIN ' )
	    IF ( iloc .eq. 0 ) THEN
	        iloc = INDEX ( report ( :len ), ' IN AREA ' )
		IF ( iloc .eq. 0 ) THEN
		    iloc = INDEX ( report ( :len ), ' BOUNDED ' )
	            IF ( iloc .eq. 0 ) THEN
			IF ( ifeggy .eq. 4 ) THEN
			  iloc = INDEX ( report ( :len ), ' IN ' )
   			  IF ( iloc .eq. 0 ) 
     +				iloc = INDEX ( report ( :len ), ' AT ' )
			END IF
			IF ( iloc .eq. 0 ) THEN
			    nokeyw = 1
    	 		  ELSE
       			    iloc = iloc - 5
			END IF
		    END IF
	        END IF
	        iloc = iloc + 1
	    END IF
	    iloc = iloc + 4
	END IF
	IF ( nokeyw .eq. 0 ) THEN
	    iloc = iloc + 4
	  ELSE
	    iloc = iloc - 5
	END IF
C
C* 	Check for distance (in nm) from a line or from center.
C
	inm   = INDEX ( report ( iloc:iloc + 15 ), 'NM ' )
	aside = .false.
	IF ( inm .ne. 0 ) THEN
	    iend = iloc + inm -2
	    IF ( report ( iend:iend ) .eq. CHSPAC ) iend = iend - 1
	    ibeg = MAX ( iloc, iend - 2 )
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
	    iof  = INDEX ( report ( iloc:iloc + 10 ), 'OF ' )
	    icen = INDEX ( report ( iloc:iloc + 16 ), ' CENTER' )
	    IF ( ( iof .ne. 0 ) .and. ( icen .ne. 0 ) ) THEN
		irad = iside
		iptr = iloc + icen + 7
		RETURN
	      ELSE
C
C*		Check to see if a cardinal point is given.
C
		card = report ( iloc:iloc + 1 )
		IF ( ( card .eq. 'N ' ) .or. ( card .eq. 'E ' ) .or.
     +		     ( card .eq. 'S ' ) .or. ( card .eq. 'W ' ) ) THEN
		    aside = . true.
		  ELSE
		    card5 = report ( iloc:iloc + 4 )
		    IF ( ( card5 .eq. 'NORTH')
     +                      .or. ( card5 .eq. 'EAST ' )
     +                      .or. ( card5 .eq. 'SOUTH' )
     +                      .or. ( card5 .eq. 'WEST ' ) ) THEN
                        aside = .true.
                        card = card5 ( 1:1 ) // ' '
                    END IF
		END IF
	    END IF
  	  ELSE
  	    IF ( nokeyw .eq. 1 ) THEN
		IF ( ifeggy .eq. 4 ) THEN
		    iloc = INDEX ( report ( :len ), 'SATELLITE' )
		    IF ( iloc .ne. 0 ) iloc = iloc + 10
		    irad = INDEX ( report ( :len ), 'RADAR' )
		    IF ( irad .ne. 0 ) irad = irad + 6
		    iloc = MAX ( iloc, irad )
		    IF ( iloc .eq. 0 ) THEN
		 	iloc = INDEX ( report ( :len ), ' AREA ' )
			IF ( iloc .ne. 0 ) THEN
			    iloc = iloc + 6
			  ELSE
			    iret = -4
			    RETURN
			END IF 
		    END IF
		  ELSE IF ( ifeggy .eq. 3 ) THEN
		    iloc = INDEX ( report ( :len ), ')' )
		    IF ( iloc .eq. 0 ) THEN
			iret = -4
			RETURN
		      ELSE
			iloc = iloc + 1
			ibeg = iloc - 1
		    END IF
		  ELSE
  		    iret = -4
  		    RETURN
		END IF
  	    END IF
	END IF
C
C*	Find the beginning and end of the point definition string.
C
	ii   = iloc
	done = .false.
	DO WHILE ( .not. done )
C
C*	    if found 'BY:', replace with ' ' since sometimes no space
C*	    separating it and lat/lon position
C
	    IF ( INDEX ( report ( ii:ii ) , ':' ) .ne. 0 ) THEN
		IF ( INDEX ( report ( ii-3:ii-1), ' BY' ) .ne. 0) THEN
		    ii1 = ii + 1
		    IF ( ii1 .lt. lenr ) THEN
			IF ( INDEX ( report (ii1:ii1), ' ' ) .eq. 0 )
     +				report ( ii:ii ) = ' '
		    END IF
		END IF
	    END IF
	    CALL ST_ALNM ( report ( ii:ii ), ityp, ier )
	    IF ( ityp .eq. 1 ) THEN
		done = .true.
		ibeg = ii - 1
	      ELSE
		ii = ii + 1
		IF ( ii .gt. lenr ) THEN
		    iret = -4
		    RETURN
		END IF
	    END IF
	END DO
	iend = INDEX ( report ( iloc:len ), '=' )
	IF ( iend .eq. 0 ) THEN
	    ln = len - iloc + 1
	    CALL IS_EGFD ( report (iloc:len), ln, ifeggy, iend, ier ) 
	    IF ( iend .eq. 0 ) THEN
		iend = lenr - iloc + 1
	    END IF
	    IF ( ifeggy .eq. 3 .and.
     +		    report(iloc+iend-1:iloc+iend-1) .eq. ')' ) THEN
C
C*		For a numeric delimeter, invoke IS_EGFD again to
C*		find a '.' to end the area definition.
C
		ln2= iend -3
		CALL IS_EGFD ( report (iloc:iloc+iend-3),ln2,
     +			       ifeggy, iend, ier )
	        IF ( iend .eq. 0 ) THEN
		    iend = lenr - iloc + 1
		END IF
	    END IF
	END IF
	iend = iloc + iend - 1
	trpt = report
	IF ( ifeggy .eq. 4 ) THEN
C
C*	    Replace "," used as decimal point with "." and remaining ","
C*	    with " " for MUHA reports.  Also remove "/".
C
	    leng = iend - iloc
	    CALL IS_EGCA ( leng, trpt (iloc:iend), ier )
	END IF
C
C*	Determine if this is a circle, a bounded area, or either side of
C*	line.
C
	IF ( inm .ne. 0 ) THEN
	    IF ( ( INDEX ( report (iloc:iend), 'LINE' ) .eq. 0 ) .and.
     +           ( INDEX ( report (iloc:iend), ' LN ' ) .eq. 0 ) .and.
     +           ( INDEX ( report (iloc:iend), ' LNN' ) .eq. 0 ) .and.
     +		 ( INDEX ( report (iloc:iend), 'AXIS' ) .eq. 0 ) ) THEN
C
C*		  This is a circle. 
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
	iloc = ibeg
	CALL ST_CLST ( trpt ( iloc:iend ), ' ', ' ', mxp, carr, 
     +	 	       npts, ier )
	ii = 1
	DO WHILE ( ii .le. npts )
	    work = carr ( ii )
	    CALL ST_LSTR ( work, lenw, ier )
C
C*	    For NTAA, search for possible lat/lon value prefixed
C*	    with ')'
C
	    IF ( ifeggy .eq. 3 ) THEN
	        ipar = INDEX ( work, ')' )
	        IF ( ipar .ge. 1 .and. ipar .le. 4 .and.
     +			 lenw .gt. 3) THEN
		    work = carr ( ii ) ( ipar+1:12 )
		END IF
	    END IF
C
C*	    Search for valid latitude and longitude values.  When 
C*	    necessary, if the latitude or longitude value has two
C*	    decimal places, round it to one decimal place to keep
C*	    the length of the latitude and longitude string less
C*	    than or equal to 11 for subroutine IS_LTLN.
C
	    IF ( lenw .le. 5 .and. lenw .ge. 3 ) THEN
		CALL ST_WORD ( work ( :lenw ), ityp, ier )
		IF ( ityp .eq. 1 ) THEN
		    ii   = ii + 1
		    work2 = carr (ii)
		    CALL ST_LSTR ( work2, lenn, ier )
		    IF ( ( work2 ( lenn:lenn ) .eq. 'E' ) .or.
     +		            ( work2 ( lenn:lenn ) .eq. 'W' ) ) THEN
			IF ( work2 ( lenn-3:lenn-3 ) .eq. '.' ) THEN
			    CALL ST_CRNM ( work2( :lenn-1 ), hlon, ier )
			    CALL ST_RLCH ( hlon, 1, work2, ier )
			    CALL ST_LSTR (work2, lenn2, ier )
                            work2 ( lenn2 + 1:lenn2 + 1 ) =
     +				    carr (ii) ( lenn:lenn )
                            lenn = lenn2 + 1
			END IF
		      ELSE
			CALL ST_WORD ( work2 ( :lenn ), ityp, ier )
			IF ( ityp .ne. 2 ) THEN
			    ii = ii - 1
			    work2 = ' '
			    lenn = 1
			END IF
		    END IF
		    work = work ( :lenw ) // work2 ( :lenn )
		END IF
	      ELSE IF ( lenw .eq. 6 .and.
     +			work ( lenw-3:lenw-3 ) .eq. '.' ) THEN
		IF ( ( work ( lenw:lenw ) .eq. 'N' ) .or.
     +               ( work ( lenw:lenw ) .eq. 'S' ) ) THEN
		    CALL ST_CRNM ( work ( :lenw-1 ), hlat, ier )
		    CALL ST_RLCH ( hlat, 1, work, ier )
		    CALL ST_LSTR ( work, lenw2, ier )
		    lenw2 = lenw2 + 1
                    work ( lenw2:lenw2 ) = carr (ii) ( lenw:lenw )
                    lenw = lenw2 + 1
		    ii   = ii + 1
		    work2 = carr (ii)
		    CALL ST_LSTR ( work2, lenn, ier )
		    IF ( ( work2 ( lenn:lenn ) .eq. 'E' ) .or.
     +		            ( work2 ( lenn:lenn ) .eq. 'W' ) ) THEN
			IF ( work2 ( lenn-3:lenn-3 ) .eq. '.' ) THEN
			    CALL ST_CRNM ( work2( :lenn-1 ), hlon, ier )
			    CALL ST_RLCH ( hlon, 1, work2, ier )
			    CALL ST_LSTR (work2, lenn2, ier )
                            work2 ( lenn2 + 1:lenn2 + 1 ) =
     +				    carr (ii) ( lenn:lenn )
                            lenn = lenn2 + 1
			END IF
		      ELSE
			ii = ii - 1
			work2 = ' '
			lenn = 1
		    END IF
		    work = work ( :lenw ) // work2 ( :lenn )
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
		    IF ( nptt .lt. 10 ) THEN
		        nptt = nptt + 1
		        tlat ( nptt ) = xlat
		        tlon ( nptt ) = xlon
		    END IF
		END IF 
	    END IF
	    ii = ii + 1
	END DO
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
