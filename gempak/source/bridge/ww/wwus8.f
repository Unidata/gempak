	SUBROUTINE WW_US8 ( watch, lenw, irptr, mxp, wnum, rlat, rlon,
     +		            npt, isecnd, wnum2, iret )
C************************************************************************
C* WW_US8  								*
C*									*
C* This subroutine decodes the watch number(s) and updated line points  *
C* for a WWUS8 or WOUS20 status report.                                 *
C*                                                                      *
C* WW_US8 ( WATCH, LENW, IRPTR, MXP, WNUM, RLAT, RLON, NPT, ISECND,     *
C*	    WNUM2, IRET )      				   		*
C*                                                                      *
C* Input parameters:							*
C*	WATCH 		CHAR*		Report string                   *
C*	LENW		INTEGER		Length of report		*
C*	IRPTR		INTEGER		Pointer to location in report   *
C*	MXP 		INTEGER		Maximum number of points        *
C*									*
C* Output parameters:							*
C*	WNUM		CHAR*		Watch number                    *
C*	RLAT (*)	REAL            Latitudes of updated points     *
C*	RLON (*)	REAL		Longitudes of updated points    *
C*	NPT		INTEGER		Num. of updated points in line 1*
C*	ISECND		INTEGER		Flag for 2nd watch number or    *
C*					num. of updated points in line 2*
C*	WNUM2    	CHAR*		Second watch number             *
C*	IRET		INTEGER		Return code			*
C*				          0 = normal return             *
C*				         -3 = point not in table        *
C*				         -4 = no status points decoded  *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 8/99	                                        *
C* D. Kidwell/NCEP	 9/99	Allow 2 lines to be defined; added doc. *
C* D. Kidwell/NCEP	10/99	Mods for variations in message format   *
C* D. Kidwell/NCEP	10/99	Do not look for # before watch number   *
C* D. Kidwell/NCEP	11/99	Checked for status report ending in '.' *
C* D. Kidwell/NCEP	 3/00	Fixed for station name containing 'TO'  *
C* D. Kidwell/NCEP	 3/00	Called ST_RPSL instead of ST_RPST       *
C* D. Kidwell/NCEP	 9/00	(WW_AXPT, WW_GTPT) -> CLO_DDDEC, fixed  *
C*				typo in DO initialization               *
C* D. Kidwell/NCEP	10/01	Added WOUS20 to prolog                  *
C* D. Kidwell/NCEP	 4/02	Fixes for 2nd status line - 'AND', fmt  *
C* D. Kidwell/NCEP	 5/02	Fixed for case of 'AND AND'             *
C* A. Hardy/NCEP	 7/03   Increased 'string' 120 -> 500		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	watch, wnum, wnum2 
	REAL		rlat (*), rlon (*)
C*
C*	IFMT format code interpretation for location string - 
C*	(See locator.tbl for further information)
C*	5 = nearest 5 miles
C*	2 = statute miles (SM)
C*	1 = 16 point compass direction
C*	2 = 3 character station id
C*
	PARAMETER	( IFMT = 5212 )
C*
	CHARACTER	sep*4, string*500, tmpstr*20, locnam*10, to*3
	LOGICAL		good, done, land
C------------------------------------------------------------------------
	iret   = 0
	wnum   = ' '
	npt    = 0
	isecnd = 0
	wnum2  = ' '
C
	DO i = 1, mxp
	    rlat ( i ) = RMISSD
	    rlon ( i ) = RMISSD
	END DO
C
C*	Check for watch number.
C
	iptr = irptr
	lens = MIN ( iptr + 120, lenw )
	indx = INDEX ( watch ( iptr:lens ), ' WW NUMBER ' )
	IF ( indx .gt. 0 ) THEN
	    iptr = iptr + indx + 10
	  ELSE
	    indx1 = INDEX ( watch ( iptr:lens ), ' WWS NUMBER ')
	    indx2 = INDEX ( watch ( iptr:lens ), ' WW NUMBERS ')
	    indx = MAX ( indx1, indx2 )
	    IF ( indx .gt. 0 ) THEN
C
C*		A second watch number was found.
C
		iptr   = iptr + indx + 11
		isecnd = -1
	    END IF
	END IF
C
	IF ( indx .eq. 0 ) THEN
C
C*	    Look for the NEW format, which omits the word NUMBER.
C
	    indx = INDEX ( watch ( iptr:lens ), ' WW ' )
	    IF ( indx .gt. 0 ) THEN
	        iptr = iptr + indx + 3
	      ELSE
	        indx = INDEX ( watch ( iptr:lens ), ' WWS ')
	        IF ( indx .gt. 0 ) THEN
C
C*		    A second watch number was found.
C
		    iptr   = iptr + indx + 4
		    isecnd = -1
	        END IF
	    END IF
	END IF
C
C*	Get the watch number(s).
C
	IF ( indx .gt. 0 ) THEN
	    CALL WW_WNUM ( watch ( iptr:iptr + 5 ), 6, wnum, ier )
	    IF ( isecnd .lt. 0 ) THEN
		indx = INDEX ( watch ( iptr:iptr + 10 ), ' AND ' )
		IF ( indx .gt. 0 ) THEN
		    iptr = iptr + indx + 4
	    	    CALL WW_WNUM ( watch (iptr:iptr+5), 6, wnum2, ier )
		  ELSE
		    isecnd = 0
		END IF
	    END IF
	END IF
C
C*	Check for definition of a severe weather line.  There can be up
C*	to two definitions.
C
	npts = 0
	land = .false.
	DO ii = 1, 2
	    good  = .true.
	    lens  = MIN ( iptr + 150, lenw )
	    indx1 = INDEX ( watch ( iptr:lens ), 'THREAT ' )
	    IF ( indx1 .gt. 0 ) THEN
      	        indx = INDEX ( watch ( iptr:lens ), 'CONTINUES ' )
		IF ( indx .lt. indx1 ) indx = 0
	      ELSE
		indx = 0
 	    END IF
	    IF ( land ) indx = 1
	    IF ( indx .le. 0 ) THEN
C
C*		Line definition keywords were not found.
C
	        good = .false.
	      ELSE
C
C*		A severe weather line definition may follow.  Check for
C*	  	either no change in area, or the presence of a second
C* 		watch number.  (If there is a second watch number, there
C*		can be no second line definition.)
C
	        iptr  = iptr + indx
	        IF ( ii .eq. 1 ) THEN
		    iptr = iptr + 9
		  ELSE
		    IF ( isecnd .lt. 0 ) good = .false.
		    iptr = iptr + 4
		END IF
	    END IF
C
	    IF ( good ) THEN
C
C*	        A severe weather line definition follows.
C
	        lens = MIN ( iptr + 100, lenw )
	        indx = INDEX ( watch ( iptr:lens ), 'LINE THROUGH ' )
C
C*		Determine location separators.
C
	        IF ( indx .gt. 0 ) THEN
	            sep    = '...'
		    lensep = 3
		    iptr   = iptr + indx + 12
	          ELSE
	            indx = INDEX ( watch ( iptr:lens ), 'LINE FROM ' )
	            IF ( indx .gt. 0 ) THEN
	                sep    = ' TO '
		        lensep = 4
		        iptr   = iptr + indx + 9
	              ELSE
		        IF ( .not. land ) good   = .false.
	            END IF
	        END IF
C
	        IF ( good ) THEN
C
C*		    Determine end of location string.
C
		    lens = INDEX ( watch ( iptr:lenw ), '. ' ) - 1
		    IF ( lens .lt. 0 ) lens = lenw - iptr
		    IF ( ii .eq. 1 ) THEN
C
C*			Look for ' AND ' as a second string indicator.
C
			done = .false.
			iocc = 1
			DO WHILE ( .not. done )
			    CALL ST_NOCC ( watch (iptr:lenw), ' AND ',
     +					   iocc, lena, ier )
			    IF ( ( lena .gt. 0 ) .and.
     +				 ( lena .lt. lens ) ) THEN
				to = watch(iptr+lena+4:iptr+lena+6)
				IF ( to .ne. 'TO ' )  THEN
				    land = .true.
C
C*				    The next line handles station 'AND'
C*				    followed by second string indicator.
C
				    IF ( to .eq. 'AND' ) lena = lena + 4
				  ELSE IF ( to .eq. 'TO ' ) THEN
				    nx = iptr + lena + 7
				    IF ( (watch(nx:nx+3) .eq. 'THE ')
     +				     .or. (watch(nx:nx+4) .eq. 'LEFT ')
     +				     .or. (watch(nx:nx+4) .eq. 'EAST ')
     +				     .or. (watch(nx:nx+4) .eq. 'WEST ')
     +				     .or. (watch(nx:nx+5) .eq. 'RIGHT ')
     +				     .or. (watch(nx:nx+5) .eq. 'NORTH ')
     +				     .or. (watch(nx:nx+5) .eq. 'SOUTH ')
     +				     ) THEN
				        land = .true.
				    END IF
				END IF
				IF ( land ) THEN
				    done = .true.
				    lens = lena - 1
				  ELSE
				    iocc = iocc + 1
				END IF
			      ELSE
				done = .true.
			    END IF
			END DO
		    END IF
C
		    string = watch ( iptr:iptr + lens - 1)
		    iptr = iptr + lens
		    done = .false.
		    num  = 0
C
C*		    Replace varying separator string with character '/'.
C
		    DO WHILE ( .not. done )
		        num = num + 1
		        CALL ST_RPSL ( string, sep, lensep, '/', 1,
     +				       ipos, string, ier )
		        IF ( ipos .eq. 0 ) done = .true.
		    END DO
		    num = MIN ( num, mxp )
C
C*		    Look for and remove the characters ' OF '.
C
		    done = .false.
		    DO WHILE ( .not. done )
		        CALL ST_RPSL ( string, ' OF ', 4, ' ', 1,
     +				       ipos, string, ier )
		        IF ( ipos .eq. 0 ) done = .true.
		    END DO
C
C*		    Get the points.
C
		    ib    = 1
		    DO i = 1, num
		        ie = INDEX ( string ( ib:lens ), '/' ) + ib - 2
		        IF ( ie .lt. ib ) ie = lens
			CALL ST_NULL ( string ( ib:ie ), tmpstr, 
     +				       ll, ier )
			locnam = 'ANCHOR'
			CALL ST_NULL ( locnam, locnam, ll, ier )
		        CALL CLO_DDDEC ( locnam, IFMT, tmpstr, 1,
     +					 xlat, xlon, nstn, ier )
		        IF ( ier .eq. 0 ) THEN
	                    npts = npts + 1
	                    rlat ( npts ) = xlat
	                    rlon ( npts ) = xlon
			  ELSE IF ( ier .ne. ( -2 ) ) THEN
			    CALL DC_WLOG ( 2, 'DCWTCH', -4, 
     +					   string ( ib:ie ), ir1 )
			  ELSE
			    ib = MAX ( 1, ie - 3 )
			    CALL DC_WLOG ( 0, 'DCWTCH', -3,
     +					   string ( ib:ie ), ir1 )
			    IF ( iret .eq. 0 ) iret = -3
			END IF
		        ib = ie + 2
		    END DO
		    IF ( npts .eq. 0 ) good = .false.
	        END IF
	    END IF
C
C*	    Save the number of points.
C
	    IF ( ii .eq. 1 ) THEN
		npt = npts
		IF ( .not. good ) iret = -4
	      ELSE
		IF ( good ) THEN
		    isecnd = npts - npt
		  ELSE
		    isecnd = MIN ( isecnd, 0 )
		END IF
	    END IF
	END DO
C*
	RETURN
	END
