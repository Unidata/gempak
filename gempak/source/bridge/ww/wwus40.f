	SUBROUTINE WW_US40 ( watch, lenw, iptr, iotarr, mxp, itype,
     +		wnum, strtim, stptim, icancl, rlat, rlon, npt, irepl,
     +		rwnum, iret )
C************************************************************************
C* WW_US40  								*
C*									*
C* This subroutine decodes the type, watch number, cancellation         *
C* indicator, start and stop times, corner points, and watch            *
C* replacement information for a WWUS40 or WWUS30 watch report.         *
C*                                                                      *
C* WW_US40  ( WATCH, LENW, IPTR, IOTARR, MXP, ITYPE, WNUM, STRTIM,      *
C*            STPTIM, ICANCL, RLAT, RLON, NPT, IREPL, RWNUM, IRET )     *
C*									*
C* Input parameters:							*
C*	WATCH 		CHAR*		Report string                   *
C*	LENW		INTEGER		Length of report		*
C*	IPTR		INTEGER		Pointer to location in report   *
C*	IOTARR (5)	INTEGER 	Time array                      *
C*	MXP		INTEGER		Maximum number of points        *
C*									*
C* Output parameters:							*
C*	ITYPE		INTEGER		Watch type                      *
C*	WNUM		CHAR*		Watch number                    *
C*	STRTIM		CHAR*  		Watch start time, GEMPAK format *
C*	STPTIM		CHAR*		Watch end time, GEMPAK format   *
C*	ICANCL		INTEGER		Watch cancellation indicator    *
C*	RLAT (*)	REAL            Latitudes of vertex points      *
C*	RLON (*)	REAL		Longitudes of vertex points     *
C*	NPT		INTEGER		Number of vertex points         *
C*	IREPL		INTEGER		Number of watches replaced      *
C*	RWNUM (*)	CHAR*		Replaced watch numbers          *
C*	IRET		INTEGER		Return code			*
C*				          0 = normal return             *
C*				         -1 = axis point not in table   *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 7/99	From WW_US9                             *
C* D. Kidwell/NCEP	 8/99	Removed arg. lens from WW_AXPT call;    *
C*				moved COR check to calling routine      *
C* D. Kidwell/NCEP	10/01	Added WWUS30 to prolog                  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	watch, wnum, strtim, stptim, rwnum (*)
	REAL		rlat (*), rlon (*)
	INTEGER		iotarr (*)
C*
	CHARACTER	locid (2)*4, errstr*10
	REAL		dist (2), bear (2)
	LOGICAL 	good
C------------------------------------------------------------------------
	iret = 0
	itype  = IMISSD
	wnum   = ' '
	strtim = ' '
	stptim = ' '
	icancl = 0
	npt    = 0
	irepl  = 0
	rwnum ( 1 ) = ' '
C
	DO i = 1, mxp
	    rlat ( i ) = RMISSD
	    rlon ( i ) = RMISSD
	END DO
C
C*	Check for watch number.
C
	lens = MIN ( iptr + 50, lenw )
	indx = INDEX ( watch ( iptr:lens ), ' WW ' )
	IF ( indx .gt. 0 ) THEN
	    iptr = iptr + indx + 3
	    CALL WW_WNUM ( watch ( iptr:iptr + 5 ), 6, wnum, ier )
	END IF
C
C*	Check for type - either severe thunderstorm or tornado.
C
	lens = MIN ( iptr + 40, lenw )
	indx = INDEX ( watch ( iptr:lens ), 'SEVERE TSTM ' )
	IF ( indx .gt. 0 ) THEN
	    itype = 1
	    iptr  = iptr + indx + 10
	  ELSE
	    indx = INDEX ( watch ( iptr:lens ), 'TORNADO ' )
	    IF ( indx .gt. 0 ) THEN
		itype = 0
		iptr  = iptr + indx + 6
	    END IF
	END IF
C
C*	Check for cancellation.
C
	lens = MIN ( iptr + 40, lenw )
	indx = INDEX ( watch ( iptr:lens ), 'CANCELLED' )
	IF ( indx .gt. 0 ) THEN
	    icancl = 1
	    RETURN
	END IF
C
C*	Continue decoding if this is not a watch cancellation.
C*	Get start and stop times for watch.
C
	indx = INDEX ( watch ( iptr:lenw ), '-' ) 
	IF ( indx .gt. 0 ) THEN
	    iptr = iptr + indx - 1
	    indxz1 = INDEX ( watch ( iptr - 3:iptr ), 'Z' )
	    indxz2 = INDEX ( watch ( iptr:iptr + 9 ), 'Z' ) 
	    IF ( ( indxz1 .gt. 0 ) .and. ( indxz2 .gt. 0 ) ) THEN
		istrt = iptr + indxz1 - 10
		istop = iptr + indxz2 - 7
		CALL WW_CTIM ( watch ( istrt:istrt + 5 ),
     +			       watch ( istop:istop + 5 ), iotarr,
     +			       strtim, stptim, ier )
	    END IF
	END IF
C
C*	Get corner points.
C
	good = .true.
	indx = INDEX ( watch ( iptr:lenw ), 'AXIS..' )
	IF ( indx .gt. 0 ) THEN
	    iptr = iptr + indx + 5
	    lens = INDEX ( watch ( iptr:iptr + 5 ), ' ' ) - 2
	    IF ( lens .ge. 0 ) THEN
		CALL ST_CRNM ( watch ( iptr:iptr + lens ), side, ier )
		IF ( INDEX ( watch ( iptr:lenw ),
     +			     'EAST AND WEST ' ) .ne. 0 ) THEN
		    iflag = 1
		  ELSE IF ( INDEX ( watch ( iptr:lenw ),
     +			            'NORTH AND SOUTH' ) .ne. 0 ) THEN
		    iflag = 2
		  ELSE IF ( INDEX ( watch ( iptr:lenw ),
     +			   	    'EITHER SIDE' ) .ne. 0 ) THEN
		    iflag = 3
		  ELSE
		    good = .false.
		END IF
	      ELSE
		good = .false.
	    END IF
	    IF ( good ) THEN
		indx = INDEX ( watch ( iptr:lenw ), 'LINE.. ' )
		IF ( indx .gt. 0 ) THEN
		    iptr = iptr + indx + 6
		    i    = 1
		    DO WHILE ( ( i .le. 2 ) .and. good )
			lens = INDEX ( watch ( iptr:lenw ), '/' ) - 1
			IF ( lens .ge. 3 ) THEN
		            CALL WW_AXPT ( watch ( iptr:iptr + lens-1 ), 
     +					   dist ( i ), bear ( i ),
     +			    		   locid ( i ), ier )	
			    IF ( ier .lt. 0 ) good = .false.
			  ELSE
			    good = .false.
			END IF
			IF ( ( i .eq. 1 ) .and. good ) THEN
			    indx = INDEX ( watch ( iptr:lenw ), '-' )
			    IF ( indx .gt. 0 ) THEN
				iptr = iptr + indx + 1
			      ELSE
				good = .false.
			    END IF
			END IF
			i = i + 1
		    END DO
		  ELSE
		    good = .false.
		END IF 
	    END IF
	    IF ( good ) THEN
		DO i = 1, 2
		    dist ( i ) = PR_HGSF ( dist ( i ) )
		    dist ( i ) = PR_HGFM ( dist ( i ) ) 
		END DO
		side = PR_HGSF ( side ) 
		side = PR_HGFM ( side )
	        CALL WW_CRNR ( side, iflag, locid ( 1 ), locid ( 2 ),
     +			       dist, bear, rlat, rlon, npt, ier )	
		IF ( ier .ne. 0 ) THEN
		    errstr = locid (1) (:3) // ' or ' // locid (2) (:3)
		    CALL DC_WLOG (0, 'DCWTCH', ier, errstr, ier1 ) 
		    iret = -1
		END IF
	    END IF
	END IF
C
C*	Check for watches being replaced.
C
	DO WHILE ( ( indx .gt. 0 ) .and. ( irepl .le. 10 ) ) 
	    indx = INDEX ( watch ( iptr:lenw ), 'REPLACES' )
	    IF ( indx .gt. 0 ) THEN
		iptr = iptr + indx + 7
		indx = INDEX ( watch ( iptr:lenw ), ' WW ' )
		IF ( indx .gt. 0 ) THEN
		    irepl = irepl + 1
		    iptr  = iptr + indx + 3
		    CALL WW_WNUM ( watch ( iptr:iptr + 5 ), 6,
     +				   rwnum ( irepl ), ier )
		END IF
	    END IF
	END DO
C*
	RETURN
	END
