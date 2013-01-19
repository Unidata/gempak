	SUBROUTINE WW_US40 ( watch, lenw, ibpnt, iotarr, itype, wnum,
     +		strtim, stptim, icorr, icancl, rlat, rlon, npt, irepl,
     +		rnums, iret )
C************************************************************************
C* WW_US40  								*
C*									*
C* This subroutine decodes the type, watch number, correction and       *
C* cancellation indicators, start and stop times, corner points, and    *
C* watch replacement information for a WWUS40 report.                   *
C*                                                                      *
C* WW_US40  ( WATCH, LENW, IBPNT, IOTARR, ITYPE, WNUM, STRTIM, STPTIM,  *
C*           ICORR, ICANCL, RLAT, RLON, NPT, IREPL, RNUMS, IRET )       *
C*									*
C* Input parameters:							*
C*	WATCH 		CHAR*		Report string                   *
C*	LENW		INTEGER		Length of report		*
C*	IBPNT		INTEGER		Pointer to location in report   *
C*	IOTARR (5)	INTEGER 	Time array                      *
C*									*
C* Output parameters:							*
C*	ITYPE		INTEGER		Watch type                      *
C*	WNUM		CHAR*		Watch number                    *
C*	STRTIM		CHAR*  		Watch start time, GEMPAK format *
C*	STPTIM		CHAR*		Watch end time, GEMPAK format   *
C*	ICORR		INTEGER		Correction indicator            *
C*	ICANCL		INTEGER		Watch cancellation indicator    *
C*	RLAT (*)	REAL            Latitudes of vertex points      *
C*	RLON (*)	REAL		Longitudes of vertex points     *
C*	NPT		INTEGER		Number of vertex points         *
C*	IREPL		INTEGER		Number of watches replaced      *
C*	RNUMS 		CHAR*		Replaced watch numbers          *
C*	IRET		INTEGER		Return code			*
C*				          0 = normal return             *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 7/99	From WW_US9                             *
C* E. Safford/SAIC	12/07	rename WWCRNR_CALC			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	watch, wnum, strtim, stptim, rnums
	REAL		rlat (*), rlon (*)
	INTEGER		iotarr (*)
C*
	CHARACTER	locid (2)*4, rwnum (10)*4
	REAL		dist (2), bear (2)
	LOGICAL 	good
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for a correction.
C
	iptr = ibpnt + 11
	IF ( iptr .lt. lenw ) THEN
	    indx = INDEX ( watch ( iptr:iptr+5 ), 'COR' ) 
	    IF ( indx .gt. 0 ) THEN
		icorr = 1
		iptr  = iptr + indx + 2
	      ELSE
		icorr = 0
	    END IF
	END IF
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
C
	icancl = 0
C
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
     +					 lens, dist ( i ), bear ( i ),
     +			    		 locid ( i ), ier )	
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
	        CALL WWCRNR_CALC ( side, iflag, locid ( 1 ), locid ( 2 ),
     +			       dist, bear, rlat, rlon, npt, ier )	
  	        IF ( ier .eq. 0 ) THEN
	            npt = npt + 1
	            rlat ( npt ) = rlat ( 1 )
	            rlon ( npt ) = rlon ( 1 )
	        END IF
	    END IF
	END IF
C
C*	Check for watches being replaced.
C
	irepl = 0
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
	IF ( irepl .gt. 0 ) 
     +			CALL ST_LSTC ( rwnum, irepl, ',', rnums, ier )
C*
	RETURN
	END
