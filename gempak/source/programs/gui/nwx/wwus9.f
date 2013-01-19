	SUBROUTINE WW_US9 ( watch, lenw, ibpnt, iotarr, itype, wnum,
     +		strtim, stptim, icorr, icancl, rlat, rlon, npt, iret )
C************************************************************************
C* WW_US9  								*
C*									*
C* This subroutine decodes the type, watch number, correction and       *
C* cancellation indicators, start and stop times, and corner points     *
C* for a WWUS9 report.                                                  *
C*                                                                      *
C* WW_US9  ( WATCH, LENW, IBPNT, IOTARR, ITYPE, WNUM, STRTIM, STPTIM,   *
C*           ICORR, ICANCL, RLAT, RLON, NPT, IRET )                     *
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
C*	IRET		INTEGER		Return code			*
C*				          0 = normal return             *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 8/98	                                        *
C* D. Kidwell/NCEP	 4/99	Corrected prologue                      *
C* D. Kidwell/NCEP	 4/99	Added icorr to WW_CTIM call             *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	watch, wnum, strtim, stptim
	REAL		rlat (*), rlon (*)
	INTEGER		iotarr (*)
C*
	CHARACTER 	wchnum*6, wchtim (25)*20, stmer (2)*2, zone*3
	INTEGER		istim (2)
	LOGICAL		done
C*
	CVTDEG (nn) = 	( nn - MOD (nn,10) ) * .1 + MOD (nn,10) / 6.
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for a correction.
C
	iptr = ibpnt + 10
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
C*	Check for type - either severe thunderstorm or tornado.
C
	lens = MIN ( iptr + 150, lenw )
	indx = INDEX ( watch ( iptr:lens ), 'SEVERE THUNDERSTORM ' )
	IF ( indx .gt. 0 ) THEN
	    itype = 1
	    iptr  = iptr + indx + 18
	  ELSE
	    indx = INDEX ( watch ( iptr:lens ), 'TORNADO ' )
	    IF ( indx .gt. 0 ) THEN
		itype = 0
		iptr  = iptr + indx + 6
	    END IF
	END IF
C
C*	Check for watch number and cancellation.
C
	lens = MIN ( iptr + 40, lenw )
	indx = INDEX ( watch ( iptr:lens ), 'WATCH NUMBER ' )
	IF ( indx .gt. 0 ) THEN
	    icancl = 0
	    iptr   = iptr + indx + 11  
	  ELSE
	    indx = INDEX ( watch ( iptr:lens ), 'WATCH CANCELLATION ' )
	    IF ( indx .gt. 0 ) THEN
		icancl = 1
		iptr   = iptr + indx + 17
		indx   = INDEX ( watch ( iptr:lens ), 'NUMBER ' )
		IF ( indx .gt. 0 ) THEN
		    iptr = iptr + indx + 5
		END IF
	    END IF
	END IF
C
	IF ( indx .gt. 0 ) THEN
	    CALL ST_LDSP ( watch ( iptr:iptr + 5 ), wchnum, nc, ier )
	    indx = INDEX ( wchnum, ' ' )
	    IF ( indx .eq. 0 ) indx = nc + 1
	    CALL ST_INTG ( wchnum ( :indx - 1 ), numwch, ier )
	    IF ( ier .eq. 0 ) THEN
		wnum = wchnum ( :indx - 1 )
	    END IF
	    iptr = iptr + indx
	END IF
C
C*	Continue decoding if this is not a watch cancellation.
C
	IF ( icancl .eq. 0 ) THEN
C
C*	    Get start and stop times for watch.
C
	    indx = INDEX ( watch ( iptr:lenw ), 'EFFECTIVE ' ) 
	    IF ( indx .gt. 0 ) THEN
		iptr = iptr + indx - 1
		lens = MIN ( iptr + 100, lenw )
		CALL ST_CLST ( watch ( iptr:lens ), ' ', ' ', 25, 
     +			       wchtim, nstr, ier )
		done  = .false.
		istim ( 1 ) = IMISSD
		istim ( 2 ) = IMISSD
		i     = 2
		DO WHILE ( .not. done )
		    IF ( wchtim ( i ) .eq. 'FROM' ) THEN
			itm = 1
		      ELSE IF ( wchtim ( i ) .eq. 'UNTIL' ) THEN
			itm = 2
		      ELSE
			itm = 0
		    END IF
C
		    IF ( itm .gt. 0 ) THEN
			izone = 0
			ilen = INDEX ( wchtim ( i + 1 ), ' ' ) - 1
			CALL ST_INTG ( wchtim ( i + 1 ) ( :ilen ), 
     +				       istim ( itm ), ier )
			IF ( ier .eq. 0 ) THEN
			    stmer ( itm ) = wchtim ( i + 2 ) ( 1:2 )
			    IF ( itm .eq. 2 ) izone = 3
			  ELSE IF ( wchtim(i+1)(:ilen) .eq. 'NOON' .or.
     +			      wchtim(i+1)(:ilen) .eq. 'MIDNIGHT' ) THEN
		            istim ( itm ) = 1200
			    IF ( wchtim(i+1)(:ilen) .eq. 'NOON' ) THEN
				stmer ( itm ) = 'PM'
			      ELSE
				stmer ( itm ) = 'AM'
			    END IF
			    IF ( itm .eq. 2 ) izone = 2
			END IF
			IF ( itm .eq. 1 ) THEN
			    i = i + 2
			  ELSE
			    IF ( izone .gt. 0 ) THEN
				zone = wchtim ( i + izone ) ( 1:3 )
			      ELSE
				zone = ' '
			    END IF
			    done = .true.
			END IF
		      ELSE
			i = i + 1
			IF ( i .gt. 25 ) done = .true.
		    END IF
		END DO
		CALL WW_CTIM ( istim, stmer, zone, iotarr, icorr,
     +			       strtim, stptim, ier )
	    END IF
C
C*	    Get corner points.
C
	    iptr = lenw - 50
	    indx = INDEX ( watch ( iptr:lenw ), ';' )
	    iptr = iptr + indx
	    indx = INDEX ( watch ( iptr:lenw ), ';' )
	    iend = iptr + indx - 1
	    npt   = 0
C
	    DO WHILE ( iptr .lt. iend )
		CALL ST_INTG ( watch ( iptr:iptr + 2 ), lat, ier )
		indx = INDEX ( watch ( iptr:lenw ), ',' )
		iptr = iptr + indx
		CALL ST_INTG ( watch ( iptr:iptr + 3 ), long, ier )
		indx = INDEX ( watch ( iptr:lenw ), ' ' )
		IF ( indx .gt. 0 ) THEN
		    iptr = iptr + indx
		  ELSE
		    iptr = iend
		END IF
		npt = npt + 1
		IF ( lat .ne. IMISSD )  rlat ( npt ) = CVTDEG (lat)
		IF ( long .ne. IMISSD ) rlon ( npt ) = - CVTDEG (long)
	    END DO
	    npt = npt + 1
	    rlat ( npt ) = rlat ( 1 )
	    rlon ( npt ) = rlon ( 1 )
	END IF
C*
	RETURN
	END
