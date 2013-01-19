	SUBROUTINE IS_VLOC ( report, lenr, keywd, mxp, iptr, vlat, vlon,
     +			     ipkey, iret )
C************************************************************************
C* IS_VLOC 								*
C*									*
C* This subroutine gets the volcano location.				*
C*                                                                      *
C* IS_VLOC ( REPORT, LENR, KEYWD, MXP, IPTR, VLAT, VLON, IPKEY, IRET )	*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		Partial sigmet report string    *
C*	LENR		INTEGER		Length of string                *
C*	KEYWD		INTEGER		Keyword number for search	*
C*	MXP		INTEGER		Maximum number of points	*
C*	IPTR		INTEGER		Pointer to loc after OBS/FCST	*
C*									*
C* Output parameters:							*
C*      VLAT		REAL		Latitude of volcano		*
C*	VLON		REAL		Longitudes of volcano		*
C*	IPKEY		INTEGER		Pointer to loc after keyword	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					  1 = missing location		*
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 4/00	Created					*
C* D. Kidwell/NCEP	10/01   Used ST_WORD to check for lat/lon       *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report
C*
	CHARACTER	carr (20)*12, work*20
	LOGICAL		done
C------------------------------------------------------------------------
	iret = 0
	iptr = 1
	vlat = RMISSD
	vlon  = RMISSD
	ipkey = IMISSD
	IF ( keywd .eq. 1 ) THEN
C
C*	    keywd = 1, so look for keyword ' FM ' or ' FROM '
C*	    after phenomenon.
C
	    lenv  = MIN ( 13, lenr ) 
	    iloc = INDEX ( report ( :lenv ), ' FM ' )
	    IF ( iloc .eq. 0 ) THEN
		lenv  = MIN ( 15, lenr ) 
		iloc = INDEX ( report ( :lenv ), ' FROM ' )
		IF ( iloc .eq. 0 ) THEN
		    iret = 1
		    RETURN
	          ELSE
		    ii = iloc + 6
	        END IF
	      ELSE
	        ii = iloc + 4
	    END IF
	  ELSE
	    lenv  = MIN ( 160, lenr)
	    iloc = INDEX ( report ( 1:lenv ), ' LOC ' )
	    IF ( iloc .eq. 0 ) THEN
	        iret = 1
	        RETURN
	    END IF
	    ii = iloc + 5
	END IF
	ikeysv = ii
C
C*	Look for location after keyword ' FM ', ' FROM ', or ' LOC'
C

	done = .false.
	lenv = MIN ( 50, lenr)
	DO WHILE ( .not. done )
	    CALL ST_ALNM ( report ( ii:ii ), ityp, ier )
	    IF ( ityp .eq. 1 ) THEN
	        done = .true.
	        ibeg = ii - 1
	      ELSE
	        ii = ii + 1
		IF ( ii .gt. lenv ) THEN
		    iret = 1
		    RETURN
		END IF
            END IF
	END DO
	iend = INDEX ( report ( ii:lenr), '=' )
	IF ( iend .eq. 0 ) THEN
	    IF ( iend .eq. 0 ) THEN
		iend = lenr - ii + 1
	    END IF
	END IF
	iend = ii + iend - 1
	lenv = MIN ( iend, ii + 75)
C
C*	Get the point.
C
	iloc = ibeg
	CALL ST_CLST ( report ( iloc:lenv ), ' ', ' ', mxp, carr, 
     +	 	       npts, ier )
	npts = MIN ( 4, npts)
	ii = 1
	DO WHILE ( ii .le. npts )
	    work = carr ( ii )
	    CALL ST_LSTR ( work, lenw, ier )
	    IF ( lenw .le. 5 ) THEN
		CALL ST_WORD ( work ( :lenw ), ityp, ier )
		IF ( ityp .eq. 1 ) THEN
		    IF ( ii + 1. le. npts ) THEN
		        work = work ( :lenw ) // carr ( ii + 1 )
			IF ( ii + 2 .le. npts ) THEN
		            carr ( ii + 1 ) = carr ( ii + 2 )
			    IF ( ii + 3 .le. npts ) THEN
		                carr ( ii + 2 ) = carr ( ii + 3 )
			    END IF 
			END IF
		        npts = npts - 1
		    END IF
		END IF
	    END IF
C
	    CALL IS_LTLN ( work, xlat, xlon, ier )
	    IF ( ier .eq. 0  ) THEN
		IF ( ii .eq. 1) THEN
		    vlat = xlat
		    vlon = xlon
		  ELSE
		    iret = 1
		    ipkey = ikeysv
		    RETURN
		END IF
	      ELSE
		IF ( ii .eq. 1 ) THEN
		    iret = 1
		    RETURN
		  ELSE
		    ii =5
		END IF
	    END IF
	    IF ( keywd .eq. 2) THEN
		ii = 7
	     ELSE
	        ii = ii + 1
	    END IF
	END DO
C*
	RETURN
	END
