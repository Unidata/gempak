	SUBROUTINE GG_CTRD ( np, rlat, rlon, alat, alon, iret )
C************************************************************************
C* GG_CTRD								*
C*									*
C* This subroutine gets the centroid of a bounded area.  It assumes that*
C* the first and last points are identical.                             *
C*									*
C* GG_CTRD ( NP, RLAT, RLON, ALAT, ALON, IRET )                         *
C*									*
C* Input parameters:							*
C*	NP  		INTEGER		No. of points for bounded area  *
C*	RLAT (*)	REAL		Latitudes of area points        *
C*	RLON (*)	REAL		Longitudes of area points       *
C*									*
C* Output parameters:							*
C*	ALAT 		REAL		Latitude of centroid            *
C*	ALON 		REAL		Longitude of centroid           *
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	11/99	                                        *
C************************************************************************
	REAL		rlat (*), rlon (*)
C*
	LOGICAL		dateln
	REAL		xlon ( 50 )
C------------------------------------------------------------------------
	iret = 0
	npts = np
	IF ( npts .gt. 50 ) npts = 50
	npm1 = npts - 1
C
	DO k = 1, npts
	    xlon ( k ) = rlon ( k ) 
	END DO
C
C*	Determine whether the dateline has been crossed.
C
	dateln = .false.
	DO k = 1, npm1
	    IF ( ABS ( xlon ( k ) ) .gt. 150. ) dateln = .true.
	END DO
C
	IF ( dateln ) THEN
	    dateln = .false.
	    DO k = 1, npm1
		IF ( ( xlon (k) * xlon (k+1) ) .lt. 0. ) dateln = .true.
	    END DO
C
	    IF ( dateln ) THEN
		DO k = 1, npm1
		    IF ( xlon (k) .lt. 0. ) xlon (k) = xlon (k) + 360.
		END DO
	    END IF
	END IF
C
	alat = rlat ( 1 )
	alon = xlon ( 1 )
	DO k  = 2, npm1
	    alat = alat + rlat ( k )
	    alon = alon + xlon ( k ) 
	END DO
	alat = alat / npm1
	alon = alon / npm1
	IF ( alon .gt. 180. ) alon = alon - 360.
C*
	RETURN
	END
