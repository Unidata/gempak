	SUBROUTINE GG_WLBL ( np, rlat, rlon, alat, alon, iret )
C************************************************************************
C* GG_WLBL								*
C*									*
C* This subroutine gets the location of the text label for current      *
C* thunderstorm and tornado watch boxes and international SIGMET        *
C* bounded areas.  The location is the bottommost point of the box or   *
C* area.  If the bottom of a watch box is flat or has latitudes which   *
C* differ by no more than 10 minutes, the leftmost point along the      *
C* bottom is used.                                                      *
C*									*
C* GG_WLBL ( NP, RLAT, RLON, ALAT, ALON, IRET )                         *
C*									*
C* Input parameters:							*
C*	NP  		INTEGER		Number of points                *
C*	RLAT (*)	REAL		Latitudes of points             *
C*	RLON (*)	REAL		Longitudes of points            *
C*									*
C* Output parameters:							*
C*	ALAT 		REAL		Latitude of label               *
C*	ALON 		REAL		Longitude of label              *
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 4/99	                                        *
C* D. Kidwell/NCEP	 5/99	Changed constant to .17 for 10 minutes  *
C* D. Kidwell/NCEP	11/99	Revised for SIGMETs; call GG_CTRD       *
C* S. Guan/NCEP         02/17   Incrase dimesion of indx from 20 to 500 *
C************************************************************************
	REAL		rlat (*), rlon (*)
C*
	INTEGER		indx (500)
C------------------------------------------------------------------------
	iret  = 0
	npm1 = np - 1
C
C*	Find the centroid of the watch box.
C
	CALL GG_CTRD ( np, rlat, rlon, clat, clon, ier )
C
C*	Find the leftmost point below the centroid.
C
	angmin = 0.
	alat   = clat
	alon   = clon
	ipts   = 0
C
	DO k   = 1, npm1
	    IF ( ( rlat ( k ) * clat ) .ge. 0. ) THEN
		deltay = rlat ( k ) - clat
	      ELSE
		deltay = ABS ( rlat ( k ) ) + ABS ( clat )
		IF ( clat .gt. 0. ) deltay = -deltay
	    END IF
	    IF ( ( rlon ( k ) * clon ) .ge. 0. ) THEN
		deltax = rlon ( k ) - clon
	      ELSE
		deltax = 360. - ( ABS ( rlon ( k ) ) + ABS ( clon ) )
		IF ( clon .lt. 0. ) deltax = -deltax
	    END IF
C
	    angle = atan2 ( deltay, deltax )
	    IF ( angle .lt. angmin ) THEN
		angmin = angle
		alat   = rlat ( k )
		alon   = rlon ( k )
	    END IF
C
C*	    Save the indices of all points below the centroid.
C
	    IF ( angle .lt. 0. ) THEN
		ipts = ipts + 1
		indx ( ipts ) = k
	    END IF
	END DO
C
C*	Check that the latitude of the leftmost point is within 
C*      10 minutes of the latitude of the bottommost point.
C*	If it is not, use the bottomost point.
C
	DO i = 1, ipts
	    IF ( ( alat - rlat ( indx ( i ) ) ) .gt. .17 ) THEN
	        alat = rlat ( indx ( i ) ) 
		alon = rlon ( indx ( i ) ) 
	    END IF
	END DO
C*
	RETURN
	END
