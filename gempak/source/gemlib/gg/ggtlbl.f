	SUBROUTINE GG_TLBL ( np, rlat, rlon, alat, alon, iret )
C************************************************************************
C* GG_TLBL								*
C*									*
C* This subroutine gets the location of the text label for current      *
C* Convective Sigmets and Convective Outlook bounded areas.  The        *
C* location is the topmost point of the area.  				*
C*									*
C* GG_TLBL ( NP, RLAT, RLON, ALAT, ALON, IRET )                         *
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
C* A. Hardy/NCEP	 8/02						*
C************************************************************************
	REAL		rlat (*), rlon (*)
C*
	INTEGER		indx (20)
C------------------------------------------------------------------------
	iret  = 0
	npm1 = np - 1
C
C*	Find the centroid of the area.
C
	CALL GG_CTRD ( np, rlat, rlon, clat, clon, ier )
C
C*	Find the topmost point below the centroid.
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
	    IF ( angle .gt. angmin ) THEN
		angmin = angle
		alat   = rlat ( k )
		alon   = rlon ( k )
	    END IF
C
C*	    Save the indices of all points below the centroid.
C
	    IF ( angle .gt. 0. ) THEN
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
	    IF ( ( rlat ( indx ( i ) ) - alat ) .gt. .17 ) THEN
	        alat = rlat ( indx ( i ) ) 
		alon = rlon ( indx ( i ) ) 
	    END IF
	END DO
C*
	RETURN
	END
