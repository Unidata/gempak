	SUBROUTINE GMTOWQ ( amtrx, invers, polnr,
     +			    np, rlat, rlon, slat, slon, iret )
C************************************************************************
C* GMTOWQ								*
C* 									*
C* This subroutine converts a location from actual latitude/longitude	*
C* coordinates to latitude and longitude in a rotated coordinate, if	*
C* INVERS = .false.  The inverse transformation is done if INVERS =	*
C* .true.  Actual lat/lon coordinates are M coordinates; the lat/lon	*
C* coordinates in the rotated frame are W coordinates (map) or Q	*
C* coordinates (grid).							*
C*									* 
C* This routine performs the transformation needed for the any map/grid *
C* projection.  The rotation matrix is AMTRX.  The rotation matrices	*
C* for map and grid projections are set in the update routines UPD for  *
C* map/grid projections.						*
C*									*
C* The transformation from M -> W/Q proceeds as follows:		*
C*									*
C*   1)  Transform actual lat/lon to cartesian coordinates.		*
C*									*
C*   2)  Rotate the cartesian system three times:			*
C*	 a) Through POLON = ANGLE2 to reposition xy axes 		*
C*	 b) Through POLAT = ANGLE1 to reposition xz axes		*
C*	 c) Through ROTAT = ANGLE3 to reposition yz or xy axes		*
C*									*
C*   3)  Transform from rotated cartesian to spherical W/Q coordinates.	*
C*									*
C* The inverse from W/Q -> M reverses these steps.			*
C*									*
C* The longitude rotation angle is only used for the special case when	*
C* a point falls on a pole in the rotated coordinate.  In that case,	*
C* longitude is indefinite, but for cylindrical projections a definite	*
C* longitude is required when a pole corresponds to a corner point	*
C* That is why this rotation is done.					*
C*									*
C* 									*
C* GMTOWQ ( AMTRX, INVERS, POLNR, NP, RLAT, RLON, SLAT, SLON, IRET )	*
C*									*
C* Input parameters:							*
C*	AMTRX(3,3)	REAL		Transformation matrix		*
C*	INVERS		LOGICAL		Inverse transform flag		*
C*	POLNR		REAL		Longitude rotation angle	*
C*	NP		INTEGER		Number of points		*
C* 	RLAT (NP)	REAL		Input latitudes			*
C* 	RLON (NP)	REAL		Input longitudes		*
C*									*
C* Output parameters:							*
C* 	SLAT (NP)	REAL		Output latitudes		*
C* 	SLON (NP)	REAL		Output longitudes		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* K. Brill/EMC		 3/96						*
C* T. Lee/GSC		 7/96		Fixed pole points plot for RADAR*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	PARAMETER	( EPS = .03 )
C*
	REAL 		amtrx (3,3)
	LOGICAL		invers
	REAL		rlat (*), rlon (*), slat (*), slon (*)
C*
	REAL		tmtrx (3,3), x (3), xp (3), phi,
     +			rlm, cosp, xx, yy
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Set the transformation matrix and look for identity matrix.
C
	IF ( .not. invers ) THEN
	    DO j = 1, 3
		DO i = 1, 3
		    tmtrx (i,j) = amtrx (i,j)
		END DO
	    END DO
	ELSE
C
C*	    The inverse is just the transpose.
C
	    DO j = 1, 3
		DO i = 1, 3
		    tmtrx (i,j) = amtrx (j,i)
		END DO
	    END DO
	END IF
C
C*	Process all the points.
C
	DO ii = 1, np
	    IF ( .not. ERMISS ( rlat (ii) ) .and.
     +		 .not. ERMISS ( rlon (ii) ) ) THEN
C
C*	        Convert from spherical to cartesian coordinates.
C
	    	phi   = rlat (ii) * DTR
	    	rlm   = rlon (ii) * DTR
	    	cosp  = COS (phi)
	    	xx    = COS (rlm)
	    	yy    = SIN (rlm)
	    	x (1) = cosp * xx
	    	x (2) = cosp * yy
	    	x (3) = SIN (phi)
C
C*	    	Rotate the coordinates and confine coordinate values.
C
	    	DO i = 1, 3
		    xp (i) = 0.
		    DO j = 1, 3
		    	xp (i) = xp (i) + tmtrx (i,j) * x (j)
		    END DO
		    IF ( xp (i) .gt. 1. ) xp (i) = 1.
		    IF ( xp (i) .lt. -1. ) xp (i) = -1.
	    	END DO
C
C*	    	Convert from cartesian to spherical coordinates.
C*		If the rotated coordinate is at a pole; then,
C*		rotate the longitude (since it is indefinite).
C
	    	slat (ii) = ASIN ( xp (3) ) * RTD
		IF ( ABS ( slat (ii) - 90. ) .lt. EPS ) THEN
		    slat (ii) = 90.
		    rcos = COS ( polnr )
		    rsin = SIN ( polnr )
		    IF ( invers ) rsin = -rsin
		    xxp = xx * rcos + yy * rsin
		    yyp = (-xx) * rsin + yy * rcos
		    slon (ii) = ATAN2 ( yyp, xxp ) * RTD
		ELSE
		    slon (ii) = ATAN2 ( xp (2), xp (1) ) * RTD
		END IF
	    ELSE
		slat (ii) = RMISSD
		slon (ii) = RMISSD
	    END IF
	END DO
C*
	RETURN
	END
