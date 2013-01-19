	SUBROUTINE GCYLML ( mproj, msppj, np, dlat, dlon, polat, polon, 
     +			    rotat, sclmcd, swchxy, xl, yl, iret )
C************************************************************************
C* GCYLML								*
C* 									*
C* This subroutine converts a location from latitude/longitude		*
C* coordinates to linear intermediate coordinates for cylindrical	*
C* projections.								*
C*									*
C* SWCHXY is a flag for a -90 degree rotation of the linear coordinates *
C* to develop a transverse projection with the y-axis oriented south to *
C* north.                                                               *
C* 									*
C* GCYLML ( MPROJ, MSPPJ, NP, DLAT, DLON, POLAT, POLON, ROTAT, 		*
C*          SCLMCD, SWCHXY, XL, YL, IRET )				*
C*									*
C* Input parameters:							*
C*	MPROJ		INTEGER		Projection class		*
C*	MSPPJ		INTEGER		Projection type			*
C*	NP		INTEGER		Number of points		*
C* 	DLAT (NP)	REAL		Latitudes			*
C* 	DLON (NP)	REAL		Longitudes			*
C*	POLAT		REAL		Not used			*
C*	POLON		REAL		Central longitude 		*
C*	ROTAT		REAL		Not used			*
C*	SCLMCD		REAL		Scaling for MCD			*
C*	SWCHXY		LOGICAL		Flag to rotate x,y		*
C* 									*
C* Output parameters:							*
C* 	XL (NP)		REAL		X coordinates			*
C* 	YL (NP)		REAL		Y-coordinates			*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Chatters/RDS	 7/84	GEMPLT Version 3.0			*
C* M. desJardins/GSFC	 7/87	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 1/88	Fixed CED rounding problem at pole	*
C* M. desJardins/GSFC	 6/88	Added EPS for rounding			*
C* K. Brill/EMC		 3/96	Changes for general rotation		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		xl (*), yl (*), dlat (*), dlon (*)
	LOGICAL		swchxy
C*
	PARAMETER	( EPS = .00001 )
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Loop through all points.
C
	hlfpin = -HALFPI - EPS
	hlfpip = HALFPI + EPS

	DO  i = 1, np
C
C*	  Check for missing data.
C
	  IF  ( ( ERMISS ( dlat (i) ) ) .or. 
     +		( ERMISS ( dlon (i) ) ) )  THEN
	    xl (i) = RMISSD
	    yl (i) = RMISSD
	  ELSE
	    rlat   = dlat (i) * DTR
	    rlon   = dlon (i) * DTR
	    xl (i) = rlon - polon
	    yl (i) = RMISSD
C
C*	    Compute internal projection angles.
C*	    Special case of cylinder tangent at equator.
C*	    The general case is implemented via coordinate rotation.
C
C*	    Compute Y axis coordinate for given projection type.
C
	    IF ( mproj .eq. MPCEQU )  THEN
C				! Cylindrical equidistant
		IF  ( ( rlat .ge. hlfpin ) .and. 
     +		      ( rlat .le. hlfpip ) )  THEN
		    yl (i) = rlat
		END IF
C				! Mercator
	      ELSE IF ( mproj .eq. MPCMER ) THEN
		IF (( rlat .gt. -HALFPI ) .and. ( rlat .lt. HALFPI )) 
     +			THEN
		    a      = HALFPI - rlat
		    yl (i) = ALOG ( ( 1.0 + COS ( a ) ) / SIN ( a ) )
		END IF
C				! Modified cylindrical equidistant
	      ELSE IF ( mproj .eq. MPCMCD ) THEN
		IF  ( ( rlat .ge. hlfpin ) .and. 
     +		      ( rlat .le. hlfpip ) )  THEN
		    yl (i) = sclmcd * rlat
		END IF
	   END IF
C
C*	   If the y value is missing, set the x value to missing.
C
	   IF  ( ERMISS ( yl (i) ) )  xl (i) = RMISSD
	  END IF
	  IF ( swchxy ) THEN
		xsav = xl (i)
	 	xl (i) = -yl (i)
		yl (i) = xsav
	  END IF
	END DO
	IF ( swchxy ) THEN
	    CALL PRNLNR ( np, yl, ier )
	ELSE
	    CALL PRNLNR ( np, xl, ier )
	END IF
C*
	RETURN
	END
