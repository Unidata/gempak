	SUBROUTINE GAZMML ( mproj, msppj, np, dlat, dlon, polat, 
     +			    polon, rotat, azmsav, xl, yl, iret )
C************************************************************************
C* GAZMML								*
C* 									*
C* This subroutine converts a point from latitude longitude		*
C* coordinates to linear intermediate coordinates for azimuthal map	*
C* projections. 							*
C*									*
C* GAZMML  ( MPROJ,  MSPPJ, NP, DLAT, DLON, POLAT, POLON, ROTAT, 	*
C*           AZMSAV, XL, YL, IRET )					*
C*									*
C* Input parameters:							*
C*	MPROJ		INTEGER		Map class			*
C*	MSPPJ		INTEGER		Map projection			*
C*	NP		INTEGER		Number of points		*
C* 	DLAT (NP)	REAL		Latitudes			*
C* 	DLON (NP)	REAL		Longitudes			*
C*	POLAT		REAL		Pole latitude			*
C*	POLON		REAL		Central longitude		*
C*	ROTAT		REAL		Rotation (not used)		*
C*	AZMSAV		REAL		Special azimuthal angle (unused)*
C* 									*
C* Output parameters:							*
C*	XL (NP)		REAL		X linear coordinates		*
C*	YL (NP)		REAL		Y linear coordinates		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Chatters/RDS	 7/87	GEMPLT Version 3.0			*
C* M. desJardins/GSFC	 6/85	GEMPLT Version 3.1			*
C* B. Doty/RDS		 7/87	Fixed Orthographic out-of-hemisphere	*
C* M. desJardins/GSFC	 1/88	Fixed missing data			*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* A. Taylor/ARL        12/93   Enabled Oblique Azimuthal               *
C* K. Brill/NMC          1/94   Cleanup                                 *
C* D. Keiser/GSC	10/95	Fixed rounding error of "z" with EPS	*
C* K. Brill/EMC		 3/96	Remove Taylor code; general rotated proj*
C* K. Brill/EMC		 5/96	Check for latitude out of range		*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
C*
	REAL		xl (*), yl (*), dlat (*), dlon (*)
C*
	INCLUDE		'ERMISS.FNC'
C-------------------------------------------------------------------------
	iret  = NORMAL
C
C*	Loop through all points.
C
	DO  i = 1, np
C
C*	  Check for missing data.
C
	  IF ( ERMISS (dlat (i)) .or. ERMISS (dlon (i)) .or.
     +	       dlat (i) .gt. 90.0 .or. dlat (i) .lt. -90.0 )  THEN
	    xl (i) = RMISSD
	    yl (i) = RMISSD
	  ELSE
	    rlat   = dlat (i) * DTR
	    rlon   = dlon (i) * DTR
	    xl (i) = RMISSD
	    yl (i) = RMISSD
	    gamp   = RMISSD
	    r      = RMISSD
C
C*	   Ignore bad data.
C
	   IF  ( ( ERMISS (rlat) ) .or. ( ERMISS (rlon) ) )  THEN
C
C*	      Compute projection angles.
C
C*		North azimuthal projection
C
	      ELSE IF  ( msppj .eq. MSANOR )  THEN
		gamp = rlon - polon
		a    = HALFPI - rlat
C
C*		South azimuthal projtection
C
	      ELSE IF ( msppj .eq. MSASOU ) THEN
		gamp = - ( rlon - polon )
		a    = rlat   + HALFPI
	    END IF
C
C*	    Select  range computation based on projection type.
C
C*	    Stereographic projection
C
	    IF ( ( mproj .eq. MPASTR ) .and. ( a .lt. PI ) ) THEN
		r = TAN ( a / 2.0 )
C
C*		  Azimuthal equidistant projection
C
	      ELSE IF ( mproj .eq. MPAEQU ) THEN
		r = a
C
C*		Orthographic projection
C
	      ELSE IF ( mproj .eq. MPAORT ) THEN
		IF ( a .le. HALFPI ) THEN
		  r = SIN ( a )  
		 ELSE
		  r = 1.0
		END IF
C
C* 		Lambert equal area projection
C
	      ELSE IF  ( mproj .eq. MPALAM )  THEN
		r = 2.0 * sin ( a ) / SQRT ( 2.0 * ( 1.0 + COS ( a ) ) )
C
C*		Gnomic projection
C
	      ELSE IF  ( ( mproj .eq. MPAGNO ) .and.
     +			 ( a .lt. HALFPI ) )  THEN
		r = TAN ( a )
	    END IF
C
C*	    Convert polar coodinates to Cartesian.
C
	    IF  ( ( ERMISS ( gamp ) ) .or. ( ERMISS ( r ) ) )  THEN
		xl (i) = RMISSD
		yl (i) = RMISSD
	      ELSE
		xl (i)  = r * COS ( gamp ) 
		yl (i)  = r * SIN ( gamp )
	    END IF
	  END IF
	END DO
C*
	RETURN
	END
