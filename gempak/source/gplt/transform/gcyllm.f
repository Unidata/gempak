	SUBROUTINE GCYLLM ( mproj, msppj, np, xl, yl, polat, polon,
     +			    rotat, sclmcd, swchxy, dlat, dlon, iret )
C************************************************************************
C* GCYLLM								*
C* 									*
C* This subroutine converts a location from linear intermediate		*
C* coordinates to latitude-longitude coordinates for cylindrical	*
C* projections.								*
C*									*
C* Note that the only case implemented is the case with the u-axis at	*
C* the equator.  The general case is achieved via coordinate rotation.  *
C* Therefore, this routine remains simple.				*
C*									*
C* SWCHXY is a flag for a 90 degree rotation of the linear coordinates  *
C* to develop a transverse projection with the y-axis oriented south to	*
C* north.								*
C*									*
C* GCYLLM  ( MPROJ,MSPPJ, NP, XL, YL, POLAT, POLON, ROTAT, SCLMCD,	*
C*           SWCHXY, DLAT, DLON, IRET )					*
C* 									*
C* Input parameters:							*
C*	MPROJ		INTEGER		Projection class		*
C*	MSPPJ		INTEGER		Projection type			*
C*	NP		INTEGER		Number of points		*
C* 	XL (NP)		REAL		X coordinates			*
C* 	YL (NP)		REAL		Y-coordinates			*
C*	POLAT		REAL		Not used			*
C*	POLON		REAL		Central longitude 		*
C*	ROTAT		REAL		Not used			*
C*	SCLMCD		REAL		Scaling for MCD			*
C*	SWCHXY		LOGICAL		Flag to rotate x,y		*
C* 									*
C* Output parameters:							*
C* 	DLAT (NP)	REAL		Latitudes			*
C* 	DLON (NP)	REAL		Longitudes			*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Chatters/RDS	 7/84	GEMPLT Version 3.0			*
C* M. desJardins/GSFC	 6/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 1/88	Fixed CED rounding at pole		*
C* M. desJardins/GSFC	 1/88	Fixed missing data			*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
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
        hlfpin = -HALFPI - EPS
        hlfpip = HALFPI + EPS
C
C*	Loop through all the points.
C
	DO  i = 1, np
C
C*	  Check for missing data.
C
	  IF  ( ( ERMISS ( xl (i) ) ) .or. ( ERMISS ( yl (i) ) ) )  THEN
	    dlat (i) = RMISSD
	    dlon (i) = RMISSD
	  ELSE
C
C*	    Rotate x,y if necessary.
C
	    IF ( swchxy ) THEN
		xx = yl (i)
		yy = -xl (i)
	    ELSE
		xx = xl (i)
		yy = yl (i)
	    END IF
C*
	    rlat = RMISSD
	    rlon = xx
C
C*	    Compute internal projection angle.
C
C*	    Cylindrical equidistant projection.
C
	    IF ( mproj .eq. MPCEQU ) THEN
		IF  ( ( yy .ge. hlfpin ) .and. 
     +		      ( yy .le. hlfpip ) )  THEN
		    rlat = yy
		END IF
C
C*	    Mercator projection.
C
	      ELSE IF  ( mproj .eq. MPCMER ) THEN
		rlat = HALFPI - ACOS ( TANH ( yy ) )
C
C*	    Modified cylindrical equidistant.
C
	      ELSE IF  ( mproj .eq. MPCMCD )  THEN
		IF  ( ( yy .ge. hlfpin ) .and. 
     +		      ( yy .le. hlfpip ) )  THEN
		    rlat = yy / sclmcd
		END IF
	    END IF
C
C*	    Convert from radians to degrees.
C
	    IF  ( .not. ERMISS ( rlat ) )  THEN
		dlat (i) = rlat * RTD
		dlon (i) = ( rlon + polon ) * RTD
	      ELSE
		dlat (i) = RMISSD
		dlon (i) = RMISSD
	    END IF
	  END IF
	END DO
C*
	RETURN
	END
