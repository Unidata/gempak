	SUBROUTINE GCONLM ( mproj, msppj, np, xl, yl, a1, reflon, a3,
     +			    concon, dlat, dlon, iret )
C************************************************************************
C* GCONLM								*
C* 									*
C* This subroutine transforms a point from linear intermediate		*
C* coordinates to latitude-longitude coordinates when conical		*
C* projection has been set up.						*
C*									*
C* GCONLM  ( MPROJ, MSPPJ, NP, XL, YL, A1, REFLON, A3, CONCON,		*
C*           DLAT,  DLON,  IRET )					*
C*									*
C* Input parameters:							*
C*	MPROJ		INTEGER		Map class			*
C*	MSPPJ		INTEGER		Map projection			*
C*	NP		INTEGER		Number of points		*
C*	XL (NP)		REAL		X linear coordinates		*
C*	YL (NP)		REAL		Y linear coordinates		*
C*	A1		REAL		Not used			*
C*	REFLON		REAL		Longitude rotation angle	*
C*	A3		REAL		Not used			*
C*	CONCON		REAL		Cone constant			*
C* 									*
C* Output parameters:							*
C* 	DLAT (NP)	REAL		Latitudes			*
C* 	DLON (NP)	REAL		Longitudes			*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Chatters/RDS	 7/84	GEMPLT Version 3.0			*
C* M. desJardins/GSFC	 6/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 1/88	Fixed missing data			*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* K. Brill/GSC          1/90   Replaced PRNLON with PRNLNR		*
C* K. Brill/NMC		 4/91	Check for ATAN2 ( 0, 0 )		*
C* K. Brill/EMC		 3/96	Changes for general rotation		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
C*
	REAL    	xl (*), yl (*), dlat (*), dlon (*)
C*
	INCLUDE		'ERMISS.FNC'
C-------------------------------------------------------------------------
	iret   = NORMAL
	rconcn = 1. / concon
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
C*
	    rlat = RMISSD
	    rlon = RMISSD
C
C*	    Compute radius of latitude circle.
C
	    r = SQRT ( xl(i)**2 + yl(i)**2 )
C
C*	    Computation depends on pole of projection.
C
	    IF ( mproj .eq. MPCNOR ) THEN
		ra = r ** rconcn
		rlat = 2.0 * ( PI4TH - ATAN ( ra ) ) 
		IF ( yl(i) .ne. 0.0 .or. xl(i) .ne. 0.0 ) THEN
		    al = ATAN2 ( xl(i), -yl(i) )
		    rlon = al * rconcn
		ELSE
		    rlon = 0.0
		END IF
C*
	      ELSE IF ( mproj .eq. MPCSOU )  THEN
		ra = r ** rconcn
		rlat = (-2.0) * ( PI4TH - ATAN ( ra ) )
		IF  ( yl(i) .ne. 0.0 .or. xl(i) .ne. 0.0 ) THEN
		    al = ATAN2 ( xl(i), yl(i) )
		    rlon = al * rconcn
		ELSE
		    rlon = 0.0
		END IF
	    END IF
C
C*	    Convert radians to degrees.
C
	    IF  ( ( ERMISS ( rlat ) ) .or. ( ERMISS ( rlon ) ) )  THEN
		dlat (i) = RMISSD
		dlon (i) = RMISSD
	      ELSE
		dlat (i) = rlat * RTD
		dlon (i) = ( rlon + reflon ) * RTD
	    END IF
	  END IF
	END DO
C*
	RETURN
	END
