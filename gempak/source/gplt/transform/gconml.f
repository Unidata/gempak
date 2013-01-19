	SUBROUTINE GCONML ( mproj, msppj, np, dlat, dlon, a1, reflon, 
     +			    a3, concon, xl, yl, iret)
C************************************************************************
C* GCONML								*
C* 									*
C* This subroutine transforms a point from latitude-longitude		*
C* coordinates to linear intermediate coordinates for conical		*
C* projections. This is conic with two standard	parallels.		*
C* 									*
C* GCONML  ( MPROJ, MSPPJ, NP, DLAT, DLON, A1, REFLON, A3, CONCON,	*
C*           XL,    YL,    IRET )					*
C*									*
C* Input parameters:							*
C*	MPROJ		INTEGER		Map class			*
C*	MSPPJ		INTEGER		Map projection			*
C*	NP		INTEGER		Number of points		*
C* 	DLAT (NP)	REAL		Latitudes			*
C* 	DLON (NP)	REAL		Longitudes			*
C*	A1		REAL		Not used			*
C*	REFLON		REAL		Central longitude		*
C*	A3		REAL		Not used			*
C*	CONCON		REAL		Cone constant			*
C* 									*
C* Output parameters:							*
C*	XL (NP)		REAL		X linear coordinates		*
C*	YL (NP)		REAL		Y linear coordinates		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Chatters/RDS	 1/84	GEMPLT Version 3.0			*
C* M. desJardins/GSFC	 6/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 1/88	Fixed missing data			*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* K. Brill/EMC		 3/96	Changes for general rotation		*
C* K. Brill/EMC		 5/96	Check for latitude out of range		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
C*
	REAL    	xl (*), yl (*), dlat (*), dlon (*)
C*
	INCLUDE		'ERMISS.FNC'
C-------------------------------------------------------------------------
	iret = NORMAL
C
C*	Loop through all points.
C
	plon = reflon * RTD
	DO i = 1, np
	    IF ( .not. ERMISS ( dlon (i) ) ) THEN
		yl (i) = dlon (i) - plon
	    ELSE
		yl (i) = RMISSD
	    END IF
	END DO
	CALL PRNLON ( np, yl, ier )
C*
	DO  i = 1, np
C
C*	  Check for missing data.
C
	  IF  ( ( .not. ERMISS ( dlat (i) ) ) .and. 
     +		( .not. ERMISS ( yl (i) ) ) .and. .not.
     +          ( dlat (i) .gt. 90.0 .or. dlat (i) .lt. -90.0 ) 
     +        )  THEN
	    rlat   = dlat (i) * DTR
	    rlon   = yl (i) * DTR
	    xl (i) = RMISSD
	    yl (i) = RMISSD
C
C*	   Computation depends on pole of projection.
C
	   IF ( mproj .eq. MPCNOR ) THEN
		ang1 = PI4TH - 0.5 * rlat
C
		IF ( ang1 .lt. HALFPI ) THEN
		    r       = TAN ( ang1 ) ** concon
		    ang2    = concon * ( rlon )
		    xl (i)  =  r * SIN ( ang2 ) 
		    yl (i)  = (-r) * COS ( ang2 )
		END IF
C*
	      ELSE IF ( mproj .eq. MPCSOU ) THEN
		ang1 = PI4TH + 0.5 * rlat 
C
		IF ( ang1 .lT. HALFPI ) THEN
		    r    = TAN ( ang1 ) ** concon
		    ang2 = concon * ( rlon )
		    xl (i)  = r * SIN ( ang2 )
		    yl (i)  = r * COS ( ang2 )
		END IF
	    END IF
	  ELSE
	    xl (i) = RMISSD
	    yl (i) = RMISSD
	  END IF
	END DO
C*
	RETURN
	END
