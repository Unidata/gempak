	SUBROUTINE GAZMLM ( mproj, msppj, np, xl, yl, polat, polon,
     +			    rotat, azmsav, dlat, dlon, iret )
C************************************************************************
C* GAZMLM								*
C* 									*
C* This subroutine converts a point from linear intermediate		*
C* coordinates to latitude-longitude coordinates for azimuthal map	*
C* projections.								*
C* 									*
C* GAZMLM  ( MPROJ, MSPPJ, NP, XL, YL, POLAT, POLON, ROTAT, AZMSAV,	*
C*           DLAT,  DLON,  IRET )					*
C*									*
C* Input parameters:							*
C*	MPROJ		INTEGER		Map class			*
C*	MSPPJ		INTEGER		Map projection			*
C*	NP		INTEGER		Number of points		*
C*	XL (NP)		REAL		X linear coordinates		*
C*	YL (NP)		REAL		Y linear coordinates		*
C*	POLAT		REAL		Pole latitude			*
C*	POLON		REAL		Central longitude		*
C*	ROTAT		REAL		Rotation (not used)		*
C*	AZMSAV		REAL		Special azimuthal angle (unused)*
C* 									*
C* Output parameters:							*
C* 	DLAT (NP)	REAL		Latitudes			*
C* 	DLON (NP)	REAL		Longitudes			*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Chatters/RDS	 7/84	GEMPLT Version 3.0			*
C* M. desJardins/GSFC	 6/85	GEMPLT Version 3.1			*
C* B. Doty/RDS		 5/87	Corrected ATAN2 zero check		*
C* B. Doty/RDS		 7/87	Fixed Orthographic out-of-hemisphere	*
C* M. desJardins/GSFC	 1/88	Fixed error checking			*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* K. Brill/NMC 	 4/91   Chngd .and. to .or. in chk for ATAN2	*
C* A. Taylor/ARL        12/93   Enabled Oblique Azimuthal Projections   *
C* K. Brill/NMC          1/94   Cleanup                                 *
C* K. Brill/EMC		 3/96	Remove Taylor code; general rotated proj*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		xl (*), yl (*), dlat (*), dlon (*)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Loop through all points.
C
	DO  i = 1, np
C
C*	  Check that input values are not missing.
C
	  IF  (( ERMISS ( xl (i) ) ) .or. ( ERMISS ( yl (i) ) ))  THEN
	    dlat (i) = RMISSD
	    dlon (i) = RMISSD
	  ELSE
	    gamp = RMISSD
	    a    = RMISSD
	    rlat = RMISSD
	    rlon = RMISSD
C
C*	    Compute polar coordinate componenets.
C
            IF ( ( xl(i) .ne. 0.0 ) .or. ( yl(i) .ne. 0.0 ) ) THEN
	        gamp = ATAN2 ( yl (i), xl (i) )
            ELSE
                gamp = 0.0
            ENDIF
	    r  =  SQRT  ( xl (i) ** 2 + yl (i) ** 2 )
C
C*	    Select range computation based on projection type.
C
C*	    Stereographic projection.
C
	    IF  ( mproj .eq. MPASTR ) THEN
		a = 2.0 * ATAN ( r )
C
C*	      Azimuthal equidistant projection.
C
	      ELSE IF ( ( mproj .eq. MPAEQU ) .and. ( r .le. PI ) ) THEN
		a = r
C                
C*	      Orthographic projection.
C
	      ELSE IF  ( mproj .eq. MPAORT ) THEN
                IF ( r .le. 1.0 ) THEN 
	            a = ASIN ( r )
                  ELSE
                    a = ASIN ( 1.0 ) 
                ENDIF
C
C*	      Lambert equal area projection.
C
	      ELSE IF  ( ( mproj .eq. MPALAM ) .and.
     +                  ( r .le. 2.0 ) ) THEN
		a = ACOS ( 1.0 - 0.5 * r**2 )
C
C*	      Gnomic projection.
C
	      ELSE IF ( mproj .eq. MPAGNO ) THEN
		a = ATAN ( r )
	    END IF
C
C*	    Do computations for special cases first.
C*	    General case is not implemented.
C
	    IF ((.not. ERMISS (gamp)) .and. (.not. ERMISS (a)))  THEN
C
	      IF  ( msppj .eq. MSANOR )  THEN
		rlon = gamp
		rlat = HALFPI - a 
C*
	       ELSE IF ( msppj .eq. MSASOU ) THEN
		rlon = - gamp
		rlat = a - HALFPI
	      END IF                                               
	    END IF
C
C*	    Convert to degrees.
C
	    IF ((.not. ERMISS (rlon)) .and. (.not. ERMISS (rlat))) THEN
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
