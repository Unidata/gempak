	SUBROUTINE BFRACH ( rltln, slat, slon, ok, iret )
C************************************************************************
C* BFRACH								*
C*									*
C* This subroutine checks for a station within lat/lon bounds.		*
C*									*
C* Station longitude will be corrected to be within -180 to +180 range. *
C*									*
C* BFRACH  ( RLTLN, SLAT, SLON, OK, IRET )				*
C*									*
C* Input parameters:							*
C*	RLTLN (4)	REAL		LL and UR lat/lon values	*
C*	SLAT		REAL		Station latitude		*
C*									*
C* Input and output parameters:						*
C*	SLON		REAL		Station longitude		*
C*									*
C* Output parameters:							*
C*	OK		LOGICAL		Flag (true if station is inside)*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C**									*
C* Log:									*
C* K. Brill/EMC		 2/97						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		rltln (4)
	LOGICAL		ok
C*
C-----------------------------------------------------------------------
	iret = 0
C*
	IF ( slon .lt. -180. ) THEN
	    slon = 360. + slon
	ELSE IF ( slon .gt. 180. ) THEN
	    slon = slon - 360.
	END IF
	ok = ( slat .ge. rltln (1) .and.
     +	       slat .le. rltln (3) .and.
     +	       slon .ge. rltln (2) .and.
     +	       slon .le. rltln (4) ) 
C*
	RETURN
	END
