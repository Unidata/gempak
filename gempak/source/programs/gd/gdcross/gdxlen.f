	SUBROUTINE GDXLEN ( np, rlat, rlon, rlen, iret )
C************************************************************************
C* GDXLEN								*
C*									*
C* This subroutine computes the length of a line joining the input	*
C* points.								*
C*									*
C* GDXLEN  ( NP, RLAT, RLON, RLEN, IRET )				*
C*									*
C* Input parameters:							*
C*									*
C*	NP           INTEGER	Number of points			*
C*      RLAT (NP)    REAL	Array of latitudes of the points	*
C*	RLON (NP)    REAL	Array of longitudes of the points	*
C*									*
C* Output parameters:							*
C*	RLEN	     REAL	Length (m) of the line segment		*
C*	IRET	     INTEGER	Return code				*
C*				  0 = normal return			*
C*			         -9 = no points				*
C**									*
C* Log:									*
C* K. F. Brill/GSC       7/89						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		rlat ( np ), rlon ( np )
C------------------------------------------------------------------------
	iret = 0
C*
	IF ( np .le. 1 ) THEN
	  iret = -9
	  RETURN
	END IF
C*
	rlen = 0.0
	DO k = 2, np
	  avgphi = .5 * ( rlat ( k-1 ) + rlat ( k ) ) * DTR
	  cosphi = COS ( avgphi )
	  dlam = ( rlon ( k ) - rlon ( k-1 ) ) * cosphi
	  dphi = rlat ( k ) - rlat ( k-1 )
	  dd = sqrt ( dlam * dlam + dphi * dphi )
	  rlen = rlen + dd
	END DO
C*
	rlen = rlen * RADIUS * DTR
C*
	RETURN
	END
