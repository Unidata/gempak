	SUBROUTINE VC_SGMA  ( p, ps, ptop, nxy, sgma, iret )
C************************************************************************
C* VC_SGMA								*
C*									*
C* This subroutine computes sigma coordinate values.			*
C*									*
C* VC_SGMA ( P, PS, PTOP, NXY, SGMA,  IRET )				*
C*									*
C* Input parameters:							*
C*	P  (NXY)	REAL		Pressure			*
C*	PS (NXY)	REAL		Surface pressure		*
C*	PTOP		REAL		Top pressure			*
C*	NXY		INTEGER		Number of grid points		*
C*									*
C* Output parameters:							*
C*	SGMA (NXY)	REAL		Output sigma values * 10000	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 +1 = all missing		*
C**									*
C* Log:									*
C* K. Brill/NMC      06/92						*
C* R. Miller/COMET   06/94  Changed 100000 to 10000 for eta and sgma    *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		p (*), ps (*), sgma (*)
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
C
C*	Compute the sigma coordinate values.
C
	icnt = 0
	DO i = 1, nxy
	    IF ( ERMISS ( p (i) ) .or. ERMISS ( ps (i) ) ) THEN
		sgma (i) = RMISSD
		icnt = icnt + 1
	    ELSE
		sgma (i) = ( p (i) - ptop ) / ( ps (i) - ptop )
		sgma (i) = sgma (i) * 10000.
	    END IF
	END DO
C*
	IF ( icnt .eq. nxy ) iret = +1
C*
	RETURN
	END
