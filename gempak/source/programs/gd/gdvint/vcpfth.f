	SUBROUTINE VC_PFTH  ( theta, tmp, nxy, p, iret )
C************************************************************************
C* VC_PFTH								*
C*									*
C* This subroutine computes pressure on an isentropic surface given	*
C* temperature.								*
C*									*
C* VC_PFTH ( THETA, TMP, NXY, P, IRET )					*
C*									*
C* Input parameters:							*
C*	THETA		REAL		Potential temperature		*
C*	TMP  (NXY)	REAL		Temperature			*
C*	NXY		INTEGER		Number of points		*
C*									*
C* Output parameters:							*
C*	P    (NXY)	REAL		Output pressure values		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 +1 = all missing		*
C**									*
C* Log:									*
C* K. Brill/NMC      06/92						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		tmp (*), p (*)
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
C
C*	Compute the theta coordinate values.
C
	icnt = 0
	DO i = 1, nxy
	    IF ( ERMISS ( tmp (i) ) .or. ERMISS ( theta ) ) THEN
		p (i) = RMISSD
		icnt = icnt + 1
	    ELSE
		p (i) = 1000. * ( tmp (i) / theta ) ** 3.5
	    END IF
	END DO
C*
	IF ( icnt .eq. nxy ) iret = +1
C*
	RETURN
	END
