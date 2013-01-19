	SUBROUTINE VC_THTA  ( p, t, nxy, thta, iret )
C************************************************************************
C* VC_THTA								*
C*									*
C* This subroutine computes theta coordinate values.			*
C*									*
C* VC_THTA ( P, T, NXY, THTA,  IRET )					*
C*									*
C* Input parameters:							*
C*	P  (NXY)	REAL		Pressure			*
C*	T  (NXY)	REAL		Temperature			*
C*	NXY		INTEGER		Number of grid points		*
C*									*
C* Output parameters:							*
C*	THTA (NXY)	REAL		Output theta values		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 +1 = all missing		*
C**									*
C* Log:									*
C* K. Brill/NMC      06/92						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		p (*), t (*), thta (*)
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
C
C*	Compute the theta coordinate values.
C
	icnt = 0
	DO i = 1, nxy
	    IF ( ERMISS ( p (i) ) .or. ERMISS ( t (i) ) ) THEN
		thta (i) = RMISSD
		icnt = icnt + 1
	    ELSE
		thta (i) = t (i) * ( 1000. / p (i) ) ** RKAPPA
	    END IF
	END DO
C*
	IF ( icnt .eq. nxy ) iret = +1
C*
	RETURN
	END
