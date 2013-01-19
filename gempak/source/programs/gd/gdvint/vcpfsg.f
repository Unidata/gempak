	SUBROUTINE VC_PFSG  ( sigv, ps, ptop, nxy, p, iret )
C************************************************************************
C* VC_PFSG								*
C*									*
C* This subroutine computes pressure values on a sigma surface.		*
C*									*
C* VC_PFSG ( SIGV, PS, PTOP, NXY, P, IRET )				*
C*									*
C* Input parameters:							*
C*	SIGV		REAL		Sigma value			*
C*	PS (NXY)	REAL		Surface pressure		*
C*	PTOP		REAL		Top pressure			*
C*	NXY		INTEGER		Number of grid points		*
C*									*
C* Output parameters:							*
C*	P (NXY)		REAL		Output pressure values		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 +1 = all missing		*
C**									*
C* Log:									*
C* K. Brill/NMC		06/92						*
C* K. Brill/NMC		08/92	Added all missing check			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		ps (*), p (*)
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
C
C*	Compute the pressure values.
C
	icnt = 0
	DO i = 1, nxy
	    IF ( ERMISS ( ps (i) ) ) THEN
		p (i) = RMISSD
		icnt = icnt + 1
	    ELSE
		p (i) = sigv * ( ps (i) - ptop ) + ptop
	    END IF
	END DO
C*
	IF ( icnt .eq. nxy ) iret = +1
C*
	RETURN
	END
