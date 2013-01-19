	SUBROUTINE VC_MSHT  ( psym, t, nxy, z, iret )
C************************************************************************
C* VC_MSHT								*
C*									*
C* This subroutine computes height from Montgomery Stream Function	*
C* and temperature.  This code always assumes that Montgomery Stream	*
C* Function is in the standard MKS units (m**2/s**2).			*
C*									*
C* VC_MSHT ( PSYM, T, NXY, Z, IRET )					*
C*									*
C* Input parameters:							*
C*	PSYM (NXY)	REAL		Montgomery stream function	*
C*	T    (NXY)	REAL		Temperature (K)			*
C*	NXY		INTEGER		Number of points		*
C* Output parameters:							*
C*	Z(*)		REAL		Height (m)			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 +1 = all missing		*
C**									*
C* Log:									*
C* K. Brill/NMC      05/92						*
C* K. Brill/NMC	     08/94	Documentation				*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	PARAMETER	( CP = AKAPPA * RDGAS )
C*
	REAL		psym (*), t (*), z (*)
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
C*
	icnt = 0
	DO i = 1, nxy
	    IF ( ERMISS ( psym (i) ) .or. ERMISS ( t (i) ) ) THEN
		z (i) = RMISSD
		icnt = icnt + 1
	    ELSE
	   	z (i) = ( psym (i) - CP * t (i) ) / GRAVTY
	    END IF
	END DO
C*
	IF ( icnt .eq. nxy ) iret = +1
C*
	RETURN
	END
