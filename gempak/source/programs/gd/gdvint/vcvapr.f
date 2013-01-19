	SUBROUTINE VC_VAPR  ( t, nxy, e, iret )
C************************************************************************
C* VC_VAPR								*
C*									*
C* This subroutine computes the saturation vapor pressure corresponding *
C* to a given temperature.						*
C*									*
C* VC_VAPR ( T, NXY, E, IRET )						*
C*									*
C* Input parameters:							*
C*	T  (NXY)	REAL		Temperature (K or C)		*
C*	NXY		INTEGER		Number of points		*
C*									*
C* Output parameters:							*
C*	E  (NXY)	REAL		Vapor pressure (mb)		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 +1 = all missing		*
C**									*
C* Log:									*
C* K. Brill/NMC      05/92						*
C* G. Krueger/EAI        4/96   Replaced C->K constant with TMCK	*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	REAL		t (*), e (*)
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
C*
	icnt = 0
	DO i = 1, nxy
	    IF ( ERMISS ( t (i) ) ) THEN
		e (i) = RMISSD
		icnt = icnt + 1
	    ELSE
	   	IF ( t (i) .gt. 70. ) THEN
C
C*		    Convert to C.
C
		    tmp = t (i) - TMCK 
		ELSE
		    tmp = t (i)
		END IF
	    END IF
	    tmp = 17.67 * tmp / ( tmp + 243.5 )
	    e (i) = 6.112 * EXP ( tmp )
	END DO
C*
	IF ( icnt .eq. nxy ) iret = +1
C*
	RETURN
	END
