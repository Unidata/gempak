	FUNCTION PR_D100  ( hvalue )
C************************************************************************
C* PR_D100								*
C*									*
C* This function divides a value by 100.				*
C*									*
C* REAL PR_D100  ( HVALUE )						*
C*									*
C* Input parameters:							*
C*	HVALUE		REAL		Value				*
C*									*
C* Output parameters:							*
C*	PR_D100		REAL		Value / 100			*
C**									*
C* Log:									*
C* S. Schotz/GSC	10/89	Original source				*
C* T. Piper/GSC		11/98	Updated prolog				*
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C-----------------------------------------------------------------------
C*	Check for missing value.
C
	IF  ( ERMISS ( hvalue ) )  THEN
	    PR_D100= RMISSD
	  ELSE
	    PR_D100 = hvalue / 100.
	END IF
C*
	RETURN
	END
