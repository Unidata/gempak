	FUNCTION PR_PANY  ( altm, pmsl )
C************************************************************************
C* PR_PANY								*
C*									*
C* This function returns PMSL if available, otherwise will return ALTM	*
C* if available. The purpose of this routine is to provide greater	*
C* station availability for surface analysis.				*
C*									*
C* Input parameters:							*
C*	ALTM		REAL	 	Altimeter in millibars		*
C*	PMSL		REAL	 	Sea level pressure in millibars	*
C*									*
C* Output parameters:							*
C*	PR_PANY		REAL		PMSL or ALTM, else RMISSD	*
C**									*
C* Log:									*
C* S. Chiswell/Unidata	 12/01		Created				*
C************************************************************************
        INCLUDE    'GEMPRM.PRM'
        INCLUDE    'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for bad data.
C
	IF  ( .not. ERMISS ( pmsl ) )  THEN
	    PR_PANY = pmsl
	    RETURN
	END IF

	IF  ( .not. ERMISS ( altm ) )  THEN
	    PR_PANY = altm
	    RETURN
	END IF
C
	PR_PANY = RMISSD
C
	RETURN
	END
