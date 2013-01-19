	FUNCTION PR_INMM  ( xinch )
C************************************************************************
C* PR_INMM								*
C*									*
C* This function converts inches to millimeters.  The following		*
C* equation is used:							*
C*									*
C*           INMM = XINCH * 25.4					*
C*									*
C* REAL PR_INMM ( XINCH )						*
C*									*
C* Input parameters:							*
C*	XINCH		REAL		Inches		 		*
C*									*
C* Output parameters:							*
C*	PR_INMM		REAL		Millimeters		        *
C**									*
C* Log:									*
C* S. Schotz/GSC	10/89	Original source				*
C* K. Brill/NMC		01/92	Convert inches rather than 1/100 inches *
C* T. Piper/GSC		11/98	Updated prolog				*
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C-----------------------------------------------------------------------
C*	Check for missing value.
C
	IF  ( ERMISS ( xinch ) )  THEN
	    PR_INMM = RMISSD
	  ELSE
C
C*	    Inches to millimeters
C
            PR_INMM = xinch * 25.4
	END IF
C*
	RETURN
	END
