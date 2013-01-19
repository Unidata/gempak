	FUNCTION PR_MHKN  ( smph )
C************************************************************************
C* PR_MHKN								*
C*									*
C* This function computes SKNT from SMPH.  The following equation is	*
C* used:								*
C*									*
C*      SKNT = SMPH * 0.868976						*
C*									*
C* REAL PR_MHKN  ( SMPH )						*
C*									*
C* Input parameters:							*
C*	SMPH		REAL		Speed in miles/hour		*
C*									*
C* Output parameters:							*
C*	PR_MHKN		REAL		Speed in knots			*
C**									*
C* T. Lee/SAIC		 9/01						*
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
C*      Check for missing data.
C
	IF  ( ERMISS ( smph ) )  THEN
	    PR_MHKN = RMISSD
	  ELSE
	    PR_MHKN = smph * 0.868976
	END IF
C*
	RETURN
	END
