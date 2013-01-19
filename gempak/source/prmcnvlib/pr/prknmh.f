	FUNCTION PR_KNMH  ( sknt )
C************************************************************************
C* PR_KNMH								*
C*									*
C* This function computes SMPH from SKNT.  The following equation is	*
C* used:								*
C*									*
C*               SMPH = SKNT / 0.868976					*
C*									*
C* REAL PR_KNMH  ( SKNT )						*
C*									*
C* Input parameters:							*
C*	SKNT		REAL		Speed in knots			*
C*									*
C* Output parameters:							*
C*	PR_KNMH		REAL		Speed in miles/hour		*
C**									*
C* A. Hardy/GSC		 8/99		Copied from PR_KNMS		*
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
C*      Check for missing data.
C
	IF  ( ERMISS ( sknt ) )  THEN
	    PR_KNMH = RMISSD
	  ELSE
	    PR_KNMH = sknt / 0.868976
	END IF
C*
	RETURN
	END
