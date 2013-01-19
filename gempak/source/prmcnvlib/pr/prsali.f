	FUNCTION PR_SALI  ( alti )
C************************************************************************
C* PR_SALI								*
C*									*
C* This function computes SALI from ALTI.  SALI is an abbreviated	*
C* altimeter code in inches which contains the unit digit and the	*
C* first two digits after the decimal points.  ALTI is multiplied by	*
C* 100 truncated, and the original tens digit dropped.  The following 	*
C* equation is used:							*
C*									*
C*              SALI = NINT ( MOD ( ALTI, 10 ) * 100 )			*
C*									*
C* REAL PR_SALI  ( ALTI )						*
C*									*
C* Input parameters:							*
C*	ALTI		REAL		Altimeter setting in inches	*
C*									*
C* Output parameters:							*
C*	PR_SALI		REAL		Abbreviated standard altimeter	*
C**									*
C* Log:									*
C* J. Nielsen/SUNYA	 8/90						*
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for missing data.
C
	IF  ( ERMISS ( alti ) )  THEN
	    pr_sali = RMISSD
	  ELSE
C
C*	    Drop the leading tens digits.
C
	    aalt = AMOD ( alti, 10. )
C
C*	    Include the tenths digit.
C
	    aalt = aalt * 100.
	    PR_SALI = ANINT ( aalt ) 
	END IF
C*
	RETURN
	END
