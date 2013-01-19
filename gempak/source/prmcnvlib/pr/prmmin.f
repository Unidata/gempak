	FUNCTION PR_MMIN  ( xmilm )
C************************************************************************
C* PR_MMIN								*
C*									*
C* This function converts millimeters to inches.  The following		*
C* equation is used:							*
C*									*
C*       MMIN  =  .0393701 * XMILM					*
C*									*
C* REAL PR_MMIN  ( XMILM )						*
C*									*
C* Input parameters:							*
C*	XMILM		REAL		Millimeters			*
C*									*
C* Output parameters:							*
C*	PR_MMIN		REAL		Inches				*
C**									*
C* Log:									*
C* S. Schotz/GSC	10/89	Original source				*
C* M. desJardins/GSFC	 8/90	Combine multiplication & division	*
C* K. Brill/NMC		01/92	Convert to inches not 1/100 inches	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
C*	Check for missing value.
C
	IF  ( ERMISS ( xmilm ) )  THEN
	    PR_MMIN = RMISSD
	  ELSE
	    PR_MMIN = .0393701 * xmilm
	END IF
C*
	RETURN
	END
