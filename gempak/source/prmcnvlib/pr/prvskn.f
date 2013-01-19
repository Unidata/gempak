	FUNCTION PR_VSKN  ( vsbk )
C************************************************************************
C* PR_VSKN								*
C*									*
C* This function computes VSBN from VSBK.  The following equation is	*
C* used:  								*
C*									*
C*                VSBN  =  .54 * VSBK	                 		*
C*									*
C* REAL PR_VSKN  ( VSBK )						*
C*									*
C* Input parameters:							*
C*	VSBK		REAL		Visibility in kilometers        *
C*									*
C* Output parameters:							*
C*	PR_VSKN		REAL		Visibility in nautical miles    *
C**									*
C* Log:									*
C* D. Kidwell/NCEP	5/97	                      			*
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for invalid data.
C
	IF  ( ERMISS ( vsbk ) )  THEN
	    PR_VSKN = RMISSD
	  ELSE
C
C*	    Do the conversion.
C
	    PR_VSKN = .54 * vsbk
	END IF
C*
	RETURN
	END
