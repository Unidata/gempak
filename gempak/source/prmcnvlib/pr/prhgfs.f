	FUNCTION PR_HGFS  ( hgft )
C************************************************************************
C* PR_HGFS								*	
C*									*
C* This function computes HGML, height in miles, from HGFT.  The 	*
C* following equation is used:						*
C*									*
C*               HGML = HGFT / 5280.    				*
C*									*
C* REAL PR_HGFS  ( HGFT )						*
C*									*
C* Input parameters:							*
C*	HGFT		REAL	 	Height in feet          	*
C*									*
C* Output parameters:							*
C*	PR_HGFS		REAL		Height in statute miles 	*
C**									*
C* Log:									*
C* S. Schotz    	10/89	Original source				*
C************************************************************************
        INCLUDE    'GEMPRM.PRM'
C
	REAL       ft2ml
	PARAMETER  (ft2ml = 1.0 / 5280.0)
C
        INCLUDE    'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for missing values.
C	
	IF  ( ERMISS ( hgft ) )  THEN
	    PR_HGFS = RMISSD
	  ELSE
	    PR_HGFS = hgft * ft2ml
	END IF
C*
	RETURN
	END
