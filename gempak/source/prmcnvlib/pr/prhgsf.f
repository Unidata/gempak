	FUNCTION PR_HGSF  ( hgml )
C************************************************************************
C* PR_HGSF								*
C*									*
C* This function computes HGFT from HGML.  The following equation is	*
C* used:								*
C*									*
C*               HGFT = HGML * 5280	    				*
C*									*
C* REAL PR_HGSF  ( HGML )						*
C*									*
C* Input parameters:							*
C*	HGML		REAL	 	Height in statute miles 	*
C*									*
C* Output parameters:							*
C*	PR_HGSF		REAL		Height in feet          	*
C**									*
C* Log:									*
C* S. Schotz    	10/89	Original source				*
C* J. Whistler/SSAI	 7/91	Changed hght to hgml in ERMISS check	*
C************************************************************************
        INCLUDE    'GEMPRM.PRM'
        INCLUDE    'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for missing values.
C	
	IF  ( ERMISS ( hgml ) )  THEN
	    PR_HGSF = RMISSD
	  ELSE
	    PR_HGSF = hgml * 5280.
	END IF
C*
	RETURN
	END
