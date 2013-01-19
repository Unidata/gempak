	FUNCTION PR_HMTR ( tmpf, dwpf )
C************************************************************************
C* PR_HMTR								*
C*									*
C* This function computes HMTR, the humiture index, from TMPF and DWPF, *
C* The output will be calculated in Fahrenheit  The following equation 	*
C* is used:						                *
C*									*
C*		PR_HMTR = TMPF + ( PR_VAPR ( DWPC ) - 21 )		*
C*									*
C* REAL PR_HMTR  ( TMPF, DWPF ) 					*
C*									*
C* Input parameters:							*
C*	TMPF		REAL    	Air temperature 		*
C*	DWPF		REAL		Dewpoint temperature		*
C*									*
C* Output parameters:							*
C*	PR_HMTR		REAL		Humiture Index			*
C**									*
C* Log:									*
C* M. Nelson		 7/92		From Winterling,		*
C*					  BAMS Apr 1979, p 329		*
C* S. Jacobs/EAI	 3/93		Cleaned up			*
C************************************************************************
        INCLUDE  'GEMPRM.PRM'
        INCLUDE  'ERMISS.FNC'
C------------------------------------------------------------------------
	IF  ( ( ERMISS (tmpf) ) .or. ( ERMISS (dwpf) ) )  THEN
	    PR_HMTR = RMISSD
	ELSE
	    dwpc    = PR_TMFC ( dwpf )
	    PR_HMTR = tmpf + ( PR_VAPR ( dwpc ) - 21 )
	END IF
C*
	RETURN
	END
