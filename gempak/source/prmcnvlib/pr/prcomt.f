	FUNCTION PR_COMT ( coml, comm, comh )
C************************************************************************
C* PR_COMT								*
C*									*
C* This function computes COMT from COML,COMM and COMH.			*
C*									*
C* REAL PR_COMT  ( COML, COMM, COMH )					*
C*									*
C* Input parameters:							*
C*	COML		REAL	 	Low report height & coverage	*
C*	COMM		REAL		Mid report height & coverage	*
C*	COMH		REAL		High report height & coverage	*
C*									*
C* Output parameters:							*
C*	PR_COMT		REAL		Highest combined height & 	*
C*					coverage 			*
C**									*
C* Log:									*
C* S. Schotz    	10/89	Original source				*
C************************************************************************
        INCLUDE    'GEMPRM.PRM'
C------------------------------------------------------------------------
C*	Decode coverage values for each level
C
	clcl = INT (coml) - (INT (coml) / 10 * 10)
     	clcm = INT (comm) - (INT (comm) / 10 * 10)
	clch = INT (comh) - (INT (comh) / 10 * 10)
C
C*      Get maximum coverage value.
C
	clct = PR_CLCT  ( clcl, clcm, clch )
C
C*      Find maximum combined value.
C
	IF  ( clct .eq. clcl )  THEN
	    PR_COMT = coml
	ELSE IF  ( clct .eq. clcm )  THEN
 	    PR_COMT = comm
	ELSE
	    PR_COMT = comh	
	END IF
C*
	RETURN
	END
