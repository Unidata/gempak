	FUNCTION PR_M100  ( value )
C************************************************************************
C* PR_M100								*
C*									*
C* This function multiplies a value by 100.				*
C*									*
C* REAL PR_M100  ( VALUE )						*
C*									*
C* Input parameters:							*
C*	VALUE		REAL	 	Value				*
C*									*
C* Output parameters:							*
C*	PR_M100		REAL		Value * 100			*
C**									*
C* Log:									*
C* S. Schotz/GSC	10/89	Original source				*
C************************************************************************
        INCLUDE    'GEMPRM.PRM'
        INCLUDE    'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for missing values.
C*	
	IF  ( ERMISS ( value ) )  THEN
	    PR_M100 = RMISSD
	  ELSE
	    PR_M100 = value * 100.
	END IF
C*
	RETURN
	END
