	FUNCTION PR_QUOT ( x, y )
C************************************************************************
C* PR_QUOT								*
C*									*
C* This function computes the quotient (x/y) for parameter ratios.	*
C* The following equation is used:					*
C*									*
C*               PR_QUOT = X / Y					*
C*									*
C*  REAL PR_QUOT  ( X, Y )    						*
C*									*
C* Input parameters:							*
C*	X		REAL    	Numerator			*
C*	Y		REAL		Denominator			*
C*									*
C* Output parameters:							*
C*	PR_QUOT		REAL		Ratio				*
C**									*
C* Log:									*
C* K. Brill/HPC		 9/01	For computing QPF / Watch Threshold	*
C************************************************************************
        INCLUDE  'GEMPRM.PRM'
        INCLUDE  'ERMISS.FNC'
C------------------------------------------------------------------------
	IF  ( ERMISS (x) .or. ERMISS (y) .or. ( y .eq. 0.0 ) )  THEN
	    PR_QUOT = RMISSD
	  ELSE
	    PR_QUOT = x / y
	END IF
C*
	RETURN
	END
