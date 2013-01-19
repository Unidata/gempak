	FUNCTION PR_TMPK  ( pres, thta )
C************************************************************************
C* PR_TMPK				                                *
C*									*
C* This function computes TMPK from PRES and THTA.  The Poisson		*
C* equation is used:							*
C*									*
C*             TMPK = THTA * ( PRES / 1000 ) ** RKAPPA			*
C*									*
C* REAL PR_TMPK  ( PRES, THTA )						*
C*									*
C* Input parameters:							*
C*	PRES		REAL		Pressure in millibars		*
C*	THTA		REAL    	Potential temperature in K	*
C*									*
C* Output parameters:							*
C*	PR_TMPK		REAL		Temperature in Kelvin		*
C**									*
C* Log:									*
C* T. Roegner/CSC	6/80	Original source code			*
C* M. Goodman/RDS	3/84	Modified prologue and KAPPA		*
C* I. Graffman/RDS	12/87	GEMPAK4					*
C* G. Huffman/GSC	8/88	Documentation; check PRES .lt. 0	*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
	IF  ( ERMISS ( pres ) .or. ERMISS ( thta )
     *       .or. ( pres .lt. 0. ) )  THEN
	    PR_TMPK = RMISSD
	  ELSE
C
C*	    The Poisson equation.
C
	    PR_TMPK = thta * ( pres / 1000. ) ** RKAPPA          
	END IF
C*
	RETURN
	END
