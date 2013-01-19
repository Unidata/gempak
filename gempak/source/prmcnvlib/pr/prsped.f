	FUNCTION PR_SPED ( uwnd, vwnd )
C************************************************************************
C* PR_SPED 								*
C*									*
C* This function computes SPED from UWND and VWND.  The following	*
C* equation is used:				 			*
C* 									*
C*           SPED = SQRT ( (UWND**2) + (VWND**2) )			*
C*									*
C* This function computes SKNT if UKNT and VKNT are input.		*
C*									*
C* REAL PR_SPED  ( UWND, VWND )						*
C*									*
C* Input parameters:							*
C*	UWND		REAL		U component of velocity		*
C*	VWND		REAL		V component of velocity		*
C*									*
C* Output parameters:							*
C*	PR_SPED		REAL		Wind speed 			*
C**									*
C* Log:									*
C* P. Kocin/914		1980	Original source code			*
C* I. Graffman/RDS	12/87	GEMPAK4					*
C* G. Huffman/GSC	 7/88	Documentation				*
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for missing data.
C
	IF  ( ERMISS ( uwnd ) .or. ERMISS ( vwnd ) )  THEN
	    PR_SPED = RMISSD
	  ELSE
	    PR_SPED = SQRT ( ( uwnd **2 ) + ( vwnd **2 ) )
	END IF
C*
	RETURN
	END
