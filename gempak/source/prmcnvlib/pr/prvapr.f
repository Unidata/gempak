	FUNCTION PR_VAPR  ( dwpc )
C************************************************************************
C* PR_VAPR								*
C*									*
C* This function computes VAPR from DWPC.  The following equation is	*
C* used:								*
C*									*
C*     VAPR = 6.112 * EXP [ (17.67 * DWPC) / (DWPC + 243.5) ]		*
C*									*
C* Bolton.								*
C*									*
C* This function will compute VAPS if TMPC is input.			*
C*									*
C* REAL PR_VAPR  ( DWPC )						*
C*									*
C* Input parameters:							*
C*	DWPC		REAL		Dewpoint in Celsius		*
C*									*
C* Output parameters:							*
C*	PR_VAPR		REAL		Vapor pressure in millibars	*
C**									*
C* Log:									*
C* P. Kocin/914		1980						*
C* M. Goodman/RDS	 4/84	Updated prologue and added PR_TMCK	*
C* I. Graffman/RDS	11/84	Updated to use Bolton eqn.		*
C* G. Huffman/GSC	 8/88	Documentation; test for low DWPC	*
C************************************************************************
        INCLUDE    'GEMPRM.PRM'
        INCLUDE    'ERMISS.FNC'
C------------------------------------------------------------------------
	IF  ( ERMISS ( dwpc ) .or. ( dwpc .lt. -240. ) )  THEN
	    PR_VAPR = RMISSD
	  ELSE
	    PR_VAPR = 6.112 * EXP (( 17.67 * dwpc ) / ( dwpc + 243.5 ))
	END IF
C*
	RETURN
	END
