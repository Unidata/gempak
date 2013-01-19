	FUNCTION PR_TMCK  ( tmpc )
C************************************************************************
C* PR_TMCK								*
C*									*
C* This function computes TMPK from TMPC.  The following equation is	*
C* used:								*
C*									*
C*                   TMPK = TMPC + TMCK					*
C*									*
C* REAL PR_TMCK  ( TMPC )						*
C*									*
C* Input parameters:							*
C*	TMPC		REAL		Temperature in Celsius		*
C*									*
C* Output parameters:							*
C*	PR_TMCK		REAL		Temperature in Kelvin		*
C**									*
C* Log:									*
C* M. Goodman/RDS	 3/84	Original source code			*
C* G. Huffman/GSC	 8/88	Modified documentation			*
C* G. Krueger/EAI        4/96   Replaced C->K constant with TMCK	*
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
	IF  ( ERMISS ( tmpc ) )  THEN
	    PR_TMCK = RMISSD
	  ELSE
	    PR_TMCK = tmpc + TMCK
	END IF
C*
	RETURN
	END
