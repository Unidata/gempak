	FUNCTION PR_TMKC  ( tmpk )
C************************************************************************
C* PR_TMKC								*
C*									*
C* This function computes TMPC from TMPK.  The following equation is	*
C* used:								*
C*									*
C*                   TMPC = TMPK - TMCK					*
C*									*
C* REAL PR_TMKC  ( TMPK )						*
C*									*
C* Input parameters:							*
C*	TMPK		REAL		Temperature in Kelvin		*
C*									*
C* Output parameters:							*
C*	PR_TMKC		REAL		Temperature in Celsius		*
C**									*
C* Log:									*
C* I. Graffman/CSC	 8/83	Original source code			*
C* I. Graffman/RDS	12/87	GEMPAK4					*
C* G. Huffman/GSC	 8/88	Modified documentation			*
C* G. Krueger/EAI        4/96   Replaced C->K constant with TMCK	*
C************************************************************************
        INCLUDE    'GEMPRM.PRM'
        INCLUDE    'ERMISS.FNC'
C------------------------------------------------------------------------
	IF  ( ERMISS ( tmpk ) )  THEN
	    PR_TMKC = RMISSD
	  ELSE
	    PR_TMKC = tmpk - TMCK
	END IF
C*
	RETURN
	END
