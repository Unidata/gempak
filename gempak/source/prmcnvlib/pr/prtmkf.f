	FUNCTION PR_TMKF  ( tmpk )
C************************************************************************
C* PR_TMKF								*
C*									*
C* This function computes TMPF from TMPK.  The following equation is	*
C* used:								*
C*									*
C*             TMPF  =  ( ( TMPK - TMCK ) * 9 / 5 ) + 32		*
C*									*
C* REAL PR_TMKF  ( TMPK )						*
C*									*
C* Input parameters:							*
C*	TMPK		REAL		Temperature in Kelvin		*
C*									*
C* Output parameters:							*
C*	PR_TMKF		REAL		Temperature in Fahrenheit	*
C**									*
C* Log:									*
C* I. Graffman/CSC	 8/83	Original source code			*
C* I. Graffman/RDS	12/84	Modified equation			*
C* M. desJardins/GSFC	 4/86	Added PARAMETER statement		*
C* I. Graffman/RDS	12/87	GEMPAK4					*
C* G. Huffman/GSC	 8/88	Modified documentation			*
C* G. Krueger/EAI        4/96   Replaced C->K constant with PR_TMKC	*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
	PARAMETER	( RPRM = 9.0 / 5.0 )
C*
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
	IF  ( ERMISS ( tmpk ) )  THEN
	    PR_TMKF = RMISSD
	  ELSE
	    PR_TMKF = ( ( PR_TMKC (tmpk) ) * RPRM ) + 32.
	END IF
C*
	RETURN
	END
