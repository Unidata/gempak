	FUNCTION PR_TMFC  ( tmpf )
C************************************************************************
C* PR_TMFC								*
C*									*
C* This function computes TMPC from TMPF.  The following equation is	*
C* used:			 					*
C*									*
C*                TMPC = ( TMPF - 32 ) * 5 / 9				*
C*									*
C* REAL PR_TMFC  ( TMPF )						*
C*									*
C* Input parameters:							*
C*	TMPF		REAL		Temperature in Fahrenheit	*
C*									*
C* Output parameters:							*
C*	PR_TMFC		REAL		Temperature in Celsius		*
C**									*
C* Log:									*
C* J. Woytek/GSFC	10/82	Original source code			*
C* M. desJardins/GSFC	 4/86	Added PARAMETER statement		*
C* I. Graffman/RDS	12/87	GEMPAK4					*
C* G. Huffman/GSC	 8/88	Modified documentation			*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
	PARAMETER	( RPRM = 5. / 9. )
C*
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
	IF  ( ERMISS ( tmpf ) )  THEN
	    PR_TMFC = RMISSD
	  ELSE
	    PR_TMFC = ( tmpf - 32.0 ) * RPRM
	END IF
C*
	RETURN
	END
