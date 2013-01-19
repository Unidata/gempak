	FUNCTION PR_TMFK  ( tmpf )
C************************************************************************
C* PR_TMFK								*
C*									*
C* This function computes TMPK from TMPF.  The following equation is	*
C* used:								*
C*									*
C*           TMPK = ( TMPF - 32 ) * 5 / 9 + TMCK			*
C*									*
C* REAL PR_TMFK  ( TMPF )						*
C*									*
C* Input parameters:							*
C*	TMPF		REAL		Temperature in Fahrenheit	*
C*									*
C* Output parameters:							*
C*	PR_TMFK		REAL		Temperature in Kelvin		*
C**									*
C* Log:									*
C* M. Goodman/RDS	 8/84	Original source				*
C* I. Graffman/RDS	12/84	Modified equation			*
C* M. desJardins/GSFC	 4/86	Added PARAMETER statement		*
C* I. Graffman/RDS	12/87	GEMPAK4					*
C* G. Huffman/GSC	 8/88	Modified documentation			*
C* G. Krueger/EAI        4/96   Replaced C->K constant with TMCK	*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
	IF  ( ERMISS ( tmpf ) )  THEN
	    PR_TMFK = RMISSD
	  ELSE
	    tmpc = PR_TMFC ( tmpf )
	    PR_TMFK = PR_TMCK ( tmpc )
	END IF
C*
	RETURN
	END
