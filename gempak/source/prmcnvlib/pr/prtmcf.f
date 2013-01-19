	FUNCTION PR_TMCF  ( tmpc )
C************************************************************************
C* PR_TMCF								*
C*									*
C* This function computes TMPF from TMPC.  The following equation is    *
C* used:								*
C*									*
C*              TMPF = ( TMPC * 9 / 5 ) + 32				*
C*									*
C* REAL PR_TMCF  ( TMPC )						*
C*									*
C* Input parameters:							*
C*	TMPC		REAL    	Temperature in Celsius		*
C*									*
C* Output parameters:							*
C*	PR_TMCF		REAL    	Temperature in Fahrenheit	*
C**									*
C* Log:									*
C* J. Woytek/GSFC	10/82	Original source code			*
C* M. Goodman/RDS	 9/84	Added error branch			*
C* M. desJardins/GSFC	 4/86	Added PARAMETER statement		*
C* I. Graffman/RDS	12/87	GEMPAK4					*
C* G. Huffman/GSC	 8/88	Modified documentation			*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
	PARAMETER	( RPRM = 9.0 / 5.0 )
C*
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
	IF  ( ERMISS ( tmpc ) )  THEN
	    PR_TMCF = RMISSD
	  ELSE
	    PR_TMCF = ( tmpc * RPRM ) + 32.
	END IF
C*
	RETURN
	END
