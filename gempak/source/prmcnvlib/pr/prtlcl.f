	FUNCTION PR_TLCL  ( tmpc, dwpc )
C************************************************************************
C* PR_TLCL 								*
C*									*
C* This function computes temperature at the Lifted Condensation	*
C* Level for a parcel of air given TMPC and  DWPC.  The following 	*
C* equation is used:							*
C*									*
C*  TLCL = [ 1 / ( 1 / (DWPK-56) + ALOG (TMPK/DWPK) / 800 ) ] + 56	*
C*									*
C* Bolton.								*
C*									*
C* REAL PR_TLCL  ( TMPC, DWPC )						*
C*									*
C* Input parameters:							*
C*	TMPC		REAL    	Temperature in Celsius		*
C*	DWPC		REAL		Dewpoint in Celsius		*
C*									*
C* Output parameters:							*
C*	PR_TLCL		REAL    	LCL temperature in Kelvin	*
C**									*
C* Log:									*
C* M. Goodman/RDS	8/84	Original source				*
C* I. Graffman/RDS	12/87	GEMPAK4					*
C* G. Huffman/GSC	8/88	Modified documentation, made -273 real  *
C* G. Krueger/EAI        4/96   Replaced C->K constant with TMCK	*
C* T. Lee/GSC		 8/97	Rearranged equation			*
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
C*      Check for missing or out-of-bounds values.
C
	IF  ( ( ERMISS ( tmpc ) ) .or. ( ERMISS ( dwpc ) ) .or.
     +        ( tmpc .lt. -TMCK ) .or. ( dwpc .lt. -TMCK ) )  THEN
	    PR_TLCL = RMISSD
	  ELSE
	    tmpk = PR_TMCK ( tmpc )
	    dwpk = PR_TMCK ( dwpc )
	    PR_TLCL = ( 800. * ( dwpk - 56. ) / ( 800. + ( dwpk - 56. )*
     +			ALOG ( tmpk / dwpk ) ) ) + 56.
	END IF
C*
	RETURN
	END
