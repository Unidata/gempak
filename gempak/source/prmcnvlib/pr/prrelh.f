	FUNCTION PR_RELH  ( tmpc, dwpc )
C************************************************************************
C* PR_RELH								*
C*									*
C* This function computes RELH from TMPC and DWPC.  The following	*
C* equation is used:							*
C*									*
C*               RELH  =  VAPR / VAPS * 100				*
C*									*
C*                     VAPR = vapor pressure				*
C*                          = PR_VAPR ( DWPC )				*
C*                     VAPS = saturation vapor pressure			*
C*                          = PR_VAPR ( TMPC )				*
C*									*
C* REAL PR_RELH  ( TMPC, DWPC )						*
C*									*
C* Input parameters:							*
C*	TMPC		REAL		Temperature in Celsius		*
C*	DWPC		REAL    	Dewpoint in Celsius		*
C*									*
C* Output parameters:							*
C*	PR_RELH		REAL		Relative humidity in percent	*
C**									*
C* Log:									*
C* I. Graffman/CSC	8/83	Original source code			*
C* M. Goodman/RDS	3/84	Modified prologue			*
C* I. Graffman/RDS	12/87	GEMPAK4					*
C* G. Huffman/GSC	7/88	Documentation				*
C* M. Linda/GSC		 9/97	Corrected right border of prologue	*
C************************************************************************
        INCLUDE    'GEMPRM.PRM'
        INCLUDE    'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for missing data.
C
	IF  ( ERMISS ( tmpc ) .or. ERMISS ( dwpc ) )  THEN
	    PR_RELH = RMISSD
	  ELSE
C
C*	    Find the vapor pressure .
C
	    E = PR_VAPR ( dwpc )
C
C*	    Find the saturated vapor pressure.
C
	    es = PR_VAPR ( tmpc )
C
C*	    Calculate humidity.
C
	    PR_RELH = ( e / es ) * 100.
	END IF
C*
	RETURN
	END
