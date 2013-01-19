	FUNCTION PR_DDEP ( tmpx, dwpx )
C************************************************************************
C* PR_DDEP								*
C*									*
C* This function computes DPDX, the dewpoint depression, from TMPX and 	*
C* DWPX, both of which must be in the same units (Celsius, Kelvin, or	*
C* Fahrenheit).  The output will be calculated in these units.  The 	*
C* following equation is used:						*
C*									*
C*               DPDX = TMPX - DWPX					*
C*									*
C* REAL PR_DDEP  ( TMPX, DWPX )   					*
C*									*
C* Input parameters:							*
C*	TMPX		REAL    	Air temperature 		*
C*	DWPX		REAL		Dewpoint temperature		*
C*									*
C* Output parameters:							*
C*	PR_DDEP		REAL		Dewpoint depression		*
C**									*
C* Log:									*
C* M. Goodman/RDS	1/84	Original source				*
C* I. Graffman/RDS	12/87	GEMPAK4					*
C* G. Huffman/GSC	7/88	Documentation, variable names		*
C************************************************************************
        INCLUDE  'GEMPRM.PRM'
        INCLUDE  'ERMISS.FNC'
C------------------------------------------------------------------------
	IF  ( ERMISS (tmpx) .or. ERMISS (dwpx) .or. 
     +	    ( dwpx .gt. tmpx ) )  THEN
	    PR_DDEP = RMISSD
	  ELSE
	    PR_DDEP = tmpx - dwpx
	END IF
C*
	RETURN
	END
