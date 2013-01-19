	FUNCTION PR_DWPT  ( rmix, pres )
C************************************************************************
C* PR_DWPT								*
C* 									*
C* This function computes DWPT from MIXR and PRES.  The following	*
C* equation is used:							*
C*									*
C*   DWPT = ALOG (E / 6.112) * 243.5 / ( 17.67 - ALOG (E / 6.112) )	*
C*									*
C*        E = vapor pressure						*
C*          = e / ( 1.001 + ( (PRES - 100.) / 900. ) * .0034 ) 		*
C*        e = ( PRES * MIXR ) / ( .62197 + MIXR ) 			*
C*									*
C* Bolton.								*
C*									*
C* This function also computes TMPC from MIXS and PRES.			*
C*									*
C* REAL PR_DWPT  ( RMIX, PRES )						*
C*									*
C* Input parameters:							*
C*	RMIX		REAL		Mixing ratio in g/kg		*
C*	PRES   		REAL    	Pressure in millibars		*
C*									*
C* Output parameters:							*
C*	PR_DWPT		REAL		Dewpoint in Celsius		*
C**									*
C* Log:									*
C* P. Kocin/914		1980	Original source code			*
C* M. Goodman/RDS  	8/84	Cleaned code				*
C* G.Huffman/GSC	7/88	Constant .62197, p .le. 0		*
C* J. Nielsen/SUNYA	8/90	Assume mixing ratio is in g/kg		*
C* T. Piper/GSC		11/98	Updated prolog				*
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
	IF  ( ERMISS ( rmix ) .or. ( rmix .le. 0. ) .or. 
     +	      ERMISS ( pres ) .or. ( pres .le. 0. ) )  THEN	
	    PR_DWPT = RMISSD
	  ELSE
C
C*	    Convert G/KG to G/G.
C
	    ratio = rmix / 1000.
C
C*	    Calculate vapor pressure from mixing ratio and pressure.
C
	    e = (pres * ratio) / (.62197 + ratio)
C
C*	    Correct vapor pressure.
C
	    e = e / (1.001 + ((pres - 100.) / 900.) * .0034)
C
C*	    Calculate dewpoint.
C
	    PR_DWPT = ALOG (e/6.112) * 243.5 / (17.67 - ALOG(e/6.112))
	END IF
C*
	RETURN
	END
