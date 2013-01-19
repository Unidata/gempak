	FUNCTION PR_THTA  ( tmpc, pres )
C************************************************************************
C* PR_THTA								*
C*									*
C* This function computes THTA from TMPC and PRES using Poisson's	*
C* equation:								*
C*									*
C*            THTA = TMPK * ( 1000 / PRES ) ** RKAPPA			*
C*									*
C* This function also computes STHA from TMPC and PALT, THTV from	*
C* TVRC and PRES, and THTV from TVRC and PALT.				*
C*									*
C* REAL PR_THTA  ( TMPC, PRES )						*
C*									*
C* Input parameters:							*
C*	TMPC		REAL		Temperature in Celsius		*
C*	PRES		REAL    	Pressure in millibars		*
C*									*
C* Output parameters:							*
C*	PR_THTA		REAL		Potential temperature in K	*
C**									*
C* Log:									*
C* P. Kocin/914		1980	Original source code			*
C* G. Huffman/GSC       8/88    Documentation; check p .le. 0		*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
C*      Check for missing values.
C
	IF  ( ERMISS ( tmpc ) .or. ERMISS ( pres )
     *       .or.  ( pres .le. 0. ) )  THEN
	    PR_THTA = RMISSD
	  ELSE
C
C*	    Change temperature in degrees Celsius to Kelvin.
C
	    tmpk = PR_TMCK( tmpc )
C
C*	    Calculate theta.
C
	    PR_THTA = tmpk * (1000. / pres) ** RKAPPA 
	END IF
C*
	RETURN
	END
