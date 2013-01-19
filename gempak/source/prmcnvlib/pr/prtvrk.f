	FUNCTION PR_TVRK  ( tmpc, dwpc, pres )
C************************************************************************
C* PR_TVRK 								*
C*									*
C* This function computes TVRK from TMPC, DWPC and PRES, where DWPC	*
C* and PRES are used to compute MIXR.  The following equation is	*
C* used:								*
C*									*
C*   TVRK = TMPK * (1 + .001 * MIXR / .62197) / (1 + .001 * MIXR)	*
C*	                                                                *
C* If DWPC is missing, dry air is assumed and TMPK is returned.		*
C*									*
C* REAL PR_TVRK  ( TMPC, DWPC, PRES )					*
C*									*
C* Input parameters:							*
C*	TMPC		REAL		Temperature in Celsius		*
C*	DWPC		REAL		Dewpoint in Celsius		*
C*	PRES		REAL		Pressure in millibars		*
C*									*
C* Output parameters:							*
C*	PR_TVRK		REAL		Virtual temperature in Kelvin	*
C**	                                                                *
C* Log:									*
C* P. Kocin/CSC		1980    Original source PR_TVIRT		*
C* M. Goodman/RDS	 8/84	Renamed 				*
C* M. desJardins/GSFC	 9/86	Return temp if dewpoint is missing	*
C* I. Graffman/RDS	12/87	GEMPAK4					*
C* G. Huffman/GSC	 8/88	Documentation; RMIX test		*
C************************************************************************
        INCLUDE    'GEMPRM.PRM'
        INCLUDE    'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check if temperature or pressure is missing.
C
	IF  ( ERMISS ( tmpc ) .or. ERMISS ( pres ) )  THEN
	    PR_TVRK = RMISSD
C
C*	    If dewpoint is missing, return temperature.
C
	  ELSE IF  ( ERMISS ( dwpc ) )  THEN
	    PR_TVRK = PR_TMCK ( tmpc )
	  ELSE
C
C*	    Change temperature to Kelvin.
C
	    tmpk = PR_TMCK ( tmpc )
C
C*	    Find mixing ratio in g/kg; if missing, return temperature.
C
	    rmix = PR_MIXR  ( dwpc, pres )
	    IF  ( ERMISS ( rmix ) )  THEN
	        PR_TVRK = PR_TMCK ( tmpc )
	      ELSE
	        PR_TVRK =
     *           tmpk * (1. + .001*rmix/.62197) / (1. + .001*rmix)
	    END IF
	END IF
C*
	RETURN
	END
