	FUNCTION PR_THTE  ( pres, tmpc, dwpc )
C************************************************************************
C* PR_THTE								*
C*									*
C* This function computes THTE from PRES, TMPC, DWPC.  In the		*
C* calculation, MIXR depends on PRES and DWPC; TLCL depends on 		*
C* TMPC and DWPC.  The following equation is used:			*
C*									*
C*           THTE = THTAM * EXP [ ( 3.376/TLCL - .00254 ) *		*
C*                                ( MIXR * ( 1 + .81*.001*MIXR ) ) ]	*
C*									*
C*                THTAM = potential temperature of moist air		*
C*                      = TMPK * (1000 / PRES) ** E			*
C*                    E = RKAPPA * ( 1 - ( .28 * .001 * MIXR ) )	*
C*									*
C* Bolton.								*
C*									*
C* REAL PR_THTE  ( PRES, TMPC, DWPC )					*
C*									*
C* Input parameters:							*
C*	PRES   		REAL   		Pressure in millibars		*
C*	TMPC		REAL		Temperature in Celsius		*
C*	DWPC   		REAL		Dewpoint in Celsius		*
C*									*
C* Output parameters:							*
C*	PR_THTE		REAL   		Equivalent potential temp in K	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	1983	Original source code			*
C* M. Goodman/RDS	3/84	Added PR_TMCK				*
C* I. Graffman/RDS	11/84 	changed .2854 to 2./7.			*
C* I. Graffman/RDS	12/87 	use GEMPAK4 constant file		*
C* G. Huffman/GSC       8/88    Documentation; check PRES .le. 0,	*
C*				missing RMIX				*
C************************************************************************
        INCLUDE   	'GEMPRM.PRM'
        INCLUDE   	'ERMISS.FNC'
C------------------------------------------------------------------------
C*      Check for missing values.
C
	IF  ( ERMISS ( pres ) .or. ERMISS ( tmpc ) .or.
     +	      ERMISS ( dwpc ) .or. ( pres .le. 0. ) )  THEN
	    PR_THTE = RMISSD
	  ELSE
C
C*	    Find mixing ratio; check for bad values.
C
	    rmix = PR_MIXR  ( dwpc, pres )
	    IF  ( ERMISS ( rmix ) )  THEN
		PR_THTE = RMISSD
		RETURN
	    END IF
C
C*	    Change degrees Celsius to Kelvin.
C
	    tmpk = PR_TMCK ( tmpc )
C
C*	    Calculate  THETA-M  (theta for moist air)
C
	    E = RKAPPA * (1. - (.28 * .001 * rmix) )
	    thtam = tmpk * (1000. / pres) ** e
C
C*	    Find the temperature at the LCL.
C
	    tlcl = PR_TLCL ( tmpc, dwpc )
	    e = ((3.376 /tlcl) - .00254) * (rmix * (1. + .81*.001*rmix))
	    PR_THTE = thtam * EXP (e)
	END IF
C*
	RETURN
	END
