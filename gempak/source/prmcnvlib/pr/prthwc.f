	FUNCTION PR_THWC  ( pres, tmpc, dwpc )
C************************************************************************
C* PR_THWC								*
C*									*
C* This subroutine computes wet bulb potential temperature in Celsius	*
C* from PRES, TMPC and DWPC.  The result is obtained by first		*
C* computing THTE of the the air parcel at level PRES.  Then the air	*
C* parcel is brought to 1000 mb moist adiabatically to get THWC.	*
C*									*
C* REAL PR_THWC  ( PRES, TMPC, DWPC )					*
C*									*
C* Input parameters:							*
C*	PRES   		REAL   		Pressure in millibars		*
C*	TMPC		REAL		Temperature in Celsius		*
C*	DWPC   		REAL		Dewpoint in Celsius		*
C*									*
C* Output parameters:							*
C*	PR_THWC		REAL   		Wet bulb potential temp in C	*
C**									*
C* Log:									*
C* T. Lee/GSC		11/97						*
C* T. Lee/GSC		12/97	Added missing PR_THWC checks		*
C************************************************************************
        INCLUDE   	'GEMPRM.PRM'
        INCLUDE   	'ERMISS.FNC'
C------------------------------------------------------------------------
C*      Check for missing values.
C
	IF  ( ERMISS ( pres ) .or. ERMISS ( tmpc ) .or.
     +	      ERMISS ( dwpc ) .or. ( pres .le. 0. ) )  THEN
	    PR_THWC = RMISSD
	    RETURN
	  ELSE
C
C*	    Find THTE.  
C
	    thte = PR_THTE  ( pres, tmpc, dwpc )
	END IF
C
C*	Check for missing THTE and compute wet bulb temperature.
C
	IF  ( ERMISS ( thte ) )  THEN
	    PR_THWC = RMISSD
	  ELSE
	    tg      = 0.
	    p1000   = 1000.
	    PR_THWC = PR_TMST ( thte, p1000, tg )
	    IF  ( .not. ERMISS ( PR_THWC ) )  PR_THWC = PR_THWC - TMCK
	END IF
C*
	RETURN
	END
