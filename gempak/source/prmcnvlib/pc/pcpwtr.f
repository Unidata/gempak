	FUNCTION PC_PWTR  ( tdb, tdt, pb, pt, pwb )
C************************************************************************
C* PC_PWTR								*
C*									*
C* This function computes precipitable water. 				*
C*									*
C* REAL PC_PWTR  ( TDB, TDT, PB, PT, PWB )				*
C*									*
C* Input parameters:							*
C*	TDB		REAL		Bottom dewpoint			*
C*	TDT		REAL		Top dewpoint 			*
C*	PB		REAL		Bottom pressure			*
C*	PT		REAL		Top pressure			*
C*	PWB		REAL		Bottom precipitable water	*
C*									*
C* Output parameters:							*
C*	PC_PWTR		REAL		Precipitable water		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/84						*
C* M. desJardins/GSFC	10/87	Fixed typing errors			*
C* M. desJardins/GSFC	10/87	Multiply by 100. to convert to mm.	*
C* J. Whistler/SSAI	 2/91	Changed wrong variable to PWB		*
C* T. Piper/GSC		11/98	Updated prolog				*
C************************************************************************
        INCLUDE 'GEMPRM.PRM'
        INCLUDE 'ERMISS.FNC'
C------------------------------------------------------------------------
	IF ( ( .not. ERMISS (tdb) ) .and. ( .not. ERMISS (tdt) ) .and. 
     +	     ( .not. ERMISS  (pb) ) .and. ( .not. ERMISS  (pt) ) )  THEN
C
C*	    Compute the average mixing ratio.  Note that the mixing
C*	    ratio is divided by 1000. since PR_MIXR computes g/kg.
C
	    rmb  = PR_MIXR ( tdb, pb )
	    rmt  = PR_MIXR ( tdt, pt )
	    rmav = ( rmb + rmt ) / 2.0 / 1000.
	    PC_PWTR = 100. * rmav * ( pb - pt ) / GRAVTY + pwb
	  ELSE
	    PC_PWTR = pwb
	END IF
C*
	RETURN
	END
