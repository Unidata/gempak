	FUNCTION PR_WPHM  ( wper, whgt, poww, howw )
C************************************************************************
C* PR_WPHM								*
C*									*
C* This function computes WPHM, the combined wave period and wave       *
C* height in half meters, from WPER and WHGT, or from POWW and HOWW.    *
C* Either instrument wave data or wind wave data, but not both, can be  *
C* reported.  A group number is prefixed to the combined value to       *
C* distinguish between instrument waves and wind waves.                 *
C* The following equation is used:                                      *
C*									*
C*       WPHM = PR_WVPH ( POWV, 2 * HOWV, GROUP )                       *
C*									*
C* REAL PR_WPHM  ( WPER, WHGT, POWW, HOWW )                             *
C*									*
C* Input parameters:							*
C*	WPER		REAL		Instrument wave period, seconds *
C*	WHGT 		REAL		Instrument wave height, meters  *
C*	POWW		REAL		Wind wave period in seconds     *
C*	HOWW 		REAL		Wind wave height in meters    	*
C*									*
C* Output parameters:							*
C*	PR_WPHM		REAL		Combined wave period and height *
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 2/99	                                        *
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
	IF ( ( .not. ERMISS ( wper ) ) .and. 
     +       ( .not. ERMISS ( whgt ) ) ) THEN
C
C*	    Get the combined instrument wave period and height.
C
	    PR_WPHM = PR_WVPH ( wper, 2. * whgt, 1. )
	  ELSE IF ( ( .not. ERMISS ( poww ) ) .and. 
     +       ( .not. ERMISS ( howw ) ) ) THEN
C
C*	    Get the combined wind wave period and height.
C
	    PR_WPHM = PR_WVPH ( poww, 2. * howw, 2. )
	  ELSE
	    PR_WPHM = RMISSD
	END IF
C*
	RETURN
	END
