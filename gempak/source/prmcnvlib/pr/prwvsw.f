	FUNCTION PR_WVSW  ( dosw, posw, hosw )
C************************************************************************
C* PR_WVSW								*
C*									*
C* This function computes WVSW, the combined swell wave direction in    *
C* tens of degrees, the swell wave period in seconds, and the swell     *
C* wave height in half meters, from DOSW, POSW, and HOSW.               *
C* The following equation is used:                                      *
C*									*
C*       WVSW = ( DOSW / 10 ) * 10000 + PR_WVPH ( POSW, 2 * HOSW, 0 )   *
C*									*
C* REAL PR_WVSW  ( DOSW, POSW, HOSW )                                   *
C*									*
C* Input parameters:							*
C*	DOSW		REAL	Swell wave direction in degrees         *
C*	POSW		REAL	Swell wave period in seconds            *
C*	HOSW 		REAL	Swell wave height in meters             *
C*									*
C* Output parameters:							*
C*	PR_WVSW		REAL	Combined direction, period and height   *
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 2/99	                                        *
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
	PR_WVSW = RMISSD
C
	IF ( ( .not. ERMISS ( dosw ) ) .and. 
     +       ( .not. ERMISS ( posw ) ) .and. 
     +       ( .not. ERMISS ( hosw ) ) ) THEN
	    pphh = PR_WVPH ( posw, 2. * hosw, 0. )
	    IF ( .not. ERMISS ( pphh ) ) THEN
	        PR_WVSW = NINT ( dosw / 10. ) * 10000 + NINT ( pphh )
	    END IF
	END IF
C*
	RETURN
	END
