	FUNCTION PR_XVFR  ( ceil, vsby )
C************************************************************************
C* PR_XVFR								*
C*									*
C* This function computes LIFR/IFR/MVFR/VFR flight conditions based on  *
C* ceiling and visibility.                                              *
C*									*
C* REAL PR_XVFR  ( CEIL, VSBY )						*
C*									*
C* Input parameters:							*
C*      CEIL		REAL		Ceiling in hundreds of feet     *
C*	VSBY		REAL    	Visibility in statute miles	*
C*									*
C* Output parameters:							*
C*	PR_XVFR		REAL	        Flight conditions index value   *
C*                                        0 = LIFR                      *
C*                                        1 = IFR                       *
C*                                        2 = MVFR                      *
C*                                        3 = VFR                       *
C*									*
C**									*
C* Log:									*
C* J. Green/AWC		6/99	Original source code			*
C* D. Kidwell/NCEP	7/99	Restructured         			*
C* D. Kidwell/NCEP	5/00	Added LIFR (Low Instrument Flight Rules)*
C* L. Hinson/AWC        1/14    Added fix for comparisons to CIG/VSBY   *
C*                              on borderline MVFR thresholds.          *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'ERMISS.FNC'
C       This parameter is used in the .le. comparisons to VSBY/CIG
C       on borderline MVFR conditions.  There was a problem found with
C       system not storing 5 SM as exactly 5.0, but something slightly
C       higher causing problems with .le comparisons.  This caused 
C       certain TAFs/METARs to read VFR, when they should be MVFR.
C
        PARAMETER       ( EPS = .01 )
C------------------------------------------------------------------------
C*      Check for missing values.
C
	PR_XVFR = RMISSD
	IF  ( ERMISS ( ceil ) .and. ERMISS ( vsby ) ) THEN
	    RETURN
	END IF
C
C*      Compute categorical flight rules. 
C*	Check the ceiling value.
C      
	vc = RMISSD
      	IF ( .not. ( ERMISS ( ceil ) ) ) THEN
	    IF ( ceil .lt. 0. ) THEN
	      ELSE IF ( ceil .lt. 5. )  THEN
		vc = 0.
              ELSE IF ( ceil .lt. 10. ) THEN
		vc = 1.
	      ELSE IF ( ceil .le. (30. + EPS) ) THEN
		vc = 2.
	      ELSE IF ( ( vsby .gt. (5. + EPS) ) .or. 
     +                  ( vsby .lt. 0. ) .or.
     +			( ERMISS ( vsby ) ) ) THEN
		PR_XVFR = 3.
		RETURN
	    END IF
	END IF
C
C*	Check the visibility value.
C
	vs = RMISSD
	IF ( .not. ERMISS ( vsby ) ) THEN
	    IF ( vsby .lt. 0. ) THEN
	      ELSE IF ( vsby .lt. 1. ) THEN
		vs = 0.
	      ELSE IF ( vsby .lt. 3. ) THEN
		vs = 1.
	      ELSE IF ( vsby .le. (5. + EPS) ) THEN 
		vs = 2.
	      ELSE
		vs = 3.
	    END IF
	END IF
C
C*	Determine the more restrictive of the two values.
C
	IF ( ERMISS ( vc ) ) THEN
	    PR_XVFR = vs
	  ELSE IF ( ERMISS ( vs ) ) THEN
	    PR_XVFR = vc
	  ELSE
	    PR_XVFR = AMIN1 ( vc, vs )
	END IF
C*
	RETURN
	END
