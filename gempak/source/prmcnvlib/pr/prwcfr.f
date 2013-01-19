        FUNCTION PR_WCFR ( xvfr, txvf, cfrt, tcfr )
C************************************************************************
C* PR_WCFR                                                              *
C*                                                                      *
C* This function computes WCFR, the numeric total cloud cover for the   *
C* worst case aviation flight condition, based on the categorical       *
C* identification of flight rules for prevailing and temporary/         *
C* probability conditions.                                              *
C*                                                                      *
C* REAL PR_WCFR  ( XVFR, TXVF, CFRT, TCFR )                             *
C*                                                                      *
C* Input parameters:                                                    *
C*      XVFR            REAL    Prevailing categorical id of flt rules  *
C*	TXVF 		REAL    Tempo/prob categorical id of flt rules  *
C*	CFRT 		REAL    Prevailing numeric total cloud cover    *
C*	TCFR 		REAL    Tempo/prob numeric total cloud cover    *
C*                                                                      *
C* Output parameters:                                                   *
C*      PR_WCFR         REAL    Worst case numeric total cloud cover    *
C**                                                                     *
C* Log:                                                                 *
C* D. Kidwell/NCEP	 5/03                                           *
C************************************************************************
        INCLUDE   	'GEMPRM.PRM'
	INCLUDE	  	'ERMISS.FNC'
C------------------------------------------------------------------------
	IF ( ERMISS ( xvfr ) .or. ERMISS ( txvf ) ) THEN
	    PR_WCFR = AMAX1 ( cfrt, tcfr )
	  ELSE
	    IF ( txvf .lt. xvfr ) THEN
		PR_WCFR = tcfr
	      ELSE IF ( txvf .eq. xvfr ) THEN
	        PR_WCFR = AMAX1 ( cfrt, tcfr )
	      ELSE
		PR_WCFR = cfrt
	    END IF
	END IF
C*
        RETURN
        END

