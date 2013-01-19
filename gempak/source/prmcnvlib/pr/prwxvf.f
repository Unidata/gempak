        FUNCTION PR_WXVF ( xvfr, txvf )
C************************************************************************
C* PR_WXVF                                                              *
C*                                                                      *
C* This function computes WXVF, the worst case categorical              *
C* identification of flight rules for prevailing and temporary/         *
C* probability conditions.                                              *
C*                                                                      *
C* REAL PR_WXVF  ( XVFR, TXVF )                                         *
C*                                                                      *
C* Input parameters:                                                    *
C*      XVFR            REAL    Prevailing categorical id of flt rules  *
C*	TXVF 		REAL    Tempo/prob categorical id of flt rules  *
C*                                                                      *
C* Output parameters:                                                   *
C*      PR_WXVF         REAL    Worst case categorical id of flt rules  *
C**                                                                     *
C* Log:                                                                 *
C* D. Kidwell/NCEP	 5/03                                           *
C************************************************************************
        INCLUDE   	'GEMPRM.PRM'
	INCLUDE	  	'ERMISS.FNC'
C------------------------------------------------------------------------
	IF ( ERMISS ( xvfr ) .and. ERMISS ( txvf ) ) THEN
	    PR_WXVF = RMISSD
	  ELSE IF ( ERMISS ( txvf ) ) THEN
	    PR_WXVF = xvfr
          ELSE IF ( ERMISS ( xvfr ) ) THEN
	    PR_WXVF = txvf
	  ELSE
	    PR_WXVF = AMIN1 ( xvfr, txvf )
	END IF
C*
        RETURN
        END

