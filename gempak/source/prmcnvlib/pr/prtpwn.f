        FUNCTION PR_TPWN ( twnm, vwnm, pprb )
C************************************************************************
C* PR_TPWN                                                              *
C*                                                                      *
C* This function computes TPWN, the numeric weather code for temporary/ *
C* probability/vicinity weather, with probability 30 weather taking     *
C* precedence over vicinity weather and vicinity weather taking         *
C* precedence over temporary weather.                                   *
C*                                                                      *
C* REAL PR_TPWN  ( TWNM, VWNM, PPRB )                                   *
C*                                                                      *
C* Input parameters:                                                    *
C*	TWNM 		REAL    Tempo./probability numeric weather code *
C*      VWNM		REAL	Vicinity numeric weather code           *
C*	PPRB		REAL	Probability for fcst change indicator   *
C*                                                                      *
C* Output parameters:                                                   *
C*      PR_TPWN         REAL    Tempo./prob./vicinity numeric wthr code *
C**                                                                     *
C* Log:                                                                 *
C* D. Kidwell/NCEP	 5/03                                           *
C************************************************************************
        INCLUDE   	'GEMPRM.PRM'
	INCLUDE	  	'ERMISS.FNC'
C------------------------------------------------------------------------
	IF ( ( NINT ( pprb ) .eq. 30 ) .and.
     +       ( .not. ERMISS ( twnm ) ) ) THEN 
	    PR_TPWN = twnm
	  ELSE IF ( .not. ERMISS ( vwnm ) ) THEN
	    PR_TPWN = vwnm
	  ELSE
	    PR_TPWN = twnm
	END IF
C*
        RETURN
        END
