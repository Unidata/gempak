        FUNCTION PR_AWNM ( wnum, twnm, vwnm, pprb )
C************************************************************************
C* PR_AWNM                                                              *
C*                                                                      *
C* This function computes AWNM, the numeric weather code for prevailing/*
C* temporary/probability/vicinity weather, with probability 30 weather  *
C* taking precedence over vicinity weather, vicinity weather taking     *
C* precedence over temporary weather, and temporary weather taking      *
C* precedence over prevailing weather.                                  *
C*                                                                      *
C* REAL PR_AWNM  ( WNUM, TWNM, VWNM, PPRB )                             *
C*                                                                      *
C* Input parameters:                                                    *
C*	WNUM		REAL	Prevailing numeric weather code         *
C*	TWNM 		REAL    Tempo./probability numeric weather code *
C*      VWNM		REAL	Vicinity numeric weather code           *
C*	PPRB		REAL	Probability for fcst change indicator   *
C*                                                                      *
C* Output parameters:                                                   *
C*      PR_AWNM         REAL    All weather numeric weather code        *
C**                                                                     *
C* Log:                                                                 *
C* D. Kidwell/NCEP	 5/03                                           *
C************************************************************************
        INCLUDE   	'GEMPRM.PRM'
	INCLUDE	  	'ERMISS.FNC'
C------------------------------------------------------------------------
	PR_AWNM = PR_TPWN ( twnm, vwnm, pprb )
	IF ( ERMISS ( PR_AWNM ) )  PR_AWNM = wnum 
C*
        RETURN
        END
