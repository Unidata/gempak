        FUNCTION PR_PR24 ( p01, p02, p03, p04 )
C************************************************************************
C* PR_PR24                                                              *
C*                                                                      *
C* This function computes PR24, the 24-hour precipitation calculated by *
C* summing four 6-hour precipitation values.                            *
C*                                                                      *
C* REAL PR_PR24  ( P01, P02, P03, P04 )					*
C*                                                                      *
C* Input parameters:                                                    *
C*      P01		REAL	First 6-hour precipitation amount	*
C*	P02 		REAL    Second 6-hour precipitation amount	*
C*	P03		REAL    Third 6-hour precipitation amount	*
C*	P04		REAL    Fourth 6-hour precipitation amount	*
C*                                                                      *
C* Output parameters:                                                   *
C*      PR_PR24         REAL    Total 24-hour precipitation amount      *
C**                                                                     *
C* Log:                                                                 *
C* D. Kidwell/NCEP	11/00                                           *
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
C------------------------------------------------------------------------
	p24 = AMAX1 ( p01, p02, p03, p04 )
C
	IF ( p24 .gt. 0. ) THEN
	    p24 = 0.
	    IF ( p01 .gt. 0. ) p24 = p24 + p01
	    IF ( p02 .gt. 0. ) p24 = p24 + p02
	    IF ( p03 .gt. 0. ) p24 = p24 + p03
	    IF ( p04 .gt. 0. ) p24 = p24 + p04
	  ELSE IF ( p24 .lt. 0. ) THEN
            p24 = RMISSD
	END IF
C
	PR_PR24 = p24
C*
        RETURN
        END

