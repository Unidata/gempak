	FUNCTION MT_CEIL  ( chc1, chc2, chc3 )
C************************************************************************
C* MT_CEIL								*
C*									*
C* This function gets CEIL from CHC1, CHC2, and CHC3.  CEIL is the	*
C* ceiling, defined in FMH-1 as the height above the earth's surface    *
C* of the lowest layer that is reported as broken or overcast, or the   *
C* vertical visibility into an indefinite ceiling.  For METAR data, a   *
C* valid cloud height of zero is indicated by assigning a negative      *
C* value to CHCx.                                                       *
C*									*
C* REAL MT_CEIL  ( CHC1, CHC2, CHC3 )					*
C*									*
C* Input parameters:							*
C*	CHC1		REAL		Cloud height & coverage 1	*
C*	CHC2		REAL		Cloud height & coverage 2	*
C*	CHC3		REAL		Cloud height & coverage 3	*
C*									*
C* Output parameters:							*
C*	MT_CEIL		REAL		Ceiling in 100's of feet       	*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 5/98						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		MT_CEIL 
C*
	REAL		work (3)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	MT_CEIL = RMISSD
C 
	work ( 1 ) = chc1
	work ( 2 ) = chc2
	work ( 3 ) = chc3
	DO i = 1, 3
	    awork = ABS ( work ( i ) ) 
	    icovr = INT ( PR_CLCX ( awork ) )
	    IF ( icovr .ge. 3 .and. icovr .le. 5 ) THEN
	        MT_CEIL = PR_CLHX ( awork ) 
C
C*		Check for cloud height of zero.
C
		IF ( ERMISS ( MT_CEIL ) )  THEN
		    IF ( work ( i ) .lt. 0. ) MT_CEIL = 0.
		END IF
	        RETURN
	    END IF
	END DO
C*
	RETURN
	END
