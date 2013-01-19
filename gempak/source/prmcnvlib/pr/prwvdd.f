	FUNCTION PR_WVDD  ( dosw, dos2 )
C************************************************************************
C* PR_WVDD								*
C*									*
C* This function computes WVDD, the combined predominant swell wave     *
C* direction and secondary swell wave direction, from DOSW and DOS2.    *
C* A group number of 3 is prefixed to the combined value.  On output,   *
C* the direction of the swell waves is in tens of degrees.  If the      *
C* secondary direction is missing, a value of 99 is used.               *
C* The following equation is used:                                      *
C*									*
C*       WVDD = 30000 + ( DOSW / 10 ) * 100 + ( DOS2 / 10 )             *
C*									*
C* REAL PR_WVDD  ( DOSW, DOS2 )                                         *
C*									*
C* Input parameters:							*
C*	DOSW		REAL	Predominant swell wave dir. in degrees  *
C*	DOS2		REAL	Secondary swell wave dir. in degrees    *
C*									*
C* Output parameters:							*
C*	PR_WVDD		REAL	Combined swell wave directions          *
C**									*
C* Log:									*
C* D. Kidwell/NCEP	12/98						*
C* D. Kidwell/NCEP	 2/99	Changed to use NINT; cleaned up         *
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
	PR_WVDD = RMISSD
C
        IF ( .not. ERMISS ( dos2 ) ) THEN
	    idos2 = NINT ( dos2 / 10. )
	  ELSE
	    idos2 = 99
	END IF
C
	IF ( .not. ERMISS ( dosw ) ) THEN 
	    idosw = NINT ( dosw / 10. )
	    IF ( ( idosw .ge. 0 ) .and. ( idosw .le. 99 ) .and.
     +		 ( idos2 .ge. 0 ) .and. ( idos2 .le. 99 ) ) THEN
		PR_WVDD = 30000 + idosw * 100 + idos2
	    END IF
	END IF
C*
	RETURN
	END
