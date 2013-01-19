	FUNCTION PR_DMAX  ( t00x, t06x, tdxc )
C************************************************************************
C* PR_DMAX								*
C*									*
C* This function computes DMAX, the maximum temperature obtained by     *
C* comparing the 6-hour maximum at 00Z, the 6-hour maximum at 06Z, and  *
C* the local midnight maximum (reported at 00 LST).  If either of the   *
C* 6-hour values is missing, the maximum is set to missing.  The inputs *
C* are in degrees C, the output in degrees F.                           *
C*									*
C* REAL PR_DMAX  ( T00X, T06X, TDXC )                                   *
C*									*
C* Input parameters:							*
C*	T00X		REAL	6-hour maximum temperature at 00Z, deg C*
C*	T06X		REAL	6-hour maximum temperature at 06Z, deg C*
C*	TDXC 		REAL	Local midnight max temp at 00 LST, deg C*
C*									*
C* Output parameters:							*
C*	PR_DMAX		REAL	Maximum of the 3 temperature vals, deg F*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 3/99	                                        *
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
	PR_DMAX = RMISSD
C
	IF ( ( ERMISS ( t00x ) ) .or. ( ERMISS ( t06x ) ) ) THEN  
	    RETURN
	  ELSE IF ( .not. ERMISS ( tdxc ) ) THEN
	    PR_DMAX = PR_TMCF ( AMAX1 ( t00x, t06x, tdxc ) )
	  ELSE
	    PR_DMAX = PR_TMCF ( AMAX1 ( t00x, t06x ) )
	END IF
C*
	RETURN
	END
