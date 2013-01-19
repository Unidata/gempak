	FUNCTION PR_DMIN  ( t12n, t18n )
C************************************************************************
C* PR_DMIN								*
C*									*
C* This function computes DMIN, the minimum temperature obtained by     *
C* comparing the 6-hour minimum at 12Z and the 6-hour minimum at 18Z.   *
C* If either of the 6-hour values is missing, the minimum is set to     *
C* missing.  The inputs are in degrees C, the output in degrees F.      *
C*									*
C* REAL PR_DMIN  ( T12N, T18N )                                         *
C*									*
C* Input parameters:							*
C*	T12N		REAL	6-hour minimum temperature at 12Z, deg C*
C*	T18N		REAL	6-hour minimum temperature at 18Z, deg C*
C*									*
C* Output parameters:							*
C*	PR_DMIN		REAL	Min of the two temperature values, deg F*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 3/99	                                        *
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
	PR_DMIN = RMISSD
C
	IF ( ( ERMISS ( t12n ) ) .or. ( ERMISS ( t18n ) ) ) THEN  
	    RETURN
	  ELSE
	    PR_DMIN = PR_TMCF ( AMIN1 ( t12n, t18n ) )
	END IF
C*
	RETURN
	END
