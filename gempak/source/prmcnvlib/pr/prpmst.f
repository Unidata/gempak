	FUNCTION PR_PMST  ( thte, tmpk )
C************************************************************************
C* PR_PMST 								*
C*									*
C* This function computes parcel PRES from THTE and TMPC, where TMPC	*
C* is the parcel temperature at PRES on a specified moist adiabat	*
C* (THTE). The computation is an iterative Newton-Raphson technique	*
C* of the form:								*
C*	                                                                *
C*   x = x(guess) + [ f( x ) - f( x(guess) ) ] / f'( x(guess) )		*
C*	                                                                *
C*     f' is approximated with finite differences			*
C*     f' = [ f( x(guess) + 1 ) - f( x(guess) ) ] / 1			*
C*	                                                                *
C* Convergence is not guaranteed for extreme input values.  If the	*
C* computation does not converge after 100 iterations, the missing	*
C* data value will be returned.						*
C*	                                                                *
C* REAL PR_PMST  ( THTE, TMPK )						*
C*									*
C* Input parameters:							*
C*	THTE   		REAL		Equivalent potential temp in K	*
C*	TMPK		REAL		Parcel temperature in Kelvin	*
C*									*
C* Output parameters:							*
C*	PR_PMST       	REAL    	Pressure in millibars		*
C**									*
C* Log:									*
C* T. Lee/GSC		 1/00	Created					*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
	LOGICAL		done
C*
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
	IF  ( ERMISS ( thte ) .or. ERMISS ( tmpk )
     +	      .or. ( thte .le. 0. ) ) THEN
	    PR_PMST = RMISSD
	    RETURN
	END IF
C
C*	Set convergence and initial guess of pressure.
C
	epsi = .01
	tmpc = PR_TMKC ( tmpk )
	pgdn = 1000. * ( tmpk / thte ) ** AKAPPA
C
C*	Set a limit of 100 iterations.  Compute TEDN and TEUP, the
C*	THTE's at one degree above the guess temperature.
C
	done = .false.
	i = 1
	DO  WHILE ( .not. done )
	  pgup =  pgdn + 1.
	  tedn = PR_THTE ( pgdn, tmpc, tmpc )
	  teup = PR_THTE ( pgup, tmpc, tmpc )
C
C*	  Check THTE.
C
	  IF  ( ( ERMISS ( tedn ) ) .or. ( ERMISS ( teup ) ) )  THEN
	      PR_PMST = RMISSD
	      RETURN
	  END IF
C
C*	  Compute the correction; return on convergence.
C
	  cor  = ( thte - tedn ) / ( teup - tedn )
	  pgdn = pgdn + cor
	  IF  ( ABS ( cor ) .lt. epsi )  THEN
	      PR_PMST = pgdn
	      RETURN
	  END IF
	  i = i + 1
	  IF  ( i .gt. 100 ) done = .true.
	END DO
C
C*	Failed to converge; return missing.
C
	PR_PMST = RMISSD
C*
	RETURN
	END
