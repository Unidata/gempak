	FUNCTION PR_TMST  ( thte, pres, tguess )
C************************************************************************
C* PR_TMST 								*
C*									*
C* This function computes TMST from THTE, PRES, TGUESS.  TMST is the	*
C* parcel temperature at level PRES on a specified moist adiabat 	*
C* (THTE).  The computation is an iterative Newton-Raphson technique	*
C* of the form:								*
C*	                                                                *
C*   x = x(guess) + [ f( x ) - f( x(guess) ) ] / f'( x(guess) )		*
C*	                                                                *
C*     f' is approximated with finite differences			*
C*     f' = [ f( x(guess) + 1 ) - f( x(guess) ) ] / 1			*
C*	                                                                *
C* If TGUESS is 0, a reasonable first guess will be made.		*
C*									*
C* Convergence is not guaranteed for extreme input values.  If the	*
C* computation does not converge after 100 iterations, the missing	*
C* data value will be returned.						*
C*	                                                                *
C* REAL PR_TMST  ( THTE, PRES, TGUESS )                        		*
C*									*
C* Input parameters:							*
C*	THTE   		REAL		Equivalent potential temp in K	*
C*	PRES       	REAL    	Pressure in millibars		*
C*	TGUESS   	REAL    	First guess temperature in K	*
C*									*
C* Output parameters:							*
C*	PR_TMST		REAL		Parcel temperature in Kelvin	*
C**									*
C* Log:									*
C* P. Kocin		1980    Orginal source				*
C* M. Goodman/RDS	 9/84	Modified prologue			*
C* G. Huffman/GSC	 8/88	Recode (retain Newton-Raphson concept);	*
C*				use Mark Handel (MIT) first guess	*
C* M. desJardins/GSFC	 8/90	Move tguess into temp variable		*
C* M. Linda/GSC		 9/97	Corrected right border of prologue	*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
	IF  ( ERMISS ( thte ) .or. ERMISS ( pres )
     +	      .or. ERMISS ( tguess ) .or. ( thte .le. 0. )
     +        .or. ( pres .le. 0 ) .or. ( tguess .lt. 0. ) )  THEN
	    PR_TMST = RMISSD
	    RETURN
	END IF
C
C*	Move tguess into another variable.
C
	tg = tguess
C
C*	If TGUESS is passed as 0. it is computed from an MIT scheme.
C
	IF  ( tg .eq. 0. )  tg =
     *  	(thte - .5 * ( MAX ( thte-270., 0. ) ) ** 1.05 )
     *  		* ( pres / 1000. ) ** .2
C
C*	Set convergence and initial guess in degrees C.
C
	epsi = .01
	tgnu = PR_TMKC ( tg )
C
C*	Set a limit of 100 iterations.  Compute TENU, TENUP, the
C*	THTE's at, one degree above the guess temperature.
C
	DO  i = 1, 100
	  tgnup = tgnu + 1.
	  tenu = PR_THTE ( pres, tgnu, tgnu )
	  tenup = PR_THTE ( pres, tgnup, tgnup )
C
C*	  Check that the THTE's exist.
C
	  IF  ( ( ERMISS ( tenu ) ) .or. ( ERMISS ( tenup ) ) )  THEN
	      PR_TMST = RMISSD
	      RETURN
	  END IF
C
C*	  Compute the correction, DELTG; return on convergence.
C
	  cor  = ( thte - tenu ) / ( tenup - tenu )
	  tgnu = tgnu + cor
	  IF  ( ( cor .lt. epsi ) .and. ( -cor .lt. epsi ) )  THEN
	      PR_TMST = PR_TMCK ( tgnu )
	      RETURN
	  END IF
	END DO
C
C*	Failed to converge - return missing.
C
	PR_TMST = RMISSD
	RETURN
	END
