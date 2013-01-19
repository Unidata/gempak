	FUNCTION PR_LTMP  ( thta, thte, pres )
C************************************************************************
C* PR_LTMP								*
C*									*
C* This function computes the temperature of a parcel lifted (or sunk)	*
C* adiabatically to a given pressure.					*
C*									*
C* REAL PR_LTMP  ( THTA, THTE, PRES )					*
C*									*
C* Input parameters:							*
C*	THTA		REAL	Potential temperature in Kelvin		*
C*	THTE		REAL	Equivalent potential temp in Kelvin	*
C*	PRES		REAL	Lifted pressure				*
C*									*
C* Output parameters:							*
C*	PR_LTMP		REAL	Lifted temperature in Celsius		*
C**									*
C* Log:									*
C* J. Nielsen/SUNYA	 8/90						*
C* T. Lee/GSC		 8/97	Computed temperature in Celsius		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
	PR_LTMP = RMISSD
	IF  ( ERMISS (thta) .or. ERMISS (thte) .or. ERMISS (pres) )
     +	      RETURN
C
C*	Compute parcel temperatures on moist and dry adiabats.
C
	guess = 0.
	tmpe  = PR_TMST  ( thte, pres, guess )
	tmpd  = PR_TMPK  ( pres, thta )
	IF  ( ERMISS (tmpe) .or. ERMISS (tmpd) )  RETURN
C
C*	The correct parcel temperature is the warmer of the 
C*	temperature on the dry adiabat and the temperature on the
C*	moist adiabat.
C
	IF  ( tmpe .gt. tmpd )  THEN
	    PR_LTMP = PR_TMKC (tmpe)
	  ELSE
	    PR_LTMP = PR_TMKC (tmpd)
	END IF
C*
	RETURN
	END
