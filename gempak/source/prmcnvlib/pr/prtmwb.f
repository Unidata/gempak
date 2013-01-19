	FUNCTION PR_TMWB  ( tmpk, rmix, pres )
C************************************************************************
C* PR_TMWB								*
C*									*
C* This function computes wet bulb temperature from the temperature,	*
C* mixing ratio, and pressure.  The result is obtained by solving	*
C* for the temperature at which saturation occurs when the latent	*
C* heat required to vaporize the water is provided by a cooling of	*
C* the air.  The equation representing the process is			*
C*									*
C*	( TMPK - TMWB ) * CP - ( Rsat (TMWB) - RMIX ) * LVAP = 0	*
C*									*
C* This implicit equation is solved by Newton's method, since the	*
C* saturation mixing ratio Rsat is a transcendental function of		*
C* TMWB.								*
C*									*
C* The expressions for the heat of vaporization (LVAP) and saturation	*
C* vapor pressure are equations (2) and (10) from Bolton (MWR, 1980).	*
C*									*
C* REAL PR_TMWB  ( TMPK, RMIX, PRES )					*
C*									*
C* Input parameters:							*
C*	TMPK		REAL		Temperature (K)			*
C*	RMIX		REAL		Mixing ratio (g/kg)		*
C*	PRES		REAL    	Pressure (mb)			*
C*									*
C* Output parameters:							*
C*	PR_TMWB		REAL		Wet bulb temperature (K)	*
C**									*
C* Log:									*
C* K. Brill/NMC		 4/94						*
C* S. Jacobs/NMC	 4/94	Changed input RMIX from	kg/kg to g/kg	*
C* G. Krueger/EAI        4/96	Replaced C->K constant with PR_TMKC 	*
C* T. Lee/GSC		11/96	Changed constant 			*
C* S. Jacobs/NCEP	 8/97	Changed latent heat calc to use PR_LHVP	*
C* T. Lee/GSC		 8/97	Set TMWB <= temp; Simplify computation	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER  	( A = 6.112, B = 17.67, C = 243.5, EPSI = .622 )
	PARAMETER  	( G = B * C )
	PARAMETER  	( ERRMAX = .001 )
C*
	LOGICAL		convrg
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
C*      Check for missing values.
C
	IF  ( ERMISS ( tmpk ) .or. ERMISS ( rmix ) .or. ERMISS ( pres )
     +       .or.  ( pres .le. 0. ) )  THEN
	    PR_TMWB = RMISSD
	  ELSE
C
C*	    Change temperature to degrees Celsius.
C
	    tmp = PR_TMKC (tmpk)
C
C*	    Compute the latent heat of vaporization.
C
	    lvap = PR_LHVP ( tmp )
C
C*	    Compute the specific heat of moist air.
C
	    rmix = rmix / 1000.
	    cp = 1005.7 * ( 1. + .887 * rmix )
C
C*	    Compute L / cp.
C
	    rlocp = lvap / cp
C
C*	    Do Newton iteration.  
C
	    iter = 0
	    twb = tmp
	    convrg = .false.
	    DO WHILE ( iter .le. 50 .and. .not. convrg )
		iter = iter + 1
		bt = B * twb
		tpc = twb + C
		d = ( pres / A ) * EXP ( (-bt) / tpc )
		dm1 = d - 1.
		f = ( tmp - twb ) - rlocp * ( EPSI / dm1 - rmix )
		df = (-G)  / ( tpc * tpc )
		df = d * df * rlocp * EPSI / ( dm1 * dm1 ) - 1.
		cor = f / df
		twb = twb - cor
		IF ( ABS ( cor ) .le. ERRMAX ) convrg = .true.
	    END DO
C*
	    IF ( .not. convrg ) THEN
		PR_TMWB = RMISSD
	      ELSE
		twk     = PR_TMCK (twb)
		IF  ( twk .gt. tmpk )  twk = tmpk
		PR_TMWB = twk
	    END IF
	END IF
C*
	RETURN
	END
