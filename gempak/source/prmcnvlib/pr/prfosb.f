	FUNCTION PR_FOSB  ( tmpc, relh, sped )
C************************************************************************
C* PR_RELH								*
C*									*
C* This function computes the Fosberg Index from temperature, relative	*
C* humidity and wind speed at the surface.				*
C*									*
C* REAL PR_FOSB  ( TMPC, RELH, SPED					*
C*									*
C* Input parameters:							*
C*	TMPC		REAL		Temperature in Celsius		*
C*	RELH		REAL    	Relative humidity in percent	*
C*	SPED		REAL		Wind speed in meters/second	*
C*									*
C* Output parameters:							*
C*	PR_FOSB		REAL		Fosberg Index			*
C**									*
C* Log:									*
C* T. Lee/SAIC		 6/03	Created					*
C************************************************************************
        INCLUDE    	'GEMPRM.PRM'
	PARAMETER       ( A = .03229,  B = .281073, C = .000578 )
	PARAMETER       ( D = 2.22749, E = .160107, F = .014784 )
	PARAMETER       ( G = 21.0606, H = .005565, P = .00035  )
	PARAMETER       ( Q = .483199, R = .3002  , T = 9./5.   )
	PARAMETER       ( U = 1.9425,  V = .868976 )
        INCLUDE    	'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for missing data.
C
	IF  ( ERMISS (tmpc) .or. ERMISS (relh) .or. ERMISS (sped) ) THEN
	    PR_FOSB = RMISSD
	  ELSE
C
C*	    Change temperature to degrees Fahrenheit, wind speed
C*	    from meters/second to miles/hr.	
C
	    tf   = PR_TMCF ( tmpc )
	    sknt = PR_MSKN ( sped )
	    smph = PR_KNMH ( sknt )
C
	    IF ( relh .le. 10. )  THEN
		fw = A + B * relh - C * relh * tf
	      ELSE IF ( relh .le. 50. )  THEN
		fw = D + E * relh - F * tf
	      ELSE
		fw = G + H * relh ** 2 - P * relh * tf - Q * relh
	    END IF
C
	    sss  = sqrt ( 1. + ( smph * smph ) )
	    fwd  = fw / 30.
	    fwd2 = fwd * fwd
	    fwd3 = fwd2 * fwd
	    fire = 1. - 2. * fwd + 1.5 * fwd2 - .5 * fwd3
C
C*	    Find the Fosberg Index.
C
	    PR_FOSB  = ( fire * sss ) / R

	END IF
C*
	RETURN
	END
