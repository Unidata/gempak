	FUNCTION PR_WCEQ ( tmpf, sknt )
C************************************************************************
C* PR_WCEQ								*
C*									*
C* This function computes WCEQ, the wind chill equivalent temperature,	*
C* from TMPF and SKNT.  The input values will first be converted to	*
C* Celsius and meters per second.  The output will be calculated in	*
C* Fahrenheit.								*
C*									*
C* WCEQ is the temperature with calm winds that produces the same	*
C* cooling effect as the given temperature with the given wind speed.	*
C*									*
C* The following equation is used:					*
C*									*
C*	PR_WCEQ = 33.0 - ( (33.0 - TMPC) * WCI (SPED) / WCI (1.34) )	*
C*									*
C*		where: WCI ( V ) = ( 10.0 * SQRT ( V ) + 10.45 - V )	*
C*									*
C* From: R. Falconer, "Windchill, A Useful Wintertime Weather		*
C*			Variable", Weatherwise, Dec 1968.		*
C*									*
C* REAL PR_WCEQ ( TMPF, SKNT )						*
C*									*
C* Input parameters:							*
C*	TMPF		REAL		Air temperature in deg F	*
C*	SKNT		REAL		Wind speed in knots		*
C*									*
C* Output parameters:							*
C*	PR_WCEQ		REAL		Wind Chill equivalent		*
C*					  temperature in deg F		*
C**									*
C* Log:									*
C* M. Nelson		 7/92	Environmental Information		*
C*				  Summaries C-3, 1975.			*
C* S. Jacobs/EAI	 3/93	Cleaned up				*
C* S. Jacobs/NCEP	 3/96	Recoded and commented equation		*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERMISS.FNC'
C*
	WCI ( vel ) = ( 10.0 * SQRT ( vel ) + 10.45 - vel )
C------------------------------------------------------------------------
C*	Convert input variables to Celsius and meters/second.
C
	tmpc = PR_TMFC ( tmpf )
	sped = PR_KNMS ( sknt )
C
C*	Compute the wind chill temp if the inputs are not missing
C*	and the wind speed is greater than 1.34 m/s.
C
	IF  ( ( ERMISS ( tmpc ) ) .or. ( ERMISS ( sped ) ) ) THEN
	    wndchl = RMISSD
	ELSE IF  ( sped .le. 1.34 )  THEN
	    wndchl = tmpc
	ELSE
	    wndchl = 33.0 - ( ( 33.0-tmpc ) * WCI(sped) / WCI(1.34) )
	END IF
C
C*	Convert to Fahrenheit for the user.
C
	PR_WCEQ = PR_TMCF ( wndchl )
C*
	RETURN
	END
