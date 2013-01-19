	FUNCTION PR_WCHT ( tmpf, sknt )
C************************************************************************
C* PR_WCHT								*
C*									*
C* This function computes WCHT, the wind chill temperature from TMPF	*
C* and SKNT.  The input values will first be converted to miles per	*
C* hour.  The output will be calculated in Fahrenheit.			*
C*									*
C* WCHT is the temperature with calm winds that produces the same	*
C* cooling effect as the given temperature with the given wind speed.	*
C*									*
C* The following equation is used:					*
C*									*
C*	PR_WCHT = 35.74 + ( .6215 * TMPF ) - 35.75 * WCI ( V ) +	*
C*		  0.4274 * TMPF * WCI ( V )				*
C*									*
C*		where: 	WCI ( V ) = ( V ** 0.16 )			*
C*			V: Wind Speed (mph)				*
C*									*
C* REAL PR_WCHT ( TMPF, SKNT )						*
C*									*
C* Input parameters:							*
C*	TMPF		REAL		Air temperature in deg F	*
C*	SKNT		REAL		Wind speed in knots		*
C*									*
C* Output parameters:							*
C*	PR_WCHT		REAL		Wind Chill temperature in deg F	*
C**									*
C* Log:									*
C* T. Lee/SAIC		 9/01						*
C* T. Lee/SAIC		 9/01	Set windchill <= air temp		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERMISS.FNC'
C*
	WCI ( vel ) =  ( vel ** 0.16 )
C------------------------------------------------------------------------
C*	Convert input variables to mph.
C
	smph = PR_KNMH ( sknt )
C
C*	Compute the wind chill temp if the inputs are not missing
C*	and the wind speed is greater than 3 mph.
C
	IF  ( ( ERMISS ( tmpf ) ) .or. ( ERMISS ( smph ) ) ) THEN
	    PR_WCHT = RMISSD
	ELSE IF ( smph .le. 3. )  THEN
	    PR_WCHT = tmpf
	ELSE
	    wcht = 35.74 + .6215 * tmpf - 35.75 * WCI ( smph ) +
     +		      .4275 * tmpf * WCI ( smph )
	    IF  ( wcht .gt. tmpf )  THEN
		PR_WCHT = tmpf
	      ELSE
		PR_WCHT = wcht
	    END IF
	END IF
C
	RETURN
	END
