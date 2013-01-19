	FUNCTION PR_WIND  ( drct, sped ) 
C************************************************************************
C* PR_WIND								*
C*									*
C* This function computes WIND from DRCT and SPED.  WIND is in the	*
C* form SSSDDD, where SSS is the speed and DDD is the direction.	*
C* SPED may be entered in meters/sec or knots; DRCT is in degrees	*
C* The following equation is used:					*
C*									*
C*             WIND = NINT ( SPED ) * 1000 + NINT ( DRCT )		*
C*									*
C* REAL PR_WIND  ( DRCT, SPED )						*
C*									*
C* Input parameters:							*
C*	DRCT		REAL		Wind direction in degrees	*
C*	SPED		REAL		Wind speed 			*
C*									*
C* Output parameters:							*
C*	PR_WIND		REAL		Packed speed and direction	*
C**									*
C* Log:									*
C* M. desJardins/NMC	11/91						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for missing data.
C
	IF  ( ( .not. ERMISS ( drct ) ) .and. 
     +	      ( .not. ERMISS ( sped ) ) )  THEN
	    jdrct   = NINT ( drct )
	    jsped   = NINT ( sped )
	    PR_WIND = jdrct + jsped * 1000
	  ELSE
	    PR_WIND = RMISSD
	END IF
C*
	RETURN
	END
