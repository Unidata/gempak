	FUNCTION PR_SKYX  ( skyc, drct, sped ) 
C************************************************************************
C* PR_SKYX								*
C*									*
C* This function combines the sky coverage symbol number with the   	*
C* wind speed (m/s or knots) and direction in degrees.			*
C* The following equation is used:					*
C*									*
C* SKYX = NINT ( SKYC ) + NINT ( SPED ) * 10000 + NINT ( DRCT ) * 10	*
C*									*
C* REAL PR_SKYX  ( SKYC, DRCT, SPED )					*
C*									*
C* Input parameters:							*
C*	SKYC		REAL 		Sky coverage			*
C*	DRCT		REAL		Wind direction in degrees	*
C*	SPED		REAL		Wind speed 			*
C*									*
C* Output parameters:							*
C*	PR_SKYX		REAL		Packed speed and direction	*
C**									*
C* Log:									*
C* K. Brill/NMC		12/91						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for missing data.
C
	IF  ( ( .not. ERMISS ( skyc ) ) .and. 
     +	      ( .not. ERMISS ( drct ) ) .and.
     +	      ( .not. ERMISS ( sped ) ) )  THEN
	    jdrct   = NINT ( drct )
	    jsped   = NINT ( sped )
	    jskyc   = NINT ( skyc )
	    PR_SKYX = jdrct * 10 + jsped * 10000 + jskyc 
	  ELSE
	    PR_SKYX = RMISSD
	END IF
C*
	RETURN
	END
