	FUNCTION PR_WNML  ( drct, sped, dcmp )
C************************************************************************
C* PR_WNML 								*
C*									*
C* This function computes the wind component toward a direction 90	*
C* degrees counterclockwise of a specified direction.  If no		*
C* direction is specified, the component toward north is returned.	*
C* The following equation is used:					*
C*									*
C*    WNML = -COS ( DRCT - ( DCMP-90 ) ) * SPED				*
C*									*
C*         WNML = component of wind in meters/second			*
C*         DRCT = wind direction in degrees				*
C*         DCMP = specified direction					*
C*         SPED = wind speed in meters/second				*
C*									*
C* REAL PR_WNML  ( DRCT, SPED, DCMP )					*
C*									*
C* Input parameters:							*
C*	DRCT		REAL		Wind direction in degrees	*
C*	SPED		REAL		Wind speed in meters/sec	*
C*	DCMP		REAL		Input direction in degrees	*
C*									*
C* Output parameters:							*
C*	PR_WNML		REAL		Component of wind in m/s	*
C**									*
C* Log:									*
C* John Nielsen/MIT	9/88						*
C* John Nielsen/SUNYA	8/90	GEMPAK 5				*
C* J. Whistler/SSAI	7/91	Changed COSD to COS and converted to rad*
C* T. Piper/GSC		11/98	Updated prolog				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
C* 	Check for missing input parameters.
C
	IF  ( ERMISS ( sped ) .or. ERMISS ( drct ) ) THEN
	    PR_WNML = RMISSD
	  ELSE IF  ( ( dcmp .lt. 0. ) .or. ( dcmp .gt. 360. ) )  THEN
		PR_WNML = RMISSD
	  ELSE
C
C*	    Calculate wind speed 90 degrees to left of given direction.
C
	    PR_WNML = (-COS( ( drct - dcmp - 90. ) * DTR )) * sped
	END IF
C*
	RETURN
	END
