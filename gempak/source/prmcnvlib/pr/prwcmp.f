	FUNCTION PR_WCMP  ( drct, sped, dcmp )
C************************************************************************
C* PR_WCMP 								*
C*									*
C* This function computes the wind component toward a specified		*
C* direction.  The following equation is used:				*
C*									*
C*    WCMP = -COS ( DRCT - DCMP ) * SPED				*
C*									*
C*         WCMP = component of wind in meters/second			*
C*         DRCT = wind direction in degrees				*
C*         DCMP = direction of desired component			*
C*         SPED = wind speed in meters/second				*
C*									*
C* REAL PR_WCMP ( DRCT, SPED, DCMP )					*
C*									*
C* Input parameters:							*
C*	DRCT		REAL		Wind direction in degrees	*
C*	SPED		REAL		Wind speed in meters/second	*
C*	DCMP		REAL		Direction of desired component	*
C*									*
C* Output parameters:							*
C*	PR_WCMP		REAL		Component of wind in m/s	*
C**									*
C* Log:									*
C* John Nielsen/MIT	 9/88						*
C* John Nielsen/SUNYA	 8/90	GEMPAK 5				*
C* J. Whistler/SSAI	 7/91	Changed COSD to COS and converted to rad*
C* T. Piper/GSC		11/98	Updated prolog				*
C************************************************************************
C*
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
C
C* 	Check for missing input parameters.
C
	IF ( ERMISS (sped) .or. ERMISS (drct) .or. ERMISS (dcmp)) THEN
	    PR_WCMP = RMISSD
	  ELSE
C
C*	Calculate wind speed toward specified direction.
C
	    PR_WCMP = (-COS( ( drct - dcmp ) * DTR )) * sped
	END IF
C*
	RETURN
	END
