	FUNCTION PR_PWAO ( pwwa )
C************************************************************************
C* PR_PWAO								*
C*									*
C* This function converts the numeric WMO weather code for past         *
C* weather reported from an automatic station (WMO code table 4531) to  *
C* the corresponding numeric WMO weather code for past weather          *
C* reported from a manned station (WMO code table 4561).                *
C*									*
C* REAL PR_PWAO ( PWWA ) 						*
C*									*
C* Input parameters:							*
C*	PWWA		REAL		Auto station past weather code  *
C*									*
C* Output parameters:							*
C*	PR_PWAO		REAL		Manned station past code        *
C**									*
C* LOG:									*
C* D. Kidwell/NCEP	10/99		       				*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	INTEGER 	man (0:9)
C*	
 	DATA		man /
     +			 -1, -1,  3,  4,  -1,  5,  6,  7,  8,  9  /
C------------------------------------------------------------------------
C
C*  	Convert automatic station past weather number to manual station
C*	past weather number.  Those values not having an analogue in 
C*	table 4561 are mapped to RMISSD.
C
	PR_PWAO = RMISSD
	iwx     = NINT ( pwwa )
	IF ( ( iwx .ge. 0 ) .and. ( iwx .le. 9 ) ) THEN
	    IF ( man ( iwx ) .ge. 0 ) PR_PWAO = man ( iwx )
	END IF
C*
	RETURN
	END
