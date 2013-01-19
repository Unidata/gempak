	FUNCTION PR_CSYT ( ctyl, ctym, ctyh )
C************************************************************************
C* PR_CSYT								*
C*									*
C* This function will compute the cloud type symbol number for the 	*
C* first level at which clouds are reported.				*
C*									*
C* REAL PR_CSYT ( CTYL, CTYM, CTYH )					*
C*									*
C* Input parameters:							*
C*	CTYL		REAL		Synoptic code for low cloud	*
C*	CTYM		REAL		Synoptic code for middle cloud	*
C*	CTYH		REAL		Synoptic code for high cloud	*
C*									*
C* Output parameters:							*
C*	PR_CSYT		REAL		Cloud symbol number		*
C**									*
C* Log:									*
C* K. Brill/NMC		10/91						*
C* T. Piper/GSC		11/98		Updated prolog			*
C************************************************************************
	IF  ( ctyl .ge. 1 .and. ctyl .le. 9 )  THEN
	    PR_CSYT = ctyl
	  ELSE IF ( ctym .ge. 1 .and. ctym .le. 9 ) THEN
	    PR_CSYT = ctym + 10.
	  ELSE IF ( ctyh .ge. 1 .and. ctyh .le. 9 ) THEN
	    PR_CSYT = ctyh + 20.
	  ELSE
	    PR_CSYT = 0.0
	ENDIF
C*
	RETURN
	END
