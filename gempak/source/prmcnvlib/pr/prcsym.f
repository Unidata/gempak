	FUNCTION PR_CSYM ( ctym )
C************************************************************************
C* PR_CSYM								*
C*									*
C* This function will compute the cloud type symbol number from the 	*
C* synoptic code cloud type number.					*
C*									*
C* REAL PR_CSYM ( CTYM )						*
C*									*
C* Input parameters:							*
C*	CTYM		REAL		Synoptic code for cloud type	*
C*									*
C* Output parameters:							*
C*	PR_CSYM		REAL		Cloud symbol number		*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	 9/91						*
C* K. Brill/NMC		10/91	New names; return zero for missing	*
C************************************************************************
	IF  ( ctym .ge. 1 .and. ctym .le. 9 )  THEN
	    PR_CSYM = ctym + 10.
	  ELSE
	    PR_CSYM = 0.0
	ENDIF
C*
	RETURN
	END
