	FUNCTION PR_CSYL ( ctyl )
C************************************************************************
C* PR_CSYL								*
C*									*
C* This function will compute the cloud type symbol number from the 	*
C* synoptic code cloud type number.					*
C*									*
C* REAL PR_CSYL ( CTYL )						*
C*									*
C* Input parameters:							*
C*	CTYL		REAL		Synoptic code for cloud type	*
C*									*
C* Output parameters:							*
C*	PR_CSYL		REAL		Cloud symbol number		*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	 9/91						*
C* K. Brill/NMC		10/91	New names; return zero for missing	*
C************************************************************************
	IF  ( ctyl .ge. 1 .and. ctyl .le. 9 )  THEN
	    PR_CSYL = ctyl
	  ELSE
	    PR_CSYL = 0.0
	ENDIF
C*
	RETURN
	END
